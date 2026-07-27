-- | The deterministic fold (PEP-19 "Deterministic materialization").
--
-- Split in two so the security-critical admission logic is testable without
-- crypto: 'resolve' does the SignedBox unbox (rules 1-2 + event-id binding)
-- and yields a 'Resolved'; 'materialize' runs the seq-ordered pass with the
-- admission rules 3-5, the repo binding, dedup, dangling-thread and
-- revise-of-record checks, and the delegate/revoke maintainer set, over
-- 'Resolved' values alone.
--
-- The fold is pure and total: sort by (seq, event-id), apply. Re-folding the
-- same events always yields the same result, including the drop report.
module HBS2.Hub.Fold
  ( Resolved(..)
  , DropReason(..)
  , ThreadState(..)
  , tsTitle
  , PRState(..)
  , Comment(..)
  , FoldResult(..)
  , resolve
  , materialize
  , foldEvents
  ) where

import HBS2.Hub.Types

import HBS2.Data.Types.Refs (HashRef)

import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (sortOn)
import Data.Maybe (fromMaybe,isJust,isNothing)
import Data.Text (Text)
import Data.Word (Word64)

-- | An event whose two boxes verified and whose canon box references its
-- own event-id (rules 1-2 discharged). The recovered keys are carried so
-- the pure admission pass never touches crypto again.
data Resolved = Resolved
  { rId        :: EventId
  , rSeq       :: Word64
  , rAuthorKey :: HubKey        -- ^ author box signer
  , rCanonKey  :: HubKey        -- ^ canon box signer (canon-by)
  , rContent   :: AuthorContent
  , rCanon     :: CanonContent
  }

-- | Why an event was not applied. Surfaced for @hub verify@ (PEP-22), so
-- the reasons are split finely enough to diagnose from.
data DropReason =
    BadAuthorSig      -- ^ author box failed to verify (rule 1)
  | BadCanonSig       -- ^ canon box failed to verify (rule 2)
    -- | A box whose signature is good but whose content this reader cannot
    -- decode: canon is newer than this build. Distinct from a bad signature,
    -- which is an accusation of forgery, and split by box like the signature
    -- failures are, since which side is from the future is the useful part.
  | UndecodableAuthor
  | UndecodableCanon
  | IdMismatch        -- ^ canon box blesses a different event-id (rule 2)
  | WrongTarget       -- ^ open event authored for a different repo (cross-repo replay)
  | DupId             -- ^ event-id already seen (rewrap/replay, PEP-18/PEP-19)
  | UnauthorizedCanon -- ^ canon-by (or author of an owner-op) not an authorized key
  | UnauthorizedDelegate -- ^ delegate/revoke not signed by the LWWRef owner key (rule 5)
  | BadThread         -- ^ reply to a non-admitted open (dangling)
  | BadRevise         -- ^ revise not from the author of record
  | BadKind           -- ^ kind and payload disagree, or a PR-only op on an issue
  | UnknownRedact     -- ^ redact target never admitted (no-op)
    -- | A canon box whose stamped values are not usable: a @number@ on
    -- anything but an @open@, or a @seq@/@number@ at the very top of the
    -- range, which would leave the next mint with nowhere to go. A wrong or
    -- hostile maintainer could otherwise poison the cursor permanently.
  | BadStamp
  deriving stock (Eq,Ord,Show)

data Comment = Comment
  { cId         :: EventId
  , cAuthor     :: HubKey
  , cAuthorTs   :: Word64   -- ^ declared by the author; advisory, may be anything
  , cFoldedTs   :: Word64   -- ^ owner clock at fold; trusted
  , cBody       :: Maybe Text
  , cBodyPart   :: Maybe HashRef     -- ^ large body shipped as an encrypted tree
  , cPartSecret :: Maybe ByteString  -- ^ group secret the owner published for it
  , cOrigin     :: Maybe HashRef     -- ^ the Tier B letter this was folded from
  }
  deriving stock (Eq,Show)

data PRState = PRState
  { psCoords :: PRCoords
    -- | The group secret for THIS revision's bundle, taken from the canon
    -- box of the event that supplied 'psCoords'. It must travel with the
    -- coordinates: every Mailbox message has its own per-message group key,
    -- so a later revise ships a new bundle under a new secret, and keeping
    -- the opening event's secret here would hand a reader the wrong key.
  , psPartSecret :: Maybe ByteString
  , psMerge  :: Maybe (Text,Text)  -- ^ (merge-commit, merged-into)
  }
  deriving stock (Eq,Show)

-- | Materialized thread. Structured fields (status, labels, assignee,
-- title, ...) all live in 'tsAttrs' under last-writer-wins, so there is a
-- single source of truth for anything a @set@ can change (PEP-19).
--
-- Times come from the canon box ('ccFoldedTs'), not from the author's
-- declared timestamp, which is advisory and attacker-chosen; the declared
-- value is kept alongside for display.
data ThreadState = ThreadState
  { tsId       :: ThreadId
  , tsKind     :: HubKind
  , tsNumber   :: Maybe Word64
  , tsAuthor   :: HubKey
  , tsAuthorTs :: Word64           -- ^ author-declared creation time (advisory)
  , tsCreated  :: Word64           -- ^ folded-at time of the open event (trusted)
  , tsUpdated  :: Word64           -- ^ folded-at time of the latest event (trusted)
  , tsAttrs    :: HashMap Text Text
  , tsComments :: [Comment]        -- ^ in seq order
  , tsPR       :: Maybe PRState
    -- | Labels the author asked for on open. Deliberately NOT merged into
    -- 'tsAttrs': applying them would let a stranger label their own
    -- submission. Triage shows them; the owner honours one with a signed set.
  , tsLabelsRequested :: [Text]
  , tsBody       :: Maybe Text
    -- | Body shipped as an encrypted tree, with the group secret the owner
    -- published at fold so any clone can fetch and decrypt it (PEP-18/19
    -- "Attachments in public canon"). This pair belongs to the OPENING
    -- event only; a comment carries its own, and a PR bundle's secret
    -- lives in 'PRState', because each message has its own group key.
  , tsBodyPart   :: Maybe HashRef
  , tsPartSecret :: Maybe ByteString
    -- | The Tier B letter this was folded from, for provenance.
  , tsOrigin     :: Maybe HashRef
  }

-- | The thread title, an LWW attribute like any other.
tsTitle :: ThreadState -> Text
tsTitle = fromMaybe "" . HM.lookup "title" . tsAttrs

data FoldResult = FoldResult
  { frThreads  :: HashMap ThreadId ThreadState
  , frRedacted :: HashSet EventId          -- ^ events a renderer must withhold
  , frDropped  :: [(EventId,DropReason)]   -- ^ deterministic order
    -- | The largest @seq@ and @number@ actually admitted, 0 when none. A
    -- publisher mints the next ones from these, so minting is a function of
    -- canon rather than of any node-local counter.
  , frMaxSeq    :: Word64
  , frMaxNumber :: Word64
    -- | Every admitted event, mapped to the thread it belongs to, or
    -- 'Nothing' for the repo-scope ones (@delegate@/@revoke@).
    --
    -- The fold knows this exactly; reconstructing it from 'frThreads' would
    -- miss every event that leaves no visible trace (a @set@, a @merge@, a
    -- note-less @close@), which is enough to break both dedup and the layout
    -- a writer needs (PEP-19 puts thread events under @threads/@ and the
    -- rest under @repo/@).
  , frAdmitted  :: HashMap EventId (Maybe ThreadId)
    -- | The authorized canon keys as of the end of the log: the owner plus
    -- everyone delegated and not since revoked. A folder needs this to know
    -- whether its own key may still bless anything, which nothing else in
    -- the result reveals.
  , frMaintainers :: HashSet HubKey
  }

-- | Discharge rules 1-2: both boxes verify, and the canon box references
-- this event's id.
resolve :: Event -> Either DropReason Resolved
resolve e = do
  (akey, ac) <- box BadAuthorSig UndecodableAuthor (evAuthorBox e)
  (ckey, cc) <- box BadCanonSig  UndecodableCanon  (evCanonBox e)
  let eid = eventId e
  if ccEventId cc /= eid
    then Left IdMismatch
    else Right Resolved { rId = eid, rSeq = ccSeq cc
                        , rAuthorKey = akey, rCanonKey = ckey
                        , rContent = ac, rCanon = cc }
  where
    box badSig undecodable b = case unboxChecked b of
      Left BoxBadSig      -> Left badSig
      Left BoxUndecodable -> Left undecodable
      Right ok            -> Right ok

-- | The whole fold: resolve, then materialize.
--
-- The first argument is the repo's LWWRef owner key: it is both the root of
-- trust for canon signing and the repo identity an @open@ must name.
foldEvents :: HubKey -> [Event] -> FoldResult
foldEvents owner es = materializeWith owner resolved unresolved
  where
    (unresolved, resolved) = foldr step ([],[]) es
    step e (bad,oks) = case resolve e of
      Right r     -> (bad, r:oks)
      Left reason -> ((eventId e, reason):bad, oks)

-- | The pure, seq-ordered fold over resolved events (PEP-19 admission rules
-- 3-5 + the fold body). Deterministic: total order by (seq, event-id).
materialize :: HubKey -> [Resolved] -> FoldResult
materialize owner rs = materializeWith owner rs []

materializeWith :: HubKey -> [Resolved] -> [(EventId,DropReason)] -> FoldResult
materializeWith owner rs0 pre = finish (go (sortOn key rs0) st0)
  where
    key r = (rSeq r, rId r)

    st0 = S { sMaint    = HS.singleton owner
            , sThreads  = HM.empty
            , sSeen     = HM.empty
            , sRedacted = HS.empty
            -- Unresolvable events have no seq; order them after the rest,
            -- still deterministically among themselves by event-id.
            , sDropped  = [ (maxBound, eid, reason) | (eid,reason) <- pre ]
            , sMaxSeq    = 0
            , sMaxNumber = 0
            }

    -- Comments accumulate reversed (O(1) append) and are flipped here.
    finish s = FoldResult
      { frThreads  = fmap unrev (sThreads s)
      , frRedacted = sRedacted s
      -- Ordered by the whole triple: two drops sharing (seq, event-id) but
      -- differing in reason must not fall back on input order.
      , frDropped  = [ (e,d) | (_,e,d) <- sortOn id (sDropped s) ]
      , frMaxSeq    = sMaxSeq s
      , frMaxNumber = sMaxNumber s
      , frAdmitted  = sSeen s
      , frMaintainers = sMaint s
      }

    unrev t = t { tsComments = reverse (tsComments t) }

    go [] s = s
    go (r:rest) s
      | HM.member (rId r) (sSeen s) = go rest (dropE r DupId s)
      | not (sane r)                = go rest (dropE r BadStamp s)
      | otherwise                   = go rest (apply r s)

    -- The stamped values must be usable. A number belongs to an open and
    -- nothing else, and neither counter may sit at the top of its range: the
    -- next mint takes the maximum plus one, so admitting maxBound would wrap
    -- the cursor to zero and send every later event to the front of the
    -- order. Cheap to check here, and it keeps one bad canon box from
    -- poisoning the repo permanently.
    sane r = numberOK && seqOK && numberSmallEnough
      where
        cc = rCanon r
        numberOK = case (rContent r, ccNumber cc) of
          (AOpen{}, _)      -> True
          (_, Nothing)      -> True
          (_, Just _)       -> False
        seqOK = ccSeq cc /= maxBound
        numberSmallEnough = maybe True (/= maxBound) (ccNumber cc)

    apply r s = case rContent r of

      ADelegate k _
        | ownerOnly r -> keep repoScope r s { sMaint = HS.insert k (sMaint s) }
        | otherwise   -> dropE r UnauthorizedDelegate s

      -- Revoking the owner key is a no-op: it is the root of trust and
      -- cannot be delegated away, so it cannot be withdrawn either.
      ARevoke k _
        | not (ownerOnly r) -> dropE r UnauthorizedDelegate s
        | k == owner        -> keep repoScope r s
        | otherwise         -> keep repoScope r s { sMaint = HS.delete k (sMaint s) }

      -- Owner-authored (PEP-19 rule 4): BOTH boxes must be an authorized
      -- key. Checking only the canon box would let anyone author a redact
      -- and hide any event in the repo as soon as it got blessed.
      ARedact target _
        | not (ownerAuthored r s)    -> dropE r UnauthorizedCanon s
        | HM.member target (sSeen s) -> keep (threadOfEvent target s) r s { sRedacted = HS.insert target (sRedacted s) }
        | otherwise                  -> dropE r UnknownRedact s

      AOpen target kind title labels body bodypart mpr ts
        -- The repo binding: an author box is signed for one repo, so it
        -- cannot be lifted out of another repo's canon and replayed here.
        | target /= owner    -> dropE r WrongTarget s
        | not (canonOK r s)  -> dropE r UnauthorizedCanon s
        -- Kind and payload must agree. A PR must carry its coordinates
        -- (nothing may invent them later); an issue must not, or canon
        -- would silently materialize less than the author box says.
        | kind == HubPR && isNothing mpr -> dropE r BadKind s
        | kind == HubIssue && isJust mpr -> dropE r BadKind s
        | maybe False (not . reachableCoords) mpr -> dropE r BadKind s
        | otherwise ->
            let t = ThreadState
                      { tsId = rId r, tsKind = kind
                      , tsNumber = ccNumber (rCanon r)
                      , tsAuthor = rAuthorKey r
                      , tsAuthorTs = ts
                      , tsCreated = foldedTs r, tsUpdated = foldedTs r
                      , tsAttrs = HM.fromList [("status","open"),("title",title)]
                      , tsComments = []
                      , tsPR = if kind == HubPR
                                 then fmap (\c -> PRState c (ccPartSecret (rCanon r)) Nothing) mpr
                                 else Nothing
                      , tsLabelsRequested = labels
                      , tsBody = body
                      , tsBodyPart = bodypart
                      , tsPartSecret = ccPartSecret (rCanon r)
                      , tsOrigin = ccOrigin (rCanon r)
                      }
            in keep (Just (rId r)) r s { sThreads = HM.insert (rId r) t (sThreads s) }

      AComment thr _replyto body bodypart ts -> onThread r thr s $ \t ->
        touch r t { tsComments = mkComment r ts body bodypart : tsComments t }

      ARevise thr coords _ts -> onThreadWith r thr s $ \t ->
        if tsKind t /= HubPR then Left BadKind
        else if not (reachableCoords coords) then Left BadKind
        else if rAuthorKey r == tsAuthor t || HS.member (rAuthorKey r) (sMaint s)
          -- The new coordinates come with the secret for their own bundle.
          then Right (touch r t { tsPR = Just (PRState coords (ccPartSecret (rCanon r))
                                                      (tsPR t >>= psMerge)) })
          else Left BadRevise

      ASet thr k v _ts -> onOwnerThread r thr s $ \t ->
        touch r t { tsAttrs = HM.insert k v (tsAttrs t) }

      AClose thr note ts -> onOwnerThread r thr s $ \t ->
        addNote note r ts $ touch r t { tsAttrs = HM.insert "status" "closed" (tsAttrs t) }

      AReopen thr note ts -> onOwnerThread r thr s $ \t ->
        addNote note r ts $ touch r t { tsAttrs = HM.insert "status" "open" (tsAttrs t) }

      AMerge thr mc into _ts -> onOwnerThreadWith r thr s $ \t ->
        case (tsKind t, tsPR t) of
          (HubPR, Just pr) ->
            Right (touch r t { tsPR = Just pr { psMerge = Just (mc,into) }
                             , tsAttrs = HM.insert "status" "merged" (tsAttrs t) })
          _ -> Left BadKind

    foldedTs = ccFoldedTs . rCanon

    touch r t = t { tsUpdated = max (tsUpdated t) (foldedTs r) }

    -- rule 5: delegate/revoke require BOTH author and canon-by == owner key.
    ownerOnly r = rAuthorKey r == owner && rCanonKey r == owner

    -- rules 3+4: canon-by must be an authorized canon key; for owner-authored
    -- ops the author box signer must be too. For open/comment only canon-by
    -- must be authorized (the author may be anyone).
    canonOK r s = HS.member (rCanonKey r) (sMaint s)
    ownerAuthored r s = canonOK r s && HS.member (rAuthorKey r) (sMaint s)

    onThread r thr s f = withThread canonOK r thr s (Right . f)
    onOwnerThread r thr s f = withThread ownerAuthored r thr s (Right . f)
    onThreadWith r thr s f = withThread canonOK r thr s f
    onOwnerThreadWith r thr s f = withThread ownerAuthored r thr s f

    withThread auth r thr s f
      | not (auth r s) = dropE r UnauthorizedCanon s
      | otherwise = case HM.lookup thr (sThreads s) of
          Nothing -> dropE r BadThread s
          Just t  -> case f t of
                       Left reason -> dropE r reason s
                       Right t'    -> keep (Just thr) r s { sThreads = HM.insert thr t' (sThreads s) }

-- Internal accumulator.
data St = S
  { sMaint    :: HashSet HubKey
  , sThreads  :: HashMap ThreadId ThreadState
  , sSeen     :: HashMap EventId (Maybe ThreadId)
  , sRedacted :: HashSet EventId
  , sDropped  :: [(Word64,EventId,DropReason)]
  , sMaxSeq    :: Word64
  , sMaxNumber :: Word64
  }

-- Mark an event applied (dedup + the id every later redact resolves
-- against), record which thread it belongs to, and advance the high-water
-- marks a publisher mints from. Only admitted events count: a dropped one
-- must not consume a seq or a number.
keep :: Maybe ThreadId -> Resolved -> St -> St
keep scope r s = s
  { sSeen      = HM.insert (rId r) scope (sSeen s)
  , sMaxSeq    = max (sMaxSeq s) (rSeq r)
  , sMaxNumber = max (sMaxNumber s) (fromMaybe 0 (ccNumber (rCanon r)))
  }

-- | Can the proposed change actually be obtained?
--
-- PEP-20 offers two ways to ship a diff, a bundle attached to the letter or
-- a fork to pull from, and the coordinates say which by leaving the other
-- absent. With neither, canon would carry a PR nobody can fetch, which is
-- worse than refusing it: a maintainer would see a review request with
-- nothing to review and no way to ask for the missing half.
--
-- This is an admission rule, so it is consensus (see the versioning rule in
-- PEP-19): it cannot be relaxed once a repo has canon written under it.
reachableCoords :: PRCoords -> Bool
reachableCoords c = isJust (prBundle c) || isJust (prSource c)

-- Events that belong to no thread.
repoScope :: Maybe ThreadId
repoScope = Nothing

-- Which thread an already-admitted event belongs to, for a redact naming it.
threadOfEvent :: EventId -> St -> Maybe ThreadId
threadOfEvent eid s = fromMaybe Nothing (HM.lookup eid (sSeen s))

dropE :: Resolved -> DropReason -> St -> St
dropE r reason s = s { sDropped = (rSeq r, rId r, reason) : sDropped s }

-- A close/reopen note becomes a comment attached to the status change.
addNote :: Maybe Text -> Resolved -> Word64 -> ThreadState -> ThreadState
addNote Nothing _ _ t = t
addNote note@(Just _) r ts t =
  t { tsComments = mkComment r ts note Nothing : tsComments t }

-- | Build a comment, carrying the attachment hashref and the group secret
-- the owner published for it, so a reader has everything needed to fetch
-- and decrypt the body without going back to the mailbox.
mkComment :: Resolved -> Word64 -> Maybe Text -> Maybe HashRef -> Comment
mkComment r ts body bodypart = Comment
  { cId = rId r
  , cAuthor = rAuthorKey r
  , cAuthorTs = ts
  , cFoldedTs = ccFoldedTs (rCanon r)
  , cBody = body
  , cBodyPart = bodypart
  , cPartSecret = ccPartSecret (rCanon r)
  , cOrigin = ccOrigin (rCanon r)
  }
