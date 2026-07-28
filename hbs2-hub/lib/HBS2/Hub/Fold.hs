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
  , reachableCoords
  , Anomaly(..)
  , eventParts
  , referencesPart
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
    -- | Hash of the canon box, the third and last sort key. Nothing refers to
    -- it; it is here so that two copies of one author box blessed at the same
    -- @seq@ are ordered by their content rather than by the order they were
    -- read (PEP-19 "Ordering").
  , rCanonId   :: HashRef
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
  | UndecodableAuthor HubKey UndecodableWhy
  | UndecodableCanon HubKey UndecodableWhy
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

-- | Something admitted canon should not contain.
--
-- Not a drop reason: every one of these is a fold-legal event, and refusing
-- them would make a clone show less than canon holds, which is worse than
-- showing it and saying so. They are collected here because the fold is the
-- one pass that sees the whole log in order, and reported by @hub verify@
-- (PEP-22), which is the only place they can be acted on.
--
-- The pairs carry (what came before, what this event says), because the useful
-- output names both.
data Anomaly =
    -- | Two admitted events stamped with one @seq@. Deterministic here (the
    -- sort key settles it) but not what either signer intended.
    DupSeq Word64
  | DupNumber Word64          -- ^ two threads with one human number
  | NumberWentBack Word64 Word64
    -- | @folded-ts@ decreasing as @seq@ increases. Load-bearing, since the
    -- render contract's times come from it, and asserted by whoever signs the
    -- canon box (PEP-22).
  | FoldedTsWentBack Word64 Word64
    -- | Two events folded from one Mailbox message. PEP-19 allows exactly one
    -- event per letter, so this is either a bug or two folders racing.
  | DupOrigin HashRef
    -- | An event naming an encrypted part with no @part-secret@ to open it.
    -- Admitted, because the reference is inside a signed author box and
    -- nothing is wrong with the event; what is missing is the key, and no
    -- later event can supply it.
  | PartWithoutSecret
    -- | An attribute whose value is not in the canonical form for its name
    -- ('normalizeAttr'), so the same set of labels can appear under two
    -- different event-ids.
  | UnnormalizedAttr Text
  deriving stock (Eq,Ord,Show)

data Comment = Comment
  { cId         :: EventId
  , cAuthor     :: HubKey
    -- | Which authorized key blessed THIS comment. Per-comment rather than
    -- per-thread because under a delegation the events of one thread are
    -- blessed by different keys, and PEP-22 requires the provenance of each
    -- item separately.
  , cCanonBy    :: HubKey
    -- | What the author said they were replying to, carried through
    -- unvalidated: the target may be an event the owner chose not to fold,
    -- or one in another thread. PEP-19 admits the comment either way and
    -- PEP-22 says the renderer falls back to a flat reply. Dropping it here
    -- would lose a field the author signed and the event-id covers.
  , cReplyTo    :: Maybe EventId
  , cAuthorTs   :: Word64   -- ^ author-declared, epoch ms; advisory, may be anything
  , cFoldedTs   :: Word64   -- ^ owner clock at fold, epoch ms; trusted
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
    -- | Which authorized key blessed the OPEN event. Later events on the
    -- thread may be blessed by other keys under a delegation; each comment
    -- carries its own ('cCanonBy').
  , tsCanonBy  :: HubKey
  , tsAuthorTs :: Word64           -- ^ author-declared creation, epoch ms (advisory)
  , tsCreated  :: Word64           -- ^ folded-at of the open event, epoch ms (trusted)
  , tsUpdated  :: Word64           -- ^ folded-at of the latest event, epoch ms (trusted)
  , tsAttrs    :: HashMap Text Text
  , tsComments :: [Comment]        -- ^ in seq order, oldest first
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
  deriving stock (Eq,Show)

-- | The thread title, an LWW attribute like any other.
tsTitle :: ThreadState -> Text
tsTitle = fromMaybe "" . HM.lookup "title" . tsAttrs

-- | Everything a reader needs from one fold.
--
-- The fold itself is deterministic: it is a single pass over events sorted by
-- @(seq, event-id)@. The CONTAINERS are not an order, though. A HashMap and a
-- HashSet iterate in whatever order the hash of the keys gives, which is
-- stable for a build but is not something to render from. Anything
-- user-visible has to impose its own order, by @number@ for threads and by
-- @seq@ for events (PEP-22); 'frDropped' is a list because @hub verify@
-- prints it and therefore does need a fixed order.
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
    -- | Every Tier B letter already folded, by message hash.
    --
    -- Without this a triage loop cannot tell, after a restart, that a letter
    -- it is re-reading from the mailbox was already honoured: the accepted
    -- path is safe because the author box is stored verbatim and its id is
    -- stable, but an honoured request is re-authored, so its id depends on
    -- the clock and a second honour mints a second event. Canon records the
    -- provenance either way; this is the fold handing it back.
  , frOrigins :: HashSet HashRef
    -- | The repo this canon belongs to: the owner key the fold was given.
    --
    -- Recorded so a caller cannot end up with a view built for one repo and
    -- a fold of another. It is the same key that seeds the maintainer set.
  , frOwner :: HubKey
    -- | Anomalies in what was admitted, in @seq@ order. See 'Anomaly': none of
    -- these stops the fold, and all of them are somebody's mistake.
  , frAnomalies :: [(EventId,Anomaly)]
    -- | Every encrypted part canon references, from any admitted event.
    --
    -- Retention needs this as a set rather than as a walk over the threads:
    -- a canon-aware purge (PEP-21) must keep the trees canon points at, and
    -- they are otherwise scattered across three fields of two records.
  , frParts :: HashSet HashRef
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
                        , rCanonId = canonBoxId (evCanonBox e)
                        , rContent = ac, rCanon = cc }
  where
    box badSig undecodable b = case unboxChecked b of
      Left BoxBadSig      -> Left badSig
      Left (BoxUndecodable k why) -> Left (undecodable k why)
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
-- 3-5 + the fold body). Deterministic: total order by
-- (seq, event-id, canon-box hash).
materialize :: HubKey -> [Resolved] -> FoldResult
materialize owner rs = materializeWith owner rs []

materializeWith :: HubKey -> [Resolved] -> [(EventId,DropReason)] -> FoldResult
materializeWith owner rs0 pre = finish (go (sortOn key rs0) st0)
  where
    -- The third key is what makes this independent of the input order rather
    -- than merely stable. Two copies of one author box at the same seq are one
    -- event ('DupId' drops the loser), and they differ only in their canon
    -- box, which the first two keys do not look at: without this, whichever
    -- copy the caller happened to list first supplied the number, and two
    -- folders reading the same tree in different orders would report a
    -- different frMaxNumber and so mint different numbers next.
    --
    -- A well-formed canon cannot contain such a pair, because the file name is
    -- 'eventFileName' and both copies are therefore the same path. The fold is
    -- still a public function over a list, and @hub verify@ exists to read
    -- canon somebody else wrote.
    key r = (rSeq r, rId r, rCanonId r)

    st0 = S { sMaint    = HS.singleton owner
            , sThreads  = HM.empty
            , sSeen     = HM.empty
            , sRedacted = HS.empty
            -- Unresolvable events have no seq; order them after the rest,
            -- still deterministically among themselves by event-id.
            , sDropped  = [ (maxBound, eid, reason) | (eid,reason) <- pre ]
            , sMaxSeq    = 0
            , sMaxNumber = 0
            , sOrigins   = HS.empty
            , sSeqSeen    = HS.empty
            , sNumbers    = HS.empty
            , sLastNumber = 0
            , sLastFolded = 0
            , sAnoms      = []
            , sParts      = HS.empty
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
      , frOrigins   = sOrigins s
      , frOwner     = owner
      , frAnomalies = [ (e,a) | (_,e,a) <- reverse (sAnoms s) ]
      , frParts     = sParts s
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
    -- order.
    --
    -- This is the top of the range only, not a monotonicity rule: a
    -- maintainer who stamps maxBound-1 still strands the cursor one mint
    -- later, at which point every open is refused as 'CursorExhausted'.
    -- Closing that needs an admission rule about what a stamp may be
    -- relative to the ones before it, which is consensus and cannot be
    -- introduced without bumping @hub-meta@ (PEP-19). Recorded there as a
    -- known bound rather than fixed here.
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
        -- Moderating a thread changes it, so its clock moves with it:
        -- otherwise a thread whose comment was just withdrawn looks
        -- untouched since before the redaction.
        | HM.member target (sSeen s) ->
            keep (threadOfEvent target s) r s
              { sRedacted = HS.insert target (sRedacted s)
              , sThreads  = maybe (sThreads s)
                              (\thr -> HM.adjust (touch r) thr (sThreads s))
                              (threadOfEvent target s)
              }
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
                      , tsCanonBy = rCanonKey r
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

      AComment thr replyto body bodypart ts -> onThread r thr s $ \t ->
        touch r t { tsComments = mkComment r replyto ts body bodypart : tsComments t }

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
  , sOrigins   :: HashSet HashRef
    -- Anomaly bookkeeping: what has been stamped already, and the last
    -- values seen, so a counter going backwards is visible.
  , sSeqSeen    :: HashSet Word64
  , sNumbers    :: HashSet Word64
  , sLastNumber :: Word64
  , sLastFolded :: Word64
  , sAnoms      :: [(Word64,EventId,Anomaly)]
  , sParts      :: HashSet HashRef
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
  , sOrigins   = maybe (sOrigins s) (\o -> HS.insert o (sOrigins s)) (ccOrigin (rCanon r))
  , sSeqSeen   = HS.insert (rSeq r) (sSeqSeen s)
  , sNumbers   = maybe (sNumbers s) (\n -> HS.insert n (sNumbers s)) num
  , sLastNumber = fromMaybe (sLastNumber s) num
  , sLastFolded = folded
  , sParts     = foldr HS.insert (sParts s) (eventParts (rContent r))
    -- Newest first while accumulating, reversed once at the end.
  , sAnoms     = [ (rSeq r, rId r, a) | a <- reverse anoms ] <> sAnoms s
  }
  where
    cc = rCanon r
    num = ccNumber cc
    folded = ccFoldedTs cc

    anoms = concat
      [ [ DupSeq (rSeq r) | HS.member (rSeq r) (sSeqSeen s) ]
      , [ DupNumber n | Just n <- [num], HS.member n (sNumbers s) ]
        -- Numbers are assigned in open order, so one that does not advance is
        -- either a duplicate publisher or a hand-written stamp.
      , [ NumberWentBack (sLastNumber s) n
        | Just n <- [num], sLastNumber s > 0, n <= sLastNumber s ]
      , [ FoldedTsWentBack (sLastFolded s) folded | folded < sLastFolded s ]
      , [ DupOrigin o | Just o <- [ccOrigin cc], HS.member o (sOrigins s) ]
      , [ PartWithoutSecret
        | referencesPart (rContent r), isNothing (ccPartSecret cc) ]
      , [ UnnormalizedAttr k
        | ASet _ k v _ <- [rContent r], not (normalizedAttr k v) ]
      ]

-- | Every encrypted part an event references.
--
-- Here rather than in the bridge because two callers need the same answer for
-- different reasons: the bridge refuses to mint an event whose parts it cannot
-- vouch for, and retention must keep the trees canon points at. One list, so
-- the two cannot drift.
eventParts :: AuthorContent -> [HashRef]
eventParts = \case
  AOpen _ _ _ _ _ part coords _ -> maybe [] pure part <> maybe [] bundle coords
  AComment _ _ _ part _         -> maybe [] pure part
  ARevise _ coords _            -> bundle coords
  _                             -> []
  where
    bundle = maybe [] pure . prBundle

-- | Does this event reference an encrypted part, so that its canon box must
-- carry the group secret for it (PEP-19 "Attachments in public canon")?
referencesPart :: AuthorContent -> Bool
referencesPart = not . null . eventParts

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
  -- A note on a close or reopen answers the thread as a whole, not one
  -- event in it, so it carries no reply-to.
  t { tsComments = mkComment r Nothing ts note Nothing : tsComments t }

-- | Build a comment, carrying the attachment hashref and the group secret
-- the owner published for it, so a reader has everything needed to fetch
-- and decrypt the body without going back to the mailbox.
mkComment :: Resolved -> Maybe EventId -> Word64 -> Maybe Text -> Maybe HashRef -> Comment
mkComment r replyto ts body bodypart = Comment
  { cId = rId r
  , cAuthor = rAuthorKey r
  , cCanonBy = rCanonKey r
  , cReplyTo = replyto
  , cAuthorTs = ts
  , cFoldedTs = ccFoldedTs (rCanon r)
  , cBody = body
  , cBodyPart = bodypart
  , cPartSecret = ccPartSecret (rCanon r)
  , cOrigin = ccOrigin (rCanon r)
  }
