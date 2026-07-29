-- The fold dispatches on every constructor of 'AuthorContent', and one it
-- forgets is a crash rather than a drop, so that warning is an error here.
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | The deterministic fold (PEP-19 "Deterministic materialization").
--
-- Split in two so the security-critical admission logic is testable without
-- crypto: 'resolve' does the SignedBox unbox (rules 1-2 + event-id binding)
-- and yields a 'Resolved'; 'materialize' runs the seq-ordered pass with the
-- admission rules 3-5, the repo binding, dedup, dangling-thread and
-- revise-of-record checks, and the delegate/revoke maintainer set, over
-- 'Resolved' values alone.
--
-- An event whose author box this build cannot use still takes part in the
-- ordered pass as a stamp and nothing else (see 'ghostStamp'): its canon box is
-- a separate signature over separate bytes, and the @seq@ is in that one.
--
-- The fold is pure and total: sort by (seq, event-id), apply. Re-folding the
-- same events always yields the same result, including the drop report.
module HBS2.Hub.Fold
  ( Resolved(..)
  , DropReason(..)
  , Dropped(..)
  , Anomalous(..)
  , LogEntry(..)
  , ThreadState(..)
  , tsTitle
  , PRState(..)
  , Comment(..)
  , FoldResult(..)
  , resolve
  , materialize
  , foldEvents
  , foldCanon
  , CanonTooNew(..)
  , reachableCoords
  , Anomaly(..)
  , eventParts
  , referencesPart
  ) where

import HBS2.Hub.Types

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef)
import HBS2.Prelude.Plated (Doc,Pretty(..),(<+>))

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (sortOn)
import Data.Maybe (fromMaybe,isJust,isNothing)
import Data.Text (Text)
import Data.Word (Word32,Word64)

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
  | UnknownRedact     -- ^ redact target never admitted (no-op)
    -- Kind and payload disagreeing, split four ways, because @hub verify@ has
    -- to name the cause and the event cannot be re-examined to recover it: a
    -- reader looking at a dropped PR cannot tell whether the coordinates were
    -- missing, unreachable, or on the wrong kind of thread.
  | PROpenWithoutCoords   -- ^ a @pr@ open carrying none
  | IssueOpenWithCoords   -- ^ an @issue@ open carrying some
  | CoordsUnreachable     -- ^ neither a fork to pull nor a bundle to fetch
  | PROnlyOnIssue         -- ^ a @revise@ or a @merge@ on an issue thread
    -- A canon box whose stamped values are not usable, likewise split: a wrong
    -- or hostile maintainer can poison a counter permanently, and which counter
    -- is the first thing an operator needs.
  | NumberOnNonOpen       -- ^ a @number@ on anything but an @open@
  | SeqAtTopOfRange       -- ^ the next mint would wrap to zero
  | NumberAtTopOfRange
  | FoldedTsAboveCeiling  -- ^ past 'maxFoldedTs'
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
    -- | And the mirror: a @part-secret@ on an event that references no part.
    -- Not a leak now that the parts have a secret of their own (PEP-18), but
    -- it is still a key published for nothing.
  | SecretWithoutPart
    -- | A @part-secret@ that cannot be a key: the wrong number of bytes. Not a
    -- drop, since the event is intact and only the key is unusable, and not
    -- repairable either, so a reader has to be told the attachment will never
    -- open.
  | UnusablePartSecret
    -- | An attribute whose value is not in the canonical form for its name
    -- ('normalizeAttr'), so the same set of labels can appear under two
    -- different event-ids.
  | UnnormalizedAttr Text
  deriving stock (Eq,Ord,Show)

-- | One refusal, with what @hub verify@ has to print about it (PEP-22, which
-- requires naming the canon keys involved).
--
-- A pair of (event-id, reason) was not enough for that: neither half says who
-- blessed the event or where in the log it sits, and both are what an operator
-- needs to act. The two are optional together, and only for an event whose
-- canon box could not be opened at all, which is the one case where nothing is
-- known but the file's own hash.
data Dropped = Dropped
  { drEvent   :: EventId
  , drSeq     :: Maybe Word64
  , drCanonBy :: Maybe HubKey
  , drWhy     :: DropReason
  }
  deriving stock (Eq,Show)

-- | One admitted event, in the order the fold applied it.
--
-- The materialized threads are a HashMap and say nothing about order, and
-- 'frAdmitted' is a map too, so neither can answer "what happened, in order",
-- which is the whole of @hub log@ (PEP-22) and the only view an event that
-- leaves no visible trace appears in at all: a @set@, a @merge@, a note-less
-- @close@, a @delegate@. Reconstructing it outside would mean sorting canon a
-- second time and getting the tie-break right a second time.
data LogEntry = LogEntry
  { lgSeq      :: Word64
  , lgEvent    :: EventId
  , lgThread   :: Maybe ThreadId   -- ^ 'Nothing' for the repo-scope ops
  , lgAuthor   :: HubKey
  , lgCanonBy  :: HubKey
  , lgFoldedTs :: Word64
  , lgOrigin   :: Maybe HashRef
  , lgContent  :: AuthorContent
  }
  deriving stock (Eq,Show)

-- | One anomaly, with the same provenance. Never optional here: an anomaly is
-- reported against an event that WAS admitted, so its stamp and its blesser
-- are both known.
data Anomalous = Anomalous
  { anEvent   :: EventId
  , anSeq     :: Word64
  , anCanonBy :: HubKey
  , anWhat    :: Anomaly
  }
  deriving stock (Eq,Show)

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
  , cPartSecret :: Maybe PartSecret  -- ^ group secret the owner published for it
  , cOrigin     :: Maybe HashRef     -- ^ the Tier B letter this was folded from
    -- | This comment was redacted, so a renderer must withhold its body.
    --
    -- Here for the reason 'tsRedacted' is on the thread, only more so: a
    -- comment is what a redaction is usually FOR, and a renderer that forgot
    -- to join against the flat set would publish the withdrawn text in every
    -- clone while the redact reported success.
  , cRedacted   :: Bool
  }
  deriving stock (Eq,Show)

data PRState = PRState
  { psCoords :: PRCoords
    -- | The group secret for THIS revision's bundle, taken from the canon
    -- box of the event that supplied 'psCoords'. It must travel with the
    -- coordinates: every Mailbox message has its own per-message group key,
    -- so a later revise ships a new bundle under a new secret, and keeping
    -- the opening event's secret here would hand a reader the wrong key.
  , psPartSecret :: Maybe PartSecret
  , psMerge  :: Maybe (Text,Text)  -- ^ (merge-commit, merged-into)
    -- | Who supplied THESE coordinates, and which canon key blessed that.
    --
    -- Not the same as the thread's author: PEP-19 lets a maintainer revise a
    -- PR, so the tip a reviewer is looking at can have been chosen by someone
    -- other than the person the thread is attributed to. Without this pair a
    -- thread displayed as alice's can point at anyone's fork and nothing
    -- distinguishes the two.
  , psAuthor  :: HubKey
  , psCanonBy :: HubKey
  }
  deriving stock (Eq,Show)

-- | Materialized thread. Structured fields (status, labels, assignees,
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
    -- | The thread's OWN opening event is redacted, so a renderer must
    -- withhold its title and body. Here as well as in 'frRedacted' because
    -- the render contract asks per thread (PEP-22) and a join by 'tsId'
    -- against a flat set is a join a renderer will forget.
  , tsRedacted :: Bool
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
  , tsPartSecret :: Maybe PartSecret
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
  , frDropped  :: [Dropped]                -- ^ deterministic order
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
    -- | Every request already honoured, by the author box the requester signed.
    --
    -- Separate from the origins above because a rewrap gives one request many
    -- message hashes and exactly one box: dedup by the hash honours the same
    -- request again under every new envelope.
  , frHonoured :: HashSet EventId
    -- | The repo this canon belongs to: the owner key the fold was given.
    --
    -- Recorded so a caller cannot end up with a view built for one repo and
    -- a fold of another. It is the same key that seeds the maintainer set.
  , frOwner :: HubKey
    -- | Anomalies in what was admitted, in @seq@ order. See 'Anomaly': none of
    -- these stops the fold, and all of them are somebody's mistake.
  , frAnomalies :: [Anomalous]
    -- | Every encrypted part canon references, from any admitted event.
    --
    -- Retention needs this as a set rather than as a walk over the threads:
    -- a canon-aware purge (PEP-21) must keep the trees canon points at, and
    -- they are otherwise scattered across three fields of two records.
  , frParts :: HashSet HashRef
    -- | Every admitted event in @seq@ order. See 'LogEntry'.
  , frLog :: [LogEntry]
    -- | The @folded-ts@ of the highest-@seq@ admitted event, 0 when none.
    --
    -- A folder mints the next one from this, so a clock that is behind canon
    -- cannot walk it backwards. Same reasoning as the cursor: the value a
    -- publisher stamps comes from canon, not from node-local state.
  , frLastFolded :: Word64
  }

-- | What a canon box stamps: the two counters, and the key that blessed them.
--
-- Recovered separately from the event, because it is spent separately from it
-- (see 'stamp' in 'materializeWith').
data Stamp = Stamp
  { spSeq    :: Word64
  , spKey    :: HubKey        -- ^ the canon box signer
    -- | The repository the canon box was signed for. Without it, "a key this
    -- log authorized" is not the question the check below thinks it is asking:
    -- a maintainer of two repositories is authorized in both, so their stamp
    -- from one would be counted in the other.
  , spTarget :: RepoRef
  }

-- | The stamp of an event this build could not resolve.
--
-- The author box and the canon box are two signatures over two payloads, and
-- only the second carries the @seq@. So an event whose author content is from a
-- newer schema ('UndecodableAuthor') still says exactly where it sits, and this
-- is the case that forces the distinction: PEP-19 makes a new op an
-- append-only addition to 'AuthorContent', which is precisely what an older
-- build cannot decode. A build that skipped those seqs would mint into them,
-- and the newer build reading the result would see two events at one @seq@ with
-- every LWW attribute between them settled by a hash rather than by time.
--
-- 'Nothing' when the canon box itself does not verify or does not decode: then
-- there is no stamp, only bytes claiming to be one.
-- Deliberately says nothing about @number@: that one is spent on admission, and
-- this event was not admitted. A build that counted it would hand out different
-- numbers than the build that can read the content and drops it, from the same
-- canon.
ghostStamp :: Event -> Maybe (Stamp, Maybe Word64)
ghostStamp e = case unboxChecked (evCanonBox e) of
  -- The blessing has to be a blessing OF THIS EVENT. A canon box is a separate
  -- signature over separate bytes, so anyone can staple a maintainer's canon
  -- box to an author box of their own: the signature verifies, the key is
  -- authorized, and the stamp used to be spent on the strength of that. What it
  -- bought was a number recorded from a stranger's file, which the next honest
  -- open is then accused of duplicating, with the honest maintainer's key
  -- printed beside it. 'resolve' already calls this IdMismatch on the readable
  -- path; this is the same rule where the content cannot be read.
  Right (k, cc) | ccEventId cc == eventId e ->
    Just (Stamp (ccSeq cc) k (ccTarget cc), ccNumber cc)
  _ -> Nothing

-- One entry in the ordered pass.
data Item =
    Whole Resolved
    -- | An event that did not resolve but whose canon box did: its id, why it
    -- was dropped, its stamp, and the canon box hash for the tie-break.
  | Ghost EventId DropReason Stamp (Maybe Word64) HashRef

-- | How a refusal reads on a terminal (PEP-22 @hub verify@).
--
-- Here rather than in whatever prints it, and for the reason every renderer in
-- this package is: a key printed by 'show' is a short internal digest and not
-- the base58 anyone can look up, so six call sites left to their own devices
-- produce six spellings of one fact, and the operator learns which tool they
-- are using rather than what happened.
instance Pretty DropReason where
  pretty = \case
    BadAuthorSig          -> "author signature does not verify"
    BadCanonSig           -> "canon signature does not verify"
    UndecodableAuthor k w -> "author content" <+> why w <+> "from" <+> key k
    UndecodableCanon k w  -> "canon content" <+> why w <+> "from" <+> key k
    IdMismatch            -> "canon box blesses another event"
    WrongTarget           -> "authored or blessed for another repository"
    DupId                 -> "this author box is already in canon"
    UnauthorizedCanon     -> "not blessed by an authorized key"
    UnauthorizedDelegate  -> "delegate or revoke not signed by the owner key"
    BadThread             -> "reply to a thread canon does not hold"
    BadRevise             -> "revise from someone other than the author of record"
    UnknownRedact         -> "redacts an event canon does not hold"
    PROpenWithoutCoords   -> "a pull request with no coordinates"
    IssueOpenWithCoords   -> "an issue carrying pull request coordinates"
    CoordsUnreachable     -> "coordinates with nothing to fetch"
    PROnlyOnIssue         -> "a pull-request-only op on an issue"
    NumberOnNonOpen       -> "a number on something that is not an open"
    SeqAtTopOfRange       -> "seq at the top of its range"
    NumberAtTopOfRange    -> "number at the top of its range"
    FoldedTsAboveCeiling  -> "folded-ts above the ceiling canon admits"
    where
      why = \case
        Undecodable  -> "this build cannot decode"
        TrailingData -> "with bytes left over"
        WrongDomain  -> "signed as another kind of record"

instance Pretty Anomaly where
  pretty = \case
    DupSeq n            -> "two events at seq" <+> pretty n
    DupNumber n         -> "two threads numbered" <+> pretty n
    NumberWentBack a b  -> "number went from" <+> pretty a <+> "to" <+> pretty b
    FoldedTsWentBack a b -> "folded-ts went from" <+> pretty a <+> "to" <+> pretty b
    DupOrigin h         -> "two events folded from message" <+> pretty h
    PartWithoutSecret   -> "an attachment with no key published for it"
    SecretWithoutPart   -> "a key published for no attachment"
    UnusablePartSecret  -> "a part secret that cannot be a key"
    UnnormalizedAttr k  -> "attribute" <+> pretty (safeText k) <+> "is not in canonical form"

-- | One line per refusal, naming the event, where it sits, and who blessed it,
-- which is what PEP-22 requires of the report.
instance Pretty Dropped where
  pretty d = at (drSeq d) <+> pretty (drEvent d)
         <+> pretty (drWhy d) <+> by (drCanonBy d)
    where
      at = maybe "seq ?" (("seq" <+>) . pretty)
      by = maybe mempty (\k -> "(blessed by" <+> key k <> ")")

instance Pretty Anomalous where
  pretty a = "seq" <+> pretty (anSeq a) <+> pretty (anEvent a)
         <+> pretty (anWhat a) <+> "(blessed by" <+> key (anCanonBy a) <> ")"

-- Keys are printed the way anyone can look them up, which the derived Show is
-- not: it is a short internal digest.
key :: HubKey -> Doc ann
key = pretty . AsBase58

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

-- | Canon declares a version this build does not speak.
--
-- Carries the declared version so a reader can say which, and is a distinct
-- type rather than a 'DropReason' because it is not about an event: nothing
-- was folded at all.
-- Only the tree's version, deliberately. A file's own @(hub-event N)@ used to
-- veto the fold as well, taken as the maximum over the tree, which handed a
-- veto to whoever could write one file: the clause is unsigned text, so a
-- single line saying @(hub-event 4294967295)@ made a repository unreadable for
-- every clone, permanently and for free. A file this build cannot read is one
-- file, refused by the reader ("HBS2.Hub.Canon") and reported, exactly like an
-- event whose author box does not decode.
newtype CanonTooNew =
    MetaTooNew Word32   -- ^ the tree's @hub-meta@
  deriving stock (Eq,Show)

-- | Fold canon that declares its consensus version: the gate PEP-19 requires.
--
-- A reader that meets a higher @hub-meta@ than it knows must report it rather
-- than fold, because the admission rules are the format. Folding anyway would
-- produce a view that quietly disagrees with every up-to-date clone, which is
-- worse than showing nothing: the rules govern which events count.
--
-- 'foldEvents' remains for callers that already hold canon of a known version
-- (the bridge, which is minting into canon it is reading).
foldCanon
  :: Word32   -- ^ the tree's @(hub-meta N)@
  -> HubKey
  -> [Event]
  -> Either CanonTooNew FoldResult
foldCanon meta owner es
  | meta > hubMetaVersion = Left (MetaTooNew meta)
  | otherwise             = Right (foldEvents owner es)

-- | The whole fold: resolve, then materialize.
--
-- The first argument is the repo's LWWRef owner key: it is both the root of
-- trust for canon signing and the repo identity an @open@ must name.
foldEvents :: HubKey -> [Event] -> FoldResult
foldEvents owner es = materializeWith owner items unstamped
  where
    (unstamped, items) = foldr step ([],[]) es
    step e (bad,oks) = case resolve e of
      Right r     -> (bad, Whole r : oks)
      Left reason -> case ghostStamp e of
        Just (sp,num) -> (bad, Ghost (eventId e) reason sp num (canonBoxId (evCanonBox e)) : oks)
        Nothing -> ((eventId e, reason):bad, oks)

-- | The pure, seq-ordered fold over resolved events (PEP-19 admission rules
-- 3-5 + the fold body). Deterministic: total order by
-- (seq, event-id, canon-box hash).
materialize :: HubKey -> [Resolved] -> FoldResult
materialize owner rs = materializeWith owner (fmap Whole rs) []

materializeWith :: HubKey -> [Item] -> [(EventId,DropReason)] -> FoldResult
materializeWith owner rs0 pre = finish (go (sortOn sortKey rs0) st0)
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
    sortKey (Whole r)            = (rSeq r, rId r, rCanonId r)
    sortKey (Ghost eid _ sp _ cid) = (spSeq sp, eid, cid)

    st0 = S { sMaint    = HS.singleton owner
            , sEver     = HS.singleton owner
            , sThreads  = HM.empty
            , sSeen     = HM.empty
            , sRedacted = HS.empty
            -- These are the ones whose canon box did not verify or did not
            -- decode either, so there is no seq to order them by at all: after
            -- the rest, still deterministically among themselves by event-id.
            , sDropped  = [ Dropped eid Nothing Nothing reason | (eid,reason) <- pre ]
            , sMaxSeq    = 0
            , sMaxNumber = 0
            , sOrigins   = HS.empty
            , sHonoured  = HS.empty
            , sSeqSeen    = HS.empty
            , sNumbers    = HS.empty
            , sLastNumber = 0
            , sLastFolded = 0
            , sPrevFolded = 0
            , sAnoms      = []
            , sLog        = []
            , sParts      = HS.empty
            }

    -- Comments accumulate reversed (O(1) append) and are flipped here.
    finish s = FoldResult
      { frThreads  = fmap (markRedacted (sRedacted s) . unrev) (sThreads s)
      , frRedacted = sRedacted s
      -- Ordered by the whole triple: two drops sharing (seq, event-id) but
      -- differing in reason must not fall back on input order.
      , frDropped  = sortOn dropKey (sDropped s)
      , frMaxSeq    = sMaxSeq s
      , frMaxNumber = sMaxNumber s
      , frAdmitted  = sSeen s
      , frMaintainers = sMaint s
      , frOrigins   = sOrigins s
      , frHonoured  = sHonoured s
      , frOwner     = owner
      , frAnomalies = reverse (sAnoms s)
      , frLog       = reverse (sLog s)
      , frParts     = sParts s
      , frLastFolded = sLastFolded s
      }

    unrev t = t { tsComments = reverse (tsComments t) }

    go [] s = s
    -- Nothing to admit and nothing to check: this build cannot read the event.
    -- What it can still do is spend the stamp, which is the whole reason a
    -- ghost is in the ordered pass at all.
    go (Ghost eid reason sp num _ : rest) s =
      -- The number is remembered but not spent: not spending it is what keeps
      -- this build and the build that can read the event minting the same
      -- numbers, and remembering it is what makes the collision visible when
      -- one of them hands the same number out again.
      --
      -- Remembered only if the stamp counts at all, though, and that guard was
      -- missing: a number recorded from a file nobody authorized becomes a
      -- 'DupNumber' the next honest open is accused of, with the honest
      -- maintainer's key beside it. The seq has always been spelled this way;
      -- this is the number catching up.
      go rest (dropAt eid (Just (spSeq sp)) (Just (spKey sp)) reason
                 (if spendable sp s then seen num (stamp sp s) else s))
      where
        seen (Just n) t = t { sNumbers = HS.insert n (sNumbers t) }
        seen Nothing t  = t
    go (Whole r : rest) s0
      -- Before the stamp, because a canon box signed for another repository is
      -- not a blessing here at all, and the content it blesses would be dropped
      -- a moment later for a reason that says nothing about why.
      | ccTarget (rCanon r) /= owner = go rest (dropE r WrongTarget s0)
      -- Before the stamp, unlike the refusals below it. A duplicate is already
      -- in canon under the seq that admitted it, so its seq is spent there and
      -- nothing needs spending here; counting it as well is how one surviving
      -- copy of a pre-compaction file, re-stamped near the top of the range,
      -- sends the cursor to maxBound and stops the repository for good.
      | HM.member (rId r) (sSeen s0) = go rest (dropE r DupId s0)
      | Just why <- unusable r        = go rest (dropE r why s)
      | otherwise                    = go rest (apply r s)
      where
        s = stamp (stampOf r) s0

    -- The stamped values must be usable. A number belongs to an open and
    -- nothing else, and neither counter may sit at the top of its range: the
    -- next mint takes the maximum plus one, so admitting maxBound would wrap
    -- the cursor to zero and send every later event to the front of the
    -- order.
    --
    -- The counters are the top of the range only, not a monotonicity rule: a
    -- maintainer who stamps maxBound-1 still strands the cursor one mint
    -- later, at which point every open is refused as 'CursorExhausted'.
    -- Closing that needs an admission rule about what a stamp may be
    -- relative to the ones before it, and one that a partial clone can
    -- evaluate at that; PEP-19 records it as a known bound.
    unusable r
      | not numberOK          = Just NumberOnNonOpen
      | ccSeq cc == maxBound  = Just SeqAtTopOfRange
      | numberIsMax           = Just NumberAtTopOfRange
      | not foldedOK          = Just FoldedTsAboveCeiling
      | otherwise             = Nothing
      where
        cc = rCanon r
        numberOK = case (rContent r, ccNumber cc) of
          (AOpen{}, _)      -> True
          (_, Nothing)      -> True
          (_, Just _)       -> False
        numberIsMax = ccNumber cc == Just maxBound
        -- A ceiling rather than the top of the range, because the harm here is
        -- not the wrap the counters suffer: the next stamp is clamped to be no
        -- lower than this one, so 'max' carries a bad value forward from
        -- anywhere in the range, and every time the render contract shows comes
        -- from this field. See 'maxFoldedTs' for why the bound cannot be a
        -- window around the reader's own clock.
        foldedOK = ccFoldedTs cc <= maxFoldedTs

    -- Spend a stamp: raise the two high-water marks that describe a POSITION in
    -- the log, the @seq@ and the @folded-ts@. The human number is not one of
    -- them; it is spent by 'keep', on admission, and the difference is
    -- deliberate (see there).
    --
    -- Spent by anything whose canon box a key this log ever authorized signed,
    -- admitted or not, for two reasons. Admission is not final: a reply dropped
    -- as dangling is admitted as soon as the opening event it names arrives,
    -- and canon arrives a file at a time. And even where it is final, the file
    -- occupies that seq, so minting into it leaves canon holding two events at
    -- one seq for good, with every LWW attribute between them settled by a hash
    -- instead of by time.
    --
    -- Who may spend one is three-valued, and the middle case is the whole of
    -- the difficulty.
    --
    -- A key authorized right now spends its stamp outright. A key this log
    -- never authorized spends nothing: the fold is a public function over
    -- whatever files a tree holds, so counting a stranger would let anyone who
    -- can write one file strand the cursor at the top of its range.
    --
    -- A key whose delegation has been WITHDRAWN spends a stamp only next to the
    -- cursor. The case that needs it is honest and ordinary: a delegate minted
    -- from a view built before the revocation and the publisher wrote the
    -- result, which leaves a refused file at a seq that is nonetheless taken,
    -- and such a seq is always within a step or two of the cursor because it
    -- came from a cursor. Counting them without that bound made the revocation
    -- useless as a remedy: before, a mutinous delegate's @maxBound - 1@ sorted
    -- after their revoke and was ignored, and the owner had an answer; after,
    -- the answer was compaction. The window keeps the honest case and takes the
    -- mutiny back out, since a run of files can only creep the cursor by the
    -- window each time.
    stamp sp s
      | not (spendable sp s) = s
      | otherwise = s
          { sMaxSeq = if spSeq sp == maxBound
                        then sMaxSeq s
                        else max (sMaxSeq s) (spSeq sp)
          }

    -- One predicate, because two callers ask it and one of them (the ghost
    -- path, which also records the number) got it wrong by asking differently.
    spendable sp s
      -- This repository's blessing, before anything about the key. A maintainer
      -- of two repositories is authorized in both, so their stamp from the
      -- other one would otherwise pass every check below it.
      | spTarget sp /= owner            = False
      | HS.member (spKey sp) (sMaint s) = True
      | HS.member (spKey sp) (sEver s)  = spSeq sp <= sMaxSeq s + staleStampWindow
      | otherwise                       = False

    -- A resolved event's own stamp.
    stampOf r = Stamp
      { spSeq    = rSeq r
      , spKey    = rCanonKey r
      , spTarget = ccTarget (rCanon r)
      }

    apply r s = case rContent r of

      ADelegate target k _
        -- The repo binding, exactly as on an open: an author box is signed for
        -- one repository, so a delegation cannot be lifted out of another one's
        -- canon and replayed here, which an owner of two repos would otherwise
        -- sign for both at once.
        | target /= owner -> dropE r WrongTarget s
        | ownerOnly r -> keep repoScope r s { sMaint = HS.insert k (sMaint s)
                                            , sEver  = HS.insert k (sEver s)
                                            }
        | otherwise   -> dropE r UnauthorizedDelegate s

      -- Revoking the owner key is a no-op: it is the root of trust and
      -- cannot be delegated away, so it cannot be withdrawn either.
      ARevoke target k _
        | target /= owner   -> dropE r WrongTarget s
        | not (ownerOnly r) -> dropE r UnauthorizedDelegate s
        | k == owner        -> keep repoScope r s
        | otherwise         -> keep repoScope r s { sMaint = HS.delete k (sMaint s) }

      -- Owner-authored (PEP-19 rule 4): BOTH boxes must be an authorized
      -- key. Checking only the canon box would let anyone author a redact
      -- and hide any event in the repo as soon as it got blessed.
      ARedact repo target _
        | repo /= owner              -> dropE r WrongTarget s
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
        | kind == HubPR && isNothing mpr -> dropE r PROpenWithoutCoords s
        | kind == HubIssue && isJust mpr -> dropE r IssueOpenWithCoords s
        | maybe False (not . reachableCoords) mpr -> dropE r CoordsUnreachable s
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
                      , tsRedacted = False
                      , tsPR = if kind == HubPR
                                 then fmap (\c -> PRState c (ccPartSecret (rCanon r)) Nothing
                                                    (rAuthorKey r) (rCanonKey r)) mpr
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
        if tsKind t /= HubPR then Left PROnlyOnIssue
        else if not (reachableCoords coords) then Left CoordsUnreachable
        else if rAuthorKey r == tsAuthor t || HS.member (rAuthorKey r) (sMaint s)
          -- The new coordinates come with the secret for their own bundle.
          then Right (touch r t { tsPR = Just (PRState coords (ccPartSecret (rCanon r))
                                                      (tsPR t >>= psMerge)
                                                      (rAuthorKey r) (rCanonKey r)) })
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
          _ -> Left PROnlyOnIssue

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
    -- Every key this log has EVER authorized, which only grows. Used for one
    -- thing, spending stamps, and separate from 'sMaint' because a withdrawn
    -- delegation must stop an event being ADMITTED without also making the
    -- seq it occupies available again (see 'stamp').
  , sEver     :: HashSet HubKey
  , sThreads  :: HashMap ThreadId ThreadState
  , sSeen     :: HashMap EventId (Maybe ThreadId)
  , sRedacted :: HashSet EventId
  , sDropped  :: [Dropped]
  , sMaxSeq    :: Word64
  , sMaxNumber :: Word64
  , sOrigins   :: HashSet HashRef
  , sHonoured  :: HashSet EventId
    -- Anomaly bookkeeping: what has been stamped already, and the last
    -- values seen, so a counter going backwards is visible.
  , sSeqSeen    :: HashSet Word64
  , sNumbers    :: HashSet Word64
  , sLastNumber :: Word64
  , sLastFolded :: Word64
    -- The folded-ts of the previous ADMITTED event, which is a different
    -- question from the high-water mark above and needs its own field: one is
    -- the floor the next mint is clamped to, the other is what a clock going
    -- backwards is measured against. Sharing them made an event that was not
    -- admitted raise the bar for the anomaly, so a strictly increasing log of
    -- admitted events reported itself as going backwards.
  , sPrevFolded :: Word64
    -- Newest first while accumulating. No sort key alongside, unlike
    -- 'sDropped': the pass runs in (seq, event-id, canon-box hash) order and
    -- appends, so reversing at the end is already the log's own order. A drop
    -- can happen before that order is established, which is why that one
    -- carries its seq.
  , sAnoms      :: [Anomalous]
    -- Newest first while accumulating, like the anomalies and for the same
    -- reason: the pass runs in the order the log itself has.
  , sLog        :: [LogEntry]
  , sParts      :: HashSet HashRef
  }

-- | How far past the cursor a stamp from a withdrawn delegation may still
-- reach. Small on purpose: a mint from a stale view sits a step or two behind
-- the cursor, never a leap, so every file such a key writes can creep the
-- cursor by at most this much and a revocation stays a remedy.
staleStampWindow :: Word64
staleStampWindow = 16

-- Mark an event applied: the dedup set, the id every later redact resolves
-- against, which thread it belongs to, and the provenance a later fold reads.
keep :: Maybe ThreadId -> Resolved -> St -> St
keep scope r s = s
  { sSeen      = HM.insert (rId r) scope (sSeen s)
    -- The @seq@ is advanced by 'stamp' before this, for events that were not
    -- admitted too, because it is a position in the log and the file occupies
    -- it either way. Everything below is advanced only here, on admission.
    --
    -- The origin, because an event that was not admitted folded no letter.
    --
    -- The human number, because it is not a position but a label on a thread
    -- that exists: an @open@ aimed at another repository, blessed by a
    -- maintainer of this one, is refused here and never showed anyone a number,
    -- so spending it would burn one for nothing, and at the top of the range it
    -- would strand the counter and abort every later triage run. A number
    -- handed out twice is a reported anomaly on a display field; a number that
    -- cannot be handed out at all stops the repo.
    --
    -- And the clock, for the same shape of reason: it is the floor the next
    -- mint is clamped to, so a refused file stamped at the ceiling would pin
    -- every future stamp in the repository to the year 2100. A 'max' rather
    -- than an assignment, because the floor is the highest thing canon holds
    -- and not the last thing it happens to hold: assigning let an admitted
    -- event with a lower stamp lower the floor, and a folder that rebuilt from
    -- canon after a restart then disagreed with the one that had been running.
  , sOrigins   = maybe (sOrigins s) (\o -> HS.insert o (sOrigins s)) (ccOrigin (rCanon r))
  , sHonoured  = maybe (sHonoured s) (\o -> HS.insert o (sHonoured s)) (ccHonours (rCanon r))
  , sSeqSeen   = HS.insert (rSeq r) (sSeqSeen s)
  , sNumbers   = maybe (sNumbers s) (\n -> HS.insert n (sNumbers s)) num
  , sLastNumber = fromMaybe (sLastNumber s) num
  , sMaxNumber = case num of
      Just n | n /= maxBound -> max (sMaxNumber s) n
      _                      -> sMaxNumber s
  , sLastFolded = if folded > maxFoldedTs then sLastFolded s
                                          else max (sLastFolded s) folded
  , sPrevFolded = folded
  , sParts     = foldr HS.insert (sParts s) (eventParts (rContent r))
  , sLog       = LogEntry { lgSeq = rSeq r, lgEvent = rId r, lgThread = scope
                          , lgAuthor = rAuthorKey r, lgCanonBy = rCanonKey r
                          , lgFoldedTs = folded, lgOrigin = ccOrigin cc
                          , lgContent = rContent r
                          } : sLog s
  , sAnoms     = [ Anomalous (rId r) (rSeq r) (rCanonKey r) a | a <- reverse anoms ] <> sAnoms s
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
      , [ FoldedTsWentBack (sPrevFolded s) folded | folded < sPrevFolded s ]
      , [ DupOrigin o | Just o <- [ccOrigin cc], HS.member o (sOrigins s) ]
      , [ PartWithoutSecret
        | referencesPart (rContent r), isNothing (ccPartSecret cc) ]
      , [ SecretWithoutPart
        | not (referencesPart (rContent r)), isJust (ccPartSecret cc) ]
      , [ UnusablePartSecret
        | Just sec <- [ccPartSecret cc], not (usablePartSecret sec) ]
      , [ UnnormalizedAttr k
        | ASet _ k v _ <- [rContent r], not (normalizedAttr k v) ]
      ]

-- A thread whose own opening event was redacted. Applied at the end rather
-- than when the redact is admitted, because a redact may name an event that
-- is not in a thread at all, and because the fold is one pass: the redact of
-- an open is the same shape as any other.
markRedacted :: HashSet EventId -> ThreadState -> ThreadState
markRedacted red t = t
  { tsRedacted = HS.member (tsId t) red
  , tsComments = [ c { cRedacted = HS.member (cId c) red } | c <- tsComments t ]
  }

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
  -- Spelled out rather than caught by a wildcard: a new constructor with an
  -- attachment must not silently arrive here as one without. A wildcard is
  -- what makes the module's own -Werror=incomplete-patterns blind to it, and
  -- the event would then be minted with no secret required and no tree held.
  ASet{}                        -> []
  AClose{}                      -> []
  AReopen{}                     -> []
  AMerge{}                      -> []
  ARedact{}                     -> []
  ADelegate{}                   -> []
  ARevoke{}                     -> []
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
dropE r = dropAt (rId r) (Just (rSeq r)) (Just (rCanonKey r))

-- The provenance is passed in rather than taken from an event, because a ghost
-- has no event to take it from and an unreadable canon box has neither half.
dropAt :: EventId -> Maybe Word64 -> Maybe HubKey -> DropReason -> St -> St
dropAt eid sq by reason s = s { sDropped = Dropped eid sq by reason : sDropped s }

-- Ordered by the whole triple, so that two drops sharing an event-id and a seq
-- but differing in reason do not fall back on input order. An event with no
-- readable canon box has no seq to sort by and goes after the rest.
dropKey :: Dropped -> (Word64, EventId, DropReason)
dropKey d = (fromMaybe maxBound (drSeq d), drEvent d, drWhy d)

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
    -- Filled in at the end: a redact may arrive after the comment it hides.
  , cRedacted = False
  }
