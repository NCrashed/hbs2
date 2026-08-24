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
  , readableHere
  , CanonTooNew(..)
  , Admitted(..)
  , redactable
  , reachableCoords
  , openTrouble
  , threadOpTrouble
  , Anomaly(..)
  , eventParts
  , eventPartRefs
  , metaVersionFor
  , seqStampWindow
  , numberStampWindow
  , referencesPart
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Canon (MetaVersions(..))

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef)
import HBS2.Prelude.Plated (Doc,Pretty(..),(<+>))

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (sortOn)
import Data.List qualified as List
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
    -- | A @number@ more than 'numberStampWindow' above the highest one canon
    -- holds. Numbers are dense (compaction never drops an @open@), so a leap is
    -- not something an honest cursor produces, and the counter it would strand
    -- is the cheap one to strand.
  | NumberTooFarAhead
  | FoldedTsAboveCeiling  -- ^ past 'maxFoldedTs'
    -- | The canon box publishes a part-secret the author never proved they
    -- knew (PEP-18 'PartRef'). A drop and not an anomaly: an event whose
    -- attachment is somebody else's is not this thread's event, and admitting
    -- it would show a thief's issue in every clone's tracker beside the secret
    -- it was sent to publish. The bytes are in the tree either way, which is
    -- why the check that matters is the one the bridge makes before minting;
    -- this is the half every clone can make for itself.
  | PartNotProven
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
    -- | An admitted event whose @seq@ is more than 'seqStampWindow' above the
    -- mark below it, carrying the mark and the seq. The event is kept and the
    -- mark did not follow it, so this is the ONLY trace of it: without this,
    -- the rule that keeps a canon box from stranding the cursor would work in
    -- silence, and PEP-19's promise that the situation is noticed before the
    -- counter runs out would stay unkept.
    --
    -- Two honest readings besides the hostile one, and a report has to allow
    -- for both: a tree that lost files, and a compaction that dropped more
    -- consecutive events between two survivors than the window allows.
  | SeqTooFarAhead Word64 Word64
    -- | @folded-ts@ decreasing as @seq@ increases. Load-bearing, since the
    -- render contract's times come from it, and asserted by whoever signs the
    -- canon box (PEP-22).
  | FoldedTsWentBack Word64 Word64
    -- | Two events folded from one Mailbox message. PEP-19 allows exactly one
    -- event per letter, so this is either a bug or two folders racing.
  | DupOrigin HashRef
    -- | Two admitted events carrying out the SAME request. An origin is a
    -- message hash and a message can be rewrapped, so the same request under
    -- two envelopes has two origins and one honours-id: DupOrigin cannot see
    -- it, and the bridge's AlreadyHonoured gate only protects a folder that
    -- already has the first event in its view -- which is exactly the case that
    -- did not happen when two maintainers honoured it at once, or when one
    -- crashed between minting and publishing.
  | DupHonours HashRef
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
    -- | Every admitted event and what canon remembers about it.
    --
    -- The fold knows this exactly; reconstructing it from 'frThreads' would
    -- miss every event that leaves no visible trace (a @set@, a @merge@, a
    -- note-less @close@), which is enough to break both dedup and the layout
    -- a writer needs (PEP-19 puts thread events under @threads/@ and the
    -- rest under @repo/@).
  , frAdmitted  :: HashMap EventId Admitted
    -- | What the TREE declared as its rules version, absent when it had no
    -- @version@ file.
    --
    -- Carried rather than collapsed, because it is what a writer has to compare
    -- against: a commit must never lower the version, and "the tree said
    -- nothing" is not "the tree said 1".
  , frMeta      :: Maybe Word32
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
    NumberTooFarAhead     -> "number too far above the ones canon holds"
    FoldedTsAboveCeiling  -> "folded-ts above the ceiling canon admits"
    PartNotProven         -> "a part-secret its author never proved they knew"
    where
      why = \case
        Undecodable  -> "this build cannot decode"
        TrailingData -> "with bytes left over"
        WrongDomain  -> "signed as another kind of record"

-- 'hashDoc' used to live here, next to its first caller, and stayed private to
-- this module. That is why the triage renderer next door printed a stranger's
-- thread-id with a bare 'pretty': the guard existed, was documented, was tested,
-- and was not reachable from the second place that needed it. It is in
-- "HBS2.Hub.Types" now, beside 'validHashRef', with 'keyDoc' for the same reason.

instance Pretty Anomaly where
  pretty = \case
    DupSeq n            -> "two events at seq" <+> pretty n
    DupNumber n         -> "two threads numbered" <+> pretty n
    NumberWentBack a b  -> "number went from" <+> pretty a <+> "to" <+> pretty b
    SeqTooFarAhead a b  ->
      "seq jumped from" <+> pretty a <+> "to" <+> pretty b
        <+> "(the mark did not follow; canon lost files, was compacted"
        <+> "hard, or somebody stamped past the log)"
    FoldedTsWentBack a b -> "folded-ts went from" <+> pretty a <+> "to" <+> pretty b
    DupOrigin h         -> "two events folded from message" <+> hashDoc h
    DupHonours h        -> "two events carrying out request" <+> hashDoc h
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
    MetaTooNew Word32   -- ^ the tree's @hub-min@: the lowest reader it trusts
  deriving stock (Eq,Show)

-- | May a reader at this build's version fold a tree that declares these?
--
-- THE FLOOR AND NOT THE RULES VERSION, which is what the two-number scheme is
-- for and what this got wrong for as long as the scheme existed. @(hub-meta N)@
-- says what rules the tree was written under; @(hub-min M)@ is the writer
-- saying that a reader at M is still SOUND about it, which is a claim only the
-- writer can make -- it knows whether its bump changed an outcome for events an
-- older reader decodes perfectly well, or merely added a shape that reader
-- cannot decode at all.
--
-- Gating on N instead refuses the whole tree over one event an older reader
-- cannot read, and the ghost path in 'materializeWith' -- built to spend that
-- event's seq so the numbering does not shift, and fold the rest -- could then
-- never run. The gate was moved onto M in the reader ("HBS2.Hub.Repo") and left
-- standing here, so it kept firing first and the move changed nothing: a tree
-- at @(hub-meta 2) (hub-min 1)@ was refused by a build at 1, which is the exact
-- case M exists to admit.
--
-- One function because there are two call sites: the reader checks it before
-- fetching blobs, so an unreadable tree costs one file rather than one cat-file
-- per event, and 'foldCanon' checks it for a caller that did not.
readableHere :: MetaVersions -> Maybe CanonTooNew
readableHere mv | mvMin mv > hubMetaVersion = Just (MetaTooNew (mvMin mv))
                | otherwise                 = Nothing

-- | Fold canon that declares its consensus version: the gate PEP-19 requires.
--
-- A reader below the tree's floor must report it rather than fold, because
-- below it the rules govern which events count and it would produce a view that
-- quietly disagrees with every up-to-date clone. Above the floor and below the
-- rules version it folds, ghosting what it cannot decode: see 'readableHere'.
--
-- 'foldEvents' remains for callers that already hold canon of a known version
-- (the bridge, which is minting into canon it is reading).
foldCanon
  :: MetaVersions   -- ^ the tree's @(hub-meta N)@ and @(hub-min M)@
  -> HubKey
  -> [Event]
  -> Either CanonTooNew FoldResult
foldCanon mv owner es =
  maybe (Right (foldEvents owner es)) Left (readableHere mv)

-- | The whole fold: resolve, then materialize.
--
-- The first argument is the repo's LWWRef owner key: it is both the root of
-- trust for canon signing and the repo identity an @open@ must name.
foldEvents :: HubKey -> [Event] -> FoldResult
foldEvents owner es = materializeWith owner items unstamped
  where
    -- foldl' and a reversal, not foldr. The step pattern-matches its
    -- accumulator, so the foldr was strict in it and built the whole spine on
    -- the stack before returning anything -- over a canon bounded by
    -- 'maxCanonFiles' files rather than by anything this fold chooses. The
    -- reversal restores the input order exactly, so nothing downstream can tell:
    -- both lists are consumed whole and one of them is sorted.
    (unstamped, items) = swapEnds (List.foldl' step ([],[]) es)
    swapEnds (a,b) = (reverse a, reverse b)
    step (bad,oks) e = case resolve e of
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
            , sSeqFloor   = 0
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
        -- Nothing: this is the fold of a LIST of events, which has no tree and
        -- so no declaration. `readCanonAt` fills it in from the file.
      , frMeta      = Nothing
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
                 (if spendable sp s then noted (seen num (stamp sp s)) else s))
      where
        -- The SEQ is remembered as well as the number, and it was not. The two
        -- halves of a stamp were treated alike everywhere except here: a ghost
        -- spent its seq and left no trace of it, so a genuine collision -- a
        -- maintainer on a newer build minting at seq N, an owner on this one
        -- minting at N because its cursor had not seen that event -- was
        -- invisible to `hub verify` on THIS build, the one that can see it from
        -- the stamp alone, while the build that can read the event reports it.
        -- Anomaly bookkeeping only: sSeqSeen feeds nothing but the report.
        seen mn t = t { sNumbers = maybe (sNumbers t) (`HS.insert` sNumbers t) mn
                      , sSeqSeen = HS.insert (spSeq sp) (sSeqSeen t)
                      }

        -- And said from here too, so that the collision is reported whichever
        -- of the two sorts first: the order between them is by event-id, which
        -- is to say arbitrary, and an anomaly that depends on it is one that
        -- shows up in half the clones.
        noted t
          | HS.member (spSeq sp) (sSeqSeen s) =
              t { sAnoms = Anomalous eid (spSeq sp) (spKey sp) (DupSeq (spSeq sp))
                             : sAnoms t }
          | otherwise = t
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
      -- After 'unusable', so that a number on something that is not an @open@,
      -- or one above the ceiling, still reports the deeper fact. Here rather
      -- than inside it because this is the one stamp rule that is relative to
      -- the log so far, and 'unusable' is a function of the event alone.
      | numberTooFar                 = go rest (dropE r NumberTooFarAhead s)
      | otherwise                    = go rest (apply r s)
      where
        s = stamp (stampOf r) s0
        -- Against the mark BEFORE this event, like every other stamp rule: the
        -- question is what the log could reach, and the log is what came below.
        numberTooFar = case ccNumber (rCanon r) of
          Just n  -> not (withinWindow numberStampWindow (sMaxNumber s0) n)
          Nothing -> False

    -- The stamped values must be usable. A number belongs to an open and
    -- nothing else, and neither counter may sit at the top of its range: the
    -- next mint takes the maximum plus one, so admitting maxBound would wrap
    -- the cursor to zero and send every later event to the front of the
    -- order.
    --
    -- The counters here are the top of the range ONLY. What a stamp may be
    -- relative to the ones below it is a separate rule and a separate place:
    -- the seq window lives in 'spendable' (out of window is admitted and does
    -- not move the mark) and the number window is the guard below this one (out
    -- of window is refused). Both are rules of @hub-meta 1@; before they existed
    -- this comment said the gap was a bound PEP-19 recorded and nothing closed.
    unusable r
      | not numberOK          = Just NumberOnNonOpen
      | ccSeq cc == maxBound  = Just SeqAtTopOfRange
      | numberIsMax           = Just NumberAtTopOfRange
      | not foldedOK          = Just FoldedTsAboveCeiling
      | not partsProven       = Just PartNotProven
      | otherwise             = Nothing
      where
        cc = rCanon r
        -- Only when a secret was actually published: an event that names a part
        -- and carries no key for it publishes nothing, and says so as
        -- 'PartWithoutSecret'. An UNUSABLE secret keeps its own anomaly and its
        -- admission too, because a key of the wrong length opens nothing and
        -- dropping the event for it would hide an intact event over a broken
        -- field.
        partsProven = case ccPartSecret cc of
          Just sec | usablePartSecret sec ->
            all (\p -> provesPart p sec (rAuthorKey r)) (eventPartRefs (rContent r))
          _ -> True
        numberOK = case (rContent r, ccNumber cc) of
          (AOpen{}, _)      -> True
          (_, Nothing)      -> True
          (_, Just _)       -> False
        -- ABOVE THE CEILING, not only at the top of the range. See
        -- 'maxCanonNumber': the render contract emits this as a bare JSON
        -- integer and names a web layer as its reader, and a double cannot hold
        -- the top of a Word64. The name is kept because the drop is the same
        -- fact to whoever reads a report: this number is not one canon may carry.
        numberIsMax = maybe False (> maxCanonNumber) (ccNumber cc)
        -- A ceiling rather than the top of the range, because the harm here is
        -- not the wrap the counters suffer: the next stamp is clamped to be no
        -- lower than this one, so 'max' carries a bad value forward from
        -- anywhere in the range, and every time the render contract shows comes
        -- from this field. See 'maxFoldedTs' for why the bound cannot be a
        -- window around the reader's own clock.
        foldedOK = ccFoldedTs cc <= maxFoldedTs

    -- Spend a stamp: raise the @seq@ high-water mark, which is the one thing
    -- here that describes a POSITION in the log.
    --
    -- NOT the clock and NOT the number. Both are spent by 'keep', on
    -- ADMISSION, and both differences are deliberate and load-bearing --
    -- 'keep' argues the clock one at length, and the short version is that a
    -- refused file stamped at the ceiling would otherwise pin every future
    -- stamp in this repository to the year 2100. This comment used to say that
    -- a stamp raises the folded-ts too, which is what a reader would act on:
    -- 'Stamp' carries no such field, and adding one would reintroduce exactly
    -- that.
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
    --
    -- AND SO DOES A KEY AUTHORIZED RIGHT NOW, which is the whole of what the
    -- stamp window changed. This branch used to answer 'True' outright, and the
    -- window above was reasoned about as a remedy for a delegation somebody had
    -- already withdrawn -- which is the case where the owner has ALREADY
    -- noticed. A live delegate needed one file at @maxBound - 1@ to strand the
    -- cursor for good, and the entry point the owner would answer it with is
    -- gated on the same cursor. The two windows differ ('seqStampWindow' is
    -- 2^24 and 'numberStampWindow' is 16) because compaction leaves gaps in one
    -- and cannot in the other; that they exist at all no longer depends on
    -- whether the key is still trusted, because a bound that only applies after
    -- the harm is noticed is not a bound.
    stamp sp s
      | not (spendable sp s) = floored s
      | otherwise = (floored s)
          { sMaxSeq = if spSeq sp == maxBound
                        then sMaxSeq s
                        else max (sMaxSeq s) (spSeq sp)
          }
      where
        -- What the mark was BEFORE this event, kept for the one reader that
        -- needs it: 'keep' runs after the stamp and so cannot ask. Set on both
        -- branches, since the anomaly it feeds is precisely about the branch
        -- that did not spend.
        floored t = t { sSeqFloor = sMaxSeq s }

    -- One predicate, because two callers ask it and one of them (the ghost
    -- path, which also records the number) got it wrong by asking differently.
    spendable sp s
      -- This repository's blessing, before anything about the key. A maintainer
      -- of two repositories is authorized in both, so their stamp from the
      -- other one would otherwise pass every check below it.
      | spTarget sp /= owner           = False
      | HS.member (spKey sp) (sEver s) = withinWindow seqStampWindow (sMaxSeq s) (spSeq sp)
      | otherwise                      = False

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
        | not (bothAuthorized r s)    -> dropE r UnauthorizedCanon s
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
        -- Kind and payload must agree; 'openTrouble' says how, for this and
        -- for the two places in the bridge that must refuse the same shapes.
        | Just why <- openTrouble kind mpr -> dropE r why s
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
                      , tsBodyPart = fmap ptPart bodypart
                      , tsPartSecret = ccPartSecret (rCanon r)
                      , tsOrigin = ccOrigin (rCanon r)
                      }
            in keep (Just (rId r)) r s { sThreads = HM.insert (rId r) t (sThreads s) }

      AComment thr replyto body bodypart ts -> onThread r thr s $ \t ->
        touch r t { tsComments = mkComment r replyto ts body (fmap ptPart bodypart) : tsComments t }

      ARevise thr coords _ts -> onThreadWith r thr s $ \t ->
        case threadOpTrouble (tsKind t) (rContent r) of
          Just why -> Left why
          Nothing
            | rAuthorKey r == tsAuthor t || HS.member (rAuthorKey r) (sMaint s) ->
                -- The new coordinates come with the secret for their own bundle.
                Right (touch r t { tsPR = Just (PRState coords (ccPartSecret (rCanon r))
                                                        (tsPR t >>= psMerge)
                                                        (rAuthorKey r) (rCanonKey r)) })
            | otherwise -> Left BadRevise

      ASet thr k v _ts -> onOwnerThread r thr s $ \t ->
        touch r t { tsAttrs = HM.insert k v (tsAttrs t) }

      AClose thr note ts -> onOwnerThread r thr s $ \t ->
        addNote note r ts $ touch r t { tsAttrs = HM.insert "status" "closed" (tsAttrs t) }

      AReopen thr note ts -> onOwnerThread r thr s $ \t ->
        addNote note r ts $ touch r t { tsAttrs = HM.insert "status" "open" (tsAttrs t) }

      AMerge thr mc into _ts -> onOwnerThreadWith r thr s $ \t ->
        case (threadOpTrouble (tsKind t) (rContent r), tsPR t) of
          (Just why, _) -> Left why
          (Nothing, Just pr) ->
            Right (touch r t { tsPR = Just pr { psMerge = Just (mc,into) }
                             , tsAttrs = HM.insert "status" "merged" (tsAttrs t) })
          -- A pr thread with no coordinates, which 'openTrouble' does not admit
          -- and nothing else creates. Answered as it always was rather than
          -- left to a pattern-match failure inside the fold.
          (Nothing, Nothing) -> Left PROnlyOnIssue

    foldedTs = ccFoldedTs . rCanon

    touch r t = t { tsUpdated = max (tsUpdated t) (foldedTs r) }

    -- rule 5: delegate/revoke require BOTH author and canon-by == owner key.
    ownerOnly r = rAuthorKey r == owner && rCanonKey r == owner

    -- rules 3+4: canon-by must be an authorized canon key; for owner-authored
    -- ops the author box signer must be too. For open/comment only canon-by
    -- must be authorized (the author may be anyone).
    --
    -- NAMED FOR WHAT IT CHECKS and not for the op class that asks it. It used
    -- to be `ownerAuthored`, which is the PEP-19 name for the ops -- and reads
    -- as "the owner authored this", which is not what it says: either box may
    -- be any authorized key. Rule 5 is the one that means the owner, and it is
    -- `ownerOnly` above.
    canonOK r s = HS.member (rCanonKey r) (sMaint s)
    bothAuthorized r s = canonOK r s && HS.member (rAuthorKey r) (sMaint s)

    onThread r thr s f = withThread canonOK r thr s (Right . f)
    onOwnerThread r thr s f = withThread bothAuthorized r thr s (Right . f)
    onThreadWith r thr s f = withThread canonOK r thr s f
    onOwnerThreadWith r thr s f = withThread bothAuthorized r thr s f

    withThread auth r thr s f
      | not (auth r s) = dropE r UnauthorizedCanon s
      | otherwise = case HM.lookup thr (sThreads s) of
          Nothing -> dropE r BadThread s
          Just t  -> case f t of
                       Left reason -> dropE r reason s
                       Right t'    -> keep (Just thr) r s { sThreads = HM.insert thr t' (sThreads s) }

-- Internal accumulator.
-- | The accumulator of the ordered pass.
--
-- EVERY FIELD IS STRICT, and that is not a micro-optimisation. This record is
-- rebuilt once per event and only two of its fields are ever forced during the
-- pass, so a lazy one accumulated a thunk per event that CLOSED OVER THE WHOLE
-- PREVIOUS RECORD: `sAnoms` is built from an expression mentioning `s`, which
-- is an application thunk rather than a selector, so every superseded version
-- of every HAMT in here was retained instead of dying. Peak residency went as
-- O(N log N) in the number of events rather than O(N), for a fold nobody asked
-- for the anomalies of.
data St = S
  { sMaint    :: !(HashSet HubKey)
    -- Every key this log has EVER authorized, which only grows. Used for one
    -- thing, spending stamps, and separate from 'sMaint' because a withdrawn
    -- delegation must stop an event being ADMITTED without also making the
    -- seq it occupies available again (see 'stamp').
  , sEver     :: !(HashSet HubKey)
  , sThreads  :: !(HashMap ThreadId ThreadState)
  , sSeen     :: !(HashMap EventId Admitted)
  , sRedacted :: !(HashSet EventId)
  , sDropped  :: ![Dropped]
  , sMaxSeq    :: !Word64
  , sMaxNumber :: !Word64
  , sOrigins   :: !(HashSet HashRef)
  , sHonoured  :: !(HashSet EventId)
    -- Anomaly bookkeeping: what has been stamped already, and the last
    -- values seen, so a counter going backwards is visible.
  , sSeqSeen    :: !(HashSet Word64)
  , sNumbers    :: !(HashSet Word64)
  , sLastNumber :: !Word64
  , sLastFolded :: !Word64
    -- The folded-ts of the previous ADMITTED event, which is a different
    -- question from the high-water mark above and needs its own field: one is
    -- the floor the next mint is clamped to, the other is what a clock going
    -- backwards is measured against. Sharing them made an event that was not
    -- admitted raise the bar for the anomaly, so a strictly increasing log of
    -- admitted events reported itself as going backwards.
  , sPrevFolded :: !Word64
    -- The @seq@ high-water mark as it stood BEFORE the current event's stamp,
    -- so that 'keep' can say how far that event reached. A field for the same
    -- reason 'sPrevFolded' is one: the value is gone by the time anything wants
    -- to compare against it, and recomputing it would mean a second pass.
  , sSeqFloor   :: !Word64
    -- Newest first while accumulating. No sort key alongside, unlike
    -- 'sDropped': the pass runs in (seq, event-id, canon-box hash) order and
    -- appends, so reversing at the end is already the log's own order. A drop
    -- can happen before that order is established, which is why that one
    -- carries its seq.
  , sAnoms      :: ![Anomalous]
    -- Newest first while accumulating, like the anomalies and for the same
    -- reason: the pass runs in the order the log itself has.
  , sLog        :: ![LogEntry]
  , sParts      :: !(HashSet HashRef)
  }

-- | How far above the log's own high-water mark a canon box may stamp its
-- @seq@ and still move it (PEP-19).
--
-- WHY THERE IS A BOUND AT ALL. The cursor is @frMaxSeq + 1@, so a single file
-- stamped near the top of the range strands it: every later mint is refused as
-- @CursorExhausted@, including the @revoke@ that would answer the key that
-- wrote it. Bounding the step turns that from one file into
-- @2^64 / seqStampWindow@ = 2^40 of them, each one an object the publisher has
-- to write and every reader has to fold -- at which point flooding canon is the
-- cheaper attack and the cursor is no longer the weak link.
--
-- WHY IT IS THIS LARGE, when the honest gap is 1. Compaction drops superseded
-- @set@-class events out of the tree and keeps their seqs spent nowhere, so the
-- survivors are sparse, and the gap between two of them is however many events
-- were dropped between them. A tight window would refuse a legitimately
-- compacted canon, which is a repository bricked by its own maintenance -- the
-- worse failure of the two. 16 million consecutive superseded events between
-- two survivors is past what a tracker holds; 2^40 files is past what anyone
-- writes.
--
-- OUT OF WINDOW IS NOT A DROP, deliberately. The event is admitted and its seq
-- simply does not move the mark, so a compacted tree that somehow does exceed
-- this still folds, and the only cost is that the cursor may later hand out a
-- seq that file already holds -- a 'DupSeq', which the fold reports and orders
-- deterministically. Refusing instead would trade a reported collision for an
-- unreadable repository. What makes it visible is 'SeqTooFarAhead'.
seqStampWindow :: Word64
seqStampWindow = 2 ^ (24 :: Int)

-- | And the same for the human @number@, which is tight because it can be.
--
-- Numbers are handed out one per @open@ and compaction never drops an @open@
-- ('attrOf' answers 'Nothing' for them), so the surviving numbers are dense in
-- a way seqs are not and no legitimate gap exists to make room for. A mint from
-- a stale view sits a step or two behind the cursor, never a leap.
--
-- THIS ONE IS A DROP, and the asymmetry is the point: a number is a label a
-- reader is shown, so an @open@ carrying one the fold will not honour should be
-- refused rather than kept with a label nothing else agrees with. There is no
-- compaction case to spare, so nothing is lost by refusing. And it is the
-- cheaper counter to strand of the two: 'maxCanonNumber' is 2^53-1, which one
-- admitted @open@ could reach.
numberStampWindow :: Word64
numberStampWindow = 16

-- | Is @hi@ within @w@ of @lo@, where being below @lo@ always is.
--
-- SUBTRACTION RATHER THAN @lo + w@, because the addition overflows: a mark near
-- the top of the range would wrap and then refuse every stamp beneath it, which
-- is the failure this whole rule exists to prevent, arriving through the rule
-- itself. Canon written by an older build can hold such a mark, and this build
-- has to fold it rather than seize.
withinWindow :: Word64 -> Word64 -> Word64 -> Bool
withinWindow w lo hi = hi <= lo || hi - lo <= w

-- | What canon remembers about an admitted event.
--
-- One record rather than two maps, and that is the point: 'CanonView' is a
-- cache of this, the two have diverged five times in this module's history, and
-- every one of those was a field the cache updated by its own rule. A second
-- map would be a sixth chance.
data Admitted = Admitted
  { adScope :: Maybe ThreadId
    -- ^ the thread it belongs to, or 'Nothing' for the repo-scope ops
  , adHideable :: Bool
    -- ^ would redacting it change what any reader shows? See 'redactable'.
  }
  deriving stock (Eq,Show)

-- | Does redacting this event hide anything?
--
-- WHY THIS EXISTS. 'frRedacted' is consulted in exactly one place, which sets
-- 'tsRedacted' on a thread and 'cRedacted' on its comments -- so a redact
-- naming a @revise@, a @merge@, a @set@, a note-less @close@ or @reopen@, a
-- @delegate@, a @revoke@ or another @redact@ was admitted, spent a seq, moved
-- the thread's @updated@, was counted by @hub verify@, was retained forever by
-- compaction, and changed no rendering anywhere. The verb printed an event id,
-- a seq and a commit, so the maintainer moderating an abusive revision was told
-- it worked.
--
-- Total, with no wildcard, because the answer for a new op is a decision and
-- not a default: an op whose content a reader shows must be added here in the
-- same change that makes the reader show it.
redactable :: AuthorContent -> Bool
redactable = \case
  AOpen{}     -> True   -- title, body, body-part, part-secret, labels, coords
  AComment{}  -> True   -- body, body-part, part-secret
  ARevise{}   -> False
  ASet{}      -> False
  AClose{}    -> False
  AReopen{}   -> False
  AMerge{}    -> False
  ARedact{}   -> False
  ADelegate{} -> False
  ARevoke{}   -> False

-- Mark an event applied: the dedup set, the id every later redact resolves
-- against, which thread it belongs to, and the provenance a later fold reads.
keep :: Maybe ThreadId -> Resolved -> St -> St
keep scope r s = s
  { sSeen      = HM.insert (rId r) (Admitted scope (redactable (rContent r))) (sSeen s)
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
        -- The stamp that did not move the mark. 'sSeqFloor' is what the mark
        -- was before 'stamp' ran, so this is the same comparison 'spendable'
        -- made, asked again where an anomaly can be recorded against an
        -- admitted event.
      , [ SeqTooFarAhead (sSeqFloor s) (rSeq r)
        | not (withinWindow seqStampWindow (sSeqFloor s) (rSeq r)) ]
      , [ DupNumber n | Just n <- [num], HS.member n (sNumbers s) ]
        -- Numbers are assigned in open order, so one that does not advance is
        -- either a duplicate publisher or a hand-written stamp.
      , [ NumberWentBack (sLastNumber s) n
        | Just n <- [num], sLastNumber s > 0, n <= sLastNumber s ]
      , [ FoldedTsWentBack (sPrevFolded s) folded | folded < sPrevFolded s ]
      , [ DupOrigin o | Just o <- [ccOrigin cc], HS.member o (sOrigins s) ]
      , [ DupHonours o | Just o <- [ccHonours cc], HS.member o (sHonoured s) ]
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
eventParts = fmap ptPart . eventPartRefs

-- | The same, with the proof each part carries.
--
-- Two functions rather than one, because most callers are asking which trees
-- the event points at (retention, the size gate) and exactly one is asking
-- whether the sender may publish them.
-- | The lowest @(hub-meta N)@ under which this event means what it says.
--
-- WHY THE VERSION IS A FUNCTION OF THE CONTENT and not of the build. It was
-- 'hubMetaVersion' -- a constant -- written into every canon commit by a
-- 'renderMeta' with no argument, so the FIRST accept by a newer build rewrote
-- @version@ for a canon holding no new event, and every clone on the older
-- build then refused the WHOLE tree (@CanonTooNewHere@, exit 6): not one
-- unreadable event on an otherwise usable tracker, but the five hundred issues
-- it did understand, gone, with no warning on the writing side and no way back
-- since canon is append-only.
--
-- ONE FOR EVERY SHAPE THIS BUILD WRITES, and that is the answer rather than
-- the absence of one. It used to answer 2 for an event naming a 'PartRef',
-- which was the version that shape had been given; the versions were reset to
-- 1 on 2026-08-24 ('hubMetaVersion'), so every shape this build can write is
-- readable by every reader of version 1, and this says so.
--
-- IT IS NOT DEAD MACHINERY, and deleting it would be the mistake. A tree's
-- @hub-meta@ is the maximum of what its events NEED ('planCanon'), which is
-- what stops a repository from being stamped forward by a build that merely
-- read it: without this the constant went into every commit, so the first
-- accept by a newer build rewrote @version@ for a canon holding no new event,
-- and every clone on the older build refused the WHOLE tree -- five hundred
-- issues it understood perfectly, gone, with no warning on the writing side and
-- no way back since canon is append-only. That is what the shape is for, and
-- the next shape older readers cannot decode needs it to be here already.
--
-- Take 'eventPartRefs' rather than the constructor when that day comes: an
-- @open@ with an attachment and one without are different answers, and the
-- constructor cannot tell them apart.
metaVersionFor :: AuthorContent -> Word32
metaVersionFor _ = 1

eventPartRefs :: AuthorContent -> [PartRef]
eventPartRefs = \case
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

-- | Whether an @open@'s kind agrees with what it carries, and what the fold
-- calls it when it does not.
--
-- THE ONE IMPLEMENTATION, and it was three: here, and twice in
-- "HBS2.Hub.Bridge" (the letter path and the owner-native path). Three copies
-- of a rule whose entire purpose is that the gate refuses exactly what the
-- fold would drop, so a copy that drifted breaks that in one of two ways: the
-- gate mints an event the fold throws away, burning a @seq@ and losing the
-- submission with no trace a maintainer would look at, or it refuses one canon
-- would have taken.
--
-- Written over the PAIR rather than as a chain of guards, so that a third
-- 'HubKind' is a build error in the one place that decides. As three guard
-- chains it was a build error in none of them: each kept compiling and each
-- would have answered for the new kind by falling through to @otherwise@.
openTrouble :: HubKind -> Maybe PRCoords -> Maybe DropReason
openTrouble kind mpr = case (kind, mpr) of
  -- A pr must carry its coordinates. Nothing may invent them later: an event
  -- canon admits is the whole of what its author signed.
  (HubPR,    Nothing) -> Just PROpenWithoutCoords
  -- An issue must not, or canon would silently materialize less than the
  -- author box says.
  (HubIssue, Just _)  -> Just IssueOpenWithCoords
  (HubIssue, Nothing) -> Nothing
  (HubPR,    Just c)
    | reachableCoords c -> Nothing
    | otherwise         -> Just CoordsUnreachable

-- | The same question for an op that names a thread: does the op agree with
-- the kind of thread it names?
--
-- Total over 'AuthorContent' on purpose. The bridge used to ask this through
-- two predicates that each ended in a wildcard -- in a module whose header
-- pragma exists precisely to turn an unhandled op into a build error. A new
-- op would have been swallowed by those wildcards, declared to agree with
-- every thread, and minted.
--
-- The four ops that name no thread are listed rather than left to a wildcard,
-- which is the whole point: an @open@ IS its thread, and 'ARedact',
-- 'ADelegate' and 'ARevoke' are repo-scope ('authorThread' is 'Nothing' for
-- all four). They cannot reach a caller that has a thread's kind in hand, and
-- saying so here costs four lines and keeps the check mechanical.
threadOpTrouble :: HubKind -> AuthorContent -> Maybe DropReason
threadOpTrouble kind = \case
  ARevise _ c _
    | kind /= HubPR     -> Just PROnlyOnIssue
    | reachableCoords c -> Nothing
    | otherwise         -> Just CoordsUnreachable

  AMerge{}
    | kind /= HubPR -> Just PROnlyOnIssue
    | otherwise     -> Nothing

  -- Ops any thread carries, whatever its kind.
  AComment{} -> Nothing
  ASet{}     -> Nothing
  AClose{}   -> Nothing
  AReopen{}  -> Nothing

  AOpen{}     -> Nothing
  ARedact{}   -> Nothing
  ADelegate{} -> Nothing
  ARevoke{}   -> Nothing

-- Events that belong to no thread.
repoScope :: Maybe ThreadId
repoScope = Nothing

-- Which thread an already-admitted event belongs to, for a redact naming it.
threadOfEvent :: EventId -> St -> Maybe ThreadId
threadOfEvent eid s = maybe Nothing adScope (HM.lookup eid (sSeen s))

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
