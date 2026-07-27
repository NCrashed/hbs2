-- | The triage bridge: Tier B letter to Tier A canon event (PEP-19
-- "Folding Tier B letters into Tier A").
--
-- This is the only place where a letter becomes canon, and it is a gate, not
-- a stamp. It refuses before minting, for two different reasons:
--
--   * Authority. A bridge that blessed whatever arrived would let anyone
--     author an owner-native op (a @redact@ hiding any event) and hand it to
--     the owner to sign. The deny-list is applied to the INNER author, since
--     an envelope-key ban is evaded by rewrapping (PEP-18).
--
--   * Order. The fold is a single ascending pass over @seq@, so a reply must
--     carry a higher @seq@ than the event it answers. Minting a reply whose
--     thread is not in canon yet produces an event that can never be
--     admitted: the fold drops it as dangling, and re-folding changes
--     nothing, so the submission is lost for good. The bridge therefore
--     requires the thread to exist already, which means a maintainer folds
--     an opening letter before the replies to it.
--
-- The bridge never re-signs the author box of an accepted letter. It takes
-- the inner box out verbatim and wraps an owner-signed canon box around it,
-- so the event-id the sender computed is the event-id canon ends up with,
-- and the authorship proof survives.
--
-- Everything here is pure: minting is a function of the current canon plus
-- the letter, so two folders given the same inputs mint the same events.
--
-- The property the module owes its caller is that it never mints an event
-- the fold will drop, and that holds by enumeration over 'DropReason':
--
--   * @BadAuthorSig@, @BadCanonSig@, @UndecodableAuthor@,
--     @UndecodableCanon@, @IdMismatch@: impossible for boxes minted here.
--   * @WrongTarget@, @BadKind@, @BadThread@, @UnknownRedact@, @DupId@,
--     @BadRevise@, @UnauthorizedDelegate@: refused by the gate, on both the
--     letter path and the owner-native one.
--   * @UnauthorizedCanon@: refused by checking this folder's own key against
--     the maintainer set canon reports, which is why 'CanonView' carries it.
--   * @BadStamp@: a number is minted only for an open, and the cursor is
--     refused once it reaches the top of its range ('CursorExhausted'),
--     which is the only way a stamp minted here could be out of bounds.
--
-- Anything that weakens one of those checks weakens the property, so the
-- view must come from the fold ('viewOf') rather than be reconstructed.
-- The accumulated view must also stay equal to the rebuilt one: it is a
-- cache of what the fold would say, and a divergence is a bug even where
-- nothing currently reads the field that diverged.
module HBS2.Hub.Bridge
  ( CanonCursor(..)
  , EventScope(..)
  , CanonView(..)
  , Accepted(..)
  , TriageError(..)
  , viewOf
  , emptyView
  , initialCursor
  , cursorFrom
  , authorizedCanon
  , acceptLetter
  , honourRequest
  , honourWith
  , ownerEvent
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Hub.Letter

import HBS2.Data.Types.Refs (HashRef)
import HBS2.Data.Types.SignedBox (SignedBox)
import HBS2.Net.Auth.Credentials

import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe (isJust,isNothing)
import Data.Word (Word64)

-- | What the next canon box gets stamped with.
--
-- Both counters come from canon itself ('viewOf'), not from node-local
-- state, so a folder that rebuilds from the repo mints the same values a
-- previous one would have. PEP-19 leaves uniqueness to there being a single
-- publisher; @hub verify@ reports it if that ever stops being true.
data CanonCursor = CanonCursor
  { ccrNextSeq    :: Word64
  , ccrNextNumber :: Word64
  }
  deriving stock (Eq,Show)

-- | Where an event lives in the canon tree (PEP-19 layout).
data EventScope =
    ThreadScope ThreadId  -- ^ under @threads/<thread-id>/@
  | RepoScope             -- ^ under @repo/@: delegate and revoke
  deriving stock (Eq,Show)

-- | As much of canon as the bridge needs to decide before minting: what to
-- stamp, which threads a reply may name, and which events already exist.
data CanonView = CanonView
  { cvCursor  :: CanonCursor
    -- | Thread id to (kind, author of record). The author is needed because
    -- only they may revise a PR; the kind, because revise is PR-only.
  , cvThreads :: HashMap ThreadId (HubKind, HubKey)
    -- | Every admitted event and the thread it belongs to, taken straight
    -- from the fold. Reconstructing it from the materialized threads would
    -- silently omit every event that leaves no visible trace (a @set@, a
    -- @merge@, a note-less @close@), so a redact of one would be refused and
    -- a resent revise would not be caught as a duplicate. That divergence
    -- only appears after a restart, when the view is rebuilt from canon.
  , cvEvents  :: HashMap EventId (Maybe ThreadId)
    -- | The canon keys still authorized at the end of the log. A folder must
    -- check its OWN key against this: a maintainer whose delegation has been
    -- revoked can still sign, but the fold will not admit what they sign, so
    -- minting would consume a triaged letter and produce nothing.
  , cvMaintainers :: HashSet HubKey
  }
  deriving stock (Eq,Show)

-- | The view of an empty repo.
--
-- Seeded with the owner exactly as the fold seeds its own maintainer set, so
-- that a view carried forward across accepts stays equal to one rebuilt with
-- 'viewOf' from the same canon. Any divergence between the two is a bug: the
-- accumulated view is a cache of what the fold would say.
emptyView :: RepoRef -> CanonView
emptyView repo = CanonView initialCursor HM.empty HM.empty (HS.singleton repo)

-- | May this key bless events for this repo right now?
authorizedCanon :: RepoRef -> CanonView -> HubKey -> Bool
authorizedCanon repo view k = k == repo || HS.member k (cvMaintainers view)

initialCursor :: CanonCursor
initialCursor = CanonCursor 1 1

-- | Derive the view from folded canon.
viewOf :: FoldResult -> CanonView
viewOf fr = CanonView
  { cvCursor  = cursorFrom fr
  , cvThreads = fmap (\t -> (tsKind t, tsAuthor t)) (frThreads fr)
  , cvEvents  = frAdmitted fr
  , cvMaintainers = frMaintainers fr
  }

-- | The next seq and number to mint, given folded canon.
cursorFrom :: FoldResult -> CanonCursor
cursorFrom fr = CanonCursor (frMaxSeq fr + 1) (frMaxNumber fr + 1)

data TriageError =
    -- | The letter could not be opened at all.
    BadLetter LetterError
    -- | The letter asks for something a stranger's signature cannot carry
    -- into canon: a request the owner must re-author ('honourRequest'), or
    -- an owner-native op that should never have arrived as a letter.
  | NotAcceptable Disposition
    -- | The letter was authored for a different repository.
  | WrongRepo
    -- | A reply naming a thread that is not in canon yet, or an owner event
    -- referring to one. Fold the opening letter first: minting now would
    -- produce an event the fold can never admit.
  | UnknownThread
    -- | This exact author box is already in canon (a rewrapped resend).
  | AlreadyInCanon
    -- | A revise from someone other than the thread's author of record, or
    -- on a thread that is not a PR.
  | NotAuthorOfRecord
    -- | Kind and payload disagree: a PR without coordinates, an issue
    -- carrying them, or a PR-only op on an issue thread. The fold would drop
    -- it, so it is refused before minting.
  | BadContent
    -- | This key may not bless events for this repo: either it was never a
    -- maintainer, its delegation has been revoked, or it is not the owner
    -- key and the op is delegate/revoke (PEP-19 rule 5).
  | UnauthorizedForRepo
    -- | A redact naming an event that is not in canon. Distinct from
    -- 'UnknownThread': the target is an event, and the fold reports this
    -- case separately too.
  | UnknownTarget
    -- | The cursor has reached the top of its range, so the next stamp would
    -- be one the fold refuses ('BadStamp'). Unreachable in practice, but it
    -- is the one case that would otherwise leave the module's promise
    -- incomplete.
  | CursorExhausted
  deriving stock (Eq,Show)

-- Refuse an author box canon already holds: the fold would drop the second
-- copy as a duplicate.
seenAlready :: CanonView -> EventId -> Either TriageError ()
seenAlready view eid
  | HM.member eid (cvEvents view) = Left AlreadyInCanon
  | otherwise                     = Right ()

-- Refuse to mint under a key canon will not accept as a blesser, or with a
-- stamp the fold would reject.
requireCanon :: RepoRef -> CanonView -> HubKey -> Either TriageError ()
requireCanon repo view k
  | not (authorizedCanon repo view k) = Left UnauthorizedForRepo
  | ccrNextSeq cur == maxBound        = Left CursorExhausted
  | ccrNextNumber cur == maxBound     = Left CursorExhausted
  | otherwise                         = Right ()
  where
    cur = cvCursor view

-- | What accepting a letter produced.
data Accepted = Accepted
  { acEvent  :: Event
  , acView   :: CanonView        -- ^ canon plus this event, for the next accept
    -- | Where the event belongs in the tree. A redact goes with the thread
    -- of the event it hides, not under an id of its own, and delegate and
    -- revoke belong to no thread at all.
  , acScope  :: EventScope
  , acNumber :: Maybe Word64     -- ^ minted here, on an open
    -- | The seq stamped on this event. The writer names the file by it, and
    -- deriving it back out of the cursor is both awkward and easy to get
    -- wrong by one.
  , acSeq    :: Word64
    -- | Who authored the box, and what it says. Both are known here, and
    -- recovering them downstream would mean verifying the signature again.
  , acAuthor  :: HubKey
  , acContent :: AuthorContent
    -- | The sender's back-channel, already vetted: present only when the
    -- envelope signer was the inner author. Returned so the caller can
    -- notify without opening the letter a second time.
  , acReply  :: ReplyChannel
  }

-- | Accept a letter into canon.
--
-- Only 'FoldsToCanon' ops (open, comment, revise) may be blessed as they
-- stand, because only those carry an authorship the owner is merely
-- admitting rather than asserting. A @close@ from a stranger is a request
-- (see 'honourRequest'); a @redact@ is owner-native and is refused outright.
--
-- The part secret is supplied by the caller, which is the layer that
-- decrypted the Mailbox message and therefore holds it. It is published only
-- when the letter actually references an attachment, so canon does not carry
-- secrets for messages with nothing encrypted in them.
acceptLetter
  :: (HubKey, PrivKey 'Sign HubScheme)  -- ^ canon (owner or maintainer) keypair
  -> (HubKey -> Bool)                   -- ^ may this inner author be folded? (PEP-21)
  -> HubKey                             -- ^ the envelope (Mailbox message) signer
  -> RepoRef                            -- ^ the repo being folded into
  -> CanonView
  -> Word64                             -- ^ folded-at, the owner's clock
  -> HashRef                            -- ^ origin: hash of the Mailbox message
  -> Maybe ByteString                   -- ^ the message's group secret, if any
  -> MessageData
  -> Either TriageError Accepted
acceptLetter canonKp@(canonPk,_) allowed envelopeSigner repo view folded origin secret md = do
  -- Our own authority first: a revoked maintainer can still sign, but the
  -- fold will not admit it, so minting would burn a triaged letter.
  requireCanon repo view canonPk

  (box, author, content, reply) <-
    either (Left . BadLetter) Right (openLetterAs allowed envelopeSigner md)

  case classify content of
    FoldsToCanon -> Right ()
    other        -> Left (NotAcceptable other)

  seenAlready view (authorBoxId box)

  thread <- case content of
    AOpen target kind _ _ _ _ coords _
      | target /= repo                    -> Left WrongRepo
      | kind == HubPR && isNothing coords -> Left BadContent
      | kind == HubIssue && isJust coords -> Left BadContent
      | otherwise                         -> Right (authorBoxId box)

    AComment thr _ _ _ _ -> known thr

    ARevise thr _ _ -> do
      _ <- known thr
      case HM.lookup thr (cvThreads view) of
        -- Kind first: a revise on an issue is the wrong shape, not a
        -- question of who signed it.
        Just (HubIssue, _) -> Left BadContent
        Just (HubPR, recorded)
          | recorded == author -> Right thr
          | otherwise          -> Left NotAuthorOfRecord
        Nothing -> Left UnknownThread

    -- classify has already restricted this to the three ops above.
    _ -> Left BadContent

  Right (accepted canonKp repo view folded (Just origin) secret content box author
           (ThreadScope thread) reply)
  where
    known thr
      | HM.member thr (cvThreads view) = Right thr
      | otherwise                      = Left UnknownThread

-- | Honour a request a letter made, by re-authoring it under the canon key.
--
-- A stranger's @close@/@reopen@/@label@ letter cannot become canon as
-- theirs: the fold would drop it, because those ops are owner-authored. What
-- the owner can do is agree, which means authoring the same content
-- themselves. The declared timestamp becomes the owner's, since the owner is
-- the one declaring it now; the letter's provenance is kept as the origin.
honourRequest
  :: (HubKey, PrivKey 'Sign HubScheme)  -- ^ canon keypair, which also authors
  -> (HubKey -> Bool)
  -> HubKey                             -- ^ envelope signer
  -> RepoRef                            -- ^ the repo, for the authority check
  -> CanonView
  -> Word64                             -- ^ folded-at, and the owner's declared time
  -> HashRef                            -- ^ origin: the requesting message
  -> MessageData
  -> Either TriageError Accepted
honourRequest canonKp allowed envelopeSigner repo view folded origin md = do
  (_box, _author, content, reply) <-
    either (Left . BadLetter) Right (openLetterAs allowed envelopeSigner md)
  honourOpened canonKp repo view folded origin content content reply

-- | Honour a request, but sign content the maintainer has looked at and
-- possibly edited.
--
-- 'honourRequest' signs the requester's content verbatim, which means an
-- arbitrary attribute name and value chosen by a stranger goes into canon
-- under the owner's signature. Triage should normally decide what it is
-- agreeing to, so this takes the content explicitly; the letter is still
-- required, for its provenance and to check that what it asked for was a
-- request in the first place.
honourWith
  :: (HubKey, PrivKey 'Sign HubScheme)
  -> (HubKey -> Bool)
  -> HubKey                             -- ^ envelope signer
  -> RepoRef
  -> CanonView
  -> Word64                             -- ^ folded-at, and the owner's declared time
  -> HashRef                            -- ^ origin: the requesting message
  -> AuthorContent                      -- ^ what the owner actually signs
  -> MessageData                        -- ^ the request being honoured
  -> Either TriageError Accepted
honourWith canonKp allowed envelopeSigner repo view folded origin content0 md = do
  (_box, _author, asked, reply) <-
    either (Left . BadLetter) Right (openLetterAs allowed envelopeSigner md)
  honourOpened canonKp repo view folded origin asked content0 reply

-- The shared body, taking the letter already opened: 'honourRequest' would
-- otherwise verify the same signature twice.
honourOpened
  :: (HubKey, PrivKey 'Sign HubScheme)
  -> RepoRef
  -> CanonView
  -> Word64
  -> HashRef
  -> AuthorContent    -- ^ what the letter asked for
  -> AuthorContent    -- ^ what the owner signs, possibly edited
  -> ReplyChannel
  -> Either TriageError Accepted
honourOpened canonKp@(pk,sk) repo view folded origin asked content0 reply = do
  requireCanon repo view pk

  case classify asked of
    RequestOnly -> Right ()
    other       -> Left (NotAcceptable other)

  -- What the owner signs must still be a request-shaped op on a thread that
  -- exists, whether or not it is exactly what was asked for.
  case classify content0 of
    RequestOnly -> Right ()
    other       -> Left (NotAcceptable other)

  thread <- case authorThread content0 of
    Just thr | HM.member thr (cvThreads view) -> Right thr
             | otherwise                      -> Left UnknownThread
    Nothing -> Left BadContent

  -- Re-author: a new box signed by the owner, carrying the owner's clock.
  -- Honouring the same request twice at the same clock produces the same
  -- bytes and so the same id, which the fold would drop as a duplicate.
  let content = withAuthorTs folded content0
      box = signAuthor pk sk content
  seenAlready view (authorBoxId box)

  Right (accepted canonKp repo view folded (Just origin) Nothing content box pk
           (ThreadScope thread) reply)

-- | An owner-native event: the owner is both author and canon (PEP-19).
-- Used for status changes, merges, redactions and delegation, none of which
-- ever arrive as a letter.
--
-- Ordering is checked here too. A redact whose target is not in canon yet is
-- refused rather than minted, because the fold treats such a redact as a
-- silent no-op, and a thread op on an unknown thread would be dropped.
ownerEvent
  :: (HubKey, PrivKey 'Sign HubScheme)
  -> RepoRef                            -- ^ the repo, and its root of trust
  -> CanonView
  -> Word64                             -- ^ folded-at
  -> Maybe ByteString                   -- ^ part secret, if the event has one
  -> AuthorContent
  -> Either TriageError Accepted
ownerEvent kp@(pk,sk) repo view folded secret content = do
  requireCanon repo view pk

  let box = signAuthor pk sk content
      eid = authorBoxId box

  seenAlready view eid

  scope <- case content of
    -- A redact belongs with the thread of the event it hides, so it lands
    -- beside it in the tree rather than under an id of its own.
    ARedact target _ -> case HM.lookup target (cvEvents view) of
      Nothing         -> Left UnknownTarget
      Just (Just thr) -> Right (ThreadScope thr)
      Just Nothing    -> Right RepoScope

    -- Only the owner key may delegate (PEP-19 rule 5): a delegate signing
    -- one would be dropped, so refuse before minting.
    ADelegate{} | pk /= repo -> Left UnauthorizedForRepo
                | otherwise  -> Right RepoScope
    ARevoke{}   | pk /= repo -> Left UnauthorizedForRepo
                | otherwise  -> Right RepoScope

    AOpen target kind _ _ _ _ coords _
      | target /= repo                    -> Left WrongRepo
      | kind == HubPR && isNothing coords -> Left BadContent
      | kind == HubIssue && isJust coords -> Left BadContent
      | otherwise                         -> Right (ThreadScope eid)

    -- The remaining ops are thread ops; the fold checks kind for the PR-only
    -- ones, so the bridge checks it too rather than minting a doomed event.
    _ -> case authorThread content of
           Nothing -> Left BadContent
           Just thr -> case HM.lookup thr (cvThreads view) of
             Nothing -> Left UnknownThread
             Just (kind,_)
               | prOnly content && kind /= HubPR -> Left BadContent
               | otherwise                       -> Right (ThreadScope thr)

  Right (accepted kp repo view folded Nothing secret content box pk scope NoReply)
  where
    prOnly = \case
      ARevise{} -> True
      AMerge{}  -> True
      _         -> False

-- The number this content mints, if any: only a thread has one.
mintedNumber :: AuthorContent -> CanonCursor -> Maybe Word64
mintedNumber AOpen{} cur = Just (ccrNextNumber cur)
mintedNumber _ _ = Nothing

-- Stamp a canon box onto an author box, advance the view, and report
-- everything the caller and the writer downstream will need.
--
-- The author is passed in rather than recovered from the box: both callers
-- already know it (one verified the letter, the other just signed it), and
-- unboxing again would be a fourth signature check per letter with a silent
-- fallback on failure, in the one place that decides who may revise a thread.
accepted
  :: (HubKey, PrivKey 'Sign HubScheme)
  -> RepoRef                           -- ^ needed to mirror the fold's owner rule
  -> CanonView
  -> Word64
  -> Maybe HashRef
  -> Maybe ByteString
  -> AuthorContent
  -> SignedBox AuthorContent HubScheme
  -> HubKey                            -- ^ the author box's signer
  -> EventScope
  -> ReplyChannel
  -> Accepted
accepted (pk,sk) repo view folded origin secret content box authorOf scope reply =
  Accepted { acEvent = Event box canonBox
           , acView = view'
           , acScope = scope
           , acNumber = mintedNumber content cur
           , acSeq = ccrNextSeq cur
           , acAuthor = authorOf
           , acContent = content
           , acReply = reply
           }
  where
    cur = cvCursor view
    eid = authorBoxId box

    opensThread = case content of
      AOpen{} -> True
      _       -> False

    -- Only publish a secret when there is something encrypted to unlock.
    secret' | referencesPart content = secret
            | otherwise              = Nothing

    canonBox = signCanon pk sk CanonContent
      { ccEventId    = eid
      , ccSeq        = ccrNextSeq cur
      , ccNumber     = mintedNumber content cur
      , ccOrigin     = origin
      , ccFoldedTs   = folded
      , ccPartSecret = secret'
      }

    view' = view
      { cvCursor = CanonCursor
          { ccrNextSeq    = ccrNextSeq cur + 1
          , ccrNextNumber = if opensThread then ccrNextNumber cur + 1 else ccrNextNumber cur
          }
      , cvThreads = case content of
          AOpen _ kind _ _ _ _ _ _ -> HM.insert eid (kind, authorOf) (cvThreads view)
          _                        -> cvThreads view
      , cvEvents = HM.insert eid (scopeThread scope) (cvEvents view)
        -- Delegation takes effect for the next accept, exactly as it will
        -- when the view is later rebuilt from canon. Revoking the owner is a
        -- no-op here for the same reason it is one in the fold: the owner is
        -- the root of trust and cannot be delegated away, so it cannot be
        -- withdrawn either. Deleting it here would leave the accumulated
        -- view disagreeing with the rebuilt one about a legitimate event.
      , cvMaintainers = case content of
          ADelegate k _           -> HS.insert k (cvMaintainers view)
          ARevoke k _ | k /= repo -> HS.delete k (cvMaintainers view)
          _                       -> cvMaintainers view
      }

    scopeThread = \case
      ThreadScope thr -> Just thr
      RepoScope       -> Nothing

-- Does this content point at an encrypted tree that a reader will need the
-- group secret for?
referencesPart :: AuthorContent -> Bool
referencesPart = \case
  AOpen _ _ _ _ _ part coords _ -> isJust part || maybe False (isJust . prBundle) coords
  AComment _ _ _ part _         -> isJust part
  ARevise _ coords _            -> isJust (prBundle coords)
  _                             -> False
