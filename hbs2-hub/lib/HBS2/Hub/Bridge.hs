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
module HBS2.Hub.Bridge
  ( CanonCursor(..)
  , CanonView(..)
  , Accepted(..)
  , TriageError(..)
  , viewOf
  , emptyView
  , initialCursor
  , cursorFrom
  , acceptLetter
  , honourRequest
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

-- | As much of canon as the bridge needs to decide before minting: what to
-- stamp, which threads a reply may name, and which events already exist.
data CanonView = CanonView
  { cvCursor  :: CanonCursor
    -- | Thread id to (kind, author of record). The author is needed because
    -- only they may revise a PR; the kind, because revise is PR-only.
  , cvThreads :: HashMap ThreadId (HubKind, HubKey)
  , cvEvents  :: HashSet EventId
  }

-- | The view of an empty repo.
emptyView :: CanonView
emptyView = CanonView initialCursor HM.empty HS.empty

initialCursor :: CanonCursor
initialCursor = CanonCursor 1 1

-- | Derive the view from folded canon.
viewOf :: FoldResult -> CanonView
viewOf fr = CanonView
  { cvCursor  = cursorFrom fr
  , cvThreads = fmap (\t -> (tsKind t, tsAuthor t)) (frThreads fr)
  , cvEvents  = HS.fromList
      (HM.keys (frThreads fr) <> concatMap (map cId . tsComments) (HM.elems (frThreads fr)))
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
    -- | Kind and payload disagree: a PR without coordinates, or an issue
    -- carrying them. The fold would drop it, so it is refused before minting.
  | BadContent
  deriving stock (Eq,Show)

-- | What accepting a letter produced.
data Accepted = Accepted
  { acEvent  :: Event
  , acView   :: CanonView        -- ^ canon plus this event, for the next accept
  , acThread :: ThreadId         -- ^ the thread this event belongs to
  , acNumber :: Maybe Word64     -- ^ minted here, on an open
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
acceptLetter canonKp allowed envelopeSigner repo view folded origin secret md = do
  (box, author, content, reply) <-
    either (Left . BadLetter) Right (openLetterAs allowed envelopeSigner md)

  case classify content of
    FoldsToCanon -> Right ()
    other        -> Left (NotAcceptable other)

  if HS.member (authorBoxId box) (cvEvents view)
    then Left AlreadyInCanon
    else Right ()

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
        Just (HubPR, recorded) | recorded == author -> Right thr
        _                                           -> Left NotAuthorOfRecord

    other -> Left (NotAcceptable (classify other))

  let (ev, view') = mint canonKp view folded (Just origin) secret content box
  Right Accepted { acEvent  = ev
                 , acView   = view'
                 , acThread = thread
                 , acNumber = mintedNumber content (cvCursor view)
                 , acReply  = reply
                 }
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
  -> CanonView
  -> Word64                             -- ^ folded-at, and the owner's declared time
  -> HashRef                            -- ^ origin: the requesting message
  -> MessageData
  -> Either TriageError Accepted
honourRequest canonKp@(pk,sk) allowed envelopeSigner view folded origin md = do
  (_box, _author, content0, reply) <-
    either (Left . BadLetter) Right (openLetterAs allowed envelopeSigner md)

  case classify content0 of
    RequestOnly -> Right ()
    other       -> Left (NotAcceptable other)

  thread <- case authorThread content0 of
    Just thr | HM.member thr (cvThreads view) -> Right thr
             | otherwise                      -> Left UnknownThread
    Nothing -> Left BadContent

  -- Re-author: a new box signed by the owner, carrying the owner's clock.
  let content = withAuthorTs folded content0
      box = signAuthor pk sk content
      (ev, view') = mint canonKp view folded (Just origin) Nothing content box
  Right Accepted { acEvent = ev, acView = view', acThread = thread
                 , acNumber = Nothing, acReply = reply }

-- | An owner-native event: the owner is both author and canon (PEP-19).
-- Used for status changes, merges, redactions and delegation, none of which
-- ever arrive as a letter.
--
-- Ordering is checked here too. A redact whose target is not in canon yet is
-- refused rather than minted, because the fold treats such a redact as a
-- silent no-op, and a thread op on an unknown thread would be dropped.
ownerEvent
  :: (HubKey, PrivKey 'Sign HubScheme)
  -> CanonView
  -> Word64                             -- ^ folded-at
  -> Maybe ByteString                   -- ^ part secret, if the event has one
  -> AuthorContent
  -> Either TriageError Accepted
ownerEvent kp@(pk,sk) view folded secret content = do
  let box = signAuthor pk sk content
      eid = authorBoxId box

  thread <- case content of
    ARedact target _
      | HS.member target (cvEvents view) -> Right target
      | otherwise                        -> Left UnknownThread
    -- Repo-scope events belong to no thread; the id stands for itself.
    ADelegate{} -> Right eid
    ARevoke{}   -> Right eid
    AOpen{}     -> Right eid
    _ -> case authorThread content of
           Just thr | HM.member thr (cvThreads view) -> Right thr
                    | otherwise                      -> Left UnknownThread
           Nothing -> Left BadContent

  let (ev, view') = mint kp view folded Nothing secret content box
  Right Accepted { acEvent = ev, acView = view', acThread = thread
                 , acNumber = mintedNumber content (cvCursor view), acReply = NoReply }

-- The number this content mints, if any: only a thread has one.
mintedNumber :: AuthorContent -> CanonCursor -> Maybe Word64
mintedNumber AOpen{} cur = Just (ccrNextNumber cur)
mintedNumber _ _ = Nothing

-- Stamp a canon box onto an author box and advance the view.
mint
  :: (HubKey, PrivKey 'Sign HubScheme)
  -> CanonView
  -> Word64
  -> Maybe HashRef
  -> Maybe ByteString
  -> AuthorContent
  -> SignedBox AuthorContent HubScheme
  -> (Event, CanonView)
mint (pk,sk) view folded origin secret content box = (Event box canonBox, view')
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

    -- The author of record is whoever signed the opening box: for a folded
    -- letter that is the contributor, for an owner-native open the owner.
    authorOf = case unboxChecked box of
      Right (k,_) -> k
      Left _      -> pk

    view' = view
      { cvCursor = CanonCursor
          { ccrNextSeq    = ccrNextSeq cur + 1
          , ccrNextNumber = if opensThread then ccrNextNumber cur + 1 else ccrNextNumber cur
          }
      , cvThreads = case content of
          AOpen _ kind _ _ _ _ _ _ -> HM.insert eid (kind, authorOf) (cvThreads view)
          _                        -> cvThreads view
      , cvEvents = HS.insert eid (cvEvents view)
      }

-- Does this content point at an encrypted tree that a reader will need the
-- group secret for?
referencesPart :: AuthorContent -> Bool
referencesPart = \case
  AOpen _ _ _ _ _ part coords _ -> isJust part || maybe False (isJust . prBundle) coords
  AComment _ _ _ part _         -> isJust part
  ARevise _ coords _            -> isJust (prBundle coords)
  _                             -> False
