-- | Core types for hbs2-hub canon (PEP-19).
--
-- An issue or PR thread is event-sourced: its state is the deterministic
-- fold of an append-only sequence of signed events. Every event carries two
-- SignedBoxes (PEP-19 "Signing and canonical encoding"):
--
--   * the author box, over 'AuthorContent' (what the author said), whose
--     content hash is the event-id; and
--   * the canon box, over 'CanonContent' (what the owner assigned at fold:
--     seq, number, origin, part-secret).
--
-- This module defines those records and the signing/identity helpers. The
-- deterministic fold lives in "HBS2.Hub.Fold".
module HBS2.Hub.Types
  ( HubScheme
  , HubKey
  , RepoRef
  , EventId
  , ThreadId
  , HubKind(..)
  , PRCoords(..)
  , AuthorContent(..)
  , CanonContent(..)
  , Event(..)
  , authorTs
  , authorThread
  , eventId
  , authorBoxId
  , signAuthor
  , signCanon
  , mkEvent
  , BoxError(..)
  , unboxChecked
  , decodeStrict
  ) where

import HBS2.Prelude.Plated
import HBS2.Hash (hashObject)
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Data.Types.SignedBox

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.Serialise (Serialise(..),serialise)
import Codec.Serialise qualified as CBOR
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString (ByteString)
import Data.Word (Word64)

-- | The whole hub is fixed to the basic scheme, like hbs2-git3.
type HubScheme = 'HBS2Basic

-- | A signing key: author, canon (maintainer), or delegate. All the same type.
type HubKey = PubKey 'Sign HubScheme

-- | A repository, named by its LWWRef signing key (the git3 repo key).
type RepoRef = PubKey 'Sign HubScheme

-- | An event-id is the hbs2 content hash of the event's author box. A
-- thread-id is the event-id of the thread's opening event.
type EventId  = HashRef
type ThreadId = EventId

data HubKind = HubIssue | HubPR
  deriving stock (Eq,Show,Generic)

instance Serialise HubKind

-- | Pull-request coordinates (PEP-20). On the delta path 'prSource' is
-- absent and 'prBundle' carries the git bundle; on the fork-pointer path
-- the reverse. 'prSourceTip'/'prBase' are always present and signed.
data PRCoords = PRCoords
  { prSource    :: Maybe Text     -- ^ hbs23://<fork-key>, fork-pointer path only
  , prSourceRef :: Text
  , prSourceTip :: Text           -- ^ git sha1 being proposed
  , prOnto      :: Text
  , prBase      :: Text           -- ^ merge-base the branch forked from
  , prBundle    :: Maybe HashRef  -- ^ bundle-part, delta path only
  }
  deriving stock (Eq,Show,Generic)

instance Serialise PRCoords

-- | What an author actually said. One shared record type across both tiers
-- (PEP-18/PEP-19): the letter subset a non-owner may author plus the
-- owner-native ops. Positional to avoid duplicate-field selectors.
--
-- WIRE FORMAT, APPEND ONLY. The generic 'Serialise' encoding tags
-- constructors by position, and an event-id is the hash of those bytes, so
-- the constructor order and every field order below are frozen: reordering
-- or removing any of them silently rewrites every existing event-id and
-- invalidates every signature ever made. Add new constructors at the END,
-- and new fields only as a new constructor.
data AuthorContent =
    -- | target, kind, title, requested labels, body?, body-part?, pr-coords?, ts
    --
    -- The labels are what the author /asks/ for. They are advisory: the fold
    -- does not apply them, because a stranger must not label their own
    -- submission (PEP-18). They travel in the signed content so triage can
    -- see the request and the owner can honour it with an owner-signed set.
    AOpen RepoRef HubKind Text [Text] (Maybe Text) (Maybe HashRef) (Maybe PRCoords) Word64
    -- | thread, reply-to?, body?, body-part?, ts
  | AComment ThreadId (Maybe EventId) (Maybe Text) (Maybe HashRef) Word64
    -- | thread, new coords, ts  (PR only, author-of-record)
  | ARevise ThreadId PRCoords Word64
    -- | thread, attr, value, ts  (owner)
  | ASet ThreadId Text Text Word64
    -- | thread, note?, ts  (owner; sets status closed)
  | AClose ThreadId (Maybe Text) Word64
    -- | thread, note?, ts  (owner; sets status open)
  | AReopen ThreadId (Maybe Text) Word64
    -- | thread, merge-commit, merged-into, ts  (owner, PR)
  | AMerge ThreadId Text Text Word64
    -- | redacts (target event-id), ts  (owner)
  | ARedact EventId Word64
    -- | maintainer key authorized, ts  (owner-key only, PEP-21)
  | ADelegate HubKey Word64
    -- | maintainer key withdrawn, ts  (owner-key only, PEP-21)
  | ARevoke HubKey Word64
  deriving stock (Eq,Show,Generic)

instance Serialise AuthorContent

-- | The author-declared timestamp (Unix epoch seconds, UTC; advisory).
authorTs :: AuthorContent -> Word64
authorTs = \case
  AOpen _ _ _ _ _ _ _ t -> t
  AComment _ _ _ _ t  -> t
  ARevise _ _ t       -> t
  ASet _ _ _ t        -> t
  AClose _ _ t        -> t
  AReopen _ _ t       -> t
  AMerge _ _ _ t      -> t
  ARedact _ t         -> t
  ADelegate _ t       -> t
  ARevoke _ t         -> t

-- | The thread a reply-class event targets. 'Nothing' for 'AOpen' (its own
-- id is the thread) and for the repo-scope ops 'ARedact'/'ADelegate'/'ARevoke'.
authorThread :: AuthorContent -> Maybe ThreadId
authorThread = \case
  AOpen{}          -> Nothing
  AComment thr _ _ _ _ -> Just thr
  ARevise thr _ _  -> Just thr
  ASet thr _ _ _   -> Just thr
  AClose thr _ _   -> Just thr
  AReopen thr _ _  -> Just thr
  AMerge thr _ _ _ -> Just thr
  ARedact{}        -> Nothing
  ADelegate{}      -> Nothing
  ARevoke{}        -> Nothing

-- | What the owner assigns at fold time (PEP-19 canon box). The signer of
-- the canon box is @canon-by@ and is recovered on unbox, not stored here.
data CanonContent = CanonContent
  { ccEventId    :: EventId          -- ^ the author box hash this blesses
  , ccSeq        :: Word64           -- ^ globally monotonic order weight
  , ccNumber     :: Maybe Word64     -- ^ human #N, on open only
  , ccOrigin     :: Maybe HashRef    -- ^ Tier B letter hash (absent if owner-native)
  , ccFoldedTs   :: Word64           -- ^ owner clock at fold
  , ccPartSecret :: Maybe ByteString -- ^ group secret, on events with encrypted parts
  }
  deriving stock (Eq,Show,Generic)

instance Serialise CanonContent

-- | A canon event: the two authoritative signed boxes.
data Event = Event
  { evAuthorBox :: SignedBox AuthorContent HubScheme
  , evCanonBox  :: SignedBox CanonContent HubScheme
  }
  deriving stock (Generic)

instance Serialise Event

-- | Events are identified by their event-id; boxes are opaque bytes.
instance Show Event where
  show = show . eventId

-- | The event-id of an author box: the hbs2 content hash of its serialised
-- bytes. Anyone holding the box can compute it, which is what lets a Tier B
-- sender know the canonical thread-id at send time (PEP-18 threading).
authorBoxId :: SignedBox AuthorContent HubScheme -> EventId
authorBoxId = HashRef . hashObject . serialise

-- | The event-id: the hbs2 content hash of the serialised author box.
-- Stable before seq/number exist and computable by the author.
eventId :: Event -> EventId
eventId = authorBoxId . evAuthorBox

-- | Why a signed box could not be opened.
data BoxError =
    BoxBadSig       -- ^ the signature does not verify: a forgery claim
  | BoxUndecodable  -- ^ signature fine, content unknown to this reader
  deriving stock (Eq,Show)

-- | Open a signed box, keeping the two failure modes apart.
--
-- 'unboxSignedBox0' collapses them into 'Nothing', which would make a
-- correctly signed message carrying a newer op look like a forgery. That
-- matters for diagnostics (@hub verify@) and for triage, so verification and
-- decoding are done in separate steps here.
unboxChecked
  :: Serialise p
  => SignedBox p HubScheme
  -> Either BoxError (HubKey, p)
unboxChecked (SignedBox pk bs sig)
  | not (verifySign @HubScheme pk sig bs) = Left BoxBadSig
  | otherwise = maybe (Left BoxUndecodable) (Right . (pk,)) (decodeStrict bs)

-- | Decode CBOR requiring the whole input to be consumed.
--
-- The usual 'deserialiseOrFail' stops at the end of a valid value and
-- ignores what follows. Every decode in the hub goes through this instead,
-- so there is one decoding rule rather than a strict path for some bytes and
-- a lenient one for others.
decodeStrict :: Serialise a => ByteString -> Maybe a
decodeStrict bs =
  case deserialiseFromBytes CBOR.decode (LBS.fromStrict bs) of
    Right (rest, a) | LBS.null rest -> Just a
    _ -> Nothing

-- | Sign author content into an author box.
signAuthor :: HubKey -> PrivKey 'Sign HubScheme -> AuthorContent -> SignedBox AuthorContent HubScheme
signAuthor = makeSignedBox

-- | Sign canon content into a canon box.
signCanon :: HubKey -> PrivKey 'Sign HubScheme -> CanonContent -> SignedBox CanonContent HubScheme
signCanon = makeSignedBox

-- | Build a complete event: sign the author content, compute its event-id,
-- then let the caller (the publisher) build the canon content against that
-- id and sign it. This is the one place the two boxes are tied together.
mkEvent
  :: (HubKey, PrivKey 'Sign HubScheme)  -- ^ author keypair
  -> (HubKey, PrivKey 'Sign HubScheme)  -- ^ canon (owner/maintainer) keypair
  -> AuthorContent
  -> (EventId -> CanonContent)          -- ^ canon builder, given the computed id
  -> Event
mkEvent (apk,askk) (cpk,cskk) ac mkCanon =
  let abox = signAuthor apk askk ac
      eid  = HashRef (hashObject (serialise abox))
      cbox = signCanon cpk cskk (mkCanon eid)
  in Event abox cbox
