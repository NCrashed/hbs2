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
  , withAuthorTs
  , authorThread
  , eventId
  , authorBoxId
  , canonBoxId
  , signAuthor
  , signCanon
  , mkEvent
  , BoxError(..)
  , SignedIn(..)
  , Domained(..)
  , unboxChecked
  , decodeStrict
  , decodeChecked
  , UndecodableWhy(..)
  , encodeLabels
  , decodeLabels
  , validLabel
  , multiValued
  , normalizeAttr
  , normalizedAttr
  , PartSecret
  , mkPartSecret
  , usablePartSecret
  , partSecretBytes
  , hubMetaVersion
  , hubEventVersion
  , threadDir
  , repoDir
  , numberIndexPath
  , eventFileName
  ) where

import HBS2.Prelude.Plated
import HBS2.Hash (hashObject)
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.GroupKeySymm (typicalKeyLength)
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Data.Types.SignedBox

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.Serialise (Serialise(..),serialise)
import Codec.Serialise qualified as CBOR
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (nub,sort)
import Data.Text qualified as Text
import Data.Word (Word32,Word64)

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

-- | WIRE FORMAT, APPEND ONLY: reachable from 'AuthorContent', so its
-- encoding is part of what an event-id hashes. See the note there.
data HubKind = HubIssue | HubPR
  deriving stock (Eq,Show,Generic)

instance Serialise HubKind

-- | Pull-request coordinates (PEP-20). On the delta path 'prSource' is
-- absent and 'prBundle' carries the git bundle; on the fork-pointer path
-- the reverse. 'prSourceTip'/'prBase' are always present and signed.
--
-- WIRE FORMAT, APPEND ONLY: reachable from 'AuthorContent', so adding or
-- reordering a field here rewrites every event-id that mentions a PR and
-- invalidates its signature, exactly as it would in 'AuthorContent' itself.
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

-- | The author-declared timestamp: Unix epoch MILLISECONDS, UTC, advisory.
--
-- Milliseconds rather than seconds because this field is inside the signed
-- content, so two owner-authored events with otherwise identical bytes in
-- the same tick collapse to one event-id and the second is refused as a
-- duplicate. A close, reopen and close in the same second is an ordinary
-- triage sequence; in the same millisecond it is not. The collision is not
-- eliminated, only priced out of reach of a human at a keyboard: a caller
-- minting owner events in a tight loop must still vary something.
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

-- | Replace the declared timestamp.
--
-- Used when the owner re-authors someone else's request: the content is
-- theirs to restate, but the clock reading must be the owner's, or an event
-- signed by the owner would carry a timestamp the owner never declared.
withAuthorTs :: Word64 -> AuthorContent -> AuthorContent
withAuthorTs t = \case
  AOpen a b c d e f g _ -> AOpen a b c d e f g t
  AComment a b c d _    -> AComment a b c d t
  ARevise a b _         -> ARevise a b t
  ASet a b c _          -> ASet a b c t
  AClose a b _          -> AClose a b t
  AReopen a b _         -> AReopen a b t
  AMerge a b c _        -> AMerge a b c t
  ARedact a _           -> ARedact a t
  ADelegate a _         -> ADelegate a t
  ARevoke a _           -> ARevoke a t

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
--
-- WIRE FORMAT, APPEND ONLY: not hashed into any event-id, but it is what
-- the canon box signs, so a changed encoding invalidates every canon
-- signature ever made.
data CanonContent = CanonContent
  { ccEventId    :: EventId          -- ^ the author box hash this blesses
  , ccSeq        :: Word64           -- ^ globally monotonic order weight
  , ccNumber     :: Maybe Word64     -- ^ human #N, on open only
  , ccOrigin     :: Maybe HashRef    -- ^ Tier B letter hash (absent if owner-native)
  , ccFoldedTs   :: Word64           -- ^ owner clock at fold, Unix epoch milliseconds
    -- | The group secret for this event's encrypted parts, on events that
    -- reference one.
    --
    -- Raw key bytes: exactly what @Saltine.encode@ gives for a 'GroupSecret',
    -- which is what a reader hands back to the decryptor. Deliberately not the
    -- 'GroupSecret' type: canon is forever, and the CBOR shape of a bare
    -- ByteString is pinned by CBOR itself, while the shape of the key type is
    -- pinned by whatever the crypto library derives. There is no way to fix an
    -- encoding mismatch after the fact (see PEP-19 "Attachments in public
    -- canon"), so 'validPartSecret' checks what goes in.
  , ccPartSecret :: Maybe PartSecret
  }
  deriving stock (Eq,Show,Generic)

instance Serialise CanonContent

-- | A canon event: the two authoritative signed boxes.
data Event = Event
  { evAuthorBox :: SignedBox AuthorContent HubScheme
  , evCanonBox  :: SignedBox CanonContent HubScheme
  }
  deriving stock (Generic)

-- Deliberately NO Serialise instance. Canon does not store an Event as one
-- blob: PEP-19 writes the two boxes separately, base58 inside the event
-- file's S-expression, so a Serialise here would be a second, unused
-- encoding of the thing whose encoding matters most.

-- | Events are identified by their event-id; boxes are opaque bytes.
instance Show Event where
  show = show . eventId

-- | The event-id of an author box: the hbs2 content hash of its serialised
-- bytes. Anyone holding the box can compute it, which is what lets a Tier B
-- sender know the canonical thread-id at send time (PEP-18 threading).
authorBoxId :: SignedBox AuthorContent HubScheme -> EventId
authorBoxId = HashRef . hashObject . serialise

-- | The hash of a serialised canon box.
--
-- Not an identity anything refers to: it exists to break a tie the event-id
-- cannot. The same author box blessed twice at the same @seq@ is ONE event to
-- the fold, and which of the two canon boxes the fold reads its stamp from
-- must not depend on the order the events came out of the tree (PEP-19
-- "Ordering").
canonBoxId :: SignedBox CanonContent HubScheme -> HashRef
canonBoxId = HashRef . hashObject . serialise

-- | The event-id: the hbs2 content hash of the serialised author box.
-- Stable before seq/number exist and computable by the author.
eventId :: Event -> EventId
eventId = authorBoxId . evAuthorBox

-- | Encode a multi-valued attribute (labels, assignees) for 'ASet'.
--
-- Attribute values are one string, so a set of labels needs a canonical
-- rendering: sorted, deduplicated, comma-separated, no spaces. Without it
-- the same labels in a different order are different bytes and therefore a
-- different event-id, and two maintainers agreeing on the same labels would
-- mint two different events (PEP-19).
--
-- A label containing a comma is not representable and is dropped, since
-- keeping it would split into two labels on the way back.
encodeLabels :: [Text] -> Text
encodeLabels = Text.intercalate "," . nub . sort . filter validLabel . fmap Text.strip

-- | Read back what 'encodeLabels' wrote.
decodeLabels :: Text -> [Text]
decodeLabels t
  | Text.null t = []
  | otherwise   = Text.splitOn "," t

-- | A label is representable if it survives the round trip: no comma, not
-- empty.
validLabel :: Text -> Bool
validLabel l = not (Text.null l) && not (Text.isInfixOf "," l)

-- | Attribute names whose value is a set, not a scalar.
--
-- The canonical rendering of a set ('encodeLabels') is what makes two
-- maintainers agreeing on the same labels produce the same event-id, so the
-- list of names it applies to has to live next to it rather than in each
-- caller. A writer normalizes the value of these before signing; @hub verify@
-- (PEP-22) reports a value in canon that is not normalized, since the fold
-- cannot fix one (the author box is signed) and must not drop it either.
multiValued :: [Text]
multiValued = ["labels","assignees"]

-- | Normalize an attribute value for signing: canonical for a set-valued
-- name, untouched otherwise.
normalizeAttr :: Text -> Text -> Text
normalizeAttr k v
  | k `elem` multiValued = encodeLabels (decodeLabels v)
  | otherwise            = v

-- | Is this attribute already in canonical form, name and value both?
--
-- The name matters as much as the value: @Labels@ is not @labels@, so it would
-- sail past set canonicalization as if it were a scalar and then sit in the
-- attribute map as a second, near-invisible attribute. Lowercase is the whole
-- rule, since an attribute name is a vocabulary word rather than prose.
normalizedAttr :: Text -> Text -> Bool
normalizedAttr k v = Text.toLower k == k && normalizeAttr k v == v

-- | The group secret an event's encrypted parts were encrypted with.
--
-- A newtype over the raw key bytes, and the only reason it is one is that the
-- OTHER secret in reach is the same type and the same length: PEP-18 gives the
-- parts a secret of their own precisely so that publishing it into canon does
-- not publish the message payload, and nothing about @ByteString@ says which
-- of the two a caller is holding. Getting that wrong publishes the sender's
-- private reply address to every clone, retroactively and with no way back
-- (PEP-19 "Attachments in public canon").
--
-- Build one only where a part tree has actually been decrypted, which is the
-- one place the distinction is visible. The wire form is unchanged: raw bytes,
-- exactly what @Saltine.encode@ gives.
newtype PartSecret = PartSecret { partSecretBytes :: ByteString }
  deriving stock (Eq)

-- Deliberately partial: a secret must not print itself into a log.
instance Show PartSecret where
  show _ = "PartSecret <hidden>"

instance Serialise PartSecret where
  encode = CBOR.encode . partSecretBytes
  decode = PartSecret <$> CBOR.decode

-- | Build one, checking the only thing bytes can be checked for.
--
-- A length check, not a validity check: nothing here can tell a real key from
-- 32 arbitrary bytes, and it cannot tell the parts secret from the message
-- secret either. It exists because the field has a pinned meaning, and a
-- writer that put something else there (a base58 string, a serialised key
-- type) would produce canon whose attachments never open.
mkPartSecret :: ByteString -> Maybe PartSecret
mkPartSecret bs
  | usablePartSecret (PartSecret bs) = Just (PartSecret bs)
  | otherwise                        = Nothing

-- | The same check, for a secret that arrived already decoded.
--
-- 'mkPartSecret' guards the writing side, and 'Serialise' walks straight past
-- it: canon somebody else wrote can carry anything. A reader cannot refuse the
-- event over it (the event is fine, the key is not), so the fold reports it as
-- an anomaly instead, which is the only way @hub verify@ can see it.
usablePartSecret :: PartSecret -> Bool
usablePartSecret (PartSecret bs) = BS.length bs == (typicalKeyLength :: Int)

-- | The consensus version of the canon layout and fold rules: the
-- @(hub-meta N)@ of PEP-19.
--
-- A reader that meets a higher one reports it rather than folding. Any change
-- to the admission rules, to the drop reasons, or to how state is derived from
-- an admitted event bumps this.
hubMetaVersion :: Word32
hubMetaVersion = 1

-- | The version of a single event file: the @(hub-event N)@ of PEP-19.
hubEventVersion :: Word32
hubEventVersion = 1

-- | Where a thread's events live, relative to the canon tree root.
threadDir :: ThreadId -> FilePath
threadDir tid = "threads/" <> show (pretty tid)

-- | Where events that belong to no thread live (delegate, revoke).
repoDir :: FilePath
repoDir = "repo"

-- | The rebuildable number index (PEP-19); canon does not depend on it.
numberIndexPath :: FilePath
numberIndexPath = "index/number.sexp"

-- | The file name of one event: zero-padded @seq@, then the event-id.
--
-- The padding is what makes a plain lexical directory listing agree with the
-- fold's order, and the pair is what makes the name unique: two blessings of
-- one author box at one @seq@ are the same path, which is why a well-formed
-- canon cannot contain them (see the sort key in "HBS2.Hub.Fold").
eventFileName :: Word64 -> EventId -> FilePath
eventFileName sq eid = pad (show sq) <> "-" <> show (pretty eid)
  where
    pad t = replicate (20 - length t) '0' <> t

-- | Why a signed box could not be opened.
data BoxError =
    BoxBadSig  -- ^ the signature does not verify: a forgery claim
    -- | Signature fine, content this reader cannot decode. Carries the
    -- signer, because a reader that cannot name the key can neither
    -- attribute the event (@hub verify@) nor apply a deny-list to it, and
    -- an undecodable letter is retried rather than discarded.
  | BoxUndecodable HubKey UndecodableWhy
  deriving stock (Eq,Show)

-- | Why content would not decode. A newer schema and a tampered payload
-- look alike unless the two are told apart: trailing bytes are never
-- something an honest newer sender produces.
data UndecodableWhy =
    Undecodable  -- ^ decode failed outright: a newer op, or plain garbage
  | TrailingData -- ^ decoded, but bytes were left over
    -- | Correctly signed and correctly shaped, but signed as another kind of
    -- record (see 'Domained'): a signature lifted from somewhere else.
  | WrongDomain
  deriving stock (Eq,Ord,Show)

-- | Open a signed box, keeping the two failure modes apart.
--
-- 'unboxSignedBox0' collapses them into 'Nothing', which would make a
-- correctly signed message carrying a newer op look like a forgery. That
-- matters for diagnostics (@hub verify@) and for triage, so verification and
-- decoding are done in separate steps here.
unboxChecked
  :: forall p . SignedIn p
  => SignedBox p HubScheme
  -> Either BoxError (HubKey, p)
unboxChecked (SignedBox pk bs sig)
  | not (verifySign @HubScheme pk sig bs) = Left BoxBadSig
  | otherwise = case decodeChecked bs of
      Left why -> Left (BoxUndecodable pk why)
      Right (Domained d p)
        -- Signed for something else. Not a forgery claim, since the signature
        -- is real, and not a newer schema either: it is these exact bytes
        -- presented as a kind of record they were never signed as.
        | d /= domainOf (Nothing @p) -> Left (BoxUndecodable pk WrongDomain)
        | otherwise                  -> Right (pk, p)

-- | Decode CBOR requiring the whole input to be consumed.
--
-- The usual 'deserialiseOrFail' stops at the end of a valid value and
-- ignores what follows. Every decode in the hub goes through this instead,
-- so there is one decoding rule rather than a strict path for some bytes and
-- a lenient one for others.
decodeStrict :: Serialise a => ByteString -> Maybe a
decodeStrict = either (const Nothing) Just . decodeChecked

-- | As 'decodeStrict', but says which way it failed.
decodeChecked :: Serialise a => ByteString -> Either UndecodableWhy a
decodeChecked bs =
  case deserialiseFromBytes CBOR.decode (LBS.fromStrict bs) of
    Right (rest, a) | LBS.null rest -> Right a
                    | otherwise     -> Left TrailingData
    Left _ -> Left Undecodable

-- | A payload with the domain it was signed for written into the signed bytes.
--
-- Ed25519 signs the CBOR of whatever it is handed, so a signature says "this
-- key signed these bytes", never "this key signed an event". The same key
-- signs several unrelated records: an author box, a canon box, a git3 LWWRef
-- on every push, a sigil. If any two of those have a CBOR encoding in common,
-- a signature made for one is a valid signature for the other, and here that
-- means a forged event.
--
-- Nothing structural prevented that. A sum constructor tag is an ordinary
-- small CBOR integer, so @serialise (7 :: Word64, h, 5 :: Word64)@ is byte for
-- byte @serialise (ARedact h 5)@: any record of shape [small int, hash, int]
-- signed by the owner for any purpose was a signed redaction of any event.
-- Nothing has that shape today (the LWWRef escapes only because its third
-- field is a Maybe, and so an array rather than an int), but that is an
-- accident of four unrelated types, and one field type changing anywhere would
-- open it silently. There is no repairing it afterwards, since an event-id
-- hashes the whole box.
--
-- The domain constant is a large number on purpose: nothing starts with it by
-- chance, and a record that did would have to be built to.
data Domained a = Domained Word32 a
  deriving stock (Generic)

instance Serialise a => Serialise (Domained a)

-- | The payloads this package signs, each pinned to its own domain.
--
-- WIRE FORMAT: a domain is assigned once and never reused or renumbered. It is
-- inside the signed bytes, and therefore inside every event-id.
class Serialise a => SignedIn a where
  domainOf :: proxy a -> Word32

instance SignedIn AuthorContent where domainOf _ = 0x48423241  -- "HB2A"
instance SignedIn CanonContent  where domainOf _ = 0x48423243  -- "HB2C"

signIn :: forall a . SignedIn a
       => HubKey -> PrivKey 'Sign HubScheme -> a -> SignedBox a HubScheme
signIn pk sk a = retag (makeSignedBox pk sk (Domained (domainOf (Nothing @a)) a))
  where
    -- The box's type parameter is phantom: it names what the bytes mean, and
    -- they still mean an 'a'. The wrapper is an encoding detail, not a
    -- different payload, so it stays out of every signature in the API.
    retag :: SignedBox (Domained a) HubScheme -> SignedBox a HubScheme
    retag (SignedBox p b s) = SignedBox p b s

-- | Sign author content into an author box.
signAuthor :: HubKey -> PrivKey 'Sign HubScheme -> AuthorContent -> SignedBox AuthorContent HubScheme
signAuthor = signIn

-- | Sign canon content into a canon box.
signCanon :: HubKey -> PrivKey 'Sign HubScheme -> CanonContent -> SignedBox CanonContent HubScheme
signCanon = signIn

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
