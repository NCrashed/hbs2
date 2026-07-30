-- 'authorTs', 'withAuthorTs' and 'authorThread' dispatch on every constructor
-- of 'AuthorContent' with no wildcard, and a constructor added without a case
-- there is a crash on a letter an attacker composed rather than a build error.
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

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
  , validAttrName
  , validHashRef
  , validHubKey
  , safeText
  , pathText
  , hashRefBytes
  , PartSecret
  , mkPartSecret
  , MessageSecret
  , mkMessageSecret
  , sameSecret
  , usablePartSecret
  , partSecretBytes
  , hubMetaVersion
  , hubEventVersion
  , maxFoldedTs
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
import Crypto.Saltine.Class qualified as Saltine
import Codec.Serialise (Serialise(..),serialise)
import Codec.Serialise qualified as CBOR
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isAsciiLower,isDigit)
import Data.Char qualified as Char
import Data.Maybe (isJust)
import Data.List qualified as List
import Data.List (sort)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextE
import Data.Int (Int64)
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
  deriving stock (Eq,Ord,Show,Generic)

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
  deriving stock (Eq,Ord,Show,Generic)

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
    -- | target, redacts (event-id), ts  (owner)
    --
    -- The only op whose subject is not a thread, so the only one the transitive
    -- binding does not reach: a comment names a thread, a thread is an open, and
    -- an open names its repository, but a redact names an event-id and nothing
    -- else. Without the repo here, one signed by a maintainer of two repos hides
    -- an event in whichever of them holds that id, and the argument beside
    -- ADelegate applies word for word.
  | ARedact RepoRef EventId Word64
    -- | target, maintainer key authorized, ts  (owner-key only, PEP-21)
    --
    -- The repo is named here for the reason it is named on an open: an author
    -- box is signed for one repository, and without saying which, an owner who
    -- has two of them signs a delegation that is equally valid in the other.
    -- Only publication stands between that and a maintainer set nobody agreed
    -- to, and publication is the guarantee PEP-19 says not to lean on.
  | ADelegate RepoRef HubKey Word64
    -- | target, maintainer key withdrawn, ts  (owner-key only, PEP-21)
  | ARevoke RepoRef HubKey Word64
  deriving stock (Eq,Ord,Show,Generic)

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
  ARedact _ _ t       -> t
  ADelegate _ _ t     -> t
  ARevoke _ _ t       -> t

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
  ARedact a b _         -> ARedact a b t
  ADelegate a b _       -> ADelegate a b t
  ARevoke a b _         -> ARevoke a b t

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
  { -- | The repository this blessing is for.
    --
    -- The author box says which repo it was written for; without this, the
    -- blessing does not, and a blessing is what canon actually counts. One
    -- person delegated in two repositories is an ordinary arrangement, and any
    -- file they signed in one of them is byte-for-byte valid in the other: both
    -- signatures verify, the canon box references its own author box, and the
    -- stamp is therefore spent before the fold gets as far as noticing that the
    -- CONTENT belongs elsewhere. That hands the other repository's high-water
    -- mark to this one, and a hostile version of it hands over @maxBound@,
    -- after which nothing can be minted here again, the @revoke@ that would
    -- answer it included.
    --
    -- First rather than appended, because a reader asks "is this mine" before
    -- it asks anything else, and because the order is being fixed now: no canon
    -- has been published, so this is the last moment the question is free.
    ccTarget     :: RepoRef
  , ccEventId    :: EventId          -- ^ the author box hash this blesses
  , ccSeq        :: Word64           -- ^ globally monotonic order weight
  , ccNumber     :: Maybe Word64     -- ^ human #N, on open only
  , ccOrigin     :: Maybe HashRef    -- ^ Tier B letter hash (absent if owner-native)
    -- | The author box of the REQUEST this event honours, when it honours one.
    --
    -- The origin above is the Mailbox message's hash, and a message can be
    -- rewrapped: the same inner box under a fresh envelope has a new hash, so
    -- the origin no longer matches and the request is honoured a second time.
    -- The thread closes, someone reopens it, and anyone holding the old
    -- ciphertext closes it again, as often as they like. What does not change
    -- under a rewrap is the box the requester signed, which is what this is.
    --
    -- Absent on an owner-native event and on a folded letter, since a folded
    -- letter IS its own author box and 'ccEventId' already says so.
  , ccHonours    :: Maybe EventId
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
    -- canon"), so 'mkPartSecret' checks what goes in and 'usablePartSecret'
    -- checks what came back out of canon somebody else wrote.
  , ccPartSecret :: Maybe PartSecret
  }
  deriving stock (Eq,Show,Generic)

instance Serialise CanonContent

-- | A canon event: the two authoritative signed boxes.
data Event = Event
  { evAuthorBox :: SignedBox AuthorContent HubScheme
  , evCanonBox  :: SignedBox CanonContent HubScheme
  }
  -- Equality is over the two boxes byte for byte, which is what the canon file
  -- round trip has to preserve: an event that comes back "the same" but for a
  -- signature is not the same event.
  deriving stock (Eq,Generic)

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
--
-- Deduplicated by grouping the sorted list rather than by 'nub', which is
-- quadratic: the input is whatever a letter carried, and the letter layer bounds
-- how many labels it may carry but nothing bounds how many an owner-side caller
-- passes.
encodeLabels :: [Text] -> Text
encodeLabels =
  Text.intercalate "," . map head . List.group . sort . filter validLabel . fmap Text.strip

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

-- | Text on its way to a terminal, with the control characters made visible.
--
-- Every renderer in this package that prints a field a stranger chose goes
-- through this. Canon carries what was signed, as it must, so an attribute name
-- with an ESC in it is an attribute name with an ESC in it; printing that to a
-- terminal hands a stranger the terminal. PEP-22 makes this the renderer's duty
-- and this is the renderer's half of it.
--
-- Control characters, and also the marks, embeddings, overrides, isolates and
-- separators that reorder or break a line without being control characters.
-- 'Char.isControl' does not cover those, so a right-to-left override in an
-- attribute name could still reverse the tail of a report line, and a line or
-- paragraph separator could still break it in two on a terminal that honours
-- them. The whole point here is that one line of output stays one line and reads
-- as what it is.
--
-- The backslash is escaped too, which is what makes this injective: without it a
-- name containing the four characters @\\x0a@ printed exactly like a name
-- containing a newline, so the escaping that was supposed to show what a field
-- holds could be spelled out by the field itself.
safeText :: Text -> Text
safeText = Text.concatMap esc
  where
    esc c | Char.isControl c || bidi c || c == '\\' = escChar c
          | otherwise                              = Text.singleton c

    bidi c = c `elem` [ '\x200E', '\x200F'   -- LEFT/RIGHT-TO-LEFT MARK
                      , '\x061C'             -- ARABIC LETTER MARK
                      , '\x202A', '\x202B', '\x202C', '\x202D', '\x202E'
                      , '\x2066', '\x2067', '\x2068', '\x2069'  -- isolates
                      , '\x2028', '\x2029'   -- LINE/PARAGRAPH SEPARATOR
                      ]

-- | A path out of a stranger's git tree, on its way to a terminal.
--
-- Two paths that differ must print differently, and that is the whole of why
-- this is not 'Data.Text.Encoding.decodeUtf8Lenient' followed by 'safeText'. A
-- lenient decode maps every invalid byte to the same replacement character, so
-- two paths differing only in an invalid byte printed as one line each, both
-- identical; reporting a file is telling somebody which file to go and look at,
-- and one of those two lines named a file that is not there. A tree path is any
-- bytes but NUL, so anybody who can write canon can arrange it.
pathText :: ByteString -> Text
pathText bs = case TextE.decodeUtf8' bs of
  Right t -> safeText t
  -- Not UTF-8, so there is no text to escape and no encoding to guess at: every
  -- byte that is not printable ASCII is spelled out. It cannot collide with the
  -- branch above, where a backslash is itself escaped.
  Left _  -> Text.concat [ escByte b | b <- BS.unpack bs ]
  where
    escByte b | b >= 0x20, b < 0x7f, b /= 0x5c = Text.singleton (Char.chr (fromIntegral b))
              | otherwise                      = escChar (Char.chr (fromIntegral b))

-- One character as @\\xNN@, in as few digits as hold it.
escChar :: Char -> Text
escChar c = Text.pack ("\\x" <> hex (Char.ord c))
  where
    hex n | n < 256   = pad 2 n
          | n < 65536 = pad 4 n
          | otherwise = pad 6 n
    pad w n = [ digit ((n `div` (16 ^ i)) `mod` 16) | i <- [w-1, w-2 .. 0] ]
    digit d = "0123456789abcdef" !! d

-- | Is this a hash at all?
--
-- A 'HashRef' is a newtype over a ByteString with a generic 'Serialise'
-- instance, so it accepts any length on the way in: nothing about the type says
-- 32 bytes. Most of the ones an event carries are compared against something (a
-- thread that must exist, a redact target that must be in canon), and a
-- wrong-length one simply matches nothing. A @reply-to@ is compared against
-- nothing on purpose, so it was the one place a sender could put fifty
-- kilobytes inside a signed author box, past every size gate, and produce a
-- file the reader then refuses forever in every clone. It cannot be repaired:
-- the bytes are inside the signature and inside the event-id.
--
-- The width is the digest's, and it is also what the box-size budget assumes
-- (see @maxBoxBytes@): four kilobytes of slack covers a handful of hashes only
-- while a hash is a hash.
validHashRef :: HashRef -> Bool
validHashRef h = LBS.length (serialise h) == hashRefBytes

-- | What a well-formed one weighs on the wire.
--
-- Measured from a real hash rather than written down as a number, so that it
-- stays right if the digest or its framing ever changes, and so that there is
-- no second opinion about the encoding anywhere in this module.
hashRefBytes :: Int64
hashRefBytes = LBS.length (serialise (HashRef (hashObject ("" :: ByteString))))

-- | Is this a well-formed attribute name?
--
-- A shape rule, not a spelling rule, and the difference is the whole point. A
-- name is matched literally against 'multiValued' to decide whether its value
-- is a set, so @labels @, @ labels@, @la bels@ and @""@ are each a separate
-- attribute that no set canonicalization touches, that the render contract
-- never reads, and that nothing can delete afterwards, since the ops only ever
-- insert. Checking the case alone closes one spelling out of an infinite
-- family. A name is a vocabulary word: a lowercase letter, then lowercase
-- letters, digits and hyphens.
validAttrName :: Text -> Bool
validAttrName k = case Text.uncons k of
  Just (c,rest) -> isAsciiLower c && Text.all part rest
  Nothing       -> False
  where
    part c = isAsciiLower c || isDigit c || c == '-'

-- | Is this attribute already in canonical form, name and value both?
normalizedAttr :: Text -> Text -> Bool
normalizedAttr k v = validAttrName k && normalizeAttr k v == v

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

-- | The secret a Mailbox message's own payload is encrypted with.
--
-- Never published, and here only so that it can be told apart from
-- 'PartSecret' by type rather than by whether the caller remembered which was
-- which. They are the same bytes and the same length, and confusing them
-- publishes the sender's private reply address into every clone forever.
--
-- There is deliberately no accessor at all, unlike 'partSecretBytes': a part
-- secret is published into canon and a reader hands those bytes to the
-- decryptor, while nothing outside this module has any business reading these.
-- An accessor is also how one type becomes the other in one composition, by a
-- caller who is sure they know which they are holding, which is the mistake
-- both newtypes exist to make impossible.
newtype MessageSecret = MessageSecret ByteString
  deriving stock (Eq)

instance Show MessageSecret where
  show _ = "MessageSecret <hidden>"

-- | Build one. Same length check as 'mkPartSecret', for the same reason.
mkMessageSecret :: ByteString -> Maybe MessageSecret
mkMessageSecret bs
  | BS.length bs == (typicalKeyLength :: Int) = Just (MessageSecret bs)
  | otherwise                                 = Nothing

-- | Are these two the same key?
--
-- The one thing the two secret types can be asked about each other, and the
-- only thing anything needs: the bridge refuses to publish a parts secret that
-- is really the message secret, and no reader can tell them apart by their
-- bytes. Comparing them any other way needs an accessor, and an accessor is
-- what puts a secret in a log.
sameSecret :: PartSecret -> MessageSecret -> Bool
sameSecret (PartSecret a) (MessageSecret b) = a == b

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

-- | The highest @folded-ts@ canon admits: 2100-01-01T00:00:00Z in epoch
-- milliseconds.
--
-- An admission rule, so it is consensus and part of @hub-meta 1@: it cannot be
-- raised or lowered once a repo has canon written under it.
--
-- It has to be a constant rather than a window around the reader's own clock,
-- which is what a "not absurdly far ahead" rule would want. The fold is a pure
-- function of canon; a rule that consulted the wall clock would have two nodes
-- disagree about which events count, and the same node disagree with itself an
-- hour later, which is the one thing the fold may never do.
--
-- What it bounds is the damage, not the mistake. The next stamp is clamped to
-- be no lower than the last one, so a stamp anywhere in the range pins every
-- later event to it; the ceiling only stops that from being permanent in the
-- sense of "no clock will ever pass it". Recovery from a stamp inside the
-- range is compaction (PEP-19), which restamps.
maxFoldedTs :: Word64
maxFoldedTs = 4102444800000

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
  -- Before the verify, because the verify is where the bytes stop being ours.
  -- The Serialise instances for a signing key and a signature are generic over
  -- a newtype, so they take any length and walk straight past the length check
  -- the crypto library does on its own decoder; the verifier then hands the
  -- pointers to a C function that reads thirty-two and sixty-four bytes without
  -- asking. On this heap that lands in slack and returns a mismatch rather than
  -- a crash, which is luck rather than a defence: it is a read out of bounds
  -- either way. This runs the check the instance skipped, using the crypto
  -- library's own decoder so there is no second opinion about the sizes.
  | not (wellFormed pk sig)               = Left BoxBadSig
  | not (verifySign @HubScheme pk sig bs) = Left BoxBadSig
  | otherwise = case decodeChecked bs of
      Left why -> Left (BoxUndecodable pk why)
      Right (Domained d p)
        -- Signed for something else. Not a forgery claim, since the signature
        -- is real, and not a newer schema either: it is these exact bytes
        -- presented as a kind of record they were never signed as.
        | d /= domainOf (Nothing @p) -> Left (BoxUndecodable pk WrongDomain)
        | otherwise                  -> Right (pk, p)

-- | Is this a key at all?
--
-- Defence in depth rather than a hole being closed: the base58 path a key
-- normally arrives by does check the length. A key inside a signed author box
-- did not come that way.
validHubKey :: HubKey -> Bool
validHubKey k = isJust (Saltine.decode (Saltine.encode k) :: Maybe HubKey)

-- | Are these the right shape to hand to the verifier at all?
--
-- Round-tripping through the crypto library's own encoder and decoder, which is
-- the length check the 'Serialise' instances do not do. Deliberately not a pair
-- of constants: the sizes belong to the scheme, and writing them down here
-- would be a second place for them to be wrong.
wellFormed :: HubKey -> Signature HubScheme -> Bool
wellFormed pk sig = ok @(PubKey 'Sign HubScheme) pk && ok @(Signature HubScheme) sig
  where
    ok :: forall a . Saltine.IsEncoding a => a -> Bool
    ok a = isJust (Saltine.decode (Saltine.encode a) :: Maybe a)

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
