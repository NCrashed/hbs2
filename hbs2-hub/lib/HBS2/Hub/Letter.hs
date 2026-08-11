-- 'classify' and 'letterSyntax' dispatch on every constructor of
-- 'AuthorContent' with no wildcard, and 'classify' runs on content an attacker
-- composed, inside the triage loop. A constructor added without a case there is
-- a crash in that loop rather than a build error, so the warning is an error.
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | The Tier B letter (PEP-18).
--
-- A letter rides the Mailbox protocol: the Mailbox 'Message' is a
-- @SignedBox MessageContent@ whose body is encrypted to the maintainers.
-- That outer signature covers ciphertext and binds the recipient set, so it
-- is transport only. What this module defines is the /plaintext/ that goes
-- into @messageData@: a versioned envelope around either
--
--   * a 'Letter': a 'SignedBox' over 'AuthorContent' (the durable, publicly
--     verifiable authorship proof) plus a transport-only 'ReplyChannel'
--     discarded at fold; or
--   * an 'Ack': a courtesy notification from the owner back to a
--     contributor, which carries no inner box and never becomes canon.
--
-- The inner box is the PEP-19 author box verbatim: the triage bridge stores
-- it unchanged, so its 'authorBoxId' is the canonical event-id, and a sender
-- can compute the thread-id at send time without any handshake.
--
-- Encryption, signing of the envelope, and the parts (attachments) are the
-- Mailbox layer's job; this module only produces and consumes the payload
-- bytes, which keeps it network-free and testable.
module HBS2.Hub.Letter
  ( MessageData(..)
  , MessageBody(..)
  , ReplyChannel(..)
  , AckRecord(..)
  , LetterError(..)
  , Disposition(..)
  , EnvelopeSigner(..)
  , hubMsgVersion
  , hubMsgWrite
  , hubMsgReadable
  , maxInlineBody
  , maxPartBytes
  , maxMessageParts
  , maxBoxBytes
  , maxPayloadBytes
  , maxTitle
  , maxAttrValue
  , maxAttrName
  , maxRef
  , maxLabel
  , maxLabels
  , textSize
  , oversizedField
  , malformedRef
  , noReplyChannel
  , sigilNames
  , makeLetter
  , makeAck
  , letterPayload
  , parsePayload
  , openLetterNoPolicy
  , openLetterAs
  , openAckNoPolicy
  , openAckFor
  , letterEventId
  , letterThreadId
  , classify
  , letterSyntax
  , contentSyntax
  , ackSyntax
  , sexpStr
  , Envelope(..)
  ) where

import HBS2.Hub.Types

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Data.Types.SignedBox
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil (Sigil(..))
import HBS2.Prelude.Plated (Pretty(..),(<+>))

import Data.Config.Suckless.Syntax

import Codec.Serialise (Serialise(..),serialise)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe,listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Char qualified as Char
import Numeric (showHex)
import Data.Word (Word32,Word64)
import GHC.Generics (Generic)

-- | Who signed the Mailbox envelope a letter arrived in.
--
-- A newtype rather than a bare key because the functions that take one also
-- take a 'RepoRef', and both are the same underlying type: swapped by
-- accident, the envelope check would run against the wrong key and the
-- reply channel would be silently dropped instead of honoured.
newtype EnvelopeSigner = EnvelopeSigner { fromEnvelopeSigner :: HubKey }
  deriving stock (Eq,Show)

-- | Schema version of the payload envelope: the @(hub-msg N)@ of PEP-18.
--
-- It lives here, not in 'AuthorContent', on purpose. An event-id is the hash
-- of the author box, so anything inside it is frozen forever; the envelope
-- bytes are not hashed, so a version field here stays cheap to bump and lets
-- an old reader answer "newer schema" instead of "malformed". Canon carries
-- its own version at the file and tree level (PEP-19).
--
-- The guarantee covers the whole payload, because the body is carried as
-- opaque bytes and decoded only after the version is checked: a v2 letter
-- gets 'UnsupportedVersion' even if its body shape is unknown here. What it
-- cannot cover is a v2 change INSIDE the inner box, which is signed content
-- this reader must hand to the fold verbatim; that is what the append-only
-- rule on 'AuthorContent' is for.
-- When a v2 appears this becomes a set of versions this build can decode,
-- not a single number: letters already sitting in mailboxes are v1, and a
-- v2 build that refuses them would strand them.
hubMsgVersion :: Word32
hubMsgVersion = 1

-- | The version this build WRITES.
--
-- Split from what it reads, and that is the whole of the change: one constant
-- answered both questions and was compared in five places, so a v2 build had to
-- edit five sites to keep reading v1 letters -- and the one that gets missed
-- costs nothing loudly. 'letterThreadId' would answer 'Nothing' for a perfectly
-- good v1 letter, @hub updates@ would correlate against nothing, and the exit
-- code would be zero.
--
-- Today it is the same number, so this changes no behaviour. What it changes is
-- that v2 edits ONE line to keep reading what is already in the mailboxes.
hubMsgWrite :: Word32
hubMsgWrite = hubMsgVersion

-- | Can this build decode a letter of that version?
--
-- The one place the answer lives. A v2 build makes this @v `elem` [1,2]@ and
-- everything that reads a letter follows.
hubMsgReadable :: Word32 -> Bool
hubMsgReadable v = v == hubMsgVersion

-- | The soft limit PEP-18 puts on an inline body, as a number.
--
-- @messageData@ is one secretbox over the whole payload, unchunked, and it
-- rides in a gossiped message, so an inline body is a cost every peer that
-- relays the mailbox pays. Above this the body belongs in a @body-part@
-- attachment, which is chunked and fetched only by whoever wants it.
--
-- Triage policy, not consensus: the fold admits an event of any size, and two
-- hubs may draw the line differently without disagreeing about canon. The
-- bridge enforces it because that is the last gate before permanence.
maxInlineBody :: Int
maxInlineBody = 32 * 1024

-- | And on one encrypted part, in bytes.
--
-- The inline limit above pushes anything substantial into an attachment, so
-- without a limit here there is no limit at all: what the sender saves on the
-- gossiped payload they spend on a tree that canon then references from inside
-- a signed author box, which means every clone that wants the thread keeps it
-- for as long as canon does. A part cannot be trimmed afterwards either, since
-- a new ciphertext has a new hash.
--
-- Triage policy like 'maxInlineBody', not consensus: two hubs may draw the line
-- differently without disagreeing about canon, and a part over the line is
-- parked rather than deleted.
maxPartBytes :: Word64
maxPartBytes = 64 * 1024 * 1024

-- | And on HOW MANY parts one letter may name.
--
-- The bound above is per part, which bounds nothing on its own: the count is
-- the sender's to choose, and the triage path walks every part a message names
-- before the bridge sees any of them. So a letter naming a hundred parts is a
-- hundred times whatever one part costs -- up to 'maxPartBytes' resident each
-- while the accept holds them, and 'maxPartBlocks' storage reads each while it
-- measures them, which is the cheaper attack of the two because a tree that
-- costs its whole walk budget is about a kilobyte to send.
--
-- The number is chosen from what a letter is FOR. PEP-18 gives a letter one
-- body part and PEP-20 gives a pull request one bundle; anything past a
-- handful is not a shape this format has. Sixteen leaves room for a shape
-- nobody has thought of yet and still bounds the walk at something a
-- maintainer waits through.
--
-- Triage policy like the two above, so a letter over it is PARKED and not
-- refused: canon says nothing about attachment counts, and a hub that raises
-- this later must be able to fold what it turned away today.
maxMessageParts :: Int
maxMessageParts = 16

-- | And on a title, which has no attachment form: it is an attribute of the
-- thread, rendered in every list. A title long enough to need chunking is a
-- body in the wrong field.
maxTitle :: Int
maxTitle = 512

-- | And on an attribute name or value. Wider than a title because a
-- multi-valued attribute is a list of keys ('encodeLabels'), and narrow enough
-- that an attribute cannot be used as a body.
maxAttrValue :: Int
maxAttrValue = 4 * 1024

-- | And on the name, which is a vocabulary word rather than a value: it is
-- rendered as a key, matched against a fixed list ('multiValued'), and has no
-- reason to be long. Sharing the value's bound let a three-kilobyte attribute
-- name through.
maxAttrName :: Int
maxAttrName = 128

-- | And on a git ref, sha or fork locator. These are identifiers, not prose,
-- and every one of them ends up in canon verbatim.
maxRef :: Int
maxRef = 512

-- | And on one label, and on how many an open may request. Labels are
-- advisory (the owner decides what to apply), so an unbounded list is free
-- storage for whoever sends it.
maxLabel :: Int
maxLabel = 128

maxLabels :: Int
maxLabels = 32

-- | The largest a signed box can get, given every limit above.
--
-- Derived, not chosen, and that is the whole point of it being here rather than
-- next to the reader that needs it. A reader has to bound what it will decode,
-- and a bound picked independently of the writer's limits is a reader that
-- refuses what its own writer produced: with a token bound of 4 KiB the bridge
-- minted a 32 KiB body, wrote the file, and then could not read it back, which
-- nothing found because every body in the fixtures was one character long.
--
-- The sum is deliberately loose. It adds the widest open (title, the full label
-- list, an inline body, coordinates) to the two fields only a @set@ carries,
-- rather than taking a maximum over the ops, and then adds four kilobytes for
-- the key, the signature, the domain tag, the hashes and the CBOR framing.
-- Being generous costs a reader nothing and being exact would have to be
-- recomputed every time a constructor changes.
-- Eight refs and not six: PEP-18's 'PartRef' puts a proof beside each of the
-- two parts an open can name, and a bound that counted the parts and not their
-- proofs would be a reader refusing what its own writer produced, which is the
-- failure this constant exists to record.
maxBoxBytes :: Int
maxBoxBytes = maxTitle + maxLabels * maxLabel + maxInlineBody + 8 * maxRef
            + maxAttrName + maxAttrValue
            + 4096

-- | The largest payload this reader will decode, derived like 'maxBoxBytes'
-- rather than chosen beside it.
--
-- A payload is the envelope, the inner box, and the reply channel. The inner box
-- is all of it that can be large; the rest is a version word, a key, a hash and
-- framing. The slack is generous on purpose, for the same reason it is generous
-- there: being exact would have to be recomputed every time a constructor
-- changes, and being generous costs a reader nothing.
maxPayloadBytes :: Int
maxPayloadBytes = maxBoxBytes + 4096

-- Refuse a payload over it before anything decodes it. 'MalformedPayload'
-- rather than a size of its own: to the sender it means the same thing, the
-- caller does the same thing with it, and a distinct constructor here would be
-- one more shape for a triage loop to route with nothing different to say.
boundedPayload :: Int -> Either LetterError ()
boundedPayload n
  | n > maxPayloadBytes = Left MalformedPayload
  | otherwise           = Right ()

-- | The size of a text field as it costs on the wire: UTF-8 bytes, not
-- characters. A limit about payload size has to be measured in payload.
textSize :: Text -> Int
textSize = BS.length . Text.encodeUtf8

-- | Which text field is over its limit, if any.
--
-- Returns the field name rather than a Bool so triage can tell the sender what
-- to move to an attachment.
oversizedField :: AuthorContent -> Maybe Text
oversizedField = \case
  AOpen _ _ title labels body _ coords _
    | textSize title > maxTitle        -> Just "title"
    | length labels > maxLabels        -> Just "labels"
    | any ((> maxLabel) . textSize) labels -> Just "label"
    | maybe False big body             -> Just "body"
    | otherwise                        -> maybe Nothing coordsOver coords
  AComment _ _ body _ _
    | maybe False big body      -> Just "body"
    | otherwise                 -> Nothing
  ARevise _ coords _            -> coordsOver coords
  AClose _ note _
    | maybe False big note      -> Just "note"
    | otherwise                 -> Nothing
  AReopen _ note _
    | maybe False big note      -> Just "note"
    | otherwise                 -> Nothing
  ASet _ k v _
    | textSize k > maxAttrName  -> Just "attr"
    | textSize v > maxAttrValue -> Just "value"
    | otherwise                 -> Nothing
  AMerge _ mc into _
    | textSize mc > maxRef      -> Just "merge-commit"
    | textSize into > maxRef    -> Just "merged-into"
    | otherwise                 -> Nothing
  -- The three with no text in them at all, spelled out rather than caught by a
  -- wildcard: a new constructor carrying prose must not arrive here as one
  -- carrying none. That is what makes the module's own
  -- -Werror=incomplete-patterns see it, and this is the last gate before a
  -- field is in every clone forever.
  ARedact{}                     -> Nothing
  ADelegate{}                   -> Nothing
  ARevoke{}                     -> Nothing
  where
    big = (> maxInlineBody) . textSize

    -- Five unbounded strings otherwise, and a revise is nothing but these.
    coordsOver c = listToMaybe
      [ name
      | (name, t) <- [ ("source", fromMaybe "" (prSource c))
                     , ("source-ref", prSourceRef c)
                     , ("source-tip", prSourceTip c)
                     , ("onto", prOnto c)
                     , ("base", prBase c)
                     ]
      , textSize t > maxRef
      ]

-- | Which hash-shaped field is not a hash, if any.
--
-- A 'HashRef' takes any length on the wire ('validHashRef'), and the size gates
-- above all measure text, so this was the way past every one of them: fifty
-- kilobytes in a @reply-to@ sits inside a signed author box, past 'maxBoxBytes',
-- and produces a canon file the reader refuses forever in every clone, with the
-- bytes inside the signature and inside the event-id where nothing can reach
-- them.
--
-- Every hash an event carries is checked, not only the one that was reachable.
-- Most of the others are compared against something that exists (a thread, a
-- redact target), so a wrong-length one merely fails to match; @reply-to@ is
-- deliberately compared against nothing, which is what left it open, and one
-- rule over all of them is easier to keep true than a list of exceptions.
-- The repository an op names is checked with the same rule, and for the same
-- reason: it is a key, keys take any length on the wire too, and the four ops
-- that carry one are the four whose target the fold compares against the owner.
-- A wrong-length one merely fails to match there, which would be a drop rather
-- than a refusal, and the file would already be written.
malformedRef :: AuthorContent -> Maybe Text
malformedRef = \case
  AOpen target _ _ _ _ part coords _ -> key "target" target
                               <|> named "body-part" part
                               <|> (coordsRefs =<< coords)
  AComment thr replyto _ part _ -> bad "thread" thr
                               <|> (bad "reply-to" =<< replyto)
                               <|> named "body-part" part
  ARevise thr coords _          -> bad "thread" thr <|> coordsRefs coords
  ASet thr _ _ _                -> bad "thread" thr
  AClose thr _ _                -> bad "thread" thr
  AReopen thr _ _               -> bad "thread" thr
  AMerge thr _ _ _              -> bad "thread" thr
  ARedact target e _            -> key "target" target <|> bad "redacts" e
  ADelegate target k _          -> key "target" target <|> key "delegate" k
  ARevoke target k _            -> key "target" target <|> key "revoke" k
  where
    key name k | validHubKey k = Nothing
               | otherwise     = Just name
    bad name h | validHashRef h = Nothing
               | otherwise      = Just name
    -- BOTH halves of a part reference. The proof is a hash off the same wire as
    -- the part, so it takes any width too, and it is compared against a computed
    -- value rather than against something that exists: a wrong-length one would
    -- simply never match, which is a refusal made in the wrong place and after
    -- the expensive part.
    partRef name p = bad name (ptPart p)
                       <|> bad (name <> "-proof") (proofRef (ptProof p))
    named name = (>>= partRef name)
    proofRef (PartProof h) = h
    coordsRefs c = named "bundle-part" (prBundle c)

-- | Where the owner should send acknowledgements and status updates.
--
-- This lives OUTSIDE the inner box: the inner box is published into public
-- canon verbatim, and nothing here needs to be. Its authenticity comes from the
-- outer Mailbox signature over the envelope.
--
-- WHAT THAT KEEPS OUT OF CANON IS THE SIGIL, NOT THE ADDRESS, and this comment
-- used to claim otherwise -- "a contributor's personal mailbox key must not end
-- up in every clone forever". It does end up there. 'vetted' below requires the
-- channel key to equal the inner author, and 'HBS2.Hub.CLI.Ack.sendAck'
-- addresses through a sigil whose own signer must be that same key, so the
-- mailbox this names IS the author key -- which canon publishes verbatim,
-- forever, because authorship is the point of publishing it.
--
-- So the honest statement is narrower. Anyone reading canon can NAME a
-- contributor's mailbox and watch its tree grow; what they cannot do is seal
-- anything to it, because that needs the encryption key, which is in the sigil,
-- which canon does not carry. Traffic is observable; delivery is not open.
--
-- And the leak is not one of identity: the author key already correlates a
-- person across every repository they have written in, and a separate reply key
-- would not change that. Only rotating the author key does.
--
-- DECOUPLING THE TWO IS A REAL DESIGN AND IT IS DEFERRED, not forgotten. It
-- means relaxing 'vetted' and re-deriving the anti-reflection property from a
-- proof: the channel would carry a signature by the reply key over the inner
-- box's hash, so naming a mailbox needs its private key, and the reply key
-- could then be one canon never sees. It is not a one-way door -- this type is
-- versioned by 'hubMsgWrite' and never reaches canon at all (@acReply@ feeds
-- 'sendAck' and nothing else), so the cost is a flag day for letters in flight
-- rather than for canon. It waits on the sigil being checked against its key at
-- INGRESS, which today happens only on the reply side: while the two keys must
-- be equal that is tolerable, and the moment they may differ it is load-bearing.
--
-- Entirely optional: a drive-by contributor who hosts no mailbox uses
-- 'noReplyChannel' and simply forgoes notifications. Threading never depends
-- on it, because the canonical thread-id is sender-computable.
--
-- A mailbox key alone is useless (a sign key is not an encryption key), so
-- the type pairs it with the sigil rather than allowing the invalid
-- half-specified state.
-- WIRE FORMAT, versioned by the envelope, under the same rule as
-- 'MessageBody': it is reachable from one, so a change here is a change to that
-- sum and has to move 'hubMsgWrite' with it.
data ReplyChannel =
    NoReply
  | ReplyTo HubKey HashRef   -- ^ mailbox key + a sigil resolving its encryption key
  deriving stock (Eq,Ord,Generic)

-- Ord is derived and prints nothing: it exists so a refusal carrying one can
-- be put in a map, and comparing two keys reveals neither.
--
-- Deliberately partial, for the reason this type exists: it holds a
-- contributor's personal mailbox key and sigil, which are kept out of the
-- signed letter precisely so they never reach canon. A derived instance puts
-- them in a log the first time anyone traces a refusal, which is the same
-- leak by a shorter route. 'Show Pending' and 'Show Accepted' are hand-written
-- next door for exactly this.
instance Show ReplyChannel where
  show NoReply    = "NoReply"
  show ReplyTo{}  = "ReplyTo <hidden>"

instance Serialise ReplyChannel

noReplyChannel :: ReplyChannel
noReplyChannel = NoReply

-- | Does this sigil name this key?
--
-- Three answers. 'Just True' is the sigil a reply channel claims; 'Just False'
-- is a sigil that resolves and names somebody else, which is the one worth
-- refusing; 'Nothing' is a sigil this node cannot read at all, either because
-- the block has not arrived or because it is not a sigil, and those two are the
-- same event -- nothing about the channel has been established either way.
--
-- HERE AND NOT BESIDE ITS CALLERS, because there are two of them on opposite
-- sides of the exchange and one rule between them. The hub asks it before
-- acking, so a maintainer's ack cannot be reflected at somebody the contributor
-- named; the contributor asks it before SENDING, because a mismatch is a letter
-- that folds perfectly and whose answer can never arrive, and the only machine
-- that can say so cheaply is the one composing it. It used to live in the hub's
-- half only, so the sender learned nothing and the send exited 0.
--
-- Pure, and takes the sigil rather than its hash, so the rule can be asked
-- questions without a storage. That is why it could not live next to
-- 'openLetterAs', which is where it belongs by subject.
sigilNames :: HubKey -> Maybe (Sigil HubScheme) -> Maybe Bool
sigilNames k msi = do
  si <- msi
  (owner, _) <- unboxSignedBox0 (sigilData si)
  pure (owner == k)

-- | A courtesy notification from the owner to a contributor: the number and
-- status the contributor could not compute themselves. Not canon, carries no
-- inner box, never folded. Trust comes from checking the envelope signer
-- against the repo's maintainer set ('openAckNoPolicy'); the authoritative status is
-- always in canon.
--
-- WIRE FORMAT, AND THE RULE HERE IS NOT 'AuthorContent'S. This travels inside
-- the versioned envelope, so it CAN change -- but only together with
-- 'hubMsgWrite'. Changing it without raising the version is not a compatible
-- addition: 'MessageBody' is a CBOR sum of fixed arity, so an old reader
-- answers 'MalformedPayload', which reads as "somebody sent me rubbish" rather
-- than "somebody sent me something newer". The difference is what an operator
-- does next, and it is the whole reason the envelope carries a version at all.
data AckRecord = AckRecord
  { akTarget      :: RepoRef
  , akThread      :: ThreadId
  , akNumber      :: Maybe Word64
  , akStatus      :: Text
  , akMergeCommit :: Maybe Text   -- ^ on a merged PR
    -- | Why, in the maintainer's own words, when the event that prompted this
    -- carried one: the note on a @close@ or a @reopen@.
    --
    -- Without it an acknowledgement can say a submission was closed and never
    -- why, and the contributor has to read canon to find a sentence that was
    -- written for them. It is not authority (canon is), and a reader that
    -- distrusts the ack distrusts this line with it.
  , akNote        :: Maybe Text
  }
  deriving stock (Eq,Show,Generic)

instance Serialise AckRecord

-- | The plaintext of @messageData@: a letter, or an acknowledgement.
--
-- WIRE FORMAT, versioned by the envelope. A CBOR sum has fixed arity and
-- positional constructor tags, so neither a new constructor nor a new field is
-- a compatible addition: an old reader cannot ignore what it does not know, it
-- fails the decode. That is survivable and is the design -- the body travels as
-- opaque bytes and the version is read first -- but ONLY if 'hubMsgWrite' moves
-- in the same change. Without it an old reader answers 'MalformedPayload',
-- which accuses the sender of rubbish instead of reporting a version skew, and
-- an operator acts on the difference.
data MessageBody =
    Letter (SignedBox AuthorContent HubScheme) ReplyChannel
  | Ack AckRecord
  deriving stock (Generic)

instance Serialise MessageBody

-- | The plaintext of @messageData@: a version plus the body.
--
-- On the wire this is two layers: an 'Envelope' carrying the version and the
-- body as opaque bytes, decoded in a second step. Encoding the body inline
-- would make the version useless in exactly the case it exists for, because
-- a single decode fails on an unknown body constructor before it ever reads
-- the version, and the reader reports "malformed" instead of "newer schema".
data MessageData = MessageData
  { mdVersion :: Word32
  , mdBody    :: MessageBody
  }

-- | The wire form: version plus not-yet-decoded body.
data Envelope = Envelope Word32 ByteString
  deriving stock (Generic)

instance Serialise Envelope

data LetterError =
    MalformedPayload      -- ^ bytes are not a MessageData
  | UnsupportedVersion Word32  -- ^ a newer schema than this reader knows
  | BadInnerSig           -- ^ the inner box does not verify: a forgery claim
    -- | The inner box is correctly signed but carries content this reader
    -- cannot decode, typically an op added by a newer schema. Distinct from
    -- 'BadInnerSig' so triage does not report a newer sender as a forger.
  | UndecodableContent HubKey UndecodableWhy
  | NotALetter            -- ^ an Ack where a Letter was expected
  | NotAnAck              -- ^ a Letter where an Ack was expected
    -- | A deny-listed author (triage, PEP-21). The INNER author, since that is
    -- the identity a ban is about and an envelope key is rewrapped away, except
    -- on a letter whose schema this build cannot parse: there is no inner
    -- author to name there, and the envelope signer is all there is
    -- ('openLetterAs'). So this one error has two subjects, and a deny-list
    -- built from inner authors alone will never fire on the second.
  | AuthorDenied
  | UntrustedAck          -- ^ the ack's envelope signer is not a maintainer
    -- | The ack is signed by a maintainer of the repo it names, but that
    -- (repo, thread) pair is not one this reader submitted. See 'openAckFor'.
  | UnrelatedAck
  deriving stock (Eq,Ord,Show)

-- | What a triage loop says about a letter it would not open.
instance Pretty LetterError where
  pretty = \case
    MalformedPayload       -> "not a letter payload"
    UnsupportedVersion v   -> "schema version" <+> pretty v <+> "is newer than this build"
    BadInnerSig            -> "the inner signature does not verify"
    UndecodableContent k w -> "content from" <+> pretty (AsBase58 k) <+> case w of
      Undecodable  -> "this build cannot decode"
      TrailingData -> "with bytes left over"
      WrongDomain  -> "signed as another kind of record"
    NotALetter             -> "an acknowledgement, where a letter was expected"
    NotAnAck               -> "a letter, where an acknowledgement was expected"
    AuthorDenied           -> "from a deny-listed key"
    UntrustedAck           -> "acknowledged by a key that is not a maintainer"
    UnrelatedAck           -> "about a thread this reader never submitted"

-- | What a letter's op can become once the owner triages it (PEP-18/PEP-19).
data Disposition =
    FoldsToCanon   -- ^ open/comment/revise: becomes canon as the sender's own
  | RequestOnly    -- ^ close/reopen/label(set): a request the owner may honour
  | OwnerNative    -- ^ merge/redact/delegate/revoke: never arrives as a letter
  deriving stock (Eq,Ord,Show)

-- | Classify by op. The fold enforces the same split by key (a stranger's
-- 'RequestOnly' event is dropped unless an authorized key authored it); this
-- is the sender/triage-side view of it.
classify :: AuthorContent -> Disposition
classify = \case
  AOpen{}     -> FoldsToCanon
  AComment{}  -> FoldsToCanon
  ARevise{}   -> FoldsToCanon
  AClose{}    -> RequestOnly
  AReopen{}   -> RequestOnly
  ASet{}      -> RequestOnly
  AMerge{}    -> OwnerNative
  ARedact{}   -> OwnerNative
  ADelegate{} -> OwnerNative
  ARevoke{}   -> OwnerNative

-- | Build a letter: sign the content into the inner box, attach the
-- transport-only back-channel.
makeLetter
  :: HubKey -> PrivKey 'Sign HubScheme
  -> AuthorContent
  -> ReplyChannel
  -> MessageData
makeLetter pk sk ac rc = MessageData hubMsgWrite (Letter (signAuthor pk sk ac) rc)

makeAck :: AckRecord -> MessageData
makeAck = MessageData hubMsgWrite . Ack

-- | The event-id this letter will have in canon: the hash of its inner box,
-- computable by the sender before delivery.
letterEventId :: MessageData -> Maybe EventId
letterEventId md
  | not (hubMsgReadable (mdVersion md)) = Nothing
  | otherwise = case mdBody md of
      Letter box _ -> Just (authorBoxId box)
      Ack _        -> Nothing

-- | The canonical thread this letter belongs to.
--
-- For an @open@ that is the letter's own event-id (the thread root); for a
-- reply it is the thread named inside the signed content, which is NOT the
-- reply's own id. Getting this wrong would break correlation against an
-- 'AckRecord', whose 'akThread' is always the thread.
letterThreadId :: MessageData -> Maybe ThreadId
letterThreadId md
  | not (hubMsgReadable (mdVersion md)) = Nothing
  | otherwise = case mdBody md of
      Ack a -> Just (akThread a)
      Letter box _ -> do
        (_, ac) <- either (const Nothing) Just (unboxChecked box)
        pure (fromMaybe (authorBoxId box) (authorThread ac))

-- | Serialise to the bytes that go into @messageData@ before encryption.
letterPayload :: MessageData -> ByteString
letterPayload (MessageData v b) =
  LBS.toStrict (serialise (Envelope v (LBS.toStrict (serialise b))))

-- | Parse the decrypted @messageData@ bytes, rejecting a schema this reader
-- does not know rather than reporting it as malformed.
parsePayload :: ByteString -> Either LetterError MessageData
parsePayload bs = do
  -- Before the decode, because the decode is what the bound protects. These are
  -- the first bytes of a stranger's message that this build touches, and until
  -- now nothing stood between "a peer relayed a payload" and "CBOR allocates
  -- whatever the length prefix says". The bound is the letter layer's own
  -- ('maxPayloadBytes'), so it admits everything this module will produce.
  boundedPayload (BS.length bs)
  Envelope v body <- strictDecode bs
  -- Version first: a newer schema is reported as such even when its body is
  -- something this reader cannot decode at all. Any version other than the
  -- one this build speaks is unsupported, including a lower one: an older
  -- schema is no more decodable here than a newer one, and treating an
  -- unknown small number as "close enough to v1" is how a reader ends up
  -- interpreting bytes it does not understand.
  if not (hubMsgReadable v)
    then Left (UnsupportedVersion v)
    else MessageData v <$> strictDecode body

-- | Decode requiring the whole input consumed, so appended bytes cannot ride
-- along on a letter that still parses. One rule for every decode in the hub
-- ('decodeStrict').
strictDecode :: Serialise a => ByteString -> Either LetterError a
strictDecode = maybe (Left MalformedPayload) Right . decodeStrict

-- | Verify and open a letter: recover the author key, the content it signed,
-- and the back-channel. The inner box is returned too, because the triage
-- bridge must store it verbatim as the canon author box (re-signing would
-- change the event-id and destroy the authorship proof).
--
-- The signature is checked here; nothing else is. The deny-list and the
-- reply-channel rule live in 'openLetterAs', and a caller that reaches for this
-- one gets a verified letter that policy has never seen: a banned author's box
-- rewrapped under a fresh envelope opens fine, and a reply channel naming a
-- stranger's mailbox comes back as though the sender owned it.
--
-- Named for what it is missing rather than for what it does, because the name
-- is the only thing between a caller and that. Triage wants 'openLetterAs'.
openLetterNoPolicy
  :: MessageData
  -> Either LetterError (SignedBox AuthorContent HubScheme, HubKey, AuthorContent, ReplyChannel)
openLetterNoPolicy md
  -- The constructor is exported, so a MessageData can reach here without
  -- passing parsePayload; check the version rather than trust that it did.
  | not (hubMsgReadable (mdVersion md)) = Left (UnsupportedVersion (mdVersion md))
  | otherwise = case mdBody md of
      Ack _ -> Left NotALetter
      Letter box rc ->
        case unboxChecked box of
          Left BoxBadSig      -> Left BadInnerSig
          Left (BoxUndecodable k why) -> Left (UndecodableContent k why)
          Right (pk , ac)     -> Right (box, pk, ac, rc)

-- | Triage-side open, applying the two policy rules the peer layer cannot
-- (PEP-18 "Replay, rewrap, and deduplication"):
--
--   * the deny-list is checked against the INNER author, since a banned
--     author's box can be rewrapped under a fresh envelope key; and
--   * the reply channel is honoured only when the envelope signer is the
--     inner author AND the channel names that same key's mailbox, because a
--     rewrapper can substitute their own and a sender can name a stranger's.
--
-- WHO CAN REWRAP, exactly, and it is a wider set than PEP-18 assumes. The spec
-- reads as though rewrapping is available to a recipient -- somebody who
-- decrypted the letter and re-sent it. It is available to ANYONE WHO SAW THE
-- CIPHERTEXT: 'SignedBox' keeps its payload as opaque bytes and
-- @MessageContent@ has no sender field, so re-signing a captured envelope under
-- a fresh key needs no key of the attacker's and no plaintext.
--
-- What that does and does not reach. Canon is not at risk: an event-id is the
-- hash of the inner box, so a second copy is refused as @AlreadyInCanon@, and a
-- request honoured twice is caught by the honours-id, which is the request's own
-- box. What IS reached is everything a person does -- one queue line, one
-- decision and one tombstone per copy -- and 'copiesOf' in "HBS2.Hub.Ingress" is
-- the answer to that: the ciphertext is what a rewrap cannot change, so copies
-- group without any key at all.
--
-- WHAT STAYS TRUE AND IS WORTH SAYING OUT LOUD: the only key every copy shares
-- is the INNER AUTHOR's, so the only ban that stops a flood of rewraps is a ban
-- on the person whose letter was captured. That is a property of the protocol
-- and not a gap in this function. @hub block@ names the envelope key, and the
-- rewrapper mints a fresh one per copy. Pricing the flood is @(pow D)@, which is
-- zero unless somebody sets it (see @hub policy pow@).
openLetterAs
     -- | May a letter from this key be folded? Asked about the inner author,
     -- and about the envelope signer when there is no inner author to ask
     -- about (see 'AuthorDenied').
  :: (HubKey -> Bool)
  -> EnvelopeSigner     -- ^ who signed the Mailbox envelope
  -> MessageData
  -> Either LetterError (SignedBox AuthorContent HubScheme, HubKey, AuthorContent, ReplyChannel)
openLetterAs allowed (EnvelopeSigner envelopeSigner) md =
  case openLetterNoPolicy md of
    -- The version is checked before the body, so for a letter from a schema
    -- this build cannot parse there is no inner author to ban: the only key
    -- in hand is the envelope's. PEP-21 is right that an envelope ban is
    -- evaded by rewrapping, but here it is all there is, and without it a
    -- version number and a few bytes of garbage are the cheapest way to grow
    -- the parked set.
    Left (UnsupportedVersion v)
      | not (allowed envelopeSigner) -> Left AuthorDenied
      | otherwise                    -> Left (UnsupportedVersion v)
    -- The deny-list has to reach the undecodable case too. Such a letter is
    -- retried rather than discarded, so without this a banned author whose
    -- content this build cannot read is retried forever with no way out: the
    -- key is known, and the ban is the only mechanism that would stop it.
    Left (UndecodableContent k why)
      | not (allowed k) -> Left AuthorDenied
      | otherwise       -> Left (UndecodableContent k why)
    Left e -> Left e
    Right (box, author, ac, rc)
      | not (allowed author) -> Left AuthorDenied
      | otherwise -> pure (box, author, ac, vetted author rc)
  where
    -- Two separate conditions, and only the first was checked. The envelope
    -- signature says who put this letter on the wire, which is why a rewrapper
    -- must not keep the channel. It says nothing about whose mailbox the
    -- channel NAMES, and PEP-18 requires the sender's own: without that, one
    -- key can send N letters naming a stranger's mailbox and the hub becomes a
    -- reflector, turning each into a maintainer-signed ack delivered to
    -- someone who asked for none of it.
    --
    -- ONLY THE KEY IS CHECKED HERE, and that is a limit of this function rather
    -- than the whole of the rule. A channel is a key and a SIGIL HASH, and the
    -- sigil is what actually addresses the message -- resolveKeys takes the
    -- recipient's sign key out of the sigil's own signed box, not from the key
    -- beside it. Deciding whether the sigil belongs to the key means reading
    -- the sigil, which needs a storage, which this cannot have. So the other
    -- half is 'HBS2.Hub.CLI.Ack.sigilNames', applied where the ack is sent, and
    -- a channel arriving from here is vetted only as far as its key.
    vetted author = \case
      ReplyTo k sig | k == author, k == envelopeSigner -> ReplyTo k sig
      _                                                -> NoReply

-- | Open an ack. It carries no inner box, so the only trust available is the
-- envelope signer being a current maintainer of the repo; the authoritative
-- status is in canon regardless.
-- The predicate takes the repo as well as the key: which maintainer set
-- applies is only known once the ack says what it is about, and the caller
-- cannot be asked to decide that before reading it.
--
-- What this does NOT establish, and the caller must: that the thread belongs
-- to the repo the ack names. The check here is "a maintainer of X signed an
-- ack about X", which any maintainer of any repo passes for their own repo
-- while naming a thread in someone else's. Prefer 'openAckFor', which closes
-- that. Note also that an ack carries no clock and no counter, so an old one
-- replays verbatim; nothing here dedups, and canon is what decides status.
openAckNoPolicy
  :: (RepoRef -> HubKey -> Bool)  -- ^ is this key a maintainer of that repo?
  -> EnvelopeSigner               -- ^ who signed the Mailbox envelope
  -> MessageData
  -> Either LetterError AckRecord
openAckNoPolicy isMaintainer (EnvelopeSigner envelopeSigner) md
  | not (hubMsgReadable (mdVersion md)) = Left (UnsupportedVersion (mdVersion md))
  | otherwise = case mdBody md of
      Letter{} -> Left NotAnAck
      Ack a
        | isMaintainer (akTarget a) envelopeSigner -> Right a
        | otherwise                                -> Left UntrustedAck

-- | Text as an S-expression string literal, escaped so that it reads back.
--
-- The printer wraps a literal in quotes and does nothing else, so a title
-- containing one is not a display wart: everything after it is re-read as
-- whatever it happens to look like. PEP-19 puts this same projection in the
-- event FILE, next to the two authoritative boxes, and a title comes from
-- whoever sent the letter. One quote would make that file unparseable, or
-- worse, parseable as something else, permanently and with valid signatures
-- inside it.
--
-- The escapes are the ones the reader understands (it un-escapes with
-- 'Prelude.readLitChar'), so this is the inverse of the parse, not a
-- best-effort sanitizer: nothing is dropped and nothing is replaced.
--
-- Every other control byte is escaped too, and that one is not about parsing.
-- A canon file is read with @git show@ and @cat@ far more often than with this
-- module, and a raw ESC in a body is a terminal escape sequence: a title can
-- reposition the cursor, clear the line, and rewrite what a maintainer thinks
-- they are looking at while they decide whether to sign it. Written as @\\xNN@
-- the same bytes still read back byte for byte and print as themselves.
--
-- The trailing @\\&@ is the empty escape, and it is load-bearing: a numeric
-- escape swallows any hex digit that follows it, so a body of @ESC@ then @5@
-- would otherwise read back as one character U+1B5 and the file would say
-- something the letter did not.
sexpStr :: Text -> Syntax C
sexpStr = mkStr @C . Text.unpack . Text.concatMap esc
  where
    esc = \case
      '\\' -> "\\\\"
      '"'  -> "\\\""
      -- The three that have a short spelling keep it: they are most of what a
      -- body contains, and a mnemonic escape cannot run into what follows it.
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      -- 'invisible', not 'Char.isControl'. isControl is category Cc and nothing
      -- else, which is the fact "HBS2.Hub.Types" writes down about itself two
      -- hundred lines from here and which this function did not act on: a RIGHT-
      -- TO-LEFT OVERRIDE (Cf), a LINE SEPARATOR (Zl), a ZERO WIDTH SPACE (Cf) and
      -- a NO-BREAK SPACE (Zs) all went into the file raw. That is worse here than
      -- in a report, because this is what 'renderEvent' writes into canon and
      -- canon is what a maintainer reads with `git show` before deciding whether
      -- to sign -- the exact reading the paragraph above says a raw escape can
      -- rewrite. The two escapers now ask one question and differ only in how
      -- they spell the answer.
      --
      -- It costs nothing to widen: @\\xNNNN\\&@ reads back for any code point,
      -- so the round trip this function exists for is unaffected.
      c | invisible c -> hex c
        | otherwise   -> Text.singleton c

    -- justifyRight PADS and does not truncate, so a code point above U+00FF
    -- keeps all of its digits; the two is a minimum for the C0 controls that
    -- were the only thing this used to see.
    hex c = "\\x" <> Text.justifyRight 2 '0' (Text.pack (showHex (Char.ord c) "")) <> "\\&"

-- Shared by both projections below.
b58 :: HubKey -> Syntax C
b58 x = mkSym @C (show (pretty (AsBase58 x)))

href :: HashRef -> Syntax C
href h = mkSym @C (show (pretty h))

txt :: Text -> Syntax C
txt = sexpStr

-- An attribute NAME. A string like the value, not a bare symbol, and that is
-- the whole of the fix: a name comes from whoever sent the letter, the fold
-- admits one that is not a vocabulary word (it only reports it), and this
-- projection is what the canon file carries beside the two boxes. A name of
-- @a)(canon-box@ written bare puts a clause of the attacker's choosing into
-- that file, and a name with a quote in it makes the file unparseable, with two
-- valid signatures inside it and no way to rewrite them.
tsym :: Text -> Syntax C
tsym = sexpStr

-- | Open an ack and bind it to something this reader actually sent.
--
-- The second predicate answers what 'openAckNoPolicy' cannot: is this (repo, thread)
-- pair mine? The thread-id is the hash of an author box, so nothing in the ack
-- proves which repo the thread was opened against, and a reader correlating by
-- thread alone would show a stranger's status on its own submission
-- (@hub updates@, PEP-22). A sender can answer it, though, because it computed
-- the thread-id itself before delivery ('letterThreadId').
openAckFor
  :: (RepoRef -> HubKey -> Bool)    -- ^ is this key a maintainer of that repo?
  -> (RepoRef -> ThreadId -> Bool)  -- ^ did I submit that thread to that repo?
  -> EnvelopeSigner                 -- ^ who signed the Mailbox envelope
  -> MessageData
  -> Either LetterError AckRecord
openAckFor isMaintainer isMine signer md = do
  a <- openAckNoPolicy isMaintainer signer md
  if isMine (akTarget a) (akThread a) then Right a else Left UnrelatedAck

-- | The readable S-expression projection of an acknowledgement (PEP-18
-- "Acknowledgement letter"). Pinned because it is what a contributor's
-- tooling reads (@hub updates@, PEP-22), and it has no @op@: an ack asserts
-- nothing the contributor authored.
--
-- Display only, like 'letterSyntax': the wire form is the CBOR 'AckRecord'.
ackSyntax :: AckRecord -> [Syntax C]
ackSyntax a =
  [ mkForm "hub-msg" [mkInt hubMsgWrite]
  , mkForm "kind"   [mkSym @C "ack"]
  , mkForm "target" [b58 (akTarget a)]
  , mkForm "thread" [href (akThread a)]
  ]
  <> [ mkForm "number" [mkInt n] | Just n <- [akNumber a] ]
  -- A string, not a symbol: the status is an LWW attribute the owner sets,
  -- so it is whatever text they signed, not a fixed vocabulary.
  <> [ mkForm "status" [txt (akStatus a)] ]
  <> [ mkForm "merge-commit" [txt c] | Just c <- [akMergeCommit a] ]
  <> [ mkForm "note" [txt n] | Just n <- [akNote a] ]

-- | The readable S-expression projection of a letter (PEP-18). It is
-- regenerated from the decoded content and never signed or parsed back: the
-- authoritative form is the binary inner box. Used by @hub show@ and for
-- debugging.
letterSyntax :: AuthorContent -> [Syntax C]
letterSyntax ac = mkForm "hub-msg" [mkInt hubMsgWrite] : contentSyntax ac

-- | The same projection without the envelope clause.
--
-- Shared with the canon event file (PEP-19), which carries these very clauses
-- beside the authoritative boxes: one projection, so a reader looking at a
-- letter and at the event it became reads the same words, and one place where
-- a field can be forgotten.
contentSyntax :: AuthorContent -> [Syntax C]
contentSyntax = body
  where
    kindOf :: HubKind -> Syntax C
    kindOf = \case
      HubIssue -> mkSym @C "issue"
      HubPR    -> mkSym @C "pr"

    op :: String -> Syntax C
    op o = mkForm "op" [mkSym @C o]

    -- Strings, not symbols: a label may contain a space.
    labelsOf :: [Text] -> [Syntax C]
    labelsOf ls = [ mkForm "labels" (fmap txt ls) | not (null ls) ]

    bodyOf :: Maybe Text -> [Syntax C]
    bodyOf mb = [ mkForm "body" [txt t] | Just t <- [mb] ]

    -- The proof as a second atom, because the projection is what a person
    -- reads canon with and a field inside the signed box that the readable form
    -- omits is a field nobody ever looks at. It is not what the check runs on:
    -- the box is.
    partOf :: Maybe PartRef -> [Syntax C]
    partOf mp = [ mkForm "body-part" (partAtoms p) | Just p <- [mp] ]

    partAtoms :: PartRef -> [Syntax C]
    partAtoms p = [ href (ptPart p), href (proofHash (ptProof p)) ]

    proofHash (PartProof h) = h

    coordsOf :: PRCoords -> [Syntax C]
    coordsOf c =
      [ mkForm "source" [txt src] | Just src <- [prSource c] ]
      <> [ mkForm "source-ref" [txt (prSourceRef c)]
         , mkForm "source-tip" [txt (prSourceTip c)]
         , mkForm "onto"       [txt (prOnto c)]
         , mkForm "base"       [txt (prBase c)]
         ]
      <> [ mkForm "bundle-part" (partAtoms p) | Just p <- [prBundle c] ]

    body :: AuthorContent -> [Syntax C]
    body = \case
      AOpen target kind title labels mb mp mpr ts ->
        [ mkForm "kind" [kindOf kind], op "open"
        , mkForm "target" [b58 target]
        , mkForm "title" [txt title]
        , mkForm "created" [mkInt ts]
        ] <> labelsOf labels <> bodyOf mb <> partOf mp <> maybe [] coordsOf mpr

      AComment thr mrep mb mp ts ->
        [ op "comment", mkForm "thread" [href thr], mkForm "created" [mkInt ts] ]
        <> [ mkForm "reply-to" [href e] | Just e <- [mrep] ]
        <> bodyOf mb <> partOf mp

      ARevise thr c ts ->
        [ op "revise", mkForm "thread" [href thr], mkForm "created" [mkInt ts] ]
        <> coordsOf c

      ASet thr k v ts ->
        [ op "set", mkForm "thread" [href thr]
        , mkForm "set" [tsym k, txt v], mkForm "created" [mkInt ts] ]

      AClose thr mb ts ->
        [ op "close", mkForm "thread" [href thr], mkForm "created" [mkInt ts] ]
        <> bodyOf mb

      AReopen thr mb ts ->
        [ op "reopen", mkForm "thread" [href thr], mkForm "created" [mkInt ts] ]
        <> bodyOf mb

      AMerge thr mc into ts ->
        [ op "merge", mkForm "thread" [href thr]
        , mkForm "merge-commit" [txt mc], mkForm "merged-into" [txt into]
        , mkForm "created" [mkInt ts] ]

      ARedact target eid ts ->
        [ op "redact", mkForm "target" [b58 target]
        , mkForm "redacts" [href eid], mkForm "created" [mkInt ts] ]

      ADelegate target k ts ->
        [ op "delegate", mkForm "target" [b58 target]
        , mkForm "delegate" [b58 k], mkForm "created" [mkInt ts] ]

      ARevoke target k ts ->
        [ op "revoke", mkForm "target" [b58 target]
        , mkForm "revoke" [b58 k], mkForm "created" [mkInt ts] ]
