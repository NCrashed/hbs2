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
  , maxInlineBody
  , maxPartBytes
  , maxTitle
  , maxAttrValue
  , maxAttrName
  , maxRef
  , maxLabel
  , maxLabels
  , textSize
  , oversizedField
  , noReplyChannel
  , makeLetter
  , makeAck
  , letterPayload
  , parsePayload
  , openLetter
  , openLetterAs
  , openAck
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
import HBS2.Prelude.Plated (Pretty(..))

import Data.Config.Suckless.Syntax

import Codec.Serialise (Serialise(..),serialise)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe,listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
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

-- | Where the owner should send acknowledgements and status updates.
--
-- This lives OUTSIDE the inner box on purpose: the inner box is published
-- into public canon verbatim, and a contributor's personal mailbox key must
-- not end up in every clone forever. Its authenticity comes from the outer
-- Mailbox signature over the envelope.
--
-- Entirely optional: a drive-by contributor who hosts no mailbox uses
-- 'noReplyChannel' and simply forgoes notifications. Threading never depends
-- on it, because the canonical thread-id is sender-computable.
--
-- A mailbox key alone is useless (a sign key is not an encryption key), so
-- the type pairs it with the sigil rather than allowing the invalid
-- half-specified state.
data ReplyChannel =
    NoReply
  | ReplyTo HubKey HashRef   -- ^ mailbox key + a sigil resolving its encryption key
  deriving stock (Eq,Generic)

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

-- | A courtesy notification from the owner to a contributor: the number and
-- status the contributor could not compute themselves. Not canon, carries no
-- inner box, never folded. Trust comes from checking the envelope signer
-- against the repo's maintainer set ('openAck'); the authoritative status is
-- always in canon.
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
  deriving stock (Eq,Show)

-- | What a letter's op can become once the owner triages it (PEP-18/PEP-19).
data Disposition =
    FoldsToCanon   -- ^ open/comment/revise: becomes canon as the sender's own
  | RequestOnly    -- ^ close/reopen/label(set): a request the owner may honour
  | OwnerNative    -- ^ merge/redact/delegate/revoke: never arrives as a letter
  deriving stock (Eq,Show)

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
makeLetter pk sk ac rc = MessageData hubMsgVersion (Letter (signAuthor pk sk ac) rc)

makeAck :: AckRecord -> MessageData
makeAck = MessageData hubMsgVersion . Ack

-- | The event-id this letter will have in canon: the hash of its inner box,
-- computable by the sender before delivery.
letterEventId :: MessageData -> Maybe EventId
letterEventId md
  | mdVersion md /= hubMsgVersion = Nothing
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
  | mdVersion md /= hubMsgVersion = Nothing
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
  Envelope v body <- strictDecode bs
  -- Version first: a newer schema is reported as such even when its body is
  -- something this reader cannot decode at all. Any version other than the
  -- one this build speaks is unsupported, including a lower one: an older
  -- schema is no more decodable here than a newer one, and treating an
  -- unknown small number as "close enough to v1" is how a reader ends up
  -- interpreting bytes it does not understand.
  if v /= hubMsgVersion
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
-- This is the plain form, with no policy applied; triage should prefer
-- 'openLetterAs'.
openLetter
  :: MessageData
  -> Either LetterError (SignedBox AuthorContent HubScheme, HubKey, AuthorContent, ReplyChannel)
openLetter md
  -- The constructor is exported, so a MessageData can reach here without
  -- passing parsePayload; check the version rather than trust that it did.
  | mdVersion md /= hubMsgVersion = Left (UnsupportedVersion (mdVersion md))
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
openLetterAs
     -- | May a letter from this key be folded? Asked about the inner author,
     -- and about the envelope signer when there is no inner author to ask
     -- about (see 'AuthorDenied').
  :: (HubKey -> Bool)
  -> EnvelopeSigner     -- ^ who signed the Mailbox envelope
  -> MessageData
  -> Either LetterError (SignedBox AuthorContent HubScheme, HubKey, AuthorContent, ReplyChannel)
openLetterAs allowed (EnvelopeSigner envelopeSigner) md =
  case openLetter md of
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
openAck
  :: (RepoRef -> HubKey -> Bool)  -- ^ is this key a maintainer of that repo?
  -> EnvelopeSigner               -- ^ who signed the Mailbox envelope
  -> MessageData
  -> Either LetterError AckRecord
openAck isMaintainer (EnvelopeSigner envelopeSigner) md
  | mdVersion md /= hubMsgVersion = Left (UnsupportedVersion (mdVersion md))
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
sexpStr :: Text -> Syntax C
sexpStr = mkStr @C . Text.unpack . Text.concatMap esc
  where
    esc = \case
      '\\' -> "\\\\"
      '"'  -> "\\\""
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      c    -> Text.singleton c

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
-- The second predicate answers what 'openAck' cannot: is this (repo, thread)
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
  a <- openAck isMaintainer signer md
  if isMine (akTarget a) (akThread a) then Right a else Left UnrelatedAck

-- | The readable S-expression projection of an acknowledgement (PEP-18
-- "Acknowledgement letter"). Pinned because it is what a contributor's
-- tooling reads (@hub updates@, PEP-22), and it has no @op@: an ack asserts
-- nothing the contributor authored.
--
-- Display only, like 'letterSyntax': the wire form is the CBOR 'AckRecord'.
ackSyntax :: AckRecord -> [Syntax C]
ackSyntax a =
  [ mkForm "hub-msg" [mkInt hubMsgVersion]
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
letterSyntax ac = mkForm "hub-msg" [mkInt hubMsgVersion] : contentSyntax ac

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

    partOf :: Maybe HashRef -> [Syntax C]
    partOf mp = [ mkForm "body-part" [href h] | Just h <- [mp] ]

    coordsOf :: PRCoords -> [Syntax C]
    coordsOf c =
      [ mkForm "source" [txt src] | Just src <- [prSource c] ]
      <> [ mkForm "source-ref" [txt (prSourceRef c)]
         , mkForm "source-tip" [txt (prSourceTip c)]
         , mkForm "onto"       [txt (prOnto c)]
         , mkForm "base"       [txt (prBase c)]
         ]
      <> [ mkForm "bundle-part" [href h] | Just h <- [prBundle c] ]

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

      ARedact target ts ->
        [ op "redact", mkForm "redacts" [href target], mkForm "created" [mkInt ts] ]

      ADelegate k ts ->
        [ op "delegate", mkForm "delegate" [b58 k], mkForm "created" [mkInt ts] ]

      ARevoke k ts ->
        [ op "revoke", mkForm "revoke" [b58 k], mkForm "created" [mkInt ts] ]
