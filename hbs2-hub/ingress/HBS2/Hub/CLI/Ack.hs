-- | The courtesy notification back to a contributor (PEP-18 "back-channel").
--
-- A letter goes into a mailbox and nothing comes back: the sender cannot see
-- canon until they fetch it, cannot tell "not folded yet" from "refused", and
-- has no number to refer to. The ack closes that, and closes it as a courtesy
-- and not as authority: it carries no inner box, is never folded, and canon is
-- what a reader believes if the two ever disagree.
--
-- WHO MAY SEND ONE IS NOT DECIDED HERE. The reading side takes an ack seriously
-- only when its envelope signer is a maintainer of the repository it names
-- ('openAckNoPolicy'), so what makes this trustworthy is that the CANON key
-- signs it -- the same key that signed the event being acked. A hub that signed
-- acks with its mailbox key would produce notifications no reader could check.
--
-- WHERE IT GOES IS THE SENDER'S OWN CHOICE, and this module checks the half of
-- that nobody else can. PEP-18 honours a reply channel only when it names the
-- inner author's own mailbox AND the envelope signer is that same key.
-- 'openLetterAs' establishes the second half and the KEY of the first, and it
-- can do no more, because it is pure: a channel is a key and a SIGIL HASH, and
-- deciding whether the sigil belongs to the key means reading the sigil.
--
-- The sigil is the half that matters, because the sigil is what addresses the
-- message: 'resolveKeys' takes BOTH the recipient's sign key and its encryption
-- key out of the sigil's own signed box, and the key beside it is never
-- consulted. So a letter naming its own author and a VICTIM'S sigil passed the
-- key check and had the hub deliver a canon-signed message to the victim, one
-- per accepted letter, from an address they cannot easily ban because it is the
-- repository's. That is a hub turned into a reflector, which is exactly what
-- PEP-18 says the pair exists to prevent.
--
-- So the sigil is loaded here and refused unless it names the key that asked.
-- Which is also why the refusal is not a failure of the accept: see 'AckTrouble'.
--
-- A SIGIL MADE ON THE SPOT. Sealing a message needs the SENDER's encryption
-- key, which a sigil carries, and a hub has no reason to have published a sigil
-- for its canon key. So one is built in memory from the credentials the accept
-- already loaded. It is never stored: what the contributor needs to check the
-- ack is the envelope signer, which travels with the message.
module HBS2.Hub.CLI.Ack
  ( sendAck
  , sigilNames
  , ackTarget
  , AckTrouble(..)
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Ingress (PeerSilent(..))
import HBS2.Hub.CLI.Compose (Outbound(..),sendPayload,NotStored(..),PoWTooHard(..))

import HBS2.CLI.Prelude

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef)
import HBS2.Data.Types.SignedBox (unboxSignedBox0)
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil (Sigil(..),loadSigil,makeSigilFromCredentials)

import Data.Text qualified as Text

-- | Why the contributor was not told.
--
-- None of these is a failure of the accept that prompted it: by the time an ack
-- is attempted the event is in canon, so every one of them is a notification
-- that did not happen and nothing more.
data AckTrouble =
    -- | The letter asked for no acknowledgement, or asked for one this build
    -- would not honour (a rewrapped envelope, a channel whose key is not the
    -- author's). That vetting is 'openLetterAs' and the answer arrives here as
    -- 'NoReply', so the two cases are indistinguishable on purpose: neither is
    -- a reason to do anything.
    AckNotAsked
    -- | The channel's sigil does not belong to the key that asked for the
    -- reply.
    --
    -- Its OWN answer, and not folded into 'AckNotAsked', because the two are
    -- different events: one is a contributor who wanted no notification, the
    -- other is a letter trying to have this hub deliver a canon-signed message
    -- to somebody who asked for nothing. The second is worth a line in a log.
  | AckWrongSigil HubKey HashRef
    -- | The signing credentials carry no encryption key, so no sigil can be
    -- made for them and nothing can be sealed FROM this identity.
  | AckNoEncKey HubKey
    -- | The peer would not take it, would not answer, or the recipient's sigil
    -- could not be resolved.
  | AckNotSent Text
  deriving stock (Eq,Show)

instance Pretty AckTrouble where
  pretty = \case
    AckNotAsked -> "the letter asked for no acknowledgement"
    AckWrongSigil k h ->
      "the reply sigil" <+> hashDoc h <+> "does not belong to"
        <+> keyDoc k <> line
        <> "  nothing was sent: that sigil addresses somebody else's mailbox"
    AckNoEncKey k ->
      "no encryption key for" <+> pretty (AsBase58 k) <> line
        <> "  an ack is sealed to its reader, so the signer needs one too"
    AckNotSent e -> "the acknowledgement was not sent:" <+> pretty (safeText e)

-- | Does this sigil name this key?
--
-- Three answers. 'Just True' is the sigil the reply channel claims; 'Just
-- False' is a sigil that resolves and names somebody else, which is the one
-- worth refusing; 'Nothing' is a sigil this node cannot read at all, either
-- because the block has not arrived or because it is not a sigil, and those two
-- are the same event here -- nothing about the channel has been established
-- either way.
--
-- Pure, and takes the sigil rather than its hash, so that the rule can be
-- asked questions without a storage: the reason this check did not exist is
-- that its natural home ('openLetterAs') is pure and could not load one.
sigilNames :: HubKey -> Maybe (Sigil HubScheme) -> Maybe Bool
sigilNames k msi = do
  si <- msi
  (owner, _) <- unboxSignedBox0 (sigilData si)
  pure (owner == k)

-- | Where an ack goes, given a vetted channel and what reading its sigil said.
--
-- The whole decision, as a function, so that a test can ask it rather than
-- infer it from a send that needs a peer. It is three lines and it was three
-- lines inside 'sendAck', where the case that matters -- 'Just False' -- was
-- reachable by no test in this repository.
--
-- 'Nothing' PROCEEDS, and that is deliberate: a sigil this node cannot read yet
-- is not evidence of anything, and the send below fails on its own with the
-- reason ('AckNotSent'), which is a better answer than an accusation. Only a
-- sigil that resolves and names somebody else is refused here.
ackTarget :: ReplyChannel -> Maybe Bool -> Either AckTrouble HashRef
ackTarget NoReply           _          = Left AckNotAsked
ackTarget (ReplyTo k sigil) (Just False) = Left (AckWrongSigil k sigil)
ackTarget (ReplyTo _ sigil) _            = Right sigil

-- | Tell the contributor what happened to their letter.
--
-- Answers rather than throws, and every caller so far is a verb that has
-- already published something: a notification that failed must not turn a
-- completed accept into a non-zero exit.
sendAck :: forall m . MonadUnliftIO m
        => Outbound
        -> HubKey                     -- ^ the canon key: signs the envelope
        -> PeerCredentials HBS2Basic  -- ^ its credentials, already loaded
        -> ReplyChannel               -- ^ vetted, from 'openLetterAs'
        -> AckRecord
        -> m (Either AckTrouble HashRef)
sendAck ob canonKey creds reply rec = do
  owned <- case reply of
             NoReply        -> pure Nothing
             ReplyTo k href -> sigilNames k <$> loadSigil @HubScheme (obStorage ob) href

  case ackTarget reply owned of
    Left e -> pure (Left e)

    -- The FIRST keyring entry, and the choice is arbitrary because nothing
    -- about it can be checked from here: an identity may hold several
    -- encryption keys and none of them is marked. The reader does not care --
    -- what it checks is the SIGN key on the envelope -- so any key that can
    -- seal will do.
    Right sigil -> case [ _krPk e | e <- _peerKeyring creds ] of
      [] -> pure (Left (AckNoEncKey canonKey))
      (enc : _) ->
        case makeSigilFromCredentials @HBS2Basic creds enc Nothing Nothing of
          Nothing -> pure (Left (AckNoEncKey canonKey))
          Just me ->
            fmap Right (sendPayload ob (Right me) [sigil] [] (makeAck rec))
              -- Every way the send can fail, as a value. The recipient's sigil
              -- is the interesting one: it is a hash the CONTRIBUTOR published,
              -- and this node may simply not have the block, in which case
              -- there is nothing to seal to and nothing to be done about it
              -- here.
              `catch` (\(e :: PeerSilent) -> pure (Left (AckNotSent (tshow e))))
              `catch` (\(e :: NotStored)  -> pure (Left (AckNotSent (tshow e))))
              `catch` (\(e :: PoWTooHard) -> pure (Left (AckNotSent (tshow e))))
              -- And the sigil, which is a MailboxServiceError or a
              -- SigilNotFound out of the message layer rather than one of the
              -- three above. Caught by shape, because the alternative is an
              -- accept that reports a crash after canon is written.
              `catch` (\(e :: SomeException) -> pure (Left (AckNotSent (tshow e))))
  where
    tshow :: Show e => e -> Text
    tshow = Text.pack . show
