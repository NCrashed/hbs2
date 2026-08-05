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
-- WHERE IT GOES IS THE SENDER'S OWN CHOICE, already vetted. PEP-18 honours a
-- reply channel only when it names the inner author's own mailbox AND the
-- envelope signer is that same key ('openLetterAs'), so by the time a channel
-- reaches this module it cannot address a stranger: a hub cannot be turned into
-- a reflector that delivers maintainer-signed messages to somebody who asked
-- for none.
--
-- A SIGIL MADE ON THE SPOT. Sealing a message needs the SENDER's encryption
-- key, which a sigil carries, and a hub has no reason to have published a sigil
-- for its canon key. So one is built in memory from the credentials the accept
-- already loaded. It is never stored: what the contributor needs to check the
-- ack is the envelope signer, which travels with the message.
module HBS2.Hub.CLI.Ack
  ( sendAck
  , AckTrouble(..)
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Ingress (PeerSilent(..))
import HBS2.Hub.CLI.Compose (Outbound(..),sendPayload,NotStored(..),PoWTooHard(..))

import HBS2.CLI.Prelude

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef)
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil (makeSigilFromCredentials)

import Data.Text qualified as Text

-- | Why the contributor was not told.
--
-- None of these is a failure of the accept that prompted it: by the time an ack
-- is attempted the event is in canon, so every one of them is a notification
-- that did not happen and nothing more.
data AckTrouble =
    -- | The letter asked for no acknowledgement, or asked for one this build
    -- would not honour (a rewrapped envelope, a channel naming a stranger's
    -- mailbox). The vetting is 'openLetterAs' and the answer arrives here as
    -- 'NoReply', so the two cases are indistinguishable on purpose: neither is
    -- a reason to do anything.
    AckNotAsked
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
    AckNoEncKey k ->
      "no encryption key for" <+> pretty (AsBase58 k) <> line
        <> "  an ack is sealed to its reader, so the signer needs one too"
    AckNotSent e -> "the acknowledgement was not sent:" <+> pretty (safeText e)

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
sendAck ob canonKey creds reply rec = case reply of
  NoReply -> pure (Left AckNotAsked)
  ReplyTo _ sigil ->
    -- The FIRST keyring entry, and the choice is arbitrary because nothing
    -- about it can be checked from here: an identity may hold several
    -- encryption keys and none of them is marked. The reader does not care --
    -- what it checks is the SIGN key on the envelope -- so any key that can
    -- seal will do.
    case [ _krPk e | e <- _peerKeyring creds ] of
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
