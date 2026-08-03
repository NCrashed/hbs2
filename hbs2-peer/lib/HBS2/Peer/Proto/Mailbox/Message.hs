{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.Proto.Mailbox.Message where

import HBS2.Prelude.Plated

import HBS2.Peer.Proto.Mailbox.Types

import HBS2.Data.Types.SmallEncryptedBlock
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.Net.Auth.GroupKeySymm
import HBS2.Merkle.MetaData

import HBS2.OrDie
import HBS2.Base58
import HBS2.Storage
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.SignedBox
import HBS2.Data.Types.Refs
import HBS2.Net.Auth.Schema()

import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Set qualified as Set
import Data.HashMap.Strict qualified as HM
import Lens.Micro.Platform
import Streaming.Prelude qualified as S
import UnliftIO


data CreateMessageError =
    SenderNotSet
  | RecipientsNotSet
  | SigilNotFound HashRef
  | MalformedSigil (Maybe HashRef)
  | SenderNoAccesToGroupKey
  | NoCredentialsFound String
  | NoKeyringFound String
  deriving stock (Show,Typeable,Generic)

instance Exception CreateMessageError


defMessageFlags :: MonadIO m => m MessageFlags
defMessageFlags = MessageFlags1 <$> (round <$> liftIO getPOSIXTime)
                                <*> pure mzero
                                <*> pure mzero
                                <*> pure mzero

data CreateMessageServices s =
  CreateMessageServices
  { cmStorage          :: AnyStorage
  , cmLoadCredentials  :: forall m . MonadUnliftIO m => PubKey 'Sign s  -> m (Maybe (PeerCredentials s))
  , cmLoadKeyringEntry :: forall m . MonadUnliftIO m => PubKey 'Encrypt s -> m (Maybe (KeyringEntry s))
  }

-- | Build a signed, encrypted message for a set of recipients.
--
-- The parts get their own group secret, separate from the one over
-- @messageData@. They are separate objects with separate lifetimes: a part
-- tree is fetched and kept by whoever is given the key to it, while
-- @messageData@ holds transport metadata that is not meant to travel with the
-- attachment. The forge (PEP-18/PEP-19) is the case that forces this: folding
-- an attachment into public canon means publishing the key to it, and if that
-- were the message key it would also publish the payload it was in, including
-- the sender's private reply address, to every clone forever. The ciphertext
-- is held by every peer that ever hosted or relayed the mailbox, so the leak
-- would be retroactive and irreversible.
--
-- The 'GroupSecret' argument therefore applies to @messageData@ only; the
-- parts always get a freshly generated one.
--
-- ONE CALL BUILDS BOTH, which is all a sender needs unless it has to SIGN a
-- reference to its own attachment. A part is named by the hash of its
-- encrypted tree, so a payload that names one has to be written after the tree
-- exists, and this makes the tree and signs the payload in that order with
-- nothing in between. PEP-18's letters do need to (@body-part@,
-- @bundle-part@), so the two halves are also available separately:
-- 'createAttachments' then 'createMessageWith'. This is the two of them in a
-- row.
createMessage :: forall s m . (MonadUnliftIO m , s ~ HBS2Basic)
              => CreateMessageServices s
              -> MessageFlags
              -> Maybe GroupSecret         -- ^ secret for @messageData@, not for the parts
              -> Either HashRef (Sigil s)    -- ^ sender
              -> [Either HashRef (Sigil s)]  -- ^ sigil keys (recipients)
              -> [([(Text, Text)], m LBS.ByteString)] -- ^ message parts
              -> ByteString                  -- ^ payload
              -> m (Message s)
createMessage cms flags gks sender' rcpts' parts bs = do
  trees <- createAttachments cms sender' rcpts' parts
  createMessageWith cms flags gks sender' rcpts' trees bs

-- | Store a message's attachments, and say what they are called.
--
-- Separate from the message so a sender can name a part in the payload it is
-- about to sign. The hash of an encrypted tree is what names it, so the tree
-- has to exist first; a caller that could only pass a producer to
-- 'createMessage' would learn the name after the signature was already over
-- bytes that did not mention it.
--
-- The parts share ONE freshly generated group key, wrapped for the same
-- recipients, and it is not the message's. See 'createMessage' for why that
-- separation is load-bearing rather than tidy. Each tree embeds that key, so a
-- reader given a part hash and the parts secret needs nothing from the message
-- the part arrived in, which is what lets a fold publish an attachment without
-- publishing the letter around it.
--
-- The answer is in the order the parts were given, so a caller with two of
-- them can tell which is which.
createAttachments :: forall s m . (MonadUnliftIO m , s ~ HBS2Basic)
                  => CreateMessageServices s
                  -> Either HashRef (Sigil s)    -- ^ sender
                  -> [Either HashRef (Sigil s)]  -- ^ sigil keys (recipients)
                  -> [([(Text, Text)], m LBS.ByteString)]
                  -> m [HashRef]
createMessageWith :: forall s m . (MonadUnliftIO m , s ~ HBS2Basic)
                  => CreateMessageServices s
                  -> MessageFlags
                  -> Maybe GroupSecret
                  -> Either HashRef (Sigil s)
                  -> [Either HashRef (Sigil s)]
                  -> [HashRef]                   -- ^ parts already stored
                  -> ByteString
                  -> m (Message s)

createAttachments cms@CreateMessageServices{..} sender' rcpts' parts = do

  (sender, pips) <- resolveKeys cms sender' rcpts'

  -- A group key over the same recipients as the message will have, with a
  -- secret of its own. Nothing is passed here on purpose: a caller's secret,
  -- if any, is for messageData, and the whole point is that the parts do not
  -- share it.
  gkParts <- generateGroupKey @s Nothing (fmap snd pips)

  KeyringEntry pk sk _ <- cmLoadKeyringEntry (snd sender)
                            >>= orThrow (NoKeyringFound (show $ pretty $ AsBase58 (snd sender)))

  gksParts <- lookupGroupKey sk pk gkParts & orThrow SenderNoAccesToGroupKey

  -- Each tree carries its own group key, so a reader given a part hash
  -- resolves the key from the tree and never needs the message it arrived in.
  for parts $ \(meta, lbsRead)-> do

    let mt = vcat [ pretty k <> ":" <+> dquotes (pretty v)
                  | (k,v) <- HM.toList (HM.fromList meta)
                  ]
               & show & Text.pack

    lbs <- lbsRead
    createEncryptedTree cmStorage gksParts gkParts (DefSource mt lbs)

createMessageWith cms@CreateMessageServices{..} flags gks sender' rcpts' trees bs = do

  (sender, pips) <- resolveKeys cms sender' rcpts'
  let recipients = drop 1 pips

  gk <- generateGroupKey @s gks (fmap snd pips)

  KeyringEntry pk sk _ <- cmLoadKeyringEntry (snd sender)
                            >>= orThrow (NoKeyringFound (show $ pretty $ AsBase58 (snd sender)))

  gks' <- lookupGroupKey sk pk gk & orThrow SenderNoAccesToGroupKey

  encrypted <- encryptBlock cmStorage gks' (Right gk) Nothing  bs

  let content = MessageContent @s
                  flags
                  (Set.fromList (fmap fst recipients))
                  (Right gk)
                  (Set.fromList trees)
                  encrypted

  creds <- cmLoadCredentials (fst sender)
             >>=  orThrow (NoCredentialsFound (show $ pretty $ AsBase58 (fst sender)))

  let ssk = view peerSignSk creds

  let box = makeSignedBox @s (fst sender) ssk content

  pure $ MessageBasic box

-- | The sender and everybody the message is for, out of sigils.
--
-- Both halves of building a message need this, and they must agree: the parts
-- key and the message key are wrapped for one set of recipients, and two
-- resolutions that disagreed would produce a message whose attachments some
-- recipient cannot open. Resolving twice from the same arguments gives the
-- same answer, so the cost is a second read of the sigils and not a risk.
--
-- The sender is returned apart from the list because it is the one whose
-- keyring this side has to hold, and it is also the first element: the group
-- keys are wrapped for the sender too, so a sender can read back what it sent.
resolveKeys :: forall s m . (MonadUnliftIO m, s ~ HBS2Basic)
            => CreateMessageServices s
            -> Either HashRef (Sigil s)
            -> [Either HashRef (Sigil s)]
            -> m ((PubKey 'Sign s, PubKey 'Encrypt s), [(PubKey 'Sign s, PubKey 'Encrypt s)])
resolveKeys CreateMessageServices{..} sender' rcpts' = do
  pips <- getKeys
  case pips of
    []              -> throwIO SenderNotSet
    ( s : _ : _ )   -> pure (s, pips)
    _               -> throwIO RecipientsNotSet
  where
    getKeys = do
      S.toList_ $ for_ (sender' : rcpts') $ \case
          Right si -> fromSigil Nothing si
          Left hs -> do
            si <- loadSigil @s cmStorage hs >>= orThrow (SigilNotFound hs)
            fromSigil (Just hs) si
    fromSigil h si = do
      (rcpt, SigilData{..}) <- unboxSignedBox0 (sigilData si) & orThrow (MalformedSigil h)
      S.yield (rcpt, sigilDataEncKey)


data ReadMessageServices s =
  ReadMessageServices
  { rmsFindGKS  :: forall m . MonadIO m => GroupKey 'Symm s -> m (Maybe GroupSecret)
  }

data ReadMessageError =
    ReadSignCheckFailed
  | ReadNoGroupKey
  | ReadNoGroupKeyAccess
  deriving stock (Show,Typeable)

instance Exception ReadMessageError

readMessage :: forall s m . ( MonadUnliftIO m
                            , s ~ HBS2Basic
                            )
            => ReadMessageServices s
            -> Message s
            -> m (PubKey 'Sign s, MessageContent s, ByteString)

readMessage ReadMessageServices{..} msg = do

  (pk, co@MessageContent{..}) <- unboxSignedBox0 (messageContent msg)
                                   & orThrow ReadSignCheckFailed

  -- TODO: support-groupkey-by-reference
  gk <- messageGK0 & orThrow ReadNoGroupKey

  gks <- rmsFindGKS gk >>= orThrow ReadNoGroupKeyAccess

  bs <- runExceptT (decryptBlockWithSecret @_ @s gks  messageData)
          >>= orThrowPassIO

  pure (pk, co, bs)

