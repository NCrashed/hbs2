-- | A storage, two peers and the services that build a message out of them.
--
-- Shared because two test modules need the same three things to get a
-- 'Message' at all, and a message is not what either of them is testing. Kept
-- in memory and on a temporary directory: what is exercised here is the
-- message, never how a key is found, so neither keyman nor a stored sigil is
-- involved.
module MailboxFixture
  ( S
  , withStore
  , aPeer
  , services
  , reader
  , contentOf
  , aMessage
  ) where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.Net.Auth.GroupKeySymm (lookupGroupKey)
import HBS2.Net.Auth.Schema()
import HBS2.Data.Types.SignedBox (unboxSignedBox0)
import HBS2.Peer.Proto.Mailbox.Message
import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Hash (HbSync)
import HBS2.Storage
import HBS2.Storage.Simple

import Control.Concurrent.Async (async,cancel)
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Lens.Micro.Platform (view)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import UnliftIO (bracket)

type S = 'HBS2Basic

-- A storage on disk, because the parts are merkle trees and there is nowhere
-- else in this project to build one.
withStore :: (AnyStorage -> IO a) -> IO a
withStore act =
  withSystemTempDirectory "hbs2-mailbox-test" $ \dir -> do
    sto <- simpleStorageInit @HbSync [StoragePrefix (dir </> ".storage")]
    bracket (replicateM 4 (async (simpleStorageWorker sto)))
            (mapM_ cancel)
            (const (act (AnyStorage sto)))

-- Credentials with one encryption key, and the sigil that publishes it.
--
-- Built in memory and handed straight to the services record, so this needs
-- neither keyman nor a sigil in storage: what is under test is the ORDER the
-- two calls happen in, not how a key is found.
aPeer :: IO (PeerCredentials S, Sigil S)
aPeer = do
  cred <- newCredentialsEnc @S 1
  let ke = view krPk (head (view peerKeyring cred))
  si <- makeSigilFromCredentials @S cred ke Nothing Nothing
          & orThrowUser "no sigil"
  pure (cred, si)

services :: AnyStorage -> [PeerCredentials S] -> CreateMessageServices S
services sto creds = CreateMessageServices
  { cmStorage = sto
  , cmLoadCredentials = \pk ->
      pure (first_ [ c | c <- creds, view peerSignPk c == pk ])
  , cmLoadKeyringEntry = \pk ->
      pure (first_ [ k | c <- creds, k <- view peerKeyring c, view krPk k == pk ])
  }

-- What a recipient uses to open one: their own keyring against the message's
-- group key, which is what keyman does in production.
reader :: PeerCredentials S -> ReadMessageServices S
reader cred = ReadMessageServices
  { rmsFindGKS = \gk ->
      pure (first_ [ s | k <- view peerKeyring cred
                       , Just s <- [lookupGroupKey (view krSk k) (view krPk k) gk] ])
  }

first_ :: [a] -> Maybe a
first_ = \case { (x:_) -> Just x ; [] -> Nothing }

contentOf :: Message S -> IO (MessageContent S)
contentOf msg =
  unboxSignedBox0 (messageContent msg) & orThrowUser "message box will not open" <&> snd

-- | One letter from a sender to a set of recipients, with nothing attached.
aMessage :: AnyStorage -> (PeerCredentials S, Sigil S) -> [Sigil S] -> ByteString -> IO (Message S)
aMessage sto (cred, sigil) rcpts payload = do
  flags <- defMessageFlags
  createMessage (services sto [cred]) flags Nothing (Right sigil) (fmap Right rcpts)
    mempty payload
