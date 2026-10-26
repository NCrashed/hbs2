-- | A connection to the peer that a long-running verb opens for itself, and
-- that says so when it drops.
--
-- WHY NOT THE ONE EVERY VERB GETS. The RPC clients a verb is handed are built
-- once, before it runs ('HBS2.CLI.Run.Internal.recover'), on a messaging
-- client that retries forever in the background and logs a warning on every
-- try. A verb that ends in a second never notices. @hub inbox --follow@ runs
-- for days, and when the peer restarted it was left calling through a client
-- that never came back, under a stream of
-- @MessagingUnix. runClient failed, probably server is gone@ lines that a
-- watcher merging stderr took for events.
--
-- So a follower opens its own connection here, one per attempt, with the
-- messaging told not to retry: a dropped socket ends the messaging thread,
-- and that end is the signal. Every call goes through the function this hands
-- out, which answers 'PeerGone' instead of waiting out a timeout on a socket
-- nobody is at.
module HBS2.Hub.PeerLink
  ( PeerGone(..)
  , Over
  , withPeerLink
  , socketOf
  , peerAnswers
  ) where

import HBS2.Hub.Ingress (PeerSilent(..))

import HBS2.CLI.Prelude
import HBS2.Storage (AnyStorage(..))
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.Service
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient (StorageClient(..))

import Control.Exception qualified as E
import Data.Maybe (isJust)
import Network.Socket qualified as N

-- | The connection to the peer dropped, or there was no peer to connect to.
--
-- A value and not an exception, because the caller has to do something with
-- it every time: a follower that let it propagate would be the follower that
-- died on a restart, which is what this module is for.
data PeerGone = PeerGone
  deriving stock (Eq,Show)

-- | Run one call over the link. The flag says how to take a peer that does not
-- answer: 'True' reads it as not back yet ('PeerGone'), 'False' as the peer
-- being up and stuck ('PeerSilent' is rethrown).
--
-- The lenient reading is for the first call after a restart, when the socket
-- may be there before the services behind it are.
type Over m = forall a . Bool -> m a -> m (Either PeerGone a)

-- | The socket path a service caller talks to. The clients a verb is handed
-- were made for one path, and a link opened later has to open the same one.
socketOf :: ServiceCaller api UNIX -> FilePath
socketOf c = case callPeer c of PeerUNIX p -> p

-- | Whether something accepts connections at the path right now.
--
-- Asked before the messaging is started, because the messaging's own connect
-- loop never gives up: it logs a warning and tries again every second, which is
-- the flood this module exists to end.
peerAnswers :: MonadIO m => FilePath -> m Bool
peerAnswers path = liftIO $
  E.handle (\(_ :: E.IOException) -> pure False) $
    E.bracket (N.socket N.AF_UNIX N.Stream N.defaultProtocol) N.close $ \s ->
      N.connect s (N.SockAddrUnix path) >> pure True

-- | Open a connection to the peer at the path, hand it to the body with the
-- storage and mailbox clients on it, and close it when the body returns.
--
-- 'Nothing' when nothing answers at the path. A body that sees 'PeerGone' from
-- 'Over' should return: the connection is dead and will not come back, and the
-- next attempt is a new 'withPeerLink'.
withPeerLink :: MonadUnliftIO m
             => FilePath
             -> (AnyStorage -> ServiceCaller MailboxAPI UNIX -> Over m -> m r)
             -> m (Maybe r)
withPeerLink path body = peerAnswers path >>= \case
  False -> pure Nothing
  True  -> do
    -- MUDontRetry: a lost connection ends 'runMessagingUnix' with
    -- 'UnixMessagingStopped' instead of starting it over in the background.
    client     <- newMessagingUnixOpts [MUDontRetry] False 1.0 path
    storageAPI <- makeServiceCaller @StorageAPI (fromString path)
    mailboxAPI <- makeServiceCaller @MailboxAPI (fromString path)
    let endpoints = [ Endpoint @UNIX storageAPI, Endpoint @UNIX mailboxAPI ]
        wire     = void (tryAny (runMessagingUnix client))
        services = void (tryAny (liftIO (runReaderT (runServiceClientMulti endpoints) client)))
    withAsync wire $ \w -> withAsync services $ \s -> do
      let over :: MonadUnliftIO n => Bool -> n a -> n (Either PeerGone a)
          over lenient act =
            race (waitEither_ w s) (try act) >>= \case
              Left ()               -> pure (Left PeerGone)
              Right (Right a)       -> pure (Right a)
              -- A call that timed out because the socket went away under it
              -- is the peer gone, not the peer stuck.
              Right (Left (e :: PeerSilent)) -> do
                dead <- isJust <$> poll w
                if dead || lenient then pure (Left PeerGone) else throwIO e
      Just <$> body (AnyStorage (StorageClient storageAPI)) mailboxAPI over
