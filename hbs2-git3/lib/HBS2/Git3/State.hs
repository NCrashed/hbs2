{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git3.State
  ( module HBS2.Git3.State
  , module Exported
  ) where

import HBS2.Git3.Prelude

import HBS2.Merkle
import HBS2.Git3.State.Internal.Types as Exported
import HBS2.Git3.State.Internal.LWWBlock as Exported
import HBS2.Git3.State.Internal.RefLog as Exported
import HBS2.Git3.State.Internal.Segment as Exported
import HBS2.Git3.State.Internal.Index as Exported
import HBS2.Git3.State.Internal.Wait as Exported

import HBS2.Net.Auth.GroupKeySymm

import HBS2.Storage.Operations.Missed
import HBS2.Net.Auth.Credentials
import HBS2.KeyMan.Keys.Direct
import HBS2.Data.Detect
import HBS2.CLI.Run.MetaData (getTreeContents)

import Data.Config.Suckless.Script

import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.System.Dir
import HBS2.Peer.CLI.Detect
import HBS2.Peer.Proto.RepositoryLease (replicaAdvertProvider)

import Data.ByteString.Lazy qualified as LBS
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Kind
import Data.Maybe
import Data.List qualified as L
import Data.Text.Encoding.Error qualified as TE
import Data.Text.Encoding qualified as TE
import Data.Word
import Lens.Micro.Platform

import Codec.Compression.Zstd (maxCLevel)


newtype RepoManifest = RepoManifest [Syntax C]


-- FIXME: cache
getGK :: forall m . HBS2GitPerks m => Git3 m (Maybe (HashRef, GroupKey 'Symm 'HBS2Basic))
getGK = do
  sto <- getStorage
  mf <- getRepoManifest
  runMaybeT do
    gkh <- headMay [ x | ListVal [SymbolVal "gk", HashLike x ] <- coerce mf ] & toMPlus
    gk <- loadGroupKeyMaybe sto gkh >>= toMPlus
    pure (gkh,gk)

getRefLog :: RepoManifest -> Maybe GitRemoteKey
getRefLog mf = lastMay [ x
                       | ListVal [SymbolVal "reflog", SignPubKeyLike x]  <- coerce mf
                       ]

updateRepoKey :: forall m . HBS2GitPerks m => GitRepoKey -> Git3 m ()
updateRepoKey key = do

  status $ "updateRepoKey" <+> pretty (AsBase58 key)

  setGitRepoKey key

  reflog <- getRepoManifest <&> getRefLog

  ask >>= \case
        Git3Connected{..} -> do
          debug $ yellow "UPDATED REFLOG" <+> pretty (fmap AsBase58 reflog)
          atomically $ writeTVar gitRefLog reflog

        _ -> none

getRepoRefMaybe :: forall m . HBS2GitPerks m => Git3 m (Maybe (LWWRef 'HBS2Basic))
getRepoRefMaybe = do
  lwwAPI  <- getClientAPI @LWWRefAPI @UNIX

  pk <- getGitRepoKey >>= orThrow GitRepoRefNotSet

  callRpcWaitMay @RpcLWWRefGet (TimeoutSec 1) lwwAPI (LWWRefKey pk)
    >>= orThrow RpcTimeout

getRepoRefLogCredentials :: forall m . HBS2GitPerks m
                         => Git3 m (PubKey 'Sign 'HBS2Basic, PrivKey 'Sign HBS2Basic)

getRepoRefLogCredentials = do
  -- FIXME: memoize-this
  mf <- getRepoManifest
  rk <- getGitRepoKey >>= orThrow GitRepoRefNotSet

  reflog <- getGitRemoteKey >>= orThrow Git3ReflogNotSet

  creds <- runKeymanClientRO (loadCredentials rk)
             >>= orThrowUser ("not found credentials for"  <+> pretty (AsBase58 rk))

  seed <- [ x | ListVal [SymbolVal "seed", LitIntVal x ] <- coerce mf ]
             & lastMay & orThrow GitRepoManifestMalformed
             <&> fromIntegral @_ @Word64

  let sk = view peerSignSk creds

  (p,s) <- derivedKey @'HBS2Basic @'Sign seed sk

  unless ( p == reflog ) do
    throwIO RefLogCredentialsNotMatched

  pure (p,s)

getRepoManifest :: forall m . HBS2GitPerks m => Git3 m RepoManifest
getRepoManifest = do

  sto <- getStorage

  LWWRef{..} <- getRepoRefMaybe >>= orThrow GitRepoRefEmpty

  mfref <- readLogThrow (getBlock sto) lwwValue
               <&> headMay
               >>= orThrow GitRepoManifestMalformed

  runExceptT (getTreeContents sto mfref)
     >>= orThrowPassIO
     <&> TE.decodeUtf8With TE.lenientDecode . LBS.toStrict
     <&> parseTop
     >>= orThrow GitRepoManifestMalformed
     <&> RepoManifest

nullGit3Env :: forall m . MonadIO m => m Git3Env
nullGit3Env = Git3Disconnected
                <$> newTVarIO defSegmentSize
                <*> newTVarIO defCompressionLevel
                <*> newTVarIO defIndexBlockSize
                <*> newTVarIO Nothing
                <*> (newTVarIO =<< repoWaitFromEnv)
                <*> pure Nothing


connectedDo :: (MonadIO m) => Git3 m a -> Git3 m a
connectedDo what = do
  env <- ask
  debug $ red "connectedDo"
  case env of
    Git3Disconnected{} -> do
      throwIO Git3PeerNotConnected

    _ -> what

withGit3Env :: Git3Perks m => Git3Env -> Git3 m a -> m a
withGit3Env env a = runReaderT (fromGit3 a) env

runGit3 :: Git3Perks m => Git3Env -> Git3 m b -> m b
runGit3 env action = withGit3Env env action

withStateDo :: (MonadUnliftIO m) => Git3 m a -> Git3 m a
withStateDo action = do

  waitRepo Nothing =<< getGitRepoKeyThrow

  getStatePathM >>= mkdir
  action

recover :: Git3 IO a -> Git3 IO a
recover m = fix \again -> do
  catch m $ \case
    Git3PeerNotConnected -> do

      soname <- detectRPC
                  `orDie` "can't locate hbs2-peer rpc"


      flip runContT pure do

        client <- lift $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
                    >>= orThrowUser ("can't connect to" <+> pretty soname)

        void $ ContT $ withAsync $ runMessagingUnix client

        peer       <- makeServiceCaller @PeerAPI (fromString soname)
        refLogAPI  <- makeServiceCaller @RefLogAPI (fromString soname)
        storageAPI <- makeServiceCaller @StorageAPI (fromString soname)
        lwwAPI     <- makeServiceCaller @LWWRefAPI (fromString soname)

        -- let sto = AnyStorage (StorageClient storageAPI)

        let endpoints = [ Endpoint @UNIX  peer
                        , Endpoint @UNIX  refLogAPI
                        , Endpoint @UNIX  lwwAPI
                        , Endpoint @UNIX  storageAPI
                        ]

        void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

        let sto = AnyStorage (StorageClient storageAPI)

        rk <- lift $ getGitRepoKey

        -- debug $ yellow $ "REPOKEY" <+> pretty (AsBase58 rk)

        dict <- asks gitRuntimeDict
        waitT <- asks gitRepoWait

        connected <- Git3Connected soname sto peer refLogAPI lwwAPI
                        <$> newTVarIO rk
                        <*> newTVarIO Nothing
                        <*> newTVarIO Nothing
                        <*> newTVarIO defSegmentSize
                        <*> newTVarIO defCompressionLevel
                        <*> newTVarIO defIndexBlockSize
                        <*> pure waitT
                        <*> pure dict


        liftIO $ withGit3Env connected do
          -- updateRepoKey rk
          -- ref <- getGitRemoteKey >>= orThrow GitRepoManifestMalformed
          -- state <- getStatePathM
          -- mkdir state
          again

    e -> throwIO e


data ReflogWaitTimeout =
  ReflogWaitTimeout
  deriving stock (Show,Typeable)

instance Exception ReflogWaitTimeout


data CWRepo =
    CWaitLWW
  | CCheckManifest (LWWRef HBS2Basic)
  | CAborted


-- | Wait until the repository can be used: its lwwref has a value, the
-- manifest it points at can be read, and the reflog that manifest names has a
-- value whose blocks are all here.
--
-- EVERY STAGE IS BOUNDED. An lwwref that nobody holds and one that has not
-- arrived yet look the same from here, so without a deadline a repository
-- that is gone is a `git push` that never returns and never says anything.
-- The limit is 'getRepoWaitTimeout', and each stage gives up with a
-- 'RepoWaitError' that says which stage it was and what to do.
--
-- The argument only changes the reflog stage. With @Just t@ the caller needs
-- the manifest and nothing else: the reflog is waited for at most @t@ and its
-- absence is not an error, as it never was. With 'Nothing' (push, fetch,
-- anything that reads or writes history) the reflog is required, and a reflog
-- whose blocks stop arriving is reported rather than waited on forever; one
-- that keeps arriving is never cut off.
waitRepo :: forall m . HBS2GitPerks m
         => Maybe (Timeout 'Seconds)
         -> GitRepoKey
         -> Git3 m ()
waitRepo timeout repoKey = do

  status $ yellow "waitRepo" <+> pretty (AsBase58 repoKey)

  limit <- getRepoWaitTimeout

  ask >>= \case
    Git3Disconnected{} -> throwIO Git3PeerNotConnected
    Git3Connected{..} -> do

      sto <- getStorage

      flip runContT pure $ callCC \done -> do

        rlv <- readTVarIO gitRefLogVal <&> isJust
        rlog <- readTVarIO gitRefLog <&> isJust

        when (rlv && rlog) $ done ()

        status "wait for peer"

        -- A fresh peer often reaches its RPC socket before it has authenticated
        -- its configured relay.  One lookup at that instant sees no peers and
        -- permanently misses an otherwise live replica advert.  Keep discovery
        -- beside the ordinary LWWRef polling until one provider authenticates or
        -- this wait finishes; the ContT scope cancels the worker on every exit.
        _ <- ContT $ withAsync $ flip fix 1 \next delay -> do
          provider <- callRpcWaitMay @RpcRepositoryAdvertFind
            (TimeoutSec 3) peerAPI repoKey
          case join provider of
            Just advert -> status $ "repository provider" <+>
              pretty (AsBase58 (replicaAdvertProvider advert))
            Nothing -> do
              pause @'Seconds delay
              next (min 10 (delay * 2))

        void $ lift (pollUntil limit (callRpcWaitMay @RpcPollAdd (TimeoutSec 2) peerAPI (repoKey, "lwwref", 31)))
                 >>= orThrow (RepoPeerNotAnswering limit)

        -- Asked often at first, when an answer is most likely, and every ten
        -- seconds after that.
        pFetch <- ContT $ withAsync $ flip fix 1 \next i -> do
                    void  (callRpcWaitMay @RpcLWWRefFetch (TimeoutSec 1) lwwAPI (LWWRefKey repoKey))
                    pause @'Seconds i
                    next (min 10 (i * 2))

        status $ "wait for" <+> pretty (AsBase58 repoKey)

        lww <- lift (pollUntil limit (join <$> callRpcWaitMay @RpcLWWRefGet (TimeoutSec 1) lwwAPI (LWWRefKey repoKey)))
                 >>= orThrow (RepoNotFound repoKey limit)

        setGitRepoKey repoKey

        status $ "lwwref value" <+> pretty (lwwValue lww)

        status "wait for manifest"

        mf <- lift (pollUntil limit (either (const Nothing) Just <$> try @_ @SomeException getRepoManifest))
                >>= orThrow (RepoManifestUnreadable repoKey (lwwValue lww) limit)

        reflog <- getRefLog mf & orThrow GitRepoManifestMalformed

        lift (callRpcWaitMay @RpcPollAdd (TimeoutSec 1) peerAPI (reflog, "reflog", 11))
                 >>= orThrow RpcTimeout

        ContT $ withAsync $ do
                pause @'Seconds 1
                flip fix 2 $ \next i -> do
                  debug $ "fetch reflog" <+> pretty (AsBase58 reflog)
                  void $ lift (callRpcWaitMay @RpcRefLogFetch (TimeoutSec 2) reflogAPI reflog)
                  pause @'Seconds i
                  next (min 10 (i*1.5))

        status $ "wait for reflog" <+> pretty (AsBase58 reflog)

        rv' <- lift $ pollUntil (fromMaybe limit timeout)
                        (join <$> callRpcWaitMay @RpcRefLogGet (TimeoutSec 2) reflogAPI reflog)

        case rv' of
          Nothing | isJust timeout -> done ()
                  | otherwise      -> throwIO (RepoRefLogEmpty repoKey reflog limit)

          Just rv -> do
            atomically $ writeTVar gitRefLogVal (Just rv)

            cancel pFetch

            status $ "reflog" <+> pretty (AsBase58 reflog) <+> pretty rv

            synced <- lift $ pollProgress (fromMaybe limit timeout) do
              try @_ @OperationError (findMissedBlocks sto rv) >>= \case
                Left{}  -> pure (Left maxBound)
                Right missed
                  | L.null missed -> pure (Right ())
                  | otherwise -> do
                      status $ "wait reflog to sync in consistent state" <+> pretty (length missed)
                      pure (Left (length missed))

            case synced of
              Right () -> lift (updateRepoKey repoKey)
              Left n | isJust timeout -> none
                     | otherwise      -> throwIO (RepoRefLogIncomplete repoKey reflog n limit)
