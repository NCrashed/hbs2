module RepositoryLeaseWorker
  ( repositoryLeaseWorker
  ) where

import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.Refs (HashRef, fromHashRef)
import HBS2.Peer.Brains
import HBS2.Peer.Prelude
import HBS2.Peer.Proto.RepositoryLease
import HBS2.Peer.RepositoryLease.Worker
import HBS2.Storage

import PeerLogger (warn)

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)

-- | Reconcile durable local leases with the blocks currently available in the
-- peer storage.  Network fetching remains the responsibility of the existing
-- LWWRef and RefLog workers; this loop only schedules the library reconciler
-- and reports failures.
repositoryLeaseWorker
  :: [PeerAddr L4Proto]
  -> (ByteString -> IO ())
  -> PeerCredentials 'HBS2Basic
  -> SomeBrains L4Proto
  -> AnyStorage
  -> IO ()
repositoryLeaseWorker reachable publish credentials brains storage = do
  published <- newIORef Map.empty
  forever do
    now <- floor <$> getPOSIXTime
    leases <- listRepositoryLeases @L4Proto brains
    let liveLeaseIds = Set.fromList (repositoryLeaseId <$> leases)
    atomicModifyIORef' published $ \state ->
      (Map.restrictKeys state liveLeaseIds, ())
    for_ leases $ \lease -> do
      result <- try @SomeException do
        refreshed <- refreshRepositoryLease credentials brains storage lease
        maintained <- maintainRepositoryReplicaAdvert
          policy storage credentials refreshed
        case maintained of
          Right Nothing -> forgetPublished published (repositoryLeaseId lease)
          Right (Just (advertHash, _)) -> do
            shouldPublish <- needsPublication
              now published (repositoryLeaseId lease) advertHash
            when shouldPublish $ do
              advertBytes <- getBlock storage (fromHashRef advertHash)
              for_ advertBytes $ \bytes -> do
                publish (LBS.toStrict bytes)
                recordPublication
                  now published (repositoryLeaseId lease) advertHash
          Left{} -> pure ()
        pure maintained
      case result of
        Left exception ->
          warn $ "repository lease refresh failed" <+> pretty (repositoryLeaseId lease)
            <+> viaShow exception
        Right (Left problem)
          | not (null reachable) ->
              warn $ "repository advert refresh failed" <+> pretty (repositoryLeaseId lease)
                <+> viaShow problem
        Right{} -> none
    pause @'Seconds 15
  where
    policy = defaultRepositoryAdvertPolicy reachable

needsPublication
  :: Word64
  -> IORef (Map.Map RepositoryLeaseId (HashRef, Word64))
  -> RepositoryLeaseId
  -> HashRef
  -> IO Bool
needsPublication now published leaseId advertHash = do
  previous <- Map.lookup leaseId <$> readIORef published
  pure $ case previous of
    Nothing -> True
    Just (previousHash, previousTime) ->
         previousHash /= advertHash
      || now - min now previousTime >= repositoryAdvertRepublishInterval

recordPublication
  :: Word64
  -> IORef (Map.Map RepositoryLeaseId (HashRef, Word64))
  -> RepositoryLeaseId
  -> HashRef
  -> IO ()
recordPublication now published leaseId advertHash =
  atomicModifyIORef' published $ \state ->
    (Map.insert leaseId (advertHash, now) state, ())

forgetPublished
  :: IORef (Map.Map RepositoryLeaseId (HashRef, Word64))
  -> RepositoryLeaseId
  -> IO ()
forgetPublished published leaseId =
  atomicModifyIORef' published $ \state -> (Map.delete leaseId state, ())

repositoryAdvertRepublishInterval :: Word64
repositoryAdvertRepublishInterval = 60
