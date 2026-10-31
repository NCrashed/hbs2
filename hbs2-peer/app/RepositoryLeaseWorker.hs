module RepositoryLeaseWorker
  ( repositoryLeaseWorker
  ) where

import HBS2.Net.Auth.Credentials
import HBS2.Peer.Brains
import HBS2.Peer.Prelude
import HBS2.Peer.Proto.RepositoryLease
import HBS2.Peer.RepositoryLease.Worker
import HBS2.Storage

import PeerLogger (warn)

import Control.Exception (SomeException, try)

-- | Reconcile durable local leases with the blocks currently available in the
-- peer storage.  Network fetching remains the responsibility of the existing
-- LWWRef and RefLog workers; this loop only schedules the library reconciler
-- and reports failures.
repositoryLeaseWorker
  :: [PeerAddr L4Proto]
  -> PeerCredentials 'HBS2Basic
  -> SomeBrains L4Proto
  -> AnyStorage
  -> IO ()
repositoryLeaseWorker reachable credentials brains storage = forever do
  leases <- listRepositoryLeases @L4Proto brains
  for_ leases $ \lease -> do
    result <- try @SomeException do
      refreshed <- refreshRepositoryLease credentials brains storage lease
      maintainRepositoryReplicaAdvert policy storage credentials refreshed
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
