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
  :: PeerCredentials 'HBS2Basic
  -> SomeBrains L4Proto
  -> AnyStorage
  -> IO ()
repositoryLeaseWorker credentials brains storage = forever do
  leases <- listRepositoryLeases @L4Proto brains
  for_ leases $ \lease -> do
    refreshed <- try @SomeException
      (refreshRepositoryLease credentials brains storage lease)
    case refreshed of
      Left exception ->
        warn $ "repository lease refresh failed" <+> pretty (repositoryLeaseId lease)
          <+> viaShow exception
      Right{} -> none
  pause @'Seconds 15
