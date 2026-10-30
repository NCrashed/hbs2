module RepositoryLeaseWorker
  ( repositoryLeaseWorker
  , refreshRepositoryLease
  ) where

import HBS2.Data.Types.Refs
import HBS2.Peer.Brains
import HBS2.Peer.Prelude
import HBS2.Peer.Proto.RepositoryLease
import HBS2.Peer.RepositoryLease
import HBS2.Storage
import HBS2.Storage.Inventory

import PeerLogger (warn)

import Control.Exception (SomeException, try)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text

-- | Reconcile durable local leases with the blocks currently available in the
-- peer storage.  Network fetching remains the responsibility of the existing
-- LWWRef and RefLog workers; this worker durably subscribes them and seals a
-- pin only after the complete typed repository graph verifies.
repositoryLeaseWorker :: SomeBrains L4Proto -> AnyStorage -> IO ()
repositoryLeaseWorker brains storage = forever do
  leases <- listRepositoryLeases @L4Proto brains
  for_ leases $ \lease -> do
    refreshed <- try @SomeException (refreshRepositoryLease brains storage lease)
    case refreshed of
      Left exception ->
        warn $ "repository lease refresh failed" <+> pretty (repositoryLeaseId lease)
          <+> viaShow exception
      Right{} -> none
  pause @'Seconds 15

refreshRepositoryLease
  :: SomeBrains L4Proto
  -> AnyStorage
  -> RepositoryLease
  -> IO RepositoryLease
refreshRepositoryLease brains storage lease
  | repositoryLeaseState lease == RepositoryLeaseTerminated =
      releaseRepositoryLeasePin storage (repositoryLeaseId lease) $> lease
  | repositoryLeaseVersion lease /= 1 = do
      now <- floor <$> getPOSIXTime
      persistIfChanged now lease lease
        { repositoryLeaseState = case repositoryLeaseSnapshot lease of
            Nothing -> RepositoryLeaseSyncing
            Just{}  -> RepositoryLeaseDegraded
        , repositoryLeaseLastError = Just "unsupported repository lease version"
        }
  | otherwise = do
      now <- floor <$> getPOSIXTime
      case repositoryLeaseExpiresAt lease of
        Just expiresAt | now >= expiresAt -> do
          releaseRepositoryLeasePin storage (repositoryLeaseId lease)
          persistIfChanged now lease lease
            { repositoryLeaseState = RepositoryLeaseTerminated
            , repositoryLeaseLastError = Just "lease expired"
            }
        _ -> refresh now
  where
    refresh now = do
      ensurePoll (repositoryLeaseRepository lease) "lwwref"

      discoveredRefLog <- discoverRepositoryRefLogKey storage
                            (repositoryLeaseRepository lease)
      for_ discoveredRefLog $ \refLogKey -> ensurePoll refLogKey "reflog"

      let limits = InventoryLimits
            { inventoryMaxBlocks = repositoryLeaseMaxBlocks lease
            , inventoryMaxBytes = repositoryLeaseMaxBytes lease
            }

      discoverRepositorySnapshot limits storage
        (repositoryLeaseRepository lease) >>= \case
          Left problem -> persistProblem now problem
          Right candidate -> activateCandidate now candidate

    ensurePoll key kind = do
      exists <- isPolledRef @L4Proto brains kind key
      unless exists (addPolledRef @L4Proto brains key kind 1)

    persistProblem now problem =
      persistIfChanged now lease lease
        { repositoryLeaseState = case repositoryLeaseSnapshot lease of
            Nothing -> RepositoryLeaseSyncing
            Just{}  -> RepositoryLeaseDegraded
        , repositoryLeaseLastError = Just (Text.pack (show problem))
        }

    activateCandidate now candidate = do
      let sameSnapshot =
            (repositorySnapshotId <$> repositoryLeaseSnapshot lease)
              == Just (candidateSnapshotId candidate)

      pin <- getRef storage (RepositoryLeasePin (repositoryLeaseId lease))
      let samePin = case repositoryLeaseSnapshot lease of
            Just snapshot -> fmap HashRef pin
              == Just (repositorySnapshotPinRoot snapshot)
            Nothing -> False

      if sameSnapshot && samePin
        then persistIfChanged now lease lease
          { repositoryLeaseState = RepositoryLeaseActive
          , repositoryLeaseLastError = Nothing
          }
        else sealRepositorySnapshot storage (repositoryLeaseId lease) candidate >>= \case
          Left problem -> persistProblem now problem
          Right snapshot -> persistIfChanged now lease lease
            { repositoryLeaseState = RepositoryLeaseActive
            , repositoryLeaseSnapshot = Just snapshot
            , repositoryLeaseLastError = Nothing
            }

    persistIfChanged now before after = do
      current <- getRepositoryLease @L4Proto brains (repositoryLeaseId before)
                   <&> fromMaybe before
      if repositoryLeaseState current == RepositoryLeaseTerminated
           && repositoryLeaseState after /= RepositoryLeaseTerminated
        then releaseRepositoryLeasePin storage (repositoryLeaseId current) $> current
        else if withoutTimestamp current == withoutTimestamp after
          then pure current
          else do
            let updated = after { repositoryLeaseUpdatedAt = now }
            putRepositoryLease @L4Proto brains updated
            pure updated

    withoutTimestamp value = value { repositoryLeaseUpdatedAt = 0 }
