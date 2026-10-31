module HBS2.Git3.Repo.Lease.Policy
  ( defaultLeaseMaxBytes
  , defaultLeaseMaxBlocks
  , defaultLeaseGracePeriod
  , defaultLeaseMaxUpdateLag
  , renewRepositoryLeaseAt
  , selectActiveRepositoryLeaseAt
  , leaseExpiryAt
  , positiveWord64
  , nonNegativeWord64
  , leaseStateName
  ) where

import HBS2.Git3.Prelude
import HBS2.Peer.Proto.RepositoryLease

import Data.List (find)
import Data.Word (Word64)

defaultLeaseMaxBytes, defaultLeaseMaxBlocks :: Word64
defaultLeaseMaxBytes = 10 * 1024 * 1024 * 1024
defaultLeaseMaxBlocks = 10 * 1000 * 1000

defaultLeaseGracePeriod :: Word64
defaultLeaseGracePeriod = 0

defaultLeaseMaxUpdateLag :: Word64
defaultLeaseMaxUpdateLag = 5 * 60

-- | Build a new lease or renew the durable lease for the same repository.
-- Renewal preserves its identity, creation time, and last verified snapshot.
renewRepositoryLeaseAt
  :: Word64
  -> Word64
  -> GitRepoKey
  -> Word64
  -> Word64
  -> Word64
  -> Maybe Word64
  -> Word64
  -> Maybe RepositoryLease
  -> RepositoryLease
renewRepositoryLeaseAt
  now nonce repository maxBytes maxBlocks maxUpdateLag ttl grace previous =
  RepositoryLease
    { repositoryLeaseVersion = repositoryLeaseCurrentVersion
    , repositoryLeaseId = maybe
        (makeRepositoryLeaseId repository nonce)
        repositoryLeaseId
        previous
    , repositoryLeaseRepository = repository
    , repositoryLeaseState = maybe
        RepositoryLeaseSyncing
        (const RepositoryLeaseDegraded)
        previousSnapshot
    , repositoryLeaseMaxBytes = maxBytes
    , repositoryLeaseMaxBlocks = maxBlocks
    , repositoryLeaseCreatedAt = maybe now repositoryLeaseCreatedAt previous
    , repositoryLeaseUpdatedAt = now
    , repositoryLeaseExpiresAt = leaseExpiryAt now <$> ttl
    , repositoryLeaseGracePeriod = grace
    , repositoryLeaseMaxUpdateLag = maxUpdateLag
    , repositoryLeaseUpdateStartedAt =
        previous >>= repositoryLeaseUpdateStartedAt
    , repositoryLeaseSnapshot = previousSnapshot
    , repositoryLeaseLastError = Nothing
    }
  where
    previousSnapshot = previous >>= repositoryLeaseSnapshot

-- | Select a lease that still protects a repository at the supplied time.
-- The expiry boundary is inclusive, just like the peer reconciler.
selectActiveRepositoryLeaseAt
  :: Word64
  -> GitRepoKey
  -> [RepositoryLease]
  -> Maybe RepositoryLease
selectActiveRepositoryLeaseAt now repository = find isActive
  where
    isActive lease =
         repositoryLeaseRepository lease == repository
      && repositoryLeaseState lease /= RepositoryLeaseTerminated
      && repositoryLeaseLifetimeAt now lease /= RepositoryLeaseElapsed

-- | Add a TTL without allowing Word64 wraparound to turn a long lease into an
-- already-expired one.
leaseExpiryAt :: Word64 -> Word64 -> Word64
leaseExpiryAt now ttl = now + min ttl (maxBound - now)

positiveWord64 :: Integer -> Maybe Word64
positiveWord64 value
  | value > 0 && value <= fromIntegral (maxBound :: Word64) =
      Just (fromIntegral value)
  | otherwise = Nothing

nonNegativeWord64 :: Integer -> Maybe Word64
nonNegativeWord64 value
  | value >= 0 && value <= fromIntegral (maxBound :: Word64) =
      Just (fromIntegral value)
  | otherwise = Nothing

leaseStateName :: RepositoryLeaseState -> String
leaseStateName = \case
  RepositoryLeaseSyncing    -> "syncing"
  RepositoryLeaseActive     -> "active"
  RepositoryLeaseDegraded   -> "degraded"
  RepositoryLeaseGrace      -> "grace"
  RepositoryLeaseTerminated -> "terminated"
