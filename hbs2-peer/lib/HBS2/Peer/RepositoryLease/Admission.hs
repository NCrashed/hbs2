module HBS2.Peer.RepositoryLease.Admission
  ( RepositoryLeaseAdmissionPolicy(..)
  , RepositoryLeaseCapacity(..)
  , RepositoryLeaseAdmissionError(..)
  , defaultRepositoryLeaseBudgetBytes
  , defaultRepositoryLeaseMaxContracts
  , repositoryLeaseReservedBytesAt
  , repositoryLeaseCapacityAt
  , admitRepositoryLeaseAt
  , repositoryLeaseAdmissionErrorMessage
  ) where

import HBS2.Peer.Proto.RepositoryLease

import Codec.Serialise (Serialise)
import Data.List (foldl')
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)

data RepositoryLeaseAdmissionPolicy = RepositoryLeaseAdmissionPolicy
  { repositoryLeaseBudgetBytes  :: Word64
  , repositoryLeaseMaxContracts :: Word64
  }
  deriving stock (Eq, Generic, Show)

instance Serialise RepositoryLeaseAdmissionPolicy

data RepositoryLeaseCapacity = RepositoryLeaseCapacity
  { repositoryLeaseCapacityBudgetBytes :: Word64
  , repositoryLeaseCapacityReservedBytes :: Word64
  , repositoryLeaseCapacityAvailableBytes :: Word64
  , repositoryLeaseCapacityContracts :: Word64
  , repositoryLeaseCapacityMaxContracts :: Word64
  }
  deriving stock (Eq, Generic, Show)

instance Serialise RepositoryLeaseCapacity

data RepositoryLeaseAdmissionError
  = RepositoryLeaseCapacityExceeded Word64 Word64
  | RepositoryLeaseContractLimitExceeded Word64
  | RepositoryLeaseUnsupportedVersion Word16
  | RepositoryLeaseInvalidLimit
  | RepositoryLeaseAlreadyElapsed
  | RepositoryLeaseAdmissionStorageFailure
  deriving stock (Eq, Generic, Show)

instance Serialise RepositoryLeaseAdmissionError

-- | The provider-local defaults preserve the original single 10 GiB manual
-- lease while making aggregate overcommit impossible unless the operator
-- explicitly raises the donated budget.
defaultRepositoryLeaseBudgetBytes :: Word64
defaultRepositoryLeaseBudgetBytes = 10 * 1024 * 1024 * 1024

defaultRepositoryLeaseMaxContracts :: Word64
defaultRepositoryLeaseMaxContracts = 64

repositoryLeaseReservedBytesAt :: Word64 -> [RepositoryLease] -> Word64
repositoryLeaseReservedBytesAt now = foldl' reserve 0 . filter (reservesCapacity now)
  where
    reserve total lease =
      total + min (repositoryLeaseMaxBytes lease) (maxBound - total)

repositoryLeaseCapacityAt
  :: Word64
  -> RepositoryLeaseAdmissionPolicy
  -> [RepositoryLease]
  -> RepositoryLeaseCapacity
repositoryLeaseCapacityAt now RepositoryLeaseAdmissionPolicy{..} leases =
  RepositoryLeaseCapacity
    { repositoryLeaseCapacityBudgetBytes = repositoryLeaseBudgetBytes
    , repositoryLeaseCapacityReservedBytes = reserved
    , repositoryLeaseCapacityAvailableBytes =
        repositoryLeaseBudgetBytes - min repositoryLeaseBudgetBytes reserved
    , repositoryLeaseCapacityContracts = fromIntegral (length reserving)
    , repositoryLeaseCapacityMaxContracts = repositoryLeaseMaxContracts
    }
  where
    reserving = filter (reservesCapacity now) leases
    reserved = repositoryLeaseReservedBytesAt now reserving

admitRepositoryLeaseAt
  :: Word64
  -> RepositoryLeaseAdmissionPolicy
  -> RepositoryLease
  -> [RepositoryLease]
  -> Either RepositoryLeaseAdmissionError RepositoryLeaseCapacity
admitRepositoryLeaseAt now policy candidate leases
  | repositoryLeaseState candidate == RepositoryLeaseTerminated =
      Right capacityWithoutCandidate
  | not (repositoryLeaseVersionSupported (repositoryLeaseVersion candidate)) =
      Left (RepositoryLeaseUnsupportedVersion (repositoryLeaseVersion candidate))
  | repositoryLeaseMaxBytes candidate == 0
      || repositoryLeaseMaxBlocks candidate == 0 =
      Left RepositoryLeaseInvalidLimit
  | repositoryLeaseLifetimeAt now candidate /= RepositoryLeaseUnexpired =
      Left RepositoryLeaseAlreadyElapsed
  | repositoryLeaseCapacityContracts capacityWithoutCandidate
      >= repositoryLeaseMaxContracts policy =
      Left (RepositoryLeaseContractLimitExceeded
        (repositoryLeaseMaxContracts policy))
  | repositoryLeaseMaxBytes candidate
      > repositoryLeaseCapacityAvailableBytes capacityWithoutCandidate =
      Left (RepositoryLeaseCapacityExceeded
        (repositoryLeaseMaxBytes candidate)
        (repositoryLeaseCapacityAvailableBytes capacityWithoutCandidate))
  | otherwise = Right (repositoryLeaseCapacityAt now policy
      (candidate : leasesWithoutCandidate))
  where
    leasesWithoutCandidate = filter ((/= repositoryLeaseRepository candidate)
      . repositoryLeaseRepository) leases
    capacityWithoutCandidate = repositoryLeaseCapacityAt now policy
      leasesWithoutCandidate

reservesCapacity :: Word64 -> RepositoryLease -> Bool
reservesCapacity now lease =
     repositoryLeaseState lease /= RepositoryLeaseTerminated
  && repositoryLeaseLifetimeAt now lease /= RepositoryLeaseElapsed

repositoryLeaseAdmissionErrorMessage :: RepositoryLeaseAdmissionError -> String
repositoryLeaseAdmissionErrorMessage = \case
  RepositoryLeaseCapacityExceeded requested available ->
    "repository lease capacity exceeded: requested " <> show requested
      <> " bytes, available " <> show available
  RepositoryLeaseContractLimitExceeded limit ->
    "repository lease contract limit exceeded: " <> show limit
  RepositoryLeaseUnsupportedVersion version ->
    "unsupported repository lease version: " <> show version
  RepositoryLeaseInvalidLimit ->
    "repository lease byte and block limits must be positive"
  RepositoryLeaseAlreadyElapsed ->
    "repository lease already elapsed"
  RepositoryLeaseAdmissionStorageFailure ->
    "repository lease admission could not be stored"
