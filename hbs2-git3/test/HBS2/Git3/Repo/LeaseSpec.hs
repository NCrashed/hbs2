module HBS2.Git3.Repo.LeaseSpec (spec) where

import HBS2.Git3.Repo.Lease.Policy
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (HbSync, hashObject)
import HBS2.Net.Auth.Credentials
import HBS2.Peer.Proto.RepositoryLease

import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Word (Word64)
import Lens.Micro.Platform (view)
import Test.Hspec

spec :: Spec
spec = describe "repository lease policy" $ do
  it "creates a syncing lease with bounded expiry" $ do
    repository <- view peerSignPk <$> newCredentials @'HBS2Basic
    let lease = renewRepositoryLeaseAt 100 7 repository 200 300 (Just 20) Nothing
    repositoryLeaseId lease `shouldBe` makeRepositoryLeaseId repository 7
    repositoryLeaseState lease `shouldBe` RepositoryLeaseSyncing
    repositoryLeaseCreatedAt lease `shouldBe` 100
    repositoryLeaseUpdatedAt lease `shouldBe` 100
    repositoryLeaseExpiresAt lease `shouldBe` Just 120
    repositoryLeaseSnapshot lease `shouldBe` Nothing

  it "renews the durable identity and last verified snapshot" $ do
    repository <- view peerSignPk <$> newCredentials @'HBS2Basic
    refLog <- view peerSignPk <$> newCredentials @'HBS2Basic
    let ref label = HashRef (hashObject @HbSync (LBS.pack label))
        snapshot = RepositorySnapshot
          { repositorySnapshotId = ref "snapshot"
          , repositorySnapshotLWWRefBlock = ref "lww"
          , repositorySnapshotLWWSequence = 4
          , repositorySnapshotHeadRoot = ref "head"
          , repositorySnapshotRefLogKey = refLog
          , repositorySnapshotRefLogRoot = ref "reflog"
          , repositorySnapshotInventoryRoot = ref "inventory"
          , repositorySnapshotPinRoot = ref "pin"
          , repositorySnapshotBlockCount = 12
          , repositorySnapshotLogicalBytes = 34
          , repositorySnapshotCompletedAt = 56
          }
        previous = (renewRepositoryLeaseAt 100 7 repository 200 300 Nothing Nothing)
          { repositoryLeaseState = RepositoryLeaseActive
          , repositoryLeaseSnapshot = Just snapshot
          , repositoryLeaseLastError = Just "old failure"
          }
        renewed = renewRepositoryLeaseAt 200 8 repository 400 500 Nothing
          (Just previous)
    repositoryLeaseId renewed `shouldBe` repositoryLeaseId previous
    repositoryLeaseCreatedAt renewed `shouldBe` 100
    repositoryLeaseState renewed `shouldBe` RepositoryLeaseDegraded
    repositoryLeaseSnapshot renewed `shouldBe` Just snapshot
    repositoryLeaseLastError renewed `shouldBe` Nothing

  it "publishes finite defaults" $ do
    defaultLeaseMaxBytes `shouldBe` 10 * 1024 * 1024 * 1024
    defaultLeaseMaxBlocks `shouldBe` 10 * 1000 * 1000

  it "saturates TTL arithmetic instead of wrapping it" $ do
    leaseExpiryAt (maxBound - 2) 2 `shouldBe` (maxBound :: Word64)
    leaseExpiryAt (maxBound - 2) 3 `shouldBe` (maxBound :: Word64)

  it "accepts only positive Word64 option values" $ do
    positiveWord64 1 `shouldBe` Just 1
    positiveWord64 0 `shouldBe` Nothing
    positiveWord64 (-1) `shouldBe` Nothing
    positiveWord64 (fromIntegral (maxBound :: Word64) + 1) `shouldBe` Nothing

  it "treats the expiry boundary and termination as inactive" $ do
    repository <- view peerSignPk <$> newCredentials @'HBS2Basic
    otherRepository <- view peerSignPk <$> newCredentials @'HBS2Basic
    let lease = renewRepositoryLeaseAt 10 7 repository 200 300 (Just 10) Nothing
        other = renewRepositoryLeaseAt 10 8 otherRepository 200 300 Nothing Nothing
    selectActiveRepositoryLeaseAt 19 repository [other, lease]
      `shouldBe` Just lease
    selectActiveRepositoryLeaseAt 20 repository [lease]
      `shouldBe` Nothing
    selectActiveRepositoryLeaseAt 19 repository
      [lease { repositoryLeaseState = RepositoryLeaseTerminated }]
      `shouldBe` Nothing

  it "names every externally visible state" $ do
    map leaseStateName
      [ RepositoryLeaseSyncing
      , RepositoryLeaseActive
      , RepositoryLeaseDegraded
      , RepositoryLeaseGrace
      , RepositoryLeaseTerminated
      ] `shouldBe` ["syncing", "active", "degraded", "grace", "terminated"]
