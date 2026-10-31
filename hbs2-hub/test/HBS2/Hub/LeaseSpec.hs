module HBS2.Hub.LeaseSpec (spec) where

import HBS2.Hub.CLI.Lease
import HBS2.Hub.Types (HubKey)

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject)
import HBS2.Net.Auth.Credentials
import HBS2.Peer.Proto.RepositoryLease
import HBS2.Peer.RepositoryLease.Admission
import HBS2.Prelude.Plated (Doc,pretty)

import Data.Aeson (Value(..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Test.Hspec
import Text.Read (readMaybe)

mh :: String -> HashRef
mh = HashRef . hashObject . LBS.pack

said :: [Doc ()] -> String
said = unlines . fmap show

at :: [String] -> Value -> Maybe Value
at [] value = Just value
at (name:names) (Object value) = KM.lookup (Key.fromString name) value >>= at names
at (index:names) (Array values) = do
  offset <- readMaybe index
  value <- listToMaybe (drop offset (toList values))
  at names value
at _ _ = Nothing

lease :: HubKey -> RepositoryLease
lease repository = RepositoryLease
  { repositoryLeaseVersion = repositoryLeaseCurrentVersion
  , repositoryLeaseId = RepositoryLeaseId (mh "lease")
  , repositoryLeaseRepository = repository
  , repositoryLeaseState = RepositoryLeaseActive
  , repositoryLeaseMaxBytes = 1000
  , repositoryLeaseMaxBlocks = 100
  , repositoryLeaseCreatedAt = 10
  , repositoryLeaseUpdatedAt = 20
  , repositoryLeaseExpiresAt = Just 100
  , repositoryLeaseGracePeriod = 30
  , repositoryLeaseMaxUpdateLag = 40
  , repositoryLeaseUpdateStartedAt = Nothing
  , repositoryLeaseSnapshot = Just RepositorySnapshot
      { repositorySnapshotId = mh "snapshot"
      , repositorySnapshotLWWRefBlock = mh "lww"
      , repositorySnapshotLWWSequence = 1
      , repositorySnapshotHeadRoot = mh "head"
      , repositorySnapshotRefLogKey = repository
      , repositorySnapshotRefLogRoot = mh "reflog"
      , repositorySnapshotInventoryRoot = mh "inventory"
      , repositorySnapshotPinRoot = mh "pin"
      , repositorySnapshotBlockCount = 7
      , repositorySnapshotLogicalBytes = 700
      , repositorySnapshotCompletedAt = 19
      }
  , repositoryLeaseLastError = Nothing
  }

spec :: Spec
spec = describe "hub lease status" $ do
  it "reports configured capacity and active durable leases" $ do
    repository <- _peerSignPk <$> newCredentials @'HBS2Basic
    let standing = LeaseStanding
          { leaseCapacity = Right (RepositoryLeaseCapacity 10000 1000 9000 1 64)
          , leaseRecords = Right [lease repository]
          }
        contract = leaseStatusContract standing
    at ["document"] contract `shouldBe` Just (String "lease-status")
    at ["capacity","budget_bytes"] contract `shouldBe` Just (Number 10000)
    at ["capacity","reserved_bytes"] contract `shouldBe` Just (Number 1000)
    at ["capacity","available_bytes"] contract `shouldBe` Just (Number 9000)
    at ["capacity","contracts"] contract `shouldBe` Just (Number 1)
    at ["capacity","max_contracts"] contract `shouldBe` Just (Number 64)
    at ["leases","0","repository"] contract
      `shouldBe` Just (String (Text.pack (show (pretty (AsBase58 repository)))))
    at ["leases","0","state"] contract `shouldBe` Just (String "active")
    at ["leases","0","snapshot","bytes"] contract `shouldBe` Just (Number 700)

  it "never turns an unanswered peer into zero donated capacity or leases" $ do
    let contract = leaseStatusContract LeaseStanding
          { leaseCapacity = Left "capacity RPC timed out"
          , leaseRecords = Left "lease list RPC timed out"
          }
    at ["capacity","budget_bytes"] contract `shouldBe` Just Null
    at ["capacity","contracts"] contract `shouldBe` Just Null
    at ["capacity","error"] contract `shouldBe` Just (String "capacity RPC timed out")
    at ["leases"] contract `shouldBe` Just Null
    at ["leases_error"] contract `shouldBe` Just (String "lease list RPC timed out")

  it "names the peer configuration which controls donated capacity" $ do
    let report = said (leaseStatusDoc LeaseStanding
          { leaseCapacity = Right (RepositoryLeaseCapacity 10000 1000 9000 1 64)
          , leaseRecords = Right []
          })
    report `shouldSatisfy` isInfixOf "barter-budget-bytes"
    report `shouldSatisfy` isInfixOf "barter-max-contracts"
    report `shouldSatisfy` isInfixOf "no repository leases"
