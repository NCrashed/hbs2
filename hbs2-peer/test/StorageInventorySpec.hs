module StorageInventorySpec (tests) where

import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Peer.StorageInventory

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.List qualified as List
import Data.Word (Word64)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "storage inventory report"
  [ testCase "accounts shared roots and explains retention" testSharedRoots
  , testCase "unknown root sources suppress cleanup" testIncompleteCoverage
  , testCase "JSON exposes stable operator fields" testStableJSON
  , testCase "cleanup accepts only the same complete stable snapshot" testCleanupSnapshot
  , testCase "cleanup refuses concurrent physical and root changes" testCleanupRaces
  ]

href :: String -> HashRef
href = HashRef . hashObject @HbSync . LBS8.pack

fixture :: (Map.Map HashRef Word64, [StorageRetentionRoot], HashRef, HashRef)
fixture =
  ( Map.fromList [(a, 10), (b, 20), (shared, 5), (orphan, 7)]
  , [ StorageRetentionRoot "repo" "repository" a
        (Set.singleton "poll:lwwref") True
        (Map.fromList [(a, 10), (shared, 5)])
    , StorageRetentionRoot "sync" "file-sync" b
        (Set.singleton "explicit-pin") False
        (Map.fromList [(b, 20), (shared, 5)])
    ]
  , shared
  , orphan
  )
  where
    a = href "repo"
    b = href "sync"
    shared = href "shared"
    orphan = href "orphan"

testSharedRoots :: Assertion
testSharedRoots = do
  let (physical, roots, shared, orphan) = fixture
  report <- either (assertFailure . show) pure $
    makeStorageInventoryReport True Set.empty physical roots

  storageInventoryPhysicalBytes report @?= 42
  storageInventoryRetainedBytes report @?= 35
  storageHashDisposition report shared @?=
    StorageHashRetained 5 ["repo", "sync"]
  storageHashDisposition report orphan @?= StorageHashReclaimable 7
  fmap storageInventoryRootSharedBytes (storageInventoryRoots report) @?= [5, 5]

testIncompleteCoverage :: Assertion
testIncompleteCoverage = do
  let (physical, roots, _, orphan) = fixture
  report <- either (assertFailure . show) pure $
    makeStorageInventoryReport True (Set.singleton "poll:refchan") physical roots

  storageInventoryReclaimable report @?= Map.empty
  storageHashDisposition report orphan @?= StorageHashUnattributed 7

testStableJSON :: Assertion
testStableJSON = do
  let (physical, roots, _, _) = fixture
  report <- either (assertFailure . show) pure $
    makeStorageInventoryReport True Set.empty physical roots
  let encoded = Aeson.encode report
  let json = LBS8.unpack encoded
  assertBool "stable flag missing" ("\"stable\":true" `List.isInfixOf` json)
  assertBool "reclaimable bytes missing"
    ("\"reclaimableBytes\":7" `List.isInfixOf` json)
  assertBool "root reasons missing" ("poll:lwwref" `List.isInfixOf` json)

testCleanupSnapshot :: Assertion
testCleanupSnapshot = do
  let (physical, roots, _, orphan) = fixture
  report <- either (assertFailure . show) pure $
    makeStorageInventoryReport True Set.empty physical roots
  validateStorageCleanupSnapshot report report @?=
    Right (Map.singleton orphan 7)

testCleanupRaces :: Assertion
testCleanupRaces = do
  let (physical, roots, _, _) = fixture
  planned <- either (assertFailure . show) pure $
    makeStorageInventoryReport True Set.empty physical roots
  physicalChanged <- either (assertFailure . show) pure $
    makeStorageInventoryReport True Set.empty
      (Map.insert (href "concurrent") 3 physical) roots
  rootsChanged <- either (assertFailure . show) pure $
    makeStorageInventoryReport True Set.empty physical
      (StorageRetentionRoot "late" "file-sync" (href "orphan")
        (Set.singleton "explicit-pin") False
        (Map.singleton (href "orphan") 7) : roots)
  incomplete <- either (assertFailure . show) pure $
    makeStorageInventoryReport True (Set.singleton "unknown root") physical roots

  assertBool "physical race accepted" $
    either (const True) (const False) $
      validateStorageCleanupSnapshot planned physicalChanged
  assertBool "root race accepted" $
    either (const True) (const False) $
      validateStorageCleanupSnapshot planned rootsChanged
  assertBool "incomplete coverage accepted" $
    either (const True) (const False) $
      validateStorageCleanupSnapshot planned incomplete
