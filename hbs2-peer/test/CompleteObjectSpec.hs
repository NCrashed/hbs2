module CompleteObjectSpec (tests) where

import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (HbSync, Hash, hashObject)
import HBS2.Merkle (MTree(..), newMNode)
import HBS2.Peer.CompleteObject
import HBS2.Storage (AnyStorage(..), Storage(..))
import HBS2.Storage.Simple (StoragePrefix(..), simpleStorageInit, simpleStorageWorker)

import Codec.Serialise (serialise)
import Control.Concurrent.Async (async, cancel)
import Control.Exception (bracket)
import Data.ByteString.Lazy (ByteString)
import Data.HashSet qualified as HashSet
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests = testGroup "complete merkle object"
  [ testCase "enumerates the root and every reachable physical block" $ withStore $ \sto -> do
      let (root, blocks) = twoBlockTree
      mapM_ (putBlock sto) blocks

      inspectCompleteObject sto (HashRef root) >>= \case
        Left err -> fail (show err)
        Right found ->
          HashSet.fromList found @?= HashSet.fromList (HashRef . hashObject <$> blocks)

  , testCase "reports the exact missing descendant" $ withStore $ \sto -> do
      let (root, blocks) = twoBlockTree
          leaf = head blocks
          rootBlock = last blocks
      _ <- putBlock sto rootBlock

      result <- inspectCompleteObject sto (HashRef root)
      result @?= Left (CompleteObjectIncomplete (HashRef root) [HashRef (hashObject leaf)])
  ]

twoBlockTree :: (Hash HbSync, [ByteString])
twoBlockTree = (rootHash, [leafBlock, rootBlock])
  where
    leafBlock = serialise (MLeaf [] :: MTree [HashRef])
    leafHash = hashObject leafBlock
    rootBlock = serialise (newMNode 1 [leafHash] :: MTree [HashRef])
    rootHash = hashObject rootBlock

withStore :: (AnyStorage -> IO a) -> IO a
withStore action =
  withSystemTempDirectory "hbs2-complete-object" $ \dir -> do
    storage <- simpleStorageInit @HbSync [StoragePrefix (dir </> "storage")]
    bracket (async (simpleStorageWorker storage)) cancel $ \_ ->
      action (AnyStorage storage)
