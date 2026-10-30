{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TestStorageInventory
  ( testInventoryDeterministic
  , testInventoryReportsBadFrontier
  , testInventoryBoundsWork
  ) where

import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Storage.Inventory

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Test.Tasty.HUnit

block :: ByteString -> (HashRef, ByteString)
block bytes = (HashRef (hashObject @HbSync bytes), bytes)

limits :: InventoryLimits
limits = InventoryLimits 100 100000

fixture :: (Map HashRef ByteString, Map HashRef [HashRef], [HashRef])
fixture = (blocks, graph, [rootHash, leftHash])
  where
    root@(rootHash, _) = block "root"
    left@(leftHash, _) = block "left"
    right@(rightHash, _) = block "right"
    shared@(sharedHash, _) = block "shared"

    blocks = Map.fromList [root, left, right, shared]
    graph = Map.fromList
      [ (rootHash, [rightHash, leftHash, rightHash])
      , (leftHash, [sharedHash])
      , (rightHash, [sharedHash])
      , (sharedHash, [rootHash])
      ]

reader :: Map HashRef ByteString -> HashRef -> IO (Maybe ByteString)
reader blocks href = pure (Map.lookup href blocks)

decoder :: Map HashRef [HashRef] -> HashRef -> ByteString -> [HashRef]
decoder graph href _ = Map.findWithDefault [] href graph

-- | Shared children, duplicate edges, cycles, and root order do not change a
-- canonical inventory or charge the same block twice.
testInventoryDeterministic :: IO ()
testInventoryDeterministic = do
  let (blocks, graph, roots) = fixture
  a <- scanInventory limits (reader blocks) (decoder graph) roots
  b <- scanInventory limits (reader blocks) (decoder graph) (reverse roots)

  a @?= b
  inventoryStatus a @?= InventoryTraversalComplete
  Map.keysSet (inventoryBlocks a) @?= Map.keysSet blocks
  inventoryLogicalBytes a @?= fromIntegral (sum (LBS.length <$> Map.elems blocks))
  inventoryMissing a @?= Set.empty
  inventoryCorrupt a @?= Set.empty

-- | Missing bytes and bytes returned under the wrong hash are different
-- evidence and neither contributes stored logical bytes.
testInventoryReportsBadFrontier :: IO ()
testInventoryReportsBadFrontier = do
  let good@(goodHash, goodBytes) = block "good"
      (missingHash, _) = block "missing"
      (corruptHash, _) = block "expected"
      blocks = Map.fromList [good, (corruptHash, "somebody else's bytes")]
      graph = Map.singleton goodHash [missingHash, corruptHash]

  result <- scanInventory limits (reader blocks) (decoder graph) [goodHash]

  inventoryStatus result @?= InventoryTraversalComplete
  inventoryBlocks result @?= Map.singleton goodHash (fromIntegral (LBS.length goodBytes))
  inventoryMissing result @?= Set.singleton missingHash
  inventoryCorrupt result @?= Set.singleton corruptHash

-- | Bounds stop traversal deterministically; a partial result is explicitly
-- not complete and cannot be mistaken for a small valid inventory.
testInventoryBoundsWork :: IO ()
testInventoryBoundsWork = do
  let (blocks, graph, roots) = fixture

  byBlocks <- scanInventory (InventoryLimits 2 100000)
                (reader blocks) (decoder graph) roots
  case inventoryStatus byBlocks of
    InventoryBlockLimitExceeded _ -> pure ()
    other -> assertFailure ("expected block limit, got " <> show other)

  let smallestBlock = minimum (LBS.length <$> Map.elems blocks)
  byBytes <- scanInventory (InventoryLimits 100 (fromIntegral smallestBlock - 1))
               (reader blocks) (decoder graph) roots
  case inventoryStatus byBytes of
    InventoryByteLimitExceeded _ -> pure ()
    other -> assertFailure ("expected byte limit, got " <> show other)
