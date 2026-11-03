{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TestStorageInventory
  ( testInventoryDeterministic
  , testInventoryReportsBadFrontier
  , testInventoryBoundsWork
  , testInventoryBlockLimitStartsAtZero
  , testInventoryByteLimitIsCumulative
  , testTypedInventoryRejectsMalformedRole
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
  inventoryMalformed a @?= Set.empty

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
  inventoryMalformed result @?= Set.empty

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

-- | A zero block budget must reject the first unseen block.  This boundary is
-- easy to lose by turning the inclusive limit check into a strict one.
testInventoryBlockLimitStartsAtZero :: IO ()
testInventoryBlockLimitStartsAtZero = do
  let item@(href, _) = block "root"
      blocks = Map.singleton href (snd item)

  result <- scanInventory (InventoryLimits 0 100000)
              (reader blocks) (const (const [])) [href]

  inventoryStatus result @?= InventoryBlockLimitExceeded href
  inventoryBlocks result @?= Map.empty

-- | A block which exactly fills the byte budget is admitted, but the next
-- block is rejected against the remaining budget rather than the original
-- total.
testInventoryByteLimitIsCumulative :: IO ()
testInventoryByteLimitIsCumulative = do
  let root@(rootHash, rootBytes) = block "root"
      child@(childHash, _) = block "child"
      blocks = Map.fromList [root, child]
      graph = Map.singleton rootHash [childHash]

  result <- scanInventory
              (InventoryLimits 2 (fromIntegral (LBS.length rootBytes)))
              (reader blocks) (decoder graph) [rootHash]

  inventoryStatus result @?= InventoryByteLimitExceeded childHash
  inventoryBlocks result @?=
    Map.singleton rootHash (fromIntegral (LBS.length rootBytes))

data Role = Tree | Opaque
  deriving stock (Eq, Ord, Show)

-- | The same bytes are not guessed into a structure: the role supplied by the
-- owning protocol decides whether they must decode and expose children.
testTypedInventoryRejectsMalformedRole :: IO ()
testTypedInventoryRejectsMalformedRole = do
  let item@(href, _) = block "not a tree"
      blocks = Map.singleton href (snd item)
      decode Opaque _ _ = Right []
      decode Tree h _ = Left ("not a tree: " <> show h)

  result <- scanInventoryTyped limits (reader blocks) decode
              [(Opaque, href), (Tree, href)]

  inventoryStatus result @?= InventoryTraversalComplete
  Map.size (inventoryBlocks result) @?= 1
  inventoryMalformed result @?= Set.singleton href
