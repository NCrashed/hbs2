module HBS2.Storage.Inventory
  ( InventoryLimits(..)
  , InventoryStatus(..)
  , InventoryScan(..)
  , scanInventory
  ) where

import HBS2.Data.Types.Refs
import HBS2.Hash

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word

-- | Hard bounds for walking a graph supplied by another peer.
--
-- Both bounds describe work admitted by the caller, not a valid inventory.
-- Hitting either one leaves the scan incomplete and must never produce an
-- Active lease or a replica advertisement.
data InventoryLimits = InventoryLimits
  { inventoryMaxBlocks :: Word64
  , inventoryMaxBytes  :: Word64
  }
  deriving stock (Eq, Show)

data InventoryStatus
  = InventoryTraversalComplete
  | InventoryBlockLimitExceeded HashRef
  | InventoryByteLimitExceeded HashRef
  deriving stock (Eq, Show)

-- | The deterministic result of examining a set of content-addressed roots.
--
-- 'inventoryBlocks' is a set represented as a map to the exact stored byte
-- length.  Its 'Ord HashRef' order is the canonical enumeration order.  A
-- missing or corrupt block is not counted as stored logical bytes.
data InventoryScan = InventoryScan
  { inventoryStatus       :: InventoryStatus
  , inventoryBlocks       :: Map HashRef Word64
  , inventoryMissing      :: Set HashRef
  , inventoryCorrupt      :: Set HashRef
  , inventoryLogicalBytes :: Word64
  }
  deriving stock (Eq, Show)

-- | Walk a content-addressed graph once per distinct hash.
--
-- The edge decoder is deliberately supplied by the protocol which owns the
-- root.  Guessing a block type with 'HBS2.Data.Detect.tryDetect' is useful for
-- diagnostics, but is not canonical: several hbs2 structures are untagged
-- CBOR and a RefLogUpdate hides its transaction root inside the signed update.
-- A repository inventory therefore composes this walker with explicit
-- LWWRef, repository-head, RefLogUpdate, and GitTx decoders.
--
-- Pending hashes are visited in 'Ord HashRef' order.  Consequently roots and
-- child edges may arrive in any order without changing either a complete
-- result or the first hash reported at a limit.
scanInventory
  :: Monad m
  => InventoryLimits
  -> (HashRef -> m (Maybe ByteString))
  -> (HashRef -> ByteString -> [HashRef])
  -> [HashRef]
  -> m InventoryScan
scanInventory limits readBlock edges roots =
  go Set.empty (Set.fromList roots) Map.empty Set.empty Set.empty 0
  where
    go seen pending blocks missing corrupt total =
      case Set.minView pending of
        Nothing -> pure InventoryScan
          { inventoryStatus = InventoryTraversalComplete
          , inventoryBlocks = blocks
          , inventoryMissing = missing
          , inventoryCorrupt = corrupt
          , inventoryLogicalBytes = total
          }

        Just (href, pending')
          | Set.member href seen ->
              go seen pending' blocks missing corrupt total

          | fromIntegral (Set.size seen) >= inventoryMaxBlocks limits ->
              pure InventoryScan
                { inventoryStatus = InventoryBlockLimitExceeded href
                , inventoryBlocks = blocks
                , inventoryMissing = missing
                , inventoryCorrupt = corrupt
                , inventoryLogicalBytes = total
                }

          | otherwise -> do
              let seen' = Set.insert href seen
              readBlock href >>= \case
                Nothing ->
                  go seen' pending' blocks (Set.insert href missing) corrupt total

                Just bytes
                  | hashObject @HbSync bytes /= fromHashRef href ->
                      go seen' pending' blocks missing (Set.insert href corrupt) total

                  | size > inventoryMaxBytes limits - total ->
                      pure InventoryScan
                        { inventoryStatus = InventoryByteLimitExceeded href
                        , inventoryBlocks = blocks
                        , inventoryMissing = missing
                        , inventoryCorrupt = corrupt
                        , inventoryLogicalBytes = total
                        }

                  | otherwise ->
                      let blocks' = Map.insert href size blocks
                          pending'' = Set.union pending' (Set.fromList (edges href bytes))
                      in go seen' pending'' blocks' missing corrupt (total + size)
                  where
                    size = fromIntegral (LBS.length bytes)
