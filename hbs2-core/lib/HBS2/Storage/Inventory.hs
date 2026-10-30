module HBS2.Storage.Inventory
  ( InventoryLimits(..)
  , InventoryStatus(..)
  , InventoryScan(..)
  , scanInventory
  , scanInventoryTyped
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
  , inventoryMalformed    :: Set HashRef
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
  scanInventoryTyped limits readBlock decode (((),) <$> roots)
  where
    decode () href bytes = Right (((),) <$> edges href bytes)

-- | Role-aware variant of 'scanInventory'.
--
-- A hash is charged once, while each distinct @(role, hash)@ is expanded once.
-- This lets a protocol distinguish, for example, a Merkle node from an opaque
-- Git payload even though both are ordinary CAS blocks.  Returning 'Left'
-- means the bytes hash correctly but are malformed for the expected role.
scanInventoryTyped
  :: (Monad m, Ord r)
  => InventoryLimits
  -> (HashRef -> m (Maybe ByteString))
  -> (r -> HashRef -> ByteString -> Either String [(r, HashRef)])
  -> [(r, HashRef)]
  -> m InventoryScan
scanInventoryTyped limits readBlock edges roots =
  go Set.empty Set.empty (Set.fromList roots) Map.empty Set.empty Set.empty Set.empty 0
  where
    go seen expanded pending blocks missing corrupt malformed total =
      case Set.minView pending of
        Nothing -> pure InventoryScan
          { inventoryStatus = InventoryTraversalComplete
          , inventoryBlocks = blocks
          , inventoryMissing = missing
          , inventoryCorrupt = corrupt
          , inventoryMalformed = malformed
          , inventoryLogicalBytes = total
          }

        Just (node@(_role, href), pending')
          | Set.member node expanded ->
              go seen expanded pending' blocks missing corrupt malformed total

          | Set.notMember href seen
          , fromIntegral (Set.size seen) >= inventoryMaxBlocks limits ->
              pure InventoryScan
                { inventoryStatus = InventoryBlockLimitExceeded href
                , inventoryBlocks = blocks
                , inventoryMissing = missing
                , inventoryCorrupt = corrupt
                , inventoryMalformed = malformed
                , inventoryLogicalBytes = total
                }

          | otherwise -> do
              let seen' = Set.insert href seen
                  expanded' = Set.insert node expanded
              readBlock href >>= \case
                Nothing ->
                  go seen' expanded' pending' blocks (Set.insert href missing)
                    corrupt malformed total

                Just bytes
                  | hashObject @HbSync bytes /= fromHashRef href ->
                      go seen' expanded' pending' blocks missing
                        (Set.insert href corrupt) malformed total

                  | Set.notMember href seen
                  , size > inventoryMaxBytes limits - total ->
                      pure InventoryScan
                        { inventoryStatus = InventoryByteLimitExceeded href
                        , inventoryBlocks = blocks
                        , inventoryMissing = missing
                        , inventoryCorrupt = corrupt
                        , inventoryMalformed = malformed
                        , inventoryLogicalBytes = total
                        }

                  | otherwise -> case edges _role href bytes of
                      Left{} ->
                        go seen' expanded' pending' blocks' missing corrupt
                          (Set.insert href malformed) total'

                      Right children ->
                        let pending'' = Set.union pending' (Set.fromList children)
                        in go seen' expanded' pending'' blocks' missing corrupt
                             malformed total'
                  where
                    size = fromIntegral (LBS.length bytes)
                    firstVisit = Set.notMember href seen
                    blocks' = if firstVisit then Map.insert href size blocks else blocks
                    total' = if firstVisit then total + size else total
