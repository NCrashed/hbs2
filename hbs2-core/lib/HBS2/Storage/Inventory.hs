module HBS2.Storage.Inventory
  ( InventoryLimits(..)
  , InventoryStatus(..)
  , InventoryScan(..)
  , scanInventory
  , scanInventoryTyped
  , InventoryRoot(..)
  , InventoryRootsCoverage(..)
  , InventoryRootAccounting(..)
  , InventoryAccounting(..)
  , InventoryAccountingError(..)
  , accountInventory
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
import Data.Text (Text)
import Control.Monad (foldM_)

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

-- | One application-owned retention root and the exact blocks reachable from
-- its current snapshot.  The root key is deliberately supplied by the owning
-- protocol: storage accounting must not guess application identity from CAS
-- bytes.
data InventoryRoot root = InventoryRoot
  { inventoryRootKey    :: root
  , inventoryRootBlocks :: Map HashRef Word64
  }
  deriving stock (Eq, Show)

-- | Whether every live source of retention roots participated in a snapshot.
-- Unknown roots make unattributed bytes observable, but never reclaimable.
data InventoryRootsCoverage
  = InventoryRootsComplete
  | InventoryRootsIncomplete (Set Text)
  deriving stock (Eq, Show)

data InventoryRootAccounting root = InventoryRootAccounting
  { inventoryRootAccountingKey         :: root
  , inventoryRootAccountingLogicalBytes :: Word64
  , inventoryRootAccountingUniqueBytes  :: Word64
  , inventoryRootAccountingSharedBytes  :: Word64
  , inventoryRootAccountingMissingBytes :: Word64
  }
  deriving stock (Eq, Show)

data InventoryAccounting root = InventoryAccounting
  { inventoryAccountingCoverage       :: InventoryRootsCoverage
  , inventoryAccountingRoots          :: [InventoryRootAccounting root]
  , inventoryAccountingRetained       :: Map HashRef Word64
  , inventoryAccountingUnattributed   :: Map HashRef Word64
  , inventoryAccountingReclaimable    :: Map HashRef Word64
  , inventoryAccountingPhysicalBytes  :: Word64
  , inventoryAccountingRetainedBytes  :: Word64
  }
  deriving stock (Eq, Show)

data InventoryAccountingError
  = InventoryBlockSizeConflict HashRef Word64 Word64
  deriving stock (Eq, Show)

-- | Account one physical store against a complete set of application roots.
--
-- A hash is charged once globally.  Per-root unique bytes are blocks named by
-- exactly one canonical root; shared bytes remain retained until the last
-- root disappears.  An incomplete root registry is fail-closed: unattributed
-- data is reported, while the cleanup candidate set stays empty.
accountInventory
  :: Ord root
  => InventoryRootsCoverage
  -> Map HashRef Word64
  -> [InventoryRoot root]
  -> Either InventoryAccountingError (InventoryAccounting root)
accountInventory coverage physical suppliedRoots = do
  validateBlockSizes physical suppliedRoots
  let roots = canonicalRoots suppliedRoots
      references = Map.unionsWith (+)
        [ Map.map (const (1 :: Word64)) blocks
        | blocks <- Map.elems roots
        ]
      retained = Map.restrictKeys physical (Map.keysSet references)
      unattributed = Map.withoutKeys physical (Map.keysSet references)
      reclaimable = case coverage of
        InventoryRootsComplete -> unattributed
        InventoryRootsIncomplete{} -> Map.empty
      rootAccounting =
        [ accountRoot physical references root blocks
        | (root, blocks) <- Map.toAscList roots
        ]
  pure InventoryAccounting
    { inventoryAccountingCoverage = coverage
    , inventoryAccountingRoots = rootAccounting
    , inventoryAccountingRetained = retained
    , inventoryAccountingUnattributed = unattributed
    , inventoryAccountingReclaimable = reclaimable
    , inventoryAccountingPhysicalBytes = sum (Map.elems physical)
    , inventoryAccountingRetainedBytes = sum (Map.elems retained)
    }

canonicalRoots
  :: Ord root
  => [InventoryRoot root]
  -> Map root (Map HashRef Word64)
canonicalRoots = Map.fromListWith Map.union
  . fmap (\InventoryRoot{..} -> (inventoryRootKey, inventoryRootBlocks))

validateBlockSizes
  :: Map HashRef Word64
  -> [InventoryRoot root]
  -> Either InventoryAccountingError ()
validateBlockSizes physical roots =
  foldM_ addEvidence physical
    [ entry
    | InventoryRoot{..} <- roots
    , entry <- Map.toList inventoryRootBlocks
    ]
  where
    addEvidence known (href, size) = case Map.lookup href known of
      Nothing -> pure (Map.insert href size known)
      Just expected
        | expected == size -> pure known
        | otherwise -> Left (InventoryBlockSizeConflict href expected size)

accountRoot
  :: Map HashRef Word64
  -> Map HashRef Word64
  -> root
  -> Map HashRef Word64
  -> InventoryRootAccounting root
accountRoot physical references root blocks = InventoryRootAccounting
  { inventoryRootAccountingKey = root
  , inventoryRootAccountingLogicalBytes = logical
  , inventoryRootAccountingUniqueBytes = unique
  , inventoryRootAccountingSharedBytes = shared
  , inventoryRootAccountingMissingBytes = missing
  }
  where
    logical = sum (Map.elems blocks)
    unique = sum
      [ size
      | (href, size) <- Map.toList blocks
      , Map.lookup href physical == Just size
      , Map.lookup href references == Just 1
      ]
    shared = sum
      [ size
      | (href, size) <- Map.toList blocks
      , Map.lookup href physical == Just size
      , references Map.! href > 1
      ]
    missing = logical - unique - shared

data InventoryWalk r = InventoryWalk
  { walkSeen       :: Set HashRef
  , walkExpanded   :: Set (r, HashRef)
  , walkPending    :: Set (r, HashRef)
  , walkBlocks     :: Map HashRef Word64
  , walkMissing    :: Set HashRef
  , walkCorrupt    :: Set HashRef
  , walkMalformed  :: Set HashRef
  , walkTotalBytes :: Word64
  }

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
  scanNext limits readBlock edges InventoryWalk
    { walkSeen = Set.empty
    , walkExpanded = Set.empty
    , walkPending = Set.fromList roots
    , walkBlocks = Map.empty
    , walkMissing = Set.empty
    , walkCorrupt = Set.empty
    , walkMalformed = Set.empty
    , walkTotalBytes = 0
    }

scanNext
  :: (Monad m, Ord r)
  => InventoryLimits
  -> (HashRef -> m (Maybe ByteString))
  -> (r -> HashRef -> ByteString -> Either String [(r, HashRef)])
  -> InventoryWalk r
  -> m InventoryScan
scanNext limits readBlock edges walk =
  case Set.minView (walkPending walk) of
    Nothing -> pure (finishInventory InventoryTraversalComplete walk)
    Just (node, pending) -> scanNode limits readBlock edges node pending walk

scanNode
  :: (Monad m, Ord r)
  => InventoryLimits
  -> (HashRef -> m (Maybe ByteString))
  -> (r -> HashRef -> ByteString -> Either String [(r, HashRef)])
  -> (r, HashRef)
  -> Set (r, HashRef)
  -> InventoryWalk r
  -> m InventoryScan
scanNode limits readBlock edges node@(role, href) pending walk
  | Set.member node (walkExpanded walk) =
      scanNext limits readBlock edges walk { walkPending = pending }
  | firstVisit && blockLimitReached =
      pure (finishInventory (InventoryBlockLimitExceeded href) walk)
  | otherwise = do
      bytes <- readBlock href
      scanBlock limits readBlock edges role href firstVisit bytes walk
        { walkSeen = Set.insert href (walkSeen walk)
        , walkExpanded = Set.insert node (walkExpanded walk)
        , walkPending = pending
        }
  where
    firstVisit = Set.notMember href (walkSeen walk)
    blockLimitReached =
      fromIntegral (Set.size (walkSeen walk)) >= inventoryMaxBlocks limits

scanBlock
  :: (Monad m, Ord r)
  => InventoryLimits
  -> (HashRef -> m (Maybe ByteString))
  -> (r -> HashRef -> ByteString -> Either String [(r, HashRef)])
  -> r
  -> HashRef
  -> Bool
  -> Maybe ByteString
  -> InventoryWalk r
  -> m InventoryScan
scanBlock limits readBlock edges role href firstVisit bytes walk =
  case bytes of
    Nothing -> scanNext limits readBlock edges walk
      { walkMissing = Set.insert href (walkMissing walk) }
    Just value ->
      scanStoredBlock limits readBlock edges role href firstVisit value walk

scanStoredBlock
  :: (Monad m, Ord r)
  => InventoryLimits
  -> (HashRef -> m (Maybe ByteString))
  -> (r -> HashRef -> ByteString -> Either String [(r, HashRef)])
  -> r
  -> HashRef
  -> Bool
  -> ByteString
  -> InventoryWalk r
  -> m InventoryScan
scanStoredBlock limits readBlock edges role href firstVisit value walk
  | hashObject @HbSync value /= fromHashRef href =
      scanNext limits readBlock edges walk
        { walkCorrupt = Set.insert href (walkCorrupt walk) }
  | firstVisit && size > inventoryMaxBytes limits - walkTotalBytes walk =
      pure (finishInventory (InventoryByteLimitExceeded href) walk)
  | otherwise =
      scanEdges limits readBlock edges role href value charged
  where
    size = fromIntegral (LBS.length value)
    charged
      | firstVisit = walk
          { walkBlocks = Map.insert href size (walkBlocks walk)
          , walkTotalBytes = walkTotalBytes walk + size
          }
      | otherwise = walk

scanEdges
  :: (Monad m, Ord r)
  => InventoryLimits
  -> (HashRef -> m (Maybe ByteString))
  -> (r -> HashRef -> ByteString -> Either String [(r, HashRef)])
  -> r
  -> HashRef
  -> ByteString
  -> InventoryWalk r
  -> m InventoryScan
scanEdges limits readBlock edges role href bytes walk =
  case edges role href bytes of
    Left{} -> scanNext limits readBlock edges walk
      { walkMalformed = Set.insert href (walkMalformed walk) }
    Right children -> scanNext limits readBlock edges walk
      { walkPending = Set.union (walkPending walk) (Set.fromList children) }

finishInventory :: InventoryStatus -> InventoryWalk r -> InventoryScan
finishInventory status walk = InventoryScan
  { inventoryStatus = status
  , inventoryBlocks = walkBlocks walk
  , inventoryMissing = walkMissing walk
  , inventoryCorrupt = walkCorrupt walk
  , inventoryMalformed = walkMalformed walk
  , inventoryLogicalBytes = walkTotalBytes walk
  }
