module HBS2.Peer.StorageInventory
  ( StorageRetentionRoot(..)
  , StoragePin(..)
  , StorageInventoryRoot(..)
  , StorageInventoryReport(..)
  , StorageHashDisposition(..)
  , StorageCleanupResult(..)
  , makeStorageInventoryReport
  , storageHashDisposition
  , validateStorageCleanupSnapshot
  , renderStorageInventory
  , renderStorageHashDisposition
  , renderStorageCleanup
  ) where

import HBS2.Data.Types.Refs
import HBS2.Storage.Inventory

import Codec.Serialise (Serialise)
import Data.Aeson (ToJSON(..), Value, object, (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word
import GHC.Generics (Generic)

-- | An application-owned reason to retain one exact content snapshot.
-- Applications choose their own stable id and kind; storage accounting only
-- consumes the exact reachable block set they register.
data StorageRetentionRoot = StorageRetentionRoot
  { storageRetentionId      :: Text
  , storageRetentionKind    :: Text
  , storageRetentionHash    :: HashRef
  , storageRetentionReasons :: Set Text
  , storageRetentionRefetch :: Bool
  , storageRetentionBlocks  :: Map HashRef Word64
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Serialise StorageRetentionRoot

data StoragePin = StoragePin
  { storagePinName :: Text
  , storagePinKind :: Text
  , storagePinRoot :: HashRef
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Serialise StoragePin

instance ToJSON StoragePin where
  toJSON StoragePin{..} = object
    [ "name" .= storagePinName
    , "kind" .= storagePinKind
    , "root" .= hashText storagePinRoot
    ]

data StorageInventoryRoot = StorageInventoryRoot
  { storageInventoryRootId           :: Text
  , storageInventoryRootKind         :: Text
  , storageInventoryRootHash         :: HashRef
  , storageInventoryRootReasons      :: Set Text
  , storageInventoryRootRefetch      :: Bool
  , storageInventoryRootLogicalBytes :: Word64
  , storageInventoryRootUniqueBytes  :: Word64
  , storageInventoryRootSharedBytes  :: Word64
  , storageInventoryRootMissingBytes :: Word64
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Serialise StorageInventoryRoot

data StorageInventoryReport = StorageInventoryReport
  { storageInventoryStable            :: Bool
  , storageInventoryCoverageProblems  :: Set Text
  , storageInventoryRoots             :: [StorageInventoryRoot]
  , storageInventoryRootBlocks        :: Map Text (Map HashRef Word64)
  , storageInventoryPhysical          :: Map HashRef Word64
  , storageInventoryRetained          :: Map HashRef Word64
  , storageInventoryUnattributed      :: Map HashRef Word64
  , storageInventoryReclaimable       :: Map HashRef Word64
  , storageInventoryPhysicalBytes     :: Word64
  , storageInventoryRetainedBytes     :: Word64
  }
  deriving stock (Eq, Show, Generic)

instance Serialise StorageInventoryReport

data StorageHashDisposition
  = StorageHashMissing
  | StorageHashRetained Word64 [Text]
  | StorageHashReclaimable Word64
  | StorageHashUnattributed Word64
  deriving stock (Eq, Ord, Show, Generic)

instance Serialise StorageHashDisposition

instance ToJSON StorageHashDisposition where
  toJSON = \case
    StorageHashMissing -> object
      [ "disposition" .= ("missing" :: Text)
      ]
    StorageHashRetained size roots -> object
      [ "disposition" .= ("retained" :: Text)
      , "bytes" .= size
      , "roots" .= roots
      ]
    StorageHashReclaimable size -> object
      [ "disposition" .= ("reclaimable" :: Text)
      , "bytes" .= size
      ]
    StorageHashUnattributed size -> object
      [ "disposition" .= ("unattributed" :: Text)
      , "bytes" .= size
      ]

data StorageCleanupResult = StorageCleanupResult
  { storageCleanupExecuted     :: Bool
  , storageCleanupReport       :: StorageInventoryReport
  , storageCleanupDeleted      :: Map HashRef Word64
  }
  deriving stock (Eq, Show, Generic)

instance Serialise StorageCleanupResult

instance ToJSON StorageCleanupResult where
  toJSON StorageCleanupResult{..} = object
    [ "executed" .= storageCleanupExecuted
    , "report" .= storageCleanupReport
    , "deletedBlocks" .= Map.size storageCleanupDeleted
    , "deletedBytes" .= sum (Map.elems storageCleanupDeleted)
    , "deleted" .= hashEntries storageCleanupDeleted
    ]

-- | Produce the single snapshot consumed by both inventory output and
-- cleanup. Incomplete root coverage and concurrent physical writes are
-- fail-closed: bytes remain visible as unattributed but never reclaimable.
makeStorageInventoryReport
  :: Bool
  -> Set Text
  -> Map HashRef Word64
  -> [StorageRetentionRoot]
  -> Either InventoryAccountingError StorageInventoryReport
makeStorageInventoryReport stable suppliedProblems physical suppliedRoots = do
  accounting <- accountInventory coverage physical accountingRoots
  pure StorageInventoryReport
    { storageInventoryStable = stable
    , storageInventoryCoverageProblems = problems
    , storageInventoryRoots = fmap renderRoot (inventoryAccountingRoots accounting)
    , storageInventoryRootBlocks = Map.map storageRetentionBlocks roots
    , storageInventoryPhysical = physical
    , storageInventoryRetained = inventoryAccountingRetained accounting
    , storageInventoryUnattributed = inventoryAccountingUnattributed accounting
    , storageInventoryReclaimable = inventoryAccountingReclaimable accounting
    , storageInventoryPhysicalBytes = inventoryAccountingPhysicalBytes accounting
    , storageInventoryRetainedBytes = inventoryAccountingRetainedBytes accounting
    }
  where
    roots = Map.fromListWith mergeRoot
      [(storageRetentionId root, root) | root <- suppliedRoots]
    problems = suppliedProblems <>
      Set.fromList ["storage changed during inventory" | not stable]
    coverage
      | Set.null problems = InventoryRootsComplete
      | otherwise = InventoryRootsIncomplete problems
    accountingRoots =
      [ InventoryRoot rootId (storageRetentionBlocks root)
      | (rootId, root) <- Map.toAscList roots
      ]
    renderRoot InventoryRootAccounting{..} =
      let root = roots Map.! inventoryRootAccountingKey
      in StorageInventoryRoot
          { storageInventoryRootId = storageRetentionId root
          , storageInventoryRootKind = storageRetentionKind root
          , storageInventoryRootHash = storageRetentionHash root
          , storageInventoryRootReasons = storageRetentionReasons root
          , storageInventoryRootRefetch = storageRetentionRefetch root
          , storageInventoryRootLogicalBytes = inventoryRootAccountingLogicalBytes
          , storageInventoryRootUniqueBytes = inventoryRootAccountingUniqueBytes
          , storageInventoryRootSharedBytes = inventoryRootAccountingSharedBytes
          , storageInventoryRootMissingBytes = inventoryRootAccountingMissingBytes
          }

mergeRoot :: StorageRetentionRoot -> StorageRetentionRoot -> StorageRetentionRoot
mergeRoot newer older = newer
  { storageRetentionReasons =
      storageRetentionReasons newer <> storageRetentionReasons older
  , storageRetentionRefetch =
      storageRetentionRefetch newer || storageRetentionRefetch older
  , storageRetentionBlocks =
      Map.union (storageRetentionBlocks newer) (storageRetentionBlocks older)
  }

storageHashDisposition :: StorageInventoryReport -> HashRef -> StorageHashDisposition
storageHashDisposition report hash =
  case Map.lookup hash (storageInventoryPhysical report) of
    Nothing -> StorageHashMissing
    Just size -> case retainingRoots of
      roots@(_ : _) -> StorageHashRetained size roots
      [] | Map.member hash (storageInventoryReclaimable report) ->
             StorageHashReclaimable size
         | otherwise -> StorageHashUnattributed size
  where
    retainingRoots =
      [ rootId
      | (rootId, blocks) <- Map.toAscList (storageInventoryRootBlocks report)
      , Map.member hash blocks
      ]

-- | Cleanup consumes the same model inventory displays.  An execution plan
-- is valid only when an immediate second snapshot is complete, stable, and
-- byte-for-byte identical at every retention and physical boundary.
validateStorageCleanupSnapshot
  :: StorageInventoryReport
  -> StorageInventoryReport
  -> Either Text (Map HashRef Word64)
validateStorageCleanupSnapshot planned current
  | not (storageInventoryStable planned) = Left "planned inventory is unstable"
  | not (storageInventoryStable current) = Left "storage changed during cleanup validation"
  | not (Set.null (storageInventoryCoverageProblems planned)) =
      Left "planned inventory has incomplete retention-root coverage"
  | not (Set.null (storageInventoryCoverageProblems current)) =
      Left "current inventory has incomplete retention-root coverage"
  | storageInventoryPhysical planned /= storageInventoryPhysical current =
      Left "physical storage changed during cleanup validation"
  | storageInventoryRootBlocks planned /= storageInventoryRootBlocks current =
      Left "retention roots changed during cleanup validation"
  | storageInventoryReclaimable planned /= storageInventoryReclaimable current =
      Left "reclaimable set changed during cleanup validation"
  | otherwise = Right (storageInventoryReclaimable current)

renderStorageInventory :: StorageInventoryReport -> Text
renderStorageInventory StorageInventoryReport{..} = Text.unlines $
  [ "storage inventory"
  , "  stable: " <> yesNo storageInventoryStable
  , "  complete: " <> yesNo (Set.null storageInventoryCoverageProblems)
  , summary "physical" storageInventoryPhysical
  , summary "retained" storageInventoryRetained
  , summary "unattributed" storageInventoryUnattributed
  , summary "reclaimable" storageInventoryReclaimable
  ]
  <> problemLines
  <> concatMap rootLines storageInventoryRoots
  where
    summary label blocks = "  " <> label <> ": "
      <> Text.pack (show (Map.size blocks)) <> " blocks, "
      <> Text.pack (show (sum (Map.elems blocks))) <> " bytes"
    problemLines
      | Set.null storageInventoryCoverageProblems = []
      | otherwise = "coverage problems:" :
          [ "  - " <> problem
          | problem <- Set.toAscList storageInventoryCoverageProblems
          ]
    rootLines StorageInventoryRoot{..} =
      [ "root " <> storageInventoryRootId <> " (" <> storageInventoryRootKind <> ")"
      , "  hash: " <> hashText storageInventoryRootHash
      , "  logical: " <> number storageInventoryRootLogicalBytes <> " bytes"
      , "  unique: " <> number storageInventoryRootUniqueBytes <> " bytes"
      , "  shared: " <> number storageInventoryRootSharedBytes <> " bytes"
      , "  missing: " <> number storageInventoryRootMissingBytes <> " bytes"
      , "  reasons: " <> Text.intercalate ", "
          (Set.toAscList storageInventoryRootReasons)
      , "  refetchable: " <> yesNo storageInventoryRootRefetch
      ]
    number = Text.pack . show

renderStorageHashDisposition :: HashRef -> StorageHashDisposition -> Text
renderStorageHashDisposition hash disposition = Text.unlines $ case disposition of
  StorageHashMissing ->
    [ hashText hash <> ": not present in physical storage" ]
  StorageHashRetained size roots ->
    [ hashText hash <> ": retained (" <> number size <> " bytes)"
    , "  roots: " <> Text.intercalate ", " roots
    ]
  StorageHashReclaimable size ->
    [ hashText hash <> ": reclaimable (" <> number size <> " bytes)" ]
  StorageHashUnattributed size ->
    [ hashText hash <> ": unattributed and protected (" <> number size <> " bytes)" ]
  where
    number = Text.pack . show

renderStorageCleanup :: StorageCleanupResult -> Text
renderStorageCleanup StorageCleanupResult{..} =
  renderStorageInventory storageCleanupReport <> Text.unlines
    [ if storageCleanupExecuted then "cleanup executed" else "cleanup dry run"
    , "  selected: " <> number selectedCount <> " blocks, "
        <> number selectedBytes <> " bytes"
    , "  deleted: " <> number (Map.size storageCleanupDeleted) <> " blocks, "
        <> number (sum (Map.elems storageCleanupDeleted)) <> " bytes"
    , "  note: cleanup writes logical tombstones; NCQ compaction reclaims disk space separately"
    ]
  where
    selected = storageInventoryReclaimable storageCleanupReport
    selectedCount = Map.size selected
    selectedBytes = sum (Map.elems selected)
    number :: Show a => a -> Text
    number = Text.pack . show

yesNo :: Bool -> Text
yesNo True = "yes"
yesNo False = "no"

instance ToJSON StorageInventoryRoot where
  toJSON StorageInventoryRoot{..} = object
    [ "id" .= storageInventoryRootId
    , "kind" .= storageInventoryRootKind
    , "hash" .= hashText storageInventoryRootHash
    , "reasons" .= Set.toAscList storageInventoryRootReasons
    , "refetch" .= storageInventoryRootRefetch
    , "logicalBytes" .= storageInventoryRootLogicalBytes
    , "uniqueBytes" .= storageInventoryRootUniqueBytes
    , "sharedBytes" .= storageInventoryRootSharedBytes
    , "missingBytes" .= storageInventoryRootMissingBytes
    ]

instance ToJSON StorageInventoryReport where
  toJSON StorageInventoryReport{..} = object
    [ "stable" .= storageInventoryStable
    , "complete" .= Set.null storageInventoryCoverageProblems
    , "coverageProblems" .= Set.toAscList storageInventoryCoverageProblems
    , "physicalBytes" .= storageInventoryPhysicalBytes
    , "retainedBytes" .= storageInventoryRetainedBytes
    , "unattributedBytes" .= sum (Map.elems storageInventoryUnattributed)
    , "reclaimableBytes" .= sum (Map.elems storageInventoryReclaimable)
    , "physicalBlocks" .= Map.size storageInventoryPhysical
    , "retainedBlocks" .= Map.size storageInventoryRetained
    , "unattributedBlocks" .= Map.size storageInventoryUnattributed
    , "reclaimableBlocks" .= Map.size storageInventoryReclaimable
    , "roots" .= storageInventoryRoots
    , "unattributed" .= hashEntries storageInventoryUnattributed
    , "reclaimable" .= hashEntries storageInventoryReclaimable
    ]

hashEntries :: Map HashRef Word64 -> [Value]
hashEntries = fmap (\(hash, size) -> object
  ["hash" .= hashText hash, "bytes" .= size]) . Map.toAscList

hashText :: HashRef -> Text
hashText = Text.pack . show
