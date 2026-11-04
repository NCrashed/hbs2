module HBS2.Peer.StorageInventory.Collector
  ( collectStorageInventory
  , cleanupStorageInventory
  ) where

import HBS2.Base58
import HBS2.Data.Types.Refs
import HBS2.Data.Detect (extractBlockRefs)
import HBS2.Hash
import HBS2.Net.Auth.Schema
import HBS2.Peer.Brains
import HBS2.Peer.Proto.LWWRef
import HBS2.Peer.Proto.RefLog
import HBS2.Peer.Proto.RepositoryLease
import HBS2.Peer.RepositoryLease
import HBS2.Peer.StorageInventory
import HBS2.Storage
import HBS2.Storage.Inventory
import HBS2.Storage.NCQ3

import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad (forM, forM_)
import Data.Bifunctor (first)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word
import Prettyprinter (pretty)

-- | Collect the daemon's current physical NCQ view and the retention sources
-- already owned by peer protocols. Unsupported poll/ref kinds remain visible
-- as coverage problems, which deliberately disables cleanup without hiding
-- their bytes.
collectStorageInventory
  :: NCQStorage
  -> AnyStorage
  -> SomeBrains L4Proto
  -> IO (Either Text StorageInventoryReport)
collectStorageInventory ncq storage brains = do
  physicalRef <- newIORef Map.empty
  refsRef <- newIORef Map.empty
  status <- ncqInventoryEntries ncq $ \case
    NCQInventoryBlock hash size ->
      modifyIORef' physicalRef (Map.insert hash size)
    NCQInventoryReference key value ->
      modifyIORef' refsRef (Map.insert key value)
  physical <- readIORef physicalRef
  storageRefs <- readIORef refsRef
  polls <- listPolledRefs @L4Proto brains Nothing
  leases <- listRepositoryLeases @L4Proto brains
  pins <- listStoragePins @L4Proto brains
  now <- floor <$> getPOSIXTime

  let activeLeases = filter (leaseRetainsAt now) leases
      pollProblems = Set.fromList
        [ "unsupported poll type: " <> Text.pack pollType
        | (_, pollType, _) <- polls
        , pollType /= "lwwref"
        ]
      repositories = Map.fromListWith (<>) $
        [ (repository, Set.singleton "poll:lwwref")
        | (repository, "lwwref", _) <- polls
        ] <>
        [ ( repositoryLeaseRepository lease
          , Set.singleton ("lease:" <> Text.pack (show (repositoryLeaseId lease)))
          )
        | lease <- activeLeases
        ]
      maxBlocks = fromIntegral (Map.size physical) + 1
      maxBytes = sum (Map.elems physical)
      limits = InventoryLimits maxBlocks maxBytes

  discovered <- forM (Map.toAscList repositories) $ \(repository, reasons) -> do
    snapshot <- discoverRepositorySnapshot limits storage repository
    pure (repository, reasons, snapshot)

  scannedPins <- forM pins $ \pin -> do
    scan <- scanInventory limits
      (getBlock storage . fromHashRef)
      (extractBlockRefs . fromHashRef)
      [storagePinRoot pin]
    pure (pin, scan)

  scannedLeaseArtifacts <- forM activeLeases $ \lease -> do
    let leaseId = repositoryLeaseId lease
        pinKey = logicalRef (RepositoryLeasePin leaseId)
        receiptKey = logicalRef (RepositoryLeaseReceipt leaseId)
        snapshotRoots = case repositoryLeaseSnapshot lease of
          Nothing -> []
          Just snapshot -> [repositorySnapshotInventoryRoot snapshot]
        artifactRoots = snapshotRoots
          <> maybe [] pure (Map.lookup pinKey storageRefs)
          <> maybe [] pure (Map.lookup receiptKey storageRefs)
    scan <- scanInventory limits
      (getBlock storage . fromHashRef)
      (extractBlockRefs . fromHashRef)
      artifactRoots
    pure (lease, scan)

  let roots = mapMaybe discoveredRoot discovered
        <> mapMaybe pinRoot scannedPins
        <> mapMaybe leaseArtifactRoot scannedLeaseArtifacts
      discoveryProblems = Set.fromList
        [ "repository inventory failed: " <> keyText repository
            <> ": " <> Text.pack (show problem)
        | (repository, _, Left problem) <- discovered
        ]
      pinProblems = Set.fromList
        [ "explicit pin inventory failed: " <> storagePinName pin
            <> ": " <> Text.pack (show (inventoryStatus scan))
        | (pin, scan) <- scannedPins
        , inventoryStatus scan /= InventoryTraversalComplete
            || not (Set.null (inventoryMissing scan))
            || not (Set.null (inventoryCorrupt scan))
            || not (Set.null (inventoryMalformed scan))
        ]
      leaseArtifactProblems = Set.fromList
        [ "lease artifact inventory failed: "
            <> Text.pack (show (repositoryLeaseId lease))
            <> ": " <> Text.pack (show (inventoryStatus scan))
        | (lease, scan) <- scannedLeaseArtifacts
        , not (inventoryComplete scan)
        ]
      successful =
        [ (repository, candidate)
        | (repository, _, Right candidate) <- discovered
        ]
      expectedRefs = Set.fromList $
        [ logicalRef (LWWRefKey @'HBS2Basic repository)
        | (repository, _) <- successful
        ] <>
        [ logicalRef (RefLogKey @'HBS2Basic (candidateRefLogKey candidate))
        | (_, candidate) <- successful
        ] <>
        concat
          [ [ logicalRef (RepositoryLeasePin (repositoryLeaseId lease))
            , logicalRef (RepositoryLeaseReceipt (repositoryLeaseId lease))
            ]
          | lease <- activeLeases
          ]
      unknownRefs = Map.keysSet storageRefs `Set.difference` expectedRefs
      refProblems = Set.map
        ("unregistered storage ref: " <>) (Set.map hashText unknownRefs)
      problems = pollProblems <> discoveryProblems <> pinProblems
        <> leaseArtifactProblems <> refProblems
      stable = status == NCQInventoryStable

  pure $ first (Text.pack . show) $
    makeStorageInventoryReport stable problems physical roots
  where
    discoveredRoot (repository, reasons, Right candidate) = Just StorageRetentionRoot
      { storageRetentionId = "repository:" <> keyText repository
      , storageRetentionKind = "repository"
      , storageRetentionHash = candidateSnapshotId candidate
      , storageRetentionReasons = reasons
      , storageRetentionRefetch = any ("poll:" `Text.isPrefixOf`) reasons
      , storageRetentionBlocks = candidateInventory candidate
      }
    discoveredRoot _ = Nothing

    pinRoot (pin, scan)
      | inventoryComplete scan = Just StorageRetentionRoot
          { storageRetentionId = "pin:" <> storagePinName pin
          , storageRetentionKind = storagePinKind pin
          , storageRetentionHash = storagePinRoot pin
          , storageRetentionReasons = Set.singleton
              ("explicit-pin:" <> storagePinName pin)
          , storageRetentionRefetch = False
          , storageRetentionBlocks = inventoryBlocks scan
          }
      | otherwise = Nothing

    leaseArtifactRoot (lease, scan)
      | inventoryComplete scan = Just StorageRetentionRoot
          { storageRetentionId = "lease-artifacts:"
              <> Text.pack (show (repositoryLeaseId lease))
          , storageRetentionKind = "repository-lease"
          , storageRetentionHash = maybe
              (logicalRef (RepositoryLeasePin (repositoryLeaseId lease)))
              repositorySnapshotId
              (repositoryLeaseSnapshot lease)
          , storageRetentionReasons = Set.singleton
              ("active-lease:" <> Text.pack (show (repositoryLeaseId lease)))
          , storageRetentionRefetch = False
          , storageRetentionBlocks = inventoryBlocks scan
          }
      | otherwise = Nothing

    keyText = Text.pack . show . pretty . AsBase58
    logicalRef :: Hashed HbSync ref => ref -> HashRef
    logicalRef = HashRef . hashObject

hashText :: HashRef -> Text
hashText = Text.pack . show

leaseRetainsAt :: Word64 -> RepositoryLease -> Bool
leaseRetainsAt now lease =
     repositoryLeaseState lease /= RepositoryLeaseTerminated
  && repositoryLeaseLifetimeAt now lease /= RepositoryLeaseElapsed

inventoryComplete :: InventoryScan -> Bool
inventoryComplete scan =
     inventoryStatus scan == InventoryTraversalComplete
  && Set.null (inventoryMissing scan)
  && Set.null (inventoryCorrupt scan)
  && Set.null (inventoryMalformed scan)

-- | Dry-run returns one inventory snapshot.  Execution obtains a second
-- snapshot under a daemon-local cleanup lock and refuses to delete unless
-- every physical and retention boundary is unchanged.
cleanupStorageInventory
  :: MVar ()
  -> NCQStorage
  -> AnyStorage
  -> SomeBrains L4Proto
  -> Bool
  -> IO (Either Text StorageCleanupResult)
cleanupStorageInventory lock ncq storage brains execute = withMVar lock $ \_ -> do
  planned <- collectStorageInventory ncq storage brains
  case planned of
    Left problem -> pure (Left problem)
    Right report
      | not execute -> pure $ Right StorageCleanupResult
          { storageCleanupExecuted = False
          , storageCleanupReport = report
          , storageCleanupDeleted = Map.empty
          }
      | otherwise -> do
          current <- collectStorageInventory ncq storage brains
          case current of
            Left problem -> pure (Left problem)
            Right currentReport -> case
                validateStorageCleanupSnapshot report currentReport of
              Left problem -> pure (Left problem)
              Right candidates -> do
                forM_ (Map.keys candidates) $ \hash ->
                  delBlock storage (fromHashRef hash)
                pure $ Right StorageCleanupResult
                  { storageCleanupExecuted = True
                  , storageCleanupReport = currentReport
                  , storageCleanupDeleted = candidates
                  }
