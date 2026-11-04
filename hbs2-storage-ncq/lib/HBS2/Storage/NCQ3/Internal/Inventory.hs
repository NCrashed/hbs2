module HBS2.Storage.NCQ3.Internal.Inventory
  ( NCQInventoryEntry(..)
  , NCQInventoryStatus(..)
  , ncqInventoryEntries
  ) where

import HBS2.Storage.NCQ3.Internal.Fossil
import HBS2.Storage.NCQ3.Internal.Index
import HBS2.Storage.NCQ3.Internal.MMapCache
import HBS2.Storage.NCQ3.Internal.Memtable (ncqOperation)
import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.State
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal (ncqGetEntryBS)

import Data.ByteString qualified as BS
import Control.Concurrent.STM qualified as STM
import Data.Bits (shiftR)
import Data.Foldable (traverse_)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Vector qualified as Vector

-- | The logical live entries in an NCQ store. Physical tombstones and
-- internal metadata do not appear here.
data NCQInventoryEntry
  = NCQInventoryBlock HashRef Word64
  | NCQInventoryReference HashRef HashRef
  deriving stock (Eq, Ord, Show, Generic)

-- | A scan is useful for observability even when a writer was active, but a
-- cleanup plan must only be made from a stable scan.
data NCQInventoryStatus
  = NCQInventoryStable
  | NCQInventoryConcurrentWrites
  deriving stock (Eq, Ord, Show, Generic)

-- | Stream the current logical NCQ entries without retaining the whole store
-- in memory. One high-nibble hash partition is deduplicated at a time; newer
-- in-memory entries and newer indexes shadow older index records, including
-- tombstones.
ncqInventoryEntries
  :: MonadUnliftIO m
  => NCQStorage
  -> (NCQInventoryEntry -> m ())
  -> m NCQInventoryStatus
ncqInventoryEntries storage emit = do
  ncqOperation storage (pure NCQInventoryConcurrentWrites) do
    waitInventoryReady storage
    writesBefore <- readTVarIO (ncqWrites storage)
    ncqWithState storage $ \stateKey -> do
      state <- inventoryState storage stateKey
      for_ [0 .. 15 :: Word8] $ \prefix ->
        inventoryPartition storage state prefix emit
    writesAfter <- readTVarIO (ncqWrites storage)
    pure $ if writesBefore == writesAfter
      then NCQInventoryStable
    else NCQInventoryConcurrentWrites

-- 'ncqAlive' is raised before recovery loads the state. The first durable
-- state key is published only after recovery, index adoption and worker
-- startup have completed, so it is the read-side startup barrier.
waitInventoryReady :: MonadIO m => NCQStorage -> m ()
waitInventoryReady NCQStorage{..} = liftIO $ atomically do
  stateKey <- readTVar ncqStateKey
  when (stateKey == ncqNullStateKey) STM.retry

inventoryState :: MonadIO m => NCQStorage -> FileKey -> m NCQState
inventoryState NCQStorage{..} stateKey = liftIO $ atomically do
  uses <- readTVar ncqStateUse
  case HashMap.lookup stateKey uses of
    Just (state, _) -> pure state
    Nothing -> readTVar ncqState

inventoryPartition
  :: MonadUnliftIO m
  => NCQStorage
  -> NCQState
  -> Word8
  -> (NCQInventoryEntry -> m ())
  -> m ()
inventoryPartition storage state prefix emit = do
  memory <- inventoryMemoryPartition storage prefix
  seen <- newIORef (HashSet.fromList (fmap fst memory))
  traverse_ (emitLocation storage emit . snd) memory

  for_ (ncqStateIndex state) $ \(_, fileKey) -> do
    CachedIndex bytes index <- ncqGetCachedIndex storage fileKey
    nwayHashScanAll index bytes $ \_ rawKey rawEntry -> do
      let key = coerce rawKey :: HashRef
      when (rawKey /= emptyKey && inPartition prefix key) do
        fresh <- liftIO $ atomicModifyIORef' seen $ \known ->
          if HashSet.member key known
            then (known, False)
            else (HashSet.insert key known, True)
        when fresh do
          let IndexEntry dataFile offset size = unpackIndexEntry rawEntry
          emitLocation storage emit
            (InFossil (FileLocation dataFile offset size))

inventoryMemoryPartition
  :: MonadIO m
  => NCQStorage
  -> Word8
  -> m [(HashRef, Location)]
inventoryMemoryPartition NCQStorage{..} prefix = liftIO $ atomically do
  shards <- traverse readTVar (Vector.toList ncqMemTable)
  fmap concat $ for (concatMap HashMap.toList shards) $ \(key, NCQEntry value) ->
    if inPartition prefix key
      then do
        location <- readTVar value <&> \case
          EntryHere bytes -> InMemory bytes
          EntryThere fossil -> InFossil fossil
        pure [(key, location)]
      else pure []

emitLocation
  :: MonadUnliftIO m
  => NCQStorage
  -> (NCQInventoryEntry -> m ())
  -> Location
  -> m ()
emitLocation storage emit location =
  ncqGetEntryBS storage location >>= traverse_ (traverse_ emit . decodeEntry)

decodeEntry :: ByteString -> Maybe NCQInventoryEntry
decodeEntry bytes = case ncqEntryUnwrap bytes of
  (rawKey, Right (B, payload)) ->
    Just (NCQInventoryBlock (coerce rawKey) (fromIntegral (BS.length payload)))
  (_, Right (R, payload))
    | BS.length payload == 2 * ncqKeyLen ->
        let value = coerce (BS.take ncqKeyLen payload)
            logicalKey = coerce (BS.drop ncqKeyLen payload)
        in Just (NCQInventoryReference logicalKey value)
  _ -> Nothing

inPartition :: Word8 -> HashRef -> Bool
inPartition prefix key = BS.head (coerce key) `shiftR` 4 == prefix
