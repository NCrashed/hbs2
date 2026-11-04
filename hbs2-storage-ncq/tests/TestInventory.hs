module TestInventory
  ( testInventoryListsCurrentEntries
  , testInventorySurvivesReopen
  , testInventoryNewestReferenceWins
  ) where

import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (HbSync, hashObject)
import HBS2.Storage (putBlock)
import HBS2.Storage.NCQ3

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.List qualified as List
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty.HUnit

collectInventory :: NCQStorage -> IO [NCQInventoryEntry]
collectInventory storage = snd <$> collectInventoryWithStatus storage

collectInventoryWithStatus
  :: NCQStorage
  -> IO (NCQInventoryStatus, [NCQInventoryEntry])
collectInventoryWithStatus storage = do
  found <- newIORef []
  status <- ncqInventoryEntries storage $ \entry ->
    modifyIORef' found (entry :)
  inventory <- List.sort <$> readIORef found
  pure (status, inventory)

withNCQ :: FilePath -> (NCQStorage -> IO a) -> IO a
withNCQ = ncqWithStorage

putHash :: NCQStorage -> LBS.ByteString -> IO HashRef
putHash storage bytes =
  putBlock storage bytes >>= maybe (assertFailure "put failed") (pure . HashRef)

testInventoryListsCurrentEntries :: Assertion
testInventoryListsCurrentEntries =
  withSystemTempDirectory "ncq3-inventory" $ \directory ->
    withNCQ directory $ \storage -> do
      kept <- putHash storage "kept"
      removed <- putHash storage "removed"
      let logicalRef = HashRef (hashObject @HbSync (BS8.pack "logical-ref"))
      ncqStorageSetRef storage logicalRef kept
      ncqStorageDelBlock storage removed

      (status, inventory) <- collectInventoryWithStatus storage
      status @?= NCQInventoryStable
      inventory @?=
        List.sort
          [ NCQInventoryBlock kept 4
          , NCQInventoryReference logicalRef kept
          ]

testInventorySurvivesReopen :: Assertion
testInventorySurvivesReopen =
  withSystemTempDirectory "ncq3-inventory-reopen" $ \directory -> do
    expected <- withNCQ directory $ \storage -> do
      first <- putHash storage "first"
      second <- putHash storage "second"
      let logicalRef = HashRef (hashObject @HbSync (BS8.pack "reopen-ref"))
      ncqStorageSetRef storage logicalRef second
      pure $ List.sort
        [ NCQInventoryBlock first 5
        , NCQInventoryBlock second 6
        , NCQInventoryReference logicalRef second
        ]

    withNCQ directory $ \storage -> do
      actual <- collectInventory storage
      actual @?= expected

testInventoryNewestReferenceWins :: Assertion
testInventoryNewestReferenceWins =
  withSystemTempDirectory "ncq3-inventory-shadow" $ \directory -> do
    (logicalRef, second) <- withNCQ directory $ \storage -> do
      first <- putHash storage "first"
      second <- putHash storage "second"
      let logicalRef = HashRef (hashObject @HbSync (BS8.pack "shadowed-ref"))
      ncqStorageSetRef storage logicalRef first
      pure (logicalRef, second)

    withNCQ directory $ \storage ->
      ncqStorageSetRef storage logicalRef second

    withNCQ directory $ \storage -> do
      current <- ncqStorageGetRef storage logicalRef
        >>= maybe (assertFailure "ref disappeared") pure
      inventory <- collectInventory storage
      List.filter isReference inventory @?=
        [NCQInventoryReference logicalRef current]
  where
    isReference NCQInventoryReference{} = True
    isReference _ = False
