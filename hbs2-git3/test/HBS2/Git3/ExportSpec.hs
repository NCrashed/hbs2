module HBS2.Git3.ExportSpec (spec) where

import HBS2.Git3.Export
  ( indexedObjectsBackedByStorage
  , indexedObjectsIfComplete
  )
import HBS2.Git3.Prelude (GitHash(..), fromStringMay)

import HBS2.Hash (HbSync)
import HBS2.Storage (AnyStorage(..))
import HBS2.Storage.Simple
  ( StoragePrefix(..)
  , simpleStorageInit
  , simpleStorageWorker
  )

import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.HashSet qualified as HS
import Data.IORef
import Data.List (sort)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import UnliftIO (async, cancel)

spec :: Spec
spec = describe "indexedObjectsIfComplete" do
  it "trusts the index only when every transaction is readable" do
    let entries :: [(String, String)]
        entries = [("old", "lost"), ("new", "present"), ("again", "present")]
    seen <- newIORef ([] :: [String])

    result <- indexedObjectsIfComplete
      (\transaction -> modifyIORef' seen (transaction :) >> pure (transaction /= "lost"))
      entries

    result `shouldBe` Left (HS.singleton "lost")
    sort <$> readIORef seen `shouldReturn` ["lost", "present"]

  it "returns all indexed objects when their transactions are readable" do
    result <- indexedObjectsIfComplete (const (pure True))
      ([ ("one", "first"), ("two", "second"), ("one", "first") ]
        :: [(String, String)])

    result `shouldBe` Right (HS.fromList ["one", "two"])

  it "rejects an index whose transaction block is absent from storage" do
    withSystemTempDirectory "hbs2-export-index" $ \dir -> do
      simple <- simpleStorageInit @HbSync [StoragePrefix (dir </> "storage")]
      bracket (async (simpleStorageWorker simple)) cancel $ \_ -> do
        let storage = AnyStorage simple
            object = GitHash (BS.replicate 20 1)
            transaction = fromMaybe (error "invalid test hash")
              (fromStringMay "va4AdZWUi3JnjYeJquSkNFAMbqQdst6DXp4ejpkrJP8")

        indexedObjectsBackedByStorage storage [(object, transaction)]
          `shouldReturn` Left (HS.singleton transaction)
