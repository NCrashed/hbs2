{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TestGroupKeyLoad
  ( testGroupKeyMissingRoot
  , testGroupKeyMissingLeaf
  , testGroupKeyLegacyEnvelope
  ) where

import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.GroupKeySymm
import HBS2.Storage
import HBS2.Storage.Operations.Class
import HBS2.Storage.Operations.GroupKey

import Control.Monad.IO.Class
import Crypto.Saltine.Core.Box qualified as Box
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Functor ((<&>))
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Prettyprinter (pretty)
import Test.Tasty.HUnit

data MemStorage = MemStorage
  { memBlocks :: IORef (Map (Hash HbSync) LBS.ByteString)
  , memRefs :: IORef (Map (Hash HbSync) (Hash HbSync))
  }

instance MonadIO m => Storage MemStorage HbSync LBS.ByteString m where
  putBlock storage bytes = liftIO do
    let hash = hashObject @HbSync bytes
    modifyIORef' (memBlocks storage) (Map.insert hash bytes)
    pure (Just hash)
  enqueueBlock = putBlock
  getBlock storage hash = liftIO $ Map.lookup hash <$> readIORef (memBlocks storage)
  delBlock storage hash = liftIO $ modifyIORef' (memBlocks storage) (Map.delete hash)
  getChunk storage hash (Offset offset) (Size size) =
    getBlock storage hash <&> fmap (LBS.take (fromIntegral size) . LBS.drop (fromIntegral offset))
  hasBlock storage hash = getBlock storage hash <&> fmap (fromIntegral . LBS.length)
  updateRef storage key value = liftIO $
    modifyIORef' (memRefs storage) (Map.insert (hashObject key) value)
  getRef storage key = liftIO $ Map.lookup (hashObject key) <$> readIORef (memRefs storage)
  delRef storage key = liftIO $ modifyIORef' (memRefs storage) (Map.delete (hashObject key))

newStorage :: IO (MemStorage, AnyStorage)
newStorage = do
  storage <- MemStorage <$> newIORef mempty <*> newIORef mempty
  pure (storage, AnyStorage storage)

testGroupKeyMissingRoot :: Assertion
testGroupKeyMissingRoot = do
  (_, storage) <- newStorage
  let root = HashRef $ hashObject @HbSync ("absent-group-key" :: LBS.ByteString)
  result <- loadGroupKeyChecked @'HBS2Basic storage root
  problem <- expectLoadError result
  problem @?= GroupKeyObjectIncomplete root [root]
  show problem @?=
    "group-key object " <> show (pretty root)
      <> " is incomplete; missing physical block(s): " <> show (pretty root)
      <> "; run `hbs2-peer fetch --timeout 60 " <> show (pretty root) <> "`"

testGroupKeyMissingLeaf :: Assertion
testGroupKeyMissingLeaf = do
  (memory, storage) <- newStorage
  rootHash <- writeAsMerkle storage (LBS.replicate (2 * 1024 * 1024) 7)
  let root = HashRef rootHash
  physical <- Map.keys <$> readIORef (memBlocks memory)
  let missing = fromJust $ find (/= rootHash) physical
  delBlock storage missing
  result <- loadGroupKeyChecked @'HBS2Basic storage root
  expectLoadError result >>= (@?= GroupKeyObjectIncomplete root [HashRef missing])

testGroupKeyLegacyEnvelope :: Assertion
testGroupKeyLegacyEnvelope = do
  (_, storage) <- newStorage
  Box.Keypair _ publicKey <- Box.newKeypair
  modern <- generateGroupKey @'HBS2Basic Nothing [publicKey]
  root <- HashRef <$> writeAsMerkle storage
    (serialise $ GroupKeySymmV1 @'HBS2Basic (recipients modern))
  result <- loadGroupKeyChecked @'HBS2Basic storage root
  case result of
    Right GroupKeySymmPlain{} -> pure ()
    _ -> assertFailure "expected compatible legacy envelope"

expectLoadError :: Either GroupKeyLoadError a -> IO GroupKeyLoadError
expectLoadError = either pure (const $ assertFailure "expected a load error" >> error "unreachable")
