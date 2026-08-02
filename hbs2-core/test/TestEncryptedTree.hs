{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | What separates two encryptions under one group key.
--
-- An encrypted tree derives its content key from the group secret and a nonce
-- that is a hash of the payload's first megabyte. That prefix rule is
-- deliberate: it is what lets an appended-to file keep its earlier blocks byte
-- for byte, so a second version costs only the blocks that changed.
--
-- It also means two payloads sharing that megabyte derive the same key. When
-- each block's nonce was its position in the tree, that was the whole of the
-- separation between them, so everything past the point where such payloads
-- diverged went out under one keystream, and one Poly1305 one-time key. Two
-- versions of a file over a megabyte long, under a group key that lives as long
-- as the repo or the synced directory, is the ordinary case, not a corner.
--
-- These cases pin the fix and its price: the nonce now comes from the block, so
-- a block that changed cannot collide with what it replaced, and a block that
-- did not still encrypts to the same bytes and deduplicates.
module TestEncryptedTree
  ( testEncryptedTreeRoundTrip
  , testEncryptedTreeSharedPrefixNoNonceReuse
  , testEncryptedTreeAppendStillDedups
  , testEncryptedTreeReadsLegacyMethod1
  ) where

import HBS2.Defaults
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Merkle
import HBS2.Merkle.MetaData
import HBS2.Net.Auth.Credentials ()
import HBS2.Net.Auth.GroupKeySymm
import HBS2.Net.Auth.Schema ()
import HBS2.Storage
import HBS2.Storage.Operations.Class

import Codec.Serialise
import Control.Monad (forM, forM_)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (runWriterT, tell)
import Crypto.KDF.HKDF qualified as HKDF
import Crypto.Saltine.Class qualified as Saltine
import Crypto.Saltine.Core.Box qualified as AK
import Crypto.Saltine.Core.SecretBox qualified as SK
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Word (Word64)

import Prettyprinter (pretty)
import Streaming.Prelude qualified as S

import Test.Tasty.HUnit

-- A store that keeps blocks in a map, so these cases say nothing about the
-- filesystem. Blocks are content addressed, which is what the deduplication
-- case reads: writing the same bytes twice leaves one entry.
data MemStorage =
  MemStorage
  { memBlocks :: IORef (Map (Hash HbSync) LBS.ByteString)
  , memRefs   :: IORef (Map (Hash HbSync) (Hash HbSync))
  }

instance MonadIO m => Storage MemStorage HbSync LBS.ByteString m where
  putBlock s bs = liftIO do
    let h = hashObject @HbSync bs
    modifyIORef' (memBlocks s) (Map.insert h bs)
    pure (Just h)

  enqueueBlock = putBlock

  getBlock s h = liftIO $ readIORef (memBlocks s) <&> Map.lookup h

  delBlock s h = liftIO $ modifyIORef' (memBlocks s) (Map.delete h)

  getChunk s h (Offset o) (Size n) =
    getBlock s h <&> fmap (LBS.take (fromIntegral n) . LBS.drop (fromIntegral o))

  hasBlock s h = getBlock s h <&> fmap (fromIntegral . LBS.length)

  updateRef s k v = liftIO $ modifyIORef' (memRefs s) (Map.insert (hashObject k) v)

  getRef s k = liftIO $ readIORef (memRefs s) <&> Map.lookup (hashObject k)

  delRef s k = liftIO $ modifyIORef' (memRefs s) (Map.delete (hashObject k))

newMemStorage :: IO AnyStorage
newMemStorage = AnyStorage <$> (MemStorage <$> newIORef mempty <*> newIORef mempty)

-- A group of one, which is all these cases need: what is under test is the
-- content key, and every member derives the same one.
newGroup :: IO (GroupSecret, GroupKey 'Symm 'HBS2Basic)
newGroup = do
  AK.Keypair sk pk <- AK.newKeypair
  gk <- generateGroupKey @'HBS2Basic Nothing [pk]
  let gks = lookupGroupKey @'HBS2Basic sk pk gk & fromJust
  pure (gks, gk)

-- Filler that does not repeat, so a block that ought to differ from its
-- neighbour actually does, and a case that compares blocks is comparing
-- something. Deterministic, so a failure reproduces.
filler :: ByteString -> Int -> LBS.ByteString
filler seed n = LBS.take (fromIntegral n) $ LBS.fromChunks (go seed)
  where
    go s = let HbSyncHash h = hashObject @HbSync s in h : go h

writeTree :: AnyStorage
          -> GroupSecret
          -> GroupKey 'Symm 'HBS2Basic
          -> Text
          -> LBS.ByteString
          -> IO HashRef
writeTree sto gks gk meta lbs =
  createEncryptedTree @'HBS2Basic sto gks gk (DefSource meta lbs)

readTree :: AnyStorage -> GroupSecret -> HashRef -> IO LBS.ByteString
readTree sto gks h =
  runExceptT (readFromMerkle sto (ToDecryptBS (fromHashRef h) (\_ -> pure (Just gks))))
    >>= either (assertFailure . ("cannot read back: " <>) . show) pure

annOf :: AnyStorage -> HashRef -> IO (MTreeAnn [HashRef])
annOf sto h = do
  bs <- getBlock sto (fromHashRef h) >>= maybe (assertFailure "no root block") pure
  either (assertFailure . ("bad annotation: " <>) . show) pure
    (deserialiseOrFail @(MTreeAnn [HashRef]) bs)

-- The leaves in order, which for an encrypted tree are the ciphertext blocks.
leavesOf :: AnyStorage -> HashRef -> IO [HashRef]
leavesOf sto h = do
  ann <- annOf sto h
  S.toList_ $ walkMerkleTree (_mtaTree ann) (lift . getBlock sto) $ \case
    Left miss -> lift $ assertFailure ("missing block " <> show (pretty miss))
    Right hs  -> S.each hs

-- Each block as the writer stored it: its nonce, and the ciphertext that nonce
-- was used on.
blocksOf :: AnyStorage -> HashRef -> IO [(SK.Nonce, ByteString)]
blocksOf sto h = do
  hs <- leavesOf sto h
  forM hs $ \x -> do
    bs <- getBlock sto (fromHashRef x) >>= maybe (assertFailure "missing leaf") pure
    either (assertFailure . ("leaf is not a nonce and a box: " <>) . show) pure
      (deserialiseOrFail @(SK.Nonce, ByteString) bs)

treeNonce :: AnyStorage -> HashRef -> IO ByteString
treeNonce sto h = do
  ann <- annOf sto h
  case _mtaCrypt ann of
    EncryptGroupNaClSymm _ n -> pure n
    _                        -> assertFailure "tree is not group encrypted"

oneMegabyte :: Int
oneMegabyte = 1024 * 1024

-- | A payload comes back as it went in.
testEncryptedTreeRoundTrip :: IO ()
testEncryptedTreeRoundTrip = do
  sto <- newMemStorage
  (gks, gk) <- newGroup

  let payload = filler "round trip" (oneMegabyte + 3 * 4096 + 17)

  h <- writeTree sto gks gk "name: \"a\"" payload
  back <- readTree sto gks h

  assertEqual "a tree decrypts to what was encrypted" payload back

  -- And the metadata, which travels in a block of its own with a nonce of its
  -- own, for the same reason.
  meta <- runExceptT (extractMetaData @'HBS2Basic (\_ -> pure (Just gks)) sto h)
            >>= either (assertFailure . show) pure
  assertEqual "the metadata block decrypts too" "name: \"a\"" meta

-- | Two payloads that share a first megabyte do not share a keystream.
--
-- They do share a content key: the tree nonce is a hash of that megabyte, and
-- the case asserts as much, so that it keeps testing what it was written for
-- even if the prefix rule is reworked. What must not repeat is the per-block
-- nonce, and a repeat is only harmless when the ciphertext repeats with it --
-- that is the same block, encrypted the same way, which is the deduplication
-- this format is built around.
testEncryptedTreeSharedPrefixNoNonceReuse :: IO ()
testEncryptedTreeSharedPrefixNoNonceReuse = do
  sto <- newMemStorage
  (gks, gk) <- newGroup

  -- Exactly the megabyte getNonce hashes, so the two payloads agree on it, then
  -- two blocks each of something else.
  let shared = filler "shared prefix" oneMegabyte
  let one = shared <> filler "first tail"  (2 * fromIntegral defBlockSize)
  let two = shared <> filler "second tail" (2 * fromIntegral defBlockSize)

  h1 <- writeTree sto gks gk "name: \"one\"" one
  h2 <- writeTree sto gks gk "name: \"two\"" two

  n1 <- treeNonce sto h1
  n2 <- treeNonce sto h2
  assertEqual "the two trees do derive one content key" n1 n2

  b1 <- blocksOf sto h1
  b2 <- blocksOf sto h2

  -- The concrete thing that used to break: same key, same position in the tree,
  -- different bytes.
  let tailIndex = oneMegabyte `div` fromIntegral defBlockSize
  case (drop tailIndex b1, drop tailIndex b2) of
    ((na, ca) : _, (nb, cb) : _) -> do
      assertBool "the two tails really are different bytes" (ca /= cb)
      assertBool "a block that differs gets a nonce that differs" (na /= nb)
    _ -> assertFailure "expected blocks past the shared prefix"

  -- And nowhere else either.
  let bySeen = Map.fromListWith (<>) [ (Saltine.encode n, [c]) | (n, c) <- b1 <> b2 ]
  forM_ (Map.toList bySeen) $ \(_, cs) ->
    assertEqual "one nonce is used on one ciphertext" 1 (length (L.nub cs))

  -- Both still readable, each as itself.
  readTree sto gks h1 >>= assertEqual "the first payload survives" one
  readTree sto gks h2 >>= assertEqual "the second payload survives" two

-- | Appending to a payload still costs only the blocks that changed.
--
-- This is what the prefix rule buys and what the fix had to keep: the leading
-- blocks are the same blocks, by hash, so a store already holding the first
-- version gains nothing but the tail.
testEncryptedTreeAppendStillDedups :: IO ()
testEncryptedTreeAppendStillDedups = do
  sto <- newMemStorage
  (gks, gk) <- newGroup

  let before = filler "a log" (oneMegabyte + 2 * fromIntegral defBlockSize)
  let after  = before <> filler "one more line" (fromIntegral defBlockSize)

  h1 <- writeTree sto gks gk "name: \"log\"" before
  h2 <- writeTree sto gks gk "name: \"log\"" after

  l1 <- leavesOf sto h1
  l2 <- leavesOf sto h2

  assertBool "the appended version has more blocks" (length l2 > length l1)
  assertEqual "the blocks that did not change are the same blocks"
    l1 (take (length l1) l2)

  readTree sto gks h2 >>= assertEqual "and the longer payload reads back" after

-- | A tree written the way trees used to be written still reads.
--
-- The nonce moving into the block changed what a writer produces, so this
-- builds a tree by the old rule -- one key for the tree, the block's position
-- for its nonce, no nonce stored -- and asks the current reader for it back.
testEncryptedTreeReadsLegacyMethod1 :: IO ()
testEncryptedTreeReadsLegacyMethod1 = do
  sto <- newMemStorage
  (gks, gk) <- newGroup

  let payload = filler "written in 2023" (3 * fromIntegral defBlockSize + 1024)

  h <- writeLegacyMethod1Tree sto gks gk payload

  ann <- annOf sto h
  case _mtaCrypt ann of
    EncryptGroupNaClSymm1{} -> pure ()
    _ -> assertFailure "the fixture is supposed to be a Method1 tree"

  readTree sto gks h >>= assertEqual "an old tree still decrypts" payload

-- The pre-fix writer, kept here rather than in the library because nothing
-- should write this again.
writeLegacyMethod1Tree :: AnyStorage
                       -> GroupSecret
                       -> GroupKey 'Symm 'HBS2Basic
                       -> LBS.ByteString
                       -> IO HashRef
writeLegacyMethod1Tree sto gks gk lbs = do

  gkh <- writeAsMerkle sto (serialise gk) <&> HashRef

  let HbSyncHash nonceS = hashObject @HbSync (LBS.take (fromIntegral oneMegabyte) lbs)

  let prk = HKDF.extractSkip @_ @HbSyncHash (Saltine.encode gks)
  let key0 = HKDF.expand prk nonceS typicalKeyLength & Saltine.decode & fromJust
  let nonce0 = nonceFrom @SK.Nonce nonceS

  let chunks = go lbs
        where go bs | LBS.null bs = []
                    | otherwise   = let (a,b) = LBS.splitAt (fromIntegral defBlockSize) bs
                                    in a : go b

  hashes <- forM (zip [1 :: Word64 ..] chunks) $ \(i, bs) -> do
    let ct = SK.secretbox key0 (nonceFrom (nonce0, i)) (LBS.toStrict bs)
    putBlock sto (LBS.fromStrict ct)
      >>= maybe (assertFailure "cannot store a block") (pure . HashRef)

  let pt = toPTree (MaxSize defHashListChunk) (MaxNum defTreeChildNum) hashes

  result <- runWriterT $ makeMerkle 0 pt $ \(hx, mt, bss) -> do
    _ <- lift $ putBlock sto bss
    tell [(hx, mt)]

  tree <- case [ mt | (hx, mt) <- snd result, hx == fst result ] of
            (t : _) -> pure t
            _       -> assertFailure "cannot build the tree"

  let ann = MTreeAnn (ShortMetadata "name: \"old\"")
                     (EncryptGroupNaClSymm1 (fromHashRef gkh) nonceS)
                     tree

  putBlock sto (serialise ann)
    >>= maybe (assertFailure "cannot store the annotation") (pure . HashRef)
