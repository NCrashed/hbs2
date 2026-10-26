-- | What a download does with bytes that are not the block it asked for.
--
-- Against a store that deduplicates, because that is where the defect lived: a
-- put of bytes the store already holds is a no-op, so put-then-delete-on-mismatch
-- deleted a block this node had before the request was made.
module TestPutBlockAsked
  ( testPutAskedKeepsWhatWasAlreadyHeld
  , testPutAskedWritesNothingItWasNotAskedFor
  , testPutAskedStoresWhatWasAskedFor
  ) where

import HBS2.Hash
import HBS2.Storage

import Control.Concurrent.STM
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Test.Tasty.HUnit

-- | The smallest store that behaves like the real ones about the one thing that
-- matters here: blocks are keyed by their hash, and a second put of the same
-- bytes changes nothing.
newtype MemStore = MemStore (TVar (HashMap (Hash HbSync) ByteString))

newMemStore :: IO MemStore
newMemStore = MemStore <$> newTVarIO mempty

instance Storage MemStore HbSync ByteString IO where
  putBlock (MemStore v) bs = do
    let h = hashObject @HbSync bs
    atomically $ modifyTVar' v (HM.insert h bs)
    pure (Just h)
  enqueueBlock = putBlock
  getBlock (MemStore v) h = HM.lookup h <$> readTVarIO v
  delBlock (MemStore v) h = atomically $ modifyTVar' v (HM.delete h)
  getChunk _ _ _ _ = pure Nothing
  hasBlock (MemStore v) h = fmap (fromIntegral . LBS.length) . HM.lookup h <$> readTVarIO v
  updateRef _ _ _ = pure ()
  getRef _ _ = pure Nothing
  delRef _ _ = pure ()

ours, asked :: ByteString
ours  = "a block this node already holds, and which any peer can know"
asked = "the block that was actually requested"

-- | The attack: a request for one block answered with the bytes of another
-- block this node already holds. That block has to survive it.
testPutAskedKeepsWhatWasAlreadyHeld :: IO ()
testPutAskedKeepsWhatWasAlreadyHeld = do
  sto <- newMemStore
  Just hOurs <- putBlock sto ours

  r <- putBlockAsked sto (hashObject @HbSync asked) ours
  r @?= PutAskedMismatch

  kept <- getBlock sto hOurs
  kept @?= Just ours

-- | And bytes nobody here held are not written, not even for a moment.
testPutAskedWritesNothingItWasNotAskedFor :: IO ()
testPutAskedWritesNothingItWasNotAskedFor = do
  sto <- newMemStore
  r <- putBlockAsked sto (hashObject @HbSync asked) ours
  r @?= PutAskedMismatch
  there <- hasBlock sto (hashObject @HbSync ours)
  there @?= Nothing

-- | The ordinary case still stores the block.
testPutAskedStoresWhatWasAskedFor :: IO ()
testPutAskedStoresWhatWasAskedFor = do
  sto <- newMemStore
  r <- putBlockAsked sto (hashObject @HbSync asked) asked
  r @?= PutAskedStored
  got <- getBlock sto (hashObject @HbSync asked)
  got @?= Just asked
