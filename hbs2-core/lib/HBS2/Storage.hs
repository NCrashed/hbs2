{-# Language FunctionalDependencies #-}
{-# Language DefaultSignatures #-}
module HBS2.Storage where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Data.Types.Refs (RefMetaData(..))

import Data.Kind
import Lens.Micro.Platform
import Data.ByteString.Lazy (ByteString)
import Control.Monad.Trans.Maybe
import Control.Exception
import Data.Word

import Codec.Serialise()

data StorageException =
  StorageAddTaskTimeout
  deriving (Show,Typeable)

instance Exception StorageException

class Pretty (Hash h) => IsKey h where
  type family Key h :: Type

instance Key HbSync ~ Hash HbSync => IsKey HbSync where
  type instance Key HbSync = Hash HbSync

newtype StoragePrefix = StoragePrefix  { fromPrefix :: FilePath }
                        deriving stock (Data,Show)
                        deriving newtype (IsString,Pretty)

newtype Offset = Offset Integer
                 deriving newtype (Eq,Ord,Enum,Num,Real,Integral,Hashable,Pretty,Serialise)
                 deriving stock (Show)

newtype Size = Size Integer
               deriving newtype (Eq,Ord,Enum,Num,Real,Integral,Hashable,Pretty,Serialise)
               deriving stock (Show)

data ExpiredAfter a = ExpiredAfter Word64 a
                      deriving stock (Generic)

instance Serialise a => Serialise (ExpiredAfter a)


class ( Monad m
      , IsKey h
      , Hashed h block
      ) => Storage a h block m | a -> block, a -> h where

  putBlock :: a -> block -> m (Maybe (Key h))

  enqueueBlock :: a -> block -> m (Maybe (Key h))

  getBlock :: a -> Key h -> m (Maybe block)

  delBlock :: a -> Key h -> m ()

  getChunk :: a -> Key h -> Offset -> Size -> m (Maybe block)

  hasBlock :: a -> Key h -> m (Maybe Integer)

  updateRef :: (Hashed h k, RefMetaData k) => a -> k -> Key h -> m ()

  getRef :: (Hashed h k, Pretty k, RefMetaData k) => a -> k -> m (Maybe (Key h))

  delRef :: (Hashed h k, RefMetaData k) => a -> k -> m ()

data AnyStorage = forall zu  . ( Storage zu HbSync ByteString IO
                               ) => AnyStorage zu

class HasStorage m where
  getStorage :: m AnyStorage

instance (Monad m, HasStorage m) => HasStorage (MaybeT m) where
  getStorage = lift getStorage

instance Hashed h a => Hashed h (ExpiredAfter a) where
  hashObject (ExpiredAfter _ a) = hashObject a

instance RefMetaData a => RefMetaData (ExpiredAfter a) where
  refMetaData (ExpiredAfter t x) = [("expires", show t)] <> refMetaData x

instance (IsKey HbSync, MonadIO m) => Storage AnyStorage HbSync ByteString m  where
  putBlock (AnyStorage s) = liftIO . putBlock s
  enqueueBlock (AnyStorage s) = liftIO . enqueueBlock s
  getBlock (AnyStorage s) = liftIO . getBlock s
  getChunk (AnyStorage s) h a b = liftIO $ getChunk s h a b
  hasBlock (AnyStorage s) = liftIO . hasBlock s
  updateRef (AnyStorage s) r v = liftIO $ updateRef s r v
  getRef (AnyStorage s) = liftIO . getRef s
  delBlock (AnyStorage s) = liftIO . delBlock s
  delRef (AnyStorage s) = liftIO . delRef s


-- | Разбиение блока на куски.
--
-- Размер куска, не больший нуля, даёт пустой список, а не исключение. Раньше
-- функция сразу шла в `divMod`, и вызов с нулём кидал DivideByZero. Это
-- достижимо из сети: blockChunksProto берёт ChunkSize (Word16) прямо из пакета
-- и передаёт сюда, а исключение из обработчика на пайплайне deferred уходит в
-- связанный поток (runPipeline не ловит ничего, запускается через asyncLinked)
-- и валит процесс целиком. То есть один пакет с size = 0 был выключателем
-- демона.
--
-- Пустой список -- правильный ответ и по смыслу: на кусок нулевой длины блок
-- не делится никак, и отвечать тут нечем.
calcChunks :: forall a b . (Integral a, Integral b)
           => Integer  -- | block size
           -> Integer  -- | chunk size
           -> [(a, b)]

calcChunks s1 s2
  | s2 <= 0   = []
  | otherwise = fmap (over _1 fromIntegral . over _2 fromIntegral)  chu
  where
    (n,rest) = s1 `divMod` s2
    chu = [ (x*s2,s2) | x <- [0..n-1] ] <> [(n * s2, rest) | rest > 0]

-- | What became of bytes offered as a block somebody asked for.
data PutAsked =
    PutAskedStored    -- ^ they were that block, and it is stored
  | PutAskedMismatch  -- ^ they were some other block, and nothing was written
  | PutAskedFailed    -- ^ they were that block, and the store refused it
  deriving stock (Eq,Show)

-- | Store bytes as the block that was asked for, or not at all.
--
-- HASHED BEFORE IT IS STORED, and the order is the whole point. A download used
-- to put the bytes, compare the hash the store answered with, and on a mismatch
-- delete THAT hash. But the store is content-addressed and deduplicates: if the
-- bytes a peer sent are a block this node already held, the put is a no-op and
-- the delete removes this node's own copy. Any handshaked peer that knew one
-- block of ours -- a public one is enough -- could answer a request for
-- anything with its bytes and delete it here, one block per request.
--
-- Bytes that are not what was asked for are never written, so there is nothing
-- to take back and no moment in which they are readable.
putBlockAsked :: Storage s HbSync ByteString m
              => s -> Hash HbSync -> ByteString -> m PutAsked
putBlockAsked sto h bs
  | hashObject @HbSync bs /= h = pure PutAskedMismatch
  | otherwise = putBlock sto bs <&> \case
      Just h1 | h1 == h -> PutAskedStored
      _                 -> PutAskedFailed



