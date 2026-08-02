{-# Language RankNTypes #-}
module HBS2.Peer.Proto.BlockChunks where

import HBS2.Events
import HBS2.Hash
import HBS2.Net.Proto
import HBS2.Peer.Proto.Peer
import HBS2.Prelude.Plated
import HBS2.Actors.Peer
import HBS2.Storage
import HBS2.Net.Proto.Sessions
import HBS2.System.Logger.Simple

import Data.Word
import Data.ByteString.Lazy (ByteString)
import Data.Maybe

-- | Наименьший размер куска, который мы соглашаемся нарезать по чужой просьбе.
--
-- Просящий выбирает ChunkSize сам, а платит нарезкой отвечающий: на каждый
-- кусок заводится отдельная задача deferred и уходит отдельная датаграмма. При
-- size = 1 блок в 256 КиБ (defBlockSize) превращается в 262144 задачи на
-- общем пайплайне, который держит 131072; addJob, простояв три секунды на
-- полной очереди, кидает PipelineAddJobTimeout, а исключение отсюда валит
-- процесс (см. комментарий у calcChunks). То есть маленький size был вторым
-- выключателем демона, рядом с нулевым.
--
-- 256 выбрано с запасом вниз от defChunkSize = 1024, которым просит наш
-- собственный загрузчик, и заведомо ниже потолка датаграммы в 4096. Кусок
-- мельче этого не имеет смысла: заголовки съедают больше, чем полезная часть.
minChunkSize :: Integral a => a
minChunkSize = 256

-- | Сколько кусков может быть в одном ответе.
--
-- Не политика, а предел самого протокола: номер куска едет в ChunkNum, то есть
-- в Word16. Запрос, дающий больше кусков, не просто дорог -- он неисполним,
-- потому что счётчик в zip offsets' [0..] заворачивается и разные куски
-- получают одинаковые номера. Раньше такой запрос обслуживался молча и слал
-- перенумерованную кашу.
maxChunksPerRequest :: Integral a => a
maxChunksPerRequest = 65536

newtype ChunkSize = ChunkSize Word16
                    deriving newtype (Num,Enum,Real,Integral,Pretty)
                    deriving stock (Eq,Ord,Show,Data,Generic)


newtype ChunkNum = ChunkNum Word16
                   deriving newtype (Num,Enum,Real,Integral,Pretty)
                   deriving stock (Eq,Ord,Show,Data,Generic)


type OnBlockReady h m = Hash h -> m ()


type GetBlockChunk h m = Hash h -> Offset -> Size -> m (Maybe ByteString)


type AcceptChunk h e m = Response e (BlockChunks e) m
                       => ( Cookie e, Peer e, Hash HbSync, ChunkNum, ByteString ) -> m ()

type GetBlockHash h e m = (Peer e, Cookie e) -> m (Maybe (Hash h))

data BlockChunksI e m =
  BlockChunksI
  { blkSize         :: GetBlockSize HbSync m
  , blkChunk        :: GetBlockChunk HbSync m
  , blkGetHash      :: GetBlockHash HbSync e m
  , blkAcceptChunk  :: AcceptChunk HbSync e m
  }


data BlockChunks e = BlockChunks (Cookie e) (BlockChunksProto e)
                     deriving stock (Generic, Show)

data BlockChunksProto e = BlockGetAllChunks (Hash HbSync) ChunkSize
                        | BlockGetChunks (Hash HbSync) ChunkSize Word32 Word32
                        | BlockNoChunks
                        | BlockChunk ChunkNum ByteString
                        | BlockLost
                        deriving stock (Generic, Show)


instance HasCookie e (BlockChunks e) where
  type instance Cookie e = Word32
  getCookie (BlockChunks c _) = Just c

instance Serialise ChunkSize
instance Serialise ChunkNum
instance Serialise (BlockChunksProto e)
instance Serialise (BlockChunks e)


newtype instance EventKey e (BlockChunks e) =
  BlockChunksEventKey (Cookie e, Hash HbSync)
  deriving stock (Typeable, Eq, Generic)

deriving instance Hashable (EventKey e (BlockChunks e))

data instance Event e (BlockChunks e) =
    BlockReady      (Hash HbSync)
  | BlockChunksLost (Hash HbSync)
  deriving stock (Typeable)

blockChunksProto :: forall e m proto . ( MonadIO m
                                       , Response e (BlockChunks e) m
                                       , HasDeferred (BlockChunks e) e m
                                       , HasOwnPeer e m
                                       , Sessions e (KnownPeer e) m
                                       , Pretty (Peer e)
                                       , proto ~ BlockChunks e
                                       )
                 => BlockChunksI e m
                 -> BlockChunks e
                 -> m ()

blockChunksProto adapter (BlockChunks c p) = do

  peer <- thatPeer @proto
  auth <- find (KnownPeerKey peer) id <&> isJust

  case p of

    BlockGetChunks h size n1 num | auth -> deferred @proto do

      bsz' <- blkSize adapter h

      maybe1 bsz' (pure ()) $ \bsz -> do

        whenServable peer size bsz do
          let offsets' = calcChunks bsz (fromIntegral size) :: [(Offset, Size)]
          let offsets = take (fromIntegral num) $ drop (fromIntegral n1) $ zip offsets' [0..]

          for_ offsets $ \((o,sz),i) -> deferred @proto do
            chunk <- blkChunk adapter h o sz
            maybe (pure ()) (response_ . BlockChunk @e i) chunk

    BlockGetAllChunks h size | auth -> deferred @proto do

      bsz' <- blkSize adapter h

      maybe1 bsz' (pure ()) $ \bsz -> do

        whenServable peer size bsz do
          let offsets' = calcChunks bsz (fromIntegral size) :: [(Offset, Size)]
          let offsets = zip offsets' [0..]

          for_ offsets $ \((o,sz),i) -> deferred @proto do
            chunk <- blkChunk adapter h o sz
            maybe (pure ()) (response_ . BlockChunk @e i) chunk

    BlockChunk n bs | auth -> deferred @(BlockChunks e) do
      who <- thatPeer @proto
      h <- blkGetHash adapter (who, c)

      maybe1 h (response_ (BlockLost @e)) $ \hh -> do
        void $ blkAcceptChunk adapter (c, who, hh, n, bs)

    BlockNoChunks {} -> do
      -- TODO: notification
      pure ()

    BlockLost{} -> do
      -- liftIO $ print "GOT BLOCK LOST MESSAGE - means IO ERROR"
      pure ()

    _ -> do
      pure ()

  where
    response_ pt = response (BlockChunks c pt)

    -- Соглашаемся ли мы вообще нарезать блок так, как просят.
    --
    -- Оба предела и причины к ним -- у minChunkSize и maxChunksPerRequest выше.
    -- Отказ шумит в лог, потому что это не рядовое событие: наш собственный
    -- загрузчик просит defChunkSize, и до этих границ ему очень далеко.
    whenServable peer size bsz action
      | (fromIntegral size :: Integer) < minChunkSize = do
          warn $ "block-chunks: refused chunk size" <+> pretty size
                   <+> "from" <+> pretty peer

      | chunks > maxChunksPerRequest = do
          warn $ "block-chunks: refused" <+> pretty chunks
                   <+> "chunks for one request from" <+> pretty peer

      | otherwise = action

      where
        -- size здесь заведомо больше нуля: первый охранник уже отсёк всё, что
        -- меньше minChunkSize.
        chunks :: Integer
        chunks = (bsz + fromIntegral size - 1) `div` fromIntegral size


