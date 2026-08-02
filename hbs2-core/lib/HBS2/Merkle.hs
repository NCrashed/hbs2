{-# Language  TemplateHaskell #-}
{-# Language  DeriveFunctor #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module HBS2.Merkle where

import HBS2.Prelude
import HBS2.Hash

import Codec.Serialise (serialise, deserialiseOrFail)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Data
import Data.Foldable (traverse_)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Word
import Lens.Micro.Platform
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict (StateT,evalStateT,gets,modify')
import Control.Monad
-- import Prettyprinter



newtype MerkleHash = MerkleHash { fromMerkleHash :: Hash HbSync }
                     deriving newtype (Eq,Ord,IsString,Pretty)
                     deriving stock (Data,Generic)

class Foldable t => Chunks t a where
  chunks :: Int -> a -> t a

instance Chunks [] ByteString where
  chunks i bs = go [] bs
    where
      size = fromIntegral i
      go acc x | BS.null x  = acc
               | BS.length x <= size = acc <> [x]
               | otherwise = go (acc <> [BS.take size x]) (BS.drop size x)

instance Chunks [] LBS.ByteString where
  chunks i bs = go [] bs
    where
      size = fromIntegral i
      go acc x | LBS.null x  = acc
               | LBS.length x <= size = acc <> [x]
               | otherwise = go (acc <> [LBS.take size x]) (LBS.drop size x)

instance Chunks [] [a] where
  chunks i xs = go xs
    where
      go [] = []
      go es | length es <= i = [es]
            | otherwise = let (p, ps) = List.splitAt i es in p : go ps


data PTree  a = T [PTree a] | L a
                deriving stock (Show, Functor, Generic)


instance Foldable PTree where
  foldr fn acc (L a)  = fn a acc
  foldr fn acc (T xs) = go acc xs
    where
      go b [] = b
      go b (y:ys) = foldr fn (go b ys) y

instance Traversable PTree where
  traverse fn (L a) = L <$> fn a
  traverse fn (T xs) = T <$> traverse (traverse fn) xs

newtype MaxNum a = MaxNum a
newtype MaxSize a = MaxSize a


newtype MNodeData =
  MNodeData
  { _mnodeHeight :: Integer
  }
  deriving stock (Generic,Data,Show)

makeLenses ''MNodeData

instance Serialise MNodeData

data AnnMetaData = NoMetaData | ShortMetadata Text | AnnHashRef (Hash HbSync)
  deriving stock (Generic,Data,Show,Eq)

instance Serialise AnnMetaData

data MTreeAnn a = MTreeAnn
 { _mtaMeta :: !AnnMetaData
 , _mtaCrypt :: !MTreeEncryption
 , _mtaTree :: !(MTree a)
 }
 deriving stock (Generic,Data,Show)

instance Serialise a => Serialise (MTreeAnn a)

data MerkleEncryptionType
  deriving stock (Data)

data EncryptGroupNaClSymmOpts =
  EncryptGroupNaClSymmBlockSIP (Word64, Word64)
  deriving stock (Eq,Ord,Show,Generic,Data)

instance Serialise EncryptGroupNaClSymmOpts

data MTreeEncryption
  = NullEncryption
  | CryptAccessKeyNaClAsymm (Hash HbSync)
  | EncryptGroupNaClSymm1 (Hash HbSync) ByteString
  | EncryptGroupNaClSymm2 EncryptGroupNaClSymmOpts (Hash HbSync) ByteString
  deriving stock (Eq,Generic,Data,Show)
{-# COMPLETE NullEncryption, CryptAccessKeyNaClAsymm, EncryptGroupNaClSymm #-}

instance Serialise MTreeEncryption

pattern EncryptGroupNaClSymm :: Hash HbSync -> ByteString -> MTreeEncryption
pattern EncryptGroupNaClSymm a b  <- ( isEncryptGroupNaClSymm -> Just (a, b) ) where
  EncryptGroupNaClSymm a b = EncryptGroupNaClSymm1 a b

isEncryptGroupNaClSymm :: MTreeEncryption
                       -> Maybe (Hash HbSync, ByteString)
isEncryptGroupNaClSymm = \case
  EncryptGroupNaClSymm2 _ a b -> Just (a,b)
  EncryptGroupNaClSymm1  a b  -> Just (a,b)
  _                           -> Nothing


data MTree a = MNode MNodeData [Hash HbSync] | MLeaf a
               deriving stock (Generic,Data,Show)

instance Serialise a => Serialise (MTree a)

newMNode :: Integer -> [Hash HbSync] -> MTree a
newMNode h = MNode (MNodeData h)

toPTree :: (Chunks [] a)
        => MaxSize Int
        -> MaxNum Int
        -> a
        -> PTree a

toPTree (MaxSize s) (MaxNum n) items | n <= 1 =
  case T $ fmap L (chunks s items) of
    T [L x] -> L x
    _       -> T []

toPTree (MaxSize s) (MaxNum n) items = go $ T (fmap L (chunks s items))
  where
    go (T []) = T []
    go (T [L x]) = L x
    go (T xs) | length xs <= n = T xs
              | otherwise = go $ T $ fmap go [ T x | x <- chunks n xs ]

    go leaf = leaf


makeMerkle :: (Monad m, Serialise a, Serialise (MTree a))
           => Integer -- | initial height
           -> PTree a
           -> ((Hash HbSync, MTree a, LBS.ByteString) -> m ())
           -> m (Hash HbSync)

makeMerkle h0 pt f = fst <$> go h0 pt
  where
    go hx (T xs) = do
      rs <- mapM (go hx) xs
      let hxx = maximumDef hx (fmap snd rs)
      let o = newMNode hxx (fmap fst rs)
      let bs = serialise o
      let h  = hashObject bs
      f (h, o, bs)
      pure (h, 1+hxx)

    go hx (L x) = do
      let o = MLeaf x
      let bs = serialise o
      let h  = hashObject bs
      f (h, o, bs)
      pure (h, 1+hx)



-- NOTE: MerkleAnn-note
--   MerkleAnn ломает всю красоту, но, видимо,
--   с этим уже ничего не поделать, пусть живёт.
--   Надо было аннотации просто класть 0-ым блоком
--   в меркл дерево, или вообще делать cons-ячейки,
--   но что уж теперь.

-- | Walk a merkle tree, sinking every node.
--
-- FOLLOWS EVERY EDGE, including an edge to a node it has already entered, and
-- that is deliberate: reconstructing content depends on a repeated leaf being
-- emitted once per occurrence. It also means the cost is the number of PATHS
-- and not the number of blocks, so a root somebody else chose can cost 2^depth
-- for depth+1 blocks. Use 'walkMerkleUnique'' where the question is about a set
-- of blocks rather than a sequence of bytes; the numbers and the reasoning are
-- in its haddock.
walkMerkle' :: forall a m . (Serialise (MTree a), (Serialise (MTreeAnn a)), Monad m)
           => Hash HbSync
           -> ( Hash HbSync -> m (Maybe LBS.ByteString) )
           -> ( Either (Hash HbSync) (MTree a) -> m () )
           -> m ()

walkMerkle' root flookup sink = go root
  where
    go hash = void $ runMaybeT do

      bs0 <- lift $ flookup hash

      bs <- MaybeT $ maybe1 bs0 (sink (Left hash) >> pure mzero) (pure . Just)

      let t1 = deserialiseOrFail @(MTree a) bs

      either (const $ runWithAnnTree hash bs) runWithTree t1

    runWithAnnTree _hash bs  = do
      let t = deserialiseOrFail @(MTreeAnn a) bs
      case t of
        Left{} -> pure ()
        Right (MTreeAnn { _mtaTree = t1 }) -> runWithTree t1

    runWithTree t = lift do
      case t of
        n@(MLeaf _) -> sink (Right n)
        n@(MNode _ hashes) -> sink (Right n) >> traverse_ go hashes

walkMerkle :: forall a m . (Serialise (MTree a), Serialise (MTreeAnn a), Serialise a, Monad m)
           => Hash HbSync
           -> ( Hash HbSync -> m (Maybe LBS.ByteString) )
           -> ( Either (Hash HbSync) a -> m () )
           -> m ()

walkMerkle root flookup sink = do

  walkMerkle' root flookup withTree

  where
    withTree = \case
        (Right (MLeaf s))   -> sink (Right s)
        (Right (MNode _ _)) -> pure ()
        Left hx             -> sink (Left hx)


-- | As 'walkMerkle'', but each node block is entered at most once.
--
-- A merkle "tree" here is whatever the block graph turns out to be, and a node
-- lists child hashes with nothing stopping two of them being equal. 'walkMerkle''
-- follows every edge, so a chain in which every node names its one child TWICE
-- costs 2^depth for depth+1 blocks. Measured against this module, with the
-- blocks in a map so that the cost is the walk and nothing else:
--
-- >  depth   blocks         visits    seconds
-- >      4        5             31      0.000
-- >     16       17         131071      0.127
-- >     20       21        2097151      2.084
-- >     22       23        8388607      8.958
--
-- Twenty-three blocks is about a kilobyte, it is well-formed, and it is
-- four times the cost for every two levels added: forty-one blocks is 2^40
-- visits and does not finish. A peer serves those blocks the ordinary way, so
-- anything that walks a root somebody else chose can be stopped with a
-- kilobyte. That reaches the mailbox status handler, and 'findMissedBlocks'
-- from there.
--
-- WHY THIS IS A SEPARATE FUNCTION and not a fix to 'walkMerkle''. Skipping a
-- repeated node is only sound when the caller is asking about a SET. It is not
-- sound for reading content: 'readFromMerkle' concatenates leaf payloads in
-- traversal order, and identical runs of content hash identically, so a file of
-- ten megabytes of zeroes genuinely does have the same leaf block listed many
-- times over. Deduplicating there would silently return a shorter file. So the
-- traversal that reconstructs bytes keeps following every edge, and the callers
-- that are asking which blocks a graph mentions use this.
--
-- The visited set also bounds what a caller can accumulate: the number of
-- distinct blocks, rather than the number of paths to them.
walkMerkleUnique' :: forall a m . (Serialise (MTree a), (Serialise (MTreeAnn a)), Monad m)
           => Hash HbSync
           -> ( Hash HbSync -> m (Maybe LBS.ByteString) )
           -> ( Either (Hash HbSync) (MTree a) -> m () )
           -> m ()

walkMerkleUnique' root flookup sink = evalStateT (go root) mempty
  where
    go :: Hash HbSync -> StateT (HashSet (Hash HbSync)) m ()
    go hash = do
      seen <- gets (HashSet.member hash)
      unless seen do
        modify' (HashSet.insert hash)
        bs0 <- lift (flookup hash)
        case bs0 of
          Nothing -> lift (sink (Left hash))
          Just bs -> case deserialiseOrFail @(MTree a) bs of
            Right t -> walkTree t
            Left{}  -> case deserialiseOrFail @(MTreeAnn a) bs of
              -- See the MerkleAnn-note above: an annotated tree is a different
              -- block shape holding the same tree, and bytes that are neither
              -- are not an error here, they are simply not a tree.
              Right (MTreeAnn { _mtaTree = t }) -> walkTree t
              Left{}                            -> pure ()

    walkTree :: MTree a -> StateT (HashSet (Hash HbSync)) m ()
    walkTree = \case
      n@(MLeaf _)        -> lift (sink (Right n))
      n@(MNode _ hashes) -> lift (sink (Right n)) >> traverse_ go hashes

-- | As 'walkMerkle', but each node block is entered at most once.
--
-- The leaf-payload half of 'walkMerkleUnique''. For anything asking which
-- blocks a graph mentions; NOT for reconstructing content, for the reason
-- 'walkMerkleUnique'' gives.
walkMerkleUnique :: forall a m . (Serialise (MTree a), Serialise (MTreeAnn a), Serialise a, Monad m)
           => Hash HbSync
           -> ( Hash HbSync -> m (Maybe LBS.ByteString) )
           -> ( Either (Hash HbSync) a -> m () )
           -> m ()

walkMerkleUnique root flookup sink = do

  walkMerkleUnique' root flookup withTree

  where
    withTree = \case
        (Right (MLeaf s))   -> sink (Right s)
        (Right (MNode _ _)) -> pure ()
        Left hx             -> sink (Left hx)


walkMerkleTree :: (Serialise (MTree a), Serialise a, Monad m)
           => MTree a
           -> ( Hash HbSync -> m (Maybe LBS.ByteString) )
           -> ( Either (Hash HbSync) a -> m () )
           -> m ()

walkMerkleTree tree flookup sink = case tree of
    (MLeaf s)   -> sink (Right s)
    (MNode _ hashes) -> forM_ hashes \h -> walkMerkle h flookup sink

