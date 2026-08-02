{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | What a merkle walk costs when somebody else chose the root.
--
-- A node lists child hashes and nothing stops two of them being equal, so the
-- block graph is a DAG and the number of PATHS through it is not the number of
-- blocks in it. 'walkMerkle'' follows every edge, so a chain in which every node
-- names its one child twice costs 2^depth for depth+1 blocks. Measured against
-- this module with the blocks in a map, so the cost is the walk and nothing
-- else:
--
-- >  depth   blocks         visits    seconds
-- >      4        5             31      0.000
-- >     16       17         131071      0.127
-- >     20       21        2097151      2.084
-- >     22       23        8388607      8.958
--
-- Twenty-three blocks is about a kilobyte and it is well-formed; a peer serves
-- it the ordinary way. Depth 40 is forty-one blocks and does not finish.
--
-- The tests below pin both halves of the answer: 'walkMerkleUnique'' enters each
-- node once, and 'walkMerkle'' still does not, because reconstructing content
-- depends on a repeated leaf being emitted once per occurrence.
module TestMerkleWalk (testMerkleWalkBounded, testMerkleWalkKeepsDuplicates) where

import HBS2.Hash
import HBS2.Merkle
import HBS2.Data.Types.Refs

import Codec.Serialise
import Control.Monad
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.IORef

import Test.Tasty.HUnit

type Blocks = Map (Hash HbSync) LBS.ByteString

-- A chain of depth d in which every node lists its one child TWICE. Well formed
-- in every respect: d+1 distinct blocks, no cycle, every hash resolves.
bomb :: Int -> (Hash HbSync, Blocks)
bomb d = go d
  where
    leafBs = serialise (MLeaf [] :: MTree [HashRef])
    leafH  = hashObject leafBs :: Hash HbSync

    go :: Int -> (Hash HbSync, Blocks)
    go 0 = (leafH, Map.singleton leafH leafBs)
    go n =
      let (ch, m) = go (n - 1)
          nodeBs  = serialise (newMNode (fromIntegral n) [ch, ch] :: MTree [HashRef])
          nodeH   = hashObject nodeBs :: Hash HbSync
      in (nodeH, Map.insert nodeH nodeBs m)

-- Two leaves carrying the same payload, under one node that names each once.
-- This is the shape a file of repeated content really has: identical runs are
-- identical blocks and hash the same, so the tree names one block twice.
twinLeaves :: (Hash HbSync, Blocks)
twinLeaves = (nodeH, Map.fromList [(leafH, leafBs), (nodeH, nodeBs)])
  where
    leafBs = serialise (MLeaf [HashRef (hashObject @HbSync (LBS.pack [1,2,3]))] :: MTree [HashRef])
    leafH  = hashObject leafBs :: Hash HbSync
    nodeBs = serialise (newMNode 1 [leafH, leafH] :: MTree [HashRef])
    nodeH  = hashObject nodeBs :: Hash HbSync

countVisits :: (Hash HbSync
                -> (Hash HbSync -> IO (Maybe LBS.ByteString))
                -> (Either (Hash HbSync) (MTree [HashRef]) -> IO ())
                -> IO ())
            -> (Hash HbSync, Blocks)
            -> IO Int
countVisits walk (root, m) = do
  cnt <- newIORef (0 :: Int)
  walk root (\h -> pure (Map.lookup h m)) (\_ -> modifyIORef' cnt (+ 1))
  readIORef cnt

-- | The unique walk enters each node at most once, whatever the fan-in.
testMerkleWalkBounded :: IO ()
testMerkleWalkBounded = do
  -- Depth 20 is 2097151 visits for the unbounded walk and two seconds; anything
  -- that finishes here in a bounded number of visits has stopped re-entering.
  let b@(_, blocks) = bomb 20
  n <- countVisits (walkMerkleUnique' @[HashRef]) b
  assertBool ("visited " <> show n <> " nodes for " <> show (Map.size blocks) <> " blocks")
    (n <= Map.size blocks)

  -- And the walk is not merely cheap, it is complete: every block is entered.
  n @?= Map.size blocks

  -- The shape scales the way the fix claims: twice the depth is not four times
  -- the work. Depth 40 is 2^40 for the unbounded walk, which is why this case
  -- cannot be written for it at all.
  let b2@(_, blocks2) = bomb 40
  n2 <- countVisits (walkMerkleUnique' @[HashRef]) b2
  n2 @?= Map.size blocks2

-- | The plain walk still emits a repeated leaf once per occurrence.
--
-- This is not a wart being pinned, it is the property 'readFromMerkle' stands
-- on: it concatenates leaf payloads in traversal order, and a file with two
-- identical runs of content genuinely has one leaf block named twice. A visited
-- set there would return a shorter file, silently.
testMerkleWalkKeepsDuplicates :: IO ()
testMerkleWalkKeepsDuplicates = do
  leaves <- newIORef ([] :: [[HashRef]])
  let (root, m) = twinLeaves
  walkMerkle @[HashRef] root (\h -> pure (Map.lookup h m)) $ \case
    Left _   -> assertFailure "no block should be missing here"
    Right hs -> modifyIORef' leaves (hs :)

  got <- readIORef leaves
  length got @?= 2
  assertBool "both occurrences carry the same payload"
    (length (filter (== head got) got) == 2)

  -- The same tree through the unique walk yields it once, which is the whole
  -- difference between the two and the reason both exist.
  leaves2 <- newIORef ([] :: [[HashRef]])
  walkMerkleUnique @[HashRef] root (\h -> pure (Map.lookup h m)) $ \case
    Left _   -> assertFailure "no block should be missing here"
    Right hs -> modifyIORef' leaves2 (hs :)

  got2 <- readIORef leaves2
  length got2 @?= 1

  -- A missing block is still reported by both, rather than being swallowed as
  -- "already seen".
  misses <- newIORef ([] :: [Hash HbSync])
  walkMerkleUnique @[HashRef] root (const (pure Nothing)) $ \case
    Left h  -> modifyIORef' misses (h :)
    Right _ -> assertFailure "nothing should be readable here"
  readIORef misses >>= \ms -> length ms @?= 1
