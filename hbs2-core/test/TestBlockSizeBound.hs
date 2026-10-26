-- | What a declared block size costs the peer that believes it.
--
-- A download starts by asking a peer how big a block is, and the answer is an
-- 'Integer' off the wire. The requester turns it into 'calcChunks' and takes
-- that list's length, so the declared size is a multiplier on work this node
-- does before a single byte of the block has been transferred.
--
-- These are arithmetic, deliberately: the gate that uses the bound lives inside
-- a protocol handler with no harness, and what a test can pin is the thing the
-- gate's number is chosen against. Pinning it here is what stops the ceiling
-- from being raised later without anybody noticing what it buys.
module TestBlockSizeBound
  ( testBlockSizeCeilingIsAboveAnythingWritten
  , testBlockSizeCeilingBoundsTheWork
  ) where

import HBS2.Defaults
import HBS2.Storage (calcChunks)

import Test.Tasty.HUnit

-- | The ceiling has to admit every block this software could have produced.
--
-- Pinned rather than assumed, because the two numbers are in one file and a
-- writer that chunks larger would otherwise make its own blocks undownloadable
-- -- which reads as a network fault and is a one-line arithmetic mistake.
testBlockSizeCeilingIsAboveAnythingWritten :: IO ()
testBlockSizeCeilingIsAboveAnythingWritten = do
  assertBool "the ceiling admits a block this build writes"
    (defMaxBlockSize > defBlockSize)
  -- And by a margin, which is the difference between a bound and a coincidence:
  -- a ceiling one byte above the writer's chunk size would be met by the first
  -- build that chose differently.
  assertBool "and with room for a writer that chunks larger"
    (defMaxBlockSize >= 16 * defBlockSize)

-- | And it has to keep the work finite.
--
-- The number that matters is not the byte count -- nothing is transferred yet
-- -- but the CHUNK count, which is the length the requester walks. At the
-- ceiling it is sixteen thousand; without one, a peer answering 2^40 asks for
-- about a billion.
testBlockSizeCeilingBoundsTheWork :: IO ()
testBlockSizeCeilingBoundsTheWork = do
  let chunksAt n = length (calcChunks n defChunkSize :: [(Int,Int)])

  assertBool "a block at the ceiling is a few thousand chunks"
    (chunksAt defMaxBlockSize <= 65536)

  -- What the bound removes, stated as the number it removes. Divided, not
  -- walked: walking that list is the work the bound exists to refuse, and it
  -- takes this test suite tens of seconds.
  assertBool "and the size a stranger could have named is not"
    ((2 ^ (40 :: Int)) `div` defChunkSize > 100 * toInteger (chunksAt defMaxBlockSize))
