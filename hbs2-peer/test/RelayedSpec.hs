-- | What this peer has already put back on the wire.
--
-- The rule is pure, so these ask it directly rather than through a peer. What
-- they pin is the three things the gossip path depends on: a message is
-- forwarded once, a different message is a different fact, and the memory
-- cannot grow without bound.
--
-- The last one is why this replaced a block. Four protocols gossip and all
-- four asked the block store; the old marker was a block whose
-- address a stranger could compute and whose bytes a stranger could serve, so
-- "already forwarded" was writable by whoever wanted a letter stopped; and it
-- was never collected, so the store grew by one block per distinct message
-- forever.
--
-- The generation size is passed in rather than taken from 'relayGeneration',
-- which is what lets the rollover be watched with eight hashes instead of
-- sixty-five thousand. The real constant is exercised by one case below, so
-- that a value the peer could not actually hold would still be caught.
module RelayedSpec (tests) where

import HBS2.Peer.Proto.Relayed

import HBS2.Hash
import HBS2.Data.Types.Refs (HashRef(..))

import Data.ByteString.Char8 qualified as B8
import Data.HashSet qualified as HS
import Data.List (foldl')

import Test.Tasty
import Test.Tasty.HUnit

mh :: Int -> HashRef
mh = HashRef . hashObject . B8.pack . show

-- | A generation small enough to watch, and not 1 or 2, so that "full" and
-- "empty" are not the same state.
gen :: Int
gen = 8

-- | Offer a run of distinct hashes, keeping the answers.
offer :: [Int] -> RelayMemory -> ([Bool], RelayMemory)
offer hs m0 = let (acc, m) = foldl' step ([], m0) hs in (reverse acc, m)
  where
    step (acc, m) i = let (fresh, m') = rememberRelayed gen (mh i) m in (fresh : acc, m')

tests :: TestTree
tests = testGroup "mailbox: what this peer has already relayed"
  [ testCase "a message nobody offered is fresh" $ do
      fst (rememberRelayed gen (mh 1) emptyRelayMemory) @?= True

  , testCase "the same message twice is forwarded once" $ do
      -- The property gossip needs. Without it a message goes round the graph
      -- for as long as the graph has cycles.
      let (fresh, m) = rememberRelayed gen (mh 1) emptyRelayMemory
      fresh @?= True
      fst (rememberRelayed gen (mh 1) m) @?= False

  , testCase "two messages are two facts" $ do
      let (_, m) = rememberRelayed gen (mh 1) emptyRelayMemory
      fst (rememberRelayed gen (mh 2) m) @?= True

  , testCase "a run of distinct messages is forwarded once each" $ do
      let (answers, _) = offer [1 .. gen] emptyRelayMemory
      answers @?= replicate gen True
      -- And the same run again is forwarded none.
      let (_, m) = offer [1 .. gen] emptyRelayMemory
          (again, _) = offer [1 .. gen] m
      again @?= replicate gen False

  , testCase "the memory is bounded by count" $ do
      -- The half that makes this not a leak. Offer four generations' worth of
      -- distinct messages and assert neither half has grown past one.
      let (_, m) = offer [1 .. 4 * gen + 3] emptyRelayMemory
      assertBool "young generation is bounded" (HS.size (relayYoung m) <= gen)
      assertBool "old generation is bounded"   (HS.size (relayOld m) <= gen)

  , testCase "suppression survives at least one generation" $ do
      -- What the bound costs, stated as what it still guarantees: a message
      -- offered now is still suppressed after a whole generation of others has
      -- gone by, which is far more than one propagation.
      let (_, m0) = rememberRelayed gen (mh 0) emptyRelayMemory
          (_, m1) = offer [1 .. gen] m0
      fst (rememberRelayed gen (mh 0) m1) @?= False

  , testCase "an old enough message is offered again" $ do
      -- Honest about what was traded. After two generations the memory has let
      -- go, and re-forwarding is what happens. It buys a flooder nothing:
      -- re-sending a message they hold costs them what a new one costs.
      let (_, m0) = rememberRelayed gen (mh 0) emptyRelayMemory
          (_, m1) = offer [1 .. 2 * gen + 1] m0
      fst (rememberRelayed gen (mh 0) m1) @?= True

  , testCase "the generation the peer actually uses is a usable one" $ do
      -- The one case that touches 'relayGeneration'. It asserts nothing about
      -- the number beyond it being a size at which the structure still behaves
      -- -- which is what a 0 or a negative would break, silently, by making
      -- every message fresh and gossip loop forever.
      assertBool "a generation holds something" (relayGeneration > 0)
      let (_, m) = rememberRelayed relayGeneration (mh 1) emptyRelayMemory
      fst (rememberRelayed relayGeneration (mh 1) m) @?= False
  ]
