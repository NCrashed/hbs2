-- | Which ping nonces this peer signs.
--
-- The handshake signs the nonce it is pinged with, and since 494fbc63 only a
-- nonce of 'pingNonceSize' bytes. That number was written as 32, read off the
-- @BS.take 32@ in the HasNonces instance, while the instance hands out a
-- crypto_box nonce, which is 24 bytes, and take does not lengthen it. So a peer
-- with the check refused every ping it got, from old peers and from its own
-- kind, and ended up with no peers at all.
--
-- These cases ask the real instance what it produces and the real rule what it
-- accepts, so the two cannot drift apart again without a red test.
module PingNonceSpec (tests) where

import HBS2.Net.Proto.Types
import HBS2.Peer.Proto

import Control.Monad
import Data.ByteString qualified as BS

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "ping nonce"
  [ testCase "what newNonce produces is what the handshake signs" do
      ns <- replicateM 64 (newNonce @(PeerHandshake L4Proto))
      forM_ ns $ \n -> do
        BS.length n @?= pingNonceSize
        assertBool "own nonce refused" (pingNonceSignable n)

  , testCase "a released peer's nonce is 24 bytes and is signed" do
      -- Every build since 2023 sends 24 bytes; 0.25.4 and 0.25.5 included.
      pingNonceSize @?= 24
      assertBool "24-byte nonce refused" (pingNonceSignable (BS.replicate 24 0x5a))

  , testCase "any other length is refused" do
      forM_ [0, 1, 23, 25, 32, 33, 64, 4096] $ \k ->
        assertBool ("signed a nonce of length " <> show k)
                   (not (pingNonceSignable (BS.replicate k 0x5a)))
  ]
