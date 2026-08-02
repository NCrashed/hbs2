-- | Tests for the freshness rule a peer's mailbox status is judged by.
--
-- Pure, like 'MailboxMerge', and for the same reason: the rule lived as one
-- expression inside a 'ContT' inside the wire handler, where nothing could ask
-- it anything, and it was wrong for as long as it lived there.
--
-- What it got wrong is worth stating, because the expression LOOKED right.
-- @abs (now - nonce) < 10@ reads as a ten-second window either side. Both
-- operands are 'Word64': on an unsigned type 'abs' is the identity and the
-- subtraction wraps, so the window was ten seconds in ONE direction. A
-- responder whose clock was a single second ahead produced a difference of
-- about 2^64 and had its status dropped by @exit ()@, which logs nothing. Two
-- honest peers with their clocks off in that direction never synchronised a
-- mailbox and nothing anywhere said why.
module MailboxStatus (mailboxStatusTests) where

import HBS2.Peer.Proto.Mailbox.Types (clockSkew)

import Data.Word (Word64)

import Test.Tasty
import Test.Tasty.HUnit

-- The window the handler compares against. Not exported by the handler, so it
-- is written down again here; what the tests below pin is the SKEW, and this is
-- only used to say which side of the boundary a skew falls on.
window :: Word64
window = 10

mailboxStatusTests :: TestTree
mailboxStatusTests = testGroup "mailbox status freshness"
  [ testCase "a responder whose clock is ahead is still fresh" $ do
      -- THE REGRESSION. Before the fix this was 18446744073709551615 and the
      -- status was discarded; a peer one second ahead of us is the ordinary
      -- state of two machines, not an attack.
      clockSkew 1000 1001 @?= 1
      assertBool "one second ahead is inside the window"
        (clockSkew 1000 1001 < window)

      -- And the far side of the boundary, so that "fresh" does not now mean
      -- "always": eleven seconds ahead is still refused.
      assertBool "eleven seconds ahead is outside the window"
        (not (clockSkew 1000 1011 < window))

  , testCase "a responder whose clock is behind is judged the same way" $ do
      -- The direction that already worked, kept so that the fix is not a swap
      -- of which half of the window is broken.
      clockSkew 1001 1000 @?= 1
      assertBool "one second behind is inside the window"
        (clockSkew 1001 1000 < window)
      assertBool "eleven seconds behind is outside the window"
        (not (clockSkew 1011 1000 < window))

  , testCase "the boundary is where the window says it is" $ do
      -- Nine in, ten out, from both sides. The comparison is strict, so a skew
      -- of exactly the window is refused.
      assertBool "nine ahead"  (clockSkew 1000 1009 < window)
      assertBool "nine behind" (clockSkew 1009 1000 < window)
      assertBool "ten ahead"   (not (clockSkew 1000 1010 < window))
      assertBool "ten behind"  (not (clockSkew 1010 1000 < window))

  , testCase "identical clocks are zero apart" $
      clockSkew 1000 1000 @?= 0

  , testCase "the answer does not depend on which argument is which" $ do
      -- Symmetry is the property that makes the call site impossible to get
      -- wrong by writing the arguments the other way round, which is half the
      -- reason this is a function at all.
      let pairs = [ (0, 0), (0, 1), (1, 0), (7, 9000)
                  , (maxBound, 0), (maxBound, maxBound)
                  , (maxBound - 1, maxBound)
                  ] :: [(Word64, Word64)]
      mapM_ (\(a,b) -> clockSkew a b @?= clockSkew b a) pairs

  , testCase "nothing wraps at the ends of the range" $ do
      -- The shape of the original defect, asserted directly: a difference that
      -- spans the whole range must come back as the whole range and not as a
      -- small number that would pass the window.
      clockSkew 0 maxBound @?= maxBound
      clockSkew maxBound 0 @?= maxBound
      assertBool "the extreme is not mistaken for fresh"
        (not (clockSkew 0 maxBound < window))
      clockSkew maxBound (maxBound - 1) @?= 1
  ]
