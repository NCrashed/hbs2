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
--
-- The groups below it are the rule that replaces it: the requester now judges a
-- status by whether it echoes a nonce the requester itself issued, for the
-- mailbox it issued it about. The clock window survives as a fallback for TWO
-- reasons, and only the first is transitional -- a responder on an older build
-- echoes nothing, and @mailboxSetPolicy@ gossips a status nobody asked for, for
-- which no nonce exists at any protocol version. Whoever comes to delete the
-- fallback has to answer the second one.
module MailboxStatus (mailboxStatusTests) where

import HBS2.Clock (getPOSIXTime)
import HBS2.Peer.Proto.Mailbox.Types (clockSkew)
import HBS2.Peer.Proto.Mailbox.Nonce

import Control.Monad (replicateM)
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HM
import Data.List (nub)
import Data.Word (Word64)

import Test.Tasty
import Test.Tasty.HUnit

-- The window the handler compares against. Not exported by the handler, so it
-- is written down again here; what the tests below pin is the SKEW, and this is
-- only used to say which side of the boundary a skew falls on.
window :: Word64
window = 10

mailboxStatusTests :: TestTree
mailboxStatusTests = testGroup "mailbox status"
  [ mailboxStatusClockTests
  , mailboxStatusNonceTests
  , mailboxStatusStoreTests
  , mailboxStatusUseTests
  ]

-- | The store as it is actually used, clock and generator included.
--
-- The group below this one tests the decision; this one tests that the peer
-- writes down what it asked. Without it 'issueCheckNonce' could stop recording
-- the nonce, or record it under another key, and every other test stays green
-- while the peer silently refuses every status it gets -- on precisely the path
-- this change exists to fix.
mailboxStatusStoreTests :: TestTree
mailboxStatusStoreTests = testGroup "mailbox status nonce store"
  [ testCase "a nonce we issued comes back accepted, and only for its mailbox" $ do
      ns <- newCheckNonces :: IO (CheckNonces String)
      n  <- issueCheckNonce ns "boxA"
      issued <- acceptCheckNonce ns "boxA" n
      assertBool "the nonce we just issued is accepted" issued
      elsewhere <- acceptCheckNonce ns "boxB" n
      assertBool "the same nonce for another mailbox is refused" (not elsewhere)
      invented <- acceptCheckNonce ns "boxA" (n + 1)
      assertBool "a nonce we did not issue is refused" (not invented)

  , testCase "two requests for one mailbox do not evict each other" $ do
      -- mailboxFetchQ and mailboxCheckQ can both have a request outstanding for
      -- the same mailbox. "One nonce per mailbox, replaced on insert" is the
      -- natural thing to write while bounding the map, and it would silently
      -- drop the answer to whichever request was older.
      ns <- newCheckNonces :: IO (CheckNonces String)
      a  <- issueCheckNonce ns "boxA"
      b  <- issueCheckNonce ns "boxA"
      assertBool "two draws differ" (a /= b)
      first'  <- acceptCheckNonce ns "boxA" a
      second' <- acceptCheckNonce ns "boxA" b
      assertBool "the older request is still answerable" first'
      assertBool "the newer request is answerable"       second'

  , testCase "a nonce is drawn, not read off the clock" $ do
      -- The property the whole change rests on, and nothing else asserts it: a
      -- revert to the `getPOSIXTime <&> round` this replaced would pass every
      -- other test in this file.
      ns  <- newCheckNonces :: IO (CheckNonces String)
      now <- getPOSIXTime <&> round
      nonces <- replicateM 16 (issueCheckNonce ns "boxA")
      assertBool "sixteen draws are sixteen values" (length (nub nonces) == 16)
      assertBool "none of them is a plausible clock reading"
        (all (\n -> clockSkew now n > 1000000) nonces)

  , testCase "the TTL is the number the changelog and the draft both name" $
      -- Every other TTL assertion is written in terms of checkNonceTTL, so
      -- setting it to 5 would leave them all green and two documents wrong.
      checkNonceTTL @?= 60
  ]

-- | The accept-either rule the handler applies, as one function.
--
-- THE RULE THIS TESTED IS GONE. It was @nonceOk || clockSkew now nonce < 10@,
-- and the second half made the first decide nothing: the timestamp is written
-- by whoever sends the status. What the group asserted, therefore, was that a
-- forged status is admitted -- correctly, since that was the code.
--
-- Freshness is now the nonce store alone, which the group below has always
-- tested. Nothing replaces this one: there is no second rule left to combine.
--
-- The property it was really guarding -- that the split in 9c3822af APPLIES --
-- is not assertable here, because it lives at the call site in the accept path
-- and this suite has no harness for that. Recorded rather than quietly dropped:
-- 'statusUse' says what an origin entitles a peer to, and nothing in this suite
-- says which origin a real status gets.

-- | The nonce store, which is what freshness now means.
--
-- Written over 'String' keys rather than a 'MailboxRefKey': the store is
-- polymorphic in the key precisely so that the decision can be asked questions
-- without standing up a peer, a mailbox or a signing key.
mailboxStatusNonceTests :: TestTree
mailboxStatusNonceTests = testGroup "mailbox status nonce"
  [ testCase "a nonce we issued for this mailbox is fresh" $ do
      let m = insertCheckNonce 1000 "boxA" 7777 HM.empty
      assertBool "our own nonce comes back" (lookupCheckNonce 1000 "boxA" 7777 m)

  , testCase "a nonce we never issued is not fresh" $ do
      let m = insertCheckNonce 1000 "boxA" 7777 HM.empty
      assertBool "an invented nonce is refused"
        (not (lookupCheckNonce 1000 "boxA" 7778 m))
      assertBool "an empty store accepts nothing"
        (not (lookupCheckNonce 1000 "boxA" 7777 HM.empty))

  , testCase "a nonce is bound to the mailbox it was issued for" $ do
      -- THE POINT OF KEYING BY MAILBOX. CheckMailbox goes out by gossip, so
      -- every connected peer sees the nonce; without this a peer could take the
      -- nonce it was shown for one mailbox and answer with a status for another.
      let m = insertCheckNonce 1000 "boxA" 7777 HM.empty
      assertBool "the same nonce for another mailbox is refused"
        (not (lookupCheckNonce 1000 "boxB" 7777 m))

  , testCase "a nonce is not consumed by the peer that answers first" $ do
      -- A CheckMailbox is gossiped and every peer holding the mailbox may
      -- answer. Taking the first answer and refusing the rest would throw away
      -- the reason to ask more than one.
      let m = insertCheckNonce 1000 "boxA" 7777 HM.empty
      assertBool "first answer"  (lookupCheckNonce 1001 "boxA" 7777 m)
      assertBool "second answer" (lookupCheckNonce 1002 "boxA" 7777 m)
      assertBool "third answer"  (lookupCheckNonce 1003 "boxA" 7777 m)

  , testCase "a nonce expires, and the boundary is where the TTL says" $ do
      let m = insertCheckNonce 1000 "boxA" 7777 HM.empty
      assertBool "one second later"
        (lookupCheckNonce 1001 "boxA" 7777 m)
      assertBool "a second short of the TTL"
        (lookupCheckNonce (1000 + checkNonceTTL - 1) "boxA" 7777 m)
      assertBool "exactly the TTL is already too old"
        (not (lookupCheckNonce (1000 + checkNonceTTL) "boxA" 7777 m))
      assertBool "well past the TTL"
        (not (lookupCheckNonce (1000 + 2 * checkNonceTTL) "boxA" 7777 m))

  , testCase "a clock that jumps backwards expires nonces rather than freezing them" $ do
      -- Distance, not difference, for the reason clockSkew exists: an entry
      -- stamped in what is now the future must age out like any other instead of
      -- staying valid until the clock catches up.
      let m = insertCheckNonce 100000 "boxA" 7777 HM.empty
      assertBool "issued far in the future is not fresh now"
        (not (lookupCheckNonce 1000 "boxA" 7777 m))
      assertBool "and nothing wraps at the bottom of the range"
        (not (lookupCheckNonce 0 "boxA" 7777 m))

  , testCase "inserting prunes what has expired" $ do
      -- What keeps the map bounded without a sweeper.
      let m0 = insertCheckNonce 1000 "boxA" 1 HM.empty
          m1 = insertCheckNonce 1001 "boxB" 2 m0
          m2 = insertCheckNonce (1000 + checkNonceTTL + 1) "boxC" 3 m1
      HM.size m0 @?= 1
      HM.size m1 @?= 2
      HM.size m2 @?= 1
      assertBool "the survivor is the one just added"
        (HM.member ("boxC", 3) m2)

  , testCase "pruning keeps what is still live" $ do
      let m0 = insertCheckNonce 1000 "boxA" 1 HM.empty
          m1 = insertCheckNonce 1000 "boxB" 2 m0
      HM.size (pruneCheckNonces 1001 m1) @?= 2
      HM.size (pruneCheckNonces (1000 + checkNonceTTL) m1) @?= 0

  , testCase "pruning drops an entry stamped in the future" $ do
      -- The lookup side of a backwards clock jump is covered above; this is the
      -- prune side, and prune is what bounds the map. An entry that prune keeps
      -- forever is a leak, not just a stale accept.
      let m = HM.singleton ("boxA" :: String, 7777 :: Word64) 100000
      HM.size (pruneCheckNonces 1000 m) @?= 0
      HM.size (pruneCheckNonces 100000 m) @?= 1
  ]

mailboxStatusClockTests :: TestTree
mailboxStatusClockTests = testGroup "mailbox status freshness"
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

-- | The two halves of a status, and which of them a stranger can move.
--
-- The reason the clock window could not simply be deleted was that
-- @mailboxSetPolicy@ gossips a status NOBODY ASKED FOR, so a new policy would
-- reach nobody without it. That was one decision doing two jobs: the policy
-- inside a status signs for itself and is admitted only at a greater version,
-- while the tree it names is a hash somebody announced and acting on it means
-- fetching whatever is under it.
--
-- Split, the broadcast keeps working with no window at all, and the window is
-- left with exactly one user: the answer of a peer on an older build. That is a
-- transition with an end, rather than a hole with a reason.
mailboxStatusUseTests :: TestTree
mailboxStatusUseTests = testGroup "mailbox status: what it entitles a peer to"
  [ testCase "an answer of ours moves both the tree and the policy" $ do
      useTree (statusUse StatusAnswered) @?= True
      usePolicy (statusUse StatusAnswered) @?= True

  -- The half that used to be lost with the whole status, and the reason the
  -- fallback existed.
  , testCase "a status nobody asked for still carries its policy" $
      usePolicy (statusUse StatusUnasked) @?= True

  -- And the half that must not travel unasked: a hash a stranger announced,
  -- whose blocks this peer would fetch and merge (see the poisoning note in
  -- the accept path).
  , testCase "a status nobody asked for does not move the tree" $
      useTree (statusUse StatusUnasked) @?= False
  ]
