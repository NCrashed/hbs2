-- | What may enter the mailbox input queue (PEP-21 "Proof-of-work").
--
-- The two decisions at the door, tested apart from the peer that makes them.
-- Both exist for the same failure: the queue is bounded and drained on a timer,
-- so anything that can take slots for free can starve everything behind it. One
-- of them charges work where a mailbox asks for it; the other bounds one
-- sender's share whether or not any work is asked for, which is the case that
-- covers the mailboxes that charge nothing -- i.e. nearly all of them.
module MailboxQueueSpec (tests) where

import MailboxQueue

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "mailbox: what gets a queue slot"
  [ testGroup "the work that pays for it"
      [ testCase "a mailbox that charges nothing is paid for by anything" $ do
          paidFor (const (Just 0)) (\_ _ -> False) ["a"] @?= True

      , testCase "a mailbox that charges is not" $ do
          paidFor (const (Just 12)) (\_ _ -> False) ["a"] @?= False
          paidFor (const (Just 12)) (\_ _ -> True) ["a"] @?= True

      -- FAILS OPEN, and this is the property that keeps the cache honest: the
      -- authority is the drain, which reads the signed policy. A message let
      -- through costs a slot and is then refused properly; one refused here is
      -- refused on stale information.
      , testCase "a mailbox this peer knows nothing about is paid for" $ do
          paidFor (const Nothing) (\_ _ -> False) ["a"] @?= True

      , testCase "a message naming nobody is not refused for want of work" $ do
          paidFor (const (Just 12)) (\_ _ -> False) ([] :: [String]) @?= True

      -- ANY and not ALL: a letter to a charging inbox and a free one is paid
      -- for as far as the free one is concerned, and the drain still decides
      -- each recipient separately.
      , testCase "one paid recipient out of two buys the slot" $ do
          let charges r = if r == "paid" then Just 0 else Just 12
          paidFor charges (\_ _ -> False) ["free", "paid"] @?= True
          paidFor charges (\_ _ -> False) ["free", "other"] @?= False

      -- The whole point, said as the case it exists for: distinct messages
      -- carrying no work, to a mailbox that charges, do not get in.
      , testCase "an unstamped flood at a charging mailbox pays for nothing" $ do
          let unstamped = \_ _ -> False
          all (\n -> not (paidFor (const (Just 20)) unstamped [show n])) [1 .. 100 :: Int]
            @?= True
      ]

  , testGroup "the share one sender may hold"
      [ testCase "an empty queue takes anything paid for" $
          admitTo 0 (Just 0) True @?= Right ()

      , testCase "a full queue takes nothing" $
          admitTo inQueueDepth (Just 0) True @?= Left QueueFull

      , testCase "a sender at its share is refused while the queue has room" $ do
          admitTo (perPeerShare + 1) (Just perPeerShare) True @?= Left QueueShare
          admitTo (perPeerShare + 1) (Just (perPeerShare - 1)) True @?= Right ()

      -- The share is what leaves room for everybody else while one peer floods,
      -- and it has to be a real fraction of the queue rather than a token.
      , testCase "one sender cannot hold the whole queue" $
          perPeerShare < inQueueDepth @?= True

      -- This node's own RPC submission is not somebody else's traffic.
      , testCase "a local send is not rationed against a share" $
          admitTo (perPeerShare + 1) Nothing True @?= Right ()

      , testCase "unpaid work is refused even with room and no share taken" $
          admitTo 0 (Just 0) False @?= Left QueueUnpaid

      -- The order of the refusals is the order of what they say about the
      -- world: a full queue is about this peer, a share is about one sender,
      -- and unpaid work is about one message. Reporting the innermost when the
      -- outermost is also true sends an operator to the wrong place.
      , testCase "reports the outermost reason when several are true" $ do
          admitTo inQueueDepth (Just perPeerShare) False @?= Left QueueFull
          admitTo 0 (Just perPeerShare) False @?= Left QueueShare
      ]

  , testGroup "the copy already in flight"
      [ -- The dedup this replaced, and the half it still is: a repeat of what
        -- is queued costs nothing.
        testCase "a message nobody has queued takes a slot" $ do
          takesASlot Nothing False @?= True
          takesASlot Nothing True  @?= True

      , testCase "a repeat of a queued copy takes none" $ do
          takesASlot (Just False) False @?= False
          takesASlot (Just True)  True  @?= False

      -- THE ONE THIS EXISTS FOR. A stamp is not part of the message it pays
      -- for, so a stripped copy and the paid one hash alike. Anybody who has
      -- seen a paid letter on gossip can re-send it stripped, land the plain
      -- copy first in each drain window, and have the honest one deduped away
      -- -- after which the drain refuses the queued copy for want of work.
      , testCase "a paid copy is admitted beside an unpaid one already queued" $ do
          takesASlot (Just False) True @?= True

      -- And only once: after the upgrade the map says paid, so the next copy
      -- is a repeat like any other. One extra slot per message per batch is
      -- the whole cost.
      , testCase "an unpaid copy does not displace a paid one" $ do
          takesASlot (Just True) False @?= False
      ]
  ]
