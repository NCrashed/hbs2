{-# Language AllowAmbiguousTypes #-}
-- | What may enter the mailbox input queue (PEP-21 "Proof-of-work").
--
-- The queue is bounded and drained on a timer, and until this module existed
-- the only thing checked at its door was a dedup of identical copies in flight.
-- Everything else -- the signature, the policy, the work -- was checked ten
-- seconds later by the drain. So a flood of DISTINCT messages, each carrying no
-- work at all, took a slot apiece for a mailbox that charges @(pow D)@, and the
-- honest submissions behind them were dropped. Bounded in SIZE (nothing reaches
-- disk) and free to send, which is the one thing proof-of-work exists to stop.
--
-- Two decisions live here, and both are pure so that a test can ask about them
-- without a peer, a mailbox or a policy:
--
--   * whether the work a message carries pays for a slot, and
--   * whether one sender may hold as much of the queue as they are asking for.
--
-- The second is not about work and applies to every mailbox, including the many
-- that charge nothing: it bounds STARVATION rather than cost, which is the half
-- a proof-of-work floor cannot reach.
module MailboxQueue
  ( QueueRefusal(..)
  , paidFor
  , admitTo
  , inQueueDepth
  , perPeerShare
  ) where

import HBS2.Peer.Proto.Mailbox.Types (PoWDifficulty)

-- | Why a message did not get a slot.
--
-- Three answers rather than one boolean, because they are three different
-- events for whoever reads the log: the queue is full (this peer is behind),
-- one sender is monopolising it (somebody is flooding), and the work does not
-- pay (somebody is flooding cheaply, which is the case @(pow D)@ is for).
data QueueRefusal =
    QueueFull
  | QueueShare
  | QueueUnpaid
  deriving stock (Eq,Show)

-- | How deep the input queue is.
--
-- Named rather than left as a literal at the one construction site, because
-- 'perPeerShare' is a fraction of it and a bound expressed against a number
-- somebody has to go and look up is a bound that drifts.
inQueueDepth :: Int
inQueueDepth = 8000

-- | The most one peer may hold at once.
--
-- An eighth, which is a judgement and not a derivation: it leaves seven eighths
-- for everybody else while one peer floods, and it is far above what an honest
-- neighbour relaying a burst of legitimate traffic will ever hold in the ten
-- seconds between drains.
--
-- It bounds ONE peer. A flood from eight of them fills the queue again, and
-- that is a different attack with a different answer (the peer layer decides
-- who may talk to this node at all); this is not pretended to be a defence
-- against it.
perPeerShare :: Int
perPeerShare = inQueueDepth `div` 8

-- | Does the work this message carries pay for a slot?
--
-- FAILS OPEN, deliberately and in three places, because this check runs on a
-- cache and the authoritative one runs in the drain with the signed policy in
-- hand. A message let through here costs a slot and is then refused properly; a
-- message refused here is refused on stale information, and that is the error
-- worth avoiding.
--
--   * a recipient this peer knows nothing about pays (it may not even be a
--     mailbox this peer holds, and the drain will say so);
--   * a recipient charging zero pays, which is every mailbox that has not asked
--     for work;
--   * a message naming no recipient at all pays, since refusing it here would
--     be answering a question ("who is this for") that it does not ask.
--
-- ANY and not ALL: one message names several mailboxes, and a letter to a
-- charging inbox and a free one is paid for as far as the free one is
-- concerned. The drain decides each recipient separately, as it always did.
paidFor :: forall k .
           (k -> Maybe PoWDifficulty)     -- ^ what this peer believes each charges
        -> (PoWDifficulty -> k -> Bool)   -- ^ does the stamp satisfy that mailbox
        -> [k]                            -- ^ the recipients the message names
        -> Bool
paidFor known ok rs
  | null rs = True
  | otherwise = any pays rs
  where
    pays r = case known r of
      Nothing -> True
      Just d | d == 0    -> True
             | otherwise -> ok d r

-- | May this message have a slot?
--
-- The order of the three refusals is the order of what they say about the
-- world: a full queue is about this peer, a share taken is about one sender,
-- and unpaid work is about one message. Reporting the innermost when the
-- outermost is also true would send an operator looking at the wrong thing.
admitTo :: Int          -- ^ how many are queued now
        -> Maybe Int    -- ^ how many this sender holds, or Nothing for a local send
        -> Bool         -- ^ 'paidFor'
        -> Either QueueRefusal ()
admitTo queued held paid
  | queued >= inQueueDepth = Left QueueFull
  -- Nothing is this node's own submission, over its own RPC. It is not
  -- somebody else's traffic and is not rationed against it.
  | maybe False (>= perPeerShare) held = Left QueueShare
  | not paid = Left QueueUnpaid
  | otherwise = Right ()
