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
  , paidRecipients
  , heldBy
  , admitTo
  , takesASlot
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
  -- NOTHING HERE TO CHARGE FOR. This peer holds a policy for none of them, so
  -- it has no opinion to enforce and the drain will say what it says. Failing
  -- open here is what keeps a cold peer, and a relay that holds none of these
  -- mailboxes, behaving as they did.
  | null (heldBy known rs) = True
  | otherwise = not (null (paidRecipients known ok rs))

-- | The recipients of this message that this peer holds a policy for.
--
-- SEPARATE FROM THE FAIL-OPEN, because the two used to be the same answer and
-- that was the hole: `any pays` over the raw list treated an UNKNOWN recipient
-- as a payment, so a letter naming one charging mailbox and one key nobody has
-- ever heard of read as paid with no stamp at all. Padding the recipient list
-- was free, and the slot is what the work is supposed to buy.
--
-- An unknown recipient is not a payment and is not a refusal either: it is a
-- recipient this peer has no policy for, which the drain will answer for.
heldBy :: (k -> Maybe PoWDifficulty) -> [k] -> [(k, PoWDifficulty)]
heldBy known rs = [ (r, d) | r <- rs, Just d <- [known r] ]

-- | Which of them this copy actually pays for.
--
-- WHICH AND NOT WHETHER, because a copy's value is per recipient: one stamp
-- pays for one mailbox (PEP-21), and a letter to two charging mailboxes is two
-- copies of one message carrying two stamps. Answering yes-or-no made those two
-- copies indistinguishable to the queue, so the second was deduped away as a
-- repeat and its mailbox never got the letter. See 'takesASlot'.
paidRecipients :: (k -> Maybe PoWDifficulty)
               -> (PoWDifficulty -> k -> Bool)
               -> [k]
               -> [k]
paidRecipients known ok rs =
  [ r | (r, d) <- heldBy known rs, d == 0 || ok d r ]

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

-- | Does this copy get a slot, given what is already in flight for the same
-- message?
--
-- The third decision at the door, and the one that used to be a plain "have we
-- seen this hash". A stamp is deliberately NOT part of the message it pays for
-- -- it cannot be, since the sender signs the message and then grinds -- so a
-- stamped copy and a stripped one hash alike, and the queue keeps whichever
-- arrived first.
--
-- That made a paid letter suppressible by anyone who had seen it: re-send it
-- stripped, land the plain copy first in each ten-second window, have the
-- honest stamped one deduped away as a repeat, and let the drain refuse the
-- queued copy for want of work. The sender paid 2^D hashes and gets no
-- rejection to look at.
--
-- So: a copy that PAYS displaces nothing but is admitted once beside a copy
-- that does not. One extra slot, once per message per batch -- after it the
-- map says paid and every further copy is free again. Everything else is a
-- repeat and costs nothing, which is what the dedup is for.
-- WHICH RECIPIENTS, and not whether it pays. A stamp pays for ONE mailbox
-- (PEP-21), so a letter to two charging mailboxes is two copies of one message
-- carrying two stamps -- and `hbs2-hub` sends exactly that. Under a yes-or-no
-- map the first copy recorded "paid" and the second was a repeat, so whichever
-- stamp arrived first was the only mailbox that ever got the letter. No
-- attacker needed for that one: it is what this tool's own sender does.
--
-- Keyed on the set instead, a copy is admitted when it pays for a recipient no
-- queued copy has paid for yet, and is a repeat when it does not. The bound is
-- the same shape as before -- at worst one slot per recipient per message per
-- batch, and a message's recipient list is bounded by the format.
takesASlot :: Eq k
           => Maybe [k]    -- ^ what queued copies of this message already pay for
           -> [k]          -- ^ what THIS copy pays for
           -> Bool
takesASlot inflight mine = case inflight of
  Nothing   -> True
  Just seen -> any (`notElem` seen) mine
