{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
-- | The proof-of-work stamp on a mailbox message (PEP-21).
--
-- WHAT IT BOUNDS. An open inbox is @(sender allow all)@, which is anyone
-- growing the mailbox tree. A stamp makes each DISTINCT message cost work, so
-- it bounds the rate at which a spammer can create them. Re-sending one solved
-- message is bounded by dedup instead, not by work.
--
-- WHAT IT BINDS TO. The hash of the message as it will be stored, so the work
-- is paid for exactly the bytes that will occupy disk, and the mailbox key, so
-- one solution cannot be moved to another mailbox. Deliberately NOT the hash
-- the peer computes for its routing marker: that one covers the whole
-- 'MailBoxProto' value, stamp included, so binding to it would be circular and
-- every grind attempt would move the target.
--
-- WHY THE GRIND IS CHEAP TO VERIFY AND HONEST TO PAY. The message hash is
-- fixed before the search starts, so solving is hashing and nothing else: the
-- message is signed once, before the stamp exists, and the solver never touches
-- ed25519. Verifying is one hash.
module HBS2.Peer.Proto.Mailbox.PoW
  ( PoWDifficulty
  , messageKey
  , stampPreimage
  , stampBits
  , stampNames
  , stampOk
  , forwardable
  , stampMarker
  , solveStamp
  , leadingZeroBits
  ) where

import HBS2.Prelude.Plated

import HBS2.Hash
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Peer.Proto.Mailbox.Types

import Codec.Serialise
import Data.Bits (countLeadingZeros)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Set qualified as Set
import Data.Word

-- | The identity of a message: the hash of the bytes the peer stores.
--
-- The same value the mailbox uses to name a message everywhere else, which is
-- the point. One identity names it for the work, for the routing marker and for
-- the block it becomes.
messageKey :: forall s . ForMailbox s => Message s -> HashRef
messageKey = HashRef . hashObject @HbSync . serialise

-- | What gets hashed for one attempt.
--
-- A CBOR triple rather than a concatenation of three byte strings: PEP-21
-- writes the binding as @mailboxKey || messageHash || nonce@, and spelled that
-- way the fields have no unambiguous boundary. The encoding gives them one.
stampPreimage :: forall s . ForMailbox s => MailboxKey s -> HashRef -> Word64 -> LBS.ByteString
stampPreimage mbox h nonce = serialise (mbox, h, nonce)

-- | How much work this stamp actually carries, in leading zero bits.
--
-- Answers with a number rather than a verdict so a caller can report what it
-- got against what it wanted, which is the difference between a peer that says
-- "refused" and one that says "18 bits, wanted 20".
stampBits :: forall s . ForMailbox s => Message s -> MessageStamp s -> Int
stampBits msg MessageStamp1{..} =
  leadingZeroBits (fromHbSyncHash (hashObject @HbSync (stampPreimage @s msMailbox (messageKey @s msg) msNonce)))

-- | Is the mailbox this stamp names one the message is actually addressed to?
--
-- The check for a peer deciding whether to amplify: it has no policy for any of
-- these mailboxes and no business picking one, but work solved for a mailbox
-- that is not a recipient is work for some other message's delivery.
stampNames :: forall s . ForMailbox s => MessageContent s -> MessageStamp s -> Bool
stampNames content MessageStamp1{..} = Set.member msMailbox (messageRecipients content)

-- | Does this stamp satisfy a difficulty for a given mailbox?
--
-- The check for the peer that HOSTS the mailbox and knows what its policy
-- charges. Both halves matter: work for the wrong mailbox is not work for this
-- one.
stampOk :: forall s . ForMailbox s => PoWDifficulty -> MailboxKey s -> Message s -> MessageStamp s -> Bool
stampOk d mbox msg st = msMailbox st == mbox && stampBits msg st >= fromIntegral d

-- | Will this peer carry the packet on?
--
-- THE RELAY'S COUNTERPART TO 'stampOk', and one rule where there were three
-- spellings. 'mailboxProto' decides this in three branches -- an unstamped
-- message, a stamped one, and a delete -- and wrote it out inline each time:
-- @floorD == 0@, @named && bits >= floorD@, @floorD == 0@. The first and third
-- are the second with the zero an unstamped packet pays, which the comments at
-- both sites say in words. Three spellings are three chances to raise a floor
-- in two places, and none of the three was reachable by a test: they live
-- inside the protocol handler, which has no harness.
--
-- NAMED, because a stamp solved for a mailbox this message is not addressed to
-- is work for somebody else's delivery. It buys the sender nothing here, and
-- this peer has no policy for any of these mailboxes and no business picking
-- one -- which is 'stampNames', asked here.
--
-- IT GATES CARRYING AND NOT TAKING, which is the distinction the whole design
-- rests on: a weak stamp means "I will not carry this further", the message
-- still reaches this peer's queue, and what happens to it there is the MAILBOX
-- policy's decision, which knows a real difficulty. A relay that refused to
-- take would be a relay that decides delivery for a mailbox it does not hold.
forwardable :: PoWDifficulty -> Bool -> Int -> Bool
forwardable d named bits = named && bits >= fromIntegral d

-- | The gossip dedup identity of a stamped message.
--
-- The NONCE is not in it, and that is the whole reason this is a function
-- rather than the hash of the wire value. The peer's marker for a plain message
-- covers the whole 'MailBoxProto', so re-sending one message under a fresh
-- nonce would look like a new message and buy a second flood for each solution.
--
-- It is not the plain marker either. Sharing one would let anyone who sees a
-- stamped message strip the stamp, re-send it plain for free, and have the
-- honest stamped copy suppressed as already seen everywhere it had not yet
-- reached.
--
-- THE WORK IS IN IT, and that is what stops the marker being used as a weapon.
-- With the bits left out, anybody who saw a message could mint
-- @MessageStamp1 mbox 0@ -- free, and accepted for gossip wherever the peer's
-- floor is zero, which is the default -- and race it ahead of the honest copy.
-- Both have the same marker, so the honest twenty-bit copy is @seen@ and dies
-- at its sender's first hop, while the attacker's junk copy is refused for want
-- of work at the host. The letter is nowhere, the sender paid twenty bits for
-- it, and PEP-21 admits there is no rejection signal to notice with.
--
-- With the bits in it, a restamp at the SAME difficulty is still one message to
-- gossip (the marker does not move with the nonce), and suppressing a copy
-- worth D bits costs D bits of work, which is what the honest sender paid. What
-- it buys an attacker is a second flood per difficulty rather than a
-- suppression: a bounded, exponentially-priced amplification instead of an
-- unbounded denial, and the peer's floor cuts the cheap end of it off.
stampMarker :: forall s . ForMailbox s => MessageStamp s -> Message s -> HashRef
stampMarker st@MessageStamp1{..} msg =
  HashRef (hashObject @HbSync (serialise (msMailbox, stampBits @s msg st, msg)))

-- | Grind until the stamp meets the difficulty.
--
-- Pure, and it does not terminate for a difficulty nobody can reach, which is
-- the caller's problem to bound: this is a search, and how long a sender is
-- willing to search is not a decision this function can make. Deterministic
-- from zero, so the same message always yields the same stamp.
solveStamp :: forall s . ForMailbox s => PoWDifficulty -> MailboxKey s -> Message s -> MessageStamp s
solveStamp d mbox msg = go 0
  where
    h = messageKey @s msg
    need = fromIntegral d
    go n | bits n >= need = MessageStamp1 mbox n
         | otherwise      = go (succ n)
    bits n = leadingZeroBits (fromHbSyncHash (hashObject @HbSync (stampPreimage @s mbox h n)))

-- | Leading zero bits of a digest.
leadingZeroBits :: ByteString -> Int
leadingZeroBits = go 0 . BS.unpack
  where
    go acc (w:ws) | w == 0    = go (acc + 8) ws
                  | otherwise = acc + countLeadingZeros w
    go acc []                 = acc
