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

-- | The gossip dedup identity of a stamped message.
--
-- The nonce is NOT in it, and that is the whole reason this is a function
-- rather than the hash of the wire value. The peer's marker for a plain message
-- covers the whole 'MailBoxProto', so re-sending one message under a fresh
-- nonce would look like a new message and buy a second flood for each solution.
--
-- It is not the plain marker either. Sharing one would let anyone who sees a
-- stamped message strip the stamp, re-send it plain for free, and have the
-- honest stamped copy suppressed as already seen everywhere it had not yet
-- reached.
stampMarker :: forall s . ForMailbox s => MessageStamp s -> Message s -> HashRef
stampMarker MessageStamp1{..} msg = HashRef (hashObject @HbSync (serialise (msMailbox, msg)))

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
