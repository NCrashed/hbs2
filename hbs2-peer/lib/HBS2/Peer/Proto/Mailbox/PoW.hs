{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
-- | The proof-of-work stamp on a mailbox packet (PEP-21, PEP-23 step A).
--
-- WHAT IT BOUNDS. An open inbox is @(sender allow all)@, which is anyone
-- growing the mailbox tree. A stamp makes each DISTINCT message cost work, so
-- it bounds the rate at which a spammer can create them. Re-sending one solved
-- message is bounded by dedup instead, not by work.
--
-- WHAT IT BINDS TO. The hash of the thing as it will be stored, so the work is
-- paid for exactly the bytes that will occupy disk, and the mailbox key, so one
-- solution cannot be moved to another mailbox. Deliberately NOT the hash the
-- peer computes for its routing marker: that one covers the whole
-- 'MailBoxProto' value, stamp included, so binding to it would be circular and
-- every grind attempt would move the target.
--
-- TWO THINGS CAN BE PAID FOR, and everything below comes in pairs because of
-- it: a letter ('messageKey') and a delete ('deleteKey'). The arithmetic is
-- shared -- 'stampBitsOver' and 'solveStampOver' -- and only the hash differs,
-- because a verifier and a solver have to agree on the preimage down to the
-- byte and two copies of it would be two chances not to.
--
-- A delete pays because it is the other packet a peer floods, and until it had
-- a field to carry work in, any non-zero floor stopped relaying deletes
-- entirely rather than pricing them. See 'forwardable'.
--
-- WHY THE GRIND IS CHEAP TO VERIFY AND HONEST TO PAY. The message hash is
-- fixed before the search starts, so solving is hashing and nothing else: the
-- message is signed once, before the stamp exists, and the solver never touches
-- ed25519. Verifying is one hash.
module HBS2.Peer.Proto.Mailbox.PoW
  ( PoWDifficulty
  , maxPayableFloor
  , payableFloor
  , cheapestFloor
  , messageKey
  , deleteKey
  , stampPreimage
  , stampBits
  , stampBitsOver
  , deleteStampBits
  , stampNames
  , stampNamesDelete
  , stampOk
  , forwardable
  , stampMarker
  , deleteMarker
  , solveStamp
  , solveStampOver
  , solveDeleteStamp
  , leadingZeroBits
  ) where

import HBS2.Prelude.Plated

import HBS2.Hash
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Data.Types.SignedBox (SignedBox)
import HBS2.Peer.Proto.Mailbox.Types

import Codec.Serialise
import Data.Bits (countLeadingZeros)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Set qualified as Set
import Data.Maybe (fromMaybe)
import Data.Word

-- | The most work a client will do for a RELAY FLOOR, whatever it is told.
--
-- A CLIENT-SIDE VALVE ON UNTRUSTED INPUT, and not a protocol rule: no peer
-- enforces it and no packet carries it. The floor a client solves for comes out
-- of a neighbour's peer-meta, which is a stranger's bytes, so without a ceiling
-- one handshaked neighbour publishing a large number sets this machine's CPU
-- bill and, since a floor nobody can pay is a floor nobody sends through, stops
-- its outgoing mail. A client that meets a floor above this sends UNSTAMPED and
-- says why, which is what it did before any of this existed.
--
-- IT DOES NOT CAP WHAT A MAILBOX CHARGES. @(pow D)@ comes out of that mailbox's
-- own signed policy, the sender chose to write there, and that is a price with
-- an author who can be held to it. Only the relay floor is capped, because only
-- the relay floor is a number somebody else picked for you.
--
-- Twenty bits is about a second of hashing on an ordinary machine, so an honest
-- sender pays a second and a flooder pays a second per distinct message. It is
-- therefore also a statement about the largest floor worth SETTING: above this,
-- no honest client will pay, and the floor stops being a price and becomes a
-- way of talking to nobody.
maxPayableFloor :: PoWDifficulty
maxPayableFloor = 20

-- | What a client will actually solve for, given what it was told.
--
-- 'Nothing' above the cap, and not a clamped value, because the caller has to
-- tell the two apart: solving 20 when 255 was asked satisfies nobody who asked
-- for 255, so the honest thing is to send unstamped and say so rather than to
-- grind for a second and be refused anyway.
payableFloor :: PoWDifficulty -> Maybe PoWDifficulty
payableFloor d | d <= maxPayableFloor = Just d
               | otherwise            = Nothing

-- | The floor to solve for, given what each neighbour published.
--
-- THE CHEAPEST AND NOT THE DEAREST. A packet is gossiped to every neighbour at
-- once and each decides separately whether to carry it on, so one willing
-- neighbour is enough for it to travel: the number that decides whether it goes
-- anywhere is the smallest any of them wants. The largest is what reaching ALL
-- of them would cost, which is a different and much more expensive promise.
--
-- 'Nothing' -- a neighbour that published no floor -- COUNTS AS ZERO here, and
-- the same absence means "unknown, do not assume zero" to a reader asking about
-- one peer. Both readings under-report, which is the safe direction: silence is
-- not evidence of a price, and under a minimum an unknown neighbour is exactly
-- the one that might carry the packet for free.
--
-- Pure and separate from the peer that walks its neighbour table, because it is
-- the whole of the decision and the walk is not: as an expression inside that
-- walk it was a maximum for a day, which set this node's price from any one
-- handshaked stranger's meta.
cheapestFloor :: [Maybe PoWDifficulty] -> PoWDifficulty
cheapestFloor [] = 0
cheapestFloor xs = minimum (fmap (fromMaybe 0) xs)

-- | The identity of a message: the hash of the bytes the peer stores.
--
-- The same value the mailbox uses to name a message everywhere else, which is
-- the point. One identity names it for the work, for the routing marker and for
-- the block it becomes.
messageKey :: forall s . ForMailbox s => Message s -> HashRef
messageKey = HashRef . hashObject @HbSync . serialise

-- | The identity of a delete: the hash of the signed box (PEP-23 step A).
--
-- What 'messageKey' is for a letter. The same value again names three things:
-- the work, this peer's routing marker, and the proof block a @Deleted@ entry
-- points at -- @mailboxAcceptDelete@ writes @serialise box@ and the store is
-- content-addressed, so these are the same bytes under the same hash.
--
-- THE BOX AND NOT THE PAYLOAD. The payload is what the owner signed; the box is
-- payload plus key plus signature. Binding to the payload would let one solution
-- be reused under a different signature, and the box is what is stored anyway.
--
-- Work solved for a letter cannot be spent on a delete or the other way round:
-- the two hashes are digests of differently-typed CBOR values, so matching them
-- is a Blake2b collision and not a choice a sender has.
deleteKey :: forall s . ForMailbox s => SignedBox (DeleteMessagesPayload s) s -> HashRef
deleteKey = HashRef . hashObject @HbSync . serialise

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
stampBits msg = stampBitsOver @s (messageKey @s msg)

-- | The same, over whatever the work was solved against.
--
-- The one arithmetic, and there is exactly one because a delete pays for a
-- different hash than a letter (PEP-23 step A) and nothing else about the count
-- changes. A second copy of these three lines would be a second chance to
-- disagree about the preimage, which is the value a verifier and a solver have
-- to agree on down to the byte.
stampBitsOver :: forall s . ForMailbox s => HashRef -> MessageStamp s -> Int
stampBitsOver h MessageStamp1{..} =
  leadingZeroBits (fromHbSyncHash (hashObject @HbSync (stampPreimage @s msMailbox h msNonce)))

-- | How much work a delete's stamp carries.
deleteStampBits :: forall s . ForMailbox s
                => SignedBox (DeleteMessagesPayload s) s -> MessageStamp s -> Int
deleteStampBits box = stampBitsOver @s (deleteKey @s box)

-- | Is the mailbox this stamp names one the message is actually addressed to?
--
-- The check for a peer deciding whether to amplify: it has no policy for any of
-- these mailboxes and no business picking one, but work solved for a mailbox
-- that is not a recipient is work for some other message's delivery.
stampNames :: forall s . ForMailbox s => MessageContent s -> MessageStamp s -> Bool
stampNames content MessageStamp1{..} = Set.member msMailbox (messageRecipients content)

-- | The same question for a delete: is the mailbox named the one being deleted from?
--
-- A letter has several recipients and the work is bound to one of them; a delete
-- has exactly one mailbox, and it is the key RECOVERED from the signature rather
-- than a field on the wire. So a relay has it already -- the delete branch
-- unboxes before it decides anything -- and this check is free.
--
-- It is worth making even though the signer chose that key. Work solved for
-- another mailbox is work for another delete, and without this a single solution
-- could be stapled to any number of delete boxes.
stampNamesDelete :: forall s . ForMailbox s => MailboxKey s -> MessageStamp s -> Bool
stampNamesDelete mbox MessageStamp1{..} = msMailbox == mbox

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
-- FOUR BRANCHES SINCE PEP-23 STEP A, which is the reason this had to be a
-- function before that step and not after: a delete gained a stamped form, so
-- the count of places that could disagree grew.
--
-- NAMED, because a stamp solved for a mailbox this packet is not addressed to
-- is work for somebody else's delivery. It buys the sender nothing here, and
-- this peer has no policy for any of these mailboxes and no business picking
-- one. For a message that is 'stampNames'; for a delete, 'stampNamesDelete';
-- for a packet with no stamp at all it is trivially true, since there is
-- nobody it could be misdirected to.
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

-- | The gossip dedup identity of a stamped delete (PEP-23 step A).
--
-- Built like 'stampMarker' and for all three of its reasons, restated because
-- the delete branch is where getting this wrong would be easy: the plain branch
-- marks a delete with the hash of the whole wire value, and a stamped one cannot
-- do that.
--
-- THE NONCE IS OUT. The wire value contains it, so marking by the wire value
-- would make a re-stamp of one box under a fresh nonce look like a new delete
-- and buy a second flood per solution.
--
-- THE WORK IS IN. Without the bits, anyone who saw a stamped delete could mint
-- @MessageStamp1 mbox 0@ -- free, and carried wherever a peer's floor is zero --
-- and race it ahead of the honest copy, which would then be suppressed as seen
-- everywhere it had not yet reached, while the free copy is refused for want of
-- work by the peers that charge. With the bits in, suppressing a copy worth D
-- bits costs D bits.
--
-- It is also NOT the plain branch's marker, which is the hash of the whole
-- 'MailBoxProto' value and so differs by construction: stripping a stamp and
-- re-sending the delete plain does not suppress the honest stamped copy. It
-- gains an attacker nothing either -- acceptance was never gated by the floor,
-- so a stripped delete still takes effect wherever it arrives.
deleteMarker :: forall s . ForMailbox s
             => MessageStamp s -> SignedBox (DeleteMessagesPayload s) s -> HashRef
deleteMarker st@MessageStamp1{..} box =
  HashRef (hashObject @HbSync (serialise (msMailbox, bits, box)))
  where bits = deleteStampBits @s box st

-- | Grind until the stamp meets the difficulty.
--
-- Pure, and it does not terminate for a difficulty nobody can reach, which is
-- the caller's problem to bound: this is a search, and how long a sender is
-- willing to search is not a decision this function can make. Deterministic
-- from zero, so the same message always yields the same stamp.
solveStamp :: forall s . ForMailbox s => PoWDifficulty -> MailboxKey s -> Message s -> MessageStamp s
solveStamp d mbox msg = solveStampOver @s d mbox (messageKey @s msg)

-- | The same search, over whatever is being paid for.
--
-- The solver's half of 'stampBitsOver', and it has to be the same three lines
-- inverted or a sender grinds against a preimage no verifier computes.
solveStampOver :: forall s . ForMailbox s
               => PoWDifficulty -> MailboxKey s -> HashRef -> MessageStamp s
solveStampOver d mbox h = go 0
  where
    need = fromIntegral d
    go n | bits n >= need = MessageStamp1 mbox n
         | otherwise      = go (succ n)
    bits n = leadingZeroBits (fromHbSyncHash (hashObject @HbSync (stampPreimage @s mbox h n)))

-- | Grind a stamp for a delete.
--
-- The mailbox is the one the box is signed by, and a caller that passes another
-- has bought nothing: 'stampNamesDelete' is what a relay asks.
solveDeleteStamp :: forall s . ForMailbox s
                 => PoWDifficulty -> MailboxKey s
                 -> SignedBox (DeleteMessagesPayload s) s -> MessageStamp s
solveDeleteStamp d mbox box = solveStampOver @s d mbox (deleteKey @s box)

-- | Leading zero bits of a digest.
leadingZeroBits :: ByteString -> Int
leadingZeroBits = go 0 . BS.unpack
  where
    go acc (w:ws) | w == 0    = go (acc + 8) ws
                  | otherwise = acc + countLeadingZeros w
    go acc []                 = acc
