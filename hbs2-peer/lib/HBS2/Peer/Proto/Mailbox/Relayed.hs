-- | What this peer has already put back on the wire.
--
-- A flood suppressor, and only that: gossip hands the same message to every
-- neighbour, so without a memory of what has been forwarded a message
-- circulates for as long as the graph has cycles. The question it answers is
-- "have WE relayed this", which is a fact about this process and nothing else.
--
-- WHY IT IS NOT A BLOCK, which is why this module exists. The answer used to
-- be the presence of a block at @hashObject (serialise (RoutedEntry h))@, where
-- @h@ is the hash of a message anybody can read off the wire. So the marker's
-- address was computable by a stranger, and the store it lived in is content
-- addressed and fetched on demand -- which means a stranger could put it there
-- (see the header of @MailboxMerged@ for the delivery route, which is the same
-- one). Planting it on the peers between a sender and a hub kept a chosen
-- letter from ever being forwarded, and nothing said so.
--
-- The general rule again, because this is its second instance: the presence of
-- a block in a GLOBAL content-addressed store cannot carry a LOCAL decision.
--
-- BOUNDED BY COUNT, not by time, and not unbounded as the block marker was
-- (which is the @$class: leak@ that stood beside it). Two generations: entries
-- go into the young one, both are consulted, and when the young one fills, it
-- becomes the old one and a fresh one takes its place. Every operation is O(1)
-- and memory is bounded by twice 'relayGeneration', with no sweeper and no pass
-- over live entries on insert -- which matters here and did not matter for the
-- check-nonces next door, because what drives this map is remote traffic at
-- whatever rate a stranger cares to send.
--
-- WHAT IS TRADED for that bound: suppression is no longer permanent. After
-- 'relayGeneration' distinct messages have passed, an old one presented again
-- is forwarded again. That buys an attacker nothing -- re-sending a message
-- they already hold costs them exactly what sending a new one costs -- and the
-- alternative, remembering every message forever, is the leak.
module HBS2.Peer.Proto.Mailbox.Relayed
  ( Relayed
  , relayGeneration
  , newRelayed
  , relayOnce
  , RelayMemory(..)
  , emptyRelayMemory
  , rememberRelayed
  ) where

import HBS2.Prelude
import HBS2.Data.Types.Refs (HashRef)

import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import UnliftIO

-- | How many distinct messages one generation holds.
--
-- A judgement, not a derivation. It has to be comfortably more than the
-- messages in flight across the gossip fan-out during one propagation, so that
-- an honest message is never forwarded twice; and small enough that two
-- generations of hashes are a rounding error against a peer's footprint.
-- Sixty-four thousand is about four megabytes of 'HashRef' at two generations.
relayGeneration :: Int
relayGeneration = 65536

-- | The two generations, youngest first.
--
-- A plain record with a pure step, so the rule can be asked questions without a
-- peer, a socket or a clock.
data RelayMemory =
  RelayMemory
  { relayYoung :: HashSet HashRef
  , relayOld   :: HashSet HashRef
  }
  deriving stock (Eq,Show)

emptyRelayMemory :: RelayMemory
emptyRelayMemory = RelayMemory mempty mempty

-- | Record one hash, and say whether it was new.
--
-- @(True, m')@ means this peer had not relayed it and now has. The two answers
-- come from one step on purpose: split into a lookup and an insert they are two
-- transactions, and two handlers meeting the same message between them both
-- forward it.
--
-- The generation size is an ARGUMENT and not 'relayGeneration' read from here,
-- so that a test can watch a generation roll over without offering sixty-five
-- thousand hashes to do it. 'relayOnce' is the caller that supplies the real
-- one, and it is the only caller that should.
rememberRelayed :: Int -> HashRef -> RelayMemory -> (Bool, RelayMemory)
rememberRelayed gen h m@RelayMemory{..}
  | HS.member h relayYoung || HS.member h relayOld = (False, m)
  | HS.size relayYoung >= gen =
      -- The young generation is full: it ages, and this hash opens the next
      -- one. Nothing is scanned and nothing is compared, which is the whole
      -- point of doing it this way.
      (True, RelayMemory { relayYoung = HS.singleton h, relayOld = relayYoung })
  | otherwise = (True, m { relayYoung = HS.insert h relayYoung })

newtype Relayed = Relayed (TVar RelayMemory)

newRelayed :: MonadIO m => m Relayed
newRelayed = Relayed <$> newTVarIO emptyRelayMemory

-- | Should this peer relay this message now?
--
-- Test and set, in one transaction. 'True' on the first sighting and 'False'
-- ever after, for as long as the memory holds it.
relayOnce :: MonadIO m => Relayed -> HashRef -> m Bool
relayOnce (Relayed t) h = atomically $ stateTVar t (rememberRelayed relayGeneration h)
