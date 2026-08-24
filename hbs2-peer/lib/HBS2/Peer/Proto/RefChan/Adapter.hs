module HBS2.Peer.Proto.RefChan.Adapter where

import HBS2.Peer.Proto.RefChan.Types
import HBS2.Net.Proto.Notify
-- import HBS2.Peer.Notify
import HBS2.Data.Types.Refs

data RefChanAdapter e m =
  RefChanAdapter
  { refChanOnHead                 :: RefChanId e -> RefChanHeadBlockTran e -> m ()
  , refChanSubscribed             :: RefChanId e -> m Bool
  , refChanWriteTran              :: HashRef -> m ()
  , refChanValidatePropose        :: RefChanId e -> HashRef -> m Bool
  , refChanNotifyRely             :: RefChanId e -> RefChanNotify e -> m ()
    -- | Should this peer put this packet back on the wire now?
    --
    -- Test and set: 'True' the first time a hash is offered and 'False' ever
    -- after, for as long as this peer remembers it. Gossip hands the same
    -- packet to every neighbour, so without it a packet circulates for as long
    -- as the graph has cycles.
    --
    -- A METHOD HERE rather than a lookup in the block store, where both refchan
    -- protocols used to ask it. The hash is over a packet a stranger sends, so
    -- they can compute it before sending, put the block, and have this answer
    -- "seen" for a packet nobody handled -- which silences it, permanently and
    -- silently, on every peer they can reach. See "HBS2.Peer.Proto.Relayed".
  , refChanRelayOnce              :: HashRef -> m Bool
  -- , refChanNotifySink             :: SomeNotifySource (RefChanEvents L4Proto)
  }


