{-# Language AllowAmbiguousTypes #-}

-- | The mailbox options a peer reads out of its own config, and nothing else.
--
-- Its own module so a test can ask what a config MEANS without building a
-- worker. Both functions here decide how much a stranger can make this peer
-- spend, and while they lived beside the worker the only way to exercise them
-- was to run one: 'poWFloorFrom' carried a haddock saying it was pure so it
-- could be tested without a peer, and had no test.
module MailboxConfig
  ( hbs2MailboxPoWMinOpt
  , poWFloorFrom
  , hbs2MailboxReplicateFromOpt
  , replicateFromIn
  ) where

import HBS2.Prelude.Plated
import HBS2.Net.Auth.Credentials
import HBS2.Peer.Proto.Mailbox.Types

import Data.Config.Suckless

import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe
import Safe (lastMay)

-- | The least proof-of-work this peer will FORWARD, in leading zero bits.
--
-- Peer-wide and not per-mailbox: it is consulted before gossip, where the peer
-- does not yet know which mailbox a packet is for and usually hosts none of
-- them. What a mailbox charges for STORAGE is @(pow D)@ in its own signed
-- policy, which is a different number in a different place.
--
-- Absent means zero, and zero is the default: this peer carries a stamped
-- packet on the same terms as a plain one.
--
-- WHAT A NON-ZERO VALUE COSTS SOMEBODY ELSE, said here because the operator who
-- sets it is the only one who can weigh it. It is in nobody's policy, so a
-- sender solving what a mailbox asked for can still be refused by a relay on the
-- way; on a pure relay sitting on the only path, the letter is simply gone. Two
-- things make that less than invisible, and neither makes it visible: the peer
-- publishes this number in its meta so a neighbour ONE HOP away can pay it
-- ('PeerMetaAnnounce.mkPeerMeta'), and refusals are counted into the periodic
-- report ('mpwPoWNotForwarded'). A relay further out is still silence.
--
-- The rest of the reasoning -- why both floodable packets have a stamped form,
-- what a client will and will not pay for this number, and why the default has
-- not moved -- is in "HBS2.Peer.Proto.Mailbox.PoW".
hbs2MailboxPoWMinOpt :: String
hbs2MailboxPoWMinOpt = "hbs2:mailbox:pow-min"

-- | The floor as the config states it, or zero if it does not.
--
-- The last clause wins, which is how the other options here read; a value that
-- does not fit is ignored rather than clamped, because a clamped 4096 would
-- silently become a floor nobody asked for.
poWFloorFrom :: [Syntax C] -> PoWDifficulty
poWFloorFrom conf =
  fromMaybe 0 $ lastMay [ fromIntegral d
                        | ListVal [StringLike o, LitIntVal d] <- conf
                        , o == hbs2MailboxPoWMinOpt
                        , d >= 0 && d <= 255
                        ]

-- | Peers whose replication of a CHARGING mailbox this peer will take.
--
-- The hole this closes. A message pulled out of a status tree arrives as
-- @Replicated@, and a replicated message carries no stamp and cannot: the tree
-- stores messages, not the work that bought them. Charging there would mean a
-- mailbox with @(pow D)@ stops replicating between its own hosts, so the check
-- was skipped, and the boundary was left to @(peer allow|deny)@.
--
-- That boundary cannot hold it, because one clause answers two different
-- questions. @(peer ...)@ gates the RELAYING neighbour, and an open inbox has
-- to say @(peer allow all)@ or a stranger's letter never reaches it over
-- gossip. The same clause then lets any handshaked peer announce a status
-- naming a tree it invented, and every message in that tree was admitted with
-- the work check skipped. On the one configuration @(pow D)@ exists for, it
-- bounded nothing.
--
-- So the second question is asked separately, and locally: replication is disk
-- this peer spends, and which co-hosts it trusts to fill it is not something a
-- stranger's policy should be able to answer.
hbs2MailboxReplicateFromOpt :: String
hbs2MailboxReplicateFromOpt = "hbs2:mailbox:replicate-from"

-- | The co-hosts the config names, as keys.
--
-- Every clause counts, not the last one: this is a set and not a setting. A key
-- that will not parse is ignored rather than failing the whole option, which is
-- the same reading the floor above takes of a number out of range.
--
-- Absent means nobody, so a mailbox that charges takes no replication until an
-- operator names the peers it replicates with; a mailbox that charges nothing
-- is unaffected and keeps behaving exactly as it did.
replicateFromIn :: forall s . ForMailbox s => [Syntax C] -> HashSet (PubKey 'Sign s)
replicateFromIn conf =
  HS.fromList [ k
              | ListVal [StringLike o, StringLike v] <- conf
              , o == hbs2MailboxReplicateFromOpt
              , Just k <- [fromStringMay @(PubKey 'Sign s) v]
              ]
