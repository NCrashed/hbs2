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

-- | The least proof-of-work this peer will forward, in leading zero bits.
--
-- A peer-wide floor, and it has to be peer-wide rather than per-mailbox: it is
-- consulted before gossip, where the peer does not yet know which mailbox a
-- message is for and usually hosts none of them. What a mailbox charges is
-- @(pow D)@ in its own signed policy, and that one bounds storage.
--
-- Absent means zero, which forwards a stamped message on the same terms as a
-- plain one.
--
-- WHAT A NON-ZERO FLOOR COSTS SOMEBODY ELSE, said here because the operator who
-- sets it is the only one who can weigh it. A sender solves for what the
-- MAILBOX charges, which is in the mailbox's signed policy; this is a peer's
-- own number and is in nobody's policy. So a peer with a floor of 16 does not
-- forward a message that honestly paid the 12 its destination asked for.
--
-- On a peer that HOSTS the mailbox the letter still arrives -- the floor gates
-- forwarding only, and the mailbox's own policy decides acceptance. The case
-- that bites is a pure relay: if it is on the only path, the letter is gone.
-- What makes it visible from this side is 'mpwPoWNotForwarded' in the worker,
-- which counts it into the periodic report.
--
-- FROM THE OTHER SIDE it is published (PEP-23 step C): this number goes into
-- the peer's meta as @mailbox-pow-min@, so a neighbour can read what this peer
-- wants and pay it rather than discovering the refusal as silence. That reaches
-- ONE HOP. A relay four hops away with a higher floor still drops the packet
-- with nothing said, and in a flood network with no end-to-end feedback there
-- is nothing to do about that but count it, above.
--
-- SINCE PEP-23 STEP A THIS IS A DIAL AND NOT A SWITCH, which it was not before
-- and which is the reason it stayed at zero. A message and a delete both have a
-- stamped form on the wire now, so a floor prices them; before the delete had a
-- field to carry work in, any non-zero value here stopped this peer relaying all
-- plain mail and all deletes outright, silently and forever.
--
-- What a sender does about it is PEP-23 step D: `hbs2-hub` and
-- `hbs2-peer mailbox delete:message` ask this peer what the road out charges
-- and solve it. A sender that does not ask, or a build older than the stamped
-- forms, still pays zero and is still refused -- so a floor remains a thing to
-- raise deliberately and not a default.
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
