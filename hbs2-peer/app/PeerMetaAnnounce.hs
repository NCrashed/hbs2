-- | What this peer tells a neighbour about itself, and nothing else.
--
-- Its own module for the reason 'MailboxConfig' is: this is a function of a
-- config and a recipient, with no peer, no socket and no clock in it, and while
-- it lived in @PeerTypes@ the only way to ask what a config publishes was to run
-- a peer and packet-capture the answer. Two of the four keys below gate on the
-- recipient, and one of them is an address that must not reach the wrong kind of
-- neighbour (PEP-05 G), which is exactly the sort of rule that should be a test.
--
-- THE CONSUMER IS "PeerMeta" (@fillPeerMeta@), which is where a neighbour's
-- answer is parsed and acted on. Keeping the two apart is deliberate: what we
-- say is a decision about this peer's config, and what we do with what THEY say
-- is a decision about the network.
module PeerMetaAnnounce (mkPeerMeta) where

import HBS2.Prelude.Plated
import HBS2.Merkle (AnnMetaData)
import HBS2.Net.IP.Addr
import HBS2.Net.Proto.Types (NetworkClass(..),classOf)
import HBS2.Peer.Proto (PeerAddr(..), L4Proto)
import HBS2.Peer.Proto.PeerMeta

import MailboxConfig (poWFloorFrom)
import PeerConfig

import Control.Monad.Reader (runReader)
import Control.Monad.Writer qualified as W
import Data.List qualified as L
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Word

-- | Build the peer-meta announced to a neighbour.
--
-- The neighbour's reachable network classes gate which of our own public
-- addresses we disclose: an onion address is handed only to onion-capable peers,
-- so a clearnet peer never learns it (PEP-05 G + the network-class policy).
mkPeerMeta :: PeerConfig -> Set NetworkClass -> AnnMetaData
mkPeerMeta (PeerConfig syn) recipientClasses = do

    -- Only a port a neighbour can actually open. The API binds loopback
    -- unless `http-listen` says otherwise, and announcing a loopback port
    -- just sends everyone probing an address that will never answer them.
    let mHttpPort :: Maybe Integer
        mHttpPort = do
          (host, port) <- runReader peerHttpListen syn
          guard (not (isLoopbackHost host))
          pure port

    let mTcpPort :: Maybe Word16
        mTcpPort =
          (
          fmap (\case L4Address _ (IPAddrPort (_, p)) -> p
                      L4AddressName _ _ p             -> p)
            . fromStringMay @(PeerAddr L4Proto)
          )
          =<< runReader (cfgValue @PeerListenTCPKey) syn

    -- our own public address(es); disclose the first one whose class the
    -- recipient can actually reach
    let mPubAddr :: Maybe String
        mPubAddr = listToMaybe
          [ a | a <- Set.toList (runReader (cfgValue @PeerPublicAddressKey) syn)
              , Just pa <- [fromStringMay @(PeerAddr L4Proto) a]
              , classOf pa `Set.member` recipientClasses
          ]

    -- THE MAILBOX RELAY FLOOR, and it is here rather than in a
    -- new message because a peer's floor is a small number every neighbour
    -- benefits from knowing and nobody needs to ask for twice.
    --
    -- What it fixes: a sender solves for what the MAILBOX charges, which is in
    -- the mailbox's signed policy, while a relay's floor is that peer's own
    -- setting and is in nobody's policy. So a relay refusing to carry a letter
    -- that honestly paid its destination's price was invisible from both ends.
    -- Published, a sender's peer can at least see what its own neighbours want
    -- and pay it.
    --
    -- NOT gated on the recipient's classes, unlike the address above: this is a
    -- price, not a location, and a peer that will not carry cheap traffic has no
    -- reason to keep that a secret from anybody it would refuse.
    --
    -- Omitted at zero rather than published as "0". Zero is the default and is
    -- what an absent key already means to a reader, so writing it would put a
    -- key in every peer's meta to say nothing.
    let mPoWMin :: Maybe Word8
        mPoWMin = case poWFloorFrom syn of
                    0 -> Nothing
                    d -> Just d

    annMetaFromPeerMeta . PeerMeta $ W.execWriter do
      mHttpPort `forM` \p -> elem "http-port" (TE.encodeUtf8 . Text.pack . show $ p)
      mTcpPort `forM` \p -> elem "listen-tcp" (TE.encodeUtf8 . Text.pack . show $ p)
      mPubAddr `forM` \a -> elem "public-address" (TE.encodeUtf8 . Text.pack . show $ a)
      mPoWMin `forM` \d -> elem "mailbox-pow-min" (TE.encodeUtf8 . Text.pack . show $ d)

  where
    elem k = W.tell . L.singleton . (k ,)
