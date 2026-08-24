-- | What a peer publishes about itself, and what a neighbour reads back.
--
-- The two halves are in different packages -- 'PeerMetaAnnounce' writes and
-- "HBS2.Peer.Proto.PeerMeta" reads -- and the values go through @show@ on one
-- side and @readMay@ on the other, so nothing but a round trip establishes that
-- a key survives the journey. Until PEP-23 step C nothing could ask: 'mkPeerMeta'
-- lived in @PeerTypes@ and took a 'PeerEnv' it never used, so the only way to
-- see what a config announces was to run a peer and capture a packet.
module PeerMetaSpec (peerMetaTests) where

import HBS2.Merkle (AnnMetaData(..))
import HBS2.Net.Proto.Types (NetworkClass(..))
import HBS2.Peer.Proto.PeerMeta

import PeerConfig
import PeerMetaAnnounce

import Data.Either (fromRight)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import Data.Word

import Test.Tasty
import Test.Tasty.HUnit

-- | What a neighbour of the given classes actually receives and parses.
saidTo :: Set NetworkClass -> String -> Maybe PeerMeta
saidTo classes src =
  case mkPeerMeta (PeerConfig (fromRight mempty (parseTop src))) classes of
    ShortMetadata t -> parsePeerMeta t
    _               -> Nothing

said :: String -> Maybe PeerMeta
said = saidTo (Set.singleton Clearnet)

floorSaid :: String -> Maybe Word8
floorSaid src = peerMetaNat "mailbox-pow-min" =<< said src

peerMetaTests :: TestTree
peerMetaTests = testGroup "PEP-23: the relay floor a peer publishes"

  [ testCase "a peer that charges nothing says nothing about a floor" do
      -- Absent already means zero to a reader, so publishing "0" would put a
      -- key in every peer's meta to say what its absence says.
      floorSaid "" @?= Nothing
      floorSaid "(hbs2:mailbox:pow-min 0)" @?= Nothing

  , testCase "the floor a config sets is the floor a neighbour reads" do
      -- The whole of step C in one line: this number was previously in nobody's
      -- policy and readable by nobody, so a relay that would not carry a letter
      -- was invisible from both ends.
      floorSaid "(hbs2:mailbox:pow-min 12)" @?= Just 12

  , testCase "what is published is what the peer will actually enforce" do
      -- Through 'poWFloorFrom', the same function the forwarding decision uses,
      -- so a config the peer reads one way and announces another is not
      -- expressible. The two cases below are the ones that reading would be
      -- tempted to get right independently: the last clause wins, and a value
      -- out of range is ignored rather than clamped.
      floorSaid "(hbs2:mailbox:pow-min 4)\n(hbs2:mailbox:pow-min 20)" @?= Just 20
      floorSaid "(hbs2:mailbox:pow-min 4096)" @?= Nothing
      floorSaid "(hbs2:mailbox:pow-min 7)\n(hbs2:mailbox:pow-min 4096)" @?= Just 7

  , testCase "every neighbour is told, whatever it can reach" do
      -- Unlike the public address beside it, which is disclosed only to a peer
      -- reachable on its class (PEP-05 G). A floor is a price and not a
      -- location: there is nobody this peer would refuse to carry for and also
      -- want to keep the reason from.
      let f classes = peerMetaNat @Word8 "mailbox-pow-min"
                        =<< saidTo classes "(hbs2:mailbox:pow-min 9)"
      f (Set.singleton Clearnet) @?= Just 9
      f (Set.singleton Onion)    @?= Just 9
      f mempty                   @?= Just 9

  , testCase "a new key does not disturb the keys already there" do
      -- Why the floor could go here at all: 'PeerMeta' is an association list,
      -- so a reader that does not know a key ignores it. An older peer reading
      -- this meta still finds its http-port, which is the property that makes
      -- this step cost no compatibility.
      let m = said "http-listen \"0.0.0.0\"\n(hbs2:mailbox:pow-min 3)"
      (peerMetaValue @Integer "http-port" =<< m) @?= Just 5005
      (peerMetaNat @Word8 "mailbox-pow-min" =<< m) @?= Just 3

  , testCase "a key that is absent and a key that will not parse are one answer" do
      -- Deliberately, and 'peerMetaPoWFloor' depends on it: a peer that said
      -- nothing and a peer that said something this build cannot read are the
      -- same amount of knowledge, which is none -- and neither is a floor of
      -- zero, which would be a claim that it carries anything.
      let m = PeerMeta [ ("mailbox-pow-min", TE.encodeUtf8 "twelve") ]
      peerMetaNat @Word8 "mailbox-pow-min" m @?= Nothing
      peerMetaNat @Word8 "nothing-said-about-this" m @?= Nothing

  -- THE READER IS READING A STRANGER'S BYTES, and 'peerMetaValue' is the wrong
  -- tool for a bounded number: the derived Read for Word8 goes through Integer
  -- and then fromInteger, which WRAPS. So "-1" -- the cheapest thing to write --
  -- parsed as 255, the largest floor there is, and a neighbour could set the
  -- price of everything this node sends by publishing a minus sign.
  , testCase "a number out of range is refused, not wrapped" do
      let said' v = PeerMeta [ ("mailbox-pow-min", TE.encodeUtf8 v) ]
      -- What the wrapping reader answered, kept here so the case cannot be
      -- quietly reverted to it: Just 255, Just 44, Just 232.
      peerMetaValue @Word8 "mailbox-pow-min" (said' "-1")   @?= Just 255
      -- What this build answers.
      peerMetaNat @Word8 "mailbox-pow-min" (said' "-1")     @?= Nothing
      peerMetaNat @Word8 "mailbox-pow-min" (said' "300")    @?= Nothing
      peerMetaNat @Word8 "mailbox-pow-min" (said' "1000")   @?= Nothing
      peerMetaNat @Word8 "mailbox-pow-min" (said' "twelve") @?= Nothing
      -- Refused and not clamped, which is the same reading `poWFloorFrom` takes
      -- of a number out of range in this peer's OWN config: a clamped 300 would
      -- be a floor nobody wrote, and here the person who did not write it is a
      -- stranger.
      peerMetaNat @Word8 "mailbox-pow-min" (said' "0")      @?= Just 0
      peerMetaNat @Word8 "mailbox-pow-min" (said' "255")    @?= Just 255

  , testCase "a port out of range is refused too" do
      -- The same trap one key over, and it predates the floor: `listen-tcp` is
      -- read as a Word16, so "-1" was a request to ping port 65535 and "70000"
      -- one to ping 4464.
      let m = PeerMeta [ ("listen-tcp", TE.encodeUtf8 "-1") ]
      peerMetaNat @Word16 "listen-tcp" m @?= Nothing
  ]
