-- | What a proof-of-work stamp is worth, and what it is worth it FOR (PEP-21).
--
-- The stamp is unsigned, so every property below is a property of arithmetic
-- over the message and the mailbox rather than of anybody's authority. That is
-- also what makes them worth pinning: nothing else refuses a stamp that was
-- solved for the wrong thing.
--
-- The difficulty here is deliberately tiny. What is under test is which inputs
-- the work is bound to, and one bit binds to exactly what twenty do.
module MailboxPoW (mailboxPoWTests, mailboxConfigTests) where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (HbSync,hashObject)
import HBS2.Net.Auth.Credentials (peerSignPk)
import HBS2.Peer.Proto.Mailbox.PoW
import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Base58 (AsBase58(..))

import MailboxConfig

import Data.Config.Suckless
import Data.HashSet qualified as HS

import MailboxFixture

import Codec.Serialise (serialise)
import Data.ByteString.Char8 qualified as B8
import Data.Word (Word64)
import Lens.Micro.Platform (view)

import Test.Tasty
import Test.Tasty.HUnit

-- Small enough that the grind is part of no measurement here.
easy :: PoWDifficulty
easy = 8

-- | Where a NEGATIVE has to hold, and the difference is not stylistic.
--
-- "This stamp does not satisfy that mailbox" is exact -- the key is compared.
-- "This stamp does not satisfy another message" cannot be: the wrong message
-- passes whenever its own hash happens to start with enough zeroes, which is
-- what a hash does one time in 2^D. It is not a property a test can make
-- certain, only one it can price, so the negatives below are asked at a
-- difficulty where the accident is 1 in 65536 rather than 1 in 256.
harder :: PoWDifficulty
harder = 16

mailboxPoWTests :: TestTree
mailboxPoWTests = testGroup "mailbox proof-of-work"

  [ testCase "work solved is work verified" $
      withStore $ \sto -> do
        alice <- aPeer
        (bobC, bobS) <- aPeer
        let bob = view peerSignPk bobC

        msg <- aMessage sto alice [bobS] (B8.pack "a letter")

        let stamp = solveStamp easy bob msg

        assertBool "the stamp carries at least what was asked for"
          (stampBits msg stamp >= fromIntegral easy)
        assertBool "and the mailbox it was solved for accepts it"
          (stampOk easy bob msg stamp)

  , testCase "a solution does not move to another mailbox" $
      withStore $ \sto -> do
        alice <- aPeer
        (bobC, bobS)   <- aPeer
        (carolC, carolS) <- aPeer
        let bob   = view peerSignPk bobC
            carol = view peerSignPk carolC

        msg <- aMessage sto alice [bobS, carolS] (B8.pack "to both of them")

        let stamp = solveStamp easy bob msg

        -- Both are recipients, so this is not about who the letter is for. The
        -- work was bound to one mailbox key and buys delivery to that one:
        -- otherwise a single grind would pay for every mailbox in the world
        -- that charges the same difficulty.
        assertBool "the mailbox it names takes it"
          (stampOk easy bob msg stamp)
        assertBool "the other one does not"
          (not (stampOk easy carol msg stamp))

  , testCase "a solution does not move to another message" $
      withStore $ \sto -> do
        alice <- aPeer
        (bobC, bobS) <- aPeer
        let bob = view peerSignPk bobC

        one <- aMessage sto alice [bobS] (B8.pack "the letter that was paid for")
        two <- aMessage sto alice [bobS] (B8.pack "a different letter")

        let stamp = solveStamp harder bob one

        assertBool "the message it was solved over takes it"
          (stampOk harder bob one stamp)
        -- The whole bound PoW provides: each DISTINCT message costs fresh work.
        assertBool "another message does not"
          (not (stampOk harder bob two stamp))

  , testCase "a stamp nobody solved is not a stamp" $
      withStore $ \sto -> do
        alice <- aPeer
        (bobC, bobS) <- aPeer
        let bob = view peerSignPk bobC

        msg <- aMessage sto alice [bobS] (B8.pack "a letter")

        -- The negative every other case here reaches through solveStamp: a
        -- stamp that was never solved at all. Free to mint, and the whole
        -- question PoW asks is whether it counts.
        let free = MessageStamp1 bob 0
        assertBool "the mailbox refuses work nobody did"
          (not (stampOk harder bob msg free))
        -- ...and at difficulty zero it is a stamp, because zero is a mailbox
        -- that asks for nothing. That is what every mailbox predating PEP-21
        -- is, so it has to keep working.
        assertBool "a mailbox that charges nothing takes it"
          (stampOk 0 bob msg free)

  , testCase "a stamp for a mailbox nobody addressed is not a stamp" $
      withStore $ \sto -> do
        alice <- aPeer
        (bobC, bobS)     <- aPeer
        (strangerC, _)   <- aPeer
        let bob      = view peerSignPk bobC
            stranger = view peerSignPk strangerC

        msg <- aMessage sto alice [bobS] (B8.pack "a letter")
        content <- contentOf msg

        -- The check a forwarding peer can make WITHOUT holding any of these
        -- mailboxes: it has no policy to consult and no business picking a
        -- recipient, but work solved for a mailbox this letter is not
        -- addressed to is work for somebody else's delivery.
        assertBool "a recipient's stamp names a recipient"
          (stampNames content (solveStamp 0 bob msg))
        assertBool "a stranger's does not"
          (not (stampNames content (solveStamp 0 stranger msg)))

  , testCase "a restamp is the same message to gossip" $
      withStore $ \sto -> do
        alice <- aPeer
        (bobC, bobS) <- aPeer
        let bob = view peerSignPk bobC

        msg <- aMessage sto alice [bobS] (B8.pack "a letter")

        -- THE ONE THAT KEEPS PoW WORTH ANYTHING. The routing marker must not
        -- depend on the nonce: if it did, re-solving one message would look
        -- like a new message to every peer, and each fresh solution would buy
        -- another flood -- which is the amplification the work was supposed to
        -- price. Two nonces OF THE SAME STRENGTH, because the marker carries
        -- the work: a restamp is one message to gossip, and buying a second
        -- flood means buying a bit, which doubles the price.
        let bitsOf n = stampBits msg (MessageStamp1 bob n)
            sameAs b = [ n | n <- [1 .. 4096 :: Word64], bitsOf n == b ]
        case take 2 (sameAs (bitsOf 1)) of
          [n1, n2] -> stampMarker (MessageStamp1 bob n1) msg
                        @?= stampMarker (MessageStamp1 bob n2) msg
          _ -> assertFailure "no two nonces of equal strength in four thousand"

        -- AND A CHEAP STAMP DOES NOT SPEAK FOR AN EXPENSIVE ONE, which is what
        -- leaving the work out of the marker allowed. Anybody who saw a message
        -- could mint MessageStamp1 mbox 0 -- free, and forwarded wherever the
        -- peer's floor is zero, which is the default -- and race it ahead of the
        -- honest copy. Same marker, so the honest twenty-bit copy was `seen` and
        -- died at its sender's first hop, while the junk copy was refused for
        -- want of work at the host: the letter nowhere, twenty bits paid for it,
        -- and no rejection signal to notice with.
        let weak = head [ MessageStamp1 bob n | n <- [0 ..], bitsOf n == 0 ]
            strong = solveStamp 8 bob msg
        assertBool "the solved stamp carries at least what was asked" (stampBits msg strong >= 8)
        assertBool "a free stamp does not suppress a solved one"
          (stampMarker weak msg /= stampMarker strong msg)

        -- And it does depend on the mailbox, because two mailboxes are two
        -- deliveries: a copy stamped for one must still reach the other.
        (carolC, _) <- aPeer
        assertBool "two mailboxes are two markers"
          (stampMarker (MessageStamp1 bob 1) msg
             /= stampMarker (MessageStamp1 (view peerSignPk carolC) 1) msg)

  , testCase "the message key is the name the mailbox already uses" $
      withStore $ \sto -> do
        alice <- aPeer
        (_, bobS) <- aPeer

        msg <- aMessage sto alice [bobS] (B8.pack "a letter")

        -- Not a tautology, and the peer is why: 'mailboxInQ' derives the entry
        -- it stores from `hashObject (serialise m)` over the same value. One
        -- identity names the message for the work, for the marker and for the
        -- block it becomes, and the work is therefore paid for precisely the
        -- bytes that will occupy disk.
        messageKey msg @?= HashRef (hashObject @HbSync (serialise msg))

  , testCase "leading zero bits are counted across bytes" $ do
      leadingZeroBits (B8.pack "\x00\x00\xff") @?= 16
      leadingZeroBits (B8.pack "\x0f") @?= 4
      leadingZeroBits (B8.pack "\xff") @?= 0
      -- A digest that is all zeroes has no first one bit to stop at, so the
      -- count is the whole length rather than an error or a wrap.
      leadingZeroBits (B8.pack "\x00\x00") @?= 16
  ]

-- | What the peer's own config means, without a peer to read it.
--
-- Both of these decide how much a stranger can make this peer spend, and
-- neither had a test: 'poWFloorFrom' carried a haddock saying it was pure so it
-- could be tested without a peer, and 'replicateFromIn' is what stands between
-- an open inbox and a status tree somebody invented.
mailboxConfigTests :: TestTree
mailboxConfigTests = testGroup "PEP-21: the mailbox options a peer reads"
  [ testCase "no floor is a floor of zero, which forwards everything" $ do
      poWFloorFrom (conf "") @?= 0
      poWFloorFrom (conf "(hbs2:mailbox:pow-min 12)") @?= 12

  , testCase "the last clause wins, and one out of range is not a clamp" $ do
      poWFloorFrom (conf "(hbs2:mailbox:pow-min 4)\n(hbs2:mailbox:pow-min 20)") @?= 20
      -- 4096 clamped would be a floor nobody asked for, so it is ignored...
      poWFloorFrom (conf "(hbs2:mailbox:pow-min 4096)") @?= 0
      -- ...and ignoring it leaves whatever a readable clause said, which is
      -- worth pinning because "the last clause wins" and "this clause is not a
      -- clause" have to compose in exactly this order.
      poWFloorFrom (conf "(hbs2:mailbox:pow-min 7)\n(hbs2:mailbox:pow-min 4096)") @?= 7
      poWFloorFrom (conf "(hbs2:mailbox:pow-min -1)") @?= 0

  , testCase "no replication peers is nobody, not everybody" $
      -- The default that closes the hole: a mailbox charging work takes no
      -- replicated message until an operator names who it replicates with.
      HS.null (replicateFromIn @S (conf "")) @?= True

  , testCase "every named peer counts, and an unreadable one does not" $ do
      (alice, _) <- aPeer
      (bob, _)   <- aPeer
      let k c = show (pretty (AsBase58 (view peerSignPk c)))
          txt = "(hbs2:mailbox:replicate-from " <> k alice <> ")\n"
             <> "(hbs2:mailbox:replicate-from " <> k bob <> ")\n"
             <> "(hbs2:mailbox:replicate-from \"not a key\")\n"
          hosts = replicateFromIn @S (conf txt)
      -- A set and not a setting: the last clause does not win here, they all do.
      HS.size hosts @?= 2
      HS.member (view peerSignPk alice) hosts @?= True
      HS.member (view peerSignPk bob) hosts @?= True
  ]
  where
    conf :: String -> [Syntax C]
    conf = either (error . show) id . parseTop
