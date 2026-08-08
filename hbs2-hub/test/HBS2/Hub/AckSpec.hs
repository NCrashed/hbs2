-- | Whether a reply channel's sigil belongs to the key that asked for the
-- reply (PEP-18 "back-channel").
--
-- WHY THIS IS ITS OWN RULE. A reply channel is a key AND a sigil hash, and
-- @openLetterAs@ can only check the key: it is pure, and deciding the sigil
-- means reading it. Meanwhile the sigil is the half that decides where the
-- message goes -- @resolveKeys@ takes the recipient's sign key and its
-- encryption key out of the sigil's own signed box and never looks at the key
-- beside it. So an unchecked sigil made every accepted letter a
-- maintainer-signed message to an address of the sender's choosing.
--
-- These drive the rule directly, which is the point of it being a function: the
-- old arrangement had nowhere to ask the question, and the test that existed
-- passed one constant sigil into every case.
module HBS2.Hub.AckSpec (spec) where

import HBS2.Hub.CLI.Ack (ackTarget,AckTrouble(..))
import HBS2.Hub.Letter (sigilNames,ReplyChannel(..))
import HBS2.Hash (hashObject)
import HBS2.Data.Types.Refs (HashRef(..))

import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil (Sigil,makeSigilFromCredentials)

import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Test.Hspec

-- | Credentials with an encryption key, which is what a sigil needs.
--
-- 'newCredentials' alone has an empty keyring, and a sigil binds exactly one
-- encryption key, so without the added pair there is nothing to make one from.
someone :: IO (PeerCredentials 'HBS2Basic)
someone = newCredentials @'HBS2Basic >>= addKeyPair Nothing

-- | That identity's own sigil.
--
-- Built with the same helper 'sendAck' uses for the hub's own, so the fixture
-- and the code under test cannot disagree about what a sigil is.
sigilOf :: PeerCredentials 'HBS2Basic -> Sigil 'HBS2Basic
sigilOf c = fromJust (makeSigilFromCredentials @'HBS2Basic c enc Nothing Nothing)
  where
    enc = head [ _krPk e | e <- _peerKeyring c ]

spec :: Spec
spec = do

  describe "PEP-18 back-channel: whose mailbox the reply sigil names" $ do

    it "says yes to a sigil made from the asking key's own credentials" $ do
      alice <- someone
      sigilNames (_peerSignPk alice) (Just (sigilOf alice)) `shouldBe` Just True

    -- THE ONE THE HUB TURNED ON. A letter naming its own author and somebody
    -- else's sigil passed every check there was, and the ack went to the
    -- somebody else, signed by the repository.
    it "says no to a sigil that names a different key" $ do
      alice <- someone
      victim <- someone
      sigilNames (_peerSignPk alice) (Just (sigilOf victim)) `shouldBe` Just False

    -- Distinct from "no" on purpose: nothing has been established either way,
    -- and the send reports its own failure to resolve. Answering False here
    -- would accuse a contributor whose sigil block simply has not arrived.
    it "says nothing about a sigil it cannot read" $ do
      alice <- someone
      sigilNames (_peerSignPk alice) Nothing `shouldBe` Nothing

    it "is about the sigil and not about the key being well formed" $ do
      -- Two different keys, each with its own sigil: neither answer depends on
      -- which key was asked about, only on which sigil it is paired with.
      alice <- someone
      bob   <- someone
      sigilNames (_peerSignPk bob)   (Just (sigilOf bob))   `shouldBe` Just True
      sigilNames (_peerSignPk bob)   (Just (sigilOf alice)) `shouldBe` Just False
      sigilNames (_peerSignPk alice) (Just (sigilOf bob))   `shouldBe` Just False

  -- And what the verb DOES with that answer, which is the half that was
  -- reachable from no test: the rule can be right and still be consulted and
  -- ignored, which is what the previous arrangement amounted to.
  describe "PEP-18 back-channel: what an accept does with the answer" $ do

    it "sends nothing when no reply was asked for" $ do
      ackTarget NoReply Nothing        `shouldBe` Left AckNotAsked
      -- Whatever a sigil would have said. There is no sigil.
      ackTarget NoReply (Just True)    `shouldBe` Left AckNotAsked

    it "refuses a sigil that resolves to somebody else" $ do
      k <- _peerSignPk <$> someone
      ackTarget (ReplyTo k aSigil) (Just False)
        `shouldBe` Left (AckWrongSigil k aSigil)

    it "sends to the sigil the channel names when it is the asker's own" $ do
      k <- _peerSignPk <$> someone
      ackTarget (ReplyTo k aSigil) (Just True) `shouldBe` Right aSigil

    -- The case that must NOT be an accusation. A sigil block that has not
    -- arrived is ordinary, and the send reports its own failure with a reason;
    -- refusing here would tell a contributor their channel was forged.
    it "proceeds when the sigil could not be read at all" $ do
      k <- _peerSignPk <$> someone
      ackTarget (ReplyTo k aSigil) Nothing `shouldBe` Right aSigil

  where
    -- Any hash: what these ask about is which branch is taken, and the sigil's
    -- content has already been reduced to the Maybe Bool beside it.
    aSigil = HashRef (hashObject ("a sigil" :: ByteString))
