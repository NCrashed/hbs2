-- | What @hub block@ and @hub unblock@ decide (PEP-21 peer layer).
--
-- Two things, and both are about a file that gets hashed and versioned: what
-- one clause does to a policy, and what a policy renders as. A verb that
-- produced a different file for the same policy would bump a version and
-- republish to every peer holding the mailbox on every run.
module HBS2.Hub.PolicySpec (spec) where

import HBS2.Hub.Types (HubKey)
import HBS2.Hub.CLI.Policy
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))
import HBS2.Peer.Proto.Mailbox.Policy.Basic

import Data.Config.Suckless
import Data.HashMap.Strict qualified as HM
import Data.List (isInfixOf)
import Data.Text qualified as Text
import Prettyprinter (pretty)
import Test.Hspec

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

argv :: [String] -> [Syntax C]
argv = fmap argvAtom

b58 :: HubKey -> String
b58 = show . pretty . AsBase58

spec :: Spec
spec = do

  describe "PEP-21 peer layer: one clause at a time" $ do

    it "denies a sender, and says so in the file" $ do
      k <- aKey
      let p = denying True k (defaultBasicPolicy @'HBS2Basic)
      HM.lookup k (bpSenders p) `shouldBe` Just Deny
      Text.unpack (policyText p) `shouldSatisfy` (("sender deny " <> b58 k) `isInfixOf`)

    -- Idempotence is not a nicety here. The verb writes a file, hashes it and
    -- bumps a version, and a version bump republishes to every peer holding
    -- the mailbox: blocking a key already blocked must produce the same policy
    -- so the verb can decline to write anything.
    it "blocking twice is blocking once" $ do
      k <- aKey
      let once  = denying True k (defaultBasicPolicy @'HBS2Basic)
          twice = denying True k once
      policyText twice `shouldBe` policyText once

    -- Removed, not set to Allow. A clause saying Allow beside a default of
    -- Allow says nothing and never goes away, and against a default of Deny it
    -- would be a permission the operator did not ask this verb for.
    it "unblocking removes the clause rather than inverting it" $ do
      k <- aKey
      let p = denying False k (denying True k (defaultBasicPolicy @'HBS2Basic))
      HM.lookup k (bpSenders p) `shouldBe` Nothing
      policyText p `shouldBe` policyText (defaultBasicPolicy @'HBS2Basic)

    it "unblocking somebody who was never blocked changes nothing" $ do
      k <- aKey
      policyText (denying False k (defaultBasicPolicy @'HBS2Basic))
        `shouldBe` policyText (defaultBasicPolicy @'HBS2Basic)

    it "leaves the other senders alone" $ do
      a <- aKey ; b <- aKey
      let p = denying True b (denying True a (defaultBasicPolicy @'HBS2Basic))
          p' = denying False a p
      HM.lookup b (bpSenders p') `shouldBe` Just Deny
      HM.lookup a (bpSenders p') `shouldBe` Nothing

  describe "PEP-21 peer layer: the file it writes" $ do

    -- getAsSyntax renders from two HashMaps, so its order is hash order. This
    -- text is hashed and versioned, so the same policy has to give the same
    -- bytes however the map was built.
    it "renders one policy as one text, whatever order it was built in" $ do
      a <- aKey ; b <- aKey ; c <- aKey
      let one = denying True c (denying True b (denying True a base))
          other = denying True a (denying True c (denying True b base))
          base = defaultBasicPolicy @'HBS2Basic
      policyText one `shouldBe` policyText other

    it "says what the default is, so an empty policy is not read as open" $ do
      let t = Text.unpack (policyText (defaultBasicPolicy @'HBS2Basic))
      t `shouldSatisfy` ("sender deny all" `isInfixOf`)
      t `shouldSatisfy` ("peer deny all" `isInfixOf`)

  describe "PEP-21 peer layer: arguments" $ do

    it "reads the mailbox alone, for show" $ do
      mbox <- aKey
      policyArgs (argv ["--mailbox", b58 mbox]) `shouldBe` Just (PolicyArgs mbox Nothing)

    it "reads the key when one is given" $ do
      mbox <- aKey ; k <- aKey
      policyArgs (argv ["--mailbox", b58 mbox, "--key", b58 k])
        `shouldBe` Just (PolicyArgs mbox (Just k))

    it "refuses a form with no mailbox" $ do
      k <- aKey
      policyArgs (argv ["--key", b58 k]) `shouldBe` Nothing
      policyArgs (argv [b58 k]) `shouldBe` Nothing

    it "refuses a repeated flag rather than choosing one" $ do
      mbox <- aKey ; other <- aKey
      policyArgs (argv ["--mailbox", b58 mbox, "--mailbox", b58 other])
        `shouldBe` Nothing

  describe "PEP-21 peer layer: the proof-of-work floor" $ do

    it "writes the floor into the file, and takes it out again" $ do
      let p = withPoW 12 (defaultBasicPolicy @'HBS2Basic)
      Text.unpack (policyText p) `shouldSatisfy` isInfixOf "(pow 12)"
      -- Zero IS absent: bpPoW is documented as zero when unset, so clearing
      -- and setting to zero are one operation, and a floor of zero bits is
      -- work every message has already done.
      (withPoW 0 p == defaultBasicPolicy @'HBS2Basic) `shouldBe` True

    -- A re-run that changes nothing must produce the same policy: the verb
    -- compares before publishing, and a version bump republishes the file to
    -- every peer holding the mailbox.
    it "is idempotent, so re-running it publishes nothing" $ do
      let p = withPoW 12 (defaultBasicPolicy @'HBS2Basic)
      (withPoW 12 p == p) `shouldBe` True

    it "changes nothing else about the policy" $ do
      k <- aKey
      let p = denying True k (defaultBasicPolicy @'HBS2Basic)
      (bpSenders (withPoW 12 p) == bpSenders p) `shouldBe` True

  describe "PEP-21 peer layer: the two defaults" $ do

    it "sets each half on its own and leaves the other alone" $ do
      let p0 = withDefaults (Just Allow) (Just Allow) (defaultBasicPolicy @'HBS2Basic)
          p1 = withDefaults (Just Deny) Nothing p0
      bpDefaultSenderAction p1 `shouldBe` Deny
      bpDefaultPeerAction p1 `shouldBe` Allow
      let p2 = withDefaults Nothing (Just Deny) p0
      bpDefaultSenderAction p2 `shouldBe` Allow
      bpDefaultPeerAction p2 `shouldBe` Deny

    it "naming neither is the policy unchanged" $ do
      let p = withDefaults (Just Allow) (Just Deny) (defaultBasicPolicy @'HBS2Basic)
      (withDefaults Nothing Nothing p == p) `shouldBe` True

    it "says both defaults in the file it writes" $ do
      let out = Text.unpack (policyText (withDefaults (Just Allow) (Just Allow)
                                          (defaultBasicPolicy @'HBS2Basic)))
      out `shouldSatisfy` isInfixOf "(sender allow all)"
      out `shouldSatisfy` isInfixOf "(peer allow all)"

  describe "PEP-21 peer layer: arguments of the new verbs" $ do

    it "reads a floor, in either order" $ do
      mbox <- aKey
      powArgs (argv ["--mailbox", b58 mbox, "--bits", "12"])
        `shouldBe` Just (mbox, 12)
      powArgs (argv ["--bits", "12", "--mailbox", b58 mbox])
        `shouldBe` Just (mbox, 12)

    -- The difficulty is a Word8 on the wire. Truncating 256 to 0 would leave
    -- an operator believing the mailbox charges the most work possible while
    -- it charges none.
    it "refuses a floor that is not one, rather than truncating it" $ do
      mbox <- aKey
      let with n = powArgs (argv ["--mailbox", b58 mbox, "--bits", n])
      with "0" `shouldBe` Just (mbox, 0)
      with "255" `shouldBe` Just (mbox, 255)
      with "256" `shouldBe` Nothing
      with "-1" `shouldBe` Nothing
      with "lots" `shouldBe` Nothing

    it "refuses a floor with no mailbox, and an unknown flag" $ do
      mbox <- aKey
      powArgs (argv ["--bits", "12"]) `shouldBe` Nothing
      powArgs (argv ["--mailbox", b58 mbox, "--bits", "12", "--force"])
        `shouldBe` Nothing

    it "reads the defaults, either or both" $ do
      mbox <- aKey
      defaultArgs (argv ["--mailbox", b58 mbox, "--sender", "allow", "--peer", "deny"])
        `shouldBe` Just (mbox, Just Allow, Just Deny)
      defaultArgs (argv ["--mailbox", b58 mbox, "--peer", "allow"])
        `shouldBe` Just (mbox, Nothing, Just Allow)
      defaultArgs (argv ["--mailbox", b58 mbox])
        `shouldBe` Just (mbox, Nothing, Nothing)

    -- The two words the policy file itself uses, and nothing else: a reader
    -- that guessed at "yes" or "open" would be guessing which way a mailbox
    -- faces.
    it "takes allow and deny and no synonym for either" $ do
      mbox <- aKey
      let with v = defaultArgs (argv ["--mailbox", b58 mbox, "--sender", v])
      with "allow" `shouldBe` Just (mbox, Just Allow, Nothing)
      with "deny" `shouldBe` Just (mbox, Just Deny, Nothing)
      with "yes" `shouldBe` Nothing
      with "open" `shouldBe` Nothing
      with "Allow" `shouldBe` Nothing
