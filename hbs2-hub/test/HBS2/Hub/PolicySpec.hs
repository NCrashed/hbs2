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
