-- | @hub whoami@: the verb that answers "am I set up to send anything".
--
-- What is asserted here is the ARGUMENT reader and the paragraph, both pure.
-- The two checks the verb performs are tested where they live: 'sigilNames' in
-- AckSpec, since it is the same rule the hub applies before acking, and the
-- keyman lookup is a database this suite has no business standing up.
--
-- The paragraph is asserted at all because it is the answer to the finding that
-- prompted this verb: three values are needed, three different tools make them,
-- and no help text named any of the three. A paragraph that quietly lost one of
-- those names would leave the gap exactly where it was.
module HBS2.Hub.WhoamiSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.CLI.Whoami

import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))
import HBS2.Prelude.Plated (pretty,fromString)

import Data.Config.Suckless
import Data.List (isInfixOf)
import Test.Hspec

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

sym :: String -> Syntax C
sym = mkSym

said :: [HubKey] -> String
said = show . identityDoc

spec :: Spec
spec = do

  describe "PEP-22 hub whoami: arguments" $ do

    it "reads nothing at all, which is the call somebody with nothing makes" $ do
      whoamiArgs @C [] `shouldBe` Just (WhoamiArgs Nothing Nothing Nothing)

    it "reads the pair, in either order" $ do
      k <- aKey
      let ks = show (pretty (AsBase58 k))
          h  = "8yqJyq5jxKmDdMwzGvQhx3srKuc1FqmxCYSZKz5yzXWJ"
      whoamiArgs @C [sym "--author", sym ks, sym "--sender", sym h]
        `shouldBe` Just (WhoamiArgs (Just k) (Just (fromString h)) Nothing)
      whoamiArgs @C [sym "--sender", sym h, sym "--author", sym ks]
        `shouldBe` Just (WhoamiArgs (Just k) (Just (fromString h)) Nothing)

    it "reads --repo, and --target as its alias" $ do
      k <- aKey
      let ks = show (pretty (AsBase58 k))
      whoamiArgs @C [sym "--repo", sym ks]
        `shouldBe` Just (WhoamiArgs Nothing Nothing (Just k))
      whoamiArgs @C [sym "--target", sym ks]
        `shouldBe` Just (WhoamiArgs Nothing Nothing (Just k))

    it "refuses a flag it does not have" $ do
      -- The guard every verb in this package has: an unknown flag is a typo for
      -- one that exists, and silently ignoring it answers a question nobody
      -- asked.
      whoamiArgs @C [sym "--mailbox", sym "whatever"] `shouldBe` Nothing

    it "refuses a value that is not a key or not a hash" $ do
      whoamiArgs @C [sym "--author", sym "not-a-key"] `shouldBe` Nothing
      whoamiArgs @C [sym "--sender", sym "not-a-hash"] `shouldBe` Nothing

    it "refuses a flag with no value" $ do
      whoamiArgs @C [sym "--author"] `shouldBe` Nothing

  describe "PEP-22 hub whoami: what it tells somebody with nothing" $ do

    it "names the tool for each of the three things" $ do
      k <- aKey
      let s = said [k]
      s `shouldSatisfy` isInfixOf "hbs2-cli hbs2:sigil:create:from-keyring"
      s `shouldSatisfy` isInfixOf "hbs2-peer mailbox create"
      s `shouldSatisfy` isInfixOf "hbs2-keyman"

    it "says the author key and the sender sigil have to agree" $ do
      -- The one fact nothing else in the tool states, and the reason a letter
      -- can fold and never be answered.
      k <- aKey
      said [k] `shouldSatisfy` isInfixOf "HAVE TO AGREE"
      said [k] `shouldSatisfy` isInfixOf "whoami --author"

    it "prints the keys it found" $ do
      k <- aKey
      said [k] `shouldSatisfy` isInfixOf (show (pretty (AsBase58 k)))

    it "says so when there are none, instead of an empty list" $ do
      -- An empty heading over nothing reads as a bug in the verb rather than an
      -- answer about the machine.
      said [] `shouldSatisfy` isInfixOf "no signing key"
      said [] `shouldSatisfy` isInfixOf "hbs2-keyman"
