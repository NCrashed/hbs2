-- | The triage deny-list (PEP-21 "Deny-lists").
--
-- The layer that decides what enters canon, and the one thing here that is
-- deliberately NOT canon: PEP-21 defers a published ban to hub-meta 2, so this
-- is local, unsigned state. What can be asked of it is what it holds, what it
-- refuses to read, and who it lets through.
module HBS2.Hub.BanSpec (spec) where

import HBS2.Hub.Types (HubKey)
import HBS2.Hub.CLI.Ban
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))

import Data.Config.Suckless
import Data.HashSet qualified as HS
import Data.List (isInfixOf)
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

  describe "PEP-21 triage layer: the list" $ do

    it "round-trips through the file it is stored as" $ do
      a <- aKey ; b <- aKey
      let ks = HS.fromList [a,b]
      parseBans (renderBans ks) `shouldBe` Right ks

    it "writes one file whatever order the set was built in" $ do
      a <- aKey ; b <- aKey
      renderBans (HS.fromList [a,b]) `shouldBe` renderBans (HS.fromList [b,a])

    it "reads an empty file as nobody" $
      parseBans "" `shouldBe` Right HS.empty

    -- A deny-list that quietly drops what it cannot read is one an attacker
    -- shortens by writing something odd into it. Every line has to parse or
    -- the whole file is refused.
    it "refuses a file it cannot read rather than reading part of it" $ do
      a <- aKey
      let good = renderBans (HS.fromList [a])
      parseBans (good <> "(ban not-a-key)\n") `shouldSatisfy` isLeft
      parseBans (good <> "(banish x)\n") `shouldSatisfy` isLeft
      parseBans "«" `shouldSatisfy` isLeft

    it "lets everybody through when nobody is banned" $ do
      a <- aKey
      allowedBy HS.empty a `shouldBe` True

    it "stops exactly the keys it holds" $ do
      a <- aKey ; b <- aKey
      let bans = HS.fromList [a]
      allowedBy bans a `shouldBe` False
      allowedBy bans b `shouldBe` True

  describe "PEP-21 triage layer: where it lives" $

    -- Outside the working tree, so that a list this build cannot publish does
    -- not get committed by somebody's `git add -A`. Keyed by repository,
    -- because one node may serve two and must not confuse them.
    it "keys the file by repository, outside any repository" $ do
      a <- aKey ; b <- aKey
      pa <- banPath a
      pb <- banPath b
      pa `shouldSatisfy` (/= pb)
      pa `shouldSatisfy` (b58 a `isInfixOf`)
      pa `shouldSatisfy` ("hbs2-hub" `isInfixOf`)

  describe "PEP-21 triage layer: arguments" $ do

    it "reads the repo alone, for list" $ do
      repo <- aKey
      banArgs (argv ["--repo", b58 repo]) `shouldBe` Just (BanArgs repo Nothing)

    it "reads the author when one is given" $ do
      repo <- aKey ; k <- aKey
      banArgs (argv ["--repo", b58 repo, "--key", b58 k])
        `shouldBe` Just (BanArgs repo (Just k))

    it "refuses a form with no repository" $ do
      k <- aKey
      banArgs (argv ["--key", b58 k]) `shouldBe` Nothing

    it "refuses a repeated flag rather than choosing one" $ do
      repo <- aKey ; other <- aKey
      banArgs (argv ["--repo", b58 repo, "--repo", b58 other]) `shouldBe` Nothing

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)
