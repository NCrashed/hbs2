-- | The triage deny-list (PEP-21 "Deny-lists").
--
-- The layer that decides what enters canon, and the one thing here that is
-- deliberately NOT canon: PEP-21 defers a published ban to hub-meta 2, so this
-- is local, unsigned state. What can be asked of it is what it holds, what it
-- refuses to read, and who it lets through.
module HBS2.Hub.BanSpec (spec) where

import HBS2.Hub.Types (HubKey)
import HBS2.Hub.CLI.Ban
import HBS2.Hub.Deny
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
spec = keyNames >> do

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

-- | @--key@ MEANT FOUR DIFFERENT KEYS.
--
-- An inner author at `ban`, an envelope key at `block`, a canon signer at
-- `maintainer add`, a person at `assign --to` -- all thirty-two bytes of base58,
-- so every swap between them was well-typed and silent. The verbs keep their
-- names, which say what they DO, and the flag says which layer, which is the
-- half that was missing: a reader who has the flag right cannot have the layer
-- wrong.
--
-- Both spellings, for one release. The old one still works and is no longer
-- printed, exactly as `--target` is handled.
keyNames :: Spec
keyNames =
  describe "PEP-22 the four keys that were all called --key" $ do

    it "reads a ban under its own name and under the old one" $ do
      repo <- aKey ; who <- aKey
      let want = Just (BanArgs repo (Just who))
      banArgs (argv ["--repo", b58 repo, "--author-key", b58 who]) `shouldBe` want
      banArgs (argv ["--repo", b58 repo, "--key", b58 who]) `shouldBe` want

    -- The repository's flags come from 'repoFlags' now, and this is the bug
    -- that fixes: `ban` built its flag list by hand, so --target worked on
    -- `ban list` and not on `ban`, one verb apart.
    it "takes --target on the verb as well as on the listing" $ do
      repo <- aKey ; who <- aKey
      banArgs (argv ["--target", b58 repo, "--author-key", b58 who])
        `shouldBe` Just (BanArgs repo (Just who))

    -- Two spellings of one value, so a line carrying both is a line somebody
    -- edited half way, and choosing between them is a guess.
    it "refuses both spellings at once" $ do
      repo <- aKey ; who <- aKey ; other <- aKey
      banArgs (argv ["--repo", b58 repo, "--author-key", b58 who, "--key", b58 other])
        `shouldBe` Nothing
      banArgs (argv ["--repo", b58 repo, "--target", b58 other, "--author-key", b58 who])
        `shouldBe` Nothing
