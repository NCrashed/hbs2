-- | The argument reading of @hub inbox accept@ (PEP-22 "Maintain").
--
-- Only the part that is a function. What the verb DOES needs a peer and a git
-- repository and is exercised through the layers under it: 'planCanon' in
-- "HBS2.Hub.RepoSpec", the sink in "HBS2.Hub.GitWriteSpec", the bridge in
-- "HBS2.Hub.BridgeSpec".
--
-- Worth its own module because this verb writes. Every other one reads, so a
-- misread argument costs a wrong listing; here it costs an event in canon,
-- attributed and published, that nobody can take back. Two of the three
-- arguments are keys of the same type, which is the shape that makes a swap
-- well-typed.
module HBS2.Hub.AcceptSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.CLI.Accept
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))
import HBS2.Hash (hashObject)
import HBS2.Data.Types.Refs (HashRef(..))

import Data.Config.Suckless
import Data.ByteString.Lazy.Char8 qualified as LBS
import Prettyprinter (pretty)
import Test.Hspec

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

-- The words as the argv reader produces them, which is the only shape the verb
-- ever sees. Building Syntax by hand here would test a form nobody types.
argv :: [String] -> [Syntax C]
argv = fmap argvAtom

b58 :: HubKey -> String
b58 = show . pretty . AsBase58

aHash :: String -> HashRef
aHash s = HashRef (hashObject (LBS.pack s))

spec :: Spec
spec = spec1 >> spec2

spec1 :: Spec
spec1 = do

  describe "PEP-22 hub inbox accept: arguments" $ do

    it "reads the flags in any order" $ do
      mbox <- aKey
      repo <- aKey
      let h = aHash "a message"
          want = Just (mbox, repo, h, Nothing)
      acceptArgs (argv ["--mailbox", b58 mbox, "--repo", b58 repo, "--message", show (pretty h)])
        `shouldBe` want
      acceptArgs (argv ["--repo", b58 repo, "--mailbox", b58 mbox, "--message", show (pretty h)])
        `shouldBe` want
      acceptArgs (argv ["--message", show (pretty h), "--repo", b58 repo, "--mailbox", b58 mbox])
        `shouldBe` want

    it "reads the delegate key when one is given" $ do
      mbox <- aKey
      repo <- aKey
      bob  <- aKey
      let h = aHash "a message"
      acceptArgs (argv [ "--mailbox", b58 mbox, "--repo", b58 repo
                       , "--as", b58 bob, "--message", show (pretty h) ])
        `shouldBe` Just (mbox, repo, h, Just bob)

    -- Both are keys, so a form missing one is not a form with one key in the
    -- wrong place: it is a form this verb must not act on at all.
    it "refuses a form with either key missing" $ do
      k <- aKey
      let h = aHash "a message"
      acceptArgs (argv ["--mailbox", b58 k, "--message", show (pretty h)]) `shouldBe` Nothing
      acceptArgs (argv ["--repo", b58 k, "--message", show (pretty h)]) `shouldBe` Nothing
      acceptArgs (argv [b58 k, b58 k, "--message", show (pretty h)]) `shouldBe` Nothing

    it "refuses a form with no message" $ do
      mbox <- aKey
      repo <- aKey
      acceptArgs (argv ["--mailbox", b58 mbox, "--repo", b58 repo]) `shouldBe` Nothing

    -- Picking the first of two would write to canon on a guess about which one
    -- the caller meant.
    it "refuses two messages rather than choosing one" $ do
      mbox <- aKey
      repo <- aKey
      acceptArgs (argv [ "--mailbox", b58 mbox, "--repo", b58 repo
                       , "--message", show (pretty (aHash "one")), "--message", show (pretty (aHash "two")) ])
        `shouldBe` Nothing

    -- WHY NOTHING IS POSITIONAL, stated as the fact that forces it: a sign key
    -- and a hash are both thirty-two bytes of base58, so each of the two
    -- patterns matches the other's value and no reading of a bare word can tell
    -- them apart. This test failed when the message was positional, and it
    -- failed by SUCCEEDING: `--repo <hash> <key>` parsed, with the hash taken as
    -- the repository and the key taken as the message.
    it "cannot tell a key from a hash, which is why every value is flagged" $ do
      k <- aKey
      let h = aHash "a message"
      -- Each really does match the other's pattern; if that ever stops being
      -- true the flags below are belt and braces rather than the whole belt.
      acceptArgs (argv ["--mailbox", b58 k, "--repo", b58 k, "--message", b58 k])
        `shouldSatisfy` (/= Nothing)
      acceptArgs (argv [ "--mailbox", show (pretty h), "--repo", show (pretty h)
                       , "--message", show (pretty h) ])
        `shouldSatisfy` (/= Nothing)
      -- And with nothing positional, a line missing a flag is refused rather
      -- than resolved by position.
      acceptArgs (argv ["--mailbox", b58 k, "--repo", show (pretty h), b58 k])
        `shouldBe` Nothing

    it "refuses a repeated flag rather than taking one of them" $ do
      mbox <- aKey
      repo <- aKey
      other <- aKey
      let h = aHash "a message"
      acceptArgs (argv [ "--mailbox", b58 mbox, "--repo", b58 repo
                       , "--repo", b58 other, "--message", show (pretty h) ])
        `shouldBe` Nothing

-- Whether an accept verifies anything at all. Three answers, and only one of
-- them means "run the bundle checks": getting it wrong either skips
-- verification on a real pull request or tries to verify an issue.
spec2 :: Spec
spec2 =
  describe "PEP-20 hub inbox accept: what there is to verify" $ do

    it "finds the bundle a pull request proposes" $ do
      repo <- aKey
      let part = aHash "the bundle"
          c = PRCoords Nothing "refs/heads/feature" "aa" "refs/heads/master" "bb"
                       (Just part)
      bundleOf (AOpen repo HubPR "t" [] Nothing Nothing (Just c) 1)
        `shouldBe` Just (part, "refs/heads/feature", "aa", "bb")

    it "finds it on a revise too, which proposes new coordinates" $ do
      let part = aHash "the second bundle"
          c = PRCoords Nothing "refs/heads/feature" "cc" "refs/heads/master" "bb"
                       (Just part)
      bundleOf (ARevise (aHash "thread") c 1)
        `shouldBe` Just (part, "refs/heads/feature", "cc", "bb")

    it "has nothing to verify for an issue" $ do
      repo <- aKey
      bundleOf (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
        `shouldBe` Nothing

    -- The fork-pointer path names a fork to fetch over hbs2 instead of
    -- carrying an attachment (PEP-20). Nothing to check HERE is not the same
    -- as nothing to check, and the difference is why this returns the parts
    -- rather than a Bool.
    it "has nothing to verify for a pull request that carries no bundle" $ do
      repo <- aKey
      let c = PRCoords (Just "hbs23://fork") "refs/heads/feature" "aa"
                       "refs/heads/master" "bb" Nothing
      bundleOf (AOpen repo HubPR "t" [] Nothing Nothing (Just c) 1)
        `shouldBe` Nothing

    it "has nothing to verify for a comment" $ do
      bundleOf (AComment (aHash "thread") Nothing (Just "hi") Nothing 1)
        `shouldBe` Nothing
