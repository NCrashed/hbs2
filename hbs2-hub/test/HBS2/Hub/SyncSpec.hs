-- | @hub sync@: what it says and what it exits with.
--
-- The fetching itself is tested against real git in "HBS2.Hub.GitBundleSpec";
-- what is here is the half a person and a script read. Both matter for one
-- outcome in particular: a divergence is not a failed fetch, and a caller that
-- reads it as one either retries forever or folds a canon missing what this
-- clone holds.
module HBS2.Hub.SyncSpec (spec) where

import HBS2.Hub.Repo.GitBundle (Synced(..),SyncedCanon(..))
import HBS2.Hub.CLI.Sync
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Prelude.Plated (Doc)

import Data.Config.Suckless (Syntax,C)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Test.Hspec

shown :: [Doc ()] -> String
shown = unlines . fmap show

argv :: [String] -> [Syntax C]
argv = fmap argvAtom

synced :: SyncedCanon -> Synced
synced c = Synced c True

spec :: Spec
spec = do

  describe "PEP-22 hub sync: the report" $ do

    it "says which remote it fetched, and that the pull refs came" $ do
      let out = shown (syncDoc "origin" (synced CanonSame) (Left CanonSame))
      out `shouldSatisfy` isInfixOf "fetched from origin"
      out `shouldSatisfy` isInfixOf "pulls: fetched"

    -- The three quiet outcomes are told apart, because they call for different
    -- things: wait, nothing, and nothing.
    it "tells no canon there from no change here" $ do
      shown (syncDoc "origin" (synced CanonNone) (Left CanonNone))
        `shouldSatisfy` isInfixOf "the remote has none"
      shown (syncDoc "origin" (synced CanonSame) (Left CanonSame))
        `shouldSatisfy` isInfixOf "already here"

    it "prints where canon moved from and to, and says new when it was absent" $ do
      shown (syncDoc "origin" (synced (CanonMoved "aaa" "bbb")) (Left (CanonMoved "aaa" "bbb")))
        `shouldSatisfy` isInfixOf "aaa -> bbb"
      shown (syncDoc "origin" (synced (CanonMoved "" "bbb")) (Left (CanonMoved "" "bbb")))
        `shouldSatisfy` isInfixOf "new -> bbb"

    -- Both hashes and the command, because the recovery is a git invocation
    -- over values neither of which is derivable from the other.
    it "prints both sides of a divergence and how to resolve it by hand" $ do
      let out = shown (syncDoc "origin" (synced (CanonDiverged "mine" "theirs")) (Left (CanonDiverged "mine" "theirs")))
      out `shouldSatisfy` isInfixOf "DIVERGED"
      out `shouldSatisfy` isInfixOf "nothing was written"
      out `shouldSatisfy` isInfixOf "mine"
      out `shouldSatisfy` isInfixOf "theirs"
      out `shouldSatisfy` isInfixOf "git update-ref refs/hbs2/meta theirs"

  describe "PEP-22 hub sync: the exit code" $ do

    it "is zero for every outcome that is not a divergence" $ do
      map (\c -> syncCode (synced c) (Left c)) [CanonNone, CanonSame, CanonMoved "a" "b"]
        `shouldBe` [0, 0, 0]

    -- Its own code, not a general failure: the fetch worked, and one ref was
    -- deliberately left alone.
    it "is its own number when canon diverged" $ do
      syncCode (synced (CanonDiverged "a" "b")) (Left (CanonDiverged "a" "b"))
        `shouldBe` codeDiverged
      codeDiverged `shouldSatisfy` (/= 0)

  describe "PEP-22 hub sync: arguments" $ do

    it "takes no arguments at all, and defaults the remote later" $ do
      syncArgs (argv []) `shouldBe` Just (SyncArgs Nothing Nothing)

    it "takes a remote when one is named" $ do
      syncArgs (argv ["--remote", "upstream"])
        `shouldBe` Just (SyncArgs (Just "upstream") Nothing)
      -- A remote may be called 2026, and argvAtom keeps that as a number.
      syncArgs (argv ["--remote", "2026"]) `shouldBe` Just (SyncArgs (Just "2026") Nothing)

    it "refuses a positional value and an unknown flag" $ do
      syncArgs (argv ["origin"]) `shouldBe` Nothing
      syncArgs (argv ["--remote", "origin", "--prune"]) `shouldBe` Nothing
      syncArgs (argv ["--remote", "a", "--remote", "b"]) `shouldBe` Nothing

  describe "PEP-19 hub sync: what a divergence turned out to be" $ do

    -- The case that makes compaction and non-forcing sync compatible: the
    -- remote rewrote canon, both lineages fold to the same state, the ref
    -- moved. A caller has nothing to do, so the code is zero.
    it "takes a rewrite, says what is gone, and exits zero" $ do
      let s = Rewritten "old" "new"
          out = shown (syncDoc "origin" (synced (CanonDiverged "old" "new")) (Right s))
      out `shouldSatisfy` isInfixOf "folds to the same state"
      out `shouldSatisfy` isInfixOf "old"
      out `shouldSatisfy` isInfixOf "new"
      -- What was traded away is named, and so is the way back.
      out `shouldSatisfy` isInfixOf "timeline of overwritten values"
      out `shouldSatisfy` isInfixOf "git update-ref refs/hbs2/meta old"
      syncCode (synced (CanonDiverged "old" "new")) (Right s) `shouldBe` 0

    -- And the case it exists to tell apart. Nothing was written, and the code
    -- says a person is needed.
    it "keeps a fork non-zero, and says the two do not agree" $ do
      let s = Forked "mine" "theirs"
          out = shown (syncDoc "origin" (synced (CanonDiverged "mine" "theirs")) (Right s))
      out `shouldSatisfy` isInfixOf "do not say the same thing"
      out `shouldSatisfy` isInfixOf "Nothing was written"
      out `shouldSatisfy` not . isInfixOf "folds to the same state"
      syncCode (synced (CanonDiverged "mine" "theirs")) (Right s) `shouldBe` codeDiverged

    -- A canon this clone cannot fold says nothing about the other, and the
    -- verb that reports why is `hub verify`.
    it "says nothing either way when one lineage will not fold" $ do
      let s = Unreadable "mine" "theirs"
          out = shown (syncDoc "origin" (synced (CanonDiverged "mine" "theirs")) (Right s))
      out `shouldSatisfy` isInfixOf "will not fold"
      out `shouldSatisfy` isInfixOf "hub verify"
      syncCode (synced (CanonDiverged "a" "b")) (Right s) `shouldBe` codeDiverged

    -- A rewrite whose write lost the race is a RETRY, and a caller that read it
    -- as success would carry on against canon it did not take.
    it "keeps a rewrite that did not land non-zero, and says to run it again" $ do
      let s = NotTaken "mine" "theirs" "the ref moved"
          out = shown (syncDoc "origin" (synced (CanonDiverged "mine" "theirs")) (Right s))
      out `shouldSatisfy` isInfixOf "would not move"
      out `shouldSatisfy` isInfixOf "Run it again"
      syncCode (synced (CanonDiverged "a" "b")) (Right s) `shouldBe` codeDiverged

    it "reads a repository for the check, which is what buys the difference" $ do
      -- A key-shaped word, since the reader takes a key and nothing else.
      let k = "5xhFoc2rEnV87GrAZzkTNGppBNKuAft363uR12Bfbqu8"
      fmap saRepo (syncArgs (argv ["--repo", k])) `shouldSatisfy` isJust
      fmap saRepo (syncArgs (argv [])) `shouldBe` Just Nothing
      syncArgs (argv ["--repo", "not-a-key"]) `shouldBe` Nothing
