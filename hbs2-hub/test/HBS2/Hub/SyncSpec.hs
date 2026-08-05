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
      let out = shown (syncDoc "origin" (synced CanonSame))
      out `shouldSatisfy` isInfixOf "fetched from origin"
      out `shouldSatisfy` isInfixOf "pulls: fetched"

    -- The three quiet outcomes are told apart, because they call for different
    -- things: wait, nothing, and nothing.
    it "tells no canon there from no change here" $ do
      shown (syncDoc "origin" (synced CanonNone))
        `shouldSatisfy` isInfixOf "the remote has none"
      shown (syncDoc "origin" (synced CanonSame))
        `shouldSatisfy` isInfixOf "already here"

    it "prints where canon moved from and to, and says new when it was absent" $ do
      shown (syncDoc "origin" (synced (CanonMoved "aaa" "bbb")))
        `shouldSatisfy` isInfixOf "aaa -> bbb"
      shown (syncDoc "origin" (synced (CanonMoved "" "bbb")))
        `shouldSatisfy` isInfixOf "new -> bbb"

    -- Both hashes and the command, because the recovery is a git invocation
    -- over values neither of which is derivable from the other.
    it "prints both sides of a divergence and how to resolve it by hand" $ do
      let out = shown (syncDoc "origin" (synced (CanonDiverged "mine" "theirs")))
      out `shouldSatisfy` isInfixOf "DIVERGED"
      out `shouldSatisfy` isInfixOf "nothing was written"
      out `shouldSatisfy` isInfixOf "mine"
      out `shouldSatisfy` isInfixOf "theirs"
      out `shouldSatisfy` isInfixOf "git update-ref refs/hbs2/meta theirs"

  describe "PEP-22 hub sync: the exit code" $ do

    it "is zero for every outcome that is not a divergence" $ do
      map (syncCode . synced) [CanonNone, CanonSame, CanonMoved "a" "b"]
        `shouldBe` [0, 0, 0]

    -- Its own code, not a general failure: the fetch worked, and one ref was
    -- deliberately left alone.
    it "is its own number when canon diverged" $ do
      syncCode (synced (CanonDiverged "a" "b")) `shouldBe` codeDiverged
      codeDiverged `shouldSatisfy` (/= 0)

  describe "PEP-22 hub sync: arguments" $ do

    it "takes no arguments at all, and defaults the remote later" $ do
      syncArgs (argv []) `shouldBe` Just (SyncArgs Nothing)

    it "takes a remote when one is named" $ do
      syncArgs (argv ["--remote", "upstream"]) `shouldBe` Just (SyncArgs (Just "upstream"))
      -- A remote may be called 2026, and argvAtom keeps that as a number.
      syncArgs (argv ["--remote", "2026"]) `shouldBe` Just (SyncArgs (Just "2026"))

    it "refuses a positional value and an unknown flag" $ do
      syncArgs (argv ["origin"]) `shouldBe` Nothing
      syncArgs (argv ["--remote", "origin", "--prune"]) `shouldBe` Nothing
      syncArgs (argv ["--remote", "a", "--remote", "b"]) `shouldBe` Nothing
