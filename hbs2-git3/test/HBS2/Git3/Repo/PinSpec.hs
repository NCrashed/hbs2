-- | Which clones the remote helper names for hbs2-hub (PEP-25): a working
-- copy, and not a bare clone, a linked worktree or a throwaway.
module HBS2.Git3.Repo.PinSpec (spec) where

import HBS2.Git3.Repo.Pin

import Test.Hspec

excluded :: [FilePath]
excluded = ["/tmp", "/home/u/.cache"]

spec :: Spec
spec = describe "PEP-25 the clone pin" $ do

  it "names the working tree of a .git directory" $ do
    pinnableDir excluded "/home/u/dev/hbs2/.git" `shouldBe` Just "/home/u/dev/hbs2"
    pinnableDir excluded "/home/u/dev/hbs2/.git/" `shouldBe` Just "/home/u/dev/hbs2"

  it "names no bare clone and no linked worktree" $ do
    pinnableDir excluded "/home/u/dev/hbs2.git" `shouldBe` Nothing
    pinnableDir excluded "/home/u/dev/hbs2/.git/worktrees/wt" `shouldBe` Nothing

  it "names nothing under a temporary or a cache directory" $ do
    pinnableDir excluded "/tmp/x/.git" `shouldBe` Nothing
    pinnableDir excluded "/home/u/.cache/hbs2-hub/repos/K/.git" `shouldBe` Nothing

  it "is not fooled by a name that only begins like an excluded one" $
    pinnableDir excluded "/tmpfiles/x/.git" `shouldBe` Just "/tmpfiles/x"

  it "wants an absolute path" $
    pinnableDir excluded ".git" `shouldBe` Nothing

  it "keys the pin by the repository" $
    pinKey "K" `shouldBe` "hbs2-hub.K.clone"
