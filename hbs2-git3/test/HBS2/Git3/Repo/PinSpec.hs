-- | Which clones the remote helper names for hbs2-hub (PEP-25): a working
-- copy, and not a bare clone, a linked worktree or a throwaway.
module HBS2.Git3.Repo.PinSpec (spec) where

import HBS2.Git3.Repo.Pin
import HBS2.Git3.Repo.Tools (pickRemote3)

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

  describe "the hbs23 remote a verb means with no repository named" $ do
    let two = [("hbs2", 'a'), ("indicate-exist", 'b')]
    it "takes the one hbs2.hub.repo names, then origin, then the only one" $ do
      pickRemote3 (Just 'b') two `shouldBe` Right 'b'
      pickRemote3 Nothing (("origin", 'c') : two) `shouldBe` Right 'c'
      pickRemote3 Nothing [("hbs2", 'a')] `shouldBe` Right 'a'
    it "names them when that does not decide" $ do
      pickRemote3 Nothing two `shouldBe` Left ["hbs2", "indicate-exist"]
      pickRemote3 (Just 'z') two `shouldBe` Left ["hbs2", "indicate-exist"]
      pickRemote3 Nothing ([] :: [(String, Char)]) `shouldBe` Left []
