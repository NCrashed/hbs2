-- | The command line a verb sees when the clone already knows the answer.
module HBS2.Hub.DefaultsSpec (spec) where

import HBS2.Hub.CLI.Defaults

import Data.Text qualified as Text
import Test.Hspec

repo, author, sender :: String
repo   = "HprKoCozfpbfu17zueJKAcY29Yb9a33PcbS14BWoUoj6"
author = "234cz3gCazVh6qJyiKPHJvfLd22sue2odXXFRGY3cadh"
sender = "CMRhLSEWrPqhY7ca1CZjWKXuM4HTLnd6AdsPr65Gm2oH"

known :: Surroundings
known = Surroundings (Just repo) (Just author) (Just sender) (Just "master") (Just "fix-typo") (Just "hbs2")

fill :: String -> [String] -> [String]
fill v = fillArgv known (wantsOf (Text.pack v))

spec :: Spec
spec = describe "hub defaults from the clone" $ do

  it "lets a newcomer open a pull request with only a title" $
    fill "hub:pr:new" ["--title", "Fix it"]
      `shouldBe` [ "--title", "Fix it", "--repo", repo, "--author", author
                 , "--sender", sender, "--onto", "master", "--from", "fix-typo" ]

  it "files an issue with a title and a body" $
    fill "hub:issue:new" ["--title", "t", "--body", "-"]
      `shouldBe` ["--title", "t", "--body", "-", "--repo", repo, "--author", author, "--sender", sender]

  it "never touches a flag given by hand, in either spelling" $ do
    fill "hub:issue:close" ["--repo", "X", "--number", "1"]
      `shouldBe` ["--repo", "X", "--number", "1"]
    fill "hub:issue:close" ["--repo=X", "--number", "1"]
      `shouldBe` ["--repo=X", "--number", "1"]
    fill "hub:issue:list" ["--target", "X"] `shouldBe` ["--target", "X"]

  it "gives a read verb its repository as the first word" $ do
    fill "hub:issue:list" [] `shouldBe` [repo]
    fill "hub:issue:show" ["7"] `shouldBe` [repo, "7"]
    fill "hub:issue:show" ["--thread", "H", "--json"] `shouldBe` ["--thread", "H", "--json", "--repo", repo]

  -- Finding 18: `pr show --number 1 --json` got the key in front, which is
  -- neither the positional form nor the flagged one.
  it "gives a flagged read line its repository behind --repo" $ do
    fill "hub:pr:show" ["--number", "1", "--json"] `shouldBe` ["--number", "1", "--json", "--repo", repo]
    fill "hub:log" ["--json"] `shouldBe` ["--json", "--repo", repo]
    fill "hub:issue:list" ["--status", "open"] `shouldBe` ["--status", "open", "--repo", repo]

  it "leaves a read verb alone when it already starts with a key" $
    fill "hub:issue:show" [repo, "7"] `shouldBe` [repo, "7"]

  it "does not take the checkout for a ref of another repository" $
    fill "hub:pr:new" ["--title", "t", "--from-url", "https://github.com/a/b", "--from", "pull/3/head"]
      `shouldBe` [ "--title", "t", "--from-url", "https://github.com/a/b", "--from", "pull/3/head"
                 , "--repo", repo, "--author", author, "--sender", sender, "--onto", "master" ]

  it "does not propose a branch onto itself" $ do
    let onMaster = known { cxFrom = Just "master" }
    fillArgv onMaster (wantsOf "hub:pr:new") ["--title", "t"]
      `shouldNotContain` ["--from"]
    fill "hub:pr:new" ["--title", "t", "--onto", "fix-typo"]
      `shouldNotContain` ["--from"]

  -- Finding 17: in a clone kept on GitHub, origin is GitHub and the hbs23
  -- remote is another one; publish pushed canon to GitHub.
  it "publishes to and syncs from the hbs23 remote, not origin" $ do
    fill "hub:publish" [] `shouldBe` ["--remote", "hbs2"]
    fill "hub:sync" [] `shouldBe` ["--repo", repo, "--remote", "hbs2"]
    fill "hub:publish" ["--remote", "origin"] `shouldBe` ["--remote", "origin"]
    fillArgv nothingKnown (wantsOf "hub:publish") [] `shouldBe` []

  it "reads a contributor's own mailbox as their author key" $
    fill "hub:updates" [] `shouldBe` ["--repo", repo, "--mailbox", author]

  it "starts a maintainer working as their own identity" $ do
    fill "hub:maintainer:use" [] `shouldBe` ["--repo", repo, "--maintainer-key", author]
    fill "hub:maintainer:use" ["--clear"] `shouldBe` ["--clear", "--repo", repo]
    fill "hub:maintainer:use" ["--maintainer-key", "K"] `shouldBe` ["--maintainer-key", "K", "--repo", repo]

  it "adds nothing nobody knows" $
    fillArgv nothingKnown (wantsOf "hub:pr:new") ["--title", "t"] `shouldBe` ["--title", "t"]

  it "leaves a verb with no defaults exactly as typed" $ do
    wantsOf "hub:policy:show" `shouldBe` []
    wantsOf "hub:whoami" `shouldBe` []

  it "reads the key out of an hbs23 remote and nothing else" $ do
    hbs23Key ("hbs23://" <> repo) `shouldBe` Just repo
    hbs23Key ("hbs23://" <> repo <> "\n") `shouldBe` Just repo
    hbs23Key "git@github.com:NCrashed/hbs2.git" `shouldBe` Nothing
    hbs23Key "hbs23://not a key" `shouldBe` Nothing

  it "tells a key from a number" $ do
    keyShaped repo `shouldBe` True
    keyShaped "7" `shouldBe` False
    keyShaped "--json" `shouldBe` False

  it "picks the hbs23 remote the configured key names, then origin, then the only one" $ do
    let other = "8ZUnKpYfxTbDPQH85WMh9xmG5zXTVUeevsHexFxHLQAS"
        two = [("hbs2", repo), ("indicate-exist", other)]
    pickRemote Nothing two `shouldBe` Left ["hbs2", "indicate-exist"]
    pickRemote (Just repo) two `shouldBe` Right ("hbs2", repo)
    pickRemote (Just other) (("origin", repo) : two) `shouldBe` Right ("indicate-exist", other)
    pickRemote Nothing (("origin", other) : two) `shouldBe` Right ("origin", other)
    pickRemote (Just sender) [("hbs2", repo)] `shouldBe` Right ("hbs2", repo)
    pickRemote Nothing [] `shouldBe` Left []
