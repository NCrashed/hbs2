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
known = Surroundings (Just repo) (Just author) (Just sender) (Just "master") (Just "fix-typo")

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
    fill "hub:log" ["--json"] `shouldBe` [repo, "--json"]

  it "leaves a read verb alone when it already starts with a key" $
    fill "hub:issue:show" [repo, "7"] `shouldBe` [repo, "7"]

  it "does not propose a branch onto itself" $ do
    let onMaster = known { cxFrom = Just "master" }
    fillArgv onMaster (wantsOf "hub:pr:new") ["--title", "t"]
      `shouldNotContain` ["--from"]
    fill "hub:pr:new" ["--title", "t", "--onto", "fix-typo"]
      `shouldNotContain` ["--from"]

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
