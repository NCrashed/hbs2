-- | What @repo:init <key>@ does with what the key already has.
--
-- The rule is small and the cost of getting it wrong is not: a readable
-- repository replaced by an empty one loses every clone's view of its history,
-- so the table is pinned here, --force included.
module HBS2.Git3.Repo.InitSpec (spec) where

import HBS2.Git3.Repo.Init

import HBS2.Data.Types.Refs (HashRef)
import HBS2.Prelude.Plated (fromStringMay)

import Data.Maybe (fromMaybe)
import Test.Hspec

aHash :: HashRef
aHash = fromMaybe (error "not a hash")
          (fromStringMay "8yqJyq5jxKmDdMwzGvQhx3srKuc1FqmxCYSZKz5yzXWJ")

spec :: Spec
spec = describe "decideReinit" do

  it "publishes under a key that has no value" do
    decideReinit False RefEmpty `shouldBe` PublishFresh
    decideReinit True  RefEmpty `shouldBe` PublishFresh

  it "never replaces a repository that can be read, even with --force" do
    decideReinit False (RefReadable aHash) `shouldBe` KeepExisting aHash
    decideReinit True  (RefReadable aHash) `shouldBe` KeepExisting aHash

  it "replaces a value whose manifest cannot be read only with --force" do
    decideReinit False (RefUnreadable aHash) `shouldBe` RefuseUnreadable aHash
    decideReinit True  (RefUnreadable aHash) `shouldBe` ReplaceUnreadable aHash
