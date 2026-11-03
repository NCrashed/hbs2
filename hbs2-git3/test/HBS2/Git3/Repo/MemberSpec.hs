module HBS2.Git3.Repo.MemberSpec (spec) where

import HBS2.Git3.Repo.Member

import HBS2.Net.Auth.Credentials (CryptoAction (..), CryptoScheme (..), PubKey)
import HBS2.Prelude.Plated (fromStringMay)

import Data.Either (isLeft)
import Data.HashSet qualified as HS
import Data.Maybe (fromMaybe)
import Test.Hspec

memberA, memberB :: PubKey 'Encrypt 'HBS2Basic
memberA = parseKey "AxmGgWG5W25w7nTHTPdhYAeQCYe3SHLsFFk3ZvNp5vJP"
memberB = parseKey "2mP3XWjnCMUKKGe2DbLmhB261fyKoYD4RNrFufErDqPc"

parseKey :: String -> PubKey 'Encrypt 'HBS2Basic
parseKey = fromMaybe (error "invalid fixture encryption key") . fromStringMay

spec :: Spec
spec = describe "encrypted repository membership" do
    describe "typed member keys" do
        it "accepts an explicitly typed encryption key" do
            parseMemberKey "enc:AxmGgWG5W25w7nTHTPdhYAeQCYe3SHLsFFk3ZvNp5vJP"
                `shouldBe` Right memberA

        it "rejects an untyped key, including a mistakenly copied sign-key" do
            parseMemberKey "AxmGgWG5W25w7nTHTPdhYAeQCYe3SHLsFFk3ZvNp5vJP"
                `shouldSatisfy` isLeft
            show MemberKeyNeedsEncryptionPrefix
                `shouldBe` "repository member keys must be explicitly typed as enc:<base58-key>"

        it "rejects malformed encryption-key payloads" do
            parseMemberKey "enc:not-a-key" `shouldSatisfy` isLeft

    describe "member addition plan" do
        it "preserves whether the plan is a dry run" do
            memberAddDryRun ApplyMemberAdd `shouldBe` False
            memberAddDryRun DryRunMemberAdd `shouldBe` True

        it "adds each absent member once" do
            membersToAdd (HS.singleton memberA) [memberA, memberB, memberB]
                `shouldBe` HS.singleton memberB

        it "is empty when rerunning the same request" do
            membersToAdd (HS.fromList [memberA, memberB]) [memberB, memberA]
                `shouldBe` mempty
