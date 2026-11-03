module HBS2.Git3.ImportSpec (spec) where

import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Git3.Import
import HBS2.Hash (HbSync, hashObject)
import HBS2.Storage.Operations.Class

import Data.ByteString.Lazy.Char8 qualified as LBS
import Prettyprinter (pretty)
import Test.Hspec

spec :: Spec
spec = describe "encrypted Git pack import failures" do
  it "turns a missing group-key secret into a prompt actionable error" do
    let gkh = HashRef (hashObject @HbSync (LBS.pack "group key"))
    classifyTreeReadError (GroupKeySecretNotFound gkh)
      `shouldBe` TreeGroupKeySecretUnavailable gkh

    let message = show (ImportGroupKeySecretUnavailable gkh)
    message `shouldContain` show (pretty gkh)
    message `shouldContain` "repository membership"
    message `shouldContain` "hbs2-keyman"

  it "keeps a genuinely missing block in the missing-block path" do
    classifyTreeReadError MissedBlockError `shouldBe` TreeOperationError MissedBlockError
