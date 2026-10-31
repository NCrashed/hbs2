module HBS2.Git3.Repo.ReplicationSpec (spec) where

import HBS2.Git3.Repo.Mailbox (renderManifest)
import HBS2.Git3.Repo.Replication

import Data.Config.Suckless.Script
import Data.Either (isLeft)
import Data.Text qualified as Text
import Test.Hspec

manifest :: Text.Text -> [Syntax C]
manifest source = either (error . show) id (parseTop source)

base :: [Syntax C]
base = manifest "(hbs2-git 3)\n(seed 1)\n(public)\n"

right :: Either Text.Text a -> a
right = either (error . Text.unpack) id

spec :: Spec
spec = describe "M5 repository replication policy" $ do
  it "writes and reads a bounded external replica minimum" $ do
    let changed = right (setMinimumReplicas (Just 3) base)
    replicationPolicyOf changed `shouldBe` ReplicationPolicy (Just 3)
    writesBackAsReplicationPolicy (ReplicationPolicy (Just 3)) changed `shouldBe` True
    Text.pack (renderManifest changed)
      `shouldSatisfy` Text.isInfixOf "(replication (minimum 3))"

  it "distinguishes an explicit zero target from no policy" $ do
    let zero = right (setMinimumReplicas (Just 0) base)
        cleared = right (setMinimumReplicas Nothing zero)
    replicationPolicyOf zero `shouldBe` ReplicationPolicy (Just 0)
    replicationPolicyOf cleared `shouldBe` ReplicationPolicy Nothing
    renderManifest cleared `shouldBe` renderManifest base

  it "replaces duplicate minima and preserves future fields" $ do
    let old = base <> manifest
          "(replication (minimum 1) (availability-bps 9900))\n(marker stays-here)\n(replication (minimum 2) (failure-domain site))\n"
        changed = right (setMinimumReplicas (Just 4) old)
    replicationPolicyOf changed `shouldBe` ReplicationPolicy (Just 4)
    Text.pack (renderManifest changed) `shouldSatisfy` Text.isInfixOf "(availability-bps 9900)"
    Text.pack (renderManifest changed) `shouldSatisfy` Text.isInfixOf "(failure-domain site)"
    length [ () | ListVal (SymbolVal "replication" : _) <- changed ] `shouldBe` 1
    case drop (length base) changed of
      ListVal (SymbolVal "replication" : _) : ListVal [SymbolVal "marker", SymbolVal "stays-here"] : _ -> pure ()
      _ -> expectationFailure "the consolidated replication clause moved from its first position"

  it "ignores malformed values and refuses an excessive target" $ do
    replicationPolicyOf (manifest "(replication (minimum -1))\n")
      `shouldBe` ReplicationPolicy Nothing
    replicationPolicyOf (manifest "(replication (minimum 33))\n")
      `shouldBe` ReplicationPolicy Nothing
    replicationPolicyOf (manifest "(replication (minimum many))\n")
      `shouldBe` ReplicationPolicy Nothing
    let maximumTarget = right (setMinimumReplicas (Just maxMinimumReplicas) base)
    replicationPolicyOf maximumTarget `shouldBe` ReplicationPolicy (Just maxMinimumReplicas)
    isLeft (setMinimumReplicas (Just (maxMinimumReplicas + 1)) base) `shouldBe` True

  it "refuses a manifest which cannot be parsed after rendering" $ do
    let malformed = base <> [mkForm @C "note" [mkSym ("(" :: String)]]
    writesBackAsReplicationPolicy (replicationPolicyOf malformed) malformed `shouldBe` False
