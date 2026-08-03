-- | The maintainer verbs' decisions (PEP-21 "Delegation").
--
-- What they publish is who may bless canon, which is the one thing in this
-- system that grants authority rather than exercising it. The fold decides
-- admission; what is here is the reading of it and the arguments, and the
-- arguments are two keys of one type.
module HBS2.Hub.MaintainerSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Hub.CLI.Maintainer
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))

import Data.Config.Suckless
import Data.List (isInfixOf)
import Data.Word (Word64)
import Prettyprinter (Doc,pretty)
import Test.Hspec

type KP = (HubKey, PrivKey 'Sign HubScheme)

kp :: IO KP
kp = do
  c <- newCredentials @'HBS2Basic
  pure (_peerSignPk c, _peerSignSk c)

canonOf :: RepoRef -> Word64 -> EventId -> CanonContent
canonOf repo sq eid = CanonContent repo eid sq Nothing Nothing Nothing sq Nothing

argv :: [String] -> [Syntax C]
argv = fmap argvAtom

b58 :: HubKey -> String
b58 = show . pretty . AsBase58

rendered :: [Doc ann] -> String
rendered = unlines . fmap show

spec :: Spec
spec = do

  describe "PEP-21 maintainers: who may bless canon" $ do

    it "starts with the owner and nobody else" $ do
      owner <- kp
      let fr = foldEvents (fst owner) []
      rendered (maintainerDoc (fst owner) fr)
        `shouldBe` unlines [ b58 (fst owner) <> " (owner)" ]

    it "adds a delegate and takes it out again" $ do
      owner <- kp
      bob   <- kp
      let repo = fst owner
          e1 = mkEvent owner owner (ADelegate repo (fst bob) 1000) (canonOf repo 1)
          after' = foldEvents repo [e1]
          e2 = mkEvent owner owner (ARevoke repo (fst bob) 2000) (canonOf repo 2)
          gone = foldEvents repo [e1, e2]
      rendered (maintainerDoc repo after') `shouldSatisfy` (b58 (fst bob) `isInfixOf`)
      rendered (maintainerDoc repo gone) `shouldSatisfy` (not . (b58 (fst bob) `isInfixOf`))

    -- The owner is in the set by definition rather than by any event, so a
    -- reader comparing the list against the log would otherwise find one entry
    -- with nothing behind it.
    it "marks the owner, which no event put there" $ do
      owner <- kp
      bob   <- kp
      let repo = fst owner
          fr = foldEvents repo [ mkEvent owner owner (ADelegate repo (fst bob) 1000)
                                   (canonOf repo 1) ]
          out = lines (rendered (maintainerDoc repo fr))
      length out `shouldBe` 2
      length (filter ("(owner)" `isInfixOf`) out) `shouldBe` 1

    -- Rule 5: only the repository's own key may delegate. A delegate that
    -- could delegate could grow the maintainer set.
    it "does not let a delegate delegate" $ do
      owner <- kp
      bob   <- kp
      carol <- kp
      let repo = fst owner
          e1 = mkEvent owner owner (ADelegate repo (fst bob) 1000) (canonOf repo 1)
          -- bob is a maintainer now, and signs one anyway
          e2 = mkEvent bob bob (ADelegate repo (fst carol) 2000) (canonOf repo 2)
          fr = foldEvents repo [e1, e2]
      rendered (maintainerDoc repo fr) `shouldSatisfy` (not . (b58 (fst carol) `isInfixOf`))
      fmap drWhy (frDropped fr) `shouldBe` [UnauthorizedDelegate]

    it "treats revoking the owner as the no-op it has to be" $ do
      owner <- kp
      let repo = fst owner
          fr = foldEvents repo [ mkEvent owner owner (ARevoke repo repo 1000)
                                   (canonOf repo 1) ]
      -- The root of trust cannot be delegated away, so it cannot be withdrawn:
      -- a repository whose owner had revoked itself would admit nothing ever
      -- again, including the event that put it back.
      rendered (maintainerDoc repo fr) `shouldSatisfy` (b58 repo `isInfixOf`)
      frDropped fr `shouldBe` []

  describe "PEP-21 maintainers: arguments" $ do

    it "reads both keys, in either order" $ do
      repo <- fst <$> kp
      k    <- fst <$> kp
      maintainerArgs (argv ["--repo", b58 repo, "--key", b58 k])
        `shouldBe` Just (Maintainer repo k)
      maintainerArgs (argv ["--key", b58 k, "--repo", b58 repo])
        `shouldBe` Just (Maintainer repo k)

    -- Both are keys of one type, so the swap is well typed: it would delegate
    -- the repository to itself and name a maintainer nobody has a repo for.
    -- The flag is the only thing that says which is which.
    it "refuses a form with either flag missing" $ do
      k <- fst <$> kp
      maintainerArgs (argv ["--repo", b58 k]) `shouldBe` Nothing
      maintainerArgs (argv ["--key", b58 k]) `shouldBe` Nothing
      maintainerArgs (argv [b58 k, b58 k]) `shouldBe` Nothing

    it "refuses a repeated flag rather than choosing one" $ do
      repo <- fst <$> kp
      k    <- fst <$> kp
      other <- fst <$> kp
      maintainerArgs (argv ["--repo", b58 repo, "--key", b58 k, "--key", b58 other])
        `shouldBe` Nothing
