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
import HBS2.Hub.CLI.Common (Writing(..))

import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))

import Data.Config.Suckless
import Data.HashSet qualified as HS
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

    -- RULE 5 HAS TWO HALVES AND ONLY ONE OF THEM WAS EVER EXERCISED. Every
    -- delegation in the suite was signed `mkEvent owner owner` or
    -- `mkEvent bob bob`, so on the negative cases both boxes failed together;
    -- dropping the AUTHOR half of the check left the whole suite green. Rule 5
    -- is a statement about the author, and the author is what nothing asked
    -- about.
    it "refuses a delegation the owner blessed and did not author" $ do
      owner <- kp
      alice <- kp
      carol <- kp
      let repo = fst owner
          -- The canon box is the owner's, so rule 3 is satisfied and only rule
          -- 5's reading of the author box refuses this.
          e = mkEvent alice owner (ADelegate repo (fst carol) 1000) (canonOf repo 1)
          fr = foldEvents repo [e]
      fmap drWhy (frDropped fr) `shouldBe` [UnauthorizedDelegate]
      frMaintainers fr `shouldBe` HS.fromList [repo]

    -- The other half, and the one with an attack behind it: canon is PUBLIC,
    -- so an owner-authored box is something anybody can lift out of it. If any
    -- authorized canon key could bless one, a maintainer could re-add a
    -- delegation the owner had revoked by re-blessing the owner's own old box.
    it "refuses a delegation the owner authored and a delegate blessed" $ do
      owner <- kp
      bob   <- kp
      carol <- kp
      let repo = fst owner
          e1 = mkEvent owner owner (ADelegate repo (fst bob) 1000) (canonOf repo 1)
          -- bob is an authorized canon key by now, so the check every other op
          -- makes would pass here.
          e2 = mkEvent owner bob (ADelegate repo (fst carol) 2000) (canonOf repo 2)
          fr = foldEvents repo [e1, e2]
      fmap drWhy (frDropped fr) `shouldBe` [UnauthorizedDelegate]
      frMaintainers fr `shouldBe` HS.fromList [repo, fst bob]

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
        `shouldBe` Just (Maintainer repo k ForReal)
      maintainerArgs (argv ["--key", b58 k, "--repo", b58 repo])
        `shouldBe` Just (Maintainer repo k ForReal)

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

    it "rehearses when asked to" $ do
      repo <- fst <$> kp
      k    <- fst <$> kp
      fmap mnDry (maintainerArgs (argv ["--repo", b58 repo, "--key", b58 k]))
        `shouldBe` Just ForReal
      fmap mnDry (maintainerArgs (argv [ "--repo", b58 repo, "--key", b58 k
                                       , "--dry-run" ]))
        `shouldBe` Just DryRun
      -- A switch, so it cannot swallow the key after it.
      maintainerArgs (argv ["--repo", b58 repo, "--dry-run", "--key", b58 k])
        `shouldBe` Just (Maintainer repo k DryRun)
      maintainerArgs (argv ["--repo", b58 repo, "--key", b58 k, "--dry-run", "x"])
        `shouldBe` Nothing

  -- WHAT THE EVENT WOULD DO, ASKED BEFORE IT IS SIGNED. Each of these mints a
  -- well-formed owner-signed event that the fold admits and that then changes
  -- nothing: canon grows, the seq is spent, and the report prints the same
  -- maintainer set it would have printed anyway.
  describe "PEP-21 maintainers: an event that would change nothing" $ do

    it "refuses to delegate to a key that is already a maintainer" $ do
      owner <- fst <$> kp
      bob   <- fst <$> kp
      let set = HS.fromList [owner, bob]
      pointless Delegate owner bob set `shouldBe` Just AlreadyAMaintainer
      -- Including the owner, who is in the set with no event behind it.
      pointless Delegate owner owner set `shouldBe` Just AlreadyAMaintainer

    it "refuses to revoke a key that is not one" $ do
      owner  <- fst <$> kp
      stranger <- fst <$> kp
      pointless Revoke owner stranger (HS.fromList [owner])
        `shouldBe` Just NotAMaintainer

    -- Admission reads the repository key as a maintainer whatever the log says
    -- (PEP-19 rule 5), so the event is admitted and then ignored, forever.
    it "refuses to revoke the owner, whom nothing can revoke" $ do
      owner <- fst <$> kp
      pointless Revoke owner owner (HS.fromList [owner]) `shouldBe` Just OwnerIsAlways

    it "lets the two that do something through" $ do
      owner <- fst <$> kp
      bob   <- fst <$> kp
      pointless Delegate owner bob (HS.fromList [owner]) `shouldBe` Nothing
      pointless Revoke owner bob (HS.fromList [owner, bob]) `shouldBe` Nothing
