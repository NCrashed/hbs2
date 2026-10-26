-- | Which key signs canon when no --as was given (HBS2.Hub.CLI.Signer).
--
-- The order is --as, then HBS2_HUB_AS, then git config hbs2.hub.as, then the
-- repository key. The pure half is 'pickSigner'; the other half reads a real
-- git config in a scratch repository, because "which config does git answer
-- from" is the part a pure test cannot see.
module HBS2.Hub.SignerSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.CLI.Signer
import HBS2.Hub.CLI.Ack (AckTrouble(..))
import HBS2.Hub.CLI.Maintainer (maintainerUseArgs,MaintainerUse(..))
import HBS2.Hub.CLI.Status (Standing(..),Publication(..),statusDoc)
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Base58 (AsBase58(..))
import HBS2.Net.Auth.Credentials
import HBS2.Prelude.Plated (Doc,pretty)

import Data.Config.Suckless (C,Syntax)
import Data.List (isInfixOf)
import Data.Text qualified as Text
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc,runProcess_,setWorkingDir,setEnv)
import System.Environment qualified as Env
import Data.Maybe (fromMaybe)
import Test.Hspec

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

b58 :: HubKey -> String
b58 = show . pretty . AsBase58

argv :: [String] -> [Syntax C]
argv = fmap argvAtom

said :: [Doc ()] -> String
said = unlines . fmap show

-- A scratch repository, with nothing of the caller's git config in the way of
-- the commands that set it up.
withRepo :: (FilePath -> IO a) -> IO a
withRepo act = withSystemTempDirectory "hub-signer" $ \dir -> do
  git dir ["init", "-q"]
  act dir

git :: FilePath -> [String] -> IO ()
git dir args = do
  path <- fromMaybe "/usr/bin:/bin" <$> Env.lookupEnv "PATH"
  runProcess_ ( setEnv [ ("PATH", path), ("HOME", dir)
                       , ("GIT_CONFIG_NOSYSTEM", "1"), ("LC_ALL", "C") ]
                (setWorkingDir dir (proc "git" args)) )

spec :: Spec
spec = do

  describe "hub --as default: the order" $ do

    it "takes --as over everything" $ do
      [r, f, e, c] <- sequence [aKey, aKey, aKey, aKey]
      pickSigner r (Just f) (Just e) (Just c) `shouldBe` Signer f FromFlag
      pickSigner r (Just f) Nothing Nothing  `shouldBe` Signer f FromFlag

    it "takes the environment over git config" $ do
      [r, e, c] <- sequence [aKey, aKey, aKey]
      pickSigner r Nothing (Just e) (Just c) `shouldBe` Signer e FromEnv
      pickSigner r Nothing (Just e) Nothing  `shouldBe` Signer e FromEnv

    it "takes git config over the repository key" $ do
      [r, c] <- sequence [aKey, aKey]
      pickSigner r Nothing Nothing (Just c) `shouldBe` Signer c FromGitConfig

    it "falls back to the repository key, and says so" $ do
      r <- aKey
      pickSigner r Nothing Nothing Nothing `shouldBe` Signer r RepoKeyDefault
      show (signerDoc @() (Signer r RepoKeyDefault))
        `shouldSatisfy` isInfixOf "the repository key"

  describe "hub --as default: reading a value" $ do

    it "reads unset and blank as nothing" $ do
      defaultFrom FromEnv Nothing `shouldBe` Right Nothing
      defaultFrom FromEnv (Just "  ") `shouldBe` Right Nothing

    it "reads a key, around whitespace" $ do
      k <- aKey
      defaultFrom FromGitConfig (Just (" " <> Text.pack (b58 k) <> "\n"))
        `shouldBe` Right (Just k)

    -- Skipping it would sign canon with the repository key, under a key the
    -- operator did not name.
    it "refuses a value that is not a key, and says where it was" $ do
      defaultFrom FromEnv (Just "alice") `shouldBe` Left (NotAKey FromEnv "alice")
      show (pretty (NotAKey FromGitConfig "alice"))
        `shouldSatisfy` isInfixOf "git config hbs2.hub.as"

  describe "hub --as default: from a real git config" $ do

    it "is the repository key when nothing is set" $ withRepo $ \dir -> do
      r <- aKey
      resolveSignerIn (Just dir) Nothing r Nothing
        `shouldReturn` Right (Signer r RepoKeyDefault)

    it "reads hbs2.hub.as from the repository" $ withRepo $ \dir -> do
      [r, c] <- sequence [aKey, aKey]
      git dir ["config", "--local", signerGitKey, b58 c]
      resolveSignerIn (Just dir) Nothing r Nothing
        `shouldReturn` Right (Signer c FromGitConfig)

    it "lets the environment win over it, and a blank one not" $ withRepo $ \dir -> do
      [r, e, c] <- sequence [aKey, aKey, aKey]
      git dir ["config", "--local", signerGitKey, b58 c]
      resolveSignerIn (Just dir) (Just (b58 e)) r Nothing
        `shouldReturn` Right (Signer e FromEnv)
      resolveSignerIn (Just dir) (Just "") r Nothing
        `shouldReturn` Right (Signer c FromGitConfig)

    it "lets --as win over both" $ withRepo $ \dir -> do
      [r, f, e, c] <- sequence [aKey, aKey, aKey, aKey]
      git dir ["config", "--local", signerGitKey, b58 c]
      resolveSignerIn (Just dir) (Just (b58 e)) r (Just f)
        `shouldReturn` Right (Signer f FromFlag)

    it "refuses a broken default, unless --as was given" $ withRepo $ \dir -> do
      [r, f] <- sequence [aKey, aKey]
      git dir ["config", "--local", signerGitKey, "not-a-key"]
      resolveSignerIn (Just dir) Nothing r Nothing
        `shouldReturn` Left (NotAKey FromGitConfig "not-a-key")
      resolveSignerIn (Just dir) (Just "nope") r Nothing
        `shouldReturn` Left (NotAKey FromEnv "nope")
      resolveSignerIn (Just dir) Nothing r (Just f)
        `shouldReturn` Right (Signer f FromFlag)

  describe "hub maintainer use: arguments" $ do

    it "takes a repository and a key, or --clear" $ do
      [r, k] <- sequence [aKey, aKey]
      maintainerUseArgs (argv ["--repo", b58 r, "--maintainer-key", b58 k])
        `shouldBe` Just (UseKey r k)
      maintainerUseArgs (argv ["--clear"]) `shouldBe` Just UseClear
      maintainerUseArgs (argv ["--repo", b58 r, "--clear"]) `shouldBe` Just UseClear

    it "refuses a key without a repository, and a key with --clear" $ do
      [r, k] <- sequence [aKey, aKey]
      maintainerUseArgs (argv ["--maintainer-key", b58 k]) `shouldBe` Nothing
      maintainerUseArgs (argv ["--repo", b58 r, "--maintainer-key", b58 k, "--clear"])
        `shouldBe` Nothing
      maintainerUseArgs (argv ["--repo", b58 r]) `shouldBe` Nothing

  describe "who would sign, as the reports say it" $ do

    let standing repo s maint = Standing
          { stRepo = repo, stCanSign = True, stSigner = s, stMaintains = maint
          , stCanon = Just "cafe1234", stThreads = 1, stEvents = 1, stKeys = 2
          , stPublished = Right [("origin", Level)], stMailbox = Nothing
          , stWaiting = Right 0 }

    it "status names the key and where it came from" $ do
      [r, c] <- sequence [aKey, aKey]
      let s = said (statusDoc (standing r (Right (Signer c FromGitConfig)) True))
      s `shouldSatisfy` isInfixOf (b58 c)
      s `shouldSatisfy` isInfixOf "git config hbs2.hub.as"
      s `shouldNotSatisfy` isInfixOf "not a maintainer"

    it "status warns about a default that is not a maintainer" $ do
      [r, e] <- sequence [aKey, aKey]
      let s = said (statusDoc (standing r (Right (Signer e FromEnv)) False))
      s `shouldSatisfy` isInfixOf "HBS2_HUB_AS"
      s `shouldSatisfy` isInfixOf "not a maintainer"

    it "status says when the default does not read" $ do
      r <- aKey
      said (statusDoc (standing r (Left (NotAKey FromEnv "x")) True))
        `shouldSatisfy` isInfixOf "signing  unknown"

    -- The message met in real use named a key and nothing else to do.
    it "a missing ack says which key signed and how to sign as another" $ do
      k <- aKey
      let s = show (pretty (AckNoEncKey k))
      s `shouldSatisfy` isInfixOf (b58 k)
      s `shouldSatisfy` isInfixOf "--as"
      s `shouldSatisfy` isInfixOf "hbs2.hub.as"
      s `shouldSatisfy` isInfixOf "maintainer use"
