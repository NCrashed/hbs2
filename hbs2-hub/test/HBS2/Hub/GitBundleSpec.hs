-- | The PEP-20 delta path against real git.
--
-- None of this can be checked without git, because all of it IS git: what
-- @bundle create@ does with an empty range, what a fetch writes to FETCH_HEAD,
-- what @merge-base --is-ancestor@ exits with, and whether the objects that
-- arrive are the ones the letter signed for.
--
-- The name checks are here rather than in a pure module for the same reason:
-- what they exist to prevent is a value reaching a git command line, so the
-- test that matters is the one where git is on the other end.
module HBS2.Hub.GitBundleSpec (spec) where

import HBS2.Hub.Repo.GitBundle

import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment qualified as Env
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed
import Test.Hspec

-- Git for the FIXTURE, sealed from the developer's configuration. What is
-- under test runs with the ambient environment, which is what an operator's
-- shell has.
git :: FilePath -> [String] -> IO String
git cwd args = do
  path <- fromMaybe "/usr/bin:/bin" <$> Env.lookupEnv "PATH"
  let cfg = setEnv [ ("GIT_CONFIG_GLOBAL", "/dev/null")
                   , ("GIT_CONFIG_SYSTEM", "/dev/null")
                   , ("GIT_CONFIG_NOSYSTEM", "1")
                   , ("HOME", cwd)
                   , ("LC_ALL", "C")
                   , ("PATH", path)
                   ]
              (setWorkingDir cwd (proc "git" (conf <> args)))
      conf = [ "-c", "user.email=t@t", "-c", "user.name=t"
             , "-c", "commit.gpgsign=false", "-c", "init.defaultBranch=master" ]
  (code, out, errOut) <- readProcess (setStdin closed cfg)
  case code of
    ExitSuccess -> pure (takeWhile (`notElem` ("\n\r" :: String)) (LBS8.unpack out))
    _ -> fail ("git " <> unwords args <> ": " <> LBS8.unpack errOut)

-- A repository with a commit on master and two more on a branch, which is the
-- shape a pull request has: a base both sides hold, and a range only one does.
withWork :: (FilePath -> Text -> Text -> IO a) -> IO a
withWork act = withSystemTempDirectory "hub-bundle" $ \dir -> do
  void $ git dir ["init", "-q", "."]
  writeFile (dir <> "/a.txt") "one\n"
  void $ git dir ["add", "a.txt"]
  void $ git dir ["commit", "-q", "-m", "base"]
  base <- Text.pack <$> git dir ["rev-parse", "HEAD"]
  void $ git dir ["checkout", "-q", "-b", "feature"]
  writeFile (dir <> "/b.txt") "two\n"
  void $ git dir ["add", "b.txt"]
  void $ git dir ["commit", "-q", "-m", "work"]
  tip <- Text.pack <$> git dir ["rev-parse", "HEAD"]
  act dir base tip

ok :: Show e => Either e a -> IO a
ok = either (fail . show) pure

spec :: Spec
spec = spec1 >> spec2 >> spec3 >> spec4

spec1 :: Spec
spec1 = do

  describe "PEP-20 delta path: building a bundle" $ do

    it "bundles the range and reports the tip it recorded" $ withWork $ \dir base tip -> do
      b <- ok =<< bundleRange (Just dir) base "feature"
      bnTip b `shouldBe` tip
      -- A bundle, and not an empty one: git's own signature is on the front.
      BS.take 4 (bnBytes b) `shouldBe` "# v2"
      BS.length (bnBytes b) `shouldSatisfy` (> 100)

    -- The ordinary mistake, not a failure: a contributor who has not committed
    -- yet, or who named the branch they are on as the base. git says it in
    -- prose, so it is read out of the prose and given its own answer.
    it "says an empty range is empty rather than quoting git at the user" $
      withWork $ \dir _ tip -> do
        r <- bundleRange (Just dir) tip "feature"
        r `shouldBe` Left BundleEmpty

    -- A ref name comes out of a stranger's signed box, and git reads a leading
    -- dash as an option. This is the letter that would run a program.
    it "will not pass a ref name shaped like an option to git" $
      withWork $ \dir base _ -> do
        r <- bundleRange (Just dir) base "--upload-pack=touch /tmp/pwned"
        case r of
          Left (BundleBadName what v) -> do
            what `shouldBe` "ref name"
            v `shouldSatisfy` ("--upload-pack" `Text.isPrefixOf`)
          other -> expectationFailure ("expected a refusal, got " <> show other)

    it "will not pass a base that is not an object name" $
      withWork $ \dir _ _ -> do
        r <- bundleRange (Just dir) "HEAD" "feature"
        case r of
          Left (BundleBadName what _) -> what `shouldBe` "object name"
          other -> expectationFailure ("expected a refusal, got " <> show other)

  describe "PEP-20 delta path: taking one in" $ do

    -- The round trip, and the assertion PEP-20 turns on: what arrives is the
    -- commit the contributor signed for.
    it "fetches a bundle and the tip is the one that was bundled" $
      withSystemTempDirectory "hub-pair" $ \root -> do
        let ours = root <> "/contributor"
            theirs = root <> "/maintainer"

        -- The base is made ONCE and cloned, which is the only way the two
        -- repositories genuinely share it. Building it twice from the same
        -- content does not: a commit hashes its timestamps too, so two
        -- identical trees committed a moment apart are two different commits,
        -- and a test asserting otherwise passes only until it is slow.
        void $ git root ["init", "-q", ours]
        writeFile (ours <> "/a.txt") "one\n"
        void $ git ours ["add", "a.txt"]
        void $ git ours ["commit", "-q", "-m", "base"]
        base <- Text.pack <$> git ours ["rev-parse", "HEAD"]
        void $ git root ["clone", "-q", ours, theirs]

        void $ git ours ["checkout", "-q", "-b", "feature"]
        writeFile (ours <> "/b.txt") "two\n"
        void $ git ours ["add", "b.txt"]
        void $ git ours ["commit", "-q", "-m", "work"]
        tip <- Text.pack <$> git ours ["rev-parse", "HEAD"]

        b <- ok =<< bundleRange (Just ours) base "feature"
        bnTip b `shouldBe` tip

        -- The maintainer holds the base and nothing of the branch, which is
        -- what makes this the delta path rather than a full clone.
        (ExitFailure _, _, _) <- readProcess . setStdin closed
          =<< pure (setWorkingDir theirs (proc "git" ["cat-file", "-e", Text.unpack tip]))

        got <- ok =<< acceptBundle (Just theirs) (bnBytes b) "feature" tip
        got `shouldBe` tip
        -- and the objects really are here now
        void $ git theirs ["cat-file", "-e", Text.unpack tip]

    -- AGAINST A MAINTAINER WHO HOLDS THE BASE, which is the whole test.
    --
    -- It used to run against a fresh `git init`, and there it proved nothing:
    -- that repository does not have the prerequisite commit, so `git bundle
    -- verify` refuses before the pack is ever read, and the same refusal, the
    -- same constructor, comes back for a PRISTINE bundle. The assertion held
    -- with `bytes` in place of `broken`. What it claims to be about -- git's own
    -- object hashing catching a flipped byte -- was never reached.
    it "refuses a bundle whose bytes were tampered with" $
      withSystemTempDirectory "hub-pair" $ \root -> do
        let ours = root <> "/contributor"
            theirs = root <> "/maintainer"

        void $ git root ["init", "-q", ours]
        writeFile (ours <> "/a.txt") "one\n"
        void $ git ours ["add", "a.txt"]
        void $ git ours ["commit", "-q", "-m", "base"]
        base <- Text.pack <$> git ours ["rev-parse", "HEAD"]
        -- TWO maintainers, both cloned before the branch exists, so each holds
        -- the prerequisite and neither holds the tip. One takes the good
        -- bundle and one is offered the broken one; sharing a clone between
        -- them would mean the second fetch found the objects already there,
        -- which is a way to pass this test without checking anything.
        void $ git root ["clone", "-q", ours, theirs]
        let other = root <> "/maintainer2"
        void $ git root ["clone", "-q", ours, other]

        void $ git ours ["checkout", "-q", "-b", "feature"]
        writeFile (ours <> "/b.txt") "two\n"
        void $ git ours ["add", "b.txt"]
        void $ git ours ["commit", "-q", "-m", "work"]
        tip <- Text.pack <$> git ours ["rev-parse", "HEAD"]

        bytes <- bnBytes <$> (ok =<< bundleRange (Just ours) base "feature")

        -- The control: this bundle is good, and the maintainer takes it. If
        -- this line ever fails the negative below has stopped meaning anything,
        -- which is exactly how the old version of this test died.
        (ok =<< acceptBundle (Just theirs) bytes "feature" tip) >>= (`shouldBe` tip)

        let n = BS.length bytes `div` 2
            broken = BS.concat [ BS.take n bytes
                               , BS.singleton (BS.index bytes n + 1)
                               , BS.drop (n + 1) bytes ]
        r <- acceptBundle (Just other) broken "feature" tip
        case r of
          Left BundleRefused{} -> pure ()
          other -> expectationFailure ("expected a refusal, got " <> show other)
        -- and nothing of it landed: the tip is not in the maintainer's objects
        (code, _, _) <- readProcess . setStdin closed
          =<< pure (setWorkingDir other (proc "git" ["cat-file", "-e", Text.unpack tip]))
        code `shouldSatisfy` (/= ExitSuccess)

    -- THE CASE THAT USED TO LEAVE ITS PACK BEHIND. The bundle is perfectly
    -- good, so the fetch succeeds and writes the objects; what refuses is the
    -- tip check afterwards. Every refused pull request was therefore disk a
    -- stranger chose, in the maintainer's repository, until git gc got round to
    -- it two weeks later. The fetch lands in a quarantine now and is repeated
    -- into the repository only once the tip is the signed one.
    it "leaves nothing behind when the tip is not the one that was signed" $
      withSystemTempDirectory "hub-pair" $ \root -> do
        let ours = root <> "/contributor"
            theirs = root <> "/maintainer"

        void $ git root ["init", "-q", ours]
        writeFile (ours <> "/a.txt") "one\n"
        void $ git ours ["add", "a.txt"]
        void $ git ours ["commit", "-q", "-m", "base"]
        base <- Text.pack <$> git ours ["rev-parse", "HEAD"]
        void $ git root ["clone", "-q", ours, theirs]

        void $ git ours ["checkout", "-q", "-b", "feature"]
        writeFile (ours <> "/b.txt") "two\n"
        void $ git ours ["add", "b.txt"]
        void $ git ours ["commit", "-q", "-m", "work"]
        tip <- Text.pack <$> git ours ["rev-parse", "HEAD"]

        bytes <- bnBytes <$> (ok =<< bundleRange (Just ours) base "feature")

        r <- acceptBundle (Just theirs) bytes "feature" (Text.replicate 40 "a")
        case r of
          Left (BundleTipMismatch _ got) -> got `shouldBe` tip
          other -> expectationFailure ("expected a tip mismatch, got " <> show other)

        -- The objects the good bundle carried are NOT here.
        (code, _, _) <- readProcess . setStdin closed
          =<< pure (setWorkingDir theirs (proc "git" ["cat-file", "-e", Text.unpack tip]))
        code `shouldSatisfy` (/= ExitSuccess)

  describe "PEP-20 delta path: verifying and staging" $ do

    it "answers the ancestor question both ways" $ withWork $ \dir base tip -> do
      (ok =<< isAncestor (Just dir) base tip) >>= (`shouldBe` True)
      (ok =<< isAncestor (Just dir) tip base) >>= (`shouldBe` False)

    it "stages the proposed tip where PEP-19 puts it" $ withWork $ \dir _ tip -> do
      ok =<< stagePull (Just dir) 7 tip Nothing
      staged <- git dir ["rev-parse", Text.unpack (pullRef 7)]
      staged `shouldBe` Text.unpack tip

    -- Compare-and-swap, like the canon ref: a pull ref that moved means
    -- somebody else staged this number, and overwriting would replace one
    -- contributor's proposal with another's under one number.
    it "refuses to restage a number somebody else already used" $
      withWork $ \dir base tip -> do
        ok =<< stagePull (Just dir) 7 tip Nothing
        r <- stagePull (Just dir) 7 base Nothing
        case r of
          Left BundleRefused{} -> pure ()
          other -> expectationFailure ("expected a refusal, got " <> show other)
        -- and the first one is still there
        staged <- git dir ["rev-parse", Text.unpack (pullRef 7)]
        staged `shouldBe` Text.unpack tip

    it "restages when the old value is named" $ withWork $ \dir base tip -> do
      ok =<< stagePull (Just dir) 7 tip Nothing
      ok =<< stagePull (Just dir) 7 base (Just tip)
      staged <- git dir ["rev-parse", Text.unpack (pullRef 7)]
      staged `shouldBe` Text.unpack base

    -- Which is what a revise needs, and what it could not get: the accept path
    -- passed Nothing, i.e. "this ref must not exist", for a ref that exists by
    -- definition on the second proposal under one number.
    it "says what a pull ref holds, and that it holds nothing yet" $
      withWork $ \dir _ tip -> do
        (ok =<< pullTip (Just dir) 7) >>= (`shouldBe` Nothing)
        ok =<< stagePull (Just dir) 7 tip Nothing
        (ok =<< pullTip (Just dir) 7) >>= (`shouldBe` Just tip)

    it "moves a staged ref to a new tip through what it currently holds" $
      withWork $ \dir base tip -> do
        ok =<< stagePull (Just dir) 7 tip Nothing
        old <- ok =<< pullTip (Just dir) 7
        ok =<< stagePull (Just dir) 7 base old
        staged <- git dir ["rev-parse", Text.unpack (pullRef 7)]
        staged `shouldBe` Text.unpack base

  describe "PEP-20: what may reach a git command line" $ do

    it "takes the shape a branch name has" $ do
      map validRefName ["refs/heads/feature", "feature", "v1.2-rc1", "a/b/c"]
        `shouldBe` [True, True, True, True]

    it "refuses the shapes git or a shell would read as something else" $ do
      map validRefName [ "--upload-pack=x", "-x", "/leading", "a..b", "a//b"
                       , "a.", "a.lock", "a b", "a\nb", "", "a;rm -rf /" ]
        `shouldBe` replicate 11 False

    it "takes an object name of either hash length and nothing else" $ do
      validSha (Text.replicate 40 "a") `shouldBe` True
      validSha (Text.replicate 64 "a") `shouldBe` True
      map validSha [ "HEAD", "", Text.replicate 39 "a", Text.replicate 40 "g"
                   , "HEAD~1", Text.replicate 41 "a" ]
        `shouldBe` replicate 6 False

-- A bundle that fetches cleanly and is not what the letter signed for. The
-- objects are git's own and perfectly valid; what is wrong is whose they are.
-- This is the one check the delta path rests on, so it is refused inside
-- acceptBundle rather than left for a caller to remember.
spec2 :: Spec
spec2 =
  describe "PEP-20 delta path: the signed claim" $
    it "refuses a bundle that fetches something other than the signed tip" $
      withSystemTempDirectory "hub-pair" $ \root -> do
        let ours = root <> "/contributor"
            theirs = root <> "/maintainer"
        void $ git root ["init", "-q", ours]
        writeFile (ours <> "/a.txt") "one\n"
        void $ git ours ["add", "a.txt"]
        void $ git ours ["commit", "-q", "-m", "base"]
        base <- Text.pack <$> git ours ["rev-parse", "HEAD"]
        void $ git root ["clone", "-q", ours, theirs]
        void $ git ours ["checkout", "-q", "-b", "feature"]
        writeFile (ours <> "/b.txt") "two\n"
        void $ git ours ["add", "b.txt"]
        void $ git ours ["commit", "-q", "-m", "work"]
        tip <- Text.pack <$> git ours ["rev-parse", "HEAD"]
        b <- ok =<< bundleRange (Just ours) base "feature"

        -- The base is a real commit and a wrong answer: a contributor who
        -- signed one tip and shipped the objects for another.
        r <- acceptBundle (Just theirs) (bnBytes b) "feature" base
        r `shouldBe` Left (BundleTipMismatch base tip)

-- The fork point, which is what a contributor's bundle is a range from. It is
-- computed rather than asked for: too old and the bundle is the whole history,
-- too new and the maintainer cannot apply it.
spec3 :: Spec
spec3 =
  describe "PEP-20 delta path: the fork point" $ do

    it "finds where the branch left the trunk" $ withWork $ \dir base _ -> do
      got <- ok =<< mergeBase (Just dir) "master" "feature"
      got `shouldBe` base

    it "refuses a ref name it would not pass to git" $ withWork $ \dir _ _ -> do
      r <- mergeBase (Just dir) "master" "--output=/tmp/pwned"
      case r of
        Left (BundleBadName what _) -> what `shouldBe` "ref name"
        other -> expectationFailure ("expected a refusal, got " <> show other)

spec4 :: Spec
spec4 =
  describe "PEP-22 hub pr checkout: putting a proposal on a branch" $ do

    it "makes the branch and leaves the work tree on it" $
      withWork $ \dir base tip -> do
        -- From the base, so that switching is a real change rather than a
        -- no-op: this is what a reviewer standing on master does.
        void (git dir ["checkout", "-q", "master"])
        ok =<< checkoutBranch (Just dir) "pr/7" tip
        git dir ["rev-parse", "HEAD"] >>= (`shouldBe` Text.unpack tip)
        git dir ["rev-parse", "--abbrev-ref", "HEAD"] >>= (`shouldBe` "pr/7")
        -- And the base is still where it was: nothing was moved onto it.
        git dir ["rev-parse", "master"] >>= (`shouldBe` Text.unpack base)

    it "runs again on the branch it already made" $
      withWork $ \dir _ tip -> do
        void (git dir ["checkout", "-q", "master"])
        ok =<< checkoutBranch (Just dir) "pr/7" tip
        void (git dir ["checkout", "-q", "master"])
        ok =<< checkoutBranch (Just dir) "pr/7" tip
        git dir ["rev-parse", "--abbrev-ref", "HEAD"] >>= (`shouldBe` "pr/7")

    -- The one that matters: `checkout -B` would move the branch and throw away
    -- whatever a reviewer had committed on that name. This refuses instead.
    it "will not move a branch that points somewhere else" $
      withWork $ \dir base tip -> do
        void (git dir ["checkout", "-q", "master"])
        ok =<< checkoutBranch (Just dir) "pr/7" base
        r <- checkoutBranch (Just dir) "pr/7" tip
        case r of
          Left (BundleTipMismatch want got) -> do
            want `shouldBe` tip
            got `shouldBe` base
          other -> expectationFailure ("expected a refusal, got " <> show other)
        -- And the branch still holds what it held.
        git dir ["rev-parse", "pr/7"] >>= (`shouldBe` Text.unpack base)

    it "refuses a branch name or a tip it would not pass to git" $
      withWork $ \dir _ tip -> do
        r <- checkoutBranch (Just dir) "--track=x" tip
        case r of
          Left (BundleBadName what _) -> what `shouldBe` "ref name"
          other -> expectationFailure ("expected a refusal, got " <> show other)
        r2 <- checkoutBranch (Just dir) "pr/7" "HEAD"
        case r2 of
          Left (BundleBadName what _) -> what `shouldBe` "object name"
          other -> expectationFailure ("expected a refusal, got " <> show other)

    it "says what any ref holds, and that a branch nobody made holds nothing" $
      withWork $ \dir _ tip -> do
        (ok =<< refTip (Just dir) "feature") >>= (`shouldBe` Just tip)
        (ok =<< refTip (Just dir) "pr/7") >>= (`shouldBe` Nothing)
