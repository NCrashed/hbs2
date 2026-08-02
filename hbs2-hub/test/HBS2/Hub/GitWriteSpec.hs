-- | 'withGitSink' against real git.
--
-- The half of writing canon that cannot be checked without git, which is all of
-- the part that has ever been wrong in the reader next door: which repository a
-- command lands in, what it does to the index it finds there, and whether a
-- path survives the trip. Added to that, the one thing only the writer has: the
-- ref move is a compare-and-swap, and a test that never races it is a test of a
-- plain write.
--
-- The assertions run the READER over what the writer produced wherever they
-- can. Two halves of one format checked against each other is worth more than
-- either checked against a fixture somebody typed.
module HBS2.Hub.GitWriteSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git
import HBS2.Hub.Repo.GitWrite
import HBS2.Hub.Canon (renderMeta)

import HBS2.Prelude.Plated (Pretty(..))

import HBS2.Net.Auth.Credentials

import Control.Monad (void,(>=>))
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.HashMap.Strict qualified as HM
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import System.Environment qualified as Env
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed
import Test.Hspec

type KP = (HubKey, PrivKey 'Sign HubScheme)

kp :: IO KP
kp = do
  c <- newCredentials @'HBS2Basic
  pure (_peerSignPk c, _peerSignSk c)

canonOf :: RepoRef -> Word64 -> Maybe Word64 -> EventId -> CanonContent
canonOf repo sq num eid = CanonContent repo eid sq num Nothing Nothing sq Nothing

-- Git, sealed from the developer's configuration, for building the FIXTURE
-- only. What is under test runs with the ambient environment, because that is
-- what an operator's accept runs with.
git :: FilePath -> [String] -> IO String
git cwd args = do
  path <- fromMaybe "/usr/bin:/bin" <$> Env.lookupEnv "PATH"
  let cfg = setEnv ( [ ("GIT_CONFIG_GLOBAL", "/dev/null")
                     , ("GIT_CONFIG_SYSTEM", "/dev/null")
                     , ("GIT_CONFIG_NOSYSTEM", "1")
                     , ("HOME", cwd)
                     , ("LC_ALL", "C")
                     , ("PATH", path)
                     ] )
              (setWorkingDir cwd (proc "git" (conf <> args)))
      conf = [ "-c", "user.email=t@t", "-c", "user.name=t"
             , "-c", "commit.gpgsign=false" ]
  (code, out, errOut) <- readProcess (setStdin closed cfg)
  case code of
    ExitSuccess -> pure (takeWhile (`notElem` ("\n\r" :: String)) (LBS8.unpack out))
    _ -> fail ("git " <> unwords args <> ": " <> LBS8.unpack errOut)

withRepo :: (FilePath -> IO a) -> IO a
withRepo act = withSystemTempDirectory "hub-write" $ \dir -> do
  void $ git dir ["init", "-q", "."]
  act dir

-- Commit a plan, and insist it landed.
commitOk :: FilePath -> Maybe Text -> CanonCommit -> Word64 -> IO Text
commitOk dir parent cw at =
  withGitSinkIn (Just dir) $ \sk ->
    skCommit sk (CanonWrite parent (cwFiles cw) "canon" at)
      >>= either (fail . show) pure

-- Read canon back with the reader this package ships.
readBack :: FilePath -> RepoRef -> IO CanonState
readBack dir repo =
  withGitCanonIn (Just dir) $ \cs -> readCanon cs repo >>= either (fail . show) pure

-- One open event and the plan that puts it in canon.
anOpen :: KP -> KP -> Word64 -> Text -> IO (Event, CanonCommit)
anOpen alice owner sq title = do
  let repo = fst owner
      ev = mkEvent alice owner
             (AOpen repo HubIssue title [] (Just "body") Nothing Nothing (sq * 1000))
             (canonOf repo sq (Just sq))
      p = threadDir (eventId ev) <> "/" <> eventFileName sq (eventId ev)
  cw <- either (fail . show) pure (planCanon [(p, ev)] [(sq, eventId ev)])
  pure (ev, cw)

spec :: Spec
spec = do

  describe "PEP-19 canon writer against git" $ do

    it "creates canon the reader folds back" $ withRepo $ \dir -> do
      owner <- kp
      alice <- kp
      (ev, cw) <- anOpen alice owner 1 "an issue"

      parent <- withGitSinkIn (Just dir) (skParent >=> either (fail . show) pure)
      parent `shouldBe` Nothing

      void $ commitOk dir Nothing cw 1000

      st <- readBack dir (fst owner)
      stVersion st `shouldBe` Just hubMetaVersion
      stBad st `shouldBe` []
      stMisnamed st `shouldBe` []
      frDropped (stFold st) `shouldBe` []
      HM.keys (frThreads (stFold st)) `shouldBe` [eventId ev]

    it "keeps what the parent commit held" $ withRepo $ \dir -> do
      owner <- kp
      alice <- kp
      (e1, cw1) <- anOpen alice owner 1 "first"
      c1 <- commitOk dir Nothing cw1 1000

      (e2, cw2) <- anOpen alice owner 2 "second"
      -- The plan carries only the new event; the parent's file survives because
      -- the sink seeds its index from the parent tree. This is the assertion
      -- that a missing read-tree would fail.
      void $ commitOk dir (Just c1) cw2 2000

      st <- readBack dir (fst owner)
      frDropped (stFold st) `shouldBe` []
      sort (fmap (show . pretty) (HM.keys (frThreads (stFold st))))
        `shouldBe` sort (fmap (show . pretty) [eventId e1, eventId e2])

    -- The publish step is the only one that is atomic, and a writer that
    -- overwrote the ref would silently drop whichever accept lost.
    it "refuses to publish onto a canon that moved" $ withRepo $ \dir -> do
      owner <- kp
      alice <- kp
      (_, cw1) <- anOpen alice owner 1 "first"
      _ <- commitOk dir Nothing cw1 1000

      (_, cw2) <- anOpen alice owner 2 "second"
      -- Minted against "canon does not exist", which is what a triage loop
      -- that folded before the other accept landed would be holding.
      r <- withGitSinkIn (Just dir) $ \sk ->
             skCommit sk (CanonWrite Nothing (cwFiles cw2) "canon" 2000)
      case r of
        Left (RefMoved want got) -> do
          want `shouldBe` Nothing
          got `shouldSatisfy` (/= Nothing)
        other -> expectationFailure ("expected RefMoved, got " <> show other)

    -- Building canon through the repository's own index would stage the
    -- operator's working tree into a canon commit and leave their git status
    -- rearranged by a tool they ran to accept an issue.
    it "leaves the repository's index alone" $ withRepo $ \dir -> do
      owner <- kp
      alice <- kp
      -- Something staged, the way an operator mid-edit has.
      writeFile (dir <> "/theirs.txt") "work in progress"
      void $ git dir ["add", "theirs.txt"]
      before' <- git dir ["diff", "--cached", "--name-only"]

      (_, cw) <- anOpen alice owner 1 "an issue"
      void $ commitOk dir Nothing cw 1000

      after' <- git dir ["diff", "--cached", "--name-only"]
      after' `shouldBe` before'
      after' `shouldBe` "theirs.txt"

    -- Under LC_ALL=C a path with any byte above 127 does not survive argv, which
    -- is why every path here goes in on stdin. A thread id is hex, so the case
    -- arrives through a repository whose canon somebody else wrote; the writer
    -- must not be the thing that cannot round-trip it.
    it "writes a path git could not have taken as an argument" $ withRepo $ \dir -> do
      owner <- kp
      alice <- kp
      (ev, _) <- anOpen alice owner 1 "an issue"
      let odd' = "threads/\1055\1088\1080\1074\1077\1090/" <> eventFileName 1 (eventId ev)
      cw <- either (fail . show) pure (planCanon [(odd', ev)] [])
      void $ commitOk dir Nothing cw 1000

      names <- git dir ["ls-tree", "-r", "--name-only", "refs/hbs2/meta"]
      -- git prints a non-ASCII name quoted and escaped by default; asking for
      -- the bytes is what -z is for, and here it is enough that the tree has
      -- three entries and the reader can still fold it.
      names `shouldSatisfy` (not . null)
      st <- readBack dir (fst owner)
      frDropped (stFold st) `shouldBe` []
      HM.size (frThreads (stFold st)) `shouldBe` 1

    -- The commit id is a function of canon and the caller's stamp, not of when
    -- the writer ran. What this buys is a retry after a lost answer: the same
    -- accept re-run produces the same commit rather than a second one.
    it "produces the same commit for the same canon and stamp" $ do
      owner <- kp
      alice <- kp
      (_, cw) <- anOpen alice owner 1 "an issue"
      a <- withRepo $ \dir -> commitOk dir Nothing cw 1717171717000
      b <- withRepo $ \dir -> commitOk dir Nothing cw 1717171717000
      a `shouldBe` b

    it "reports what git said when it refuses" $ withRepo $ \dir -> do
      owner <- kp
      alice <- kp
      (_, cw) <- anOpen alice owner 1 "an issue"
      -- A parent that is not an object: the read-tree fails, and the point is
      -- that the failure arrives as git's own words rather than as a silence.
      r <- withGitSinkIn (Just dir) $ \sk ->
             skCommit sk (CanonWrite (Just "0000000000000000000000000000000000000000")
                                     (cwFiles cw) "canon" 1000)
      case r of
        -- Either the ref check catches it first (nothing is at that commit) or
        -- read-tree does. Both are refusals that name what happened; what must
        -- not happen is a commit.
        Left RefMoved{} -> pure ()
        Left (WriterRefused what _) -> what `shouldSatisfy` (not . Text.null)
        other -> expectationFailure ("expected a refusal, got " <> show other)

    it "writes the version file every commit" $ withRepo $ \dir -> do
      owner <- kp
      alice <- kp
      (_, cw) <- anOpen alice owner 1 "an issue"
      void $ commitOk dir Nothing cw 1000
      v <- git dir ["cat-file", "-p", "refs/hbs2/meta:version"]
      v `shouldBe` Text.unpack (Text.strip renderMeta)
