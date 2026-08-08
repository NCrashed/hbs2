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
import HBS2.Hub.Compact
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git
import HBS2.Hub.Repo.GitWrite
import HBS2.Hub.CLI.Read (threadsOf,noFilter,listDoc,showDoc,statusOf)
import HBS2.Hub.Canon (renderMeta,metaAt)

import HBS2.Prelude.Plated (Pretty(..))

import HBS2.Net.Auth.Credentials

import Control.Monad (void,(>=>))
import UnliftIO.Async (mapConcurrently)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.HashMap.Strict qualified as HM
import Data.List (sort,isInfixOf)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import System.Environment qualified as Env
import System.Directory (createDirectoryIfMissing,getPermissions,setPermissions,setOwnerExecutable,removeFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed
import Data.Time.Clock (getCurrentTime,diffUTCTime)
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

-- The same, whole. 'git' above answers with the first line, which is what a
-- rev-parse gives; a listing has many.
gitAll :: FilePath -> [String] -> IO String
gitAll cwd args = do
  path <- fromMaybe "/usr/bin:/bin" <$> Env.lookupEnv "PATH"
  let cfg = setEnv [ ("GIT_CONFIG_GLOBAL", "/dev/null")
                   , ("GIT_CONFIG_SYSTEM", "/dev/null")
                   , ("GIT_CONFIG_NOSYSTEM", "1")
                   , ("HOME", cwd)
                   , ("LC_ALL", "C")
                   , ("PATH", path)
                   ]
              (setWorkingDir cwd (proc "git" args))
  (code, out, errOut) <- readProcess (setStdin closed cfg)
  case code of
    ExitSuccess -> pure (LBS8.unpack out)
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
  cw <- either (fail . show) pure (planCanon Nothing [(p, ev)] [(sq, eventId ev)])
  pure (ev, cw)

spec :: Spec
spec = spec1 >> spec2 >> spec3

spec1 :: Spec
spec1 = do

  describe "PEP-19 canon writer against git" $ do

    -- git's index answers a path it will not take with "Ignoring path ..." on
    -- STDERR AND EXIT ZERO, so write-tree, commit-tree and update-ref all
    -- succeed on a tree with the file missing and the verb reports the commit
    -- it made. Reproduced here rather than argued: this is what `hub compact`
    -- would publish if one file in the tree it is rewriting is named like this,
    -- and a compaction puts back the names the tree already had on purpose.
    it "refuses to publish a commit git did not take every file into" $
      withRepo $ \dir -> do
        owner <- kp
        alice <- kp
        (_, cw) <- anOpen alice owner 1 "an issue"

        let bad = [ ("threads/../escaped.sexp", c) | (_, c) <- take 1 (cwFiles cw) ]

        r <- withGitSinkIn (Just dir) $ \sk ->
               skCommit sk (CanonWrite Nothing (cwFiles cw <> bad) "canon" 1000)
        case r of
          Left WriterDropped{} -> pure ()
          other -> expectationFailure
                     ("expected the write to be refused, got " <> show (fmap Text.unpack other))

        -- And nothing was published: the ref is still where it was.
        parent <- withGitSinkIn (Just dir) (skParent >=> either (fail . show) pure)
        parent `shouldBe` Nothing

    it "creates canon the reader folds back" $ withRepo $ \dir -> do
      owner <- kp
      alice <- kp
      (ev, cw) <- anOpen alice owner 1 "an issue"

      parent <- withGitSinkIn (Just dir) (skParent >=> either (fail . show) pure)
      parent `shouldBe` Nothing

      void $ commitOk dir Nothing cw 1000

      st <- readBack dir (fst owner)
      -- The floor, not 1: the fixture's events name no part, so nothing here
      -- raises the version above the lowest a tree may declare.
      stVersion st `shouldBe` Just hubMetaMin
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
      cw <- either (fail . show) pure (planCanon Nothing [(odd', ev)] [])
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
      -- The FLOOR, not this build's constant: the fixture's event names no
      -- part, so nothing in it raises the version above the lowest a tree may
      -- declare. The version a commit declares is a function of what it holds
      -- and of that floor, which is what stops a newer build shutting older
      -- clones out of a tree it added nothing new to.
      --
      -- The FIRST LINE, because the fixture's git helper keeps one and the file
      -- now has two clauses: the rules version and the floor a reader is gated
      -- on. That the two round-trip is CanonSpec's job.
      v `shouldBe` ("(hub-meta " <> show hubMetaMin <> ")")

    -- The whole chain in one assertion: mint, plan, commit to a real git ref,
    -- read it back with the reader, and render it with the verb's own
    -- function. Each half is covered on its own above and in ReadSpec; what
    -- only this can catch is the two halves agreeing about everything except
    -- the thing that joins them.
    it "an issue written to canon is an issue the listing shows" $ withRepo $ \dir -> do
      owner <- kp
      alice <- kp
      let repo = fst owner
          ev = mkEvent alice owner
                 (AOpen repo HubIssue "a real issue" [] (Just "body") Nothing Nothing 1000)
                 (canonOf repo 1 (Just 1))
          p = threadDir (eventId ev) <> "/" <> eventFileName 1 (eventId ev)
      cw <- either (fail . show) pure (planCanon Nothing [(p, ev)] [(1, eventId ev)])
      void $ commitOk dir Nothing cw 1000

      st <- readBack dir repo
      case threadsOf HubIssue noFilter (stFold st) of
        [t] -> do
          tsNumber t `shouldBe` Just 1
          statusOf t `shouldBe` "open"
          unlines (fmap show (listDoc [t])) `shouldSatisfy` ("a real issue" `isInfixOf`)
          unlines (fmap show (showDoc t)) `shouldSatisfy` ("body" `isInfixOf`)
        other -> expectationFailure ("expected one issue, got " <> show (length other))

    -- The bound this whole module's callers pass in, against a git that will
    -- not finish. It bounded nothing: `timeout (readProcess ...)` cannot fire,
    -- because readProcess is a bracket whose release closes the handles that
    -- byteStringOutput's never-cancelled readers are sitting on, so the call
    -- returned when GIT decided to. Measured at 12.02 s against a 2 s bound.
    --
    -- And then it lied about it. The release's own waitForProcess threw on the
    -- way out, tryAny caught it, and the answer was GitUnstartable -- "git
    -- could not be started, worth retrying" -- about a git that had just run to
    -- completion. An index-pack that outran bundleSeconds told the operator to
    -- install git.
    it "gives up on a git that will not finish, and says that is what happened" $ do
      let waits = ["-c", "alias.z=!sleep 30", "z"]
      t0 <- getCurrentTime
      r <- gitRun Nothing [] 2 "slow" waits mempty
      t1 <- getCurrentTime
      case r of
        Left (GitStalled said) ->
          Text.unpack said `shouldSatisfy` ("did not finish" `isInfixOf`)
        other ->
          expectationFailure ("expected GitStalled, got " <> show other)
      -- Well inside the thirty seconds the child asked for: the bound plus the
      -- teardown's own escalation, and nothing waiting on the child itself.
      diffUTCTime t1 t0 `shouldSatisfy` (< 15)

    -- THE PUBLISH REFUSING, which nothing exercised. The test above is caught
    -- by the pre-check -- a stale parent, refused before anything is written --
    -- and the module's own comment says the check that "actually holds when two
    -- accepts race" is the third argument to update-ref. That one is reachable
    -- only by moving the ref BETWEEN the two, which a single-threaded test
    -- cannot do; what it can do is make the publish itself fail and pin what
    -- the writer says about it. It used to say what git said, in the same shape
    -- as "git is not installed", so a caller could not tell "somebody else got
    -- there first" from "this machine is broken".
    it "reports a publish that did not happen, and leaves canon alone" $
      withRepo $ \dir -> do
        owner <- kp
        alice <- kp
        (_, cw1) <- anOpen alice owner 1 "first"
        first' <- commitOk dir Nothing cw1 1000

        -- A hook that aborts every ref transaction: the objects are written and
        -- the ref move is refused, which is the shape of a lost race without
        -- the race.
        let hooks = dir <> "/.git/hooks"
        createDirectoryIfMissing True hooks
        writeFile (hooks <> "/reference-transaction") "#!/bin/sh\nexit 1\n"
        p <- getPermissions (hooks <> "/reference-transaction")
        setPermissions (hooks <> "/reference-transaction") (setOwnerExecutable True p)

        (_, cw2) <- anOpen alice owner 2 "second"
        r <- withGitSinkIn (Just dir) $ \sk ->
               skCommit sk (CanonWrite (Just first') (cwFiles cw2) "canon" 2000)
        case r of
          Left _ -> pure ()
          Right c -> expectationFailure ("the publish was refused and reported success: " <> show c)

        -- And canon is where it was: everything before the publish writes
        -- objects git will collect, and nothing anybody fetches has changed.
        removeFile (hooks <> "/reference-transaction")
        head' <- Text.pack <$> git dir ["rev-parse", Text.unpack metaRef]
        head' `shouldBe` first'

-- The property the CAS exists for, run as a race rather than as a sequence.
--
-- The sibling test above proves the check FIRES when the ref has already moved,
-- which is the easy half: it is one process observing another's finished work.
-- What it does not exercise is the window this build actually runs in -- two
-- accepts holding the same parent, both committing, neither having seen the
-- other -- and the whole reason canon is published with `update-ref <new>
-- <old>` rather than `update-ref <new>` is that git takes the ref lock and one
-- of them loses.
--
-- A LOST WRITE HERE IS A LOST EVENT, permanently: the loser's commit is written
-- and unreferenced, so canon simply does not have the letter its maintainer
-- accepted, and the mailbox path has already dropped it.
spec2 :: Spec
spec2 =
  describe "PEP-19 canon writer: two accepts at once" $ do

    it "lets exactly one of a crowd publish, and refuses the rest" $
      withRepo $ \dir -> do
        owner <- kp
        alice <- kp

        -- A parent both sides hold: the state a queue is triaged from.
        (_, cw0) <- anOpen alice owner 1 "first"
        parent <- commitOk dir Nothing cw0 1000

        -- Eight events, each a valid next commit onto that parent, all minted
        -- against the same canon. This is a queue with eight letters in it and
        -- a maintainer accepting them in parallel.
        plans <- mapM (\n -> snd <$> anOpen alice owner n ("racer " <> Text.pack (show n)))
                      [2 .. 9]

        rs <- mapConcurrently
                (\cw -> withGitSinkIn (Just dir) $ \sk ->
                          skCommit sk (CanonWrite (Just parent) (cwFiles cw) "canon" 2000))
                plans

        let won = [ c | Right c <- rs ]
            moved = [ () | Left RefMoved{} <- rs ]

        length won `shouldBe` 1
        -- Every other one is refused as what it is, and not as a git error:
        -- an accept that cannot tell "somebody else got there" from "git is
        -- broken" retries the wrong one of them.
        length moved `shouldBe` length plans - 1

        -- And the ref holds the winner, which is the part a count would miss:
        -- one success plus seven refusals is also what "the last writer wins
        -- and the ref is somebody else's" looks like.
        head' <- git dir ["rev-parse", "refs/hbs2/meta"]
        [head'] `shouldBe` map Text.unpack won

    -- The same shape with the parent nobody holds: every one of them believes
    -- canon does not exist yet, which is the FIRST accept in a repository and
    -- the case where a lost write loses the whole thread rather than one event.
    it "lets exactly one of a crowd create canon" $
      withRepo $ \dir -> do
        owner <- kp
        alice <- kp

        plans <- mapM (\n -> snd <$> anOpen alice owner n ("first " <> Text.pack (show n)))
                      [1 .. 8]

        rs <- mapConcurrently
                (\cw -> withGitSinkIn (Just dir) $ \sk ->
                          skCommit sk (CanonWrite Nothing (cwFiles cw) "canon" 1000))
                plans

        length [ c | Right c <- rs ] `shouldBe` 1
        length [ () | Left RefMoved{} <- rs ] `shouldBe` length plans - 1

-- The rewrite, end to end against real git: canon written, compacted, and read
-- back. The rule is tested on its own in HBS2.Hub.CompactSpec; what is here is
-- that a lineage written by 'skRewrite' is one the reader folds to the same
-- state, which is the claim a clone checks before taking it.
spec3 :: Spec
spec3 =
  describe "PEP-19 canon writer: a rewritten lineage" $ do

    it "writes a root commit that keeps only the files it was given" $
      withRepo $ \dir -> do
        owner <- kp
        alice <- kp
        (_, cw1) <- anOpen alice owner 1 "first"
        first' <- commitOk dir Nothing cw1 1000
        (_, cw2) <- anOpen alice owner 2 "second"
        second <- commitOk dir (Just first') cw2 2000

        -- A lineage holding only the first event, replacing a canon that has
        -- two: this is what a compaction does, with the plan chosen by hand so
        -- the test does not depend on the policy.
        rewritten <- withGitSinkIn (Just dir) $ \sk ->
          skRewrite sk (CanonWrite (Just second) (cwFiles cw1) "compacted" 3000)
            >>= either (fail . show) pure

        -- A ROOT: no parent, so the old lineage is not reachable from the new
        -- ref and the size win is real.
        parents <- git dir ["rev-list", "--parents", "-n", "1", Text.unpack rewritten]
        words parents `shouldSatisfy` ((== 1) . length)

        -- The tree holds the first event and not the second.
        names <- gitAll dir ["ls-tree", "-r", "--name-only", Text.unpack rewritten]
        length [ n | n <- lines names, "threads/" `isInfixOf` n ] `shouldBe` 1

        -- And the ref moved onto it.
        git dir ["rev-parse", "refs/hbs2/meta"] >>= (`shouldBe` Text.unpack rewritten)

    -- The swap is against the canon that was compacted, not against the
    -- commit's (absent) parent: a letter folded between the read and the write
    -- must lose the race rather than be replaced.
    it "refuses to publish a rewrite onto a canon that moved" $
      withRepo $ \dir -> do
        owner <- kp
        alice <- kp
        (_, cw1) <- anOpen alice owner 1 "first"
        first' <- commitOk dir Nothing cw1 1000
        (_, cw2) <- anOpen alice owner 2 "second"
        _ <- commitOk dir (Just first') cw2 2000

        r <- withGitSinkIn (Just dir) $ \sk ->
               skRewrite sk (CanonWrite (Just first') (cwFiles cw1) "compacted" 3000)
        case r of
          Left (RefMoved want got) -> do
            want `shouldBe` Just first'
            got `shouldSatisfy` (/= Just first')
          other -> expectationFailure ("expected RefMoved, got " <> show other)

    it "produces a lineage the reader folds to the same state" $
      withRepo $ \dir -> do
        owner <- kp
        alice <- kp
        let repo = fst owner

        (e1, cw1) <- anOpen alice owner 1 "first"
        c1 <- commitOk dir Nothing cw1 1000

        -- Two sets on one attribute, the first superseded by the second.
        let thr = eventId e1
            s1 = mkEvent owner owner (ASet thr "labels" (encodeLabels ["a"]) 2000)
                         (canonOf repo 2 Nothing)
            s2 = mkEvent owner owner (ASet thr "labels" (encodeLabels ["b"]) 3000)
                         (canonOf repo 3 Nothing)
        cwBoth <- either (fail . show) pure
                    (planCanon Nothing [ (threadDir thr <> "/" <> eventFileName 2 (eventId s1), s1)
                               , (threadDir thr <> "/" <> eventFileName 3 (eventId s2), s2) ]
                               [(1, thr)])
        c2 <- commitOk dir (Just c1) cwBoth 3000

        before <- readBack dir repo

        -- Everything the tree holds, minus what the rule says may go.
        let c = compactionOf (stFold before) (fmap snd (stEvents before))
            held = [ (BS8.unpack p, e) | (p, e) <- stEvents before
                                       , eventId e `elem` fmap eventId (cpKeep c) ]
        length (cpDrop c) `shouldBe` 1

        plan <- either (fail . show) pure
                  (planCanon (stVersion before) held (numberIndexOf (stFold before)))
        _ <- withGitSinkIn (Just dir) $ \sk ->
               skRewrite sk (CanonWrite (Just c2) (cwFiles plan) "compacted" 4000)
                 >>= either (fail . show) pure

        after <- readBack dir repo
        equivalentTo (stFold before) (stFold after) `shouldBe` True
