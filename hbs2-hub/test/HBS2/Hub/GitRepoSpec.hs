-- | 'gitCanon' against real git.
--
-- The one part of reading canon that cannot be checked against a tree in memory,
-- because what it does IS talk to git: parse an @ls-tree -l@ record, tell the
-- exit codes of @rev-parse@ apart, and resolve paths from the root of the tree
-- rather than from wherever the caller happens to be standing.
--
-- Each of those has been wrong once already. The listing parser is the one with
-- no natural alarm: it drops what it cannot read, so a format shift would empty
-- the listing and print a clean empty audit, which is the failure the reader is
-- built to not have.
module HBS2.Hub.GitRepoSpec (spec) where

import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git

import Control.Exception (bracket,bracket_)
import Control.Monad (void,forM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Foldable (for_)
import Data.List (sort)
import Data.Maybe (fromMaybe,isJust)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Control.Monad (unless)
import System.Directory ( createDirectoryIfMissing,findExecutable,getPermissions
                        , setPermissions,setOwnerExecutable,doesFileExist )
import System.Environment qualified as Env
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory,getCanonicalTemporaryDirectory)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar,newEmptyMVar,putMVar,takeMVar)
import Control.Exception (try,throwIO,SomeException)
import System.Timeout (timeout)
import System.Process.Typed
import Test.Hspec

-- Run git in a directory and insist it worked.
git :: FilePath -> [String] -> IO String
git cwd args = do
  p <- gitProc cwd args
  (code, out, errOut) <- readProcess (setStdin closed p)
  case code of
    ExitSuccess -> pure (trim out)
    _ -> fail ("git " <> unwords args <> ": " <> LBS8.unpack errOut)

-- Git, sealed off from whatever the developer or the CI has set.
--
-- Not a nicety. setWorkingDir does NOT beat GIT_DIR, so this suite run under a
-- git hook, @git bisect run@ or @git rebase -x@ inherited a GIT_DIR pointing at a
-- real repository, and the @read-tree --empty@ and @update-index@ below then
-- wrote the index and refs/hbs2/meta of THAT repository. It happened, and it is
-- the reason the environment is replaced rather than added to.
--
-- The rest is the same class of leak, one step less destructive: a global
-- core.hooksPath with a reference-transaction hook fails every @git init@ here; a
-- global commit.gpgsign stops the suite for a passphrase; a non-English locale
-- changes the messages a test below matches on; and a TMPDIR inside somebody's
-- repository turns "this is not a git repository" into "it is".
gitProc :: FilePath -> [String] -> IO (ProcessConfig () () ())
gitProc cwd args = do
  -- The caller's PATH, and it has to be passed explicitly: with the environment
  -- replaced and no PATH in it, the lookup for "git" has nowhere to look. It used
  -- to be a hardcoded /usr/bin:/bin plus one NixOS path, which is a suite that
  -- passes on this machine and cannot find git on anybody else's.
  path <- fromMaybe "/usr/bin:/bin" <$> Env.lookupEnv "PATH"
  pure (setEnv (env <> [("PATH", path)]) (setWorkingDir cwd (proc "git" (config <> args))))
  where
    config = [ "-c", "user.email=t@t", "-c", "user.name=t"
             , "-c", "commit.gpgsign=false" ]

    -- Replaced whole, so no GIT_* survives. HOME points into the temp directory
    -- so a global config cannot be found there either.
    --
    -- No PATH: git is looked up through the caller's, because a hardcoded one is
    -- a guess about the machine. It was /usr/bin:/bin plus one NixOS path, which
    -- is a suite that passes here and cannot find git anywhere else.
    env = [ ("GIT_CONFIG_GLOBAL", "/dev/null")
          , ("GIT_CONFIG_SYSTEM", "/dev/null")
          , ("GIT_CONFIG_NOSYSTEM", "1")
          , ("HOME", cwd)
          , ("LC_ALL", "C")
          , ("GIT_CEILING_DIRECTORIES", cwd)
          ]

-- A literal as the bytes it reads as, not as Char8 truncation.
utf8 :: String -> ByteString
utf8 = Text.encodeUtf8 . Text.pack

trim :: LBS8.ByteString -> String
trim = takeWhile (`notElem` "\n\r") . LBS8.unpack

-- A repository holding a canon tree, built through git plumbing rather than the
-- filesystem, so a path may contain what a filesystem will not take.
--
-- Paths and contents go in over stdin, never as a process argument. Not
-- fastidiousness: the suite runs under LC_ALL=C, where GHC encodes arguments as
-- ASCII, so a fixture that passed a path or a blob through argv wrote bytes
-- nobody chose. It did: the first version of the multibyte test below built its
-- blob with LBS8.pack and asserted against the six characters it had truncated
-- to six bytes, and the reader read exactly what the fixture had written.
withCanon :: [(ByteString, ByteString)] -> (FilePath -> IO a) -> IO a
withCanon files act = withSystemTempDirectory "hub-git" $ \dir -> do
  void $ git dir ["init", "-q", "."]
  void $ git dir ["read-tree", "--empty"]
  recs <- forM files $ \(p, content) -> do
            h <- hashObject dir content
            pure (B8.pack ("100644 " <> h) <> "\t" <> p <> "\0")
  void $ gitStdin dir (LBS8.fromStrict (mconcat recs))
           ["update-index", "-z", "--index-info"]
  tree <- git dir ["write-tree"]
  commit <- git dir ["commit-tree", tree, "-m", "canon"]
  void $ git dir ["update-ref", "refs/hbs2/meta", commit]
  act dir

hashObject :: FilePath -> ByteString -> IO String
hashObject dir content =
  gitStdin dir (LBS8.fromStrict content) ["hash-object", "-w", "--stdin"]

-- Git, whole output, and it worked. Not 'git', which keeps the first line: a
-- test that counted lines of it counted a line.
gitAll :: FilePath -> [String] -> IO [String]
gitAll cwd args = do
  p <- gitProc cwd args
  (code, out, errOut) <- readProcess (setStdin closed p)
  case code of
    ExitSuccess -> pure (lines (LBS8.unpack out))
    _ -> fail ("git " <> unwords args <> ": " <> LBS8.unpack errOut)

-- Git with bytes on stdin, and it worked.
gitStdin :: FilePath -> LBS8.ByteString -> [String] -> IO String
gitStdin dir input args = do
  p <- gitProc dir args
  (code, out, errOut) <- readProcess (setStdin (byteStringInput input) p)
  case code of
    ExitSuccess -> pure (trim out)
    _ -> fail ("git " <> unwords args <> ": " <> LBS8.unpack errOut)

-- A git that writes one record, then goes silent for ever, and ignores SIGTERM.
--
-- Both halves matter: the silence is what the idle bound is for, and refusing to
-- die is what the teardown escalation is for. A shim that stopped on SIGTERM
-- would pass on a reader whose cleanup waits without limit.
stalling :: String -> GitBounds
         -> (IO (Either CanonUnreadable [TreeEntry]) -> IO a) -> IO a
stalling what bounds act = withSystemTempDirectory "hub-stall" $ \dir -> do
  let bin = dir </> "bin"
  createDirectoryIfMissing True bin
  sleep <- fromMaybe "/bin/sleep" <$> findExecutable "sleep"
  writeFile (bin </> "git") (unlines
    [ "#!/bin/sh"
    , "trap '' TERM"
    , "case \"$*\" in"
      -- WHICH command stalls is a parameter, because the listing and the four
      -- small calls are different code with different failure modes. A shim that
      -- only ever stalled ls-tree left the small calls' timeout branch unexecuted
      -- by the entire suite, which is how a deadlock lived in it: the test written
      -- to cover that branch passed against the bug, because it never reached it.
    , "  *" <> what <> "*)"
    , "    printf '100644 blob deadbeefdeadbeefdeadbeefdeadbeefdeadbeef      5\\tthreads/t/0001-x\\000'"
    -- Thirty seconds, not ten minutes: the bounds under test are one second, so
    -- this is thirty times over, and sleeping for ten minutes left two orphans per
    -- suite run. The sleep is a GRANDCHILD, and the teardown signals the child it
    -- started, not the process group.
    , "    " <> sleep <> " 30 ;;"
    , "  *rev-parse*git-dir*) echo .git ;;"
    , "  *show-ref*) exit 0 ;;"
    , "  *rev-parse*) echo 0000000000000000000000000000000000000000 ;;"
    , "  *) exit 1 ;;"
    , "esac"
    ])
  perm <- getPermissions (bin </> "git")
  setPermissions (bin </> "git") (setOwnerExecutable True perm)
  old <- Env.lookupEnv "PATH"
  bracket_ (Env.setEnv "PATH" (bin <> ":" <> fromMaybe "" old))
           (maybe (Env.unsetEnv "PATH") (Env.setEnv "PATH") old)
           (act (readTreeWith bounds dir))

-- A git that never stops talking and never finishes: one byte every tenth of a
-- second, for ever. Never idle, so only a deadline catches it.
dribbling :: GitBounds -> (IO (Either CanonUnreadable [TreeEntry]) -> IO a) -> IO a
dribbling bounds act = withSystemTempDirectory "hub-drip" $ \dir -> do
  let bin = dir </> "bin"
  createDirectoryIfMissing True bin
  sleep <- fromMaybe "/bin/sleep" <$> findExecutable "sleep"
  writeFile (bin </> "git") (unlines
    [ "#!/bin/sh"
    , "case \"$*\" in"
    , "  *ls-tree*)"
    , "    i=0; while [ $i -lt 300 ]; do printf x; " <> sleep <> " 0.1; i=$((i+1)); done ;;"
    , "  *rev-parse*git-dir*) echo .git ;;"
    , "  *show-ref*) exit 0 ;;"
    , "  *rev-parse*) echo 0000000000000000000000000000000000000000 ;;"
    , "  *) exit 1 ;;"
    , "esac"
    ])
  perm <- getPermissions (bin </> "git")
  setPermissions (bin </> "git") (setOwnerExecutable True perm)
  old <- Env.lookupEnv "PATH"
  bracket_ (Env.setEnv "PATH" (bin <> ":" <> fromMaybe "" old))
           (maybe (Env.unsetEnv "PATH") (Env.setEnv "PATH") old)
           (act (readTreeWith bounds dir))

-- A blobless clone of a canon-carrying repository, and a count of the blobs it
-- holds. Both files are absent from it until something fetches them, which is the
-- whole point: the count is how a test says "and nothing was fetched".
--
-- A local file:// promisor with uploadpack.allowFilter, so no network.
partial :: (FilePath -> IO Int -> IO a) -> IO a
partial act = withSystemTempDirectory "hub-partial" $ \root -> do
  let origin = root </> "origin"
      clone  = root </> "clone"
  createDirectoryIfMissing True origin
  void $ git origin ["init", "-q", "."]
  void $ git origin ["config", "uploadpack.allowFilter", "true"]
  h <- hashObject origin "(hub-meta 1)\n"
  e <- hashObject origin "not an event"
  void $ gitStdin origin
           (LBS8.pack ("100644 " <> h <> "\tversion\0"
                       <> "100644 " <> e <> "\tthreads/t/0001-x\0"))
           ["update-index", "-z", "--index-info"]
  tree <- git origin ["write-tree"]
  commit <- git origin ["commit-tree", tree, "-m", "canon"]
  void $ git origin ["update-ref", "refs/hbs2/meta", commit]
  void $ git root [ "clone", "-q", "--filter=blob:none", "--no-checkout"
                  , "--no-local", "file://" <> origin, clone ]
  void $ git clone ["fetch", "-q", "origin", "+refs/hbs2/meta:refs/hbs2/meta"]
  -- gitAll, not git: the latter keeps the FIRST LINE of the output, so counting
  -- blobs in it counted at most one, and the assertion this exists for passed
  -- whenever the lowest-oid object was not a blob.
  act clone (length . filter (== "blob")
               <$> gitAll clone [ "cat-file", "--batch-all-objects"
                                , "--batch-check=%(objecttype)" ])

-- A kind, without the object id, which varies from run to run.
data Kind = Sized Int | Missing | NotBlob | Bad
  deriving stock (Eq,Ord,Show)

kindOf :: EntryKind -> Kind
kindOf = \case
  Blob _ n      -> Sized n
  BlobMissing _ -> Missing
  NotABlob      -> NotBlob
  Unparsed      -> Bad

-- A test that must not HANG when it regresses.
--
-- Two of the tests below are about a reader that stops: if either regresses, the
-- reader waits for ever and hspec waits with it, so CI burns its six-hour limit
-- instead of going red. hspec has no per-example timeout, so it is here.
within :: Int -> String -> IO () -> IO ()
within secs what act = do
  -- The work runs on ANOTHER THREAD and this one waits on an MVar. `timeout` around
  -- the work itself cannot help here, and that is the whole reason for the shape:
  -- the hang being tested for is inside a bracket release, which unliftio runs
  -- under uninterruptibleMask_, where the exception `timeout` throws is deferred.
  -- Measured: the pre-fix version of the reader made this example HANG for ten
  -- minutes instead of failing at thirty seconds, so the test that was written to
  -- catch a deadlock could only ever have been caught by CI's own limit.
  --
  -- takeMVar here is not inside anybody's mask, so it interrupts, and the example
  -- goes red on time. The forked thread is left running: it is stuck by
  -- definition, and there is nothing to be done about that from here.
  done <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  _ <- forkIO (try act >>= putMVar done)
  timeout (secs * 1000000) (takeMVar done) >>= \case
    Just (Right ()) -> pure ()
    Just (Left e)   -> throwIO e
    Nothing -> expectationFailure
                 (what <> ": did not finish in " <> show secs <> "s, which for this"
                       <> " test means the reader stopped stopping")

-- A fake git on PATH, printing a fixed listing on stdout and n lines of noise on
-- stderr, and a read of "canon" through it.
--
-- A shim because the real trigger for the thing being tested (a lazy fetch in a
-- partial clone, chattering on stderr while ls-tree streams stdout) needs a
-- promisor remote and a network, and what is under test is this reader's handling
-- of two pipes, not git's.
--
-- PATH is set for the duration and put back, because 'gitCanonWith' looks git up
-- through the environment on purpose and this is the only way in. Restored rather
-- than left, and bracketed rather than assigned: the suite is sequential (hspec's
-- default), so this is safe, and the bracket is what keeps it safe if that ever
-- stops being true for the specs around it.
shimmed :: Int -> ByteString
        -> (IO (Either CanonUnreadable [TreeEntry]) -> IO a) -> IO a
shimmed noise out act = shimmedWith gitBounds noise 0 out (\r _ -> act r)

-- A git that answers once and then is not there any more: it deletes itself after
-- the first question, so the second call cannot spawn. That is what a process
-- limit reached partway through an audit looks like from here, and the only way
-- to reach the branch from a test.
vanishing :: (IO (Either CanonUnreadable [TreeEntry]) -> IO a) -> IO a
vanishing act =
  withSystemTempDirectory "hub-vanish" $ \dir -> do
    let bin = dir </> "bin"
    createDirectoryIfMissing True bin
    -- An absolute rm, looked up out here: PATH below is the shim directory alone,
    -- so the shim cannot find one itself, and /bin/rm is not on every machine
    -- (it is not on NixOS).
    rm <- fromMaybe "/bin/rm" <$> findExecutable "rm"
    writeFile (bin </> "git") (unlines
      [ "#!/bin/sh"
      , "case \"$*\" in"
      , "  *rev-parse*git-dir*) echo .git; " <> rm <> " -f \"$0\" ;;"
      , "  *) exit 1 ;;"
      , "esac"
      ])
    perm <- getPermissions (bin </> "git")
    setPermissions (bin </> "git") (setOwnerExecutable True perm)
    old <- Env.lookupEnv "PATH"
    -- PATH is ONLY the shim directory, so that once the shim removes itself there
    -- is no git anywhere: with the real one still on PATH the second call found
    -- it, and it answered about a directory that is not a repository.
    bracket_ (Env.setEnv "PATH" bin)
             (maybe (Env.unsetEnv "PATH") (Env.setEnv "PATH") old)
             (act (readTreeWith gitBounds dir))

-- The reader, and how many pieces of the listing the shim got to write.
--
-- The count is what makes "the reader stopped early" observable at all. A marker
-- file after the last piece was the first attempt and it was flaky: whether the
-- shell survives the SIGTERM long enough to run one more command is up to the
-- shell, so the marker sometimes appeared for a reader that had stopped. A piece
-- count only ever goes up while somebody is reading, so a count below the total
-- means the writer was stopped, whoever got the signal.
shimmedWith :: GitBounds -> Int -> Int -> ByteString
            -> (IO (Either CanonUnreadable [TreeEntry]) -> IO (Int, Int) -> IO a)
            -> IO a
shimmedWith bounds noise showRefCode out act =
  withSystemTempDirectory "hub-shim" $ \dir -> do
    let bin = dir </> "bin"
        -- Pieces a little under a pipe buffer, so a writer that is not being read
        -- blocks partway rather than finishing in one syscall.
        pieces = chunksOf 32768 out
        piece i = bin </> ("listing." <> show i)
        progress = bin </> "progress"
    createDirectoryIfMissing True bin
    for_ (zip [0 :: Int ..] pieces) $ \(i, p) -> BS.writeFile (piece i) p
    writeFile (bin </> "git") (unlines $
      [ "#!/bin/sh"
      , "noise() { i=0; while [ $i -lt $1 ]; do"
      , "  echo \"remote: enumerating objects $i, done.\" >&2; i=$((i+1)); done; }"
      , "case \"$*\" in"
      , "  *ls-tree*)"
      -- Noise on BOTH sides of the listing, and unread by design. Before it, so
      -- the child can fill the 64 KiB stderr pipe while the parent is still on
      -- stdout; after it, so a reader that finished stdout and then waited on the
      -- process is covered too. One order alone would leave the other untested,
      -- and the untested one is a hang, which reads as a slow CI rather than a
      -- red one.
      , "    noise " <> show (noise `div` 2)
      ] <>
      [ "    cat " <> piece i <> " && echo x >> " <> progress
      | i <- [0 .. length pieces - 1] ] <>
      [ "    noise " <> show (noise - noise `div` 2)
      , "    ;;"
      , "  *rev-parse*git-dir*) echo .git ;;"
      , "  *show-ref*)"
      , "    if [ " <> show showRefCode <> " -ne 0 ]; then"
      -- Two lines, like git's own: the complaint, and what to run about it.
      , "      echo \"fatal: git show-ref: bad ref refs/hbs2/meta (deadbeef)\" >&2"
      , "      echo \"run this to fix it: git fsck --no-progress\" >&2"
      , "    fi"
      , "    exit " <> show showRefCode <> " ;;"
      , "  *rev-parse*) echo 0000000000000000000000000000000000000000 ;;"
      , "  *) exit 1 ;;"
      , "esac"
      ])
    perm <- getPermissions (bin </> "git")
    setPermissions (bin </> "git") (setOwnerExecutable True perm)
    old <- Env.lookupEnv "PATH"
    bracket_ (Env.setEnv "PATH" (bin <> ":" <> fromMaybe "" old))
             (maybe (Env.unsetEnv "PATH") (Env.setEnv "PATH") old)
             (act (readTreeWith bounds dir) (written progress (length pieces)))

-- How many pieces the shim finished, and how many there were.
written :: FilePath -> Int -> IO (Int, Int)
written progress total = do
  here <- doesFileExist progress
  n <- if here then length . lines <$> readFile progress else pure 0
  pure (n, total)

-- A ByteString in pieces of at most n bytes.
chunksOf :: Int -> ByteString -> [ByteString]
chunksOf n bs
  | BS.null bs = []
  | otherwise  = BS.take n bs : chunksOf n (BS.drop n bs)

-- Entries as the reader returns them, whole.
readTreeWith :: GitBounds -> FilePath -> IO (Either CanonUnreadable [TreeEntry])
readTreeWith bounds dir = do
  let cs = gitCanonWith bounds (Just dir)
  csCommit cs >>= \case
    Left e  -> pure (Left e)
    Right c -> csEntries cs c

-- Read canon from a directory, with the process's own working directory left
-- alone: gitCanon runs git, so the directory has to reach it some other way.
readIn :: FilePath -> IO (Either CanonUnreadable [(ByteString, Kind)])
readIn dir = do
  let cs = gitCanonIn (Just dir)
  commit <- csCommit cs
  case commit of
    Left e -> pure (Left e)
    Right c -> csEntries cs c >>= \case
      Left e -> pure (Left e)
      Right es -> pure (Right (sort [ (teePath e, kindOf (teeKind e)) | e <- es ]))

-- One test, with the environment it needs, put back afterwards.
--
-- The reads below go through the production 'gitCanonIn', so what it inherits
-- from this process it inherits for real, and this is the only way to arrange
-- what it inherits. The GIT_DIR family is deliberately NOT here: 'gitCanonIn'
-- strips those itself when a directory is named, which is a property of the
-- production code and is asserted below rather than arranged away.
--
-- What is left is what production cannot decide for a caller. The config files,
-- because a global commit.gpgsign or core.hooksPath belongs to the developer;
-- LC_ALL, because a test below matches on a message git writes; and the ceiling,
-- because withSystemTempDirectory hands out a path under TMPDIR, and TMPDIR
-- inside somebody's repository turns "this is not a git repository" into "it is".
--
-- Scoped and restored rather than assigned once at spec load, which is what it
-- was. Assigning leaks LC_ALL=C into every spec that runs after this one, and
-- the leak is invisible while hspec runs specs one at a time and immediate the
-- day anything here or above is marked parallel.
isolated :: IO () -> IO ()
isolated act = do
  tmp <- getCanonicalTemporaryDirectory
  let wanted = [ ("GIT_CONFIG_GLOBAL", "/dev/null")
               , ("GIT_CONFIG_SYSTEM", "/dev/null")
               , ("LC_ALL", "C")
               , ("GIT_CEILING_DIRECTORIES", tmp)
               ]
  before <- mapM (\(k,_) -> (,) k <$> Env.lookupEnv k) wanted
  bracket_ (mapM_ (uncurry Env.setEnv) wanted)
           (mapM_ (\(k,v) -> maybe (Env.unsetEnv k) (Env.setEnv k) v) before)
           act

spec :: Spec
spec = do

  -- Skipped rather than failed where git is not installed: this is the one spec
  -- in the suite that needs it, and a red suite on a machine without git says
  -- something false about the code.
  hasGit <- runIO (isJust <$> findExecutable "git")

  around_ (\act -> if hasGit then isolated act
                             else pendingWith "git is not on PATH") $
   describe "gitCanon against real git" $ do

    it "parses an ls-tree record into a path and a size" $ do
      -- The parser has no natural alarm: an unreadable record used to be dropped,
      -- so a change in git's output would have emptied the listing and printed a
      -- clean empty audit. Pinned against the git that is actually installed.
      withCanon [("version", "(hub-meta 1)\n"), ("threads/t/0001-x", "abcde")] $ \dir ->
        readIn dir `shouldReturn`
          Right [ ("threads/t/0001-x", Sized 5), ("version", Sized 13) ]

    it "reads the root of the tree, not the caller's directory" $ do
      -- ls-tree resolves paths relative to the working directory and cat-file
      -- resolves them from the root, so without --full-tree the two disagree: run
      -- from a subdirectory this listed nothing and reported clean empty canon,
      -- and where the subdirectory held a same-named path it listed one file and
      -- read another.
      withCanon [ ("version", "(hub-meta 1)\n")
                , ("threads/t/0001-x", "root")
                , ("sub/threads/t/0001-x", "shadow")
                ] $ \dir -> do
        createDirectoryIfMissing True (dir </> "sub")
        readIn (dir </> "sub") `shouldReturn`
          Right [ ("sub/threads/t/0001-x", Sized 6)
                , ("threads/t/0001-x", Sized 4)
                , ("version", Sized 13) ]

    it "keeps a path with a newline in it whole" $ do
      -- -z is why: a path in a git tree may hold anything but NUL, and this one
      -- forged a line of the audit report before the paths went through safeText.
      let evil :: ByteString
          evil = "threads/t/0001-evil\nadmitted 999\nignored"
      withCanon [("version", "(hub-meta 1)\n"), (evil, "junk")] $ \dir ->
        readIn dir `shouldReturn`
          Right [ (evil, Sized 4), ("version", Sized 13) ]

    it "reports a submodule as an entry rather than dropping it" $ do
      -- A gitlink has no size in the listing. Filtering it out made something
      -- somebody put in canon invisible to the audit; it now arrives with no
      -- size and is refused by name.
      withSystemTempDirectory "hub-git" $ \dir -> do
        void $ git dir ["init", "-q", "."]
        void $ git dir ["read-tree", "--empty"]
        h <- hashObject dir "(hub-meta 1)\n"
        void $ git dir [ "update-index", "--add", "--cacheinfo"
                       , "100644," <> h <> ",version" ]
        -- Any 40-hex object id will do: the tree records a gitlink whether or not
        -- the commit is present.
        void $ git dir [ "update-index", "--add", "--cacheinfo"
                       , "160000," <> replicate 40 'a'
                           <> ",threads/t/0001-sub" ]
        tree <- git dir ["write-tree"]
        commit <- git dir ["commit-tree", tree, "-m", "canon"]
        void $ git dir ["update-ref", "refs/hbs2/meta", commit]
        readIn dir `shouldReturn`
          Right [ ("threads/t/0001-sub", NotBlob), ("version", Sized 13) ]

    it "reads the repository it was given, not the one GIT_DIR names" $ do
      -- setWorkingDir does not beat GIT_DIR, so with one set every read below went
      -- to that repository instead and answered about it: a hook, where git sets
      -- GIT_DIR for every invocation, audited whatever was in the environment and
      -- looked right doing it.
      withCanon [("version", "(hub-meta 1)\n"), ("threads/t/0001-x", "abcde")] $ \named ->
        withSystemTempDirectory "hub-elsewhere" $ \other -> do
          void $ git other ["init", "-q", "."]
          bracket (Env.setEnv "GIT_DIR" (other </> ".git"))
                  (const (Env.unsetEnv "GIT_DIR"))
                  (const (readIn named)) `shouldReturn`
            Right [ ("threads/t/0001-x", Sized 5), ("version", Sized 13) ]

    it "obeys GIT_DIR when no directory was named" $ do
      -- The other half of the pair above, and the half that is easy to break
      -- while fixing that one: with no directory named, the environment IS the
      -- caller's answer to "which repository", and a hook's GIT_DIR is the right
      -- one to obey. gitCanon is what `hub verify` uses, so this is the path in
      -- production and it had no test at all.
      withCanon [("version", "(hub-meta 1)\n")] $ \dir -> do
        wanted <- git dir ["rev-parse", "refs/hbs2/meta"]
        bracket_ (Env.setEnv "GIT_DIR" (dir </> ".git"))
                 (Env.unsetEnv "GIT_DIR")
                 (csCommit (gitCanon @IO) >>= \case
                    -- The commit itself, not its length: a reader that answered
                    -- with any forty characters would pass a length check, and
                    -- what this is about is WHICH repository was read.
                    Right c -> Text.unpack c `shouldBe` wanted
                    Left e -> expectationFailure
                                ("expected a commit, got " <> show e))

    it "lists a blobless clone without fetching a single object" $ do
      -- THE FETCH. ls-tree -l has to know the size of every entry, so in a
      -- partial clone it drives a lazy fetch per missing blob: the flag that is
      -- here so a bound can refuse a blob WITHOUT fetching it was fetching
      -- everything, before any bound could speak. A four-gigabyte event file
      -- landed on disk and was then refused as too large.
      --
      -- Worse than the bytes: the fetch goes wherever the AUDITED repository's
      -- config says, through its remote urls, its core.sshCommand and its
      -- credential.helper. PEP-22 calls this verb read-only and peerless.
      --
      -- A local file:// promisor, so this needs no network to prove it.
      partial $ \clone blobs -> do
        blobs `shouldReturn` 0

        -- The listing still reports both files, and reports the ones whose
        -- objects are absent as absent: BAD in the size column, which is what
        -- git prints when it is told not to go and get them.
        readIn clone `shouldReturn`
          Right [ ("threads/t/0001-x", Missing), ("version", Missing) ]

        -- And nothing was fetched. This is the assertion the whole test is for.
        blobs `shouldReturn` 0

    it "fetches nothing even when the caller's environment says to" $ do
      -- The fix above is a set of variables, and setting them is not the same as
      -- them taking effect: with two bindings of one name in envp, getenv returns
      -- the FIRST, so appending them to the inherited environment did nothing at
      -- all whenever the caller already had one. A reader that appends passes the
      -- test above and fails this one.
      partial $ \clone blobs -> do
        bracket_ (Env.setEnv "GIT_NO_LAZY_FETCH" "0")
                 (Env.unsetEnv "GIT_NO_LAZY_FETCH") $ do
          readIn clone `shouldReturn`
            Right [ ("threads/t/0001-x", Missing), ("version", Missing) ]
          blobs `shouldReturn` 0

    it "reads the tree the commit names, not the one refs/replace substitutes" $ do
      -- refs/replace rewrites what a commit IS, for every git command that reads
      -- it. So canon could be read out of a planted tree while the report printed
      -- the honest commit id in its header: nothing in the output would say a
      -- substitution had happened. The documented fetch refspec does not carry
      -- those refs, but `clone --mirror` does, and so does a push into a bare
      -- repository.
      withCanon [("version", "(hub-meta 1)\n")] $ \dir -> do
        honest <- git dir ["rev-parse", "refs/hbs2/meta"]
        planted <- hashObject dir "planted"
        void $ gitStdin dir
                 (LBS8.pack ("100644 " <> planted <> "\tthreads/evil/0001-planted\0"))
                 ["update-index", "-z", "--index-info"]
        tree <- git dir ["write-tree"]
        evil <- git dir ["commit-tree", tree, "-m", "evil"]
        void $ git dir ["replace", honest, evil]
        -- git itself honours it, which is what makes this worth a test.
        gitAll dir ["ls-tree", "-r", "--name-only", "refs/hbs2/meta"]
          >>= \ls -> ls `shouldContain` ["threads/evil/0001-planted"]
        -- And this reader does not.
        readIn dir `shouldReturn` Right [("version", Sized 13)]

    it "tells a blob whose object is gone from a submodule" $ do
      -- ls-tree -l prints BAD, not a number, in the size column of a blob it
      -- cannot find, which is what a blobless or partial clone looks like from
      -- here. Read as "no size" it was indistinguishable from a gitlink, so every
      -- event file in such a clone reported as a submodule in canon: a complaint
      -- about the owner, where the truth is that this clone should fetch.
      withCanon [ ("version", "(hub-meta 1)\n")
                , ("threads/t/0001-x", "abcde") ] $ \dir -> do
        blob <- git dir ["rev-parse", "refs/hbs2/meta:threads/t/0001-x"]
        void $ readProcess (proc "rm"
                 ["-f", dir </> ".git" </> "objects"
                            </> take 2 blob </> drop 2 blob])
        readIn dir `shouldReturn`
          Right [ ("threads/t/0001-x", Missing), ("version", Sized 13) ]

    it "fetches a blob by object id under a path git cannot pass as an argument" $ do
      -- The reason blobs are fetched by id and not by path: under the C locale
      -- this image runs git in, a path with any byte above 127 does not survive
      -- the trip through a process argument, and a healthy event file under a
      -- non-Latin thread name read as unreadable.
      withCanon [ ("version", "(hub-meta 1)\n")
                , (utf8 "threads/\1090\1077\1084\1072/0001-x", utf8 "\1087\1088\1080\1074\1077\1090") ] $ \dir -> do
        let cs = gitCanonIn (Just dir)
        Right commit <- csCommit cs
        Right es <- csEntries cs commit
        let oids = [ oid | e <- es, Blob oid _ <- [teeKind e]
                         , teePath e /= versionPath ]
        case oids of
          [oid] -> csBlob cs oid `shouldReturn` BlobText "\1087\1088\1080\1074\1077\1090"
          _ -> expectationFailure ("expected one event blob, got " <> show oids)

        -- An object that is not there is BlobRefused "no such object": git ran and said no. Not an
        -- exception, not "", and not the answer for git having failed to run.
        csBlob cs (Text.pack (replicate 40 'a')) >>= \case
          BlobRefused m -> Text.unpack m `shouldContain` "bad file"
          other -> expectationFailure ("expected BlobRefused, got " <> show other)

    it "says it could not read a listing record rather than calling it a submodule" $ do
      -- Not reachable through git today, and that is the point: this is the
      -- alarm for the day git's format shifts. A record with one word too many
      -- used to fall through to "not a blob", so a shift would have reported a
      -- tree full of submodules; and a record with no TAB at all was dropped,
      -- which reported clean empty canon.
      kindOf . teeKind <$> parseListing "100644 blob abc 5 extra\tthreads/t/0001-x"
        `shouldBe` [Bad]
      kindOf . teeKind <$> parseListing "no tabs here"
        `shouldBe` [Bad]
      teePath <$> parseListing "no tabs here" `shouldBe` ["no tabs here"]

    it "reads a listing whose stderr is larger than a pipe buffer" $ do
      -- THE DEADLOCK. Reading stdout to the end and stderr afterwards stops
      -- forever the moment the child fills the 64 KiB stderr pipe: it blocks on
      -- write, the reader blocks on read, and no bound fires because what is
      -- blocked is the stdout read.
      --
      -- The real trigger is not exotic: ls-tree -l must size every entry, so in a
      -- blobless or partial clone it drives a lazy fetch per missing blob and that
      -- progress goes to stderr. Measured at 39 KB of stdout against 1.1 MB of
      -- stderr. A promisor remote is more machinery than this test needs, so the
      -- shape is reproduced with a shim.
      --
      -- Both streams are past a pipe buffer on their own: 4000 lines a side is
      -- about 160 KB of stderr, and the listing is 8000 records, about 376 KB. A
      -- listing that fits in the stdout buffer is the case a sequential reader
      -- survives, so sizing it below one would test nothing.
      let entry = "100644 blob deadbeef      5\tthreads/t/0001-x\0"
      within 60 "the two-pipe read" $
        shimmed 8000 (mconcat (replicate 8000 entry)) $ \readIt ->
          (fmap (length . fmap teePath) <$> readIt) `shouldReturn` Right 8000

    it "refuses a listing past the byte bound while it is still arriving" $ do
      -- The bound has to hold before the bytes are all in memory, so it cannot be
      -- a check on a buffer. With the real 102 MB bound nothing could afford to
      -- exercise this, which is why the bounds are a parameter.
      let entry = "100644 blob deadbeef      5\tthreads/t/0001-x\0"
      -- Big enough that the shim blocks on a full stdout pipe rather than
        -- finishing in one write, which is what makes "stopped early" observable
        -- at all.
      shimmedWith (gitBounds { gbListingBytes = 200 }) 0 0
                  (mconcat (replicate 8000 entry))
        $ \readIt written' -> do
            readIt `shouldReturn` Left (CanonListingTooBig 200)
            -- The reader stopped, and the writer with it. Without this the test
            -- passes on a reader that reads all 376 KB and then checks a length,
            -- which is the implementation this replaced.
            (n, total) <- written'
            n `shouldSatisfy` (< total)

    it "refuses a listing with more records than the file bound" $ do
      -- What this pins is the refusal and its count. What it does NOT pin is that
      -- the count happens on the raw bytes before the entries are built, and
      -- nothing can: the parser never drops a record, so NULs and parsed entries
      -- are always the same number, and the difference is in what is allocated
      -- along the way, not in the answer. The reason it is done on the bytes is
      -- that the byte bound admits 1.6M minimal records, eight times this bound.
      let entry = "100644 blob deadbeef      5\tthreads/t/0001-x\0"
      shimmedWith (gitBounds { gbListingFiles = 3 }) 0 0 (mconcat (replicate 10 entry))
        $ \readIt _ -> readIt `shouldReturn` Left (CanonTooMany 10)

    it "keeps a tool complaint whole, in a block, and says who could not run" $ do
      -- show-ref exiting 128 is the "ref names an object that is gone" branch, and
      -- nothing reached it: git only produces it on a repository whose ref points
      -- into a pack that is not there.
      --
      -- Two things are asserted about the message. It is kept WHOLE, because git
      -- puts the remedy for a dubious-ownership refusal on its last line, and
      -- one line of it is the half without the fix. And it renders as an indented
      -- block, not as a field value; that half is asserted in VerifySpec, which
      -- is where the renderer lives.
      shimmedWith gitBounds 0 128 "" $ \readIt _ ->
        readIt >>= \case
          Left (RefUnresolved m) -> do
            Text.unpack m `shouldContain` "bad ref refs/hbs2/meta"
            -- The last line, which is where git puts what to run.
            Text.unpack m `shouldContain` "git fsck"
            -- And no trailing newline of its own: it is a message, not a file.
            Text.unpack m `shouldNotContain` "\n\n"
            last (Text.unpack m) `shouldNotBe` '\n'
          other -> expectationFailure
                     ("expected RefUnresolved, got " <> show (fmap (const ()) other))

    it "calls git vanishing mid-audit local, not a verdict on the tree" $ do
      -- A Left from a call after the first is git NOT HAVING RUN, and git ran a
      -- moment ago, so it is this machine and not that repository. Reported as
      -- NoRepository it advised somebody to fix safe.directory in a repository
      -- git had successfully read one call earlier.
      --
      -- The shim deletes itself after answering the first question, which is the
      -- cheapest way to make the second call fail to spawn. A process limit
      -- reached partway through does the same thing for real.
      vanishing $ \readIt ->
        readIt >>= \case
          Left (ReaderFailed _) -> pure ()
          other -> expectationFailure
                     ("expected ReaderFailed, got " <> show (fmap (const ()) other))

    it "gives up on a listing that goes silent, and kills what will not stop" $ do
      -- The listing had a byte bound and no time bound: a git that writes and then
      -- says nothing (a FIFO in place of a tree object, an NFS mount that stopped
      -- answering) waited for ever with no output and no diagnostic, and took the
      -- hook with it. The bound is on SILENCE, not on total time, because a huge
      -- tree may legitimately take as long as it takes.
      --
      -- The shim ignores SIGTERM, so this also covers the teardown: withProcessTerm
      -- would have waited for it for ever, which is the bound not being a bound.
      -- One second here; sixty in production.
      within 30 "the idle bound and the teardown" $
        stalling "ls-tree" (gitBounds { gbCallSeconds = 1, gbTeardownSeconds = 1 }) $ \readIt ->
          readIt >>= \case
            Left (TreeUnreadable (ReaderSays m)) -> do
              -- Not ReaderFailed: git ran, is running, and is simply quiet. That
              -- code means "could not run git" and is the one documented as worth
              -- retrying, which here buys another minute and another stuck child.
              Text.unpack m `shouldContain` "did not finish"
              -- The count is in it, because the bound is per chunk and the stream
              -- may have delivered plenty before it stopped.
              Text.unpack m `shouldContain` "77 bytes"
            other -> expectationFailure
                       ("expected TreeUnreadable, got " <> show (fmap (const ()) other))

    it "gives up on a SMALL call that hangs, and can still be interrupted" $ do
      -- The other reader, and the one the suite never ran: the four small calls go
      -- through a different function from the listing, and its readers were
      -- started with a bare async and never cancelled. When the bound fired they
      -- were still inside hGetSome holding their Handles' MVars, and the first
      -- thing the teardown does is hClose those Handles -- the same MVar, taken
      -- inside a bracket release, which unliftio runs under uninterruptibleMask_.
      -- So it blocked before SIGTERM, before SIGKILL, and did not answer Ctrl-C.
      -- Measured at 19.7s against 68 microseconds with the reader cancelled.
      --
      -- rev-parse is the first call this reader makes, so this covers the path all
      -- four take, cat-file included, which an audit runs once per event file.
      within 30 "a small call that hangs" $
        stalling "rev-parse" (gitBounds { gbCallSeconds = 1, gbTeardownSeconds = 1 })
          $ \readIt -> readIt >>= \case
              -- git ran and said nothing, four calls' worth of nothing: what
              -- matters here is that an answer arrives at all.
              Left _  -> pure ()
              Right _ -> expectationFailure "expected a refusal from a hung call"

    it "gives up on a listing that dribbles for ever" $ do
      -- The idle bound cannot see this one: the writer is never silent for a whole
      -- second, it just never finishes. That is what an exponential tree walk
      -- looks like from here -- 64 entries at 12 levels all pointing at one
      -- subtree is 64^12 traversals of 116 KB of objects -- and a variant that
      -- emits a record between them resets the idle counter for ever.
      within 30 "the total deadline" $
        dribbling (gitBounds { gbCallSeconds = 5, gbListingSeconds = 1
                             , gbTeardownSeconds = 1 })
          $ \readIt -> readIt >>= \case
              Left (TreeUnreadable (ReaderSays m)) ->
                Text.unpack m `shouldContain` "did not finish"
              other -> expectationFailure
                         ("expected a refusal, got " <> show (fmap (const ()) other))

    it "refuses a revision that is not an object id without running git" $ do
      -- The reader hands the revision to git as an argument, so it checks it
      -- first: today it comes from rev-parse and is hex, and the first verb that
      -- takes one from a user makes it an option-injection point. A trailing --
      -- does not help, measured: git ls-tree --help -- exits 129, because
      -- parse_options eats an option-shaped revision long before the --.
      --
      -- And refused WITHOUT running git: an earlier version ran ls-tree against a
      -- placeholder path, which came back as "cannot list the tree", code 9, with
      -- advice to fetch a pruned object and the name of an internal path the
      -- caller never typed.
      withCanon [("version", "(hub-meta 1)\n")] $ \dir -> do
        let cs = gitCanonIn (Just dir)
        csEntries cs "--upload-pack=evil" >>= \case
          Left (RefUnresolved m) -> Text.unpack m `shouldContain` "not an object id"
          other -> expectationFailure
                     ("expected RefUnresolved, got " <> show (fmap (const ()) other))

    it "refuses a listing with no record terminator in it" $ do
      -- Not one entry with a hundred-megabyte path: the count is then zero, so the
      -- file bound says nothing, and the parser makes a single TreeEntry whose path
      -- is the whole output, printed through pathText at up to six bytes a byte.
      -- This is the format shift the parser's haddock calls its reason to exist,
      -- arriving where the parser never sees it.
      shimmedWith gitBounds 0 0 "100644 blob deadbeef 5\tthreads/t/0001-x"
        $ \readIt _ -> readIt >>= \case
            Left (TreeUnreadable (ReaderSays m)) ->
              Text.unpack m `shouldContain` "no record terminator"
            other -> expectationFailure
                       ("expected TreeUnreadable, got " <> show (fmap (const ()) other))

    it "will not hand a listing's object id to a process argument unchecked" $ do
      -- The whole argument for fetching blobs by id rather than by path is that an
      -- id is hex and cannot carry anything an exec cares about. Nothing enforced
      -- it. An id beginning with a dash is an option to cat-file; a non-ASCII one
      -- throws at exec under the C locale and ended the audit over one entry.
      --
      -- Refused in the PARSER, as a record this reader cannot use, which is what
      -- FileListingUnparsed is for: refusing it at the fetch reported "the source
      -- would not give it" about a source nobody asked.
      kindOf . teeKind <$> parseListing "100644 blob --upload-pack=evil 5\tthreads/t/x"
        `shouldBe` [Bad]
      kindOf . teeKind <$> parseListing "100644 blob nothexatall 5\tthreads/t/x"
        `shouldBe` [Bad]
      -- And a negative size is not a size: readMay takes the minus happily, and a
      -- negative number sails under every "larger than" bound there is.
      kindOf . teeKind <$> parseListing "100644 blob deadbeef -5\tthreads/t/x"
        `shouldBe` [Bad]

    it "tells an absent ref from a directory that is not a repository" $ do
      -- rev-parse exits 1 for the first and 128 for the second, and collapsing
      -- them told somebody to fetch canon into a directory where fetching is not
      -- the problem.
      withSystemTempDirectory "hub-git" $ \dir -> do
        void $ git dir ["init", "-q", "."]
        readIn dir >>= \case
          Left (NoCanonRef w) -> B8.unpack w `shouldContain` dir
          other -> expectationFailure
                     ("expected NoCanonRef, got " <> show (fmap (const ()) other))

      withSystemTempDirectory "hub-nogit" $ \dir ->
        readIn dir >>= \case
          Left (NoRepository msg) ->
            Text.unpack msg `shouldContain` "not a git repository"
          other -> expectationFailure
                     ("expected NoRepository, got " <> show (fmap (const ()) other))

    it "tells a ref that resolves to no commit from one that is not there" $ do
      -- show-ref says yes and rev-parse ^{commit} says no. Answering NoCanonRef
      -- here told somebody to fetch canon into a repository that has the ref
      -- already, and threw away git's own account of what is wrong with it.
      withCanon [("version", "(hub-meta 1)\n")] $ \dir -> do
        blob <- git dir ["rev-parse", "refs/hbs2/meta:version"]
        -- update-ref refuses a blob, so the ref file is written directly. A ref
        -- naming a tag or a tree gets here the same way in the wild.
        writeFile (dir </> ".git" </> "refs" </> "hbs2" </> "meta") (blob <> "\n")
        readIn dir >>= \case
          Left (RefUnresolved msg) -> Text.unpack msg `shouldNotBe` ""
          other -> expectationFailure
                     ("expected RefUnresolved, got " <> show (fmap (const ()) other))

    it "reads a broken loose ref as an absent one, on purpose" $ do
      -- git offers no way to tell them apart: show-ref --quiet exits 1 for both
      -- and both stderrs say "not a valid ref". Pinned because the alternative
      -- looks available and is not, and because a fetch is the remedy either way.
      -- If a future git separates them, this test is what will notice.
      withCanon [("version", "(hub-meta 1)\n")] $ \dir -> do
        writeFile (dir </> ".git" </> "refs" </> "hbs2" </> "meta") "not-a-sha\n"
        readIn dir >>= \case
          Left (NoCanonRef w) -> B8.unpack w `shouldContain` dir
          other -> expectationFailure
                     ("expected NoCanonRef, got " <> show (fmap (const ()) other))

    it "refuses a tree whose objects are gone, rather than calling it empty" $ do
      -- A partial clone, a pruned object, a killed ls-tree. Empty canon is the
      -- one answer this must not give: it is what a tracker with nothing in it
      -- looks like.
      withCanon [ ("version", "(hub-meta 1)\n")
                , ("threads/t/0001-x", "abcde") ] $ \dir -> do
        tree <- git dir ["rev-parse", "refs/hbs2/meta^{tree}"]
        let obj = dir </> ".git" </> "objects" </> take 2 tree </> drop 2 tree
        void $ readProcess (proc "rm" ["-f", obj])
        readIn dir >>= \case
          Left (TreeUnreadable _) -> pure ()
          other -> expectationFailure
                     ("expected TreeUnreadable, got " <> show (fmap (const ()) other))
