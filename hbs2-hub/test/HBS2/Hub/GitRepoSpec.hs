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

import Control.Exception (bracket)
import Control.Monad (void,forM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List (sort)
import Data.Maybe (fromMaybe,isJust)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing,findExecutable)
import System.Environment qualified as Env
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory,getCanonicalTemporaryDirectory)
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

-- Git with bytes on stdin, and it worked.
gitStdin :: FilePath -> LBS8.ByteString -> [String] -> IO String
gitStdin dir input args = do
  p <- gitProc dir args
  (code, out, errOut) <- readProcess (setStdin (byteStringInput input) p)
  case code of
    ExitSuccess -> pure (trim out)
    _ -> fail ("git " <> unwords args <> ": " <> LBS8.unpack errOut)

-- A kind, without the object id, which varies from run to run.
data Kind = Sized Int | Missing | NotBlob | Bad
  deriving stock (Eq,Ord,Show)

kindOf :: EntryKind -> Kind
kindOf = \case
  Blob _ n      -> Sized n
  BlobMissing _ -> Missing
  NotABlob      -> NotBlob
  Unparsed      -> Bad

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

spec :: Spec
spec = do

  -- The reads below go through the production 'gitCanonIn', so what it inherits
  -- from this process it inherits for real. The GIT_DIR family is not in here:
  -- 'gitCanonIn' strips those itself when a directory is named, which is a
  -- correctness property of the production code and is asserted below rather than
  -- arranged away here.
  --
  -- What is left is what production cannot decide for a caller. The config files,
  -- because a global commit.gpgsign or core.hooksPath belongs to the developer;
  -- LC_ALL, because a test below matches on a message git writes; and the ceiling,
  -- because withSystemTempDirectory hands out a path under TMPDIR and TMPDIR
  -- inside somebody's repository turns "this is not a git repository" into "it is".
  _ <- runIO $ do
    Env.setEnv "GIT_CONFIG_GLOBAL" "/dev/null"
    Env.setEnv "GIT_CONFIG_SYSTEM" "/dev/null"
    Env.setEnv "LC_ALL" "C"
    getCanonicalTemporaryDirectory >>= Env.setEnv "GIT_CEILING_DIRECTORIES"

  -- Skipped rather than failed where git is not installed: this is the one spec
  -- in the suite that needs it, and a red suite on a machine without git says
  -- something false about the code.
  hasGit <- runIO (isJust <$> findExecutable "git")

  before_ (unless hasGit (pendingWith "git is not on PATH")) $
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
          [oid] -> csBlob cs oid `shouldReturn` Just "\1087\1088\1080\1074\1077\1090"
          _ -> expectationFailure ("expected one event blob, got " <> show oids)

        -- An object that is not there is Nothing, not an exception and not "".
        csBlob cs (Text.pack (replicate 40 'a')) `shouldReturn` Nothing

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

    it "tells an absent ref from a directory that is not a repository" $ do
      -- rev-parse exits 1 for the first and 128 for the second, and collapsing
      -- them told somebody to fetch canon into a directory where fetching is not
      -- the problem.
      withSystemTempDirectory "hub-git" $ \dir -> do
        void $ git dir ["init", "-q", "."]
        readIn dir `shouldReturn` Left NoCanonRef

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
        readIn dir `shouldReturn` Left NoCanonRef

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
