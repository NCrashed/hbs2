-- | A 'CanonSource' backed by a git repository.
--
-- The whole of what talks to git, and all of it is five commands. The reading of
-- what they return is in "HBS2.Hub.Repo", which is pure.
--
-- Git is run as a process rather than reached through hbs2-git3: the ref is an
-- ordinary git ref that PEP-19 says is fetched and pushed by the mechanism code
-- branches already use, and @gitReadTree@ there dedups entries by object hash,
-- which would collapse two paths whose blobs happen to be identical.
--
-- A path never reaches a process argument. Blobs are fetched by object id, which
-- the listing supplies and which is hex, so nothing here depends on the encoding
-- GHC uses for arguments. Under the C locale that encoding is ASCII, and a path
-- with any byte above 127 failed to round-trip through it: a healthy event file
-- under a non-Latin thread name was reported unreadable, in exactly the image
-- this stage added git to.
module HBS2.Hub.Repo.Git
  ( gitCanon
  , gitCanonIn
  , gitCanonWith
  , GitBounds(..)
  , gitBounds
  , parseListing
  ) where

import HBS2.Hub.Repo

import HBS2.CLI.Prelude hiding (filter)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Maybe (isJust)
import Control.Concurrent qualified as Conc
import System.Environment (getEnvironment)
import System.Posix.Signals (signalProcess,sigKILL)
import System.Process qualified as P
import System.Process (terminateProcess)
import System.Process.Typed

-- | What this reader will not go past.
--
-- A parameter, because the real numbers make the refusals cost a hundred
-- megabytes and two hundred thousand files to reach, and a refusal nothing can
-- afford to exercise is a refusal nothing has exercised.
data GitBounds = GitBounds
  { gbListingBytes :: Int   -- ^ 'maxListingBytes'
  , gbListingFiles :: Int   -- ^ 'maxCanonFiles', counted before anything is built
  , gbToolMessage  :: Int   -- ^ how much of a tool's complaint to keep
    -- | How much of a complaint to keep PER FILE. Smaller than the above by two
    -- orders, because it is kept until the report is printed and there can be
    -- 'maxCanonFiles' of them: 64 KiB apiece would be twelve gigabytes.
  , gbBlobMessage  :: Int
    -- | How long a git call may take. For the four small ones it is the whole
    -- call; for the listing, which may legitimately run as long as an enormous
    -- tree takes, it is how long the stream may be SILENT. Two meanings, and one
    -- earlier haddock claimed the listing had no bound at all.
  , gbCallSeconds  :: Int
    -- | How long the whole listing may take, however busy it is. The idle bound
    -- alone is not enough: @ls-tree -r@ walks the tree as a TREE, so a commit
    -- whose subtrees all point at the same subtree costs 64^12 traversals for
    -- 116 KB of objects, and a variant that emits a record now and then resets
    -- the idle counter for ever. Measured: twelve levels of 64 entries produced
    -- zero bytes at 99% of a core.
  , gbListingSeconds :: Int
    -- | How long each step of tearing a process down may take: closing the pipes,
    -- then SIGTERM, then SIGKILL. Short, because by the time it is used something
    -- has already gone wrong and the caller is owed an answer.
  , gbTeardownSeconds :: Int
  }

gitBounds :: GitBounds
gitBounds = GitBounds
  { gbListingBytes = maxListingBytes
  , gbListingFiles = maxCanonFiles
  , gbToolMessage  = 64 * 1024
  , gbBlobMessage  = 512
  , gbCallSeconds  = 60
  , gbListingSeconds = 600
  , gbTeardownSeconds = 2
  }

-- | Canon from the repository the process is standing in.
gitCanon :: MonadUnliftIO m => CanonSource m
gitCanon = gitCanonIn Nothing

-- | Canon from a named repository.
--
-- A parameter rather than a chdir, because chdir is process-global and this is a
-- library: a caller reading two repositories, or a test reading one without
-- moving the suite it runs in, cannot use a global. It is also what makes the
-- git-facing half testable.
gitCanonIn :: MonadUnliftIO m => Maybe FilePath -> CanonSource m
gitCanonIn = gitCanonWith gitBounds

-- | As 'gitCanonIn', with the bounds named. For the test suite.
gitCanonWith :: forall m . MonadUnliftIO m => GitBounds -> Maybe FilePath -> CanonSource m
gitCanonWith bounds cwd = CanonSource
  { csCommit = do
      -- Three questions, one call each, because each answer calls for something
      -- different and any two of them collapsed together lose a diagnostic. Is
      -- this a repository at all; does the ref exist; does it resolve to a commit.
      --
      -- rev-parse --verify exits 1 for a missing ref AND for a ref whose object is
      -- gone or is not a commit, so a single call could not tell "fetch canon"
      -- from "the object it points at is not here" and threw the latter's message
      -- away. show-ref alone could not either: a ref whose object is missing fails
      -- it with a code that is not 1, which then read as "not a repository".
      --
      -- Measured against git 2.55, for refs/hbs2/meta in five states:
      --
      --   state                 show-ref   rev-parse ^{commit}   with --quiet
      --   absent                1          128                   1
      --   loose ref is garbage  1          128                   1
      --   names a blob          0          128                   1
      --   names a gone object   128        128                   1
      --   names a commit        0          0                     0
      --
      -- The middle column is the one this code reads, because it calls rev-parse
      -- WITHOUT --quiet: the message is wanted. The third column is there because
      -- an earlier version of this table quoted it while the code did not use it.
      --
      -- A broken loose ref is therefore NOT distinguishable from an absent one:
      -- both are 1, and both stderrs say "not a valid ref". git knows the
      -- difference (rev-parse without --quiet warns "ignoring broken ref") and
      -- exposes it only as prose, which is not something to branch on. So this
      -- answers NoCanonRef for both, and the advice for it names both causes.
      --
      -- A Left is git NOT HAVING RUN, which on the first call cannot be told from
      -- a directory that is not a repository, and after it can: git ran a moment
      -- ago. Every later Left is therefore a local failure and says so. It read as
      -- NoRepository, which under a process limit advised somebody to fix
      -- safe.directory in a repository git had successfully read one call earlier.
      -- --absolute-git-dir, because the answer is put in front of the operator
      -- when the ref is missing, and a relative ".git" printed by a hook whose
      -- GIT_DIR points elsewhere is the sentence that sent them in circles.
      isRepo <- raw ["rev-parse", "--absolute-git-dir"]
      case isRepo of
        Left e -> pure (Left (NoRepository (msg e)))
        Right (ExitFailure _, e) -> pure (Left (NoRepository (msg (decodeS e))))
        Right (ExitSuccess, gitDirRaw) -> do
          here <- git ["show-ref", "--verify", "--quiet", Text.unpack metaRef]
          case here of
            Left e -> pure (Left (ReaderFailed (msg e)))
            -- The BYTES, and only the trailing newline taken off: Text.strip would
            -- eat a real trailing space in a directory name, and decoding to Text
            -- loses a byte that is not UTF-8 before pathText can escape it.
            Right (ExitFailure 1, _) ->
              pure (Left (NoCanonRef (B8.dropWhileEnd (`elem` ("\r\n" :: String))
                                        gitDirRaw)))
            Right (ExitFailure _, e) -> pure (Left (RefUnresolved (msg e)))
            Right (ExitSuccess, _) ->
              git ["rev-parse", "--verify", Text.unpack metaRef <> "^{commit}"] <&> \case
                Left e -> Left (ReaderFailed (msg e))
                Right (ExitSuccess, out) -> Right (Text.strip out)
                Right (_, e) -> Left (RefUnresolved (msg e))

  , csEntries = \commit ->
      -- --full-tree, and this is not a nicety: ls-tree resolves paths relative to
      -- the current directory, so run from a subdirectory an audit listed nothing
      -- and reported clean empty canon. -l for the size and the object id, so a
      -- bound can refuse a blob unfetched and a blob can be fetched without
      -- naming a path. -z because a path may hold any byte but NUL.
      --
      -- "Unfetched" is true only because GIT_NO_LAZY_FETCH is set for every call
      -- here: -l has to know a size, and without that flag a blobless clone
      -- FETCHES each blob to answer. See the note on the environment below.
      --
      -- Read to a bound as it arrives, which is the one call here that is not
      -- 'raw'. Every bound downstream is computed FROM this listing, so none of
      -- them can refuse it, and the paths parsed out of it are slices of it, so
      -- holding one holds the whole: a tree of ten million paths was a gigabyte
      -- resident before the file-count bound got a chance to say no. Checking the
      -- length of a buffer already read would bound what this reader HOLDS and not
      -- what it peaks at, which is the thing that was wrong.
      -- The commit is checked BEFORE git is run, and the check is what protects it:
      -- measured,  ls-tree ... --help --@ exits 129, because parse_options eats
      -- an option-shaped revision long before it reaches the 9118-@. The trailing
      -- 9118-@ separates a revision from paths and protects nothing here; a comment
      -- that credited it would invite the next person to keep it and delete the
      -- guard.
      --
      -- Refused WITHOUT running git, like csBlob: an earlier version ran
      -- -tree -- /dev/null/not-a-commit@ instead, which exits 128 with "Not a
      -- valid object name" and arrives as TreeUnreadable, code 9, advising a fetch
      -- for a pruned object, and naming an internal placeholder the caller never
      -- typed. The honest answer is the ref does not resolve, which is code 5.
      if not (isObjectId commit)
        then pure (Left (RefUnresolved
                          ( "not an object id: "
                              <> Text.take 100 commit )))
        else
      listing (["ls-tree", "-r", "-z", "-l", "--full-tree"
                      , Text.unpack commit, "--" ])
        <&> \case
          -- The bound first: past it the process is torn down, so its exit code
          -- says how it was killed and not anything about the tree. It carries
          -- the BOUND: reading stops mid-chunk, so the byte count at that moment
          -- is neither the bound nor the size of the tree, and printing it said
          -- "over 102404096 bytes" where the bound is 102400000.
          Right (_, Right Nothing) -> Left (CanonListingTooBig (gbListingBytes bounds))
          -- A stall. NOT ReaderFailed: that one says git could not be run and is
          -- documented as the one worth retrying, and here git ran, is running,
          -- and has simply stopped saying anything -- a retry buys another minute
          -- and another stuck child. The message says what was seen, since the
          -- bound is per chunk and the stream may have delivered plenty first.
          -- Either bound: silence for gbCallSeconds, or gbListingSeconds in total
          -- however busy it was. The message does not say which, because from
          -- here they are one thing: git was asked for a listing and did not
          -- produce one in the time this reader will wait.
          Right (_, Left seen) ->
            Left (TreeUnreadable (ReaderSays ( "git ls-tree did not finish: it sent "
                                     <> Text.pack (show seen)
                                     <> " bytes, then nothing for "
                                     <> Text.pack (show (gbCallSeconds bounds))
                                     <> "s or ran past "
                                     <> Text.pack (show (gbListingSeconds bounds))
                                     <> "s in total, and was given up on" )))
          Right (ExitSuccess, Right (Just out))
            -- Counted on the RAW BYTES, before one TreeEntry exists. Records are
            -- NUL-terminated, so this is a single memchr scan. Counting the parsed
            -- list left the byte bound as the only thing between a listing of
            -- minimal 63-byte records and 1.6M entries built, sorted and held:
            -- eight times the file bound, refused after the allocation the bound
            -- exists to prevent.
            | recs out > gbListingFiles bounds -> Left (CanonTooMany (recs out))
            -- Output with no terminator in it at all is not a listing of one
            -- entry, it is the format having shifted: the count is then zero, so
            -- the bound above says nothing, and parseListing makes ONE entry whose
            -- path is the whole hundred megabytes, printed through pathText at up
            -- to six bytes a byte. This is the shift that haddock calls the case
            -- the parser exists for, arriving where the parser never sees it.
            | not (BS.null out) && recs out == 0 ->
                Left (TreeUnreadable (ReaderSays
                        "the tree listing has no record terminator in it"))
            | otherwise -> Right (parseListing out)
          Right (_, Right (Just e)) -> Left (TreeUnreadable (ToolSaid (msg (decodeS e))))
          -- Again: git not having run at all, four calls in, is local.
          Left e -> Left (ReaderFailed (msg e))

  , csBlob = \oid -> if not (isObjectId oid)
      -- Belt and braces: the listing parser already turns a non-hex id into
      -- Unparsed, so nothing should reach here. If something does, git is not
      -- called (an id beginning with a dash is an option to cat-file, and a
      -- non-ASCII one throws at exec under the C locale), and the answer says
      -- what is true: this reader could not ask.
      then pure (BlobUnavailable "not an object id")
      else
      raw ["cat-file", "blob", Text.unpack oid] <&> \case
        Right (ExitSuccess, out) -> BlobText (decodeS out)
        -- git ran and would not give it, and its words are kept: a pruned object,
        -- a corrupt pack and EACCES on an object file are all exit 128 here, and
        -- calling them all "gone" put "fetch, or unshallow" under a permission
        -- error. Distinct from git not having run, below, which says nothing about
        -- canon at all: read as one answer, a fork that failed on EAGAIN became an
        -- unreadable event file and exit 2, the code for an audit that ran.
        Right (_, e) -> BlobRefused (Text.take (gbBlobMessage bounds) (msg (decodeS e)))
        Left e -> BlobUnavailable (msg e)
  }
  where
    -- Where git is to run, and what it is allowed to do while it runs.
    --
    -- GIT_NO_LAZY_FETCH is the important one, and it is why this is on EVERY call
    -- rather than only when a directory is named. @ls-tree -l@ has to know the
    -- size of every entry, so in a blobless or partial clone it drives a lazy
    -- fetch per missing blob: measured on git 2.55, listing a three-file tree in
    -- a @--filter=blob:none@ clone ran a git fetch per blob and left three blobs
    -- on disk that were not there before. The flag whose entire purpose is to let
    -- a bound refuse a blob WITHOUT fetching it was fetching everything.
    --
    -- So every bound downstream was a claim about a fetch that had already
    -- happened, and an audit that PEP-22 calls read-only and peerless was opening
    -- connections named by the audited repository's own config: remote urls,
    -- core.sshCommand, credential.helper. With the flag, the same listing prints
    -- BAD in the size column and fetches nothing, and BAD is already the case
    -- this reader reports as "the object is not in this clone; fetch".
    --
    -- GIT_TERMINAL_PROMPT for the same reason one step further: git asks for a
    -- password on /dev/tty, which setStdin closed does not cover, so a hook could
    -- stop dead waiting for a human who is not there. Nothing here should be
    -- reaching anything that authenticates in the first place.
    --
    -- setWorkingDir does NOT beat GIT_DIR: with one set, git reads that
    -- repository from any directory, so a caller that named a repository got
    -- another one, silently and with a plausible answer. A hook is exactly where
    -- both happen at once, since git sets GIT_DIR for every hook it runs, and a
    -- hook auditing a repository other than the one it was invoked for is the
    -- kind of wrong that reads as correct.
    --
    -- The repository stripping is only for a NAMED directory. With none, the
    -- environment is the caller's whole answer to "which repository", and a
    -- hook's GIT_DIR is then the right one to obey. Everything else here applies
    -- to both, because the command line is what names a repository and none of
    -- the rest is about which one.
    inDir p = do
      env0 <- liftIO getEnvironment
      let kept = List.filter ((`notElem` (fmap fst forced <> named)) . fst) env0
          named = case cwd of Nothing -> [] ; Just _ -> whichRepository
          here = maybe id setWorkingDir cwd
      pure (setEnv (kept <> forced) (here p))

    -- REMOVED FROM THE INHERITED SET and then added, which is the whole of the
    -- difference between this working and not. Appending alone does nothing: with
    -- two bindings of one name in envp, getenv returns the FIRST, so a caller
    -- with GIT_NO_LAZY_FETCH=0 in their environment got a reader that fetched the
    -- tree it was auditing, with the bounds firing after the bytes had landed.
    -- Measured on the same clone and the same binary, changing only the parent's
    -- environment: 0 blobs against 2, and one file refused as 270177 bytes after
    -- it had been downloaded.
    forced =
      [ -- The audit is read-only and offline; see the note above.
        ("GIT_NO_LAZY_FETCH", "1")
        -- refs/replace rewrites what a commit's tree IS, so canon could be read
        -- from a substituted tree while the report printed the honest commit id.
        -- Verified: a planted replacement added an event to a clean audit and the
        -- header line named the real commit throughout. Not reachable through the
        -- documented fetch refspec, but a mirror clone carries refs/replace.
      , ("GIT_NO_REPLACE_OBJECTS", "1")
        -- No prompting, in three places, because git tries them in order and only
        -- the last is a terminal: GIT_ASKPASS, then core.askPass, then
        -- SSH_ASKPASS, then /dev/tty, which `setStdin closed` does not cover. A
        -- hook that stops for a password nobody is there to type is a hung hook.
        --
        -- EMPTY, not a path to a program that fails. git's check is `askpass &&
        -- *askpass`, so an empty value means "no askpass" and skips the rest of
        -- the chain, while a path means "run this": /bin/false was the first
        -- attempt and does not exist on NixOS, so git would have complained about
        -- a missing helper and that complaint would have been printed to the user
        -- as what git said about their repository.
      , ("GIT_ASKPASS", "")
      , ("SSH_ASKPASS", "")
      , ("GIT_TERMINAL_PROMPT", "0")
      ]

    -- Variables that answer "which repository" from outside it. Obeyed when the
    -- caller named no directory, dropped when they did: naming one and being
    -- given another is the failure, and a hook has GIT_DIR set for every command
    -- it runs.
    whichRepository = [ "GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE"
                      , "GIT_OBJECT_DIRECTORY", "GIT_COMMON_DIR"
                      , "GIT_ALTERNATE_OBJECT_DIRECTORIES"
                      ]

    -- GIT_CEILING_DIRECTORIES and GIT_DISCOVERY_ACROSS_FILESYSTEM are LEFT ALONE,
    -- in both cases, and two earlier versions of this got it wrong in opposite
    -- directions.
    --
    -- Dropping them was wrong: the ceiling is how a caller says "do not walk up
    -- past here", which is a thing this reader should obey and not override, and
    -- the discovery flag was described as stopping git from looking when it does
    -- the reverse (it lets git cross a filesystem boundary), so removing it is
    -- how a bind mount inside a working tree becomes "not a git repository".
    --
    -- Setting a ceiling at the named directory's parent was also wrong: discovery
    -- walking up is not a bug, it is how every git command behaves in a
    -- subdirectory, and `hub verify` run three directories deep in a working tree
    -- has to find the repository the way `git status` does. A caller who does not
    -- want the walk sets a ceiling, and it is obeyed.

    -- GIT_CONFIG_* are in NO list: a config that turns off gpg signing or sets
    -- safe.directory is the caller's business, and this only reads.

    -- Lenient, so a blob that is not UTF-8 becomes replacement characters rather
    -- than an exception.
    --
    -- It costs up to three bytes per invalid one, and NEITHER bound accounts for
    -- that: both are sums of the sizes git reported, which are the sizes on disk.
    -- A tree at the byte bound made of invalid UTF-8 is three times that in
    -- memory. The comment here used to claim the tree-wide bound covered it,
    -- which it cannot, being made of the same numbers as the per-file one.
    decodeS = Text.decodeUtf8Lenient

    -- A tool's complaint as a message. Its trailing newline is not part of what
    -- it said, and it survived into a report where every newline is escaped, so
    -- every one of these lines ended in a literal \x0a. Only the end: the interior
    -- newlines are kept, because the LAST line of a dubious-ownership complaint is
    -- the command that fixes it, three lines below the complaint itself.
    msg = Text.stripEnd

    -- Decoded output, for the calls whose result is text.
    git args = raw args <&> fmap (fmap decodeS)

    -- Records in a raw listing: one NUL each.
    recs = BS.count 0

    -- A git process, torn down WITHIN A BOUND on the way out.
    --
    -- Not 'withProcessTerm', whose cleanup is stopProcess: SIGTERM and then an
    -- unbounded waitForProcess. Everything above this line was made to give up in
    -- bounded time and then handed the process to a cleanup that does not, so a
    -- git ignoring SIGTERM (or sitting in uninterruptible sleep on a dead mount)
    -- took the audit with it after every bound had already fired. Measured on a
    -- shim that ignores SIGTERM: the read returned at 2002 ms and the call never
    -- did.
    --
    -- The escalation is the usual one and each step is bounded: close the pipes so
    -- the child sees EOF and EPIPE, TERM, KILL, and then give up on it. Giving up
    -- is not tidy and it is the only honest end: a process in D state does not
    -- answer SIGKILL either, and there is nothing further anybody can do to it.
    withGit :: forall a . ProcessConfig () Handle Handle
            -> (Process () Handle Handle -> m a) -> m a
    withGit cfg = bracket (startProcess cfg) teardown

    teardown :: Process () Handle Handle -> m ()
    teardown p = do
      -- Our ends of the pipes, first: a child blocked writing into a pipe nobody
      -- reads dies of EPIPE without needing a signal at all.
      for_ [getStdout p, getStderr p] $ \h ->
        tryAny (liftIO (hClose h))
      done <- settle
      unless done do
        tryAny (liftIO (terminateProcess (unsafeProcessHandle p)))
        harder <- settle
        unless harder do
          -- getPid on the ProcessHandle (from `process`, there since 1.6.3), not
          -- typed-process's getPid on a Process, which only exists in
          -- typed-process 0.2.12 and later.
          --
          -- The split is CABAL against NIX, not dynamic against static: the freeze
          -- file pins typed-process 0.2.13.0 and nixpkgs gives 0.2.11.1 to both
          -- the dynamic and the static package sets, so `cabal build` and the
          -- whole suite were green while every nix output failed to compile. It is
          -- the only version skew in this package's dependencies, and the next use
          -- of a post-0.2.11 API will land in it again.
          pid <- liftIO (P.getPid (unsafeProcessHandle p))
          for_ pid $ \pid' -> tryAny (liftIO (signalProcess sigKILL pid'))
          void settle

      where
        -- Wait for the process to be gone, or give up. POLLED, and that is the
        -- whole point: this runs as a bracket's RELEASE action, and unliftio runs
        -- release under uninterruptibleMask, where the async exception `timeout`
        -- throws is deferred and does nothing at all. Measured: a one-second
        -- timeout around waitExitCode in here returned after thirty, when the
        -- child happened to finish on its own, so a git ignoring SIGTERM took the
        -- audit with it exactly as it had before the bound was written.
        -- getExitCode does not block, so a poll gets out on its own.
        settle = poll (max 1 (gbTeardownSeconds bounds * 1000000 `div` step))
        step = 20000
        poll :: Int -> m Bool
        poll 0 = isJust <$> getExitCode p
        poll n = getExitCode p >>= \case
                   Just _  -> pure True
                   Nothing -> liftIO (Conc.threadDelay step) >> poll (n - 1)

    -- The listing, read to 'gbListingBytes' and no further: @Nothing@ is "there
    -- was more than that", and the process is dead by the time it is returned.
    --
    -- BOTH PIPES ARE READ AT ONCE, and this is the whole of why this is not five
    -- lines. Reading stdout to the end and stderr afterwards deadlocks the moment
    -- the child fills the 64 KiB stderr pipe: it blocks on write, this blocks on
    -- read, and neither the byte bound nor anything else fires, because what is
    -- blocked is the stdout read.
    --
    -- Not hypothetical, and the trigger is the very case this reader was taught
    -- about last: @ls-tree -l@ must know the size of every entry, so in a blobless
    -- or partial clone it drives a lazy fetch per missing blob, and that fetch's
    -- progress goes to the inherited stderr. Measured on git 2.55: 39 561 bytes of
    -- stdout against 1 129 712 bytes of stderr, which hung forever. The comment
    -- that used to be here reasoned it away with "ls-tree's stderr is a line or
    -- two".
    --
    -- The stderr reader is 'drain', which reads to EOF even past its keep-bound: a
    -- reader that stopped would leave the child blocked on a full pipe, which is
    -- the same deadlock one step over. It is an async, so a stdout read that
    -- returns early (the bound) cancels it rather than waiting for an EOF that a
    -- terminated process will never send.
    listing args = do
      cfg <- inDir (proc "git" args)
      let piped = setStdin closed (setStdout createPipe (setStderr createPipe cfg))
      -- The answer is recorded the moment it is reached, because tearing the
      -- process down can fail AFTER that and the failure would otherwise replace
      -- it. Refusing at the bound means killing a git that is still writing, and
      -- the wait in the teardown then raced the RTS reaping it: "waitForProcess:
      -- does not exist (No child processes)", reported as a tree that will not
      -- list, over a listing this reader had already read enough of to refuse.
      answer <- newIORef Nothing
      r <- tryAny
        ( withGit piped $ \p ->
            withAsync (drain (gbToolMessage bounds) (getStderr p)) $ \errA -> do
              out <- upTo (gbListingBytes bounds) (getStdout p)
              a <- case out of
                     Left seen -> pure (ExitFailure 1, Left seen)
                     Right Nothing -> pure (ExitFailure 1, Right Nothing)
                     Right (Just bs) -> do
                       -- Bounded, like every other wait in this module. A child
                       -- that writes a record, closes stdout, catches SIGTERM and
                       -- sleeps leaves this waiting: measured at over 20s, and it
                       -- was the only unbounded wait left. Through a real git it
                       -- is unlikely; through a shim or a wrapper on PATH it is
                       -- one line of shell.
                       code <- fromMaybe (ExitFailure 1)
                                 <$> timeout (gbCallSeconds bounds * 1000000)
                                             (waitExitCode p)
                       -- The message is only wanted when git failed, and waiting
                       -- for it is bounded even then: stderr EOFs when the last
                       -- holder of the fd closes it, and a grandchild (an ssh
                       -- ControlPersist master, say) inherits it and outlives
                       -- git. On the success path there is nothing to wait for at
                       -- all, and withAsync cancels the drain on the way out.
                       err <- if code == ExitSuccess
                                then pure mempty
                                else fromMaybe mempty <$> timeout 2000000 (wait errA)
                       pure (code, Right (Just (if code == ExitSuccess then bs else err)))
              writeIORef answer (Just a)
              pure a
        )
      case r of
        Right a -> pure (Right a)
        Left e  -> readIORef answer <&> maybe (Left (Text.pack (show e))) Right

    -- Read a handle to a bound. Stops at the first chunk that crosses it and does
    -- not read the rest, which is the whole point.
    --
    -- @Left n@ is EITHER time bound, with n the bytes seen so far.
    --
    -- The idle one (gbCallSeconds without a byte) catches a reader that has
    -- stopped: reached with a FIFO in place of a tree object, where three calls
    -- answer and the fourth waits for ever with no output and no diagnostic.
    --
    -- The total one (gbListingSeconds) catches a reader that has not stopped and
    -- will not finish. An earlier comment here argued a total limit was the wrong
    -- shape, because a legitimately enormous tree takes as long as it takes; that
    -- is true of the SIZE of a tree and false of the WALK, because @ls-tree -r@
    -- walks it as a tree, so 64 entries at 12 levels all pointing at one subtree
    -- is 64^12 traversals of 116 KB of objects. Dribbling one record between them
    -- resets the idle bound for ever. So there are two bounds, and the deadline is
    -- the one that answers the second case.
    --
    -- Checked BEFORE each read, so the worst case is gbListingSeconds plus one
    -- gbCallSeconds, not gbListingSeconds exactly.
    upTo n h = do
      started <- getMonotonicTime
      go started 0 []
      where
        go started seen chunks = do
          now <- getMonotonicTime
          if now - started > fromIntegral (gbListingSeconds bounds)
            -- The DEADLINE, which the idle bound cannot replace: a listing that
            -- keeps dribbling bytes is never idle and can still be a tree walk
            -- that will not finish this century.
            then pure (Left seen)
            else do
             c <- timeout (gbCallSeconds bounds * 1000000) (liftIO (BS.hGetSome h 65536))
             case c of
              Nothing -> pure (Left seen)
              Just c'
                | BS.null c' -> pure (Right (Just (BS.concat (List.reverse chunks))))
                | otherwise -> do
                    let seen' = seen + BS.length c'
                    if seen' > n then pure (Right Nothing)
                                 else go started seen' (c' : chunks)

    -- Read a handle to the END, keeping only the first n bytes. For a stream whose
    -- content is a message rather than data: the bound is on what is remembered,
    -- never on what is read, because not reading is what deadlocks the writer.
    drain n h = go 0 []
      where
        go seen chunks = do
          c <- liftIO (BS.hGetSome h 65536)
          if BS.null c
            then pure (BS.concat (List.reverse chunks))
            else go (seen + BS.length c)
                   (if seen < n then BS.take (n - seen) c : chunks else chunks)

    -- Raw output. The listing is bytes: decoding it first merged two paths that
    -- differ only in an invalid byte, after which the same blob was read for both.
    --
    -- The whole of stderr, not its first line. On a repository git considers
    -- dubiously owned, the remedy (the safe.directory command to run) is on the
    -- line AFTER the complaint, so keeping one line kept the half without the fix
    -- in it while claiming stderr was what a human needs.
    -- Bounded in time, like the listing read and for the same reason: readProcess
    -- waits for EOF on both pipes, and EOF comes when the LAST holder of the fd
    -- closes it, which is not necessarily git. A grandchild that outlives it hangs
    -- the call for ever. The listing got a bound and these four did not, which
    -- left the hang on the path that runs two hundred thousand times.
    --
    -- Generous, because none of these is allowed to be slow: three ref lookups and
    -- one blob read, all local, all off the network by construction.
    raw args = do
      cfg <- inDir (proc "git" args)
      let piped = setStdin closed (setStdout createPipe (setStderr createPipe cfg))
      -- Both pipes read at once, for the reason 'listing' spells out, and the
      -- process torn down within a bound whatever happens, which is 'withGit'.
      --
      -- An earlier version left the read running on an abandoned thread when the
      -- bound was reached, on the theory that waiting for a child in
      -- uninterruptible sleep is waiting for the disk. The theory is right and the
      -- code was wrong: readProcess is a bracket, and a thread abandoned inside it
      -- never leaves it, so the cleanup never ran at all. No SIGTERM, no closed
      -- handles: two descriptors and one live child per timeout, and at a limit of
      -- 64 the audit died of "no file descriptors" and blamed the machine for its
      -- own leak. A bounded teardown is the answer, not the absence of one.
      -- tryAny, and in THIS prelude that is UnliftIO's: its try and tryAny both
      -- rethrow asynchronous exceptions, so Ctrl-C still ends the audit instead of
      -- arriving as a file that will not read. An earlier comment here drew a
      -- distinction between the two that does not exist (tryAny is try at
      -- SomeException); the name is kept for what it says, not for what it fixes.
      -- withAsync for BOTH readers, not async, and this is the whole difference
      -- between a bounded call and a hang that Ctrl-C cannot end.
      --
      -- A bare async is never cancelled, so when the bound fires the readers are
      -- still sitting in hGetSome holding their Handles' MVars. The first thing
      -- the teardown does is hClose those same Handles, which takes the same MVar
      -- and blocks; and the teardown is a bracket release, which unliftio runs
      -- under uninterruptibleMask_, so it never reaches TERM, never reaches KILL,
      -- and does not answer Ctrl-C. Measured: 19.7 s against 68 microseconds with
      -- the reader cancelled, and the whole construction not returning in 90 s.
      -- withAsync cancels on the way out of its scope, which is INSIDE withGit's,
      -- so both readers are gone before a handle is closed.
      r <- tryAny do
             withGit piped $ \p ->
               withAsync (drain (gbToolMessage bounds * 16) (getStdout p)) $ \out ->
               withAsync (drain (gbToolMessage bounds) (getStderr p)) $ \err ->
                 timeout (gbCallSeconds bounds * 1000000) do
                   code <- waitExitCode p
                   o <- wait out
                   e <- wait err
                   pure (code, if code == ExitSuccess then o else e)
      pure case r of
        Left e -> Left (Text.pack (show e))
        Right Nothing ->
          Left ( "git " <> Text.pack (unwords (take 2 args))
                   <> " did not finish in "
                   <> Text.pack (show (gbCallSeconds bounds))
                   <> "s and was given up on" )
        Right (Just x) -> Right x

-- | Is this hex of a plausible object-id length? sha1 is 40, sha256 is 64, and
-- git accepts an unambiguous prefix, so the low end is where ambiguity starts
-- rather than a spec.
isObjectId :: Text -> Bool
isObjectId t = Text.length t >= 4 && Text.length t <= 64 && Text.all hex t
  where hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
                  || (c >= 'A' && c <= 'F')

-- | Parse the output of @ls-tree -r -z -l@.
--
-- Each record is @\<mode> SP \<type> SP \<oid> SP* \<size> TAB \<path>@, NUL
-- terminated. The size column is @-@ for anything that is not a blob, and @BAD@
-- for a blob whose object this clone does not have.
--
-- Those two are told apart on purpose. Reading both as "no size" made every event
-- file in a blobless or partial clone report as a submodule in canon, which is a
-- complaint about the owner where the truth is a complaint about the clone: the
-- fix is to fetch, and the message said the tree was malformed.
--
-- A record this cannot parse is kept whole as its own path rather than dropped.
-- Dropping was the behaviour, and it is the one failure mode with no alarm: if
-- this format ever shifts, every record becomes unparsable, and a reader that
-- drops them prints a clean empty audit instead of saying it can no longer read a
-- tree.
parseListing :: ByteString -> [TreeEntry]
parseListing = fmap entry . filter (not . B8.null) . B8.split '\0'
  where
    entry rec = case B8.break (== '\t') rec of
      -- No TAB at all: the record is kept as the path, because it is the only
      -- thing there is to show a human, and marked as what it is.
      (_, rest) | B8.null rest -> TreeEntry rec Unparsed
      (meta, rest) -> TreeEntry (B8.drop 1 rest) (kindOf meta)

    kindOf meta = case B8.words meta of
      [_, ty, oid, sz]
        | ty /= "blob" -> NotABlob
        -- An id that is not hex is a listing this reader cannot use, not a blob it
        -- could ask for: refusing it later, at the fetch, reported "the source ran
        -- and would not give it" about a source that was never asked.
        | not (isObjectId (Text.decodeUtf8Lenient oid)) -> Unparsed
        -- A size that is not a NON-NEGATIVE number is the same kind of nonsense.
        -- readMay takes a leading minus quite happily, and a negative size sails
        -- under every  > maxEventBytes@ bound there is.
        | Just n <- readMay (B8.unpack sz), n >= 0 -> Blob (Text.decodeUtf8Lenient oid) n
        -- git's exact spelling for a blob it could not size, and nothing else. A
        -- negative size is not a size and an unknown word is not BAD: both are the
        -- format having shifted, which is Unparsed and is loud, rather than "the
        -- object is missing, go and fetch", which is quiet and would be a guess.
        | sz == "BAD" -> BlobMissing (Text.decodeUtf8Lenient oid)
        | otherwise -> Unparsed
      -- A record with the right shape but the wrong number of words is a format
      -- shift, and is reported as one rather than as a tree full of submodules.
      _ -> Unparsed
