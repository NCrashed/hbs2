{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
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
  ( withGitCanon
  , withGitCanonIn
  , withGitCanonWith
  , GitBounds(..)
  , gitBounds
  , parseListing
  , nowSeconds
  , gitIn
  , gitRun
  , GitTrouble(..)
  ) where

import HBS2.Hub.Repo
import HBS2.Hub.Canon (maxEventBytes)

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
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getEnvironment)
import System.IO qualified as IO
import System.IO.Error (isEOFError)
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
    -- | How long a git call may take. For the three small ones it is the whole
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
    -- | How long the whole of reading canon may take, blobs included. The listing
    -- has its own two bounds and they say nothing about the walk that follows.
    --
    -- Blobs are read through one batch process, so this is a bound on I/O and
    -- parsing rather than on forks: the tree that made this necessary (45000
    -- paths, 172 KB, one git per path, 82 seconds) is seconds now. It is still
    -- here because a walk can be made expensive in other ways, and because this
    -- verb is meant for a pre-receive hook, where a minute is a minute somebody
    -- is waiting.
    --
    -- A tree this refuses is refused with a code and a message that names
    -- compaction, which is what PEP-19 says to do about a canon this large.
  , gbReadSeconds  :: Int
    -- | How many bytes of blob the whole walk may hand back. 'maxCanonBytes' in
    -- production, and a FIELD rather than that constant read in place, for the
    -- reason every other number here is one: at a hundred megabytes nothing in
    -- the suite can afford to reach the branch, so nothing did.
  , gbReadBytes    :: Int
    -- | How large one blob may be before this reader refuses to hold it.
    -- 'maxEventBytes' in production, and a field for the same reason.
    --
    -- NOT the same number as the listing's size column, though today they are
    -- equal: that column is the audited tree's word for it and this is this
    -- reader's own ceiling. A loose object can be self-consistent and lie -- a
    -- header saying @blob 10@ over two megabytes of body hashes to its own name,
    -- so @cat-file -s@ answers 10 -- which is why the read has a ceiling of its
    -- own and does not trust the one it was told.
  , gbBlobBytes    :: Int
    -- | How long the batch process may go silent PART WAY THROUGH A BODY it has
    -- already announced the length of.
    --
    -- Much shorter than 'gbCallSeconds', and a separate number rather than an
    -- oversight. That one covers a git that has been asked a question and has not
    -- answered yet, where a minute is not obviously too long. This covers a git
    -- that has said "blob 20000" and stopped at ten bytes, which is not slowness:
    -- it is an object whose header announces more than its body holds, and the
    -- bytes come from a local object store through a process that has already
    -- committed to sending them. Measured on the general bound, a 346-byte
    -- repository stalled a pre-receive hook for a minute, and for two before the
    -- newline read stopped running after a body read that had already failed.
  , gbBodySeconds  :: Int
    -- | How long ONE blob may take, start to finish, however busy the stream is.
    --
    -- The idle bounds above cannot give this, for the reason the listing needed
    -- both an idle bound and a deadline: a source that says something before each
    -- of them is never idle. 8 KiB every nine seconds passes 'gbBodySeconds' for
    -- ever and passes the walk's budget too, because that one is checked between
    -- blobs and never inside one.
  , gbBlobSeconds  :: Int
    -- | How long, in MILLISECONDS, to wait for git's own words about an object it
    -- answered "missing" for, before saying the plain thing instead.
    --
    -- Milliseconds because the words are already written when the answer arrives:
    -- what is being waited for is the thread that collects them being scheduled,
    -- not git doing anything. Paid at most once per walk -- see 'missing' -- so
    -- the number is chosen to survive a loaded machine rather than to be tight.
  , gbBlobWords    :: Int
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
  , gbReadSeconds  = 180
  , gbReadBytes    = maxCanonBytes
  , gbBlobBytes    = maxEventBytes
  , gbBodySeconds  = 10
  , gbBlobSeconds  = 60
  , gbBlobWords    = 500
  , gbTeardownSeconds = 2
  }

-- | Canon from the repository the process is standing in.
--
-- Bracketed, and that is not a style choice: blobs are read through ONE
-- @cat-file --batch@ for the whole walk, so there is a process to close.
--
-- A fork per blob was what made a small tree expensive. 45000 paths whose
-- entries all point at one shared subtree is five objects and 172 KB on disk,
-- and the listing bounds cannot see it: 3.6 MB against 102 MB, 45001 records
-- against 200000. Measured at 82 seconds, about six minutes at the file bound,
-- in a verb whose whole point is that it runs in a pre-receive hook. With batch
-- reading the same tree is one process and the walk is I/O.
withGitCanon :: MonadUnliftIO m => (CanonSource m -> m a) -> m a
withGitCanon = withGitCanonIn Nothing

-- | Canon from a named repository.
--
-- A parameter rather than a chdir, because chdir is process-global and this is a
-- library: a caller reading two repositories, or a test reading one without
-- moving the suite it runs in, cannot use a global. It is also what makes the
-- git-facing half testable.
withGitCanonIn :: MonadUnliftIO m => Maybe FilePath -> (CanonSource m -> m a) -> m a
withGitCanonIn = withGitCanonWith gitBounds

-- | As 'withGitCanonIn', with the bounds named. For the test suite.
withGitCanonWith :: MonadUnliftIO m
                 => GitBounds -> Maybe FilePath -> (CanonSource m -> m a) -> m a
withGitCanonWith bounds cwd act = do
  cs <- gitCanonWith bounds cwd
  act cs `finally` csClose cs

-- | The source itself. Private, and only 'withGitCanonWith' builds one, because
-- what it holds has to be released.
--
-- Monadic, because a budget over the whole read needs somewhere to count: the
-- bytes accumulate across every blob and the clock starts at the first of them.
-- Without that there is no bound on the WALK at all, only on the listing, and a
-- tree of 45000 paths sharing one subtree (five objects, 164 KB) passed every
-- listing bound and then took 82 seconds of one git per path.
gitCanonWith :: forall m . MonadUnliftIO m
             => GitBounds -> Maybe FilePath -> m (CanonSource m)
gitCanonWith bounds cwd = do
  -- WHEN THE WALK BEGAN, and not when the source was built. Built-time was one
  -- clock for two budgets: gbReadSeconds then ran from before csCommit, so the
  -- listing's own gbListingSeconds (600) could never be reached -- the walk's 180
  -- put a ceiling on it -- and a listing refused at 180s printed the advice for a
  -- walk that had not started.
  clock   <- newIORef Nothing
  spent   <- newIORef (0 :: Int)
  batch   <- newIORef Nothing
  said    <- newIORef mempty
  patient <- newIORef True
  pure (source clock spent batch said patient)
 where
 source clock spent batch said patient = CanonSource
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
      -- --absolute-git-dir, because the answer is put in front of the operator
      -- when the ref is missing, and a relative ".git" printed by a hook whose
      -- GIT_DIR points elsewhere is the sentence that sent them in circles.
      -- Each of the three answers below tells the FOUR outcomes apart, which is
      -- what 'Small' is for. They used to be an Either, in which "git could not
      -- be started" and "git started and never answered" were the same Left: a
      -- FIFO at .git/refs/hbs2/meta leaves rev-parse answering and show-ref
      -- blocked on open, and that came out as code 12 after a minute, whose
      -- advice is that it is local and the one refusal worth retrying. The retry
      -- blocks for another minute. The listing has told these apart since it grew
      -- bounds of its own; these three had not.
      -- ON THE FIRST CALL TOO, and this is the half the last commit got wrong. It
      -- said in the documentation that 4 is "not a git repository" and 12 is "git
      -- not running", and left the first call answering 4 for a git that could
      -- not be started: measured, `git` off PATH exited 4 with advice about
      -- safe.directory. The old reasoning was that the first call cannot tell the
      -- two apart, and it could not while both were one Left. SmallUnstartable is
      -- exec having failed, which is never a fact about the repository.
      raw ["rev-parse", "--absolute-git-dir"] >>= \case
        SmallUnstartable e -> pure (Left (ReaderFailed e))
        SmallStalled e -> pure (Left (ToolStalled e))
        SmallFailed _ e -> pure (Left (NoRepository (msg (decodeS e))))
        SmallOk gitDirRaw -> do
          here <- raw ["show-ref", "--verify", "--quiet", Text.unpack metaRef]
          case here of
            SmallUnstartable e -> pure (Left (ReaderFailed e))
            SmallStalled e -> pure (Left (ToolStalled e))
            -- The BYTES, and only the trailing newline taken off: Text.strip would
            -- eat a real trailing space in a directory name, and decoding to Text
            -- loses a byte that is not UTF-8 before pathText can escape it.
            SmallFailed (ExitFailure 1) _ ->
              pure (Left (NoCanonRef (B8.dropWhileEnd (`elem` ("\r\n" :: String))
                                        gitDirRaw)))
            SmallFailed _ e -> pure (Left (RefUnresolved (ToolSaid (msg (decodeS e)))))
            SmallOk _ ->
              raw ["rev-parse", "--verify", Text.unpack metaRef <> "^{commit}"] <&> \case
                SmallUnstartable e -> Left (ReaderFailed e)
                SmallStalled e -> Left (ToolStalled e)
                SmallOk out -> Right (Text.strip (decodeS out))
                SmallFailed _ e -> Left (RefUnresolved (ToolSaid (msg (decodeS e))))

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
      -- The commit is checked BEFORE git is run, and the check is what protects
      -- it: measured, @git ls-tree -r -z -l --full-tree --help --@ exits 129,
      -- because parse_options eats an option-shaped revision long before it
      -- reaches the trailing @--@. That separator divides a revision from paths
      -- and protects nothing here; a comment that credited it would invite the
      -- next person to keep it and delete the guard.
      --
      -- Refused WITHOUT running git, like csBlob: an earlier version ran
      -- @git ls-tree ... /dev/null/not-a-commit --@ instead, which exits 128 with
      -- "Not a valid object name" and arrives as TreeUnreadable, code 9, advising
      -- a fetch for a pruned object, and naming an internal placeholder the caller
      -- never typed. The honest answer is the ref does not resolve, which is
      -- code 5.
      if not (isObjectId commit)
        then pure (Left (RefUnresolved (ReaderSays
                          ( "not an object id: "
                              <> Text.take 100 commit ))))
        else
      listing (["ls-tree", "-r", "-z", "-l", "--full-tree"
                      , Text.unpack commit, "--" ])
        <&> \case
          -- The bound first: past it the process is torn down, so its exit code
          -- says how it was killed and not anything about the tree. It carries
          -- the BOUND: reading stops mid-chunk, so the byte count at that moment
          -- is neither the bound nor the size of the tree, and printing it said
          -- "over 102404096 bytes" where the bound is 102400000.
          Right ListedTooBig -> Left (CanonListingTooBig (gbListingBytes bounds))
          -- A stall. NOT ReaderFailed: that one says git could not be run and is
          -- documented as the one worth retrying, and here git ran, is running,
          -- and has simply stopped saying anything -- a retry buys another minute
          -- and another stuck child. The message says what was seen, since the
          -- bound is per chunk and the stream may have delivered plenty first.
          -- Either bound: silence for gbCallSeconds, or gbListingSeconds in total
          -- however busy it was. The message does not say which, because from
          -- here they are one thing: git was asked for a listing and did not
          -- produce one in the time this reader will wait.
          -- WHICH bound, and how long it really took. This used to name both
          -- configured numbers and neither measurement, so the line said the same
          -- thing whichever bound had fired and however fast it had fired: an
          -- audit describing its own settings instead of what it saw. Thirty-two
          -- of these in a CI log on a machine nobody can log into say nothing at
          -- all, which is how they were read for a while.
          Right (ListedStalled why seen took) ->
            Left (TreeUnreadable (ReaderSays ( "git ls-tree did not finish: it sent "
                                     <> Text.pack (show seen)
                                     <> " bytes and "
                                     <> stallWord why
                                     <> ", " <> tookText took
                                     <> " into the listing" )))
          -- EOF on stdout and no exit code. NOT a complete listing: end of file
          -- means the write end of the pipe was closed, which a writer that has
          -- finished does and a writer that has been killed, has crashed, or has
          -- closed the descriptor on purpose does too. The count is what a reader
          -- can act on, so it is printed; the listing is not used.
          Right (ListedUnfinished seen) ->
            Left (TreeUnreadable (ReaderSays ( "git ls-tree closed its output after "
                                     <> Text.pack (show seen)
                                     <> " bytes and had not exited "
                                     <> Text.pack (show (gbCallSeconds bounds))
                                     <> "s later, so whether that listing is the"
                                     <> " whole tree is not known" )))
          Right (ListedOk out)
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
          Right (ListedFailed e) -> Left (TreeUnreadable (ToolSaid (msg (decodeS e))))
          -- Again: git not having run at all, four calls in, is local.
          Left e -> Left (ReaderFailed (msg e))

  , csClose = readIORef batch >>= maybe (pure ()) stop

  , csBlob = \oid -> budgeted $ if not (isObjectId oid)
      -- Belt and braces: the listing parser already turns a non-hex id into
      -- Unparsed, so nothing should reach here. If something does, git is not
      -- asked (an id beginning with a dash is an option, and a non-ASCII one
      -- throws at exec under the C locale) and the answer says what is true.
      then pure (BlobUnavailable "not an object id")
      -- THE ONE EXIT PAST `give`, so it stops the process itself. Every refusal
      -- inside askBatch tears the batch down before answering, because the stream
      -- is out of step by then; an exception thrown anywhere in there left a live
      -- git with an unread tail, and the next blob would have read that tail as
      -- its own reply. No trigger was built for it, which is the reason to close
      -- it rather than to argue about reachability.
      else tryAny (askBatch oid) >>= \case
             Right r -> pure r
             Left e -> do
               readIORef batch >>= maybe (pure ()) stop
               pure (BlobUnavailable (msg (Text.pack (show e))))
  }

   where
    -- One `cat-file --batch` for the whole walk, started on first use and kept.
    --
    -- The protocol, pinned against git 2.55: write "<oid>\n", read one line back.
    -- Either "<oid> <type> <size>", then <size> bytes and a newline, or
    -- "<oid> missing" and nothing else.
    --
    -- NONE OF THAT IS OPTIONAL AND THE SIZE IS NOT A FRAME. git does not write
    -- the size it announced: it writes the size out of the object's header and
    -- then the whole body, and those differ, because a loose object can be
    -- self-consistent and lie. Measured on git 2.55, a header saying @blob 10@
    -- over two megabytes of @A@: the reply is "<oid> blob 10" and 2 097 202 bytes
    -- on stdout. A reader that takes the announced ten and a newline leaves
    -- 2 097 191 bytes in the pipe, and they become the answer to the NEXT object
    -- -- one path's content, size and "missing" verdict attributed to another,
    -- silently, with a clean exit. That is worse than the fork-per-blob version
    -- this replaced, where a lie stayed inside its own answer. The commit that
    -- introduced batch reading claimed the opposite in its own message; it was not
    -- measured, and this is.
    --
    -- So the fields are checked here and none of these checks is what holds:
    -- 'ended' is. The echoed id and the trailing newline are bytes a lying body
    -- can contain, and a body built to contain them was built and does. What
    -- these do is answer precisely for the shapes they name, which is worth
    -- having on the one path where every answer is about somebody's file.
    askBatch oid = do
      (p, a) <- batchProc
      -- ONE DEADLINE FOR THE WHOLE REPLY, and not only a bound per read.
      --
      -- Every read below has its own idle bound, and a source that says something
      -- before each of them is never idle: 8 KiB every nine seconds is a blob
      -- that takes as long as it likes, inside a walk whose budget is checked
      -- only BETWEEN blobs. Measured shape, on a slow store reached through
      -- objects/info/alternates: 216 s inside one blob against a 180 s budget for
      -- the whole walk, in a verb meant for a pre-receive hook.
      --
      -- This is the same defect the listing reader found and fixed two functions
      -- down, in the same words: "a variant that emits a record now and then
      -- resets the idle counter for ever". The blob reader was written without
      -- the deadline half.
      now <- nowSeconds
      let dl = now + fromIntegral (gbBlobSeconds bounds)
          oidB = Text.encodeUtf8 oid
          give = restart (p, a)
      wrote <- tryAny do
                 liftIO (B8.hPutStrLn (getStdin p) oidB)
                 liftIO (hFlush (getStdin p))
      case wrote of
        -- A batch process that has died since the last blob. The tool ran and
        -- stopped running, which is what ReaderFailed is for.
        Left e -> give (BlobUnavailable (msg (Text.pack (show e))))
        Right () -> lineFrom dl (getStdout p) >>= \case
          -- THREE DIFFERENT THINGS, and they were one Nothing that became "this
          -- is local: no process slots". End of file is git gone; silence is git
          -- sitting there; a reply line with no newline in four kilobytes is a
          -- format this reader does not know.
          ReadEof -> give (gone "before answering")
          ReadIdle -> give (BlobStalled ( "cat-file --batch did not answer in "
                              <> Text.pack (show (gbCallSeconds bounds)) <> "s" ))
          -- THE DEADLINE IS A BUDGET, not a silence: its own message says the
          -- source "was still sending", and routing it to "git ran and did not
          -- answer" printed a refusal that contradicted itself and advised
          -- looking for a dead mount. CanonTooSlow is the one whose advice is
          -- already about a read costing more than this reader will spend.
          ReadOverdue e -> give (BlobBudget e)
          ReadTooLong -> give (BlobProtocol "cat-file --batch sent no reply line")
          ReadGot header -> case B8.words header of
            -- The ECHOED ID. git answers in the order it is asked, so a reply
            -- about another object means this reader and git have stopped
            -- counting the same replies, and every answer after it would land on
            -- the wrong path.
            (echo : _) | echo /= oidB ->
              give (BlobProtocol "cat-file --batch answered about another object")
            [_, "missing"] -> missing oid
            [_, ty, szB]
              -- The TYPE, because the listing already said blob and this reader
              -- asked for what the listing named. Anything else is the two of
              -- them disagreeing about what the tree holds.
              | ty /= "blob" ->
                  -- The type NAMED, not echoed. It is a token from a fixed
                  -- vocabulary when git wrote it, and a stranger's bytes when
                  -- anything else did; PEP-22 puts a tool's words in a quoted
                  -- block and nowhere else, and this is a field in a line of
                  -- advice. So the known ones are spelled out and everything
                  -- else is described.
                  give (BlobProtocol ("cat-file --batch answered " <> typeWord ty))
              | Just n <- sizeOf szB ->
                  if n > gbBlobBytes bounds
                    -- The stream is now out of step by n+1 bytes, and skipping
                    -- them is the work this reader has just refused to do.
                    -- Cheaper and safer to start a new process for what follows.
                    then give (BlobOversize n)
                    else exactly dl (getStdout p) n >>= \case
                      -- NOT followed by a read of the newline, which is what the
                      -- first version did unconditionally. The reverse lie -- a
                      -- header announcing more than git then writes -- made that
                      -- read wait out its own bound after this one had waited out
                      -- its: 269 bytes of repository, two minutes of a
                      -- pre-receive hook, and then "this is local".
                      --
                      -- A body that stops short has git ALIVE and waiting for the
                      -- next request, so it arrives as SILENCE: that is the one
                      -- case here that is a fact about the object.
                      --
                      -- End of file is not. It is git gone -- killed by an OOM
                      -- killer inside a hook's cgroup, say -- and reporting it as
                      -- a short object gave one false finding per remaining file
                      -- and exit 2, where the truth is that nothing was learned
                      -- about canon. The split was made on the newline read below
                      -- and not on this one.
                      BodyShort _ ReadEof -> give (gone "mid-body")
                      BodyShort got ReadIdle -> give (short n got)
                      BodyShort _ (ReadOverdue e) -> give (BlobBudget e)
                      BodyShort _ _ -> give (BlobProtocol "cat-file --batch reply is unreadable")
                      BodyGot bs -> exactly dl (getStdout p) 1 >>= \case
                        BodyGot "\n" -> ended give p n bs
                        -- ONLY a byte that is not the newline says the body ran
                        -- past its header. End of file here says the opposite:
                        -- git delivered exactly what it announced and then ended,
                        -- so "git delivered more" was the one thing that had not
                        -- happened.
                        BodyGot _ -> give (over n)
                        BodyShort _ ReadEof -> give (gone "mid-reply")
                        BodyShort _ (ReadOverdue e) -> give (BlobBudget e)
                        BodyShort _ _ -> give (BlobStalled
                                    ( "cat-file --batch sent a body of "
                                        <> Text.pack (show n)
                                        <> " bytes and not the newline after it" ))
            _ -> give (BlobProtocol "cat-file --batch said something else")

    -- The batch process ended. NOT a finding about the file it was in the middle
    -- of: a git the kernel killed says nothing about somebody's tree, and one
    -- false finding per remaining file, with exit 2, is a report about canon that
    -- is not about canon.
    gone where_ = BlobUnavailable ("cat-file --batch closed its output " <> where_)

    -- An object type as a word this program chose. git's four are named; anything
    -- else is described rather than repeated, since a field in a line of advice
    -- is not where a stranger's bytes go.
    typeWord ty | ty `elem` ["blob","tree","commit","tag"] = "a " <> decodeS ty
                | otherwise = "a type this reader does not know"

    over n = BlobRefused ( "the object's header announces "
               <> Text.pack (show n)
               <> " bytes and git delivered more" )

    -- What was announced and what arrived, and no verdict on which of them is at
    -- fault: a short body is the object lying, and a body that stops for ten
    -- seconds on a sick disk looks the same from here. Both are true of this line.
    short n got = BlobRefused ( "the object's header announces "
                    <> Text.pack (show n) <> " bytes and git delivered "
                    <> Text.pack (show got) <> " and stopped" )

    -- The end of a reply, and the only check here that actually holds.
    --
    -- A reply is not self-delimiting and the announced size does not delimit it,
    -- so THE REPLY IS NOT OVER UNTIL THE STREAM IS QUIET. Checking the echoed id
    -- and the newline after the body -- the obvious two, and the two this reader
    -- was told would close the hole -- closes nothing: those are the first eleven
    -- bytes of a lying object's body, and an object whose body begins
    -- @AAAAAAAAAA\\n<next-oid> blob 18\\n...@ passes both while still being a
    -- forged reply for the NEXT path. Built and measured: 479 bytes of git
    -- objects, and a file holding @(event (id "abc") (kind note))@ audited as the
    -- contents of somebody else's.
    --
    -- hReady is what has no such gap. Read exactly the announced bytes and the
    -- newline and an honest reply is CONSUMED WHOLE: git has written nothing else
    -- and is waiting for the next request, so there is nothing to be ready. A
    -- body longer than its header says leaves the rest of itself in the pipe, and
    -- there is no timing window in that -- git wrote it before the reply this
    -- reader has already finished reading.
    --
    -- At EOF hReady throws rather than answering, and an EOF is git gone, not git
    -- verbose: it is not this object's fault, and the next request says so on its
    -- own.
    --
    -- EOF AND NOTHING ELSE, though, which is the second half of the fix whose
    -- first half is the hSetBinaryMode in 'batchProc'. This used to catch every
    -- exception and read all of them as "consumed whole" -- the benign answer,
    -- chosen for the benign cause -- so the decoder error that a non-UTF-8 byte
    -- raised took the same path as an EOF and waved the lying object through.
    -- Binary mode means no decoder error is raised any more; treating an
    -- unexpected one as unknown means that if some other cause is found later,
    -- the reader gives up on the batch rather than believing a prefix.
    --
    -- And it gives up with 'gone' rather than with 'over', which is a difference
    -- about who is at fault. 'over' says "the object's header announces N bytes
    -- and git delivered more" -- an accusation against somebody's tree, made
    -- here on the evidence of a handle this reader could not question. What is
    -- actually known is that the answer cannot be framed any more, which is what
    -- 'gone' says, and which the two mid-reply cases above already say for the
    -- same condition.
    ended give p n bs = tryAny (liftIO (hReady (getStdout p))) >>= \case
      Right True  -> give (over n)
      Right False -> consumed
      Left e | isEof e   -> consumed
             | otherwise -> give (gone "while checking whether its reply was over")
      where
        consumed = do
          modifyIORef' spent (+ BS.length bs)
          pure (BlobText (decodeS bs))

        isEof e = maybe False isEOFError (fromException e)

    -- "missing", WITH GIT'S OWN WORDS FOR IT when there are any.
    --
    -- Batch mode collapses "not here" and "here and unreadable" into one word and
    -- puts the reason on stderr: measured, an object with its permissions removed
    -- answers @<oid> missing@ on stdout and "error: unable to open loose object
    -- <oid>: Permission denied" on stderr. Reported as the bare word it was a
    -- story this reader had chosen, which is what 'BlobRefused' exists not to do.
    --
    -- Matched BY OBJECT ID rather than by what arrived since the request was
    -- written. stderr and stdout are two pipes read by two threads, so a line git
    -- wrote before the answer can be collected after it, and a window would then
    -- put it under the next path. git names the object in these messages, so the
    -- id is what ties a line to a request; a line that does not name one is about
    -- the object store rather than this path and is left out.
    --
    -- WAITED FOR, briefly, because matching by id fixes the attribution and not
    -- the timing: git writes the complaint before the answer, so the bytes are in
    -- the pipe by the time this runs, but the thread that collects them may not
    -- have woken yet. Without the wait the message was git's words or this
    -- reader's fallback depending on the scheduler, which showed up as one flaky
    -- example in twelve runs of the suite -- and an audit whose text depends on
    -- timing is an audit two people cannot compare.
    --
    -- Bounded twice. Per object by 'gbBlobWords'; and once one of these waits
    -- runs out, no later one waits at all, because a source that did not explain
    -- its first missing object is not going to explain the hundredth and the
    -- waiting would be the whole walk. Giving up on the WAIT only: a message
    -- already collected is still used.
    missing oid = do
      let mine errs = [ l | l <- B8.lines errs, Text.encodeUtf8 oid `BS.isInfixOf` l ]
          -- Cut in BYTES, before decoding. gbBlobMessage is a byte budget (its
          -- own haddock reaches twelve gigabytes by multiplying it by
          -- maxCanonFiles), and spending it with Text.take spends characters,
          -- which are up to four bytes apiece: the budget was up to four times
          -- what it says. The same confusion was fixed for the walk's byte budget
          -- one commit ago and left standing here.
          say ls = BlobRefused (msg (decodeS (BS.take (gbBlobMessage bounds)
                                                (B8.unlines ls))))
          waitFor 0 = writeIORef patient False >> pure (BlobRefused "missing from this clone")
          waitFor k = readIORef said <&> mine >>= \case
            [] -> liftIO (Conc.threadDelay 2000) >> waitFor (k - 1 :: Int)
            ls -> pure (say ls)
      here <- readIORef said <&> mine
      case here of
        (_:_) -> pure (say here)
        [] -> readIORef patient >>= \case
          False -> pure (BlobRefused "missing from this clone")
          -- Steps of two milliseconds, so the count is the budget halved.
          True  -> waitFor (max 1 (gbBlobWords bounds `div` 2))

    -- The batch process and the thread reading its stderr.
    --
    -- THAT THREAD IS NOT FOR THE MESSAGE. A pipe holds 64 KiB and git blocks on
    -- write when it is full: measured on git 2.55, one @cat-file --batch@ request
    -- against a repository whose objects/info/alternates names 2000 missing
    -- directories writes 206 893 bytes of stderr before answering. With nobody
    -- reading it, git stops in write() while this reader waits on stdout, the wait
    -- runs out at gbCallSeconds, and the audit reports "this is local". The same
    -- reasoning is spelled out at length over 'listing', where both pipes have
    -- been read at once since the day it deadlocked; the batch process was given
    -- a third pipe and none of it.
    --
    -- Under mask_, because an async exception between startProcess returning and
    -- the IORef being written is a git nothing has a handle to any more.
    batchProc = readIORef batch >>= \case
      Just pa -> pure pa
      Nothing -> mask_ do
        cfg <- inDir (proc "git" ["cat-file", "--batch"])
        p <- startProcess (setStdin createPipe
                            (setStdout createPipe
                              (setStderr createPipe cfg)))
        -- BINARY, and this is load-bearing rather than tidiness. Every read of
        -- this handle is 'BS.hGetSome', which does not decode -- but 'hReady' in
        -- 'ended' DOES: it is hWaitForInput, which runs the handle's text decoder
        -- to find out whether a character is available. A pipe handle carries the
        -- locale encoding unless told otherwise, so under any byte-rejecting
        -- encoding (which is UTF-8 and also LC_ALL=C) hReady on an undecodable
        -- byte throws instead of answering, and on the lone first byte of a
        -- multi-byte sequence answers False. 'ended' treated both as "the reply
        -- was consumed whole". Measured on this build:
        --
        --   excess in the pipe   text mode    binary mode
        --   none                 False        False
        --   ASCII                True         True
        --   0xff then ASCII      throws       True
        --   lone 0xe2            False        True
        --
        -- So a loose object whose header announces fewer bytes than its body
        -- carries defeated the one check that catches it by starting the excess
        -- with a byte that is not UTF-8: the truncated prefix was returned as
        -- that path's content and the batch was left desynchronised. A git object
        -- body is bytes, and asking a decoder about it was the whole mistake.
        liftIO (IO.hSetBinaryMode (getStdout p) True)
        a <- async (tailInto (gbToolMessage bounds) said (getStderr p))
        writeIORef batch (Just (p, a))
        pure (p, a)

    -- Stop the batch process, in the one order that does not wedge.
    --
    -- The reader is cancelled FIRST. It is sitting in hGetSome holding the
    -- stderr Handle's MVar, and the first thing the teardown does is hClose that
    -- same Handle, which takes the same MVar; and csClose runs inside unliftio's
    -- finally, whose cleanup is uninterruptibly masked, so the block would not
    -- answer Ctrl-C either. This is the trap the small-call reader fell into with
    -- a bare async, measured there at 19.7 s against 68 microseconds.
    --
    -- Then stdin: cat-file --batch ends on EOF, which is the graceful way out and
    -- the common one, so the signals in the teardown are for a git that does not
    -- take it.
    -- The buffer is cleared AFTER the reader that fills it is gone. Cleared
    -- first, a line still in flight from the dying process landed in the pool the
    -- next one starts with, where 'missing' matches by object id and would have
    -- attributed it to whatever object shares that id -- the same misattribution
    -- the id matching exists to prevent, arriving through the other end.
    stop (p, a) = do
      writeIORef batch Nothing
      tryAny (cancel a)
      writeIORef said mempty
      tryAny (liftIO (hClose (getStdin p)))
      teardown p

    restart pa answer = stop pa >> pure answer

    -- One line, bounded in time like every other read here.
    --
    -- A byte at a time, and it has to be: the reply line is followed by the body,
    -- and a buffered read would take part of it. The bound on the accumulator is
    -- for a stream with no newline in it at all.
    lineFrom dl h = go mempty
      where
        go acc = readStep dl (gbCallSeconds bounds) h 1 >>= \case
          Left r -> pure r
          Right c | c == "\n" -> pure (ReadGot acc)
                  | BS.length acc > 4096 -> pure ReadTooLong
                  | otherwise -> go (acc <> c)

    -- Exactly n bytes, on the BODY bound: past the header git has said what it is
    -- about to send, so silence in the middle of it is not a git thinking.
    exactly dl h n = go n mempty
      where
        go 0 acc = pure (BodyGot acc)
        go k acc = readStep dl (gbBodySeconds bounds) h k >>= \case
          Left r -> pure (BodyShort (n - k) r)
          Right c -> go (k - BS.length c) (acc <> c)

    -- One read, against BOTH an idle bound and the reply's deadline.
    --
    -- The idle bound catches a source that has stopped; the deadline catches one
    -- that has not stopped and will not finish, which no per-read bound can see.
    -- Checked before the read, so the worst case is the deadline plus one idle
    -- bound rather than the deadline exactly.
    readStep dl idle h k = do
      now <- nowSeconds
      if now > dl
        then pure (Left (ReadOverdue ( "cat-file --batch was still sending after "
                                         <> Text.pack (show (gbBlobSeconds bounds))
                                         <> "s on one object" )))
        else timeout (idle * 1000000) (liftIO (BS.hGetSome h k)) >>= \case
          Nothing -> pure (Left ReadIdle)
          Just c | BS.null c -> pure (Left ReadEof)
                 | otherwise -> pure (Right c)

    -- Read a handle to EOF, keeping the LAST n bytes where another thread can see
    -- them as it goes. For the batch process's stderr: it has to be read
    -- continuously or git blocks on a full pipe, and what is wanted out of it is
    -- the most recent complaint rather than the first.
    tailInto n ref h = go
      where
        go = do
          c <- liftIO (BS.hGetSome h 65536)
          unless (BS.null c) do
            -- atomicModifyIORef', because this runs on its own thread and the
            -- buffer is read from another. The clear that used to race it is
            -- ordered after the cancel now, so the specific race is gone; a
            -- read-modify-write on a shared IORef from a thread of its own is
            -- still not something to leave non-atomic for the next reader of
            -- this code to rediscover.
            atomicModifyIORef' ref \acc ->
              let a = acc <> c
              in (if BS.length a > n then BS.drop (BS.length a - n) a else a, ())
            go

    -- The budget on the WHOLE read, checked before each blob: a wall-clock
    -- deadline and a ceiling on the bytes handed back. Neither can live in
    -- readCanon, which is pure and has no clock, and neither can be a listing
    -- bound, because the listing of a tree that costs six minutes to walk is 3.6
    -- MB and passes every bound there is.
    --
    -- The clock starts HERE, at the first blob, and not when the source was
    -- built. Started at build time it ran from before csCommit, so it was also a
    -- second and shorter bound on the listing: gbListingSeconds is 600 and
    -- gbReadSeconds is 180, and a listing that ran past 180 got the walk's
    -- refusal, whose advice is about a walk that had not begun.
    --
    -- The bytes are counted where the blob is read, in BYTES. Counted here they
    -- were Text.length -- characters -- against a byte bound, so a canon of
    -- multibyte text was allowed past it by however much of it was not ASCII.
    budgeted act = do
      now <- nowSeconds
      begun <- readIORef clock >>= \case
                 Just t  -> pure t
                 Nothing -> writeIORef clock (Just now) >> pure now
      used <- readIORef spent
      -- With the measurement, for the reason the listing reader now carries one:
      -- a budget that reports only the number it was given cannot be told from a
      -- clock that is lying about how much of it was spent, and both look like
      -- "passed 180s" in a log.
      if now - begun > fromIntegral (gbReadSeconds bounds)
        then pure (BlobBudget ( "reading canon passed "
                                  <> Text.pack (show (gbReadSeconds bounds))
                                  <> "s, after " <> tookText (now - begun) ))
        else if used > gbReadBytes bounds
          then pure (BlobBudget ( "reading canon passed "
                                    <> Text.pack (show (gbReadBytes bounds))
                                    <> " bytes" ))
          else act

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
    inDir p = gitIn cwd [] p

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

    teardown :: forall i . Process i Handle Handle -> m ()
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
        settle = waitGone (max 1 (gbTeardownSeconds bounds * 1000000 `div` step))
        step = 20000
        waitGone :: Int -> m Bool
        waitGone 0 = isJust <$> getExitCode p
        waitGone n = getExitCode p >>= \case
                   Just _  -> pure True
                   Nothing -> liftIO (Conc.threadDelay step) >> waitGone (n - 1)

    -- The listing, read to 'gbListingBytes' and no further; past that the process
    -- is dead by the time the answer is returned.
    --
    -- The answer is a 'Listed' and was a triply nested @Either@ of tuples, in
    -- which "the bound was reached", "the reader gave up" and "git failed" were
    -- told apart by which side of which Either held a Maybe. Two of the five
    -- outcomes now in it were missing, and one was missing because there was no
    -- room in the shape to put it.
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
                     Left (why, seen, took) -> pure (ListedStalled why seen took)
                     Right Nothing -> pure ListedTooBig
                     Right (Just bs) -> do
                       -- Bounded, like every other wait in this module. A child
                       -- that writes a record, closes stdout, catches SIGTERM and
                       -- sleeps leaves this waiting: measured at over 20s, and it
                       -- was the only unbounded wait left.
                       --
                       -- A wait that runs out is UNKNOWN, and gets its own answer.
                       -- It was made to keep the listing, on the reasoning that
                       -- stdout at EOF means the listing is complete: that is
                       -- false, and it traded a loud hypothetical false negative
                       -- for a quiet demonstrable false positive. EOF means the
                       -- write end was closed, which a writer that has finished
                       -- does, and so does one that was killed, crashed, or closed
                       -- the descriptor with the tree half written. Only the exit
                       -- code tells them apart, which is why it is waited for.
                       waited <- timeout (gbCallSeconds bounds * 1000000)
                                         (waitExitCode p)
                       case waited of
                         Nothing -> pure (ListedUnfinished (BS.length bs))
                         Just ExitSuccess -> pure (ListedOk bs)
                         -- The message is only wanted when git failed, and waiting
                         -- for it is bounded even then: stderr EOFs when the last
                         -- holder of the fd closes it, and a grandchild (an ssh
                         -- ControlPersist master, say) inherits it and outlives
                         -- git. On the success path there is nothing to wait for
                         -- at all, and withAsync cancels the drain on the way out.
                         Just _ -> ListedFailed . fromMaybe mempty
                                     <$> timeout 2000000 (wait errA)
              writeIORef answer (Just a)
              pure a
        )
      case r of
        Right a -> pure (Right a)
        Left e  -> readIORef answer <&> maybe (Left (Text.pack (show e))) Right

    -- Which bound ran out, in the words that say what to do about it. The idle
    -- one is a git that is there and has stopped; the deadline is a git that has
    -- not stopped and is not going to finish.
    stallWord = \case
      StallIdle     -> "then nothing for " <> Text.pack (show (gbCallSeconds bounds)) <> "s"
      StallDeadline -> "ran past its " <> Text.pack (show (gbListingSeconds bounds))
                         <> "s deadline for the whole listing"

    -- One decimal place, which is the resolution a person acts on and enough to
    -- tell "it waited the whole minute" from "it gave up at once". The second of
    -- those is what a clock or a timer misbehaving looks like, and the report had
    -- no way to say it.
    tookText t = Text.pack (show (fromIntegral (round (t * 10) :: Int) / 10 :: Double)) <> "s"

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
      started <- nowSeconds
      go started 0 []
      where
        go started seen chunks = do
          now <- nowSeconds
          if now - started > fromIntegral (gbListingSeconds bounds)
            -- The DEADLINE, which the idle bound cannot replace: a listing that
            -- keeps dribbling bytes is never idle and can still be a tree walk
            -- that will not finish this century.
            then pure (Left (StallDeadline, seen, now - started))
            else do
             c <- timeout (gbCallSeconds bounds * 1000000) (liftIO (BS.hGetSome h 65536))
             -- MEASURED after the read and not before it, so what is reported is
             -- how long the reader really spent rather than how long it was
             -- allowed to. The two are the same only when nothing is wrong.
             stopped <- nowSeconds
             case c of
              Nothing -> pure (Left (StallIdle, seen, stopped - started))
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
    -- the call for ever. The listing got a bound and these did not, which left the
    -- hang in front of every other bound this reader has.
    --
    -- Generous, because none of these is allowed to be slow: three ref lookups,
    -- all local, all off the network by construction.
    --
    -- BOTH STREAMS ARE DRAINED and both are kept to 'gbToolMessage'. There used
    -- to be a @cap@ parameter here for reading a blob to a ceiling; the blob read
    -- moved to the batch process and the parameter stayed, called from one place
    -- with @maxBound@, which left stdout accumulating without a ceiling at all.
    -- These three calls answer with a ref, an object id and a path, so a bound of
    -- 64 KiB is four orders over anything git can honestly say here, and the
    -- reading continues past it because a stream nobody reads is a writer that
    -- blocks.
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
               withAsync (drain (gbToolMessage bounds) (getStdout p)) $ \out ->
               withAsync (drain (gbToolMessage bounds) (getStderr p)) $ \errA ->
                 timeout (gbCallSeconds bounds * 1000000) do
                   o <- wait out
                   code <- waitExitCode p
                   e <- wait errA
                   pure (code, if code == ExitSuccess then o else e)
      pure case r of
        -- THIS READER'S OWN WORDS FIRST, then the runtime's as evidence. It used
        -- to be the runtime's alone, and those are not the same sentence on two
        -- platforms: linux says "startProcess: exec: does not exist (No such file
        -- or directory)" and aarch64-osx says "startProcess: find_executable:
        -- failed (Unknown error: -2)". A report made of somebody else's words
        -- says whatever they happen to say, which is the rule 'typeWord' states
        -- twenty lines up and this line did not follow.
        --
        -- The runtime's text is KEPT rather than replaced: it is the only thing
        -- that distinguishes a git that is not installed from a fork that failed
        -- for want of process slots, and both reach here.
        Left e -> SmallUnstartable ( "git could not be started: "
                                       <> Text.pack (show e) )
        Right Nothing ->
          SmallStalled ( "git " <> Text.pack (unwords (take 2 args))
                           <> " did not finish in "
                           <> Text.pack (show (gbCallSeconds bounds))
                           <> "s and was given up on" )
        Right (Just (ExitSuccess, o)) -> SmallOk o
        Right (Just (code, e)) -> SmallFailed code e

-- | How one read from the batch process ended.
--
-- Five outcomes, and they were @Maybe ByteString@: end of file, silence, a
-- deadline reached, and a reply line with no newline in four kilobytes all came
-- back as Nothing and were reported as "this reader could not run git: no
-- process slots, no file descriptors". git had run, was running, and in three of
-- the four cases was still there.
data Read1 =
    ReadGot ByteString
    -- | The source closed its output.
  | ReadEof
    -- | The source is there and has said nothing for the idle bound. Told apart
    -- from 'ReadOverdue' because in the middle of a body they mean opposite
    -- things: git has already committed to a length, so silence is the object
    -- having less in it than its header says, while a source still sending is a
    -- source this reader is giving up on.
  | ReadIdle
    -- | Past the deadline for the whole reply, however busy it has been.
  | ReadOverdue Text
    -- | A reply line longer than this reader will read one.
  | ReadTooLong

-- | Reading a body of an announced length.
--
-- Carries HOW MUCH ARRIVED when it did not all arrive, because the honest
-- message names it: "announced 20000, delivered 10, then stopped" is true whether
-- the object is short or the disk is sick, and "the object did not deliver them"
-- is an accusation against the tree that only one of those two deserves.
data Body = BodyGot ByteString | BodyShort Int Read1

-- | How one of the three small calls ended.
--
-- Four outcomes and they used to be two: an @Either Text (ExitCode, ByteString)@
-- whose Left meant BOTH "git could not be started" and "git started and never
-- said anything". Those two get opposite advice -- one is local, the other is a
-- git sitting there that a retry buys another minute of -- and the caller had no
-- way to tell them apart.
data Small =
    -- | Exited 0, with stdout.
    SmallOk ByteString
    -- | Exited non-zero, with the code and what it said on stderr.
  | SmallFailed ExitCode ByteString
    -- | Ran and did not finish within 'gbCallSeconds'.
  | SmallStalled Text
    -- | Could not be run at all, with the reason the runtime gave.
  | SmallUnstartable Text

-- | Now, in seconds, from a counter whose UNIT IS IN ITS NAME.
--
-- Every bound in this module is a duration in seconds and every one of them was
-- computed from 'UnliftIO.IO.getMonotonicTime', which is documented to return
-- seconds and does on linux. On aarch64-osx it returns NANOSECONDS, and the
-- whole @gitCanon against real git@ group went red there and stayed green
-- everywhere else: every listing was refused for running past a 600s deadline it
-- had been in for a few milliseconds. The measurement that caught it is the one
-- the report started carrying, and the number that named it is a test which
-- deliberately waits two seconds and reported @2.008234875e9@ -- two seconds, to
-- three decimal places, in nanoseconds.
--
-- So the scaling is done here rather than trusted from there.
-- 'getMonotonicTimeNSec' returns a @Word64@ of nanoseconds by its type, which is
-- a contract a platform cannot quietly reinterpret, and dividing is one line.
-- Verified equal to the old call on linux to four decimal places over a 250ms
-- delay; see 'HBS2.Hub.GitRepoSpec' for the test that fails on a platform where
-- it would not be.
nowSeconds :: MonadIO m => m Double
nowSeconds = liftIO do
  ns <- getMonotonicTimeNSec
  pure (fromIntegral ns / 1e9)

-- | Which of the listing reader's two time bounds ran out.
--
-- They mean different things and call for different things, and the report could
-- not tell them apart: it printed both configured numbers side by side and left
-- the reader to guess. 'StallIdle' is a writer that STOPPED -- git is there and
-- has sent nothing for a whole 'gbCallSeconds'. 'StallDeadline' is a writer that
-- has not stopped and will not finish, which the idle bound can never catch,
-- because a listing that dribbles one record now and then is never idle.
data StallWhy = StallIdle | StallDeadline
  deriving stock (Eq,Show)

-- | How a run of @ls-tree@ ended. One constructor per outcome, because the
-- caller's answer differs for every one of them and three of them used to share
-- a shape.
data Listed =
    -- | git exited 0 and stdout was read to EOF: the listing, whole.
    ListedOk ByteString
    -- | git exited non-zero, with what it said.
  | ListedFailed ByteString
    -- | Past 'gbListingBytes' while it was still arriving.
  | ListedTooBig
    -- | The reader gave up: WHICH bound, the bytes seen by then, and how long it
    -- actually took.
    --
    -- All three, because the report used to carry only the bytes and then print
    -- both configured bounds beside them -- "nothing for 60s or ran past 600s in
    -- total" -- which describes the configuration and not the observation. A CI
    -- log full of that cannot say which of the two fired, or after how long, so a
    -- failure on a machine nobody can log into is uninterpretable. That is the
    -- state this reader was in when its whole listing group went red on
    -- aarch64-osx and green on linux.
  | ListedStalled StallWhy Int Double
    -- | stdout reached EOF and git had not exited by 'gbCallSeconds' after it,
    -- with the bytes read. NOT a complete listing: EOF is the write end being
    -- closed, and a writer that was killed or crashed closes it too.
  | ListedUnfinished Int

-- | A size column, or a batch reply's size, as an 'Int'.
--
-- Not 'readMay' on its own, which accepts two things this cannot use. A leading
-- minus: a negative size sails under every @> bound@ check there is. And a number
-- past 'maxBound', because 'read' for an Int goes through Integer and
-- 'fromInteger' WRAPS, so 18446744073709551626 arrives as 10 and a body of two
-- megabytes is admitted under a ten-byte ceiling. Eighteen digits is the widest
-- decimal that cannot overflow a 64-bit Int.
sizeOf :: ByteString -> Maybe Int
sizeOf b
  | BS.null b = Nothing
  | not (B8.all (\c -> c >= '0' && c <= '9') b) = Nothing
  | B8.length b > 18 = Nothing
  | otherwise = readMay (B8.unpack b)

-- | Is this a FULL object id as git writes one: 40 lowercase hex for sha1, 64
-- for sha256?
--
-- It accepted 4 to 64 characters in either case, which is what git accepts when
-- a HUMAN types one. Nothing here is typed by a human: every id this reader uses
-- comes out of @rev-parse@ or the size column of @ls-tree@, both of which write
-- the full canonical lowercase form. Measured on git 2.55: the batch reply echoes
-- the canonical form too, whatever was written to it.
--
-- That matters because the batch reader compares the echoed id to the one it
-- wrote BYTE FOR BYTE, and treats a mismatch as the two of them having lost
-- count of each other. A guard that admits @DEADBEEF@ and @dead@ is weaker than
-- the invariant the protocol rests on, so an id that could never round-trip
-- would have been read as a desync rather than as the bad id it is.
isObjectId :: Text -> Bool
isObjectId t = (Text.length t == 40 || Text.length t == 64) && Text.all hex t
  where hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

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
        -- A size that is not a non-negative number this reader can hold is the
        -- same kind of nonsense; 'sizeOf' says which numbers those are.
        | Just n <- sizeOf sz -> Blob (Text.decodeUtf8Lenient oid) n
        -- git's exact spelling for a blob it could not size, and nothing else. A
        -- negative size is not a size and an unknown word is not BAD: both are the
        -- format having shifted, which is Unparsed and is loud, rather than "the
        -- object is missing, go and fetch", which is quiet and would be a guess.
        | sz == "BAD" -> BlobMissing (Text.decodeUtf8Lenient oid)
        | otherwise -> Unparsed
      -- A record with the right shape but the wrong number of words is a format
      -- shift, and is reported as one rather than as a tree full of submodules.
      _ -> Unparsed

-- | Where a git command runs, and what it is allowed to do while it runs.
--
-- One definition for the whole package. The reader and the writer disagree
-- about almost everything else and must not disagree about this: which
-- repository a command lands in is decided here, and two spellings of it are
-- two answers to that question.
--
-- @extra@ is forced on top of the list below, with the same removal rule, for
-- the variables only one caller needs (the writer names an index file and a
-- committer; the reader has neither).
gitIn :: MonadIO m
  => Maybe FilePath -> [(String,String)] -> ProcessConfig a b c -> m (ProcessConfig a b c)
gitIn cwd extra p = do
  env0 <- liftIO getEnvironment
  let forced = forcedEnv <> extra
      kept = List.filter ((`notElem` (fmap fst forced <> named)) . fst) env0
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
forcedEnv :: [(String,String)]
forcedEnv =
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
whichRepository :: [String]
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


-- | The two answers a git call gives that are not about the command.
--
-- Everything else is: a non-zero exit means one thing to the writer, another
-- to the bundle plumbing, and for one caller it is not a failure at all (an
-- absent ref exits 1). So the runner decides these two and hands the rest
-- back, rather than inventing a vocabulary every caller has to translate out
-- of.
data GitTrouble =
    GitUnstartable Text   -- ^ git could not be started, or stopped being there
  | GitStalled Text       -- ^ git ran and did not finish in the time allowed
  deriving stock (Eq,Show)

-- | Run one git command to completion, bounded.
--
-- For callers whose input is their own: the writer's files and the bundle
-- plumbing's arguments, all produced by this build. The audit reader next door
-- does NOT use this, and should not: it reads a tree a stranger wrote and
-- needs the per-chunk bounds, the shared @cat-file --batch@ and the teardown
-- escalation that this does not have.
--
-- @extra@ goes to 'gitIn'. @what@ names the command in a message; the args
-- carry it too, and quoting them whole would put a caller's path in a report.
gitRun :: MonadUnliftIO m
       => Maybe FilePath          -- ^ which repository, or the discovered one
       -> [(String,String)]       -- ^ forced environment, see 'gitIn'
       -> Int                     -- ^ seconds before giving up
       -> Text                    -- ^ what to call it if it does not answer
       -> [String]
       -> LBS.ByteString          -- ^ stdin
       -> m (Either GitTrouble (ExitCode, ByteString, ByteString))
gitRun cwd extra secs what args input = do
  cfg <- gitIn cwd extra (proc "git" args)
  let piped = setStdin (byteStringInput input)
                (setStdout byteStringOutput (setStderr byteStringOutput cfg))
  r <- tryAny (timeout (secs * 1000000) (readProcess piped))
  pure case r of
    -- The runtime's words are kept as evidence, for the reason the reader
    -- states at length: they are the only thing that tells a git that is not
    -- installed from a fork that failed for want of process slots.
    Left e -> Left (GitUnstartable ( "git " <> what <> " could not be started: "
                                       <> Text.pack (show e) ))
    Right Nothing ->
      Left (GitStalled ( "git " <> what <> " did not finish in "
                           <> Text.pack (show secs) <> "s" ))
    Right (Just (code, out, err)) ->
      Right (code, LBS.toStrict out, LBS.toStrict err)
