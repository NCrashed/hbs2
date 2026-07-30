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
import System.Environment (getEnvironment)
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
    -- | How long any single git call may take. Not the listing, which streams
    -- under its own byte bound; the four small ones, which have no reason to be
    -- slow and every reason not to hang.
  , gbCallSeconds  :: Int
  }

gitBounds :: GitBounds
gitBounds = GitBounds
  { gbListingBytes = maxListingBytes
  , gbListingFiles = maxCanonFiles
  , gbToolMessage  = 64 * 1024
  , gbBlobMessage  = 512
  , gbCallSeconds  = 60
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
gitCanonWith :: MonadUnliftIO m => GitBounds -> Maybe FilePath -> CanonSource m
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
      isRepo <- git ["rev-parse", "--git-dir"]
      case isRepo of
        Left e -> pure (Left (NoRepository (msg e)))
        Right (ExitFailure _, e) -> pure (Left (NoRepository (msg e)))
        Right (ExitSuccess, _) -> do
          here <- git ["show-ref", "--verify", "--quiet", Text.unpack metaRef]
          case here of
            Left e -> pure (Left (ReaderFailed (msg e)))
            Right (ExitFailure 1, _) -> pure (Left NoCanonRef)
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
      listing ["ls-tree", "-r", "-z", "-l", "--full-tree", Text.unpack commit]
        <&> \case
          -- The bound first: past it the process is torn down, so its exit code
          -- says how it was killed and not anything about the tree. It carries
          -- the BOUND: reading stops mid-chunk, so the byte count at that moment
          -- is neither the bound nor the size of the tree, and printing it said
          -- "over 102404096 bytes" where the bound is 102400000.
          Right (_, Nothing) -> Left (CanonListingTooBig (gbListingBytes bounds))
          Right (ExitSuccess, Just out)
            -- Counted on the RAW BYTES, before one TreeEntry exists. Records are
            -- NUL-terminated, so this is a single memchr scan. Counting the parsed
            -- list left the byte bound as the only thing between a listing of
            -- minimal 63-byte records and 1.6M entries built, sorted and held:
            -- eight times the file bound, refused after the allocation the bound
            -- exists to prevent.
            | recs out > gbListingFiles bounds -> Left (CanonTooMany (recs out))
            | otherwise -> Right (parseListing out)
          Right (_, Just e) -> Left (TreeUnreadable (msg (decodeS e)))
          -- Again: git not having run at all, four calls in, is local.
          Left e -> Left (ReaderFailed (msg e))

  , csBlob = \oid ->
      raw ["cat-file", "blob", Text.unpack oid] <&> \case
        Right (ExitSuccess, out) -> BlobText (decode out)
        -- git ran and would not give it, and its words are kept: a pruned object,
        -- a corrupt pack and EACCES on an object file are all exit 128 here, and
        -- calling them all "gone" put "fetch, or unshallow" under a permission
        -- error. Distinct from git not having run, below, which says nothing about
        -- canon at all: read as one answer, a fork that failed on EAGAIN became an
        -- unreadable event file and exit 2, the code for an audit that ran.
        Right (_, e) -> BlobRefused (Text.take (gbBlobMessage bounds) (msg (decode e)))
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
      let dropped = fmap fst forced <> refuseToLook
                      <> (case cwd of Nothing -> [] ; Just _ -> whichRepository)
          kept = List.filter ((`notElem` dropped) . fst) env0
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
        -- the last is a terminal: core.askPass, then GIT_ASKPASS (which overrides
        -- it), then /dev/tty, which `setStdin closed` does not cover. A hook that
        -- stops for a password nobody is there to type is a hung hook.
      , ("GIT_ASKPASS", "/bin/false")
      , ("SSH_ASKPASS", "/bin/false")
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

    -- Variables that stop git looking at all. Dropped ALWAYS, including the path
    -- the CLI actually takes, which is the one with no directory named: they do
    -- not answer "which repository", they refuse the question, and the answer
    -- here is already given by the caller's own working directory. Under a CI
    -- harness or `git bisect run` that sets a ceiling above it, a healthy
    -- repository reported "not a git repository" and exit 4.
    --
    -- The cost is honest: a caller who set a ceiling to keep git off a slow
    -- network mount does not get that protection here. This reader only ever
    -- looks where it was pointed.
    refuseToLook = [ "GIT_CEILING_DIRECTORIES", "GIT_DISCOVERY_ACROSS_FILESYSTEM" ]

    -- GIT_CONFIG_* are in NEITHER list: a config that turns off gpg signing or
    -- sets safe.directory is the caller's business, and this only reads.

    -- Lenient, so a blob that is not UTF-8 becomes replacement characters rather
    -- than an exception. It costs up to three bytes per invalid one, which the
    -- per-file bound does not account for since git reports the encoded size; the
    -- tree-wide bound does, being the sum of those same sizes.
    decode = decodeS . LBS.toStrict
    decodeS = Text.decodeUtf8Lenient

    -- A tool's complaint as a message. Its trailing newline is not part of what
    -- it said, and it survived into a report where every newline is escaped, so
    -- every one of these lines ended in a literal \x0a. Only the end: the interior
    -- newlines are kept, because the second line of a dubious-ownership complaint
    -- is the command that fixes it.
    msg = Text.stripEnd

    -- Decoded output, for the calls whose result is text.
    git args = raw args <&> fmap (fmap decode)

    -- Records in a raw listing: one NUL each.
    recs = BS.count 0

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
        ( withProcessTerm piped $ \p ->
            withAsync (drain (gbToolMessage bounds) (getStderr p)) $ \errA -> do
              out <- upTo (gbListingBytes bounds) (getStdout p)
              a <- case out of
                     Nothing -> pure (ExitFailure 1, Nothing)
                     Just bs -> do
                       code <- waitExitCode p
                       -- The message is only wanted when git failed, and waiting
                       -- for it is bounded even then: stderr EOFs when the last
                       -- holder of the fd closes it, and a grandchild (an ssh
                       -- ControlPersist master, say) inherits it and outlives
                       -- git. On the success path there is nothing to wait for at
                       -- all, and withAsync cancels the drain on the way out.
                       err <- if code == ExitSuccess
                                then pure mempty
                                else fromMaybe mempty <$> timeout 2000000 (wait errA)
                       pure (code, Just (if code == ExitSuccess then bs else err))
              writeIORef answer (Just a)
              pure a
        )
      case r of
        Right a -> pure (Right a)
        Left e  -> readIORef answer <&> maybe (Left (Text.pack (show e))) Right

    -- Read a handle to a bound. Stops at the first chunk that crosses it and does
    -- not read the rest, which is the whole point.
    upTo n h = go 0 []
      where
        go seen chunks = do
          c <- liftIO (BS.hGetSome h 65536)
          if BS.null c
            then pure (Just (BS.concat (List.reverse chunks)))
            else do
              let seen' = seen + BS.length c
              if seen' > n then pure Nothing
                           else go seen' (c : chunks)

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
      -- tryAny and not try @SomeException: the latter also caught UserInterrupt and
      -- AsyncCancelled, so Ctrl-C during an audit came back as a file that will
      -- not read and the audit went on to report it.
      tryAny (timeout (gbCallSeconds bounds * 1000000)
                      (readProcess (setStdin closed cfg)))
        <&> \case
          Left e -> Left (Text.pack (show e))
          Right Nothing ->
            Left ( "git " <> Text.pack (unwords (take 2 args))
                     <> " did not finish in "
                     <> Text.pack (show (gbCallSeconds bounds)) <> "s" )
          Right (Just (code, out, errOut)) ->
            Right (code, if code == ExitSuccess then out else errOut)

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
        | Just n <- readMay (B8.unpack sz) -> Blob (Text.decodeUtf8Lenient oid) n
        -- A blob with no number where the size goes. BAD today; anything that is
        -- not a number lands here, which is the safe side: it says fetch, and a
        -- fetch that finds nothing missing costs nothing.
        | otherwise -> BlobMissing (Text.decodeUtf8Lenient oid)
      -- A record with the right shape but the wrong number of words is a format
      -- shift, and is reported as one rather than as a tree full of submodules.
      _ -> Unparsed
