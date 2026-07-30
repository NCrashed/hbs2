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
  , parseListing
  ) where

import HBS2.Hub.Repo

import HBS2.CLI.Prelude hiding (filter)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.Environment (getEnvironment)
import System.Process.Typed

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
gitCanonIn cwd = CanonSource
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
      --   state                 show-ref --quiet   rev-parse ^{commit}
      --   absent                1                  1
      --   loose ref is garbage  1                  1
      --   names a blob          0                  1
      --   names a gone object   128                1
      --   names a commit        0                  0
      --
      -- A broken loose ref is therefore NOT distinguishable from an absent one:
      -- both are 1, and both stderrs say "not a valid ref". git knows the
      -- difference (rev-parse without --quiet warns "ignoring broken ref") and
      -- exposes it only as prose, which is not something to branch on. So this
      -- answers NoCanonRef for both, and the advice for it names both causes.
      isRepo <- git ["rev-parse", "--git-dir"]
      case isRepo of
        Left e -> pure (Left (NoRepository (msg e)))
        Right (ExitFailure _, e) -> pure (Left (NoRepository (msg e)))
        Right (ExitSuccess, _) -> do
          here <- git ["show-ref", "--verify", "--quiet", Text.unpack metaRef]
          case here of
            Left e -> pure (Left (NoRepository (msg e)))
            Right (ExitFailure 1, _) -> pure (Left NoCanonRef)
            Right (ExitFailure _, e) -> pure (Left (RefUnresolved (msg e)))
            Right (ExitSuccess, _) ->
              git ["rev-parse", "--verify", Text.unpack metaRef <> "^{commit}"] <&> \case
                Left e -> Left (NoRepository (msg e))
                Right (ExitSuccess, out) -> Right (Text.strip out)
                Right (_, e) -> Left (RefUnresolved (msg e))

  , csEntries = \commit ->
      -- --full-tree, and this is not a nicety: ls-tree resolves paths relative to
      -- the current directory, so run from a subdirectory an audit listed nothing
      -- and reported clean empty canon. -l for the size and the object id, so a
      -- bound can refuse a blob unfetched and a blob can be fetched without
      -- naming a path. -z because a path may hold any byte but NUL.
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
          -- says how it was killed and not anything about the tree.
          Right (_, Left n) ->
            Left (TreeUnreadable
                   ( "the tree listing is over "
                     <> Text.pack (show n) <> " bytes"
                     <> "; compaction is the answer to a canon this large"
                     <> " (PEP-19), not a bigger reader" ))
          Right (ExitSuccess, Right out) -> Right (parseListing out)
          Right (_, Right e) -> Left (TreeUnreadable (msg (decodeS e)))
          Left e -> Left (TreeUnreadable (msg e))

  , csBlob = \oid ->
      git ["cat-file", "blob", Text.unpack oid] <&> \case
        Right (ExitSuccess, out) -> Just out
        _                        -> Nothing
  }
  where
    -- Where git is to run, and it is not just a chdir.
    --
    -- setWorkingDir does NOT beat GIT_DIR: with one set, git reads that
    -- repository from any directory, so a caller that named a repository got
    -- another one, silently and with a plausible answer. A hook is exactly where
    -- both happen at once, since git sets GIT_DIR for every hook it runs, and a
    -- hook auditing a repository other than the one it was invoked for is the
    -- kind of wrong that reads as correct.
    --
    -- Only when a directory was named. With none, the environment is the caller's
    -- whole answer to "which repository", and a hook's GIT_DIR is then the right
    -- one to obey.
    inDir p = case cwd of
      Nothing -> pure p
      Just d -> do
        env0 <- liftIO getEnvironment
        pure (setEnv (List.filter (not . override . fst) env0) (setWorkingDir d p))

    -- The variables that name a repository, or part of one, from outside it.
    -- GIT_CONFIG_* are not here: a config that turns off gpg signing or sets
    -- safe.directory is the caller's business and this only reads.
    override k = k `elem` [ "GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE"
                          , "GIT_OBJECT_DIRECTORY", "GIT_COMMON_DIR"
                          , "GIT_ALTERNATE_OBJECT_DIRECTORIES"
                          ]

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

    -- The listing, read to 'maxListingBytes' and no further: @Left n@ is "there
    -- was more than n", and the process is dead by the time it is returned.
    --
    -- stdout to the bound, then stderr whole, then the exit code. Sequential and
    -- so deadlockable in principle, if git filled the stderr pipe while this was
    -- still reading stdout; it cannot here, because ls-tree's stderr is a line or
    -- two and a pipe buffer is 64 KiB. Anything with real output on both is a
    -- reason to go back to 'raw', which reads them concurrently.
    listing args = do
      cfg <- inDir (proc "git" args)
      let piped = setStdin closed (setStdout createPipe (setStderr createPipe cfg))
      try @_ @SomeException
        ( withProcessTerm piped $ \p -> do
            out <- upTo maxListingBytes (getStdout p)
            case out of
              Left n -> pure (ExitFailure 1, Left n)
              Right bs -> do
                err  <- liftIO (BS.hGetContents (getStderr p))
                code <- waitExitCode p
                pure (code, Right (if code == ExitSuccess then bs else err))
        ) <&> either (Left . Text.pack . show) Right

    -- Read a handle to a bound. Stops at the first chunk that crosses it and does
    -- not read the rest, which is the whole point.
    upTo n h = go 0 []
      where
        go seen chunks = do
          c <- liftIO (BS.hGetSome h 65536)
          if BS.null c
            then pure (Right (BS.concat (List.reverse chunks)))
            else do
              let seen' = seen + BS.length c
              if seen' > n then pure (Left seen')
                           else go seen' (c : chunks)

    -- Raw output. The listing is bytes: decoding it first merged two paths that
    -- differ only in an invalid byte, after which the same blob was read for both.
    --
    -- The whole of stderr, not its first line. On a repository git considers
    -- dubiously owned, the remedy (the safe.directory command to run) is on the
    -- line AFTER the complaint, so keeping one line kept the half without the fix
    -- in it while claiming stderr was what a human needs.
    raw args = do
      cfg <- inDir (proc "git" args)
      try @_ @SomeException (readProcess (setStdin closed cfg))
        <&> \case
          Left e -> Left (Text.pack (show e))
          Right (code, out, errOut) ->
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
