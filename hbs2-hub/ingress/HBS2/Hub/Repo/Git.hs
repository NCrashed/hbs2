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
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
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
      isRepo <- git ["rev-parse", "--git-dir"]
      case isRepo of
        Left e -> pure (Left (NoRepository e))
        Right (ExitFailure _, e) -> pure (Left (NoRepository e))
        Right (ExitSuccess, _) -> do
          here <- git ["show-ref", "--verify", "--quiet", Text.unpack metaRef]
          case here of
            Left e -> pure (Left (NoRepository e))
            Right (ExitFailure 1, _) -> pure (Left NoCanonRef)
            Right (ExitFailure _, e) -> pure (Left (RefUnresolved e))
            Right (ExitSuccess, _) ->
              git ["rev-parse", "--verify", Text.unpack metaRef <> "^{commit}"] <&> \case
                Left e -> Left (NoRepository e)
                Right (ExitSuccess, out) -> Right (Text.strip out)
                Right (_, e) -> Left (RefUnresolved e)

  , csEntries = \commit ->
      -- --full-tree, and this is not a nicety: ls-tree resolves paths relative to
      -- the current directory, so run from a subdirectory an audit listed nothing
      -- and reported clean empty canon. -l for the size and the object id, so a
      -- bound can refuse a blob unfetched and a blob can be fetched without
      -- naming a path. -z because a path may hold any byte but NUL.
      raw ["ls-tree", "-r", "-z", "-l", "--full-tree", Text.unpack commit]
        <&> \case
          Right (ExitSuccess, out) -> Right (parseListing (LBS.toStrict out))
          Right (_, e) -> Left (TreeUnreadable (decode e))
          Left e -> Left (TreeUnreadable e)

  , csBlob = \oid ->
      git ["cat-file", "blob", Text.unpack oid] <&> \case
        Right (ExitSuccess, out) -> Just out
        _                        -> Nothing
  }
  where
    inDir = maybe id setWorkingDir cwd

    -- Lenient, so a blob that is not UTF-8 becomes replacement characters rather
    -- than an exception. It costs up to three bytes per invalid one, which the
    -- per-file bound does not account for since git reports the encoded size; the
    -- tree-wide bound does, being the sum of those same sizes.
    decode = Text.decodeUtf8Lenient . LBS.toStrict

    -- Decoded output, for the calls whose result is text.
    git args = raw args <&> fmap (fmap decode)

    -- Raw output. The listing is bytes: decoding it first merged two paths that
    -- differ only in an invalid byte, after which the same blob was read for both.
    --
    -- The whole of stderr, not its first line. On a repository git considers
    -- dubiously owned, the remedy (the safe.directory command to run) is on the
    -- line AFTER the complaint, so keeping one line kept the half without the fix
    -- in it while claiming stderr was what a human needs.
    raw args =
      try @_ @SomeException (readProcess (setStdin closed (inDir (proc "git" args))))
        <&> \case
          Left e -> Left (Text.pack (show e))
          Right (code, out, errOut) ->
            Right (code, if code == ExitSuccess then out else errOut)

-- | Parse the output of @ls-tree -r -z -l@.
--
-- Each record is @\<mode> SP \<type> SP \<oid> SP* \<size> TAB \<path>@, NUL
-- terminated, and the size is @-@ for anything that is not a blob.
--
-- A record this cannot parse keeps whatever path it could recover and loses its
-- blob; one it cannot even split becomes an entry with an empty path. It is never
-- dropped, and that is the point. Dropping was the behaviour, and it is the one
-- failure mode with no alarm: if this format ever shifts, every record becomes
-- unparsable, and a reader that drops them prints a clean empty audit instead of
-- saying it can no longer read a tree.
parseListing :: ByteString -> [TreeEntry]
parseListing = fmap entry . filter (not . B8.null) . B8.split '\0'
  where
    entry rec = case B8.break (== '\t') rec of
      (_, rest) | B8.null rest -> TreeEntry B8.empty Nothing
      (meta, rest) ->
        TreeEntry (B8.drop 1 rest) $ case B8.words meta of
          [_, ty, oid, sz] | ty == "blob" ->
            (,) (Text.decodeUtf8Lenient oid) <$> readMay (B8.unpack sz)
          _ -> Nothing
