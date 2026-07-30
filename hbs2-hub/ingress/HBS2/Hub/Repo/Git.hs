-- | A 'CanonSource' backed by the git repository the caller is standing in.
--
-- The whole of what talks to git, and all of it is three commands. The reading
-- of what they return is in "HBS2.Hub.Repo", which is pure.
--
-- Git is run as a process rather than reached through hbs2-git3. Two reasons: the
-- ref is an ordinary git ref that PEP-19 says is fetched and pushed by the
-- mechanism code branches already use, so plain git is the honest tool for it;
-- and @gitReadTree@ there dedups its entries by object hash, which would
-- silently collapse two paths whose blobs happen to be identical.
module HBS2.Hub.Repo.Git
  ( gitCanon
  ) where

import HBS2.Hub.Repo

import HBS2.CLI.Prelude hiding (mapMaybe)

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.Exit (ExitCode(..))
import System.Process.Typed

gitCanon :: MonadUnliftIO m => CanonSource m
gitCanon = CanonSource
  { csCommit = do
      -- Three answers, not two. rev-parse exits 1 for a ref that is not there
      -- and 128 for a directory that is not a repository, and collapsing those
      -- told somebody to fetch canon into a directory where fetching is not the
      -- problem. Running git at all can also fail, which used to be an uncaught
      -- IOException.
      git ["rev-parse", "--verify", "--quiet", Text.unpack metaRef <> "^{commit}"]
        <&> \case
          Left e                -> Left (NoRepository e)
          Right (ExitFailure 1, _) -> Left NoCanonRef
          Right (ExitFailure _, e) -> Left (NoRepository (firstLine e))
          Right (ExitSuccess, out) -> Right (Text.strip out)

  , csEntries = \commit ->
      -- --full-tree, and this is not a nicety. ls-tree resolves paths relative
      -- to the CURRENT DIRECTORY while cat-file resolves them from the root, so
      -- run from a subdirectory the two disagree: an audit from anywhere but the
      -- top listed nothing under threads/ and reported a clean empty canon, and
      -- where the subdirectory happened to contain a same-named path it listed
      -- one file and read the contents of another.
      --
      -- -l for the size, so the byte bound can refuse a blob without fetching
      -- it; -z because a path in a git tree may contain anything but NUL, and
      -- splitting on newlines breaks on the first path that contains one.
      git ["ls-tree", "-r", "-z", "-l", "--full-tree", Text.unpack commit]
        <&> \case
          Right (ExitSuccess, out) -> Just (mapMaybe entry (split out))
          _                        -> Nothing

  , csBlob = \commit path ->
      git ["cat-file", "blob", Text.unpack commit <> ":" <> path]
        <&> \case
          Right (ExitSuccess, out) -> Just out
          _                        -> Nothing
  }
  where
    split = Prelude.filter (not . Text.null) . Text.split (== '\0')

    -- "<mode> SP <type> SP <object> SP* <size> TAB <path>". The size is "-" for
    -- anything that is not a blob, which is how a submodule (gitlink) or a
    -- subtree entry is skipped here rather than being handed on to fail an event
    -- parse: -r flattens subtrees, but a gitlink is still listed.
    entry t = do
      (meta, path) <- breakTab t
      case Text.words meta of
        [_, "blob", _, sz] -> TreeEntry (Text.unpack path) <$> readMay (Text.unpack sz)
        _                  -> Nothing

    breakTab t = case Text.break (== '\t') t of
      (a, b) | Text.null b -> Nothing
             | otherwise   -> Just (a, Text.drop 1 b)

    firstLine = Text.strip . Text.takeWhile (/= '\n')

    -- The exit code and stderr, rather than Maybe: which failure it was is the
    -- thing the caller above branches on, and stderr is what a human needs when
    -- the answer is "this is not a repository".
    git args = try @_ @SomeException (readProcess (setStdin closed (proc "git" args)))
                 <&> \case
                   Left e -> Left (Text.pack (show e))
                   Right (code, out, errOut) ->
                     Right (code, decode (if code == ExitSuccess then out else errOut))

    decode = Text.decodeUtf8Lenient . LBS8.toStrict
