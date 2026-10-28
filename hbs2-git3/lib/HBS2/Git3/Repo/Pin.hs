-- | Remember where the user's clone of a repository is, for hbs2-hub
-- (PEP-24, PEP-25): @git config --global hbs2-hub.<key>.clone <dir>@.
--
-- The browser extension and @hbs2-hub serve@ write to a repository through
-- that clone: they need its keys and its branches, and a page knows only the
-- key. The user used to set it by hand; the remote helper knows both the key
-- and the clone, so it sets it, on a clone or on any fetch or push after.
--
-- Only when nothing names a clone yet, or what is named is gone: a second
-- clone of the same repository does not take it from the first. Never for a
-- clone that is not the user's working copy: a bare one, a linked worktree,
-- one under a temporary or a cache directory (the hub's own cache clones, a
-- test's, a throwaway in /tmp), and none at all with @HBS2_GIT_NO_HUB_PIN@.
module HBS2.Git3.Repo.Pin
  ( pinClone
  , pinnableDir
  , pinKey
  ) where

import HBS2.Git3.Prelude

import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import System.Process.Typed

-- | The config key for a repository's clone.
pinKey :: Text -> String
pinKey k = "hbs2-hub." <> Text.unpack k <> ".clone"

-- | The working tree a @GIT_DIR@ belongs to, if it is one worth naming: the
-- parent of a @.git@ directory that is under none of the excluded places.
pinnableDir :: [FilePath] -> FilePath -> Maybe FilePath
pinnableDir excluded gitDir = do
  let gd = dropTrailingPathSeparator (normalise gitDir)
  guard (takeFileName gd == ".git")
  let wt = takeDirectory gd
  guard (isAbsolute wt)
  guard (not (any (`under` wt) excluded))
  pure wt
  where
    under top dir = let t = splitDirectories (normalise top)
                    in not (List.null t) && t `List.isPrefixOf` splitDirectories (normalise dir)

-- | Name this clone for the repository, as the module says. Quiet: a pin is
-- a convenience, and nothing a fetch or a push should fail over.
pinClone :: MonadUnliftIO m => Text -> m ()
pinClone key = liftIO $ handleAny (const none) do
  off <- lookupEnv "HBS2_GIT_NO_HUB_PIN"
  gitDir <- lookupEnv "GIT_DIR"
  case (off, gitDir) of
    (Nothing, Just gd0) -> do
      gd <- canonicalizePath gd0
      tmp <- getTemporaryDirectory
      cache <- getXdgDirectory XdgCache ""
      excluded <- traverse (\d -> canonicalizePath d `catchAny` const (pure d)) [tmp, "/tmp", "/var/tmp", cache]
      for_ (pinnableDir excluded gd) \wt -> do
        (code, out, _) <- readProcess (proc "git" ["config", "--global", "--get", pinKey key])
        let named = Text.unpack (Text.strip (Text.decodeUtf8Lenient (LBS.toStrict out)))
        have <- if code == ExitSuccess && not (List.null named) then doesDirectoryExist named else pure False
        unless have $
          void $ runProcess (proc "git" ["config", "--global", pinKey key, wt]
                               & setStdin nullStream & setStdout nullStream & setStderr nullStream)
    _ -> none
