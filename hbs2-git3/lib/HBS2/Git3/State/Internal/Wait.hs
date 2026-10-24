-- | How long a command waits for a repository, and what it says when it gives up.
--
-- A repository is found through its lwwref, and an lwwref nobody holds looks
-- exactly like one that has not arrived yet: the peer answers "no value" in
-- both cases. So the only way a wait can end for a repository that does not
-- exist is a deadline, and the only useful thing to do at the deadline is to
-- say which of the two it probably is and what to do about it. Before this,
-- `git push` to a repository whose lwwref was lost waited forever and wrote
-- nothing at all.
module HBS2.Git3.State.Internal.Wait
  ( RepoWaitError(..)
  , renderRepoWaitError
  , defRepoWaitTimeout
  , repoWaitEnvVar
  , parseRepoWait
  , repoWaitFromEnv
  , pollUntil
  , pollProgress
  ) where

import HBS2.Git3.Prelude

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- | Long enough for a value that exists to arrive from a neighbour in an
-- ordinary network, where it takes a few seconds; short enough that a user
-- waiting on `git push` learns something before giving up on it.
defRepoWaitTimeout :: Timeout 'Seconds
defRepoWaitTimeout = 60

repoWaitEnvVar :: String
repoWaitEnvVar = "HBS2_GIT_REPO_WAIT"

-- | A positive number of seconds.
parseRepoWait :: String -> Maybe (Timeout 'Seconds)
parseRepoWait s = do
  x <- readMaybe @Double s
  guard (x > 0 && not (isInfinite x))
  pure (realToFrac x)

-- | The environment, because the remote helper is started by git: there is no
-- command line to put an option on, and its local config is evaluated only
-- after the repository has been found.
repoWaitFromEnv :: MonadIO m => m (Timeout 'Seconds)
repoWaitFromEnv = liftIO (lookupEnv repoWaitEnvVar)
                    <&> fromMaybe defRepoWaitTimeout . (>>= parseRepoWait)

data RepoWaitError =
    -- | The lwwref has no value.
    RepoNotFound GitRepoKey (Timeout 'Seconds)
    -- | It has one, and the manifest it points at cannot be read.
  | RepoManifestUnreadable GitRepoKey HashRef (Timeout 'Seconds)
    -- | The manifest names a reflog that has no value.
  | RepoRefLogEmpty GitRepoKey GitRemoteKey (Timeout 'Seconds)
    -- | The reflog has a value and this many of its blocks stopped arriving.
  | RepoRefLogIncomplete GitRepoKey GitRemoteKey Int (Timeout 'Seconds)
    -- | hbs2-peer took no request at all.
  | RepoPeerNotAnswering (Timeout 'Seconds)
  deriving stock (Typeable)

-- | The message IS the Show instance: the remote helper prints what it caught
-- with 'show', and the CLI dies through GHC's top-level handler, which shows.
instance Show RepoWaitError where
  show = renderRepoWaitError

instance Exception RepoWaitError where
  displayException = renderRepoWaitError

renderRepoWaitError :: RepoWaitError -> String
renderRepoWaitError = \case
  RepoNotFound k t -> unwords
    [ "repository", key k, "not found: its lwwref has no value on this peer"
    , "or its neighbours (waited", secs t <> ")."
    , longer
    ]

  RepoManifestUnreadable k h t -> unwords
    [ "repository", key k <> ": its lwwref points at", show (pretty h) <> ","
    , "but the manifest could not be read from this peer or its neighbours"
    , "(waited", secs t <> "): its blocks are missing."
    , longer
    ]

  RepoRefLogEmpty k r t -> unwords
    [ "repository", key k <> ": its reflog", key r
    , "has no value on this peer or its neighbours (waited", secs t <> ")."
    , "Either nothing was ever written to it, or it has not reached this peer yet."
    , longer
    ]

  RepoRefLogIncomplete k r n t -> unwords
    [ "repository", key k <> ":", count n, "block(s) of reflog", key r
    , "stopped arriving (no progress for", secs t <> ")."
    , longer
    ]

  RepoPeerNotAnswering t -> unwords
    [ "hbs2-peer did not answer for", secs t <> "; is it running?" ]

  where
    key :: PubKey 'Sign 'HBS2Basic -> String
    key = show . pretty . AsBase58

    secs :: Timeout 'Seconds -> String
    secs t = show (max 1 (round (realToFrac t :: Double)) :: Integer) <> "s"

    count n | n == maxBound = "some"
            | otherwise    = show n

    longer = "To wait longer, set " <> repoWaitEnvVar <> " to a number of seconds."

-- | Ask until there is an answer or the time is up.
--
-- The probe runs once more at the deadline, so a value that lands during the
-- last pause is still seen, and the answer is Nothing only after a probe that
-- started at or past the deadline found nothing. Pauses start short, since a
-- local answer is the common case, and grow to five seconds.
pollUntil :: MonadIO m => Timeout 'Seconds -> m (Maybe a) -> m (Maybe a)
pollUntil limit probe = do
  t0 <- getTimeCoarse
  let deadline = t0 + toTimeSpec limit
  flip fix 0.5 $ \next w ->
    probe >>= \case
      Just x  -> pure (Just x)
      Nothing -> do
        now <- getTimeCoarse
        if now >= deadline
          then pure Nothing
          else do
            pause (min w (toSec (deadline - now)))
            next (min 5 (w * 1.5))

-- | Like 'pollUntil' for something that arrives in parts.
--
-- The probe answers Right when it is done and Left with how much is still
-- missing. The deadline is counted from the last time that number went down,
-- so a large repository that keeps arriving is never cut off, and one that
-- stalls is given up on after the limit. Left is the last count seen.
pollProgress :: MonadIO m => Timeout 'Seconds -> m (Either Int a) -> m (Either Int a)
pollProgress limit probe = do
  t0 <- getTimeCoarse
  flip fix (0.5, Nothing, t0 + toTimeSpec limit) $ \next (w, best, deadline) ->
    probe >>= \case
      Right x -> pure (Right x)
      Left n  -> do
        now <- getTimeCoarse
        let moved     = maybe True (n <) best
            deadline' = if moved then now + toTimeSpec limit else deadline
            best'     = Just (maybe n (min n) best)
        if now >= deadline'
          then pure (Left n)
          else do
            pause (min w (toSec (deadline' - now)))
            next (min 5 (w * 1.5), best', deadline')

toSec :: TimeSpec -> Timeout 'Seconds
toSec ts = realToFrac (fromIntegral (toNanoSecs ts) / 1e9 :: Double)
