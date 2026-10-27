-- | One call of the `hub mcp` tool table, for a frontend that names a
-- repository and not a directory (PEP-24, PEP-25).
--
-- The native host (a browser extension) and `hub serve` (HTTP) are two
-- transports over this: a tool, its arguments, progress on the way, and a
-- result or a refusal. What is decided here is decided once for both: which
-- tools are offered, WHICH CLONE a call runs in, and which repositories may be
-- named at all.
module HBS2.Hub.Host
  ( HostConfig(..)
  , Hub(..)
  , HostError(..)
  , hostErrorJson
  , newHub
  , hostHello
  , cacheCloneDir
  , publicTools
  ) where

import HBS2.Hub.Mcp
import HBS2.Hub.Types (hubMetaVersion,hubMetaMin)
import HBS2.Hub.CLI.Defaults (keyShaped)

import Control.Concurrent.MVar
import Control.Monad (void,when)
import Data.Aeson (Value(..),object,(.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Clock (UTCTime,getCurrentTime,diffUTCTime)
import System.Directory (createDirectoryIfMissing,doesDirectoryExist)
import System.FilePath ((</>),takeDirectory)
import System.Process.Typed

-- | What a host was started with.
data HostConfig = HostConfig
  { hcMcp      :: McpConfig    -- ^ the tool layer: binary, timeout, read-only
  , hcCache    :: FilePath     -- ^ where cache clones live, one per repository
  , hcSyncEach :: Int          -- ^ seconds between syncs of one cache clone
    -- | Whether a call may run in the working clone the user named with
    -- @git config hbs2-hub.<key>.clone@, and read the user's identity. A
    -- public server must not: what it runs is a stranger's request.
  , hcWorking  :: Bool
    -- | The repositories that may be named, or any. A public server names
    -- them, since a stranger who could name any key could make it clone and
    -- fetch anything on the network.
  , hcAllow    :: Maybe (HS.HashSet Text)
    -- | The tools offered, or every one the table has for this mode. A public
    -- server names them: reading is not the same as public. The inbox opens
    -- letters sealed to the operator's mailbox, which are nobody else's until
    -- accepted, and status and whoami describe the operator's own keys.
  , hcTools    :: Maybe (HS.HashSet Text)
  }

-- | What a public server offers: the repository's record and its code, and
-- nothing that is the operator's.
publicTools :: HS.HashSet Text
publicTools = HS.fromList
  [ "issue_list", "pr_list", "issue_show", "pr_show", "log", "verify", "maintainer_list"
  , "code_refs", "code_tree", "code_blob", "code_log", "code_commit" ]

-- | Where the cache clone of a repository lives.
cacheCloneDir :: HostConfig -> Text -> FilePath
cacheCloneDir hc key = hcCache hc </> Text.unpack key

-- | Why a call was not answered.
data HostError = HostError
  { heCode    :: Value  -- ^ the verb's exit code, when one ran and refused
  , heMeaning :: Text   -- ^ a short name: the code's meaning, or this layer's own
  , heMessage :: Text
  }

hostErrorJson :: HostError -> Value
hostErrorJson e = object ["code" .= heCode e, "meaning" .= heMeaning e, "message" .= heMessage e]

data Hub = Hub
  { hubConfig :: HostConfig
  , hubTools  :: [Tool]
    -- | Run one call: progress, the tool's name, its arguments.
  , hubCall   :: (Text -> IO ()) -> Text -> KM.KeyMap Value -> IO (Either HostError Value)
  }

-- | What a frontend asks first, so an old host and a new page can tell each
-- other apart instead of failing on a field.
hostHello :: Hub -> Value
hostHello h = object
  [ "host" .= ("hbs2-hub" :: Text)
  , "version" .= mcVersion cfg
  , "hub_meta" .= hubMetaVersion, "hub_min" .= hubMetaMin
  , "contracts" .= object [ "thread" .= (1 :: Int), "index" .= (1 :: Int)
                          , "log" .= (1 :: Int), "queue" .= (1 :: Int)
                          , "letter" .= (1 :: Int) ]
  , "read_only" .= mcReadOnly cfg ]
  where cfg = hcMcp (hubConfig h)

newHub :: HostConfig -> IO Hub
newHub hc = do
  synced <- newMVar (HM.empty :: HM.HashMap Text UTCTime)
  cloning <- newMVar ()

  let cfg = hcMcp hc
      offered = [ t | t <- toolsFor cfg, maybe True (HS.member (toolName t)) (hcTools hc) ]
      bad code meaning msg = pure (Left (HostError code meaning msg))

      call progress name as =
        case List.find ((== name) . toolName) offered of
          Nothing -> bad Null "unknown-tool" ("no such tool here: " <> name)
          Just t -> runCall progress t as

      -- WHICH CLONE: the working clone the user named for this repository,
      -- or, for a read, a cache clone this host keeps. A write never runs in
      -- the cache: canon written there would be published from a directory
      -- nobody looks at.
      runCall progress t as = do
        let repo = case KM.lookup "repo" as of
                     Just (String r) | not (Text.null r) -> Just r
                     _ -> mcRepo cfg
        dir <- case repo of
          Nothing -> pure (Right Nothing)
          -- A KEY AND NOTHING ELSE, checked before it names a directory: the
          -- value comes from a browser and becomes a path under the cache and
          -- an argument to git, so `../../x` would have been both.
          Just key | not (keyShaped (Text.unpack key)) ->
            pure (Left ("no-clone", "not a repository key: " <> Text.take 64 key))
          Just key | Just allow <- hcAllow hc, not (HS.member key allow) ->
            pure (Left ("not-served", "this server does not show " <> key))
          Just key -> do
            working <- if hcWorking hc then workingClone key else pure Nothing
            case (working, toolAccess t) of
              (Just w, _)     -> pure (Right (Just w))
              (Nothing, Writes) -> pure (Left ("no-clone", "no working clone for " <> key <> ": set one with "
                                                 <> "`git config --global hbs2-hub." <> key <> ".clone"
                                                 <> " <path>`; writes do not go to the cache" ))
              (Nothing, Reads) -> fmap Just <$> cacheClone progress key
        case dir of
          Left (meaning, why) -> bad Null meaning why
          Right d -> do
            -- THE IDENTITY `hub identity init` wrote, for a letter: the host
            -- is started by a browser and given no flags, and a letter needs
            -- an author and a sender. Read per call, so an identity made after
            -- the host started is the one used. Not for a public server.
            (au, se) <- if not (hcWorking hc) then pure (mcAuthor cfg, mcSender cfg) else
              (,) <$> maybe (globalConfig "hbs2.hub.author") (pure . Just) (mcAuthor cfg)
                  <*> maybe (globalConfig "hbs2.hub.sender") (pure . Just) (mcSender cfg)
            let cfg' = cfg { mcDir = d, mcRepo = repo, mcAuthor = au, mcSender = se }
            case planFor cfg' t as of
              Left why -> bad Null "bad-arguments" why
              Right a -> act progress cfg' a

      act progress cfg' = \case
        RunVerb r -> answer r <$> runVerb cfg' r
        Rehearse probe r -> fromMcp <$> rehearsed cfg' probe r
        WaitInbox argv known secs ->
          fromMcp <$> waitInbox cfg' (\_ _ -> progress "waiting for letters") argv known secs

      answer r o
        | oTimedOut o = Left (HostError (Number 124) "timeout" ("stopped: " <> argvShown r))
        | oCode o == 0 || (runShape r == AsAudit && oCode o == 2) = Right $ case runShape r of
            AsJson | Just v@(Object _) <- Aeson.decodeStrict (Text.encodeUtf8 (Text.strip (oOut o))) -> v
            _ -> object [ "text" .= Text.strip (oOut o), "notes" .= Text.strip (oErr o) ]
        | otherwise = Left (HostError (Number (fromIntegral (oCode o))) (exitMeaning (oCode o))
                                      (Text.strip (oErr o <> "\n" <> oOut o)))

      -- A result already shaped for MCP, reshaped for this layer.
      fromMcp = \case
        Object m ->
          let isErr = KM.lookup "isError" m == Just (Bool True)
              texts = case KM.lookup "content" m of
                Just (Array xs) -> [ s | Object b <- foldr (:) [] xs, Just (String s) <- [KM.lookup "text" b] ]
                _ -> []
          in if isErr
               then Left (HostError Null "refused" (Text.intercalate "\n" texts))
               else Right (fromMaybe (object ["text" .= Text.intercalate "\n" texts]) (KM.lookup "structuredContent" m))
        v -> Right v

      globalConfig k = do
        (code, out, _) <- readProcess (proc "git" ["config", "--global", "--get", k])
        let v = Text.strip (Text.decodeUtf8 (LBS.toStrict out))
        pure (if code == ExitSuccess && not (Text.null v) then Just v else Nothing)

      workingClone key = do
        (code, out, _) <- readProcess (proc "git" ["config", "--global", "--get", "hbs2-hub." <> Text.unpack key <> ".clone"])
        let p = Text.unpack (Text.strip (Text.decodeUtf8 (LBS.toStrict out)))
        exists <- if null p then pure False else doesDirectoryExist p
        pure (if code == ExitSuccess && exists then Just p else Nothing)

      -- Made on first use and synced at most every 'hcSyncEach' seconds, so a
      -- page that lists and then shows does not fetch twice.
      cacheClone progress key = withMVar cloning $ \_ -> do
        let dir = cacheCloneDir hc key
        have <- doesDirectoryExist dir
        made <- if have then pure (Right ()) else do
          progress ("cloning hbs23://" <> key)
          createDirectoryIfMissing True (takeDirectory dir)
          (code, _, err) <- readProcess (proc "git" ["clone", "-q", "hbs23://" <> Text.unpack key, dir])
          pure $ if code == ExitSuccess then Right ()
                 else Left ("no-clone", "git clone hbs23://" <> key <> " failed: " <> Text.strip (Text.decodeUtf8 (LBS.toStrict err)))
        case made of
          Left e -> pure (Left e)
          Right () -> do
            now <- getCurrentTime
            last' <- withMVar synced (pure . HM.lookup key)
            let stale = maybe True (\t -> diffUTCTime now t > fromIntegral (hcSyncEach hc)) last'
            when stale $ do
              progress "fetching the record"
              void $ readProcess (setWorkingDir dir (proc (mcHub cfg) ["sync", "--repo", Text.unpack key]))
              modifyMVar_ synced (pure . HM.insert key now)
            pure (Right dir)

  pure (Hub hc offered call)
