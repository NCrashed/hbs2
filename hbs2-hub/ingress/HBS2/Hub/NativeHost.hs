-- | @hbs2-hub native-host@: the forge's tools for a browser extension (PEP-24).
--
-- A browser starts this program and talks to it over stdin and stdout with
-- native messaging: each message is a 32-bit length followed by that many bytes
-- of UTF-8 JSON. What travels is the `hub mcp` tool table, run the way `hub mcp`
-- runs it -- every tool a child process of this binary, through 'planFor' and
-- 'runVerb' -- so the two front doors cannot come to disagree about a tool's
-- name, its arguments or what it returns. Only the framing, the operations and
-- one decision are this module's own: WHICH CLONE a call runs in, since a
-- browser tab names a repository and not a directory.
--
-- Operations, one JSON object per request, answered by one with the same id:
--
--   * @{"id":1,"op":"hello"}@: versions, so an old host and a new extension can
--     tell each other apart instead of failing on a field;
--   * @{"id":2,"op":"tools"}@: the tool table, as `hub mcp` lists it;
--   * @{"id":3,"op":"call","tool":"issue_list","args":{"repo":"..."}}@;
--   * @{"id":4,"op":"cancel","target":3}@: stop a read that is still running.
--
-- An answer is @{"id":3,"ok":true,"result":...}@ or @{"id":3,"ok":false,
-- "error":{"code":52,"meaning":"...","message":"..."}}@, and a long call may
-- send @{"id":3,"progress":"..."}@ first.
module HBS2.Hub.NativeHost
  ( HostConfig(..)
  , serveNative
    -- * The parts that decide something
  , readFrame
  , encodeFrame
  , maxFrameOut
  , cacheCloneDir
  , hostName
  ) where

import HBS2.Hub.Mcp
import HBS2.Hub.Types (hubMetaVersion,hubMetaMin)
import HBS2.Hub.CLI.Defaults (keyShaped)

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.MVar
import Control.Exception qualified as E
import Control.Monad (forM_,unless,void,when)
import Data.Aeson (Value(..),object,(.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Bits (shiftL,shiftR,(.&.),(.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isSpace)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Clock (UTCTime,getCurrentTime,diffUTCTime)
import Data.Word (Word32)
import System.Directory (createDirectoryIfMissing,doesDirectoryExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>),takeDirectory)
import System.IO
import System.Process.Typed

-- | What the host was started with.
data HostConfig = HostConfig
  { hcMcp      :: McpConfig    -- ^ the tool layer: binary, timeout, read-only
  , hcCache    :: FilePath     -- ^ where cache clones live, one per repository
  , hcSyncEach :: Int          -- ^ seconds between syncs of one cache clone
  }

-- | The name both browsers know this host by, in its manifest.
hostName :: Text
hostName = "app.hbs2.hub"

-- | The most a browser takes in one message from a native host: 1 MB, in
-- Chrome and in Firefox.
maxFrameOut :: Int
maxFrameOut = 1024 * 1024

-- | One message in, or Nothing at the end of input.
--
-- The length is in the machine's byte order, which the browsers define it as;
-- every platform hbs2 ships for is little-endian, so it is read as such rather
-- than through a host-order primitive that would hide the assumption.
readFrame :: Handle -> IO (Maybe BS.ByteString)
readFrame h = do
  hdr <- BS.hGet h 4
  if BS.length hdr < 4
    then pure Nothing
    else do
      let n = foldr (\b acc -> acc `shiftL` 8 .|. fromIntegral b) (0 :: Word32) (BS.unpack hdr)
      body <- BS.hGet h (fromIntegral n)
      pure (if BS.length body == fromIntegral n then Just body else Nothing)

-- | One message out: the length, little-endian, then the bytes.
encodeFrame :: LBS.ByteString -> LBS.ByteString
encodeFrame bs = LBS.pack [ fromIntegral ((n `shiftR` s) .&. 0xff) | s <- [0, 8, 16, 24] ] <> bs
  where n = fromIntegral (LBS.length bs) :: Word32

-- | Where the cache clone of a repository lives.
cacheCloneDir :: HostConfig -> Text -> FilePath
cacheCloneDir hc key = hcCache hc </> Text.unpack key

serveNative :: HostConfig -> Handle -> Handle -> IO ()
serveNative hc hin hout = do
  hSetBinaryMode hin True
  hSetBinaryMode hout True
  hSetBuffering hout (BlockBuffering Nothing)
  outLock <- newMVar ()
  calls <- newMVar (HM.empty :: HM.HashMap Value (IO (), MVar (), Bool))
  synced <- newMVar (HM.empty :: HM.HashMap Text UTCTime)
  cloning <- newMVar ()

  let cfg = hcMcp hc
      offered = toolsFor cfg

      -- Uninterruptible, as in `hub mcp`: a message cut off half way is a
      -- broken stream for every call after it.
      send v = withMVar outLock $ \_ -> E.uninterruptibleMask_ $ do
        let bytes = Aeson.encode v
        if LBS.length bytes > fromIntegral maxFrameOut
          then LBS.hPut hout (encodeFrame (Aeson.encode (tooLarge v (LBS.length bytes))))
          else LBS.hPut hout (encodeFrame bytes)
        hFlush hout

      tooLarge v n = object
        [ "id" .= idOf v, "ok" .= False
        , "error" .= object [ "code" .= Null, "meaning" .= ("too-large" :: Text)
                            , "message" .= ("the answer is " <> tshow n
                                             <> " bytes, over the browser's 1 MB for one message") ] ]

      idOf = \case { Object o -> fromMaybe Null (KM.lookup "id" o) ; _ -> Null }

      ok i r = send (object ["id" .= i, "ok" .= True, "result" .= r])
      bad i code meaning msg = send (object
        [ "id" .= i, "ok" .= False
        , "error" .= object ["code" .= code, "meaning" .= (meaning :: Text), "message" .= (msg :: Text)] ])
      progress i m = send (object ["id" .= i, "progress" .= (m :: Text)])

      request i op o = case op of
        "hello" -> ok i $ object
          [ "host" .= ("hbs2-hub" :: Text)
          , "version" .= mcVersion cfg
          , "hub_meta" .= hubMetaVersion, "hub_min" .= hubMetaMin
          , "contracts" .= object [ "thread" .= (1 :: Int), "index" .= (1 :: Int)
                                  , "log" .= (1 :: Int), "queue" .= (1 :: Int)
                                  , "letter" .= (1 :: Int) ]
          , "read_only" .= mcReadOnly cfg
          , "ops" .= (["hello","tools","call","cancel"] :: [Text]) ]
        "tools" -> ok i (object ["tools" .= fmap toolJson offered])
        "call" -> do
          let name = case KM.lookup "tool" o of { Just (String s) -> Just s ; _ -> Nothing }
              args = case KM.lookup "args" o of
                       Just (Object a) -> Right a
                       Nothing -> Right KM.empty
                       Just Null -> Right KM.empty
                       Just _ -> Left ()
          case (name >>= \n -> List.find ((== n) . toolName) offered, args) of
            (Nothing, _) -> bad i Null "unknown-tool" ("no such tool here: " <> fromMaybe "(none)" name)
            (_, Left ()) -> bad i Null "bad-arguments" "args must be an object"
            (Just t, Right as) -> spawnCall i (toolAccess t == Writes) (runCall i t as)
        "cancel" -> do
          let target = fromMaybe Null (KM.lookup "target" o)
          k <- withMVar calls (pure . HM.lookup target)
          -- A read is stopped; a write is left to finish, as in `hub mcp`.
          forM_ k $ \(stop, _, writes) -> unless writes stop
          ok i (object ["cancelled" .= maybe False (\(_, _, w) -> not w) k])
        _ -> bad i Null "unknown-op" ("no such op: " <> op)

      -- WHICH CLONE: the working clone the user named for this repository,
      -- or, for a read, a cache clone this host keeps. A write never runs in
      -- the cache: canon written there would be published from a directory
      -- nobody looks at.
      runCall i t as = do
        let repo = case KM.lookup "repo" as of
                     Just (String r) | not (Text.null r) -> Just r
                     _ -> mcRepo cfg
        dir <- case repo of
          Nothing -> pure (Right Nothing)
          -- A KEY AND NOTHING ELSE, checked before it names a directory: the
          -- value comes from a browser and becomes a path under the cache and
          -- an argument to git, so `../../x` would have been both.
          Just key | not (keyShaped (Text.unpack key)) ->
            pure (Left ("not a repository key: " <> Text.take 64 key))
          Just key -> do
            working <- workingClone key
            case (working, toolAccess t) of
              (Just w, _)     -> pure (Right (Just w))
              (Nothing, Writes) -> pure (Left ( "no working clone for " <> key <> ": set one with "
                                                 <> "`git config --global hbs2-hub." <> key <> ".clone"
                                                 <> " <path>`; writes do not go to the cache" ))
              (Nothing, Reads) -> fmap Just <$> cacheClone i key
        case dir of
          Left why -> bad i Null "no-clone" why
          Right d -> do
            let cfg' = cfg { mcDir = d, mcRepo = repo }
            case planFor cfg' t as of
              Left why -> bad i Null "bad-arguments" why
              Right a -> act i cfg' a

      act i cfg' = \case
        RunVerb r -> do
          o <- runVerb cfg' r
          answer i r o
        Rehearse probe r -> fromMcp i =<< rehearsed cfg' probe r
        WaitInbox argv known secs ->
          fromMcp i =<< waitInbox cfg' (\_ _ -> progress i "waiting for letters") argv known secs

      answer i r o
        | oTimedOut o = bad i (Number 124) "timeout" ("stopped: " <> argvShown r)
        | oCode o == 0 || (runShape r == AsAudit && oCode o == 2) = ok i $ case runShape r of
            AsJson | Just v@(Object _) <- Aeson.decodeStrict (Text.encodeUtf8 (Text.strip (oOut o))) -> v
            _ -> object [ "text" .= Text.strip (oOut o), "notes" .= Text.strip (oErr o) ]
        | otherwise = bad i (Number (fromIntegral (oCode o))) (exitMeaning (oCode o))
                        (Text.strip (oErr o <> "\n" <> oOut o))

      -- A result already shaped for MCP, reshaped for this transport.
      fromMcp i = \case
        Object m -> do
          let isErr = KM.lookup "isError" m == Just (Bool True)
              texts = case KM.lookup "content" m of
                Just (Array xs) -> [ s | Object b <- foldr (:) [] xs, Just (String s) <- [KM.lookup "text" b] ]
                _ -> []
          if isErr
            then bad i Null "refused" (Text.intercalate "\n" texts)
            else ok i (fromMaybe (object ["text" .= Text.intercalate "\n" texts]) (KM.lookup "structuredContent" m))
        v -> ok i v

      workingClone key = do
        (code, out, _) <- readProcess (proc "git" ["config", "--global", "--get", "hbs2-hub." <> Text.unpack key <> ".clone"])
        let p = Text.unpack (Text.strip (Text.decodeUtf8 (LBS.toStrict out)))
        exists <- if null p then pure False else doesDirectoryExist p
        pure (if code == ExitSuccess && exists then Just p else Nothing)

      -- Made on first use and synced at most every 'hcSyncEach' seconds, so a
      -- page that lists and then shows does not fetch twice.
      cacheClone i key = withMVar cloning $ \_ -> do
        let dir = cacheCloneDir hc key
        have <- doesDirectoryExist dir
        made <- if have then pure (Right ()) else do
          progress i ("cloning hbs23://" <> key)
          createDirectoryIfMissing True (takeDirectory dir)
          (code, _, err) <- readProcess (proc "git" ["clone", "-q", "hbs23://" <> Text.unpack key, dir])
          pure $ if code == ExitSuccess then Right ()
                 else Left ("git clone hbs23://" <> key <> " failed: " <> Text.strip (Text.decodeUtf8 (LBS.toStrict err)))
        case made of
          Left e -> pure (Left e)
          Right () -> do
            now <- getCurrentTime
            last' <- withMVar synced (pure . HM.lookup key)
            let stale = maybe True (\t -> diffUTCTime now t > fromIntegral (hcSyncEach hc)) last'
            when stale $ do
              progress i "fetching the record"
              void $ readProcess (setWorkingDir dir (proc (mcHub cfg) ["sync", "--repo", Text.unpack key]))
              modifyMVar_ synced (pure . HM.insert key now)
            pure (Right dir)

      spawnCall i writes body = do
        gate <- newEmptyMVar
        finished <- newEmptyMVar
        tid <- forkIO $ (`E.finally` putMVar finished ()) $ do
          takeMVar gate
          E.try body >>= \case
            Left (e :: E.SomeException)
              | Just E.ThreadKilled <- E.fromException e -> bad i Null "cancelled" "cancelled"
              | otherwise -> bad i Null "internal" (tshow e)
            Right () -> pure ()
          modifyMVar_ calls (pure . HM.delete i)
        let stop = do
              modifyMVar_ calls (pure . HM.delete i)
              void (forkIO (E.throwTo tid E.ThreadKilled))
        modifyMVar_ calls (pure . HM.insert i (stop, finished, writes))
        putMVar gate ()

      message bytes = case Aeson.eitherDecodeStrict bytes of
        Left e -> bad Null Null "parse-error" (Text.pack e)
        Right (Object o) -> case (KM.lookup "id" o, KM.lookup "op" o) of
          (Just i, Just (String op)) -> request i op o
          (i, _) -> bad (fromMaybe Null i) Null "invalid-request" "a request needs an id and an op"
        Right _ -> bad Null Null "invalid-request" "a request is a JSON object"

      loop = readFrame hin >>= \case
        Nothing -> pure ()
        Just bytes -> do
          unless (BS.all (isSpace . toEnum . fromIntegral) bytes) (message bytes)
          loop

      -- The browser closed the port: reads in flight are stopped, writes are
      -- left to finish within their own timeout, and both are waited for, for
      -- the reason `hub mcp` gives.
      shutdown = do
        cs <- HM.elems <$> readMVar calls
        sequence_ [ stop | (stop, _, False) <- cs ]
        let waitAll = mapM_ (\(_, f, _) -> readMVar f) cs
        void (E.try @E.SomeException (raceTimeout ((mcTimeout cfg + 10) * 1000000) waitAll))

  loop `E.finally` shutdown
  where
    tshow :: Show a => a -> Text
    tshow = Text.pack . show

    raceTimeout us act = do
      done <- newEmptyMVar
      void $ forkIO (act >> putMVar done ())
      void $ forkIO (threadDelay us >> putMVar done ())
      takeMVar done
