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
import HBS2.Hub.Host

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.MVar
import Control.Exception qualified as E
import Control.Monad (forM_,unless,void)
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
import Data.Word (Word32)
import System.IO

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

serveNative :: HostConfig -> Handle -> Handle -> IO ()
serveNative hc hin hout = do
  hSetBinaryMode hin True
  hSetBinaryMode hout True
  hSetBuffering hout (BlockBuffering Nothing)
  outLock <- newMVar ()
  calls <- newMVar (HM.empty :: HM.HashMap Value (IO (), MVar (), Bool))
  hub <- newHub hc

  let cfg = hcMcp hc
      offered = hubTools hub

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
        "hello" -> ok i $ case hostHello hub of
          Object h -> Object (KM.insert "ops" (Aeson.toJSON (["hello","tools","call","cancel"] :: [Text])) h)
          v -> v
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
            (Just t, Right as) -> spawnCall i (toolAccess t == Writes) $
              hubCall hub (progress i) (toolName t) as
                >>= either (\e -> bad i (heCode e) (heMeaning e) (heMessage e)) (ok i)
        "cancel" -> do
          let target = fromMaybe Null (KM.lookup "target" o)
          k <- withMVar calls (pure . HM.lookup target)
          -- A read is stopped; a write is left to finish, as in `hub mcp`.
          forM_ k $ \(stop, _, writes) -> unless writes stop
          ok i (object ["cancelled" .= maybe False (\(_, _, w) -> not w) k])
        _ -> bad i Null "unknown-op" ("no such op: " <> op)

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
