-- | What bounds the work @hbs2-hub serve@ does for a request (PEP-25): how
-- many verbs run at once, how many wait for one, and how long an answer is
-- reused.
--
-- A public server runs verbs on the operator's machine for anyone who asks,
-- and one page asks for six things at once. Refusing the fifth outright left
-- half a page blank; waiting without a bound would let a stranger hold every
-- connection the server has. So: a few at once, a queue of a fixed depth, a
-- wait of a fixed length, and past either an answer that says try later.
module HBS2.Hub.Serve.Limits
  ( Gate
  , newGate
  , withGate
  , Refused(..)
  , AnswerCache
  , newAnswerCache
  , cachedAnswer
  , cacheKey
  , maxCachedAnswers
  ) where

import Control.Concurrent.STM
import Control.Exception qualified as E
import Control.Monad (join)
import Data.Aeson (Value,(.=),object)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Data.Time.Clock (UTCTime,getCurrentTime,addUTCTime,NominalDiffTime)

-- | Why a call was not run.
data Refused
  = QueueFull   -- ^ every slot taken and the queue at its depth
  | WaitedOut   -- ^ queued, and no slot came free in time
  deriving stock (Eq,Show)

data Gate = Gate
  { gBusy    :: Int
  , gDepth   :: Int
  , gWait    :: Int           -- ^ microseconds a queued call waits at most
  , gRunning :: TVar Int
  , gWaiting :: TVar Int
  }

-- | At most @busy@ at once, at most @depth@ waiting, each for at most @secs@.
newGate :: Int -> Int -> Int -> IO Gate
newGate busy depth secs = Gate (max 1 busy) (max 0 depth) (max 0 secs * 1000000) <$> newTVarIO 0 <*> newTVarIO 0

-- | Run the action in a slot, waiting in the queue for one if need be.
withGate :: Gate -> IO a -> IO (Either Refused a)
withGate g act = do
  admitted <- join $ atomically $ do
    r <- readTVar (gRunning g)
    w <- readTVar (gWaiting g)
    if r < gBusy g then writeTVar (gRunning g) (r + 1) >> pure (pure (Right ()))
    else if w >= gDepth g then pure (pure (Left QueueFull))
    else do
      writeTVar (gWaiting g) (w + 1)
      pure (queued g)
  case admitted of
    Left why -> pure (Left why)
    Right () -> Right <$> (act `E.finally` atomically (modifyTVar' (gRunning g) (subtract 1)))
  where
    -- Out of the queue either way, and into a slot only if one came free
    -- before the wait ran out. Taken in the same transaction that leaves the
    -- queue, so a slot freed while two wait goes to exactly one of them.
    queued g' = do
      timer <- registerDelay (gWait g')
      atomically $ do
        r <- readTVar (gRunning g')
        late <- readTVar timer
        if r < gBusy g' then do
          writeTVar (gRunning g') (r + 1)
          modifyTVar' (gWaiting g') (subtract 1)
          pure (Right ())
        else if late then do
          modifyTVar' (gWaiting g') (subtract 1)
          pure (Left WaitedOut)
        else retry

-- | Answers by call, each kept for a while; and calls in flight, so the same
-- question asked twice at once is answered by one run.
data AnswerCache e = AnswerCache
  { acFor     :: NominalDiffTime
  , acEntries :: TVar (HM.HashMap ByteString (Entry e))
  }

data Entry e
  = Pending (TMVar (Maybe (Either e Value)))
  | Done UTCTime Value

-- | The most answers kept. Past it the stale ones go, and if that is not
-- enough every finished one: a stranger choosing arguments chooses keys.
maxCachedAnswers :: Int
maxCachedAnswers = 2048

newAnswerCache :: Int -> IO (AnswerCache e)
newAnswerCache secs = AnswerCache (fromIntegral (max 0 secs)) <$> newTVarIO HM.empty

-- | The key of a call: its tool and its arguments, as bytes. Arguments are a
-- key map, which encodes in key order, so the same call is the same key.
cacheKey :: Text -> KM.KeyMap Value -> ByteString
cacheKey tool args = LBS.toStrict (Aeson.encode (object ["tool" .= tool, "args" .= args]))

-- | The answer the cache holds for this key, the one a run in flight gives,
-- or a run of its own. Only an answer is kept; a refusal is given to the
-- calls that waited on it and forgotten.
cachedAnswer :: AnswerCache e -> ByteString -> IO (Either e Value) -> IO (Either e Value)
cachedAnswer ac key run
  | acFor ac <= 0 = run
  | otherwise = do
      now <- getCurrentTime
      fresh <- newEmptyTMVarIO
      found <- atomically $ do
        m <- readTVar (acEntries ac)
        case HM.lookup key m of
          Just (Done t v) | t > now -> pure (Left (Right v))
          Just (Pending w) -> pure (Left (Left w))
          _ -> do
            writeTVar (acEntries ac) (HM.insert key (Pending fresh) (prune now m))
            pure (Right ())
      case found of
        Left (Right v) -> pure (Right v)
        Left (Left w) -> atomically (readTMVar w) >>= maybe run pure
        Right () -> do
          r <- run `E.onException` settle fresh Nothing
          settle fresh (Just r)
          pure r
  where
    settle w r = do
      now <- getCurrentTime
      atomically $ do
        modifyTVar' (acEntries ac) $ \m -> case r of
          Just (Right v) -> HM.insert key (Done (addUTCTime (acFor ac) now) v) m
          _ -> HM.delete key m
        -- Whoever waited gets this answer. On an exception the run's own
        -- caller sees it, and the others are told to run the call themselves
        -- rather than left waiting on an answer that is not coming.
        putTMVar w r
    prune now m
      | HM.size m < maxCachedAnswers = m
      | otherwise =
          let live = HM.filter (\case { Done t _ -> t > now ; Pending _ -> True }) m
          in if HM.size live < maxCachedAnswers then live else HM.filter (\case { Pending _ -> True ; _ -> False }) live
