module TestStateRecovery
  ( testStateFollowsTheData
  , testCrashKeepsSyncedBlocks
  , testOldStateKeepsNewerData
  , testTornTailIsKeptAside
  , testUnloadableStateIsKeptAside
  , testMissingPendingFileDoesNotStopStart
  ) where

import HBS2.Data.Types.Refs (HashRef)
import HBS2.Storage.NCQ.Types (DataFile(..), StateFile(..), ToFileName(..))
import HBS2.Storage.NCQ3.Internal (ncqStorageOpen, ncqPutBlock)
import HBS2.Storage.NCQ3.Internal.Class (ncqStorageGetBlock)
import HBS2.Storage.NCQ3.Internal.Files (ncqListFilesBy, ncqGetWorkDir, ncqGetFileName)
import HBS2.Storage.NCQ3.Internal.Run (ncqStorageRun, ncqStorageStop)
import HBS2.Storage.NCQ3.Internal.State (readStateMay)
import HBS2.Storage.NCQ3.Internal.Types

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.HashSet qualified as HS
import Data.Function (fix)
import Data.List qualified as List
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty.HUnit

-- Service loops on a short leash, so that the sweep, the merges and the index
-- compaction all get to run within a test.
tune :: Int -> NCQStorage -> NCQStorage
tune minLog s = s { ncqPostponeService = 1
                  , ncqSweepTime       = 1
                  , ncqMergeTimeA      = 1
                  , ncqMergeTimeB      = 2
                  , ncqCompactTimeA    = 1
                  , ncqCompactTimeB    = 2
                  , ncqMinLog          = minLog
                  }

bigLog, smallLog :: Int
bigLog   = 512 * 1024 * 1024
smallLog = 64 * 1024

withNCQ :: Int -> FilePath -> (NCQStorage -> IO a) -> IO a
withNCQ minLog dir act = do
  sto <- ncqStorageOpen dir (tune minLog)
  withAsync (ncqStorageRun sto) $ \w -> do
    link w
    r <- act sto
    ncqStorageStop sto
    wait w
    pure r

-- | Within a time limit, or the test fails instead of hanging.
limited :: Int -> IO a -> IO a
limited secs m = timeout (secs * 1_000_000) m >>= maybe (assertFailure "timed out") pure

blockN :: String -> Int -> LBS.ByteString
blockN tag i = LBS.fromStrict $ BS8.pack (tag <> "-" <> show i <> "-") <> BS.replicate (512 + (i * 37) `mod` 1500) (fromIntegral i)

putBatch :: NCQStorage -> String -> Int -> IO [(HashRef, LBS.ByteString)]
putBatch sto tag n = forM [1..n] $ \i -> do
  let b = blockN tag i
  h <- ncqPutBlock sto b >>= maybe (assertFailure "put failed") pure
  pure (h, b)

-- | Everything put so far is written and covered by a sync record.
syncAll :: NCQStorage -> IO ()
syncAll sto = do
  atomically $ readTVar (ncqWriteQ sto) >>= check . Seq.null
  n0 <- readTVarIO (ncqSyncNo sto)
  atomically $ writeTVar (ncqSyncReq sto) True
  atomically $ readTVar (ncqSyncNo sto) >>= check . (> n0)

missingOf :: NCQStorage -> [(HashRef, LBS.ByteString)] -> IO Int
missingOf sto bs = fmap (length . filter not) $ forM bs $ \(h, b) -> do
  got <- ncqStorageGetBlock sto h
  pure (got == Just (LBS.toStrict b))

assertAllThere :: String -> NCQStorage -> [(HashRef, LBS.ByteString)] -> IO ()
assertAllThere what sto bs = do
  n <- missingOf sto bs
  assertEqual (what <> ": blocks missing out of " <> show (length bs)) 0 n

-- | What a kill -9 at this moment leaves on disk: the files as they are,
--   with no finalizer run. Copied while the storage keeps running.
crashCopy :: FilePath -> FilePath -> IO ()
crashCopy from to = do
  createDirectoryIfMissing True to
  es <- listDirectory from
  forM_ es $ \e -> do
    let a = from </> e
    let b = to </> e
    isDir <- doesDirectoryExist a
    if isDir then crashCopy a b else do
      here <- doesFileExist a
      when here $ copyFileWithMetadata a b

stateFilesOf :: NCQStorage -> IO [FileKey]
stateFilesOf sto = ncqListFilesBy sto (List.isPrefixOf "s-") <&> fmap snd

latestState :: NCQStorage -> IO (Maybe NCQState)
latestState sto = do
  ks <- stateFilesOf sto
  if null ks then pure Nothing else readStateMay sto (maximum ks)

quarantined :: NCQStorage -> IO [FilePath]
quarantined sto = do
  let d = ncqGetWorkDir sto </> ncqQuarantineDirName
  here <- doesDirectoryExist d
  if here then listDirectory d else pure []

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- | The state on disk follows the data while the storage runs: once a data
--   file is synced, a state that knows it (with its synced size, or indexed)
--   appears, without a restart or a stop.
testStateFollowsTheData :: Assertion
testStateFollowsTheData = withSystemTempDirectory "ncq3-state" $ \dir -> limited 60 do
  withNCQ bigLog dir $ \sto -> do
    void $ putBatch sto "a" 200
    syncAll sto
    ok <- flip fix (50 :: Int) $ \again n -> do
      yes <- latestState sto <&> maybe False knowsSyncedData
      if yes || n <= 0 then pure yes else threadDelay 100_000 >> again (pred n)
    assertBool "no state on disk knows the synced data file" ok
  where
    knowsSyncedData s = not (HS.null (ncqStateFiles s))
                     || or [ sz > 0 | P (PData _ sz) <- Set.toList (ncqStateFacts s) ]

-- | Crash right after a sync, restart from what is on disk, work, crash again:
--   every synced block of every session is still there, also after the
--   sweep has had time to run.
testCrashKeepsSyncedBlocks :: Assertion
testCrashKeepsSyncedBlocks = withSystemTempDirectory "ncq3-crash" $ \tmp -> limited 120 do
  let a = tmp </> "a"
  let b = tmp </> "b"
  let c = tmp </> "c"

  batch1 <- withNCQ bigLog a $ \sto -> do
    bs <- putBatch sto "one" 300
    syncAll sto
    crashCopy a b
    pure bs

  batch2 <- withNCQ bigLog b $ \sto -> do
    assertAllThere "after the first crash" sto batch1
    threadDelay 3_000_000 -- sweep, merges
    assertAllThere "after the first crash and a sweep" sto batch1
    bs <- putBatch sto "two" 300
    syncAll sto
    crashCopy b c
    pure bs

  withNCQ bigLog c $ \sto -> do
    assertAllThere "after the second crash (session 1)" sto batch1
    assertAllThere "after the second crash (session 2)" sto batch2
    threadDelay 3_000_000
    assertAllThere "after the second crash and a sweep (session 1)" sto batch1
    assertAllThere "after the second crash and a sweep (session 2)" sto batch2

-- | The situation found on the node: the newest state on disk is older than
--   the data files next to it. Nothing written after that state may be lost,
--   and the files must survive the sweep.
testOldStateKeepsNewerData :: Assertion
testOldStateKeepsNewerData = withSystemTempDirectory "ncq3-old-state" $ \dir -> limited 120 do

  (batch1, saved) <- withNCQ smallLog dir $ \sto -> do
    bs <- putBatch sto "one" 400
    syncAll sto
    threadDelay 2_000_000
    pure (bs, sto)

  -- the state as it was after session 1
  oldStates <- do
    ks <- stateFilesOf saved
    forM ks $ \k -> do
      let fn = ncqGetFileName saved (StateFile k)
      s <- BS.readFile fn
      pure (fn, s)

  batch2 <- withNCQ smallLog dir $ \sto -> do
    bs <- putBatch sto "two" 400
    syncAll sto
    threadDelay 2_000_000
    pure bs

  -- roll the state back, keep the data
  do
    ks <- stateFilesOf saved
    forM_ ks $ \k -> removeFile (ncqGetFileName saved (StateFile k))
    forM_ oldStates $ \(fn, s) -> BS.writeFile fn s

  withNCQ smallLog dir $ \sto -> do
    assertAllThere "older session" sto batch1
    assertAllThere "newer session" sto batch2
    threadDelay 4_000_000 -- sweep, merges, compaction
    assertAllThere "older session, after the sweep" sto batch1
    assertAllThere "newer session, after the sweep" sto batch2

  withNCQ smallLog dir $ \sto -> do
    assertAllThere "older session, one more restart" sto batch1
    assertAllThere "newer session, one more restart" sto batch2

-- | A data file that ends in bytes written after its last sync record is cut
--   back to that record on start, and the cut-off bytes are kept.
testTornTailIsKeptAside :: Assertion
testTornTailIsKeptAside = withSystemTempDirectory "ncq3-tail" $ \dir -> limited 60 do
  (batch1, sto0) <- withNCQ bigLog dir $ \sto -> do
    bs <- putBatch sto "one" 200
    syncAll sto
    pure (bs, sto)

  fs <- ncqListFilesBy sto0 (\f -> "f-" `List.isPrefixOf` f && ".data" `List.isSuffixOf` f)
  let newest = ncqGetFileName sto0 (DataFile (maximum (fmap snd fs)))
  let junk = BS.replicate 777 0xAB
  BS.appendFile newest junk

  withNCQ bigLog dir $ \sto -> do
    assertAllThere "after a torn tail" sto batch1
    q <- quarantined sto
    let tails = [ f | f <- q, ".tail-" `List.isInfixOf` f ]
    assertBool ("no tail kept in quarantine: " <> show q) (not (null tails))
    kept <- BS.readFile (ncqGetWorkDir sto </> ncqQuarantineDirName </> head tails)
    assertEqual "the kept tail is what was cut off" junk kept

-- | A newer state file that cannot be loaded is moved aside, not deleted,
--   and the storage starts from the one before it.
testUnloadableStateIsKeptAside :: Assertion
testUnloadableStateIsKeptAside = withSystemTempDirectory "ncq3-bad-state" $ \dir -> limited 60 do
  (batch1, sto0) <- withNCQ bigLog dir $ \sto -> do
    bs <- putBatch sto "one" 200
    pure (bs, sto)

  -- names an index that does not exist
  let bogus = ncqGetFileName sto0 (StateFile (FileKey 0xFFFFFF))
  writeFile bogus "i 4294967 1700000000\nn 4294968\n"

  q1 <- withNCQ bigLog dir $ \sto -> do
    assertAllThere "after a bad newer state" sto batch1
    q <- quarantined sto
    let name = toFileName (StateFile (FileKey 0xFFFFFF))
    assertBool ("bad state not in quarantine: " <> show q)
               (any (name `List.isPrefixOf`) q)
    pure q

  -- what is in quarantine stays out of the storage's way
  withNCQ bigLog dir $ \sto -> do
    assertAllThere "one more restart" sto batch1
    q2 <- quarantined sto
    assertEqual "quarantine changed on a clean restart" (List.sort q1) (List.sort q2)

-- | A state whose pending fact names a data file that is not there (a crash
--   between the dump and the file) does not stop the storage from starting.
testMissingPendingFileDoesNotStopStart :: Assertion
testMissingPendingFileDoesNotStopStart = withSystemTempDirectory "ncq3-missing" $ \dir -> limited 60 do
  (batch1, sto0) <- withNCQ bigLog dir $ \sto -> do
    bs <- putBatch sto "one" 200
    pure (bs, sto)

  ks <- stateFilesOf sto0
  let latest = ncqGetFileName sto0 (StateFile (maximum ks))
  s <- BS.readFile latest
  let newer = ncqGetFileName sto0 (StateFile (FileKey 0xFFFFFF))
  BS.writeFile newer (s <> "\nfp 4294000 4096\n")

  withNCQ bigLog dir $ \sto -> do
    assertAllThere "after a fact for a missing file" sto batch1
