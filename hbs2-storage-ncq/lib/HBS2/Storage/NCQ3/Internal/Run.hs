{-# Language MultiWayIf #-}
module HBS2.Storage.NCQ3.Internal.Run where

import HBS2.Storage.NCQ.Types hiding (FileKey)
import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.Memtable
import HBS2.Storage.NCQ3.Internal.Index
import HBS2.Storage.NCQ3.Internal.State
import HBS2.Storage.NCQ3.Internal.Sweep
import HBS2.Storage.NCQ3.Internal.MMapCache
import HBS2.Storage.NCQ3.Internal.Fossil
import HBS2.Storage.NCQ3.Internal.Flags
import HBS2.Storage.NCQ3.Internal.Fsync

import Control.Concurrent.STM qualified as STM
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Either
import Data.Fixed
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Vector qualified as V
import Lens.Micro.Platform (over)
import Data.Generics.Labels ()
import System.FileLock as FL
import System.Posix.Files qualified as PFS
import System.Posix.IO as PosixBase
import System.Posix.IO.ByteString as Posix
import System.Posix.Types as Posix
import System.Posix.Unistd

import Data.ByteString qualified as BS

import HBS2.Storage.NCQ3.Internal.UnixCompat (fdWriteBS, openFdCompat)

{- HLINT ignore "Eta reduce" -}

ncqStorageStop :: forall m . MonadUnliftIO m => NCQStorage -> m ()
ncqStorageStop NCQStorage{..} = do
  atomically $ writeTVar ncqStopReq True


ncqRemoveGarbage :: forall m. MonadIO m
                => NCQStorage
                -> m ()

ncqRemoveGarbage me = do
  let wd = ncqGetWorkDir me
  let garb x = List.isSuffixOf ".part" x
                || List.isSuffixOf ".cq$" x
                || List.isSuffixOf ".merge" x

  dirFiles wd <&> filter garb >>= mapM_ rm

-- | Load the state at start-up and reconcile it with what is on disk.
--
-- A STATE FILE IS A HINT, THE DATA FILES ARE THE TRUTH. Background state
-- dumps used to stop after the first one of a session (see 'ncqStateDump'),
-- so the newest state on disk could be weeks older than the data. Start-up
-- then took that state, deleted every other state file, and the sweep deleted
-- every data and index file the state did not mention: all data written since
-- that state. Now:
--
--   * the newest state (by key) that passes 'checkState' is loaded, as before;
--   * newer state files that could not be loaded go to quarantine, not away;
--   * older state files are left to 'ncqSweepObsoleteStates', which removes
--     them only once a newer state is on disk;
--   * the file sequence is moved past every key already on disk, so no new
--     file reuses the name of an old one;
--   * a data file the state does not know is adopted: it is checked, its
--     unsynced tail (if any) is cut off and kept in quarantine, and it is
--     indexed with its own mtime, so what was written later still wins;
--   * an index file the state does not know goes to quarantine (its data file
--     is indexed again if it is not known either);
--   * nothing is deleted.
ncqTryLoadState :: forall m. MonadUnliftIO m
                => NCQStorage
                -> m ()

ncqTryLoadState me@NCQStorage{..} = do

  debug "ncqTryLoadState"

  stateFiles <- ncqListFilesBy me ( List.isPrefixOf "s-" )
                  <&> List.sortOn ( Down . snd )

  (rejected, new@NCQState{..}) <- flip fix ([], stateFiles) $ \next -> \case
    (l, []) -> pure (l, ncqState0)
    (l, (_,s):ss) -> do
      ms <- try @_ @SomeException (readStateMay me s) <&> either (const Nothing) id
      case ms of
        Nothing -> next (s : l, ss)
        Just ns -> do
          ok <- checkState ns
          debug $ "state status" <+> pretty s <+> pretty ok
          if ok then pure (l, ns) else next (s : l, ss)

  for_ rejected $ \s -> do
    warn $ red "state rejected" <+> pretty (toFileName (StateFile s))
    void $ ncqQuarantineFile me (ncqGetFileName me (StateFile s))

  atomically $ modifyTVar ncqState (<> new)

  -- never hand out a key some file already has
  onDisk <- ncqListFilesBy me (\f -> any (`List.isPrefixOf` f) ["f-","i-","s-"])
  let keys = [ k | (_,k) <- onDisk, k /= ncqNullStateKey ]
  unless (List.null keys) do
    let next = succ (maximum keys)
    atomically $ modifyTVar ncqState (over #ncqStateFileSeq (max next))

  -- indexes the state does not know
  let knownIdx = HS.fromList (fmap snd ncqStateIndex)
  idxOnDisk <- ncqListFilesBy me (\f -> List.isPrefixOf "i-" f && List.isSuffixOf ".cq" f)
  for_ idxOnDisk $ \(_,k) -> when (k /= ncqNullStateKey && not (HS.member k knownIdx)) do
    warn $ red "index not in state" <+> pretty (toFileName (IndexFile k))
    void $ ncqQuarantineFile me (ncqGetFileName me (IndexFile k))

  -- data files the state does not know
  let knownData = ncqStateFiles <> HS.fromList [ k | P (PData (DataFile k) _) <- Set.toList ncqStateFacts ]
  dataOnDisk <- ncqListFilesBy me (\f -> List.isPrefixOf "f-" f && List.isSuffixOf ".data" f)
  orphans <- for [ (ts,k) | (ts,k) <- dataOnDisk, k /= ncqNullStateKey, not (HS.member k knownData) ] $ \(ts,k) -> do
    warn $ red "data file not in state, adopting" <+> pretty (toFileName (DataFile k))
    ncqStateUpdate me $ ncqStateAddFact (P (PData (DataFile k) 0))
    pure (DataFile k, 0, Just ts)

  let pending = [ (d, s, Nothing) | P (PData d s) <- Set.toList ncqStateFacts ] <> orphans

  for_ pending $ \(dataFile, s, mts) -> ncqRecoverDataFile me dataFile s mts

  where

    checkState NCQState{..} = flip runContT pure $ callCC \exit -> do

      for_ ncqStateFiles $ \fk -> do

        let dataFile = ncqGetFileName me (DataFile fk)
        here <- doesFileExist dataFile

        unless here $ exit False

      for_ ncqStateIndex $ \(_,fk) -> do

        let idxFile = ncqGetFileName me (IndexFile fk)
        here <- doesFileExist idxFile

        unless here do
          err $ red "missed index in state" <+> pretty idxFile
          exit False

      pure True

-- | Bring a data file that is not indexed yet (a pending fact of the state,
--   or a file the state did not know) into the index.
--
-- @s@ is the size the state last recorded as synced, @mts@ the timestamp to
-- index it with ('Nothing': now).
--
-- A FILE WHOSE END IS NOT A SYNC RECORD is cut back to the last one, but the
-- bytes cut off are first copied into quarantine. A file in which nothing can
-- be vouched for is moved to quarantine whole. In no case is data deleted.
ncqRecoverDataFile :: forall m . MonadUnliftIO m
                   => NCQStorage
                   -> DataFile FileKey
                   -> Word64
                   -> Maybe POSIXTime
                   -> m ()
ncqRecoverDataFile me dataFile s mts = flip runContT pure $ callCC \exit -> do

  let path = ncqGetFileName me dataFile
  let dropFact = ncqStateUpdate me (ncqStateDelFact (P (PData dataFile 0)))

  here <- doesFileExist path

  unless here do
    warn $ red "pending data file is missing" <+> pretty (takeFileName path)
    dropFact
    exit ()

  realSize <- fileSize path

  when (realSize == 0) do
    -- an empty file the last session opened; the empty fossil sweep drops it
    exit ()

  let fastCheck = lift (try @_ @SomeException (ncqFileFastCheck path)) <&> isRight

  good <- fastCheck

  when good do
    if realSize <= typicalFileTailRecordLen then do
      warn $ "skip indexing" <+> pretty realSize <+> pretty (takeFileName path)
    else do
      debug $ "indexing" <+> pretty dataFile
      void $ lift $ ncqIndexFile me mts dataFile
    exit ()

  o <- lift (try @_ @SomeException (ncqFileTryRecover path)) <&> either (const 0) fromIntegral

  let lo = min realSize o
  let hi = min realSize (max (fromIntegral s) lo)

  warn $ "ncqFileTryRecover" <+> pretty (takeFileName path)
           <+> "size" <+> pretty realSize
           <+> "synced" <+> pretty s
           <+> "recovered" <+> pretty o

  when (hi == 0) do
    void $ ncqQuarantineFile me path
    dropFact
    exit ()

  void $ ncqQuarantineTail me path lo

  for_ (List.nub [hi, lo]) $ \cut -> when (cut > 0) do
    warn $ red "trim" <+> pretty (takeFileName path) <+> pretty realSize <+> "=>" <+> pretty cut
    liftIO $ PFS.setFileSize path (fromIntegral cut)
    ok <- fastCheck
    when ok do
      ncqSyncPath path
      void $ lift $ ncqIndexFile me mts dataFile
      exit ()

  void $ ncqQuarantineFile me path
  dropFact


ncqStorageRun :: forall m . MonadUnliftIO m
               => NCQStorage
               -> m ()
ncqStorageRun ncq@NCQStorage{..} = withSem ncqRunSem $ flip runContT pure do

  debug "ncqStorageRun"

  liftIO (FL.tryLockFile (ncqGetFileName ncq ".lock") Exclusive)
    >>= orThrow NCQStorageCurrentAlreadyOpen
    >>= atomically . writeTVar ncqFileLock . Just

  ContT $ bracket setAlive (const unsetAlive)

  ContT $ bracket none $ const $ liftIO do
    readTVarIO ncqFileLock >>= mapM_  FL.unlockFile

  ContT $ bracket none $ const $ liftIO do
    void $ ncqStateDump ncq
    debug "storage done"

  ncqRemoveGarbage ncq

  liftIO (ncqTryLoadState ncq)

  indexQ <- liftIO newTQueueIO

  indexer <- spawnActivity $ liftIO $ fix \loop -> do
    what <- atomically do
      tryReadTQueue indexQ >>= \case
        Just e  -> pure $ Just e
        Nothing -> do
          stop  <- readTVar ncqStopReq
          if not stop then STM.retry else pure Nothing

    maybe1 what none $ \(fk :: FileKey) -> do
      ncqIndexFile ncq Nothing (DataFile fk)
      dropReplaces fk
      atomically $ modifyTVar ncqCurrentFossils (HS.delete fk)
      loop

  let shLast = V.length ncqWriteOps - 1
  spawnActivity $ pooledForConcurrentlyN_ (V.length ncqWriteOps) [0..shLast] $ \i -> do
    let q = ncqWriteOps ! i
    forever (liftIO $ join $ atomically (readTQueue q))

  replicateM_ ncqReadThreads $ spawnActivity $ forever $ flip runContT pure $ callCC \exit -> do

      (h, answ) <- atomically $ readTQueue ncqReadReq
      let answer l = atomically (putTMVar answ l)

      -- debug $ "REQ" <+> pretty h

      atomically (ncqLookupEntrySTM ncq h) >>= \case
        Nothing  -> none
        Just (_, EntryHere bs)   -> answer (Just (InMemory bs)) >> exit ()
        Just (_, EntryThere loc) -> answer (Just $ InFossil loc) >> exit ()

      ContT $ ncqWithState ncq

      -- debug $ "REQ IN STATE" <+> pretty h

      NCQState{..} <- readTVarIO ncqState

      for_ ncqStateIndex $ \(_, fk) -> do
       -- debug $ "SCAN FUCKING INDEX" <+> pretty fk
       CachedIndex bs nw <- lift $ ncqGetCachedIndex ncq fk
       lift (ncqLookupIndex h (bs, nw)) >>= \case
        Just (IndexEntry fk o s) -> answer (Just (InFossil (FileLocation fk o s))) >> exit ()
        Nothing -> none

      -- debug $ "NOT FOUND SHIT" <+> pretty h
      answer Nothing >> exit ()

  spawnActivity measureWPS

  spawnActivity (ncqStateUpdateLoop ncq)

  spawnActivity $ flip runContT pure $ callCC \exit -> do

    unless ncqAuditEnabled $ exit ()

    let auditName = ncqGetFileName ncq AuditFile
    touch auditName
    let flags = defaultFileFlags { exclusive = False, append = True }
    fd <- liftIO (openFdCompat auditName Posix.WriteOnly Nothing flags)

    void $ ContT $ bracket (pure fd) $ \h -> liftIO do
      bss <- atomically $ STM.flushTQueue ncqAuditQ
      void $ fdWriteBS h (mconcat bss)
      closeFd h

    forever do
      flip fix mempty $ \next bss -> do
        -- if BS.length bss >= 4096 then do
        if True then do
            liftIO (fdWriteBS fd bss >> fileSynchronisePortable fd)
            next mempty
        else do
         s <- atomically (readTQueue ncqAuditQ)
         next (bss <> s)

  spawnActivity $ forever do
    pause @'Seconds 30
    ema <- readTVarIO ncqWriteEMA
    debug $ "EMA" <+> pretty (realToFrac @_ @(Fixed E3) ema)

  spawnActivity $ postponed ncqPostponeService $ forever do
    ncqRemoveEmptyFossils ncq
    ncqSweepObsoleteStates ncq
    ncqSweepFiles ncq
    void $ race (pause @'Seconds ncqSweepTime) do
      atomically (ncqWaitFlagSTM ncqSweepReq)

  spawnActivity $ postponed ncqPostponeService
    $ compactLoop ncqMergeReq  ncqMergeTimeA ncqMergeTimeB $ withSem ncqServiceSem do
        ncqFossilMergeStep ncq

  spawnActivity $ postponed ncqPostponeService
    $ compactLoop ncqCompactReq ncqCompactTimeA ncqCompactTimeB $ withSem ncqServiceSem do
        ncqIndexCompactStep ncq

  flip fix RunNew $ \loop s -> do
    -- debug $ viaShow s
    case s of
      RunFin mfh -> do
        liftIO $ for_ mfh closeFd
        rest <- readTVarIO ncqWriteQ <&> Seq.length
        debug $ "exit storage" <+> pretty rest
        atomically $ pollSTM indexer >>= maybe STM.retry (const none)

      RunNew -> do
        alive <- readTVarIO ncqAlive
        empty <- readTVarIO ncqWriteQ <&> Seq.null
        if not alive && empty
          then loop (RunFin Nothing)
          else do
            (fk, fhx) <- openNewDataFile
            loop $ RunWrite (fk, fhx, 0, 0)


      RunSync (fk, fh, w, total, continue) -> do

        (stop,sync) <- atomically do
                             (,) <$> readTVar ncqStopReq
                                 <*> readTVar ncqSyncReq
                                 -- <*> readTVar ncqWriteEMA

        let needClose = total >= ncqMinLog || stop

        rest <- if not (sync || needClose || w > ncqFsync) then
                    pure w
                  else do

                    ss <- appendTailSection fh
                    liftIO (fileSynchronisePortable fh)
                    flushReplaces fk

                    ncqStateUpdate ncq do
                      ncqStateAddFact (P (PData (DataFile fk) ss))

                    -- ss <- liftIO (PFS.getFdStatus fh) <&> fromIntegral . PFS.fileSize

                    -- atomically $ ncqDeferredWriteOpSTM ncq do

                    atomically do
                      writeTVar  ncqSyncReq False
                      modifyTVar ncqSyncNo succ

                    pure 0

        if | needClose && continue -> do
                liftIO $ closeFd fh
                flushReplaces fk
                debug $ "closeFd" <+> viaShow fh
                atomically $ writeTQueue indexQ fk
                loop RunNew

           | not continue -> loop (RunFin (Just fh))

           | otherwise -> loop $ RunWrite (fk, fh, rest, total)


      RunWrite (fk, fh, w, total') -> do

        let timeoutMicro = 10_000_000

        chunk <- liftIO $ timeout timeoutMicro $ atomically do
          stop  <- readTVar ncqStopReq
          sy    <- readTVar ncqSyncReq

          chunk <- if not stop then
                     stateTVar ncqWriteQ (Seq.splitAt 1)
                   else do
                     r <- readTVar ncqWriteQ
                     modifyTVar ncqWriteQ mempty
                     pure r

          if | Seq.null chunk && stop             -> pure $ Left ()
             | Seq.null chunk && not (stop || sy) -> STM.retry
             | otherwise                          -> pure $ Right chunk

        stop <- readTVarIO ncqStopReq

        case chunk of
          Nothing -> do
            liftIO $ join $ readTVarIO ncqOnRunWriteIdle
            stop <- readTVarIO ncqStopReq
            if w == 0 && not stop then do
              loop $ RunWrite (fk,fh,w,total')
            else do
              atomically $ writeTVar ncqSyncReq True
              loop $ RunSync (fk, fh, w, total', not stop) -- exit ()

          Just (Left{})  -> loop $ RunSync (fk, fh, w, total', False) -- exit ()

          Just (Right chu) -> do
            ws <- for chu $ \h -> do
                    atomically (ncqLookupEntrySTM ncq h) >>= \case
                      Just (NCQEntry w, EntryHere bs)  -> do
                        off <- fromIntegral <$> liftIO (fdSeek fh RelativeSeek 0)
                        n <- lift (appendSection fh bs)

                        let op = do
                             readTVar w  >>= \case
                               EntryHere bs1 | bs1 == bs -> do
                                writeTVar w (EntryThere (FileLocation fk off (fromIntegral n)))
                               _ -> none

                        atomically $ modifyTVar ncqReplQueue (HM.insertWith (<>) fk  [op])
                        pure n

                      _ -> pure 0

            let written = sum ws
            loop $ RunSync (fk, fh, w + written, total' + written, not stop)

  mapM_ wait [indexer]

  where
    setAlive   = atomically $ writeTVar ncqAlive True
    unsetAlive = atomically $ writeTVar ncqAlive False

    dropReplaces :: forall m1 . MonadIO m1 => FileKey -> m1 ()
    dropReplaces fk = atomically do
        modifyTVar ncqReplQueue (HM.delete fk)

    flushReplaces :: forall m1 . MonadIO m1 => FileKey -> m1 ()
    flushReplaces fk = do
      atomically do
        ncqDelCachedDataSTM ncq fk
        ops <- readTVar ncqReplQueue <&> fromMaybe mempty . HM.lookup fk
        modifyTVar ncqReplQueue (HM.delete fk)
        sequence_ ops

    openNewDataFile :: forall mx . MonadIO mx => mx (FileKey, Fd)
    openNewDataFile = do
      fk <- ncqGetNewFileKey ncq DataFile

      atomically $ modifyTVar ncqCurrentFossils (HS.insert fk)

      -- the file exists before any state can name it
      let fname = ncqGetFileName ncq (DataFile fk)
      let flags = defaultFileFlags { exclusive = False }
      fd <- liftIO (openFdCompat fname Posix.WriteOnly (Just 0o666) flags)

      ncqStateUpdate ncq do
        ncqStateAddFact (P (PData (DataFile fk) 0))
        -- FIXM: asap-remove-this
        --  это неправильно. из-за этого файл
        --  болтается в current и мешает при мерже
        --  хотя это еще не настоящий файл (до индексации).
        --  почему мы вообще его сюда засунули?
        -- ncqStateAddDataFile fk

      pure (fk, fd)

    spawnActivity m = do
      a <- ContT $ withAsync m
      link a
      pure a

    measureWPS :: m ()
    measureWPS = void $ flip fix Nothing \loop -> \case
      Nothing      -> do
        w <- readTVarIO ncqWrites
        t <- getTimeCoarse
        pause @'Seconds step >> loop (Just (w,t))

      Just (w0,t0) -> do
        w1 <- readTVarIO ncqWrites
        t1 <- getTimeCoarse
        let dt = max 1e-9 (realToFrac @_ @Double (t1 - t0)) / 1e9
            dw = fromIntegral (w1 - w0)
        atomically $ modifyTVar ncqWriteEMA \ema -> alpha * (dw/dt) + 0.9 * ema
        pause @'Seconds step >> loop (Just (w1,t1))

      where
        alpha = 0.1
        step  = 1.00

    postponed n m = liftIO (pause @'Seconds n) >> m

    compactLoop :: TVar Bool
                -> Timeout 'Seconds
                -> Timeout 'Seconds
                -> m Bool
                -> m ()
    compactLoop flag t1 t2 what = forever $ void $ runMaybeT do
      ema   <- readTVarIO ncqWriteEMA
      fired <- ncqGetFlag flag

      when (ema > ncqIdleThrsh && not fired) $ pause @'Seconds t1 >> mzero

      ncqClearFlag flag
      compacted <- lift what

      when compacted do
        ncqSetFlag ncqSweepReq
        mzero

      k0 <- readTVarIO ncqStateKey
      void $ lift $ race (pause @'Seconds t2) do
        flip fix k0 $ \waitState k1 -> do
          pause @'Seconds t2
          k2 <- readTVarIO ncqStateKey
          when (k2 == k1) $  waitState k2


data RunSt =
    RunNew
  | RunWrite (FileKey, Fd, Int, Int)
  | RunSync  (FileKey, Fd, Int, Int, Bool)
  | RunFin   (Maybe Fd)
  deriving stock Show


