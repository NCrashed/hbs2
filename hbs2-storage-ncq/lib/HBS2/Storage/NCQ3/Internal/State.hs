{-# Language ViewPatterns #-}
module HBS2.Storage.NCQ3.Internal.State where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.Flags
import HBS2.Storage.NCQ3.Internal.MMapCache

import Data.Config.Suckless.Script

import Data.Generics.Labels()
import Data.List qualified as List
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as Set
import UnliftIO.IO.File
import System.IO qualified as IO
import Lens.Micro.Platform

import Control.Concurrent.STM qualified as STM

newtype StateOP a =
  StateOP { fromStateOp :: ReaderT NCQStorage STM a }
  deriving newtype (Functor,Applicative,Monad,MonadReader NCQStorage)

{- HLINT ignore "Eta reduce"-}

-- | Write the current state to a new state file and make it the current one.
--
-- WHAT GETS WRITTEN AND WHAT GETS FORGOTTEN ARE ONE TRANSACTION. The dump
-- request flag used to be cleared after the file was written, and the loop
-- read the version it waits on after that, so an update that landed while
-- the file was being written (the first data file of a session, every time,
-- since it is opened right at start) cleared its own wake-up and was never
-- written. The state file of the whole session then stayed the start-up one.
-- Here the flag is cleared in the same transaction that reads the state, and
-- the version that was written is returned, so a later update always wakes
-- the loop again.
--
-- THE KEY IS TAKEN FIRST, so the file sequence written into the state is past
-- the state's own key, and a restart does not hand that key out again.
--
-- ONE DUMP AT A TIME, so 'ncqStateKey' never goes back to an older file.
--
-- The file itself is written durably and atomically (temporary file, fsync,
-- rename, fsync of the directory). The directory fsync also makes the renames
-- of index and data files done before it durable, so a state never names a
-- file that a power loss could take back.
ncqStateDump :: MonadIO m
             => NCQStorage
             -> m FileKey
ncqStateDump ncq = fst <$> ncqStateDump0 ncq

ncqStateDump0 :: MonadIO m
              => NCQStorage
              -> m (FileKey, StateVersion)
ncqStateDump0 ncq@NCQStorage{..} = liftIO $ withSem ncqStateDumpSem do
    key <-  ncqGetNewFileKey ncq StateFile
    let snkFile = ncqGetFileName ncq (StateFile key)

    state <- atomically do
      ncqClearFlagSTM ncqStateDumpReq
      readTVar ncqState

    withBinaryFileDurableAtomic snkFile WriteMode $ \fh -> do
      IO.hPrint fh (pretty state)

    atomically $ writeTVar ncqStateKey key

    debug $ yellow "ncqStateDump" <+> pretty key <+> pretty (toFileName (StateFile key))
    pure (key, ncqStateVersion state)

ncqStateUpdateLoop :: MonadIO m
                   => NCQStorage
                   -> m ()

ncqStateUpdateLoop ncq@NCQStorage{..} = do

  debug $ red "ncqStateUpdateLoop"

  (_, sInit) <- ncqStateDump0 ncq

  flip fix sInit $ \next s0 -> do
    atomically do
      s1   <- readTVar ncqState <&> ncqStateVersion
      stop <- readTVar ncqStopReq
      dump <- readTVar ncqStateDumpReq
      when (s1 == s0 && not stop && not dump) STM.retry

    (_, written) <- ncqStateDump0 ncq

    done <- atomically do
      modifyTVar ncqWrites succ
      readTVar ncqStopReq

    unless done do
      next written

-- | Apply a change to the state; if it changed anything, bump the version
--   and ask for a dump.
--
-- A FACT'S SIZE IS A CHANGE. 'PData' compares by file only, so a fact whose
-- size grew after an fsync left the state equal to the old one, and the size
-- the next start-up trusts was never written. The sizes are compared here
-- explicitly.
ncqStateUpdate :: MonadIO m
               => NCQStorage
               -> StateOP a
               -> m ()

ncqStateUpdate ncq@NCQStorage{..} action = do
  atomically do
    s0 <- readTVar ncqState
    void $ runReaderT (fromStateOp action) ncq
    s1 <- readTVar ncqState
    when (s0 /= s1 || factSizes s0 /= factSizes s1) do
      modifyTVar ncqState (over #ncqStateVersion succ)
      ncqSetFlagSTM ncqStateDumpReq

  where
    factSizes s = [ (k, sz) | P (PData (DataFile k) sz) <- Set.toList (ncqStateFacts s) ]

ncqStateAddDataFile :: FileKey -> StateOP ()
ncqStateAddDataFile fk = do
  NCQStorage{..} <- ask
  StateOP $ lift do
    modifyTVar ncqState (over #ncqStateFiles (HS.insert fk))

ncqStateDelDataFile :: FileKey -> StateOP ()
ncqStateDelDataFile fk = do
  sto@NCQStorage{..} <- ask
  StateOP $ lift do
    modifyTVar ncqState (over #ncqStateFiles (HS.delete fk))
    ncqDelCachedDataSTM sto fk

ncqStateAddFact :: Fact -> StateOP ()
ncqStateAddFact fact = do
  NCQStorage{..} <- ask
  StateOP $ lift do
    modifyTVar ncqState (over #ncqStateFacts (Set.insert fact))

ncqStateDelFact :: Fact -> StateOP ()
ncqStateDelFact fact = do
  NCQStorage{..} <- ask
  StateOP $ lift do
    modifyTVar ncqState (over #ncqStateFacts (Set.delete fact))

ncqStateAddIndexFile :: POSIXTime
                     -> FileKey
                     -> StateOP ()

ncqStateAddIndexFile ts fk  = do
  NCQStorage{..} <- ask
  StateOP $ lift do
    modifyTVar' ncqState (sortIndexes . over #ncqStateIndex ((Down ts, fk) :))

ncqStateDelIndexFile :: FileKey  -> StateOP ()
ncqStateDelIndexFile fk  = do
  sto@NCQStorage{..} <- ask
  StateOP $ lift do
    modifyTVar' ncqState (over #ncqStateIndex $ filter f)
    ncqDelCachedIndexSTM sto fk

  where f (_,b) = b /= fk

sortIndexes :: NCQState -> NCQState
sortIndexes = over #ncqStateIndex sortIndexes0


ncqStateCapture :: forall m . MonadUnliftIO m
            => NCQStorage
            -> m FileKey

ncqStateCapture me@NCQStorage{..} = do
  atomically do
    key      <- readTVar ncqStateKey
    stateUse <- readTVar ncqStateUse
    case HM.lookup key stateUse of
      Just (_, tv) -> modifyTVar tv succ
      Nothing      -> do
        state    <- readTVar ncqState
        new   <- (state,) <$> newTVar 1
        modifyTVar ncqStateUse (HM.insert key new)
    pure key

ncqStateDismiss :: forall m . MonadUnliftIO m
                => NCQStorage
                -> FileKey
                -> m ()
ncqStateDismiss me@NCQStorage{..} key = atomically do
  useMap <- readTVar ncqStateUse
  case HM.lookup key useMap of
    Nothing -> pure ()
    Just (_, tv) -> do
      modifyTVar tv (max 0 . pred)
      cnt <- readTVar tv
      when (cnt <= 0) do
        modifyTVar ncqStateUse (HM.delete key)

ncqWithState :: forall a m . MonadUnliftIO m
             => NCQStorage
             -> ( FileKey -> m a  )
             -> m a
ncqWithState sto = bracket (ncqStateCapture sto) (ncqStateDismiss sto)

readStateMay :: forall m . MonadUnliftIO m
             => NCQStorage
             -> FileKey
             -> m (Maybe NCQState)
readStateMay sto key = fmap sortIndexes <$> do
  s <- liftIO (readFile (ncqGetFileName sto (StateFile key)))
  runMaybeT do
    sexps <- parseTop s & toMPlus

    flip fix (ncqState0, sexps) $ \next -> \case
      (acc, []) -> pure acc
      (acc, e : ss)  -> next (acc <> entryOf e, ss)

  where

    entryOf = \case
       ListVal [SymbolVal "i",  LitIntVal n, LitIntVal ts] ->
         ncqState0 { ncqStateIndex = [ (fromIntegral ts, fromIntegral n) ] }

       ListVal [SymbolVal "f",  LitIntVal n] ->
         ncqState0 { ncqStateFiles = HS.singleton (fromIntegral n) }

       ListVal [SymbolVal "fp", LitIntVal a, LitIntVal s] ->
         ncqState0 { ncqStateFacts = Set.singleton (P (PData (DataFile $ fromIntegral a) (fromIntegral s))) }

       ListVal [SymbolVal "n", LitIntVal a] ->
         ncqState0 { ncqStateFileSeq = fromIntegral a }

       _ -> ncqState0

-- aux. functions for tests and something

ncqGetIndex :: MonadIO m => NCQStorage -> m [(Down POSIXTime, FileKey)]
ncqGetIndex NCQStorage{..} =
  readTVarIO ncqState <&> view #ncqStateIndex

ncqGetIndexSTM :: NCQStorage -> STM [(Down POSIXTime, FileKey)]
ncqGetIndexSTM NCQStorage{..} =
  view #ncqStateIndex <$> readTVar ncqState

