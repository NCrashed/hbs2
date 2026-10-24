module HBS2.Storage.NCQ3.Internal.Sweep where


import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.Fossil
import HBS2.Storage.NCQ3.Internal.State

import Control.Monad.Trans.Cont
import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as List
import System.Posix.Files qualified as PFS
import Streaming.Prelude qualified as S

ncqLiveKeysSTM :: NCQStorage -> STM (HashSet FileKey)
ncqLiveKeysSTM NCQStorage{..} = do

  s0      <- readTVar ncqState
  merged  <- readTVar ncqStateUse <&> (s0<>) . foldMap fst . HM.elems
  current <- readTVar ncqCurrentFossils

  pure $ current <> HS.fromList (universeBi @_ @FileKey merged)

ncqLiveKeys :: forall m . MonadIO m => NCQStorage -> m (HashSet FileKey)
ncqLiveKeys  ncq = atomically $ ncqLiveKeysSTM  ncq

{- HLINT ignore "Functor law"-}

ncqRemoveEmptyFossils :: forall m . MonadUnliftIO m => NCQStorage -> m ()
ncqRemoveEmptyFossils me@NCQStorage{..} = flip runContT pure $ callCC \exit -> do

  s@NCQState{..} <- readTVarIO ncqState
  debug $ red "CURRENT STATE" <+> pretty ncqStateVersion <> line <> pretty s

  check <- atomically do
    NCQState{..}  <- readTVar ncqState
    current <- readTVar ncqCurrentFossils
    let fks =  HS.fromList [ coerce fk | P (PData fk _) <- universeBi ncqStateFacts ]
    pure $ HS.toList (fks `HS.difference` current)

  current <- readTVarIO ncqCurrentFossils
  debug $ "ncqRemoveEmptyFossils" <+> pretty (HS.toList current) <+> pretty check

  loosers <- S.toList_ $ for_ check $ \fk -> do
    let path = ncqGetFileName me (toFileName (DataFile fk))
    s <- fileSize path
    when (s <= typicalFileTailRecordLen) $ S.yield fk

  debug $ "ncqRemoveEmptyFossils" <+> pretty loosers

  when (List.null loosers) $ exit ()

  ncqStateUpdate me $ for_ loosers $ \fk -> do
    ncqStateDelDataFile fk
    ncqStateDelFact (P (PData (DataFile fk) 0))

-- | Delete the data and index files this process dropped from the state,
--   once no state (in memory, in use, or on disk) names them any more.
--
-- ONLY WHAT WAS DROPPED, NOT WHATEVER IS UNKNOWN. This used to delete every
-- f- and i- file not named by a live state, so when the state on disk was old
-- it deleted everything written after it. A file can be unknown to the state
-- because the state is behind; it is obsolete only if this process removed it
-- (a merge, a compaction, an empty fossil). Unknown files are dealt with once,
-- at start-up, by 'ncqTryLoadState', which never deletes them.
ncqSweepFiles :: forall m . MonadUnliftIO m => NCQStorage -> m ()
ncqSweepFiles me@NCQStorage{..} = do

  debug "ncqSweepFiles"

  obsolete <- readTVarIO ncqObsoleteFiles <&> HS.toList

  unless (List.null obsolete) do

    stateFiles  <- ncqListFilesBy me (List.isPrefixOf "s-") <&> fmap snd

    liveOnDisk <- for stateFiles (readStateMay me)
                    <&> mconcat . catMaybes
                    <&> HS.fromList . universeBi @_ @FileKey

    live <- ncqLiveKeys me <&> (<> liveOnDisk)

    debug $ "ALIVE" <+> pretty (HS.toList live)

    for_ obsolete $ \name -> do
      let k = fromString @FileKey name
      let fn = ncqGetFileName me name
      unless (HS.member k live) do
        here <- doesFileExist fn
        when here do
          debug $ yellow "REMOVING" <+> pretty (takeFileName fn)
          removeFile fn
        atomically $ modifyTVar ncqObsoleteFiles (HS.delete name)


ncqSweepObsoleteStates :: forall m .  MonadUnliftIO m => NCQStorage -> m ()
ncqSweepObsoleteStates me@NCQStorage{..} = flip runContT pure $ callCC \exit -> do
    debug $ "ncqSweepObsoleteStates"

    k <- readTVarIO ncqStateKey

    when (k == ncqNullStateKey) $ exit ()

    r <- liftIO $ try @_ @SomeException do
          ts <- PFS.getFileStatus (ncqGetFileName me (StateFile k)) <&> PFS.modificationTimeHiRes

          filez <- ncqListFilesBy me (List.isPrefixOf "s-")
                      <&> List.drop 1 . List.sortOn (Down . snd) -- delete old 10 states

          for_ filez $ \(t,f) -> do

            when (f /= k && t < ts) do
              debug $ yellow "TO REMOVE" <+> pretty (toFileName (StateFile f))
              removeFile (ncqGetFileName me (StateFile f))

    case r of
      Left e  -> err ("SweepStates failed" <+> viaShow e)
      Right{} -> none


