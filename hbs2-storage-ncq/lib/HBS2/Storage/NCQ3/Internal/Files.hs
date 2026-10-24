{-# Language OverloadedRecordDot #-}
module HBS2.Storage.NCQ3.Internal.Files where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types

import HBS2.Storage.NCQ3.Internal.Fsync
import HBS2.Storage.NCQ3.Internal.UnixCompat (openFdCompat)

import System.Posix.Files qualified as PFS
import System.Posix.IO qualified as Posix
import System.IO qualified as IO
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import UnliftIO.IO.File (withBinaryFileDurable)

{- HLINT ignore "Eta reduce" -}


removeFile :: MonadIO m => FilePath -> m ()
removeFile fp = do
  debug $ "removeFile" <+> pretty fp
  rm fp

moveFile :: MonadIO m => FilePath -> FilePath -> m ()
moveFile a b = do
  debug $ "moveFile" <+> pretty a <+> pretty b
  mv a b

ncqGetFileName :: forall f . ToFileName f => NCQStorage -> f -> FilePath
ncqGetFileName ncq fp = ncqGetWorkDir ncq </> takeFileName (toFileName fp)

ncqGetWorkDir :: NCQStorage -> FilePath
ncqGetWorkDir NCQStorage{..} = ncqRoot </> show ncqGen

ncqGetLockFileName :: NCQStorage -> FilePath
ncqGetLockFileName ncq = ncqGetFileName ncq ".lock"

ncqGetNewFileKey :: forall f m . (ToFileName f, MonadIO m)
                    => NCQStorage
                    -> ( FileKey -> f )
                    -> m FileKey
ncqGetNewFileKey me@NCQStorage{..} fnameOf = fix \next -> do
    n <- atomically $ stateTVar ncqState (\e -> (e.ncqStateFileSeq , succSeq e))
    here <- doesFileExist (ncqGetFileName me (fnameOf n))
    if here then next else pure n
  where
    succSeq e = e { ncqStateFileSeq = succ e.ncqStateFileSeq }

ncqListFilesBy :: forall m . MonadUnliftIO m => NCQStorage -> (FilePath -> Bool) -> m [(POSIXTime, FileKey)]
ncqListFilesBy  me@NCQStorage{..} filt = do
  -- only the work dir itself: what is in quarantine/ is not the storage's
  let wd = ncqGetWorkDir me
  w <- dirFiles wd
           <&> filter (\fn -> fn == wd </> takeFileName fn)
           <&> filter (filt .  takeFileName)

  r <- for w $ \fn -> do
         ts <- liftIO (PFS.getFileStatus fn) <&> PFS.modificationTimeHiRes
         pure (ts, fromString (takeBaseName fn))

  pure $ List.sortOn ( Down . fst ) r

ncqFindMinPairOfBy :: forall fa m . (ToFileName fa, MonadUnliftIO m)
                   => NCQStorage
                   -> (fa -> Bool)                         -- ^ eligible predicate
                   -> [fa]
                   -> m (Maybe (NCQFileSize, fa, fa))
ncqFindMinPairOfBy sto eligible lst =
  go lst Nothing
  where
    go :: [fa] -> Maybe (NCQFileSize, fa, fa) -> m (Maybe (NCQFileSize, fa, fa))
    go (a:b:rest) best = do
      best' <- if eligible a && eligible b
                 then do
                   let pa = ncqGetFileName sto a
                   let pb = ncqGetFileName sto b
                   s1 <- fsize pa
                   s2 <- fsize pb
                   let sz = fromIntegral (s1 + s2)
                   pure $ case best of
                     Nothing                -> Just (sz, a, b)
                     Just (sz0,_,_) | sz<sz0 -> Just (sz, a, b)
                     _                       -> best
                 else pure best
      go (b:rest) best'
    go _ best = pure best

    fsize s = liftIO (PFS.getFileStatus s) <&> PFS.fileSize

ncqFindMinPairOf ::  forall fa m . (ToFileName fa, MonadUnliftIO m)
                 => NCQStorage
                 -> [fa]
                 -> m (Maybe (NCQFileSize, fa, fa))
ncqFindMinPairOf sto lst = ncqFindMinPairOfBy sto (const True) lst


-- | fsync a file (or a directory) by name.
ncqSyncPath :: MonadIO m => FilePath -> m ()
ncqSyncPath fp = liftIO do
  bracket (openFdCompat fp Posix.ReadOnly Nothing Posix.defaultFileFlags)
          Posix.closeFd
          fileSynchronisePortable

ncqGetQuarantineDir :: NCQStorage -> FilePath
ncqGetQuarantineDir ncq = ncqGetWorkDir ncq </> ncqQuarantineDirName

-- | A name in the quarantine directory that nothing has yet.
ncqQuarantineName :: MonadIO m => NCQStorage -> FilePath -> m FilePath
ncqQuarantineName ncq what = do
  let dir = ncqGetQuarantineDir ncq
  mkdir dir
  t <- liftIO getPOSIXTime <&> round @_ @Integer . (* 1000)
  flip fix (0 :: Int) $ \next i -> do
    let suff = if i == 0 then "" else "." <> show i
    let dest = dir </> (takeFileName what <> "." <> show t <> suff)
    here <- doesFileExist dest
    if here then next (succ i) else pure dest

-- | Move a file the storage cannot account for out of its way.
--
-- NOTHING IS DELETED ON UNCERTAINTY. What start-up cannot fit into the state
-- it loaded goes here, where the storage never looks again and an operator
-- can.
ncqQuarantineFile :: MonadIO m => NCQStorage -> FilePath -> m FilePath
ncqQuarantineFile ncq fp = do
  dest <- ncqQuarantineName ncq fp
  warn $ red "quarantine" <+> pretty fp <+> "=>" <+> pretty dest
  mv fp dest
  ncqSyncPath (takeDirectory dest)
  ncqSyncPath (takeDirectory fp)
  pure dest

-- | Copy the bytes of a file from an offset on into the quarantine directory,
--   durably, before that part of the file is cut off.
ncqQuarantineTail :: MonadIO m => NCQStorage -> FilePath -> Integer -> m (Maybe FilePath)
ncqQuarantineTail ncq fp from = do
  size <- fileSize fp
  if from >= size then pure Nothing else do
    dest <- ncqQuarantineName ncq (takeFileName fp <> ".tail-" <> show from)
    liftIO $ IO.withBinaryFile fp IO.ReadMode $ \h -> do
      IO.hSeek h IO.AbsoluteSeek from
      bytes <- LBS.hGetContents h
      withBinaryFileDurable dest IO.WriteMode $ \o -> LBS.hPut o bytes
    ncqSyncPath (takeDirectory dest)
    warn $ red "saved tail" <+> pretty (size - from) <+> "bytes of"
             <+> pretty (takeFileName fp) <+> "=>" <+> pretty dest
    pure (Just dest)
