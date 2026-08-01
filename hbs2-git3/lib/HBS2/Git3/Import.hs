{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language MultiWayIf #-}
module HBS2.Git3.Import where

import HBS2.Git3.Prelude
import HBS2.Git3.State
import HBS2.Git3.Git
import HBS2.Git3.Git.Pack
import HBS2.Git3.Config.Local

import HBS2.Data.Detect (readLogThrow,deepScan,ScanLevel(..))
import HBS2.Storage.Operations.Missed
import HBS2.CLI.Run.Internal.Merkle (getTreeContents)
import HBS2.Data.Log.Structured

import HBS2.System.Dir
import Data.Config.Suckless.Almost.RPC
import Data.Config.Suckless.Script

import Control.Applicative
import Codec.Compression.Zlib qualified as Zlib
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe
import Data.Either
import Data.List qualified as L
import Network.ByteOrder qualified as N
import System.IO.Temp as Temp
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO.IO.File qualified as UIO
import System.IO (hPrint)

import Streaming.Prelude qualified as S

data ImportException =
    ImportInvalidSegment HashRef
  | ImportBlocksUnavailable Int String
  deriving stock (Show,Typeable)

instance Exception ImportException

writeAsGitPack :: forall m . (HBS2GitPerks m, HasStorage m)
              => FilePath
              -> HashRef
              -> m (Maybe FilePath)

writeAsGitPack dir href = do

  sto <- getStorage

  file <- liftIO $ Temp.emptyTempFile dir (show (pretty href) <> ".pack")

  no_ <- newTVarIO 0

  liftIO $ UIO.withBinaryFileAtomic file ReadWriteMode $ \fh -> flip runContT pure do

    let header = BS.concat [ "PACK", N.bytestring32 2,  N.bytestring32 0 ]

    liftIO $ BS.hPutStr fh header

    seen_ <- newTVarIO (mempty :: HashSet GitHash)

    source <- liftIO (runExceptT (getTreeContents sto href))
                >>= orThrow (MissedBlockError2 (show $ pretty href))

    lbs' <- decompressSegmentLBS source

    lbs <- ContT $ maybe1 lbs' none

    runConsumeLBS lbs $ readLogFileLBS () $ \h s obs -> do
      seen <- readTVarIO seen_ <&> HS.member h
      unless seen do

        let (t, body) = LBS.splitAt 1 obs

        let tp = fromStringMay @(Short GitObjectType) (LBS8.unpack t)
                     & maybe Blob coerce

        let params = Zlib.defaultCompressParams { Zlib.compressMethod = Zlib.deflateMethod }

        let packed = Zlib.compressWith params body

        let preamble = encodeObjectSize (gitPackTypeOf tp) (fromIntegral $ LBS.length body)

        liftIO do
          atomically $ modifyTVar seen_ (HS.insert h)
          BS.hPutStr fh preamble
          LBS.hPutStr fh packed

        atomically $ modifyTVar no_ succ

    no <- readTVarIO no_
    hSeek fh AbsoluteSeek 8
    liftIO $ BS.hPutStr fh (N.bytestring32 no)
    hFlush fh

    sz <- hFileSize fh
    hSeek fh AbsoluteSeek 0

    sha <- liftIO $ LBS.hGetNonBlocking fh (fromIntegral sz) <&> sha1lazy

    hSeek fh SeekFromEnd 0

    liftIO $ BS.hPutStr fh sha

  no <- readTVarIO no_

  if no > 0 then do
    pure $ Just file
  else do
    rm file
    pure Nothing


-- | How many rounds of an unmoving download count as "not moving".
--
-- At roughly three seconds a round this is a couple of minutes of patience,
-- which is generous for a counter that is supposed to change whenever anything
-- completes, and finite, which is the point.
maxStuckRounds :: Int
maxStuckRounds = 40

-- | How many times to come back for blocks that are not here yet.
--
-- Each round waits a little longer than the last, so this is roughly three
-- minutes of asking before the import calls the blocks unavailable. It gives up
-- for good at that point: the exception is an 'ImportException', so the retry
-- wrapped around 'doImport' does not catch it and start over. Any bound would
-- do; what matters is that there is one. Without it a block nobody can serve is
-- indistinguishable from a block that is on its way, and the import waits for
-- the difference forever, which is what a stuck push looked like from outside.
maxMissedRounds :: Int
maxMissedRounds = 16

data ImportStage =
    ImportStart
  | ImportWIP  (Timeout 'Seconds) Int (Maybe HashRef)
  | ImportWait (Timeout 'Seconds)  (Maybe (Int,Int)) ImportStage
  | ImportDone (Maybe HashRef)

{- HLINT ignore "Functor law" -}

importGitRefLog :: forall m . ( HBS2GitPerks m
                              )
             => Git3 m (Maybe HashRef)

importGitRefLog = do
    packs <- gitDir
               >>= orThrow NoGitDir
               <&> (</> "objects/pack")

    mkdir packs

    doImport packs `catch` (\( e :: OperationError) -> err (viaShow e) >> pause @'Seconds 1 >> doImport packs)

  where
    doImport packs = withStateDo $ ask >>= \case
      Git3Disconnected{} -> throwIO Git3PeerNotConnected
      Git3Connected{..} ->  flip runContT pure do

        sto <- getStorage

        already_ <- newTVarIO (mempty :: HashSet HashRef)

        oldRvl <- gitRefLogVal & readTVarIO
        reflog <- getGitRemoteKey >>= orThrow Git3ReflogNotSet
        newRvl_ <- newTVarIO Nothing

        void $ ContT $ withAsync $ forever do
          void $ lift (callRpcWaitMay @RpcRefLogFetch (TimeoutSec 2) reflogAPI reflog)

          lift (callRpcWaitMay @RpcRefLogGet (TimeoutSec 2) reflogAPI reflog)
            >>= \case
                   Just (Just x) | Just x /= oldRvl -> atomically (writeTVar newRvl_ (Just x))
                   _  -> none

          pause @'Seconds 10

        lift $ flip fix ImportStart $ \again -> \case
          ImportDone x -> do
            notice "import done"

            newRlv <- readTVarIO newRvl_
            let doAgain = newRlv /= oldRvl

            updateReflogIndex
            for_ x updateImportedCheckpoint

            refs <- importedRefs

            if not (null refs && isJust x) || doAgain then do
              pure x
            else do
              atomically do
                writeTVar newRvl_ Nothing
                writeTVar gitRefLogVal (newRlv <|> oldRvl)

              notice $ "import: go again"
              again ImportStart

          ImportWait sec d next -> do

            pause sec

            down <- callRpcWaitRetry @RpcGetProbes (TimeoutSec 1) 3 peerAPI ()
                       >>= orThrow RpcTimeout
                       <&> maybe 0 fromIntegral . headMay . mapMaybe \case
                             ProbeSnapshotElement "Download.wip" n -> Just n
                             _ -> Nothing

            notice $ "wait-for-download" <+> parens (pretty down)

            -- The count comes from the peer as a whole, not from this import, so
            -- one download that will never finish (a block nobody has, asked for
            -- by anything on this node) holds the number at a constant non-zero
            -- forever. Waiting for it to move was then waiting for good: the
            -- repository looked broken, and the peer's own queue was empty.
            --
            -- Stop waiting once it has not moved for a while and go on. The
            -- import that follows fetches what it needs and retries with its own
            -- backoff if a block really is missing, so the cost of giving up too
            -- early is a slower round, and the cost of not giving up is a
            -- repository nobody can fetch or push.
            case d of
              Just (n, _) | down /= n || down == 0 -> again next

              Just (_, stuck) | stuck >= maxStuckRounds -> do
                notice $ "the peer has been downloading" <+> pretty down
                           <+> "block(s) without progress; going on without waiting"
                again next

              _ -> do
                let stuck = case d of
                              Just (n, s) | n == down -> succ s
                              _                       -> 0
                pause @'Seconds 2.85
                again (ImportWait (sec*1.10) (Just (down, stuck)) next)

          ImportStart -> do

            rvl  <- readTVarIO gitRefLogVal

            importGroupKeys

            prev <- importedCheckpoint

            if | isNothing prev -> again $ ImportWIP 1.0 0 prev

               | prev /= rvl -> do
                  again $ ImportWIP 1.0 0 prev

               | otherwise -> again $ ImportDone prev

          ImportWIP w attempt prev -> do

            notice $ "download wip" <+> pretty attempt

            r <- try @_ @OperationError $ do

              -- The checkpoint is a local marker saying "imported up to here",
              -- and the tree it names lives in shared storage. Once the reflog
              -- stops referencing that checkpoint nothing fetches it again, so
              -- a node that dropped those blocks, or never had them, keeps a
              -- marker pointing at a block it cannot read.
              --
              -- That walk used to throw, and the throw landed on the retry path
              -- below, which waits for the peer's download queue to drain. The
              -- queue was empty, because nobody had asked for the block, so the
              -- wait ended at once and the same walk threw again, and again:
              -- "download wip" and "wait-for-download (0)" in turn while no
              -- fetch and no push against the repository could ever finish.
              --
              -- The set is only used to skip work already done, so an unreadable
              -- marker costs a re-import and nothing more. Pay that instead.
              -- 'txImported' reaches the same conclusion for the same reason.
              excl <- maybe1 prev (pure mempty) $ \p -> do
                try @_ @OperationError (txListAll (Just p)) >>= \case
                  Right txs -> pure (HS.fromList (fmap fst txs))
                  Left e    -> do
                    notice $ "cannot read the imported checkpoint" <+> pretty p
                               <+> parens (viaShow e) <> ", importing again"
                    pure mempty

              rv <- refLogRef

              hxs <- txList ( pure . not . flip HS.member excl ) rv

              (cp', pending) <- flip fix (fmap snd hxs, Nothing, mempty) $ \next -> \case
                ([], r, pend) -> pure (r, pend)
                (TxSegment{}:xs, l, pend) -> next (xs, l, pend)
                (cp@(TxCheckpoint n tree) : xs, l, pend) -> do

                  missed <- findMissedBlocks sto tree

                  let full = L.null missed

                  if full && Just n > (getGitTxRank <$> l) then do
                    next (xs, Just cp, pend)
                  else if full then do
                    next (xs, l, pend)
                  else do
                    next (xs, l, (n, missed) : pend)

              -- The blocks a checkpoint turned out to be missing were counted
              -- and then dropped on the floor. The wait below watches the peer's
              -- download queue, so counting without asking left it watching a
              -- queue nobody had filled: it read zero, concluded there was
              -- nothing to wait for, and came straight back to count the same
              -- blocks again. Ask for them.
              --
              -- Only for the best checkpoint that could not be used, never for
              -- every one that came up short. An old checkpoint can be missing
              -- blocks that nobody on the network still has, and a request that
              -- nobody can answer sits in the queue for good; there is no reason
              -- to file one for a checkpoint already superseded.
              let chosenRank = maybe 0 getGitTxRank cp'
              for_ (lastMay (L.sortOn fst [ p | p@(n,_) <- pending, n > chosenRank ])) $
                \(rank, missed) -> do
                  notice $ "asking for" <+> pretty (length missed)
                             <+> "missing block(s) of checkpoint rank" <+> pretty rank
                  for_ missed $ \h ->
                    void $ callRpcWaitMay @RpcFetch (TimeoutSec 1) peerAPI h

              case cp' of
                Just TxCheckpoint{..} -> do

                  notice $ "checkpoint" <+> pretty gitTxTree <+> pretty gitTxRank
                  txs <- txList ( pure . not . flip HS.member excl ) (Just gitTxTree)

                  forConcurrently_ txs $ \case
                    (_, TxCheckpoint{}) -> none
                    (h, TxSegment tree) -> do
                      new <- readTVarIO already_ <&> not . HS.member tree

                      when new do
                        s <- writeAsGitPack  packs tree

                        for_ s $ \file -> do
                          gitRunCommand [qc|git index-pack {file}|]
                            >>= orThrowPassIO

                        atomically $ modifyTVar already_ (HS.insert tree)
                        notice $ "imported" <+> pretty h

                  pure (Just gitTxTree)

                _ -> do
                  notice "no checkpoints found"
                  pure Nothing

            -- Say which block. 'txList' goes out of its way to name the one it
            -- could not read, and reporting "missed blocks" threw that away:
            -- whoever hit this had a repository that would not sync and no way
            -- to tell which block to go looking for.
            --
            -- Then stop. This retry used to be unbounded, on the reasoning that
            -- a missing block is usually a block still on its way. Usually. When
            -- it was not, the loop was the only thing the user ever saw, and a
            -- push that fails with a hash to chase beats one that never returns.
            let missedAgain what = do
                  when (attempt >= maxMissedRounds) do
                    err $ "giving up on missing block(s) after" <+> pretty attempt
                            <+> "attempts:" <+> pretty what
                    throwIO (ImportBlocksUnavailable attempt what)

                  notice $ "missed blocks" <+> pretty what
                  again (ImportWait w Nothing (ImportWIP (w*1.15) (succ attempt) prev))

            case r of
              Right cp -> again $ ImportDone cp

              Left  (MissedBlockError2 s) -> missedAgain s

              Left  MissedBlockError      -> missedAgain "(block not named)"

              Left  e                     -> err (viaShow e) >> throwIO e


groupKeysFile :: (MonadIO m) => Git3 m FilePath
groupKeysFile = getStatePathM <&> (</> "groupkeys")

readGroupKeyFile :: (MonadIO m) => Git3 m (Maybe HashRef)
readGroupKeyFile = do
  file <- groupKeysFile
  debug $ "readGroupKeyFile" <+> pretty file
  liftIO (try @_ @IOError (readFile file))
    <&> fromRight mempty
    <&> parseTop
    <&> fromRight mempty
    <&> \x -> headMay [ w | ListVal [HashLike w] <- x ]

importGroupKeys :: forall m . ( HBS2GitPerks m
                              )
             => Git3 m ()

importGroupKeys = do

  notice $ "importGroupKeys"
  sto <- getStorage

  already <- readGroupKeyFile

  LWWRef{..} <- getRepoRefMaybe >>= orThrow GitRepoRefNotSet
  rhead <- readLogThrow (getBlock sto) lwwValue
  let keyTree' = headMay (tailSafe rhead)

  when (keyTree' /= already) do

    ops <- S.toList_ $ for_ keyTree' $ \tree -> do
      keyhashes <- readLogThrow (getBlock sto) tree
      for_ keyhashes $ \h -> do
        S.yield $ mkForm @C "gk:track" [mkSym (show $ pretty h)]

    -- FIXME: check-added-keys
    unless (null ops) do
      _ <- callProc "hbs2-keyman" ["run","stdin"] ops
      updateGroupKeys keyTree'

  where

    updateGroupKeys keyTree' = do
      file <- groupKeysFile
      void $ runMaybeT do
        val <- keyTree' & toMPlus
        liftIO $ UIO.withBinaryFileAtomic file WriteMode $ \fh -> do
          hPrint fh $ pretty (mkSym @C (show $ pretty val))

