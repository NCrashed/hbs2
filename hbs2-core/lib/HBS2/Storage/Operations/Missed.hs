module HBS2.Storage.Operations.Missed where

import HBS2.Prelude.Plated
import HBS2.Data.Detect
import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Merkle
import HBS2.Storage

import HBS2.System.Logger.Simple

import Streaming.Prelude qualified as S
import Streaming.Prelude (Stream, Of(..))
import Control.Monad.Trans.Maybe
import Control.Monad
import Data.Coerce
import Data.Maybe

-- TODO: slow-dangerous
findMissedBlocks :: (MonadIO m) => AnyStorage -> HashRef -> m [HashRef]
findMissedBlocks sto href = do
  -- TODO: limit-recursion-depth?
  -- TODO: cache-results-limit-calls-freq
  -- trace $ "findMissedBlocks" <+> pretty href
  S.toList_ $ findMissedBlocks2 sto href

findMissedBlocks2 :: (MonadIO m) => AnyStorage -> HashRef -> Stream (Of HashRef) m ()
findMissedBlocks2 sto href = void $ runMaybeT do

    self' <- getBlock sto (coerce href)

    unless (isJust self') do
      lift $ S.yield (coerce href)

    self <- toMPlus self'

    let refs = extractBlockRefs (coerce href) self

    for_ refs $ \r -> do
      -- findMissedBlocks sto r >>= lift . mapM_ S.yield
      here <- hasBlock sto (coerce r) <&> isJust
      unless here $ lift $ S.yield r

    -- walkMerkleUnique, and the href is whatever root the caller was given: this
    -- is reached from the mailbox status handler with a root another peer chose.
    -- A plain walk follows every edge, so a chain of nodes each naming its one
    -- child twice costs 2^depth for depth+1 blocks -- nine seconds at depth 22,
    -- and it never returns at depth 40, for about two kilobytes of well-formed
    -- blocks. The answer here is the SET of blocks this graph mentions that we
    -- do not hold, so entering each node once is what the question asks for, and
    -- it bounds what findMissedBlocks can accumulate to the number of distinct
    -- blocks rather than the number of paths to them.
    lift $ walkMerkleUnique (fromHashRef href) (lift . getBlock sto) $ \(hr :: Either (Hash HbSync) [HashRef]) -> do
      case hr of
        -- FIXME: investigate-this-wtf
        Left hx -> S.yield (HashRef hx)

        Right (hrr :: [HashRef]) -> do
          forM_ hrr $ \hx -> runMaybeT do
              blk <- lift $ getBlock sto (fromHashRef hx)

              unless (isJust blk) do
                lift $ S.yield hx

              maybe1 blk none $ \bs -> do
                let w = tryDetect (fromHashRef hx) bs
                let refs = extractBlockRefs (coerce hx) bs

                for_ refs $ \r -> do
                  -- findMissedBlocks sto r >>= lift . mapM_ S.yield
                  here <- hasBlock sto (coerce r) <&> isJust
                  unless here $ lift $ S.yield r

                r <- case w of
                      Merkle{}    -> lift $ lift $ findMissedBlocks sto hx
                      MerkleAnn t -> lift $ lift do
                        -- FIXME: make-tail-recursive

                        b0 <- case _mtaMeta t of
                                AnnHashRef hm -> findMissedBlocks sto (HashRef hm)
                                _ -> pure mempty

                        b1 <- findMissedBlocks sto hx

                        b2 <-  case _mtaCrypt t of
                                (EncryptGroupNaClSymm hash _) ->
                                  findMissedBlocks sto (HashRef hash)

                                _ -> pure mempty

                        pure (b0 <> b1 <> b2)

                      _ -> pure mempty

                lift $ mapM_ S.yield r

