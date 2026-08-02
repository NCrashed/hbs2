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
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.IORef
import Data.Maybe

-- TODO: slow-dangerous
findMissedBlocks :: (MonadIO m) => AnyStorage -> HashRef -> m [HashRef]
findMissedBlocks sto href = do
  -- TODO: cache-results-limit-calls-freq
  -- trace $ "findMissedBlocks" <+> pretty href
  S.toList_ $ findMissedBlocks2 sto href

findMissedBlocks2 :: (MonadIO m) => AnyStorage -> HashRef -> Stream (Of HashRef) m ()
findMissedBlocks2 sto href = do
  seen <- liftIO $ newIORef mempty
  findMissedBlocksOn seen sto href

-- | As 'findMissedBlocks2', with the set of roots already walked carried in.
--
-- The walk over ONE tree is bounded by 'walkMerkleUnique', which enters a node
-- once. What that does not bound is this function's own recursion: a leaf
-- payload may name a nested merkle root, and every one of them used to start a
-- fresh walk with a fresh visited set. So N leaf entries all naming the same
-- nested root cost N complete walks of it, and a tree of such trees multiplies
-- again at every level -- polynomial rather than exponential, but the exponent
-- is the nesting depth and the input is a root somebody else chose.
--
-- Sharing the set makes the whole recursion cost the number of DISTINCT blocks
-- reachable from the root, once. It also bounds what 'findMissedBlocks' can
-- accumulate, since it collects the stream with 'S.toList_'.
--
-- Deduplicating is sound here for the reason it is sound in 'walkMerkleUnique'
-- and not in 'walkMerkle'': the question is which blocks a graph mentions that
-- this node does not hold, and that is a SET. Nothing downstream counts
-- occurrences.
findMissedBlocksOn :: (MonadIO m)
                   => IORef (HashSet HashRef)
                   -> AnyStorage
                   -> HashRef
                   -> Stream (Of HashRef) m ()
findMissedBlocksOn seen sto href = void $ runMaybeT do

    already <- liftIO $ atomicModifyIORef' seen $ \v ->
                 (HS.insert href v, HS.member href v)

    guard (not already)

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
                      Merkle{}    -> lift $ lift $ nested hx
                      MerkleAnn t -> lift $ lift do
                        -- FIXME: make-tail-recursive

                        b0 <- case _mtaMeta t of
                                AnnHashRef hm -> nested (HashRef hm)
                                _ -> pure mempty

                        b1 <- nested hx

                        b2 <-  case _mtaCrypt t of
                                (EncryptGroupNaClSymm hash _) ->
                                  nested (HashRef hash)

                                _ -> pure mempty

                        pure (b0 <> b1 <> b2)

                      _ -> pure mempty

                lift $ mapM_ S.yield r

  where
    -- A nested root goes back through the SAME visited set, which is the whole
    -- of the fix: it used to call findMissedBlocks, and that starts a fresh one.
    nested hx = S.toList_ (findMissedBlocksOn seen sto hx)

