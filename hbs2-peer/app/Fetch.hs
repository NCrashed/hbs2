module Fetch where

import HBS2.Prelude
import HBS2.Actors.Peer
import HBS2.Data.Types.Refs
import HBS2.Storage.Operations.Missed
import HBS2.Net.Proto.Types
import HBS2.Peer.CompleteObject

import PeerTypes
import UnliftIO.Async (race)

fetchHash :: forall e m . ( e ~ L4Proto
                          , MonadIO m
                          )
      => PeerEnv e
      -> HashRef
      -> m ()

fetchHash penv href  = do
  debug  $ "fetchAction" <+> pretty h
  liftIO $ withPeerM penv $ do
    sto <- getStorage
    missed <- findMissedBlocks sto href
    for_ missed $ \miss -> do
       addDownload @e Nothing (fromHashRef miss)

  where
    h = fromHashRef href

-- | Request a logical object and wait until all of its reachable physical
-- blocks are present.  Re-submitting the current missing set lets a newly
-- arrived root reveal and schedule its descendants; the deadline turns a
-- provider with an incomplete graph into a command failure rather than a
-- successful no-op.
fetchCompleteObject :: PeerEnv L4Proto
                    -> Int
                    -> HashRef
                    -> IO (Either CompleteObjectError ())
fetchCompleteObject penv seconds root = do
  storage <- withPeerM penv getStorage
  race (pause @'Seconds (fromIntegral (max 1 seconds))) (waitComplete storage) >>= \case
    Right result -> pure result
    Left () -> inspectCompleteObject storage root <&> fmap (const ())
  where
    waitComplete storage = fix $ \next -> do
      inspectCompleteObject storage root >>= \case
        Right _ -> pure (Right ())
        Left _ -> do
          fetchHash penv root
          pause @'Seconds 0.25
          next
