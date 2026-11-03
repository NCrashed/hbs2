module StorageAuditTransport
  ( downloadStorageAuditBlocksFromPeer
  ) where

import HBS2.Data.Types.Refs (HashRef, fromHashRef)
import HBS2.Net.Auth.Credentials (L4Proto)
import HBS2.Net.Proto.Types (Peer)
import HBS2.Peer.StorageAudit
import HBS2.Storage.Audit

import BlockDownloadNew
import HBS2.Actors.Peer (PeerEnv)

import Data.ByteString.Lazy qualified as LBS
import UnliftIO (MonadUnliftIO)

-- | Retrieve an audit sample from exactly the selected provider.  Unlike the
-- recursive replica downloader, 'downloadFromPeer' does not accept an existing
-- local storage block as proof that this peer served it.
downloadStorageAuditBlocksFromPeer
  :: (MonadUnliftIO m, BlockSizeCache L4Proto cache)
  => Int
  -> cache
  -> PeerEnv L4Proto
  -> Peer L4Proto
  -> StorageAuditRequest
  -> StorageAuditReply
  -> m (Either (StorageAuditRetrievalError (DownloadError L4Proto))
               [(HashRef, LBS.ByteString)])
downloadStorageAuditBlocksFromPeer burst cache env provider =
  retrieveStorageAuditBlocks $ \block ->
    downloadFromPeer burst cache env (fromHashRef block) provider
