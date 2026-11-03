module HBS2.Peer.StorageAudit
  ( StorageAuditRetrievalError(..)
  , retrieveStorageAuditBlocks
  ) where

import HBS2.Data.Types.Refs (HashRef)
import HBS2.Storage.Audit

import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LBS

data StorageAuditRetrievalError transportError
  = StorageAuditTransportError HashRef transportError
  | StorageAuditEvidenceError StorageAuditError
  deriving stock (Eq, Show)

-- | Fetch every sampled block through the caller-supplied targeted transport,
-- then verify the returned bytes against the challenge.  In particular, the
-- fetch action must not substitute a local CAS hit for evidence from the
-- selected provider.
retrieveStorageAuditBlocks
  :: Monad m
  => (HashRef -> m (Either transportError LBS.ByteString))
  -> StorageAuditRequest
  -> StorageAuditReply
  -> m (Either (StorageAuditRetrievalError transportError)
               [(HashRef, LBS.ByteString)])
retrieveStorageAuditBlocks fetch request reply =
  case storageAuditReplyResult reply of
    StorageAuditReady -> go [] (storageAuditSampledBlocks request)
    _ -> pure $ Left $ StorageAuditEvidenceError StorageAuditInvalidReply
  where
    go fetched [] = do
      let ordered = reverse fetched
      pure $ ordered <$ first StorageAuditEvidenceError
        (verifyStorageAuditBlocks request reply ordered)
    go fetched (block : rest) = do
      result <- fetch block
      case result of
        Left problem ->
          pure $ Left $ StorageAuditTransportError block problem
        Right bytes -> go ((block, bytes) : fetched) rest
