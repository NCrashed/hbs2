{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module StorageAuditRetrievalSpec (tests) where

import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Peer.StorageAudit
import HBS2.Storage.Audit

import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Map.Strict qualified as Map
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "targeted storage audit retrieval"
  [ testCase "fetches every sample from the supplied transport in order" $ do
      fetched <- newIORef []
      let request = sampleRequest [firstHash, secondHash]
          reply = readyReply request
          transport block = do
            modifyIORef' fetched (<> [block])
            pure $ maybe (Left "missing") Right $ Map.lookup block blocks
      retrieveStorageAuditBlocks transport request reply
        >>= (@?= Right [(firstHash, firstBytes), (secondHash, secondBytes)])
      readIORef fetched >>= (@?= [firstHash, secondHash])
  , testCase "attributes a transport failure to its challenged block" $ do
      let request = sampleRequest [firstHash, secondHash]
          reply = readyReply request
          transport block
            | block == firstHash = pure (Right firstBytes)
            | otherwise = pure (Left "provider timeout")
      retrieveStorageAuditBlocks transport request reply
        >>= (@?= Left (StorageAuditTransportError secondHash "provider timeout"))
  , testCase "does not accept corrupt provider bytes" $ do
      let request = sampleRequest [firstHash]
          reply = readyReply request
      retrieveStorageAuditBlocks (const $ pure $ Right "corrupt") request reply
        >>= (@?= Left
          (StorageAuditEvidenceError (StorageAuditCorruptBlock firstHash)))
  , testCase "does not turn an unavailable reply into retrieval evidence" $ do
      calls <- newIORef (0 :: Int)
      let request = sampleRequest [firstHash]
          reply = (readyReply request)
            { storageAuditReplyResult = StorageAuditSnapshotUnavailable
            , storageAuditReplyAvailableBlocks = []
            }
          transport _ = modifyIORef' calls succ >> pure (Right firstBytes)
      retrieveStorageAuditBlocks transport request reply
        >>= (@?= Left (StorageAuditEvidenceError StorageAuditInvalidReply))
      readIORef calls >>= (@?= 0)
  ]

sampleRequest :: [HashRef] -> StorageAuditRequest
sampleRequest blocks' = StorageAuditRequest
  { storageAuditVersion = storageAuditCurrentVersion
  , storageAuditContractId = hashed "contract"
  , storageAuditSnapshotId = hashed "snapshot"
  , storageAuditChallengeId = StorageAuditChallengeId "challenge"
  , storageAuditNonce = "nonce"
  , storageAuditExpiresAt = 200
  , storageAuditSampledBlocks = blocks'
  }

readyReply :: StorageAuditRequest -> StorageAuditReply
readyReply request = StorageAuditReply
  { storageAuditReplyVersion = storageAuditCurrentVersion
  , storageAuditReplyContractId = storageAuditContractId request
  , storageAuditReplySnapshotId = storageAuditSnapshotId request
  , storageAuditReplyChallengeId = storageAuditChallengeId request
  , storageAuditReplyNonce = storageAuditNonce request
  , storageAuditReplyResult = StorageAuditReady
  , storageAuditReplyAvailableBlocks = storageAuditSampledBlocks request
  , storageAuditReplyIssuedAt = 150
  }

firstBytes, secondBytes :: LBS.ByteString
firstBytes = "first-block"
secondBytes = "second-block"

firstHash, secondHash :: HashRef
firstHash = hashed firstBytes
secondHash = hashed secondBytes

blocks :: Map.Map HashRef LBS.ByteString
blocks = Map.fromList [(firstHash, firstBytes), (secondHash, secondBytes)]

hashed :: LBS.ByteString -> HashRef
hashed = HashRef . hashObject @HbSync
