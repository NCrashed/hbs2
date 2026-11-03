{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TestStorageAudit
  ( testStorageAuditAuthenticatesRequest
  , testStorageAuditBindsReply
  , testStorageAuditVerifiesBlocks
  , testStorageAuditRejectsMalformedEvidence
  , testStorageAuditKeepsWireBounds
  ) where

import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Hash
import HBS2.Net.Auth.Credentials hiding (request)
import HBS2.Storage.Audit

import Codec.Serialise (deserialiseOrFail)
import Crypto.Saltine.Core.Sign qualified as Sign
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Test.Tasty.HUnit

testStorageAuditAuthenticatesRequest :: IO ()
testStorageAuditAuthenticatesRequest = do
  Sign.Keypair requesterSk requesterPk <- Sign.newKeypair
  Sign.Keypair _ strangerPk <- Sign.newKeypair
  let request = sampleRequest
      signed = makeSignedBox @'HBS2Basic requesterPk requesterSk request
  verifyStorageAuditRequestAt 150 requesterPk signed @?= Right request
  verifyStorageAuditRequestAt 150 strangerPk signed
    @?= Left StorageAuditUnexpectedSigner
  verifyStorageAuditRequestAt 200 requesterPk signed
    @?= Left StorageAuditExpired

testStorageAuditBindsReply :: IO ()
testStorageAuditBindsReply = do
  Sign.Keypair providerSk providerPk <- Sign.newKeypair
  let request = sampleRequest
      reply = sampleReply request
      signed = makeSignedBox @'HBS2Basic providerPk providerSk reply
  verifyStorageAuditReplyAt 150 providerPk request signed @?= Right reply
  let wrong = reply { storageAuditReplyNonce = "another-nonce" }
  verifyStorageAuditReplyAt 150 providerPk request
    (makeSignedBox @'HBS2Basic providerPk providerSk wrong)
      @?= Left StorageAuditChallengeMismatch

testStorageAuditVerifiesBlocks :: IO ()
testStorageAuditVerifiesBlocks = do
  let firstBytes = "first-block"
      secondBytes = "second-block"
      first = hashed firstBytes
      second = hashed secondBytes
      request = sampleRequest
        { storageAuditSampledBlocks = [first, second] }
      reply = (sampleReply request)
        { storageAuditReplyAvailableBlocks = [first, second] }
  verifyStorageAuditBlocks request reply
    [(first, firstBytes), (second, secondBytes)] @?= Right ()
  verifyStorageAuditBlocks request reply [(first, firstBytes)]
    @?= Left (StorageAuditMissingBlock second)
  verifyStorageAuditBlocks request reply
    [(first, firstBytes), (second, "corrupt")]
      @?= Left (StorageAuditCorruptBlock second)

testStorageAuditRejectsMalformedEvidence :: IO ()
testStorageAuditRejectsMalformedEvidence = do
  Sign.Keypair requesterSk requesterPk <- Sign.newKeypair
  Sign.Keypair providerSk providerPk <- Sign.newKeypair
  Sign.Keypair _ strangerPk <- Sign.newKeypair
  let request = sampleRequest
      signRequest = makeSignedBox @'HBS2Basic requesterPk requesterSk
      reply = sampleReply request
      signReply = makeSignedBox @'HBS2Basic providerPk providerSk
  verifyStorageAuditRequestAt 150 requesterPk
    (signRequest request { storageAuditVersion = 2 })
      @?= Left StorageAuditUnsupportedVersion
  verifyStorageAuditRequestAt 150 requesterPk
    (signRequest request { storageAuditVersion = 0 })
      @?= Left StorageAuditUnsupportedVersion
  verifyStorageAuditRequestAt 150 requesterPk
    (signRequest request
      { storageAuditChallengeId = StorageAuditChallengeId "" })
      @?= Left StorageAuditInvalidChallenge
  verifyStorageAuditRequestAt 150 requesterPk
    (signRequest request { storageAuditNonce = "" })
      @?= Left StorageAuditInvalidChallenge
  verifyStorageAuditReplyAt 150 providerPk
    request
      { storageAuditChallengeId = StorageAuditChallengeId
          (BS.replicate (storageAuditMaxChallengeIdBytes + 1) 99) }
    (signReply reply)
      @?= Left StorageAuditInvalidChallenge
  verifyStorageAuditReplyAt 150 providerPk
    request
      { storageAuditNonce = BS.replicate
          (storageAuditMaxNonceBytes + 1) 110 }
    (signReply reply)
      @?= Left StorageAuditInvalidChallenge
  verifyStorageAuditRequestAt 150 requesterPk
    (signRequest request { storageAuditSampledBlocks = [] })
      @?= Left StorageAuditInvalidChallenge
  verifyStorageAuditReplyAt 150 providerPk
    request
      { storageAuditSampledBlocks = boundedHashes
          <> [hashed "one-too-many"] }
    (signReply reply)
      @?= Left StorageAuditInvalidChallenge
  verifyStorageAuditRequestAt 150 requesterPk
    (signRequest request
      { storageAuditSampledBlocks = storageAuditSampledBlocks request
          <> storageAuditSampledBlocks request })
      @?= Left StorageAuditInvalidChallenge
  verifyStorageAuditReplyAt 150 strangerPk request (signReply reply)
    @?= Left StorageAuditUnexpectedSigner
  verifyStorageAuditReplyAt 150 providerPk request
    (signReply reply { storageAuditReplyIssuedAt = 151 })
      @?= Left StorageAuditInvalidReply
  verifyStorageAuditReplyAt 150 providerPk request
    (signReply reply { storageAuditReplyIssuedAt = 200 })
      @?= Left StorageAuditExpired
  verifyStorageAuditReplyAt 150 providerPk request
    (signReply reply
      { storageAuditReplyAvailableBlocks =
          storageAuditSampledBlocks request <> storageAuditSampledBlocks request
      }) @?= Left StorageAuditInvalidReply
  verifyStorageAuditReplyAt 150 providerPk request
    (signReply reply { storageAuditReplyAvailableBlocks = [hashed "not-requested"] })
      @?= Left StorageAuditInvalidReply
  verifyStorageAuditReplyAt 150 providerPk request
    (signReply reply
      { storageAuditReplyResult = StorageAuditRejected
      , storageAuditReplyAvailableBlocks = storageAuditSampledBlocks request
      }) @?= Left StorageAuditInvalidReply

testStorageAuditKeepsWireBounds :: IO ()
testStorageAuditKeepsWireBounds = do
  Sign.Keypair requesterSk requesterPk <- Sign.newKeypair
  Sign.Keypair providerSk providerPk <- Sign.newKeypair
  let request = sampleRequest
        { storageAuditChallengeId = StorageAuditChallengeId
            (BS.replicate storageAuditMaxChallengeIdBytes 99)
        , storageAuditNonce = BS.replicate storageAuditMaxNonceBytes 110
        , storageAuditSampledBlocks = boundedHashes
        }
      unavailable = (sampleReply request)
        { storageAuditReplyResult = StorageAuditSnapshotUnavailable
        , storageAuditReplyAvailableBlocks = []
        }
  assertRoundTrip request
  assertRoundTrip unavailable
  verifyStorageAuditRequestAt 150 requesterPk
    (makeSignedBox @'HBS2Basic requesterPk requesterSk request)
      @?= Right request
  verifyStorageAuditReplyAt 150 providerPk request
    (makeSignedBox @'HBS2Basic providerPk providerSk unavailable)
      @?= Right unavailable

sampleRequest :: StorageAuditRequest
sampleRequest = StorageAuditRequest
  { storageAuditVersion = storageAuditCurrentVersion
  , storageAuditContractId = hashed "contract"
  , storageAuditSnapshotId = hashed "snapshot"
  , storageAuditChallengeId = StorageAuditChallengeId "challenge"
  , storageAuditNonce = "nonce"
  , storageAuditExpiresAt = 200
  , storageAuditSampledBlocks = [hashed "first-block"]
  }

sampleReply :: StorageAuditRequest -> StorageAuditReply
sampleReply request = StorageAuditReply
  { storageAuditReplyVersion = storageAuditCurrentVersion
  , storageAuditReplyContractId = storageAuditContractId request
  , storageAuditReplySnapshotId = storageAuditSnapshotId request
  , storageAuditReplyChallengeId = storageAuditChallengeId request
  , storageAuditReplyNonce = storageAuditNonce request
  , storageAuditReplyResult = StorageAuditReady
  , storageAuditReplyAvailableBlocks = storageAuditSampledBlocks request
  , storageAuditReplyIssuedAt = 150
  }

hashed :: LBS.ByteString -> HashRef
hashed = HashRef . hashObject @HbSync

boundedHashes :: [HashRef]
boundedHashes =
  [ hashed (LBS.singleton byte)
  | byte <- take storageAuditMaxSampledBlocks [0 ..]
  ]

assertRoundTrip :: (Eq a, Show a, Serialise a) => a -> Assertion
assertRoundTrip value =
  case deserialiseOrFail (serialise value) of
    Left err -> assertFailure (show err)
    Right decoded -> decoded @?= value
