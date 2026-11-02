module HBS2.Storage.Audit
  ( StorageAuditChallengeId(..)
  , StorageAuditRequest(..)
  , StorageAuditReplyResult(..)
  , StorageAuditReply(..)
  , StorageAuditError(..)
  , storageAuditCurrentVersion
  , storageAuditMaxChallengeIdBytes
  , storageAuditMaxNonceBytes
  , storageAuditMaxSampledBlocks
  , verifyStorageAuditRequestAt
  , verifyStorageAuditReplyAt
  , verifyStorageAuditBlocks
  ) where

import HBS2.Data.CBOR
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Hash
import HBS2.Net.Auth.Credentials hiding (decode, encode, request)

import Codec.CBOR.Decoding (Decoder, decodeListLen, decodeWord)
import Codec.CBOR.Encoding (Encoding, encodeListLen, encodeWord)
import Codec.Serialise (Serialise(..))
import Control.Monad (replicateM, unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (for_)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word

-- | Opaque identifier chosen by the auditor.  It is separate from the nonce:
-- the id correlates local journal entries, while the nonce prevents replay.
newtype StorageAuditChallengeId = StorageAuditChallengeId ByteString
  deriving stock (Eq, Ord, Show)

data StorageAuditRequest = StorageAuditRequest
  { storageAuditVersion       :: Word16
  , storageAuditContractId    :: HashRef
  , storageAuditSnapshotId    :: HashRef
  , storageAuditChallengeId   :: StorageAuditChallengeId
  , storageAuditNonce         :: ByteString
  , storageAuditExpiresAt     :: Word64
  , storageAuditSampledBlocks :: [HashRef]
  }
  deriving stock (Eq, Show)

data StorageAuditReplyResult
  = StorageAuditReady
  | StorageAuditSnapshotUnavailable
  | StorageAuditRejected
  deriving stock (Eq, Ord, Show)

data StorageAuditReply = StorageAuditReply
  { storageAuditReplyVersion         :: Word16
  , storageAuditReplyContractId      :: HashRef
  , storageAuditReplySnapshotId      :: HashRef
  , storageAuditReplyChallengeId     :: StorageAuditChallengeId
  , storageAuditReplyNonce           :: ByteString
  , storageAuditReplyResult          :: StorageAuditReplyResult
  , storageAuditReplyAvailableBlocks :: [HashRef]
  , storageAuditReplyIssuedAt        :: Word64
  }
  deriving stock (Eq, Show)

data StorageAuditError
  = StorageAuditUnexpectedSigner
  | StorageAuditUnsupportedVersion
  | StorageAuditInvalidChallenge
  | StorageAuditExpired
  | StorageAuditChallengeMismatch
  | StorageAuditInvalidReply
  | StorageAuditMissingBlock HashRef
  | StorageAuditCorruptBlock HashRef
  | StorageAuditUnexpectedBlock HashRef
  deriving stock (Eq, Show)

storageAuditCurrentVersion :: Word16
storageAuditCurrentVersion = 1

storageAuditMaxChallengeIdBytes :: Int
storageAuditMaxChallengeIdBytes = 32

storageAuditMaxNonceBytes :: Int
storageAuditMaxNonceBytes = 32

storageAuditMaxSampledBlocks :: Int
storageAuditMaxSampledBlocks = 16

instance Serialise StorageAuditChallengeId where
  encode (StorageAuditChallengeId challenge) = encode challenge
  decode = do
    challenge <- decode
    when (BS.length challenge > storageAuditMaxChallengeIdBytes) $
      fail "StorageAuditChallengeId: too long"
    pure (StorageAuditChallengeId challenge)

instance Serialise StorageAuditRequest where
  encode StorageAuditRequest{..} =
       encodeRecordHeader 8
    <> encode storageAuditVersion
    <> encode storageAuditContractId
    <> encode storageAuditSnapshotId
    <> encode storageAuditChallengeId
    <> encode storageAuditNonce
    <> encode storageAuditExpiresAt
    <> encodeHashList storageAuditSampledBlocks
  decode = do
    decodeRecordHeader "StorageAuditRequest" 8
    StorageAuditRequest
      <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode
      <*> decodeHashList

instance Serialise StorageAuditReplyResult where
  encode = encodeWord . \case
    StorageAuditReady -> 0
    StorageAuditSnapshotUnavailable -> 1
    StorageAuditRejected -> 2
  decode = decodeWord >>= \case
    0 -> pure StorageAuditReady
    1 -> pure StorageAuditSnapshotUnavailable
    2 -> pure StorageAuditRejected
    _ -> fail "StorageAuditReplyResult: invalid tag"

instance Serialise StorageAuditReply where
  encode StorageAuditReply{..} =
       encodeRecordHeader 9
    <> encode storageAuditReplyVersion
    <> encode storageAuditReplyContractId
    <> encode storageAuditReplySnapshotId
    <> encode storageAuditReplyChallengeId
    <> encode storageAuditReplyNonce
    <> encode storageAuditReplyResult
    <> encodeHashList storageAuditReplyAvailableBlocks
    <> encode storageAuditReplyIssuedAt
  decode = do
    decodeRecordHeader "StorageAuditReply" 9
    StorageAuditReply
      <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode
      <*> decodeHashList <*> decode

encodeHashList :: [HashRef] -> Encoding
encodeHashList hashes =
  encodeListLen (fromIntegral (length hashes)) <> foldMap encode hashes

decodeHashList :: Decoder s [HashRef]
decodeHashList = do
  count <- decodeListLen
  when (count > storageAuditMaxSampledBlocks) $
    fail "StorageAudit: too many sampled blocks"
  replicateM count decode

verifyStorageAuditRequestAt
  :: Word64
  -> PubKey 'Sign 'HBS2Basic
  -> SignedBox StorageAuditRequest 'HBS2Basic
  -> Either StorageAuditError StorageAuditRequest
verifyStorageAuditRequestAt now requester signed = do
  (signer, request) <- maybe (Left StorageAuditUnexpectedSigner) Right
    (unboxSignedBox0 signed)
  unless (signer == requester) (Left StorageAuditUnexpectedSigner)
  validateRequest now request
  pure request

verifyStorageAuditReplyAt
  :: Word64
  -> PubKey 'Sign 'HBS2Basic
  -> StorageAuditRequest
  -> SignedBox StorageAuditReply 'HBS2Basic
  -> Either StorageAuditError StorageAuditReply
verifyStorageAuditReplyAt now provider request signed = do
  validateRequest now request
  (signer, reply) <- maybe (Left StorageAuditUnexpectedSigner) Right
    (unboxSignedBox0 signed)
  unless (signer == provider) (Left StorageAuditUnexpectedSigner)
  unless (storageAuditReplyVersion reply == storageAuditCurrentVersion) $
    Left StorageAuditUnsupportedVersion
  unless (replyChallenge reply == requestChallenge request) $
    Left StorageAuditChallengeMismatch
  when (storageAuditReplyIssuedAt reply >= storageAuditExpiresAt request) $
    Left StorageAuditExpired
  when (storageAuditReplyIssuedAt reply > now) $
    Left StorageAuditInvalidReply
  let requested = Set.fromList (storageAuditSampledBlocks request)
      available = storageAuditReplyAvailableBlocks reply
  when (hasDuplicates available || any (`Set.notMember` requested) available) $
    Left StorageAuditInvalidReply
  case storageAuditReplyResult reply of
    StorageAuditReady
      | Set.fromList available /= requested -> Left StorageAuditInvalidReply
    result
      | result /= StorageAuditReady && not (null available) ->
          Left StorageAuditInvalidReply
    _ -> pure ()
  pure reply

verifyStorageAuditBlocks
  :: StorageAuditRequest
  -> StorageAuditReply
  -> [(HashRef, LBS.ByteString)]
  -> Either StorageAuditError ()
verifyStorageAuditBlocks request reply fetched = do
  let requested = storageAuditSampledBlocks request
      requestedSet = Set.fromList requested
      fetchedMap = Map.fromList fetched
  case find (`Set.notMember` requestedSet) (fmap fst fetched) of
    Just unexpected -> Left (StorageAuditUnexpectedBlock unexpected)
    Nothing -> pure ()
  when (Map.size fetchedMap /= length fetched) $
    Left StorageAuditInvalidReply
  for_ requested $ \href -> do
    unless (href `elem` storageAuditReplyAvailableBlocks reply) $
      Left (StorageAuditMissingBlock href)
    bytes <- maybe (Left (StorageAuditMissingBlock href)) Right
      (Map.lookup href fetchedMap)
    unless (HashRef (hashObject @HbSync bytes) == href) $
      Left (StorageAuditCorruptBlock href)

validateRequest :: Word64 -> StorageAuditRequest -> Either StorageAuditError ()
validateRequest now request = do
  unless (storageAuditVersion request == storageAuditCurrentVersion) $
    Left StorageAuditUnsupportedVersion
  let StorageAuditChallengeId challenge = storageAuditChallengeId request
      nonce = storageAuditNonce request
      samples = storageAuditSampledBlocks request
  when (BS.null challenge
        || BS.length challenge > storageAuditMaxChallengeIdBytes
        || BS.null nonce
        || BS.length nonce > storageAuditMaxNonceBytes
        || null samples
        || length samples > storageAuditMaxSampledBlocks
        || hasDuplicates samples) $
    Left StorageAuditInvalidChallenge
  when (now >= storageAuditExpiresAt request) (Left StorageAuditExpired)

requestChallenge
  :: StorageAuditRequest
  -> (HashRef, HashRef, StorageAuditChallengeId, ByteString)
requestChallenge request =
  ( storageAuditContractId request
  , storageAuditSnapshotId request
  , storageAuditChallengeId request
  , storageAuditNonce request
  )

replyChallenge
  :: StorageAuditReply
  -> (HashRef, HashRef, StorageAuditChallengeId, ByteString)
replyChallenge reply =
  ( storageAuditReplyContractId reply
  , storageAuditReplySnapshotId reply
  , storageAuditReplyChallengeId reply
  , storageAuditReplyNonce reply
  )

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates values = Set.size (Set.fromList values) /= length values
