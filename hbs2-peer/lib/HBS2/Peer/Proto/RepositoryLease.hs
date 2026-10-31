{-# LANGUAGE UndecidableInstances #-}

module HBS2.Peer.Proto.RepositoryLease
  ( RepositoryLeaseId(..)
  , RepositoryLeaseState(..)
  , RepositoryLeaseLifetime(..)
  , RepositoryInventoryEntry(..)
  , RepositorySnapshot(..)
  , RepositoryLease(..)
  , RepositoryLeasePin(..)
  , RepositoryLeaseReceipt(..)
  , ReplicaReceiptV1(..)
  , repositoryLeaseCurrentVersion
  , repositoryLeaseVersionSupported
  , repositoryLeaseLifetimeAt
  , makeRepositoryLeaseId
  , makeRepositorySnapshotId
  ) where

import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Net.Auth.Credentials hiding (encode)
import HBS2.Prelude.Plated

import Codec.CBOR.Term (decodeTerm, encodeTerm)
import Codec.CBOR.Write (toLazyByteString)
import Codec.Serialise (Serialise(..), deserialiseOrFail)
import Data.Word

newtype RepositoryLeaseId = RepositoryLeaseId
  { fromRepositoryLeaseId :: HashRef
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Serialise, Pretty, Hashable, FromStringMaybe, IsString)

instance Show RepositoryLeaseId where
  show = show . pretty

data RepositoryLeaseState
  = RepositoryLeaseSyncing
  | RepositoryLeaseActive
  | RepositoryLeaseDegraded
  | RepositoryLeaseGrace
  | RepositoryLeaseTerminated
  deriving stock (Eq, Ord, Show, Generic)

instance Serialise RepositoryLeaseState

data RepositoryLeaseLifetime
  = RepositoryLeaseUnexpired
  | RepositoryLeaseInGrace
  | RepositoryLeaseElapsed
  deriving stock (Eq, Ord, Show)

-- | Canonically ordered inventory entry committed by the snapshot.  The
-- pin tree is separate because its leaves must remain plain HashRefs for the
-- storage root walker.
data RepositoryInventoryEntry = RepositoryInventoryEntry
  { repositoryInventoryHash   :: HashRef
  , repositoryInventoryLength :: Word64
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Serialise RepositoryInventoryEntry

-- | One locally verified complete repository state.
data RepositorySnapshot = RepositorySnapshot
  { repositorySnapshotId            :: HashRef
  , repositorySnapshotLWWRefBlock   :: HashRef
  , repositorySnapshotLWWSequence   :: Word64
  , repositorySnapshotHeadRoot      :: HashRef
  , repositorySnapshotRefLogKey     :: PubKey 'Sign 'HBS2Basic
  , repositorySnapshotRefLogRoot    :: HashRef
  , repositorySnapshotInventoryRoot :: HashRef
  , repositorySnapshotPinRoot       :: HashRef
  , repositorySnapshotBlockCount    :: Word64
  , repositorySnapshotLogicalBytes  :: Word64
  , repositorySnapshotCompletedAt   :: Word64
  }
  deriving stock (Eq, Show, Generic)

instance Serialise RepositorySnapshot

-- | Durable local M1 lease.  Bilateral signatures and credit are added by
-- PEP-08; this record is deliberately provider-local.
data RepositoryLease = RepositoryLease
  { repositoryLeaseVersion       :: Word16
  , repositoryLeaseId            :: RepositoryLeaseId
  , repositoryLeaseRepository    :: PubKey 'Sign 'HBS2Basic
  , repositoryLeaseState         :: RepositoryLeaseState
  , repositoryLeaseMaxBytes      :: Word64
  , repositoryLeaseMaxBlocks     :: Word64
  , repositoryLeaseCreatedAt     :: Word64
  , repositoryLeaseUpdatedAt     :: Word64
  , repositoryLeaseExpiresAt     :: Maybe Word64
  , repositoryLeaseGracePeriod   :: Word64
  , repositoryLeaseSnapshot      :: Maybe RepositorySnapshot
  , repositoryLeaseLastError     :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

-- Version 1 records had no grace-period field.  These private wire records
-- deliberately use the generic codec that encoded the original durable type.
-- Decoding through a CBOR term lets us safely try both shapes without relying
-- on decoder backtracking.
data RepositoryLeaseWireV1 = RepositoryLeaseWireV1
  Word16
  RepositoryLeaseId
  (PubKey 'Sign 'HBS2Basic)
  RepositoryLeaseState
  Word64
  Word64
  Word64
  Word64
  (Maybe Word64)
  (Maybe RepositorySnapshot)
  (Maybe Text)
  deriving stock (Generic)

instance Serialise RepositoryLeaseWireV1

data RepositoryLeaseWireV2 = RepositoryLeaseWireV2
  Word16
  RepositoryLeaseId
  (PubKey 'Sign 'HBS2Basic)
  RepositoryLeaseState
  Word64
  Word64
  Word64
  Word64
  (Maybe Word64)
  Word64
  (Maybe RepositorySnapshot)
  (Maybe Text)
  deriving stock (Generic)

instance Serialise RepositoryLeaseWireV2

instance Serialise RepositoryLease where
  encode = encode . repositoryLeaseToWireV2
  decode = do
    bytes <- toLazyByteString . encodeTerm <$> decodeTerm
    case deserialiseOrFail @RepositoryLeaseWireV2 bytes of
      Right wire -> pure (repositoryLeaseFromWireV2 wire)
      Left _ -> case deserialiseOrFail @RepositoryLeaseWireV1 bytes of
        Right wire -> pure (repositoryLeaseFromWireV1 wire)
        Left failure -> fail (show failure)

repositoryLeaseToWireV2 :: RepositoryLease -> RepositoryLeaseWireV2
repositoryLeaseToWireV2 RepositoryLease{..} = RepositoryLeaseWireV2
  repositoryLeaseVersion
  repositoryLeaseId
  repositoryLeaseRepository
  repositoryLeaseState
  repositoryLeaseMaxBytes
  repositoryLeaseMaxBlocks
  repositoryLeaseCreatedAt
  repositoryLeaseUpdatedAt
  repositoryLeaseExpiresAt
  repositoryLeaseGracePeriod
  repositoryLeaseSnapshot
  repositoryLeaseLastError

repositoryLeaseFromWireV1 :: RepositoryLeaseWireV1 -> RepositoryLease
repositoryLeaseFromWireV1
  (RepositoryLeaseWireV1 version leaseId repository state maxBytes maxBlocks
    createdAt updatedAt expiresAt snapshot lastError) = RepositoryLease
      version leaseId repository state maxBytes maxBlocks createdAt updatedAt
      expiresAt 0 snapshot lastError

repositoryLeaseFromWireV2 :: RepositoryLeaseWireV2 -> RepositoryLease
repositoryLeaseFromWireV2
  (RepositoryLeaseWireV2 version leaseId repository state maxBytes maxBlocks
    createdAt updatedAt expiresAt grace snapshot lastError) = RepositoryLease
      version leaseId repository state maxBytes maxBlocks createdAt updatedAt
      expiresAt grace snapshot lastError

repositoryLeaseCurrentVersion :: Word16
repositoryLeaseCurrentVersion = 2

repositoryLeaseVersionSupported :: Word16 -> Bool
repositoryLeaseVersionSupported version =
  version == 1 || version == repositoryLeaseCurrentVersion

repositoryLeaseLifetimeAt
  :: Word64
  -> RepositoryLease
  -> RepositoryLeaseLifetime
repositoryLeaseLifetimeAt now lease =
  case repositoryLeaseExpiresAt lease of
    Nothing -> RepositoryLeaseUnexpired
    Just expiresAt
      | now < expiresAt -> RepositoryLeaseUnexpired
      | now < graceDeadline -> RepositoryLeaseInGrace
      | otherwise -> RepositoryLeaseElapsed
      where
        grace = repositoryLeaseGracePeriod lease
        graceDeadline = expiresAt + min grace (maxBound - expiresAt)

-- | Local mutable ref which makes the inventory tree a storage root.
newtype RepositoryLeasePin = RepositoryLeasePin RepositoryLeaseId
  deriving stock (Eq, Ord, Generic)

instance RefMetaData RepositoryLeasePin where
  refMetaData _ = [("kind", "repository-lease-pin")]

instance Hashed HbSync RepositoryLeasePin where
  hashObject (RepositoryLeasePin leaseId) =
    hashObject ("hbs2-repository-lease-pin-v1\0" <> serialise leaseId)

instance Pretty RepositoryLeasePin where
  pretty (RepositoryLeasePin leaseId) = "repository-lease-pin:" <> pretty leaseId

-- | Durable local ref to the signed receipt block for a lease.  Keeping the
-- receipt in its own root namespace makes it survive ordinary storage GC and
-- lets recovery verify the Active invariant without trusting the state DB.
newtype RepositoryLeaseReceipt = RepositoryLeaseReceipt RepositoryLeaseId
  deriving stock (Eq, Ord, Generic)

instance RefMetaData RepositoryLeaseReceipt where
  refMetaData _ = [("kind", "repository-lease-receipt")]

instance Hashed HbSync RepositoryLeaseReceipt where
  hashObject (RepositoryLeaseReceipt leaseId) =
    hashObject ("hbs2-repository-lease-receipt-v1\0" <> serialise leaseId)

instance Pretty RepositoryLeaseReceipt where
  pretty (RepositoryLeaseReceipt leaseId) =
    "repository-lease-receipt:" <> pretty leaseId

-- | Provider-signed evidence that one exact complete snapshot was pinned.
-- This is deliberately a versioned payload: the enclosing SignedBox encoding
-- is already wire-frozen, while later receipt versions may add policy fields.
data ReplicaReceiptV1 = ReplicaReceiptV1
  { replicaReceiptVersion       :: Word16
  , replicaReceiptLeaseId       :: RepositoryLeaseId
  , replicaReceiptRepository    :: PubKey 'Sign 'HBS2Basic
  , replicaReceiptProvider      :: PubKey 'Sign 'HBS2Basic
  , replicaReceiptSnapshotId    :: HashRef
  , replicaReceiptInventoryRoot :: HashRef
  , replicaReceiptBlockCount    :: Word64
  , replicaReceiptLogicalBytes  :: Word64
  , replicaReceiptCompletedAt   :: Word64
  , replicaReceiptIssuedAt      :: Word64
  }
  deriving stock (Eq, Show, Generic)

instance Serialise ReplicaReceiptV1

makeRepositoryLeaseId
  :: PubKey 'Sign 'HBS2Basic
  -> Word64
  -> RepositoryLeaseId
makeRepositoryLeaseId repository nonce = RepositoryLeaseId . HashRef $
  hashObject @HbSync
    ("hbs2-repository-lease-v1\0" <> serialise (repository, nonce))

makeRepositorySnapshotId
  :: PubKey 'Sign 'HBS2Basic
  -> HashRef
  -> Word64
  -> HashRef
  -> PubKey 'Sign 'HBS2Basic
  -> HashRef
  -> HashRef
makeRepositorySnapshotId repository lwwBlock lwwSeq headRoot refLog refLogRoot =
  HashRef $ hashObject @HbSync bytes
  where
    bytes = "hbs2-repository-snapshot-v1\0" <> payload
    payload = serialise
      ( repository
      , lwwBlock
      , lwwSeq
      , headRoot
      , refLog
      , refLogRoot
      , 1 :: Word16 -- FullRefLogV1
      )
