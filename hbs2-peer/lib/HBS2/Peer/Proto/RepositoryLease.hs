{-# LANGUAGE UndecidableInstances #-}

module HBS2.Peer.Proto.RepositoryLease
  ( RepositoryLeaseId(..)
  , RepositoryLeaseState(..)
  , RepositoryInventoryEntry(..)
  , RepositorySnapshot(..)
  , RepositoryLease(..)
  , RepositoryLeasePin(..)
  , RepositoryLeaseReceipt(..)
  , ReplicaReceiptV1(..)
  , makeRepositoryLeaseId
  , makeRepositorySnapshotId
  ) where

import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Prelude.Plated

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
  , repositoryLeaseSnapshot      :: Maybe RepositorySnapshot
  , repositoryLeaseLastError     :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance Serialise RepositoryLease

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
      , (1 :: Word16) -- FullRefLogV1
      )
