module HBS2.Storage.Resource
  ( StorageProfile
  , mkStorageProfile
  , storageProfileId
  , StorageResource(..)
  , makeStorageResource
  , StorageSnapshot(..)
  ) where

import HBS2.Data.CBOR
import HBS2.Data.Types.Refs
import HBS2.Hash

import Codec.Serialise (Serialise(..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word

-- | A bounded, versioned protocol name.  Profiles are deliberately not free
-- form text: relays and receipts can carry this name without accepting an
-- unbounded application-controlled identifier.
newtype StorageProfile = StorageProfile ByteString
  deriving stock (Eq, Ord, Show)

-- | Construct a profile name from lowercase ASCII letters, digits, and
-- hyphens.  The first byte must be alphanumeric so a profile cannot look like
-- a command-line flag or an empty namespace component.
mkStorageProfile :: ByteString -> Maybe StorageProfile
mkStorageProfile name
  | isValidProfileName name = Just (StorageProfile name)
  | otherwise = Nothing
  where
    isValidProfileName bytes =
      not (BS.null bytes)
        && BS.length bytes <= 64
        && isAlphaNumeric (BS.head bytes)
        && BS.all isProfileByte bytes
    isAlphaNumeric byte = isLowercase byte || isDigit byte
    isProfileByte byte = isAlphaNumeric byte || byte == 45
    isLowercase byte = byte >= 97 && byte <= 122
    isDigit byte = byte >= 48 && byte <= 57

storageProfileId :: StorageProfile -> ByteString
storageProfileId (StorageProfile name) = name

instance Serialise StorageProfile where
  encode = encode . storageProfileId
  decode = do
    name <- decode
    maybe (fail "StorageProfile: invalid profile id") pure (mkStorageProfile name)

-- | An opaque resource identity.  Its digest binds both the profile and that
-- profile's canonical locator; code outside the profile must not infer
-- application semantics from the locator.
data StorageResource = StorageResource
  { storageResourceProfile :: StorageProfile
  , storageResourceId      :: HashRef
  }
  deriving stock (Eq, Ord, Show)

instance Serialise StorageResource where
  encode StorageResource{..} =
       encodeRecordHeader 3
    <> encode storageResourceProfile
    <> encode storageResourceId
  decode = do
    decodeRecordHeader "StorageResource" 3
    StorageResource <$> decode <*> decode

-- | Domain-separated resource identity.  The CBOR pair makes the profile and
-- locator boundary unambiguous, matching the PEP-16 resource contract.
makeStorageResource :: StorageProfile -> ByteString -> StorageResource
makeStorageResource profile locator = StorageResource profile (HashRef digest)
  where
    digest = hashObject @HbSync
      ("hbs2-storage-resource-v1\0" <> serialise (storageProfileId profile, locator))

-- | The profile-neutral evidence a provider needs to account for a complete
-- snapshot.  'storageSnapshotId' remains opaque because each profile owns its
-- authenticated source-state encoding; the inventory commitment is shared.
data StorageSnapshot = StorageSnapshot
  { storageSnapshotResource      :: StorageResource
  , storageSnapshotId            :: HashRef
  , storageSnapshotInventoryRoot :: HashRef
  , storageSnapshotBlockCount    :: Word64
  , storageSnapshotLogicalBytes  :: Word64
  , storageSnapshotCompletedAt   :: Word64
  }
  deriving stock (Eq, Show)

instance Serialise StorageSnapshot where
  encode StorageSnapshot{..} =
       encodeRecordHeader 7
    <> encode storageSnapshotResource
    <> encode storageSnapshotId
    <> encode storageSnapshotInventoryRoot
    <> encode storageSnapshotBlockCount
    <> encode storageSnapshotLogicalBytes
    <> encode storageSnapshotCompletedAt
  decode = do
    decodeRecordHeader "StorageSnapshot" 7
    StorageSnapshot <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode
