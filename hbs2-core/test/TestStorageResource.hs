{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TestStorageResource
  ( testStorageProfileValidation
  , testStorageResourceBindsProfileAndLocator
  , testStorageResourceRoundTrip
  , testStorageSnapshotRoundTrip
  ) where

import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Storage.Resource

import Codec.Serialise (deserialiseOrFail)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (isJust)

import Test.Tasty.HUnit

testStorageProfileValidation :: IO ()
testStorageProfileValidation = do
  assertBool "valid versioned profile is accepted" $
    isJust (mkStorageProfile "hbs2-git-repository-v1")
  mapM_ (assertBool "valid profile boundary is accepted" . isJust . mkStorageProfile)
    [ "a"
    , "z"
    , "0"
    , "9"
    , "a-b"
    , BS.replicate 64 97
    ]
  mapM_ (assertEqual "invalid profile is rejected" Nothing . mkStorageProfile)
    [ ""
    , "HBS2-git-repository-v1"
    , "-hbs2-git-repository-v1"
    , "a_b"
    , BS.replicate 65 97
    ]

testStorageResourceBindsProfileAndLocator :: IO ()
testStorageResourceBindsProfileAndLocator = do
  git <- requireProfile "hbs2-git-repository-v1"
  sync <- requireProfile "hbs2-sync-directory-v1"
  let gitResource = makeStorageResource git "repository-key"
  assertBool "same canonical input is stable" $
    gitResource == makeStorageResource git "repository-key"
  assertBool "profile is part of the resource identity" $
    gitResource /= makeStorageResource sync "repository-key"
  assertBool "locator is part of the resource identity" $
    gitResource /= makeStorageResource git "another-repository-key"

testStorageResourceRoundTrip :: IO ()
testStorageResourceRoundTrip = do
  profile <- requireProfile "hbs2-git-repository-v1"
  let resource = makeStorageResource profile "repository-key"
  decoded <- decodeValue (serialise resource)
  decoded @?= resource

testStorageSnapshotRoundTrip :: IO ()
testStorageSnapshotRoundTrip = do
  profile <- requireProfile "hbs2-git-repository-v1"
  let snapshot = StorageSnapshot
        { storageSnapshotResource = makeStorageResource profile "repository-key"
        , storageSnapshotId = hashed "snapshot"
        , storageSnapshotInventoryRoot = hashed "inventory"
        , storageSnapshotBlockCount = 3
        , storageSnapshotLogicalBytes = 99
        , storageSnapshotCompletedAt = 42
        }
  decoded <- decodeValue (serialise snapshot)
  decoded @?= snapshot

requireProfile :: BS.ByteString -> IO StorageProfile
requireProfile = maybe (assertFailure "test profile is invalid") pure . mkStorageProfile

hashed :: BS.ByteString -> HashRef
hashed = HashRef . hashObject @HbSync

decodeValue :: Serialise a => LBS.ByteString -> IO a
decodeValue bytes = either (assertFailure . show) pure (deserialiseOrFail bytes)
