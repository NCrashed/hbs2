module PeerConfigSpec (tests) where

import HBS2.Net.Auth.Credentials

import PeerConfig

import Data.ByteString qualified as BS
import Lens.Micro.Platform (view)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "peer init credentials"
  [ testCase "a fresh peer has an encryption key" do
      withSystemTempDirectory "hbs2-peer-init" $ \dir -> do
        peerConfigInit (Just dir)
        credentials <- readCredentials (dir </> "default.key")
        assertEqual "one encryption key" 1 (length (view peerKeyring credentials))

  , testCase "re-running init preserves the existing keyring" do
      withSystemTempDirectory "hbs2-peer-init" $ \dir -> do
        peerConfigInit (Just dir)
        before <- BS.readFile (dir </> "default.key")
        peerConfigInit (Just dir)
        afterInit <- BS.readFile (dir </> "default.key")
        assertEqual "default.key is unchanged" before afterInit
  ]

readCredentials :: FilePath -> IO (PeerCredentials 'HBS2Basic)
readCredentials path = do
  raw <- BS.readFile path
  maybe (assertFailure "default.key is not a credentials file") pure
    (parseCredentials (AsCredFile raw))
