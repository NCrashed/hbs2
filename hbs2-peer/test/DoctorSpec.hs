{-# LANGUAGE OverloadedStrings #-}

module DoctorSpec (tests) where

import HBS2.Peer.CLI.Doctor

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.List (find)
import Data.Text qualified as Text
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "doctor"
  [ testCase "healthy runtime passes" do
      let report = diagnose healthy
      doctorPassed report @?= True
      statusOf "config" report @?= Just CheckOK
      statusOf "rpc" report @?= Just CheckOK
      statusOf "version" report @?= Just CheckOK
      statusOf "peers" report @?= Just CheckOK

  , testCase "version skew names both versions and fails" do
      let report = diagnose healthy
            { doctorPoke = Just (poke "0.25.5.0") }
          versionCheck = checkNamed "version" report
      doctorPassed report @?= False
      doctorCheckStatus versionCheck @?= CheckFailure
      assertBool "CLI version is reported"
        ("0.26.0.6" `Text.isInfixOf` doctorCheckMessage versionCheck)
      assertBool "daemon version is reported"
        ("0.25.5.0" `Text.isInfixOf` doctorCheckMessage versionCheck)

  , testCase "an unavailable RPC skips dependent checks" do
      let report = diagnose healthy
            { doctorPoke = Nothing, doctorPeerCount = Nothing }
      doctorPassed report @?= False
      statusOf "rpc" report @?= Just CheckFailure
      statusOf "version" report @?= Just CheckSkipped
      statusOf "peers" report @?= Just CheckSkipped

  , testCase "an isolated peer is a warning, not a failure" do
      let report = diagnose healthy { doctorPeerCount = Just 0 }
      doctorPassed report @?= True
      statusOf "peers" report @?= Just CheckWarning

  , testCase "an old daemon without a version fails explicitly" do
      let report = diagnose healthy
            { doctorPoke = Just "peer-key: \"peer\"\nrpc: \"/tmp/hbs2-rpc.socket\"\n" }
          versionCheck = checkNamed "version" report
      doctorPassed report @?= False
      doctorCheckStatus versionCheck @?= CheckFailure
      assertBool "upgrade remedy is present"
        (maybe False (Text.isInfixOf "restart") (doctorCheckRemedy versionCheck))

  , testCase "JSON has a stable document envelope" do
      case doctorJSON (diagnose healthy) of
        Aeson.Object obj -> do
          AesonTypes.parseMaybe (Aeson..: "contract") obj @?= Just (1 :: Int)
          AesonTypes.parseMaybe (Aeson..: "document") obj @?= Just ("doctor" :: Text.Text)
          AesonTypes.parseMaybe (Aeson..: "ok") obj @?= Just True
        _ -> assertFailure "doctor JSON is not an object"
  ]

healthy :: DoctorFacts
healthy = DoctorFacts
  { doctorCLIVersion = "0.26.0.6"
  , doctorConfigPath = "/home/alice/.config/hbs2-peer/config"
  , doctorConfigPresent = True
  , doctorRPCSocket = "/tmp/hbs2-rpc.socket"
  , doctorPoke = Just (poke "0.26.0.6")
  , doctorPeerCount = Just 2
  }

poke :: Text.Text -> Text.Text
poke version = Text.unlines
  [ "peer-key: \"peer\""
  , "rpc: \"/tmp/hbs2-rpc.socket\""
  , "version: \"" <> version <> "\""
  ]

statusOf :: Text.Text -> DoctorReport -> Maybe CheckStatus
statusOf name = fmap doctorCheckStatus . findCheck name

findCheck :: Text.Text -> DoctorReport -> Maybe DoctorCheck
findCheck name = find ((== name) . doctorCheckName) . doctorChecks

checkNamed :: Text.Text -> DoctorReport -> DoctorCheck
checkNamed name report = case findCheck name report of
  Just check -> check
  Nothing -> error ("missing doctor check: " <> Text.unpack name)
