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
      statusOf "bootstrap" report @?= Just CheckOK
      statusOf "configured-peers" report @?= Just CheckOK

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

  , testCase "bootstrap DNS with no usable records fails with a remedy" do
      let report = diagnose healthy
            { doctorBootstrap = DoctorBootstrapChecked
                [("bootstrap.hbs2.app", 0)] }
          bootstrapCheck = checkNamed "bootstrap" report
      doctorPassed report @?= False
      doctorCheckStatus bootstrapCheck @?= CheckFailure
      assertBool "the failed name is visible"
        ("bootstrap.hbs2.app" `Text.isInfixOf` doctorCheckMessage bootstrapCheck)
      assertBool "the remedy says how to bypass DNS discovery"
        (maybe False (Text.isInfixOf "known-peer") (doctorCheckRemedy bootstrapCheck))

  , testCase "a partial bootstrap result is an actionable warning" do
      let report = diagnose healthy
            { doctorBootstrap = DoctorBootstrapChecked
                [("bootstrap.hbs2.app", 2), ("relay.example", 0)] }
          bootstrapCheck = checkNamed "bootstrap" report
      doctorPassed report @?= True
      doctorCheckStatus bootstrapCheck @?= CheckWarning
      assertBool "the unresolved name is visible"
        ("relay.example" `Text.isInfixOf` doctorCheckMessage bootstrapCheck)

  , testCase "an explicitly disabled bootstrap is skipped" do
      let report = diagnose healthy
            { doctorBootstrap = DoctorBootstrapDisabled }
      doctorPassed report @?= True
      statusOf "bootstrap" report @?= Just CheckSkipped

  , testCase "reachable configured peers name their verified identities" do
      let report = diagnose healthy
            { doctorConfiguredPeers = DoctorConfiguredPeersChecked
                [ DoctorPeerReachable "8dePeer@udp" (Just "relay-signing-key") ] }
          configured = checkNamed "configured-peers" report
      doctorCheckStatus configured @?= CheckOK
      assertBool "the verified identity is visible"
        ("relay-signing-key" `Text.isInfixOf` doctorCheckMessage configured)

  , testCase "partial configured-peer reachability is a warning" do
      let report = diagnose healthy
            { doctorConfiguredPeers = DoctorConfiguredPeersChecked
                [ DoctorPeerReachable "relay-a" (Just "key-a")
                , DoctorPeerUnreachable "relay-b"
                ] }
          configured = checkNamed "configured-peers" report
      doctorPassed report @?= True
      doctorCheckStatus configured @?= CheckWarning
      assertBool "the unreachable address is visible"
        ("relay-b" `Text.isInfixOf` doctorCheckMessage configured)

  , testCase "no reachable configured peer is a failure" do
      let report = diagnose healthy
            { doctorConfiguredPeers = DoctorConfiguredPeersChecked
                [DoctorPeerUnreachable "relay-a"] }
          configured = checkNamed "configured-peers" report
      doctorPassed report @?= False
      doctorCheckStatus configured @?= CheckFailure
      assertBool "the remedy points at the network boundary"
        (maybe False (Text.isInfixOf "firewall") (doctorCheckRemedy configured))

  , testCase "an invalid known-peer entry is a configuration failure" do
      let report = diagnose healthy
            { doctorConfiguredPeers = DoctorConfiguredPeersChecked
                [DoctorPeerInvalid "not-a-peer-address"] }
          configured = checkNamed "configured-peers" report
      doctorPassed report @?= False
      doctorCheckStatus configured @?= CheckFailure
      assertBool "the invalid entry is visible"
        ("not-a-peer-address" `Text.isInfixOf` doctorCheckMessage configured)

  , testCase "no configured peers is explicitly skipped" do
      let report = diagnose healthy
            { doctorConfiguredPeers = DoctorConfiguredPeersChecked [] }
      doctorPassed report @?= True
      statusOf "configured-peers" report @?= Just CheckSkipped

  , testCase "configured peers are skipped when RPC is unavailable" do
      let report = diagnose healthy
            { doctorPoke = Nothing
            , doctorPeerCount = Nothing
            , doctorConfiguredPeers = DoctorConfiguredPeersUnavailable
            }
      statusOf "configured-peers" report @?= Just CheckSkipped

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
  , doctorBootstrap = DoctorBootstrapChecked [("bootstrap.hbs2.app", 1)]
  , doctorConfiguredPeers = DoctorConfiguredPeersChecked
      [DoctorPeerReachable "8dePeer@udp" (Just "relay-signing-key")]
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
