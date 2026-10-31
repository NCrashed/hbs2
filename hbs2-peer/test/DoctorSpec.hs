{-# LANGUAGE OverloadedStrings #-}

module DoctorSpec (tests) where

import HBS2.Peer.CLI.Doctor
import HBS2.Peer.Proto.PeerMeta (PeerMeta(..))

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
      statusOf "peer-metadata" report @?= Just CheckOK
      statusOf "repository-key" report @?= Just CheckSkipped
      statusOf "repository-lwwref" report @?= Just CheckSkipped
      statusOf "repository-manifest" report @?= Just CheckSkipped
      statusOf "repository-reflog" report @?= Just CheckSkipped
      statusOf "repository-snapshot" report @?= Just CheckSkipped
      statusOf "repository-sigils" report @?= Just CheckSkipped

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

  , testCase "peer metadata reports advertised routes and relay policy" do
      let metadata = checkNamed "peer-metadata" (diagnose healthy)
      doctorCheckStatus metadata @?= CheckOK
      assertBool "the signed network class is visible"
        ("Clearnet" `Text.isInfixOf` doctorCheckMessage metadata)
      assertBool "the advertised TCP listener is visible"
        ("listen TCP 7351" `Text.isInfixOf` doctorCheckMessage metadata)
      assertBool "the relay mailbox floor is visible"
        ("mailbox PoW 18" `Text.isInfixOf` doctorCheckMessage metadata)

  , testCase "malformed peer metadata is an actionable warning" do
      let report = diagnose healthy
            { doctorPeerMetadata = DoctorPeerMetadataChecked
                [ healthyPeerMetadata
                    { doctorMetadataIdentity = "bad-relay"
                    , doctorMetadataInvalid = ["listen-tcp", "mailbox-pow-min"]
                    }
                ]
            }
          metadata = checkNamed "peer-metadata" report
      doctorPassed report @?= True
      doctorCheckStatus metadata @?= CheckWarning
      assertBool "the peer identity is visible"
        ("bad-relay" `Text.isInfixOf` doctorCheckMessage metadata)
      assertBool "the malformed keys are visible"
        ("listen-tcp" `Text.isInfixOf` doctorCheckMessage metadata
          && "mailbox-pow-min" `Text.isInfixOf` doctorCheckMessage metadata)

  , testCase "metadata still in flight is not mistaken for an empty policy" do
      let report = diagnose healthy
            { doctorPeerMetadata = DoctorPeerMetadataChecked
                [healthyPeerMetadata { doctorMetadataObserved = False }] }
          metadata = checkNamed "peer-metadata" report
      doctorCheckStatus metadata @?= CheckWarning
      assertBool "the remedy asks for another observation"
        (maybe False (Text.isInfixOf "again") (doctorCheckRemedy metadata))

  , testCase "missing signed network classes is a warning" do
      let report = diagnose healthy
            { doctorPeerMetadata = DoctorPeerMetadataChecked
                [healthyPeerMetadata { doctorMetadataNetworkClasses = [] }] }
          metadata = checkNamed "peer-metadata" report
      doctorCheckStatus metadata @?= CheckWarning
      assertBool "the identity with the bad handshake is visible"
        ("relay-signing-key" `Text.isInfixOf` doctorCheckMessage metadata)

  , testCase "peer metadata is skipped when its RPC is unavailable" do
      let report = diagnose healthy
            { doctorPeerMetadata = DoctorPeerMetadataUnavailable }
      doctorPassed report @?= True
      statusOf "peer-metadata" report @?= Just CheckSkipped

  , testCase "a complete repository names the local peer as a provider" do
      let report = diagnose healthy
            { doctorRepository = completeRepository (DoctorSigilsComplete 1 2) }
          snapshot = checkNamed "repository-snapshot" report
          sigils = checkNamed "repository-sigils" report
      doctorPassed report @?= True
      doctorCheckStatus snapshot @?= CheckOK
      assertBool "complete local storage is provider evidence"
        ("complete local provider" `Text.isInfixOf` doctorCheckMessage snapshot)
      doctorCheckStatus sigils @?= CheckOK
      assertBool "required sigil count is visible"
        ("2 required sigil" `Text.isInfixOf` doctorCheckMessage sigils)

  , testCase "a missing LWWRef is the first failed repository boundary" do
      let report = diagnose healthy
            { doctorRepository = DoctorRepositoryChecked "repo-key" DoctorLWWRefMissing }
      doctorPassed report @?= False
      statusOf "repository-key" report @?= Just CheckOK
      statusOf "repository-lwwref" report @?= Just CheckFailure
      statusOf "repository-manifest" report @?= Just CheckSkipped
      statusOf "repository-reflog" report @?= Just CheckSkipped
      statusOf "repository-snapshot" report @?= Just CheckSkipped
      statusOf "repository-sigils" report @?= Just CheckSkipped

  , testCase "an incomplete snapshot reports the missing block count" do
      let report = diagnose healthy
            { doctorRepository = repositorySnapshot (DoctorSnapshotIncomplete 7) }
          snapshot = checkNamed "repository-snapshot" report
      doctorPassed report @?= False
      statusOf "repository-reflog" report @?= Just CheckOK
      doctorCheckStatus snapshot @?= CheckFailure
      assertBool "the root is visible"
        ("reflog-root" `Text.isInfixOf` doctorCheckMessage snapshot)
      assertBool "the missing count is visible"
        ("7 block" `Text.isInfixOf` doctorCheckMessage snapshot)
      statusOf "repository-sigils" report @?= Just CheckSkipped

  , testCase "missing required sigils fail after a complete snapshot" do
      let report = diagnose healthy
            { doctorRepository = completeRepository (DoctorSigilsIncomplete
                ["mailbox-a declares no sigil", "sigil-hash is not on this peer"]) }
          sigils = checkNamed "repository-sigils" report
      doctorPassed report @?= False
      statusOf "repository-snapshot" report @?= Just CheckOK
      doctorCheckStatus sigils @?= CheckFailure
      assertBool "every sigil problem is named"
        ("mailbox-a" `Text.isInfixOf` doctorCheckMessage sigils
          && "sigil-hash" `Text.isInfixOf` doctorCheckMessage sigils)

  , testCase "a repository with no mailbox needs no sigil" do
      let report = diagnose healthy
            { doctorRepository = completeRepository (DoctorSigilsComplete 0 0) }
      doctorPassed report @?= True
      statusOf "repository-sigils" report @?= Just CheckSkipped

  , testCase "a malformed repository key stops before RPC" do
      let report = diagnose healthy
            { doctorRepository = DoctorRepositoryInvalid "not-a-key" }
      doctorPassed report @?= False
      statusOf "repository-key" report @?= Just CheckFailure
      statusOf "repository-lwwref" report @?= Just CheckSkipped

  , testCase "unavailable repository RPC is a failed required boundary" do
      let report = diagnose healthy
            { doctorRepository = DoctorRepositoryUnavailable "repo-key" }
      doctorPassed report @?= False
      statusOf "repository-key" report @?= Just CheckOK
      statusOf "repository-lwwref" report @?= Just CheckFailure
      statusOf "repository-manifest" report @?= Just CheckSkipped

  , testCase "untrusted peer metadata is validated at the boundary" do
      let facts = doctorPeerMetadataFacts "relay" "relay:7351" ["Clearnet"] . Just $
            PeerMeta
              [ ("listen-tcp", "-1")
              , ("http-port", "70000")
              , ("public-address", "\"not an address\"")
              , ("mailbox-pow-min", "300")
              ]
      doctorMetadataTCPPort facts @?= Nothing
      doctorMetadataHTTPPort facts @?= Nothing
      doctorMetadataPublicAddress facts @?= Nothing
      doctorMetadataMailboxPoW facts @?= Nothing
      doctorMetadataInvalid facts @?=
        ["listen-tcp", "http-port", "public-address", "mailbox-pow-min"]

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
  , doctorPeerMetadata = DoctorPeerMetadataChecked [healthyPeerMetadata]
  , doctorRepository = DoctorRepositoryNotRequested
  }

healthyPeerMetadata :: DoctorPeerMetadata
healthyPeerMetadata = DoctorPeerMetadata
  { doctorMetadataIdentity = "relay-signing-key"
  , doctorMetadataAddress = "8dePeer@udp"
  , doctorMetadataNetworkClasses = ["Clearnet"]
  , doctorMetadataObserved = True
  , doctorMetadataTCPPort = Just 7351
  , doctorMetadataHTTPPort = Nothing
  , doctorMetadataPublicAddress = Just "81.88.219.217:7351"
  , doctorMetadataMailboxPoW = Just 18
  , doctorMetadataInvalid = []
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

repositorySnapshot :: DoctorSnapshot -> DoctorRepository
repositorySnapshot = DoctorRepositoryChecked "repo-key" . DoctorLWWRefFound
  . DoctorManifestFound . DoctorRefLogFound "reflog-root"

completeRepository :: DoctorSigils -> DoctorRepository
completeRepository = repositorySnapshot . DoctorSnapshotComplete
