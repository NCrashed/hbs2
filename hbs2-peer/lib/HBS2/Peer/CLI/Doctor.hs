{-# LANGUAGE OverloadedStrings #-}

-- | Pure decisions and render contracts for @hbs2-peer doctor@.
--
-- IO belongs to the executable: it finds the config, asks the RPC service,
-- and hands those facts here. Keeping diagnosis pure makes every boundary and
-- every exit decision testable without starting a peer.
module HBS2.Peer.CLI.Doctor
  ( CheckStatus(..)
  , DoctorCheck(..)
  , DoctorBootstrap(..)
  , DoctorConfiguredPeer(..)
  , DoctorConfiguredPeers(..)
  , DoctorPeerMetadata(..)
  , DoctorPeerMetadataStatus(..)
  , DoctorFacts(..)
  , DoctorReport(..)
  , diagnose
  , doctorPeerMetadataFacts
  , doctorPassed
  , doctorJSON
  , renderDoctor
  ) where

import Data.Aeson (Value, object, (.=))
import HBS2.Net.Proto.Types (L4Proto, PeerAddr)
import HBS2.Peer.Proto.PeerMeta
import HBS2.Prelude.Plated (fromStringMay)

import Control.Monad (guard)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8, Word16)

data CheckStatus
  = CheckOK
  | CheckWarning
  | CheckFailure
  | CheckSkipped
  deriving stock (Eq, Show)

data DoctorCheck = DoctorCheck
  { doctorCheckName :: Text
  , doctorCheckStatus :: CheckStatus
  , doctorCheckMessage :: Text
  , doctorCheckRemedy :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | The bounded DNS discovery probe performed by the executable. Counts are
-- usable peer addresses, not raw TXT records: a DNS name that answers only
-- with malformed or unsupported transports is not a route into the network.
data DoctorBootstrap
  = DoctorBootstrapDisabled
  | DoctorBootstrapChecked [(Text, Int)]
  deriving stock (Eq, Show)

-- | What the daemon proved about one configured @known-peer@ address. A
-- reachable identity comes from the signed peer handshake, not from config.
data DoctorConfiguredPeer
  = DoctorPeerInvalid Text
  | DoctorPeerUnreachable Text
  | DoctorPeerReachable Text (Maybe Text)
  deriving stock (Eq, Show)

data DoctorConfiguredPeers
  = DoctorConfiguredPeersUnavailable
  | DoctorConfiguredPeersChecked [DoctorConfiguredPeer]
  deriving stock (Eq, Show)

-- | Signed handshake routes and the last peer-meta announcement observed for
-- one connected peer. The executable validates untrusted metadata at the RPC
-- boundary; diagnosis only decides how that evidence affects user health.
data DoctorPeerMetadata = DoctorPeerMetadata
  { doctorMetadataIdentity :: Text
  , doctorMetadataAddress :: Text
  , doctorMetadataNetworkClasses :: [Text]
  , doctorMetadataObserved :: Bool
  , doctorMetadataTCPPort :: Maybe Int
  , doctorMetadataHTTPPort :: Maybe Int
  , doctorMetadataPublicAddress :: Maybe Text
  , doctorMetadataMailboxPoW :: Maybe Int
  , doctorMetadataInvalid :: [Text]
  }
  deriving stock (Eq, Show)

data DoctorPeerMetadataStatus
  = DoctorPeerMetadataUnavailable
  | DoctorPeerMetadataChecked [DoctorPeerMetadata]
  deriving stock (Eq, Show)

-- | Validate the untrusted association list once, at the RPC boundary. A
-- present-but-invalid value is different evidence from an optional key that a
-- peer did not advertise.
doctorPeerMetadataFacts
  :: Text
  -> Text
  -> [Text]
  -> Maybe PeerMeta
  -> DoctorPeerMetadata
doctorPeerMetadataFacts identity address networkClasses metadata = DoctorPeerMetadata
  { doctorMetadataIdentity = identity
  , doctorMetadataAddress = address
  , doctorMetadataNetworkClasses = networkClasses
  , doctorMetadataObserved = isJust metadata
  , doctorMetadataTCPPort = fromIntegral <$> (metadata >>= tcpPort)
  , doctorMetadataHTTPPort = fromIntegral <$> (metadata >>= httpPort)
  , doctorMetadataPublicAddress = metadata >>= publicAddress
  , doctorMetadataMailboxPoW = fromIntegral <$> (metadata >>= mailboxPoW)
  , doctorMetadataInvalid = maybe [] invalidMetadata metadata
  }

tcpPort :: PeerMeta -> Maybe Word16
tcpPort = peerMetaNat "listen-tcp"

httpPort :: PeerMeta -> Maybe Word16
httpPort = peerMetaNat "http-port"

mailboxPoW :: PeerMeta -> Maybe Word8
mailboxPoW = peerMetaNat "mailbox-pow-min"

publicAddress :: PeerMeta -> Maybe Text
publicAddress peerMetadata = do
  rawAddress <- peerMetaValue @String "public-address" peerMetadata
  guard (isJust (fromStringMay @(PeerAddr L4Proto) rawAddress))
  pure (Text.pack rawAddress)

invalidMetadata :: PeerMeta -> [Text]
invalidMetadata peerMetadata =
  invalidField peerMetadata "listen-tcp" (tcpPort peerMetadata) <>
  invalidField peerMetadata "http-port" (httpPort peerMetadata) <>
  invalidField peerMetadata "public-address" (publicAddress peerMetadata) <>
  invalidField peerMetadata "mailbox-pow-min" (mailboxPoW peerMetadata)

invalidField :: PeerMeta -> Text -> Maybe a -> [Text]
invalidField peerMetadata field parsed =
  maybe [] (const (invalidParsed field parsed))
    (lookup field (unPeerMeta peerMetadata))

invalidParsed :: Text -> Maybe a -> [Text]
invalidParsed field = maybe [field] (const [])

data DoctorFacts = DoctorFacts
  { doctorCLIVersion :: Text
  , doctorConfigPath :: FilePath
  , doctorConfigPresent :: Bool
  , doctorRPCSocket :: FilePath
  , doctorPoke :: Maybe Text
  , doctorPeerCount :: Maybe Int
  , doctorBootstrap :: DoctorBootstrap
  , doctorConfiguredPeers :: DoctorConfiguredPeers
  , doctorPeerMetadata :: DoctorPeerMetadataStatus
  }
  deriving stock (Eq, Show)

newtype DoctorReport = DoctorReport
  { doctorChecks :: [DoctorCheck]
  }
  deriving stock (Eq, Show)

diagnose :: DoctorFacts -> DoctorReport
diagnose facts = DoctorReport
  [ configCheck facts
  , rpcCheck facts
  , versionCheck facts
  , peersCheck facts
  , bootstrapCheck facts
  , configuredPeersCheck facts
  , peerMetadataCheck facts
  ]

doctorPassed :: DoctorReport -> Bool
doctorPassed = all ((/= CheckFailure) . doctorCheckStatus) . doctorChecks

doctorJSON :: DoctorReport -> Value
doctorJSON report = object
  [ "contract" .= (1 :: Int)
  , "document" .= ("doctor" :: Text)
  , "ok" .= doctorPassed report
  , "checks" .= fmap checkJSON (doctorChecks report)
  ]

renderDoctor :: DoctorReport -> Text
renderDoctor report = Text.unlines
  (concatMap renderCheck (doctorChecks report) <> [summary])
  where
    renderCheck item =
      [ "[" <> statusText (doctorCheckStatus item) <> "] "
          <> doctorCheckName item <> ": " <> doctorCheckMessage item
      ] <> maybe [] (pure . ("  remedy: " <>)) (doctorCheckRemedy item)

    failures = count CheckFailure
    warnings = count CheckWarning
    count status = length
      [ () | item <- doctorChecks report, doctorCheckStatus item == status ]
    summary
      | failures > 0 = "doctor: FAILED (" <> tshow failures <> " failed, "
                         <> tshow warnings <> " warning)"
      | warnings > 0 = "doctor: OK with " <> tshow warnings <> " warning"
      | otherwise = "doctor: OK"

configCheck :: DoctorFacts -> DoctorCheck
configCheck facts
  | doctorConfigPresent facts = mkCheck "config" CheckOK
      ("using " <> pathText (doctorConfigPath facts)) Nothing
  | otherwise = mkCheck "config" CheckWarning
      ("configuration file not found: " <> pathText (doctorConfigPath facts))
      (Just "run `hbs2-peer init`")

rpcCheck :: DoctorFacts -> DoctorCheck
rpcCheck facts = case doctorPoke facts of
  Just _ -> mkCheck "rpc" CheckOK
    ("peer answered on " <> pathText (doctorRPCSocket facts)) Nothing
  Nothing -> mkCheck "rpc" CheckFailure
    ("peer did not answer on " <> pathText (doctorRPCSocket facts))
    (Just "start it with `hbs2-peer run`, or select its socket with `--rpc PATH`")

versionCheck :: DoctorFacts -> DoctorCheck
versionCheck facts = case doctorPoke facts of
  Nothing -> skipped "version" "not checked because RPC is unavailable"
  Just answer -> case pokeField "version" answer of
    Nothing -> mkCheck "version" CheckFailure
      ("CLI is " <> doctorCLIVersion facts
        <> ", but the running peer does not report its version")
      (Just "upgrade and restart the peer daemon, then run doctor again")
    Just daemonVersion
      | daemonVersion == doctorCLIVersion facts -> mkCheck "version" CheckOK
          ("CLI and daemon are " <> daemonVersion) Nothing
      | otherwise -> mkCheck "version" CheckFailure
          ("CLI is " <> doctorCLIVersion facts <> ", daemon is " <> daemonVersion)
          (Just "run matching binaries and restart the peer daemon")

peersCheck :: DoctorFacts -> DoctorCheck
peersCheck facts = case doctorPoke facts of
  Nothing -> skipped "peers" "not checked because RPC is unavailable"
  Just _ -> case doctorPeerCount facts of
    Nothing -> mkCheck "peers" CheckFailure "the peer list RPC timed out"
      (Just "check the peer log and run `hbs2-peer peers`")
    Just 0 -> mkCheck "peers" CheckWarning "no network peers are connected"
      (Just "check bootstrap DNS, known-peer entries, and firewall reachability")
    Just n -> mkCheck "peers" CheckOK (tshow n <> " network peer(s) connected") Nothing

bootstrapCheck :: DoctorFacts -> DoctorCheck
bootstrapCheck facts = case doctorBootstrap facts of
  DoctorBootstrapDisabled ->
    skipped "bootstrap" "DNS bootstrap is disabled in the selected configuration"
  DoctorBootstrapChecked results
    | null working -> mkCheck "bootstrap" CheckFailure
        ("no usable peer addresses resolved from " <> names (fmap fst results))
        (Just "check DNS, or add a reachable `known-peer` entry to the peer config")
    | null failed -> mkCheck "bootstrap" CheckOK
        ("resolved " <> tshow (sum (fmap snd working)) <> " usable peer address(es) from "
          <> names (fmap fst working)) Nothing
    | otherwise -> mkCheck "bootstrap" CheckWarning
        ("some bootstrap names resolved; no usable peer address from "
          <> names (fmap fst failed))
        (Just "check those bootstrap-dns records, or remove names this peer should not use")
    where
      working = filter ((> 0) . snd) results
      failed = filter ((== 0) . snd) results

configuredPeersCheck :: DoctorFacts -> DoctorCheck
configuredPeersCheck facts = case doctorConfiguredPeers facts of
  DoctorConfiguredPeersUnavailable ->
    skipped "configured-peers" "not checked because RPC is unavailable"
  DoctorConfiguredPeersChecked probes -> configuredPeersResult probes

configuredPeersResult :: [DoctorConfiguredPeer] -> DoctorCheck
configuredPeersResult probes
  | null probes = skipped "configured-peers" "no known-peer entries are configured"
  | not (null invalid) = mkCheck "configured-peers" CheckFailure
      ("invalid known-peer entries: " <> names invalid)
      (Just "fix or remove those known-peer entries in the selected peer config")
  | null reached = mkCheck "configured-peers" CheckFailure
      ("no configured peer answered a signed handshake: " <> names unreachable)
      (Just "check the peer addresses, remote service, and firewall reachability")
  | not (null unreachable) = mkCheck "configured-peers" CheckWarning
      ("reached " <> tshow (length reached) <> " of " <> tshow (length probes)
        <> " configured peer(s); no answer from " <> names unreachable)
      (Just "check the unreachable peer addresses, remote services, and firewall")
  | not (null unidentified) = mkCheck "configured-peers" CheckWarning
      ("all configured peers answered, but identity was unavailable for "
        <> names unidentified)
      (Just "run doctor again; if this persists, inspect `hbs2-peer peers`")
  | otherwise = mkCheck "configured-peers" CheckOK
      ("reached " <> tshow (length reached) <> " configured peer(s): "
        <> names (fmap describe identified)) Nothing
  where
    invalid = [address | DoctorPeerInvalid address <- probes]
    unreachable = [address | DoctorPeerUnreachable address <- probes]
    reached = [() | DoctorPeerReachable _ _ <- probes]
    unidentified = [address | DoctorPeerReachable address Nothing <- probes]
    identified = [(address, identity) | DoctorPeerReachable address (Just identity) <- probes]
    describe (address, identity) = address <> " as " <> identity

peerMetadataCheck :: DoctorFacts -> DoctorCheck
peerMetadataCheck = peerMetadataStatusCheck . doctorPeerMetadata

peerMetadataStatusCheck :: DoctorPeerMetadataStatus -> DoctorCheck
peerMetadataStatusCheck DoctorPeerMetadataUnavailable =
  skipped "peer-metadata" "not checked because peer diagnostics RPC is unavailable"
peerMetadataStatusCheck (DoctorPeerMetadataChecked peers) = peerMetadataResult peers

peerMetadataResult :: [DoctorPeerMetadata] -> DoctorCheck
peerMetadataResult peers = if null peers
  then skipped "peer-metadata" "no connected peers have metadata"
  else peerMetadataPresent peers (concatMap metadataProblems peers)

peerMetadataPresent :: [DoctorPeerMetadata] -> [Text] -> DoctorCheck
peerMetadataPresent peers problems = if null problems
  then mkCheck "peer-metadata" CheckOK
      (names (fmap describeMetadata peers)) Nothing
  else mkCheck "peer-metadata" CheckWarning
      (names problems)
      (Just "run doctor again; if this persists, inspect the peer configuration and logs")

metadataProblems :: DoctorPeerMetadata -> [Text]
metadataProblems peer =
  metadataPendingProblem peer <>
  networkClassesProblem peer <>
  invalidMetadataProblem peer

metadataPendingProblem :: DoctorPeerMetadata -> [Text]
metadataPendingProblem peer =
  [doctorMetadataIdentity peer <> ": peer metadata has not been observed"
  | not (doctorMetadataObserved peer)]

networkClassesProblem :: DoctorPeerMetadata -> [Text]
networkClassesProblem peer =
  [doctorMetadataIdentity peer <> ": signed network classes are empty"
  | null (doctorMetadataNetworkClasses peer)]

invalidMetadataProblem :: DoctorPeerMetadata -> [Text]
invalidMetadataProblem peer =
  [doctorMetadataIdentity peer <> ": invalid " <> names (doctorMetadataInvalid peer)
  | not (null (doctorMetadataInvalid peer))]

describeMetadata :: DoctorPeerMetadata -> Text
describeMetadata peer = doctorMetadataIdentity peer <> " at "
  <> doctorMetadataAddress peer <> ": "
  <> names (doctorMetadataNetworkClasses peer) <> "; "
  <> port "listen TCP" (doctorMetadataTCPPort peer) <> "; "
  <> port "HTTP" (doctorMetadataHTTPPort peer) <> "; "
  <> publicAddress <> "; mailbox PoW "
  <> maybe "0" tshow (doctorMetadataMailboxPoW peer)
  where
    port label = maybe (label <> " not advertised") (\value -> label <> " " <> tshow value)
    publicAddress = maybe "public address not advertised"
      ("public " <>) (doctorMetadataPublicAddress peer)

mkCheck :: Text -> CheckStatus -> Text -> Maybe Text -> DoctorCheck
mkCheck = DoctorCheck

skipped :: Text -> Text -> DoctorCheck
skipped name message = mkCheck name CheckSkipped message Nothing

pokeField :: Text -> Text -> Maybe Text
pokeField name body = case
  [ unquote (Text.strip value)
  | line <- Text.lines body
  , Just value <- [Text.stripPrefix (name <> ":") (Text.strip line)]
  ] of
    value : _ -> Just value
    [] -> Nothing
  where
    unquote value = fromMaybe value do
      withoutOpen <- Text.stripPrefix "\"" value
      Text.stripSuffix "\"" withoutOpen

checkJSON :: DoctorCheck -> Value
checkJSON item = object
  [ "name" .= doctorCheckName item
  , "status" .= statusText (doctorCheckStatus item)
  , "message" .= doctorCheckMessage item
  , "remedy" .= doctorCheckRemedy item
  ]

statusText :: CheckStatus -> Text
statusText = \case
  CheckOK -> "ok"
  CheckWarning -> "warning"
  CheckFailure -> "failure"
  CheckSkipped -> "skipped"

pathText :: FilePath -> Text
pathText = Text.pack

names :: [Text] -> Text
names = Text.intercalate ", "

tshow :: Show a => a -> Text
tshow = Text.pack . show
