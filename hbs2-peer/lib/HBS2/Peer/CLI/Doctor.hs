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
  , DoctorFacts(..)
  , DoctorReport(..)
  , diagnose
  , doctorPassed
  , doctorJSON
  , renderDoctor
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

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

data DoctorFacts = DoctorFacts
  { doctorCLIVersion :: Text
  , doctorConfigPath :: FilePath
  , doctorConfigPresent :: Bool
  , doctorRPCSocket :: FilePath
  , doctorPoke :: Maybe Text
  , doctorPeerCount :: Maybe Int
  , doctorBootstrap :: DoctorBootstrap
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
