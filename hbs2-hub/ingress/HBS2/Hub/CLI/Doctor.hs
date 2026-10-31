{-# LANGUAGE OverloadedStrings #-}

-- | @hbs2-hub doctor@: one product report over peer and forge boundaries.
--
-- D1-D3 stay owned by @hbs2-peer doctor@.  The hub consumes that command's
-- stable JSON contract and appends the facts only the forge owns: its ingress
-- mailbox, policy, contributor identity, and the strongest local evidence for
-- the latest submission.  In particular, a sent-log row is called a local
-- handoff and never delivery; there is no receipt protocol to prove the
-- intermediate network states yet.
module HBS2.Hub.CLI.Doctor
  ( doctorEntries
  , DoctorArgs(..)
  , doctorArgs
  , ForgeFacts(..)
  , ForgeMailbox(..)
  , ForgePolicy(..)
  , ForgeSender(..)
  , ForgeDelivery(..)
  , combineDoctor
  , decodePeerDoctor
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold (FoldResult(..),foldEvents)
import HBS2.Hub.Letter (sigilNames)
import HBS2.Hub.Manifest (HubMailbox(..),mailboxByTier)
import HBS2.Hub.Repo (CanonState(..),CanonUnreadable(..),readCanon)
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.Repo.Manifest (readManifest)
import HBS2.Hub.Sent (Sent(..),loadSent)
import HBS2.Hub.CLI.Argv
  ( badArgs, flagsAndSwitches, flagMaybe, flagRepo, flagSwitch, repoFlags )
import HBS2.Hub.CLI.Common (signerOf)
import HBS2.Hub.CLI.Policy (readPolicy)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Net.Auth.Credentials.Sigil (loadSigil)
import HBS2.Peer.CLI.Doctor
import HBS2.Peer.Proto.Mailbox.Policy.Basic
import HBS2.Peer.Proto.Mailbox.PoW (PoWDifficulty,maxPayableFloor)
import HBS2.Peer.RPC.API.LWWRef (LWWRefAPI)
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage
import HBS2.KeyMan.Keys.Direct (loadCredentials,runKeymanClientRO)

import Control.Exception qualified as Exception
import Data.Aeson ((.:),(.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.Maybe (fromMaybe,isJust,isNothing,maybeToList)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Word (Word64)
import System.Exit (exitFailure)
import System.Process.Typed qualified as Process
import System.Timeout qualified as Timeout

data DoctorArgs = DoctorArgs
  { daRepo   :: RepoRef
  , daAuthor :: Maybe HubKey
  , daSender :: Maybe HashRef
  , daJSON   :: Bool
  }
  deriving stock (Eq,Show)

doctorUsage :: Doc ()
doctorUsage =
  "usage: hbs2-hub doctor --repo <key> [--author <key>] [--sender <sigil>] [--json]"

doctorArgs :: forall c . IsContext c => [Syntax c] -> Maybe DoctorArgs
doctorArgs syn = do
  kvs <- flagsAndSwitches (repoFlags <> ["--author","--sender"]) ["--json"] syn
  repo <- flagRepo signKey kvs
  author <- flagMaybe kvs "--author" signKey
  sender <- flagMaybe kvs "--sender" hashRef
  asJSON <- flagSwitch kvs "--json"
  pure (DoctorArgs repo author sender asJSON)
  where
    signKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    hashRef = \case { HashLike h -> Just h ; _ -> Nothing }

data ForgeMailbox
  = ForgeMailboxUnavailable Text
  | ForgeMailboxMissing
  | ForgeMailboxFound Text (Maybe Text)
  deriving stock (Eq,Show)

data ForgePolicy
  = ForgePolicyBlocked
  | ForgePolicyUnavailable Text
  | ForgePolicyMissing
  | ForgePolicyCurrent Int Int
  deriving stock (Eq,Show)

-- | Problems make sending impossible; warnings make its outcome uncertain.
data ForgeSender
  = ForgeSenderBlocked
  | ForgeSenderChecked [Text] [Text] Text
  deriving stock (Eq,Show)

data ForgeDelivery
  = ForgeDeliveryBlocked
  | ForgeDeliveryUnavailable Text
  | ForgeNothingHanded
  | ForgeLocalHandoff Text Word64
  | ForgeCanonPublished Text Text
  deriving stock (Eq,Show)

data ForgeFacts = ForgeFacts
  { forgeMailbox  :: ForgeMailbox
  , forgePolicy   :: ForgePolicy
  , forgeSender   :: ForgeSender
  , forgeDelivery :: ForgeDelivery
  }
  deriving stock (Eq,Show)

combineDoctor :: DoctorReport -> ForgeFacts -> DoctorReport
combineDoctor peer facts = DoctorReport
  (doctorChecks peer <> forgeChecks facts)

forgeChecks :: ForgeFacts -> [DoctorCheck]
forgeChecks facts = [mailbox, policy, sender, delivery]
  where
    mailbox = mailboxCheck (forgeMailbox facts)

    mailboxReady = case forgeMailbox facts of
      ForgeMailboxFound{} -> True
      _ -> False

    policy = if mailboxReady
      then policyCheck (forgePolicy facts)
      else blocked "forge-policy" "forge mailbox"

    policyReady = mailboxReady && case forgePolicy facts of
      ForgePolicyMissing -> False
      ForgePolicyBlocked -> False
      _ -> True

    sender = if policyReady
      then senderCheck (forgeSender facts)
      else blocked "forge-sender" "forge policy"

    senderReady = policyReady && case forgeSender facts of
      ForgeSenderChecked problems _ _ -> List.null problems
      ForgeSenderBlocked -> False

    delivery = if senderReady
      then deliveryCheck (forgeDelivery facts)
      else blocked "forge-delivery" "local sender readiness"

mailboxCheck :: ForgeMailbox -> DoctorCheck
mailboxCheck = \case
  ForgeMailboxUnavailable reason -> check "forge-mailbox" CheckFailure
    ("the repository manifest could not be read: " <> reason)
    (Just "repair repository discovery first, then run `hbs2-hub doctor` again")
  ForgeMailboxMissing -> check "forge-mailbox" CheckFailure
    "the repository declares no `mailbox ... hub` ingress"
    (Just "the owner must publish a hub mailbox and its mailbox-sigil in the manifest")
  ForgeMailboxFound mailboxName tier -> check "forge-mailbox" CheckOK
    ("default ingress is " <> mailboxName
      <> maybe "" (\name -> " (tier " <> name <> ")") tier) Nothing

policyCheck :: ForgePolicy -> DoctorCheck
policyCheck = \case
  ForgePolicyBlocked -> blocked "forge-policy" "forge mailbox"
  ForgePolicyUnavailable reason -> check "forge-policy" CheckWarning
    ("current mailbox policy is not observable on this peer: " <> reason)
    (Just "host and fetch the ingress mailbox on a trusted peer to inspect its signed policy")
  ForgePolicyMissing -> check "forge-policy" CheckFailure
    "the ingress mailbox has no policy"
    (Just "the mailbox owner must publish a policy that allows intended senders and peers")
  ForgePolicyCurrent version powBits -> check "forge-policy" CheckOK
    ("policy version " <> tshow version <> ", PoW " <> tshow powBits <> " bit(s)") Nothing

senderCheck :: ForgeSender -> DoctorCheck
senderCheck = \case
  ForgeSenderBlocked -> blocked "forge-sender" "forge policy"
  ForgeSenderChecked problems warnings identity
    | not (List.null problems) -> check "forge-sender" CheckFailure
        (Text.intercalate "; " problems)
        (Just "run `hbs2-hub identity init`, then run doctor again")
    | not (List.null warnings) -> check "forge-sender" CheckWarning
        (identity <> "; " <> Text.intercalate "; " warnings)
        (Just "check the peer's outbound mailbox path before relying on delivery")
    | otherwise -> check "forge-sender" CheckOK
        (identity <> "; signing key, reply sigil, and reply mailbox are ready") Nothing

deliveryCheck :: ForgeDelivery -> DoctorCheck
deliveryCheck = \case
  ForgeDeliveryBlocked -> blocked "forge-delivery" "local sender readiness"
  ForgeDeliveryUnavailable reason -> check "forge-delivery" CheckWarning
    ("last known state is unavailable: " <> reason)
    (Just "repair the local sent log, then run doctor again")
  ForgeNothingHanded -> check "forge-delivery" CheckSkipped
    "no repository-bound letter is recorded in this node's sent log" Nothing
  ForgeLocalHandoff message handedAt -> check "forge-delivery" CheckWarning
    ("local-peer handoff at " <> tshow handedAt <> " ms: " <> message
      <> "; this is not delivery, because no signed mailbox receipt exists")
    (Just "run `hbs2-hub updates`; a future receipt protocol must prove network delivery")
  ForgeCanonPublished message event -> check "forge-delivery" CheckOK
    ("canon-published: message " <> message <> ", event " <> event) Nothing

check :: Text -> CheckStatus -> Text -> Maybe Text -> DoctorCheck
check = DoctorCheck

blocked :: Text -> Text -> DoctorCheck
blocked name boundary = check name CheckSkipped
  ("not checked because the earlier " <> boundary <> " boundary failed") Nothing

-- | Read the lower-level stable contract rather than its human rendering.
decodePeerDoctor :: LBS.ByteString -> Either Text DoctorReport
decodePeerDoctor bytes = do
  value <- firstText (Aeson.eitherDecode bytes)
  firstText (AesonTypes.parseEither parseReport value)
  where
    parseReport = Aeson.withObject "doctor report" $ \obj -> do
      contract <- obj .: "contract"
      document <- obj .: "document"
      when ((contract :: Int) /= 1) $
        fail "unsupported doctor contract"
      when ((document :: Text) /= "doctor") $
        fail "document is not doctor"
      values <- obj .: "checks"
      DoctorReport <$> traverse parseCheck values

    parseCheck = Aeson.withObject "doctor check" $ \obj -> DoctorCheck
      <$> obj .: "name"
      <*> (obj .: "status" >>= parseStatus)
      <*> obj .: "message"
      <*> obj .:? "remedy"

    parseStatus = \case
      "ok"      -> pure CheckOK
      "warning" -> pure CheckWarning
      "failure" -> pure CheckFailure
      "skipped" -> pure CheckSkipped
      other      -> fail ("unknown doctor status: " <> Text.unpack other)

    firstText = either (Left . Text.pack) Right

doctorEntries :: forall c m . ( IsContext c
                               , MonadUnliftIO m
                               , HasStorage m
                               , HasClientAPI MailboxAPI UNIX m
                               , HasClientAPI LWWRefAPI UNIX m
                               , Exception (BadFormException c)
                               ) => MakeDictM c m ()
doctorEntries =
  brief "diagnose peer, repository, forge policy, sender readiness, and last known delivery state"
    $ args [arg "string" "--repo key", arg "string" "[--author key]"
           ,arg "string" "[--sender sigil]", arg "string" "[--json]"]
    $ desc ("Runs the read-only peer doctor first, then appends forge checks."
             <> line <> "A sent-log row is reported as local-peer handoff, not delivery."
             <> line <> "Defaults for repo, author and sender are the same as issue/pr new.")
    $ entry $ bindMatch "hub:doctor" $ nil_ \case
        (doctorArgs -> Just doctorRequest) -> lift (runDoctor doctorRequest)
        other -> liftIO (badArgs doctorUsage other)
  where
    runDoctor doctorRequest = do
      peer <- liftIO (peerDoctor (daRepo doctorRequest))
      forge <- gatherForgeFacts doctorRequest
      let report = combineDoctor peer forge
      liftIO $ if daJSON doctorRequest
        then LBS.putStr (Aeson.encode (doctorJSON report))
        else TextIO.putStr (renderDoctor report)
      unless (doctorPassed report) (liftIO exitFailure)

peerDoctor :: RepoRef -> IO DoctorReport
peerDoctor repo = do
  let command = Process.proc "hbs2-peer"
        ["doctor", "--repo", Text.unpack (keyText repo), "--json"]
  attempt <- Timeout.timeout (30 * 1000 * 1000)
    (Exception.try @Exception.SomeException (Process.readProcess command))
  pure $ case attempt of
    Nothing -> failedPeer "hbs2-peer doctor timed out"
    Just (Left exception) -> failedPeer (Text.pack (show exception))
    Just (Right (_, out, errOut)) -> case decodePeerDoctor out of
      Right report -> report
      Left reason -> failedPeer (reason <> stderrNote errOut)
  where
    failedPeer reason = DoctorReport
      [check "peer-doctor" CheckFailure reason
        (Just "run `hbs2-peer doctor --repo <key> --json` directly")]
    stderrNote errOut
      | LBS.null errOut = ""
      | otherwise = "; stderr: "
          <> Text.take 512 (Text.decodeUtf8Lenient (LBS.toStrict errOut))

gatherForgeFacts :: ( MonadUnliftIO m
                    , HasStorage m
                    , HasClientAPI MailboxAPI UNIX m
                    , HasClientAPI LWWRefAPI UNIX m
                    ) => DoctorArgs -> m ForgeFacts
gatherForgeFacts doctorRequest = do
  manifest <- readManifest (daRepo doctorRequest)
  case manifest of
    Left reason -> pure ForgeFacts
      { forgeMailbox = ForgeMailboxUnavailable (prettyText reason)
      , forgePolicy = ForgePolicyBlocked
      , forgeSender = ForgeSenderBlocked
      , forgeDelivery = ForgeDeliveryBlocked
      }
    Right clauses -> case mailboxByTier Nothing clauses of
      Nothing -> pure ForgeFacts
        { forgeMailbox = ForgeMailboxMissing
        , forgePolicy = ForgePolicyBlocked
        , forgeSender = ForgeSenderBlocked
        , forgeDelivery = ForgeDeliveryBlocked
        }
      Just mailbox -> do
        (policyFact, targetPolicy) <- inspectPolicy (mbKey mailbox)
        senderFact <- inspectSender doctorRequest targetPolicy
        deliveryFact <- inspectDelivery (daRepo doctorRequest)
        pure ForgeFacts
          { forgeMailbox = ForgeMailboxFound (keyText (mbKey mailbox)) (mbTier mailbox)
          , forgePolicy = policyFact
          , forgeSender = senderFact
          , forgeDelivery = deliveryFact
          }

inspectPolicy :: ( MonadUnliftIO m
                 , HasStorage m
                 , HasClientAPI MailboxAPI UNIX m
                 ) => HubKey -> m (ForgePolicy, Maybe (BasicPolicy HubScheme))
inspectPolicy mailbox = readPolicy mailbox <&> \case
  Left reason -> (ForgePolicyUnavailable (prettyText reason), Nothing)
  Right Nothing -> (ForgePolicyMissing, Nothing)
  Right (Just (version, policy)) ->
    ( ForgePolicyCurrent (fromIntegral version) (fromIntegral (bpPoW policy))
    , Just policy
    )

inspectSender :: ( MonadUnliftIO m
                 , HasStorage m
                 , HasClientAPI MailboxAPI UNIX m
                 ) => DoctorArgs -> Maybe (BasicPolicy HubScheme) -> m ForgeSender
inspectSender doctorRequest targetPolicy = do
  storage <- getStorage
  mailboxAPI <- getClientAPI @MailboxAPI @UNIX
  signingProblem <- signingKeyProblem (daAuthor doctorRequest)
  sigilProblem <- senderSigilProblem storage
    (daAuthor doctorRequest) (daSender doctorRequest)
  replyProblems <- replyMailboxProblems (daAuthor doctorRequest)
  floorAnswer <- callRpcWaitMay @RpcMailboxPoWFloor (TimeoutSec 2) mailboxAPI ()
  let problems = configuredSenderProblems doctorRequest
        <> maybeToList signingProblem
        <> maybeToList sigilProblem
        <> replyProblems
        <> destinationPolicyProblems (daAuthor doctorRequest) targetPolicy
      warnings = outboundFloorWarnings floorAnswer
        <> destinationPolicyWarnings targetPolicy
      identity = senderIdentity (daAuthor doctorRequest) (daSender doctorRequest)
  pure (ForgeSenderChecked problems warnings identity)

signingKeyProblem :: MonadIO m => Maybe HubKey -> m (Maybe Text)
signingKeyProblem = \case
  Nothing -> pure Nothing
  Just author -> liftIO $ do
    answer <- Exception.try @Exception.SomeException
      (runKeymanClientRO (loadCredentials author))
    pure $ case answer of
      Left _ -> Just "the key database cannot be read"
      Right credentials
        | isJust (signerOf author credentials) -> Nothing
        | otherwise -> Just ("no signing key here for " <> keyText author)

senderSigilProblem :: MonadIO m
                   => AnyStorage -> Maybe HubKey -> Maybe HashRef -> m (Maybe Text)
senderSigilProblem storage authorKey senderHash = case (authorKey, senderHash) of
  (Just author, Just sender) -> loadSigil @HubScheme storage sender <&> \case
    Nothing -> Just ("sender sigil " <> hashText sender <> " is not readable here")
    Just sigil -> case sigilNames author (Just sigil) of
      Just True -> Nothing
      Just False -> Just "sender sigil names another key"
      Nothing -> Just "sender sigil has no verifiable owner"
  _ -> pure Nothing

replyMailboxProblems :: ( MonadUnliftIO m
                        , HasStorage m
                        , HasClientAPI MailboxAPI UNIX m
                        ) => Maybe HubKey -> m [Text]
replyMailboxProblems = \case
  Nothing -> pure []
  Just author -> readPolicy author <&> \case
    Left reason -> ["reply mailbox is not ready: " <> prettyText reason]
    Right Nothing -> ["reply mailbox has no policy"]
    Right (Just (_, policy)) ->
      [ "reply mailbox does not allow unknown senders"
      | bpDefaultSenderAction policy /= Allow ]
      <> [ "reply mailbox does not allow unknown replication peers"
         | bpDefaultPeerAction policy /= Allow ]

outboundFloorWarnings :: Maybe PoWDifficulty -> [Text]
outboundFloorWarnings = \case
  Nothing -> ["outbound mailbox PoW floor is unknown"]
  Just floorBits
    | floorBits > maxPayableFloor ->
        [ "outbound road asks " <> tshow floorBits <> " bits; the client cap is "
          <> tshow maxPayableFloor ]
    | otherwise -> []

destinationPolicyProblems :: Maybe HubKey -> Maybe (BasicPolicy HubScheme) -> [Text]
destinationPolicyProblems authorKey policy = case (authorKey, policy) of
  (Just author, Just current)
    | senderAction current author /= Allow ->
        ["the destination policy denies this author key"]
  _ -> []

destinationPolicyWarnings :: Maybe (BasicPolicy HubScheme) -> [Text]
destinationPolicyWarnings = \case
  Just current | bpPoW current > maxPayableFloor ->
    [ "destination policy asks " <> tshow (bpPoW current)
      <> " PoW bits, above the usual relay cap of " <> tshow maxPayableFloor
      <> "; solving may exhaust the sender's work budget" ]
  _ -> []

configuredSenderProblems :: DoctorArgs -> [Text]
configuredSenderProblems doctorRequest =
  ["no author is configured" | isNothing (daAuthor doctorRequest)]
  <> ["no sender sigil is configured" | isNothing (daSender doctorRequest)]

senderIdentity :: Maybe HubKey -> Maybe HashRef -> Text
senderIdentity authorKey senderHash = case (authorKey, senderHash) of
  (Just author, Just sender) -> keyText author <> " with " <> hashText sender
  _ -> "local contributor identity"

inspectDelivery :: MonadUnliftIO m => RepoRef -> m ForgeDelivery
inspectDelivery repo = loadSent >>= \case
  Left reason -> pure (ForgeDeliveryUnavailable reason)
  Right sent -> withGitCanon (`readCanon` repo) <&> \case
    Left NoCanonRef{} -> evidence (foldEvents repo []) sent
    Left reason -> ForgeDeliveryUnavailable
      ("canon could not be read: " <> prettyText reason)
    Right canon -> evidence (stFold canon) sent
  where
    evidence canon sent = case List.reverse [row | row <- sent, seRepo row == Just repo] of
      [] -> ForgeNothingHanded
      row : _
        | canonical canon row -> ForgeCanonPublished
            (hashText (seMessage row)) (hashText (seEvent row))
        | otherwise -> ForgeLocalHandoff (hashText (seMessage row)) (seAt row)

    canonical canon row =
      HM.member (seEvent row) (frAdmitted canon)
      || HS.member (seEvent row) (frHonoured canon)
      || HS.member (seMessage row) (frOrigins canon)

senderAction :: BasicPolicy HubScheme -> HubKey -> BasicPolicyAction
senderAction policy author =
  fromMaybe (bpDefaultSenderAction policy) (HM.lookup author (bpSenders policy))

prettyText :: Pretty a => a -> Text
prettyText = Text.pack . show . pretty

keyText :: HubKey -> Text
keyText = Text.pack . show . pretty . AsBase58

hashText :: HashRef -> Text
hashText = Text.pack . show . pretty

tshow :: Show a => a -> Text
tshow = Text.pack . show
