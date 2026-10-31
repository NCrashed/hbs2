{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Read-only repository discovery for @hbs2-peer doctor --repo@.
--
-- This follows the same boundaries as the git remote helper, but does not
-- fetch, subscribe, announce, or alter storage. It reports the first boundary
-- for which the selected daemon has no complete evidence.
module HBS2.Peer.CLI.Doctor.Repository
  ( doctorRepositoryProbe
  ) where

import HBS2.Base58 (AsBase58(..))
import HBS2.Clock (Timeout(..))
import HBS2.Data.Detect (readLog)
import HBS2.Data.Types.Refs (HashRef(..), pattern HashLike)
import HBS2.Data.Types.SignedBox (unboxSignedBox0)
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil (Sigil(..), loadSigil)
import HBS2.Net.Proto.Service (ServiceCaller)
import HBS2.Peer.CLI.Doctor
import HBS2.Peer.Proto.LWWRef (LWWRef(..), LWWRefKey(..))
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.Storage (StorageAPI)
import HBS2.Peer.RPC.Client (callRpcWaitMay)
import HBS2.Peer.RPC.Client.StorageClient (StorageClient(..))
import HBS2.Peer.RPC.Client.Unix (UNIX, withRPC2)
import HBS2.Prelude.Plated (fromStringMay, headMay, pretty)
import HBS2.Storage (AnyStorage(..), Storage(getBlock))
import HBS2.Storage.Operations.ByteString (TreeKey(SimpleKey), readFromMerkle)
import HBS2.Storage.Operations.Missed (findMissedBlocks)

import Control.Exception qualified as Exception
import Control.Monad.Except (runExceptT)
import Data.Config.Suckless.Script
import Data.ByteString.Lazy qualified as LBS
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import System.Timeout qualified as Timeout

data ManifestFacts = ManifestFacts
  { manifestRefLog :: PubKey 'Sign 'HBS2Basic
  , manifestMailboxes :: [PubKey 'Sign 'HBS2Basic]
  , manifestSigils :: [(PubKey 'Sign 'HBS2Basic, HashRef)]
  }

type RepositoryStage input =
  AnyStorage
  -> ServiceCaller RefLogAPI UNIX
  -> PubKey 'Sign 'HBS2Basic
  -> input
  -> IO DoctorRepository

doctorRepositoryProbe :: FilePath -> Maybe Text -> IO DoctorRepository
doctorRepositoryProbe _ Nothing = pure DoctorRepositoryNotRequested
doctorRepositoryProbe socketPath (Just rawKey) = case parseRepositoryKey rawKey of
  Nothing -> pure (DoctorRepositoryInvalid rawKey)
  Just repositoryKey -> boundedRepositoryProbe socketPath repositoryKey

parseRepositoryKey :: Text -> Maybe (PubKey 'Sign 'HBS2Basic)
parseRepositoryKey = fromStringMay . Text.unpack

boundedRepositoryProbe
  :: FilePath
  -> PubKey 'Sign 'HBS2Basic
  -> IO DoctorRepository
boundedRepositoryProbe socketPath repositoryKey = do
  result <- Timeout.timeout repositoryProbeMicros
    (Exception.try @Exception.SomeException
      (repositoryProbe socketPath repositoryKey))
  pure $ case result of
    Just (Right facts) -> facts
    _ -> DoctorRepositoryUnavailable (keyText repositoryKey)

repositoryProbeMicros :: Int
repositoryProbeMicros = 10 * 1000 * 1000

repositoryProbe
  :: FilePath
  -> PubKey 'Sign 'HBS2Basic
  -> IO DoctorRepository
repositoryProbe socketPath repositoryKey =
  withRPC2 @LWWRefAPI @UNIX socketPath $ \lwwAPI ->
    withRPC2 @RefLogAPI @UNIX socketPath $ \refLogAPI ->
      withRPC2 @StorageAPI @UNIX socketPath $ \storageAPI -> do
        let storage = AnyStorage (StorageClient storageAPI)
        callRpcWaitMay @RpcLWWRefGet (TimeoutSec 1) lwwAPI
          (LWWRefKey repositoryKey) >>= \case
            Nothing -> pure (DoctorRepositoryUnavailable (keyText repositoryKey))
            Just Nothing -> pure (checked repositoryKey DoctorLWWRefMissing)
            Just (Just lwwRef) -> probeManifest storage refLogAPI repositoryKey lwwRef

probeManifest :: RepositoryStage (LWWRef 'HBS2Basic)
probeManifest storage refLogAPI repositoryKey lwwRef = do
  missingLog <- findMissedBlocks storage (lwwValue lwwRef)
  if null missingLog
    then do
      roots <- readLog (getBlock storage) (lwwValue lwwRef)
      case headMay roots of
        Nothing -> pure . checked repositoryKey . DoctorLWWRefFound $
          DoctorManifestMalformed "the manifest log is empty"
        Just root -> probeManifestTree storage refLogAPI repositoryKey root
    else pure . checked repositoryKey . DoctorLWWRefFound . DoctorManifestMissing $
      missingMessage "manifest log" missingLog

probeManifestTree :: RepositoryStage HashRef
probeManifestTree storage refLogAPI repositoryKey root = do
  missingTree <- findMissedBlocks storage root
  if null missingTree
    then do
      bytes <- runExceptT (readFromMerkle storage (SimpleKey (fromHashRef root)))
      case bytes of
        Left errorValue -> malformed ("the manifest tree cannot be read: "
          <> Text.pack (show errorValue))
        Right content -> case parseManifest content of
          Left reason -> malformed reason
          Right manifest -> probeRefLog storage refLogAPI repositoryKey manifest
    else pure . checked repositoryKey . DoctorLWWRefFound . DoctorManifestMissing $
      missingMessage "manifest tree" missingTree
  where
    malformed reason = pure . checked repositoryKey . DoctorLWWRefFound $
      DoctorManifestMalformed reason

parseManifest :: LBS.ByteString -> Either Text ManifestFacts
parseManifest bytes = case parseTop manifestText of
  Left _ -> Left "the manifest is not valid S-expression syntax"
  Right clauses -> manifestFacts clauses
  where
    manifestText = Text.unpack (TextEncoding.decodeUtf8Lenient (LBS.toStrict bytes))

manifestFacts :: [Syntax C] -> Either Text ManifestFacts
manifestFacts clauses = case refLogs of
  [] -> Left "the manifest has no `reflog` clause"
  _ -> Right ManifestFacts
    { manifestRefLog = last refLogs
    , manifestMailboxes = nub mailboxes
    , manifestSigils = sigils
    }
  where
    refLogs = [refLogKey | ListVal [SymbolVal "reflog", SignPubKeyLike refLogKey] <- clauses]
    mailboxes =
      [mailboxKey |
        ListVal (SymbolVal "mailbox" : SignPubKeyLike mailboxKey : StringLike _ : _) <- clauses]
    sigils =
      [(mailboxKey, hash) |
        ListVal (SymbolVal "mailbox-sigil" : SignPubKeyLike mailboxKey : HashLike hash : _) <- clauses]

probeRefLog :: RepositoryStage ManifestFacts
probeRefLog storage refLogAPI repositoryKey manifest =
  callRpcWaitMay @RpcRefLogGet (TimeoutSec 1) refLogAPI
    (manifestRefLog manifest) >>= \case
      Nothing -> pure (DoctorRepositoryUnavailable (keyText repositoryKey))
      Just Nothing -> pure . manifestFound repositoryKey $
        DoctorRefLogMissing (keyText (manifestRefLog manifest))
      Just (Just root) -> probeSnapshot storage repositoryKey manifest root

probeSnapshot
  :: AnyStorage
  -> PubKey 'Sign 'HBS2Basic
  -> ManifestFacts
  -> HashRef
  -> IO DoctorRepository
probeSnapshot storage repositoryKey manifest root = do
  missing <- findMissedBlocks storage root
  if null missing
    then do
      let required =
            [pair | pair@(mailbox, _) <- manifestSigils manifest
                  , mailbox `elem` manifestMailboxes manifest]
          missingDeclarations =
            ["mailbox " <> keyText mailbox <> " declares no sigil"
            | mailbox <- manifestMailboxes manifest
            , not (any ((== mailbox) . fst) (manifestSigils manifest))]
      declaredProblems <- concat <$> traverse (sigilProblems storage) required
      let problems = missingDeclarations <> declaredProblems
          sigils = if null problems
            then DoctorSigilsComplete
              (length (manifestMailboxes manifest))
              (length required)
            else DoctorSigilsIncomplete problems
      pure . manifestFound repositoryKey . DoctorRefLogFound (hashText root) $
        DoctorSnapshotComplete sigils
    else pure . manifestFound repositoryKey . DoctorRefLogFound (hashText root) $
      DoctorSnapshotIncomplete (length missing)

sigilProblems
  :: AnyStorage
  -> (PubKey 'Sign 'HBS2Basic, HashRef)
  -> IO [Text]
sigilProblems storage (mailbox, hash) = do
  missing <- findMissedBlocks storage hash
  if null missing
    then sigilContentsProblem storage mailbox hash
    else pure ["sigil " <> hashText hash <> " is incomplete ("
      <> countText (length missing) <> " missing block(s))"]

sigilContentsProblem
  :: AnyStorage
  -> PubKey 'Sign 'HBS2Basic
  -> HashRef
  -> IO [Text]
sigilContentsProblem storage mailbox hash =
  loadSigil @'HBS2Basic storage hash >>= \case
    Nothing -> pure ["sigil " <> hashText hash <> " is malformed"]
    Just sigil -> pure (sigilOwnerProblem mailbox hash sigil)

sigilOwnerProblem
  :: PubKey 'Sign 'HBS2Basic
  -> HashRef
  -> Sigil 'HBS2Basic
  -> [Text]
sigilOwnerProblem mailbox hash sigil = case fst <$> unboxSignedBox0 (sigilData sigil) of
  Nothing -> ["sigil " <> hashText hash <> " has an invalid signature"]
  Just owner
    | owner == mailbox -> []
    | otherwise -> ["sigil " <> hashText hash <> " names " <> keyText owner
        <> ", not mailbox " <> keyText mailbox]

checked :: PubKey 'Sign 'HBS2Basic -> DoctorLWWRef -> DoctorRepository
checked repositoryKey = DoctorRepositoryChecked (keyText repositoryKey)

manifestFound :: PubKey 'Sign 'HBS2Basic -> DoctorRefLog -> DoctorRepository
manifestFound repositoryKey = checked repositoryKey . DoctorLWWRefFound
  . DoctorManifestFound

missingMessage :: Text -> [HashRef] -> Text
missingMessage boundary missing = boundary <> " is incomplete: "
  <> countText (length missing) <> " block(s) missing; first is "
  <> maybe "unknown" hashText (headMay missing)

keyText :: PubKey 'Sign 'HBS2Basic -> Text
keyText = Text.pack . show . pretty . AsBase58

hashText :: HashRef -> Text
hashText = Text.pack . show . pretty

countText :: Show a => a -> Text
countText = Text.pack . show
