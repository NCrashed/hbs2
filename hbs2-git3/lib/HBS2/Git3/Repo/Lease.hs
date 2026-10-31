module HBS2.Git3.Repo.Lease
  ( addRepositoryLease
  , listRepositoryLeasesCLI
  , showRepositoryLease
  , showRepositoryLeaseReceipt
  , terminateRepositoryLease
  , inventoryRepository
  , activeRepositoryLease
  ) where

import HBS2.Git3.Prelude
import HBS2.Git3.Repo.Lease.Policy
import HBS2.Git3.Repo.Tools
import HBS2.Git3.State
import HBS2.Peer.Proto.RepositoryLease
import HBS2.Peer.RepositoryLease
import HBS2.Storage.Inventory

import Data.Config.Suckless.Script
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import System.Random

addRepositoryLease :: forall m . HBS2GitPerks m => [Syntax C] -> Git3 m ()
addRepositoryLease syntax = do
  let (options, repositorySyntax) = splitOpts
        [ ("--max-bytes", 1)
        , ("--max-blocks", 1)
        , ("--ttl", 1)
        , ("--grace", 1)
        ] syntax

      maxBytes = optionWord "--max-bytes" defaultLeaseMaxBytes options
      maxBlocks = optionWord "--max-blocks" defaultLeaseMaxBlocks options
      ttl = optionWordMay "--ttl" options
      grace = optionWord "--grace" defaultLeaseGracePeriod options

  repository <- resolveRepoKeyThrow repositorySyntax
  now <- liftIO (floor <$> getPOSIXTime)
  nonce <- liftIO (randomIO @Word64)
  peerAPI <- getClientAPI @PeerAPI @UNIX
  leases <- leaseList peerAPI

  let previous = find ((== repository) . repositoryLeaseRepository) leases
      lease = renewRepositoryLeaseAt
        now nonce repository maxBytes maxBlocks ttl grace previous

  callRpcWaitMay @RpcRepositoryLeasePut (TimeoutSec 2) peerAPI lease
    >>= orThrowUser "repository lease RPC timed out"
  callRpcWaitMay @RpcPollAdd (TimeoutSec 2) peerAPI (repository, "lwwref", 1)
    >>= orThrowUser "repository polling RPC timed out"
  printLease lease

listRepositoryLeasesCLI :: forall m . HBS2GitPerks m => Git3 m ()
listRepositoryLeasesCLI = do
  peerAPI <- getClientAPI @PeerAPI @UNIX
  leaseList peerAPI >>= mapM_ printLease

showRepositoryLease :: forall m . HBS2GitPerks m => [Syntax C] -> Git3 m ()
showRepositoryLease syntax = do
  lease <- resolveLease syntax
  printLease lease

showRepositoryLeaseReceipt :: forall m . HBS2GitPerks m => [Syntax C] -> Git3 m ()
showRepositoryLeaseReceipt syntax = do
  lease <- resolveLease syntax
  storage <- getStorage
  result <- liftIO $
    readRepositoryReplicaReceipt storage (repositoryLeaseId lease)
  (receiptHash, signer, ReplicaReceiptV1{..}) <- case result of
    Left problem -> orThrowUser (viaShow problem) Nothing
    Right value -> pure value
  liftIO $ print $ parens $
       "replica-receipt-v1"
    <+> parens ("hash" <+> pretty receiptHash)
    <+> parens ("lease" <+> pretty replicaReceiptLeaseId)
    <+> parens ("repository" <+> pretty (AsBase58 replicaReceiptRepository))
    <+> parens ("provider" <+> pretty (AsBase58 signer))
    <+> parens ("snapshot" <+> pretty replicaReceiptSnapshotId)
    <+> parens ("inventory" <+> pretty replicaReceiptInventoryRoot)
    <+> parens ("blocks" <+> pretty replicaReceiptBlockCount)
    <+> parens ("bytes" <+> pretty replicaReceiptLogicalBytes)
    <+> parens ("completed-at" <+> pretty replicaReceiptCompletedAt)
    <+> parens ("issued-at" <+> pretty replicaReceiptIssuedAt)

terminateRepositoryLease :: forall m . HBS2GitPerks m => [Syntax C] -> Git3 m ()
terminateRepositoryLease syntax = do
  lease <- resolveLease syntax
  now <- liftIO (floor <$> getPOSIXTime)
  peerAPI <- getClientAPI @PeerAPI @UNIX
  storage <- getStorage
  let terminated = lease
        { repositoryLeaseState = RepositoryLeaseTerminated
        , repositoryLeaseUpdatedAt = now
        , repositoryLeaseLastError = Nothing
        }
  callRpcWaitMay @RpcRepositoryLeasePut (TimeoutSec 2) peerAPI terminated
    >>= orThrowUser "repository lease RPC timed out"
  liftIO $ releaseRepositoryLeasePin storage (repositoryLeaseId lease)
  liftIO $ releaseRepositoryReplicaReceipt storage (repositoryLeaseId lease)
  printLease terminated

inventoryRepository :: forall m . HBS2GitPerks m => [Syntax C] -> Git3 m ()
inventoryRepository syntax = do
  repository <- resolveRepoKeyThrow syntax
  peerAPI <- getClientAPI @PeerAPI @UNIX
  storage <- getStorage
  leases <- leaseList peerAPI
  let limits = case find ((== repository) . repositoryLeaseRepository) leases of
        Just lease -> InventoryLimits
          (repositoryLeaseMaxBlocks lease) (repositoryLeaseMaxBytes lease)
        Nothing -> InventoryLimits defaultLeaseMaxBlocks defaultLeaseMaxBytes
  discovered <- liftIO $ discoverRepositorySnapshot limits storage repository
  candidate <- case discovered of
    Left problem -> orThrowUser (viaShow problem) Nothing
    Right value  -> pure value
  liftIO $ print $ parens $
       "repository-inventory"
    <+> pretty (AsBase58 repository)
    <+> parens ("snapshot" <+> pretty (candidateSnapshotId candidate))
    <+> parens ("blocks" <+> pretty (Map.size (candidateInventory candidate)))
    <+> parens ("bytes" <+> pretty (candidateLogicalBytes candidate))

activeRepositoryLease
  :: forall m . HBS2GitPerks m
  => GitRepoKey
  -> Git3 m (Maybe RepositoryLease)
activeRepositoryLease repository = do
  now <- liftIO (floor <$> getPOSIXTime)
  peerAPI <- getClientAPI @PeerAPI @UNIX
  selectActiveRepositoryLeaseAt now repository <$> leaseList peerAPI

resolveLease :: forall m . HBS2GitPerks m => [Syntax C] -> Git3 m RepositoryLease
resolveLease syntax = do
  peerAPI <- getClientAPI @PeerAPI @UNIX
  leases <- leaseList peerAPI
  let byId = case syntax of
        [StringLike value] -> do
          leaseId <- fromStringMay @RepositoryLeaseId value
          find ((== leaseId) . repositoryLeaseId) leases
        _ -> Nothing
  case byId of
    Just lease -> pure lease
    Nothing -> do
      repository <- resolveRepoKeyThrow syntax
      find ((== repository) . repositoryLeaseRepository) leases
        & orThrowUser "repository lease not found"

leaseList
  :: MonadUnliftIO m
  => ServiceCaller PeerAPI UNIX
  -> m [RepositoryLease]
leaseList peerAPI =
  callRpcWaitMay @RpcRepositoryLeaseList (TimeoutSec 2) peerAPI ()
    >>= orThrowUser "repository lease RPC timed out"

optionWord :: Id -> Word64 -> [Syntax c] -> Word64
optionWord name fallback options = fromMaybe fallback (optionWordMay name options)

optionWordMay :: Id -> [Syntax c] -> Maybe Word64
optionWordMay name options = lastMay
  [ parsed
  | ListVal [SymbolVal option, LitIntVal value] <- options
  , option == name
  , Just parsed <- [positiveWord64 value]
  ]

printLease :: MonadIO m => RepositoryLease -> m ()
printLease RepositoryLease{..} = liftIO $ print $ parens $
     "repository-lease"
  <+> pretty repositoryLeaseId
  <+> parens ("repository" <+> pretty (AsBase58 repositoryLeaseRepository))
  <+> parens ("state" <+> pretty (leaseStateName repositoryLeaseState))
  <+> parens ("max-bytes" <+> pretty repositoryLeaseMaxBytes)
  <+> parens ("max-blocks" <+> pretty repositoryLeaseMaxBlocks)
  <+> parens ("grace" <+> pretty repositoryLeaseGracePeriod)
  <+> maybe mempty (\expiresAt ->
        parens ("expires-at" <+> pretty expiresAt))
      repositoryLeaseExpiresAt
  <+> maybe mempty (\snapshot ->
        parens ("snapshot" <+> pretty (repositorySnapshotId snapshot))
        <+> parens ("bytes" <+> pretty (repositorySnapshotLogicalBytes snapshot))
        <+> parens ("blocks" <+> pretty (repositorySnapshotBlockCount snapshot)))
      repositoryLeaseSnapshot
  <+> maybe mempty (\problem -> parens ("error" <+> dquotes (pretty problem)))
      repositoryLeaseLastError
