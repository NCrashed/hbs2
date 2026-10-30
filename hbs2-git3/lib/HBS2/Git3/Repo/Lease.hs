module HBS2.Git3.Repo.Lease
  ( addRepositoryLease
  , listRepositoryLeasesCLI
  , showRepositoryLease
  , terminateRepositoryLease
  , inventoryRepository
  , activeRepositoryLease
  ) where

import HBS2.Git3.Prelude
import HBS2.Git3.Repo.Tools
import HBS2.Git3.State
import HBS2.Peer.Proto.RepositoryLease
import HBS2.Peer.RepositoryLease
import HBS2.Storage.Inventory

import Data.Config.Suckless.Script
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Word
import System.Random

defaultLeaseMaxBytes, defaultLeaseMaxBlocks :: Word64
defaultLeaseMaxBytes = 10 * 1024 * 1024 * 1024
defaultLeaseMaxBlocks = 10 * 1000 * 1000

addRepositoryLease :: forall m . HBS2GitPerks m => [Syntax C] -> Git3 m ()
addRepositoryLease syntax = do
  let (options, repositorySyntax) = splitOpts
        [ ("--max-bytes", 1)
        , ("--max-blocks", 1)
        , ("--ttl", 1)
        ] syntax

      maxBytes = optionWord "--max-bytes" defaultLeaseMaxBytes options
      maxBlocks = optionWord "--max-blocks" defaultLeaseMaxBlocks options
      ttl = optionWordMay "--ttl" options

  repository <- resolveRepoKeyThrow repositorySyntax
  now <- liftIO (floor <$> getPOSIXTime)
  nonce <- liftIO (randomIO @Word64)
  peerAPI <- getClientAPI @PeerAPI @UNIX
  leases <- leaseList peerAPI

  let previous = find ((== repository) . repositoryLeaseRepository) leases
      leaseId = maybe (makeRepositoryLeaseId repository nonce) repositoryLeaseId previous
      createdAt = maybe now repositoryLeaseCreatedAt previous
      previousSnapshot = previous >>= repositoryLeaseSnapshot
      nextState = case previousSnapshot of
        Nothing -> RepositoryLeaseSyncing
        Just{}  -> RepositoryLeaseDegraded
      lease = RepositoryLease
        { repositoryLeaseVersion = 1
        , repositoryLeaseId = leaseId
        , repositoryLeaseRepository = repository
        , repositoryLeaseState = nextState
        , repositoryLeaseMaxBytes = maxBytes
        , repositoryLeaseMaxBlocks = maxBlocks
        , repositoryLeaseCreatedAt = createdAt
        , repositoryLeaseUpdatedAt = now
        , repositoryLeaseExpiresAt = (+ now) <$> ttl
        , repositoryLeaseSnapshot = previousSnapshot
        , repositoryLeaseLastError = Nothing
        }

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
  peerAPI <- getClientAPI @PeerAPI @UNIX
  find isActive <$> leaseList peerAPI
  where
    isActive lease =
         repositoryLeaseRepository lease == repository
      && repositoryLeaseState lease /= RepositoryLeaseTerminated

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
  [ fromIntegral value
  | ListVal [SymbolVal option, LitIntVal value] <- options
  , option == name
  , value > 0
  ]

printLease :: MonadIO m => RepositoryLease -> m ()
printLease RepositoryLease{..} = liftIO $ print $ parens $
     "repository-lease"
  <+> pretty repositoryLeaseId
  <+> parens ("repository" <+> pretty (AsBase58 repositoryLeaseRepository))
  <+> parens ("state" <+> pretty (leaseStateName repositoryLeaseState))
  <+> parens ("max-bytes" <+> pretty repositoryLeaseMaxBytes)
  <+> parens ("max-blocks" <+> pretty repositoryLeaseMaxBlocks)
  <+> maybe mempty (\snapshot ->
        parens ("snapshot" <+> pretty (repositorySnapshotId snapshot))
        <+> parens ("bytes" <+> pretty (repositorySnapshotLogicalBytes snapshot))
        <+> parens ("blocks" <+> pretty (repositorySnapshotBlockCount snapshot)))
      repositoryLeaseSnapshot
  <+> maybe mempty (\problem -> parens ("error" <+> dquotes (pretty problem)))
      repositoryLeaseLastError

leaseStateName :: RepositoryLeaseState -> String
leaseStateName = \case
  RepositoryLeaseSyncing    -> "syncing" :: String
  RepositoryLeaseActive     -> "active"
  RepositoryLeaseDegraded   -> "degraded"
  RepositoryLeaseGrace      -> "grace"
  RepositoryLeaseTerminated -> "terminated"
