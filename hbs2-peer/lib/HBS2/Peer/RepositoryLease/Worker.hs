module HBS2.Peer.RepositoryLease.Worker
  ( RepositoryAdvertPolicy(..)
  , defaultRepositoryAdvertPolicy
  , maintainRepositoryReplicaAdvert
  , maintainRepositoryReplicaAdvertAt
  , refreshRepositoryLease
  , refreshRepositoryLeaseAt
  ) where

import HBS2.Data.Types.Refs
import HBS2.Net.Auth.Credentials
import HBS2.Peer.Brains
import HBS2.Peer.Prelude
import HBS2.Peer.Proto.LWWRef
import HBS2.Peer.Proto.RefLog
import HBS2.Peer.Proto.RepositoryLease
import HBS2.Peer.RepositoryLease
import HBS2.Storage
import HBS2.Storage.Inventory

import Control.Applicative ((<|>))
import Data.Bits ((.&.))
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Word (Word64)
import Lens.Micro.Platform (view)

data LeaseRefreshContext = LeaseRefreshContext
  { refreshCredentials :: PeerCredentials 'HBS2Basic
  , refreshBrains      :: SomeBrains L4Proto
  , refreshStorage     :: AnyStorage
  , refreshLease       :: RepositoryLease
  }

type RefreshRepositoryLease =
  PeerCredentials 'HBS2Basic
  -> SomeBrains L4Proto
  -> AnyStorage
  -> RepositoryLease
  -> IO RepositoryLease

-- | Local policy for the provider's short-lived discovery statement.  Public
-- endpoints are injected by the executable because the library cannot infer
-- a routable address from a bind socket such as @0.0.0.0@.
data RepositoryAdvertPolicy = RepositoryAdvertPolicy
  { advertPolicyReachableVia :: [PeerAddr L4Proto]
  , advertPolicyCapabilities :: Word64
  , advertPolicyLifetime     :: Word64
  , advertPolicyRenewBefore  :: Word64
  }
  deriving stock (Eq, Show)

defaultRepositoryAdvertPolicy
  :: [PeerAddr L4Proto]
  -> RepositoryAdvertPolicy
defaultRepositoryAdvertPolicy reachable = RepositoryAdvertPolicy
  { advertPolicyReachableVia = reachable
  , advertPolicyCapabilities = replicaAdvertCapabilityBlockDownload
  , advertPolicyLifetime = maxReplicaAdvertLifetime
  , advertPolicyRenewBefore = 5 * 60
  }

maintainRepositoryReplicaAdvert
  :: RepositoryAdvertPolicy
  -> AnyStorage
  -> PeerCredentials 'HBS2Basic
  -> RepositoryLease
  -> IO (Either RepositoryAdvertError (Maybe (HashRef, ReplicaAdvertV1)))
maintainRepositoryReplicaAdvert policy storage credentials lease = do
  now <- currentTimestamp
  maintainRepositoryReplicaAdvertAt now policy storage credentials lease

-- | Keep one provider-local advert present while the lease is active.  A
-- matching advert is reused until its renewal boundary; inactive leases and
-- unusable policies withdraw the ref immediately.  Expired or malformed
-- adverts are replaced from the verified receipt rather than trusted.
maintainRepositoryReplicaAdvertAt
  :: Word64
  -> RepositoryAdvertPolicy
  -> AnyStorage
  -> PeerCredentials 'HBS2Basic
  -> RepositoryLease
  -> IO (Either RepositoryAdvertError (Maybe (HashRef, ReplicaAdvertV1)))
maintainRepositoryReplicaAdvertAt now policy storage credentials lease
  | repositoryLeaseState lease /= RepositoryLeaseActive =
      withdraw $> Right Nothing
  | Just problem <- repositoryAdvertPolicyError policy =
      withdraw $> Left problem
  | otherwise = case repositoryLeaseSnapshot lease of
      Nothing -> withdraw $> Left RepositoryAdvertSnapshotMismatch
      Just snapshot -> maintainActiveRepositoryReplicaAdvertAt
        now policy storage credentials lease snapshot
  where
    repository = repositoryLeaseRepository lease
    provider = view peerSignPk credentials
    withdraw = releaseRepositoryReplicaAdvert storage repository provider

repositoryAdvertPolicyError
  :: RepositoryAdvertPolicy
  -> Maybe RepositoryAdvertError
repositoryAdvertPolicyError policy
  | null (advertPolicyReachableVia policy) =
      Just RepositoryAdvertNoReachableAddress
  | advertPolicyCapabilities policy
      .&. replicaAdvertCapabilityBlockDownload == 0 =
      Just RepositoryAdvertCannotServeBlocks
  | advertPolicyLifetime policy == 0 =
      Just RepositoryAdvertInvalidLifetime
  | advertPolicyLifetime policy > maxReplicaAdvertLifetime =
      Just RepositoryAdvertInvalidLifetime
  | advertPolicyRenewBefore policy >= advertPolicyLifetime policy =
      Just RepositoryAdvertInvalidLifetime
  | otherwise = Nothing

maintainActiveRepositoryReplicaAdvertAt
  :: Word64
  -> RepositoryAdvertPolicy
  -> AnyStorage
  -> PeerCredentials 'HBS2Basic
  -> RepositoryLease
  -> RepositorySnapshot
  -> IO (Either RepositoryAdvertError (Maybe (HashRef, ReplicaAdvertV1)))
maintainActiveRepositoryReplicaAdvertAt
  now policy storage credentials lease snapshot = do
    current <- readRepositoryReplicaAdvertAt now storage repository provider
    receipt <- readRepositoryReplicaReceipt storage (repositoryLeaseId lease)
    case (current, receipt) of
      ( Right (advertHash, _, advert)
        , Right (receiptHash, _, _)
        ) | repositoryAdvertReusableAt now policy snapshot receiptHash advert ->
              pure (Right (Just (advertHash, advert)))
      _ -> fmap Just <$> issueRepositoryReplicaAdvertAt
        now (advertPolicyLifetime policy)
        (advertPolicyReachableVia policy)
        (advertPolicyCapabilities policy)
        storage credentials lease snapshot
  where
    repository = repositoryLeaseRepository lease
    provider = view peerSignPk credentials

repositoryAdvertReusableAt
  :: Word64
  -> RepositoryAdvertPolicy
  -> RepositorySnapshot
  -> HashRef
  -> ReplicaAdvertV1
  -> Bool
repositoryAdvertReusableAt now policy snapshot receiptHash advert = and
  [ replicaAdvertSnapshotId advert == repositorySnapshotId snapshot
  , replicaAdvertReceipt advert == receiptHash
  , replicaAdvertReachableVia advert == advertPolicyReachableVia policy
  , replicaAdvertCapabilities advert == advertPolicyCapabilities policy
  , replicaAdvertExpiresAt advert - now > advertPolicyRenewBefore policy
  ]

-- | Reconcile one durable lease with the blocks currently present in storage.
-- Fetching is delegated to the existing LWWRef and RefLog pollers.  A lease is
-- activated only after its complete typed repository graph, pin, and signed
-- replica receipt agree.
refreshRepositoryLease :: RefreshRepositoryLease
refreshRepositoryLease credentials brains storage lease = do
  now <- currentTimestamp
  refreshRepositoryLeaseAt now credentials brains storage lease

-- | Deterministic form of 'refreshRepositoryLease'.  Supplying the timestamp
-- keeps expiry boundaries testable without racing the wall clock.
refreshRepositoryLeaseAt
  :: Word64
  -> RefreshRepositoryLease
refreshRepositoryLeaseAt now credentials brains storage lease
  | repositoryLeaseState lease == RepositoryLeaseTerminated =
      releaseArtifacts context $> lease
  | not (repositoryLeaseVersionSupported (repositoryLeaseVersion lease)) =
      persistIfChanged now context lease
        { repositoryLeaseState = incompleteLeaseState lease
        , repositoryLeaseLastError = Just "unsupported repository lease version"
        }
  | otherwise = refreshLifetime now context
  where
    context = LeaseRefreshContext credentials brains storage lease

currentTimestamp :: IO Word64
currentTimestamp = floor <$> getPOSIXTime

incompleteLeaseState :: RepositoryLease -> RepositoryLeaseState
incompleteLeaseState lease = case repositoryLeaseSnapshot lease of
  Nothing -> RepositoryLeaseSyncing
  Just{}  -> RepositoryLeaseDegraded

refreshLifetime :: Word64 -> LeaseRefreshContext -> IO RepositoryLease
refreshLifetime now context =
  case repositoryLeaseLifetimeAt now lease of
    RepositoryLeaseUnexpired -> refreshAvailableRepository now context
    RepositoryLeaseInGrace -> persistIfChanged now context lease
      { repositoryLeaseState = RepositoryLeaseGrace
      , repositoryLeaseUpdateStartedAt = Nothing
      , repositoryLeaseLastError = Just "lease grace period"
      }
    RepositoryLeaseElapsed -> do
      releaseArtifacts context
      persistIfChanged now context lease
        { repositoryLeaseState = RepositoryLeaseTerminated
        , repositoryLeaseUpdateStartedAt = Nothing
        , repositoryLeaseLastError = Just "lease expired"
        }
  where
    lease = refreshLease context

refreshAvailableRepository :: Word64 -> LeaseRefreshContext -> IO RepositoryLease
refreshAvailableRepository now context = do
  ensurePoll context (repositoryLeaseRepository lease) "lwwref"
  discoveredRefLog <- discoverRepositoryRefLogKey storage
                        (repositoryLeaseRepository lease)
  for_ discoveredRefLog $ \refLogKey -> ensurePoll context refLogKey "reflog"

  discoverRepositorySnapshot (leaseInventoryLimits lease) storage
    (repositoryLeaseRepository lease) >>= \case
      Left problem -> persistProblem now context problem
      Right candidate -> activateCandidate now context candidate
  where
    lease = refreshLease context
    storage = refreshStorage context

leaseInventoryLimits :: RepositoryLease -> InventoryLimits
leaseInventoryLimits lease = InventoryLimits
  { inventoryMaxBlocks = repositoryLeaseMaxBlocks lease
  , inventoryMaxBytes = repositoryLeaseMaxBytes lease
  }

ensurePoll
  :: LeaseRefreshContext
  -> PubKey 'Sign 'HBS2Basic
  -> String
  -> IO ()
ensurePoll context key kind = do
  exists <- isPolledRef @L4Proto (refreshBrains context) kind key
  unless exists (addPolledRef @L4Proto (refreshBrains context) key kind 1)

persistProblem
  :: Word64
  -> LeaseRefreshContext
  -> RepositorySnapshotError
  -> IO RepositoryLease
persistProblem now context problem = case repositoryLeaseSnapshot lease of
  Nothing -> persistIfChanged now context lease
    { repositoryLeaseState = RepositoryLeaseSyncing
    , repositoryLeaseUpdateStartedAt = Nothing
    , repositoryLeaseLastError = Just (Text.pack (show problem))
    }
  Just snapshot -> do
    updateObserved <- repositoryUpdateObserved context snapshot
    persistIfChanged now context
      (leaseAfterProblem now updateObserved problem lease)
  where
    lease = refreshLease context

leaseAfterProblem
  :: Word64
  -> Bool
  -> RepositorySnapshotError
  -> RepositoryLease
  -> RepositoryLease
leaseAfterProblem now updateObserved problem lease = updated
  where
    startedAt = repositoryLeaseUpdateStartedAt lease <|> Just now
    withTracking = lease { repositoryLeaseUpdateStartedAt = startedAt }
    nextState
      | updateObserved && not (repositoryLeaseUpdateOverdueAt now withTracking) =
          RepositoryLeaseActive
      | otherwise = RepositoryLeaseDegraded
    updated = lease
      { repositoryLeaseState = nextState
      , repositoryLeaseUpdateStartedAt =
          if updateObserved then startedAt else Nothing
      , repositoryLeaseLastError = Just (Text.pack (show problem))
      }

repositoryUpdateObserved :: LeaseRefreshContext -> RepositorySnapshot -> IO Bool
repositoryUpdateObserved context snapshot = do
  currentLww <- fmap HashRef <$> getRef storage
    (LWWRefKey @'HBS2Basic (repositoryLeaseRepository lease))
  currentRefLog <- fmap HashRef <$> getRef storage
    (RefLogKey @'HBS2Basic (repositorySnapshotRefLogKey snapshot))
  pure $ changed currentLww (repositorySnapshotLWWRefBlock snapshot)
      || changed currentRefLog (repositorySnapshotRefLogRoot snapshot)
  where
    lease = refreshLease context
    storage = refreshStorage context
    changed Nothing _ = False
    changed (Just current) expected = current /= expected

activateCandidate
  :: Word64
  -> LeaseRefreshContext
  -> RepositorySnapshotCandidate
  -> IO RepositoryLease
activateCandidate now context candidate = do
  case repositoryLeaseSnapshot lease of
    Just snapshot
      | repositorySnapshotId snapshot == candidateSnapshotId candidate ->
          reconcileExistingSnapshot now context candidate snapshot
    _ -> repairCandidate now context candidate Nothing
  where
    lease = refreshLease context

reconcileExistingSnapshot
  :: Word64
  -> LeaseRefreshContext
  -> RepositorySnapshotCandidate
  -> RepositorySnapshot
  -> IO RepositoryLease
reconcileExistingSnapshot now context candidate snapshot = do
  samePin <- leasePinMatches context snapshot
  sameReceipt <- leaseReceiptMatches context snapshot
  if samePin && sameReceipt
    then persistActive now context (refreshLease context)
    else repairCandidate now context candidate (snapshot <$ guard samePin)

leasePinMatches :: LeaseRefreshContext -> RepositorySnapshot -> IO Bool
leasePinMatches context snapshot = do
  pin <- getRef (refreshStorage context)
    (RepositoryLeasePin (repositoryLeaseId lease))
  pure (fmap HashRef pin == Just (repositorySnapshotPinRoot snapshot))
  where
    lease = refreshLease context

leaseReceiptMatches
  :: LeaseRefreshContext
  -> RepositorySnapshot
  -> IO Bool
leaseReceiptMatches context snapshot = do
  receipt <- readRepositoryReplicaReceipt (refreshStorage context)
    (repositoryLeaseId lease)
  pure $ case receipt of
    Right (_, signer, payload) ->
      receiptMatches context snapshot signer payload
    _ -> False
  where
    lease = refreshLease context

repairCandidate
  :: Word64
  -> LeaseRefreshContext
  -> RepositorySnapshotCandidate
  -> Maybe RepositorySnapshot
  -> IO RepositoryLease
repairCandidate now context candidate reusableSnapshot = do
  sealed <- case reusableSnapshot of
    Just snapshot -> pure (Right snapshot)
    Nothing -> sealRepositorySnapshot storage (repositoryLeaseId lease) candidate
  case sealed of
    Left problem -> persistProblem now context problem
    Right snapshot -> activateSnapshot now context snapshot
  where
    lease = refreshLease context
    storage = refreshStorage context

activateSnapshot
  :: Word64
  -> LeaseRefreshContext
  -> RepositorySnapshot
  -> IO RepositoryLease
activateSnapshot now context snapshot =
  issueRepositoryReplicaReceipt storage credentials lease snapshot >>= \case
    Left problem -> persistProblem now context problem
    Right{} -> persistActive now context lease
      { repositoryLeaseSnapshot = Just snapshot }
  where
    credentials = refreshCredentials context
    lease = refreshLease context
    storage = refreshStorage context

persistActive
  :: Word64
  -> LeaseRefreshContext
  -> RepositoryLease
  -> IO RepositoryLease
persistActive now context lease = persistIfChanged now context lease
  { repositoryLeaseState = RepositoryLeaseActive
  , repositoryLeaseUpdateStartedAt = Nothing
  , repositoryLeaseLastError = Nothing
  }

receiptMatches
  :: LeaseRefreshContext
  -> RepositorySnapshot
  -> PubKey 'Sign 'HBS2Basic
  -> ReplicaReceiptV1
  -> Bool
receiptMatches context snapshot signer ReplicaReceiptV1{..} =
  signer == provider && actual == expected
  where
    lease = refreshLease context
    provider = view peerSignPk (refreshCredentials context)
    actual =
      ( replicaReceiptLeaseId
      , replicaReceiptRepository
      , replicaReceiptProvider
      , replicaReceiptSnapshotId
      , replicaReceiptInventoryRoot
      , replicaReceiptBlockCount
      , replicaReceiptLogicalBytes
      , replicaReceiptCompletedAt
      )
    expected =
      ( repositoryLeaseId lease
      , repositoryLeaseRepository lease
      , signer
      , repositorySnapshotId snapshot
      , repositorySnapshotInventoryRoot snapshot
      , repositorySnapshotBlockCount snapshot
      , repositorySnapshotLogicalBytes snapshot
      , repositorySnapshotCompletedAt snapshot
      )

persistIfChanged
  :: Word64
  -> LeaseRefreshContext
  -> RepositoryLease
  -> IO RepositoryLease
persistIfChanged now context after = do
  current <- getRepositoryLease @L4Proto brains (repositoryLeaseId before)
               <&> fromMaybe before
  if repositoryLeaseState current == RepositoryLeaseTerminated
       && repositoryLeaseState after /= RepositoryLeaseTerminated
    then releaseArtifacts context $> current
    else if sameLeaseIgnoringTimestamp current after
      then pure current
      else do
        let updated = after { repositoryLeaseUpdatedAt = now }
        putRepositoryLease @L4Proto brains updated
        pure updated
  where
    brains = refreshBrains context
    before = refreshLease context

sameLeaseIgnoringTimestamp :: RepositoryLease -> RepositoryLease -> Bool
sameLeaseIgnoringTimestamp current after =
  current { repositoryLeaseUpdatedAt = repositoryLeaseUpdatedAt after } == after

releaseArtifacts :: LeaseRefreshContext -> IO ()
releaseArtifacts context = do
  releaseRepositoryLeasePin storage (repositoryLeaseId lease)
  releaseRepositoryReplicaReceipt storage (repositoryLeaseId lease)
  releaseRepositoryReplicaAdvert storage
    (repositoryLeaseRepository lease)
    (view peerSignPk (refreshCredentials context))
  where
    lease = refreshLease context
    storage = refreshStorage context
