module RepositoryLeaseSpec (tests) where

import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Base58
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema()
import HBS2.OrDie
import HBS2.Peer.Proto ()
import HBS2.Peer.Proto.LWWRef
import HBS2.Peer.Proto.RefLog
import HBS2.Peer.Proto.RepositoryLease
import HBS2.Peer.Brains
import HBS2.Peer.RepositoryLease
import HBS2.Peer.RepositoryLease.Admission
import HBS2.Peer.RepositoryLease.Worker
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Storage.Inventory
import HBS2.Storage.Simple

import Codec.Serialise (deserialiseOrFail)
import Control.Concurrent.Async
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Map.Strict qualified as Map
import Data.IORef
import Data.Maybe (isJust)
import Data.Either (isLeft, isRight)
import Data.Word (Word16, Word64)
import Lens.Micro.Platform
import System.FilePath
import System.IO.Temp
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO (bracket)

import Brains qualified as DB
import PeerConfig

data Fixture = Fixture
  { fixtureStorage       :: AnyStorage
  , fixtureRepository    :: PubKey 'Sign 'HBS2Basic
  , fixturePayloadBlock  :: Hash HbSync
  , fixtureStorageWrites :: IORef Int
  }

data CountingStorage = CountingStorage AnyStorage (IORef Int)

-- The shape serialised by hbs2-peer 0.26.0.x before grace periods were
-- introduced.  This fixture guards the durable on-disk compatibility path.
data RepositoryLeaseV1 = RepositoryLeaseV1
  Word16
  RepositoryLeaseId
  (PubKey 'Sign 'HBS2Basic)
  RepositoryLeaseState
  Word64
  Word64
  Word64
  Word64
  (Maybe Word64)
  (Maybe RepositorySnapshot)
  (Maybe Text)
  deriving stock (Generic)

instance Serialise RepositoryLeaseV1

-- The durable shape used after grace periods and before update-lag tracking.
data RepositoryLeaseV2 = RepositoryLeaseV2
  Word16
  RepositoryLeaseId
  (PubKey 'Sign 'HBS2Basic)
  RepositoryLeaseState
  Word64
  Word64
  Word64
  Word64
  (Maybe Word64)
  Word64
  (Maybe RepositorySnapshot)
  (Maybe Text)
  deriving stock (Generic)

instance Serialise RepositoryLeaseV2

instance Storage CountingStorage HbSync LBS.ByteString IO where
  putBlock (CountingStorage storage writes) bytes =
    countWrite writes (putBlock storage bytes)
  enqueueBlock (CountingStorage storage writes) bytes =
    countWrite writes (enqueueBlock storage bytes)
  getBlock (CountingStorage storage _) = getBlock storage
  delBlock (CountingStorage storage writes) hash =
    countWrite writes (delBlock storage hash)
  getChunk (CountingStorage storage _) = getChunk storage
  hasBlock (CountingStorage storage _) = hasBlock storage
  updateRef (CountingStorage storage writes) ref hash =
    countWrite writes (updateRef storage ref hash)
  getRef (CountingStorage storage _) = getRef storage
  delRef (CountingStorage storage writes) ref =
    countWrite writes (delRef storage ref)

countWrite :: IORef Int -> IO a -> IO a
countWrite writes action = atomicModifyIORef' writes (\count -> (count + 1, ()))
  >> action

data TestBrains = TestBrains
  { testLease :: IORef RepositoryLease
  , testPolls :: IORef [(PubKey 'Sign 'HBS2Basic, String, Int)]
  }

instance HasBrains L4Proto TestBrains where
  putRepositoryLease brains = liftIO . writeIORef (testLease brains)
  getRepositoryLease brains leaseId = do
    lease <- liftIO $ readIORef (testLease brains)
    pure $ if repositoryLeaseId lease == leaseId then Just lease else Nothing
  listRepositoryLeases brains = liftIO $ pure <$> readIORef (testLease brains)
  isPolledRef brains kind refKey =
    liftIO $ any (\(storedKey, storedKind, _) ->
      storedKey == refKey && storedKind == kind)
      <$> readIORef (testPolls brains)
  addPolledRef brains refKey kind priority =
    liftIO $ atomicModifyIORef' (testPolls brains)
      (\polls -> ((refKey, kind, priority) : polls, ()))

tests :: TestTree
tests = testGroup "repository lease"
  [ testCase "a complete repository is inventoried and pinned" $
      withRepository $ \Fixture{..} -> do
        discovered <- discoverRepositorySnapshot generous fixtureStorage fixtureRepository
                        >>= expectRight
        assertBool "payload belongs to inventory" $
          Map.member (HashRef fixturePayloadBlock) (candidateInventory discovered)

        let leaseId = makeRepositoryLeaseId fixtureRepository 1
        snapshot <- sealRepositorySnapshot fixtureStorage leaseId discovered
                      >>= expectRight
        pin <- getRef fixtureStorage (RepositoryLeasePin leaseId)
        fmap HashRef pin @?= Just (repositorySnapshotPinRoot snapshot)

        commitmentBytes <- getBlock fixtureStorage
          (fromHashRef (repositorySnapshotInventoryRoot snapshot))
            >>= orThrowUser "inventory commitment root is missing"
        commitment <- deserialiseOrFail @(MTree [RepositoryInventoryEntry]) commitmentBytes
                        & either (assertFailure . show) pure
        let expectedEntries =
              [ RepositoryInventoryEntry hashRef size
              | (hashRef, size) <- Map.toAscList (candidateInventory discovered)
              ]
        case commitment of
          MLeaf actualEntries -> actualEntries @?= expectedEntries
          MNode{} -> assertFailure "small inventory unexpectedly has an internal root"

        providerCredentials <- newCredentials @'HBS2Basic
        let lease = repositoryLeaseFixture leaseId fixtureRepository
        (_, receipt) <- issueRepositoryReplicaReceipt
          fixtureStorage providerCredentials lease snapshot >>= expectRight
        replicaReceiptSnapshotId receipt @?= repositorySnapshotId snapshot
        replicaReceiptInventoryRoot receipt @?=
          repositorySnapshotInventoryRoot snapshot
        (_, receiptSigner, storedReceipt) <-
          readRepositoryReplicaReceipt fixtureStorage leaseId >>= expectRight
        receiptSigner @?= view peerSignPk providerCredentials
        storedReceipt @?= receipt

        releaseRepositoryLeasePin fixtureStorage leaseId
        getRef fixtureStorage (RepositoryLeasePin leaseId) >>= (@?= Nothing)
        releaseRepositoryReplicaReceipt fixtureStorage leaseId
        getRef fixtureStorage (RepositoryLeaseReceipt leaseId) >>= (@?= Nothing)

  , testCase "a missing transaction payload cannot become a snapshot" $
      withRepository $ \Fixture{..} -> do
        delBlock fixtureStorage fixturePayloadBlock
        discoverRepositorySnapshot generous fixtureStorage fixtureRepository >>= \case
          Left (RepositoryInventoryIncomplete InventoryTraversalComplete 1 0 0) -> pure ()
          other -> assertFailure ("expected one missing block, got " <> show other)

  , testCase "the block limit cannot produce an apparently smaller snapshot" $
      withRepository $ \Fixture{..} -> do
        discoverRepositorySnapshot (InventoryLimits 1 maxBound)
          fixtureStorage fixtureRepository >>= \case
            Left (RepositoryInventoryIncomplete InventoryBlockLimitExceeded{} _ _ _) -> pure ()
            other -> assertFailure ("expected the block limit, got " <> show other)

  , testCase "the worker activates, degrades without dropping the old pin, and terminates" $
      withRepository $ \Fixture{..} -> do
        let leaseId = makeRepositoryLeaseId fixtureRepository 2
            initial = repositoryLeaseFixture leaseId fixtureRepository
        providerCredentials <- newCredentials @'HBS2Basic
        leaseRef <- newIORef initial
        pollsRef <- newIORef []
        let brains = TestBrains leaseRef pollsRef
            someBrains = SomeBrains @L4Proto brains

        active <- refreshRepositoryLease providerCredentials someBrains fixtureStorage initial
        repositoryLeaseState active @?= RepositoryLeaseActive
        activeSnapshot <- repositoryLeaseSnapshot active
                            & maybe (assertFailure "active lease has no snapshot") pure
        polls <- readIORef pollsRef
        assertBool "LWWRef was subscribed" $
          any (\(storedKey, storedKind, priority) ->
            storedKey == fixtureRepository
              && storedKind == "lwwref"
              && priority == 1) polls
        assertBool "RefLog was subscribed" (length polls == 2)

        (receiptHash, receiptSigner, receipt) <- readRepositoryReplicaReceipt
          fixtureStorage leaseId >>= expectRight
        receiptSigner @?= view peerSignPk providerCredentials
        replicaReceiptSnapshotId receipt @?= repositorySnapshotId activeSnapshot

        writesBeforeStableRefresh <- readIORef fixtureStorageWrites
        stable <- refreshRepositoryLease
          providerCredentials someBrains fixtureStorage active
        stable @?= active
        readIORef fixtureStorageWrites >>= (@?= writesBeforeStableRefresh)
        (stableReceiptHash, _, _) <- readRepositoryReplicaReceipt
          fixtureStorage leaseId >>= expectRight
        stableReceiptHash @?= receiptHash

        releaseRepositoryReplicaReceipt fixtureStorage leaseId
        receiptRepaired <- refreshRepositoryLease
          providerCredentials someBrains fixtureStorage stable
        (_, restoredSigner, restoredReceipt) <- readRepositoryReplicaReceipt
          fixtureStorage leaseId >>= expectRight
        restoredSigner @?= view peerSignPk providerCredentials
        restoredReceipt @?= receipt

        receiptBytes <- getBlock fixtureStorage (fromHashRef receiptHash)
          >>= orThrowUser "receipt block disappeared"
        receiptBox <- deserialiseOrFail
          @(SignedBox ReplicaReceiptV1 'HBS2Basic) receiptBytes
          & either (assertFailure . show) pure
        let forgedSignature :: SignedBox ReplicaReceiptV1 'HBS2Basic
            forgedSignature = case receiptBox of
              SignedBox signer body signature ->
                SignedBox signer (body <> "tampered") signature
        forgedSignatureHash <- put fixtureStorage (serialise forgedSignature)
        updateRef fixtureStorage (RepositoryLeaseReceipt leaseId)
          forgedSignatureHash
        readRepositoryReplicaReceipt fixtureStorage leaseId >>= \case
          Left RepositoryReceiptInvalid{} -> pure ()
          other -> assertFailure ("expected an invalid receipt signature, got " <> show other)

        signatureRepaired <- refreshRepositoryLease
          providerCredentials someBrains fixtureStorage receiptRepaired
        repositoryLeaseState signatureRepaired @?= RepositoryLeaseActive
        (_, signatureRepairedSigner, signatureRepairedReceipt) <-
          readRepositoryReplicaReceipt fixtureStorage leaseId >>= expectRight
        signatureRepairedSigner @?= view peerSignPk providerCredentials
        signatureRepairedReceipt @?= receipt

        attacker <- newCredentials @'HBS2Basic
        let attackerKey = view peerSignPk attacker
            forgedPayload = receipt { replicaReceiptProvider = attackerKey }
            forgedBox = makeSignedBox @'HBS2Basic attackerKey
              (view peerSignSk attacker) forgedPayload
        forgedHash <- put fixtureStorage (serialise forgedBox)
        updateRef fixtureStorage (RepositoryLeaseReceipt leaseId) forgedHash
        repaired <- refreshRepositoryLease
          providerCredentials someBrains fixtureStorage signatureRepaired
        repositoryLeaseState repaired @?= RepositoryLeaseActive
        (_, repairedSigner, _) <- readRepositoryReplicaReceipt
          fixtureStorage leaseId >>= expectRight
        repairedSigner @?= view peerSignPk providerCredentials

        releaseRepositoryLeasePin fixtureStorage leaseId
        repinned <- refreshRepositoryLease
          providerCredentials someBrains fixtureStorage repaired
        repairedPin <- getRef fixtureStorage (RepositoryLeasePin leaseId)
        fmap HashRef repairedPin @?= Just (repositorySnapshotPinRoot activeSnapshot)

        delBlock fixtureStorage fixturePayloadBlock
        degraded <- refreshRepositoryLease
          providerCredentials someBrains fixtureStorage repinned
        repositoryLeaseState degraded @?= RepositoryLeaseDegraded
        repositoryLeaseSnapshot degraded @?= Just activeSnapshot
        pin <- getRef fixtureStorage (RepositoryLeasePin leaseId)
        fmap HashRef pin @?= Just (repositorySnapshotPinRoot activeSnapshot)

        let terminated = degraded { repositoryLeaseState = RepositoryLeaseTerminated }
        writeIORef leaseRef terminated
        staleResult <- refreshRepositoryLease
          providerCredentials someBrains fixtureStorage degraded
        staleResult @?= terminated
        getRef fixtureStorage (RepositoryLeasePin leaseId) >>= (@?= Nothing)
        getRef fixtureStorage (RepositoryLeaseReceipt leaseId) >>= (@?= Nothing)
        _ <- refreshRepositoryLease
          providerCredentials someBrains fixtureStorage terminated
        getRef fixtureStorage (RepositoryLeasePin leaseId) >>= (@?= Nothing)
        getRef fixtureStorage (RepositoryLeaseReceipt leaseId) >>= (@?= Nothing)

  , testCase "expiry keeps the replica pinned through its grace period" $
      withRepository $ \Fixture{..} -> do
        let leaseId = makeRepositoryLeaseId fixtureRepository 4
            initial = (repositoryLeaseFixture leaseId fixtureRepository)
              { repositoryLeaseGracePeriod = 10 }
        providerCredentials <- newCredentials @'HBS2Basic
        leaseRef <- newIORef initial
        pollsRef <- newIORef []
        let someBrains = SomeBrains @L4Proto (TestBrains leaseRef pollsRef)

        active <- refreshRepositoryLeaseAt 41
          providerCredentials someBrains fixtureStorage initial
        let expiring = active { repositoryLeaseExpiresAt = Just 42 }
        writeIORef leaseRef expiring

        graceLease <- refreshRepositoryLeaseAt 42
          providerCredentials someBrains fixtureStorage expiring
        repositoryLeaseState graceLease @?= RepositoryLeaseGrace
        repositoryLeaseLastError graceLease @?= Just "lease grace period"
        repositoryLeaseUpdatedAt graceLease @?= 42
        assertBool "pin survives grace" . isJust =<<
          getRef fixtureStorage (RepositoryLeasePin leaseId)
        assertBool "receipt survives grace" . isJust =<<
          getRef fixtureStorage (RepositoryLeaseReceipt leaseId)

        lastGraceLease <- refreshRepositoryLeaseAt 51
          providerCredentials someBrains fixtureStorage graceLease
        repositoryLeaseState lastGraceLease @?= RepositoryLeaseGrace
        assertBool "pin survives the last grace second" . isJust =<<
          getRef fixtureStorage (RepositoryLeasePin leaseId)

        expiredLease <- refreshRepositoryLeaseAt 52
          providerCredentials someBrains fixtureStorage lastGraceLease
        repositoryLeaseState expiredLease @?= RepositoryLeaseTerminated
        repositoryLeaseLastError expiredLease @?= Just "lease expired"
        repositoryLeaseUpdatedAt expiredLease @?= 52
        getRef fixtureStorage (RepositoryLeasePin leaseId) >>= (@?= Nothing)
        getRef fixtureStorage (RepositoryLeaseReceipt leaseId) >>= (@?= Nothing)

  , testCase "update lag keeps the last complete snapshot until its boundary" $
      withRepository $ \Fixture{..} -> do
        let leaseId = makeRepositoryLeaseId fixtureRepository 6
            initial = (repositoryLeaseFixture leaseId fixtureRepository)
              { repositoryLeaseMaxUpdateLag = 10 }
        providerCredentials <- newCredentials @'HBS2Basic
        leaseRef <- newIORef initial
        pollsRef <- newIORef []
        let someBrains = SomeBrains @L4Proto (TestBrains leaseRef pollsRef)

        active <- refreshRepositoryLeaseAt 50
          providerCredentials someBrains fixtureStorage initial
        snapshot <- repositoryLeaseSnapshot active
          & maybe (assertFailure "active lease has no snapshot") pure
        let missingHead = hashObject @HbSync
              ("pending-repository-update" :: LBS.ByteString)
        updateRef fixtureStorage (LWWRefKey @'HBS2Basic fixtureRepository)
          missingHead

        pending <- refreshRepositoryLeaseAt 100
          providerCredentials someBrains fixtureStorage active
        repositoryLeaseState pending @?= RepositoryLeaseActive
        repositoryLeaseUpdateStartedAt pending @?= Just 100
        repositoryLeaseSnapshot pending @?= Just snapshot
        assertBool "the old snapshot remains pinned while the update is fresh" . isJust
          =<< getRef fixtureStorage (RepositoryLeasePin leaseId)

        lastFresh <- refreshRepositoryLeaseAt 109
          providerCredentials someBrains fixtureStorage pending
        repositoryLeaseState lastFresh @?= RepositoryLeaseActive
        repositoryLeaseUpdateStartedAt lastFresh @?= Just 100

        stale <- refreshRepositoryLeaseAt 110
          providerCredentials someBrains fixtureStorage lastFresh
        repositoryLeaseState stale @?= RepositoryLeaseDegraded
        repositoryLeaseUpdateStartedAt stale @?= Just 100
        repositoryLeaseSnapshot stale @?= Just snapshot

        updateRef fixtureStorage (LWWRefKey @'HBS2Basic fixtureRepository)
          (fromHashRef (repositorySnapshotLWWRefBlock snapshot))
        caughtUp <- refreshRepositoryLeaseAt 111
          providerCredentials someBrains fixtureStorage stale
        repositoryLeaseState caughtUp @?= RepositoryLeaseActive
        repositoryLeaseUpdateStartedAt caughtUp @?= Nothing

  , testCase "unchanged or missing mutable roots do not receive update lag" $ do
      withRepository $ \Fixture{..} -> do
        let leaseId = makeRepositoryLeaseId fixtureRepository 9
            initial = (repositoryLeaseFixture leaseId fixtureRepository)
              { repositoryLeaseMaxUpdateLag = 10 }
        providerCredentials <- newCredentials @'HBS2Basic
        leaseRef <- newIORef initial
        pollsRef <- newIORef []
        let someBrains = SomeBrains @L4Proto (TestBrains leaseRef pollsRef)
        active <- refreshRepositoryLeaseAt 50
          providerCredentials someBrains fixtureStorage initial

        delBlock fixtureStorage fixturePayloadBlock
        degraded <- refreshRepositoryLeaseAt 100
          providerCredentials someBrains fixtureStorage active
        repositoryLeaseState degraded @?= RepositoryLeaseDegraded
        repositoryLeaseUpdateStartedAt degraded @?= Nothing

      withRepository $ \Fixture{..} -> do
        let leaseId = makeRepositoryLeaseId fixtureRepository 10
            initial = (repositoryLeaseFixture leaseId fixtureRepository)
              { repositoryLeaseMaxUpdateLag = 10 }
        providerCredentials <- newCredentials @'HBS2Basic
        leaseRef <- newIORef initial
        pollsRef <- newIORef []
        let someBrains = SomeBrains @L4Proto (TestBrains leaseRef pollsRef)
        active <- refreshRepositoryLeaseAt 50
          providerCredentials someBrains fixtureStorage initial

        delRef fixtureStorage (LWWRefKey @'HBS2Basic fixtureRepository)
        degraded <- refreshRepositoryLeaseAt 100
          providerCredentials someBrains fixtureStorage active
        repositoryLeaseState degraded @?= RepositoryLeaseDegraded
        repositoryLeaseUpdateStartedAt degraded @?= Nothing

  , testCase "a durable lease is visible after Brains is reopened" $
      withSystemTempDirectory "hbs2-repository-lease-brains" $ \directory -> do
        credentials <- newCredentials @'HBS2Basic
        let repository = view peerSignPk credentials
            lease = RepositoryLease
              { repositoryLeaseVersion = repositoryLeaseCurrentVersion
              , repositoryLeaseId = makeRepositoryLeaseId repository 3
              , repositoryLeaseRepository = repository
              , repositoryLeaseState = RepositoryLeaseSyncing
              , repositoryLeaseMaxBytes = 1024
              , repositoryLeaseMaxBlocks = 100
              , repositoryLeaseCreatedAt = 1
              , repositoryLeaseUpdatedAt = 1
              , repositoryLeaseExpiresAt = Nothing
              , repositoryLeaseGracePeriod = 0
              , repositoryLeaseMaxUpdateLag = 60
              , repositoryLeaseUpdateStartedAt = Nothing
              , repositoryLeaseSnapshot = Nothing
              , repositoryLeaseLastError = Nothing
              }
            stateFile = directory </> "state.db"
            source = "brains-db \"" <> stateFile <> "\""
        forms <- either (assertFailure . show) pure (parseTop source)
        let config = PeerConfig forms
        first <- DB.newBasicBrains @L4Proto config
        runner <- async (DB.runBasicBrains config first)
        putRepositoryLease @L4Proto first lease

        reopened <- DB.newBasicBrains @L4Proto config
        listRepositoryLeases @L4Proto reopened >>= (@?= [lease])
        cancel runner

  , testCase "version 1 durable leases decode with no grace period" $ do
      credentials <- newCredentials @'HBS2Basic
      let repository = view peerSignPk credentials
          leaseId = makeRepositoryLeaseId repository 5
          encoded = serialise $ RepositoryLeaseV1
            1 leaseId repository RepositoryLeaseDegraded
            1024 100 10 20 (Just 30) Nothing (Just "old failure")
      decoded <- deserialiseOrFail @RepositoryLease encoded
        & either (assertFailure . show) pure
      decoded @?= RepositoryLease
        { repositoryLeaseVersion = 1
        , repositoryLeaseId = leaseId
        , repositoryLeaseRepository = repository
        , repositoryLeaseState = RepositoryLeaseDegraded
        , repositoryLeaseMaxBytes = 1024
        , repositoryLeaseMaxBlocks = 100
        , repositoryLeaseCreatedAt = 10
        , repositoryLeaseUpdatedAt = 20
        , repositoryLeaseExpiresAt = Just 30
        , repositoryLeaseGracePeriod = 0
        , repositoryLeaseMaxUpdateLag = 0
        , repositoryLeaseUpdateStartedAt = Nothing
        , repositoryLeaseSnapshot = Nothing
        , repositoryLeaseLastError = Just "old failure"
        }

  , testCase "version 2 durable leases decode with immediate update lag" $ do
      credentials <- newCredentials @'HBS2Basic
      let repository = view peerSignPk credentials
          leaseId = makeRepositoryLeaseId repository 7
          encoded = serialise $ RepositoryLeaseV2
            2 leaseId repository RepositoryLeaseDegraded
            1024 100 10 20 (Just 30) 9 Nothing (Just "old failure")
      decoded <- deserialiseOrFail @RepositoryLease encoded
        & either (assertFailure . show) pure
      repositoryLeaseGracePeriod decoded @?= 9
      repositoryLeaseMaxUpdateLag decoded @?= 0
      repositoryLeaseUpdateStartedAt decoded @?= Nothing

  , testCase "update lag boundary saturates instead of wrapping" $ do
      repository <- newRepositoryKey
      let lease = (admissionLease repository 8 100)
            { repositoryLeaseMaxUpdateLag = 10
            , repositoryLeaseUpdateStartedAt = Just (maxBound - 2)
            }
      assertBool "the final representable second is the stale boundary"
        (repositoryLeaseUpdateOverdueAt maxBound lease)
      assertBool "a pending update before the saturated boundary remains fresh"
        (not (repositoryLeaseUpdateOverdueAt (maxBound - 1) lease))
      assertBool "a lease without a pending update is never stale"
        (not (repositoryLeaseUpdateOverdueAt maxBound
          (lease { repositoryLeaseUpdateStartedAt = Nothing })))

  , testCase "grace boundary saturates instead of wrapping" $ do
      repository <- newRepositoryKey
      let lease = (admissionLease repository 11 100)
            { repositoryLeaseExpiresAt = Just (maxBound - 2)
            , repositoryLeaseGracePeriod = 10
            }
      repositoryLeaseLifetimeAt (maxBound - 1) lease @?= RepositoryLeaseInGrace
      repositoryLeaseLifetimeAt maxBound lease @?= RepositoryLeaseElapsed

  , testCase "only known durable lease versions are supported" $ do
      repositoryLeaseCurrentVersion @?= 3
      assertBool "version 1 remains readable" (repositoryLeaseVersionSupported 1)
      assertBool "version 2 remains readable" (repositoryLeaseVersionSupported 2)
      assertBool "version 3 is current" (repositoryLeaseVersionSupported 3)
      assertBool "version 0 is unsupported" (not (repositoryLeaseVersionSupported 0))
      assertBool "version 4 is unsupported" (not (repositoryLeaseVersionSupported 4))

  , testCase "capacity reserves contract limits until termination or elapsed grace" $ do
      syncingRepository <- newRepositoryKey
      activeRepository <- newRepositoryKey
      degradedRepository <- newRepositoryKey
      graceRepository <- newRepositoryKey
      terminatedRepository <- newRepositoryKey
      let syncing = (admissionLease syncingRepository 1 10)
            { repositoryLeaseState = RepositoryLeaseSyncing }
          active = (admissionLease activeRepository 2 20)
            { repositoryLeaseState = RepositoryLeaseActive }
          degraded = (admissionLease degradedRepository 3 30)
            { repositoryLeaseState = RepositoryLeaseDegraded }
          grace = (admissionLease graceRepository 4 40)
            { repositoryLeaseState = RepositoryLeaseGrace }
          terminated = (admissionLease terminatedRepository 5 50)
            { repositoryLeaseState = RepositoryLeaseTerminated }
          expiring = grace
            { repositoryLeaseExpiresAt = Just 90
            , repositoryLeaseGracePeriod = 20
            }
          policy = RepositoryLeaseAdmissionPolicy 200 10
      repositoryLeaseCapacityAt 100 policy
        [syncing, active, degraded, expiring, terminated]
        @?= RepositoryLeaseCapacity 200 100 100 4 10
      repositoryLeaseCapacityAt 110 policy
        [syncing, active, degraded, expiring, terminated]
        @?= RepositoryLeaseCapacity 200 60 140 3 10

  , testCase "capacity totals saturate instead of wrapping" $ do
      firstRepository <- newRepositoryKey
      secondRepository <- newRepositoryKey
      let leases =
            [ admissionLease firstRepository 1 maxBound
            , admissionLease secondRepository 2 1
            ]
      repositoryLeaseReservedBytesAt 10 leases @?= maxBound
      repositoryLeaseCapacityAt 10
        (RepositoryLeaseAdmissionPolicy maxBound 2) leases
        @?= RepositoryLeaseCapacity maxBound maxBound 0 2 2

  , testCase "admission accounts for renewal delta and exact budget boundary" $ do
      repository <- view peerSignPk <$> newCredentials @'HBS2Basic
      otherRepository <- view peerSignPk <$> newCredentials @'HBS2Basic
      let policy = RepositoryLeaseAdmissionPolicy 100 2
          existing = admissionLease repository 1 60
          other = admissionLease otherRepository 2 30
          exactRenewal = admissionLease repository 1 70
          oversizedRenewal = admissionLease repository 1 71
      admitRepositoryLeaseAt 10 policy exactRenewal [existing, other]
        @?= Right (RepositoryLeaseCapacity 100 100 0 2 2)
      admitRepositoryLeaseAt 10 policy oversizedRenewal [existing, other]
        @?= Left (RepositoryLeaseCapacityExceeded 71 70)

  , testCase "contract limit refuses new leases but permits renewal and termination" $ do
      repository <- view peerSignPk <$> newCredentials @'HBS2Basic
      otherRepository <- view peerSignPk <$> newCredentials @'HBS2Basic
      thirdRepository <- view peerSignPk <$> newCredentials @'HBS2Basic
      let policy = RepositoryLeaseAdmissionPolicy 1000 2
          existing = admissionLease repository 1 100
          other = admissionLease otherRepository 2 100
          renewal = admissionLease repository 1 200
          third = admissionLease thirdRepository 3 100
          terminated = existing
            { repositoryLeaseState = RepositoryLeaseTerminated }
      assertBool "renewal keeps the existing contract slot" $
        isRight (admitRepositoryLeaseAt 10 policy renewal [existing, other])
      admitRepositoryLeaseAt 10 policy third [existing, other]
        @?= Left (RepositoryLeaseContractLimitExceeded 2)
      admitRepositoryLeaseAt 10 policy terminated [existing, other]
        @?= Right (RepositoryLeaseCapacity 1000 100 900 1 2)

  , testCase "admission rejects malformed or already elapsed contracts" $ do
      repository <- newRepositoryKey
      let policy = RepositoryLeaseAdmissionPolicy 1000 10
          lease = admissionLease repository 1 100
      admitRepositoryLeaseAt 10 policy
        lease { repositoryLeaseVersion = 4 } []
        @?= Left (RepositoryLeaseUnsupportedVersion 4)
      admitRepositoryLeaseAt 10 policy
        lease { repositoryLeaseMaxBytes = 0 } []
        @?= Left RepositoryLeaseInvalidLimit
      admitRepositoryLeaseAt 10 policy
        lease { repositoryLeaseMaxBlocks = 0 } []
        @?= Left RepositoryLeaseInvalidLimit
      admitRepositoryLeaseAt 10 policy
        lease { repositoryLeaseExpiresAt = Just 10 } []
        @?= Left RepositoryLeaseAlreadyElapsed
      admitRepositoryLeaseAt 10
        (RepositoryLeaseAdmissionPolicy 1000 0) lease []
        @?= Left (RepositoryLeaseContractLimitExceeded 0)

  , testCase "admission errors explain the rejected boundary" $ do
      map repositoryLeaseAdmissionErrorMessage
        [ RepositoryLeaseCapacityExceeded 11 10
        , RepositoryLeaseContractLimitExceeded 2
        , RepositoryLeaseUnsupportedVersion 3
        , RepositoryLeaseInvalidLimit
        , RepositoryLeaseAlreadyElapsed
        , RepositoryLeaseAdmissionStorageFailure
        ] @?=
        [ "repository lease capacity exceeded: requested 11 bytes, available 10"
        , "repository lease contract limit exceeded: 2"
        , "unsupported repository lease version: 3"
        , "repository lease byte and block limits must be positive"
        , "repository lease already elapsed"
        , "repository lease admission could not be stored"
        ]

  , testCase "peer config supplies finite aggregate lease limits" $ do
      forms <- either (assertFailure . show) pure $ parseTop $ unlines
        [ "barter-budget-bytes 4096"
        , "barter-max-contracts 7"
        ]
      repositoryLeaseAdmissionPolicy (PeerConfig forms)
        @?= RepositoryLeaseAdmissionPolicy 4096 7
      repositoryLeaseAdmissionPolicy mempty
        @?= RepositoryLeaseAdmissionPolicy
              defaultRepositoryLeaseBudgetBytes
              defaultRepositoryLeaseMaxContracts
      zeroForms <- either (assertFailure . show) pure $ parseTop $ unlines
        [ "barter-budget-bytes 0"
        , "barter-max-contracts 0"
        ]
      repositoryLeaseAdmissionPolicy (PeerConfig zeroForms)
        @?= RepositoryLeaseAdmissionPolicy 0 0

  , testCase "concurrent admission cannot overcommit the durable budget" $
      withSystemTempDirectory "hbs2-repository-lease-admission" $ \directory -> do
        firstRepository <- newRepositoryKey
        secondRepository <- newRepositoryKey
        let firstLease = admissionLease firstRepository 1 60
            secondLease = admissionLease secondRepository 2 60
            policy = RepositoryLeaseAdmissionPolicy 100 10
            stateFile = directory </> "state.db"
            source = "brains-db \"" <> stateFile <> "\""
        forms <- either (assertFailure . show) pure (parseTop source)
        let config = PeerConfig forms
        brains <- DB.newBasicBrains @L4Proto config
        runner <- async (DB.runBasicBrains config brains)
        (firstResult, secondResult) <- concurrently
          (admitRepositoryLease @L4Proto brains 10 policy firstLease)
          (admitRepositoryLease @L4Proto brains 10 policy secondLease)
        assertBool "one admission succeeds" $
          length (filter isRight [firstResult, secondResult]) == 1
        assertBool "one admission is refused" $
          length (filter isLeft [firstResult, secondResult]) == 1
        durable <- listRepositoryLeases @L4Proto brains
        length durable @?= 1
        repositoryLeaseReservedBytesAt 10 durable @?= 60
        cancel runner
  ]
  where
    generous = InventoryLimits 1000 (1024 * 1024)

withRepository :: (Fixture -> IO a) -> IO a
withRepository action =
  withSystemTempDirectory "hbs2-repository-lease-test" $ \directory -> do
    simple <- simpleStorageInit @HbSync [StoragePrefix (directory </> "storage")]
    writes <- newIORef 0
    let storage = AnyStorage (CountingStorage (AnyStorage simple) writes)
    bracket (replicateM 2 (async (simpleStorageWorker simple)))
      (mapM_ cancel)
      (const (buildRepository storage writes >>= action))

buildRepository :: AnyStorage -> IORef Int -> IO Fixture
buildRepository storage writes = do
  repositoryCredentials <- newCredentials @'HBS2Basic
  refLogCredentials <- newCredentials @'HBS2Basic
  let repository = view peerSignPk repositoryCredentials
      refLog = view peerSignPk refLogCredentials

  payloadBlock <- put storage "git pack payload"
  transactionRoot <- put storage . serialise $
    MTreeAnn (ShortMetadata "") NullEncryption
      (MLeaf [HashRef payloadBlock] :: MTree [HashRef])

  update <- makeRefLogUpdate @L4Proto @'HBS2Basic
              refLog (view peerSignSk refLogCredentials)
              (LBS.toStrict (serialise (AnnotatedHashRef Nothing (HashRef transactionRoot))))
  updateBlock <- put storage (serialise update)
  refLogRoot <- put storage (serialise (MLeaf [HashRef updateBlock] :: MTree [HashRef]))
  updateRef storage (RefLogKey @'HBS2Basic refLog) refLogRoot

  manifestBytes <- put storage . LBS8.pack $
    "(reflog " <> show (pretty (AsBase58 refLog)) <> ")"
  manifestRoot <- put storage . serialise $
    MTreeAnn (ShortMetadata "") NullEncryption
      (MLeaf [HashRef manifestBytes] :: MTree [HashRef])
  headRoot <- put storage (serialise (MLeaf [HashRef manifestRoot] :: MTree [HashRef]))

  let lww = LWWRef 1 (HashRef headRoot) Nothing :: LWWRef 'HBS2Basic
      box = makeSignedBox @'HBS2Basic repository
              (view peerSignSk repositoryCredentials) lww
  lwwBlock <- put storage (serialise box)
  updateRef storage (LWWRefKey @'HBS2Basic repository) lwwBlock

  pure Fixture
    { fixtureStorage = storage
    , fixtureRepository = repository
    , fixturePayloadBlock = payloadBlock
    , fixtureStorageWrites = writes
    }

put :: AnyStorage -> LBS.ByteString -> IO (Hash HbSync)
put storage bytes = putBlock storage bytes >>= orThrowUser "test storage write failed"

expectRight :: Show e => Either e a -> IO a
expectRight = either (assertFailure . show) pure

repositoryLeaseFixture
  :: RepositoryLeaseId
  -> PubKey 'Sign 'HBS2Basic
  -> RepositoryLease
repositoryLeaseFixture leaseId repository = RepositoryLease
  { repositoryLeaseVersion = repositoryLeaseCurrentVersion
  , repositoryLeaseId = leaseId
  , repositoryLeaseRepository = repository
  , repositoryLeaseState = RepositoryLeaseSyncing
  , repositoryLeaseMaxBytes = 1024 * 1024
  , repositoryLeaseMaxBlocks = 1000
  , repositoryLeaseCreatedAt = 1
  , repositoryLeaseUpdatedAt = 1
  , repositoryLeaseExpiresAt = Nothing
  , repositoryLeaseGracePeriod = 0
  , repositoryLeaseMaxUpdateLag = 0
  , repositoryLeaseUpdateStartedAt = Nothing
  , repositoryLeaseSnapshot = Nothing
  , repositoryLeaseLastError = Nothing
  }

admissionLease
  :: PubKey 'Sign 'HBS2Basic
  -> Word64
  -> Word64
  -> RepositoryLease
admissionLease repository nonce maxBytes =
  (repositoryLeaseFixture (makeRepositoryLeaseId repository nonce) repository)
    { repositoryLeaseMaxBytes = maxBytes }

newRepositoryKey :: IO (PubKey 'Sign 'HBS2Basic)
newRepositoryKey = view peerSignPk <$> newCredentials @'HBS2Basic
