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

  , testCase "expiry is inclusive and releases the replica artifacts" $
      withRepository $ \Fixture{..} -> do
        let leaseId = makeRepositoryLeaseId fixtureRepository 4
            initial = repositoryLeaseFixture leaseId fixtureRepository
        providerCredentials <- newCredentials @'HBS2Basic
        leaseRef <- newIORef initial
        pollsRef <- newIORef []
        let someBrains = SomeBrains @L4Proto (TestBrains leaseRef pollsRef)

        active <- refreshRepositoryLeaseAt 41
          providerCredentials someBrains fixtureStorage initial
        let expiring = active { repositoryLeaseExpiresAt = Just 42 }
        writeIORef leaseRef expiring

        expiredLease <- refreshRepositoryLeaseAt 42
          providerCredentials someBrains fixtureStorage expiring
        repositoryLeaseState expiredLease @?= RepositoryLeaseTerminated
        repositoryLeaseLastError expiredLease @?= Just "lease expired"
        repositoryLeaseUpdatedAt expiredLease @?= 42
        getRef fixtureStorage (RepositoryLeasePin leaseId) >>= (@?= Nothing)
        getRef fixtureStorage (RepositoryLeaseReceipt leaseId) >>= (@?= Nothing)

  , testCase "a durable lease is visible after Brains is reopened" $
      withSystemTempDirectory "hbs2-repository-lease-brains" $ \directory -> do
        credentials <- newCredentials @'HBS2Basic
        let repository = view peerSignPk credentials
            lease = RepositoryLease
              { repositoryLeaseVersion = 1
              , repositoryLeaseId = makeRepositoryLeaseId repository 3
              , repositoryLeaseRepository = repository
              , repositoryLeaseState = RepositoryLeaseSyncing
              , repositoryLeaseMaxBytes = 1024
              , repositoryLeaseMaxBlocks = 100
              , repositoryLeaseCreatedAt = 1
              , repositoryLeaseUpdatedAt = 1
              , repositoryLeaseExpiresAt = Nothing
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
  { repositoryLeaseVersion = 1
  , repositoryLeaseId = leaseId
  , repositoryLeaseRepository = repository
  , repositoryLeaseState = RepositoryLeaseSyncing
  , repositoryLeaseMaxBytes = 1024 * 1024
  , repositoryLeaseMaxBlocks = 1000
  , repositoryLeaseCreatedAt = 1
  , repositoryLeaseUpdatedAt = 1
  , repositoryLeaseExpiresAt = Nothing
  , repositoryLeaseSnapshot = Nothing
  , repositoryLeaseLastError = Nothing
  }
