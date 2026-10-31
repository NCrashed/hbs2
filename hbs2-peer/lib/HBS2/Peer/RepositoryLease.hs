module HBS2.Peer.RepositoryLease
  ( RepositoryBlockRole(..)
  , RepositorySnapshotCandidate(..)
  , RepositorySnapshotError(..)
  , RepositoryAdvertError(..)
  , discoverRepositoryRefLogKey
  , discoverRepositorySnapshot
  , sealRepositorySnapshot
  , issueRepositoryReplicaReceipt
  , readRepositoryReplicaReceipt
  , issueRepositoryReplicaAdvertAt
  , decodeRepositoryReplicaAdvertAt
  , readRepositoryReplicaAdvertAt
  , maxReplicaAdvertAddresses
  , maxReplicaAdvertLifetime
  , releaseRepositoryLeasePin
  , releaseRepositoryReplicaReceipt
  , releaseRepositoryReplicaAdvert
  ) where

import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Data.Detect (readLogThrow)
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema()
import HBS2.Peer.Proto ()
import HBS2.Peer.Proto.LWWRef
import HBS2.Peer.Proto.RefLog
import HBS2.Peer.Proto.RepositoryLease
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Storage.Inventory
import HBS2.Storage.Operations.ByteString

import Codec.Serialise
import Control.Exception (SomeException, try)
import Control.Monad.Except
import Data.Bits ((.&.))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Config.Suckless.Script
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Word
import Lens.Micro.Platform (view)

data RepositoryBlockRole
  = RepositoryOpaque
  | RepositoryMerkleLog
  | RepositoryContentTree
  deriving stock (Eq, Ord, Show)

data RepositorySnapshotCandidate = RepositorySnapshotCandidate
  { candidateRepository    :: PubKey 'Sign 'HBS2Basic
  , candidateLWWRefBlock   :: HashRef
  , candidateLWWSequence   :: Word64
  , candidateHeadRoot      :: HashRef
  , candidateRefLogKey     :: PubKey 'Sign 'HBS2Basic
  , candidateRefLogRoot    :: HashRef
  , candidateSnapshotId    :: HashRef
  , candidateInventory     :: Map HashRef Word64
  , candidateLogicalBytes  :: Word64
  }
  deriving stock (Eq, Show)

data RepositorySnapshotError
  = RepositoryLWWRefMissing
  | RepositoryLWWRefBlockMissing HashRef
  | RepositoryLWWRefMalformed HashRef
  | RepositoryLWWRefWrongSigner
  | RepositoryHeadIncomplete HashRef
  | RepositoryManifestMissing
  | RepositoryManifestUnreadable HashRef
  | RepositoryManifestMalformed
  | RepositoryRefLogMissing
  | RepositoryRefLogIncomplete HashRef
  | RepositoryRefLogUpdateMissing HashRef
  | RepositoryRefLogUpdateMalformed HashRef
  | RepositoryRefLogUpdateWrongSigner HashRef
  | RepositoryRefLogUpdateBadSignature HashRef
  | RepositoryRefLogPayloadUnsupported HashRef
  | RepositoryInventoryIncomplete InventoryStatus Int Int Int
  | RepositoryChangedDuringInventory
  | RepositoryInventoryPinFailed
  | RepositoryReceiptPinMissing
  | RepositoryReceiptStoreFailed
  | RepositoryReceiptMissing
  | RepositoryReceiptBlockMissing HashRef
  | RepositoryReceiptMalformed HashRef
  | RepositoryReceiptInvalid HashRef
  | RepositoryReceiptMismatch
  deriving stock (Eq, Show)

data RepositoryAdvertError
  = RepositoryAdvertLeaseInactive
  | RepositoryAdvertSnapshotMismatch
  | RepositoryAdvertReceiptMismatch
  | RepositoryAdvertInvalidLifetime
  | RepositoryAdvertNoReachableAddress
  | RepositoryAdvertTooManyAddresses
  | RepositoryAdvertTooLarge
  | RepositoryAdvertCannotServeBlocks
  | RepositoryAdvertStoreFailed
  | RepositoryAdvertMissing
  | RepositoryAdvertBlockMissing HashRef
  | RepositoryAdvertMalformed HashRef
  | RepositoryAdvertInvalid HashRef
  | RepositoryAdvertWireMalformed
  | RepositoryAdvertWireInvalid
  | RepositoryAdvertMismatch
  | RepositoryAdvertNotYetValid
  | RepositoryAdvertExpired
  deriving stock (Eq, Show)

discoverRepositorySnapshot
  :: InventoryLimits
  -> AnyStorage
  -> PubKey 'Sign 'HBS2Basic
  -> IO (Either RepositorySnapshotError RepositorySnapshotCandidate)
discoverRepositorySnapshot limits sto repository = runExceptT do
  (lwwBlock, lww, headItems, manifestRoot, refLogKey, manifestGroupKeys) <-
    discoverRepositoryHead sto repository

  refLogRoot <- getRef sto (RefLogKey @'HBS2Basic refLogKey)
                  >>= note RepositoryRefLogMissing . fmap HashRef

  updateHashes <- readLogOr (RepositoryRefLogIncomplete refLogRoot) refLogRoot
  txRoots <- concat <$> forM updateHashes (readUpdate refLogKey)

  groupJournal <- fmap concat $ forM (drop 1 headItems) $ \root -> do
    refs <- readLogOr (RepositoryHeadIncomplete root) root
    pure ((RepositoryMerkleLog, root) : ((RepositoryContentTree,) <$> refs))

  let roots =
        [ (RepositoryOpaque, lwwBlock)
        , (RepositoryMerkleLog, lwwValue lww)
        , (RepositoryContentTree, manifestRoot)
        , (RepositoryMerkleLog, refLogRoot)
        ]
        <> groupJournal
        <> ((RepositoryContentTree,) <$> manifestGroupKeys)
        <> ((RepositoryOpaque,) <$> updateHashes)
        <> txRoots

  inventory <- liftIO $
    scanInventoryTyped limits (getBlock sto . fromHashRef) repositoryEdges roots

  unless (inventoryUsable inventory) $
    throwError (RepositoryInventoryIncomplete
      (inventoryStatus inventory)
      (Set.size (inventoryMissing inventory))
      (Set.size (inventoryCorrupt inventory))
      (Set.size (inventoryMalformed inventory)))

  lwwBlockAfter <- getRef sto (LWWRefKey @'HBS2Basic repository) <&> fmap HashRef
  refLogAfter <- getRef sto (RefLogKey @'HBS2Basic refLogKey) <&> fmap HashRef
  unless (lwwBlockAfter == Just lwwBlock && refLogAfter == Just refLogRoot) $
    throwError RepositoryChangedDuringInventory

  let snapshotId = makeRepositorySnapshotId repository lwwBlock
                     (lwwSeq lww) (lwwValue lww) refLogKey refLogRoot

  pure RepositorySnapshotCandidate
    { candidateRepository = repository
    , candidateLWWRefBlock = lwwBlock
    , candidateLWWSequence = lwwSeq lww
    , candidateHeadRoot = lwwValue lww
    , candidateRefLogKey = refLogKey
    , candidateRefLogRoot = refLogRoot
    , candidateSnapshotId = snapshotId
    , candidateInventory = inventoryBlocks inventory
    , candidateLogicalBytes = inventoryLogicalBytes inventory
    }
  where
    note e = maybe (throwError e) pure

    readLogOr e root = do
      result <- liftIO $ try @SomeException $
                  readLogThrow (getBlock sto) root
      either (const (throwError e)) pure result

    readUpdate expected href = do
      bytes <- getBlock sto (fromHashRef href)
                 >>= note (RepositoryRefLogUpdateMissing href)
      update <- decodeSnapshotOr @(RefLogUpdate L4Proto)
                  (RepositoryRefLogUpdateMalformed href) bytes
      unless (_refLogId update == expected) $
        throwError (RepositoryRefLogUpdateWrongSigner href)
      signed <- liftIO (verifyRefLogUpdate @L4Proto update)
      unless signed $ throwError (RepositoryRefLogUpdateBadSignature href)

      let payload = LBS.fromStrict (_refLogUpdData update)
          annotated = deserialiseOrFail @AnnotatedHashRef payload
          sequential = deserialiseOrFail @SequentialRef payload

      case (annotated, sequential) of
        (Right (AnnotatedHashRef ann root), _) ->
          pure $ (RepositoryContentTree, root)
               : maybe [] (pure . (RepositoryOpaque,)) ann

        (_, Right (SequentialRef _ (AnnotatedHashRef ann root))) ->
          pure $ (RepositoryContentTree, root)
               : maybe [] (pure . (RepositoryOpaque,)) ann

        _ -> throwError (RepositoryRefLogPayloadUnsupported href)

-- | Resolve the RefLog key as soon as the repository manifest is locally
-- available.  The lease worker uses this before a complete snapshot exists,
-- so that it can register the durable RefLog poller which fetches the rest.
discoverRepositoryRefLogKey
  :: AnyStorage
  -> PubKey 'Sign 'HBS2Basic
  -> IO (Either RepositorySnapshotError (PubKey 'Sign 'HBS2Basic))
discoverRepositoryRefLogKey sto repository =
  runExceptT do
    (_, _, _, _, refLogKey, _) <- discoverRepositoryHead sto repository
    pure refLogKey

discoverRepositoryHead
  :: AnyStorage
  -> PubKey 'Sign 'HBS2Basic
  -> ExceptT RepositorySnapshotError IO
       ( HashRef
       , LWWRef 'HBS2Basic
       , [HashRef]
       , HashRef
       , PubKey 'Sign 'HBS2Basic
       , [HashRef]
       )
discoverRepositoryHead sto repository = do
  lwwBlock <- getRef sto (LWWRefKey @'HBS2Basic repository)
                >>= note RepositoryLWWRefMissing . fmap HashRef

  lwwBytes <- getBlock sto (fromHashRef lwwBlock)
                >>= note (RepositoryLWWRefBlockMissing lwwBlock)

  box <- decodeSnapshotOr @(SignedBox (LWWRef 'HBS2Basic) 'HBS2Basic)
           (RepositoryLWWRefMalformed lwwBlock) lwwBytes
  (signer, lww) <- note (RepositoryLWWRefMalformed lwwBlock)
                     (unboxSignedBox0 @(LWWRef 'HBS2Basic) box)
  unless (signer == repository) (throwError RepositoryLWWRefWrongSigner)

  headItems <- readLogOr (RepositoryHeadIncomplete (lwwValue lww)) (lwwValue lww)
  manifestRoot <- note RepositoryManifestMissing (listToMaybe headItems)
  manifestBytes <- readPlainTree manifestRoot
  (refLogKey, manifestGroupKeys) <- parseRepositoryManifest manifestBytes
  pure (lwwBlock, lww, headItems, manifestRoot, refLogKey, manifestGroupKeys)
  where
    note e = maybe (throwError e) pure

    readLogOr e root = do
      result <- liftIO $ try @SomeException $
                  readLogThrow (getBlock sto) root
      either (const (throwError e)) pure result

    readPlainTree root = do
      result <- liftIO $ runExceptT $
                  readFromMerkle sto (SimpleKey (fromHashRef root))
      either (const (throwError (RepositoryManifestUnreadable root))) pure result

parseRepositoryManifest
  :: ByteString
  -> ExceptT RepositorySnapshotError IO
       (PubKey 'Sign 'HBS2Basic, [HashRef])
parseRepositoryManifest bytes = do
  forms <- either (const (throwError RepositoryManifestMalformed)) pure $
             parseTop (Text.decodeUtf8With Text.lenientDecode (LBS.toStrict bytes))

  refLog <- note RepositoryManifestMalformed . listToMaybe $
    [ parsedKey
    | ListVal [SymbolVal "reflog", TextLike value] <- forms
    , Just parsedKey <- [fromStringMay (Text.unpack value)]
    ]

  let groupKeys =
        [ parsedKey
        | ListVal [SymbolVal "gk", TextLike value] <- forms
        , Just parsedKey <- [fromStringMay (Text.unpack value)]
        ]

  pure (refLog, groupKeys)
  where
    note e = maybe (throwError e) pure

inventoryUsable :: InventoryScan -> Bool
inventoryUsable InventoryScan{..} =
     inventoryStatus == InventoryTraversalComplete
  && Set.null inventoryMissing
  && Set.null inventoryCorrupt
  && Set.null inventoryMalformed

decodeSnapshotOr
  :: forall a
   . Serialise a
  => RepositorySnapshotError
  -> ByteString
  -> ExceptT RepositorySnapshotError IO a
decodeSnapshotOr e bytes =
  either (const (throwError e)) pure (deserialiseOrFail bytes)

repositoryEdges
  :: RepositoryBlockRole
  -> HashRef
  -> ByteString
  -> Either String [(RepositoryBlockRole, HashRef)]
repositoryEdges RepositoryOpaque _ _ = Right []
repositoryEdges RepositoryMerkleLog href bytes =
  case deserialiseOrFail @(MTree [HashRef]) bytes of
    Left e -> Left ("malformed Merkle log " <> show href <> ": " <> show e)
    Right tree -> Right (treeEdges RepositoryMerkleLog RepositoryOpaque tree)
repositoryEdges RepositoryContentTree href bytes =
  contentTreeEdges href bytes

contentTreeEdges
  :: HashRef
  -> ByteString
  -> Either String [(RepositoryBlockRole, HashRef)]
contentTreeEdges href bytes =
  case deserialiseOrFail @(MTreeAnn [HashRef]) bytes of
    Right MTreeAnn{..} -> Right $
         annotationEdges _mtaMeta
      <> encryptionEdges _mtaCrypt
      <> treeEdges RepositoryContentTree RepositoryOpaque _mtaTree

    Left{} -> case deserialiseOrFail @(MTree [HashRef]) bytes of
      Right tree -> Right (treeEdges RepositoryContentTree RepositoryOpaque tree)
      Left e -> Left ("malformed content tree " <> show href <> ": " <> show e)

treeEdges
  :: RepositoryBlockRole
  -> RepositoryBlockRole
  -> MTree [HashRef]
  -> [(RepositoryBlockRole, HashRef)]
treeEdges nodeRole leafRole = \case
  MNode _ childRefs -> (nodeRole,) . HashRef <$> childRefs
  MLeaf childRefs -> (leafRole,) <$> childRefs

annotationEdges :: AnnMetaData -> [(RepositoryBlockRole, HashRef)]
annotationEdges = \case
  AnnHashRef h -> [(RepositoryOpaque, HashRef h)]
  _ -> []

encryptionEdges :: MTreeEncryption -> [(RepositoryBlockRole, HashRef)]
encryptionEdges = \case
  NullEncryption -> []
  CryptAccessKeyNaClAsymm h -> [(RepositoryOpaque, HashRef h)]
  EncryptGroupNaClSymm h _ -> [(RepositoryContentTree, HashRef h)]

sealRepositorySnapshot
  :: AnyStorage
  -> RepositoryLeaseId
  -> RepositorySnapshotCandidate
  -> IO (Either RepositorySnapshotError RepositorySnapshot)
sealRepositorySnapshot sto leaseId candidate = runExceptT do
  failed <- liftIO (newIORef False)
  let hashes = Map.keys (candidateInventory candidate)
      entries = uncurry RepositoryInventoryEntry <$> Map.toAscList (candidateInventory candidate)
      pinTree = toPTree (MaxSize 256) (MaxNum 256) hashes
      inventoryTree = toPTree (MaxSize 256) (MaxNum 256) entries

  inventoryRoot <- lift $ makeMerkle 0 inventoryTree $ \(_, _, bytes) -> do
    stored <- putBlock sto bytes
    when (isNothing stored) (writeIORef failed True)
  pinRoot <- lift $ makeMerkle 0 pinTree $ \(_, _, bytes) -> do
    stored <- putBlock sto bytes
    when (isNothing stored) (writeIORef failed True)

  didFail <- liftIO (readIORef failed)
  when didFail (throwError RepositoryInventoryPinFailed)

  inventoryPresent <- getBlock sto inventoryRoot
  pinPresent <- getBlock sto pinRoot
  when (isNothing inventoryPresent || isNothing pinPresent) $
    throwError RepositoryInventoryPinFailed

  updateRef sto (RepositoryLeasePin leaseId) pinRoot
  pinned <- getRef sto (RepositoryLeasePin leaseId)
  unless (pinned == Just pinRoot) (throwError RepositoryInventoryPinFailed)

  completed <- liftIO getPOSIXTime <&> floor
  pure RepositorySnapshot
    { repositorySnapshotId = candidateSnapshotId candidate
    , repositorySnapshotLWWRefBlock = candidateLWWRefBlock candidate
    , repositorySnapshotLWWSequence = candidateLWWSequence candidate
    , repositorySnapshotHeadRoot = candidateHeadRoot candidate
    , repositorySnapshotRefLogKey = candidateRefLogKey candidate
    , repositorySnapshotRefLogRoot = candidateRefLogRoot candidate
    , repositorySnapshotInventoryRoot = HashRef inventoryRoot
    , repositorySnapshotPinRoot = HashRef pinRoot
    , repositorySnapshotBlockCount = fromIntegral (Map.size (candidateInventory candidate))
    , repositorySnapshotLogicalBytes = candidateLogicalBytes candidate
    , repositorySnapshotCompletedAt = completed
    }

releaseRepositoryLeasePin :: AnyStorage -> RepositoryLeaseId -> IO ()
releaseRepositoryLeasePin sto = delRef sto . RepositoryLeasePin

-- | Sign and durably root a receipt only after the exact snapshot pin is
-- visible.  A crash before the caller persists Active may leave an inert pin
-- and receipt, which recovery can safely reuse; it can never leave Active
-- without both artifacts already durable.
issueRepositoryReplicaReceipt
  :: AnyStorage
  -> PeerCredentials 'HBS2Basic
  -> RepositoryLease
  -> RepositorySnapshot
  -> IO (Either RepositorySnapshotError (HashRef, ReplicaReceiptV1))
issueRepositoryReplicaReceipt sto credentials lease snapshot = runExceptT do
  pinned <- getRef sto (RepositoryLeasePin (repositoryLeaseId lease))
  unless (fmap HashRef pinned == Just (repositorySnapshotPinRoot snapshot)) $
    throwError RepositoryReceiptPinMissing

  issuedAt <- liftIO getPOSIXTime <&> floor
  let provider = view peerSignPk credentials
      payload = ReplicaReceiptV1
        { replicaReceiptVersion = 1
        , replicaReceiptLeaseId = repositoryLeaseId lease
        , replicaReceiptRepository = repositoryLeaseRepository lease
        , replicaReceiptProvider = provider
        , replicaReceiptSnapshotId = repositorySnapshotId snapshot
        , replicaReceiptInventoryRoot = repositorySnapshotInventoryRoot snapshot
        , replicaReceiptBlockCount = repositorySnapshotBlockCount snapshot
        , replicaReceiptLogicalBytes = repositorySnapshotLogicalBytes snapshot
        , replicaReceiptCompletedAt = repositorySnapshotCompletedAt snapshot
        , replicaReceiptIssuedAt = issuedAt
        }
      box = makeSignedBox @'HBS2Basic provider
              (view peerSignSk credentials) payload

  receiptHash <- putBlock sto (serialise box)
                   >>= maybe (throwError RepositoryReceiptStoreFailed)
                             (pure . HashRef)
  updateRef sto (RepositoryLeaseReceipt (repositoryLeaseId lease))
    (fromHashRef receiptHash)

  (storedHash, signer, storedPayload) <-
    liftIO (readRepositoryReplicaReceipt sto (repositoryLeaseId lease))
      >>= either throwError pure
  unless (storedHash == receiptHash && signer == provider
          && storedPayload == payload) $
    throwError RepositoryReceiptMismatch

  pure (receiptHash, payload)

-- | Load a receipt through its durable lease-owned ref and verify both the
-- signature and the provider identity duplicated inside the signed payload.
readRepositoryReplicaReceipt
  :: AnyStorage
  -> RepositoryLeaseId
  -> IO (Either RepositorySnapshotError
          (HashRef, PubKey 'Sign 'HBS2Basic, ReplicaReceiptV1))
readRepositoryReplicaReceipt sto leaseId = runExceptT do
  receiptHash <- getRef sto (RepositoryLeaseReceipt leaseId)
                   >>= maybe (throwError RepositoryReceiptMissing)
                             (pure . HashRef)
  bytes <- getBlock sto (fromHashRef receiptHash)
             >>= maybe (throwError (RepositoryReceiptBlockMissing receiptHash)) pure
  box <- either (const (throwError (RepositoryReceiptMalformed receiptHash))) pure $
           deserialiseOrFail @(SignedBox ReplicaReceiptV1 'HBS2Basic) bytes
  (signer, payload) <- maybe
    (throwError (RepositoryReceiptInvalid receiptHash))
    pure
    (unboxSignedBox0 box)
  unless (replicaReceiptVersion payload == 1
          && replicaReceiptLeaseId payload == leaseId
          && replicaReceiptProvider payload == signer) $
    throwError RepositoryReceiptMismatch
  pure (receiptHash, signer, payload)

releaseRepositoryReplicaReceipt :: AnyStorage -> RepositoryLeaseId -> IO ()
releaseRepositoryReplicaReceipt sto = delRef sto . RepositoryLeaseReceipt

-- | Create the provider's short-lived discovery hint only from an already
-- active lease and its verified receipt.  The lifetime is deliberately capped
-- so a lost withdrawal cannot leave a long-lived stale provider behind.
issueRepositoryReplicaAdvertAt
  :: Word64
  -> Word64
  -> [PeerAddr L4Proto]
  -> Word64
  -> AnyStorage
  -> PeerCredentials 'HBS2Basic
  -> RepositoryLease
  -> RepositorySnapshot
  -> IO (Either RepositoryAdvertError (HashRef, ReplicaAdvertV1))
issueRepositoryReplicaAdvertAt now lifetime reachable capabilities
  sto credentials lease snapshot = runExceptT do
    unless (repositoryLeaseState lease == RepositoryLeaseActive) $
      throwError RepositoryAdvertLeaseInactive
    unless (repositoryLeaseSnapshot lease == Just snapshot) $
      throwError RepositoryAdvertSnapshotMismatch
    unless (now < maxBound && lifetime > 0
            && lifetime <= maxReplicaAdvertLifetime) $
      throwError RepositoryAdvertInvalidLifetime
    when (null reachable) $
      throwError RepositoryAdvertNoReachableAddress
    when (length reachable > maxReplicaAdvertAddresses) $
      throwError RepositoryAdvertTooManyAddresses
    unless (capabilities .&. replicaAdvertCapabilityBlockDownload /= 0) $
      throwError RepositoryAdvertCannotServeBlocks

    (receiptHash, receiptSigner, receipt) <-
      liftIO (readRepositoryReplicaReceipt sto (repositoryLeaseId lease))
        >>= either (const (throwError RepositoryAdvertReceiptMismatch)) pure

    let provider = view peerSignPk credentials
    unless (receiptSigner == provider
            && receiptMatchesAdvert provider lease snapshot receipt) $
      throwError RepositoryAdvertReceiptMismatch

    let expiresAt = now + min lifetime (maxBound - now)
        payload = ReplicaAdvertV1
          { replicaAdvertVersion = 1
          , replicaAdvertRepository = repositoryLeaseRepository lease
          , replicaAdvertProvider = provider
          , replicaAdvertSnapshotId = repositorySnapshotId snapshot
          , replicaAdvertReceipt = receiptHash
          , replicaAdvertReachableVia = reachable
          , replicaAdvertCapabilities = capabilities
          , replicaAdvertIssuedAt = now
          , replicaAdvertExpiresAt = expiresAt
          }
        box = makeSignedBox @'HBS2Basic provider
                (view peerSignSk credentials) payload
        boxBytes = serialise box
        advertRef = RepositoryReplicaAdvert
          (repositoryLeaseRepository lease) provider

    when (LBS.length boxBytes > fromIntegral repositoryAdvertMaxBoxBytes) $
      throwError RepositoryAdvertTooLarge

    advertHash <- putBlock sto boxBytes
      >>= maybe (throwError RepositoryAdvertStoreFailed) (pure . HashRef)
    updateRef sto advertRef (fromHashRef advertHash)

    (storedHash, signer, storedPayload) <-
      liftIO (readRepositoryReplicaAdvertAt now sto
        (repositoryLeaseRepository lease) provider)
        >>= either throwError pure
    unless (storedHash == advertHash && signer == provider
            && storedPayload == payload) $
      throwError RepositoryAdvertMismatch

    pure (advertHash, payload)

-- | Read through the provider-owned ref and reject malformed, forged, future,
-- expired, or identity-mismatched hints before lookup code can return them.
readRepositoryReplicaAdvertAt
  :: Word64
  -> AnyStorage
  -> PubKey 'Sign 'HBS2Basic
  -> PubKey 'Sign 'HBS2Basic
  -> IO (Either RepositoryAdvertError
          (HashRef, PubKey 'Sign 'HBS2Basic, ReplicaAdvertV1))
readRepositoryReplicaAdvertAt now sto repository provider = runExceptT do
  advertHash <- getRef sto (RepositoryReplicaAdvert repository provider)
    >>= maybe (throwError RepositoryAdvertMissing) (pure . HashRef)
  bytes <- getBlock sto (fromHashRef advertHash)
    >>= maybe (throwError (RepositoryAdvertBlockMissing advertHash)) pure
  (signer, payload) <- liftIO (decodeRepositoryReplicaAdvertAt now bytes)
    >>= either (throwError . localAdvertError advertHash) pure
  unless (signer == provider
          && replicaAdvertProvider payload == provider
          && replicaAdvertRepository payload == repository) $
    throwError RepositoryAdvertMismatch
  pure (advertHash, signer, payload)

-- | Verify an opaque network advert without trusting the relay which carried
-- it.  Receipt availability is checked later by the retrieval path; discovery
-- accepts only a provider-signed, short-lived and usable hint.
decodeRepositoryReplicaAdvertAt
  :: Word64
  -> ByteString
  -> IO (Either RepositoryAdvertError
          (PubKey 'Sign 'HBS2Basic, ReplicaAdvertV1))
decodeRepositoryReplicaAdvertAt now bytes = runExceptT do
  when (LBS.length bytes > fromIntegral repositoryAdvertMaxBoxBytes) $
    throwError RepositoryAdvertTooLarge
  box <- either (const (throwError RepositoryAdvertWireMalformed)) pure $
    deserialiseOrFail @(SignedBox ReplicaAdvertV1 'HBS2Basic) bytes
  (signer, payload) <- maybe
    (throwError RepositoryAdvertWireInvalid)
    pure
    (unboxSignedBox0 box)
  let issuedAt = replicaAdvertIssuedAt payload
      expiresAt = replicaAdvertExpiresAt payload
      reachable = replicaAdvertReachableVia payload
      validShape = and
        [ replicaAdvertVersion payload == 1
        , signer == replicaAdvertProvider payload
        , issuedAt < expiresAt
        , expiresAt - issuedAt <= maxReplicaAdvertLifetime
        , not (null reachable)
        , length reachable <= maxReplicaAdvertAddresses
        , replicaAdvertCanServeBlocks payload
        ]
  unless validShape $
    throwError RepositoryAdvertMismatch
  when (now < issuedAt) $
    throwError RepositoryAdvertNotYetValid
  when (now >= expiresAt) $
    throwError RepositoryAdvertExpired
  pure (signer, payload)

localAdvertError :: HashRef -> RepositoryAdvertError -> RepositoryAdvertError
localAdvertError advertHash = \case
  RepositoryAdvertWireMalformed -> RepositoryAdvertMalformed advertHash
  RepositoryAdvertWireInvalid -> RepositoryAdvertInvalid advertHash
  other -> other

releaseRepositoryReplicaAdvert
  :: AnyStorage
  -> PubKey 'Sign 'HBS2Basic
  -> PubKey 'Sign 'HBS2Basic
  -> IO ()
releaseRepositoryReplicaAdvert sto repository provider =
  delRef sto (RepositoryReplicaAdvert repository provider)

maxReplicaAdvertLifetime :: Word64
maxReplicaAdvertLifetime = 15 * 60

maxReplicaAdvertAddresses :: Int
maxReplicaAdvertAddresses = 8

receiptMatchesAdvert
  :: PubKey 'Sign 'HBS2Basic
  -> RepositoryLease
  -> RepositorySnapshot
  -> ReplicaReceiptV1
  -> Bool
receiptMatchesAdvert provider lease snapshot receipt =
  actual == expected
  where
    actual =
      ( replicaReceiptVersion receipt
      , replicaReceiptLeaseId receipt
      , replicaReceiptRepository receipt
      , replicaReceiptProvider receipt
      , replicaReceiptSnapshotId receipt
      , replicaReceiptInventoryRoot receipt
      , replicaReceiptBlockCount receipt
      , replicaReceiptLogicalBytes receipt
      , replicaReceiptCompletedAt receipt
      )
    expected =
      ( 1
      , repositoryLeaseId lease
      , repositoryLeaseRepository lease
      , provider
      , repositorySnapshotId snapshot
      , repositorySnapshotInventoryRoot snapshot
      , repositorySnapshotBlockCount snapshot
      , repositorySnapshotLogicalBytes snapshot
      , repositorySnapshotCompletedAt snapshot
      )
