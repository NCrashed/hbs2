module HBS2.Peer.RepositoryLease.Discovery
  ( RepositoryAdvertCache
  , RepositoryAdvertCacheError(..)
  , RepositoryAdvertCacheUpdate(..)
  , newRepositoryAdvertCache
  , insertRepositoryAdvertAt
  , lookupRepositoryAdvertsAt
  , pruneRepositoryAdvertCacheAt
  , repositoryAdvertCacheSizeAt
  , repositoryAdvertCacheMaxRepositories
  , repositoryAdvertCacheMaxProviders
  , RepositoryAdvertQueries
  , newRepositoryAdvertQueries
  , newRepositoryAdvertQueryId
  , registerRepositoryAdvertQueryAt
  , acceptRepositoryAdvertQueryAt
  , acceptRepositoryAdvertResultsAt
  , repositoryAdvertPendingQueryCountAt
  , repositoryAdvertQueryTTL
  , repositoryAdvertMaxPendingQueries
  , requestRepositoryAdverts
  , lookupRepositoryReplicaAdvertsAt
  , findRepositoryAdvertProvider
  , repositoryAdvertProto
  ) where

import HBS2.Net.PeerLocator
import HBS2.Net.Proto
import HBS2.Net.Proto.Sessions
import HBS2.Net.Auth.Schema()
import HBS2.Actors.Peer.Types (HasFabriq, HasOwnPeer, HasTimeLimits)
import HBS2.Peer.Proto.Peer
import HBS2.Peer.Proto.RepositoryLease
import HBS2.Peer.RepositoryLease
import HBS2.Prelude.Plated

import Control.Concurrent.STM
import Crypto.Saltine.Core.Utils (randomByteString)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Word

data RepositoryAdvertCacheError
  = RepositoryAdvertCacheInvalid RepositoryAdvertError
  | RepositoryAdvertCacheFull
  deriving stock (Eq, Show)

data RepositoryAdvertCacheUpdate
  = RepositoryAdvertCacheInserted
  | RepositoryAdvertCacheReplaced
  | RepositoryAdvertCacheUnchanged
  | RepositoryAdvertCacheStale
  deriving stock (Eq, Show)

data CachedRepositoryAdvert = CachedRepositoryAdvert
  { cachedRepositoryAdvertBytes   :: ByteString
  , cachedRepositoryAdvertPayload :: ReplicaAdvertV1
  }

type RepositoryAdvertCacheState =
  Map
    (PubKey 'Sign 'HBS2Basic)
    (Map (PubKey 'Sign 'HBS2Basic) CachedRepositoryAdvert)

newtype RepositoryAdvertCache = RepositoryAdvertCache
  (TVar RepositoryAdvertCacheState)

data PendingRepositoryAdvertQuery peer = PendingRepositoryAdvertQuery
  { pendingRepositoryAdvertRepository :: PubKey 'Sign 'HBS2Basic
  , pendingRepositoryAdvertPeers      :: HashSet peer
  , pendingRepositoryAdvertIssuedAt   :: Word64
  }

newtype RepositoryAdvertQueries peer = RepositoryAdvertQueries
  (TVar (Map RepositoryAdvertQueryId (PendingRepositoryAdvertQuery peer)))

repositoryAdvertCacheMaxRepositories :: Int
repositoryAdvertCacheMaxRepositories = 1024

repositoryAdvertCacheMaxProviders :: Int
repositoryAdvertCacheMaxProviders =
  fromIntegral repositoryAdvertMaxResults

repositoryAdvertQueryTTL :: Word64
repositoryAdvertQueryTTL = 30

repositoryAdvertMaxPendingQueries :: Int
repositoryAdvertMaxPendingQueries = 128

newRepositoryAdvertCache :: IO RepositoryAdvertCache
newRepositoryAdvertCache = RepositoryAdvertCache <$> newTVarIO Map.empty

newRepositoryAdvertQueries :: IO (RepositoryAdvertQueries peer)
newRepositoryAdvertQueries = RepositoryAdvertQueries <$> newTVarIO Map.empty

newRepositoryAdvertQueryId :: IO RepositoryAdvertQueryId
newRepositoryAdvertQueryId =
  RepositoryAdvertQueryId <$> randomByteString 24

registerRepositoryAdvertQueryAt
  :: (Eq peer, Hashable peer)
  => Word64
  -> RepositoryAdvertQueries peer
  -> RepositoryAdvertQueryId
  -> PubKey 'Sign 'HBS2Basic
  -> [peer]
  -> IO Bool
registerRepositoryAdvertQueryAt
  now (RepositoryAdvertQueries queriesVar) query repository peers = atomically do
    queries <- pruneRepositoryAdvertQueriesAt now <$> readTVar queriesVar
    let expectedPeers = HashSet.fromList peers
        canRegister = not (HashSet.null expectedPeers)
          && Map.notMember query queries
          && Map.size queries < repositoryAdvertMaxPendingQueries
        next
          | canRegister = Map.insert query
              PendingRepositoryAdvertQuery
                { pendingRepositoryAdvertRepository = repository
                , pendingRepositoryAdvertPeers = expectedPeers
                , pendingRepositoryAdvertIssuedAt = now
                }
              queries
          | otherwise = queries
    writeTVar queriesVar next
    pure canRegister

acceptRepositoryAdvertQueryAt
  :: (Eq peer, Hashable peer)
  => Word64
  -> RepositoryAdvertQueries peer
  -> peer
  -> RepositoryAdvertQueryId
  -> PubKey 'Sign 'HBS2Basic
  -> IO Bool
acceptRepositoryAdvertQueryAt
  now (RepositoryAdvertQueries queriesVar) peer query repository = atomically do
    queries <- pruneRepositoryAdvertQueriesAt now <$> readTVar queriesVar
    writeTVar queriesVar queries
    pure $ case Map.lookup query queries of
      Nothing -> False
      Just pending ->
           pendingRepositoryAdvertRepository pending == repository
        && peer `HashSet.member` pendingRepositoryAdvertPeers pending

repositoryAdvertPendingQueryCountAt
  :: Word64
  -> RepositoryAdvertQueries peer
  -> IO Int
repositoryAdvertPendingQueryCountAt
  now (RepositoryAdvertQueries queriesVar) = atomically do
    queries <- pruneRepositoryAdvertQueriesAt now <$> readTVar queriesVar
    writeTVar queriesVar queries
    pure (Map.size queries)

pruneRepositoryAdvertQueriesAt
  :: Word64
  -> Map RepositoryAdvertQueryId (PendingRepositoryAdvertQuery peer)
  -> Map RepositoryAdvertQueryId (PendingRepositoryAdvertQuery peer)
pruneRepositoryAdvertQueriesAt now =
  Map.filter $ \pending ->
    repositoryAdvertQueryAge
      now (pendingRepositoryAdvertIssuedAt pending) < repositoryAdvertQueryTTL

repositoryAdvertQueryAge :: Word64 -> Word64 -> Word64
repositoryAdvertQueryAge now issuedAt
  | now >= issuedAt = now - issuedAt
  | otherwise = issuedAt - now

insertRepositoryAdvertAt
  :: Word64
  -> RepositoryAdvertCache
  -> ByteString
  -> IO (Either RepositoryAdvertCacheError RepositoryAdvertCacheUpdate)
insertRepositoryAdvertAt now (RepositoryAdvertCache cacheVar) bytes = do
  verified <- decodeRepositoryReplicaAdvertAt now (LBS.fromStrict bytes)
  case verified of
    Left problem -> pure (Left (RepositoryAdvertCacheInvalid problem))
    Right (provider, advert) -> atomically do
      state <- pruneRepositoryAdvertCacheStateAt now <$> readTVar cacheVar
      let (result, next) = insertVerifiedRepositoryAdvert
            bytes provider advert state
      writeTVar cacheVar next
      pure result

insertVerifiedRepositoryAdvert
  :: ByteString
  -> PubKey 'Sign 'HBS2Basic
  -> ReplicaAdvertV1
  -> RepositoryAdvertCacheState
  -> ( Either RepositoryAdvertCacheError RepositoryAdvertCacheUpdate
     , RepositoryAdvertCacheState
     )
insertVerifiedRepositoryAdvert bytes provider advert state =
  maybe insertNew replaceExisting (Map.lookup provider providers)
  where
    repository = replicaAdvertRepository advert
    providers = Map.findWithDefault Map.empty repository state
    entry = CachedRepositoryAdvert bytes advert
    save updateKind =
      (Right updateKind, Map.insert repository (Map.insert provider entry providers) state)

    replaceExisting previous
      | newIssue < oldIssue = (Right RepositoryAdvertCacheStale, state)
      | newIssue > oldIssue = save RepositoryAdvertCacheReplaced
      | bytes == cachedRepositoryAdvertBytes previous =
          (Right RepositoryAdvertCacheUnchanged, state)
      | otherwise = (Right RepositoryAdvertCacheStale, state)
      where
        newIssue = replicaAdvertIssuedAt advert
        oldIssue = replicaAdvertIssuedAt
          (cachedRepositoryAdvertPayload previous)

    insertNew
      | repositoryCacheFull repository state = cacheFull
      | Map.size providers >= repositoryAdvertCacheMaxProviders = cacheFull
      | otherwise = save RepositoryAdvertCacheInserted

    cacheFull = (Left RepositoryAdvertCacheFull, state)

repositoryCacheFull
  :: PubKey 'Sign 'HBS2Basic
  -> RepositoryAdvertCacheState
  -> Bool
repositoryCacheFull repository state =
     Map.notMember repository state
  && Map.size state >= repositoryAdvertCacheMaxRepositories

lookupRepositoryAdvertsAt
  :: Word64
  -> RepositoryAdvertCache
  -> PubKey 'Sign 'HBS2Basic
  -> Word16
  -> IO [ByteString]
lookupRepositoryAdvertsAt now cache repository requested =
  fmap cachedRepositoryAdvertBytes <$>
    lookupRepositoryAdvertEntriesAt now cache repository requested

lookupRepositoryReplicaAdvertsAt
  :: Word64
  -> RepositoryAdvertCache
  -> PubKey 'Sign 'HBS2Basic
  -> Word16
  -> IO [ReplicaAdvertV1]
lookupRepositoryReplicaAdvertsAt
  now cache repository requested =
    fmap cachedRepositoryAdvertPayload <$>
      lookupRepositoryAdvertEntriesAt now cache repository requested

-- | Try only providers which claim the ordinary block-download capability.
-- Every advertised address is attempted, but an advert is accepted only when
-- the caller confirms that the endpoint authenticated as the signed provider.
findRepositoryAdvertProvider
  :: Monad m
  => (PubKey 'Sign 'HBS2Basic -> PeerAddr L4Proto -> m Bool)
  -> [ReplicaAdvertV1]
  -> m (Maybe ReplicaAdvertV1)
findRepositoryAdvertProvider dial = go
  where
    go [] = pure Nothing
    go (advert : rest)
      | not (replicaAdvertCanServeBlocks advert) = go rest
      | otherwise = do
          connected <- anyM
            (dial (replicaAdvertProvider advert))
            (replicaAdvertReachableVia advert)
          if connected then pure (Just advert) else go rest

    anyM _ [] = pure False
    anyM predicate (value : values) = do
      matched <- predicate value
      if matched then pure True else anyM predicate values

lookupRepositoryAdvertEntriesAt
  :: Word64
  -> RepositoryAdvertCache
  -> PubKey 'Sign 'HBS2Basic
  -> Word16
  -> IO [CachedRepositoryAdvert]
lookupRepositoryAdvertEntriesAt
  now (RepositoryAdvertCache cacheVar) repository requested = atomically do
    state <- pruneRepositoryAdvertCacheStateAt now <$> readTVar cacheVar
    writeTVar cacheVar state
    let resultLimit = fromIntegral (min requested repositoryAdvertMaxResults)
    pure $ take resultLimit $
      Map.elems
        (Map.findWithDefault Map.empty repository state)

pruneRepositoryAdvertCacheAt
  :: Word64
  -> RepositoryAdvertCache
  -> IO ()
pruneRepositoryAdvertCacheAt now (RepositoryAdvertCache cacheVar) =
  atomically $ modifyTVar' cacheVar (pruneRepositoryAdvertCacheStateAt now)

repositoryAdvertCacheSizeAt
  :: Word64
  -> RepositoryAdvertCache
  -> IO Int
repositoryAdvertCacheSizeAt now cache@(RepositoryAdvertCache cacheVar) = do
  pruneRepositoryAdvertCacheAt now cache
  sum . fmap Map.size . Map.elems <$> readTVarIO cacheVar

pruneRepositoryAdvertCacheStateAt
  :: Word64
  -> RepositoryAdvertCacheState
  -> RepositoryAdvertCacheState
pruneRepositoryAdvertCacheStateAt now =
  Map.mapMaybe nonEmpty . fmap (Map.filter isLive)
  where
    isLive = (> now)
      . replicaAdvertExpiresAt
      . cachedRepositoryAdvertPayload
    nonEmpty providers
      | Map.null providers = Nothing
      | otherwise = Just providers

requestRepositoryAdverts
  :: ( MonadIO m
     , Request L4Proto RepositoryAdvertMessage m
     , HasPeerLocator L4Proto m
     , Sessions L4Proto (KnownPeer L4Proto) m
     )
  => RepositoryAdvertQueries (Peer L4Proto)
  -> PubKey 'Sign 'HBS2Basic
  -> Word16
  -> m (Maybe RepositoryAdvertQueryId)
requestRepositoryAdverts queries repository requested = do
  locator <- getPeerLocator @L4Proto
  locatedPeers <- knownPeers @L4Proto locator
  peers <- filterM
    (\peer -> find (KnownPeerKey peer) id <&> isJust)
    locatedPeers
  requestRepositoryAdvertsFromPeers
    queries repository requested peers

requestRepositoryAdvertsFromPeers
  :: (MonadIO m, Request L4Proto RepositoryAdvertMessage m)
  => RepositoryAdvertQueries (Peer L4Proto)
  -> PubKey 'Sign 'HBS2Basic
  -> Word16
  -> [Peer L4Proto]
  -> m (Maybe RepositoryAdvertQueryId)
requestRepositoryAdvertsFromPeers _ _ _ [] = pure Nothing
requestRepositoryAdvertsFromPeers queries repository requested peers = do
  query <- liftIO newRepositoryAdvertQueryId
  now <- liftIO currentTimestamp
  registered <- liftIO $ registerRepositoryAdvertQueryAt
    now queries query repository peers
  sendRepositoryAdvertQuery
    registered peers query repository requested

sendRepositoryAdvertQuery
  :: (Monad m, Request L4Proto RepositoryAdvertMessage m)
  => Bool
  -> [Peer L4Proto]
  -> RepositoryAdvertQueryId
  -> PubKey 'Sign 'HBS2Basic
  -> Word16
  -> m (Maybe RepositoryAdvertQueryId)
sendRepositoryAdvertQuery False _ _ _ _ = pure Nothing
sendRepositoryAdvertQuery True peers query repository requested = do
  let resultLimit = min requested repositoryAdvertMaxResults
  for_ peers $ \peer -> request @L4Proto peer $
    RepositoryAdvertLookup query repository resultLimit
  pure (Just query)

repositoryAdvertProto
  :: forall m
   . ( MonadIO m
     , Response L4Proto RepositoryAdvertMessage m
     , Request L4Proto RepositoryAdvertMessage m
     , HasDeferred RepositoryAdvertMessage L4Proto m
     , HasPeerLocator L4Proto m
     , Sessions L4Proto (KnownPeer L4Proto) m
     , HasFabriq L4Proto m
     , HasOwnPeer L4Proto m
     , HasTimeLimits L4Proto RepositoryAdvertMessage m
     )
  => RepositoryAdvertCache
  -> RepositoryAdvertQueries (Peer L4Proto)
  -> RepositoryAdvertMessage
  -> m ()
repositoryAdvertProto cache queries = \case
  RepositoryAdvertPublish bytes ->
    acceptRepositoryAdvertPublication cache bytes
  RepositoryAdvertLookup query repository requested ->
    answerRepositoryAdvertLookup cache query repository requested
  RepositoryAdvertResults query repository adverts ->
    acceptRepositoryAdvertResults cache queries query repository adverts

acceptRepositoryAdvertPublication
  :: ( MonadIO m
     , Request L4Proto RepositoryAdvertMessage m
     , HasPeerLocator L4Proto m
     , Sessions L4Proto (KnownPeer L4Proto) m
     , Response L4Proto RepositoryAdvertMessage m
     )
  => RepositoryAdvertCache
  -> ByteString
  -> m ()
acceptRepositoryAdvertPublication cache bytes = do
  source <- thatPeer @RepositoryAdvertMessage
  updateResult <- liftIO $ currentTimestamp >>= \now ->
    insertRepositoryAdvertAt now cache bytes
  when (updateResult `elem`
    [ Right RepositoryAdvertCacheInserted
    , Right RepositoryAdvertCacheReplaced
    ]) $
      relayRepositoryAdvert source bytes

answerRepositoryAdvertLookup
  :: ( MonadIO m
     , Response L4Proto RepositoryAdvertMessage m
     , HasDeferred RepositoryAdvertMessage L4Proto m
     )
  => RepositoryAdvertCache
  -> RepositoryAdvertQueryId
  -> PubKey 'Sign 'HBS2Basic
  -> Word16
  -> m ()
answerRepositoryAdvertLookup cache query repository requested =
  deferred @RepositoryAdvertMessage do
    now <- liftIO currentTimestamp
    adverts <- liftIO $
      lookupRepositoryAdvertsAt now cache repository requested
    response $ RepositoryAdvertResults query repository adverts

acceptRepositoryAdvertResults
  :: (MonadIO m, Response L4Proto RepositoryAdvertMessage m)
  => RepositoryAdvertCache
  -> RepositoryAdvertQueries (Peer L4Proto)
  -> RepositoryAdvertQueryId
  -> PubKey 'Sign 'HBS2Basic
  -> [ByteString]
  -> m ()
acceptRepositoryAdvertResults cache queries query repository adverts = do
  source <- thatPeer @RepositoryAdvertMessage
  now <- liftIO currentTimestamp
  void $ liftIO $ acceptRepositoryAdvertResultsAt
    now cache queries source query repository adverts

acceptRepositoryAdvertResultsAt
  :: (Eq peer, Hashable peer)
  => Word64
  -> RepositoryAdvertCache
  -> RepositoryAdvertQueries peer
  -> peer
  -> RepositoryAdvertQueryId
  -> PubKey 'Sign 'HBS2Basic
  -> [ByteString]
  -> IO Bool
acceptRepositoryAdvertResultsAt
  now cache queries source query repository adverts = do
    expected <- acceptRepositoryAdvertQueryAt
      now queries source query repository
    when expected $
      for_ adverts $ cacheMatchingRepositoryAdvertAt now cache repository
    pure expected

cacheMatchingRepositoryAdvertAt
  :: Word64
  -> RepositoryAdvertCache
  -> PubKey 'Sign 'HBS2Basic
  -> ByteString
  -> IO ()
cacheMatchingRepositoryAdvertAt now cache repository bytes = do
  decoded <- decodeRepositoryReplicaAdvertAt now (LBS.fromStrict bytes)
  let matches = either (const False)
        ((== repository) . replicaAdvertRepository . snd) decoded
  when matches $
    void (insertRepositoryAdvertAt now cache bytes)

relayRepositoryAdvert
  :: ( MonadIO m
     , Request L4Proto RepositoryAdvertMessage m
     , HasPeerLocator L4Proto m
     , Sessions L4Proto (KnownPeer L4Proto) m
     )
  => Peer L4Proto
  -> ByteString
  -> m ()
relayRepositoryAdvert source bytes = do
  locator <- getPeerLocator @L4Proto
  peers <- knownPeers @L4Proto locator
  for_ peers $ \peer -> when (peer /= source) $
    relayRepositoryAdvertToPeer peer bytes

relayRepositoryAdvertToPeer
  :: ( Request L4Proto RepositoryAdvertMessage m
     , Sessions L4Proto (KnownPeer L4Proto) m
     )
  => Peer L4Proto
  -> ByteString
  -> m ()
relayRepositoryAdvertToPeer peer bytes = do
  authenticated <- find (KnownPeerKey peer) id <&> isJust
  when authenticated $
    request @L4Proto peer (RepositoryAdvertPublish bytes)

currentTimestamp :: IO Word64
currentTimestamp = floor <$> getPOSIXTime
