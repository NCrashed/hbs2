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
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
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

repositoryAdvertCacheMaxRepositories :: Int
repositoryAdvertCacheMaxRepositories = 1024

repositoryAdvertCacheMaxProviders :: Int
repositoryAdvertCacheMaxProviders =
  fromIntegral repositoryAdvertMaxResults

newRepositoryAdvertCache :: IO RepositoryAdvertCache
newRepositoryAdvertCache = RepositoryAdvertCache <$> newTVarIO Map.empty

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
lookupRepositoryAdvertsAt now (RepositoryAdvertCache cacheVar) repository requested =
  atomically do
    state <- pruneRepositoryAdvertCacheStateAt now <$> readTVar cacheVar
    writeTVar cacheVar state
    let resultLimit = fromIntegral (min requested repositoryAdvertMaxResults)
    pure $ take resultLimit $
      cachedRepositoryAdvertBytes <$> Map.elems
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
  -> RepositoryAdvertMessage
  -> m ()
repositoryAdvertProto cache = \case
  RepositoryAdvertPublish bytes -> do
    source <- thatPeer @RepositoryAdvertMessage
    updateResult <- liftIO $ currentTimestamp >>= \now ->
      insertRepositoryAdvertAt now cache bytes
    when (updateResult `elem`
      [ Right RepositoryAdvertCacheInserted
      , Right RepositoryAdvertCacheReplaced
      ]) $
        relayRepositoryAdvert source bytes

  RepositoryAdvertLookup query repository requested -> deferred @RepositoryAdvertMessage do
    now <- liftIO currentTimestamp
    adverts <- liftIO $
      lookupRepositoryAdvertsAt now cache repository requested
    response $ RepositoryAdvertResults query repository adverts

  RepositoryAdvertResults _ repository adverts -> do
    now <- liftIO currentTimestamp
    for_ adverts $ \bytes -> liftIO do
      decoded <- decodeRepositoryReplicaAdvertAt now (LBS.fromStrict bytes)
      case decoded of
        Right (_, advert)
          | replicaAdvertRepository advert == repository ->
              void (insertRepositoryAdvertAt now cache bytes)
        _ -> pure ()
  where
    relayRepositoryAdvert source bytes = do
      locator <- getPeerLocator @L4Proto
      peers <- knownPeers @L4Proto locator
      for_ peers $ \peer -> when (peer /= source) do
        authenticated <- find (KnownPeerKey peer) id <&> isJust
        when authenticated $
          request @L4Proto peer (RepositoryAdvertPublish bytes)

currentTimestamp :: IO Word64
currentTimestamp = floor <$> getPOSIXTime
