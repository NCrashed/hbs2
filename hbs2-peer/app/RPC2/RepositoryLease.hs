{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.RepositoryLease where

import HBS2.Peer.Prelude
import HBS2.Actors.Peer
import HBS2.Data.Types.Peer (peerSignKey)
import HBS2.Net.Proto.Service
import HBS2.Net.Proto.Sessions
import HBS2.Peer.Brains
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RepositoryLease.Admission
import HBS2.Peer.RepositoryLease.Discovery
import HBS2.Peer.Proto.Peer
import HBS2.Peer.Proto.RepositoryLease

import PeerConfig
import PeerTypes

import Lens.Micro.Platform

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m)
  => HandleMethod m RpcRepositoryLeasePut where
  handleMethod lease = do
    context <- getRpcContext @PeerAPI
    now <- liftIO (floor <$> getPOSIXTime)
    admitRepositoryLease @L4Proto
      (rpcBrains context)
      now
      (repositoryLeaseAdmissionPolicy (PeerConfig (rpcConfig context)))
      lease

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m)
  => HandleMethod m RpcRepositoryLeaseGet where
  handleMethod leaseId = do
    brains <- getRpcContext @PeerAPI <&> rpcBrains
    getRepositoryLease @L4Proto brains leaseId

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m)
  => HandleMethod m RpcRepositoryLeaseList where
  handleMethod () = do
    brains <- getRpcContext @PeerAPI <&> rpcBrains
    listRepositoryLeases @L4Proto brains

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m)
  => HandleMethod m RpcRepositoryLeaseCapacity where
  handleMethod () = do
    context <- getRpcContext @PeerAPI
    now <- liftIO (floor <$> getPOSIXTime)
    leases <- listRepositoryLeases @L4Proto (rpcBrains context)
    pure $ repositoryLeaseCapacityAt now
      (repositoryLeaseAdmissionPolicy (PeerConfig (rpcConfig context)))
      leases

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m)
  => HandleMethod m RpcRepositoryAdvertFind where
  handleMethod repository = do
    context <- getRpcContext @PeerAPI
    liftIO $ discoverRepositoryProvider context repository

discoverRepositoryProvider
  :: RPC2Context
  -> PubKey 'Sign 'HBS2Basic
  -> IO (Maybe ReplicaAdvertV1)
discoverRepositoryProvider context repository = do
  void $ withPeerM (rpcPeerEnv context) $
    requestRepositoryAdverts
      (rpcRepositoryAdvertQueries context)
      repository
      repositoryAdvertMaxResults
  pause @'Seconds 1
  now <- floor <$> getPOSIXTime
  adverts <- lookupRepositoryReplicaAdvertsAt
    now
    (rpcRepositoryAdvertCache context)
    repository
    repositoryAdvertMaxResults
  findRepositoryAdvertProvider (dialRepositoryProvider context) adverts

dialRepositoryProvider
  :: RPC2Context
  -> PubKey 'Sign 'HBS2Basic
  -> PeerAddr L4Proto
  -> IO Bool
dialRepositoryProvider context provider address =
  withPeerM (rpcPeerEnv context) do
    reachable <- pingPeerWait address
    if not reachable
      then pure False
      else do
        peer <- fromPeerAddr address
        authenticated <- find (KnownPeerKey peer) id
        pure $ maybe False ((== provider) . view peerSignKey) authenticated
