{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.RepositoryLease where

import HBS2.Peer.Prelude
import HBS2.Net.Proto.Service
import HBS2.Peer.Brains
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RepositoryLease.Admission

import PeerConfig

import Data.Time.Clock.POSIX (getPOSIXTime)

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
