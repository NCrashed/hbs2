{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.RepositoryLease where

import HBS2.Peer.Prelude
import HBS2.Net.Proto.Service
import HBS2.Peer.Brains
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.Internal.Types

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m)
  => HandleMethod m RpcRepositoryLeasePut where
  handleMethod lease = do
    brains <- getRpcContext @PeerAPI <&> rpcBrains
    putRepositoryLease @L4Proto brains lease

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
