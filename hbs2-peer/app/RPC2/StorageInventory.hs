{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module RPC2.StorageInventory where

import HBS2.Net.Proto.Service
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.Brains
import Control.Monad.IO.Class (MonadIO, liftIO)

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m)
  => HandleMethod m RpcStorageInventory where
  handleMethod _ = do
    context <- getRpcContext @PeerAPI
    liftIO (rpcCollectStorageInventory context)

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m)
  => HandleMethod m RpcStorageCleanup where
  handleMethod execute = do
    context <- getRpcContext @PeerAPI
    liftIO (rpcCleanupStorageInventory context execute)

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m)
  => HandleMethod m RpcStoragePinPut where
  handleMethod pin = do
    context <- getRpcContext @PeerAPI
    putStoragePin @L4Proto (rpcBrains context) pin

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m)
  => HandleMethod m RpcStoragePinDelete where
  handleMethod name = do
    context <- getRpcContext @PeerAPI
    deleteStoragePin @L4Proto (rpcBrains context) name

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m)
  => HandleMethod m RpcStoragePinList where
  handleMethod _ = do
    context <- getRpcContext @PeerAPI
    listStoragePins @L4Proto (rpcBrains context)
