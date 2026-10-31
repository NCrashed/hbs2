{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.Peers where

import HBS2.Actors.Peer
import HBS2.Data.Types.Peer
import HBS2.Net.Proto.Types
import HBS2.Net.Proto.Service
import HBS2.Peer.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated

import PeerTypes

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.Peer

import Control.Concurrent.STM (readTVarIO)
import Lens.Micro.Platform
import Data.Maybe
import Data.Set qualified as Set


instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcPeers where

  handleMethod _ = do
   co <- getRpcContext @PeerAPI
   withPeerM (rpcPeerEnv co) $ do
      ps <-  getKnownPeers @L4Proto
      r <- forM ps $ \p -> do
              mpde <- find (KnownPeerKey p) id
              maybe1 mpde (pure Nothing) $ \pde -> do
                pa <- toPeerAddr p
                let k = view peerSignKey pde
                pure $ Just (k, pa)

      pure $ catMaybes r


instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcPeerDiagnostics where

  handleMethod _ = do
   co <- getRpcContext @PeerAPI
   withPeerM (rpcPeerEnv co) $ do
      ps <- getKnownPeers @L4Proto
      catMaybes <$> forM ps peerDiagnosticFor


peerDiagnosticFor
  :: MonadIO m
  => Peer L4Proto
  -> PeerM L4Proto m (Maybe PeerDiagnostic)
peerDiagnosticFor peer = do
  peerData <- find (KnownPeerKey peer) id
  case peerData of
    Nothing -> pure Nothing
    Just observed -> do
      address <- toPeerAddr peer
      peerInfo <- find (PeerInfoKey peer) id
      metadata <- maybe (pure Nothing) (liftIO . readTVarIO . view peerMeta) peerInfo
      pure . Just $ PeerDiagnostic
        { peerDiagnosticKey = view peerSignKey observed
        , peerDiagnosticAddress = address
        , peerDiagnosticNetworkClasses = Set.toAscList (view peerReachableVia observed)
        , peerDiagnosticMetadata = metadata
        }
