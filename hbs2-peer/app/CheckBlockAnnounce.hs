{-# Language TypeOperators #-}
module CheckBlockAnnounce where

import HBS2.Prelude.Plated
import HBS2.Actors.Peer
import HBS2.Data.Types.Peer (peerSignKey)
import HBS2.Hash
import HBS2.Peer.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Net.Proto.Types

import PeerTypes
import PeerConfig
import CheckPeer (peerBanned)
import BlockAnnouncePolicy

import Control.Monad.Trans.Maybe
import Lens.Micro.Platform


acceptAnnouncesFromPeer :: forall e m . ( MonadIO m
                                        , Sessions e (KnownPeer e) m
                                        , IsPeerAddr e m
                                        , Ord (PubKey 'Sign (Encryption e))
                                        , FromStringMaybe (PubKey 'Sign (Encryption e))
                                        , Ord (PubKey 'Sign (Encryption e))
                                        , e ~ L4Proto
                                        )
                      => PeerConfig
                      -> PeerAddr e
                      -> m Bool
acceptAnnouncesFromPeer conf pa = do
  pip <- fromPeerAddr @e pa
  known <- find @e (KnownPeerKey pip) id >>= traverse \pd -> do
    banned <- peerBanned conf pd
    pure (banned, view peerSignKey pd)
  pure $ announceAllowed (announcePolicy conf) known


checkBlockAnnounce :: forall e m . ( e ~ L4Proto
                                   , m ~ PeerM e IO
                                   )
                   => PeerConfig
                   -> PeerEnv e
                   -> PeerNonce
                   -> PeerAddr e
                   -> Hash HbSync
                   -> m ()

checkBlockAnnounce conf penv nonce pa h = void $ runMaybeT do

  accept <- lift $ acceptAnnouncesFromPeer conf pa

  myNonce  <- lift $ peerNonce @e

  guard (nonce /= myNonce)

  debug $ "Accept announce from" <+> pretty pa <+> pretty accept

  guard accept

  liftIO $ withPeerM penv do
      addDownload @e Nothing h
