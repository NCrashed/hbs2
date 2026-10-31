{-# Language AllowAmbiguousTypes #-}
{-# Language TypeOperators #-}
module Bootstrap where

import HBS2.Data.Types.Peer
import HBS2.Prelude
import HBS2.Net.Proto.Types
import HBS2.Peer.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Peer.Brains

import PeerConfig
import PeerLogger

import Network.DNS
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception qualified as Exception
import Control.Monad.Reader
import Data.ByteString.Char8 qualified as B8
import Data.Either (fromRight)
import Data.Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Data.List qualified as List
import Data.Text qualified as Text
import System.Timeout qualified as System


data PeerDnsBootStrapKey

data PeerKnownPeer

defaultBootstrapDns :: String
defaultBootstrapDns = "bootstrap.hbs2.app"

bootstrapDnsTimeoutMicros :: Int
bootstrapDnsTimeoutMicros = 2 * 1000 * 1000

instance HasCfgKey PeerDnsBootStrapKey (Set String) where
  key = "bootstrap-dns"

instance HasCfgKey PeerKnownPeer (Set String) where
  key = "known-peer"

bootstrapDnsEnabled :: PeerConfig -> Bool
bootstrapDnsEnabled (PeerConfig syn) =
  runReader (cfgValue @PeerBootstrapKey) syn == FeatureOn

bootstrapDnsNames :: PeerConfig -> [String]
bootstrapDnsNames (PeerConfig syn) = Set.toAscList $
  runReader (cfgValue @PeerDnsBootStrapKey) syn
    <> Set.singleton defaultBootstrapDns

bootstrapDnsProbe :: PeerConfig -> IO (Maybe [(Text.Text, Int)])
bootstrapDnsProbe conf
  | not (bootstrapDnsEnabled conf) = pure Nothing
  | otherwise = do
      let dns = bootstrapDnsNames conf
          failed = [(Text.pack name, 0) | name <- dns]
      checked <- Exception.try @Exception.SomeException do
        seed <- makeResolvSeed defaultResolvConf
        mapConcurrently (probeName seed) dns
      pure $ Just $ fromRight failed checked
  where
    probeName seed name = do
      addresses <- resolveBootstrapName seed name
      pure (Text.pack name, length addresses)

resolveBootstrapName :: ResolvSeed -> String -> IO [PeerAddr L4Proto]
resolveBootstrapName seed name = do
  result <- System.timeout bootstrapDnsTimeoutMicros $
    Exception.try @Exception.SomeException $
      withResolver seed $ \resolver -> lookupTXT resolver (B8.pack name)
  pure case result of
    Just (Right (Right answers)) ->
      mapMaybe (fromStringMay @(PeerAddr L4Proto) . B8.unpack) answers
    _ -> []

-- FIXME: tcp-addr-support-bootstrap
bootstrapDnsLoop :: forall e m . ( HasPeer e
                                 , Request e (PeerHandshake e) m
                                 , HasNonces (PeerHandshake e) m
                                 , Nonce (PeerHandshake e) ~ PingNonce
                                 , Sessions e (PeerHandshake e) m
                                 , Pretty (Peer e)
                                 -- , FromSockAddr 'UDP (Peer e)
                                 , e ~ L4Proto
                                 , MonadIO m
                                 )
                             => PeerConfig -> m ()

bootstrapDnsLoop conf = do

  pause @'Seconds 2

  rs <- liftIO $ makeResolvSeed defaultResolvConf

  forever do
    debug "I'm a bootstrapLoop"

    -- FIXME: utf8-domains
    for_ (bootstrapDnsNames conf) $ \dn -> do
      debug $ "bootstrapping from" <+> pretty dn
      answers <- liftIO $ resolveBootstrapName rs dn
      for_ answers $ \pa -> do
        -- FIXME: tcp-addr-support-1
        pip <- fromPeerAddr pa
        debug $ "BOOTSTRAP:" <+> pretty pip
        sendPing @e pip

    -- FIXME: fix-bootstrapDnsLoop-time-hardcode
    pause @'Seconds 300


-- FIXME: tcp-addr-support-known-peers-loop
knownPeersPingLoop :: forall e m . ( HasPeer e
                                   , Request e (PeerHandshake e) m
                                   , HasNonces (PeerHandshake e) m
                                   , Nonce (PeerHandshake e) ~ PingNonce
                                   , Sessions e (PeerHandshake e) m
                                   , Pretty (Peer e)
                                   , e ~ L4Proto
                                   , MonadIO m)
                   => PeerConfig
                   -> SomeBrains e
                   -> m ()
knownPeersPingLoop (PeerConfig syn) brains = do
  -- FIXME: add validation and error handling
  -- FIXME: tcp-addr-support-2
  let parseKnownPeers xs = do
        let pa = foldMap (maybeToList . fromStringMay) xs
        mapM fromPeerAddr pa

  let them = runReader (cfgValue @PeerKnownPeer) syn & Set.toList

  pex <- listPexInfo @e brains >>= liftIO . mapM fromPeerAddr

  knownPeers' <- liftIO $ parseKnownPeers them

  let pips = List.nub (knownPeers' <> pex)

  forever do
    forM_ pips (sendPing @e)
    pause @'Minutes 10
