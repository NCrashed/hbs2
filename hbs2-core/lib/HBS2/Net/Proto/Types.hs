{-# Language TypeFamilyDependencies #-}
{-# Language FunctionalDependencies #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language TemplateHaskell #-}
{-# Language MultiWayIf #-}
module HBS2.Net.Proto.Types
  ( module HBS2.Net.Proto.Types
  ) where

import HBS2.Prelude.Plated
import HBS2.Net.IP.Addr

import Control.Applicative
import Data.Digest.Murmur32
import Data.Hashable
import Data.Kind
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word16)
import GHC.TypeLits
import Lens.Micro.Platform
import Network.Socket
import System.Random qualified as Random
import Codec.Serialise
import Data.Maybe
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString)

-- e -> Transport (like, UDP or TChan)
-- p -> L4 Protocol (like Ping/Pong)

class NonceFrom nonce a where
  nonceFrom :: a -> nonce

data CryptoAction = Sign | Encrypt

data GroupKeyScheme = Symm | Asymm
  deriving stock (Eq,Ord,Show,Data,Generic)

data CryptoScheme = HBS2Basic

type family PubKey  (a :: CryptoAction) (s :: CryptoScheme) :: Type

type family PrivKey (a :: CryptoAction) (s :: CryptoScheme) :: Type

type family Encryption e :: CryptoScheme

type instance Encryption L4Proto = 'HBS2Basic

type family KeyActionOf k :: CryptoAction

data family GroupKey (scheme :: GroupKeyScheme) (s :: CryptoScheme)

-- NOTE: throws-error
class  MonadIO m => HasDerivedKey s (a :: CryptoAction) nonce m where
  derivedKey :: nonce -> PrivKey a s -> m (PubKey a s, PrivKey a s)

-- TODO: move-to-an-appropriate-place
newtype AsGroupKeyFile a = AsGroupKeyFile a

data family ToEncrypt (scheme :: GroupKeyScheme) (s :: CryptoScheme) a -- = ToEncrypt a

data family ToDecrypt (scheme :: GroupKeyScheme) (s :: CryptoScheme) a

-- FIXME: move-to-a-crypto-definition-modules

data L4Proto = UDP | TCP
               deriving stock (Eq,Ord,Generic)
               deriving stock (Enum,Bounded)

instance Hashable L4Proto where
  hashWithSalt s l = hashWithSalt s ("l4proto" :: String, fromEnum l)

instance Show L4Proto where
  show UDP = "udp"
  show TCP = "tcp"

instance Pretty L4Proto where
  pretty UDP = "udp"
  pretty TCP = "tcp"

-- type family Encryption e :: Type

class Monad m => GenCookie e m where
  genCookie :: Hashable salt => salt -> m (Cookie e)


class Monad m => HasNonces p m where
  type family Nonce p :: Type
  newNonce :: m (Nonce p)


class HasCookie e p | p -> e where
  type family Cookie e :: Type
  getCookie :: p -> Maybe (Cookie e)
  getCookie = const Nothing

type PeerNonce = Nonce ()

class HasPeerNonce e m where
  peerNonce :: m PeerNonce

-- instance {-# OVERLAPPABLE #-} HasPeerNonce e IO where
--   peerNonce = newNonce @()


data WithCookie e p = WithCookie (Cookie e) p

class (Hashable (Peer e), Eq (Peer e)) => HasPeer e where
  data family (Peer e) :: Type

class ( Eq (PeerAddr e)
      , Monad m
      , Hashable (PeerAddr e)
      ) => IsPeerAddr e m where
  data family PeerAddr e :: Type

  toPeerAddr   :: Peer e -> m (PeerAddr e)
  fromPeerAddr :: PeerAddr e -> m (Peer e)

-- FIXME: type-application-instead-of-proxy
class (Monad m, HasProtocol e p) => HasThatPeer p e (m :: Type -> Type) where
  thatPeer :: m (Peer e)

class (MonadIO m, HasProtocol e p) => HasDeferred p e m | p -> e where
  deferred :: m () -> m ()

-- TODO: actually-no-idea-if-it-works
instance (HasDeferred p e m, Monad m) => HasDeferred p e (MaybeT m) where
  deferred a = lift $ deferred @p (void $ runMaybeT a)

class ( MonadIO m
      , HasProtocol e p
      , HasThatPeer p e m
      ) => Response e p m | p -> e where

  response :: p -> m ()

class HasProtocol e p => Request e p (m :: Type -> Type) | p -> e where
  request :: Peer e -> p -> m ()

data ReqLimPeriod = NoLimit
                  | ReqLimPerProto   (Timeout 'Seconds)
                  | ReqLimPerMessage (Timeout 'Seconds)

class (KnownNat (ProtocolId p), HasPeer e ) => HasProtocol e p | p -> e  where
  type family ProtocolId p = (id :: Nat) | id -> p
  type family Encoded e :: Type

  protoId :: forall . KnownNat (ProtocolId p) => Proxy p -> Integer
  protoId _ = natVal (Proxy @(ProtocolId p))

  decode :: Encoded e -> Maybe p
  encode :: p -> Encoded e

  requestPeriodLim :: ReqLimPeriod
  requestPeriodLim = NoLimit

-- FIXME: slow and dumb
instance {-# OVERLAPPABLE #-} (MonadIO m, Num (Cookie e)) => GenCookie e m where
  genCookie salt = do
    r <- liftIO $ Random.randomIO @Int
    pure $ fromInteger $ fromIntegral $ asWord32 $ hash32 (hash salt + r)

class FromSockAddr ( t :: L4Proto)  a where
  fromSockAddr :: SockAddr -> a

instance HasPeer L4Proto where
  data instance Peer L4Proto =
    PeerL4
    { _sockType :: L4Proto
    , _sockAddr :: SockAddr
    }
    -- | A host-name peer (e.g. a @*.onion@ hidden service) that is never
    --   resolved locally: the name is carried verbatim down to the SOCKS5
    --   connect, where the proxy resolves it. @_sockType@ is shared with
    --   'PeerL4' so the @sockType@ lens stays total.
    | PeerL4Name
    { _sockType :: L4Proto
    , _sockHost :: Text
    , _sockPort :: Word16
    }
    deriving stock (Eq,Ord,Show,Generic)

instance AddrPriority (Peer L4Proto) where
  addrPriority (PeerL4 _ sa)    = addrPriority sa
  addrPriority (PeerL4Name{})   = 2

instance Hashable (Peer L4Proto) where
  hashWithSalt salt p = case p of
    PeerL4 _ (SockAddrInet  pn h)     -> hashWithSalt salt (4 :: Int, fromEnum (_sockType p), fromIntegral pn :: Integer, h)
    PeerL4 _ (SockAddrInet6 pn _ h _) -> hashWithSalt salt (6 :: Int, fromEnum (_sockType p), fromIntegral pn :: Integer, h)
    PeerL4 _ (SockAddrUnix s)         -> hashWithSalt salt ("unix" :: String, s)
    PeerL4Name _ h pn                 -> hashWithSalt salt ("name" :: String, fromEnum (_sockType p), h, pn)

-- FIXME: support-udp-prefix
instance Pretty (Peer L4Proto) where
  pretty (PeerL4 UDP p) = pretty p
  pretty (PeerL4 TCP p) = "tcp://" <> pretty p
  pretty (PeerL4Name TCP h p) = "tcp://" <> pretty h <> ":" <> pretty p
  pretty (PeerL4Name UDP h p) = pretty h <> ":" <> pretty p

instance FromSockAddr 'UDP (Peer L4Proto) where
  fromSockAddr = PeerL4 UDP

instance FromSockAddr 'TCP (Peer L4Proto) where
  fromSockAddr = PeerL4 TCP

makeLenses 'PeerL4

newtype FromIP a = FromIP { fromIP :: a }


-- FIXME: tcp-and-udp-support
instance (MonadIO m) => IsPeerAddr L4Proto m where
-- instance MonadIO m => IsPeerAddr L4Proto m where
  data instance PeerAddr L4Proto =
    L4Address L4Proto (IPAddrPort L4Proto)
    -- | A host-name address (e.g. @*.onion@ or a DNS name) kept unresolved.
    --   New constructor appended so that the derived 'Serialise' wire encoding
    --   of the existing 'L4Address' (constructor index 0) is unchanged; old
    --   peers keep reading clearnet addresses verbatim.
    | L4AddressName L4Proto Text Word16
    deriving stock (Eq,Ord,Show,Generic)

  -- FIXME: backlog-fix-addr-conversion
  toPeerAddr (PeerL4 t p) = pure $ L4Address t (fromString $ show $ pretty p)
  toPeerAddr (PeerL4Name t h p) = pure $ L4AddressName t h p
  --

  -- FIXME: ASAP-tcp-support
  fromPeerAddr (L4Address UDP iap) = do
    ai <- liftIO $ parseAddrUDP $ fromString (show (pretty iap))
    pure $ PeerL4 UDP $ addrAddress (head ai)

  fromPeerAddr (L4Address TCP iap) = do
    ai <- liftIO $ parseAddrTCP $ fromString (show (pretty iap))
    pure $ PeerL4 TCP $ addrAddress (head ai)

  -- a host-name peer is never resolved here; the name is handed to the
  -- SOCKS5 proxy at connect time
  fromPeerAddr (L4AddressName t h p) = pure $ PeerL4Name t h p

instance Hashable (PeerAddr L4Proto)

instance Pretty (PeerAddr L4Proto) where
  pretty (L4Address UDP a) = pretty a
  pretty (L4Address TCP a) = "tcp://" <> pretty a
  pretty (L4AddressName TCP h p) = "tcp://" <> pretty h <> ":" <> pretty p
  pretty (L4AddressName UDP h p) = pretty h <> ":" <> pretty p

instance IsString (PeerAddr L4Proto) where
  fromString s = fromMaybe (error "invalid address") (fromStringMay s)

instance FromStringMaybe (PeerAddr L4Proto) where
  fromStringMay s | Text.isPrefixOf "tcp://" txt = parseTCP
                  | otherwise                    = L4Address UDP <$> fromStringMay addr
    where
      txt = fromString s :: Text
      addr = Text.unpack $ fromMaybe txt (Text.stripPrefix "tcp://" txt <|> Text.stripPrefix "udp://" txt)
      -- a numeric IP parses as 'L4Address'; anything else (a DNS name, a
      -- @.onion@ hidden service) is kept as a name for the proxy to resolve
      parseTCP = (L4Address TCP <$> fromStringMay addr)
             <|> (uncurry (L4AddressName TCP) <$> parseHostPort addr)

-- | Parse a @host:port@ pair, keeping the host as an unresolved name.
parseHostPort :: String -> Maybe (Text, Word16)
parseHostPort s = do
  (h, p) <- getHostPort (Text.pack s)
  pure (Text.pack h, fromIntegral p)

instance Serialise L4Proto
instance Serialise (PeerAddr L4Proto)

-- | The network a peer address belongs to. Used by the PEX policy (PEP-05):
--   a peer only forwards an address to neighbours that can actually reach it,
--   so @.onion@ addresses never reach clearnet-only peers.
data NetworkClass = Clearnet | Onion
  deriving stock (Eq,Ord,Show,Generic)

instance Serialise NetworkClass
instance Hashable  NetworkClass

-- | Classify an address: a @.onion@ host is 'Onion', everything else
--   (IPv4/IPv6/DNS) is 'Clearnet'.
classOf :: PeerAddr L4Proto -> NetworkClass
classOf (L4AddressName _ h _) | ".onion" `Text.isSuffixOf` Text.toLower h = Onion
classOf _ = Clearnet

-- | Whether a peer address is a usable dial target. An inbound onion
--   connection arrives from the local Tor exit as an ephemeral loopback
--   @PeerL4@ (@127.0.0.1:\<random\>@) that cannot be dialed back, while the
--   peer's routable identity is carried by a @PeerL4Name@ (its @.onion@).
--   The peer-dedup logic uses this so that such a loopback address never
--   evicts a routable entry for the same peer.
peerDialable :: Peer L4Proto -> Bool
peerDialable PeerL4Name{}  = True
peerDialable (PeerL4 _ sa) = not (isLoopbackSockAddr sa)

-- | Render a peer for operator-facing logs without disclosing a hidden-service
--   location. A @.onion@ host is replaced by a short one-way fingerprint
--   (@\<onion:NNNN\>@) that still distinguishes peers across log lines; clearnet
--   IPs and DNS names are public and shown verbatim. Use this instead of
--   'pretty' on default log levels (INFO/NOTICE/WARN/ERROR) so an operator does
--   not leak their peers' @.onion@ addresses through their own logs
--   (PEP-05 Phase 4). Debug/trace levels are an explicit operator opt-in and
--   may still use 'pretty'.
prettyLogPeer :: Peer L4Proto -> Doc ann
prettyLogPeer (PeerL4Name proto h port)
  | ".onion" `Text.isSuffixOf` Text.toLower h =
      scheme <> "<onion:" <> pretty (asWord32 (hash32 (Text.unpack h))) <> ">:" <> pretty port
  where scheme = case proto of { TCP -> "tcp://"; UDP -> "" }
prettyLogPeer p = pretty p

-- | True for IPv4 @127.0.0.0/8@, IPv6 @::1@, and unix-domain addresses.
isLoopbackSockAddr :: SockAddr -> Bool
isLoopbackSockAddr sa = case sa of
  SockAddrInet _ ha       -> case hostAddressToTuple ha of
                               (127,_,_,_) -> True
                               _           -> False
  SockAddrInet6 _ _ h6 _  -> hostAddress6ToTuple h6 == (0,0,0,0,0,0,0,1)
  SockAddrUnix{}          -> True
  _                       -> False


deserialiseCustom :: (Serialise a, MonadPlus m) => ByteString -> m a
deserialiseCustom = either (const mzero) pure . deserialiseOrFail

