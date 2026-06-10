{-# Language UndecidableInstances #-}
module HBS2.Data.Types.Peer where

import Data.ByteString qualified as BS
import Data.Hashable
import Data.Set (Set)
import Lens.Micro.Platform

import HBS2.Prelude
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Types (NetworkClass(..))


type PingSign e = Signature (Encryption e)
type PingNonce = BS.ByteString

data PeerData e =
  PeerData
  { _peerSignKey  :: PubKey 'Sign (Encryption e)
  , _peerOwnNonce :: PeerNonce -- TODO: to use this field to detect if it's own peer to avoid loops
  , _peerReachableVia :: Set NetworkClass -- ^ the classes this peer says it is reachable on (PEP-05 PEX policy)
  }
  deriving stock (Typeable,Generic)

deriving instance
    ( Eq (PubKey 'Sign (Encryption e))
    , Eq PeerNonce
    )
    => Eq (PeerData e)

instance
    ( Hashable (PubKey 'Sign (Encryption e))
    , Hashable PeerNonce
    )
    => Hashable (PeerData e) where
  hashWithSalt s PeerData{..} = hashWithSalt s (_peerOwnNonce)

deriving instance
    ( Show (PubKey 'Sign (Encryption e))
    , Show PeerNonce
    )
    => Show (PeerData e)

makeLenses 'PeerData

