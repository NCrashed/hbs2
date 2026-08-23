module HBS2.Peer.Proto.PeerMeta where

import HBS2.Base58
import HBS2.Clock
import HBS2.Events
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.Proto
import HBS2.Peer.Proto.Peer
import HBS2.Data.Types.Peer (_peerReachableVia)
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated

import HBS2.Actors.Peer.Types

import HBS2.System.Logger.Simple

import Codec.Serialise
import Control.Monad
import Data.ByteString ( ByteString )
import Data.ByteString.Lazy qualified as LBS
import Data.Functor
import Data.Maybe
import Data.Set (Set)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Safe (readMay)

instance HasProtocol L4Proto (PeerMetaProto L4Proto) where
  type instance ProtocolId (PeerMetaProto L4Proto) = 9
  type instance Encoded L4Proto = LBS.ByteString
  decode = deserialiseCustom
  encode = serialise

  -- FIXME: real-period
  requestPeriodLim = ReqLimPerMessage 0.25

instance Expires (EventKey L4Proto (PeerMetaProto L4Proto)) where
  expiresIn _ = Just 600


data PeerMetaProto e
  = GetPeerMeta
  | ThePeerMeta AnnMetaData
  deriving stock (Eq,Generic,Show)

instance Serialise (PeerMetaProto e)


peerMetaProto :: forall e m proto  . ( MonadIO m
                                     , Response e proto m
                                     , HasDeferred proto e m
                                     , EventEmitter e proto m
                                     , Sessions e (KnownPeer e) m
                                     , Pretty (Peer e)
                                     , proto ~ PeerMetaProto e
                                     )
               => (Set NetworkClass -> AnnMetaData)
                  -- ^ build the meta given the recipient's reachable classes
               -> PeerMetaProto e
               -> m ()

peerMetaProto mkMeta =
  \case
    GetPeerMeta -> do
      p <- thatPeer @proto
      -- only answer authenticated peers; the recipient's declared classes
      -- gate which of our addresses we disclose (PEP-05 G)
      find (KnownPeerKey p) id >>= mapM_ \pd -> do
        let meta = mkMeta (_peerReachableVia pd)
        debug $ "PEER META: ANSWERING" <+> pretty p <+> viaShow meta
        deferred @proto do
          response (ThePeerMeta @e meta)

    ThePeerMeta meta -> do
      that <- thatPeer @proto
      debug $ "GOT PEER META FROM" <+> pretty that <+> viaShow meta
      emit @e (PeerMetaEventKey that) (PeerMetaEvent meta)

newtype instance EventKey e (PeerMetaProto e) =
  PeerMetaEventKey (Peer e)
  deriving stock (Typeable, Generic)

deriving instance Eq (Peer e) => Eq (EventKey e (PeerMetaProto e))
deriving instance (Eq (Peer e), Hashable (Peer e)) => Hashable (EventKey e (PeerMetaProto e))

newtype instance Event e (PeerMetaProto e)
  = PeerMetaEvent AnnMetaData
  deriving stock (Typeable)

newtype PeerMeta = PeerMeta { unPeerMeta :: [(Text, ByteString)] }
  deriving stock (Generic)
  deriving newtype (Semigroup, Monoid, Show)

instance Serialise PeerMeta

annMetaFromPeerMeta :: PeerMeta -> AnnMetaData
annMetaFromPeerMeta =
    ShortMetadata . TE.decodeUtf8 . toBase58 . LBS.toStrict . serialise

parsePeerMeta :: Text -> Maybe PeerMeta
parsePeerMeta = either (const Nothing) Just . deserialiseOrFail . LBS.fromStrict <=< fromBase58 . TE.encodeUtf8

-- | One value out of a peer's meta, decoded.
--
-- 'PeerMeta' is an association list of text keys to bytes, so a reader that does
-- not know a key ignores it and a writer may add one without changing any
-- format. That is what makes it the cheap place to publish a number, and it is
-- why this accessor is here rather than repeated at each call site: the values
-- are written with 'show' and read with 'readMay', and those two have to be
-- decided together. A key that is absent, or whose value will not parse as @v@,
-- is 'Nothing' -- there is no third answer, because a peer that did not say a
-- thing and a peer that said it in a way this build cannot read are the same
-- amount of knowledge.
peerMetaValue :: Read v => Text -> PeerMeta -> Maybe v
peerMetaValue k =
  (readMay . Text.unpack . TE.decodeUtf8 =<<) . lookup k . unPeerMeta
