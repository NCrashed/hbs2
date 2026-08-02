{-# Language UndecidableInstances #-}
{-# Language RecordWildCards #-}
module HBS2.Net.Messaging.Encrypted.ByPass
  ( ForByPass
  , ByPass(..)
  , ByPassOpts(..)
  , ByPassStat(..)
  , byPassDef
  , newByPassMessaging
  , byPassMessagingSetProbe
  , cleanupByPassMessaging
  , getStat
  -- * Handshake internals
  --
  -- Exported so a test can play the part of a peer that picks its own nonce,
  -- which is the whole shape of the flow-key attack and cannot be staged with
  -- 'newByPassMessaging' alone: it derives the nonce and will not be told one.
  -- Nothing here is a capability; these are the bytes anyone on the wire writes.
  , NonceA(..)
  , FlowKey
  , PeerFlow(..)
  , HEYBox(..)
  , EncryptHandshake(..)
  , heySeed
  , makeKey
  ) where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Net.Messaging
import HBS2.Data.Types.SignedBox
import HBS2.Net.Auth.Credentials()

import HBS2.Net.Messaging.Encrypted.RandomPrefix

import HBS2.System.Logger.Simple

import Codec.Serialise
import Control.Concurrent.STM (flushTQueue)
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Network.ByteOrder qualified as N
import Crypto.Saltine.Core.Box (Keypair(..),CombinedKey)
import Crypto.Saltine.Class qualified as SA
import Crypto.Saltine.Core.Box qualified as PKE
import Data.Bits
import Data.ByteArray.Hash qualified as BA
import Data.ByteArray.Hash (SipHash(..), SipKey(..))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HashSet
import Data.List (sortOn)
import Data.Maybe
import Data.Word
import System.Random
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans.Cont
import UnliftIO

heySeed :: Word8
heySeed = 117

newtype NonceA = NonceA { fromNonceA :: Word16 }
                 deriving newtype (Eq,Ord,Show,Pretty,Real,Num,Enum,Integral,Hashable)
                 deriving stock Generic

type FlowKey = Word32

instance Serialise NonceA

mySipHash :: Integral a => BS.ByteString -> a
mySipHash s = BA.sipHash (SipKey a b) s
                & \(SipHash w) -> fromIntegral w
  where
   a = 3857206264
   b = 1307114574


-- NOTE: key-update-on-fly
--  мы можем на ходу менять ключи:
--   меняем nonceA, перегенеряем ключ, больше ничего не трогаем:
--   тогда пакеты посланные для старого nonceA можно будет расшифровать,
--   а шифровать уже для нового.
--
--   Таким образом, хост может иметь много flow с разными
--   нонсами одновременно
--


data ByPassOpts s =
  ByPassOpts
  { byPassEnabled    :: Bool
  , byPassKeyAllowed :: PubKey 'Sign s -> IO Bool
  , byPassTimeRange  :: Maybe (Int, Int)
  }

data ByPassStat =
  ByPassStat
  { statBypassed     :: Int
  , statEncrypted    :: Int
  , statDecrypted    :: Int
  , statDecryptFails :: Int
  , statSent         :: Int
  , statReceived     :: Int
  , statSentBytes    :: Int
  , statRecvBytes    :: Int
  , statFlowNum      :: Int
  , statPeers        :: Int
  , statAuthFail     :: Int
  , statMaxPkt       :: Int
  }
  deriving stock (Show,Generic)

instance Serialise ByPassStat

-- | Everything one peer told us about its own flow, in the HEY it signed:
-- the nonce it announced, the key that signed the announcement, the encryption
-- public key carried inside the signed box, and the shared key derived from it.
--
-- NOTE: flow-key-is-not-an-identity
--   A 'FlowKey' is 32 bits and half of it is the remote side's free choice, so
--   it names a flow and never a peer. What we encrypt to a peer is therefore
--   decided here, keyed by the peer, and not by whoever reached the flow key
--   first: otherwise anyone may announce a nonce that collides with an honest
--   peer's and have our traffic for that peer encrypted to them instead.
data PeerFlow s =
  PeerFlow
  { pfNonce  :: NonceA
  , pfSigner :: PubKey 'Sign s
  , pfPubKey :: PubKey 'Encrypt s
  , pfKey    :: CombinedKey
  }

-- | Shared keys that may decrypt a packet bearing a given flow key, one per
-- announcing identity. More than one peer can land on the same flow key, by
-- accident at a few hundred peers and on purpose at any time, so this is a set
-- of candidates to try rather than a single answer.
type FlowCandidates s = HashMap FlowKey (HashMap (PubKey 'Sign s) (TimeSpec, CombinedKey))

-- | How many candidates are kept per flow key. The bound is what stops a peer
-- that manufactures collisions from turning every decryption into a long scan.
-- Eviction is by age; an honest peer evicted this way reinstalls itself on its
-- next HEY, which a failed decryption already asks for.
maxFlowCandidates :: Int
maxFlowCandidates = 8

data ByPass e them =
  ByPass
  { opts         :: ByPassOpts (Encryption e)
  , self         :: Peer e
  , pks          :: PubKey 'Sign (Encryption e)
  , sks          :: PrivKey 'Sign (Encryption e)
  , pke          :: PubKey 'Encrypt (Encryption e)
  , ske          :: PrivKey 'Encrypt (Encryption e)
  , proxied      :: them
  , nonceA       :: NonceA
  , heySent      :: TVar (HashMap (Peer e) TimeSpec)
  , heySentNum   :: TVar (HashMap (Peer e) Int)
  , peerFlow     :: TVar (HashMap (Peer e) (PeerFlow (Encryption e)))
  , banned       :: TVar (HashMap (Peer e) TimeSpec)
  , flowKeys     :: TVar (FlowCandidates (Encryption e))
  , bypassed     :: TVar Int
  , encrypted    :: TVar Int
  , decrypted    :: TVar Int
  , decryptFails :: TVar Int
  , sentNum      :: TVar Int
  , recvNum      :: TVar Int
  , sentBytes    :: TVar Int
  , recvBytes    :: TVar Int
  , authFail     :: TVar Int
  , maxPkt       :: TVar Int
  , probe        :: TVar AnyProbe
  }

type ForByPass e   = ( Hashable (Peer e)
                     , Pretty (Peer e)
                     , Eq (PubKey 'Sign (Encryption e))
                     , Serialise (PubKey 'Sign (Encryption e))
                     , PrivKey 'Encrypt (Encryption e) ~ PKE.SecretKey
                     , PubKey 'Encrypt (Encryption e) ~ PKE.PublicKey
                     , ForSignedBox (Encryption e)
                     )


data HEYBox e =
  HEYBox Int (PubKey 'Encrypt (Encryption e))
  deriving stock Generic

instance ForByPass s => Serialise (HEYBox s)

data EncryptHandshake e =
    HEY
    { heyNonceA :: NonceA
    , heyBox    :: SignedBox (HEYBox e) (Encryption e)
    }
  deriving stock (Generic)

instance ForByPass e => Serialise (EncryptHandshake e)

getStat :: forall e w m . ( ForByPass e
                          , MonadIO m
                          )
        => ByPass e w
        -> m ByPassStat
getStat ByPass{..} = liftIO do
  atomically do
    ByPassStat <$> readTVar bypassed
               <*> readTVar encrypted
               <*> readTVar decrypted
               <*> readTVar decryptFails
               <*> readTVar sentNum
               <*> readTVar recvNum
               <*> readTVar sentBytes
               <*> readTVar recvBytes
               <*> (readTVar flowKeys <&> sum . fmap HM.size . HM.elems)
               <*> (readTVar peerFlow <&> HM.size)
               <*> readTVar authFail
               <*> readTVar maxPkt

cleanupByPassMessaging :: forall e w m . ( ForByPass e
                                    , MonadIO m
                                    )
                   => ByPass e w
                   -> [Peer e]
                   -> m ()

cleanupByPassMessaging  bus pips = do
  debug "cleanupByPassMessaging"

  now <- getTimeCoarse

  let alive = HashSet.fromList pips

  atomically do
    sent  <- readTVar (heySent bus)
    flows <- readTVar (peerFlow bus)

    let livePeers = [ (k,v)
                    | (k,v) <- HM.toList flows
                    , k `HashSet.member` alive
                    ] & HM.fromList

    let liveSent = HM.filterWithKey (\k _ -> k `HM.member` livePeers) sent

    -- The candidate index is rebuilt from the peers we still know rather than
    -- filtered: a candidate nobody live stands behind has no way back in, so a
    -- squatted flow key is repaired here instead of being carried forward.
    let liveFlows = HM.map capCandidates $ HM.fromListWith (<>)
                      [ ( makeKey (nonceA bus) (pfNonce pf)
                        , HM.singleton (pfSigner pf) (now, pfKey pf)
                        )
                      | pf <- HM.elems livePeers
                      ]

    writeTVar (heySent bus) liveSent
    writeTVar (heySentNum bus) mempty
    writeTVar (peerFlow bus) livePeers
    writeTVar (flowKeys bus) liveFlows
    modifyTVar (banned bus) (HM.filter (>now))


byPassDef :: ByPassOpts e
byPassDef =
  ByPassOpts
  { byPassEnabled    = True
  , byPassKeyAllowed = const $ pure True
  , byPassTimeRange  = Nothing
  }

byPassMessagingSetProbe :: forall e w m . ( ForByPass e, MonadIO m, Messaging w e ByteString)
                        => ByPass e w
                        -> AnyProbe
                        -> m ()

byPassMessagingSetProbe ByPass{..} p = atomically $ writeTVar probe p

newByPassMessaging :: forall e w m . ( ForByPass e
                                     , MonadIO m
                                     , Messaging w e ByteString
                                     )
                   => ByPassOpts (Encryption e)
                   -> w
                   -> Peer e
                   -> PubKey 'Sign (Encryption e)
                   -> PrivKey 'Sign (Encryption e)
                   -> m (ByPass e w)

newByPassMessaging o w self ps sk  = do
  (Keypair s p) <- liftIO PKE.newKeypair
  let n = mySipHash (LBS.toStrict (serialise s))
  ByPass @e o self ps sk p s w n <$> newTVarIO mempty
                                 <*> newTVarIO mempty
                                 <*> newTVarIO mempty
                                 <*> newTVarIO mempty
                                 <*> newTVarIO mempty
                                 <*> newTVarIO 0
                                 <*> newTVarIO 0
                                 <*> newTVarIO 0
                                 <*> newTVarIO 0
                                 <*> newTVarIO 0
                                 <*> newTVarIO 0
                                 <*> newTVarIO 0
                                 <*> newTVarIO 0
                                 <*> newTVarIO 0
                                 <*> newTVarIO 0
                                 <*> newTVarIO (AnyProbe ())

instance (ForByPass e, Messaging w e ByteString)
  => Messaging (ByPass e w) e ByteString where

  sendTo bus@ByPass{..} t@(To whom) f m = void $ flip runContT pure do

    callCC \exit -> do

      ban <- readTVarIO banned <&> HM.member whom

      when ban $ exit ()

      mkey <- lookupEncKey bus whom

      atomically do
        modifyTVar sentNum succ
        modifyTVar sentBytes (+ (fromIntegral $ LBS.length m))

      case mkey of
        Just fck -> do
          sendTo proxied t f =<< encryptMessage bus fck m

        Nothing -> do
          -- let ByPassOpts{..} = opts bus

         atomically $ modifyTVar bypassed succ
         sendTo proxied t f m

        -- TODO: stop-sending-hey-after-while
        --   Если адрес кривой и мы его не знаем/не можем
        --   на него послать/ничего с него не получаем ---
        --   надо переставать слать на него HEY с какого-то момента

         heys <- readTVarIO heySentNum <&> fromMaybe 0 . HM.lookup whom

         when (heys > 10) do
          nowTs <- getTimeCoarse
          let till = toTimeSpec (TimeoutTS nowTs) + toTimeSpec (TimeoutSec 1800)
          atomically $ modifyTVar banned (HM.insert whom till)
          exit ()

          -- TODO: fix-timeout-hardcode
         withHeySent bus 30 whom do
          sendHey bus whom
          atomically $ modifyTVar heySentNum (HM.insertWith (+) whom 1)

  receive bus@ByPass{..} f = do
    msgs <- receive  proxied f

    q <- newTQueueIO

    -- TODO: run-concurrently
    for_ msgs $ \(From who, mess) -> runMaybeT do

      atomically do
        modifyTVar recvNum succ
        modifyTVar recvBytes  (+ (fromIntegral $ LBS.length mess))
        modifyTVar maxPkt (max (fromIntegral $ LBS.length mess))

      ban <- readTVarIO banned <&> HM.member who

      guard (not ban)

      hshake <- processHey who mess

      guard (not hshake)

      msg <- tryDecryptMessage bus mess

      case msg of
        Just demsg -> do
          atomically $ writeTQueue q (From who, demsg)

        Nothing    -> do
          withHeySent bus 60 who do
            sendHey bus who

          atomically $ writeTQueue q (From who, mess)

    liftIO $ atomically $ flushTQueue q

    where
      processHey orig bs = isJust <$> runMaybeT do

        nowTs <- getTimeCoarse

        let o = opts

        w <- liftIO $ race (pause @'Seconds 1) (pure $! runCodeLazy bs)

        (code, hbs) <- case w of
                         Right x -> pure x
                         Left{} -> do
                          err $ "ByPass: bytecode run timeout" <+> pretty orig
                          pure (Nothing, mempty)

        -- FIXME: check-code
        guard ( code == Just heySeed )

        trace $ "HEY CODE:" <> parens (pretty code) <+> pretty orig

        guard (not (LBS.null hbs))

        hshake <- toMPlus (deserialiseOrFail @(EncryptHandshake e) hbs)

        case hshake of
          HEY{..} -> do-- void $ runMaybeT do
            debug $ "GOT HEY MESSAGE" <+> parens (pretty code) <+> pretty heyNonceA

            when (heyNonceA  == nonceA) do
              let till = toTimeSpec (TimeoutTS nowTs) + toTimeSpec (TimeoutSec 3600)
              atomically $ modifyTVar banned (HM.insert orig till)
              warn $ "ByPass: loop detected"

            let mbx = unboxSignedBox0 heyBox

            when (isNothing mbx) do
              debug $ "HEY: failed to unbox" <+> pretty heyNonceA <+> pretty orig

            (theirSignPk, HEYBox t puk) <- toMPlus mbx

            let dt = byPassTimeRange o

            allowed <- liftIO $ byPassKeyAllowed o theirSignPk
            now <- liftIO getPOSIXTime <&> round
            let actual = maybe1 dt True (\(ta, tb) -> t >= now - ta &&  t <= now + tb)

            let authorized = allowed && actual

            unless authorized do
              atomically $ modifyTVar authFail succ
              warn $ "ByPass:" <+> "NOT AUTHORIZED" <+> pretty orig

            when authorized do
              debug $ "ByPass:" <+> "AUTHORIZED" <+> pretty now <+> pretty orig

            guard authorized

            -- A peer speaks for its own flow and for nothing else, so this
            -- always supersedes what we held for THIS peer, and never touches
            -- what we hold for another. Answer only when something moved: an
            -- unconditional reply here is a HEY ping-pong between two peers
            -- that both keep having nothing new to say.
            moved <- installPeerFlow bus orig theirSignPk puk heyNonceA

            when moved do
              withHeySent bus 30 orig do
                sendHey bus orig

        pure hshake

makeKey :: NonceA -> NonceA -> FlowKey
makeKey a b = runIdentity do
  let aa = fromIntegral a :: FlowKey
  let bb = fromIntegral b :: FlowKey

  let (f0,f1) = if aa < bb then (aa,bb) else (bb,aa)

  pure $ (f0 `shiftL` 16) .|. f1


sendHey :: forall e w m s . ( ForByPass e
                            , Messaging w e ByteString
                            , MonadIO m
                            , s ~ Encryption e
                            )
        => ByPass e w
        -> Peer e
        -> m ()

sendHey bus whom = do

  pref <- randomPrefix (PrefixMethod1 4 11 heySeed) <&> toLazyByteString

  let (code, _) = runCodeLazy pref

  ts <- liftIO getPOSIXTime <&> round

  let hbox = HEYBox @e ts (pke bus)
  let box = makeSignedBox @s (pks bus) (sks bus) hbox
  let hey = HEY @e (nonceA bus) box
  let msg = pref <> serialise hey

  debug $ "SEND HEY" <+> pretty  (heyNonceA hey)
                     <+> parens ("seed" <+> pretty code)
                     <+> pretty whom
                     <+> pretty (LBS.length  msg)

  sendTo (proxied bus) (To whom) (From (self bus)) msg

withHeySent :: forall e w m . (MonadIO m, ForByPass e)
            => ByPass e w
            -> Timeout 'Seconds
            -> Peer e
            -> m ()
            -> m ()

withHeySent w ts pip m = do
  now <- getTimeCoarse

  t0  <- readTVarIO (heySent w) <&> HM.lookup pip
           <&> fromMaybe 0

  let elapsed = toNanoSeconds $ TimeoutTS (now - t0)

  when ( elapsed >= toNanoSeconds ts ) do
    atomically $ modifyTVar (heySent w) (HM.insert pip now)
    m


-- | Keep at most 'maxFlowCandidates' entries, dropping the oldest.
capCandidates :: (Eq k, Hashable k)
              => HashMap k (TimeSpec, CombinedKey)
              -> HashMap k (TimeSpec, CombinedKey)
capCandidates b
  | HM.size b <= maxFlowCandidates = b
  | otherwise = HM.fromList
              $ drop (HM.size b - maxFlowCandidates)
              $ sortOn (fst . snd)
              $ HM.toList b

-- | Record what a peer announced about its flow, replacing whatever we held
-- for that peer, and add the shared key to the candidates for its flow key.
-- Answers whether anything actually changed.
installPeerFlow :: forall e w m . ( ForByPass e
                                  , MonadIO m
                                  )
                => ByPass e w
                -> Peer e
                -> PubKey 'Sign (Encryption e)
                -> PubKey 'Encrypt (Encryption e)
                -> NonceA
                -> m Bool

installPeerFlow bus pip signer puk nonce = do
  now <- getTimeCoarse
  old <- readTVarIO (peerFlow bus) <&> HM.lookup pip

  let same pf = pfSigner pf == signer && pfNonce pf == nonce && pfPubKey pf == puk

  let fk = makeKey (nonceA bus) nonce

  -- Deriving the shared key is a scalar multiplication, and a peer repeats its
  -- HEY on a timer, so do not pay for it to arrive at what we already hold.
  flow <- case old of
            Just pf | same pf -> pure pf
            _ -> do
              let ck = PKE.beforeNM (ske bus) puk

              debug $ "HEY: CK" <+> pretty (nonceA bus)
                                <+> pretty fk
                                <+> pretty (hashObject @HbSync (SA.encode ck))

              pure $ PeerFlow nonce signer puk ck

  atomically do
    modifyTVar' (peerFlow bus) (HM.insert pip flow)
    modifyTVar' (flowKeys bus) $ \fks ->
      let bucket = HM.insert signer (now, pfKey flow) (HM.lookupDefault mempty fk fks)
      in HM.insert fk (capCandidates bucket) fks

  pure (not (any same old))

lookupEncKey :: (ForByPass e, MonadIO m) => ByPass e w -> Peer e -> m (Maybe (FlowKey, CombinedKey))
lookupEncKey bus whom = runMaybeT do
  pf <- MaybeT $ readTVarIO (peerFlow bus) <&> HM.lookup whom
  pure (makeKey (nonceA bus) (pfNonce pf), pfKey pf)


typicalNonceLength :: Integral a => a
typicalNonceLength = unsafePerformIO PKE.newNonce & SA.encode & BS.length & fromIntegral
{-# NOINLINE typicalNonceLength #-}

newtype ByPassNonce = ByPassNonce { unByPassNonce :: PKE.Nonce }

instance NonceFrom ByPassNonce Word32 where
  nonceFrom a = ByPassNonce nonce
    where
      n = typicalNonceLength
      nonce = fromJust (SA.decode s)
      s = BS.take n (N.bytestring32 a <> BS.replicate n 0)


tryDecryptMessage :: (MonadIO m, ForByPass e)
                  => ByPass e w
                  -> ByteString
                  -> m (Maybe ByteString)

tryDecryptMessage bus bs = runMaybeT do

  let (hdr, body) = LBS.splitAt 8 bs

  guard (LBS.length hdr == 8)

  (fk, wnonce) <- liftIO $ N.withReadBuffer (LBS.toStrict hdr) $ \buf -> do
                              (,) <$> N.read32 buf <*> N.read32 buf

  let bnonce = nonceFrom @ByPassNonce wnonce

  cks <- MaybeT $ readTVarIO (flowKeys bus) <&> HM.lookup fk

  -- Several peers may hold this flow key, so try each of their shared keys:
  -- the Poly1305 tag is what says which one the sender actually used.
  let dmess = listToMaybe [ m
                          | (_, ck) <- HM.elems cks
                          , Just m  <- [PKE.boxOpenAfterNM ck (unByPassNonce bnonce) (LBS.toStrict body)]
                          ] <&> LBS.fromStrict

  atomically do
    maybe1 dmess
      (modifyTVar (decryptFails bus) succ)
      (const $ modifyTVar (decrypted bus) succ)

  toMPlus dmess


encryptMessage :: (MonadIO m, ForByPass e)
               => ByPass e w
               -> (FlowKey, CombinedKey)
               -> ByteString
               -> m ByteString

encryptMessage bus (fk, ck) bs = do

  atomically $ modifyTVar (encrypted bus) succ

  wnonce <- liftIO (randomIO @Word32)
  let bnonce = nonceFrom @ByPassNonce wnonce

  let ebs = PKE.boxAfterNM ck (unByPassNonce bnonce) (LBS.toStrict bs)

  let pkt = mconcat [ word32BE fk
                    , word32BE wnonce
                    , byteString ebs
                    ] & toLazyByteString

  pure pkt

instance Pretty ByPassStat where
  pretty (ByPassStat{..}) =
    vcat [ prettyField "bypassed"     statBypassed
         , prettyField "encrypted"    statEncrypted
         , prettyField "decrypted"    statDecrypted
         , prettyField "decryptFails" statDecryptFails
         , prettyField "sent"         statSent
         , prettyField "received"     statReceived
         , prettyField "sentBytes"    statSentBytes
         , prettyField "recvBytes"    statRecvBytes
         , prettyField "flowNum"      statFlowNum
         , prettyField "peers"        statPeers
         , prettyField "authFail"     statAuthFail
         , prettyField "maxPktLen"    statMaxPkt
         ]
    where
      prettyField x e = fill 15 (x <> colon) <+> pretty e

