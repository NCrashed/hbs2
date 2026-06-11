{-# Language TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
module HBS2.Net.Messaging.TCP
  ( MessagingTCP
  , runMessagingTCP
  , newMessagingTCP
  , tcpSOCKS5
  , tcpOwnPeer
  , tcpPeerConn
  , tcpCookie
  , tcpOnClientStarted
  , tcpPeerKick
  , tcpAdoptName
  , messagingTCPSetProbe
  ) where

import HBS2.Clock
import HBS2.OrDie
import HBS2.Net.IP.Addr
import HBS2.Net.Messaging
import HBS2.Prelude.Plated

import HBS2.Net.Messaging.Stream
import HBS2.Net.Proto.Types (peerDialable)

import HBS2.System.Logger.Simple

import Control.Monad.Trans.Maybe
import Data.Bits
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashPSQ (HashPSQ)
import Data.HashPSQ qualified as HPSQ
import Data.HashSet qualified as HS
import Data.List (isSuffixOf)
import Data.Maybe
import Data.Text qualified as Text
import Data.Word
import Lens.Micro.Platform
import Data.IP (toSockAddr)
import Network.ByteOrder hiding (ByteString)
import Network.Simple.TCP
import Network.Socket hiding (listen,connect)
import Network.Socks5 (socksConnect, defaultSocksConf)
import Network.Socks5.Types (SocksAddress(..), SocksHostAddress(..))
import System.Random hiding (next)
import Control.Monad.Trans.Cont
import Control.Exception
import Control.Concurrent.STM qualified as STM

import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Async
import UnliftIO.STM
import UnliftIO.Exception qualified as U
import Streaming.Prelude qualified as S

{-HLINT ignore "Functor law"-}

-- FIXME: control-recv-capacity-to-avoid-leaks

outMessageQLen :: Natural
outMessageQLen = 1024*32

-- | TCP Messaging environment
data MessagingTCP =
  MessagingTCP
  { _tcpSOCKS5             :: Maybe (PeerAddr L4Proto)
  , _tcpOwnPeer            :: Peer L4Proto
  , _tcpCookie             :: Word32
  , _tcpPeerConn           :: TVar (HashMap (Peer L4Proto) Word64)
  , _tcpPeerCookie         :: TVar (HashMap Word32 Int)
  , _tcpPeerToCookie       :: TVar (HashMap (Peer L4Proto) Word32)
  , _tcpPeerSocket         :: TVar (HashMap (Peer L4Proto) Socket)
  , _tcpConnDemand         :: TVar (HashPSQ (Peer L4Proto) TimeSpec ())
  , _tcpReceived           :: TBQueue (Peer L4Proto, ByteString)
  , _tcpSent               :: TVar (HashPSQ (Peer L4Proto) TimeSpec (TBQueue ByteString))
  , _tcpClientThreadNum    :: TVar Int
  , _tcpClientThreads      :: TVar (HashMap Int (Async ()))
  , _tcpServerThreadsCount :: TVar Int
  , _tcpProbe              :: TVar AnyProbe
  , _tcpOnClientStarted    :: PeerAddr L4Proto -> Word64 -> IO () -- ^ Cient TCP connection succeed
  -- | Maps a connection's socket-derived peer (e.g. the loopback Tor exit of
  --   an inbound onion connection) to the routable name the peer advertised for
  --   itself via peer-public-address. Sends to the name are routed over the
  --   aliased connection and its received frames are re-tagged with the name,
  --   so an inbound onion peer becomes known by its @.onion@ instead of
  --   @127.0.0.1@. See 'tcpAdoptName'.
  , _tcpPeerAlias          :: TVar (HashMap (Peer L4Proto) (Peer L4Proto))
  }

makeLenses 'MessagingTCP


newClientThread :: forall m . MonadUnliftIO m => MessagingTCP -> m () -> m Int
newClientThread MessagingTCP{..} a = do
  as <- async a
  atomically do
    n <- stateTVar _tcpClientThreadNum ( \x -> (x, succ x))
    modifyTVar _tcpClientThreads (HM.insert n as)
    pure n

delClientThread :: MonadIO m => MessagingTCP -> Int -> m ()
delClientThread MessagingTCP{..} threadId = atomically $
  modifyTVar' _tcpClientThreads (HM.delete threadId)

messagingTCPSetProbe :: MonadIO m => MessagingTCP -> AnyProbe -> m ()
messagingTCPSetProbe MessagingTCP{..} p = atomically $ writeTVar _tcpProbe p

newMessagingTCP :: ( MonadIO m
                   , FromSockAddr 'TCP (Peer L4Proto)
                   )
                => PeerAddr L4Proto
                -> m MessagingTCP

newMessagingTCP pa = liftIO do
  MessagingTCP Nothing
    <$> fromPeerAddr pa
    <*> randomIO
    <*> newTVarIO mempty
    <*> newTVarIO mempty
    <*> newTVarIO mempty
    <*> newTVarIO mempty
    <*> newTVarIO HPSQ.empty
    <*> newTBQueueIO (10 * outMessageQLen)
    <*> newTVarIO HPSQ.empty
    <*> newTVarIO 0
    <*> newTVarIO mempty
    <*> newTVarIO 0
    <*> newTVarIO (AnyProbe ())
    <*> pure (\_ _ -> none) -- do nothing by default
    <*> newTVarIO mempty    -- _tcpPeerAlias

instance Messaging MessagingTCP L4Proto ByteString where

  sendTo MessagingTCP{..} (To p) (From _f) msg = liftIO do
    -- let _own = tcpOwnPeer
    -- debug $ "!!!! FUCKING SEND TO" <+> pretty p

    now <- getTimeCoarse
    queue <- atomically do
      q' <- readTVar _tcpSent <&> HPSQ.lookup p

      case q' of
        Nothing  -> do
          modifyTVar _tcpConnDemand (HPSQ.insert p now ())
          q <- newTBQueue outMessageQLen
          modifyTVar _tcpSent (HPSQ.insert p now q)
          pure q

        Just (_,q) -> pure q

    atomically $ writeTBQueueDropSTM 10 queue msg
    atomically $ stateTVar _tcpSent (HPSQ.alter (\x -> ((), fmap (set _1 now) x)) p)
    -- atomically $ insert

    -- debug $ "!!!! FUCKING SEND TO" <+> pretty p <+> "DONE"

  receive MessagingTCP{..} _ = liftIO do
    atomically do
      s0 <- readTBQueue _tcpReceived
      sx <- flushTBQueue _tcpReceived
      pure $ fmap (over _1 From) ( s0 : sx )

connectionId :: Word32 -> Word32 -> Word64
connectionId a b = (fromIntegral hi `shiftL` 32) .|. fromIntegral low
  where
    low = min a b
    hi  = max a b

data ConnType = Server | Client
                deriving (Eq,Ord,Show,Generic)


sendCookie :: MonadIO m
           => MessagingTCP
           -> Socket
           -> m ()

sendCookie env so = do
  let coo = view tcpCookie env & bytestring32
  send so coo

recvCookie :: MonadIO m
           => MessagingTCP
           -> Socket
           -> m Word32

recvCookie _ so = liftIO do
  scoo <- readFromSocket so 4 <&> LBS.toStrict
  pure $ word32 scoo

handshake :: MonadIO m
           => ConnType
           -> MessagingTCP
           -> Socket
           -> m Word32

handshake Server env so = do
  cookie <- recvCookie env so
  sendCookie env so
  pure cookie

handshake Client env so = do
  sendCookie env so
  recvCookie env so

writeTBQueueDropSTM :: Integral n
                 => n
                 -> TBQueue a
                 -> a
                 -> STM ()
writeTBQueueDropSTM inQLen newInQ bs = do
  flip fix inQLen $ \more j -> do
    when (j > 0) do
      full <- isFullTBQueue newInQ
      if not full then do
        writeTBQueue newInQ bs
      else do
        void $ tryReadTBQueue newInQ
        more (pred j)



data TCPMessagingError =
    TCPPeerReadTimeout
  | TCPOnionWithoutProxy String  -- ^ a .onion target was dialed without a SOCKS5 proxy configured
  deriving stock (Show,Typeable)

instance Exception TCPMessagingError

tcpPeerKick :: forall m . MonadIO m => MessagingTCP -> Peer L4Proto -> m ()
tcpPeerKick MessagingTCP{..} p = do
  whoever <- readTVarIO _tcpPeerSocket <&> HM.lookup p
  for_ whoever $ \so -> do
    debug $ "tcpPeerKick" <+> pretty p
    liftIO $ shutdown so ShutdownBoth

-- | Adopt the routable name a peer advertised for itself (e.g. its @.onion@,
--   learned via peer-public-address) onto its existing connection. An inbound
--   onion connection arrives from the local Tor exit as an ephemeral loopback
--   peer (@127.0.0.1:\<port\>@); the symmetric outbound dial to its @.onion@ is
--   dropped by the cookie dedup, so the name never gets its own socket. This
--   re-points sends to the name at the existing (loopback) connection - same
--   socket, same out-queue - and records an alias so that connection's received
--   frames are re-tagged with the name. After this the peer can be pinged and
--   becomes known by its @.onion@ instead of @127.0.0.1@.
--
--   No-op unless the current label is non-dialable (a loopback) and the
--   advertised one is dialable, so it never disturbs ordinary clearnet peers.
tcpAdoptName :: forall m . MonadIO m => MessagingTCP -> Peer L4Proto -> Peer L4Proto -> m ()
tcpAdoptName MessagingTCP{..} lp name
  | peerDialable lp || not (peerDialable name) = pure ()
  | otherwise = liftIO do
      now <- getTimeCoarse
      atomically do
        readTVar _tcpPeerConn     <&> HM.lookup lp >>= mapM_ (modifyTVar _tcpPeerConn . HM.insert name)
        readTVar _tcpPeerSocket   <&> HM.lookup lp >>= mapM_ (modifyTVar _tcpPeerSocket . HM.insert name)
        readTVar _tcpPeerToCookie <&> HM.lookup lp >>= mapM_ (modifyTVar _tcpPeerToCookie . HM.insert name)
        readTVar _tcpSent <&> HPSQ.lookup lp >>= mapM_ (\(_,q) -> modifyTVar _tcpSent (HPSQ.insert name now q))
        modifyTVar _tcpPeerAlias (HM.insert lp name)
      debug $ "tcpAdoptName" <+> pretty lp <+> "->" <+> pretty name

-- | Open an outbound TCP connection to (host, port). When a SOCKS5 proxy is
--   configured, dial through it and let the proxy resolve the host itself (so
--   that hidden-service names like @*.onion@ work, since they have no local
--   address). Without a proxy this is a plain TCP connect. The callback
--   contract matches 'Network.Simple.TCP.connect': the socket is closed when
--   the callback returns or throws.
connectTCP :: MonadIO m
           => Maybe (PeerAddr L4Proto)        -- ^ optional SOCKS5 proxy address
           -> HostName                        -- ^ destination host (IP or hostname)
           -> Word16                          -- ^ destination port
           -> ((Socket, SockAddr) -> IO r)
           -> m r

connectTCP Nothing host port action
  -- a .onion name has no clearnet route; fail loudly instead of letting the
  -- resolver hang on NXDOMAIN
  | ".onion" `isSuffixOf` host = liftIO $ throwIO (TCPOnionWithoutProxy host)
  | otherwise                  = liftIO $ connect host (show port) action

connectTCP (Just proxy) host port action = liftIO do
  proxySA <- proxySockAddr proxy
  let conf = defaultSocksConf proxySA
  let dst  = SocksAddress (SocksAddrDomainName (BS8.pack host)) (fromIntegral port)
  bracket
    (socksConnect conf dst)
    (\(so,_) -> close so)
    -- the proxy address stands in for the (unknowable) remote SockAddr; it is
    -- used only for debug output downstream
    (\(so,_) -> action (so, proxySA))
  where
    -- a numeric proxy address is turned into a SockAddr purely; a host-name
    -- proxy (e.g. "localhost:9050") is resolved locally - it is the operator's
    -- own machine, not the anonymised target
    proxySockAddr (L4Address _ (IPAddrPort (ip,p))) = pure (toSockAddr (ip, fromIntegral p))
    proxySockAddr (L4AddressName _ h p) = do
      let hints = defaultHints { addrSocketType = Stream }
      getAddrInfo (Just hints) (Just (Text.unpack h)) (Just (show p)) >>= \case
        (a:_) -> pure (addrAddress a)
        []    -> ioError (userError ("cannot resolve SOCKS5 proxy host: " <> Text.unpack h))

runMessagingTCP :: forall m . MonadIO m => MessagingTCP -> m ()
runMessagingTCP env@MessagingTCP{..} = liftIO do

  fix \again -> do

    void $ flip runContT pure do

        p1 <- ContT $ withAsync runClient
        p2 <- ContT $ withAsync runServer

        probes <- ContT $ withAsync $ forever do
          pause @'Seconds 10
          p <- readTVarIO _tcpProbe
          acceptReport p =<< S.toList_ do
            S.yield =<< ( readTVarIO _tcpClientThreads <&> ("tcpClientThreads",) . fromIntegral . HM.size )
            S.yield =<< ( readTVarIO _tcpServerThreadsCount <&> ("tcpServerThreadsCount",) . fromIntegral )
            S.yield =<< ( readTVarIO _tcpPeerConn <&> ("tcpPeerConn",) . fromIntegral . HM.size)
            S.yield =<< ( readTVarIO _tcpConnDemand <&> ("tcpPeerConnDemand",) . fromIntegral . HPSQ.size)

            coo <- readTVarIO _tcpPeerCookie --  <&> ("tcpPeerCookie",)
            let cooNn = sum [ 1 | (_,v) <- HM.toList coo, v >= 1 ]

            S.yield  ("tcpPeerCookie", fromIntegral $ HM.size coo)
            S.yield  ("tcpPeerCookieUsed", cooNn)

            S.yield =<< ( readTVarIO _tcpSent <&> ("tcpSent",) . fromIntegral . HPSQ.size)

        sweepCookies <- ContT $ withAsync $ forever do
          pause @'Seconds 300
          atomically do
            pips <- readTVar _tcpPeerConn
            modifyTVar _tcpPeerToCookie (HM.filterWithKey (\k _ -> HM.member k pips))
            alive <- readTVar _tcpPeerToCookie <&> HS.fromList . HM.elems
            modifyTVar _tcpPeerCookie (HM.filterWithKey (\k _ -> HS.member k alive))

        sweep <- ContT $ withAsync $ forever do
          pause @'Seconds 300
          now <- getTimeCoarse
          atomically do
            w <- readTVar _tcpSent <&> HPSQ.toList
            let live = [ x | x@(_,t,_) <- w, realToFrac (now - t) / 1e9 < 300 ]
            writeTVar _tcpSent (HPSQ.fromList live)
          -- atomically do
          --   pips <- readTVar _tcpPeerConn
          --   modifyTVar _tcpSent (HM.filterWithKey (\k _ -> HM.member k pips))
          --   modifyTVar _tcpPeerSocket (HM.filterWithKey (\k _ -> HM.member k pips))
          --   modifyTVar _tcpPeerCookie (HM.filter (>=1))

        (_,e) <- waitAnyCatchCancel [p1,p2,probes,sweep,sweepCookies]

        err $ "TCP server is down because of" <+> viaShow e
        pause @'Seconds 10
        lift again

    where

      withTCPTimeout timeout action = liftIO do
        r <- race (pause timeout) action
        case r of
          Right{} -> pure ()
          Left{} -> do
            debug "tcp connection timeout!"
            throwIO TCPPeerReadTimeout

      killCookie :: Int -> Maybe Int
      killCookie  = \case
           n | n <= 1 -> Nothing
           n          -> Just (pred n)

      -- useCookie :: forall m . (?env :: MessagingTCP, MonadIO m) => Word32 -> m Bool
      useCookie peer cookie = atomically do
        let MessagingTCP{..} = ?env
        n <- readTVar _tcpPeerCookie <&> HM.member cookie
        unless n do
          modifyTVar _tcpPeerCookie (HM.insertWith (+) cookie 1)
          modifyTVar _tcpPeerToCookie (HM.insert peer cookie)
        pure n

      -- FIXME: timeout-hardcode
      readFrames :: forall n . MonadIO n => Socket -> Peer L4Proto -> TBQueue (Peer L4Proto, ByteString) -> n ()
      readFrames so peer queue = forever $ withTCPTimeout (TimeoutSec 67) do
        void $ readFromSocket so 4 <&> LBS.toStrict
        ssize <- readFromSocket so 4 <&> LBS.toStrict
        let size = word32 ssize & fromIntegral
        bs <- readFromSocket so size
        -- re-tag the frame if this connection's peer has adopted a routable
        -- name (see 'tcpAdoptName'); otherwise deliver under its own peer
        canon <- readTVarIO _tcpPeerAlias <&> HM.findWithDefault peer peer
        atomically $ writeTBQueueDropSTM outMessageQLen queue (canon, bs)

      runServer = flip runContT pure do

        own <- toPeerAddr $ view tcpOwnPeer env
        let (L4Address _ (IPAddrPort (i,p))) = own
        let myCookie = view tcpCookie env

        -- server
        liftIO $ listen (Host (show i)) (show p) $ \(sock, sa) -> do
          withFdSocket sock setCloseOnExecIfNeeded
          debug $ "Listening on" <+> pretty sa

          forever do
            void $ acceptFork sock $ \(so, remote) -> void $ flip runContT pure $ callCC \exit -> do
              liftIO $ withFdSocket so setCloseOnExecIfNeeded
              debug $ "!!! GOT INCOMING CONNECTION FROM !!!"
                <+> brackets (pretty remote)

              let ?env = env

              let newP = fromSockAddr @'TCP  remote :: Peer L4Proto

              cookie <- handshake Server env so

              when (cookie == myCookie) $ exit ()

              here <- useCookie newP cookie

              when here $ do
                debug $ "SERVER : ALREADY CONNECTED" <+> pretty cookie <+> viaShow remote
                exit ()

              atomically $ modifyTVar _tcpServerThreadsCount succ

              now <- getTimeCoarse
              newOutQ <- atomically do
                q <- readTVar _tcpSent <&> HPSQ.lookup newP >>= \case
                       Just (_,w) -> pure w
                       Nothing -> do
                         nq <- newTBQueue outMessageQLen
                         modifyTVar _tcpSent (HPSQ.insert newP now nq)
                         pure nq

                modifyTVar _tcpPeerConn (HM.insert newP (connectionId myCookie cookie))
                modifyTVar _tcpPeerSocket (HM.insert newP so)

                pure q

              wr <- ContT $ withAsync $ forever  do
                      bs <- atomically $ readTBQueue newOutQ

                      -- FIXME: check-this!
                      let pq = myCookie -- randomIO
                      let qids = bytestring32 pq
                      let size = bytestring32 (fromIntegral $ LBS.length bs)

                      let frame = LBS.fromStrict qids
                                   <> LBS.fromStrict size -- req-size
                                   <> bs -- payload

                      sendLazy so frame --(LBS.toStrict frame)

              rd <- ContT $ withAsync $ readFrames so newP _tcpReceived

              void $ ContT $ bracket none $ const do
                debug $ "SHUTDOWN SOCKET AND SHIT" <+> pretty remote
                atomically do
                  modifyTVar _tcpServerThreadsCount pred
                  modifyTVar _tcpPeerSocket (HM.delete newP)
                  modifyTVar _tcpPeerToCookie (HM.delete newP)

                shutdown so ShutdownBoth
                cancel rd
                cancel wr

                atomically do
                  modifyTVar _tcpSent (HPSQ.delete newP)
                  modifyTVar _tcpPeerCookie (HM.update killCookie cookie)
                  -- drop any routable-name alias adopted onto this connection
                  -- (see 'tcpAdoptName') so its onion key does not leak
                  alias <- readTVar _tcpPeerAlias <&> HM.lookup newP
                  for_ alias $ \name -> do
                    modifyTVar _tcpPeerConn     (HM.delete name)
                    modifyTVar _tcpPeerSocket   (HM.delete name)
                    modifyTVar _tcpPeerToCookie (HM.delete name)
                    modifyTVar _tcpSent         (HPSQ.delete name)
                  modifyTVar _tcpPeerAlias (HM.delete newP)

              void $ waitAnyCatchCancel [rd,wr]

      runClient = flip runContT pure do

        let myCookie = view tcpCookie env

        pause @'Seconds 3.14

        void $ ContT $ bracket none $ const $ do
          what <- atomically $ stateTVar _tcpClientThreads (\x -> (HM.elems x, mempty))
          mapM_ cancel what

        void $ ContT $ withAsync $ forever do
          pause @Seconds 60

          done <- readTVarIO _tcpClientThreads <&> HM.toList
                   >>= filterM ( \(_,x) -> isJust <$> poll x )
                   <&> HS.fromList . fmap fst

          atomically $ modifyTVar _tcpClientThreads (HM.filterWithKey (\k _ -> not (HS.member k done)))

        forever $ void $ runMaybeT do
          -- client sockets

          who <- atomically do
                    readTVar  _tcpConnDemand <&> HPSQ.minView >>= \case
                      Nothing -> STM.retry
                      Just (p,_,_,rest) -> writeTVar _tcpConnDemand rest >> pure p

          already <- readTVarIO _tcpPeerConn <&> HM.member who

          when already mzero

          debug $ "DEMAND:" <+> pretty who

          whoAddr <- toPeerAddr who

          liftIO $ newClientThread env $ do
            let (host, port) = case whoAddr of
                  L4Address _ (IPAddrPort (ip,p)) -> (show ip, p)
                  L4AddressName _ h p             -> (Text.unpack h, p)
            connectTCP _tcpSOCKS5 host port $ \(so, remoteAddr) -> do

              let ?env = env

              flip runContT pure $ callCC \exit -> do

                debug $ "OPEN CLIENT CONNECTION" <+> pretty host <+> pretty port <+> pretty remoteAddr
                cookie <- handshake Client env so
                let connId = connectionId cookie myCookie

                when (cookie == myCookie) $ do
                  debug $ "same peer, exit" <+> pretty remoteAddr
                  exit ()

                here <- useCookie who cookie

                -- TODO: handshake notification
                liftIO $ _tcpOnClientStarted whoAddr connId

                when here do
                  debug $ "CLIENT: ALREADY CONNECTED" <+> pretty cookie <+> pretty host <+> pretty port
                  exit ()

                atomically do
                  modifyTVar _tcpPeerCookie (HM.insertWith (+) cookie 1)
                  modifyTVar _tcpPeerConn (HM.insert who connId)
                  modifyTVar _tcpPeerSocket (HM.insert who so)

                wr <- ContT $ withAsync $ forever  do
                        bss <- atomically do
                                 q' <- readTVar _tcpSent <&> fmap (view _2) . HPSQ.lookup who
                                 maybe1 q' mempty $ \q -> do
                                  s <- readTBQueue q
                                  sx <- flushTBQueue q
                                  pure (s:sx)

                        for_ bss $ \bs -> do
                          -- FIXME: check-this!
                          let pq = myCookie -- randomIO
                          let qids = bytestring32 pq
                          let size = bytestring32 (fromIntegral $ LBS.length bs)

                          let frame = LBS.fromStrict qids
                                       <> LBS.fromStrict size -- req-size
                                       <> bs -- payload

                          sendLazy so frame --(LBS.toStrict frame)

                void $ ContT $ bracket none $ const $ do
                  debug "!!! TCP: BRACKET FIRED IN CLIENT !!!"
                  atomically do
                    modifyTVar _tcpPeerConn (HM.delete who)
                    modifyTVar _tcpPeerCookie (HM.update killCookie cookie)
                    modifyTVar _tcpPeerToCookie (HM.delete who)
                    modifyTVar _tcpPeerSocket (HM.delete who)
                    modifyTVar _tcpSent (HPSQ.delete who)

                void $ ContT $ bracket none (const $ cancel wr)

                readFrames so who _tcpReceived

