-- | A TCP peer that was down when we dialled it is dialled again.
--
-- AGAINST REAL SOCKETS on 127.0.0.1, because the defect is in who asks for a
-- connection and when, and that lives in 'runMessagingTCP' itself. The peer
-- side is a second 'MessagingTCP' started only after the first dial has failed,
-- which is what a neighbour restarting looks like from here.
module TestTCPRedial (testTCPRedialsAfterFailedDial) where

import HBS2.Clock
import HBS2.Net.Messaging
import HBS2.Net.Messaging.TCP
import HBS2.Net.Proto.Types
import HBS2.Prelude.Plated

import Control.Concurrent.Async (withAsync, race)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Network.Socket
import Test.Tasty.HUnit

-- A port nobody is listening on right now: bind to 0, read it back, let go.
freePort :: IO Int
freePort = do
  s <- socket AF_INET Stream defaultProtocol
  bind s (SockAddrInet 0 (tupleToHostAddress (127,0,0,1)))
  p <- socketPort s
  close s
  pure (fromIntegral p)

tcpAddr :: Int -> PeerAddr L4Proto
tcpAddr p = fromMaybe (error "bad address")
          $ fromStringMay ("tcp://127.0.0.1:" <> show p)

-- | The first send finds the far end down; a send after it comes up arrives.
--
-- Before the fix the first send left an out-queue behind with no connection
-- under it, every later send went into that queue, and nothing asked for a
-- connection again, so the second half of this test waited forever.
testTCPRedialsAfterFailedDial :: IO ()
testTCPRedialsAfterFailedDial = do
  pa <- freePort
  pb <- freePort

  a <- newMessagingTCP (tcpAddr pa)
  b <- newMessagingTCP (tcpAddr pb)

  me    <- fromPeerAddr (tcpAddr pa)
  there <- fromPeerAddr (tcpAddr pb)

  let send' = sendTo a (To there) (From me) . LBS.fromStrict

  withAsync (runMessagingTCP a) $ \_ -> do

    -- b is not running yet: this dial is refused.
    send' "while you were down"
    pause @'Seconds 5

    withAsync (runMessagingTCP b) $ \_ -> do
      pause @'Seconds 1

      let keepSending = forever do
            send' "you are back"
            pause @'Seconds 1

      r <- race (pause @'Seconds 30) $ withAsync keepSending $ \_ ->
             receive @_ @_ @LBS.ByteString b (To me)

      case r of
        Left{}   -> assertFailure "nothing reached the peer once it was back"
        Right ms -> assertBool "received an empty batch" (not (null ms))
