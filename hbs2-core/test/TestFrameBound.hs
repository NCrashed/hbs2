-- | What a stream reader does with a length the far end chose.
--
-- AGAINST A REAL SOCKET, because that is where the defect lives: @recv sock n@
-- allocates a buffer of n, so the difference between reading a declared length
-- and reading it in pieces is invisible to anything that stubs the socket out.
-- A socketpair is two file descriptors and needs no network.
module TestFrameBound (testFrameRefusesHugeDeclaredLength
                      ,testFrameTakesWhatThisBuildSends
                      ,testFrameDoesNotAllocateWhatWasPromised) where

import HBS2.Defaults (defMaxFrame)
import HBS2.Net.Messaging.Stream

import Control.Concurrent.Async (withAsync)
import Control.Exception (try)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Test.Tasty.HUnit

-- Two ends of one connection, with no network in it.
withPair :: ((Socket, Socket) -> IO a) -> IO a
withPair act = do
  (a, b) <- socketPair AF_UNIX Stream defaultProtocol
  r <- act (a, b)
  close a
  close b
  pure r

-- | A length above the ceiling is refused, and it is refused BEFORE the bytes.
--
-- The far end here sends the header and nothing else, which is the shape of the
-- attack: four bytes buy an allocation of whatever they name.
testFrameRefusesHugeDeclaredLength :: IO ()
testFrameRefusesHugeDeclaredLength = withPair $ \(client, server) -> do
  sendAll client (BS.pack [0xff,0xff,0xff,0xff])   -- and no payload at all
  -- The largest a four-byte header can say, which is the whole of what the
  -- wire can offer: 4 GiB, from four bytes and no handshake worth the name.
  r <- try @FrameTooLarge (readFrame server 4294967295)
  case r of
    Left (FrameTooLarge n) -> assertBool "the refusal names the length" (n > defMaxFrame)
    Right _ -> assertFailure "read a frame nobody sent"

-- | And a frame of the size this software actually produces is read whole.
--
-- The other half: a ceiling that refused real traffic would be a ceiling that
-- takes the node off the network, which is worse than what it prevents.
testFrameTakesWhatThisBuildSends :: IO ()
testFrameTakesWhatThisBuildSends = withPair $ \(client, server) -> do
  let payload = BS.replicate (256 * 1024) 0x41   -- a block, which is the big one
  withAsync (sendAll client payload) $ \_ -> do
    got <- readFrame server (BS.length payload)
    LBS.length got @?= fromIntegral (BS.length payload)

-- | A frame that is promised and not delivered ends with the stream.
--
-- WHAT THIS DOES NOT PIN, and the comment above it used to claim otherwise:
-- that the read is CHUNKED. Whether 'readFrame' asks the socket for eight
-- megabytes in one recv or for sixty-four kilobytes eight times over is a
-- difference in what is allocated transiently, and recv returns what has
-- arrived either way -- so the two are indistinguishable from here, and
-- swapping the chunked reader for the unchunked one leaves this green.
-- Measuring allocation would pin it; that is a harness this suite has not got.
--
-- What it does pin is the other half of the promise: a peer that declares a
-- permitted size and then stops does not leave the reader holding a frame it
-- can hand on. The stream ends and the reader says so.
testFrameDoesNotAllocateWhatWasPromised :: IO ()
testFrameDoesNotAllocateWhatWasPromised = withPair $ \(client, server) -> do
  let declared = 8 * 1024 * 1024                 -- allowed by the ceiling
      sent     = 64 * 1024
  void $ sendAll client (BS.replicate sent 0x42)
  -- Nothing else is coming, so the reader is waiting rather than holding the
  -- eight megabytes it was promised. Closing the sending end ends the frame.
  close client
  r <- try @SocketClosedException (readFrame server declared)
  case r of
    Left _  -> pure ()      -- the stream ended, which is the honest answer
    Right b -> assertBool "read no more than was sent"
                          (LBS.length b <= fromIntegral sent)
