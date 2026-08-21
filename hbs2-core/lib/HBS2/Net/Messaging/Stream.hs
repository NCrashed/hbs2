module HBS2.Net.Messaging.Stream where

import HBS2.Prelude.Plated
import HBS2.Defaults (defMaxFrame)

import Control.Exception (Exception,throwIO)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Network.Socket hiding (listen,connect)
import Streaming.Prelude qualified as S
import Data.ByteString qualified as BS
import Network.Simple.TCP

data SocketClosedException =
    SocketClosedException
    deriving stock (Show, Typeable)

instance Exception SocketClosedException

-- | The far end declared a frame longer than this build will read.
--
-- Its own exception rather than a silent drop, because the connection is not
-- usable afterwards: the length is how a reader finds the next frame, so a
-- reader that skipped an oversized one would be guessing where the stream
-- resumes. The caller closes the connection.
data FrameTooLarge = FrameTooLarge Int
    deriving stock (Show, Typeable)

instance Exception FrameTooLarge

-- | Read a frame whose length the far end chose.
--
-- THE LENGTH IS NOT A PROMISE. Both stream transports read four bytes and then
-- that many, and until this they read exactly what was asked for: @recv sock
-- n@ allocates a buffer of n, so a peer declaring 4 GiB had this process try to
-- hold 4 GiB before a single byte of it had arrived. On TCP that sits behind a
-- four-byte cookie handshake and nothing else.
--
-- Two things, and neither is enough alone. The ceiling ('defMaxFrame') refuses
-- a length no frame this software produces could have; and the read is chunked,
-- so even an accepted length is allocated as it arrives rather than up front --
-- which is what stops a peer that declares a permitted size and then sends
-- nothing from pinning it.
readFrame :: forall m . MonadIO m => Socket -> Int -> m ByteString
readFrame sock size
  -- BOTH ENDS OF THE RANGE. A Word32 header cannot say "negative" on a 64-bit
  -- Int, so the lower guard is not about the wire: it is about this being an
  -- exported function whose argument is a number somebody computed, and recv
  -- answers a non-positive length with an "invalid argument" that names
  -- nothing. A length is not something to trust in either direction.
  | size < 0 || size > defMaxFrame = liftIO (throwIO (FrameTooLarge size))
  | otherwise                      = readFromSocket1 sock size


-- FIXME: why-streaming-then?
--  Ну и зачем тут вообще стриминг,
--  если чтение всё равно руками написал?
--  Если fromChunks - O(n), и reverse O(n)
--  то мы все равно пройдем все чанки, на
--  кой чёрт тогда вообще стриминг? бред
--  какой-то.
readFromSocket :: forall m . MonadIO m
               => Socket
               -> Int
               -> m ByteString

readFromSocket sock size = LBS.fromChunks <$> (go size & S.toList_)
  where
    go 0 = pure ()
    go n = do
      r <- liftIO $ recv sock n
      maybe1 r eos $ \bs -> do
        let nread = BS.length bs
        S.yield bs
        go (max 0 (n - nread))

    eos = do
      liftIO $ throwIO SocketClosedException

readFromSocket1 :: forall m . MonadIO m
               => Socket
               -> Int
               -> m ByteString

readFromSocket1 sock size = LBS.fromChunks <$> (go size & S.toList_)
  where
    go 0 = pure ()
    go n = do
      r <- liftIO $ recv sock (min 65536 n)
      maybe1 r eos $ \bs -> do
        let nread = BS.length bs
        S.yield bs
        go (max 0 (n - nread))

    eos = do
      liftIO $ throwIO SocketClosedException
