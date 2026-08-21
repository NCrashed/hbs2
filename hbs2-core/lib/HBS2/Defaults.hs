module HBS2.Defaults where

import HBS2.Clock
import Data.String

defMaxDatagram :: Int
defMaxDatagram = 4096

defMaxDatagramRPC :: Int
defMaxDatagramRPC = 4096


-- | The largest frame a stream transport will read before hanging up.
--
-- WHY A CEILING EXISTS AT ALL. A stream frame is a four-byte length and then
-- that many bytes, and the length is a number the far end chose. Reading it as
-- given means an unauthenticated peer can declare 4 GiB and have this process
-- try to hold it: on TCP that is past a four-byte cookie handshake and nothing
-- else, and on the UNIX socket it is anybody who can open the path.
--
-- ABOVE ANY FRAME THIS CODEBASE SENDS, by two orders. A block is
-- 'defBlockSize' (256 KiB) and never travels whole -- it is requested in chunks
-- bounded by the datagram ceiling -- and the largest single protocol payload is
-- a mailbox message, which its own layer bounds well below this. So the number
-- is not a tuning knob: it is the line between "a frame this software could
-- have produced" and "a length somebody typed".
--
-- IT IS NOT A MEMORY BUDGET. One connection may still hold this much, and a
-- hundred connections a hundred times it; what bounds THAT is how many peers a
-- node talks to, which is a different decision in a different place. What this
-- removes is the part where one peer picks the number.
defMaxFrame :: Int
defMaxFrame = 16 * 1024 * 1024

defMessageQueueSize :: Integral a => a
defMessageQueueSize = 65536*10

defBurstMax :: Integral a => a
defBurstMax = 128

defBurst :: Integral a => a
defBurst = defBurstMax `div` 4

-- defChunkSize :: Integer
defChunkSize :: Integral a => a
defChunkSize = 1024
-- defChunkSize = 480

defBlockSize :: Integer
defBlockSize =  256 * 1024

defStorePath :: IsString a => a
defStorePath = "hbs2"

defPipelineSize :: Int
defPipelineSize = 16000

defBlockDownloadQ :: Integral a => a
defBlockDownloadQ = 65536*10

defBlockDownloadThreshold :: Integral a => a
defBlockDownloadThreshold = 1

-- typical block hash 530+ chunks * parallel wip blocks amount
defProtoPipelineSize :: Int
defProtoPipelineSize = 65536*2

defCookieTimeoutSec :: Timeout 'Seconds
defCookieTimeoutSec = 7200

defCookieTimeout :: TimeSpec
defCookieTimeout = toTimeSpec defCookieTimeoutSec

defRequestLimit :: TimeSpec
defRequestLimit = toTimeSpec defRequestLimitSec

defBlockSizeCacheTime :: TimeSpec
defBlockSizeCacheTime = toTimeSpec ( 30 :: Timeout 'Seconds )

defRequestLimitSec :: Timeout 'Seconds
defRequestLimitSec = 300

defBlockBanTime :: TimeSpec
defBlockBanTime = toTimeSpec defBlockBanTimeSec

defBlockPostponeTime :: TimeSpec
defBlockPostponeTime = toTimeSpec ( 45 :: Timeout 'Seconds)

defBlockBanTimeSec :: Timeout 'Seconds
defBlockBanTimeSec = 60 :: Timeout 'Seconds

defBlockWipTimeout :: TimeSpec
defBlockWipTimeout = defCookieTimeout

defBlockInfoTimeout :: Timeout 'Seconds
defBlockInfoTimeout = 5

defBlockInfoTimeoutSpec :: TimeSpec
defBlockInfoTimeoutSpec = toTimeSpec defBlockInfoTimeout

-- how much time wait for block from peer?
defBlockWaitMax :: Timeout 'Seconds
defBlockWaitMax = 60 :: Timeout 'Seconds

-- how much time wait for block from peer?
defChunkWaitMax :: Timeout 'Seconds
defChunkWaitMax = 30  :: Timeout 'Seconds

defSweepTimeout :: Timeout 'Seconds
defSweepTimeout = 60 -- FIXME: only for debug!

defPeerAnnounceTime :: Timeout 'Seconds
defPeerAnnounceTime = 120

defPexMaxPeers :: Int
defPexMaxPeers = 50

defDownloadFails :: Int
defDownloadFails = 100

defGetPeerMetaTimeout :: Timeout 'Seconds
defGetPeerMetaTimeout = 10

-- TODO: peer-does-not-have-a-block-ok
--  Это нормально, когда у пира нет блока.
--  У него может не быть каких-то блоков,
--  а какие-то могут быть. Нужно более умный
--  алгоритм, чем бан пира за отсутствие блока.

defUsefulLimit :: Double
defUsefulLimit = 0.25

defInterBlockDelay :: Timeout 'Seconds
defInterBlockDelay = 0.0085


defHashListChunk :: Integral a => a
defHashListChunk = 1024
{-# INLINE defHashListChunk #-}

defTreeChildNum :: Integral a => a
defTreeChildNum = 256
{-# INLINE defTreeChildNum #-}


