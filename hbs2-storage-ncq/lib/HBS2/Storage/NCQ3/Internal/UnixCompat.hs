{-# LANGUAGE CPP #-}

-- | Cross-version shims for `unix` package API differences between 2.7 and 2.8+.
--
-- pkgsStatic defaults to GHC 9.4 with unix 2.7.3, where:
--
-- * `openFd` takes 4 arguments including @Maybe FileMode@ for create mode,
--   not 3 arguments with @creat@ as an @OpenFileFlags@ field.
-- * `fdRead` returns @(String, ByteCount)@ rather than @ByteString@.
-- * `fdWrite` takes a @String@ rather than a @ByteString@.
--
-- Dynamic builds run GHC 9.6 with unix 2.8+, which uses the newer API.
-- These helpers expose one signature that maps to whichever underlying
-- API is available, keeping call sites portable without scattering CPP.

module HBS2.Storage.NCQ3.Internal.UnixCompat
  ( openFdCompat
  , fdReadBS
  , fdWriteBS
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Foreign.Ptr (castPtr)
import System.Posix.IO qualified as P
import System.Posix.Types (ByteCount, Fd, FileMode)

-- | Open a file descriptor, optionally creating it with the given mode.
openFdCompat :: FilePath -> P.OpenMode -> Maybe FileMode -> P.OpenFileFlags -> IO Fd
#if MIN_VERSION_unix(2,8,0)
openFdCompat fp mode mfm flags = P.openFd fp mode (flags { P.creat = mfm })
#else
openFdCompat fp mode mfm flags = P.openFd fp mode mfm flags
#endif

-- | Read up to @n@ bytes from a file descriptor, returning them as a strict
-- ByteString. Uses 'P.fdReadBuf' under the hood for a stable signature.
fdReadBS :: Fd -> ByteCount -> IO ByteString
fdReadBS fd count = BSI.createUptoN (fromIntegral count) $ \ptr -> do
  bc <- P.fdReadBuf fd ptr count
  pure (fromIntegral bc)

-- | Write a strict ByteString to a file descriptor via 'P.fdWriteBuf'.
fdWriteBS :: Fd -> ByteString -> IO ByteCount
fdWriteBS fd bs = BS.useAsCStringLen bs $ \(p, n) ->
  P.fdWriteBuf fd (castPtr p) (fromIntegral n)
