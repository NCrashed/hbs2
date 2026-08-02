{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | The wire shape of a key, and the length rule that used to be missing.
--
-- The five saltine types this project puts on the wire are newtypes over a
-- ByteString, and they used to get DERIVED 'Serialise' instances. The generic
-- encoding is that ByteString, and the generic decoding takes a ByteString of
-- any length; saltine's own decoder is the length check for the scheme, and it
-- was being walked straight past.
--
-- Two things have to hold at once, and this module is here because they pull in
-- opposite directions. The DECODER must refuse a key that is not the right
-- length, because libsodium's verify takes no length argument and reads its
-- thirty-two or sixty-four bytes out of whatever buffer it is handed: a key with
-- a byte appended verifies its owner's signatures and is a different value,
-- equal to nothing on any deny list. The ENCODER must not change by one byte,
-- because these bytes are inside signatures, inside block hashes, and inside
-- every ref key derived from a public key, so a new spelling would invalidate
-- the network and everything already stored in it.
module TestKeyEncoding (testKeyEncodingUnchanged, testKeyEncodingRefusesBadLength) where

import HBS2.Net.Auth.Credentials ()

import Codec.Serialise
import Control.Monad (forM_)
import Crypto.Saltine.Class qualified as Crypto
import Crypto.Saltine.Core.Box qualified as Encrypt
import Crypto.Saltine.Core.Sign qualified as Sign
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Test.Tasty.HUnit

-- One case: a value, the bytes saltine says it is, and the name to complain
-- under. Existentially packed so the five types can be walked over as a list.
data Cased = forall a . (Serialise a, Crypto.IsEncoding a, Eq a) => Cased String a

cases :: IO [Cased]
cases = do
  skp <- Sign.newKeypair
  ekp <- Encrypt.newKeypair
  let sig = Sign.signDetached (Sign.secretKey skp) "message"
  pure [ Cased "Sign.PublicKey"    (Sign.publicKey skp)
       , Cased "Sign.SecretKey"    (Sign.secretKey skp)
       , Cased "Sign.Signature"    sig
       , Cased "Encrypt.PublicKey" (Encrypt.publicKey ekp)
       , Cased "Encrypt.SecretKey" (Encrypt.secretKey ekp)
       ]

-- | The encoding is what it always was, byte for byte.
testKeyEncodingUnchanged :: IO ()
testKeyEncodingUnchanged = do
  cs <- cases
  forM_ cs $ \(Cased nm a) -> do
    let enc = serialise a
        raw = Crypto.encode a

    -- A two-element array, the constructor tag, then the raw bytes as a CBOR
    -- byte string. 0x58 0x20 is 32 bytes and 0x58 0x40 is 64: the two sizes the
    -- schemes use. These four bytes ARE the compatibility promise.
    LBS.unpack (LBS.take 2 enc) @?= [0x82, 0x00]
    assertEqual (nm <> ": length prefix")
      (if BS.length raw == 32 then [0x58, 0x20] else [0x58, 0x40])
      (LBS.unpack (LBS.take 2 (LBS.drop 2 enc)))

    -- Nothing is added, reordered or re-encoded around the key: the tail of the
    -- encoding is exactly what saltine calls the value.
    assertEqual (nm <> ": payload is the saltine encoding")
      (LBS.fromStrict raw)
      (LBS.drop 4 enc)
    assertEqual (nm <> ": total size") (4 + BS.length raw) (fromIntegral (LBS.length enc))

    -- And it still comes back.
    case deserialiseOrFail enc of
      Left e  -> assertFailure (nm <> ": did not round trip: " <> show e)
      Right b -> assertBool (nm <> ": round tripped to a different value") (a == b)

-- | A key that is not the length its scheme wants is not a key.
testKeyEncodingRefusesBadLength :: IO ()
testKeyEncodingRefusesBadLength = do
  cs <- cases
  forM_ cs $ \(Cased nm a) -> do
    let raw = Crypto.encode a

    -- Re-encode the same value with a byte appended, the way an attacker
    -- would: the prefix says one more byte and one more byte follows. This is
    -- what used to decode, and it is what every deny list in the project would
    -- then fail to match.
    assertRefused nm (encodedAs (raw <> "A")) a

    -- The quieter direction: shorter than the scheme, which libsodium reads
    -- past the end of rather than complaining about.
    assertRefused nm (encodedAs (BS.take 1 raw)) a

    -- Empty is a length too.
    assertRefused nm (encodedAs mempty) a

  where
    -- The project's own encoding, with an arbitrary payload substituted.
    encodedAs :: BS.ByteString -> LBS.ByteString
    encodedAs bs = LBS.pack [0x82, 0x00] <> serialise bs

    assertRefused :: forall a . (Serialise a, Eq a) => String -> LBS.ByteString -> a -> IO ()
    assertRefused nm bs _ =
      case deserialiseOrFail @a bs of
        Left _  -> pure ()
        Right _ -> assertFailure (nm <> ": a wrong-length payload decoded as a key")
