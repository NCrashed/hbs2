module HBS2.Data.CBOR
  ( encodeRecordHeader
  , decodeRecordHeader
  ) where

import Codec.CBOR.Decoding (Decoder, decodeListLen, decodeWord)
import Codec.CBOR.Encoding (Encoding, encodeListLen, encodeWord)
-- | The constructor-tagged product framing used by hbs2's explicit stable
-- CBOR codecs.  Keeping it in one place prevents two protocol records from
-- accidentally choosing subtly different frozen shapes.
encodeRecordHeader :: Word -> Encoding
encodeRecordHeader fields = encodeListLen fields <> encodeWord 0

decodeRecordHeader :: String -> Int -> Decoder s ()
decodeRecordHeader name expectedFields = do
  fields <- decodeListLen
  constructorTag <- decodeWord
  if fields == expectedFields && constructorTag == 0
    then pure ()
    else fail (name <> ": invalid CBOR record header")
