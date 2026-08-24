module Main where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Base58 as B58
import HBS2.Hash
import HBS2.Net.Proto.Types
import HBS2.Peer.Proto.RefLog
import HBS2.Net.Auth.Schema
import HBS2.Misc.PrettyStuff

import MailboxMerge (mailboxMergeTests)
import MailboxStatus (mailboxStatusTests)
import MailboxPolicy (mailboxPolicyTests)
import MailboxEntry (mailboxEntryTests)
import MailboxPoW (mailboxPoWTests, mailboxConfigTests)
import MailboxMergedSpec qualified
import MailboxQueueSpec qualified
import RelayedSpec qualified
import MailboxPolicyCacheSpec qualified
import MessageParts (messagePartsTests)
import HttpListen (httpListenTests)
import PeerMetaSpec (peerMetaTests)

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe
import Data.ByteString
import Data.ByteString.Lazy qualified as LBS
import Codec.Serialise
import Crypto.Saltine (sodiumInit)
import Crypto.Saltine.Core.Sign qualified as Sign


newtype W a = W a
              deriving stock Generic

instance Serialise a => Serialise (W a)


newtype X a = X a
              deriving stock Generic

instance Serialise a => Serialise (X a)

newtype VersionedPubKey = VersionedPubKey { versionedPubKey :: ByteString }
                          deriving stock (Show,Generic)

data RefLogRequestVersioned e =
  RefLogRequestVersioned
  { refLogRequestVersioned :: VersionedPubKey
  }
  deriving stock (Show,Generic)

instance Serialise VersionedPubKey

instance Serialise (RefLogRequestVersioned e)

-- | What a public key on the wire is, and what it is not.
--
-- This test used to ESTABLISH THE OPPOSITE of what it establishes now, and the
-- change is the point of it. It was an experiment in versioning a key by
-- appending bytes to it: `RefLogRequestVersioned` carries a bare ByteString
-- where `RefLogRequest` carries a `PubKey`, and the old assertion was that a
-- request built with a key plus @"AAA"@ decoded back as an ordinary
-- `RefLogRequest`. It did, and that was not a compatibility property worth
-- having. It was the observation that a public key was whatever length somebody
-- said it was.
--
-- Every `SignedBox` in this project recovers a key that way and then compares it
-- against something: a mailbox key, a deny list, a maintainer list. libsodium's
-- verify takes no length argument and reads its thirty-two bytes out of whatever
-- buffer it is handed, so a key with a byte appended verifies its owner's
-- signatures exactly as the real key does and is a different value, equal to
-- nothing on any list. So the bytes below are now refused, and the test says so
-- in the direction it is refused in.
testVersionedKeysHashes :: IO ()
testVersionedKeysHashes = do

  keypart <- fromBase58 "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"
               & orThrowUser "bad base58"
               <&> LBS.fromStrict

  pk <- fromStringMay @(PubKey 'Sign 'HBS2Basic) "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"
          & orThrowUser "key decode"

  let pks = serialise pk

  -- The ENCODING is unchanged and has to be: this key's bytes are inside every
  -- ref key derived from it and inside every signature over it, so a different
  -- spelling would invalidate everything already stored. Two elements, the
  -- constructor tag, and the raw thirty-two bytes.
  LBS.unpack (LBS.take 4 pks) @?= [0x82, 0x00, 0x58, 0x20]
  LBS.length pks @?= 36

  -- Trailing bytes AFTER a complete value are still ignored, which is a property
  -- of deserialiseOrFail and not of keys, and is deliberately left alone here.
  _ <- deserialiseOrFail @(PubKey 'Sign 'HBS2Basic) (pks <> "12345")
         & orThrowUser "a complete key followed by junk should still decode"

  let rfk  = serialise (RefLogKey @'HBS2Basic pk)
  let wrfk = serialise $ W (RefLogKey @'HBS2Basic pk)
  let xrfk = serialise $ X (RefLogKey @'HBS2Basic pk)

  -- A key is the same bytes however deeply it is wrapped: the derivations below
  -- are what ref keys and block hashes are taken over.
  forM_ [rfk, wrfk, xrfk] $ \enc ->
    assertBool "the key's own bytes end every wrapping of it"
      (LBS.isSuffixOf keypart enc)

  let req1  = RefLogRequest @L4Proto pk
  let req1s = serialise req1

  -- A real key still decodes as the loose versioned shape, because that shape
  -- has no length rule of its own. Kept so that the test says which direction
  -- closed and which did not.
  _ <- deserialiseOrFail @(RefLogRequestVersioned L4Proto) req1s
         & orThrowUser "failed simple -> versioned"

  -- THE ONE THAT MATTERS. A "key" of thirty-five bytes is not a key, at the top
  -- level and inside a request alike.
  let req2  = RefLogRequestVersioned @L4Proto ( VersionedPubKey (LBS.toStrict keypart <> "AAA") )
  let req2s = serialise req2

  case deserialiseOrFail @(RefLogRequest L4Proto) req2s of
    Left _  -> pure ()
    Right _ -> assertFailure "a 35-byte public key decoded as a RefLogRequest"

  let fat = LBS.take 3 pks <> LBS.pack [0x23] <> LBS.drop 4 pks <> "AAA"
  case deserialiseOrFail @(PubKey 'Sign 'HBS2Basic) fat of
    Left _  -> pure ()
    Right _ -> assertFailure "a 35-byte public key decoded on its own"

  -- And the other side of the boundary, which is the quiet one: a key SHORTER
  -- than the scheme used to reach libsodium and be read past the end of.
  let thin = LBS.take 3 pks <> LBS.pack [0x41] <> LBS.take 1 (LBS.drop 4 pks)
  case deserialiseOrFail @(PubKey 'Sign 'HBS2Basic) thin of
    Left _  -> pure ()
    Right _ -> assertFailure "a 1-byte public key decoded"

  pure ()

main :: IO ()
main = do
  -- The nonce store draws from libsodium, and the peer initialises it in
  -- PeerMain before anything can ask. The suite has no such entry point, so it
  -- does it here.
  sodiumInit

  defaultMain $
    testGroup "root"
      [
        testCase "testVersionedKeys" testVersionedKeysHashes
      , mailboxMergeTests
      , mailboxStatusTests
      , mailboxPolicyTests
      , mailboxEntryTests
      , httpListenTests
      , peerMetaTests
      , messagePartsTests
      , mailboxPoWTests
      , mailboxConfigTests
      , MailboxQueueSpec.tests
      , MailboxMergedSpec.tests
      , RelayedSpec.tests
      , MailboxPolicyCacheSpec.tests
      ]



