{-# Language DataKinds #-}
module MailboxRetentionSpec (tests) where

import HBS2.Peer.Proto.Mailbox.Retention
import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject)
import MailboxConfig

import Data.Config.Suckless

import Test.Tasty
import Test.Tasty.HUnit

import Data.Word (Word32,Word64)
import Data.Set qualified as Set
import Data.ByteString.Char8 qualified as BS8

flags :: Word64 -> Maybe Word32 -> MessageFlags
flags created ttl = MessageFlags1
  { messageCreated = MessageTimestamp created
  , messageTTL = MessageTTL <$> ttl
  , messageCompression = Nothing
  , messageSchema = Nothing
  }

tests :: TestTree
tests = testGroup "mailbox retention"
  [ testCase "a message without TTL remains live" $
      mailboxLifetime 300 999999 (flags 10 Nothing) @?= MessageLive
  , testCase "TTL includes the allowed clock skew" $ do
      mailboxLifetime 300 1400 (flags 1000 (Just 100)) @?= MessageExpired
      mailboxLifetime 300 1399 (flags 1000 (Just 100)) @?= MessageLive
  , testCase "a future timestamp beyond skew is refused" $ do
      mailboxLifetime 300 1000 (flags 1301 Nothing) @?= MessageNotYetValid
      mailboxLifetime 300 1000 (flags 1300 Nothing) @?= MessageLive
  , testCase "deadline arithmetic saturates" $
      messageDeadline (flags (maxBound - 1) (Just 10)) @?= Just maxBound
  , testCase "expiration plus skew arithmetic saturates" $
      mailboxLifetime 300 (maxBound - 1) (flags (maxBound - 1) (Just 10)) @?= MessageLive
  , testCase "retention windows are configurable in seconds" $
      retentionWindowsFrom (conf
        "(hbs2:mailbox:purge-grace 34)")
        @?= (defaultMailboxClockSkew,34)
  , testCase "invalid retention windows keep safe defaults" $
      retentionWindowsFrom (conf "(hbs2:mailbox:purge-grace -1)")
        @?= (defaultMailboxClockSkew,defaultMailboxPurgeGrace)
  , testCase "purge grace respects SQLite's integer boundaries" $ do
      retentionWindowsFrom (conf "(hbs2:mailbox:purge-grace 0)")
        @?= (defaultMailboxClockSkew,0)
      retentionWindowsFrom (conf
        ("(hbs2:mailbox:purge-grace " <> show maximumMailboxRetentionSecond <> ")"))
        @?= (defaultMailboxClockSkew,maximumMailboxRetentionSecond)
      retentionWindowsFrom (conf
        ("(hbs2:mailbox:purge-grace " <> show (maximumMailboxRetentionSecond + 1) <> ")"))
        @?= (defaultMailboxClockSkew,defaultMailboxPurgeGrace)
  , testCase "canon-reachable and live shared blocks always beat purge" $ do
      let named = HashRef . hashObject . BS8.pack
          victimMessage = named "victim"
          privatePart   = named "private"
          canonPart     = named "canon"
          sharedPart    = named "shared"
      mailboxPurgeCandidates
        (Set.singleton victimMessage)
        (Set.fromList [privatePart,canonPart,sharedPart])
        mempty
        (Set.singleton sharedPart)
        (Set.singleton canonPart)
        @?= Set.fromList [victimMessage,privatePart]
  , testCase "group-key references and attachments are both purge roots" $ do
      let named = HashRef . hashObject . BS8.pack
          groupKey = named "group-key"
          attachment = named "attachment"
          content :: MessageContent 'HBS2Basic
          content = MessageContent (flags 0 Nothing) mempty (Left groupKey)
                      (Set.singleton attachment) (error "message data is not inspected")
      messageObjectRoots content @?= Set.fromList [groupKey,attachment]
  ]
  where
    conf :: String -> [Syntax C]
    conf = either (error . show) id . parseTop
