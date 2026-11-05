{-# Language DataKinds #-}
{-# Language NamedFieldPuns #-}

-- | Authoritative mailbox message lifetime semantics.
--
-- Timestamps and TTLs are whole Unix seconds.  A receiver tolerates
-- 'defaultMailboxClockSkew' in both directions: a message created farther in
-- the future is not admissible, and an otherwise expired message remains live
-- through the same window.  The latter is important for convergence: every
-- boundary asks this function, rather than inventing its own comparison.
module HBS2.Peer.Proto.Mailbox.Retention
  ( MailboxLifetime(..)
  , MailboxRetentionRequest(..)
  , MailboxRetentionEntry(..)
  , MailboxRetentionReport(..)
  , defaultMailboxClockSkew
  , mailboxLifetime
  , messageDeadline
  , messageIsLive
  , messageObjectRoots
  , mailboxPurgeCandidates
  ) where

import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Peer.Proto.Mailbox.Ref (MailboxRefKey)

import Codec.Serialise (Serialise)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)

data MailboxLifetime
  = MessageNotYetValid
  | MessageLive
  | MessageExpired
  deriving stock (Eq,Ord,Show)

data MailboxRetentionRequest = MailboxRetentionRequest
  { retentionTargetMailbox  :: Maybe (MailboxRefKey 'HBS2Basic)
  , retentionProtectedParts :: Set HashRef
  , retentionExecute        :: Bool
  }
  deriving stock (Eq,Generic)

instance Serialise MailboxRetentionRequest

data MailboxRetentionEntry = MailboxRetentionEntry
  { retentionEntryMailbox    :: Text
  , retentionEntryMessage    :: HashRef
  , retentionEntryCause      :: Text
  , retentionEntryObservedAt :: Word64
  , retentionEntryPurgeAfter :: Word64
  , retentionEntryPurged     :: Bool
  }
  deriving stock (Eq,Show,Generic)

instance Serialise MailboxRetentionEntry

data MailboxRetentionReport = MailboxRetentionReport
  { retentionReportNow             :: Word64
  , retentionReportClockSkew       :: Word64
  , retentionReportPurgeGrace      :: Word64
  , retentionReportEntries         :: [MailboxRetentionEntry]
  , retentionReportEligible        :: Int
  , retentionReportProtectedParts  :: Int
  , retentionReportCandidateBlocks :: Int
  , retentionReportDeletedBlocks   :: Int
  , retentionReportFailures        :: [Text]
  }
  deriving stock (Eq,Show,Generic)

instance Serialise MailboxRetentionReport

-- | Five minutes, in seconds.
--
-- This is deliberately much smaller than the purge grace (days): skew only
-- absorbs ordinary clock disagreement; grace is what lets disconnected peers
-- learn a tombstone before bytes disappear.
defaultMailboxClockSkew :: Word64
defaultMailboxClockSkew = 5 * 60

-- | The unsigned Unix-second deadline, saturated instead of wrapping.
messageDeadline :: MessageFlags -> Maybe Word64
messageDeadline MessageFlags1{messageCreated = MessageTimestamp created, messageTTL} =
  (\(MessageTTL ttl) -> saturatingAdd created (fromIntegral ttl)) <$> messageTTL

-- | Classify a signed message at @now@ with an allowed clock disagreement.
mailboxLifetime :: Word64 -> Word64 -> MessageFlags -> MailboxLifetime
mailboxLifetime skew now flags@MessageFlags1{messageCreated = MessageTimestamp created}
  | created > saturatingAdd now skew = MessageNotYetValid
  | maybe False (\deadline -> saturatingAdd deadline skew <= now) (messageDeadline flags) = MessageExpired
  | otherwise = MessageLive

messageIsLive :: Word64 -> Word64 -> MessageFlags -> Bool
messageIsLive skew now = (== MessageLive) . mailboxLifetime skew now

-- | Semantic storage roots owned by a message, excluding its envelope block.
messageObjectRoots :: MessageContent s -> Set HashRef
messageObjectRoots content = messageParts content <>
  either Set.singleton (const mempty) (messageGK0 content)

-- | Physical blocks safe to remove after mailbox compaction.
--
-- Message envelopes are supplied separately from semantic descendants because
-- an envelope is opaque CBOR, not a Merkle root.  Anything reachable from a
-- still-live message or accepted canon wins over a victim, which handles both
-- shared attachment trees and the fold-then-delete case.
mailboxPurgeCandidates
  :: Set HashRef -- ^ victim message blocks
  -> Set HashRef -- ^ descendants of victim attachments/group keys
  -> Set HashRef -- ^ live message blocks
  -> Set HashRef -- ^ descendants of live attachments/group keys
  -> Set HashRef -- ^ canon-reachable attachment blocks
  -> Set HashRef
mailboxPurgeCandidates victims victimDescendants live liveDescendants canon =
  (victims <> victimDescendants)
    `Set.difference` (live <> liveDescendants <> canon)

saturatingAdd :: Word64 -> Word64 -> Word64
saturatingAdd a b
  | maxBound - a < b = maxBound
  | otherwise = a + b
