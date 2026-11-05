{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language QuasiQuotes #-}
{-# Language RecordWildCards #-}
{-# Language UndecidableInstances #-}

-- | Durable mailbox retention decisions.
--
-- The wire tombstone intentionally carries no local observation time.  This
-- table records the first time this peer authenticated one (or independently
-- derived TTL expiry), so reconnecting an old peer cannot restart the grace
-- period forever.
module MailboxRetention
  ( RetentionCause(..)
  , RetentionRow(..)
  , retentionDDL
  , observeRetention
  , retentionRows
  , markRetentionPurged
  ) where

import HBS2.Prelude.Plated
import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Peer.Proto.Mailbox.Ref (MailboxRefKey(..))
import MailboxMerged ()

import DBPipe.SQLite
import Data.Word (Word64)
import Text.InterpolatedString.Perl6 (qc)

data RetentionCause = RetentionTTL | RetentionDeleted
  deriving stock (Eq,Ord,Show)

data RetentionRow s = RetentionRow
  { retentionMailbox    :: MailboxRefKey s
  , retentionMessage    :: HashRef
  , retentionCause      :: RetentionCause
  , retentionObservedAt :: Word64
  , retentionPurgeAfter :: Word64
  , retentionPurged     :: Bool
  }

deriving stock instance ForMailbox s => Eq (RetentionRow s)

instance ForMailbox s => Show (RetentionRow s) where
  show RetentionRow{..} =
    "RetentionRow " <> show (pretty retentionMailbox)
      <> " " <> show retentionMessage
      <> " " <> show retentionCause
      <> " " <> show retentionObservedAt
      <> " " <> show retentionPurgeAfter
      <> " " <> show retentionPurged

newtype RetentionRef = RetentionRef HashRef

instance ToField RetentionRef where
  toField (RetentionRef h) = toField (show (pretty h))

instance FromField RetentionRef where
  fromField w = RetentionRef . fromString <$> fromField @String w

instance ToField RetentionCause where
  toField RetentionTTL = toField ("ttl" :: String)
  toField RetentionDeleted = toField ("deleted" :: String)

instance FromField RetentionCause where
  fromField w = fromField @String w >>= \case
    "ttl" -> pure RetentionTTL
    "deleted" -> pure RetentionDeleted
    other -> fail ("unknown retention cause: " <> other)

retentionDDL :: MonadIO m => DBPipeM m ()
retentionDDL = ddl [qc|create table if not exists
  retention ( mailbox     text not null
            , message     text not null
            , cause       text not null
            , observed_at integer not null
            , purge_after integer not null
            , purged      integer not null default 0
            , primary key (mailbox, message)
            )|]

-- | Remember the first decision.  A later explicit deletion upgrades a TTL
-- reason for diagnostics, but can never move its purge deadline later.
observeRetention :: forall s m . (ForMailbox s, MonadIO m)
                 => DBPipeEnv
                 -> MailboxRefKey s
                 -> HashRef
                 -> RetentionCause
                 -> Word64
                 -> Word64
                 -> m ()
observeRetention dbe mailbox message cause observed purgeAfter = withDB dbe do
  insert [qc|insert into retention
               (mailbox,message,cause,observed_at,purge_after,purged)
             values (?,?,?,?,?,0)
             on conflict (mailbox,message) do update set
               cause = case when excluded.cause = 'deleted'
                            then excluded.cause else retention.cause end,
               observed_at = min(retention.observed_at, excluded.observed_at),
               purge_after = min(retention.purge_after, excluded.purge_after)
          |] (mailbox, RetentionRef message, cause, observed, purgeAfter)

retentionRows :: forall s m . (ForMailbox s, MonadIO m)
              => DBPipeEnv -> m [RetentionRow s]
retentionRows dbe = withDB dbe do
  rows <- select @(MailboxRefKey s,RetentionRef,RetentionCause,Word64,Word64,Int)
    [qc|select mailbox,message,cause,observed_at,purge_after,purged
          from retention order by purge_after,mailbox,message|] ()
  pure
    [ RetentionRow mailbox message cause observed purgeAfter (purged /= 0)
    | (mailbox,RetentionRef message,cause,observed,purgeAfter,purged) <- rows
    ]

markRetentionPurged :: forall s m . (ForMailbox s, MonadIO m)
                    => DBPipeEnv -> MailboxRefKey s -> HashRef -> m ()
markRetentionPurged dbe mailbox message = withDB dbe do
  update [qc|update retention set purged = 1 where mailbox = ? and message = ?|]
         (mailbox,RetentionRef message)
