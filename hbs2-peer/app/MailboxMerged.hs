{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language QuasiQuotes #-}
{-# Language UndecidableInstances #-}
-- | Which entries this node has already merged into which mailbox.
--
-- A NEGATIVE INDEX, and nothing more: it answers "have we done this already"
-- so that the merge path does not re-verify a signature, re-read a policy and
-- rebuild a mailbox tree for an entry it already holds. Nothing outside this
-- node has an opinion about it, and nothing outside this node should be able to
-- write it.
--
-- WHY IT IS A TABLE AND NOT A BLOCK, which is the whole reason this module
-- exists. The answer used to be the PRESENCE OF A BLOCK at
-- @hashObject (serialise (MergedEntry mailbox entry))@. Every input to that
-- hash is public: the mailbox key is announced, and the entry hash is derived
-- from a message anybody can see on gossip. So the hash was computable by a
-- stranger, and the block store it lived in is content addressed and fetched on
-- demand -- which is to say, plantable:
--
--   1. compute the marker hash for a message you want suppressed;
--   2. send the host a @MailboxStatus@ for that mailbox naming the marker hash
--      as the mailbox tree root;
--   3. the host asks for it, you serve the seventy-five bytes that hash to it,
--      and @putBlock@ keeps them because the hash matches.
--
-- From that moment @hasBlock@ answers @Just@ and the message is refused at
-- ingress forever, at @debug@ level, INCLUDING when an honest co-host offers it
-- during replication. One packet and one served block bought permanent, silent,
-- targeted censorship of any letter to any hosted mailbox.
--
-- The general rule, because it outlives this one marker: the presence of a
-- block in a GLOBAL content-addressed store cannot carry a LOCAL decision,
-- because any path that fetches blocks on demand is a path by which a stranger
-- chooses what that store contains. State that means "we did this" belongs
-- somewhere only we write, and the mailbox's own database is that place.
--
-- WHAT IS LOST by moving, and it is only work: an existing installation's
-- markers are blocks, this reads rows, so after an upgrade every entry already
-- in a mailbox is processed once more. That costs a signature check and one
-- rebuild per mailbox, after which the rows exist and the early exit is back.
-- It cannot cost an entry: the merge unions the new set with what the tree
-- already holds, so re-adding is identity. The orphaned marker blocks stay in
-- the store as garbage, which is the pre-existing @$class: leak@ and not made
-- worse by this.
module MailboxMerged
  ( mergedDDL
  , isMerged
  , markMerged
  ) where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Peer.Proto.Mailbox.Ref (MailboxRefKey(..))

import DBPipe.SQLite

import Text.InterpolatedString.Perl6 (qc)

-- | A mailbox key as a database key.
--
-- Here rather than beside the worker's other instances, because this module and
-- the worker now both key rows by one, and a second spelling of "how a mailbox
-- key becomes a column" is the kind of duplicate rule that drifts and then
-- silently stops matching rows written by the other half.
instance ForMailbox s => ToField (MailboxRefKey s) where
  toField (MailboxRefKey a) = toField (show $ pretty (AsBase58 a))

instance ForMailbox s => FromField (MailboxRefKey s) where
  fromField w = fromField @String w <&> fromString @(MailboxRefKey s)

-- | An entry hash as a database key.
--
-- Its own newtype for the same reason 'PolicyHash' is one: 'HashRef' has no
-- field instances of its own, and giving it orphan ones here would decide the
-- encoding for every other table in the peer.
newtype MergedRef = MergedRef HashRef

instance ToField MergedRef where
  toField (MergedRef h) = toField (show $ pretty h)

-- | The table, to be created in the same transaction as the rest of the
-- mailbox schema.
--
-- Not a function that opens its own connection: the worker creates every table
-- BEFORE the database becomes visible to the protocol handlers, and a table
-- created a moment later is a table some inbound request finds missing.
mergedDDL :: MonadIO m => DBPipeM m ()
mergedDDL =
  ddl [qc|create table if not exists
           merged ( mailbox text not null
                  , entry   text not null
                  , primary key (mailbox, entry)
                  )
         |]

-- | Has this node already merged this entry into this mailbox?
isMerged :: forall s m . (ForMailbox s, MonadIO m)
         => DBPipeEnv
         -> MailboxRefKey s
         -> HashRef
         -> m Bool
isMerged dbe mbox entry = withDB dbe do
  select @(Only Int) [qc|select 1 from merged where mailbox = ? and entry = ? limit 1|]
                     (mbox, MergedRef entry)
    <&> not . null

-- | Record that it has.
--
-- Idempotent, because both writers can reach the same pair: an entry offered
-- twice between two polls, and a mailbox whose tree already held it.
markMerged :: forall s m . (ForMailbox s, MonadIO m)
           => DBPipeEnv
           -> MailboxRefKey s
           -> HashRef
           -> m ()
markMerged dbe mbox entry = withDB dbe do
  insert [qc|insert into merged (mailbox,entry) values (?,?)
             on conflict (mailbox, entry) do nothing
           |] (mbox, MergedRef entry)
