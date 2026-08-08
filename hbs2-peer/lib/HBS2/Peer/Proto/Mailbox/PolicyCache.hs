-- | The parsed policy for a mailbox, kept until the policy changes.
--
-- WHAT THIS IS FOR is not speed for its own sake. Reading a policy is a
-- @getBlock@, a signature check, a merkle-tree read, a text parse and a clause
-- parse -- and it sat on two paths a STRANGER drives:
--
--   * @CheckMailbox@, which any handshaken peer may send, forty bytes with any
--     key in the field. The mailbox-exists gate stands in front of it, so the
--     key has to name a mailbox this peer hosts; after that the work was paid
--     per packet.
--   * @MailboxStatus@, likewise, once the signature and the nonce echo pass.
--
-- Neither is rate-limited (the mailbox verbs run under @NoLimit@), and both run
-- on the deferred pool the other protocols share, so what an attacker bought
-- with a packet was a merkle read and a parse taken out of everybody's budget.
-- That is why this is not filed as a performance note: unmetered work a
-- stranger can ask for repeatedly is a denial-of-service surface, whatever the
-- constant factor is.
--
-- KEYED BY THE HASH, which is what makes it safe as well as small. The hash
-- comes from this peer's own @policy@ table, written by @mailboxSetPolicy@
-- after the payload has been checked. Content addressing does the rest: the
-- same hash is the same bytes is the same parse, so a cached answer cannot be
-- staler than the row it was read under, and a policy change invalidates the
-- entry by simply not matching it. There is no invalidation call to forget and
-- no window where a withdrawn policy is still being applied.
--
-- BOUNDED BY THE OPERATOR'S OWN DATA, and this is the property that matters:
-- one entry per mailbox, replaced in place when the hash moves. Not one per
-- hash -- a mailbox whose owner rewrites the policy hourly leaves one entry
-- behind, not a day's worth -- and not one per peer or per request, so nothing
-- a stranger sends can make the map grow. Only mailboxes this peer hosts AND
-- whose owner has written a policy ever get an entry.
--
-- The hash lookup itself stays on every request. It is one indexed select
-- against a local database, against everything above; and the row's presence
-- is what tells "the owner said nothing" from "the owner said something we
-- could not read", which is a distinction the callers need and a cache cannot
-- be allowed to blur.
module HBS2.Peer.Proto.Mailbox.PolicyCache
  ( PolicyCache
  , newPolicyCache
  , policyAt
  , PolicyMemory
  , cachedAt
  , rememberPolicy
  ) where

import HBS2.Prelude
import HBS2.Data.Types.Refs (HashRef)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import UnliftIO

-- | What has been parsed, by mailbox, with the hash it was parsed from.
--
-- A plain map with pure steps, so the rule can be asked questions without a
-- peer, a database or a storage.
type PolicyMemory k v = HashMap k (HashRef, v)

-- | The answer for this mailbox at this hash, if the cache holds one.
--
-- 'Nothing' when the mailbox is not there AND when it is there under a
-- different hash: an entry that does not match the hash is not a worse answer,
-- it is not an answer at all, and returning it would be applying a policy the
-- owner has replaced.
cachedAt :: (Eq k, Hashable k) => k -> HashRef -> PolicyMemory k v -> Maybe v
cachedAt k ha m = case HM.lookup k m of
  Just (h, v) | h == ha -> Just v
  _                     -> Nothing

-- | Record what was parsed, replacing whatever this mailbox had.
--
-- Replacing and not accumulating: keyed by the mailbox with the hash as a
-- stamp, so the map is as long as the list of mailboxes this peer hosts, and
-- rewriting a policy does not add to it.
rememberPolicy :: (Eq k, Hashable k)
               => k -> HashRef -> v -> PolicyMemory k v -> PolicyMemory k v
rememberPolicy k ha v = HM.insert k (ha, v)

newtype PolicyCache k v = PolicyCache (TVar (PolicyMemory k v))

newPolicyCache :: (MonadIO m, Hashable k) => m (PolicyCache k v)
newPolicyCache = PolicyCache <$> newTVarIO mempty

-- | The policy this hash names, parsing it at most once per (mailbox, hash).
--
-- The parse runs OUTSIDE the transaction, which it must: it reads storage.
-- So two requests arriving on a cold entry can both parse it, and both then
-- write the same value. That is a bounded waste -- by the size of the deferred
-- pool, not by what a stranger sends -- and the alternative, a lock held across
-- a merkle read, is a way for one slow read to stall every other mailbox.
policyAt :: (MonadIO m, Eq k, Hashable k)
         => PolicyCache k v -> k -> HashRef -> m v -> m v
policyAt (PolicyCache t) k ha parse =
  readTVarIO t <&> cachedAt k ha >>= \case
    Just v  -> pure v
    Nothing -> do
      v <- parse
      atomically $ modifyTVar' t (rememberPolicy k ha v)
      pure v
