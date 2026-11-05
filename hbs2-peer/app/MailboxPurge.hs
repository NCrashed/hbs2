module MailboxPurge
  ( mailboxObjectClosure
  , mailboxObjectOrder
  , deleteMailboxBlocks
  ) where

import HBS2.Prelude.Plated
import HBS2.Data.Detect
import HBS2.Data.Types.Refs
import HBS2.Storage

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.IORef
import Control.Exception (displayException)
import UnliftIO (tryAny)

-- | Physical closure of semantic mailbox roots.  Callers deliberately do not
-- pass message-envelope blocks here: those are opaque CBOR, not Merkle roots.
mailboxObjectClosure :: AnyStorage -> Set HashRef -> IO (Set HashRef)
mailboxObjectClosure storage roots = Set.fromList <$> mailboxObjectOrder storage roots

-- | Descendants before their parents.  Keeping this order during deletion
-- makes a failed purge retryable: no Merkle root is removed while one of the
-- blocks needed to rediscover its remaining descendants failed to delete.
mailboxObjectOrder :: AnyStorage -> Set HashRef -> IO [HashRef]
mailboxObjectOrder storage roots = do
  seen <- newIORef mempty
  ordered <- newIORef []
  for_ roots $ \root ->
    deepScan ScanDeep (const (pure ())) (fromHashRef root)
      (getBlock storage) $ \block ->
        readIORef seen >>= \known -> unless (Set.member (HashRef block) known) do
          modifyIORef' seen (Set.insert (HashRef block))
          modifyIORef' ordered (HashRef block :)
  readIORef ordered

-- | Delete in the supplied child-before-parent order and stop at the first
-- failure.  Continuing could remove the only root that lets the next run
-- rediscover the failed descendant.
deleteMailboxBlocks :: AnyStorage -> [HashRef] -> IO (Int,[Text])
deleteMailboxBlocks storage = go 0
  where
    go deleted = \case
      [] -> pure (deleted,[])
      block : rest -> tryAny (delBlock storage (fromHashRef block)) >>= \case
        Right () -> go (deleted + 1) rest
        Left problem -> pure
          (deleted,[Text.pack (show block <> ": " <> displayException problem)])
