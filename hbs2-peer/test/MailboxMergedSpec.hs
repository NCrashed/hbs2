-- | The record of what this node has already merged, against a real database.
--
-- Three of these tests are the ones "MailboxEntry" used to hold about the old
-- block marker: the same message in two mailboxes is two separate facts, two
-- messages in one mailbox likewise, and asking twice answers the same. They are
-- restated here because the mechanism changed and the facts did not.
--
-- The fourth is the one that made it move, and it is the reason this module
-- exists rather than a set of pure derivations: NOTHING A STRANGER CAN DO
-- MAKES AN UNMERGED ENTRY LOOK MERGED. The old answer was the presence of a
-- block whose hash a stranger could compute and whose bytes a stranger could
-- serve, so the answer was writable by whoever wanted a message suppressed.
-- A row in this database is not, and "no row until markMerged wrote one" is
-- what that reduces to for a test.
module MailboxMergedSpec (tests) where

import MailboxMerged

import HBS2.Hash
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Net.Auth.Schema (CryptoScheme(..))
import HBS2.Net.Proto.Types (CryptoAction(..),PubKey)
import HBS2.Peer.Proto.Mailbox.Ref (MailboxRefKey(..))
import HBS2.Prelude.Plated (fromStringMay)

import DBPipe.SQLite

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Tasty
import Test.Tasty.HUnit

type S = 'HBS2Basic

mbox :: MailboxRefKey S
mbox = MailboxRefKey key
  where
    key = fromMaybe (error "bad fixture key")
            (fromStringMay @(PubKey 'Sign S) "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP")

other :: MailboxRefKey S
other = MailboxRefKey key
  where
    key = fromMaybe (error "bad fixture key")
            (fromStringMay @(PubKey 'Sign S) "EJgvBg9bL2yKXk3GvZaYJgqpHy5kvpXdtEnAgoi4B5DN")

mh :: ByteString -> HashRef
mh = HashRef . hashObject

-- | A database with the mailbox schema in it, and nothing else.
--
-- The schema is created by the same 'mergedDDL' the peer runs, so a table this
-- test can write and the peer cannot (or the other way round) is a failure here
-- rather than a surprise in a deployment.
withDb :: (DBPipeEnv -> IO a) -> IO a
withDb action = withSystemTempDirectory "hbs2-merged" $ \dir -> do
  dbe <- newDBPipeEnv dbPipeOptsDef (dir </> "state.db")
  withDB dbe (transactional mergedDDL)
  action dbe

tests :: TestTree
tests = testGroup "mailbox: what this node has already merged"
  [ testCase "an entry nobody merged is not merged" $ withDb $ \dbe -> do
      -- The property the whole move is for. There is no value a stranger can
      -- put anywhere -- a block, a tree, a message -- that answers this
      -- question, because the only thing that answers it is a row this node
      -- wrote.
      isMerged dbe mbox (existsEntry' "a") >>= (@?= False)

  , testCase "an entry this node merged is" $ withDb $ \dbe -> do
      markMerged dbe mbox (existsEntry' "a")
      isMerged dbe mbox (existsEntry' "a") >>= (@?= True)

  , testCase "the same message in two mailboxes is two facts" $ withDb $ \dbe -> do
      -- Merging it into one must not make the other look done, which is what
      -- the mailbox key in the primary key is for.
      markMerged dbe mbox (existsEntry' "a")
      isMerged dbe mbox  (existsEntry' "a") >>= (@?= True)
      isMerged dbe other (existsEntry' "a") >>= (@?= False)

  , testCase "two messages in one mailbox are two facts" $ withDb $ \dbe -> do
      markMerged dbe mbox (existsEntry' "a")
      isMerged dbe mbox (existsEntry' "a") >>= (@?= True)
      isMerged dbe mbox (existsEntry' "b") >>= (@?= False)

  , testCase "marking twice is marking once" $ withDb $ \dbe -> do
      -- Both writers can reach the same pair: an entry offered twice between
      -- two polls, and a mailbox whose tree already held it. Without the
      -- conflict clause the second write is a primary-key violation, which in
      -- the merge loop is an exception in the middle of a batch.
      markMerged dbe mbox (existsEntry' "a")
      markMerged dbe mbox (existsEntry' "a")
      isMerged dbe mbox (existsEntry' "a") >>= (@?= True)

  , testCase "it survives a reopen" $ withSystemTempDirectory "hbs2-merged" $ \dir -> do
      -- It is a cache, but not a per-process one: a peer that restarts and
      -- forgets everything it merged re-verifies and rebuilds every mailbox it
      -- holds, which is the cost the marker exists to avoid.
      let path = dir </> "state.db"
      dbe0 <- newDBPipeEnv dbPipeOptsDef path
      withDB dbe0 (transactional mergedDDL)
      markMerged dbe0 mbox (existsEntry' "a")

      dbe1 <- newDBPipeEnv dbPipeOptsDef path
      withDB dbe1 (transactional mergedDDL)
      isMerged dbe1 mbox (existsEntry' "a") >>= (@?= True)
  ]
  where
    -- Named like the entry hash the peer actually stores, so that a reader of
    -- this file is not left wondering whether the key is a message or an entry.
    existsEntry' = mh
