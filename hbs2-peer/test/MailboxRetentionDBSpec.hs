{-# Language TypeApplications #-}
module MailboxRetentionDBSpec (tests) where

import HBS2.Prelude.Plated
import HBS2.Peer.Proto.Mailbox.Ref
import HBS2.Net.Auth.Schema (CryptoScheme(..))
import MailboxRetention
import MailboxMerged (mergedDDL,isMerged,markMerged)
import MailboxFixture (aPeer)

import DBPipe.SQLite
import Lens.Micro.Platform (view)
import HBS2.Net.Auth.Credentials (peerSignPk)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "mailbox retention journal"
  [ testCase "first observation fixes grace and deletion upgrades the reason" $
      withJournal $ \dbe -> do
        (creds,_) <- aPeer
        let mailbox = MailboxRefKey @HBS2Basic (view peerSignPk creds)
            message = fromString "11111111111111111111111111111111"
        observeRetention dbe mailbox message RetentionTTL 100 200
        observeRetention dbe mailbox message RetentionDeleted 150 250
        rows <- retentionRows @HBS2Basic dbe
        rows @?= [RetentionRow mailbox message RetentionDeleted 100 200 False]
        markRetentionPurged dbe mailbox message
        purged <- map retentionPurged <$> retentionRows @HBS2Basic dbe
        purged @?= [True]
  , testCase "a purged entry remains merged after restart, so an old peer cannot replay it" $
      withSystemTempDirectory "mailbox-retention-replay" $ \dir -> do
        (creds,_) <- aPeer
        let path = dir </> "state.db"
            mailbox = MailboxRefKey @HBS2Basic (view peerSignPk creds)
            message = fromString "11111111111111111111111111111111"
            entry = fromString "22222222222222222222222222222222"
        dbe0 <- newDBPipeEnv dbPipeOptsDef path
        withDB dbe0 (transactional (mergedDDL >> retentionDDL))
        markMerged dbe0 mailbox entry
        observeRetention dbe0 mailbox message RetentionTTL 100 200
        markRetentionPurged dbe0 mailbox message

        dbe1 <- newDBPipeEnv dbPipeOptsDef path
        withDB dbe1 (transactional (mergedDDL >> retentionDDL))
        isMerged dbe1 mailbox entry >>= (@?= True)
        rows <- retentionRows @HBS2Basic dbe1
        map retentionPurged rows @?= [True]
  ]

withJournal :: (DBPipeEnv -> IO a) -> IO a
withJournal action = withSystemTempDirectory "mailbox-retention" $ \dir -> do
  dbe <- newDBPipeEnv dbPipeOptsDef (dir </> "state.db")
  withDB dbe retentionDDL
  action dbe
