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
  , testCase "an old peer cannot replay expired or deleted entries after reconnect" $
      withSystemTempDirectory "mailbox-retention-replay" $ \dir -> do
        (creds,_) <- aPeer
        let hostPath = dir </> "host.db"
            oldPeerPath = dir </> "old-peer.db"
            mailbox = MailboxRefKey @HBS2Basic (view peerSignPk creds)
            expiredMessage = fromString "11111111111111111111111111111111"
            deletedMessage = fromString "22222222222222222222222222222222"
            expiredEntry = fromString "33333333333333333333333333333333"
            deletedEntry = fromString "44444444444444444444444444444444"
            entries = [expiredEntry,deletedEntry]

        -- The disconnected peer still has the entries it will offer when it
        -- reconnects.  The host has already merged the same entry identities.
        oldPeer <- newDBPipeEnv dbPipeOptsDef oldPeerPath
        withDB oldPeer (transactional (mergedDDL >> retentionDDL))
        for_ entries (markMerged oldPeer mailbox)

        host0 <- newDBPipeEnv dbPipeOptsDef hostPath
        withDB host0 (transactional (mergedDDL >> retentionDDL))
        for_ entries (markMerged host0 mailbox)
        observeRetention host0 mailbox expiredMessage RetentionTTL 100 200
        observeRetention host0 mailbox deletedMessage RetentionDeleted 100 200
        markRetentionPurged host0 mailbox expiredMessage
        markRetentionPurged host0 mailbox deletedMessage

        -- Restart the host, then model the old peer offering both entries on
        -- reconnect.  The durable merge boundary refuses both before either
        -- can re-enter the compacted mailbox tree.
        host1 <- newDBPipeEnv dbPipeOptsDef hostPath
        withDB host1 (transactional (mergedDDL >> retentionDDL))
        for_ entries $ \entry -> do
          isMerged oldPeer mailbox entry >>= (@?= True)
          isMerged host1 mailbox entry >>= (@?= True)
        rows <- retentionRows @HBS2Basic host1
        map retentionCause rows @?= [RetentionTTL,RetentionDeleted]
        map retentionPurged rows @?= [True,True]
  ]

withJournal :: (DBPipeEnv -> IO a) -> IO a
withJournal action = withSystemTempDirectory "mailbox-retention" $ \dir -> do
  dbe <- newDBPipeEnv dbPipeOptsDef (dir </> "state.db")
  withDB dbe retentionDDL
  action dbe
