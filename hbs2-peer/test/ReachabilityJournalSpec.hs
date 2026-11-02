module ReachabilityJournalSpec (tests) where

import HBS2.Net.Auth.Credentials
import HBS2.Peer.Brains
import HBS2.Storage.Observation

import Control.Concurrent.Async (withAsync)
import Lens.Micro.Platform (view)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Brains qualified as DB
import PeerConfig

tests :: TestTree
tests = testGroup "reachability journal"
  [ testCase "persists bounded windows across restart" $
      withJournal $ \brains provider reopen -> do
        let policy = ReachabilityJournalPolicy 3
        mapM_ (expectStored brains policy provider)
          [ ReachabilityWindow 10 WindowReachable
          , ReachabilityWindow 12 WindowOffline
          , ReachabilityWindow 11 WindowObserverOffline
          , ReachabilityWindow 9 WindowReachable
          ]
        reopened <- reopen
        listReachabilityWindows @L4Proto reopened provider >>= (@?=
          [ ReachabilityWindow 10 WindowReachable
          , ReachabilityWindow 11 WindowObserverOffline
          , ReachabilityWindow 12 WindowOffline
          ])

  , testCase "accepts an identical duplicate without changing evidence" $
      withJournal $ \brains provider _ -> do
        let policy = ReachabilityJournalPolicy 4
            window = ReachabilityWindow 7 WindowReachable
        expectStored brains policy provider window
        expectStored brains policy provider window
        listReachabilityWindows @L4Proto brains provider >>= (@?= [window])

  , testCase "rejects conflicting evidence for a completed window" $
      withJournal $ \brains provider _ -> do
        let policy = ReachabilityJournalPolicy 4
        expectStored brains policy provider (ReachabilityWindow 7 WindowReachable)
        putReachabilityWindow @L4Proto brains policy provider
          (ReachabilityWindow 7 WindowOffline)
          >>= (@?= Left ReachabilityWindowConflict)
        listReachabilityWindows @L4Proto brains provider >>=
          (@?= [ReachabilityWindow 7 WindowReachable])

  , testCase "refuses a zero-sized retention policy" $
      withJournal $ \brains provider _ ->
        putReachabilityWindow @L4Proto brains (ReachabilityJournalPolicy 0) provider
          (ReachabilityWindow 1 WindowReachable)
          >>= (@?= Left ReachabilityRetentionInvalid)

  , testCase "the no-storage adapter preserves policy validation" $ do
      credentials <- newCredentials @'HBS2Basic
      let provider = view peerSignPk credentials
          window = ReachabilityWindow 1 WindowReachable
      putReachabilityWindow @L4Proto () (ReachabilityJournalPolicy 0) provider window
        >>= (@?= Left ReachabilityRetentionInvalid)
      putReachabilityWindow @L4Proto () (ReachabilityJournalPolicy 1) provider window
        >>= (@?= Right ())
  ]

expectStored
  :: DB.BasicBrains L4Proto
  -> ReachabilityJournalPolicy
  -> PubKey 'Sign 'HBS2Basic
  -> ReachabilityWindow
  -> IO ()
expectStored brains policy provider window =
  putReachabilityWindow @L4Proto brains policy provider window >>= (@?= Right ())

withJournal
  :: ( DB.BasicBrains L4Proto
    -> PubKey 'Sign 'HBS2Basic
    -> IO (DB.BasicBrains L4Proto)
    -> IO a
     )
  -> IO a
withJournal action =
  withSystemTempDirectory "hbs2-reachability-journal" $ \directory -> do
    credentials <- newCredentials @'HBS2Basic
    let provider = view peerSignPk credentials
        stateFile = directory </> "state.db"
        source = "brains-db \"" <> stateFile <> "\""
    forms <- either (assertFailure . show) pure (parseTop source)
    let config = PeerConfig forms
    brains <- DB.newBasicBrains @L4Proto config
    withAsync (DB.runBasicBrains config brains) $ \_ ->
      action brains provider (DB.newBasicBrains @L4Proto config)
