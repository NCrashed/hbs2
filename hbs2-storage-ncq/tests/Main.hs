module Main where

import TestStateRecovery

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
  testGroup "ncq3"
    [ testGroup "the state on disk keeps up with the data"
        [ testCase "a synced data file reaches a state file while running"
            testStateFollowsTheData
        ]
    , testGroup "a restart does not lose what was synced"
        [ testCase "after a crash, twice, and a sweep in between"
            testCrashKeepsSyncedBlocks
        , testCase "when the newest state is older than the data"
            testOldStateKeepsNewerData
        ]
    , testGroup "start-up deletes nothing it cannot account for"
        [ testCase "a torn tail is cut back to the last sync and kept"
            testTornTailIsKeptAside
        , testCase "a newer state that cannot be loaded is kept aside"
            testUnloadableStateIsKeptAside
        , testCase "a pending fact for a missing file does not stop start-up"
            testMissingPendingFileDoesNotStopStart
        ]
    ]
