module TestStorageObservation
  ( testAvailabilityKeepsObserverOfflineUnknown
  , testAvailabilityUsesConservativeBound
  , testAvailabilityConvergesWithWindows
  , testAvailabilityBuildsTimeProfile
  , testAvailabilityRejectsAmbiguousWindows
  ) where

import HBS2.Storage.Observation
import HBS2.Storage.Placement

import Data.Maybe (isNothing)
import Data.Word

import Test.Tasty.HUnit

testAvailabilityKeepsObserverOfflineUnknown :: IO ()
testAvailabilityKeepsObserverOfflineUnknown = do
  evidence <- requireSummary
    [ window 0 WindowObserverOffline
    , window 1 WindowObserverOffline
    ]
  availabilityKnownWindows evidence @?= 0
  availabilityReachableWindows evidence @?= 0
  availabilityUnknownWindows evidence @?= 2
  assertBool "no observation is not zero availability" $
    isNothing (availabilityLowerBound evidence)

testAvailabilityUsesConservativeBound :: IO ()
testAvailabilityUsesConservativeBound = do
  evidence <- requireSummary [window 0 WindowReachable]
  availabilityKnownWindows evidence @?= 1
  availabilityReachableWindows evidence @?= 1
  availabilityLowerBound evidence @?= mkAvailabilityBps 2065

testAvailabilityConvergesWithWindows :: IO ()
testAvailabilityConvergesWithWindows = do
  evidence <- requireSummary $
    replicateWindows 0 5000 WindowReachable
      <> replicateWindows 5000 5000 WindowOffline
      <> replicateWindows 10000 100 WindowObserverOffline
  availabilityKnownWindows evidence @?= 10000
  availabilityReachableWindows evidence @?= 5000
  availabilityUnknownWindows evidence @?= 100
  availabilityLowerBound evidence @?= mkAvailabilityBps 4902

testAvailabilityBuildsTimeProfile :: IO ()
testAvailabilityBuildsTimeProfile = do
  profile <- either (assertFailure . show) pure $
    summarizeReachabilityProfile 2
      [ window 0 WindowReachable
      , window 1 WindowOffline
      , window 2 WindowReachable
      , window 3 WindowOffline
      ]
  map availabilityKnownWindows profile @?= [2, 2]
  map availabilityReachableWindows profile @?= [2, 0]
  map availabilityLowerBound profile
    @?= [mkAvailabilityBps 3423, mkAvailabilityBps 0]

testAvailabilityRejectsAmbiguousWindows :: IO ()
testAvailabilityRejectsAmbiguousWindows = do
  summarizeReachabilityProfile 0 []
    @?= Left AvailabilityProfileBucketCountInvalid
  summarizeReachabilityProfile
    (maximumAvailabilityProfileBuckets + 1)
    []
    @?= Left AvailabilityProfileBucketCountInvalid
  summarizeReachabilityProfile 2
    [window 7 WindowReachable, window 7 WindowOffline]
    @?= Left AvailabilityDuplicateWindow
  summarizeReachability
    [window 7 WindowReachable, window 7 WindowOffline]
    @?= Left AvailabilityDuplicateWindow

window :: Word64 -> ReachabilityWindowState -> ReachabilityWindow
window index state = ReachabilityWindow
  { reachabilityWindowIndex = index
  , reachabilityWindowState = state
  }

replicateWindows
  :: Word64
  -> Int
  -> ReachabilityWindowState
  -> [ReachabilityWindow]
replicateWindows first count state =
  [window index state | index <- take count [first ..]]

requireSummary :: [ReachabilityWindow] -> IO AvailabilityEvidence
requireSummary = either (assertFailure . show) pure . summarizeReachability
