module TestStorageObservation
  ( testAvailabilityKeepsObserverOfflineUnknown
  , testAvailabilityUsesConservativeBound
  , testAvailabilityConvergesWithWindows
  , testAvailabilityBuildsTimeProfile
  , testAvailabilityRejectsAmbiguousWindows
  , testReachabilityTrackerRecordsAuthenticatedSuccess
  , testReachabilityTrackerRecordsMissedWindow
  , testReachabilityTrackerKeepsObserverGapUnknown
  , testReachabilityTrackerRejectsTimeRegression
  ) where

import HBS2.Storage.Observation
import HBS2.Storage.Placement

import Data.Maybe (isNothing)
import Data.Map.Strict qualified as Map
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

testReachabilityTrackerRecordsAuthenticatedSuccess :: IO ()
testReachabilityTrackerRecordsAuthenticatedSuccess = do
  let tracker = newReachabilityTracker 10
  (observed, first) <- requireTracking $
    observeReachability 10 "provider-a" tracker
  first @?= [("provider-a", window 10 WindowReachable)]
  requireTracking (observeReachability 10 "provider-a" observed)
    >>= (@?= []) . snd

testReachabilityTrackerRecordsMissedWindow :: IO ()
testReachabilityTrackerRecordsMissedWindow = do
  (observed, _) <- requireTracking $
    observeReachability 10 "provider-a" (newReachabilityTracker 10)
  (advanced, completed) <- requireTracking $
    advanceReachability 11 observed
  completed @?= []
  reachabilityTrackerProviders advanced
    @?= Map.fromList [("provider-a", False)]
  requireTracking (advanceReachability 12 advanced)
    >>= (@?= [("provider-a", window 11 WindowOffline)]) . snd

testReachabilityTrackerKeepsObserverGapUnknown :: IO ()
testReachabilityTrackerKeepsObserverGapUnknown = do
  (observedA, _) <- requireTracking $
    observeReachability 10 "provider-a" (newReachabilityTracker 10)
  (observedBoth, _) <- requireTracking $
    observeReachability 10 "provider-b" observedA
  (_, completed) <- requireTracking $
    advanceReachability 13 observedBoth
  completed @?=
    [ (provider, window index state)
    | let state = WindowObserverOffline
    , index <- [11, 12]
    , provider <- ["provider-a", "provider-b"]
    ]

testReachabilityTrackerRejectsTimeRegression :: IO ()
testReachabilityTrackerRejectsTimeRegression =
  advanceReachability 9 (newReachabilityTracker 10)
    @?= Left (ReachabilityWindowRegression 10 9)

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

requireTracking
  :: Show error
  => Either error value
  -> IO value
requireTracking = either (assertFailure . show) pure
