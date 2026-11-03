module ReachabilityObservationSpec (tests) where

import PeerConfig
import ReachabilityObservation

import HBS2.Storage.Observation

import Data.IORef

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "reachability observation policy"
  [ testCase "uses an explicit window and retention horizon" $ do
      forms <- either (assertFailure . show) pure $ parseTop $ unlines
        [ "barter-observation-window-seconds 60"
        , "barter-observation-retention-windows 1440"
        ]
      reachabilityObservationPolicy (PeerConfig forms)
        @?= ReachabilityObservationPolicy 60 1440

  , testCase "keeps safe defaults for absent or invalid bounds" $ do
      reachabilityObservationPolicy mempty
        @?= ReachabilityObservationPolicy
          defaultReachabilityObservationWindowSeconds
          defaultReachabilityObservationRetentionWindows
      forms <- either (assertFailure . show) pure $ parseTop $ unlines
        [ "barter-observation-window-seconds 0"
        , "barter-observation-retention-windows -1"
        ]
      reachabilityObservationPolicy (PeerConfig forms)
        @?= ReachabilityObservationPolicy
          defaultReachabilityObservationWindowSeconds
          defaultReachabilityObservationRetentionWindows

  , testCase "retries identical evidence after persistence failure" $ do
      observer <- newReachabilityObserver 10
      writes <- newIORef ([] :: [[(String, ReachabilityWindow)]])
      let persist result evidence = do
            modifyIORef' writes (<> [evidence])
            pure result
      recordReachabilitySuccess observer 10 "provider"
        (persist (Left "storage unavailable"))
        >>= (@?= Left
          (ReachabilityObserverPersistenceError "storage unavailable"))
      recordReachabilitySuccess observer 10 "provider"
        (persist (Right ()))
        >>= (@?= Right ())
      readIORef writes >>= (@?=
        [ [("provider", ReachabilityWindow 10 WindowReachable)]
        , [("provider", ReachabilityWindow 10 WindowReachable)]
        ])
  ]
