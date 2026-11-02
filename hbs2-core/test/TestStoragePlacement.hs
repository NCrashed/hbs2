module TestStoragePlacement
  ( testAvailabilityProfileValidation
  , testPlacementRejectsMismatchedProfiles
  , testPlacementRejectsDuplicateProviders
  , testPlacementFiltersIneligibleProviders
  , testPlacementPrefersIndependentFailureDomain
  , testPlacementPrefersComplementarySchedule
  , testPlacementRecomputesCoverage
  ) where

import HBS2.Storage.Placement

import Data.Set qualified as Set
import Data.Maybe (fromMaybe, isJust)
import Data.Word

import Test.Tasty.HUnit

testAvailabilityProfileValidation :: IO ()
testAvailabilityProfileValidation = do
  mkAvailabilityBps 0 @?= Just (requireAvailability 0)
  mkAvailabilityBps 10000 @?= Just (requireAvailability 10000)
  mkAvailabilityBps 10001 @?= Nothing
  mkAvailabilityProfile [] @?= Nothing
  mkAvailabilityProfile
    (replicate (maximumAvailabilityProfileBuckets + 1) (requireAvailability 1))
    @?= Nothing
  assertBool "a non-empty profile is accepted" $
    isJust (profile [1000, 9000])

testPlacementRejectsMismatchedProfiles :: IO ()
testPlacementRejectsMismatchedProfiles =
  selectPlacementCandidates policy []
    [ candidate "short" ["site-a"] 9000 [9000]
    , candidate "long" ["site-b"] 9000 [9000, 9000]
    ]
    @?= Left PlacementProfileWidthMismatch
  where
    policy = PlacementPolicy
      { placementReplicaLimit = 2
      , placementMinimumAvailability = requireAvailability 5000
      , placementExistingProviders = Set.empty
      , placementExistingFailureDomains = Set.empty
      }

testPlacementRejectsDuplicateProviders :: IO ()
testPlacementRejectsDuplicateProviders =
  selectPlacementCandidates policy []
    [ candidate "same" ["site-a"] 9000 [9000]
    , candidate "same" ["site-b"] 8000 [8000]
    ]
    @?= Left PlacementDuplicateProvider
  where
    policy = PlacementPolicy
      { placementReplicaLimit = 1
      , placementMinimumAvailability = requireAvailability 5000
      , placementExistingProviders = Set.empty
      , placementExistingFailureDomains = Set.empty
      }

testPlacementFiltersIneligibleProviders :: IO ()
testPlacementFiltersIneligibleProviders = do
  selected <- requireSelection policy []
    [ candidate "existing" ["site-a"] 9900 [9900, 9900]
    , candidate "weak" ["site-b"] 4999 [9900, 9900]
    , candidate "eligible" ["site-c"] 5000 [5000, 5000]
    ]
  map placementProvider selected @?= ["eligible"]
  where
    policy = PlacementPolicy
      { placementReplicaLimit = 3
      , placementMinimumAvailability = requireAvailability 5000
      , placementExistingProviders = Set.singleton "existing"
      , placementExistingFailureDomains = Set.singleton "site-a"
      }

testPlacementPrefersIndependentFailureDomain :: IO ()
testPlacementPrefersIndependentFailureDomain = do
  selected <- requireSelection policy [profile' [2000, 8000]]
    [ candidate "same-site" ["site-a"] 9500 [10000, 10000]
    , candidate "other-site" ["site-b"] 8000 [8000, 8000]
    ]
  map placementProvider selected @?= ["other-site"]
  where
    policy = PlacementPolicy
      { placementReplicaLimit = 1
      , placementMinimumAvailability = requireAvailability 5000
      , placementExistingProviders = Set.singleton "incumbent"
      , placementExistingFailureDomains = Set.singleton "site-a"
      }

testPlacementPrefersComplementarySchedule :: IO ()
testPlacementPrefersComplementarySchedule = do
  selected <- requireSelection policy [profile' [9000, 1000]]
    [ candidate "correlated" [] 9000 [9000, 1000]
    , candidate "complementary" [] 9000 [1000, 9000]
    ]
  map placementProvider selected @?= ["complementary"]
  where
    policy = PlacementPolicy
      { placementReplicaLimit = 1
      , placementMinimumAvailability = requireAvailability 5000
      , placementExistingProviders = Set.singleton "incumbent"
      , placementExistingFailureDomains = Set.empty
      }

testPlacementRecomputesCoverage :: IO ()
testPlacementRecomputesCoverage = do
  selected <- requireSelection policy []
    [ candidate "b-correlated" [] 9000 [9000, 1000]
    , candidate "c-complementary" [] 9000 [1000, 9000]
    , candidate "a-first" [] 9000 [9000, 1000]
    ]
  map placementProvider selected @?= ["a-first", "c-complementary"]
  where
    policy = PlacementPolicy
      { placementReplicaLimit = 2
      , placementMinimumAvailability = requireAvailability 5000
      , placementExistingProviders = Set.empty
      , placementExistingFailureDomains = Set.empty
      }

candidate
  :: String
  -> [String]
  -> Word16
  -> [Word16]
  -> PlacementCandidate String String
candidate provider domains availability windows = PlacementCandidate
  { placementProvider = provider
  , placementFailureDomains = Set.fromList domains
  , placementAvailabilityLcb = requireAvailability availability
  , placementTimeProfile = profile' windows
  }

profile :: [Word16] -> Maybe AvailabilityProfile
profile = mkAvailabilityProfile . map requireAvailability

profile' :: [Word16] -> AvailabilityProfile
profile' = fromMaybe (error "invalid test profile") . profile

requireAvailability :: Word16 -> AvailabilityBps
requireAvailability value =
  fromMaybe (error "invalid test availability") (mkAvailabilityBps value)

requireSelection
  :: (Ord provider, Ord domain, Show provider)
  => PlacementPolicy provider domain
  -> [AvailabilityProfile]
  -> [PlacementCandidate provider domain]
  -> IO [PlacementCandidate provider domain]
requireSelection policy existing =
  either (assertFailure . show) pure
    . selectPlacementCandidates policy existing
