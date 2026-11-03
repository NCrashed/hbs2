module HBS2.Storage.Placement
  ( AvailabilityBps
  , mkAvailabilityBps
  , availabilityBps
  , AvailabilityProfile
  , mkAvailabilityProfile
  , availabilityProfileBuckets
  , maximumAvailabilityProfileBuckets
  , PlacementCandidate(..)
  , PlacementPolicy(..)
  , PlacementError(..)
  , selectPlacementCandidates
  ) where

import Data.List (foldl', sortBy)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word

-- | A conservative availability estimate in basis points.  Placement accepts
-- locally derived evidence, not an unverified provider claim.
newtype AvailabilityBps = AvailabilityBps Word16
  deriving stock (Eq, Ord, Show)

mkAvailabilityBps :: Word16 -> Maybe AvailabilityBps
mkAvailabilityBps value
  | value <= 10000 = Just (AvailabilityBps value)
  | otherwise = Nothing

availabilityBps :: AvailabilityBps -> Word16
availabilityBps (AvailabilityBps value) = value

-- | Maximum number of equal-duration windows retained by the generic policy.
-- 4096 accommodates a week of five-minute buckets while keeping local policy
-- inputs bounded.
maximumAvailabilityProfileBuckets :: Int
maximumAvailabilityProfileBuckets = 4096

-- | Availability lower confidence bounds for equal-duration time windows.
-- The observation layer decides the window size and tracking period; profiles
-- are comparable only when their widths match.
newtype AvailabilityProfile = AvailabilityProfile [AvailabilityBps]
  deriving stock (Eq, Ord, Show)

mkAvailabilityProfile :: [AvailabilityBps] -> Maybe AvailabilityProfile
mkAvailabilityProfile buckets
  | null buckets = Nothing
  | length buckets > maximumAvailabilityProfileBuckets = Nothing
  | otherwise = Just (AvailabilityProfile buckets)

availabilityProfileBuckets :: AvailabilityProfile -> [AvailabilityBps]
availabilityProfileBuckets (AvailabilityProfile buckets) = buckets

-- | Resource-neutral provider evidence used by local placement.  Failure
-- domains must already have been normalized by local policy.  An empty set is
-- unknown and therefore never contributes diversity.
data PlacementCandidate provider domain = PlacementCandidate
  { placementProvider          :: provider
  , placementFailureDomains    :: Set domain
  , placementAvailabilityLcb   :: AvailabilityBps
  , placementTimeProfile       :: AvailabilityProfile
  }
  deriving stock (Eq, Ord, Show)

data PlacementPolicy provider domain = PlacementPolicy
  { placementReplicaLimit          :: Int
  , placementMinimumAvailability   :: AvailabilityBps
  , placementExistingProviders     :: Set provider
  , placementExistingFailureDomains :: Set domain
  }
  deriving stock (Eq, Show)

data PlacementError
  = PlacementProfileWidthMismatch
  | PlacementDuplicateProvider
  deriving stock (Eq, Show)

-- | Select at most the requested number of new providers.  A wholly disjoint
-- known failure domain is preferred first.  Within that class, the selector
-- maximizes coverage in weak time windows.  Coverage uses a per-window maximum
-- rather than adding probabilities, so correlated schedules do not masquerade
-- as independent availability.
selectPlacementCandidates
  :: (Ord provider, Ord domain)
  => PlacementPolicy provider domain
  -> [AvailabilityProfile]
  -> [PlacementCandidate provider domain]
  -> Either PlacementError [PlacementCandidate provider domain]
selectPlacementCandidates policy existingProfiles candidates
  | not (profilesComparable allProfiles) = Left PlacementProfileWidthMismatch
  | hasDuplicateProviders candidates = Left PlacementDuplicateProvider
  | otherwise = Right (go replicaLimit initialDomains initialCoverage eligible)
  where
    replicaLimit = max 0 (placementReplicaLimit policy)
    initialDomains = placementExistingFailureDomains policy
    initialCoverage = combinedCoverage existingProfiles
    allProfiles = existingProfiles <> map placementTimeProfile candidates
    eligible =
      [ candidate
      | candidate <- candidates
      , placementProvider candidate `Set.notMember`
          placementExistingProviders policy
      , placementAvailabilityLcb candidate >=
          placementMinimumAvailability policy
      ]

    go remaining domains coverage available
      | remaining <= 0 = []
      | otherwise = case sortBy (compareCandidate domains coverage) available of
          [] -> []
          selected : rest ->
            selected : go
              (remaining - 1)
              (domains <> placementFailureDomains selected)
              (mergeCoverage coverage (placementTimeProfile selected))
              rest

profilesComparable :: [AvailabilityProfile] -> Bool
profilesComparable [] = True
profilesComparable (profile : profiles) =
  all ((== profileWidth profile) . profileWidth) profiles

hasDuplicateProviders
  :: Ord provider
  => [PlacementCandidate provider domain]
  -> Bool
hasDuplicateProviders = go Set.empty
  where
    go _ [] = False
    go seen (candidate : candidates)
      | placementProvider candidate `Set.member` seen = True
      | otherwise = go
          (Set.insert (placementProvider candidate) seen)
          candidates

profileWidth :: AvailabilityProfile -> Int
profileWidth = length . availabilityProfileBuckets

combinedCoverage :: [AvailabilityProfile] -> [Word16]
combinedCoverage [] = []
combinedCoverage (profile : profiles) =
  foldl' mergeCoverageWords (profileWords profile) profiles

mergeCoverage :: [Word16] -> AvailabilityProfile -> [Word16]
mergeCoverage [] = profileWords
mergeCoverage coverage = mergeCoverageWords coverage

mergeCoverageWords :: [Word16] -> AvailabilityProfile -> [Word16]
mergeCoverageWords coverage = zipWith max coverage . profileWords

profileWords :: AvailabilityProfile -> [Word16]
profileWords = map availabilityBps . availabilityProfileBuckets

compareCandidate
  :: (Ord provider, Ord domain)
  => Set domain
  -> [Word16]
  -> PlacementCandidate provider domain
  -> PlacementCandidate provider domain
  -> Ordering
compareCandidate domains coverage left right =
     compare (candidateScore domains coverage right)
             (candidateScore domains coverage left)
  <> compare (placementProvider left) (placementProvider right)

candidateScore
  :: Ord domain
  => Set domain
  -> [Word16]
  -> PlacementCandidate provider domain
  -> (Bool, Word64, AvailabilityBps, Word64)
candidateScore domains coverage candidate =
  ( isIndependentDomain domains (placementFailureDomains candidate)
  , marginalCoverage coverage (placementTimeProfile candidate)
  , placementAvailabilityLcb candidate
  , profileTotal (placementTimeProfile candidate)
  )

isIndependentDomain :: Ord domain => Set domain -> Set domain -> Bool
isIndependentDomain existing candidate =
  not (Set.null candidate) && Set.null (Set.intersection existing candidate)

marginalCoverage :: [Word16] -> AvailabilityProfile -> Word64
marginalCoverage [] profile = profileTotal profile
marginalCoverage coverage profile = sum $ zipWith improvement coverage (profileWords profile)
  where
    improvement current candidate
      | candidate > current = fromIntegral (candidate - current)
      | otherwise = 0

profileTotal :: AvailabilityProfile -> Word64
profileTotal = sum . map (fromIntegral . availabilityBps) . availabilityProfileBuckets
