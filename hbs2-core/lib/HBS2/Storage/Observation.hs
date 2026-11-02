module HBS2.Storage.Observation
  ( ReachabilityWindowState(..)
  , ReachabilityWindow(..)
  , AvailabilityEvidence(..)
  , AvailabilityObservationError(..)
  , summarizeReachability
  , summarizeReachabilityProfile
  ) where

import HBS2.Storage.Placement

import Data.IntMap.Strict qualified as IntMap
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Word

-- | Reachability in one completed, equal-duration observation window.
-- 'WindowObserverOffline' is deliberately not provider failure.
data ReachabilityWindowState
  = WindowReachable
  | WindowOffline
  | WindowObserverOffline
  deriving stock (Eq, Ord, Show)

data ReachabilityWindow = ReachabilityWindow
  { reachabilityWindowIndex :: Word64
  , reachabilityWindowState :: ReachabilityWindowState
  }
  deriving stock (Eq, Ord, Show)

-- | Rolling evidence retains its denominator and unknown count so callers do
-- not confuse absence of observation with measured unavailability.
data AvailabilityEvidence = AvailabilityEvidence
  { availabilityKnownWindows     :: Word64
  , availabilityReachableWindows :: Word64
  , availabilityUnknownWindows   :: Word64
  , availabilityLowerBound       :: Maybe AvailabilityBps
  }
  deriving stock (Eq, Show)

data AvailabilityObservationError
  = AvailabilityProfileBucketCountInvalid
  | AvailabilityDuplicateWindow
  deriving stock (Eq, Show)

-- | Summarize one rolling period.  The 95% Wilson lower bound gives a new
-- provider a conservative score without inventing failures for windows in
-- which the observer itself was offline.
summarizeReachability
  :: [ReachabilityWindow]
  -> Either AvailabilityObservationError AvailabilityEvidence
summarizeReachability windows = do
  validateDistinctWindows windows
  pure (summarizeStates (map reachabilityWindowState windows))

-- | Group absolute window numbers into a recurring time profile.  Absolute
-- indices preserve the phase of a weekly/daily profile across rolling-period
-- truncation; the caller chooses the duration represented by one bucket.
summarizeReachabilityProfile
  :: Int
  -> [ReachabilityWindow]
  -> Either AvailabilityObservationError [AvailabilityEvidence]
summarizeReachabilityProfile bucketCount windows
  | bucketCount <= 0 = Left AvailabilityProfileBucketCountInvalid
  | bucketCount > maximumAvailabilityProfileBuckets =
      Left AvailabilityProfileBucketCountInvalid
  | otherwise = do
      validateDistinctWindows windows
      pure
        [ summarizeStates (IntMap.findWithDefault [] bucket grouped)
        | bucket <- [0 .. bucketCount - 1]
        ]
  where
    grouped = IntMap.fromListWith (<>)
      [ (fromIntegral (reachabilityWindowIndex entry `mod` fromIntegral bucketCount)
        , [reachabilityWindowState entry]
        )
      | entry <- windows
      ]

validateDistinctWindows
  :: [ReachabilityWindow]
  -> Either AvailabilityObservationError ()
validateDistinctWindows = go Set.empty
  where
    go _ [] = Right ()
    go seen (entry : entries)
      | reachabilityWindowIndex entry `Set.member` seen =
          Left AvailabilityDuplicateWindow
      | otherwise = go
          (Set.insert (reachabilityWindowIndex entry) seen)
          entries

summarizeStates :: [ReachabilityWindowState] -> AvailabilityEvidence
summarizeStates states = AvailabilityEvidence
  { availabilityKnownWindows = known
  , availabilityReachableWindows = reachable
  , availabilityUnknownWindows = unknown
  , availabilityLowerBound = wilsonLowerBound reachable known
  }
  where
    (known, reachable, unknown) = foldl' countState (0, 0, 0) states
    countState (knownCount, reachableCount, unknownCount) = \case
      WindowReachable -> (knownCount + 1, reachableCount + 1, unknownCount)
      WindowOffline -> (knownCount + 1, reachableCount, unknownCount)
      WindowObserverOffline -> (knownCount, reachableCount, unknownCount + 1)

wilsonLowerBound :: Word64 -> Word64 -> Maybe AvailabilityBps
wilsonLowerBound _ 0 = Nothing
wilsonLowerBound successes samples =
  Just . fromMaybe (error "Wilson availability is outside basis-point range") $
    mkAvailabilityBps (fromIntegral basisPoints)
  where
    sampleCount = fromIntegral samples :: Double
    proportion = fromIntegral successes / sampleCount
    z = 1.96 :: Double
    zSquared = z * z
    centre = proportion + zSquared / (2 * sampleCount)
    radius = z * sqrt
      ((proportion * (1 - proportion) + zSquared / (4 * sampleCount))
        / sampleCount)
    lowerBound = max 0 ((centre - radius) / (1 + zSquared / sampleCount))
    basisPoints = floor (lowerBound * 10000) :: Int
