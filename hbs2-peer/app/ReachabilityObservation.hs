module ReachabilityObservation
  ( ReachabilityObserver
  , ReachabilityObserverError(..)
  , newReachabilityObserver
  , recordReachabilitySuccess
  , advanceReachabilityObserver
  ) where

import HBS2.Storage.Observation

import Control.Concurrent.MVar
import Data.Word

newtype ReachabilityObserver provider = ReachabilityObserver
  (MVar (ReachabilityTracker provider))

data ReachabilityObserverError persistenceError
  = ReachabilityObserverTrackingError ReachabilityTrackingError
  | ReachabilityObserverPersistenceError persistenceError
  deriving stock (Eq, Show)

newReachabilityObserver :: Word64 -> IO (ReachabilityObserver provider)
newReachabilityObserver windowIndex =
  ReachabilityObserver <$> newMVar (newReachabilityTracker windowIndex)

recordReachabilitySuccess
  :: Ord provider
  => ReachabilityObserver provider
  -> Word64
  -> provider
  -> ([(provider, ReachabilityWindow)] -> IO (Either persistenceError ()))
  -> IO (Either (ReachabilityObserverError persistenceError) ())
recordReachabilitySuccess observer windowIndex provider =
  transition observer (observeReachability windowIndex provider)

advanceReachabilityObserver
  :: Ord provider
  => ReachabilityObserver provider
  -> Word64
  -> ([(provider, ReachabilityWindow)] -> IO (Either persistenceError ()))
  -> IO (Either (ReachabilityObserverError persistenceError) ())
advanceReachabilityObserver observer windowIndex =
  transition observer (advanceReachability windowIndex)

transition
  :: ReachabilityObserver provider
  -> (ReachabilityTracker provider
      -> Either ReachabilityTrackingError
           (ReachabilityTracker provider, evidence))
  -> (evidence -> IO (Either persistenceError ()))
  -> IO (Either (ReachabilityObserverError persistenceError) ())
transition (ReachabilityObserver state) step persist = modifyMVar state \current ->
  case step current of
    Left problem -> pure
      (current, Left (ReachabilityObserverTrackingError problem))
    Right (next, evidence) -> do
      persisted <- persist evidence
      case persisted of
        Left problem -> pure
          (current, Left (ReachabilityObserverPersistenceError problem))
        Right () -> pure (next, Right ())
