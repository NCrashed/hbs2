module HBS2.Peer.CompleteObject
  ( CompleteObjectError(..)
  , inspectCompleteObject
  ) where

import HBS2.Prelude.Plated
import HBS2.Data.Detect (extractBlockRefs)
import HBS2.Data.Types.Refs (HashRef(..), fromHashRef)
import HBS2.Storage (AnyStorage, getBlock)
import HBS2.Storage.Operations.Missed (findMissedBlocks)

import Codec.Serialise (Serialise)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet

-- | A logical object could not be used as a whole.  The descendants are the
-- physical hashes which are absent from the local store.
data CompleteObjectError = CompleteObjectIncomplete
  { incompleteRoot :: HashRef
  , incompleteDescendants :: [HashRef]
  }
  deriving stock (Eq, Generic, Show)

instance Serialise CompleteObjectError

-- | Resolve all physical blocks reachable from a logical root.
--
-- The result is useful to callers which must advertise the complete object,
-- rather than only its root.  An incomplete graph is reported before any
-- caller-visible action, so an announce cannot claim success for half an
-- object.
inspectCompleteObject :: MonadIO m
                      => AnyStorage
                      -> HashRef
                      -> m (Either CompleteObjectError [HashRef])
inspectCompleteObject storage root = do
  missing <- stableNub <$> findMissedBlocks storage root
  if null missing
    then Right <$> reachable
    else pure (Left (CompleteObjectIncomplete root missing))
  where
    reachable = walk mempty [] [root]

    walk _ found [] = pure (reverse found)
    walk seen found (ref : pending)
      | HashSet.member ref seen = walk seen found pending
      | otherwise = do
          block <- getBlock storage (fromHashRef ref)
          let refs = maybe [] (extractBlockRefs (fromHashRef ref)) block
          walk (HashSet.insert ref seen) (ref : found) (refs <> pending)

stableNub :: [HashRef] -> [HashRef]
stableNub = reverse . snd . foldl step (mempty, [])
  where
    step :: (HashSet HashRef, [HashRef]) -> HashRef -> (HashSet HashRef, [HashRef])
    step (seen, refs) ref
      | HashSet.member ref seen = (seen, refs)
      | otherwise = (HashSet.insert ref seen, ref : refs)
