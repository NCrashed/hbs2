module HBS2.Storage.Operations.GroupKey
  ( GroupKeyLoadError(..)
  , loadGroupKeyChecked
  ) where

import HBS2.Data.Types.Refs
import HBS2.Net.Auth.GroupKeySymm
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Storage.Operations.ByteString
import HBS2.Storage.Operations.Missed

import Codec.Serialise
import Control.Exception
import Control.Monad.Except
import Data.HashSet qualified as HS
import Data.List qualified as List

data GroupKeyLoadError
  = GroupKeyObjectIncomplete HashRef [HashRef]
  | GroupKeyObjectInvalid HashRef
  deriving stock (Eq, Typeable)

instance Show GroupKeyLoadError where
  show = \case
    GroupKeyObjectIncomplete root missing ->
      "group-key object " <> hashText root <> " is incomplete; missing physical block(s): "
        <> unwords (hashText <$> missing)
        <> "; run `hbs2-peer fetch --timeout 60 " <> hashText root <> "`"
    GroupKeyObjectInvalid root ->
      "group-key object " <> hashText root <> " is complete but has an invalid format"
    where
      hashText = show . pretty

instance Exception GroupKeyLoadError

loadGroupKeyChecked :: forall s m.
                       (ForGroupKeySymm s, MonadIO m)
                    => AnyStorage
                    -> HashRef
                    -> m (Either GroupKeyLoadError (GroupKey 'Symm s))
loadGroupKeyChecked sto root = do
  missing <- findMissedBlocks sto root
    <&> List.sortOn (show . pretty) . HS.toList . HS.fromList
  if not (null missing)
    then pure $ Left $ GroupKeyObjectIncomplete root missing
    else do
      raw <- runExceptT $ readFromMerkle sto (SimpleKey (fromHashRef root))
      pure $ case raw of
        Left _ -> Left $ GroupKeyObjectInvalid root
        Right bytes -> case deserialiseOrFail @(GroupKey 'Symm s) bytes of
          Left _ -> Left $ GroupKeyObjectInvalid root
          Right groupKey -> Right groupKey
