-- | The repository-owned replication target in its signed manifest.
--
-- The first M5 policy field is deliberately small and extensible:
--
-- > (replication (minimum 2))
--
-- It counts complete external replicas, not the owner's working copy.  An
-- absent field means that no automatic target has been declared; zero is an
-- explicit target.  Other fields inside @replication@ are preserved so that
-- availability and failure-domain policy can be added without another
-- manifest shape.
module HBS2.Git3.Repo.Replication
  ( ReplicationPolicy(..)
  , maxMinimumReplicas
  , replicationPolicyOf
  , setMinimumReplicas
  , writesBackAsReplicationPolicy
  ) where

import HBS2.Git3.Prelude
import HBS2.Git3.Repo.Mailbox (renderManifest)

import Data.Config.Suckless.Script
import Data.Text qualified as Text
import Data.Word (Word64)

newtype ReplicationPolicy = ReplicationPolicy
  { minimumReplicas :: Maybe Word64
  }
  deriving stock (Eq,Show)

-- | Keep one repository from demanding an unbounded advert/repair fan-out.
maxMinimumReplicas :: Word64
maxMinimumReplicas = 32

-- | Read the first valid minimum from all replication clauses.
--
-- Unknown and malformed fields are ignored, like the rest of the git3
-- manifest.  A later valid minimum remains readable when an older reader has
-- left an unknown field before it.
replicationPolicyOf :: [Syntax c] -> ReplicationPolicy
replicationPolicyOf manifest = ReplicationPolicy $ headMay
  [ fromIntegral n
  | ListVal (SymbolVal "replication" : fields) <- manifest
  , ListVal [SymbolVal "minimum", LitIntVal n] <- fields
  , n >= 0
  , n <= fromIntegral maxMinimumReplicas
  ]

-- | Set or clear the minimum while preserving future replication fields.
--
-- Multiple old @replication@ clauses are folded into the first one's place.
-- This removes ambiguous duplicate minima without dropping fields written by
-- a newer implementation.
setMinimumReplicas
  :: Maybe Word64
  -> [Syntax C]
  -> Either Text [Syntax C]
setMinimumReplicas wanted manifest = do
  validateMinimum wanted
  pure (replaceReplicationClause (replicationClause wanted manifest) manifest)

validateMinimum :: Maybe Word64 -> Either Text ()
validateMinimum wanted = for_ wanted $ \n ->
  when (n > maxMinimumReplicas) $
    Left ("minimum replica target is greater than " <> Text.pack (show maxMinimumReplicas))

replicationClause :: Maybe Word64 -> [Syntax C] -> Maybe (Syntax C)
replicationClause wanted manifest = case replacementFields of
  [] -> Nothing
  xs -> Just (mkForm "replication" xs)
  where
    fields = concat
      [ xs
      | ListVal (SymbolVal "replication" : xs) <- manifest
      ]
    kept = filter (not . isMinimum) fields
    replacementFields = kept <> maybe [] (\n -> [mkForm "minimum" [mkInt (fromIntegral n)]]) wanted

isMinimum :: Syntax c -> Bool
isMinimum = \case
  ListVal (SymbolVal "minimum" : _) -> True
  _ -> False

isReplication :: Syntax c -> Bool
isReplication = \case
  ListVal (SymbolVal "replication" : _) -> True
  _ -> False

replaceReplicationClause :: Maybe (Syntax C) -> [Syntax C] -> [Syntax C]
replaceReplicationClause clause = go False
  where
    go seen [] = case (seen, clause) of
      (False, Just x) -> [x]
      _ -> []
    go seen (x:xs)
      | isReplication x = case (seen, clause) of
          (False, Just y) -> y : go True xs
          _ -> go True xs
      | otherwise = x : go seen xs

-- | Check the bytes which the manifest writer will actually publish.
writesBackAsReplicationPolicy :: ReplicationPolicy -> [Syntax C] -> Bool
writesBackAsReplicationPolicy wanted manifest =
  case parseTop (renderManifest manifest) of
    Left _ -> False
    Right back -> replicationPolicyOf back == wanted
