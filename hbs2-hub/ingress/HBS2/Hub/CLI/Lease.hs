-- | Provider-local repository lease capacity and durable lease state.
--
-- This is operator metadata, not repository metadata.  The JSON document is
-- consumed by the local hbs2-hub browser and deliberately keeps a silent peer
-- distinct from zero donated capacity or an empty lease list.
module HBS2.Hub.CLI.Lease
  ( LeaseStanding(..)
  , leaseStatusContract
  , leaseStatusDoc
  , leaseEntries
  ) where

import HBS2.Hub.CLI.Argv (badArgs,flagsAndSwitches,flagSwitch)
import HBS2.Hub.Ingress (rpcTimeout)
import HBS2.Hub.Render (contractVersion,renderDocument)

import HBS2.Base58 (AsBase58(..))
import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.Peer.Proto.RepositoryLease
import HBS2.Peer.RepositoryLease.Admission
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)

import Data.Aeson (Value(..),object,(.=),ToJSON(toJSON))
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text

data LeaseStanding = LeaseStanding
  { leaseCapacity :: Either Text RepositoryLeaseCapacity
  , leaseRecords  :: Either Text [RepositoryLease]
  }
  deriving stock (Eq,Show)

leaseStatusDoc :: LeaseStanding -> [Doc ()]
leaseStatusDoc LeaseStanding{..} =
  capacityLines
  <> [ "configuration barter-budget-bytes and barter-max-contracts"
     , "              in the hbs2-peer config; restart the peer to apply"
     ]
  <> leaseLines
  where
    capacityLines = case leaseCapacity of
      Left problem ->
        [ "capacity unknown: the local peer did not answer"
        , "         " <> pretty problem
        ]
      Right RepositoryLeaseCapacity{..} ->
        [ "capacity donated" <+> pretty repositoryLeaseCapacityBudgetBytes <+> "bytes"
        , "         reserved" <+> pretty repositoryLeaseCapacityReservedBytes
            <+> "available" <+> pretty repositoryLeaseCapacityAvailableBytes
        , "         contracts" <+> pretty repositoryLeaseCapacityContracts
            <+> "of" <+> pretty repositoryLeaseCapacityMaxContracts
        ]

    leaseLines = case leaseRecords of
      Left problem ->
        [ "leases unknown: the local peer did not answer"
        , "       " <> pretty problem
        ]
      Right [] -> ["leases no repository leases"]
      Right leases -> "leases" <+> pretty (length leases) : fmap leaseLine leases

    leaseLine RepositoryLease{..} =
      "       " <> pretty repositoryLeaseId
      <+> pretty (AsBase58 repositoryLeaseRepository)
      <+> pretty (stateName repositoryLeaseState)
      <+> pretty repositoryLeaseMaxBytes <+> "bytes reserved"

leaseStatusContract :: LeaseStanding -> Value
leaseStatusContract LeaseStanding{..} = object
  [ "contract" .= contractVersion
  , "document" .= ("lease-status" :: Text)
  , "configuration" .= object
      [ "budget_bytes" .= ("barter-budget-bytes" :: Text)
      , "max_contracts" .= ("barter-max-contracts" :: Text)
      , "apply" .= ("restart hbs2-peer" :: Text)
      ]
  , "capacity" .= capacityContract leaseCapacity
  , "leases" .= either (const Null) (toJSON . fmap leaseContract) leaseRecords
  , "leases_error" .= either toJSON (const Null) leaseRecords
  ]
  where
    capacityContract = \case
      Left problem -> object
        [ "budget_bytes" .= Null, "reserved_bytes" .= Null
        , "available_bytes" .= Null, "contracts" .= Null
        , "max_contracts" .= Null, "error" .= problem
        ]
      Right RepositoryLeaseCapacity{..} -> object
        [ "budget_bytes" .= repositoryLeaseCapacityBudgetBytes
        , "reserved_bytes" .= repositoryLeaseCapacityReservedBytes
        , "available_bytes" .= repositoryLeaseCapacityAvailableBytes
        , "contracts" .= repositoryLeaseCapacityContracts
        , "max_contracts" .= repositoryLeaseCapacityMaxContracts
        , "error" .= Null
        ]

    leaseContract RepositoryLease{..} = object
      [ "id" .= textPretty repositoryLeaseId
      , "repository" .= textPretty (AsBase58 repositoryLeaseRepository)
      , "state" .= stateName repositoryLeaseState
      , "max_bytes" .= repositoryLeaseMaxBytes
      , "max_blocks" .= repositoryLeaseMaxBlocks
      , "max_update_lag" .= repositoryLeaseMaxUpdateLag
      , "grace" .= repositoryLeaseGracePeriod
      , "created_at" .= repositoryLeaseCreatedAt
      , "updated_at" .= repositoryLeaseUpdatedAt
      , "expires_at" .= repositoryLeaseExpiresAt
      , "update_started_at" .= repositoryLeaseUpdateStartedAt
      , "snapshot" .= maybe Null snapshotContract repositoryLeaseSnapshot
      , "error" .= repositoryLeaseLastError
      ]

    snapshotContract RepositorySnapshot{..} = object
      [ "id" .= textPretty repositorySnapshotId
      , "bytes" .= repositorySnapshotLogicalBytes
      , "blocks" .= repositorySnapshotBlockCount
      , "completed_at" .= repositorySnapshotCompletedAt
      ]

    textPretty :: Pretty a => a -> Text
    textPretty = Text.pack . show . pretty

stateName :: RepositoryLeaseState -> Text
stateName = \case
  RepositoryLeaseSyncing    -> "syncing"
  RepositoryLeaseActive     -> "active"
  RepositoryLeaseDegraded   -> "degraded"
  RepositoryLeaseGrace      -> "grace"
  RepositoryLeaseTerminated -> "terminated"

leaseEntries :: forall c m . ( IsContext c
                              , MonadUnliftIO m
                              , HasClientAPI PeerAPI UNIX m
                              , Exception (BadFormException c)
                              ) => MakeDictM c m ()
leaseEntries =
  brief "show donated repository capacity and durable local leases"
    $ args [arg "string" "[--json]"]
    $ desc ( "Read-only operator status from the local peer. A timeout is"
             <> line <> "reported as unknown, never as zero capacity or no leases."
             <> line <> "Capacity is configured with barter-budget-bytes and"
             <> line <> "barter-max-contracts in hbs2-peer config." )
    $ entry $ bindMatch "hub:lease:status" $ nil_ \case
        (leaseArgs -> Just asJson) -> lift (report asJson)
        other -> liftIO (badArgs usage other)
  where
    usage = "usage: hbs2-hub lease status [--json]"

    leaseArgs syn = do
      flags <- flagsAndSwitches [] ["--json"] syn
      flagSwitch flags "--json"

    report asJson = do
      api <- getClientAPI @PeerAPI @UNIX
      capacity <- callRpcWaitMay @RpcRepositoryLeaseCapacity rpcTimeout api ()
      leases <- callRpcWaitMay @RpcRepositoryLeaseList rpcTimeout api ()
      let standing = LeaseStanding
            { leaseCapacity = maybe (Left "repository lease capacity RPC timed out") Right capacity
            , leaseRecords = maybe (Left "repository lease list RPC timed out") Right leases
            }
      liftIO $ if asJson
        then LBS.putStr (renderDocument leaseOrder (leaseStatusContract standing))
        else mapM_ print (leaseStatusDoc standing)

    leaseOrder =
      [ "contract", "document", "configuration", "budget_bytes", "max_contracts", "apply"
      , "capacity", "reserved_bytes", "available_bytes", "contracts", "error"
      , "leases", "id", "repository", "state", "max_bytes", "max_blocks"
      , "max_update_lag", "grace", "created_at", "updated_at", "expires_at"
      , "update_started_at", "snapshot", "bytes", "blocks", "completed_at"
      , "leases_error"
      ]
