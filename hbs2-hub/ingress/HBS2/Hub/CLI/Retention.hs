-- | Canon-aware mailbox retention and physical purge.
--
-- The peer owns mailbox storage but cannot interpret Git canon.  This command
-- folds canon locally, hands its exact 'frParts' set to the peer, and is thus
-- the only safe path that may execute attachment collection.
module HBS2.Hub.CLI.Retention
  ( retentionEntries
  , retentionArgs
  , RetentionArgs(..)
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold (FoldResult(..))
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.CLI.Argv
import HBS2.Hub.CLI.Common (withCanon,OnMissing(..),saying)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.Peer.Proto.Mailbox.Retention
import HBS2.Peer.Proto.Mailbox.Ref (MailboxRefKey(..))
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)

import Data.HashSet qualified as HS
import Data.Set qualified as Set
import System.Exit (exitFailure)

data RetentionArgs = RetentionArgs
  { raRepo    :: RepoRef
  , raExecute :: Bool
  }
  deriving stock (Eq,Show)

retentionArgs :: forall c . IsContext c => [Syntax c] -> Maybe RetentionArgs
retentionArgs syn = do
  values <- flagsAndSwitches repoFlags ["--execute"] syn
  repo <- flagRepo asKey values
  execute <- flagSwitch values "--execute"
  pure (RetentionArgs repo execute)
  where
    asKey = \case { SignPubKeyLike repoKey -> Just repoKey; _ -> Nothing }

retentionEntries :: forall c m . ( IsContext c
                                 , MonadUnliftIO m
                                 , HasClientAPI MailboxAPI UNIX m
                                 , Exception (BadFormException c)
                                 ) => MakeDictM c m ()
retentionEntries =
  brief "inspect retention or physically purge mailbox data after grace"
  $ desc ( "Reads accepted public canon and protects every attachment in its"
        <> line <> "frParts set. Without --execute this is a read-only plan."
        <> line <> "With --execute it compacts expired/tombstoned mailbox"
        <> line <> "entries and removes unreachable message/attachment blocks." )
  $ entry $ bindMatch "hub:inbox:retention" $ nil_ $ \case
      (retentionArgs -> Just RetentionArgs{..}) -> lift do
        (_, folded) <- withCanon Refuse raRepo withGitCanon
        api <- getClientAPI @MailboxAPI @UNIX
        result <- callRpcWaitMay @RpcMailboxRetention (TimeoutSec 120) api
          (MailboxRetentionRequest (Just (MailboxRefKey raRepo))
            (Set.fromList (HS.toList (frParts folded))) raExecute)
        case result of
          Nothing -> liftIO do
            saying "the peer did not answer mailbox retention within 120 seconds"
            exitFailure
          Just (Left problem) -> liftIO do
            saying ("mailbox retention refused:" <+> viaShow problem)
            exitFailure
          Just (Right report) -> liftIO $ saying (reportDoc raExecute report)
      other -> liftIO $ badArgs usage other

usage :: Doc ann
usage = "usage: hbs2-hub inbox retention --repo <key> [--execute]"

reportDoc :: Bool -> MailboxRetentionReport -> Doc ann
reportDoc execute MailboxRetentionReport{..} = vsep $
  [ "mailbox retention" <+> (if execute then "executed" else "plan")
  , "clock skew:" <+> pretty retentionReportClockSkew <+> "seconds"
  , "purge grace:" <+> pretty retentionReportPurgeGrace <+> "seconds"
  , "tracked:" <+> pretty (length retentionReportEntries)
      <> "; eligible:" <+> pretty retentionReportEligible
  , "canon-protected roots:" <+> pretty retentionReportProtectedParts
  , "candidate blocks:" <+> pretty retentionReportCandidateBlocks
      <> "; deleted:" <+> pretty retentionReportDeletedBlocks
  ] <> [ "failure:" <+> pretty failure | failure <- retentionReportFailures ]
