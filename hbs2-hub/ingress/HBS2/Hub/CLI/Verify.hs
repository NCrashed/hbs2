-- | @hub verify@: re-run the fold's checks over canon and report (PEP-22).
--
-- The audit tool, and the one verb whose whole output is what a fold REFUSED.
-- Everything it prints comes from 'FoldResult': the drops, which are events
-- canon holds and the rules do not admit, and the anomalies, which are events
-- the rules admit and nobody should have written. The difference is the point of
-- having both, and PEP-19 keeps them apart for a reason worth repeating here: an
-- anomaly is fold-legal, so refusing it would make a clone show less than canon
-- holds, which is worse than showing it and saying so.
module HBS2.Hub.CLI.Verify
  ( verifyEntries
  ) where

import HBS2.Hub.Fold
import HBS2.Hub.Repo

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import Data.List qualified as List
import System.Exit (exitWith,ExitCode(..))

verifyEntries :: forall c m . ( IsContext c
                              , MonadUnliftIO m
                              , Exception (BadFormException c)
                              ) => MakeDictM c m ()
verifyEntries = do

  brief "re-run the fold's checks over this repository's canon and report"
    $ args [arg "string" "repo-key"]
    $ desc ( "Read-only, and needs no peer: canon is a git ref in this"
             <> line <> "repository. Reports every event the rules did not admit"
             <> line <> "and every anomaly in the ones they did."
             <> line
             <> line <> "Fetch canon first; a plain clone does not bring it, since"
             <> line <> "git's default refspec covers only heads and tags:"
             <> line <> "  git fetch <remote> '+refs/hbs2/meta:refs/hbs2/meta'"
             <> line
             <> line <> "The repository key is an argument because the tree cannot"
             <> line <> "be trusted to say whose it is: the owner key is the root of"
             <> line <> "the trust chain, so canon that named its own owner would be"
             <> line <> "canon that could rename it." )
    $ entry $ bindMatch "hub:verify" $ nil_ \case
        [ SignPubKeyLike repo ] -> lift do
          st <- readCanon gitCanon repo

          liftIO $ case stCommit st of
            Nothing -> do
              hPutDoc stderr $ "hub: no" <+> pretty metaRef <+> "in this repository."
                <+> "Fetch it with a forcing refspec, or nothing has published"
                <+> "canon yet." <> line
              exitWith (ExitFailure 1)

            Just commit -> do
              let fr = stFold st
                  dropped = frDropped fr
                  anomalies = frAnomalies fr

              print $ "canon" <+> pretty commit
                <+> parens (maybe "no version file" (("hub-meta" <+>) . pretty)
                                  (stVersion st))

              -- The unreadable files first: an event nothing could parse is not
              -- in the fold at all, so no drop or anomaly below can mention it,
              -- and a reader who stopped at the fold's own report would not learn
              -- it existed.
              for_ (stBad st) $ \(p, e) ->
                print $ "unreadable" <+> pretty p <> ":" <+> pretty e

              for_ dropped (print . pretty)
              for_ anomalies (print . pretty)

              print $ "admitted" <+> pretty (length (frLog fr))
                <+> "dropped" <+> pretty (length dropped)
                <+> "anomalies" <+> pretty (length anomalies)
                <+> "unreadable" <+> pretty (length (stBad st))

              -- Non-zero when there is anything to act on, so this is usable in
              -- a hook. All three counts, and not only the drops: an anomaly is
              -- admitted canon that should not exist, which is exactly what an
              -- audit is for, and an unreadable file is a file somebody has to
              -- look at.
              unless (List.null dropped && List.null anomalies && List.null (stBad st)) $
                exitWith (ExitFailure 2)

        _ -> throwIO (BadFormException @c nil)
