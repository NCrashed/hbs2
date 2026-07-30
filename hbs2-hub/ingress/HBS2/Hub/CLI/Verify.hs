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

import HBS2.Hub.Types (safeText)
import HBS2.Hub.Fold
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import Data.List qualified as List
import Data.Text qualified as Text
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
          readCanon gitCanon repo >>= liftIO . either refused report

        _ -> throwIO (BadFormException @c nil)

-- There is nothing to audit, and which nothing it is decides both the advice and
-- the exit code. One code for all of them told a hook that an unfetched ref, a
-- repository that is not one, a tree with a pruned object and canon from the
-- future were the same event.
refused :: CanonUnreadable -> IO ()
refused u = do
  hPutDoc stderr ("hub:" <+> pretty u <> advice u <> line)
  exitWith (ExitFailure (codeOf u))
  where
    advice = \case
      NoCanonRef -> line <> "  Fetch it, which a plain clone does not:" <> line
                      <> "    git fetch <remote> '+" <> pretty metaRef
                      <> ":" <> pretty metaRef <> "'" <> line
                      <> "  Or nothing has published canon for this repository yet."
      TreeUnreadable _ -> line <> "  A partial clone or a pruned object."
                            <> " Fetch again, or unshallow."
      CanonTooNewHere _ -> line <> "  Upgrade; this build would fold it under"
                             <> " rules it does not implement."
      _ -> mempty

    -- Distinct codes, and chosen so a hook can say "audit could not run" without
    -- enumerating them: 3 and up is this function, 2 is a completed audit that
    -- found something. 1 is left to the argument and usage failures every verb
    -- shares.
    codeOf = \case
      NoCanonRef        -> 3
      NoRepository{}    -> 4
      TreeUnreadable{}  -> 5
      CanonTooNewHere{} -> 6
      VersionUnreadable -> 7

report :: CanonState -> IO ()
report st = do
  print $ "canon" <+> pretty (stCommit st)
    <+> parens (maybe "no version file" (("hub-meta" <+>) . pretty) (stVersion st))

  -- The unreadable files first: a file nothing could parse never became an
  -- event, so no drop or anomaly below can mention it, and a reader who stopped
  -- at the fold's own report would not learn it existed.
  --
  -- Through safeText, because a path is a stranger's bytes: the tree is read with
  -- -z precisely because a path may contain anything but NUL, and a file named
  -- with a newline and a plausible summary line forged the last line of this
  -- report. Every other stranger's text in this project already goes through it.
  for_ (stBad st) $ \(p, e) ->
    print $ "unreadable" <+> pretty (safeText (Text.pack p)) <> ":" <+> pretty e

  -- Files whose own version clause did not read. Reported and never obeyed
  -- (PEP-19), and reported at all because a writer that appends to this tree is
  -- about to rewrite those files with its own version.
  for_ [ p | (p, Nothing) <- stFileVersions st ] $ \p ->
    print $ "no file version" <+> pretty (safeText (Text.pack p))

  for_ dropped (print . pretty)
  for_ anomalies (print . pretty)

  -- Counts, not a partition, and said so: an event with two anomalies adds one
  -- to admitted and two to anomalies.
  print $ "admitted" <+> pretty (length (frLog fr))
    <+> "dropped" <+> pretty (length dropped)
    <+> "anomalies" <+> pretty (length anomalies)
    <+> "unreadable" <+> pretty (length (stBad st))

  -- Non-zero when there is anything to act on, so this is usable in a hook. All
  -- three, and not only the drops: an anomaly is admitted canon that should not
  -- exist, which is exactly what an audit is for, and an unreadable file is a
  -- file somebody has to look at.
  unless (List.null dropped && List.null anomalies && List.null (stBad st)) $
    exitWith (ExitFailure 2)

  where
    fr = stFold st
    dropped = frDropped fr
    anomalies = frAnomalies fr
