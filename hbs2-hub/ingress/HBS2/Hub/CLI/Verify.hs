-- codeOf and the advice below dispatch on every constructor of CanonUnreadable
-- with no wildcard: a new one must say what it costs a hook and what to do about
-- it, and a missing case here is a pattern-match failure at the moment somebody
-- is trying to find out why their audit will not run.
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

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
    -- * The report, for testing
    --
    -- $ These are the whole output of the verb, and the reason they are exported
    -- is that nothing else could reach them: a Doc built inside an IO action that
    -- ends in exitWith cannot be asserted on.
  , reportDoc
  , reportCode
  , refusalDoc
  , codeOf
  ) where

import HBS2.Hub.Types (pathText)
import HBS2.Hub.Fold
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import Data.List qualified as List
import Data.ByteString (ByteString)
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
  hPutDoc stderr (refusalDoc u <> line)
  exitWith (ExitFailure (codeOf u))

-- | What a refusal says: the reason, and what to do about it.
refusalDoc :: CanonUnreadable -> Doc ann
refusalDoc u = "hub:" <+> pretty u <> advice u
  where
    -- Total, and the wildcard is gone with the Werror above holding it that way.
    -- Two of these used to fall into it and print a bare complaint: a repository
    -- that is not one and an unreadable version file, which are the two a reader
    -- is least likely to work out unaided.
    advice = \case
      NoCanonRef -> line <> "  Fetch it, which a plain clone does not:" <> line
                      <> "    git fetch <remote> '+" <> pretty metaRef
                      <> ":" <> pretty metaRef <> "'" <> line
                      -- Three causes, and git gives this reader no way to tell
                      -- them apart (see the note in HBS2.Hub.Repo.Git), so all
                      -- three are named. The fetch above is the remedy for two of
                      -- them, which is why it comes first.
                      <> "  Or the ref is here and broken, or nothing has"
                      <> " published canon for this repository yet."
      NoRepository _ -> line <> "  Run this inside the repository, or check that"
                          <> " git is installed and" <> line
                          <> "  that this user may read it (safe.directory)."
      RefUnresolved _ -> line <> "  The ref is here and what it names is not."
                           <> " Fetch again, or unshallow." <> line
                           <> "  A ref pointing at a tree or a tag rather than a"
                           <> " commit is not canon."
      -- No single cause, so no single remedy: the message above is git's own, and
      -- claiming "a pruned object" over a permission error or a listing past the
      -- bound sent people to fetch what was already there.
      TreeUnreadable _ -> line <> "  Read the message above: a pruned object or a"
                            <> " partial clone is fixed by" <> line
                            <> "  fetching, a permission or ownership complaint is"
                            <> " not."
      CanonTooNewHere _ -> line <> "  Upgrade; this build would fold it under"
                             <> " rules it does not implement."
      -- The one problem whose file cannot be named in the report, because it is
      -- not an event and the report is a report about events.
      VersionUnreadable _ -> line <> "  " <> pathDoc versionPath
                               <> " governs the admission rules, so this reader"
                               <> " will not guess" <> line
                               <> "  at them. Whoever published canon has to"
                               <> " rewrite that file."
      CanonTooBig _  -> line <> "  Compaction is the answer to a canon this"
                          <> " large (PEP-19), not a bigger reader."
      CanonTooMany _ -> line <> "  Compaction is the answer to a canon this"
                          <> " large (PEP-19), not a bigger reader."

-- | What a refusal exits with.
--
-- Distinct codes, and chosen so a hook can say "audit could not run" without
-- enumerating them: 3 and up is a refusal, 2 is a completed audit that found
-- something, 0 a clean one. 1 is left to the argument and usage failures every
-- verb shares.
--
-- One code per constructor, with no pair sharing one. The two that did were the
-- two worth telling apart from a script: "the ref is here and broken" against
-- "the tree will not list", and "too big" against "too many", which differ in
-- which bound there is to argue with.
codeOf :: CanonUnreadable -> Int
codeOf = \case
  NoCanonRef          -> 3
  NoRepository{}      -> 4
  RefUnresolved{}     -> 5
  CanonTooNewHere{}   -> 6
  VersionUnreadable{} -> 7
  CanonTooBig{}       -> 8
  TreeUnreadable{}    -> 9
  CanonTooMany{}      -> 10

report :: CanonState -> IO ()
report st = do
  for_ (reportDoc st) print
  -- Non-zero when there is anything to act on, so this is usable in a hook.
  case reportCode st of
    0 -> pure ()
    n -> exitWith (ExitFailure n)

-- | The audit, one 'Doc' per line.
--
-- Pure, and split out from the printing for one reason: this is the output the
-- whole verb exists to produce, and nothing exercised it. Every line here is a
-- line about a stranger's bytes.
reportDoc :: CanonState -> [Doc ann]
reportDoc st =
  [ "canon" <+> pretty (stCommit st)
      <+> parens (maybe "no version file" (("hub-meta" <+>) . pretty) (stVersion st)) ]

  -- The unreadable files first: a file nothing could parse never became an
  -- event, so no drop or anomaly below can mention it, and a reader who stopped
  -- at the fold's own report would not learn it existed.
  --
  -- Through pathDoc, because a path is a stranger's bytes: the tree is read with
  -- -z precisely because a path may contain anything but NUL, and a file named
  -- with a newline and a plausible summary line forged the last line of this
  -- report. Every other stranger's text in this project already goes through it.
  <> [ "unreadable" <+> pathDoc p <> ":" <+> pretty e | (p, e) <- stBad st ]

  -- Files whose own version clause did not read. Reported and never obeyed
  -- (PEP-19), and reported at all because a writer that appends to this tree is
  -- about to rewrite those files with its own version.
  <> [ "no file version" <+> pathDoc p | (p, Nothing) <- stFileVersions st ]

  <> fmap pretty dropped
  <> fmap pretty anomalies

  -- Counts, not a partition, and said so: an event with two anomalies adds one
  -- to admitted and two to anomalies.
  <> [ "admitted" <+> pretty (length (frLog fr))
         <+> "dropped" <+> pretty (length dropped)
         <+> "anomalies" <+> pretty (length anomalies)
         <+> "unreadable" <+> pretty (length (stBad st)) ]
  where
    fr = stFold st
    dropped = frDropped fr
    anomalies = frAnomalies fr

-- | What a completed audit exits with: 2 if it found anything, 0 otherwise.
--
-- All three, and not only the drops: an anomaly is admitted canon that should not
-- exist, which is exactly what an audit is for, and an unreadable file is a file
-- somebody has to look at.
reportCode :: CanonState -> Int
reportCode st
  | List.null (frDropped fr) && List.null (frAnomalies fr) && List.null (stBad st) = 0
  | otherwise = 2
  where fr = stFold st

-- A tree path on a terminal: escaped for display, and injectively, so two paths
-- that differ read differently. A path is a stranger's bytes, and one with a
-- newline in it forged the summary line of this report before this was here. See
-- 'pathText'.
pathDoc :: ByteString -> Doc ann
pathDoc = pretty . pathText
