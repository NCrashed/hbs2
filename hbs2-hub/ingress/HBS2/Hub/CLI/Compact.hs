-- | @hub compact@: bound canon's growth (PEP-19 "Compaction", PEP-21 policy).
--
-- The only verb in this build that REMOVES from canon, and it does so by not
-- carrying something forward rather than by deleting: it writes a new lineage
-- holding the retained events and swings @refs\/hbs2\/meta@ onto it. The old
-- commits stay in the object store until git prunes them, which is what makes
-- the printed way back a real one.
--
-- WHAT MAY GO IS NOT DECIDED HERE. "HBS2.Hub.Compact" is the rule, pure and
-- tested one retain reason at a time; this is the plumbing around it. The
-- division is deliberate: what may be dropped from append-only canon should be
-- readable without reading a verb.
--
-- THE FILES KEEP THE NAMES THEY HAD. The reader hands back each event with the
-- path the tree held it at, and this writes exactly those back. Deriving the
-- path instead would quietly rename a misnamed file, which @hub verify@ reports
-- and a compaction is not entitled to repair.
--
-- AND IT SWAPS AGAINST THE CANON IT COMPACTED. Between the read and the write a
-- letter may be folded here; the write then fails as 'RefMoved' rather than
-- replacing a lineage that has one more event in it than the plan does.
module HBS2.Hub.CLI.Compact
  ( compactEntries
  , compactUsage
  , CompactArgs(..)
  , compactArgs
  , compactDoc
  , ownsCanon
  , codeNothingToCompact
  , codeNotThisCanon
  , compactable
  , compactionStamp
  , unreadableDoc
  , codeCanonUnreadableHere
  ) where

import HBS2.Hub.Types
import HBS2.Base58 (AsBase58(..))
import HBS2.Hub.Compact
import HBS2.Hub.Fold (FoldResult(..),frLastFolded)
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.Repo.GitWrite (withGitSink)
import HBS2.Hub.CLI.Argv (flagsAndSwitches,flagSwitch,repoFlags,flagRepo)
import HBS2.Hub.CLI.Publish (notPublishedYet)
import HBS2.Hub.CLI.Common (refuse,saying,withCanon,withCanonState,OnMissing(..))
import HBS2.Hub.CLI.Accept (codeCanonUnwritable)
import HBS2.Hub.CLI.Verify (codeOf,pathDoc)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Word (Word64)
import Data.List qualified as List
import System.Exit (die,exitSuccess,exitWith,ExitCode(..))

-- | Does the key named own the canon about to be rewritten?
--
-- NOT an authorization check, and the distinction is the whole of what this
-- verb does and does not gate. Compaction signs nothing: it rewrites a git ref
-- in the repository it stands in, so what gates it is who may write to this
-- repository, and what gates PUBLISHING it is who may push -- neither of which
-- is a question a signature here could answer. PEP-21 calls compaction an owner
-- or delegated-maintainer operation, and that describes who in practice has
-- those two rights, not a test this verb performs.
--
-- What IS worth catching is the mistake that looks like nothing: a mistyped or
-- simply wrong @--repo@. The rule that selects events never asks whose canon
-- this is, so it happily plans a rewrite; the number index, on the other hand,
-- is derived from the FOLD, and a fold under a key that owns nothing here
-- admits nothing -- so the tree would be rewritten with an empty index on the
-- strength of a typo.
--
-- A canon holding events of which the fold admitted none is that case. Some
-- admitted and some dropped is ordinary and says nothing.
ownsCanon :: [a] -> FoldResult -> Bool
ownsCanon events fr = List.null events || not (HM.null (frAdmitted fr))

-- | The stamp a compaction commit carries.
--
-- Derived from what canon holds rather than read off a clock. Every other canon
-- commit is stamped with the event it publishes; a compaction publishes no
-- event, so the newest @folded-ts@ canon holds is what "when this canon is"
-- means. Two maintainers compacting the same canon therefore produce the same
-- commit, and a retry after a lost answer costs nothing instead of forking the
-- lineage.
--
-- THE FIELD MATTERS AND THIS USED TO BE 'frMaxSeq', which is not a time.
-- 'cnWhen' is documented as epoch milliseconds and the writer divides it by
-- 1000, so a canon holding forty events was published with a commit date of
-- 1970-01-01T00:00:00Z while every other canon writer passed a real clock -- and
-- a canon whose highest seq passed 2^63-1 could not be compacted at all, since
-- git refuses that date. 'frLastFolded' is the same determinism (nothing here
-- reads a clock) and is the value that means what the field says. It is bounded
-- too: 'maxFoldedTs' is an admission rule, so no admitted event can carry a date
-- git will not take.
--
-- Named and exported rather than left inline, because a value that decides a
-- commit id should be assertable without running the verb.
compactionStamp :: CanonState -> Word64
compactionStamp = frLastFolded . stFold

-- | What canon holds that this build cannot carry forward, if anything.
--
-- The rule this verb implements is about EVENTS, and 'stEvents' is only the
-- files that read, parsed and became one. Everything else -- a blob this clone
-- does not have, a file over the reader's bound, a path the layout does not
-- have, a duplicate -- is in 'stBad'. The writer is 'skRewrite', which commits
-- the plan and nothing else: no read-tree, so nothing outside the plan
-- survives. So compacting a tree with one unreadable file in it DELETED that
-- file, reported only the events it had dropped, and exited 0.
--
-- Two ways that happens and neither is exotic. A shallow or partial clone
-- classifies event blobs as absent, and @hub verify@ there exits 2 without
-- refusing, so a compaction publishes a canon those events are gone from. And a
-- hostile upstream puts one file this reader will not take into its own canon,
-- so that a maintainer who compacts launders the finding out of their lineage
-- while it stays in everybody else's.
--
-- Its own function, beside 'ownsCanon', for the same reason that one is: what
-- this verb refuses should be readable without reading the verb.
compactable :: CanonState -> Either [(ByteString, FileProblem)] ()
compactable st
  | List.null (stBad st) = Right ()
  | otherwise            = Left (stBad st)

-- | What this verb says when canon holds files it cannot carry forward.
--
-- Bounded and through 'pathDoc', because a canon path is a stranger's bytes:
-- the same rule and the same printer @hub verify@ uses to report the same list,
-- so the two cannot come to print it by different rules.
unreadableDoc :: RepoRef -> [(ByteString, FileProblem)] -> Doc AnsiStyle
unreadableDoc repo bad =
  "canon here holds" <+> pretty (length bad)
    <+> "file(s) this reader cannot take, and compacting"
    <> line <> "  would remove them:"
    <> line
    <> indent 2 (vcat [ pathDoc p <> ":" <+> pretty w | (p, w) <- take 10 bad ])
    <> line
    <> "  Nothing was written. `hbs2-hub verify" <+> pretty (AsBase58 repo)
      <> "` lists all of them;"
    <> line <> "  a shallow or partial clone is the ordinary reason, and"
      <+> "fetching the rest is the fix."

-- | The key named is not the owner of this canon.
codeNotThisCanon :: Int
codeNotThisCanon = 43

-- | Canon holds a file this reader cannot take, so compacting would remove it.
--
-- Its own code, and above the range the earlier verbs took, because a hook
-- branches on these and they are never reassigned (PEP-22). Distinct from
-- 'codeCanonUnwritable': nothing is wrong with the writer here and nothing was
-- attempted -- the tree holds something this build cannot carry forward, which
-- is usually a shallow or partial clone and is fixed by fetching rather than by
-- retrying.
codeCanonUnreadableHere :: Int
codeCanonUnreadableHere = 48

-- | There is nothing superseded to drop.
--
-- Its own code and not a failure: a repository whose canon is all opens and
-- comments has nothing a compaction can take, which is the ordinary state of a
-- young forge and of a well-behaved old one. A script that ran this on a
-- schedule should be able to tell it from a refusal.
codeNothingToCompact :: Int
codeNothingToCompact = 42

-- | And what it says when there is no canon at all.
nothingHere :: Doc AnsiStyle
nothingHere = "nothing to compact: this repository has no canon yet"

-- | What one compaction was asked to do.
data CompactArgs = CompactArgs
  { caRepo :: RepoRef
    -- | Say what would go and write nothing.
    --
    -- The first thing anybody should run, and the reason it is a switch rather
    -- than a separate verb: the plan and the write must come from one code
    -- path, or the thing shown is not the thing done.
  , caDry  :: Bool
  }
  deriving stock (Eq,Show)

compactUsage :: Doc ()
compactUsage = "usage: hbs2-hub compact --repo <key> [--dry-run]"

compactEntries :: forall c m . ( IsContext c
                               , MonadUnliftIO m
                               , Exception (BadFormException c)
                               ) => MakeDictM c m ()
compactEntries = do

  brief "drop the superseded events canon no longer needs"
    $ args [ arg "string" "--repo repo-key", arg "string" "[--dry-run]" ]
    $ desc ( "Writes a new lineage for refs/hbs2/meta holding everything but"
             <> line <> "the set events a later one overwrote, and swings the ref"
             <> line <> "onto it. Nothing is deleted: the old commits stay until"
             <> line <> "git prunes them, and the way back is printed."
             <> line
             <> line <> "WHAT IS LOST is the timeline of overwritten values -- who"
             <> line <> "set which label when. What is kept is everything a reader"
             <> line <> "or the fold can still need: every open, comment, merge,"
             <> line <> "redact, delegate and revoke, every close or reopen"
             <> line <> "carrying a note, the winning value of each attribute, and"
             <> line <> "anything a redact names."
             <> line
             <> line <> "--dry-run says what would go and writes nothing. Run it"
             <> line <> "first: canon is what every clone folds, and this is the"
             <> line <> "one verb that takes something out of it."
             <> line
             <> line <> "NOTHING IS SIGNED HERE, so nothing is checked against a"
             <> line <> "key: this rewrites a git ref in the repository you are"
             <> line <> "standing in, and what gates that is who may write to it."
             <> line <> "Publishing the result is a push, gated by who may push."
             <> line <> "PEP-21 calls compaction an owner operation, and that is"
             <> line <> "who holds those two rights, not a test run here."
             <> line
             <> line <> "What IS checked is that --repo names the owner this canon"
             <> line <> "answers to. A wrong key plans a perfectly good-looking"
             <> line <> "rewrite and an empty number index."
             <> line
             <> line <> "EVERY CLONE SEES A DIVERGENCE afterwards, because the"
             <> line <> "lineage changed. `hub sync --repo <key>` folds both and"
             <> line <> "takes the rewrite when the two materialize identically,"
             <> line <> "which a compaction does by construction; without --repo"
             <> line <> "it reports the divergence and leaves it." )
    $ entry $ bindMatch "hub:compact" $ nil_ \case
        (compactArgs -> Just ca) -> lift (compact ca)
        _ -> liftIO (die (show compactUsage))

  where

    compact ca = do
      -- RefuseWith 42, which is this verb's own "there was nothing to do".
      -- No canon and canon with nothing superseded in it are one event for
      -- whoever runs this on a schedule: neither is a failure and neither
      -- wrote anything. Answering 3 ("canon is unreadable") for the first made
      -- a hook branching on 42 miss half the cases it was written for.
      st <- withCanonState (RefuseWith codeNothingToCompact nothingHere)
                           (caRepo ca) withGitCanon

      -- BEFORE anything is planned or printed. A wrong --repo produces a
      -- perfectly good-looking plan (the rule never asks whose canon this is)
      -- and an empty number index, so the refusal has to come first.
      unless (ownsCanon (stEvents st) (stFold st)) $ liftIO $
        refuse (show ( "canon here holds" <+> pretty (length (stEvents st))
                         <+> "event(s) and none of them is blessed by"
                         <+> pretty (AsBase58 (caRepo ca))
                       <> line
                       <> "  that is a repository key this canon does not answer"
                          <+> "to: nothing was written." ))
               codeNotThisCanon

      -- AND WHAT THE READER COULD NOT TAKE, before anything is planned.
      --
      -- The rule this verb implements is about EVENTS, and 'stEvents' is only
      -- the files that read, parsed and became one. Everything else -- a blob
      -- this clone does not have, a file over the reader's bound, a path the
      -- layout does not have, a duplicate -- is in 'stBad', and the writer here
      -- is 'skRewrite', which commits the plan and nothing else: no read-tree,
      -- so nothing outside the plan survives. So a compaction over a tree with
      -- one unreadable file DELETED it, reported only the events it had
      -- dropped, and exited 0.
      --
      -- Two ways that happens and neither is exotic. A shallow or partial clone
      -- classifies event blobs as absent, and `hub verify` there exits 2 and
      -- does not refuse; compacting then publishes a canon those events are
      -- gone from. And a hostile upstream puts one file the reader will not take
      -- into its own canon, so that a maintainer who compacts launders the
      -- finding out of their lineage while it stays in everybody else's.
      --
      -- REFUSED RATHER THAN NORMALISED, and the whole tree rather than the file:
      -- what this verb may drop is written down (PEP-19 "Compaction"), a file it
      -- cannot read is not on that list, and the module header says so about
      -- events it merely cannot resolve. `hub verify` names them.
      either (\bad -> liftIO (refuse (show (unreadableDoc (caRepo ca) bad))
                                     codeCanonUnreadableHere))
             pure
             (compactable st)

      -- The FOLD is handed to the rule, not just the events: an event the fold
      -- refused must neither be dropped nor displace one it admitted. See the
      -- header of "HBS2.Hub.Compact".
      let c = compactionOf (stFold st) (fmap snd (stEvents st))
          -- Back under the names the tree had. The reader carried them for
          -- exactly this.
          --
          -- A HashSet, not `elem` over a list. The comment on 'sortCanon' in
          -- "HBS2.Hub.Repo" measured the same shape at 0.3 s for 5000 files,
          -- 4.6 s for 20000 and 20 s for 40000, against a bound of
          -- 'maxCanonFiles'.
          keptIds = HS.fromList (fmap eventId (cpKeep c))
          held = [ (p, e) | (p, e) <- stEvents st, HS.member (eventId e) keptIds ]

      -- WITH THE CODE IT IS DOCUMENTED TO EXIT WITH. This printed and exited
      -- zero, so 'codeNothingToCompact' was defined, exported, described as the
      -- thing a scheduled run tells from a refusal -- and returned by nothing.
      -- A hook branching on 42 never fired, and one reading 0 as "a compaction
      -- happened" was wrong every time canon had nothing superseded in it,
      -- which is the ordinary state of a young forge and of a well-behaved old
      -- one.
      when (List.null (cpDrop c)) $ liftIO do
        -- On stderr with the rest of the advice: what this verb PRODUCES on
        -- stdout is the plan and the way back, and a scheduled run pipes that
        -- somewhere. The exit code is what a hook branches on.
        saying ("nothing to compact: canon holds no superseded event" <> line)
        exitWith (ExitFailure codeNothingToCompact)

      liftIO $ mapM_ print (compactDoc (stCommit st) c)

      when (caDry ca) $ liftIO do
        print ("--dry-run: nothing was written" :: Doc ())
        exitSuccess

      -- The number index is regenerated rather than carried: it is a
      -- convenience map derived from the opens, all of which are retained, so
      -- rebuilding it cannot change what it says.
      plan <- either (\e -> liftIO (refuse (show (pretty e)) codeCanonUnwritable)) pure
                -- The tree's own declaration, so a compaction never lowers it:
                -- the retained set may no longer contain the event that raised
                -- it, and a rewrite that quietly said "version 1" would tell a
                -- version 1 build it may fold what it cannot.
                (planCanon (stVersion st)
                           [ (BS8.unpack p, e) | (p, e) <- held ]
                           (numbersOf (stFold st)))

      commit <- withGitSink (\sk ->
                  skRewrite sk (CanonWrite (Just (stCommit st)) (cwFiles plan)
                                           (message c) (compactionStamp st)))
                  >>= either (\e -> liftIO (refuse (show (pretty e)) codeCanonUnwritable))
                             pure

      liftIO $ print $ vcat
        [ "compacted" <+> pretty (length (cpDrop c)) <+> "event(s)"
        , "was " <+> pretty (stCommit st)
        , "now " <+> pretty commit
        , "every clone will see this as a divergence: `hub sync --repo <key>`"
            <+> "folds both and takes it."
        , "To put it back: git update-ref refs/hbs2/meta" <+> pretty (stCommit st)
        ]

      liftIO (saying (notPublishedYet <> line))

    numbersOf fr = numberIndexOf fr

    message c = "hub: compacted, dropped " <> tshow (length (cpDrop c)) <> " event(s)"


    tshow :: Int -> Text
    tshow = fromString . show

-- | What a compaction would do, for stdout.
--
-- The DROPPED events, listed, because that is what a person is being asked to
-- approve: the retained ones are everything else and counting them says
-- nothing. Bounded, because canon a stranger contributed to can hold a great
-- many superseded sets.
compactDoc :: Text -> Compaction -> [Doc ann]
compactDoc from c =
  [ "canon" <+> pretty from
  , "keeping" <+> pretty (length (cpKeep c)) <+> "event(s), dropping"
      <+> pretty (length (cpDrop c))
  ]
  <> [ "  drop" <+> hashDoc (eventId e) | e <- take maxListed (cpDrop c) ]
  <> [ "  and" <+> pretty (length (cpDrop c) - maxListed) <+> "more"
     | length (cpDrop c) > maxListed ]

-- | How many dropped events are worth printing.
maxListed :: Int
maxListed = 50

-- | @--repo <key> [--dry-run]@.
compactArgs :: forall c . IsContext c => [Syntax c] -> Maybe CompactArgs
compactArgs syn = do
  kvs  <- flagsAndSwitches repoFlags ["--dry-run"] syn
  repo <- flagRepo asKey kvs
  dry  <- flagSwitch kvs "--dry-run"
  pure (CompactArgs repo dry)
  where
    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
