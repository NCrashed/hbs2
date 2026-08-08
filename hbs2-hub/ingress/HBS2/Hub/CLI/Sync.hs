-- | @hub sync@: bring this clone up to date (PEP-19, PEP-22 "Read").
--
-- The verb the read side was missing. Every reader in this tool folds canon out
-- of @refs\/hbs2\/meta@ in the repository it stands in, and nothing put it
-- there: canon is written on the maintainer's machine and travels as a git ref
-- like any other, so a clone that never fetched it has an empty forge and no
-- way to tell that from a quiet one.
--
-- THREE THINGS ARRIVE, and they are three fetches because git replaces the
-- configured refspec as soon as you name one: the branches, canon, and the
-- proposals staged under @refs\/hbs2\/pulls\/*@ that @hub pr checkout@ reads.
--
-- CANON IS NOT FORCED. PEP-22 writes the refspec with a plus and this does not,
-- because a plus replaces the canon of whoever runs it: a maintainer who has
-- just accepted a letter holds a commit the remote has not seen, and a sync
-- between the accept and the push would drop the ref onto the older one. What
-- is fetched is compared, moved when the move is a fast-forward, and reported
-- when it is not. Nothing here writes over a divergence, in either direction.
module HBS2.Hub.CLI.Sync
  ( syncEntries
  , syncUsage
    -- * The parts that decide something
  , syncDoc
  , syncCode
  , Settled(..)
  , SyncArgs(..)
  , syncArgs
  , codeDiverged
  , codeSyncFailed
  ) where

import HBS2.Hub.Types (RepoRef,safeText)
import HBS2.Hub.Compact (equivalentTo)
import HBS2.Hub.Repo (readCanonAt,stFold)
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.Repo.GitBundle (syncFrom,takeCanon,Synced(..),SyncedCanon(..))
import HBS2.Hub.CLI.Argv (flagsOf,flagMaybe,flagText,repoFlags,flagRepo,flagRepoMaybe)
import HBS2.Hub.CLI.Common (refuse)
import HBS2.Hub.CLI.Publish (codePublishFailed)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import System.Exit (die,exitWith,ExitCode(..))

-- | Canon here and canon there have diverged, and nothing was written.
--
-- Its own code because it is the one outcome that needs a person: everything
-- else this verb does either succeeded or failed as a git command. A script
-- that treats it as a failure to retry will retry forever, and one that treats
-- it as success will fold a canon that is missing what this clone holds.
-- | git would not run, would not answer, or refused.
--
-- Its OWN code, and it used to be `pr new`'s. A fetch that failed exited 27,
-- documented as "git would not build the bundle or would not answer (hub pr
-- new)" -- a number naming a command nobody had run, on a verb that builds no
-- bundle. It is the same value `hub publish` uses for the same thing, since
-- they are the two ends of one wire and a git failure on either is one event.
codeSyncFailed :: Int
codeSyncFailed = codePublishFailed

codeDiverged :: Int
codeDiverged = 40

syncUsage :: Doc ()
syncUsage = "usage: hbs2-hub sync [--remote <name>] [--repo <key>]"

syncEntries :: forall c m . ( IsContext c
                            , MonadUnliftIO m
                            , Exception (BadFormException c)
                            ) => MakeDictM c m ()
syncEntries = do

  brief "fetch code, canon and staged proposals from a remote"
    $ args [ arg "string" "[--remote name]", arg "string" "[--repo repo-key]" ]
    $ desc ( "Talks to git and to no peer of its own: whatever the remote's"
             <> line <> "url names is what fetches it, which for hbs23:// is the"
             <> line <> "helper hbs2-git3 installs."
             <> line
             <> line <> "Three fetches, because naming a refspec replaces the"
             <> line <> "configured one: the branches, refs/hbs2/meta (canon,"
             <> line <> "which every read verb folds), and refs/hbs2/pulls/*"
             <> line <> "(what hub pr checkout puts on a branch)."
             <> line
             <> line <> "CANON IS NOT FORCED, though PEP-22 spells the refspec"
             <> line <> "with a plus. Forcing it replaces the canon of whoever"
             <> line <> "runs it, and a maintainer who has just accepted a letter"
             <> line <> "holds a commit the remote has not seen. A fast-forward"
             <> line <> "happens; a divergence is reported and left alone, in"
             <> line <> "both directions."
             <> line
             <> line <> "--remote defaults to origin." )
    $ entry $ bindMatch "hub:sync" $ nil_ \case
        (syncArgs -> Just sa) -> lift (sync sa)
        _ -> liftIO (die (show syncUsage))

  where

    sync sa = do
      let remote = fromMaybe "origin" (saRemote sa)

      r <- syncFrom Nothing remote
             >>= either (\e -> liftIO (refuse (show (pretty e)) codeSyncFailed)) pure

      -- A divergence is where a rewrite and a fork look alike, and folding both
      -- is what tells them apart. Only with a repository key: the owner key is
      -- the root of the fold's trust chain (PEP-19 rule 3), so a canon that
      -- named its own owner would be one that could rename it, which is why
      -- `hub verify` takes the same argument.
      settled <- case (syCanon r, saRepo sa) of
        (CanonDiverged here there, Just repo) -> resolve' repo here there
        (c, _) -> pure (Left c)

      liftIO $ mapM_ print (syncDoc remote r settled)

      liftIO $ case syncCode r settled of
        0 -> pure ()
        n -> exitWith (ExitFailure n)

    -- Fold both lineages and decide. A rewrite that materializes identically is
    -- taken; anything else is left exactly as it was.
    resolve' repo here there = do
      a <- foldAt repo here
      b <- foldAt repo there
      case (a, b) of
        (Just fa, Just fb)
          | equivalentTo fa fb ->
              -- Through what the ref currently holds: between the fold above
              -- and this line another process may have accepted a letter, and
              -- taking the remote over that would drop it.
              takeCanon Nothing there here >>= \case
                Right () -> pure (Right (Rewritten here there))
                Left e -> pure (Right (NotTaken here there (Text.pack (show (pretty e)))))
          | otherwise -> pure (Right (Forked here there))
        _ -> pure (Right (Unreadable here there))

    -- One lineage, folded, or nothing when this clone cannot read it. Nothing
    -- is not a failure of the sync: the fetch worked, and a canon that will not
    -- fold is a fact about the tree that `hub verify` reports properly.
    foldAt repo commit =
      withGitCanon (\cs -> readCanonAt cs repo commit) <&> \case
        Right st -> Just (stFold st)
        Left _   -> Nothing

-- | What a divergence turned out to be, once both lineages were folded.
--
-- Four answers and not two, because they call for four different things: carry
-- on, look at the two canons, install git or fetch the objects, and look at why
-- the ref would not move.
data Settled =
    -- | The remote rewrote canon and it materializes identically: taken. Old,
    -- then new.
    Rewritten Text Text
    -- | The two say different things. Nothing was written.
  | Forked Text Text
    -- | One of them would not fold here, so nothing can be said about the two.
  | Unreadable Text Text
    -- | It is a rewrite and the ref would not move: somebody folded a letter
    -- between the decision and the write, or git refused.
  | NotTaken Text Text Text
  deriving stock (Eq,Show)

-- | What one sync did, for stdout.
syncDoc :: Text -> Synced -> Either SyncedCanon Settled -> [Doc ann]
syncDoc remote r settled =
  [ "fetched from" <+> pretty remote ] <> canon <> pulls
  where
    canon = case settled of
      Right s -> settledDoc s
      Left c -> plain c

    plain = \case
      CanonNone ->
        [ "canon: the remote has none, so nothing here folds yet" ]
      CanonSame ->
        [ "canon: already here" ]
      CanonMoved from to ->
        [ "canon:" <+> (if Text.null from then "new" else pretty from)
            <+> "->" <+> pretty to ]
      -- Both hashes, because the recovery is a git command over them and
      -- neither is derivable from the other.
      CanonDiverged here there ->
        [ "canon: DIVERGED, and nothing was written"
        , "  here " <+> pretty here
        , "  there" <+> pretty there
        , "  what this clone holds is not an ancestor of what the remote"
            <+> "published."
        , "  If you folded letters here, push them. If you want the remote's"
            <+> "canon"
        , "  instead, say so out loud: git update-ref refs/hbs2/meta"
            <+> pretty there
        ]

    settledDoc = \case
      Rewritten from to ->
        [ "canon: the remote rewrote it and it folds to the same state, so it"
            <+> "was taken"
        , "  was  " <+> pretty from
        , "  now  " <+> pretty to
        , "  what is gone is the timeline of overwritten values, which is what"
            <+> "a compaction trades for size (PEP-21)."
        , "  To put it back: git update-ref refs/hbs2/meta" <+> pretty from
        ]
      Forked here there ->
        [ "canon: DIVERGED, and the two do not say the same thing"
        , "  here " <+> pretty here
        , "  there" <+> pretty there
        , "  this is not a compaction: folding both gives different state, so"
            <+> "taking either loses what the other holds."
        , "  Nothing was written."
        ]
      Unreadable here there ->
        [ "canon: DIVERGED, and one of the two will not fold in this clone"
        , "  here " <+> pretty here
        , "  there" <+> pretty there
        , "  nothing can be said about them; `hub verify` says why one will not"
            <+> "read. Nothing was written."
        ]
      NotTaken here there why ->
        [ "canon: the remote rewrote it and the ref would not move"
        , "  here " <+> pretty here
        , "  there" <+> pretty there
        , "  " <> pretty (safeText why)
        , "  somebody folded a letter here between the check and the write."
            <+> "Run it again."
        ]

    pulls = [ "pulls: fetched" | syPulls r ]

-- | 0, or what the caller has to act on.
--
-- A divergence is the only non-zero, and it is not a failure of the fetch: the
-- code arrived, the pull refs arrived, and one ref was left alone on purpose.
--
-- A rewrite that was TAKEN is zero: the ref moved, the state is what it was,
-- and there is nothing for a caller to do. Everything else a divergence can
-- turn out to be keeps the code, including a rewrite whose write lost a race:
-- that one is a retry, and a caller that read it as success would carry on
-- against canon it did not take.
syncCode :: Synced -> Either SyncedCanon Settled -> Int
syncCode r settled = case settled of
  Right (Rewritten _ _) -> 0
  Right _               -> codeDiverged
  Left c -> case c of
    CanonDiverged{} -> codeDiverged
    _               -> 0

-- | @[--remote <name>]@.
data SyncArgs = SyncArgs
  { saRemote :: Maybe Text
    -- | The repository, for the divergence check alone.
    --
    -- Optional, and what it buys is named in the help: without it a rewrite
    -- upstream and a fork are one answer, because telling them apart means
    -- folding both lineages and the fold needs the owner key.
  , saRepo   :: Maybe RepoRef
  }
  deriving stock (Eq,Show)

syncArgs :: forall c . IsContext c => [Syntax c] -> Maybe SyncArgs
syncArgs syn = do
  kvs <- flagsOf (repoFlags <> ["--remote"]) syn
  -- Through 'flagText', so a remote somebody called 2026 is one. The shape git
  -- will accept is checked further in, by 'syncFrom'.
  r <- flagMaybe kvs "--remote" (fmap Text.pack . flagText)
  repo <- flagRepoMaybe (\case { SignPubKeyLike k -> Just k ; _ -> Nothing }) kvs
  pure (SyncArgs r repo)
