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
  , SyncArgs(..)
  , syncArgs
  , codeDiverged
  ) where

import HBS2.Hub.Repo.GitBundle (syncFrom,Synced(..),SyncedCanon(..))
import HBS2.Hub.CLI.Argv (flagsOf,flagMaybe,flagText)
import HBS2.Hub.CLI.Inbox (refuse)
import HBS2.Hub.CLI.Pr (codeBundleFailed)

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
codeDiverged :: Int
codeDiverged = 40

syncUsage :: Doc ()
syncUsage = "usage: hbs2-hub sync [--remote <name>]"

syncEntries :: forall c m . ( IsContext c
                            , MonadUnliftIO m
                            , Exception (BadFormException c)
                            ) => MakeDictM c m ()
syncEntries = do

  brief "fetch code, canon and staged proposals from a remote"
    $ args [ arg "string" "--remote name" ]
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
             >>= either (\e -> liftIO (refuse (show (pretty e)) codeBundleFailed)) pure

      liftIO $ mapM_ print (syncDoc remote r)

      liftIO $ case syncCode r of
        0 -> pure ()
        n -> exitWith (ExitFailure n)

-- | What one sync did, for stdout.
syncDoc :: Text -> Synced -> [Doc ann]
syncDoc remote r =
  [ "fetched from" <+> pretty remote ] <> canon <> pulls
  where
    canon = case syCanon r of
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

    pulls = [ "pulls: fetched" | syPulls r ]

-- | 0, or what the caller has to act on.
--
-- A divergence is the only non-zero, and it is not a failure of the fetch: the
-- code arrived, the pull refs arrived, and one ref was left alone on purpose.
syncCode :: Synced -> Int
syncCode r = case syCanon r of
  CanonDiverged{} -> codeDiverged
  _               -> 0

-- | @[--remote <name>]@.
newtype SyncArgs = SyncArgs { saRemote :: Maybe Text }
  deriving stock (Eq,Show)

syncArgs :: forall c . IsContext c => [Syntax c] -> Maybe SyncArgs
syncArgs syn = do
  kvs <- flagsOf ["--remote"] syn
  -- Through 'flagText', so a remote somebody called 2026 is one. The shape git
  -- will accept is checked further in, by 'syncFrom'.
  r <- flagMaybe kvs "--remote" (fmap Text.pack . flagText)
  pure (SyncArgs r)
