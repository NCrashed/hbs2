-- | @hub publish@: put canon where other people can fetch it (PEP-22).
--
-- The verb the write side was missing, and its absence was the widest hole in
-- this tool. Every verb that writes canon -- accept, merge, close, label,
-- assign, redact, maintainer add, compact -- writes @refs\/hbs2\/meta@ in the
-- repository the operator is standing in, prints the commit it made, and exits
-- zero. None of that leaves the machine. A maintainer could triage a queue,
-- fold six letters, close two issues and record a merge, have every command
-- succeed, and have the contributor's @hub updates@ still show nothing a week
-- later -- with @hub verify@ and @hub log@ locally confirming, correctly, that
-- everything is fine.
--
-- THE OTHER HALF OF @hub sync@, and deliberately shaped like it: same
-- @--remote@, same two things carried (canon and the staged proposals), same
-- refusal to overwrite a divergence. Reading and publishing are the two ends of
-- one wire and there is no reason for them to disagree about what is on it.
--
-- IT SIGNS NOTHING AND CHECKS NO KEY. What gates this is who may push to the
-- remote, which is git's question and the reflog key's, not a signature's --
-- the same distinction @hub compact@'s help draws about who may write to a
-- repository. A delegate holding a canon key can bless events and cannot
-- publish them (PEP-21 A1, "signing versus publishing"), and this verb is where
-- that stops being an abstraction: their push is refused by the remote.
module HBS2.Hub.CLI.Publish
  ( publishEntries
  , publishUsage
    -- * The parts that decide something
  , publishDoc
  , publishCode
  , PublishArgs(..)
  , publishArgs
  , codeNotPublished
  , codePublishFailed
  , notPublishedYet
  ) where

import HBS2.Hub.Types (safeText)
import HBS2.Hub.Repo.GitBundle (publishTo,Published(..),PublishedCanon(..),PublishedPulls(..))
import HBS2.Hub.CLI.Argv (badArgs,flagsOf,flagMaybe,flagText)
import HBS2.Hub.CLI.Common (refuse)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import System.Exit (die,exitWith,ExitCode(..))

-- | The remote holds canon this clone does not contain, and nothing was
-- written.
--
-- Its own code, like @hub sync@'s divergence and for the same reason: it is the
-- one outcome that needs a person rather than a retry. A script that treats it
-- as a transient failure will loop; one that treats it as success will believe
-- it published what is still sitting on the local machine.
codeNotPublished :: Int
codeNotPublished = 45

-- | git would not run, would not answer, or refused.
--
-- Its OWN code rather than one borrowed from a verb that happens to have one.
-- @hub sync@ borrows @codeBundleFailed@, which is documented as belonging to
-- @hub pr new@, so a git failure during a fetch exits with a number naming a
-- command nobody ran. Not repeating that here.
codePublishFailed :: Int
codePublishFailed = 46

publishUsage :: Doc ()
publishUsage = "usage: hbs2-hub publish [--remote <name>]"

-- | The line every canon-writing verb prints after its commit.
--
-- One sentence in one place, because it is the same sentence six verbs owe
-- their caller, and six copies of it would be six chances to drift. On stderr
-- through 'saying', like every other note: the commit hash above it is the
-- verb's output and a script may be reading it.
notPublishedYet :: Doc ann
notPublishedYet =
  "not published yet: this wrote refs/hbs2/meta in this repository only."
    <> line <> "  `hbs2-hub publish` sends it, and until it does no clone sees it."

data PublishArgs = PublishArgs
  { paRemote :: Maybe Text
  }
  deriving stock (Eq,Show)

publishEntries :: forall c m . ( IsContext c
                               , MonadUnliftIO m
                               , Exception (BadFormException c)
                               ) => MakeDictM c m ()
publishEntries = do

  brief "push canon and the staged proposals to a remote"
    $ args [ arg "string" "[--remote name]" ]
    $ desc ( "The other half of `hub sync`. Everything that writes canon --"
             <> line <> "accept, merge, close, label, assign, redact, maintainer"
             <> line <> "add, compact -- writes a git ref in the repository you are"
             <> line <> "standing in. This is what sends it."
             <> line
             <> line <> "TWO THINGS GO: refs/hbs2/meta, which is canon, and"
             <> line <> "refs/hbs2/pulls/*, which are the proposal heads a reviewer"
             <> line <> "checks out. Without the second, `hub pr checkout` in"
             <> line <> "somebody else's clone has nothing to check out."
             <> line
             <> line <> "CANON IS NOT FORCED. If the remote holds canon this clone"
             <> line <> "does not contain -- a second maintainer folded something --"
             <> line <> "nothing is written and this says so. Run `hub sync --repo"
             <> line <> "<key>`, which folds both and takes the rewrite when it is"
             <> line <> "one, and publish again."
             <> line
             <> line <> "The proposal refs ARE forced, because a revised pull"
             <> line <> "request moves its head and that is a rewrite by"
             <> line <> "construction. `hub sync` fetches them the same way."
             <> line
             <> line <> "NOTHING IS SIGNED HERE and no key is checked: what gates"
             <> line <> "publishing is who may push, which is git's question. A"
             <> line <> "delegate can bless events into canon and cannot publish"
             <> line <> "them (PEP-21: signing is not publishing); the remote is"
             <> line <> "where they find that out." )
    $ entry $ bindMatch "hub:publish" $ nil_ \case
        (publishArgs -> Just pa) -> lift (publish pa)
        other -> liftIO (badArgs publishUsage other)

  where

    publish pa = do
      let remote = fromMaybe "origin" (paRemote pa)

      r <- publishTo Nothing remote
             >>= either (\e -> liftIO (refuse (show (pretty e)) codePublishFailed)) pure

      liftIO $ mapM_ print (publishDoc remote r)

      liftIO $ case publishCode r of
        0 -> pure ()
        n -> exitWith (ExitFailure n)

-- | What a publish did, for stdout.
--
-- A function of the answer and nothing else, so a test can ask what a given
-- outcome prints without a git, a remote or a repository.
publishDoc :: Text -> Published -> [Doc ann]
publishDoc remote p = canonLine <> pullsLine
  where
    to = "to" <+> pretty (safeText remote)

    canonLine = case pbCanon p of
      PublishedNone ->
        [ "no canon in this repository: nothing has been folded here yet" ]
      PublishedSame c ->
        [ "canon already there" <+> to <> ":" <+> pretty (safeText c) ]
      PublishedMoved c ->
        [ "canon published" <+> to <> ":" <+> pretty (safeText c) ]
      -- The one that needs a person, so it says what to do rather than what
      -- went wrong.
      PublishedRefused theirs ->
        [ "NOT published: the remote holds canon this clone does not contain"
        , "  remote:" <+> pretty (safeText theirs)
        , "  nothing was written. `hbs2-hub sync --repo <key>` folds both and"
            <+> "takes the rewrite when it is one; then publish again."
        ]

    pullsLine = case pbPulls p of
      PullsMoved -> [ "staged proposals published" <+> to ]
      -- Said rather than left out: a maintainer who has just staged one and
      -- sees no line about it should be able to tell "there were none" from
      -- "this verb does not do that".
      PullsNone  -> [ "no staged proposals here to publish" ]
      PullsHeld  ->
        [ "staged proposals NOT published either: they are numbered out of"
            <+> "canon, and this clone's"
        , "  canon is behind. Sync first, as above." ]

-- | And what it exits with.
publishCode :: Published -> Int
publishCode p = case pbCanon p of
  PublishedRefused{} -> codeNotPublished
  _                  -> 0

-- | @[--remote <name>]@.
publishArgs :: forall c . IsContext c => [Syntax c] -> Maybe PublishArgs
publishArgs syn = do
  kvs <- flagsOf ["--remote"] syn
  -- Through 'flagText', so a remote somebody called 2026 is one. The shape git
  -- will accept is checked further in, by 'publishTo'.
  r <- flagMaybe kvs "--remote" (fmap Text.pack . flagText)
  pure (PublishArgs r)
