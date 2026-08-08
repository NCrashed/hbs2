-- | @hub maintainer add|remove|list@ (PEP-21 "Delegation", PEP-22 "Moderate").
--
-- The verbs over delegation, which the fold and the bridge already decide: a
-- @delegate@ adds a key to the set that may bless canon events, a @revoke@
-- takes it out, and both are ordinary canon events so the maintainer set at
-- any point is a function of the log up to that point.
--
-- THE OWNER KEY AND NOTHING ELSE may write one, which is rule 5 of PEP-19's
-- admission: both the author box and the canon box must be the LWWRef owner
-- key exactly. There is no @--as@ here for that reason. A delegate that could
-- delegate would be a delegate that can grow the maintainer set, which is the
-- escalation the rule exists to close, and the fold would drop the event
-- anyway with the seq already spent.
--
-- SIGNING IS NOT PUBLISHING, and somebody adding a maintainer will expect
-- otherwise. The capability hierarchy is LWWRef key, then reflog key, then
-- canon key, and a delegate holds only the last: they may sign events that
-- every clone will admit, and they cannot push @refs\/hbs2\/meta@ to put them
-- there. What that means in practice is a deployment question PEP-21 answers
-- three ways; the help below says it out loud rather than leaving it to be
-- discovered by a delegate whose accept fails at the push.
module HBS2.Hub.CLI.Maintainer
  ( maintainerEntries
  , maintainerUsage
  , maintainerArgs
  , maintainerListArgs
  , Maintainer(..)
  , maintainerDoc
  , codeNotDelegated
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Hub.Bridge
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.Repo.GitWrite (withGitSink)
import HBS2.Hub.CLI.Publish (notPublishedYet)
import HBS2.Hub.CLI.Common (refuse,saying)
import HBS2.Hub.CLI.Argv (flagsOf,flagOnce,repoFlags,flagRepo)
import HBS2.Hub.CLI.Verify (codeOf)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Net.Auth.Credentials (_peerSignSk)
import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,loadCredentials)

import Data.HashSet qualified as HS
import Data.List (sortOn)
import Data.List qualified as List
import System.Exit (die)

-- | The event was not written, and canon is unchanged.
codeNotDelegated :: Int
codeNotDelegated = 31

-- | What a maintainer verb was asked to do.
data Maintainer = Maintainer
  { mnRepo :: RepoRef
  , mnKey  :: HubKey
  }
  deriving stock (Eq,Show)

maintainerUsage :: Doc ()
maintainerUsage =
  "usage: hbs2-hub maintainer add|remove --repo <key> --key <key>"
    <> line <> "       hbs2-hub maintainer list --repo <key>"

-- | Who may bless canon, in a fixed order.
--
-- The owner is marked, because it is in the set by definition rather than by
-- any event: nothing delegated it and a revoke of it is a no-op, so a reader
-- comparing this list against the log would otherwise find one entry with no
-- event behind it.
maintainerDoc :: RepoRef -> FoldResult -> [Doc ann]
maintainerDoc owner fr =
  [ pretty (AsBase58 k) <+> (if k == owner then "(owner)" else mempty)
  | k <- sortOn (show . pretty . AsBase58) (HS.toList (frMaintainers fr)) ]

maintainerEntries :: forall c m . ( IsContext c
                                  , MonadUnliftIO m
                                  , Exception (BadFormException c)
                                  ) => MakeDictM c m ()
maintainerEntries = do

  writeVerb "hub:maintainer:add" "let a key bless canon events" ADelegate
  writeVerb "hub:maintainer:remove" "stop a key blessing canon events" ARevoke

  brief "list the keys that may bless this repository's canon"
    $ args [arg "string" "--repo repo-key"]
    $ desc ( "Read-only and peerless, like the other read verbs."
             <> line
             <> line <> "The set is a function of the log: every key in it was"
             <> line <> "delegated and not since revoked, and the owner is in it"
             <> line <> "by definition rather than by any event." )
    $ entry $ bindMatch "hub:maintainer:list" $ nil_ \case
        (maintainerListArgs -> Just repo) -> lift do
          fr <- snd <$> canonOf repo
          liftIO (mapM_ print (maintainerDoc repo fr))
        _ -> liftIO (die (show maintainerUsage))

  where

    writeVerb name what mk =
      brief what
        $ args [arg "string" "--repo repo-key", arg "string" "--key maintainer-key"]
        $ desc ( (if "add" `List.isInfixOf` show name
                    then "Lets a key bless canon events for this repository."
                    else "Stops a key blessing them. Past events stay admitted.")
                 <> line
                 <> line <> "Writes an owner-signed event onto canon."
                 <> line
                 <> line <> "THE OWNER KEY SIGNS IT, and there is no --as. PEP-19"
                 <> line <> "rule 5 requires both signatures on a delegation to be"
                 <> line <> "the repository's own key: a delegate that could"
                 <> line <> "delegate could grow the maintainer set, which is the"
                 <> line <> "escalation the rule closes."
                 <> line
                 <> line <> "SIGNING IS NOT PUBLISHING. A delegate may sign events"
                 <> line <> "every clone will admit, and cannot push refs/hbs2/meta"
                 <> line <> "to put them there: that needs the reflog key. Decide"
                 <> line <> "how their events reach canon before relying on this"
                 <> line <> "(PEP-21 'Signing versus publishing')."
                 <> line
                 <> line <> "Revoking does not invalidate what a key blessed before:"
                 <> line <> "admission is judged as of each event's own seq, so past"
                 <> line <> "events stay admitted." )
        $ entry $ bindMatch name $ nil_ \case
            (maintainerArgs -> Just mn) -> lift (write mk mn)
            _ -> liftIO (die (show maintainerUsage))

    canonOf repo =
      withGitCanon (\cs -> readCanon cs repo) >>= \case
        Right st -> pure (Just (stCommit st), stFold st)
        Left e -> liftIO (refuse (show (pretty e)) (codeOf e))

    write mk mn = do
      -- The repo key, not a key of the caller's choosing: rule 5 admits no
      -- other signer, so taking one would take a value that can only be wrong.
      creds <- runKeymanClientRO (loadCredentials (mnRepo mn))
                 >>= maybe (liftIO (refuse (show ( "no signing key here for"
                                                     <+> pretty (AsBase58 (mnRepo mn))
                                                     <> line
                                                     <> "  only the repository's own key may"
                                                     <+> "delegate or revoke" ))
                                           codeNotDelegated))
                           pure

      (parent, fr) <- canonOf (mnRepo mn)

      now <- liftIO getPOSIXTime <&> floor . (* 1000)

      let ctx = TriageCtx (mnRepo mn, _peerSignSk creds) (const True) (mnRepo mn)
          content = mk (mnRepo mn) (mnKey mn) now

      acc <- either (\e -> liftIO (refuse (show ("refused:" <+> viaShow e))
                                          codeNotDelegated))
                    pure
               (ownerEvent ctx (viewOf fr) now noOwnAttachments content)

      plan <- either (\e -> liftIO (refuse (show (pretty e)) codeNotDelegated)) pure
                (planCanon [(eventPath acc, acEvent acc)] (numberIndexOf fr))

      commit <- withGitSink (\sk -> skCommit sk (CanonWrite parent (cwFiles plan)
                                                   "hub: maintainer set" now))
                  >>= either (\e -> liftIO (refuse (show (pretty e)) codeNotDelegated))
                             pure

      liftIO $ print $ vcat
        [ "event" <+> pretty (eventId (acEvent acc))
        , "commit" <+> pretty commit
        , "maintainers are now:"
        ]
      -- From the view the bridge just advanced, which is the fold's own answer
      -- and not this verb's arithmetic about it.
      liftIO $ mapM_ print
        [ "  " <> pretty (AsBase58 k)
        | k <- sortOn (show . pretty . AsBase58) (HS.toList (cvMaintainers (acView acc))) ]

      liftIO (saying (notPublishedYet <> line))

-- Both values behind flags, for the reason every verb here has them: a repo
-- key and a maintainer key are the same thirty-two bytes of base58, and the
-- swap delegates the repository to itself while claiming the maintainer is a
-- repository nobody has.
maintainerArgs :: forall c . IsContext c => [Syntax c] -> Maybe Maintainer
maintainerArgs syn = do
  kvs  <- flagsOf ["--repo","--key"] syn
  repo <- flagOnce kvs "--repo" >>= asKey
  k    <- flagOnce kvs "--key"  >>= asKey
  pure (Maintainer repo k)
  where
    asKey = \case { SignPubKeyLike v -> Just v ; _ -> Nothing }

-- | @--repo <key>@, and nothing else. See 'HBS2.Hub.CLI.Ban.banListArgs'.
maintainerListArgs :: forall c . IsContext c => [Syntax c] -> Maybe RepoRef
maintainerListArgs syn = flagsOf repoFlags syn >>= flagRepo asKey
  where
    asKey = \case { SignPubKeyLike v -> Just v ; _ -> Nothing }
