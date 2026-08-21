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
  , Delegating(..)
  , Pointless(..)
  , pointless
  , codeNotDelegated
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Hub.Bridge
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.CLI.Publish (notPublishedYet)
import HBS2.Hub.CLI.Common (refuse,saying,withCanon,OnMissing(..)
                           ,blessed,committing,oneStop,signerFor,signingPair
                           ,Writing,writingOf,dryRunHelp)
import HBS2.Hub.CLI.Argv (badArgs,flagsOf,repoFlags,flagRepo,flagsAndSwitches,flagSwitch
                         ,flagOneOf,maintainerKeyFlags)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))

import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (sortOn)
import System.Exit (die)

-- | The event was not written, and canon is unchanged.
codeNotDelegated :: Int
codeNotDelegated = 31

-- | Which of the two verbs is running.
--
-- A tag and not the event constructor it produces, because the check below has
-- to ask which verb ran, and @ADelegate@ and @ARevoke@ are two of a dozen
-- constructors of a type whose other ten have nothing to do with delegation.
data Delegating = Delegate | Revoke
  deriving stock (Eq,Show)

-- | Why an owner-signed delegation event would change nothing.
data Pointless =
    AlreadyAMaintainer  -- ^ delegating to a key that is one already
  | NotAMaintainer      -- ^ revoking a key that is not one
  | OwnerIsAlways       -- ^ revoking the owner, whom admission keeps in the set
  deriving stock (Eq,Show)

-- | Whether this delegation or revoke would change anything.
--
-- ALL THREE WERE WRITTEN HAPPILY. Each of them mints a well-formed owner-signed
-- event, so the fold admits it, canon grows, the seq is spent -- and the report
-- then prints a maintainer set identical to the one before it. An operator who
-- mistyped a key by one character saw "maintainers are now:" and a list without
-- their key in it, which reads like the tool did the thing and the key was
-- somehow already there. Canon is append-only, so the no-op cannot be taken
-- back; it can only be followed by another event.
--
-- The owner is the third case and the least obvious: it is in the set by
-- definition and by no event (see 'maintainerDoc'), because PEP-19 rule 5 reads
-- the repository key as a maintainer whatever the log says. A revoke of it is
-- admitted and then ignored, forever.
--
-- Pure and exported for the reason "HBS2.Hub.CLI.Common"'s 'signerOf' is: the
-- call site wants a keyring and a canon, the decision is the whole of the
-- check, and a check nothing asserts is a check somebody deletes as redundant.
pointless :: Delegating
          -> RepoRef        -- ^ who owns the repository
          -> HubKey         -- ^ the key named
          -> HashSet HubKey -- ^ who may bless canon as of now
          -> Maybe Pointless
pointless kind owner k maintainers = case kind of
  Revoke   | k == owner    -> Just OwnerIsAlways
           | not delegated -> Just NotAMaintainer
  Delegate | delegated     -> Just AlreadyAMaintainer
  _ -> Nothing
  where
    delegated = HS.member k maintainers

-- | The same, in the words the operator gets.
uselessly :: Pointless -> RepoRef -> Doc ann
uselessly why owner = case why of
  OwnerIsAlways ->
    "that key owns this repository, and the owner cannot be revoked"
      <> line <> "  admission takes the owner key as a maintainer by definition"
      <+> "(PEP-19 rule 5),"
      <> line <> "  so the event would be admitted and change nothing."
  NotAMaintainer ->
    "that key is not a maintainer of this repository, so there is nothing"
      <+> "to revoke"
      <> line <> "  nothing was written. `hbs2-hub maintainer list --repo"
      <+> pretty (AsBase58 owner) <> "` says who is."
  AlreadyAMaintainer ->
    "that key is already a maintainer of this repository"
      <> line <> "  nothing was written: a second delegation would be admitted"
      <+> "and change nothing."

-- | What a maintainer verb was asked to do.
data Maintainer = Maintainer
  { mnRepo :: RepoRef
  , mnKey  :: HubKey
  , mnDry  :: Writing
  }
  deriving stock (Eq,Show)

maintainerUsage :: Doc ()
maintainerUsage =
  "usage: hbs2-hub maintainer add|remove --repo <key> --maintainer-key <key> [--dry-run]"
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

  writeVerb "hub:maintainer:add" "let a key bless canon events" Delegate
  writeVerb "hub:maintainer:remove" "stop a key blessing canon events" Revoke

  brief "list the keys that may bless this repository's canon"
    $ args [arg "string" "--repo repo-key"]
    $ desc ( "Read-only and peerless, like the other read verbs."
             <> line
             <> line <> "The set is a function of the log: every key in it was"
             <> line <> "delegated and not since revoked, and the owner is in it"
             <> line <> "by definition rather than by any event." )
    $ entry $ bindMatch "hub:maintainer:list" $ nil_ \case
        (maintainerListArgs -> Just repo) -> lift do
          -- REFUSE, like every other read verb, and unlike the two write verbs
          -- above it. This shared 'canonOf' with them, so a repository whose
          -- canon has not been fetched answered with the owner key and nothing
          -- else -- which is a true sentence about an empty fold and a false
          -- one about the repository, and indistinguishable from a project that
          -- really has no delegates.
          --
          -- The asymmetry is 'withCanon''s whole point: a verb that needs canon
          -- to HOLD something refuses, a verb that may CREATE it does not, and
          -- listing is the first kind.
          fr <- snd <$> withCanon Refuse repo withGitCanon
          liftIO (mapM_ print (maintainerDoc repo fr))
        other -> liftIO (badArgs maintainerUsage other)

  where

    writeVerb name what kind =
      brief what
        $ args [ arg "string" "--repo repo-key", arg "string" "--maintainer-key key"
               , arg "string" "[--dry-run]" ]
        $ desc ( (case kind of
                    Delegate -> "Lets a key bless canon events for this repository."
                    Revoke   -> "Stops a key blessing them. Past events stay admitted.")
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
                 <> line <> "events stay admitted."
                 <> dryRunHelp
                 <> line <> "A repo key and a maintainer key are the same thirty-two"
                 <> line <> "bytes of base58, which is what makes it worth running." )
        $ entry $ bindMatch name $ nil_ \case
            (maintainerArgs -> Just mn) -> lift (write kind mn)
            other -> liftIO (badArgs maintainerUsage other)

    canonOf repo =
      -- TreatAsEmpty: a delegation may be the first event canon holds. Naming a
      -- co-maintainer before anybody has filed an issue is an ordinary order of
      -- work, and this used to refuse it -- the only reason being that accept
      -- and this verb had each written the answer themselves.
      withCanon TreatAsEmpty repo withGitCanon

    write kind mn = do
      -- The repo key, not a key of the caller's choosing: rule 5 admits no
      -- other signer, so taking one would take a value that can only be wrong.
      creds <- signerFor (mnRepo mn)
                  >>= maybe (liftIO (refuse (show ( "cannot sign as"
                                                      <+> pretty (AsBase58 (mnRepo mn))
                                                      <> line
                                                      <> "  no keyring here holds it as its own"
                                                      <+> "signing key, and"
                                                      <> line
                                                      <> "  only the repository's own key may"
                                                      <+> "delegate or revoke" ))
                                            codeNotDelegated))
                            pure

      (parent, fr) <- canonOf (mnRepo mn)

      now <- liftIO getPOSIXTime <&> floor . (* 1000)

      let ctx = TriageCtx (signingPair creds) (const True) (mnRepo mn)
          content = eventOf kind (mnRepo mn) (mnKey mn) now
          eventOf = \case { Delegate -> ADelegate ; Revoke -> ARevoke }

      -- WHAT THE EVENT WOULD DO, ASKED BEFORE IT IS SIGNED. See 'pointless'.
      for_ (pointless kind (mnRepo mn) (mnKey mn) (frMaintainers fr)) $ \why ->
        liftIO (refuse (show (uselessly why (mnRepo mn))) codeNotDelegated)

      acc <- blessed codeNotDelegated
               (ownerEvent ctx (viewOf fr) now noOwnAttachments content)

      commit <- committing (oneStop codeNotDelegated) (mnDry mn) parent
                  (frMeta fr) [(eventPath acc, acEvent acc)] (numberIndexOf fr)
                  "hub: maintainer set" now

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
  -- Through 'repoFlags' and 'maintainerKeyFlags': see 'authorKeyFlags' for why
  -- --key could not stay one name for four different kinds of key.
  kvs  <- flagsAndSwitches (repoFlags <> maintainerKeyFlags) ["--dry-run"] syn
  repo <- flagRepo asKey kvs
  k    <- flagOneOf asKey maintainerKeyFlags kvs
  dry  <- flagSwitch kvs "--dry-run"
  pure (Maintainer repo k (writingOf dry))
  where
    asKey = \case { SignPubKeyLike v -> Just v ; _ -> Nothing }

-- | @--repo <key>@, and nothing else. See 'HBS2.Hub.CLI.Ban.banListArgs'.
maintainerListArgs :: forall c . IsContext c => [Syntax c] -> Maybe RepoRef
maintainerListArgs syn = flagsOf repoFlags syn >>= flagRepo asKey
  where
    asKey = \case { SignPubKeyLike v -> Just v ; _ -> Nothing }
