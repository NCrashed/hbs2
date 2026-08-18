-- | @hub status@: where this clone stands (PEP-22).
--
-- WHY THIS EXISTS. Every fact in the report below was already computable and
-- none of it was askable: which repository this is, whether this machine can
-- sign for it, what canon here holds, whether that canon has been published,
-- which mailbox the repository declares and how many letters are waiting in it.
-- Five verbs print "not published yet" as a warning after doing something else,
-- which is the one of these a maintainer most often wants BEFORE doing
-- anything, and there was no way to ask.
--
-- IT WRITES NOTHING AND PUSHES NOTHING. The published check is a read-only
-- probe of the remote ('remoteCanon'), split out of `hub publish` for exactly
-- this: a status verb that pushed to find out whether it needed to would be a
-- status verb nobody could run twice.
--
-- EVERY LINE IS A FACT OR AN ABSENCE, and the absences are said out loud. A
-- report that silently omitted the mailbox line when the peer was unreachable
-- would read as "no letters waiting", which is the answer a maintainer acts on
-- by going away.
module HBS2.Hub.CLI.Status
  ( statusEntries
  , statusUsage
    -- * The parts that decide something
  , Standing(..)
  , Publication(..)
  , statusDoc
  , StatusArgs(..)
  , statusArgs
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold (FoldResult(..),frMaintainers)
import HBS2.Hub.Repo (readCanon,stCommit,stFold)
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.Repo.GitBundle (remoteCanon,refTip)
import HBS2.Hub.Repo.Manifest (mailboxFor)
import HBS2.Hub.Ingress
import HBS2.Hub.CLI.Argv (badArgs,flagsAndSwitches,flagOnce,flagMaybe,flagText,flagSwitch
                         ,repoFlags,flagRepo)
import HBS2.Hub.CLI.Common (overRpc,signerFor,withCanon,OnMissing(..))

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import Data.HashMap.Strict qualified as HM
import Data.Maybe (isJust,fromMaybe)
import Data.HashSet qualified as HS
import Data.Text qualified as Text

-- | Whether this clone's canon has reached the remote.
--
-- FOUR ANSWERS AND NOT TWO, because "not published" covers three states a
-- maintainer would act on differently: nothing folded here yet, the remote is
-- level, this clone is ahead, and the remote holds something this clone does
-- not (which is a second maintainer's work and the case `hub sync` is for).
data Publication =
    NothingHere         -- ^ no canon in this repository
  | NotAsked            -- ^ no remote was named, so nothing was probed
  | RemoteSilent Text   -- ^ the remote would not answer, and what it said
  | RemoteEmpty         -- ^ the remote holds no canon
  | Level               -- ^ the two agree
  | Diverged Text       -- ^ the remote holds something else, and what
  deriving stock (Eq,Show)

-- | Everything the report is a function of.
--
-- A record and not a pile of arguments, because the verb gathers these from
-- five different places -- a keyring, git, a remote, a manifest and a peer --
-- and the rendering must be assertable without any of them.
data Standing = Standing
  { stRepo      :: RepoRef
  , stCanSign   :: Bool          -- ^ this machine holds the repository's own key
  , stCanon     :: Maybe Text    -- ^ the canon commit here
  , stThreads   :: Int
  , stEvents    :: Int
  , stKeys      :: Int           -- ^ how many keys may bless canon
  , stPublished :: Publication
    -- | The declared mailbox, and what reading it answered.
    --
    -- 'Nothing' for the mailbox means the repository declares none, which
    -- PEP-18 allows; 'Left' means the read failed and carries what to say.
  , stMailbox   :: Maybe HubKey
  , stWaiting   :: Either Text Int
  }
  deriving stock (Eq,Show)

statusUsage :: Doc ()
statusUsage = "usage: hbs2-hub status --repo <key> [--remote <name>]"

-- | The report.
statusDoc :: Standing -> [Doc ann]
statusDoc st =
  [ "repository" <+> keyDoc (stRepo st)
  , "signing" <+> if stCanSign st
                    then "yes: this machine holds the repository key"
                    else "no: this machine cannot author owner events for it"
  ]
  <> canonLines
  <> [ "published" <+> publishedLine ]
  <> mailboxLines
  where
    canonLines = case stCanon st of
      Nothing -> [ "canon    none here yet" ]
      Just c ->
        [ "canon" <+> pretty (safeText c)
        , "         " <> pretty (stThreads st) <+> "thread(s),"
            <+> pretty (stEvents st) <+> "event(s),"
            <+> pretty (stKeys st) <+> "key(s) may bless it"
        ]

    publishedLine = case stPublished st of
      NothingHere    -> "nothing to publish"
      NotAsked       -> "not asked: name a remote with --remote"
      RemoteSilent e -> "unknown: the remote would not answer" <> line
                          <> "  " <> pretty (safeText e)
      RemoteEmpty    -> "NO: the remote holds no canon at all."
                          <+> "`hbs2-hub publish` sends it"
      Level          -> "yes: the remote holds this canon"
      -- NOT "behind": this clone may be ahead, behind, or forked, and the
      -- three are told apart by folding both -- which is `hub sync`'s job and
      -- not a status verb's. Saying which without doing that work would be a
      -- guess printed as a fact.
      Diverged theirs ->
        "NO: the remote holds canon this clone does not have"
          <> line <> "  remote:" <+> pretty (safeText theirs)
          <> line <> "  `hbs2-hub sync --repo <key>` folds both."

    mailboxLines = case stMailbox st of
      Nothing -> [ "mailbox  none declared: nobody outside can file anything here" ]
      Just k ->
        [ "mailbox" <+> keyDoc k ]
        <> case stWaiting st of
             Left e  -> [ "         not read:" <+> pretty (safeText e) ]
             Right 0 -> [ "         nothing waiting" ]
             Right n -> [ "        " <+> pretty n <+> "letter(s) waiting"
                            <+> parens "hbs2-hub inbox" ]

statusEntries :: forall c m . ( IsContext c
                              , MonadUnliftIO m
                              , HasStorage m
                              , HasClientAPI MailboxAPI UNIX m
                              , HasClientAPI LWWRefAPI UNIX m
                              , Exception (BadFormException c)
                              ) => MakeDictM c m ()
statusEntries = do

  brief "say where this clone stands: canon, publication, mailbox"
    $ args [ arg "string" "--repo repo-key", arg "string" "[--remote name]" ]
    $ desc ( "Read-only, and it pushes nothing: the publication check is a"
             <> line <> "probe of the remote, not a push that reports what it"
             <> line <> "would have done."
             <> line
             <> line <> "Six facts, each of which was computable and none of"
             <> line <> "which was askable: the repository, whether this machine"
             <> line <> "can sign for it, what canon here holds, whether that"
             <> line <> "canon has reached the remote, which mailbox the"
             <> line <> "repository declares, and how many letters are in it."
             <> line
             <> line <> "Every absence is said out loud. A mailbox that would"
             <> line <> "not read says so rather than being left off the report,"
             <> line <> "because a missing line reads as 'nothing waiting' and"
             <> line <> "that is the answer somebody acts on by going away."
             <> line
             <> line <> "--remote defaults to origin. Without a remote of that"
             <> line <> "name the publication line says it was not asked, which"
             <> line <> "is the honest answer and not a failure." )
    $ entry $ bindMatch "hub:status" $ nil_ \case
        (statusArgs -> Just sa) -> lift (status sa)
        other -> liftIO (badArgs statusUsage other)

  where

    status sa = do
      let repo = saRepo sa

      -- The keyring first, because it is the only question with no peer, no
      -- git and no remote in it: a report that says nothing else can still say
      -- whether this machine is the owner.
      canSign <- isJust <$> signerFor repo

      -- TreatAsEmpty, because "no canon here" is one of the answers this verb
      -- exists to give. Every other read verb refuses; this one reports.
      -- 'withCanon' and not 'withCanonState': the latter answers with a
      -- CanonState or exits, and "there is no canon here" is one of the six
      -- answers this verb exists to give.
      (mine, fr) <- withCanon TreatAsEmpty repo withGitCanon

      published <- case mine of
        Nothing -> pure NothingHere
        Just c -> remoteCanon Nothing (saRemote sa) >>= \case
          -- A remote that is not configured answers here, and it is not a
          -- failure of this verb: a clone with no remote is a clone nobody has
          -- published from yet.
          Left e -> pure (RemoteSilent (Text.pack (show (pretty e))))
          Right Nothing -> pure RemoteEmpty
          Right (Just theirs)
            | theirs == c    -> pure Level
            | otherwise      -> pure (Diverged theirs)

      -- The mailbox the repository declares, and then the queue in it. Both
      -- can fail on their own and each failure is its own sentence: a
      -- repository that declares no mailbox is a state PEP-18 allows, and a
      -- peer that will not answer is a different thing entirely.
      mbox <- mailboxFor Nothing repo <&> either (const Nothing) Just

      waiting <- case mbox of
        Nothing -> pure (Right 0)
        Just k -> do
          sto <- getStorage
          api <- getClientAPI @MailboxAPI @UNIX
          tryAny (readInbox (overRpc sto api) k) <&> \case
            Left e  -> Left (Text.pack (show e))
            Right r -> Right (length (irLetters r) + irOmitted r + irDenied r)

      liftIO $ mapM_ print $ statusDoc Standing
        { stRepo = repo
        , stCanSign = canSign
        , stCanon = mine
        , stThreads = HM.size (frThreads fr)
        , stEvents = length (frLog fr)
        , stKeys = HS.size (frMaintainers fr)
        , stPublished = published
        , stMailbox = mbox
        , stWaiting = waiting
        }

-- | What one @hub status@ was asked about.
data StatusArgs = StatusArgs
  { saRepo   :: RepoRef
    -- | The remote to probe. Defaults to @origin@, which is what a clone has.
  , saRemote :: Text
  }
  deriving stock (Eq,Show)

-- | @--repo <key> [--remote <name>]@.
statusArgs :: forall c . IsContext c => [Syntax c] -> Maybe StatusArgs
statusArgs syn = do
  kvs    <- flagsAndSwitches (repoFlags <> ["--remote"]) [] syn
  repo   <- flagRepo asKey kvs
  remote <- flagMaybe kvs "--remote" (fmap Text.pack . flagText)
  pure (StatusArgs repo (fromMaybe "origin" remote))
  where
    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
