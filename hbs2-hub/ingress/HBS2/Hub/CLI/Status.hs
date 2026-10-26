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
import HBS2.Hub.Repo.GitBundle (remoteCanon,remoteNames,refTip)
import HBS2.Hub.Repo.Manifest (mailboxFor)
import HBS2.Hub.Ingress
import HBS2.Hub.CLI.Argv (badArgs,flagsAndSwitches,flagOnce,flagMaybe,flagText,flagSwitch
                         ,repoFlags,flagRepo)
import HBS2.Hub.CLI.Common (overRpc,signerFor,withCanon,OnMissing(..))
import HBS2.Hub.CLI.Signer (Signer(..),SignerSource(..),SignerTrouble,resolveSignerIn
                           ,signerEnvVar,signerDoc)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import Data.HashMap.Strict qualified as HM
import Data.Maybe (isJust)
import Data.HashSet qualified as HS
import Data.Text qualified as Text
import System.Environment (lookupEnv)

-- | Whether this clone's canon has reached one remote.
--
-- FOUR ANSWERS AND NOT TWO, because "not published" covers states a
-- maintainer would act on differently: the remote would not say, it holds
-- nothing, the two are level, and the remote holds something this clone does
-- not (which is a second maintainer's work and the case `hub sync` is for).
data Publication =
    RemoteSilent Text   -- ^ the remote would not answer, and what it said
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
  , stCanSign   :: Bool          -- ^ this machine can sign as 'stSigner'
    -- | The key a canon write without @--as@ would sign with, and where that
    -- came from ("HBS2.Hub.CLI.Signer"). 'Left' is a default that does not
    -- read, which every writer would refuse.
  , stSigner    :: Either SignerTrouble Signer
    -- | That key is the owner or a maintainer in canon here.
  , stMaintains :: Bool
  , stCanon     :: Maybe Text    -- ^ the canon commit here
  , stThreads   :: Int
  , stEvents    :: Int
  , stKeys      :: Int           -- ^ how many keys may bless canon
    -- | Each remote that was probed, by name, and what it answered.
    --
    -- PER REMOTE, because one answer without a name was wrong in the way that
    -- costs most: `hub publish --remote practice-forum` succeeded, and the
    -- status after it probed origin, said "the remote holds no canon at all"
    -- and did not say which remote. Ignored when there is no canon here, since
    -- there is nothing to have published. 'Left' is `git remote` failing,
    -- with what it said; an empty list is a clone with no remotes.
  , stPublished :: Either Text [(Text, Publication)]
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
  [ "repository" <+> keyDoc (stRepo st) ]
  <> signingLines
  <> canonLines
  <> publishedLines
  <> mailboxLines
  where
    -- WHICH KEY, and not only whether. A delegate whose machine also holds
    -- the repository key signed as the owner without --as, and this line
    -- said "yes" about the repository key, which was true and was the wrong
    -- question.
    signingLines = case stSigner st of
      Left e -> [ "signing  unknown:" <+> pretty e ]
      Right s ->
        [ "signing" <+> case (stCanSign st, sgSource s) of
            (True, RepoKeyDefault)  -> "yes: this machine holds the repository key"
            (True, _)               -> "yes, as" <+> signerDoc s
            (False, RepoKeyDefault) -> "no: this machine cannot author owner events for it"
            (False, _)              -> "no: this machine cannot sign as" <+> signerDoc s
                                         <> line <> "         so it cannot author owner events"
        ]
        <> [ "         that key is not a maintainer in canon here, so its events"
               <+> "would be refused"
           | not (stMaintains st) ]

    canonLines = case stCanon st of
      Nothing -> [ "canon    none here yet" ]
      Just c ->
        [ "canon" <+> pretty (safeText c)
        , "         " <> pretty (stThreads st) <+> "thread(s),"
            <+> pretty (stEvents st) <+> "event(s),"
            <+> pretty (stKeys st) <+> "key(s) may bless it"
        ]

    publishedLines = case (stCanon st, stPublished st) of
      (Nothing, _)  -> [ "published nothing to publish" ]
      (_, Left e)   -> [ "published unknown: could not list this clone's remotes"
                           <> line <> "  " <> pretty (safeText e) ]
      (_, Right []) -> [ "published not asked: this clone has no git remote" ]
      (_, Right rs) -> [ "published" <+> name r <> ":" <+> publishedLine r p
                       | (r, p) <- rs ]

    name = pretty . safeText

    publishedLine r = \case
      RemoteSilent e -> "unknown: the remote would not answer" <> line
                          <> "  " <> pretty (safeText e)
      RemoteEmpty    -> "NO: this remote holds no canon at all."
                          <+> "`hbs2-hub publish --remote" <+> name r <> "` sends it"
      Level          -> "yes: this remote holds this canon"
      -- NOT "behind": this clone may be ahead, behind, or forked, and the
      -- three are told apart by folding both -- which is `hub sync`'s job and
      -- not a status verb's. Saying which without doing that work would be a
      -- guess printed as a fact.
      Diverged theirs ->
        "NO: this remote holds canon this clone does not have"
          <> line <> "  remote:" <+> pretty (safeText theirs)
          <> line <> "  `hbs2-hub sync --remote" <+> name r
                  <+> "--repo <key>` folds both."

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
             <> line <> "The signing line names the key a canon write without --as"
             <> line <> "would use, and where it came from: HBS2_HUB_AS, git config"
             <> line <> "hbs2.hub.as, or the repository key. See `hbs2-hub help"
             <> line <> "maintainer use`."
             <> line
             <> line <> "Every absence is said out loud. A mailbox that would"
             <> line <> "not read says so rather than being left off the report,"
             <> line <> "because a missing line reads as 'nothing waiting' and"
             <> line <> "that is the answer somebody acts on by going away."
             <> line
             <> line <> "Without --remote every remote of this clone is probed"
             <> line <> "and each gets its own `published <remote>:` line, so a"
             <> line <> "`hbs2-hub publish --remote X` shows up under X. With"
             <> line <> "--remote only that one is probed. A probe is an"
             <> line <> "ls-remote, so it reads the remote's FETCH url: a remote"
             <> line <> "whose push urls go elsewhere as well is reported for"
             <> line <> "where it fetches from." )
    $ entry $ bindMatch "hub:status" $ nil_ \case
        (statusArgs -> Just sa) -> lift (status sa)
        other -> liftIO (badArgs statusUsage other)

  where

    status sa = do
      let repo = saRepo sa

      -- The keyring first, because it is the only question with no peer, no
      -- git and no remote in it: a report that says nothing else can still say
      -- whether this machine can sign. For the key a canon write WOULD use,
      -- which is the repository key only when nothing names another.
      envv <- liftIO (lookupEnv signerEnvVar)
      signer <- resolveSignerIn Nothing envv repo Nothing
      canSign <- either (const (pure False)) (fmap isJust . signerFor . sgKey) signer

      -- TreatAsEmpty, because "no canon here" is one of the answers this verb
      -- exists to give. Every other read verb refuses; this one reports.
      -- 'withCanon' and not 'withCanonState': the latter answers with a
      -- CanonState or exits, and "there is no canon here" is one of the six
      -- answers this verb exists to give.
      (mine, fr) <- withCanon TreatAsEmpty repo withGitCanon

      -- Every remote unless one was named. Not origin alone, which is what
      -- this did: `hub publish --remote X` succeeded and the status after it
      -- said "NO" about origin without naming it.
      let remotes = case saRemote sa of
            Just r  -> pure (Right [r])
            Nothing -> remoteNames Nothing
                         <&> either (Left . Text.pack . show . pretty) Right

      let probe c r = remoteCanon Nothing r <&> \case
            -- A remote that is not configured answers here, and it is not a
            -- failure of this verb: it is a fact about that remote.
            Left e -> RemoteSilent (Text.pack (show (pretty e)))
            Right Nothing -> RemoteEmpty
            Right (Just theirs)
              | theirs == c    -> Level
              | otherwise      -> Diverged theirs

      published <- case mine of
        Nothing -> pure (Right [])
        Just c  -> remotes >>= traverse (traverse (\r -> (r,) <$> probe c r))

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
          -- A FLOOR and not a count, whenever the read was incomplete: a hole
          -- in the tree or a walk that ran out of 'maxMailboxBlocks' hides
          -- letters as well as resurrecting settled ones. This verb prints one
          -- number per line by design; `hub inbox` is where a read that cannot
          -- be believed says so, with the hashes and a non-zero exit.
          tryAny (readInbox (overRpc sto api) k) <&> \case
            Left e  -> Left (Text.pack (show e))
            Right r -> Right (length (irLetters r) + irOmitted r + irDenied r)

      liftIO $ mapM_ print $ statusDoc Standing
        { stRepo = repo
        , stCanSign = canSign
        , stSigner = signer
        , stMaintains = either (const True)
                          (\s -> sgKey s == repo || HS.member (sgKey s) (frMaintainers fr))
                          signer
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
    -- | The remote to probe. 'Nothing' probes every remote of the clone.
  , saRemote :: Maybe Text
  }
  deriving stock (Eq,Show)

-- | @--repo <key> [--remote <name>]@.
statusArgs :: forall c . IsContext c => [Syntax c] -> Maybe StatusArgs
statusArgs syn = do
  kvs    <- flagsAndSwitches (repoFlags <> ["--remote"]) [] syn
  repo   <- flagRepo asKey kvs
  remote <- flagMaybe kvs "--remote" (fmap Text.pack . flagText)
  pure (StatusArgs repo remote)
  where
    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
