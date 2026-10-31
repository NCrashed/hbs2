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
  , Replication(..)
  , statusDoc
  , statusContract
  , statusOrder
  , StatusArgs(..)
  , statusArgs
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold (FoldResult(..),frMaintainers)
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.Repo.GitBundle (remoteCanon,remoteNames)
import HBS2.Hub.Repo.Manifest (readManifest,mailboxOf)
import HBS2.Hub.Manifest (replicationMinimum)
import HBS2.Hub.Ingress
import HBS2.Hub.CLI.Argv (badArgs,flagsAndSwitches,flagMaybe,flagText,flagSwitch
                         ,repoFlags,flagRepo)
import HBS2.Hub.CLI.Common (overRpc,signerFor,withCanon,OnMissing(..))
import HBS2.Hub.CLI.Signer (Signer(..),SignerSource(..),SignerTrouble,resolveSignerIn
                           ,signerEnvVar,signerDoc)
import HBS2.Hub.Render (contractVersion,renderDocument,key58)
import HBS2.Hub.CLI.Defaults (currentBranch,remoteHeadBranch,localBranches)
import Data.Aeson.KeyMap qualified as KM

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Peer.Proto.RepositoryLease (ReplicaAdvertV1(..))
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import Data.Aeson (Value(..),object,(.=),toJSON)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.Maybe (isJust)
import Data.HashSet qualified as HS
import Data.Text qualified as Text
import Data.Word (Word64)
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

-- | What one bounded provider lookup can prove about repository replication.
--
-- The peer RPC deliberately returns one usable provider, not the entire
-- advert set.  'ReplicationReady' therefore proves a lower bound of one; it
-- must never be presented as the total number of replicas in the network.
data Replication =
    ReplicationUnknown Text
      -- ^ The local peer did not answer, so even a zero lower bound is unknown.
  | ReplicationAtRisk
      -- ^ Lookup completed without a reachable, verified, live advert.
  | ReplicationReady
      { replicationProvider     :: HubKey
      , replicationSnapshot     :: HashRef
      , replicationIssuedAt     :: Word64
      , replicationExpiresAt    :: Word64
      , replicationReachableVia :: [Text]
      }
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
  , stReplicationTarget :: Maybe Word64
  , stReplication :: Replication
  }
  deriving stock (Eq,Show)

statusUsage :: Doc ()
statusUsage = "usage: hbs2-hub status --repo <key> [--remote <name>] [--json] [--local]"

-- | The report.
statusDoc :: Standing -> [Doc ann]
statusDoc st =
  [ "repository" <+> keyDoc (stRepo st) ]
  <> signingLines
  <> canonLines
  <> publishedLines
  <> mailboxLines
  <> replicationLines
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

    targetLine = case stReplicationTarget st of
      Nothing -> "replication target not declared"
      Just n  -> "replication target" <+> pretty n <+> "complete external replica(s)"

    replicationLines = targetLine : case stReplication st of
      ReplicationUnknown e ->
        [ "replication unknown: the local peer did not answer"
        , "            " <> pretty (safeText e)
        ]
      ReplicationAtRisk
        | stReplicationTarget st == Just 0 ->
            [ "replication target satisfied: no external replica is required" ]
        | otherwise ->
            [ "replication at risk: no reachable verified replica advert was found" ]
      ReplicationReady{..} ->
        [ if maybe False (> 1) (stReplicationTarget st)
            then "replication at risk: at least 1 verified complete replica, below target"
            else "replication at least 1 verified complete replica"
        , "            provider" <+> keyDoc replicationProvider
        , "            advert valid from" <+> pretty replicationIssuedAt
            <+> "until" <+> pretty replicationExpiresAt
        ]

-- | The standing as a document (@hub status --json@).
--
-- Not derived from canon alone: the publication and the mailbox are answers
-- from a remote and a peer at one moment. Every fact has a key whatever the
-- answer was, and an absence is a null beside an @error@ string rather than a
-- key left out, for the reason the report says them out loud: a missing
-- @waiting@ reads as nothing waiting.
--
-- @published@ is null when there is no canon here to have published, which is
-- a different answer from an empty list of remotes.
--
-- @signing@ is whether this machine can sign as the key in @signer@, which is
-- the key a canon write without @--as@ would use and where it came from
-- (@env@, @git_config@ or @repo_key@, "HBS2.Hub.CLI.Signer"). A default that
-- does not read leaves the key null and says why in @error@.
statusContract :: Standing -> Value
statusContract st = object
  [ "contract" .= contractVersion
  , "document" .= ("status" :: Text)
  , "repo"     .= key58 (stRepo st)
  , "signing"  .= stCanSign st
  , "signer"   .= case stSigner st of
      Left e  -> object [ "key" .= Null, "source" .= Null, "maintainer" .= Null
                        , "error" .= Text.pack (show (pretty e)) ]
      Right s -> object [ "key" .= key58 (sgKey s)
                        , "source" .= sourceName (sgSource s)
                        , "maintainer" .= stMaintains st
                        , "error" .= Null ]
  , "canon"    .= object
      [ "commit"      .= stCanon st
      , "threads"     .= stThreads st
      , "events"      .= stEvents st
      , "maintainers" .= stKeys st
      ]
  , "published" .= case (stCanon st, stPublished st) of
      (Nothing, _) -> Null
      (_, Left e)  -> object [ "error" .= e, "remotes" .= ([] :: [Value]) ]
      (_, Right rs) -> object [ "error" .= Null
                              , "remotes" .= [ remote r p | (r, p) <- rs ] ]
  , "mailbox" .= object
      [ "key"     .= fmap key58 (stMailbox st)
      , "waiting" .= case (stMailbox st, stWaiting st) of
          (Nothing, _)     -> Null
          (_, Right n)     -> toJSON n
          (_, Left _)      -> Null
      , "error"   .= case (stMailbox st, stWaiting st) of
          (Just _, Left e) -> Just e
          _                -> Nothing
      ]
  , "replication" .= replicationContract (stReplicationTarget st) (stReplication st)
  ]
  where
    sourceName :: SignerSource -> Text
    sourceName = \case
      FromFlag       -> "flag"
      FromEnv        -> "env"
      FromGitConfig  -> "git_config"
      RepoKeyDefault -> "repo_key"

    remote r p = object $ ("name" .= r) : case p of
      RemoteSilent e -> [ "state" .= ("silent" :: Text), "remote_canon" .= Null, "error" .= e ]
      RemoteEmpty    -> [ "state" .= ("empty" :: Text),  "remote_canon" .= Null, "error" .= Null ]
      Level          -> [ "state" .= ("level" :: Text),  "remote_canon" .= stCanon st, "error" .= Null ]
      Diverged theirs -> [ "state" .= ("diverged" :: Text), "remote_canon" .= theirs, "error" .= Null ]

    replicationContract target replication =
      let (minimumReplica, provider, snapshot, issued, expires, endpoints, atRisk, failure) =
            case replication of
              ReplicationUnknown e ->
                (Null, Null, Null, Null, Null, [], Null, toJSON e)
              ReplicationAtRisk ->
                (toJSON (0 :: Int), Null, Null, Null, Null, []
                , Bool (maybe True (> 0) target), Null)
              ReplicationReady{..} ->
                ( toJSON (1 :: Int)
                , toJSON (key58 replicationProvider)
                , toJSON (show (pretty replicationSnapshot))
                , toJSON replicationIssuedAt
                , toJSON replicationExpiresAt
                , fmap toJSON replicationReachableVia
                , Bool (maybe False (> 1) target)
                , Null
                )
      in object
        [ "target" .= target
        , "complete_replicas_minimum" .= minimumReplica
        , "provider" .= provider
        , "snapshot" .= snapshot
        , "freshness" .= object [ "issued_at" .= issued, "expires_at" .= expires ]
        , "reachable_via" .= endpoints
        , "observed_availability" .= Null
        , "at_risk" .= atRisk
        , "error" .= failure
        ]

statusOrder :: [Text]
statusOrder =
  [ "repo", "signing", "signer", "key", "source", "maintainer"
  , "canon", "commit", "threads", "events", "maintainers"
  , "published", "remotes", "name", "state", "remote_canon"
  , "mailbox", "waiting", "error"
  , "replication", "target", "complete_replicas_minimum", "provider", "snapshot"
  , "freshness", "issued_at", "expires_at", "reachable_via"
  , "observed_availability", "at_risk"
  , "clone", "branch", "onto", "branches" ]

statusEntries :: forall c m . ( IsContext c
                              , MonadUnliftIO m
                              , HasStorage m
                              , HasClientAPI MailboxAPI UNIX m
                              , HasClientAPI LWWRefAPI UNIX m
                              , HasClientAPI PeerAPI UNIX m
                              , Exception (BadFormException c)
                              ) => MakeDictM c m ()
statusEntries = do

  brief "say where this clone stands: canon, publication, mailbox"
    $ args [ arg "string" "--repo repo-key", arg "string" "[--remote name]"
           , arg "string" "[--json]", arg "string" "[--local]" ]
    $ desc ( "Read-only, and it pushes nothing: the publication check is a"
             <> line <> "probe of the remote, not a push that reports what it"
             <> line <> "would have done."
             <> line
             <> line <> "--local asks no remote: the signer, canon and the clone only,"
             <> line <> "and the mailbox as the peer holds it, without a fetch."
             <> line
             <> line <> "Seven facts, each of which was computable and none of"
             <> line <> "which was askable: the repository, whether this machine"
             <> line <> "can sign for it, what canon here holds, whether that"
             <> line <> "canon has reached the remote, which mailbox the"
             <> line <> "repository declares, how many letters are in it, and"
             <> line <> "whether a reachable verified replica is advertised."
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
             <> line <> "where it fetches from."
             <> line
             <> line <> "--json prints one `status` document (contract 1) with the"
             <> line <> "same facts; an absence is a null or an error string there,"
             <> line <> "never a missing key." )
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
        Just _ | saLocal sa -> pure (Left "not asked (--local)")
        Just c  -> remotes >>= traverse (traverse (\r -> (r,) <$> probe c r))

      -- The mailbox and placement target are signed clauses in the same
      -- manifest. Read it once: apart from avoiding a duplicate network read,
      -- this keeps status from showing fields from two different revisions.
      manifest <- readManifest repo
      let mbox = either (const Nothing) mailboxOf manifest
          replicationTarget = either (const Nothing) replicationMinimum manifest

      -- The mailbox the repository declares, and then the queue in it. Both
      -- can fail on their own and each failure is its own sentence: a
      -- repository that declares no mailbox is a state PEP-18 allows, and a
      -- peer that will not answer is a different thing entirely.
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
          -- As the peer holds it under --local: the honest read fetches and
          -- waits a round for the copy to settle, 2.5 s a page waits on.
          tryAny ((if saLocal sa then readInboxHeld else readInbox) (overRpc sto api) k) <&> \case
            Left e  -> Left (Text.pack (show e))
            Right r -> Right (length (irLetters r) + irOmitted r + irDenied r)

      replication <- if saLocal sa
        then pure (ReplicationUnknown "not asked (--local)")
        else do
          api <- getClientAPI @PeerAPI @UNIX
          callRpcWaitMay @RpcRepositoryAdvertFind rpcTimeout api repo <&> \case
            Nothing -> ReplicationUnknown "repository advert lookup timed out"
            Just Nothing -> ReplicationAtRisk
            Just (Just advert) -> replicationFromAdvert advert

      let st = Standing
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
              , stReplicationTarget = replicationTarget
              , stReplication = replication
              }

      -- THE CLONE ITSELF, for the JSON only: what a form proposing a pull
      -- request offers to pick from (PEP-24). Kept out of 'Standing', which
      -- is about canon and its publication, and added to the document as
      -- one more object, which readers that do not know it ignore.
      clone <- if not (saJson sa) then pure Null else do
        here <- currentBranch
        onto <- remoteHeadBranch
        bs <- localBranches
        pure $ object [ "branch" .= fmap Text.pack here
                      , "onto" .= fmap Text.pack onto
                      , "branches" .= fmap Text.pack bs ]

      let withClone = \case
            Object o -> Object (KM.insert "clone" clone o)
            v -> v

      liftIO $ if saJson sa
        then LBS.putStr (renderDocument statusOrder (withClone (statusContract st)))
        else mapM_ print (statusDoc st)

    replicationFromAdvert ReplicaAdvertV1{..} = ReplicationReady
      { replicationProvider = replicaAdvertProvider
      , replicationSnapshot = replicaAdvertSnapshotId
      , replicationIssuedAt = replicaAdvertIssuedAt
      , replicationExpiresAt = replicaAdvertExpiresAt
      , replicationReachableVia =
          fmap (Text.pack . show . pretty) replicaAdvertReachableVia
      }

-- | What one @hub status@ was asked about.
data StatusArgs = StatusArgs
  { saRepo   :: RepoRef
    -- | The remote to probe. 'Nothing' probes every remote of the clone.
  , saRemote :: Maybe Text
    -- | The standing as a document. See 'statusContract'.
  , saJson   :: Bool
    -- | Ask no remote. For a caller that wants the signer and the clone and
    -- must not wait on the network for them: the browser asks on every page.
  , saLocal  :: Bool
  }
  deriving stock (Eq,Show)

-- | @--repo <key> [--remote <name>]@.
statusArgs :: forall c . IsContext c => [Syntax c] -> Maybe StatusArgs
statusArgs syn = do
  kvs    <- flagsAndSwitches (repoFlags <> ["--remote"]) ["--json","--local"] syn
  repo   <- flagRepo asKey kvs
  remote <- flagMaybe kvs "--remote" (fmap Text.pack . flagText)
  asJson <- flagSwitch kvs "--json"
  localOnly <- flagSwitch kvs "--local"
  pure (StatusArgs repo remote asJson localOnly)
  where
    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
