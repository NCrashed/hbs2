-- | Open a repository to contributions, in one step.
--
-- What it took before: a mailbox keyring, `hbs2-keyman update`, `hbs2-peer
-- mailbox create`, `hub policy default`, a sigil made and stored by hand, two
-- `hbs2-git3 repo:mailbox:*` calls to write both into the manifest -- and then
-- every accept said "no acknowledgement", because the repository key that
-- `repo:init` makes can sign and cannot encrypt, so nothing could be sealed back
-- to a contributor. Ten commands, and the result still could not answer anyone.
--
-- @hub project open@, in the clone of a repository this machine owns:
--
--   1. reuses the mailbox the manifest declares, or makes one (a new keyring in
--      hbs2-keyman, and the mailbox on this peer);
--   2. writes its policy when it has none: anyone may write, any peer may sync,
--      and @--pow N@ charges N bits of work per letter;
--   3. stores and announces the mailbox's sigil, and declares mailbox and
--      sigil in the repository manifest, through @hbs2-git3@, which owns that
--      file;
--   4. when an identity is set up (@hub identity init@), makes it a maintainer
--      and this clone's default signer, so accepts are signed by a key that CAN
--      seal an acknowledgement.
--
-- Every step that is already done is left alone, so it is safe to run again,
-- and a repository opened by hand before this existed is completed rather than
-- redone.
module HBS2.Hub.CLI.Project
  ( projectEntries
  , ProjectOpen(..)
  , projectOpenArgs
  ) where

import HBS2.Hub.Types (HubKey,HubScheme,RepoRef)
import HBS2.Hub.Ingress (rpcTimeout)
import HBS2.Hub.Repo.Manifest (readManifest,mailboxOf,sigilOf,announceSigil)
import HBS2.Hub.CLI.Argv (badArgs,flagsAndSwitches,flagSwitch,flagMaybe,flagWord,repoFlags,flagRepo)
import HBS2.Hub.CLI.Common (refuse,saying,signerFor,codePeerSilent,Writing(..))
import HBS2.Hub.CLI.Compose (codeNoKey,codeNotStored)
import HBS2.Hub.CLI.Defaults (authorEnvVar,authorGitKey)
import HBS2.Hub.CLI.Maintainer (maintainerWrite,maintainerUseIt,maintainerCanon,Maintainer(..),Who(..),MaintainerUse(..),Delegating(..))
import HBS2.Hub.CLI.Policy (readPolicy,submitPolicy,withDefaults,withPoW,policyText,policyLines)
import HBS2.Hub.Fold (frMaintainers)
import HBS2.Hub.Repo.Git (gitRun)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.KeyMan (keymanNewCredentials)

import HBS2.Base58 (AsBase58(..))
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil (makeSigilFromCredentials,storeSigil)
import HBS2.Peer.Proto.Mailbox.Types (MailboxType(..))
import HBS2.Peer.Proto.Mailbox.Policy.Basic (BasicPolicyAction(..),defaultBasicPolicy)
import HBS2.Peer.Proto.Mailbox.PoW (PoWDifficulty)
import HBS2.Peer.RPC.API.LWWRef (LWWRefAPI)
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.API.Peer (PeerAPI)
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Char (isSpace)
import Data.HashSet qualified as HS
import Data.List qualified as List
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.Process.Typed (proc,readProcess)

data ProjectOpen = ProjectOpen
  { poRepo :: RepoRef
  , poPoW  :: Maybe PoWDifficulty
  , poNoMaintainer :: Bool
  }
  deriving stock (Eq,Show)

projectOpenArgs :: forall c . IsContext c => [Syntax c] -> Maybe ProjectOpen
projectOpenArgs syn = do
  kvs  <- flagsAndSwitches (repoFlags <> ["--pow"]) ["--no-maintainer"] syn
  repo <- flagRepo (\case { SignPubKeyLike k -> Just k ; _ -> Nothing }) kvs
  pow  <- flagMaybe kvs "--pow" flagWord
  nom  <- flagSwitch kvs "--no-maintainer"
  pure (ProjectOpen repo (fromIntegral <$> pow) nom)

projectUsage :: Doc ()
projectUsage = "usage: hbs2-hub project open [--repo <key>] [--pow <bits>] [--no-maintainer]"

projectEntries :: forall c m . ( IsContext c
                               , MonadUnliftIO m
                               , HasStorage m
                               , HasClientAPI MailboxAPI UNIX m
                               , HasClientAPI LWWRefAPI UNIX m
                               , HasClientAPI PeerAPI UNIX m
                               , Exception (BadFormException c)
                               ) => MakeDictM c m ()
projectEntries = do

  brief "open a repository you own to issues and pull requests, in one step"
    $ args [ arg "string" "[--repo repo-key]", arg "string" "[--pow bits]"
           , arg "string" "[--no-maintainer]" ]
    $ desc ( "Run in the clone of a repository this machine holds the key of."
             <> line <> "It makes (or reuses) the project's mailbox, lets anyone"
             <> line <> "write to it, stores its sigil, and declares both in the"
             <> line <> "repository manifest, so a contributor needs only the"
             <> line <> "repository key. --pow N charges N bits of work per letter."
             <> line
             <> line <> "With an identity set up (`hub identity init`), it also"
             <> line <> "makes that identity a maintainer and this clone's default"
             <> line <> "signer: the repository key cannot encrypt, so without this"
             <> line <> "no contributor ever gets an acknowledgement."
             <> line <> "--no-maintainer skips that step. It writes canon, so run"
             <> line <> "`hub publish` afterwards."
             <> line
             <> line <> "Safe to run again: whatever is already in place is kept." )
    $ entry $ bindMatch "hub:project:open" $ nil_ \case
        (projectOpenArgs -> Just po) -> lift (openIt po)
        other -> liftIO (badArgs projectUsage other)

  where

    openIt ProjectOpen{..} = do
      let repo = poRepo
          b58 :: HubKey -> Doc ann
          b58 = pretty . AsBase58

      void $ signerFor repo
        >>= maybe (liftIO (refuse (show ( "this machine does not hold the key of"
                                            <+> b58 repo <> line
                                            <> "  only the owner opens a repository; a contributor"
                                            <+> "needs nothing opened" ))
                                  codeNoKey)) pure

      manifest <- readManifest repo <&> either (const []) id

      -- 1. The mailbox: the declared one if it is ours, else a new one.
      (mbox, mboxNew) <- case mailboxOf manifest of
        Just m -> signerFor m >>= \case
          Just _  -> pure (m, False)
          Nothing -> liftIO $ refuse
            ( show ( "the manifest declares mailbox" <+> b58 m
                       <+> "and this machine does not hold its key" <> line
                       <> "  open it where that key is, or take the declaration back with"
                       <+> "`hbs2-git3 repo:mailbox:drop`" ) )
            codeNoKey
        Nothing -> keymanNewCredentials (Just "hbs2-hub-mailbox") 1 <&> (, True)

      api <- getClientAPI @MailboxAPI @UNIX
      callRpcWaitMay @RpcMailboxCreate rpcTimeout api (mbox, MailboxHub) >>= \case
        Nothing -> liftIO (refuse "hbs2-peer did not answer: is it running?" codePeerSilent)
        Just (Left e) -> liftIO (refuse (show ("the peer would not create the mailbox:" <+> viaShow e)) 1)
        Just (Right ()) -> pure ()

      -- 2. The policy: written when there is none, re-priced when asked.
      policySaid <- readPolicy mbox >>= \case
        Left e -> liftIO (refuse (show (pretty e)) 1)
        Right Nothing -> do
          let p = maybe id withPoW poPoW
                    (withDefaults (Just Allow) (Just Allow) (defaultBasicPolicy @HBS2Basic))
          void (submitPolicy mbox 1 p)
          pure ("written:" <> line <> policyLines (policyText p))
        Right (Just (v, p)) -> case poPoW of
          Just bits | withPoW bits p /= p -> do
            void (submitPolicy mbox (succ v) (withPoW bits p))
            pure (if bits == 0 then "charges no proof-of-work now"
                               else "now charges" <+> pretty bits <+> "bits of work")
          _ -> pure "kept as it was"

      -- 3. The sigil, and both declarations.
      mcreds <- signerFor mbox
      creds <- maybe (liftIO (refuse "hbs2-keyman does not report the mailbox key yet" codeNoKey)) pure mcreds
      enc <- case _peerKeyring creds of
        (KeyringEntry pk _ _ : _) -> pure pk
        [] -> liftIO (refuse "the mailbox keyring has no encryption key" codeNoKey)
      sto <- getStorage
      sigil <- maybe (liftIO (refuse "could not make the mailbox sigil" codeNoKey)) pure
                 (makeSigilFromCredentials @HubScheme creds enc Nothing Nothing)
      href <- storeSigil sto sigil
                >>= maybe (liftIO (refuse "the peer would not store the sigil" codeNotStored)) pure
      announceSigil href >>= \case
        True  -> pure ()
        False -> liftIO (refuse "the peer stored the sigil but did not answer its announce request" codePeerSilent)

      when (mailboxOf manifest /= Just mbox) $
        hbs2git3 ["repo:mailbox:set", key58 repo, "--key", key58 mbox]
      when (sigilOf mbox manifest /= Just href) $
        hbs2git3 ["repo:mailbox:sigil", key58 repo, "--key", key58 mbox, "--sigil", show (pretty href)]

      -- 4. A signer that can seal an acknowledgement.
      ackSaid <- if poNoMaintainer then pure "not set up (--no-maintainer)" else do
        ident <- identity
        case ident of
          Nothing -> pure ( "not set up: run `hbs2-hub identity init`, then this again,"
                              <+> "so contributors get replies" )
          Just me | me == repo -> pure "signed by the repository key"
          Just me -> do
            (_, fr) <- maintainerCanon repo
            unless (HS.member me (frMaintainers fr)) $
              maintainerWrite Delegate (Maintainer repo (WhoKey me) ForReal)
            maintainerUseIt (UseKey repo me)
            pure ("signed by your identity" <+> b58 me <+> "(a maintainer now)")

      liftIO $ print $ vcat
        [ "repository" <+> b58 repo
        , "mailbox   " <+> b58 mbox <+> (if mboxNew then "(new)" else "(already there)")
        , "sigil     " <+> pretty href
        , "policy    " <+> policySaid
        , "replies   " <+> ackSaid
        , mempty
        , "Contributors need only the repository key:"
        , "  git clone hbs23://" <> b58 repo
        , "Then `hbs2-hub publish` here, and `hbs2-hub inbox --follow` to watch for letters."
        ]

    key58 :: HubKey -> String
    key58 = show . pretty . AsBase58

    identity = do
      envv <- liftIO (lookupEnv authorEnvVar)
      conf <- gitRun Nothing [] 10 4096 "config" ["config", "--get", authorGitKey] mempty <&> \case
        Right (ExitSuccess, out, _) -> Just (List.dropWhileEnd isSpace (BS8.unpack out))
        _ -> Nothing
      pure (List.find (not . all isSpace) (catMaybesL [envv, conf]) >>= fromStringMay @HubKey)

    catMaybesL = List.foldr (maybe id (:)) []

    -- hbs2-git3 owns the manifest: it holds the reflog key logic and the format,
    -- and a second writer here would be a second answer to both.
    hbs2git3 args = do
      (code, out, err) <- liftIO (readProcess (proc "hbs2-git3" args))
      case code of
        ExitSuccess -> liftIO (saying (pretty (LBS8.unpack out)))
        ExitFailure n -> liftIO $ refuse
          ( show ( "hbs2-git3" <+> hsep (fmap pretty args) <+> "exited" <+> pretty n <> line
                     <> pretty (LBS8.unpack err) ) )
          1
