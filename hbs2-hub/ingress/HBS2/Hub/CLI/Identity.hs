-- | Who you are on a forge, set up once.
--
-- Writing a letter takes three things that belong together: a signing key, a
-- sigil naming that key's encryption key (so an answer can be sealed to you),
-- and a mailbox under the same key for the answers to land in. Making them was
-- seven commands across three tools, and then each of the forty-four
-- character values had to be typed again on every letter.
--
-- @hub identity init@ does all of it and remembers the result in the global git
-- config, which is where 'HBS2.Hub.CLI.Defaults' looks for --author and
-- --sender. Run again, it changes nothing that is already right: the sigil of
-- one key is the same block every time, and creating a mailbox that exists is
-- not an error.
module HBS2.Hub.CLI.Identity
  ( identityEntries
  , IdentityArgs(..)
  , identityArgs
  ) where

import HBS2.Hub.Types (HubKey,HubScheme)
import HBS2.Hub.Ingress (rpcTimeout)
import HBS2.Hub.Repo.Git (gitRun)
import HBS2.Hub.Repo.Manifest (announceSigil)
import HBS2.Hub.CLI.Argv (badArgs,flagsOf,flagMaybe)
import HBS2.Hub.CLI.Common (refuse,signerFor,codePeerSilent)
import HBS2.Hub.CLI.Compose (codeNoKey,codeNotStored)
import HBS2.Hub.CLI.Defaults (authorGitKey,senderGitKey)
import HBS2.Hub.CLI.Say (saying)
import HBS2.Hub.CLI.Policy (readPolicy,submitPolicy,withDefaults)
import HBS2.Peer.Proto.Mailbox.Policy.Basic (BasicPolicyAction(..),defaultBasicPolicy)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.KeyMan (keymanNewCredentials)

import HBS2.Base58 (AsBase58(..))
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil (makeSigilFromCredentials,storeSigil)
import HBS2.Peer.Proto.Mailbox.Types (MailboxType(..))
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.API.Peer (PeerAPI)
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import Data.ByteString.Char8 qualified as BS8
import Data.Char (isSpace)
import Data.List qualified as List
import System.Exit (ExitCode(..))
import Control.Applicative ((<|>))

-- | @[--author <key>]@: which key to be, when it should not be a new one.
newtype IdentityArgs = IdentityArgs { idAuthor :: Maybe HubKey }
  deriving stock (Eq,Show)

identityArgs :: forall c . IsContext c => [Syntax c] -> Maybe IdentityArgs
identityArgs syn = do
  kvs <- flagsOf ["--author"] syn
  IdentityArgs <$> flagMaybe kvs "--author" (\case { SignPubKeyLike k -> Just k ; _ -> Nothing })

identityUsage :: Doc ()
identityUsage = "usage: hbs2-hub identity init [--author <key>]"

identityEntries :: forall c m . ( IsContext c
                                , MonadUnliftIO m
                                , HasStorage m
                                , HasClientAPI MailboxAPI UNIX m
                                , HasClientAPI PeerAPI UNIX m
                                , Exception (BadFormException c)
                                ) => MakeDictM c m ()
identityEntries = do

  brief "set up who you are: a key, its sigil and a mailbox, remembered as your defaults"
    $ args [ arg "string" "[--author key]" ]
    $ desc ( "Everything a letter needs from its sender, in one step:"
             <> line <> "a signing key (a new one, the one you already set up, or"
             <> line <> "--author), the sigil that lets a maintainer answer you,"
             <> line <> "and a mailbox on this peer for the answers. Then it"
             <> line <> "writes git config --global" <+> pretty authorGitKey
             <+> "and" <+> pretty senderGitKey <> ","
             <> line <> "so `hub issue new` and `hub pr new` in any clone need"
             <> line <> "no --author and no --sender."
             <> line
             <> line <> "Safe to run again: it reuses the key already set up,"
             <> line <> "and the sigil and the mailbox come out the same." )
    $ entry $ bindMatch "hub:identity:init" $ nil_ \case
        (identityArgs -> Just ia) -> lift (initIdentity ia)
        other -> liftIO (badArgs identityUsage other)

  where

    initIdentity IdentityArgs{..} = do

      -- Which key: the one named, else the one already remembered, else new.
      remembered <- globalConfig authorGitKey <&> (>>= fromStringMay @HubKey)
      (author, made) <- case idAuthor <|> remembered of
        Just k  -> pure (k, False)
        Nothing -> do
          k <- keymanNewCredentials (Just "hbs2-hub") 1
          pure (k, True)

      creds <- signerFor author
                 >>= maybe (liftIO (refuse (show (noKey author made)) codeNoKey)) pure

      -- The ENCRYPTION key is the half a sigil exists to publish. A keyring
      -- with none can sign letters that nobody can ever answer.
      enc <- case _peerKeyring creds of
        (KeyringEntry pk _ _ : _) -> pure pk
        [] -> liftIO $ refuse
                ( show ( "the keyring of" <+> pretty (AsBase58 author)
                           <+> "has no encryption key, so nothing could be sealed to you."
                           <> line <> "  Run without --author for a new identity that has one." ) )
                codeNoKey

      sto <- getStorage
      sigil <- maybe (liftIO (refuse "could not make a sigil from that keyring" codeNoKey)) pure
                 (makeSigilFromCredentials @HubScheme creds enc Nothing Nothing)
      href <- storeSigil sto sigil
                >>= maybe (liftIO (refuse "the peer would not store the sigil" codeNotStored)) pure
      announceSigil href >>= \case
        True  -> pure ()
        False -> liftIO (refuse "the peer stored the sigil but did not answer its announce request" codePeerSilent)

      api <- getClientAPI @MailboxAPI @UNIX
      callRpcWaitMay @RpcMailboxCreate rpcTimeout api (author, MailboxHub)
        >>= \case
          Nothing -> liftIO (refuse (show ("hbs2-peer did not answer: is it running?" :: Doc ())) codePeerSilent)
          Just (Left e) -> liftIO (refuse (show ("the peer would not create your mailbox:" <+> viaShow e)) 1)
          Just (Right ()) -> pure ()

      -- AND A POLICY, or the mailbox is shut: a mailbox with no policy takes
      -- nothing, so the first acknowledgement anybody sent to it was dropped.
      -- Anyone may write and any peer may sync, which is what a reply address
      -- is for; the relay floor already prices a flood. An existing policy is
      -- the owner's choice and is left as it is.
      readPolicy author >>= \case
        Right Nothing ->
          void (submitPolicy author 1
                  (withDefaults (Just Allow) (Just Allow) (defaultBasicPolicy @HBS2Basic)))
        _ -> pure ()

      setGlobal authorGitKey (show (pretty (AsBase58 author)))
      setGlobal senderGitKey (show (pretty href))

      liftIO $ print $ vcat
        [ "identity" <+> pretty (AsBase58 author)
            <+> (if made then "(new, in hbs2-keyman)" else "(already yours)")
        , "sigil   " <+> pretty href <+> "(lets a maintainer answer you)"
        , "mailbox " <+> pretty (AsBase58 author) <+> "on this peer (where the answers land)"
        , mempty
        , "saved as git config --global" <+> pretty authorGitKey <+> "and" <+> pretty senderGitKey
        , "In a clone of any hbs2 project you can now run:"
        , "  hbs2-hub issue new --title \"...\" --body -"
        , "  hbs2-hub pr new --title \"...\""
        ]

    noKey k made =
      "this machine holds no keyring for" <+> pretty (AsBase58 k)
        <> line <> if made
                     then "  hbs2-keyman made one but does not report it: run `hbs2-keyman update`."
                     else "  Name a key hbs2-keyman knows with --author, or run without it for a new one."

    globalConfig k =
      gitRun Nothing [] 10 4096 "config" ["config", "--global", "--get", k] mempty <&> \case
        Right (ExitSuccess, out, _) ->
          let s = List.dropWhileEnd isSpace (BS8.unpack out) in if List.null s then Nothing else Just s
        _ -> Nothing

    setGlobal k v =
      gitRun Nothing [] 10 4096 "config" ["config", "--global", k, v] mempty >>= \case
        Right (ExitSuccess, _, _) -> pure ()
        _ -> liftIO (saying ("hbs2-hub: could not write git config --global" <+> pretty k
                               <> "; pass it by hand:" <+> pretty v <> line))
