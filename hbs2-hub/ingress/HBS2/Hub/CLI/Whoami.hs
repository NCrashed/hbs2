-- | @hub whoami@: what this machine can be, and whether the pieces fit.
--
-- WHY A VERB FOR THIS. Sending a letter needs three values a contributor has to
-- get from three different tools, and until this existed no verb produced any
-- of them and no help text named a tool that did:
--
--   * an AUTHOR key, which signs the inner box and is the authorship claim
--     canon publishes forever;
--   * a SENDER sigil, which carries the encryption key an answer is sealed to;
--   * their own MAILBOX, which is where that answer lands.
--
-- The first two are separate values that have to agree, and the way they
-- disagree is the reason this is not merely a convenience. A sigil naming
-- somebody else's key produces a letter that is signed correctly, folds
-- correctly, and can never be answered: the hub refuses to seal a maintainer's
-- word to a key the sender does not hold. 'checkReplyChannel' now stops that at
-- send time; this is where a person finds out which of the two was wrong.
--
-- WHAT IT CANNOT DO. It does not create anything. Making a key, a sigil or a
-- mailbox belongs to @hbs2-keyman@, @hbs2-cli@ and @hbs2-peer@ respectively,
-- and a fourth tool that also made them would be a fourth place for them to
-- disagree about where they live. What this does is name those tools, which is
-- the part that was missing.
module HBS2.Hub.CLI.Whoami
  ( whoamiEntries
  , whoamiUsage
  , WhoamiArgs(..)
  , whoamiArgs
  , identityDoc
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter (sigilNames)
import HBS2.Hub.Repo.Manifest (readManifest,mailboxOf,sigilOf)
import HBS2.Hub.CLI.Argv (flagsOf,flagMaybe,repoFlags,flagRepoMaybe)
import HBS2.Hub.CLI.Common (refuse,saying,signerFor)
import HBS2.Hub.CLI.Compose (codeWrongSigil)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef,pattern HashLike)
import HBS2.Data.Types.SignedBox (unboxSignedBox0)
import HBS2.Net.Auth.Credentials.Sigil (Sigil(..),loadSigil)
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage
import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,listCredentials)

import Data.List qualified as List
import Data.Maybe (isNothing)
import System.Exit (die)

-- | What the caller asked about. Every field optional, and an empty call is
-- the useful one: it is what somebody with nothing yet would type.
data WhoamiArgs = WhoamiArgs
  { whoAuthor :: Maybe HubKey
  , whoSender :: Maybe HashRef
  , whoRepo   :: Maybe RepoRef
  }
  deriving stock (Eq,Show)

whoamiUsage :: Doc ()
whoamiUsage =
  "usage: hbs2-hub whoami [--author <key>] [--sender <sigil-hash>] [--repo <key>]"

whoamiArgs :: forall c . IsContext c => [Syntax c] -> Maybe WhoamiArgs
whoamiArgs syn = do
  kvs <- flagsOf (repoFlags <> ["--author","--sender"]) syn
  author <- flagMaybe kvs "--author" signKey
  sender <- flagMaybe kvs "--sender" hashOf
  repo   <- flagRepoMaybe signKey kvs
  pure (WhoamiArgs author sender repo)
  where
    signKey = \case
      SignPubKeyLike x -> Just x
      _                -> Nothing
    hashOf = \case
      HashLike x -> Just x
      _          -> Nothing

-- | What to say when nobody asked about anything in particular.
--
-- The three values and the tool that makes each. Not a tutorial: a person who
-- runs this has already been told to send a letter and does not know what a
-- sigil is, and four lines naming the three commands is the whole gap.
--
-- Pure and exported so a test can read it. What it must not become is a list of
-- flags -- `hub help issue new` is that -- and what it must keep is the ONE
-- fact nothing else states: the author key and the sender sigil are two values
-- that have to agree.
identityDoc :: [HubKey] -> Doc ann
identityDoc keys = vcat
  [ if List.null keys
      then vcat [ "This machine holds no signing key that hbs2-keyman knows about."
                , "  Make one with `hbs2-keyman`, or point it at a keyring you"
                    <+> "already have." ]
      else vcat ( ("signing keys this machine can author with"
                     <+> parens (pretty (length keys)) <> ":")
                  : [ "  " <> pretty (AsBase58 k) | k <- keys ] )
  , ""
  , "A letter takes three things, and this can only see the first."
  , ""
  , "  AUTHOR KEY   one of the keys above, passed as --author. It signs the"
  , "               letter, and canon publishes that signature forever."
  , "               `hbs2-keyman` is what holds them."
  , ""
  , "  SENDER SIGIL a sigil naming YOUR encryption key, passed as --sender, so"
  , "               that an answer can be sealed to you. Make one with:"
  , "                 hbs2-cli hbs2:sigil:create:from-keyring <keyring-file>"
  , ""
  , "  YOUR MAILBOX where that answer lands, and what `hub updates` reads."
  , "               Make one with:"
  , "                 hbs2-peer mailbox create --key <sign-key> hub"
  , ""
  , "THE FIRST TWO HAVE TO AGREE. A sigil naming somebody else's key sends a"
  , "letter that folds and is never answered. Check a pair before you need it:"
  , "  hbs2-hub whoami --author <key> --sender <sigil-hash>"
  ]

whoamiEntries :: forall c m . ( IsContext c
                              , MonadUnliftIO m
                              , HasStorage m
                              , HasClientAPI LWWRefAPI UNIX m
                              , Exception (BadFormException c)
                              ) => MakeDictM c m ()
whoamiEntries = do

  brief "say which keys this machine has, and whether a pair of them fits"
    $ args [ arg "string" "[--author key]", arg "string" "[--sender sigil-hash]"
           , arg "string" "[--repo repo-key]" ]
    $ desc ( "With no arguments: the signing keys hbs2-keyman knows about, and"
             <> line <> "the tools that make the two things it cannot see -- a"
             <> line <> "sigil and a mailbox. Nothing is created here."
             <> line
             <> line <> "--author and --sender TOGETHER is the check worth"
             <> line <> "running before the first letter. They are separate"
             <> line <> "values that have to agree, and a sigil naming somebody"
             <> line <> "else's key produces a letter that is signed correctly,"
             <> line <> "folds correctly, and can never be answered: a hub will"
             <> line <> "not seal a maintainer's reply to a key the sender does"
             <> line <> "not hold. Exits non-zero when they do not match."
             <> line
             <> line <> "--repo says where a letter to that repository would go:"
             <> line <> "the ingress mailbox it declares and the sigil to seal"
             <> line <> "to (PEP-18). A repository that declares neither is one"
             <> line <> "whose owner has not published the clause, or one that"
             <> line <> "is not a forge." )
    $ entry $ bindMatch "hub:whoami" $ nil_ \case
        (whoamiArgs -> Just w) -> lift (whoami w)
        _ -> liftIO (die (show whoamiUsage))

  where

    whoami WhoamiArgs{..} = do

      -- The bare call, which is the one somebody with nothing yet makes.
      when (isNothing whoAuthor && isNothing whoSender && isNothing whoRepo) do
        keys <- runKeymanClientRO listCredentials
        liftIO $ print (identityDoc keys)

      for_ whoAuthor $ \k -> do
        -- 'signerFor', because this line is the answer to "can I author as this
        -- key" and 'loadCredentials' does not answer it: keyman resolves a key
        -- to the FILE that holds it and returns that file's PRIMARY
        -- credentials, so a key that is a secondary in its keyring got a
        -- confident yes here and produced letters signed by a different key.
        creds <- signerFor k
        liftIO $ print $ case creds of
          Just _  -> "author" <+> pretty (AsBase58 k) <+> ": this machine can sign as it"
          -- Not a refusal on its own: naming a key you cannot sign with is a
          -- question this verb exists to answer, and answering it is not
          -- failing at it.
          Nothing -> "author" <+> pretty (AsBase58 k)
                       <+> ": NO SIGNING KEY HERE, so a letter cannot be authored as it"

      sigil <- for whoSender $ \h -> do
        sto <- getStorage
        si <- loadSigil @HubScheme sto h
        -- WHICH KEY IT NAMES, not merely that it read. That is the value the
        -- pair check compares, so a reader who gets a mismatch below can see
        -- what to compare it against without a second command.
        liftIO $ print $ case si >>= (fmap fst . unboxSignedBox0 . sigilData) of
          Nothing -> "sender" <+> pretty h
                       <+> ": this node cannot read it -- not fetched, or not a sigil"
          Just k  -> "sender" <+> pretty h <+> ": names" <+> pretty (AsBase58 k)
        pure si

      -- The pair, which is the whole point. Only when both were asked about:
      -- one of them alone says nothing about the other.
      case (whoAuthor, sigil) of
        (Just k, Just si) -> case sigilNames k si of
          Just True  -> liftIO $ print ("the sigil names that key: a reply can reach you"
                                          :: Doc ())
          Nothing    -> liftIO $ saying
            ( "the sigil could not be read here, so the pair was not checked."
                <> line <> "  Fetch it, or check on the machine that made it." <> line )
          Just False -> liftIO $ refuse
            ( show ( "THE SIGIL NAMES A DIFFERENT KEY." <> line
                       <> "  A letter with this pair folds and is never answered:"
                       <+> "the hub will" <> line <> "  not seal a reply to a key you do"
                       <+> "not hold." <> line
                       <> "  Either --author is not the identity this sigil was made"
                       <+> "for," <> line <> "  or --sender is somebody else's sigil." ) )
            codeWrongSigil
        _ -> pure ()

      for_ whoRepo $ \repo -> do
        mf <- readManifest repo
        liftIO $ case mf of
          Left e -> saying ("this node cannot read that repository:" <+> pretty e <> line)
          Right clauses -> do
            case mailboxOf clauses of
              Nothing -> print ( "repo" <+> pretty (AsBase58 repo)
                                   <+> ": declares no ingress mailbox" )
              Just mbox -> do
                print ("repo" <+> pretty (AsBase58 repo) <+> ": mailbox"
                         <+> pretty (AsBase58 mbox))
                case sigilOf mbox clauses of
                  Nothing -> print ( "  and no sigil for it, so a letter cannot be"
                                       <+> "sealed without --recipient" )
                  Just h  -> print ("  recipient sigil" <+> pretty h)
