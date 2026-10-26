-- | Which key signs canon when a verb was not told (PEP-21 delegation).
--
-- Every verb that writes canon takes @--as \<canon-key\>@ and, without it,
-- signs with the repository key. That default is right for the owner and
-- wrong for a delegate who also happens to hold the repository key on this
-- machine: the event is blessed by the owner, which is admissible and is not
-- what anybody meant, and the acknowledgement that follows is sealed (or fails
-- to be) from a key the delegate never named.
--
-- So the default can be set once, and the order is:
--
--   1. @--as \<key\>@ on the command line;
--   2. the environment variable @HBS2_HUB_AS@;
--   3. @git config hbs2.hub.as@ in the repository being worked in, as git
--      resolves it (local over global over system);
--   4. the repository key.
--
-- The environment over the config, as git itself has it for
-- @GIT_AUTHOR_NAME@ over @user.name@: the variable is the per-session answer
-- and the config is the per-clone one. An empty value counts as unset.
--
-- A VALUE THAT IS NOT A KEY IS REFUSED, and is not skipped. Skipping it would
-- fall through to the repository key and sign canon under a key nobody named,
-- which is the exact mistake 'HBS2.Hub.CLI.Accept.acceptArgs' refuses for a
-- bad @--as@.
--
-- An explicit @--as@ never reads the other two: a broken default must not stop
-- a caller who has said which key they mean.
module HBS2.Hub.CLI.Signer
  ( Signer(..)
  , SignerSource(..)
  , SignerTrouble(..)
  , pickSigner
  , defaultFrom
  , signerEnvVar
  , signerGitKey
  , signerSourceDoc
  , signerDoc
  , resolveSignerIn
  , canonSigner
  , signingAs
  , readSignerDefaults
  , writeSignerConfig
  , clearSignerConfig
  , signerPrecedenceHelp
  ) where

import HBS2.Hub.Types (HubKey,HubScheme)
import HBS2.Hub.Repo.Git (gitRun,GitTrouble(..))
import HBS2.Hub.CLI.Common (refuse,saying,signerFor)

import HBS2.CLI.Prelude

import HBS2.Base58 (AsBase58(..))
import HBS2.Net.Auth.Credentials (PeerCredentials)

import Data.ByteString.Char8 qualified as BS8
import Data.Char (isSpace)
import Data.Text qualified as Text
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))

-- | The environment variable that names a default canon key.
signerEnvVar :: String
signerEnvVar = "HBS2_HUB_AS"

-- | The git config key that names one. @hbs2.hub.*@, so that whatever else
-- hbs2-hub keeps in git config later has a section to go in.
signerGitKey :: String
signerGitKey = "hbs2.hub.as"

-- | Where the signing key came from.
data SignerSource =
    FromFlag        -- ^ @--as@
  | FromEnv         -- ^ 'signerEnvVar'
  | FromGitConfig   -- ^ 'signerGitKey'
  | RepoKeyDefault  -- ^ nothing named one: the repository key
  deriving stock (Eq,Show)

-- | The key that will sign, and why that one.
data Signer = Signer
  { sgKey    :: HubKey
  , sgSource :: SignerSource
  }
  deriving stock (Eq,Show)

-- | Why the default could not be read.
data SignerTrouble =
    -- | A value was set and is not a sign key: where, and what it said.
    NotAKey SignerSource Text
    -- | @git config@ would not answer.
  | ConfigUnreadable Text
  deriving stock (Eq,Show)

instance Pretty SignerTrouble where
  pretty = \case
    NotAKey src v ->
      signerSourceDoc src <+> "is set to" <+> dquotes (pretty (Text.take 80 v))
        <> ", which is not a sign key" <> line
        <> "  nothing was signed: a default that does not read is not skipped,"
        <+> "since" <> line
        <> "  skipping it would sign with the repository key instead."
        <+> "Fix or unset it," <> line
        <> "  or name the key with --as."
    ConfigUnreadable e ->
      "could not read git config" <+> pretty signerGitKey <> ":" <+> pretty e

-- | The whole precedence, as a function. See the module header.
pickSigner :: HubKey        -- ^ the repository key
           -> Maybe HubKey  -- ^ @--as@
           -> Maybe HubKey  -- ^ the environment
           -> Maybe HubKey  -- ^ git config
           -> Signer
pickSigner repo flag env cfg = case (flag, env, cfg) of
  (Just k, _, _)       -> Signer k FromFlag
  (_, Just k, _)       -> Signer k FromEnv
  (_, _, Just k)       -> Signer k FromGitConfig
  (Nothing, Nothing, Nothing) -> Signer repo RepoKeyDefault

-- | One configured value, read. Unset and blank are both 'Nothing'.
defaultFrom :: SignerSource -> Maybe Text -> Either SignerTrouble (Maybe HubKey)
defaultFrom src = \case
  Nothing -> Right Nothing
  Just raw
    | Text.null v -> Right Nothing
    | otherwise   -> maybe (Left (NotAKey src v)) (Right . Just)
                       (fromStringMay (Text.unpack v))
    where v = Text.strip raw

-- | The source in words, as a reader would go and change it.
signerSourceDoc :: SignerSource -> Doc ann
signerSourceDoc = \case
  FromFlag       -> "--as"
  FromEnv        -> pretty signerEnvVar
  FromGitConfig  -> "git config" <+> pretty signerGitKey
  RepoKeyDefault -> "the repository key"

-- | @\<key\> (from ...)@, for every line that says who signs.
signerDoc :: Signer -> Doc ann
signerDoc (Signer k src) = pretty (AsBase58 k) <+> parens (case src of
  RepoKeyDefault -> "the repository key: no --as," <+> pretty signerEnvVar
                      <+> "or" <+> pretty signerGitKey
  other          -> "from" <+> signerSourceDoc other)

-- | Ask git for 'signerGitKey' in the repository at @cwd@, or the one this
-- process is standing in.
readGitDefault :: MonadUnliftIO m => Maybe FilePath -> m (Either SignerTrouble (Maybe Text))
readGitDefault cwd =
  gitRun cwd [] 10 4096 "config" ["config", "--get", signerGitKey] mempty <&> \case
    Right (ExitSuccess, out, _)     -> Right (Just (decoded out))
    -- 1 is git's "the key is not set", and the only non-zero that means so.
    Right (ExitFailure 1, _, _)     -> Right Nothing
    Right (ExitFailure n, _, said)  ->
      Left (ConfigUnreadable ("git exited " <> Text.pack (show n) <> ": "
                               <> Text.strip (decoded said)))
    Left t -> Left (ConfigUnreadable (troubleText t))
  where
    decoded = Text.pack . BS8.unpack
    troubleText = \case
      GitUnstartable e -> e
      GitStalled e     -> e
      GitTooMuch w _ _ -> "git " <> w <> " said too much"

-- | Both defaults, read and checked: the environment value given, and git's.
readSignerDefaults :: MonadUnliftIO m
                   => Maybe FilePath -> Maybe String
                   -> m (Either SignerTrouble (Maybe HubKey, Maybe HubKey))
readSignerDefaults cwd envv = do
  cfg <- readGitDefault cwd
  pure do
    e <- defaultFrom FromEnv (Text.pack <$> envv)
    c <- cfg >>= defaultFrom FromGitConfig
    pure (e, c)

-- | 'pickSigner' over what is actually configured.
--
-- The repository at @cwd@ and the environment value are arguments so that a
-- test can ask without changing the process's own.
resolveSignerIn :: MonadUnliftIO m
                => Maybe FilePath -> Maybe String -> HubKey -> Maybe HubKey
                -> m (Either SignerTrouble Signer)
resolveSignerIn _ _ _ (Just k) = pure (Right (Signer k FromFlag))
resolveSignerIn cwd envv repo Nothing =
  readSignerDefaults cwd envv <&> fmap (uncurry (pickSigner repo Nothing))

-- | The signer for a canon write, or a refusal.
--
-- Says on stderr when a default chose it, because a verb that signs with a
-- key the command line does not show is a verb whose output should.
canonSigner :: MonadUnliftIO m => HubKey -> Maybe HubKey -> m Signer
canonSigner repo flag = do
  envv <- liftIO (lookupEnv signerEnvVar)
  resolveSignerIn Nothing envv repo flag >>= \case
    -- 1, a usage error: a default is an argument given ahead of time.
    Left e -> liftIO (refuse (show (pretty e)) 1)
    Right s -> do
      when (sgSource s `elem` [FromEnv, FromGitConfig]) $
        liftIO (saying ("hbs2-hub: signing canon as" <+> signerDoc s <> line))
      pure s

-- | The credentials for a signer, or a refusal that says how it was chosen and
-- how to choose another.
signingAs :: MonadUnliftIO m => Int -> Signer -> m (PeerCredentials HubScheme)
signingAs code s =
  signerFor (sgKey s) >>= maybe (liftIO (refuse (show msg) code)) pure
  where
    msg = "cannot sign as" <+> signerDoc s <> line
            <> "  no keyring here holds it as its own signing key."
            <> line <> "  To sign as another key: --as <key>,"
            <+> pretty signerEnvVar <> "=<key>, or"
            <+> "`hbs2-hub maintainer use --repo <repo> --maintainer-key <key>`"

-- | Set 'signerGitKey' in this clone's own config.
writeSignerConfig :: MonadUnliftIO m => HubKey -> m (Either Text ())
writeSignerConfig k =
  gitConfig ["config", "--local", signerGitKey, show (pretty (AsBase58 k))] [0]

-- | Unset it. Unsetting what is not set is not an error.
clearSignerConfig :: MonadUnliftIO m => m (Either Text ())
clearSignerConfig = gitConfig ["config", "--local", "--unset", signerGitKey] [0, 5]

gitConfig :: MonadUnliftIO m => [String] -> [Int] -> m (Either Text ())
gitConfig args fine =
  gitRun Nothing [] 10 4096 "config" args mempty <&> \case
    Right (ExitSuccess, _, _) -> Right ()
    Right (ExitFailure n, _, said)
      | n `elem` fine -> Right ()
      | otherwise -> Left (Text.dropWhileEnd isSpace (Text.pack (BS8.unpack said)))
    Left (GitUnstartable e) -> Left e
    Left (GitStalled e)     -> Left e
    Left (GitTooMuch w _ _) -> Left ("git " <> w <> " said too much")

-- | The paragraph every --as help text ends with.
signerPrecedenceHelp :: Doc ann
signerPrecedenceHelp =
  "Without --as the key comes from" <+> pretty signerEnvVar <> ", then"
  <> line <> "from `git config" <+> pretty signerGitKey <> "` in this clone (set it with"
  <> line <> "`hbs2-hub maintainer use`), then the repository key."
  <> line <> "`hbs2-hub status` says which one would sign."
