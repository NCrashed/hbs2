-- | What a verb was not told and the clone already knows.
--
-- Somebody standing in a clone of a project typed the project's key on every
-- command, though their own remote holds it, and typed their author key and
-- sigil on every letter, though they are the same every time. A newcomer
-- opening a pull request needed five values, three of them forty-four
-- characters of base58, before the title.
--
-- So the command line is completed here, BEFORE any verb parses it, with the
-- values a flag would have carried:
--
--   * @--repo@: @HBS2_HUB_REPO@, then @git config hbs2.hub.repo@, then the key
--     in the clone's hbs23:// remote (@origin@ if it is one, else the only one);
--   * @--author@ and @--sender@: @HBS2_HUB_AUTHOR@ / @HBS2_HUB_SENDER@, then
--     @git config hbs2.hub.author@ / @hbs2.hub.sender@, which @hub identity
--     init@ writes;
--   * for @pr new@ and @pr revise@: @--onto@ is the branch the remote's HEAD
--     names and @--from@ the branch checked out;
--   * for @updates@: @--mailbox@ is the author key, which is what a
--     contributor's own mailbox is (PEP-18);
--   * for @publish@ and @sync@: @--remote@ is the hbs23:// remote the key
--     comes from. A clone kept on GitHub has @origin@ there and its hbs23
--     remote under another name, and defaulting to @origin@ pushed canon to
--     GitHub and fetched it back from there.
--
-- The same names the MCP server reads, so one configuration serves both.
--
-- A FLAG GIVEN BY HAND IS NEVER TOUCHED, and nothing is filled that no source
-- has: a verb that needs a value nobody configured fails with its own usage
-- line, as before. Done at the argv level, in one table, so that forty verbs do
-- not each grow a fallback and come to disagree about its order.
module HBS2.Hub.CLI.Defaults
  ( Want(..)
  , Surroundings(..)
  , nothingKnown
  , wantsOf
  , fillArgv
  , keyShaped
  , gatherSurroundings
  , hbs23Key
  , hbs23Remote
  , remoteHeadBranch
  , currentBranch
  , localBranches
  , defaultsFor
  , repoEnvVar, repoGitKey
  , authorEnvVar, authorGitKey
  , senderEnvVar, senderGitKey
  ) where

import HBS2.Hub.Repo.Git (gitRun)

import HBS2.CLI.Prelude

import Data.ByteString.Char8 qualified as BS8
import Data.Char (isSpace)
import Data.List qualified as List
import Data.Maybe (catMaybes)
import System.Exit (ExitCode(..))
import Data.Text qualified as Text
import System.Environment (lookupEnv)

-- | A value a verb may be given by default.
data Want
  = WRepo         -- ^ @--repo <key>@
  | WRepoFirst    -- ^ the repository as the first positional word (read verbs)
  | WAuthor       -- ^ @--author <key>@
  | WSender       -- ^ @--sender <sigil>@
  | WOnto         -- ^ @--onto <branch>@
  | WFrom         -- ^ @--from <branch>@
  | WOwnMailbox   -- ^ @--mailbox <author-key>@
  | WSelf         -- ^ @--maintainer-key <author-key>@, unless @--clear@
  | WRemote       -- ^ @--remote <name>@ of the hbs23:// remote
  deriving stock (Eq,Show)

-- | What the surroundings say, each value already as the word a flag takes.
data Surroundings = Surroundings
  { cxRepo   :: Maybe String
  , cxAuthor :: Maybe String
  , cxSender :: Maybe String
  , cxOnto   :: Maybe String
  , cxFrom   :: Maybe String
  , cxRemote :: Maybe String
  }
  deriving stock (Eq,Show)

nothingKnown :: Surroundings
nothingKnown = Surroundings Nothing Nothing Nothing Nothing Nothing Nothing

-- | Which defaults a verb takes, by its dictionary name.
--
-- A verb missing here takes none, which is the safe side: it behaves exactly
-- as before.
wantsOf :: Text -> [Want]
wantsOf v
  | v `elem` readsByPosition = [WRepoFirst]
  | v `elem` composing       = [WRepo, WAuthor, WSender]
  | v `elem` proposing       = [WRepo, WAuthor, WSender, WOnto, WFrom]
  | v == "hub:updates"       = [WRepo, WOwnMailbox]
  | v == "hub:maintainer:use" = [WRepo, WSelf]
  | v == "hub:sync"          = [WRepo, WRemote]
  | v == "hub:publish"       = [WRemote]
  | v == "hub:pr:land"       = [WRepo, WRemote]
  | v `elem` byRepo          = [WRepo]
  | otherwise                = []
  where
    readsByPosition =
      [ "hub:issue:list", "hub:pr:list", "hub:issue:show", "hub:pr:show"
      , "hub:log", "hub:verify" ]
    composing = [ "hub:issue:new", "hub:issue:comment", "hub:pr:comment" ]
    proposing = [ "hub:pr:new", "hub:pr:revise" ]
    byRepo =
      [ "hub:ban", "hub:ban:list", "hub:unban", "hub:compact"
      , "hub:inbox", "hub:inbox:accept", "hub:inbox:honour", "hub:inbox:reject"
      , "hub:inbox:show"
      , "hub:issue:assign", "hub:issue:close", "hub:issue:label", "hub:issue:open"
      , "hub:issue:reopen", "hub:issue:reply"
      , "hub:pr:assign", "hub:pr:checkout", "hub:pr:close", "hub:pr:label"
      , "hub:pr:merge", "hub:pr:reopen", "hub:pr:reply"
      , "hub:maintainer:add", "hub:maintainer:list", "hub:maintainer:remove"
      , "hub:redact", "hub:status"
      , "hub:project:open"
      ]

-- | The arguments after the verb, completed. Pure, so the table is a test.
fillArgv :: Surroundings -> [Want] -> [String] -> [String]
fillArgv cx ws args0 = List.foldl' step args0 ws
  where
    step args = \case
      WRepo       -> flag ["--repo","--target"] "--repo" (cxRepo cx) args
      -- In front of a positional line (`show 7`), behind a flag on a flagged
      -- one: `show --number 7` with the key in front is neither form, and the
      -- verb refused the line it had just been helped with. Every read verb
      -- takes --repo.
      WRepoFirst  -> case cxRepo cx of
        Just k | not (given ["--repo","--target"] args)
               , not (startsWithKey args) ->
                   if startsWithFlag args then args <> ["--repo", k] else k : args
        _ -> args
      WAuthor     -> flag ["--author"] "--author" (cxAuthor cx) args
      WSender     -> flag ["--sender"] "--sender" (cxSender cx) args
      WOnto       -> flag ["--onto"] "--onto" (cxOnto cx) args
      -- Not when it would propose the branch onto itself, which is what being
      -- on master and typing `pr new` would otherwise mean; the verb's own
      -- usage error then names the missing --from.
      -- Nor with --from-url, whose --from is a ref of ANOTHER repository: the
      -- branch checked out here is not it.
      WFrom       -> case (cxFrom cx, cxOnto cx) of
        _ | given ["--from-url"] args -> args
        (Just f, o) | Just f /= ontoGiven args o -> flag ["--from"] "--from" (Just f) args
        _ -> args
      WOwnMailbox -> flag ["--mailbox"] "--mailbox" (cxAuthor cx) args
      -- The key a maintainer works as is their own identity, which is the
      -- step `hub maintainer use` is for once the owner has added them.
      WSelf
        | given ["--clear"] args -> args
        | otherwise -> flag ["--maintainer-key","--key"] "--maintainer-key" (cxAuthor cx) args
      WRemote     -> flag ["--remote"] "--remote" (cxRemote cx) args

    flag names name val args
      | given names args = args
      | Just v <- val    = args <> [name, v]
      | otherwise        = args

    given names = any (\a -> any (\n -> a == n || (n <> "=") `List.isPrefixOf` a) names)

    startsWithKey = \case
      (a:_) -> keyShaped a
      _     -> False

    startsWithFlag = \case
      (a:_) -> "--" `List.isPrefixOf` a
      _     -> False

    ontoGiven args fallback = case dropWhile (/= "--onto") args of
      (_:o:_) -> Just o
      _       -> fallback

-- | Does this word look like a key, as opposed to a number or a flag?
--
-- Base58 and the length a 32-byte key encodes to. A shape test and not a
-- parse: it only decides whether a read verb was already given its repository.
keyShaped :: String -> Bool
keyShaped s = length s >= 32 && length s <= 44 && all (`elem` b58) s
  where b58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

repoEnvVar, repoGitKey, authorEnvVar, authorGitKey, senderEnvVar, senderGitKey :: String
repoEnvVar   = "HBS2_HUB_REPO"
repoGitKey   = "hbs2.hub.repo"
authorEnvVar = "HBS2_HUB_AUTHOR"
authorGitKey = "hbs2.hub.author"
senderEnvVar = "HBS2_HUB_SENDER"
senderGitKey = "hbs2.hub.sender"

-- | Ask the surroundings for what these wants need, and nothing else: a verb
-- that takes no default starts no git.
gatherSurroundings :: MonadUnliftIO m => [Want] -> m Surroundings
gatherSurroundings ws
  | List.null ws = pure nothingKnown
  | otherwise = do
      let needs w = w `elem` ws
          needsRepo = needs WRepo || needs WRepoFirst
      repo   <- if needsRepo then firstOf [env repoEnvVar, conf repoGitKey, remoteKey] else pure Nothing
      author <- if needs WAuthor || needs WOwnMailbox || needs WSelf
                  then firstOf [env authorEnvVar, conf authorGitKey] else pure Nothing
      sender <- if needs WSender then firstOf [env senderEnvVar, conf senderGitKey] else pure Nothing
      onto   <- if needs WOnto then remoteHeadBranch else pure Nothing
      from   <- if needs WFrom then currentBranch else pure Nothing
      remote <- if needs WRemote then hbs23Remote <&> fmap fst else pure Nothing
      pure (Surroundings repo author sender onto from remote)
  where
    firstOf = \case
      []     -> pure Nothing
      (a:as) -> a >>= maybe (firstOf as) (pure . Just)
    env v = liftIO (lookupEnv v) <&> \case
      Just s | not (all isSpace s) -> Just (strip s)
      _ -> Nothing
    conf k = git ["config", "--get", k]

-- | A clone's hbs23:// remotes: origin when it is one, else the only one.
remoteKey :: MonadUnliftIO m => m (Maybe String)
remoteKey = hbs23Remote <&> fmap snd

hbs23Remote :: MonadUnliftIO m => m (Maybe (String, String))
hbs23Remote = do
  names <- git ["remote"] <&> maybe [] lines
  found <- forM names $ \n -> git ["remote", "get-url", n] <&> \u -> (,) n <$> (u >>= hbs23Key)
  let ours = catMaybes found
  pure $ case (List.lookup "origin" ours, ours) of
    (Just k, _) -> Just ("origin", k)
    (_, [one])  -> Just one
    _           -> Nothing

-- | The key in an hbs23:// URL.
hbs23Key :: String -> Maybe String
hbs23Key u = case List.stripPrefix "hbs23://" (strip u) of
  Just k | keyShaped k -> Just k
  _ -> Nothing

-- | The branch the hbs23 remote's HEAD names: what a clone was checked out on.
remoteHeadBranch :: MonadUnliftIO m => m (Maybe String)
remoteHeadBranch = hbs23Remote >>= \case
  Nothing -> pure Nothing
  Just (r, _) -> do
    viaHead <- git ["symbolic-ref", "--short", "refs/remotes/" <> r <> "/HEAD"]
    pure (viaHead >>= List.stripPrefix (r <> "/"))

currentBranch :: MonadUnliftIO m => m (Maybe String)
currentBranch = git ["symbolic-ref", "--short", "-q", "HEAD"]

-- | The clone's local branches, for a form that asks which one to propose.
localBranches :: MonadUnliftIO m => m [String]
localBranches = git ["for-each-ref", "--count=500", "--format=%(refname:short)", "refs/heads/"]
  <&> maybe [] lines

-- | One line out of git, or nothing for any trouble at all: a default that
-- cannot be read is a default that is not there.
git :: MonadUnliftIO m => [String] -> m (Maybe String)
git args = gitRun Nothing [] 10 65536 "a default" args mempty <&> \case
  Right (ExitSuccess, out, _) ->
    let s = strip (BS8.unpack out) in if List.null s then Nothing else Just s
  _ -> Nothing

strip :: String -> String
strip = List.dropWhileEnd isSpace . dropWhile isSpace

-- | Complete a command line for the verb it names: the whole step, for 'main'.
defaultsFor :: MonadUnliftIO m => Text -> [String] -> m [String]
defaultsFor verb args = do
  let ws = wantsOf verb
  cx <- gatherSurroundings ws
  pure (fillArgv cx ws args)
