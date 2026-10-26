-- | @hub mcp@: serve this tool's verbs to an AI agent (Model Context Protocol).
--
-- The argument handling and the help. The server is "HBS2.Hub.Mcp".
--
-- A VERB AND NOT A SECOND BINARY, so that the tools and the verbs they run
-- cannot come from two builds: the server runs every tool as a child of the
-- executable it is itself running as. An @hbs2-hub-mcp@ found on PATH next to
-- an @hbs2-hub@ installed at another time would be a server whose tools
-- describe flags its verbs do not take.
module HBS2.Hub.CLI.Mcp
  ( mcpEntries
  , mcpUsage
    -- * The parts that decide something
  , McpArgs(..)
  , mcpArgs
  , configFrom
  ) where

import HBS2.Hub.Mcp
import HBS2.Hub.CLI.Argv (badArgs,flagsAndSwitches,flagMaybe,flagSwitch,flagText,flagWord,repoFlags
                         ,flagRepoMaybe,argvAtom)
import HBS2.Hub.CLI.Common (refuse)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.Data.Types.Refs (pattern HashLike)

import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Version (showVersion)
import Paths_hbs2_hub qualified as Version
import System.Environment (getExecutablePath,lookupEnv)

-- | What the command line said. Every field is optional: the environment fills
-- what it leaves out, and a tool call can name what the server does not know.
data McpArgs = McpArgs
  { maRepo     :: Maybe Text
  , maMailbox  :: Maybe Text
  , maAs       :: Maybe Text
  , maAuthor   :: Maybe Text
  , maSender   :: Maybe Text
  , maRemote   :: Maybe Text
  , maDir      :: Maybe FilePath
  , maHub      :: Maybe FilePath
  , maTimeout  :: Maybe Int
  , maReadOnly :: Bool
  }
  deriving stock (Eq,Show)

mcpUsage :: Doc ()
mcpUsage =
  "usage: hbs2-hub mcp [--repo <key>] [--mailbox <key>] [--as <key>] [--author <key>]"
  <> line <> "                    [--sender <sigil>] [--remote <name>] [--dir <clone>]"
  <> line <> "                    [--timeout <seconds>] [--read-only]"
  <> line <> "  speaks MCP on stdin/stdout; `hbs2-hub help mcp` says more."

-- | Every value behind a flag, for the reason the rest of the package gives:
-- the repository, the mailbox, the canon key and the author are all thirty-two
-- bytes of base58.
mcpArgs :: forall c . IsContext c => [Syntax c] -> Maybe McpArgs
mcpArgs syn = do
  kvs <- flagsAndSwitches (repoFlags <> ["--mailbox","--as","--author","--sender","--remote"
                                        ,"--dir","--hub","--timeout"])
                          ["--read-only"] syn
  repo <- flagRepoMaybe signKey kvs
  mbox <- flagMaybe kvs "--mailbox" signKey
  as   <- flagMaybe kvs "--as" signKey
  au   <- flagMaybe kvs "--author" signKey
  se   <- flagMaybe kvs "--sender" hash
  rm   <- flagMaybe kvs "--remote" (fmap Text.pack . flagText)
  dir  <- flagMaybe kvs "--dir" flagText
  hub  <- flagMaybe kvs "--hub" flagText
  t    <- flagMaybe kvs "--timeout" flagWord
  ro   <- flagSwitch kvs "--read-only"
  guard (maybe True (> 0) t)
  pure (McpArgs repo mbox as au se rm dir hub (fromIntegral <$> t) ro)
  where
    -- Kept as the text that was typed, after checking it reads: the server
    -- hands it on to a verb, which reads it again.
    signKey x = case x of
      SignPubKeyLike _ -> Text.pack <$> flagText x
      _ -> Nothing
    hash x = case x of
      HashLike _ -> Text.pack <$> flagText x
      _ -> Nothing

-- | The command line over the environment, over the defaults.
--
-- An environment value is read by the same rules as a flag, and one that does
-- not read is a refusal at startup rather than a default nobody notices was
-- dropped: a server that quietly ignored HBS2_HUB_AS would sign with the
-- repository key.
configFrom :: FilePath -> McpArgs -> IO McpConfig
configFrom self ma = do
  repo <- pick maRepo    "HBS2_HUB_REPO" signKey
  mbox <- pick maMailbox "HBS2_HUB_MAILBOX" signKey
  as   <- pick maAs      "HBS2_HUB_AS" signKey
  au   <- pick maAuthor  "HBS2_HUB_AUTHOR" signKey
  se   <- pick maSender  "HBS2_HUB_SENDER"  hash
  rm   <- pick maRemote  "HBS2_HUB_REMOTE"  Just
  dir  <- maybe (fmap (fmap Text.unpack) (env "HBS2_HUB_DIR" Just)) (pure . Just) (maDir ma)
  pure (defaultMcpConfig (fromMaybe self (maHub ma)))
    { mcRepo = repo, mcMailbox = mbox, mcAs = as, mcAuthor = au, mcSender = se
    , mcRemote = rm, mcDir = dir
    , mcTimeout = fromMaybe (mcTimeout (defaultMcpConfig self)) (maTimeout ma)
    , mcReadOnly = maReadOnly ma
    , mcVersion = Text.pack (showVersion Version.version) }
  where
    pick f name rd = maybe (env name rd) (pure . Just) (f ma)
    env name rd = lookupEnv name >>= \case
      Nothing -> pure Nothing
      Just "" -> pure Nothing
      Just v -> case rd (Text.pack v) of
        Just t -> pure (Just t)
        Nothing -> refuse (name <> " does not read: " <> show v) 1
    signKey t = case argvAtom (Text.unpack t) of { SignPubKeyLike _ -> Just t ; _ -> Nothing }
    hash t = case argvAtom (Text.unpack t) of { HashLike _ -> Just t ; _ -> Nothing }

mcpEntries :: forall c m . ( IsContext c
                           , MonadUnliftIO m
                           , Exception (BadFormException c)
                           ) => MakeDictM c m ()
mcpEntries =
  brief "serve these verbs to an AI agent over MCP (JSON-RPC on stdio)"
    $ args [ arg "string" "[--repo key]", arg "string" "[--mailbox key]"
           , arg "string" "[--as canon-key]", arg "string" "[--author key]"
           , arg "string" "[--sender sigil]", arg "string" "[--remote name]"
           , arg "string" "[--dir clone]", arg "string" "[--timeout seconds]"
           , arg "string" "[--read-only]" ]
    $ desc ( "A Model Context Protocol server on stdin and stdout, for an"
             <> line <> "agent such as Claude Code. Register it with the client and"
             <> line <> "it starts this itself; there is nothing to run by hand:"
             <> line
             <> line <> "  claude mcp add hbs2-hub -- hbs2-hub mcp --repo <key>"
             <> line
             <> line <> "Each tool runs one verb of this binary, in --dir (default:"
             <> line <> "where the server was started), and returns its output, its"
             <> line <> "exit code and what the code means. issue_show, pr_show and"
             <> line <> "inbox_list return the JSON documents as structured content."
             <> line
             <> line <> "The flags are DEFAULTS a call may override, so that a call"
             <> line <> "names what differs. Each also reads from the environment:"
             <> line <> "HBS2_HUB_REPO, HBS2_HUB_MAILBOX, HBS2_HUB_AS, HBS2_HUB_AUTHOR,"
             <> line <> "HBS2_HUB_SENDER, HBS2_HUB_REMOTE, HBS2_HUB_DIR. A flag wins."
             <> line
             <> line <> "Tools that write canon take dry_run and say in their"
             <> line <> "description that canon is public and permanent once"
             <> line <> "published. --read-only offers none of them."
             <> line
             <> line <> "inbox_wait blocks until a letter arrives, by running"
             <> line <> "`inbox --follow --json`. --timeout bounds every other verb"
             <> line <> "(default 600 seconds)." )
    $ entry $ bindMatch "hub:mcp" $ nil_ \case
        (mcpArgs -> Just ma) -> liftIO $ do
          self <- getExecutablePath
          cfg <- configFrom self ma
          serveMcp cfg stdin stdout
        other -> liftIO (badArgs mcpUsage other)
