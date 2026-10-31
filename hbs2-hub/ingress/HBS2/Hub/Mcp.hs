-- | @hbs2-hub mcp@: this tool's verbs as Model Context Protocol tools.
--
-- An AI agent (Claude Code, or any MCP client) talks JSON-RPC 2.0 to this over
-- stdin and stdout, one message per line, and gets typed tools with JSON
-- schemas instead of a shell and a manual.
--
-- EVERY TOOL IS A VERB, RUN AS A CHILD PROCESS of the same binary. The layer
-- here turns a JSON argument object into the argv the verb takes and the verb's
-- output and exit code back into a tool result, and decides nothing else. Not
-- in-process, though the verbs are a library, for four reasons that are about
-- the verbs as they are and not about taste:
--
--   * A verb refuses by leaving the process: 'refuse', 'badArgs' and
--     'withCanon' end in 'exitWith', from deep inside library code. In a
--     long-lived server that is an exception to catch in every call, and one
--     missed is the server gone.
--   * A verb prints its answer on stdout, which here is the protocol channel.
--     Capturing it means redirecting file descriptor 1 for the whole process,
--     which serialises every call and puts a stray line from any thread into
--     the JSON stream.
--   * A verb that talks to the peer builds its RPC clients per run, and
--     @inbox --follow@ installs signal handlers. Neither is written to be done
--     a thousand times in one process.
--   * A child can be killed. A call that hangs on a wedged peer, or one the
--     client cancels, is a process to terminate rather than a thread to hope
--     about.
--
-- What it buys: the exit codes are the CLI's contract unchanged, a verb gains a
-- flag (a default @--as@, a @--json@) and the tool can pass it without anything
-- here learning what it means, and the argument checks a tool relies on are the
-- verb's own -- the same 'flagsOf' that refuses an unknown flag on a command
-- line refuses it here.
--
-- What IS used in-process is what decides nothing at runtime: the exit-code
-- table ('HBS2.Hub.CLI.Codes'), so a refusal comes back with its meaning, and
-- the key and hash readers, so a malformed key is refused before a process is
-- spent on it.
--
-- WAITING FOR LETTERS is the @inbox_wait@ tool, which runs @inbox --follow
-- --json@ and returns at the first document holding a letter the caller has not
-- seen, or at its timeout. A tool and not a notification, because a client
-- surfaces a tool result to the model and does not, today, surface a
-- server-initiated notification; and because the follower is exactly the
-- logic that already knows when the mailbox root moved.
module HBS2.Hub.Mcp
  ( McpConfig(..)
  , defaultMcpConfig
  , serveMcp
    -- * The parts that decide something
  , Tool(..)
  , Access(..)
  , tools
  , toolsFor
  , planFor
  , Action(..)
  , Run(..)
  , Shape(..)
  , protocolVersions
  , exitMeaning
  , argvShown
    -- * Running, for another transport (PEP-24 native host)
  , Outcome(..)
  , runVerb
  , rehearsed
  , waitInbox
  , toolJson
  ) where

import HBS2.Hub.CLI.Argv (argvAtom)
import HBS2.Hub.CLI.Codes (refusals,codeFor,meaning)

import HBS2.Data.Types.Refs (pattern HashLike)
import HBS2.Net.Auth.Credentials (pattern SignPubKeyLike)

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.MVar
import Control.Exception qualified as E
import Control.Monad (forM_,unless,void,when)
import Data.Aeson (Value(..),object,(.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isSpace,isDigit)
import Data.Foldable (toList)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.IORef
import Data.List qualified as List
import Data.Maybe (fromMaybe,isJust,mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import GHC.Clock (getMonotonicTime)
import GHC.Conc (atomically)
import System.IO
import System.IO.Error (isEOFError)
import System.Process.Typed
import System.Process qualified as P
import System.Posix.Signals (signalProcess,sigKILL)
import System.Timeout (timeout)

-- | What the server was started with. Every field but the binary is a default
-- a tool call may override, so that a call names what differs and not the
-- repository it has named a hundred times.
data McpConfig = McpConfig
  { mcHub      :: FilePath        -- ^ the @hbs2-hub@ every verb runs as
  , mcDir      :: Maybe FilePath  -- ^ the clone the verbs run in; the server's own when Nothing
  , mcRepo     :: Maybe Text      -- ^ @--repo@
  , mcMailbox  :: Maybe Text      -- ^ @--mailbox@
  , mcAs       :: Maybe Text      -- ^ @--as@, the canon key a maintainer signs with; only from the server's own --as
  , mcAuthor   :: Maybe Text      -- ^ @--author@, for letters
  , mcSender   :: Maybe Text      -- ^ @--sender@, the sigil letters go out under
  , mcRemote   :: Maybe Text      -- ^ @--remote@ for publish, sync and status
  , mcReadOnly :: Bool            -- ^ list and run no tool that writes
  , mcTimeout  :: Int             -- ^ seconds one verb may take
  , mcVersion  :: Text            -- ^ what @initialize@ calls this server
  }
  deriving stock (Eq,Show)

defaultMcpConfig :: FilePath -> McpConfig
defaultMcpConfig hub = McpConfig
  { mcHub = hub, mcDir = Nothing, mcRepo = Nothing, mcMailbox = Nothing
  , mcAs = Nothing, mcAuthor = Nothing, mcSender = Nothing, mcRemote = Nothing
  , mcReadOnly = False, mcTimeout = 600, mcVersion = "unknown" }

-- | The protocol revisions this server speaks, newest first. The first is
-- offered to a client asking for one not on the list, which is what the
-- specification asks a server to do; the client then decides whether to go on.
protocolVersions :: [Text]
protocolVersions = ["2025-06-18", "2025-03-26", "2024-11-05"]

-- | Whether a tool can change anything.
--
-- 'Writes' covers canon, a mailbox, the network and the local refs: everything
-- a read-only server must not offer. The descriptions say which of those each
-- one is, because the difference between them is the difference between a
-- local mistake and a public one.
data Access = Reads | Writes
  deriving stock (Eq,Show)

data Tool = Tool
  { toolName      :: Text
  , toolTitle     :: Text
  , toolDesc      :: Text
  , toolProps     :: [(Text, Value)]
  , toolRequired  :: [Text]
  , toolAccess    :: Access
  , toolOpenWorld :: Bool      -- ^ reaches beyond this machine (peer, remote, network)
  , toolVerb      :: [String]  -- ^ the verb it runs, as typed: @["issue","list"]@
  , toolPlan      :: McpConfig -> KM.KeyMap Value -> Either Text Action
  }

-- | One run of a verb.
data Run = Run
  { runArgv  :: [String]      -- ^ after the binary
  , runStdin :: Maybe Text    -- ^ a body, for a verb given @--body=-@
  , runShape :: Shape
  }
  deriving stock (Eq,Show)

-- | How a verb's output becomes a result.
data Shape
  = AsText    -- ^ stdout is for a person; it goes back as text
  | AsJson    -- ^ stdout is one JSON document; it goes back as structured content too
  | AsAudit   -- ^ like 'AsText', and exit 2 is a finding rather than a failure
  deriving stock (Eq,Show)

-- | What a call does.
data Action
  = RunVerb Run
    -- | A dry run the verb has no flag for: the first, read-only, is run; the
    -- second is the one that would be, and is only shown.
  | Rehearse (Maybe Run) Run
  | WaitInbox [String] (Maybe (HS.HashSet Text)) Int
  deriving stock (Eq,Show)

------------------------------------------------------------------------------
-- The tools

-- | Every tool, read-only server or not.
tools :: [Tool]
tools =
  [ tStatus, tWhoami
  , tList "issue", tList "pr", tShow "issue", tShow "pr"
  , tLog, tVerify
  , tInboxList, tInboxShow, tInboxWait
  , tAccept "inbox_accept" ["inbox","accept"] acceptDesc
  , tAccept "inbox_honour" ["inbox","honour"] honourDesc
  , tReject
  , tIssueOpen, tIssueClose, tIssueReopen, tIssueLabel, tIssueReply, tIssueComment
  , tIssueNew, tPrNew, tPrLand, tPrMerge
  , tIssueAssign, tRedact
  , tMaintainerList, tMaintainer "maintainer_add" "add" "Add a maintainer"
  , tMaintainer "maintainer_remove" "remove" "Remove a maintainer"
  , tPublish, tSync
  ] <> codeTools

-- | The tools a server with this configuration offers.
toolsFor :: McpConfig -> [Tool]
toolsFor cfg = [ t | t <- tools, not (mcReadOnly cfg) || toolAccess t == Reads ]

-- | What a call with these arguments would do, or why it cannot.
planFor :: McpConfig -> Tool -> KM.KeyMap Value -> Either Text Action
planFor cfg t as = do
  let known = fmap fst (toolProps t)
      stray = [ Key.toText k | k <- KM.keys as, Key.toText k `notElem` known ]
  -- An argument nobody claims is refused, for the reason 'flagsOf' gives: a
  -- @dryrun@ that is silently ignored is a write somebody meant to rehearse.
  unless (null stray) $
    Left ("unknown argument(s): " <> Text.intercalate ", " stray
          <> ". This tool takes: " <> Text.intercalate ", " known)
  forM_ (toolRequired t) $ \k ->
    unless (KM.member (Key.fromText k) as) $ Left ("missing required argument: " <> k)
  toolPlan t cfg as

-- Shared wording -------------------------------------------------------------

canonIsForever :: Text
canonIsForever =
  "\n\nWRITES CANON. The event is signed and committed to refs/hbs2/meta in the "
  <> "local clone; once published it is PUBLIC and PERMANENT in every clone that "
  <> "fetches it. Canon is append-only: a mistake is answered by another event, "
  <> "never withdrawn. Call with dry_run: true first; that runs every check and "
  <> "prints the event without signing or writing anything."

defaultsNote :: Text
defaultsNote =
  " Arguments left out fall back to the defaults the server was started with "
  <> "(--repo, --mailbox, --as, --author, --sender, --remote)."

-- Schema pieces ---------------------------------------------------------------

prop :: Text -> Text -> (Text, Value)
prop n d = (n, object ["type" .= ("string" :: Text), "description" .= d])

propInt :: Text -> Text -> (Text, Value)
propInt n d = (n, object ["type" .= ("integer" :: Text), "minimum" .= (1 :: Int), "description" .= d])

propBool :: Text -> Text -> (Text, Value)
propBool n d = (n, object ["type" .= ("boolean" :: Text), "description" .= d])

propEnum :: Text -> [Text] -> Text -> (Text, Value)
propEnum n xs d = (n, object ["type" .= ("string" :: Text), "enum" .= xs, "description" .= d])

propList :: Text -> Text -> (Text, Value)
propList n d = (n, object [ "type" .= ("array" :: Text)
                          , "items" .= object ["type" .= ("string" :: Text)]
                          , "description" .= d ])

pRepo, pMailbox, pAs, pDry, pNumber, pThread, pKind, pMessage, pRemote :: (Text, Value)
pRepo    = prop "repo" "Repository key (base58). Defaults to the server's --repo."
pMailbox = prop "mailbox" "Ingress mailbox key (base58). Defaults to the server's --mailbox."
pAs      = prop "as" ("Canon key to sign with, for a delegate maintainer (base58). Defaults to the "
                      <> "server's --as. Without either, the verb picks as it does on a command "
                      <> "line: HBS2_HUB_AS, then git config hbs2.hub.as in the clone, then the "
                      <> "repository key. The status tool says which key that is.")
pDry     = propBool "dry_run" "Rehearse: run every check and show what would be written, without signing, writing or sending anything."
pNumber  = propInt "number" "Thread number."
pThread  = prop "thread" "Thread id (base58 hash), instead of number."
pKind    = propEnum "kind" ["issue","pr"] "Which noun to run the verb under. Default: issue."
pMessage = prop "message" "Message hash of the letter (base58), as inbox_list reports it."
pRemote  = prop "remote" "Git remote name. Defaults to the server's --remote, then to the verb's own default."

-- Argument readers ------------------------------------------------------------

type Args = KM.KeyMap Value

optText :: Text -> Args -> Either Text (Maybe Text)
optText k as = case KM.lookup (Key.fromText k) as of
  Nothing        -> pure Nothing
  Just Null      -> pure Nothing
  Just (String s) -> pure (Just s)
  Just _         -> Left (k <> " must be a string")

optBool :: Text -> Args -> Either Text Bool
optBool k as = case KM.lookup (Key.fromText k) as of
  Nothing         -> pure False
  Just Null       -> pure False
  Just (Bool b)   -> pure b
  Just _          -> Left (k <> " must be a boolean")

optNat :: Text -> Args -> Either Text (Maybe Int)
optNat k as = case KM.lookup (Key.fromText k) as of
  Nothing   -> pure Nothing
  Just Null -> pure Nothing
  -- Through aeson's own Int reader, which refuses a fraction and an
  -- out-of-range value rather than rounding or wrapping one.
  Just v@(Number _) | Aeson.Success i <- Aeson.fromJSON v, i >= (1 :: Int) -> pure (Just i)
  -- A number spelled as a string, which some clients send for an integer
  -- field. Only digits: "7 " or "0x7" is not a number somebody meant.
  Just (String s) | not (Text.null s), Text.all isDigit s, Text.length s < 12 ->
    pure (Just (read (Text.unpack s)))
  Just _ -> Left (k <> " must be a positive integer")

optTexts :: Text -> Args -> Either Text (Maybe [Text])
optTexts k as = case KM.lookup (Key.fromText k) as of
  Nothing        -> pure Nothing
  Just Null      -> pure Nothing
  Just (Array v) -> Just <$> traverse one (toList v)
  Just (String s) -> pure (Just [s])
  Just _         -> Left (k <> " must be an array of strings")
  where one = \case { String s -> pure s ; _ -> Left (k <> " must be an array of strings") }

-- | A sign key, checked by the reader the verbs use, so that a malformed key is
-- refused here and not by a process spent on it.
asSignKey :: Text -> Text -> Either Text Text
asSignKey k v = case argvAtom (Text.unpack v) of
  SignPubKeyLike _ -> pure v
  _ -> Left (k <> " is not a base58 key: " <> v)

asHash :: Text -> Text -> Either Text Text
asHash k v = case argvAtom (Text.unpack v) of
  HashLike _ -> pure v
  _ -> Left (k <> " is not a base58 hash: " <> v)

keyOr :: Text -> Maybe Text -> Args -> Either Text (Maybe Text)
keyOr k dflt as = optText k as >>= \case
  Just v  -> Just <$> asSignKey k v
  Nothing -> traverse (asSignKey k) dflt

hashOr :: Text -> Maybe Text -> Args -> Either Text (Maybe Text)
hashOr k dflt as = optText k as >>= \case
  Just v  -> Just <$> asHash k v
  Nothing -> traverse (asHash k) dflt

needed :: Text -> Text -> Maybe a -> Either Text a
needed k why = maybe (Left ("no " <> k <> ": pass " <> k <> why)) pure

repoOf :: McpConfig -> Args -> Either Text Text
repoOf cfg as = keyOr "repo" (mcRepo cfg) as
  >>= needed "repo" ", or start the server with --repo <key> (or HBS2_HUB_REPO)"

-- | @--flag=value@, which is the spelling a verb reads without ever taking the
-- value for a flag: a title that starts with a dash is a title.
flag :: String -> Text -> String
flag f v = "--" <> f <> "=" <> Text.unpack v

flagM :: String -> Maybe Text -> [String]
flagM f = maybe [] (pure . flag f)

switch :: String -> Bool -> [String]
switch f b = ["--" <> f | b]

kindOf :: Args -> Either Text String
kindOf as = optText "kind" as >>= \case
  Nothing      -> pure "issue"
  Just "issue" -> pure "issue"
  Just "pr"    -> pure "pr"
  Just k       -> Left ("kind must be issue or pr, not " <> k)

-- | Exactly one of the two ways of naming a thread.
threadFlags :: Args -> Either Text [String]
threadFlags as = do
  n <- optNat "number" as
  t <- hashOr "thread" Nothing as
  case (n, t) of
    (Just k, Nothing)  -> pure [flag "number" (Text.pack (show k))]
    (Nothing, Just h)  -> pure [flag "thread" h]
    (Nothing, Nothing) -> Left "name the thread: number or thread"
    _                  -> Left "number and thread both given; pass one"

-- | The canon key, explicit or the server's. When neither, NO @--as@ at all:
-- the verb then reads HBS2_HUB_AS and @git config hbs2.hub.as@ itself
-- ("HBS2.Hub.CLI.Signer"), in the clone it runs in, and says which one picked
-- the signer. Passing a default as @--as@ would hide that, and a server that
-- read git config itself would read it in its own directory rather than the
-- verb's.
asFlag :: McpConfig -> Args -> Either Text [String]
asFlag cfg as = flagM "as" <$> keyOr "as" (mcAs cfg) as

native :: Bool -> Run -> Action
native dry r = RunVerb r { runArgv = runArgv r <> switch "dry-run" dry }

-- The read tools ---------------------------------------------------------------

tStatus :: Tool
tStatus = Tool
  { toolName = "status", toolTitle = "Where this clone stands"
  , toolDesc = "Where the repository stands, as the status JSON document: which key a canon "
      <> "write without 'as' would sign with and where that came from (signer.key, "
      <> "signer.source: env, git_config or repo_key), whether this machine can sign as it "
      <> "(signing) and whether it is a maintainer, what canon holds here, whether canon has "
      <> "reached each remote, which mailbox the repository declares and how many letters "
      <> "wait in it. Read-only; pushes nothing. local: true asks no remote, and says "
      <> "so under published." <> defaultsNote
  , toolProps = [pRepo, pRemote, propBool "local" "Ask no remote: the signer, canon and the clone only."]
  , toolRequired = []
  , toolAccess = Reads, toolOpenWorld = True, toolVerb = ["status"]
  , toolPlan = \cfg as -> do
      r <- repoOf cfg as
      rm <- optText "remote" as
      local <- optBool "local" as
      pure $ RunVerb (Run (["status", flag "repo" r] <> flagM "remote" (rm `orElse` mcRemote cfg)
                             <> ["--json"] <> switch "local" local)
                          Nothing AsJson)
  }

tWhoami :: Tool
tWhoami = Tool
  { toolName = "whoami", toolTitle = "Which keys this machine has"
  , toolDesc = "The signing keys this machine holds. With author and sender, checks that "
      <> "the sender sigil names the author key (a letter from a mismatched pair can never "
      <> "be answered). With repo, says where a letter to that repository would go, and "
      <> "which key a canon write there without 'as' would sign with and where that came "
      <> "from (HBS2_HUB_AS, git config hbs2.hub.as, or the repository key). "
      <> "Read-only; creates nothing." <> defaultsNote
  , toolProps = [ prop "author" "Author sign key (base58). Defaults to the server's --author."
                , prop "sender" "Sender sigil hash (base58). Defaults to the server's --sender."
                , pRepo ]
  , toolRequired = []
  , toolAccess = Reads, toolOpenWorld = True, toolVerb = ["whoami"]
  , toolPlan = \cfg as -> do
      a <- keyOr "author" (mcAuthor cfg) as
      s <- hashOr "sender" (mcSender cfg) as
      r <- keyOr "repo" (mcRepo cfg) as
      pure $ RunVerb (Run (["whoami"] <> flagM "author" a <> flagM "sender" s <> flagM "repo" r)
                          Nothing AsText)
  }

tList :: String -> Tool
tList noun = Tool
  { toolName = Text.pack noun <> "_list"
  , toolTitle = if noun == "pr" then "List pull requests" else "List issues"
  , toolDesc = "List the repository's folded " <> what <> " by number, with status and "
      <> "labels. Read-only and needs no peer: canon is a git ref in the clone the server "
      <> "runs in (fetch it with sync first). Pull request rows also report whether the "
      <> "proposal merges into the target ref in that clone. Filters are exact matches." <> defaultsNote
  , toolProps = [ pRepo
                , propEnum "status" ["open","closed","merged"] "Only threads with this status."
                , prop "label" "Only threads carrying this applied label." ]
  , toolRequired = []
  , toolAccess = Reads, toolOpenWorld = False, toolVerb = [noun, "list"]
  , toolPlan = \cfg as -> do
      r <- repoOf cfg as
      st <- optText "status" as
      lb <- optText "label" as
      -- The `index` document (PEP-24), so a client gets data and not a table.
      pure $ RunVerb (Run ([noun, "list", flag "repo" r] <> flagM "status" st <> flagM "label" lb
                           <> ["--json"])
                          Nothing AsJson)
  }
  where what = if noun == "pr" then "pull requests" else "issues"

tShow :: String -> Tool
tShow noun = Tool
  { toolName = Text.pack noun <> "_show"
  , toolTitle = if noun == "pr" then "Show one pull request" else "Show one issue"
  , toolDesc = "One thread as the PEP-22 render contract: a versioned JSON object with the "
      <> "title, body, status, labels, assignees and every comment" <> prDiff
      <> ". Everything in it already passed the admission checks. Check the 'contract' "
      <> "field; fields may be added without a bump. Read-only and peerless." <> defaultsNote
  , toolProps = [pRepo, pNumber, pThread]
  , toolRequired = []
  , toolAccess = Reads, toolOpenWorld = False, toolVerb = [noun, "show"]
  , toolPlan = \cfg as -> do
      r <- repoOf cfg as
      w <- threadFlags as
      pure $ RunVerb (Run ([noun, "show", flag "repo" r] <> w <> ["--json"]) Nothing AsJson)
  }
  where prDiff = if noun == "pr" then ", the precomputed diff, and mergeability against the target ref" else ""

tLog :: Tool
tLog = Tool
  { toolName = "log", toolTitle = "Canon event log"
  , toolDesc = "The surviving events of canon in order, as the log JSON document: per event "
      <> "seq, event_id, op, thread_id, number, author, canon_by, folded_at and origin. "
      <> "Provenance only, no titles or bodies (issue_show has those). With number, one "
      <> "thread's events. Read-only and peerless." <> defaultsNote
  , toolProps = [pRepo, propInt "number" "Only this thread's events."]
  , toolRequired = []
  , toolAccess = Reads, toolOpenWorld = False, toolVerb = ["log"]
  , toolPlan = \cfg as -> do
      r <- repoOf cfg as
      n <- optNat "number" as
      pure $ RunVerb (Run (["log", flag "repo" r] <> flagM "number" (Text.pack . show <$> n)
                             <> ["--json"])
                          Nothing AsJson)
  }

tVerify :: Tool
tVerify = Tool
  { toolName = "verify", toolTitle = "Audit canon"
  , toolDesc = "Re-run the admission rules over the whole of canon and report every event "
      <> "they did not admit and every anomaly in those they did. A finding is a result, "
      <> "not a tool error. Read-only, needs no peer and no key." <> defaultsNote
  , toolProps = [pRepo], toolRequired = []
  , toolAccess = Reads, toolOpenWorld = False, toolVerb = ["verify"]
  , toolPlan = \cfg as -> do
      r <- repoOf cfg as
      pure $ RunVerb (Run ["verify", flag "repo" r] Nothing AsAudit)
  }

tInboxList :: Tool
tInboxList = Tool
  { toolName = "inbox_list", toolTitle = "The triage queue"
  , toolDesc = "The letters waiting in the ingress mailbox, as the queue JSON document: one "
      <> "entry per letter with its message hash, author, op, thread, title and whether it "
      <> "would fold. A page: when 'omitted' is non-zero, pass 'cursor' as after. With a "
      <> "repo the node's deny-list is applied; the mailbox defaults to the one the "
      <> "repository declares. Read-only; takes a few seconds while the peer's copy settles."
      <> defaultsNote
  , toolProps = [ pRepo, pMailbox
                , prop "after" "Continue after this message hash (the previous page's cursor)."
                , propInt "limit" "Open at most this many letters." ]
  , toolRequired = []
  , toolAccess = Reads, toolOpenWorld = True, toolVerb = ["inbox"]
  , toolPlan = \cfg as -> do
      args <- queueFlags cfg as
      aft <- hashOr "after" Nothing as
      lim <- optNat "limit" as
      pure $ RunVerb (Run (["inbox"] <> args <> flagM "after" aft
                             <> flagM "limit" (Text.pack . show <$> lim) <> ["--json"])
                          Nothing AsJson)
  }

-- | A queue named by mailbox, by repository, or both; one has to be there.
queueFlags :: McpConfig -> Args -> Either Text [String]
queueFlags cfg as = do
  m <- keyOr "mailbox" (mcMailbox cfg) as
  r <- keyOr "repo" (mcRepo cfg) as
  when (not (isJust m) && not (isJust r)) $
    Left "name the queue: pass mailbox or repo, or start the server with --mailbox or --repo"
  pure (flagM "mailbox" m <> flagM "repo" r)

tInboxShow :: Tool
tInboxShow = Tool
  { toolName = "inbox_show", toolTitle = "One letter, whole"
  , toolDesc = "Open one letter from the ingress mailbox, as the letter JSON document: what "
      <> "it asks for (op, subject), the whole body untruncated, its attachments measured, "
      <> "and triage.verdict (would_fold, pending, already_folded, refused, unreadable) "
      <> "with triage.reason.code. The verdict is a preview; inbox_accept still runs the "
      <> "full check. With a repo, the deny-list and canon are consulted as well. "
      <> "Read-only." <> defaultsNote
  , toolProps = [pMessage, pMailbox, pRepo], toolRequired = ["message"]
  , toolAccess = Reads, toolOpenWorld = True, toolVerb = ["inbox","show"]
  , toolPlan = \cfg as -> do
      RunVerb <$> showRun cfg as
  }

showRun :: McpConfig -> Args -> Either Text Run
showRun cfg as = do
  h <- hashOr "message" Nothing as >>= needed "message" ""
  -- The mailbox, or the repository that declares it: the verb reads the
  -- manifest for one when it is given the other.
  q <- queueFlags cfg as
  pure (Run (["inbox","show", flag "message" h] <> q <> ["--json"])
            Nothing AsJson)

tInboxWait :: Tool
tInboxWait = Tool
  { toolName = "inbox_wait", toolTitle = "Wait for a new letter"
  , toolDesc = "Block until a letter arrives in the ingress mailbox, or until the timeout. "
      <> "Runs the inbox follower, which watches the peer's mailbox root (one local call "
      <> "every five seconds, a network fetch once a minute) and reads the queue only when "
      <> "it moves; a peer restart is waited out. Pass known: the message hashes you have "
      <> "already seen (from inbox_list "
      <> "or the previous inbox_wait's 'seen'); a letter not among them returns at once, "
      <> "so nothing that arrived between two calls is missed. Without known, the queue "
      <> "as it is now is the baseline. Returns arrived, the new letters in the queue "
      <> "document's shape, and seen for the next call. Read-only. Keep timeout_seconds "
      <> "below your client's tool timeout." <> defaultsNote
  , toolProps = [ pRepo, pMailbox
                , propList "known" "Message hashes already seen; any other letter in the queue counts as new."
                , propInt "timeout_seconds" "How long to wait, at most 3600. Default 300." ]
  , toolRequired = []
  , toolAccess = Reads, toolOpenWorld = True, toolVerb = ["inbox"]
  , toolPlan = \cfg as -> do
      args <- queueFlags cfg as
      known <- optTexts "known" as >>= traverse (traverse (asHash "known"))
      t <- fromMaybe 300 <$> optNat "timeout_seconds" as
      pure (WaitInbox (["inbox"] <> args <> ["--follow","--json"])
                      (HS.fromList <$> known) (min 3600 t))
  }

-- The write tools ----------------------------------------------------------------

acceptDesc, honourDesc :: Text
acceptDesc =
  "Fold one letter from the ingress mailbox into canon as the contributor's own event, "
  <> "blessed by the canon key. Checks the deny-list and the bridge first, verifies a pull "
  <> "request's bundle and stages its head. Afterwards the letter is dropped from the "
  <> "mailbox (keep: true skips that) and the contributor gets an acknowledgement when "
  <> "they asked for one. A letter that only ASKS for something (close, reopen, a label) "
  <> "is refused here; that is inbox_honour."
honourDesc =
  "Grant a request a letter made (close, reopen, a label) by authoring the same event "
  <> "under the canon key, with the letter kept as the origin. A request that carries "
  <> "words (a closing note, an attribute) comes back as NeedsReview: write your own "
  <> "with issue_close or issue_label."

tAccept :: Text -> [String] -> Text -> Tool
tAccept name verb d = Tool
  { toolName = name
  , toolTitle = if name == "inbox_honour" then "Honour a request" else "Accept a letter into canon"
  , toolDesc = d <> canonIsForever <> defaultsNote
  , toolProps = [ pMessage, pRepo, pMailbox, pAs
                , propBool "keep" "Leave the letter in the mailbox afterwards."
                , propBool "incomplete" "Fold even though the mailbox tree did not read whole."
                , pDry ]
  , toolRequired = ["message"]
  , toolAccess = Writes, toolOpenWorld = True, toolVerb = verb
  , toolPlan = \cfg as -> do
      h <- hashOr "message" Nothing as >>= needed "message" ""
      r <- repoOf cfg as
      m <- keyOr "mailbox" (mcMailbox cfg) as
      a <- asFlag cfg as
      keep <- optBool "keep" as
      inc <- optBool "incomplete" as
      dry <- optBool "dry_run" as
      pure $ native dry $
        Run (verb <> [flag "repo" r, flag "message" h] <> flagM "mailbox" m <> a
                  <> switch "keep" keep <> switch "incomplete" inc)
            Nothing AsText
  }

tReject :: Tool
tReject = Tool
  { toolName = "inbox_reject", toolTitle = "Reject a letter"
  , toolDesc = "Drop a letter from the ingress mailbox without folding it: writes a "
      <> "tombstone into the mailbox (signed by the mailbox key, gossiped to peers) so the "
      <> "queue stops showing it, and every copy of it with it. Refused when canon already "
      <> "holds the letter. The sender is told, when their letter asked for an answer; "
      <> "silent: true skips that, which is the usual choice for junk, since an "
      <> "acknowledgement confirms the address is read. Not canon, but not undoable either. "
      <> "The verb has no dry run of its own: dry_run: true shows the letter as inbox_show "
      <> "does (including whether canon holds it) and the command that would run."
      <> defaultsNote
  , toolProps = [ pMessage, pRepo, pMailbox, pAs
                , propBool "silent" "Send the sender no acknowledgement."
                , pDry ]
  , toolRequired = ["message"]
  , toolAccess = Writes, toolOpenWorld = True, toolVerb = ["inbox","reject"]
  , toolPlan = \cfg as -> do
      h <- hashOr "message" Nothing as >>= needed "message" ""
      r <- repoOf cfg as
      m <- keyOr "mailbox" (mcMailbox cfg) as
      a <- asFlag cfg as
      silent <- optBool "silent" as
      dry <- optBool "dry_run" as
      let run = Run (["inbox","reject", flag "repo" r, flag "message" h] <> flagM "mailbox" m
                       <> a <> switch "silent" silent) Nothing AsText
      if dry then do
          probe <- showRun cfg as
          pure (Rehearse (Just probe) run)
        else pure (RunVerb run)
  }

tIssueOpen :: Tool
tIssueOpen = Tool
  { toolName = "issue_open", toolTitle = "Open an issue in canon"
  , toolDesc = "Open an issue directly in canon as the repository owner or a maintainer, "
      <> "without a letter. Canon mints its number. Labels given here are applied, not "
      <> "requested." <> canonIsForever <> defaultsNote
  , toolProps = [ prop "title" "Issue title."
                , prop "body" "Issue body (Markdown). Optional."
                , propList "labels" "Labels to apply."
                , pRepo, pAs, pDry ]
  , toolRequired = ["title"]
  , toolAccess = Writes, toolOpenWorld = False, toolVerb = ["issue","open"]
  , toolPlan = \cfg as -> do
      title <- optText "title" as >>= needed "title" ""
      body <- optText "body" as
      ls <- fromMaybe [] <$> optTexts "labels" as
      r <- repoOf cfg as
      a <- asFlag cfg as
      dry <- optBool "dry_run" as
      pure $ native dry $
        Run (["issue","open", flag "repo" r, flag "title" title]
               <> maybe [] (const ["--body=-"]) body
               <> fmap (flag "label") ls <> a)
            body AsText
  }

tIssueClose :: Tool
tIssueClose = Tool
  { toolName = "issue_close", toolTitle = "Close a thread"
  , toolDesc = "Close an issue (or, with kind: pr, a pull request) that is in canon. A note "
      <> "is published as a comment in the signer's own words." <> canonIsForever <> defaultsNote
  , toolProps = [ pNumber, prop "note" "A closing comment, published with the close."
                , pKind, pRepo, pAs, pDry ]
  , toolRequired = ["number"]
  , toolAccess = Writes, toolOpenWorld = False, toolVerb = ["issue","close"]
  , toolPlan = \cfg as -> do
      k <- kindOf as
      n <- optNat "number" as >>= needed "number" ""
      note <- optText "note" as
      r <- repoOf cfg as
      a <- asFlag cfg as
      dry <- optBool "dry_run" as
      pure $ native dry $
        Run ([k, "close", flag "repo" r, flag "number" (Text.pack (show n))]
               <> flagM "note" note <> a)
            Nothing AsText
  }

tIssueReopen :: Tool
tIssueReopen = Tool
  { toolName = "issue_reopen", toolTitle = "Reopen a thread"
  , toolDesc = "Reopen a closed issue (or, with kind: pr, a closed pull request) that is in "
      <> "canon. A note is published as a comment in the signer's own words. A merged pull "
      <> "request stays merged." <> canonIsForever <> defaultsNote
  , toolProps = [ pNumber, prop "note" "A comment, published with the reopen."
                , pKind, pRepo, pAs, pDry ]
  , toolRequired = ["number"]
  , toolAccess = Writes, toolOpenWorld = False, toolVerb = ["issue","reopen"]
  , toolPlan = \cfg as -> do
      k <- kindOf as
      n <- optNat "number" as >>= needed "number" ""
      note <- optText "note" as
      r <- repoOf cfg as
      a <- asFlag cfg as
      dry <- optBool "dry_run" as
      pure $ native dry $
        Run ([k, "reopen", flag "repo" r, flag "number" (Text.pack (show n))]
               <> flagM "note" note <> a)
            Nothing AsText
  }

tIssueLabel :: Tool
tIssueLabel = Tool
  { toolName = "issue_label", toolTitle = "Set a thread's labels"
  , toolDesc = "Set the labels of an issue (or, with kind: pr, a pull request) that is in "
      <> "canon. The labels REPLACE the thread's current set: to add one, name the whole "
      <> "set. Removing them all takes clear: true, never an empty list." <> canonIsForever
      <> defaultsNote
  , toolProps = [ pNumber, propList "labels" "The complete new label set."
                , propBool "clear" "Remove every label (instead of labels)."
                , pKind, pRepo, pAs, pDry ]
  , toolRequired = ["number"]
  , toolAccess = Writes, toolOpenWorld = False, toolVerb = ["issue","label"]
  , toolPlan = \cfg as -> do
      k <- kindOf as
      n <- optNat "number" as >>= needed "number" ""
      ls <- fromMaybe [] <$> optTexts "labels" as
      clear <- optBool "clear" as
      case (ls, clear) of
        ([], False) -> Left "pass labels, or clear: true to remove them all"
        (_:_, True) -> Left "labels and clear both given; pass one"
        _ -> pure ()
      r <- repoOf cfg as
      a <- asFlag cfg as
      dry <- optBool "dry_run" as
      pure $ native dry $
        Run ([k, "label", flag "repo" r, flag "number" (Text.pack (show n))]
               <> fmap (flag "label") ls <> switch "clear" clear <> a)
            Nothing AsText
  }

tIssueReply :: Tool
tIssueReply = Tool
  { toolName = "issue_reply", toolTitle = "Reply in a thread, onto canon"
  , toolDesc = "Comment in an issue (or, with kind: pr, a pull request) that is in canon, as "
      <> "the repository owner or a maintainer, straight onto canon: no letter, no mailbox, "
      <> "no inbox_accept. The signing key must be one that may bless canon; any other is "
      <> "refused before anything is signed. The comment carries no origin. A contributor "
      <> "who is not a maintainer uses issue_comment instead." <> canonIsForever <> defaultsNote
  , toolProps = [ prop "body" "Comment text (Markdown)."
                , pNumber, pThread, pKind, pRepo
                , prop "reply_to" "Event id being answered (advisory)."
                , pAs, pDry ]
  , toolRequired = ["body"]
  , toolAccess = Writes, toolOpenWorld = False, toolVerb = ["issue","reply"]
  , toolPlan = \cfg as -> do
      k <- kindOf as
      body <- optText "body" as >>= needed "body" ""
      when (Text.all isSpace body) $ Left "body is empty"
      w <- threadFlags as
      r <- repoOf cfg as
      rt <- hashOr "reply_to" Nothing as
      a <- asFlag cfg as
      dry <- optBool "dry_run" as
      pure $ native dry $
        Run ([k, "reply", flag "repo" r] <> w <> ["--body=-"] <> flagM "reply-to" rt <> a)
            (Just body) AsText
  }

tIssueComment :: Tool
tIssueComment = Tool
  { toolName = "issue_comment", toolTitle = "Comment on a thread (letter)"
  , toolDesc = "Reply in a thread by composing a letter: signed by author, sealed to the "
      <> "repository's ingress mailbox and sent over the hbs2 network. It is a request until "
      <> "a maintainer folds it (inbox_accept), and nothing is in canon before that. A "
      <> "maintainer answering in a thread uses issue_reply, which writes canon directly. SENDS: "
      <> "a sent letter cannot be recalled, and once folded it is PUBLIC and PERMANENT "
      <> "canon. The author key is in canon verbatim. The verb has no dry run of its own: "
      <> "dry_run: true checks that the sender sigil names the author key and where the "
      <> "letter would go (whoami), and shows the command that would run." <> defaultsNote
  , toolProps = [ prop "body" "Comment text (Markdown)."
                , pNumber, pThread, pKind, pRepo
                , prop "author" "Author sign key (base58). Defaults to the server's --author."
                , prop "sender" "Sender sigil hash (base58). Defaults to the server's --sender."
                , prop "recipient" "Recipient sigil hash, when not the one the repository declares."
                , prop "reply_to" "Event id being answered (advisory)."
                , pDry ]
  , toolRequired = ["body"]
  , toolAccess = Writes, toolOpenWorld = True, toolVerb = ["issue","comment"]
  , toolPlan = \cfg as -> do
      k <- kindOf as
      body <- optText "body" as >>= needed "body" ""
      when (Text.all isSpace body) $ Left "body is empty"
      w <- threadFlags as
      r <- keyOr "repo" (mcRepo cfg) as
      when (any ("--number=" `List.isPrefixOf`) w && not (isJust r)) $
        Left "a thread number needs a repository to resolve against: pass repo, or thread"
      au <- keyOr "author" (mcAuthor cfg) as
              >>= needed "author" ", or start the server with --author <key> (or HBS2_HUB_AUTHOR)"
      se <- hashOr "sender" (mcSender cfg) as
              >>= needed "sender" ", or start the server with --sender <sigil> (or HBS2_HUB_SENDER)"
      rc <- hashOr "recipient" Nothing as
      rt <- hashOr "reply_to" Nothing as
      dry <- optBool "dry_run" as
      let run = Run ([k, "comment", flag "sender" se, flag "author" au, "--body=-"]
                       <> w <> flagM "repo" r <> flagM "recipient" rc <> flagM "reply-to" rt)
                    (Just body) AsText
          probe = Run (["whoami", flag "author" au, flag "sender" se] <> flagM "repo" r)
                      Nothing AsText
      pure (if dry then Rehearse (Just probe) run else RunVerb run)
  }

-- The letters a contributor sends, and the merge (PEP-24 phases 2 and 3) ------

-- | Author and sender for a letter, from the call or the server's defaults.
letterKeys :: McpConfig -> Args -> Either Text (Text, Text)
letterKeys cfg as = do
  au <- keyOr "author" (mcAuthor cfg) as
          >>= needed "author" ", or start the server with --author <key> (or HBS2_HUB_AUTHOR)"
  se <- hashOr "sender" (mcSender cfg) as
          >>= needed "sender" ", or start the server with --sender <sigil> (or HBS2_HUB_SENDER)"
  pure (au, se)

pAuthor, pSender, pRecipient :: (Text, Value)
pAuthor    = prop "author" "Author sign key (base58). Defaults to the server's --author."
pSender    = prop "sender" "Sender sigil hash (base58). Defaults to the server's --sender."
pRecipient = prop "recipient" "Recipient sigil hash, when not the one the repository declares."

lettersNote :: Text
lettersNote =
  " SENDS: a sent letter cannot be recalled, and once folded it is PUBLIC and PERMANENT "
  <> "canon. The author key is in canon verbatim. The verb has no dry run of its own: "
  <> "dry_run: true checks that the sender sigil names the author key and where the "
  <> "letter would go (whoami), and shows the command that would run."

tIssueNew :: Tool
tIssueNew = Tool
  { toolName = "issue_new", toolTitle = "File an issue (letter)"
  , toolDesc = "File an issue by composing a letter: signed by author, sealed to the "
      <> "repository's ingress mailbox and sent over the hbs2 network. It is a request until "
      <> "a maintainer folds it (inbox_accept); canon mints its number then. Labels given "
      <> "here are REQUESTED, not applied. A maintainer opening one directly uses issue_open."
      <> lettersNote <> defaultsNote
  , toolProps = [ prop "title" "Issue title."
                , prop "body" "Issue body (Markdown). Optional."
                , propList "labels" "Labels to ask for."
                , pRepo, pAuthor, pSender, pRecipient, pDry ]
  , toolRequired = ["title"]
  , toolAccess = Writes, toolOpenWorld = True, toolVerb = ["issue","new"]
  , toolPlan = \cfg as -> do
      title <- optText "title" as >>= needed "title" ""
      when (Text.all isSpace title) $ Left "title is empty"
      body <- optText "body" as
      ls <- fromMaybe [] <$> optTexts "labels" as
      r <- repoOf cfg as
      (au, se) <- letterKeys cfg as
      rc <- hashOr "recipient" Nothing as
      dry <- optBool "dry_run" as
      let run = Run ( ["issue","new", flag "repo" r, flag "sender" se, flag "author" au
                      , flag "title" title]
                      <> maybe [] (const ["--body=-"]) body
                      <> fmap (flag "label") ls <> flagM "recipient" rc )
                    body AsText
          probe = Run ["whoami", flag "author" au, flag "sender" se, flag "repo" r] Nothing AsText
      pure (if dry then Rehearse (Just probe) run else RunVerb run)
  }

tPrNew :: Tool
tPrNew = Tool
  { toolName = "pr_new", toolTitle = "Propose a pull request (letter)"
  , toolDesc = "Propose a change: bundles the commits on from that are not on onto, in the "
      <> "working clone, and sends them as an encrypted attachment to a letter, as issue_new "
      <> "does. onto defaults to the branch the clone's hbs23 remote names as HEAD, from to "
      <> "the branch checked out. Only the new commits travel."
      <> lettersNote <> defaultsNote
  , toolProps = [ prop "title" "Pull request title."
                , prop "body" "Description (Markdown). Optional."
                , prop "onto" "Branch to merge into. Default: the remote's HEAD branch."
                , prop "from" "Branch to propose. Default: the branch checked out."
                , prop "base" "Fork point, when the merge-base of onto and from is not it."
                , prop "from_url" ("Propose a ref of ANOTHER repository: fetch from from this https URL "
                                   <> "first (a GitHub fork's branch, or pull/<n>/head of a pull request "
                                   <> "opened on GitHub). from is then required.")
                , pRepo, pAuthor, pSender, pRecipient, pDry ]
  , toolRequired = ["title"]
  , toolAccess = Writes, toolOpenWorld = True, toolVerb = ["pr","new"]
  , toolPlan = \cfg as -> do
      title <- optText "title" as >>= needed "title" ""
      when (Text.all isSpace title) $ Left "title is empty"
      body <- optText "body" as
      onto <- optText "onto" as
      from <- optText "from" as
      base <- optText "base" as
      url <- optText "from_url" as
      when (isJust url && not (isJust from)) $ Left "from_url needs from: the ref to fetch from it"
      r <- repoOf cfg as
      (au, se) <- letterKeys cfg as
      rc <- hashOr "recipient" Nothing as
      dry <- optBool "dry_run" as
      -- Left out, onto and from are filled by the verb from the clone it runs
      -- in ("HBS2.Hub.CLI.Defaults"), which is the clone this server runs in.
      let run = Run ( ["pr","new", flag "repo" r, flag "sender" se, flag "author" au
                      , flag "title" title]
                      <> maybe [] (const ["--body=-"]) body
                      <> flagM "onto" onto <> flagM "from" from <> flagM "base" base
                      <> flagM "from-url" url
                      <> flagM "recipient" rc )
                    body AsText
          probe = Run ["whoami", flag "author" au, flag "sender" se, flag "repo" r] Nothing AsText
      pure (if dry then Rehearse (Just probe) run else RunVerb run)
  }

tPrLand :: Tool
tPrLand = Tool
  { toolName = "pr_land", toolTitle = "Merge a pull request"
  , toolDesc = "Merge an open pull request in the working clone, as a maintainer: a merge "
      <> "commit of the staged proposal into its target branch, computed without touching "
      <> "the working tree; pushed to the hbs23 remote (never forced) and to the mirror "
      <> "(origin, when that is another remote); the local branch fast-forwarded; the merge "
      <> "recorded in canon; canon published. A conflict, a target branch that moved, or a "
      <> "signer that may not bless canon is refused before anything is pushed. PUSHES AND "
      <> "PUBLISHES: the merge is on the remotes and in canon, PUBLIC and PERMANENT. dry_run: true "
      <> "fetches and computes the merge, and stops." <> defaultsNote
  , toolProps = [ pNumber, pRemote
                , prop "mirror" "A second remote for the branch. Default: origin, when it is another remote."
                , propBool "no_mirror" "Push to the hbs23 remote only."
                , prop "message" "The merge commit's message. Default: what git merge says, and the title."
                , pRepo, pAs, pDry ]
  , toolRequired = ["number"]
  , toolAccess = Writes, toolOpenWorld = True, toolVerb = ["pr","land"]
  , toolPlan = \cfg as -> do
      n <- optNat "number" as >>= needed "number" ""
      rm <- (`orElse` mcRemote cfg) <$> optText "remote" as
      mi <- optText "mirror" as
      nm <- optBool "no_mirror" as
      when (nm && isJust mi) $ Left "mirror and no_mirror both given; pass one"
      msg <- optText "message" as
      r <- repoOf cfg as
      a <- asFlag cfg as
      dry <- optBool "dry_run" as
      pure $ native dry $
        Run ( ["pr","land", flag "repo" r, flag "number" (Text.pack (show n))]
              <> flagM "remote" rm <> flagM "mirror" mi <> switch "no-mirror" nm
              <> flagM "message" msg <> a )
            Nothing AsText
  }

tIssueAssign :: Tool
tIssueAssign = Tool
  { toolName = "issue_assign", toolTitle = "Say who is looking at a thread"
  , toolDesc = "Set the assignee of an issue (or, with kind: pr, a pull request) that is in "
      <> "canon: one key, which replaces the one before; clear: true unassigns. A note about "
      <> "who is looking, not a grant of anything." <> canonIsForever <> defaultsNote
  , toolProps = [ pNumber, prop "assignee" "The assignee's key (base58)."
                , propBool "clear" "Unassign (instead of assignee)."
                , pKind, pRepo, pAs, pDry ]
  , toolRequired = ["number"]
  , toolAccess = Writes, toolOpenWorld = False, toolVerb = ["issue","assign"]
  , toolPlan = \cfg as -> do
      k <- kindOf as
      n <- optNat "number" as >>= needed "number" ""
      who <- keyOr "assignee" Nothing as
      clear <- optBool "clear" as
      case (who, clear) of
        (Nothing, False) -> Left "pass assignee, or clear: true to unassign"
        (Just _, True) -> Left "assignee and clear both given; pass one"
        _ -> pure ()
      r <- repoOf cfg as
      a <- asFlag cfg as
      dry <- optBool "dry_run" as
      pure $ native dry $
        Run ([k, "assign", flag "repo" r, flag "number" (Text.pack (show n))]
               <> flagM "assignee" who <> switch "clear" clear <> a)
            Nothing AsText
  }

tRedact :: Tool
tRedact = Tool
  { toolName = "redact", toolTitle = "Withhold an event's text"
  , toolDesc = "Redact one event (an opening or a comment) by its event id: readers withhold "
      <> "its title and body from then on. The event itself stays in canon, as canon is "
      <> "append-only; what a clone already holds is not taken back." <> canonIsForever
      <> defaultsNote
  , toolProps = [ prop "event" "The event id (base58), as the thread document gives it.", pRepo, pAs, pDry ]
  , toolRequired = ["event"]
  , toolAccess = Writes, toolOpenWorld = False, toolVerb = ["redact"]
  , toolPlan = \cfg as -> do
      e <- hashOr "event" Nothing as >>= needed "event" ""
      r <- repoOf cfg as
      a <- asFlag cfg as
      dry <- optBool "dry_run" as
      pure $ native dry $ Run (["redact", flag "repo" r, flag "event" e] <> a) Nothing AsText
  }

tMaintainerList :: Tool
tMaintainerList = Tool
  { toolName = "maintainer_list", toolTitle = "Who may write canon"
  , toolDesc = "The keys that may bless canon here, as the maintainers JSON document: the "
      <> "owner (the repository key) and every maintainer canon names. Read-only." <> defaultsNote
  , toolProps = [pRepo], toolRequired = []
  , toolAccess = Reads, toolOpenWorld = False, toolVerb = ["maintainer","list"]
  , toolPlan = \cfg as -> do
      r <- repoOf cfg as
      pure $ RunVerb (Run ["maintainer","list", flag "repo" r, "--json"] Nothing AsJson)
  }

tMaintainer :: Text -> String -> Text -> Tool
tMaintainer name verb title = Tool
  { toolName = name, toolTitle = title
  , toolDesc = (if verb == "add"
                  then "Make a key a maintainer: it may then accept letters and write canon. "
                  else "Take a key's maintainership away: what it wrote stays in canon. ")
      <> "Name the key, or author_of a thread number to take that thread's author. Only the "
      <> "owner may do this." <> canonIsForever <> defaultsNote
  , toolProps = [ prop "key" "The maintainer's key (base58)."
                , propInt "author_of" "Instead of key: the author of this thread number."
                , pRepo, pDry ]
  , toolRequired = []
  , toolAccess = Writes, toolOpenWorld = False, toolVerb = ["maintainer", verb]
  , toolPlan = \cfg as -> do
      k <- keyOr "key" Nothing as
      n <- optNat "author_of" as
      who <- case (k, n) of
        (Just key, Nothing) -> pure [flag "maintainer-key" key]
        (Nothing, Just m) -> pure [flag "author-of" (Text.pack (show m))]
        (Nothing, Nothing) -> Left "pass key, or author_of"
        _ -> Left "key and author_of both given; pass one"
      r <- repoOf cfg as
      dry <- optBool "dry_run" as
      pure $ native dry $ Run (["maintainer", verb, flag "repo" r] <> who) Nothing AsText
  }

tPrMerge :: Tool
tPrMerge = Tool
  { toolName = "pr_merge", toolTitle = "Record a merge made elsewhere"
  , toolDesc = "Record that an open pull request was merged by a commit made outside this "
      <> "tool (with git, or on GitHub): canon checks that the commit contains the proposed "
      <> "tip, and the status becomes merged. fetch names a remote to fetch first, for a "
      <> "merge whose commit this clone has not seen. pr_land is the one that merges."
      <> canonIsForever <> defaultsNote
  , toolProps = [ pNumber, prop "commit" "The merge commit (a full or abbreviated sha)."
                , prop "into" "The branch it landed on, as refs/heads/<name>."
                , prop "fetch" "A remote to fetch first, such as origin."
                , pRepo, pAs, pDry ]
  , toolRequired = ["number", "commit", "into"]
  , toolAccess = Writes, toolOpenWorld = True, toolVerb = ["pr","merge"]
  , toolPlan = \cfg as -> do
      n <- optNat "number" as >>= needed "number" ""
      c <- optText "commit" as >>= needed "commit" ""
      i <- optText "into" as >>= needed "into" ""
      fe <- optText "fetch" as
      r <- repoOf cfg as
      a <- asFlag cfg as
      dry <- optBool "dry_run" as
      pure $ native dry $
        Run ( ["pr","merge", flag "repo" r, flag "number" (Text.pack (show n)), flag "commit" c
              , flag "into" i] <> flagM "fetch" fe <> a )
            Nothing AsText
  }

-- The code view (PEP-25) ----------------------------------------------------------

-- | One of the @hub code@ verbs, as a read tool returning its document.
tCode :: Text -> Text -> String -> Text -> [(Text, Value)] -> [Text] -> (Args -> Either Text [String]) -> Tool
tCode name title verb what props required extra = Tool
  { toolName = name, toolTitle = title
  , toolDesc = what <> " As a JSON document, from the clone the call runs in. Read-only." <> defaultsNote
  , toolProps = props <> [pRepo], toolRequired = required
  , toolAccess = Reads, toolOpenWorld = name == "code_refs", toolVerb = ["code", verb]
  , toolPlan = \cfg as -> do
      r <- repoOf cfg as
      more <- extra as
      pure $ RunVerb (Run (["code", verb, flag "repo" r] <> more <> ["--json"]) Nothing AsJson)
  }

pRef, pPath :: (Text, Value)
pRef = prop "ref" "A branch, a tag, a sha, or HEAD (the default)."
pPath = prop "path" "A path inside the tree; the root when left out."

codeTools :: [Tool]
codeTools =
  [ tCode "code_refs" "Branches, tags, name" "refs"
      "The repository's branches and tags, the branch HEAD names, and the name and description its manifest declares."
      [] [] (const (pure []))
  , tCode "code_tree" "A directory" "tree"
      "The entries of a directory at a ref, directories first, and which of them is its README."
      [pRef, pPath] [] (\as -> (<>) <$> refPath as <*> pure [])
  , tCode "code_blob" "A file" "blob"
      "A file at a ref: its text, at most 512 KiB of it with truncated set past that, or binary and no text."
      [pRef, pPath] ["path"] refPath
  , tCode "code_log" "History" "log"
      "The commits that reach a ref, newest first, or those that touched a path; a page of them."
      [ pRef, pPath, propInt "skip" "Commits to skip, for the next page."
      , propInt "limit" "Commits in this page, at most 100. Default 30." ] []
      (\as -> do
          rp <- refPath as
          sk <- optNat "skip" as
          li <- optNat "limit" as
          pure (rp <> flagM "skip" (Text.pack . show <$> sk) <> flagM "limit" (Text.pack . show <$> li)))
  , tCode "code_commit" "A commit" "show"
      "One commit: author, committer, message, and its diff (a merge against its first parent), at most 512 KiB of it."
      [prop "commit" "The commit (a sha)."] ["commit"]
      (\as -> (\c -> [flag "commit" c]) <$> (optText "commit" as >>= needed "commit" ""))
  ]
  where
    refPath as = do
      r <- optText "ref" as
      p <- optText "path" as
      pure (flagM "ref" r <> flagM "path" p)

tPublish :: Tool
tPublish = Tool
  { toolName = "publish", toolTitle = "Publish canon"
  , toolDesc = "Push canon (refs/hbs2/meta) and the staged pull-request heads to a git "
      <> "remote. PUBLISHES: from here every event in local canon is PUBLIC and PERMANENT "
      <> "for everyone who fetches the remote. Canon is never force-pushed; when the remote "
      <> "holds canon this clone does not, nothing is written and sync is the next step. "
      <> "The verb has no dry run of its own: dry_run: true runs status (which asks the "
      <> "remote, read-only, whether canon there matches) and shows the command that would "
      <> "run." <> defaultsNote
  , toolProps = [pRemote, pRepo, pDry]
  , toolRequired = []
  , toolAccess = Writes, toolOpenWorld = True, toolVerb = ["publish"]
  , toolPlan = \cfg as -> do
      rm <- (`orElse` mcRemote cfg) <$> optText "remote" as
      r <- keyOr "repo" (mcRepo cfg) as
      dry <- optBool "dry_run" as
      let run = Run (["publish"] <> flagM "remote" rm) Nothing AsText
          probe r' = Run (["status", flag "repo" r'] <> flagM "remote" rm) Nothing AsText
      pure (if dry then Rehearse (probe <$> r) run else RunVerb run)
  }

tSync :: Tool
tSync = Tool
  { toolName = "sync", toolTitle = "Fetch code and canon"
  , toolDesc = "Fetch the branches, canon and the staged pull-request heads from a git "
      <> "remote into this clone. Canon fast-forwards; a divergence is reported and left "
      <> "alone (a compaction's rewrite is taken when it folds to the same state). Writes "
      <> "local refs only and publishes nothing. dry_run: true shows the command and runs "
      <> "status." <> defaultsNote
  , toolProps = [pRemote, pRepo, pDry]
  , toolRequired = []
  , toolAccess = Writes, toolOpenWorld = True, toolVerb = ["sync"]
  , toolPlan = \cfg as -> do
      rm <- (`orElse` mcRemote cfg) <$> optText "remote" as
      r <- keyOr "repo" (mcRepo cfg) as
      dry <- optBool "dry_run" as
      let run = Run (["sync"] <> flagM "remote" rm <> flagM "repo" r) Nothing AsText
          probe r' = Run (["status", flag "repo" r'] <> flagM "remote" rm) Nothing AsText
      pure (if dry then Rehearse (probe <$> r) run else RunVerb run)
  }

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just a) _ = Just a
orElse Nothing b  = b

------------------------------------------------------------------------------
-- Running a verb

data Outcome = Outcome
  { oCode     :: Int
  , oOut      :: Text
  , oErr      :: Text
  , oTimedOut :: Bool
  }

runVerb :: McpConfig -> Run -> IO Outcome
runVerb cfg r = do
  let pc = setStdin (maybe nullStream (byteStringInput . LBS.fromStrict . Text.encodeUtf8) (runStdin r))
         $ setStdout byteStringOutput
         $ setStderr byteStringOutput
         $ maybe id setWorkingDir (mcDir cfg)
         $ proc (mcHub cfg) (runArgv r)
  withChild pc $ \p ->
    timeout (mcTimeout cfg * 1000000) (waitExitCode p) >>= \case
      Nothing -> pure (Outcome 124 "" "" True)
      Just code -> do
        out <- atomically (getStdout p)
        err <- atomically (getStderr p)
        pure (Outcome (codeOf code) (decoded out) (decoded err) False)
  where
    codeOf = \case { ExitSuccess -> 0 ; ExitFailure n -> n }

-- | A child that is gone when this returns, however it returns.
--
-- NOT 'withProcessTerm' ALONE, and not 'readProcess', though both promise it.
-- Their cleanup closes the child's handles BEFORE it terminates the child, and
-- a 'byteStringOutput' reader blocked in a read holds its handle's lock: the
-- close waits for the read, the read for the child to exit, and the child for
-- the SIGTERM that comes after the close. So a verb that timed out, or a call
-- the client cancelled, hung its thread forever and left the verb running --
-- every @inbox_wait@ left a follower polling the peer after the server exited.
-- Terminating first ends the reads, and then the close has nothing to wait for.
--
-- SIGKILL five seconds later for a child that does not take the hint.
withChild :: ProcessConfig i o e -> (Process i o e -> IO a) -> IO a
withChild pc act = withProcessTerm pc $ \p -> act p `E.finally` stop p
  where
    stop p = do
      let ph = unsafeProcessHandle p
      P.terminateProcess ph
      void $ forkIO $ do
        threadDelay 5000000
        getExitCode p >>= \case
          Just _  -> pure ()
          Nothing -> P.getPid ph >>= maybe (pure ()) (signalProcess sigKILL)

decoded :: LBS.ByteString -> Text
decoded = Text.decodeUtf8With Text.lenientDecode . LBS.toStrict

-- | What an exit code means, from the table the verbs exit with.
exitMeaning :: Int -> Text
exitMeaning = \case
  0   -> "success"
  1   -> "a usage error: a bad argument, an unknown verb"
  2   -> "hub verify: the audit ran and found something"
  124 -> "the verb did not finish within the server's timeout and was stopped"
  141 -> "the output pipe closed"
  n | n >= 3 && n <= 16 -> "canon could not be read (see `hbs2-hub help verify`)"
    | (r:_) <- [ r | r <- refusals, codeFor r == n ] -> meaning r
    | n < 0 -> "killed by signal " <> Text.pack (show (negate n))
    | otherwise -> "an exit code this build does not document"

-- | The command line, as somebody would type it.
argvShown :: Run -> Text
argvShown r = Text.unwords ("hbs2-hub" : fmap quoted (runArgv r))
  <> maybe "" (\b -> "   (body on stdin, " <> Text.pack (show (BS.length (Text.encodeUtf8 b))) <> " bytes)")
           (runStdin r)
  where
    quoted a | all safe a, not (null a) = Text.pack a
             | otherwise = Text.pack (show a)
    safe c = not (isSpace c) && c `notElem` ("'\"\\$`;&|<>()*?!#~" :: String)

-- | Bounded, so that one verb cannot fill a model's context.
capped :: Text -> Text
capped t | Text.length t <= cap = t
         | otherwise = Text.take cap t <> "\n... [truncated: " <> Text.pack (show (Text.length t - cap))
                                          <> " more characters]"
  where cap = 200000

textBlock :: Text -> Value
textBlock t = object ["type" .= ("text" :: Text), "text" .= t]

result :: Bool -> [Text] -> Maybe Value -> Value
result isErr blocks structured = object $
  [ "content" .= fmap textBlock (if null blocks then ["(no output)"] else blocks)
  , "isError" .= isErr ]
  <> maybe [] (\s -> ["structuredContent" .= s]) structured

-- | A finished run as a tool result.
reported :: Run -> Outcome -> Value
reported r o
  | oTimedOut o = result True
      [ "stopped: " <> argvShown r <> "\n" <> exitMeaning 124 ] Nothing
  | ok = result False (outBlock <> notes) structured
  | otherwise = result True
      ( [ "hbs2-hub exited " <> Text.pack (show (oCode o)) <> ": " <> exitMeaning (oCode o)
            <> "\ncommand: " <> argvShown r
            <> (if Text.null (oErr o) then "" else "\n\n" <> capped (Text.strip (oErr o))) ]
        <> outBlock )
      Nothing
  where
    ok = oCode o == 0 || (runShape r == AsAudit && oCode o == 2)
    out = Text.strip (oOut o)
    outBlock = [ capped out | not (Text.null out) ]
    notes = [ "notes:\n" <> capped (Text.strip (oErr o)) | not (Text.null (Text.strip (oErr o))) ]
    -- Structured only when it parsed as an object: the specification wants an
    -- object there, and a document that did not parse is better as text than
    -- as nothing.
    structured = case runShape r of
      AsJson | Just v@(Object _) <- Aeson.decodeStrict (Text.encodeUtf8 out) -> Just v
      _ -> Nothing

-- | A dry run the verb could not do itself.
rehearsed :: McpConfig -> Maybe Run -> Run -> IO Value
rehearsed cfg probe run = do
  let planned = "DRY RUN: nothing was signed, written or sent. This verb has no dry run of "
                <> "its own, so this is the command a call without dry_run would run:\n  "
                <> argvShown run
  case probe of
    Nothing -> pure (result False [planned] Nothing)
    Just p -> do
      o <- runVerb cfg p
      let said = Text.strip (oOut o <> (if Text.null (Text.strip (oErr o)) then "" else "\n" <> oErr o))
          ok = oCode o == 0
      pure $ result (not ok)
        [ planned
        , "What `" <> argvShown p <> "` says now"
            <> (if ok then "" else " (exit " <> Text.pack (show (oCode o)) <> ": "
                                     <> exitMeaning (oCode o) <> ")")
            <> ":\n" <> capped said ]
        Nothing

------------------------------------------------------------------------------
-- Waiting for letters

-- | Run the follower until a letter turns up that is not in @known@, or the
-- time runs out.
--
-- The follower prints the whole queue first and then one document per read
-- that found something new. The first document is checked against @known@ when
-- the caller gave it, so a letter that arrived between the caller's last look
-- and this call is answered at once; without it the first document is only the
-- baseline.
waitInbox :: McpConfig -> (Double -> Double -> IO ()) -> [String] -> Maybe (HS.HashSet Text) -> Int
          -> IO Value
waitInbox cfg progress argv known secs = do
  start <- getMonotonicTime
  let deadline = start + fromIntegral secs
      pc = setStdin nullStream
         $ setStdout createPipe
         $ maybe id setWorkingDir (mcDir cfg)
         $ proc (mcHub cfg) argv
      shown = argvShown (Run argv Nothing AsJson)
  -- Stderr kept whole until the end: the follower says little there, and only
  -- a follower that stopped by itself has anything worth reporting.
  withChild (setStderr byteStringOutput pc) $ \p -> do
    ticking <- newIORef True
    _ <- forkIO $ let tick = do
                        threadDelay 15000000
                        on <- readIORef ticking
                        when on $ do
                          now <- getMonotonicTime
                          progress (now - start) (fromIntegral secs)
                          tick
                  in tick
    let next = do
          now <- getMonotonicTime
          let left = deadline - now
          if left <= 0 then pure (Left True) else
            timeout (ceiling (left * 1000000)) (E.try (BS.hGetLine (getStdout p))) >>= \case
              Nothing -> pure (Left True)
              Just (Left e) | isEOFError e -> pure (Left False)
                            | otherwise -> E.throwIO e
              Just (Right l) -> case Aeson.decodeStrict l of
                Just (Object d) -> pure (Right d)
                _ -> next   -- not a document; the follower prints nothing else on stdout
        lettersOf d = case KM.lookup "letters" d of
          Just (Array v) -> [ o | Object o <- toList v ]
          _ -> []
        hashOf o = case KM.lookup "message" o of
          Just (String s) -> Just s
          _ -> Nothing
        done arrived letters seen = do
          now <- getMonotonicTime
          pure $ result False
            [ if arrived
                then Text.pack (show (length letters)) <> " new letter(s)\n"
                       <> decoded (Aeson.encode (fmap Object letters))
                else "no new letter within " <> Text.pack (show secs) <> "s" ]
            (Just (object [ "arrived" .= arrived
                          , "letters" .= fmap Object letters
                          , "seen" .= List.sort (HS.toList seen)
                          , "waited_seconds" .= (round (now - start) :: Int) ]))
        -- The follower ended by itself: a peer that stopped answering, a
        -- mailbox the peer no longer holds, a usage error. An error here
        -- whatever the code, since only a signal ends it with 0 and none was
        -- sent.
        died = do
          code <- waitExitCode p
          e <- fromMaybe LBS.empty <$> timeout 2000000 (atomically (getStderr p))
          let n = case code of { ExitSuccess -> 0 ; ExitFailure k -> k }
              said = Text.strip (decoded e)
          pure $ result True
            [ "the follower stopped (exit " <> Text.pack (show n) <> ": " <> exitMeaning n
                <> ")\ncommand: " <> shown
                <> (if Text.null said then "" else "\n\n" <> capped said) ]
            Nothing
        follow seen = next >>= \case
          Left True  -> done False [] seen
          Left False -> died
          Right d -> do
            let ls = lettersOf d
                seen' = seen <> HS.fromList (mapMaybe hashOf ls)
            if null ls then follow seen' else done True ls seen'
    r <- next >>= \case
      Left True  -> done False [] (fromMaybe mempty known)
      Left False -> died
      Right d -> do
        let ls = lettersOf d
            here = HS.fromList (mapMaybe hashOf ls)
        case known of
          Just k | fresh@(_:_) <- [ l | l <- ls, maybe True (not . (`HS.member` k)) (hashOf l) ] ->
            done True fresh (k <> here)
          _ -> follow (fromMaybe mempty known <> here)
    writeIORef ticking False
    pure r

------------------------------------------------------------------------------
-- The protocol

-- | Serve until the input ends.
--
-- One message per line, as the stdio transport specifies. Requests are
-- answered concurrently, each tool call in its own thread, so that a client
-- may wait for letters and list issues at once; a cancelled read stops, and
-- the verb under it with it, while a write runs to its end.
serveMcp :: McpConfig -> Handle -> Handle -> IO ()
serveMcp cfg hin hout = do
  hSetBinaryMode hin True
  hSetBinaryMode hout True
  hSetBuffering hout (BlockBuffering Nothing)
  outLock <- newMVar ()
  calls <- newMVar (HM.empty :: HM.HashMap Value (IO (), MVar (), Bool))

  -- Uninterruptible, because a cancelled call is a killed thread, and a message
  -- cut off half way is a broken stream for every call after it.
  let send v = withMVar outLock $ \_ -> E.uninterruptibleMask_ $ do
        LBS.hPut hout (Aeson.encode v <> "\n")
        hFlush hout

      reply i v = send (object ["jsonrpc" .= ("2.0" :: Text), "id" .= i, "result" .= v])
      failure i c m = send (object [ "jsonrpc" .= ("2.0" :: Text), "id" .= i
                                   , "error" .= object ["code" .= (c :: Int), "message" .= (m :: Text)] ])

      notify m p = send (object ["jsonrpc" .= ("2.0" :: Text), "method" .= (m :: Text), "params" .= p])

      offered = toolsFor cfg

      request i method params = case method of
        "initialize" -> do
          let asked = case params of
                Object p | Just (String v) <- KM.lookup "protocolVersion" p -> Just v
                _ -> Nothing
              chosen = case asked of
                Just v | v `elem` protocolVersions -> v
                _ -> head protocolVersions
          reply i $ object
            [ "protocolVersion" .= chosen
            , "capabilities" .= object ["tools" .= object ["listChanged" .= False]]
            , "serverInfo" .= object [ "name" .= ("hbs2-hub" :: Text)
                                     , "title" .= ("hbs2-hub forge" :: Text)
                                     , "version" .= mcVersion cfg ]
            , "instructions" .= instructions cfg ]
        "ping" -> reply i (object [])
        "tools/list" -> reply i (object ["tools" .= fmap toolJson offered])
        "tools/call" -> do
          let p = case params of { Object o -> o ; _ -> KM.empty }
              name = case KM.lookup "name" p of { Just (String s) -> Just s ; _ -> Nothing }
              args = case KM.lookup "arguments" p of
                       Just (Object o) -> Right o
                       Nothing -> Right KM.empty
                       Just Null -> Right KM.empty
                       Just _ -> Left ()
              token = case KM.lookup "_meta" p of
                Just (Object m) -> KM.lookup "progressToken" m
                _ -> Nothing
          case (name >>= \n -> List.find ((== n) . toolName) offered, args) of
            (Nothing, _) -> failure i (-32602) ("unknown tool: " <> fromMaybe "(none)" name)
            (_, Left ()) -> failure i (-32602) "arguments must be an object"
            (Just t, Right as) -> spawnCall i (toolAccess t == Writes) $ do
              let progress done total = for_' token $ \tok ->
                    notify "notifications/progress" (object
                      [ "progressToken" .= tok, "progress" .= done, "total" .= total
                      , "message" .= ("waiting for letters" :: Text) ])
              r <- case planFor cfg t as of
                Left why -> pure (result True [why] Nothing)
                Right a -> act progress a
              reply i r
        _ -> failure i (-32601) ("method not found: " <> method)

      act progress = \case
        RunVerb r -> reported r <$> runVerb cfg r
        Rehearse probe r -> rehearsed cfg probe r
        WaitInbox argv known secs -> waitInbox cfg progress argv known secs

      -- A call in its own thread, registered so a cancellation can stop it.
      spawnCall i writes body = do
        gate <- newEmptyMVar
        finished <- newEmptyMVar
        tid <- forkIO $ (`E.finally` putMVar finished ()) $ do
          takeMVar gate
          E.try body >>= \case
            Left (e :: E.SomeException)
              -- Cancelled: no answer, which is what the specification asks
              -- of a request the client has withdrawn.
              | Just E.ThreadKilled <- E.fromException e -> pure ()
              | otherwise -> failure i (-32603) ("internal error: " <> Text.pack (show e))
            Right () -> pure ()
          modifyMVar_ calls (pure . HM.delete i)
        -- From a thread of its own, so that the reading loop never waits for
        -- a call that is masked (writing a message, or cleaning up its child).
        let stop = do
              modifyMVar_ calls (pure . HM.delete i)
              void (forkIO (E.throwTo tid E.ThreadKilled))
        modifyMVar_ calls (pure . HM.insert i (stop, finished, writes))
        putMVar gate ()

      notification method params = case method of
        "notifications/cancelled" -> case params of
          -- A READ is stopped. A write is not: half of a verb that writes is
          -- the one outcome nobody asked for (a letter folded and not dropped,
          -- an accept with no acknowledgement), and the specification lets a
          -- server finish what cannot be cancelled. Its answer is still sent,
          -- and the client ignores it.
          Object p | Just i <- KM.lookup "requestId" p -> do
            k <- withMVar calls (pure . HM.lookup i)
            for_' k $ \(stop, _, writes) -> unless writes stop
          _ -> pure ()
        -- notifications/initialized and anything else: nothing to do.
        _ -> pure ()

      message = \case
        Object o -> case (KM.lookup "method" o, KM.lookup "id" o) of
          (Just (String m), Just i) -> request i m (fromMaybe Null (KM.lookup "params" o))
          (Just (String m), Nothing) -> notification m (fromMaybe Null (KM.lookup "params" o))
          -- A response to a request: this server sends none, so there is
          -- nothing it could be answering.
          (Nothing, Just _) | KM.member "result" o || KM.member "error" o -> pure ()
          (_, i) -> failure (fromMaybe Null i) (-32600) "invalid request"
        Array _ -> failure Null (-32600) "batches are not supported"
        _ -> failure Null (-32600) "invalid request"

      loop = do
        eof <- hIsEOF hin
        unless eof $ do
          l <- BS.hGetLine hin
          unless (BS8.all isSpace l) $
            case Aeson.eitherDecodeStrict l of
              Left e -> failure Null (-32700) ("parse error: " <> Text.pack e)
              Right v -> message v
          loop

  -- The input ended: the client has gone. A read in flight is stopped; a write
  -- is left to finish, since half of a verb that writes is the one outcome
  -- nobody asked for, and its own timeout bounds it.
  --
  -- WAITED FOR, both kinds, because stopping a call is what terminates the
  -- verb under it, and a server that returned at once would leave the RTS to
  -- kill those threads before their cleanup ran: a follower started by
  -- inbox_wait then outlived the server, reparented to init, polling the peer.
  let shutdown = do
        cs <- HM.elems <$> readMVar calls
        sequence_ [ stop | (stop, _, False) <- cs ]
        void (timeout ((mcTimeout cfg + 10) * 1000000) (mapM_ (\(_, f, _) -> readMVar f) cs))
  loop `E.finally` shutdown
  where
    for_' m f = maybe (pure ()) f m

toolJson :: Tool -> Value
toolJson t = object
  [ "name" .= toolName t
  , "title" .= toolTitle t
  , "description" .= toolDesc t
  , "inputSchema" .= object
      [ "type" .= ("object" :: Text)
      , "properties" .= object [ Key.fromText k .= v | (k, v) <- toolProps t ]
      , "required" .= toolRequired t
      , "additionalProperties" .= False ]
  , "annotations" .= object
      [ "title" .= toolTitle t
      , "readOnlyHint" .= (toolAccess t == Reads)
      , "destructiveHint" .= (toolAccess t == Writes && toolName t /= "sync")
      , "idempotentHint" .= (toolAccess t == Reads)
      , "openWorldHint" .= toolOpenWorld t ]
  ]

instructions :: McpConfig -> Text
instructions cfg = Text.unlines $
  [ "hbs2-hub is a decentralized forge: issues and pull requests live in the git repository as"
  , "canon, a signed append-only log under refs/hbs2/meta; contributors send letters to an"
  , "encrypted ingress mailbox, and a maintainer folds them into canon or rejects them."
  , "Read tools fold canon locally and need no peer. Tools that write canon are permanent once"
  , "published: rehearse them with dry_run first. inbox_wait blocks until a letter arrives."
  ]
  <> [ "Default repository: " <> r | Just r <- [mcRepo cfg] ]
  <> [ "Default mailbox: " <> m | Just m <- [mcMailbox cfg] ]
  <> [ "This server is read-only: no tool writes." | mcReadOnly cfg ]
