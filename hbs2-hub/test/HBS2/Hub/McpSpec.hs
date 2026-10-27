-- | @hub mcp@: the tools an agent is offered, and the server, over a pipe.
--
-- Two halves. What a call WOULD run is a pure function of the tool, the
-- server's defaults and the arguments, so it is asked directly: which verb,
-- which flags, which refusals. What the server DOES is asked the way a client
-- asks it, as JSON-RPC lines over a pipe, against a repository with real canon
-- in it and the real binary running the verb.
module HBS2.Hub.McpSpec (spec) where

import HBS2.Hub.Mcp
import HBS2.Hub.Types
import HBS2.Hub.Repo
import HBS2.Hub.Repo.GitWrite
import HBS2.Hub.CLI.Argv (verbOf)
import HBS2.Hub.CLI.Dict (hubEntries,ours)
import HBS2.Hub.CLI.Mcp (McpArgs(..),configFrom)

import HBS2.CLI.Prelude (pretty)
import HBS2.CLI.Run (makeDict,internalEntries)
import HBS2.CLI.Run.Internal (HBS2Cli)
import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))
import Data.Config.Suckless (C,Id(..))

import Control.Concurrent (forkIO,killThread)
import Control.Monad (void,forM_)
import Data.Aeson (Value(..),object,(.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.List (isInfixOf,isPrefixOf,nub)
import Data.Maybe (fromMaybe,isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import System.Directory (findExecutable)
import System.Environment qualified as Env
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.IO qualified as Posix
import System.Process.Typed
import System.Timeout (timeout)
import Test.Hspec

type KP = (HubKey, PrivKey 'Sign HubScheme)

kp :: IO KP
kp = do
  c <- newCredentials @'HBS2Basic
  pure (_peerSignPk c, _peerSignSk c)

b58 :: HubKey -> Text
b58 = Text.pack . show . pretty . AsBase58

-- | The dictionary the binary answers to, as "HBS2.Hub.DictSpec" builds it.
theDict :: HM.HashMap Id ()
theDict = HM.map (const ()) $ HM.filterWithKey (\k _ -> ours k) $
  makeDict @C @(HBS2Cli IO) (internalEntries >> hubEntries)

cfg0 :: McpConfig
cfg0 = (defaultMcpConfig "hbs2-hub") { mcRepo = Just repoKey }
  where repoKey = "7VM1rtxrTmpXYVidNFHu5ZbNqRKMhKoRAV8vJebGaRN9"

toolNamed :: Text -> Tool
toolNamed n = case [ t | t <- tools, toolName t == n ] of
  (t:_) -> t
  []    -> error ("no tool " <> Text.unpack n)

plan :: McpConfig -> Text -> [(Text, Value)] -> Either Text Action
plan cfg n as = planFor cfg (toolNamed n) (KM.fromList [ (Key.fromText k, v) | (k, v) <- as ])

argvOf :: Either Text Action -> Maybe [String]
argvOf = \case
  Right (RunVerb r) -> Just (runArgv r)
  _ -> Nothing

spec :: Spec
spec = tooling >> server

tooling :: Spec
tooling = describe "hub mcp: the tools" $ do

  it "names every tool once" $
    let ns = fmap toolName tools in nub ns `shouldBe` ns

  -- A tool whose verb the dictionary does not hold is a tool that answers
  -- "unknown verb" to every call, which nothing but a call would reveal.
  it "runs only verbs this build has" $
    forM_ tools $ \t ->
      (toolName t, verbOf (\s -> HM.member (Id (Text.pack s)) theDict) (toolVerb t) /= Nothing)
        `shouldBe` (toolName t, True)

  it "gives every write a dry run, and says canon is permanent where it is written" $
    forM_ [ t | t <- tools, toolAccess t == Writes ] $ \t -> do
      (toolName t, "dry_run" `elem` fmap fst (toolProps t)) `shouldBe` (toolName t, True)
      (toolName t, any (`Text.isInfixOf` toolDesc t) ["PERMANENT", "not undoable", "publishes nothing"])
        `shouldBe` (toolName t, True)

  it "offers no write to a read-only server" $ do
    let ro = toolsFor cfg0 { mcReadOnly = True }
    [ toolName t | t <- ro, toolAccess t == Writes ] `shouldBe` []
    length ro `shouldSatisfy` (> 10)

  it "fills the repository from the server's default" $
    argvOf (plan cfg0 "issue_list" [("status", "open")])
      `shouldBe` Just ["issue","list","--repo=7VM1rtxrTmpXYVidNFHu5ZbNqRKMhKoRAV8vJebGaRN9","--status=open","--json"]

  it "asks for the JSON contract on show" $
    argvOf (plan cfg0 "issue_show" [("number", Number 7)])
      `shouldBe` Just ["issue","show","--repo=7VM1rtxrTmpXYVidNFHu5ZbNqRKMhKoRAV8vJebGaRN9","--number=7","--json"]

  -- An argument nobody claims is a refusal, like a flag nobody claims: a
  -- misspelt dry_run that is ignored is a write somebody meant to rehearse.
  it "refuses an argument the tool does not take" $
    plan cfg0 "issue_close" [("number", Number 7), ("dryrun", Bool True)]
      `shouldSatisfy` either ("dryrun" `Text.isInfixOf`) (const False)

  it "refuses a key that is not one before running anything" $
    plan cfg0 "issue_list" [("repo", "not-a-key")] `shouldSatisfy` either (const True) (const False)

  it "passes a value that starts with a dash as a value" $
    argvOf (plan cfg0 "issue_open" [("title", "--dry-run"), ("dry_run", Bool False)])
      `shouldBe` Just ["issue","open","--repo=7VM1rtxrTmpXYVidNFHu5ZbNqRKMhKoRAV8vJebGaRN9","--title=--dry-run"]

  it "sends a body on stdin, never in argv" $
    case plan cfg0 "issue_open" [("title", "t"), ("body", "a\nb")] of
      Right (RunVerb r) -> do
        runStdin r `shouldBe` Just "a\nb"
        runArgv r `shouldSatisfy` elem "--body=-"
      other -> expectationFailure (show other)

  it "rehearses the verb's own dry run where it has one" $
    argvOf (plan cfg0 "issue_close" [("number", Number 3), ("dry_run", Bool True)])
      `shouldSatisfy` maybe False (elem "--dry-run")

  it "rehearses without running where the verb has none" $
    case plan cfg0 { mcMailbox = Just "E6gFsJYdCf5SHvFgw6A683MgqerMsppgYWKGpP4D1umd" } "inbox_reject"
           [("message", "6emnPfjcUddSbasux8EEe1bR2nuuHfd1tWJzpUJxLUAr"), ("dry_run", Bool True)] of
      Right (Rehearse (Just probe) run) -> do
        take 2 (runArgv probe) `shouldBe` ["inbox","show"]
        take 2 (runArgv run) `shouldBe` ["inbox","reject"]
      other -> expectationFailure (show other)

  -- PEP-24 phases 2 and 3: what the browser's forms run.
  it "files an issue and proposes a pull request as letters, with the server's identity" $ do
    let me = cfg0 { mcAuthor = Just "3a8Rv9dbVQAprna3GzgfihUpq64wCSLX2nyfhgDB5MH3"
                  , mcSender = Just "HK6W3fanQz1z9DVymPrkiVqTsJquWfk1oAUnmXBp8xg6" }
    case plan me "issue_new" [("title", "t"), ("body", "b"), ("labels", Aeson.toJSON ["bug" :: Text])] of
      Right (RunVerb r) -> do
        take 2 (runArgv r) `shouldBe` ["issue","new"]
        runArgv r `shouldSatisfy` elem "--label=bug"
        runStdin r `shouldBe` Just "b"
      other -> expectationFailure (show other)
    -- onto and from left out are the verb's to fill from the clone.
    case plan me "pr_new" [("title", "t")] of
      Right (RunVerb r) -> do
        take 2 (runArgv r) `shouldBe` ["pr","new"]
        runArgv r `shouldSatisfy` (not . any ("--onto" `isPrefixOf`))
      other -> expectationFailure (show other)
    case plan me "pr_new" [("title", "t"), ("dry_run", Bool True)] of
      Right (Rehearse (Just probe) _) -> take 1 (runArgv probe) `shouldBe` ["whoami"]
      other -> expectationFailure (show other)
    plan cfg0 "issue_new" [("title", "t")] `shouldSatisfy` either ("author" `Text.isInfixOf`) (const False)

  it "lands a pull request with the verb's own dry run" $ do
    argvOf (plan cfg0 "pr_land" [("number", Number 4), ("dry_run", Bool True)])
      `shouldBe` Just ["pr","land","--repo=7VM1rtxrTmpXYVidNFHu5ZbNqRKMhKoRAV8vJebGaRN9","--number=4","--dry-run"]
    plan cfg0 "pr_land" [("number", Number 4), ("mirror", "gh"), ("no_mirror", Bool True)]
      `shouldSatisfy` either (const True) (const False)

  -- The browser's inbox names the repository and nothing else: the verbs read
  -- the mailbox out of its manifest, and the tools were stricter than them.
  it "opens and rejects a letter with only the repository" $ do
    let msg = ("message", "6emnPfjcUddSbasux8EEe1bR2nuuHfd1tWJzpUJxLUAr")
    argvOf (plan cfg0 "inbox_show" [msg]) `shouldSatisfy` maybe False (not . any ("--mailbox" `isPrefixOf`))
    argvOf (plan cfg0 "inbox_reject" [msg]) `shouldSatisfy` maybe False (\a -> take 2 a == ["inbox","reject"])

  it "will not clear labels because none were given" $
    plan cfg0 "issue_label" [("number", Number 3)] `shouldSatisfy` either (const True) (const False)

  it "says what an exit code means" $ do
    exitMeaning 26 `shouldSatisfy` ("no thread" `Text.isInfixOf`)
    exitMeaning 2 `shouldSatisfy` ("found" `Text.isInfixOf`)

  -- The read verbs that print a document are asked for it, and the document
  -- comes back as structured content.
  it "asks status, log and inbox show for their JSON documents" $ do
    let shaped = \case
          Right (RunVerb r) -> Just (runShape r, last (runArgv r))
          _ -> Nothing
    shaped (plan cfg0 "status" []) `shouldBe` Just (AsJson, "--json")
    shaped (plan cfg0 "log" [("number", Number 2)]) `shouldBe` Just (AsJson, "--json")
    argvOf (plan cfg0 "log" [("number", Number 2)])
      `shouldBe` Just ["log","--repo=7VM1rtxrTmpXYVidNFHu5ZbNqRKMhKoRAV8vJebGaRN9","--number=2","--json"]
    shaped (plan cfg0 { mcMailbox = Just "E6gFsJYdCf5SHvFgw6A683MgqerMsppgYWKGpP4D1umd" }
                 "inbox_show" [("message", "6emnPfjcUddSbasux8EEe1bR2nuuHfd1tWJzpUJxLUAr")])
      `shouldBe` Just (AsJson, "--json")

  -- Without a configured key the verb chooses the signer itself, from
  -- HBS2_HUB_AS and git config in the clone it runs in. An --as here would
  -- override both and be reported as an explicit flag.
  it "passes --as only when the server or the call names one" $ do
    let deleg = "E6gFsJYdCf5SHvFgw6A683MgqerMsppgYWKGpP4D1umd"
        other = "6emnPfjcUddSbasux8EEe1bR2nuuHfd1tWJzpUJxLUAr"
        asOf r = [ a | a <- fromMaybe [] (argvOf r), "--as" `isPrefixOf` a ]
    asOf (plan cfg0 "issue_close" [("number", Number 3)]) `shouldBe` []
    asOf (plan cfg0 { mcAs = Just deleg } "issue_close" [("number", Number 3)])
      `shouldBe` ["--as=" <> Text.unpack deleg]
    asOf (plan cfg0 { mcAs = Just deleg } "issue_close" [("number", Number 3), ("as", String other)])
      `shouldBe` ["--as=" <> Text.unpack other]

  it "does not turn HBS2_HUB_AS into a server default" $ do
    let deleg = "E6gFsJYdCf5SHvFgw6A683MgqerMsppgYWKGpP4D1umd"
        none = McpArgs Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing False
    old <- Env.lookupEnv "HBS2_HUB_AS"
    Env.setEnv "HBS2_HUB_AS" (Text.unpack deleg)
    fromEnv <- configFrom "hbs2-hub" none
    fromFlag <- configFrom "hbs2-hub" none { maAs = Just deleg }
    -- A value that is not a key is the verb's to refuse, not the server's.
    Env.setEnv "HBS2_HUB_AS" "not-a-key"
    broken <- configFrom "hbs2-hub" none
    maybe (Env.unsetEnv "HBS2_HUB_AS") (Env.setEnv "HBS2_HUB_AS") old
    mcAs fromEnv `shouldBe` Nothing
    mcAs fromFlag `shouldBe` Just deleg
    mcAs broken `shouldBe` Nothing

  it "replies onto canon through issue reply, the body on stdin" $ do
    case plan cfg0 "issue_reply" [("number", Number 7), ("body", "can you try 1.2?")] of
      Right (RunVerb r) -> do
        runArgv r `shouldBe` [ "issue","reply","--repo=7VM1rtxrTmpXYVidNFHu5ZbNqRKMhKoRAV8vJebGaRN9"
                             , "--number=7","--body=-" ]
        runStdin r `shouldBe` Just "can you try 1.2?"
      other -> expectationFailure (show other)
    argvOf (plan cfg0 "issue_reply" [ ("number", Number 7), ("body", "x"), ("kind", "pr")
                                    , ("dry_run", Bool True) ])
      `shouldSatisfy` maybe False (\a -> take 2 a == ["pr","reply"] && "--dry-run" `elem` a)
    plan cfg0 "issue_reply" [("number", Number 7), ("body", "  \n")]
      `shouldSatisfy` either (const True) (const False)
    plan cfg0 "issue_reply" [("body", "x")]
      `shouldSatisfy` either (const True) (const False)
    [ toolName t | t <- toolsFor cfg0 { mcReadOnly = True }, toolName t == "issue_reply" ]
      `shouldBe` []

------------------------------------------------------------------------------

-- | A repository with one issue in canon, written the way canon is written.
withCanonRepo :: (FilePath -> HubKey -> IO a) -> IO a
withCanonRepo act = withSystemTempDirectory "hub-mcp" $ \dir -> do
  path <- fromMaybe "/usr/bin:/bin" <$> Env.lookupEnv "PATH"
  let git as = runProcess_ $ setStdin nullStream $ setStdout nullStream
             $ setEnv [ ("GIT_CONFIG_GLOBAL","/dev/null"), ("GIT_CONFIG_SYSTEM","/dev/null")
                      , ("HOME", dir), ("LC_ALL","C"), ("PATH", path) ]
             $ setWorkingDir dir (proc "git" as)
  git ["init","-q","."]
  owner <- kp
  let repo = fst owner
      sq = 1 :: Word64
      ev = mkEvent owner owner
             (AOpen repo HubIssue "the tests hang on aarch64" [] (Just "body text") Nothing Nothing 1000)
             (\eid -> CanonContent repo eid sq (Just 1) Nothing Nothing sq Nothing)
      p = threadDir (eventId ev) <> "/" <> eventFileName sq (eventId ev)
  cw <- either (fail . show) pure (planCanon Nothing (const Nothing) [(p, ev)] [(sq, eventId ev)])
  void $ withGitSinkIn (Just dir) $ \sk ->
    skCommit sk (CanonWrite Nothing (cwFiles cw) "canon" 1000) >>= either (fail . show) pure
  act dir repo

-- | A client: one request at a time, answered by id.
data Client = Client { cIn :: Handle, cOut :: Handle }

call :: Client -> Int -> Text -> Value -> IO (KM.KeyMap Value)
call c i m params = do
  send c (object ["jsonrpc" .= ("2.0" :: Text), "id" .= i, "method" .= m, "params" .= params])
  answer c i

send :: Client -> Value -> IO ()
send c v = LBS.hPut (cIn c) (Aeson.encode v <> "\n") >> hFlush (cIn c)

answer :: Client -> Int -> IO (KM.KeyMap Value)
answer c i = timeout 60000000 go >>= maybe (fail "no answer within a minute") pure
  where
    go = do
      l <- BS.hGetLine (cOut c)
      case Aeson.decodeStrict l of
        Just (Object o) | KM.lookup "id" o == Just (Number (fromIntegral i)) -> pure o
        Just _ -> go
        Nothing -> fail ("not JSON: " <> show l)

withServer :: McpConfig -> (Client -> IO a) -> IO a
withServer cfg act = do
  (r1, w1) <- Posix.createPipe
  (r2, w2) <- Posix.createPipe
  [sin', cin, cout, sout] <- mapM Posix.fdToHandle [r1, w1, r2, w2]
  tid <- forkIO (serveMcp cfg sin' sout)
  a <- act (Client cin cout)
  hClose cin
  killThread tid
  pure a

field :: Text -> KM.KeyMap Value -> Maybe Value
field k = KM.lookup (Key.fromText k)

resultOf :: KM.KeyMap Value -> KM.KeyMap Value
resultOf o = case field "result" o of
  Just (Object r) -> r
  _ -> error ("not a result: " <> show o)

textOf :: KM.KeyMap Value -> String
textOf r = case field "content" r of
  Just (Array bs) -> concat [ Text.unpack t | Object b <- foldr (:) [] bs, Just (String t) <- [field "text" b] ]
  _ -> ""

server :: Spec
server = do
  -- The binary under test: HBS2_HUB_EXE, else the hbs2-hub cabal puts on PATH
  -- for this suite (build-tool-depends). Skipped rather than failed without
  -- one, like the git specs without git.
  hub <- runIO $ Env.lookupEnv "HBS2_HUB_EXE" >>= \case
           Just p -> pure (Just p)
           Nothing -> findExecutable "hbs2-hub"
  hasGit <- runIO (isJust <$> findExecutable "git")
  let guarded act = case hub of
        Just h | hasGit -> act h
        _ -> pendingWith "needs git and an hbs2-hub binary (HBS2_HUB_EXE)"

  describe "hub mcp: the server, over a pipe" $ do

    it "shakes hands, lists its tools and reads canon through one" $ guarded $ \h ->
      withCanonRepo $ \dir repo -> do
        let cfg = (defaultMcpConfig h) { mcDir = Just dir, mcRepo = Just (b58 repo) }
        withServer cfg $ \c -> do
          ini <- resultOf <$> call c 1 "initialize" (object
                   [ "protocolVersion" .= ("2025-06-18" :: Text)
                   , "capabilities" .= object []
                   , "clientInfo" .= object ["name" .= ("spec" :: Text), "version" .= ("0" :: Text)] ])
          field "protocolVersion" ini `shouldBe` Just "2025-06-18"
          send c (object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("notifications/initialized" :: Text)])

          ts <- resultOf <$> call c 2 "tools/list" (object [])
          let names = case field "tools" ts of
                Just (Array v) -> [ n | Object t <- foldr (:) [] v, Just (String n) <- [field "name" t] ]
                _ -> []
          names `shouldBe` fmap toolName tools

          -- The read-only call: the verb runs in the fixture, folds canon,
          -- and the title comes back.
          ls <- resultOf <$> call c 3 "tools/call" (object
                  ["name" .= ("issue_list" :: Text), "arguments" .= object []])
          field "isError" ls `shouldBe` Just (Bool False)
          textOf ls `shouldSatisfy` ("the tests hang on aarch64" `isInfixOf`)

          -- The render contract, as structured content.
          sh <- resultOf <$> call c 4 "tools/call" (object
                  ["name" .= ("issue_show" :: Text), "arguments" .= object ["number" .= (1 :: Int)]])
          case field "structuredContent" sh of
            Just (Object d) -> do
              field "contract" d `shouldBe` Just (Number 1)
              field "title" d `shouldBe` Just "the tests hang on aarch64"
            other -> expectationFailure ("no structured content: " <> show other)

          -- A refusal is a result with its exit code and what it means.
          no <- resultOf <$> call c 5 "tools/call" (object
                  ["name" .= ("issue_show" :: Text), "arguments" .= object ["number" .= (9 :: Int)]])
          field "isError" no `shouldBe` Just (Bool True)
          textOf no `shouldSatisfy` ("exited 26" `isInfixOf`)

          -- The log as its document: provenance, the open that is in canon.
          lg <- resultOf <$> call c 6 "tools/call" (object
                  ["name" .= ("log" :: Text), "arguments" .= object []])
          field "isError" lg `shouldBe` Just (Bool False)
          case field "structuredContent" lg of
            Just (Object d) -> do
              field "document" d `shouldBe` Just "log"
              case field "events" d of
                Just (Array es) -> [ field "op" e | Object e <- foldr (:) [] es ]
                                     `shouldBe` [Just "open"]
                other -> expectationFailure ("no events: " <> show other)
            other -> expectationFailure ("no structured content: " <> show other)

    it "answers protocol errors as protocol errors" $ guarded $ \h ->
      withServer (defaultMcpConfig h) $ \c -> do
        ini <- resultOf <$> call c 1 "initialize" (object ["protocolVersion" .= ("1999-01-01" :: Text)])
        -- A version it does not speak is answered with the one it does.
        field "protocolVersion" ini `shouldBe` Just (String (head protocolVersions))
        _ <- resultOf <$> call c 2 "ping" (object [])
        unknown <- call c 3 "tools/call" (object ["name" .= ("rm" :: Text)])
        (field "error" unknown >>= \case { Object e -> field "code" e ; _ -> Nothing })
          `shouldBe` Just (Number (-32602))
        nomethod <- call c 4 "resources/list" (object [])
        (field "error" nomethod >>= \case { Object e -> field "code" e ; _ -> Nothing })
          `shouldBe` Just (Number (-32601))
        -- Without a repository anywhere, the call says where one comes from.
        r <- resultOf <$> call c 5 "tools/call" (object ["name" .= ("issue_list" :: Text)])
        textOf r `shouldSatisfy` ("--repo" `isInfixOf`)
