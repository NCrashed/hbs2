-- | @hbs2-hub serve@: the hub in a browser, for a project without GitHub
-- (PEP-25). See "HBS2.Hub.Serve" for the server and what each mode allows.
module HBS2.Hub.CLI.Serve
  ( serveEntries
  , ServeArgs(..)
  , serveArgs
  , serveUsage
  , splitListen
  ) where

import HBS2.Hub.Host
import HBS2.Hub.Serve
import HBS2.Hub.Mcp (McpConfig(..),defaultMcpConfig)
import HBS2.Hub.CLI.Argv (badArgs,flagsAndSwitches,flagMaybe,flagSwitch,flagText,flagEvery,flagWord)
import HBS2.Hub.CLI.Common (refuse)
import HBS2.Hub.CLI.Defaults (keyShaped)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import Data.ByteString qualified as BS
import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Version (showVersion)
import Paths_hbs2_hub qualified as Version
import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath ((</>),takeDirectory)

import Text.Read (readMaybe)
import Control.Concurrent (forkIO,threadDelay)
import System.IO (hPutStrLn)
import Data.Maybe (fromMaybe,isNothing)

data ServeArgs = ServeArgs
  { saListen   :: Maybe String
  , saWeb      :: Maybe FilePath
  , saPublic   :: Bool
  , saRepos    :: [String]
  , saBusy     :: Maybe Int
  , saQueue    :: Maybe Int
  , saCache    :: Maybe Int
  }
  deriving stock (Eq,Show)

serveUsage :: Doc ()
serveUsage =
  "usage: hbs2-hub serve [--listen <host:port>] [--web <dir>]"
    <> line <> "       hbs2-hub serve --public --listen <host:port> --repo <key> [--repo <key>]... [--web <dir>]"

serveArgs :: forall c . IsContext c => [Syntax c] -> Maybe ServeArgs
serveArgs syn = do
  kvs <- flagsAndSwitches ["--listen", "--web", "--repo", "--busy", "--queue", "--cache"] ["--public"] syn
  listen <- flagMaybe kvs "--listen" flagText
  web <- flagMaybe kvs "--web" flagText
  pub <- flagSwitch kvs "--public"
  repos <- traverse flagText (flagEvery kvs "--repo")
  busy <- flagMaybe kvs "--busy" flagWord
  queue <- flagMaybe kvs "--queue" flagWord
  cache <- flagMaybe kvs "--cache" flagWord
  pure (ServeArgs listen web pub repos (fromIntegral <$> busy) (fromIntegral <$> queue) (fromIntegral <$> cache))

-- | @host:port@, the port last: an IPv6 host has colons of its own.
splitListen :: String -> Maybe (String, Int)
splitListen s = case List.break (== ':') (reverse s) of
  (rp, ':' : rh) | Just p <- readMaybe (reverse rp), p > 0, p < 65536, not (List.null rh) -> Just (reverse rh, p)
  _ -> Nothing

serveEntries :: forall c m . ( IsContext c
                             , MonadUnliftIO m
                             , Exception (BadFormException c)
                             ) => MakeDictM c m ()
serveEntries =
  brief "serve the hub's pages in a browser, for a project without GitHub"
    $ args [ arg "string" "[--listen host:port]", arg "string" "[--web dir]"
           , arg "string" "[--public]", arg "string" "[--repo key]..."
           , arg "string" "[--busy n]", arg "string" "[--queue n]", arg "string" "[--cache seconds]" ]
    $ desc ( "LOCAL, the default: on 127.0.0.1:8484, for you. It reads and"
             <> line <> "writes as the browser extension does, with your keys,"
             <> line <> "through the same tools and the same review before"
             <> line <> "anything is signed. It prints the address to open, which"
             <> line <> "carries a token; a request without it is refused, and so"
             <> line <> "is one that reached it by any name but its loopback one."
             <> line
             <> line <> "PUBLIC, with --public: read-only, for anyone, on the"
             <> line <> "--listen address, which it then needs. It shows the"
             <> line <> "repositories named with --repo and no other, from its own"
             <> line <> "cache clones, never from a working clone, and offers no"
             <> line <> "tool that writes. Its clones are synced every five minutes,"
             <> line <> "never while a request waits."
             <> line
             <> line <> "--busy bounds the verbs run at once (default 4 public, 8"
             <> line <> "local); --queue how many more wait for one, each up to 30"
             <> line <> "seconds (default 32 public, 64 local), and past it a call"
             <> line <> "is answered 503 with Retry-After. --cache is how long a"
             <> line <> "public answer is reused, in seconds (default 30, 0 for"
             <> line <> "never); the same call asked at once is run once."
             <> line
             <> line <> "The pages come from --web, else share/hbs2-hub/web beside"
             <> line <> "this binary, where the package puts them." )
    $ entry $ bindMatch "hub:serve" $ nil_ \case
        (serveArgs -> Just sa) -> liftIO (runIt sa)
        other -> liftIO (badArgs serveUsage other)
  where
    runIt sa = do
      self <- getExecutablePath
      let pub = saPublic sa
      when (pub && isNothing (saListen sa)) $
        refuse "a public server needs --listen, the address it is to be reached at" 2
      when (pub && List.null (saRepos sa)) $
        refuse "a public server needs --repo, once for each repository it shows" 2
      case [ r | r <- saRepos sa, not (keyShaped r) ] of
        [] -> pure ()
        bad -> refuse ("not a repository key: " <> List.intercalate ", " bad) 2
      (host, port) <- case maybe (Just ("127.0.0.1", 8484)) splitListen (saListen sa) of
        Just hp -> pure hp
        Nothing -> refuse "--listen is host:port, such as 127.0.0.1:8484" 2
      when (not pub && host `notElem` ["127.0.0.1", "localhost", "::1"]) $
        refuse ( "a local server listens on loopback only: it signs with your keys. "
                 <> "For a server others reach, use --public, which is read-only." ) 2

      web <- case saWeb sa of
        Just w -> pure w
        Nothing -> do
          real <- canonicalizePath self
          pure (takeDirectory (takeDirectory real) </> "share" </> "hbs2-hub" </> "web")
      have <- doesFileExist (web </> "index.html")
      unless have $
        refuse ("no pages at " <> web <> ": build them (hbs2-hub-browser, npm run build) and pass --web <dir>/dist/web") 2

      cache <- getXdgDirectory XdgCache ("hbs2-hub" </> if pub then "public" else "repos")
      let mcp = (defaultMcpConfig self)
                  { mcReadOnly = pub, mcVersion = Text.pack (showVersion Version.version) }
          hc = HostConfig mcp cache (if pub then 300 else 60) (not pub)
                 (if pub then Just (HS.fromList (fmap Text.pack (saRepos sa))) else Nothing)
                 (if pub then Just publicTools else Nothing)
                 (not pub)
      hub <- newHub hc
      -- A PUBLIC SERVER'S CLONES ON A TIMER, never on a stranger's request:
      -- made at start, then synced every five minutes, one after another.
      when pub $ void $ forkIO $ forever $ do
        for_ (saRepos sa) $ \k -> hubRefresh hub (Text.pack k) >>= \case
          Left e -> hPutStrLn stderr ("hbs2-hub serve: " <> k <> ": " <> Text.unpack (heMessage e))
          Right () -> pure ()
        threadDelay (hcSyncEach hc * 1000000)
      token <- if pub then pure "" else newToken
      let shown = if host == "::1" then "[::1]" else host
      if pub
        then putStrLn ("hbs2-hub serve: public, read-only, on http://" <> shown <> ":" <> show port <> "/")
        else putStrLn ("hbs2-hub serve: open http://" <> shown <> ":" <> show port <> "/#token=" <> Text.unpack token)
      hFlush stdout
      serveHttp (ServeConfig host port web (if pub then Public else Local) token
                   (fromMaybe (if pub then 4 else 8) (saBusy sa))
                   (fromMaybe (if pub then 32 else 64) (saQueue sa))
                   30
                   (fromMaybe 30 (saCache sa))) hub

    -- 24 bytes from the kernel, as hex: a token nobody guesses.
    newToken = do
      bytes <- withBinaryFile "/dev/urandom" ReadMode (`BS.hGet` 24)
      pure (Text.pack (concatMap hex (BS.unpack bytes)))
    hex w = [ "0123456789abcdef" !! fromIntegral (w `div` 16), "0123456789abcdef" !! fromIntegral (w `mod` 16) ]
