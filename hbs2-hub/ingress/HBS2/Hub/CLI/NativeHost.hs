-- | @hbs2-hub native-host@, as a verb: serve a browser extension, or install the
-- files that let a browser start this (PEP-24).
module HBS2.Hub.CLI.NativeHost
  ( nativeHostEntries
  , NativeArgs(..)
  , nativeArgs
  , firefoxExtensionId
  , chromeExtensionId
  , manifestFor
  , Browser(..)
  ) where

import HBS2.Hub.Mcp (McpConfig(..),defaultMcpConfig)
import HBS2.Hub.NativeHost
import HBS2.Hub.CLI.Argv (badArgs,flagsAndSwitches,flagMaybe,flagSwitch,flagText)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import Data.Aeson (object,(.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import Paths_hbs2_hub qualified as Version
import System.Directory
import System.Environment (getExecutablePath,lookupEnv)
import Control.Monad (filterM)
import Data.Maybe (listToMaybe)
import System.FilePath ((</>),takeDirectory)

data NativeArgs
  = Serve Bool                          -- ^ read-only
  | Install Bool Text Text              -- ^ read-only, the Chrome id, the Firefox id
  | Uninstall
  deriving stock (Eq,Show)

-- | The id the Firefox build of the extension is published under.
firefoxExtensionId :: Text
firefoxExtensionId = "hbs2-hub@hbs2.app"

-- | The id Chrome and Chromium give the extension.
--
-- Chrome derives an id from the extension's public key, and the build carries
-- that key (keys/chrome-public.b64 in hbs2-hub-browser), so an unpacked build
-- has this id on every machine. It was a required flag, and the one command an
-- upgrade asks for skipped both browsers without it.
chromeExtensionId :: Text
chromeExtensionId = "llmffghhboheiadaadikapcdifeolepc"

nativeArgs :: forall c . IsContext c => [Syntax c] -> Maybe NativeArgs
nativeArgs syn = do
  kvs <- flagsAndSwitches ["--chrome-id", "--firefox-id"] ["--install", "--uninstall", "--read-only"] syn
  ins <- flagSwitch kvs "--install"
  uni <- flagSwitch kvs "--uninstall"
  ro  <- flagSwitch kvs "--read-only"
  ch  <- flagMaybe kvs "--chrome-id" (fmap Text.pack . flagText)
  ff  <- flagMaybe kvs "--firefox-id" (fmap Text.pack . flagText)
  case (ins, uni) of
    (True, False) -> Just (Install ro (fromMaybe chromeExtensionId ch) (fromMaybe firefoxExtensionId ff))
    (False, True) -> Just Uninstall
    (False, False) | ch == Nothing && ff == Nothing -> Just (Serve ro)
    _ -> Nothing

nativeUsage :: Doc ()
nativeUsage =
  "usage: hbs2-hub native-host [--read-only]"
  <> line <> "       hbs2-hub native-host --install [--chrome-id <id>] [--firefox-id <id>] [--read-only]"
  <> line <> "       hbs2-hub native-host --uninstall"

data Browser = Firefox | Chrome | Chromium
  deriving stock (Eq,Show,Enum,Bounded)

-- | Where a browser looks for a native host's manifest, for this user.
manifestDir :: FilePath -> Browser -> FilePath
manifestDir home = \case
  Firefox  -> home </> ".mozilla" </> "native-messaging-hosts"
  Chrome   -> home </> ".config" </> "google-chrome" </> "NativeMessagingHosts"
  Chromium -> home </> ".config" </> "chromium" </> "NativeMessagingHosts"

-- | The manifest: which program, and which extension may start it.
--
-- The browser enforces the last field. It is what stops any other extension,
-- and every web page, from reaching this program at all.
manifestFor :: Browser -> FilePath -> Text -> LBS.ByteString
manifestFor b wrapper ext = Aeson.encode $ object $
  [ "name" .= hostName
  , "description" .= ("hbs2-hub: issues and pull requests over hbs2" :: Text)
  , "path" .= wrapper
  , "type" .= ("stdio" :: Text) ]
  <> case b of
       Firefox -> [ "allowed_extensions" .= [ext] ]
       _       -> [ "allowed_origins" .= ["chrome-extension://" <> ext <> "/"] ]

nativeHostEntries :: forall c m . ( IsContext c
                                  , MonadUnliftIO m
                                  , Exception (BadFormException c)
                                  ) => MakeDictM c m ()
nativeHostEntries =
  brief "serve the browser extension over native messaging, or install it"
    $ args [ arg "string" "[--install]", arg "string" "[--uninstall]"
           , arg "string" "[--chrome-id id]", arg "string" "[--firefox-id id]"
           , arg "string" "[--read-only]" ]
    $ desc ( "The browser starts this itself; nobody runs it by hand except"
             <> line <> "to install it. It serves the `hub mcp` tools to the hbs2-hub"
             <> line <> "browser extension (PEP-24), each one a child process of this"
             <> line <> "binary. A call for a repository runs in the clone named by"
             <> line <> "`git config --global hbs2-hub.<repo-key>.clone <path>`, or,"
             <> line <> "for a read, in a cache clone under ~/.cache/hbs2-hub/repos."
             <> line <> "A write never runs in the cache."
             <> line
             <> line <> "--install writes a small launcher next to this user's data"
             <> line <> "and the host manifest for Firefox, Chrome and Chromium,"
             <> line <> "naming the extension allowed to start it. The browser lets"
             <> line <> "no other extension and no web page reach it. Run it again"
             <> line <> "after upgrading hbs2, since the launcher names this binary."
             <> line <> "--read-only installs a host that offers no tool that writes."
             <> line <> "--chrome-id and --firefox-id name another build of the"
             <> line <> "extension; the defaults are the ids the published one has." )
    $ entry $ bindMatch "hub:native-host" $ nil_ \case
        (nativeArgs -> Just na) -> liftIO (runIt na)
        other -> liftIO (badArgs nativeUsage other)
  where
    runIt = \case
      Serve ro -> do
        self <- getExecutablePath
        cache <- getXdgDirectory XdgCache ("hbs2-hub" </> "repos")
        let mcp = (defaultMcpConfig self)
                    { mcReadOnly = ro, mcVersion = Text.pack (showVersion Version.version) }
        serveNative (HostConfig mcp cache 60) stdin stdout

      Install ro chrome firefox -> do
        real <- getExecutablePath
        home <- getHomeDirectory
        self <- stablePath home real
        dataDir <- getXdgDirectory XdgData "hbs2-hub"
        let wrapper = dataDir </> "native-host"
        createDirectoryIfMissing True dataDir
        -- A launcher and not the binary itself: a browser passes its own
        -- arguments (an origin, or a manifest path and an id) and no others.
        writeFile wrapper $ unlines
          [ "#!/bin/sh"
          , "# Written by `hbs2-hub native-host --install`. Re-run it after upgrading."
          , "exec " <> show self <> " native-host" <> (if ro then " --read-only" else "") ]
        p <- getPermissions wrapper
        setPermissions wrapper (setOwnerExecutable True p)
        say ("launcher" <+> pretty wrapper)
        forM_ [minBound .. maxBound] $ \b -> case b of
          Firefox -> put home b wrapper firefox
          _       -> put home b wrapper chrome

      Uninstall -> do
        home <- getHomeDirectory
        dataDir <- getXdgDirectory XdgData "hbs2-hub"
        forM_ [minBound .. maxBound] $ \b -> do
          let f = manifestDir home b </> Text.unpack hostName <> ".json"
          e <- doesFileExist f
          when e (removeFile f >> say ("removed" <+> pretty f))
        let w = dataDir </> "native-host"
        e <- doesFileExist w
        when e (removeFile w >> say ("removed" <+> pretty w))

    put home b wrapper ext = do
      let dir = manifestDir home b
          f = dir </> Text.unpack hostName <> ".json"
      -- Only where the browser is: a config directory that does not exist is
      -- a browser this user does not have, and creating it would be clutter.
      have <- doesDirectoryExist (takeDirectory dir)
      if have
        then do
          createDirectoryIfMissing True dir
          LBS.writeFile f (manifestFor b wrapper ext)
          say (pretty (show b) <> ":" <+> pretty f <+> parens ("allows" <+> pretty ext))
        else say (pretty (show b) <> ": not installed here, skipped")

    say :: Doc () -> IO ()
    say = print

    -- A path that survives an upgrade. Under Nix the running binary is a store
    -- path, which the next `nix profile upgrade` and a garbage collection take
    -- away, and the launcher with it. A profile link that resolves to this same
    -- binary names it as well and follows upgrades; the first one found wins,
    -- and the real path is the fallback.
    stablePath home real = do
      user <- fromMaybe "" <$> lookupEnv "USER"
      let candidates =
            [ home </> ".nix-profile" </> "bin" </> "hbs2-hub"
            , "/etc/profiles/per-user" </> user </> "bin" </> "hbs2-hub"
            , "/run/current-system/sw/bin/hbs2-hub" ]
      realC <- canonicalizePath real
      let same c = do
            e <- doesFileExist c
            if e then (== realC) <$> canonicalizePath c else pure False
      found <- filterM same candidates
      pure (fromMaybe real (listToMaybe found))
