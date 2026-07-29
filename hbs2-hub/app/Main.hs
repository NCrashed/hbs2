-- | The @hbs2-hub@ CLI (PEP-22).
--
-- A thin driver over the PEP-18..21 library and hbs2-cli's command machinery.
-- The verbs live in their own modules; this is argument handling, the dictionary
-- and the help, so that adding a verb is adding a module and one line.
module Main where

import HBS2.Hub.CLI.Compose
import HBS2.Hub.CLI.Inbox

import HBS2.CLI.Prelude
import HBS2.CLI.Run
import HBS2.CLI.Run.Help

import Data.Config.Suckless.Script.File qualified as SF

import Control.Applicative ((<|>))
import Data.HashMap.Strict qualified as HM
import Data.List (intercalate,isPrefixOf,sort)
import Data.Text qualified as Text

import System.Environment
import System.Exit (die)
import System.IO qualified as IO

setupLogger :: MonadIO m => m ()
setupLogger = do
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStderr . logPrefix ""
  setLogging @INFO   $ toStderr . logPrefix ""

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE

main :: IO ()
main = do
  setupLogger

  argv <- getArgs

  let dict = makeDict do
        internalEntries
        inboxEntries
        composeEntries
        helpEntries
        SF.entries

        -- Own top-level help. The inherited one lists every builtin the
        -- suckless script dictionary carries, alphabetically, which put the two
        -- verbs this tool has at lines 91 and 92 of 214. A first look at a tool
        -- should show what the tool does.
        entry $ bindMatch "--help" $ nil_ \case
          HelpEntryBound what -> helpEntry what
          [StringLike s]      -> helpList False (Just s)
          _                   -> liftIO (hubHelp dict)

  case verbOf dict argv of
    Nothing -> runHBS2Cli do
      eof <- liftIO IO.isEOF
      if eof
        then liftIO (hubHelp dict)
        else liftIO getContents
               >>= either (liftIO . die . show) pure . parseTop
               >>= \what -> recover (run dict what >>= eatNil display) >> silence

    -- recover is what probes for the peer socket and builds the RPC clients;
    -- without it every verb that talks to hbs2-peer fails as "not connected".
    Just form -> runHBS2Cli (recover (run dict [form] >>= eatNil display) >> silence)

  where
    -- What this tool does, rather than what its interpreter can do.
    hubHelp dict = do
      let named (Id t) = Text.unpack t
          verbs = sort [ n | k <- HM.keys dict, let n = named k, "hub:" `isPrefixOf` n ]
      putStrLn "hbs2-hub: a decentralized forge over hbs2 (PEP-17..22)"
      putStrLn ""
      putStrLn "usage: hbs2-hub <noun> <verb> [args]"
      putStrLn ""
      forM_ verbs $ \v ->
        putStrLn ("  " <> fmap (\c -> if c == ':' then ' ' else c) (drop 4 v))
      putStrLn ""
      putStrLn "  hbs2-hub help <verb>    what one verb takes"
      putStrLn "  hbs2-hub --help <text>  search every entry, including the"
      putStrLn "                          suckless script builtins this shares"

    -- The surface PEP-22 specifies is `hub <noun> <verb>`; the dictionary is
    -- keyed by `hub:noun:verb`, so `hub inbox show X` and `(hub:inbox:show X)`
    -- are one entry with one help text and one arity.
    --
    -- Which words are the command and which are its arguments is decided by
    -- ASKING THE DICTIONARY, longest match first, rather than by a rule about
    -- what an argument looks like. The rule was "join the first two plain
    -- words", and it turned `hub inbox <key>` into `inbox:<key>`: a base58 key
    -- is a plain word too. Anything a guess like that gets wrong, it gets wrong
    -- at the moment somebody is trying to use the tool.
    --
    -- The result is a form built from argv DIRECTLY, not a string to re-parse.
    -- Two bugs came out of that round trip: joining with spaces lost the
    -- boundaries the shell had already decided, and quoting everything to get
    -- them back turned every integer argument into a string, which is what
    -- `hub pr merge <n>` and half the inherited dictionary match on. There is
    -- nothing to guess if the tokens are never re-lexed.
    verbOf dict argv = case argv of
      [] -> Nothing
      (w:rest) -> go (min 3 (length argv))
        -- No noun-verb match, so this is a name from the dictionary this tool
        -- shares with hbs2-cli, spelled out. Kept reachable rather than made
        -- script-only: the entries are there either way, and a surface that
        -- silently swallows a name it holds is worse than one that does not
        -- hold it.
        <|> (if bound w then Just (mkList (mkSym @C w : fmap literal rest)) else Nothing)
      where
        go 0 = Nothing
        go n = case splitAt n argv of
          (ws, rest) | all plain ws, bound (name ws) ->
                         Just (mkList (mkSym @C (name ws) : fmap literal rest))
                     | otherwise -> go (n - 1)

        name ws = "hub:" <> intercalate ":" ws
        bound s = HM.member (fromString s) dict
        plain (c:_) = c `notElem` ("-(\"'[" :: String)
        plain []    = False

    -- One shell word becomes one atom, with its own type preserved: an integer
    -- stays an integer, a form written as one argument is still parsed as a
    -- form, and everything else is a string, which every reader in this project
    -- accepts wherever it accepts a symbol.
    literal s
      | any (`isPrefixOf` s) ["(", "["] =
          case parseTop s of
            Right [x] -> x
            _         -> mkStr @C s
      | Just n <- readMay @Integer s = mkInt n
      | otherwise = mkStr @C s
