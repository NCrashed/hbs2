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

        -- BEFORE helpEntries, because the dictionary is a left-biased union and
        -- the first binding of a name wins. Overrides the inherited `help`, which prints an "hbs2-cli tool" banner
        -- and is keyed by the full entry name. Both were wrong here: the banner
        -- names another tool, and the top-level help offers verbs as `inbox` and
        -- `issue new`, so `hbs2-hub help inbox` has to be the spelling that
        -- works. The prefixing goes through the same verbOf, so the two spellings
        -- cannot drift.
        entry $ bindMatch "help" $ nil_ \case
          [] -> liftIO (hubHelp dict)
          -- A name the dictionary holds arrives already evaluated, as the lambda
          -- it names carrying its own name. That is the full spelling and needs
          -- no prefixing.
          HelpEntryBound what -> helpEntry what
          -- A name it does not hold arrives as the word itself, which is where
          -- `inbox` and `issue new` come in: prefixed through the same verbOf the
          -- command line uses, so the spelling the top-level help prints and the
          -- spelling help accepts cannot drift apart.
          ws | Just (ListVal (SymbolVal k : _)) <- verbOf dict (fmap asWord ws) ->
                 helpEntry k
             | [StringLike s] <- ws -> helpList False (Just s)
             | otherwise -> liftIO (hubHelp dict)

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

  case (argv, verbOf dict argv) of
    -- No arguments: a script on stdin, or the help if there is no stdin either.
    ([], _) -> runHBS2Cli do
      eof <- liftIO IO.isEOF
      if eof
        then liftIO (hubHelp dict)
        else liftIO getContents
               >>= either (liftIO . die . show) pure . parseTop
               >>= \what -> recover (run dict what >>= eatNil display) >> silence

    -- Arguments that name nothing. Distinguished from the empty case, which it
    -- used to share: falling into the stdin branch made a typo exit zero after
    -- printing the help, and on a terminal or in a pipeline it waited for input
    -- nobody was going to send.
    (w:_, Nothing) -> die ("unknown verb: " <> w <> "\ntry: hbs2-hub --help")

    -- recover is what probes for the peer socket and builds the RPC clients;
    -- without it every verb that talks to hbs2-peer fails as "not connected".
    (_, Just form) -> runHBS2Cli (recover (run dict [form] >>= eatNil display) >> silence)

  where
    -- A dictionary name back to the word a user typed, so that `help` can route
    -- its argument through the same verbOf the command line does.
    asWord = \case
      SymbolVal (Id t) -> Text.unpack t
      LitStrVal t      -> Text.unpack t
      x                -> show (pretty x)

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

    -- One shell word becomes one atom, lexed by the same parser a script goes
    -- through. Not a hand-written list of cases: the first version quoted
    -- everything and lost integers, the second added a case for integers and
    -- still lost symbols and decimals, and it is symbols that --help and help
    -- match on to tell "the name of an entry" from "text to search for". A word
    -- typed at the shell and the same word in a script now get the same type,
    -- because they go through the same code.
    --
    -- Anything that does not lex as exactly one atom is a string, which is the
    -- only reading left for it.
    literal s = case parseTop s of
      Right [x] -> unwrap x
      _         -> mkStr @C s
      where
        -- parseTop makes a list per LINE, so a single atom on a line of its own
        -- comes back wrapped in one. Unwrapped here: a bare word from argv is an
        -- atom, not a call with no arguments, and leaving it wrapped is why
        -- `hbs2-hub help hub:inbox` answered BadFormException (help (hub:inbox)).
        unwrap (ListVal [x]) = x
        unwrap x = x
