-- | The @hub@ CLI (PEP-22).
--
-- A thin driver over the PEP-18..21 library and hbs2-cli's command machinery.
-- The verbs live in their own modules; this is argument handling and the
-- dictionary, so that adding a verb is adding a module and one line.
module Main where

import HBS2.Hub.CLI.Inbox

import HBS2.CLI.Prelude
import HBS2.CLI.Run
import HBS2.CLI.Run.Help

import Data.Config.Suckless.Script.File qualified as SF

import Data.HashMap.Strict qualified as HM
import Data.List (intercalate)

import System.Environment
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
        helpEntries
        SF.entries

        entry $ bindMatch "--help" $ nil_ \case
          HelpEntryBound what -> helpEntry what
          [StringLike s]      -> helpList False (Just s)
          _                   -> helpList False Nothing

  cli <- either (error . show) pure (parseTop (unwords (verbOf dict argv)))

  runHBS2Cli do
    case cli of
      [] -> do
        eof <- liftIO IO.isEOF
        if eof
          then void $ run dict [mkForm "help" []]
          else liftIO getContents
                 >>= either (error . show) pure . parseTop
                 >>= \what -> recover (run dict what >>= eatNil display)
      -- recover is what probes for the peer socket and builds the RPC clients;
      -- without it every verb that talks to hbs2-peer fails as "not connected".
      _ -> recover (run dict cli >>= eatNil display) >> silence

  where
    -- The surface PEP-22 specifies is `hub <noun> <verb>`; the dictionary is
    -- keyed by `hub:noun:verb`, so that `hub inbox show X` and
    -- `(hub:inbox:show X)` are one entry with one help text and one arity.
    --
    -- Which words are the command and which are its arguments is decided by
    -- ASKING THE DICTIONARY, longest match first, rather than by a rule about
    -- what an argument looks like. The rule was "join the first two plain
    -- words", and it turned `hub inbox <key>` into `inbox:<key>`: a base58 key
    -- is a plain word too. Anything a guess like that gets wrong, it gets
    -- wrong at the moment somebody is trying to use the tool.
    verbOf dict argv = go (min 3 (length argv))
      where
        go 0 = argv
        go n = case splitAt n argv of
          (ws, rest) | all plain ws, bound (name ws) -> name ws : rest
                     | otherwise -> go (n - 1)

        name ws = "hub:" <> intercalate ":" ws
        bound s = HM.member (fromString s) dict
        plain (c:_) = c `notElem` ("-(\"'" :: String)
        plain []    = False
