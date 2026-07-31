-- | The @hbs2-hub@ CLI (PEP-22).
--
-- A thin driver over the PEP-18..21 library and hbs2-cli's command machinery.
-- The verbs live in their own modules; this is argument handling, the dictionary
-- and the help, so that adding a verb is adding a module and one line.
module Main where

import HBS2.Hub.CLI.Compose
import HBS2.Hub.CLI.Inbox
import HBS2.Hub.CLI.Verify

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
import GHC.IO.Encoding qualified as Enc
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

  -- The output encoding is chosen here rather than inherited, because what this
  -- program prints is not this program's text: a thread title, an attribute name
  -- and a tree path all come out of a stranger's signed bytes, and canon does not
  -- have a locale. Under LC_ALL=C (a git hook, a cron job, a Docker image with no
  -- locale archive) GHC picks ASCII, and `hub verify` on a repository holding one
  -- Cyrillic path died with "commitBuffer: invalid argument" AFTER printing part
  -- of the report: an audit that exits non-zero having said something true and
  -- incomplete, which is the worst answer an audit has.
  --
  -- //TRANSLIT on the way OUT only, and it is not about UTF-8 being unable to
  -- represent something (it cannot fail on a real character). It is for the lone
  -- surrogates //ROUNDTRIP below produces out of argv bytes that are not UTF-8:
  -- without it, printing an argument nobody could decode kills the program in the
  -- encoder, halfway through a report.
  utf8Translit <- IO.mkTextEncoding "UTF-8//TRANSLIT"
  for_ [IO.stdout, IO.stderr] (`IO.hSetEncoding` utf8Translit)

  -- stdin is UTF-8 and NOT lenient. It carries the body of a letter, which is
  -- about to be signed and published, and //TRANSLIT there turns a body somebody
  -- typed in KOI8 or Latin-1 into a body full of U+FFFD, signs it, and sends it,
  -- where an exception would have told them to convert the file. Being strict is
  -- the whole difference between a bad afternoon and an unfixable event.
  IO.hSetEncoding IO.stdin IO.utf8

  -- And argv, which is not a Handle and so is covered by none of the above.
  -- getArgs decodes with the FILESYSTEM encoding, which the locale picks: under
  -- LC_ALL=C that is ASCII, and `hub issue new --title <eight UTF-8 bytes>` became
  -- eight replacement characters. For `hub verify` that would be cosmetic. It is
  -- not cosmetic here: a title from argv goes into the signed author box and into
  -- the event-id, so in a hook under the C locale a letter would be minted, sealed
  -- and delivered with the corruption inside the signature, where canon is
  -- append-only and nothing can repair it.
  utf8Roundtrip <- IO.mkTextEncoding "UTF-8//ROUNDTRIP"
  Enc.setFileSystemEncoding utf8Roundtrip
  Enc.setForeignEncoding utf8Roundtrip

  argv <- getArgs

  -- And then refused if it did not decode. //ROUNDTRIP represents an undecodable
  -- byte as a lone surrogate, which survives a String and does NOT survive
  -- Text.pack: text collapses it to U+FFFD, silently, on the first conversion,
  -- which every word of argv goes through on its way to a signed box. So the
  -- corruption the block above exists to prevent came back one layer down. There
  -- is no repair to offer, only a refusal before anything is signed.
  for_ argv $ \a ->
    when (any (\c -> c >= '\xD800' && c <= '\xDFFF') a) $
      die ( "an argument is not valid UTF-8, and this tool signs what you type."
              -- No advice about locales: the filesystem encoding is set above,
              -- before getArgs, so these bytes decode the same under LC_ALL=C and
              -- under C.UTF-8 and re-running changes nothing. And not "use stdin"
              -- either: stdin is strict UTF-8 now and would refuse the same bytes.
              -- What actually converts them is a converter.
              <> "\nConvert it first, e.g. with iconv -f <encoding> -t UTF-8." )

  let dict = makeDict do
        internalEntries
        inboxEntries
        composeEntries
        verifyEntries

        -- BEFORE helpEntries, because the dictionary is a left-biased union and
        -- the first binding of a name wins. Overrides the inherited `help`, which
        -- prints an "hbs2-cli tool" banner
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
             -- found, not helpList: a word the dictionary does not hold printed one
             -- empty line and exited zero, while --help said so. Two spellings of
             -- one verb, and only one of them admitted it had found nothing.
             | [StringLike s] <- ws -> found dict s
             | otherwise -> liftIO (hubHelp dict)

        helpEntries
        SF.entries

        -- Own top-level help. The inherited one lists every builtin the
        -- suckless script dictionary carries, alphabetically, which put the two
        -- verbs this tool has at lines 91 and 92 of 214. A first look at a tool
        -- should show what the tool does.
        -- Through the same verbOf as `help`, so that a spelling the top-level
        -- help prints works under both. They had diverged: `help inbox` found
        -- the entry and `--help inbox` printed nothing, while the comment beside
        -- the prefixing claimed the point of it was that they could not.
        entry $ bindMatch "--help" $ nil_ \case
          HelpEntryBound what -> helpEntry what
          ws | Just (ListVal (SymbolVal k : _)) <- verbOf dict (fmap asWord ws) ->
                 helpEntry k
             | [StringLike s] <- ws -> found dict s
             | otherwise -> liftIO (hubHelp dict)

  case (argv, verbOf dict argv) of
    -- No arguments: a script on stdin, or the help if there is no stdin either.
    ([], _) -> runHBS2Cli do
      -- A TERMINAL means there is no script coming: a person typed the name of
      -- the program and pressed return. Reading stdin then sat there until they
      -- worked out that Ctrl-D was expected of them. A pipe or a file still gets
      -- read, which is what a hook uses.
      tty <- liftIO (IO.hIsTerminalDevice IO.stdin)
      eof <- if tty then pure True else liftIO IO.isEOF
      if eof
        then liftIO (hubHelp dict)
        else liftIO getContents
               >>= either (liftIO . die . show) pure . parseTop
               -- The same question as for a command line, and it was not asked
               -- here: a script whose whole content is one audit paid the peer
               -- probe anyway, which is the cost this branch exists to avoid in a
               -- git hook, and a hook is exactly where a script arrives on stdin.
               >>= \what ->
                     if all (peerFree dict) what
                       then (run dict what >>= eatNil display) >> silence
                       else recover (run dict what >>= eatNil display) >> silence

    -- Arguments that name nothing. Distinguished from the empty case, which it
    -- used to share: falling into the stdin branch made a typo exit zero after
    -- printing the help, and on a terminal or in a pipeline it waited for input
    -- nobody was going to send.
    (w:_, Nothing) -> die ("unknown verb: " <> w <> "\ntry: hbs2-hub --help")

    -- recover is what probes for the peer socket and builds the RPC clients;
    -- without it every verb that talks to hbs2-peer fails as "not connected".
    -- Skipped for the verbs that do not: it pokes hbs2-peer with no timeout, so a
    -- wedged peer hung the audit hook that reading canon out of a git ref exists
    -- to be runnable without one. Measured at 1.55 s with a live peer and 6.0 s
    -- against a stub, for a verb that needs neither.
    (_, Just form)
      | peerFree dict form -> runHBS2Cli (run dict [form] >>= eatNil display >> silence)
      | otherwise -> runHBS2Cli (recover (run dict [form] >>= eatNil display) >> silence)

  where
    -- A prefix search that says so when it matches nothing, instead of printing
    -- an empty line. It is a prefix match on NAMES, not a search of the
    -- descriptions, which is worth being honest about: `--help mailbox` finds
    -- nothing though the mailbox verb is the one thing this tool is for.
    found dict s = do
      let named (Id t) = Text.unpack t
      case sort [ n | k <- HM.keys dict, let n = named k, s `isPrefixOf` n ] of
        [] -> liftIO $ putStrLn ("no entry name starts with " <> show s
                                  <> " (this matches names, not descriptions)")
        _  -> helpList False (Just s)

    -- A dictionary name back to the word a user typed, so that `help` can route
    -- its argument through the same verbOf the command line does.
    asWord = \case
      SymbolVal (Id t) -> Text.unpack t
      LitStrVal t      -> Text.unpack t
      x                -> show (pretty x)

    -- Verbs that reach nothing but the local repository. Named here rather than
    -- declared per verb, because the list is short and a wrong entry fails loudly
    -- (the verb reports "not connected" the first time anybody runs it), whereas a
    -- verb wrongly absent from it fails quietly by being slow.
    peerFreeNames :: [Id]
    -- help too, and measured: `--help` took 1.553 s against `verify`'s 0.012 s,
    -- all of it the peer probe, which is readProcess of `hbs2-peer poke` with no
    -- timeout. Against a wedged peer the two commands that hang are the two you
    -- would run to find out how to audit without one.
    peerFreeNames = ["hub:verify"] <> helpNames

    helpNames :: [Id]
    helpNames = ["help", "--help"]

    -- The WHOLE form, not its head. A head-symbol test said yes to
    -- @(hub:verify (hub:inbox:show X))@, whose argument reaches the peer and then
    -- failed as "not connected" inside a verb that had been declared not to need
    -- one. An argument is a form here, and this is a script interpreter.
    --
    -- Only names the dictionary actually holds are examined: everything else in a
    -- form is data, and a repository key parses as a symbol like any other word.
    -- Unknown names are somebody else's error to report, not a reason to guess.
    peerFree dict = \case
      -- help NAMES a verb, it does not run it, so what it names says nothing
      -- about whether a peer is needed: `help hub:inbox` paid the probe to print
      -- a paragraph.
      --
      -- Only for BARE names. help is an ordinary binding and the interpreter
      -- evaluates arguments before calling it, so `help (hub:inbox <key>)` really
      -- does run hub:inbox; declaring the whole form peer-free took the RPC
      -- clients away from it and turned a working command into a bare
      -- PeerNotConnectedException.
      ListVal (SymbolVal k : as) | k `elem` helpNames -> all bare as
      form -> go form
      where
        go = \case
          ListVal xs -> all go xs
          SymbolVal k | HM.member k dict -> k `elem` peerFreeNames
          _ -> True

        -- An atom. A form here is a call, and a call is not a name.
        bare = \case
          ListVal _ -> False
          _         -> True

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
      Right [x] | isForm s  -> x
                | otherwise -> unwrap x
      _ -> mkStr @C s
      where
        -- parseTop makes a list per LINE, so a single atom on a line of its own
        -- comes back wrapped in one. Unwrapped, because a bare word from argv is
        -- an atom and not a call with no arguments: leaving it wrapped is why
        -- `hbs2-hub help hub:inbox` answered BadFormException (help (hub:inbox)).
        --
        -- Only for a word, though, and this is why the text is consulted after
        -- all. The wrapper around a lone atom and a genuine one-element form are
        -- the SAME value, so nothing in the parsed result can tell them apart:
        -- unwrapping unconditionally turned `(list)` into the bare symbol `list`,
        -- which evaluates to a lambda instead of calling it. What distinguishes
        -- them is what the user typed, and only there.
        isForm t = any (`isPrefixOf` t) ["(", "["]
        unwrap (ListVal [x]) = x
        unwrap x = x
