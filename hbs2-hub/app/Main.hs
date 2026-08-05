-- | The @hbs2-hub@ CLI (PEP-22).
--
-- A thin driver over the PEP-18..21 library and hbs2-cli's command machinery.
-- The verbs live in their own modules; this is argument handling, the dictionary
-- and the help, so that adding a verb is adding a module and one line.
module Main where

import HBS2.Hub.Types (safeText)
import HBS2.Hub.CLI.Argv (verbOf)
import HBS2.Hub.CLI.Accept
import HBS2.Hub.CLI.Ban
import HBS2.Hub.CLI.Comment
import HBS2.Hub.CLI.Compose
import HBS2.Hub.CLI.Inbox
import HBS2.Hub.CLI.Maintainer
import HBS2.Hub.CLI.Own
import HBS2.Hub.CLI.Policy
import HBS2.Hub.CLI.Pr
import HBS2.Hub.CLI.Read
import HBS2.Hub.CLI.Show
import HBS2.Hub.CLI.Reject
import HBS2.Hub.CLI.Verify

import HBS2.CLI.Prelude
import HBS2.CLI.Run
import HBS2.CLI.Run.Help

import Data.Config.Suckless.Script.File qualified as SF

import Data.HashMap.Strict qualified as HM
import Data.List (isPrefixOf,sort)
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
        acceptEntries
        commentEntries
        composeEntries
        verifyEntries
        readEntries
        showEntries
        prEntries
        maintainerEntries
        rejectEntries
        ownEntries
        policyEntries
        banEntries

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
          ws | Just (ListVal (SymbolVal k : _)) <- verbOf (bound dict) (fmap asWord ws) ->
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
          ws | Just (ListVal (SymbolVal k : _)) <- verbOf (bound dict) (fmap asWord ws) ->
                 helpEntry k
             | [StringLike s] <- ws -> found dict s
             | otherwise -> liftIO (hubHelp dict)

  case (argv, verbOf (bound dict) argv) of
    -- No arguments: the help, and NOTHING ELSE. This branch used to read stdin
    -- and run it as a script, and that was arbitrary code execution from any
    -- byte source that happened to land on stdin.
    --
    -- Two things made it that rather than a convenience. First, the dictionary
    -- below inherits SF.entries, so it binds `rm`, `touch`, `mkdir`, `mv`, `cp`
    -- and the whole `run:proc:*` family. Second, and this is the half that made
    -- garbage dangerous, the interpreter evaluates a form's ARGUMENTS before it
    -- resolves the form's head, so a line that is not a command at all still
    -- runs every form nested inside it. `NameNotBound` is raised afterwards, and
    -- the damage is already done. HBS2.Hub.CLI.Argv records this same hazard
    -- being removed from the ARGV path; the stdin path kept it.
    --
    -- What made it reachable by a stranger: the comment that used to be here
    -- said "a hook is exactly where a script arrives on stdin", and git feeds
    -- pre-receive and post-receive `<old> <new> <ref-name>` on stdin, where the
    -- ref name is chosen by whoever pushes. Ref names forbid space and control
    -- characters but ALLOW parentheses and quotes, so
    -- `refs/heads/x(rm"/path")` passes git check-ref-format and deleted the
    -- file. Verified against the built binary, with `touch`, `mkdir` and `rm`.
    --
    -- The hook use case does not need this: `hub verify <key>` takes argv, and
    -- a hook should call it that way, with stdin left alone. Scripting keeps its
    -- explicit spelling, `hbs2-hub --run <file>`, which also covers a pipeline
    -- through `--run /dev/stdin` for anyone who really wants it. What is gone is
    -- only the IMPLICIT reading, which nobody can opt out of.
    --
    -- Still worth doing separately: this tool has three verbs and does not need
    -- `rm`/`mv`/`cp`/`run:proc:*` in its dictionary at all. Dropping SF.entries
    -- would remove the primitives even if the two conditions above ever come
    -- back. That is a decision about what the tool's scripting surface is, so it
    -- is not taken here.
    ([], _) -> hubHelp dict

    -- Arguments that name nothing. Distinguished from the empty case, which it
    -- used to share: falling into the stdin branch made a typo exit zero after
    -- printing the help, and on a terminal or in a pipeline it waited for input
    -- nobody was going to send.
    -- THROUGH safeText, like every other stranger's bytes this program prints.
    -- argv is a stranger's bytes on the one path where nothing has looked at it
    -- yet: a verb with an ESC in it went straight to the terminal, and a verb
    -- with a newline and a plausible second line forged the advice under it.
    (w:_, Nothing) -> die ( "unknown verb: " <> Text.unpack (safeText (Text.pack w))
                              <> "\ntry: hbs2-hub --help" )

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
        -- On stderr and non-zero, like the unknown-verb branch thirty lines up.
        -- This printed the miss on STDOUT and exited 0, so `hub help "$v" || die`
        -- learned nothing and the diagnostic landed in the stream a caller was
        -- capturing as output.
        [] -> liftIO $ refuse ("no entry name starts with " <> show s
                                 <> " (this matches names, not descriptions)") 1
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
    -- Every read verb, and they are peerless for the same reason `verify` is:
    -- canon is a git ref in this repository and the fold over it needs nothing
    -- else. Measured on the same wedged-peer case the note above describes,
    -- this is the difference between a listing and a hang.
    -- The WRITE verbs that reach nothing but this repository are here too, and
    -- they were the ones the list most needed. Their constraints say so:
    -- `banEntries` and `maintainerEntries` ask for neither HasStorage nor
    -- HasClientAPI, so they cannot call a peer, and `pr merge` reaches only the
    -- keyman, git and canon. Going through `recover` for them cost a
    -- `hbs2-peer poke` with no timeout, so against a wedged peer the verbs that
    -- hang were the ones an operator reaches for when things are going wrong,
    -- with `ban list` beside them answering instantly.
    peerFreeNames = [ "hub:verify"
                    , "hub:issue:list", "hub:issue:show"
                    , "hub:pr:list", "hub:pr:show"
                    , "hub:log", "hub:maintainer:list", "hub:ban:list"
                    , "hub:ban", "hub:unban"
                    , "hub:maintainer:add", "hub:maintainer:remove"
                    , "hub:pr:merge"
                    , "hub:issue:close", "hub:issue:reopen", "hub:issue:label"
                    , "hub:redact"
                    ] <> helpNames

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

    -- Whether the dictionary holds a name, as a function.
    --
    -- 'verbOf' and the atom reader it uses moved to "HBS2.Hub.CLI.Argv", where a
    -- test can reach them. They decide what the TITLE OF AN ISSUE is -- the bytes
    -- that go into a signed author box and into an event-id -- and while they
    -- lived in this file nothing could ask them a question, which is how a title
    -- containing a semicolon came to be silently truncated and then signed. The
    -- dictionary itself stays here, so what crosses the boundary is this one
    -- predicate rather than the map.
    bound dict s = HM.member (fromString s) dict
