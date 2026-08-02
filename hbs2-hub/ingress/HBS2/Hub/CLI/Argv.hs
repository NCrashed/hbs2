-- | Turning a command line into a form (PEP-22 @hub \<noun\> \<verb\>@).
--
-- In the sublibrary and not in @app/Main.hs@, for the reason the cabal file
-- gives about the ingress: a module a test cannot import is a module with no
-- tests. What lives here decides what the title of an issue IS -- the bytes that
-- go into a signed author box and into an event-id -- and while it lived in the
-- executable nothing could ask it a question.
module HBS2.Hub.CLI.Argv
  ( argvAtom
  , verbOf
  ) where

import HBS2.CLI.Prelude

import Control.Applicative ((<|>))
import Data.List (intercalate)

-- | One shell word, as one atom.
--
-- ARGV IS NOT A SCRIPT. The previous version of this handed every word to the
-- script lexer and kept whatever came back, on the argument that "a word typed
-- at the shell and the same word in a script now get the same type, because they
-- go through the same code". They should not get the same type. A script is
-- written to be lexed; an argument is text a person typed at a shell that has
-- already done all the tokenising anybody asked for. What that cost, measured on
-- the binary:
--
-- > hbs2-hub display 'fix; see later'   ->  fix
-- > hbs2-hub display 'Crash on startup' ->  NameNotBound (Id "Crash")
-- > hbs2-hub display 007                ->  7
-- > hbs2-hub display head               ->  (builtin:lambda head)
--
-- The first is the one that cannot be taken back. @;@ is the comment character,
-- a bare word lexes as a symbol, and 'StringLike' matches a symbol -- so
-- @hub issue new ... 'fix; see later'@ bound the title @fix@, put it in the
-- author box, signed it and sent it. The title is inside the signature and
-- inside the event-id, and canon is append-only. The second is worse in daily
-- use and harmless in canon: a title with a space in it, which is to say a
-- title, could not be passed at all.
--
-- So the lexer is asked for one thing only, and believed about even less:
--
--   * a number or a boolean is taken only if rendering it gives back exactly the
--     characters typed, which is what keeps @hub pr merge 12@ an integer while
--     leaving @007@ alone. 007 and 7 are different words, and the lexer's opinion
--     that they are the same number is about a number, not about what somebody
--     typed.
--   * everything else is a string, verbatim. Symbols in particular: a bare word
--     from argv is data, and evaluating it as a name is how @head@ became a
--     lambda.
--
-- A word starting with @(@ or @[@ USED to be parsed as a form and handed to the
-- evaluator, described here as "the script escape hatch and the one place a user
-- is asking to be lexed". It was neither, and this is why it is gone.
--
-- It applied to the VALUE OF A FLAG, not only to a word in the verb position,
-- and the evaluator evaluates a verb's arguments before the verb's pattern match
-- runs. The dictionary this tool shares with hbs2-cli holds @run:proc:quiet@,
-- @call:proc@, @rm@, @mv@, @cp@, @setenv@ and @cd@, so on this build
--
-- > hbs2-hub inbox '(run:proc:quiet "sh" "-c" "touch /tmp/proof")'
--
-- created the file and then printed the inbox usage. PEP-22 has a renderer that
-- "shells out to the CLI" with text a stranger wrote on the web, which is the
-- version of that where nobody typed anything.
--
-- It also contradicted the paragraph above it. @--title '(pwd)'@ would have
-- signed what the form EVALUATED to, and this tool's whole claim is that it
-- signs what was typed; @--title '[bug] segfault'@ died with
-- @NameNotBound (Id \"bug\")@, and a bracketed tag is how a great many people
-- spell an issue title. Both are now the strings they look like.
--
-- Nothing is unreachable as a result. A form as the FIRST word never got here
-- anyway (@verbOf@'s @plain@ excludes a leading bracket, so it exits as an
-- unknown verb), and a script still arrives the two ways it always did: on
-- stdin, and through @--run \<file\>@.
--
-- A quoted word gets no special treatment either, and that is a CHANGE. It used
-- to be handed to the lexer on the argument that "the quotes are the request",
-- and the lexer runs @readLitChar@ over the inside: @'"C:\\temp"'@ arrived as
-- @C:\<TAB\>emp@ and would have been signed that way -- the same class of defect
-- as the semicolon, one branch over. The quoting workaround existed only because
-- a multi-word title could not be passed any other way, and it can now, so the
-- lossy path is gone rather than documented. @'"a b"'@ is the four-character
-- string with its quotes, because that is what was typed.
--
-- Nothing downstream needs the symbols. 'StringLike' matches @LitStrVal@ as well
-- as @SymbolVal@, and @SignPubKeyLike@ and @HashLike@ go through the same door,
-- so every key, sigil and hash still binds. What changes is that they bind to
-- the bytes that were typed.
argvAtom :: String -> Syntax C
-- parseTop makes a list per LINE, so a single atom on a line of its own comes
-- back wrapped in one. Requiring exactly that shape is also what rejects a word
-- holding more than one atom: @Crash on startup@ parses fine and is three
-- symbols, which is not an argument.
argvAtom s = case parseTop s of
  Right [ListVal [x]] | keeps x -> x
  _ -> mkStr @C s
  where
    -- The ONLY thing the lexer is believed about, and only when its answer is
    -- the characters it was given. Anything else -- a symbol, a quoted string, a
    -- word holding several atoms -- is text.
    --
    -- It is also what makes the one-element shape above safe to unwrap. The
    -- wrapper around a lone atom and a genuine one-element form are the SAME
    -- value, so nothing in the parsed result can tell them apart, and unwrapping
    -- unconditionally turned @(list)@ into the bare symbol @list@, which
    -- evaluates to a lambda instead of calling it. A List is not a literal, so
    -- it never gets past here.
    keeps x = case x of
      LitIntVal _        -> roundTrips
      LitScientificVal _ -> roundTrips
      LitBoolVal _       -> roundTrips
      _                  -> False
      where roundTrips = show (pretty x) == s

-- | Which words are the command and which are its arguments.
--
-- The surface PEP-22 specifies is @hub \<noun\> \<verb\>@; the dictionary is
-- keyed by @hub:noun:verb@, so @hub inbox show X@ and @(hub:inbox:show X)@ are
-- one entry with one help text and one arity.
--
-- Decided by ASKING THE DICTIONARY, longest match first, rather than by a rule
-- about what an argument looks like. The rule was "join the first two plain
-- words", and it turned @hub inbox \<key\>@ into @inbox:\<key\>@: a base58 key is
-- a plain word too. Anything a guess like that gets wrong, it gets wrong at the
-- moment somebody is trying to use the tool.
--
-- The result is a form built from argv DIRECTLY, not a string to re-parse. Two
-- bugs came out of that round trip: joining with spaces lost the boundaries the
-- shell had already decided, and quoting everything to get them back turned every
-- integer argument into a string.
--
-- Takes the "is this a name the dictionary holds" question as a function rather
-- than the dictionary, which is what lets a test drive it with a list of names.
verbOf :: (String -> Bool) -> [String] -> Maybe (Syntax C)
verbOf bound argv = case argv of
  [] -> Nothing
  (w:rest) -> go (min 3 (length argv))
    -- No noun-verb match, so this is a name from the dictionary this tool shares
    -- with hbs2-cli, spelled out. Kept reachable rather than made script-only:
    -- the entries are there either way, and a surface that silently swallows a
    -- name it holds is worse than one that does not hold it.
    <|> (if bound w then Just (mkList (mkSym @C w : fmap argvAtom rest)) else Nothing)
  where
    go 0 = Nothing
    go n = case splitAt n argv of
      (ws, rest) | all plain ws, bound (name ws) ->
                     Just (mkList (mkSym @C (name ws) : fmap argvAtom rest))
                 | otherwise -> go (n - 1)

    name ws = "hub:" <> intercalate ":" ws
    plain (c:_) = c `notElem` ("-(\"'[" :: String)
    plain []    = False
