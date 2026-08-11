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
  , flagsOf
  , flagsAndSwitches
  , flagSwitch
  , flagOnce
  , flagEvery
  , repoFlags
  , flagRepo
  , flagRepoMaybe
  , repoAndFlags
  , flagMaybe
  , flagText
  , flagWord
  ) where

import HBS2.CLI.Prelude

import Control.Applicative ((<|>))
import Data.List (intercalate)
import Data.List qualified as List
import Data.Word (Word64)
import Text.Read (readMaybe)

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

-- | A line of @--flag value@ pairs, or nothing at all.
--
-- ONE implementation, and that is the point of it being here. Six verbs had
-- their own copy of a two-line scan that paired each word with the word after
-- it, and five of those copies were missing both of the checks below. What they
-- guard is not a formatting question: these verbs sign things, and canon is
-- append-only.
--
-- What it refuses, and why each one is a refusal rather than a default:
--
--   * A FLAG WHERE A VALUE BELONGS, which is a missing value and not a value.
--     Without this, @hub pr new ... --title --onto refs/heads/master@ parses
--     cleanly, binds the title @--onto@, and puts it in the author box and
--     therefore in the event-id, which is the hash of that box. The named form
--     of @hub issue new@ was introduced for exactly this hazard and was the only
--     reader that had the guard.
--
--   * A FLAG THIS VERB DOES NOT KNOW. Without this, @hub pr merge --repo K
--     --number 7 --commit abc --into master --dry-run@ drops the @--dry-run@ on
--     the floor and records the merge in canon. A typo does the same thing more
--     quietly. Nothing here can tell an operator's mistake from an intention, so
--     the only safe answer to a word nobody claimed is to stop.
--
--   * ANY WORD THAT IS NOT PART OF A PAIR. These verbs are all-named by design
--     (a key, a sigil and a hash are the same thirty-two bytes of base58, so
--     nothing can be told apart by position), and a stray positional is either a
--     value that lost its flag or a flag that lost its dashes.
--
-- @--flag=value@ is accepted as well, which is the spelling that lets a value
-- legitimately begin with a dash, and is what makes refusing the bare form
-- affordable. The value it yields is a STRING, so a reader that wants a number
-- has to accept one spelled as text; 'flagOnce' callers do.
flagsOf :: forall c . IsContext c => [String] -> [Syntax c] -> Maybe [(String, Syntax c)]
flagsOf known = flagsAndSwitches known []

-- | 'flagsOf', plus flags that take no value.
--
-- A switch is consumed ALONE, and the word after it stays a word, which is the
-- whole reason it cannot be a flag whose value happens to be ignored: with a
-- pair rule, @hub issue label --clear --repo K@ binds the repository key as the
-- value of @--clear@ and then refuses the line for having no repository, and
-- @--clear yes@ silently eats a word. Here the leftover word is a stray
-- positional and the line does not parse, which is the same answer every other
-- word nobody claimed gets.
--
-- @--switch=x@ is refused for the same reason: it is a value handed to
-- something that has no value to give.
--
-- The two lists are both @[String]@ and swapping them would be quiet, so there
-- is exactly one caller of this form; everything else goes through 'flagsOf'.
flagsAndSwitches :: forall c . IsContext c
                 => [String] -> [String] -> [Syntax c] -> Maybe [(String, Syntax c)]
flagsAndSwitches known switches syn = do
  kvs <- pairs syn
  guard (all ((`elem` (known <> switches)) . fst) kvs)
  pure kvs
  where
    pairs = \case
      [] -> Just []
      (StringLike k : rest)
        | k `elem` switches -> ((k, mkStr @c k) :) <$> pairs rest
      -- Split on the FIRST '=' only, so a value may contain one.
      (StringLike k : rest)
        | Just (k', v) <- splitFlag k ->
            if k' `elem` switches then Nothing
                                  else ((k', mkStr @c v) :) <$> pairs rest
      (StringLike k : v : rest)
        | "--" `List.isPrefixOf` k, not (flagLike v) -> ((k,v) :) <$> pairs rest
      _ -> Nothing

    flagLike = \case
      StringLike s -> "--" `List.isPrefixOf` s
      _            -> False

    splitFlag s = case List.break (== '=') s of
      (k, '=' : v) | "--" `List.isPrefixOf` k, length k > 2 -> Just (k, v)
      _                                                     -> Nothing

-- | The value of a flag that may appear exactly once.
--
-- Exactly one, so that @--repo a --repo b@ is a refusal rather than a silent
-- choice between them: first-wins and last-wins are both a guess about which of
-- two repositories the caller meant, and one of them gets published.
flagOnce :: [(String, Syntax c)] -> String -> Maybe (Syntax c)
flagOnce kvs k = case flagEvery kvs k of
  [v] -> Just v
  _   -> Nothing

-- | The name every verb spells the repository key with, and the one four of
-- them used to.
--
-- ONE NAME, because it was one value under two: the compose verbs took
-- @--target@ and everything else took @--repo@, one noun apart -- @hub pr new
-- --target K@ beside @hub pr merge --repo K@. Both are thirty-two bytes of
-- base58, so neither reader could tell it had been handed the other one's
-- spelling; what it produced was a usage message about a flag the caller had
-- just supplied under its sibling's name.
--
-- @--target@ still WORKS and is no longer printed. A verb's own help, its
-- synopsis and its usage line all say @--repo@; the old spelling is here for
-- the release in which somebody's script still has it, and this comment is the
-- reminder to take it out afterwards. It also collided with a second meaning:
-- @target@ is a field inside the signed author box (PEP-19), which is a
-- different thing from the flag that used to fill it.
--
-- BOTH AT ONCE IS A REFUSAL, not a preference. They are two spellings of one
-- value, so a line carrying both is a line somebody edited half way, and
-- choosing between them is the guess 'flagOnce' exists to refuse.
repoFlags :: [String]
repoFlags = ["--repo", "--target"]

-- | Read it, whichever way it was spelled.
--
-- 'Nothing' for absent, for repeated, for both spellings at once, and for a
-- value that is not a key -- which is every way the caller can be wrong, and
-- they are one answer here because the verb's usage says the same thing about
-- all of them.
flagRepo :: (Syntax c -> Maybe a) -> [(String, Syntax c)] -> Maybe a
flagRepo asKey kvs = case concatMap (flagEvery kvs) repoFlags of
  [v] -> asKey v
  _   -> Nothing

-- | The same, for a verb where the repository is optional.
--
-- 'Just Nothing' is "not given", 'Nothing' is "given wrongly", and the two are
-- not the same answer: a verb whose @--repo@ is optional still has to refuse a
-- line that names two of them.
flagRepoMaybe :: (Syntax c -> Maybe a) -> [(String, Syntax c)] -> Maybe (Maybe a)
flagRepoMaybe asKey kvs = case concatMap (flagEvery kvs) repoFlags of
  []  -> Just Nothing
  [v] -> Just <$> asKey v
  _   -> Nothing

-- | Whether a valueless flag was given.
--
-- Refuses a repeat, like 'flagOnce': a switch typed twice is a command line
-- somebody edited without reading, and these verbs sign.
flagSwitch :: [(String, Syntax c)] -> String -> Maybe Bool
flagSwitch kvs k = case flagEvery kvs k of
  []  -> Just False
  [_] -> Just True
  _   -> Nothing

-- | Every value of a flag that may repeat, in the order they were typed.
flagEvery :: [(String, Syntax c)] -> String -> [Syntax c]
flagEvery kvs k = [ v | (k',v) <- kvs, k' == k ]

-- | A flag that may be left out, and that is refused when it is there and its
-- value will not read.
--
-- The distinction the plain lookup could not make: @flagOnce ... >>= asKey@
-- answers 'Nothing' both for "not given" and for "given, and not a key", so
-- @hub inbox accept --as \<not a key\>@ fell back to the default canon key and
-- signed with a key the caller did not name. An optional flag is optional to
-- SUPPLY, not optional to mean something.
flagMaybe :: [(String, Syntax c)] -> String -> (Syntax c -> Maybe v) -> Maybe (Maybe v)
flagMaybe kvs k f = case flagEvery kvs k of
  []  -> Just Nothing
  [v] -> Just <$> f v
  _   -> Nothing

-- | A flag's value as text, including a value that spells a number.
--
-- 'argvAtom' keeps a word that spells a number AS a number, deliberately, and
-- every 'StringLike' pattern then fails to match it. So @--title 2026@ died with
-- a usage message that said nothing about why, and so did a branch called
-- @2026@ and an abbreviated sha that happens to be all digits, which is about
-- one in twenty-seven of the seven-character ones.
--
-- Rendering the atom back is lossless by construction: 'argvAtom' kept it as a
-- number only BECAUSE rendering it gives back the characters that were typed.
flagText :: Syntax c -> Maybe String
flagText = \case
  StringLike t           -> Just t
  x@(LitIntVal _)        -> Just (show (pretty x))
  x@(LitScientificVal _) -> Just (show (pretty x))
  x@(LitBoolVal _)       -> Just (show (pretty x))
  _                      -> Nothing

-- | A flag's value as a number that fits, refusing one that does not.
--
-- Through 'flagText', so that @--number=7@ reads the same as @--number 7@: the
-- @=@ spelling yields a string and the bare one yields an integer.
--
-- The bound is the whole reason this is not @fromIntegral@ at the call site. A
-- 'LitIntVal' carries an Integer, so the guard against a negative left the upper
-- end open, and @--number 18446744073709551617@ wrapped to 1: the verb then
-- looked up pull request #1, refused it against an ancestry check that was never
-- about it, and named a number nobody typed in the message it wrote.
flagWord :: Syntax c -> Maybe Word64
flagWord x = do
  s <- flagText x
  n <- readMaybe s
  guard (n >= 0 && n <= toInteger (maxBound :: Word64))
  pure (fromIntegral n)

-- | The repository a READ verb is about: positionally, or behind @--repo@.
--
-- Six verbs -- @issue list@, @pr list@, @issue show@, @pr show@, @log@ and
-- @verify@ -- took it positionally and ONLY positionally, while every verb that
-- writes takes @--repo@. So the flag a user learns from the writing half was a
-- usage error on the reading half, and the key is already in the git remote's
-- URL (@hbs23:\/\/\<key\>@) where a later version can read it from.
--
-- Both, and that is the point of doing it now: accepting the flag is additive
-- and costs nobody anything, while REMOVING the positional form after a release
-- is a break. Whichever way this eventually settles, the flag has to exist
-- first.
--
-- The verb's own flags are read either way; the repository's are accepted only
-- in the flag form, so @hub issue list K --repo K2@ is a refusal rather than a
-- guess about which key was meant.
repoAndFlags :: forall c a . IsContext c
             => (Syntax c -> Maybe a)   -- ^ how this verb reads a key
             -> [String]                -- ^ the verb's own flags
             -> [Syntax c]
             -> Maybe (a, [(String, Syntax c)])
repoAndFlags asKey own syn = case syn of
  (k : rest) | Just repo <- asKey k -> (,) repo <$> flagsOf own rest
  _ -> do
    kvs  <- flagsOf (repoFlags <> own) syn
    repo <- flagRepo asKey kvs
    pure (repo, kvs)
