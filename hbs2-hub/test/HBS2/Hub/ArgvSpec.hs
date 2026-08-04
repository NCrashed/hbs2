module HBS2.Hub.ArgvSpec (spec) where

import HBS2.Hub.CLI.Argv

import Data.Config.Suckless

import Prettyprinter (pretty)

import Data.Maybe (isJust)
import Data.Text qualified as Text
import Test.Hspec
import Test.QuickCheck

-- What a word came back as, in the only terms the verbs care about: every
-- argument pattern in this package -- StringLike, and SignPubKeyLike and
-- HashLike through it -- ends at 'stringLike'.
said :: Syntax C -> Maybe String
said = \case
  LitStrVal t      -> Just (Text.unpack t)
  SymbolVal (Id t) -> Just (Text.unpack t)
  _                -> Nothing

isSymbol :: Syntax C -> Bool
isSymbol = \case
  SymbolVal _ -> True
  _           -> False

isList :: Syntax C -> Bool
isList = \case
  ListVal _ -> True
  _         -> False

asInt :: Syntax C -> Maybe Integer
asInt = \case
  LitIntVal n -> Just n
  _           -> Nothing

-- Nothing was lost: the word came back as itself, whether it came back as text
-- or as the number it spells.
kept :: String -> Property
kept s = case argvAtom s of
  x | Just t <- said x -> t === s
    | otherwise        -> show (pretty x) === s

-- Words a person would plausibly type as a title or a key, avoiding the one
-- character that means "lex me": a quote, which is a request to be read as a
-- string literal. Everything else is fair game, and the point of the property is
-- that all of it comes back whole.
--
-- Brackets are IN the alphabet, and they were not while a bracketed word was
-- parsed as a form. That exclusion was the property agreeing not to look at the
-- case that turned out to execute commands, which is the failure mode the cover
-- monitors below exist to catch elsewhere.
titleish :: Gen String
titleish = do
  n <- choose (1, 40 :: Int)
  s <- vectorOf n (elements alphabet)
  -- A leading digit could lex as a number, and a number that round-trips is
  -- deliberately kept as a number; that case has its own examples below.
  pure (if all (`elem` ("0123456789.-" :: String)) s then 'x' : s else s)
  where
    alphabet = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']
                 <> " ;#&*+-.:/!@^_=<>?,|~$%" <> "()[]"

spec :: Spec
spec = do

  describe "PEP-22 argv: a shell word is not a script token" $ do

    it "keeps a title whole when it contains the comment character" $ do
      -- THE ONE THAT MATTERS. `;` starts a comment in the script lexer, a bare
      -- word lexes as a symbol, and StringLike matches a symbol -- so the whole
      -- of `hub issue new ... 'fix; see later'` bound the title `fix`, put it in
      -- the author box, signed it and sent it. The title is inside the signature
      -- and inside the event-id, and canon is append-only.
      said (argvAtom "fix; see later") `shouldBe` Just "fix; see later"
      said (argvAtom "a;b") `shouldBe` Just "a;b"
      said (argvAtom "trailing;") `shouldBe` Just "trailing;"

    it "keeps a title whole when it contains spaces" $ do
      -- Which is to say: when it is a title. This lexed as three symbols and the
      -- evaluator called the first of them, so `hub issue new ... 'Crash on
      -- startup'` answered NameNotBound (Id "Crash") and no issue could be filed
      -- with an ordinary title at all.
      said (argvAtom "Crash on startup") `shouldBe` Just "Crash on startup"
      isList (argvAtom "Crash on startup") `shouldBe` False

    it "never hands back a symbol, whatever the word is" $ do
      -- A symbol is a NAME, and the evaluator resolves names: a word that happens
      -- to be one of the interpreter's 215 bindings evaluated to the lambda it
      -- names, so `head` arrived at a verb as (builtin:lambda head) and matched no
      -- string pattern at all. An argument is data.
      let words' = ["head", "list", "now", "quot", "display", "true", "nil"]
      map (isSymbol . argvAtom) words' `shouldBe` map (const False) words'
      map (said . argvAtom) words' `shouldBe` map Just words'

    it "keeps a number a number, so an arity that wants one still binds" $ do
      -- The reason the lexer is consulted at all: `hub pr merge 12` and half the
      -- inherited dictionary match on an integer. Quoting everything to avoid the
      -- bugs above lost these, which is the mistake this replaced.
      asInt (argvAtom "42") `shouldBe` Just 42
      asInt (argvAtom "0")  `shouldBe` Just 0

    it "does not renumber a word that only looks like a number" $ do
      -- 007 and 7 are different words. The lexer's opinion that they are the same
      -- number is about a number, and this is about what somebody typed -- an
      -- argument that silently changes value is the same class of defect as the
      -- title above, just quieter.
      said (argvAtom "007") `shouldBe` Just "007"
      said (argvAtom "+5")  `shouldBe` Just "+5"

    it "does not unquote, because unquoting was lossy" $ do
      -- This USED to hand a quoted word to the lexer, on the argument that the
      -- quotes were the request. The lexer runs readLitChar over the inside, so
      -- '"C:\temp"' arrived as C:<TAB>emp and would have been signed that way --
      -- the semicolon bug one branch over. The workaround existed only because a
      -- multi-word title could not be passed at all, and it can now.
      said (argvAtom "\"quoted text\"") `shouldBe` Just "\"quoted text\""
      said (argvAtom "\"C:\\temp\"") `shouldBe` Just "\"C:\\temp\""

    it "never hands a bracketed word to the evaluator" $ do
      -- THE OTHER ONE THAT MATTERS, and the reason the form branch is gone. A
      -- word starting with `(` or `[` used to be parsed and evaluated, and this
      -- applied to the VALUE OF A FLAG, not only to the verb position -- the
      -- evaluator evaluates a verb's arguments before the verb's pattern match
      -- runs. On the build before this test:
      --
      --   hbs2-hub inbox '(run:proc:quiet "sh" "-c" "touch /tmp/proof")'
      --
      -- created the file and then printed the inbox usage. The dictionary this
      -- tool shares with hbs2-cli also holds rm, mv, cp, setenv and cd, and
      -- PEP-22 has a renderer that shells out to this CLI with text a stranger
      -- wrote on the web.
      let bomb = "(run:proc:quiet \"sh\" \"-c\" \"touch /tmp/proof\")"
      isList (argvAtom bomb) `shouldBe` False
      said (argvAtom bomb) `shouldBe` Just bomb
      isList (argvAtom "(list 1 2)") `shouldBe` False
      said (argvAtom "(list 1 2)") `shouldBe` Just "(list 1 2)"
      said (argvAtom "[1 2]") `shouldBe` Just "[1 2]"

    it "keeps a bracketed tag in a title, which is how issues get titled" $ do
      -- The daily-use half of the same defect: `--title '[bug] segfault'` exited
      -- with `NameNotBound (Id "bug")`, an internal constructor naming a word
      -- nobody typed. `[bug]` became a call and the evaluator resolved `bug`
      -- before the hand-written usage fallback could run at all.
      said (argvAtom "[bug] segfault") `shouldBe` Just "[bug] segfault"
      -- And the signing half: this would have signed what the form evaluated to,
      -- in a tool whose stated claim is that it signs what was typed.
      said (argvAtom "(pwd)") `shouldBe` Just "(pwd)"

    it "loses nothing from any word it is given" $
      -- The property the examples above are cases of, and the one a future
      -- convenience has to keep: an argument is bytes a person typed, and this
      -- function's whole job is to hand them over unchanged.
      --
      -- Stated as "renders back to itself" rather than "is a string", because
      -- keeping a number a number is deliberate and a generated word can be one:
      -- `1.5e3` and `#t` lex as a scientific and a boolean, round-trip exactly,
      -- and are correctly kept as such. Asserting the STRING case would have made
      -- this test fail on a correct implementation, roughly one run in a
      -- thousand, which is how a flaky test teaches people to re-run the suite.
      --
      -- The cover monitors are here for the reason PropertySpec's are: a property
      -- over a generator that never produces the interesting character is a
      -- property that passes by not looking. `;` and a space are the two that
      -- caused the bug, and each is one character in an alphabet of 85.
      property $ forAll titleish $ \s ->
        checkCoverage $
        cover 5 (';' `elem` s) "holds the comment character" $
        cover 5 (' ' `elem` s) "holds a space" $
        kept s

  argvFlags

  describe "PEP-22 argv: which words are the verb" $ do

    -- "hub:issue" is in the list on purpose, and it is the only reason the test
    -- below tests what its name says: with only "hub:issue:new" present, a
    -- shortest-first search and a longest-first search agree on every input, so
    -- the claim was unfalsifiable. A dictionary really can hold both -- PEP-22
    -- specifies `hub issue list` beside `hub issue new` -- and then the order
    -- decides whether `new` is a verb or an argument.
    let bound = (`elem` ["hub:inbox", "hub:issue", "hub:issue:new", "hub:verify", "display"])

    it "takes the longest noun-verb match the dictionary holds" $ do
      -- `hub issue new X` is one entry with one help text and one arity, and the
      -- match is decided by asking the dictionary rather than by a rule about
      -- what an argument looks like: the rule was "join the first two plain
      -- words", which turned `hub inbox <key>` into `inbox:<key>` because a
      -- base58 key is a plain word too.
      case verbOf bound ["issue","new","K","S","R","A","a title"] of
        Just (ListVal (SymbolVal k : as)) -> do
          k `shouldBe` Id "hub:issue:new"
          -- and the arguments arrive as the words that were typed
          map said as `shouldBe` map Just ["K","S","R","A","a title"]
        other -> expectationFailure ("expected hub:issue:new, got " <> show (fmap (const ()) other))

    it "does not swallow an argument into the verb name" $ do
      case verbOf bound ["inbox","SomeBase58Key"] of
        Just (ListVal (SymbolVal k : as)) -> do
          k `shouldBe` Id "hub:inbox"
          map said as `shouldBe` [Just "SomeBase58Key"]
        other -> expectationFailure ("expected hub:inbox, got " <> show (fmap (const ()) other))

    it "says nothing when nothing is named" $ do
      isJust (verbOf bound ["nosuchverb"]) `shouldBe` False
      isJust (verbOf bound []) `shouldBe` False

    it "still reaches a shared dictionary name spelled out" $ do
      -- The entries hbs2-cli contributes are there either way, and a surface that
      -- silently swallows a name it holds is worse than one that does not hold it.
      case verbOf bound ["display","x"] of
        Just (ListVal (SymbolVal k : _)) -> k `shouldBe` Id "display"
        other -> expectationFailure ("expected display, got " <> show (fmap (const ()) other))

-- The flag reader every writing verb shares. It was six copies of a two-line
-- scan, five of which had neither guard below, in the verbs that publish.
argvFlags :: Spec
argvFlags =
  describe "PEP-22 argv: a line of flags and values" $ do

    let flags = flagsOf ["--a","--b"] . fmap argvAtom
        -- The whole line refused. Spelled out rather than compared, because
        -- Syntax has no Show, which is also why every assertion below asks what
        -- a value READS as rather than what it is.
        gone = not . isJust

    it "reads a pair per flag, in any order" $ do
      fmap (fmap fst) (flags ["--a","1","--b","two"]) `shouldBe` Just ["--a","--b"]
      fmap (fmap fst) (flags ["--b","two","--a","1"]) `shouldBe` Just ["--b","--a"]

    -- The one that signs a title nobody typed. `hub pr new ... --title --onto
    -- refs/heads/master` bound the title `--onto`, and a title is inside the
    -- author box and therefore inside the event-id, which canon cannot correct.
    it "refuses a flag standing where a value belongs" $
      gone (flags ["--a","--b","two"]) `shouldBe` True

    it "refuses a value with no flag, at either end" $ do
      gone (flags ["stray","--a","1"]) `shouldBe` True
      gone (flags ["--a","1","stray"]) `shouldBe` True

    -- `pr merge --repo K --number 7 --commit abc --into master --dry-run`
    -- recorded the merge and dropped the --dry-run on the floor.
    it "refuses a flag the verb does not know" $ do
      gone (flags ["--a","1","--dry-run"]) `shouldBe` True
      gone (flags ["--a","1","--c","3"]) `shouldBe` True

    -- The spelling that lets a value begin with a dash, which is what makes the
    -- refusal above affordable. It reads as text, so a reader that wants a
    -- number has to take one spelled as text, and 'flagWord' does.
    it "takes --flag=value, and keeps a value that holds an =" $ do
      fmap (\kvs -> flagOnce kvs "--a" >>= flagText) (flags ["--a=--draft"])
        `shouldBe` Just (Just "--draft")
      fmap (\kvs -> flagOnce kvs "--a" >>= flagText) (flags ["--a=x=y"])
        `shouldBe` Just (Just "x=y")

    it "refuses a repeated flag rather than choosing one of the two" $
      fmap (\kvs -> flagOnce kvs "--a" >>= flagText) (flags ["--a","1","--a","2"])
        `shouldBe` Just Nothing

    it "tells a flag left out from one whose value will not read" $ do
      let asInt' = \case { LitIntVal n -> Just n ; _ -> Nothing }
      fmap (\kvs -> flagMaybe kvs "--b" asInt') (flags ["--a","1"])
        `shouldBe` Just (Just Nothing)
      fmap (\kvs -> flagMaybe kvs "--b" asInt') (flags ["--a","1","--b","nope"])
        `shouldBe` Just Nothing

    -- A number is what argvAtom keeps a numeric word as, and every StringLike
    -- pattern misses it: a title, a branch or an all-digit abbreviated sha died
    -- as a usage error.
    it "reads a value that spells a number as the word that was typed" $ do
      fmap (\kvs -> flagOnce kvs "--a" >>= flagText) (flags ["--a","2026"])
        `shouldBe` Just (Just "2026")
      fmap (\kvs -> flagOnce kvs "--a" >>= flagText) (flags ["--a","007"])
        `shouldBe` Just (Just "007")

    -- The guard against a negative was there and the one against wrapping was
    -- not: this used to come back as 1.
    it "refuses a number too big to be the one that was typed" $ do
      fmap (\kvs -> flagOnce kvs "--a" >>= flagWord) (flags ["--a","7"])
        `shouldBe` Just (Just 7)
      fmap (\kvs -> flagOnce kvs "--a" >>= flagWord)
           (flags ["--a","18446744073709551617"])
        `shouldBe` Just Nothing
      fmap (\kvs -> flagOnce kvs "--a" >>= flagWord) (flags ["--a","-5"])
        `shouldBe` Just Nothing
