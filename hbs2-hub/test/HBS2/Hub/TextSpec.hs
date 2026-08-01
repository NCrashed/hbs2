module HBS2.Hub.TextSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Letter (sexpStr,letterSyntax)

import HBS2.Net.Auth.Credentials
import HBS2.Prelude.Plated (pretty)

import Data.Config.Suckless

import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec

-- Characters that print as nothing, or that a terminal OBEYS, spread across
-- every category they fall into. Not a wish-list: this is the set the two
-- escapers are supposed to agree about, and the block in the middle of it is the
-- one that was missing from the hand-written half.
ignorables :: [Char]
ignorables =
  [ '\x00AD'                                   -- SOFT HYPHEN                Cf
  , '\x034F'                                   -- COMBINING GRAPHEME JOINER  Mn
  , '\x061C'                                   -- ARABIC LETTER MARK         Cf
  , '\x115F', '\x1160'                         -- HANGUL FILLERS             Lo
  , '\x17B4', '\x17B5'                         -- KHMER INHERENT VOWELS      Mn
  , '\x180B', '\x180C', '\x180D'               -- MONGOLIAN FVS ONE..THREE   Mn
  , '\x180E'                                   -- MONGOLIAN VOWEL SEPARATOR  Cf
  , '\x180F'                                   -- MONGOLIAN FVS FOUR         Mn
  , '\x200B', '\x200E'                         -- ZWSP, LRM                  Cf
  , '\x202E'                                   -- RIGHT-TO-LEFT OVERRIDE     Cf
  , '\x2060'                                   -- WORD JOINER                Cf
  , '\x2066', '\x2069'                         -- ISOLATES                   Cf
  , '\x3164'                                   -- HANGUL FILLER              Lo
  , '\xFE00', '\xFE0F'                         -- VARIATION SELECTORS        Mn
  , '\xFEFF'                                   -- ZERO WIDTH NO-BREAK SPACE  Cf
  , '\xFFA0'                                   -- HALFWIDTH HANGUL FILLER    Lo
  , '\xE0100'                                  -- VARIATION SELECTOR-17      Mn
  , '\x001B'                                   -- ESC                        Cc
  , '\x0085'                                   -- NEXT LINE                  Cc
  , '\x00A0'                                   -- NO-BREAK SPACE             Zs
  , '\x2028'                                   -- LINE SEPARATOR             Zl
  , '\x2029'                                   -- PARAGRAPH SEPARATOR        Zp
  ]

-- What 'sexpStr' did to one character: the escaped text it produced.
sexpOf :: Text -> Text
sexpOf t = case sexpStr t of
  LitStrVal e -> e
  other       -> Text.pack (show (pretty other))

-- The characters sexpStr spells specially for its own sake rather than because
-- they are invisible: its escape is a backslash and its delimiter is a quote,
-- and the three whitespace characters have short mnemonic spellings.
sexpSpecials :: [Char]
sexpSpecials = "\\\"\n\r\t"

spec :: Spec
spec = do

  describe "PEP-19 rendering: what is invisible is one question" $ do

    it "names every default-ignorable, including the block a list would miss" $ do
      -- The rule is BY CATEGORY, and the categories do not cover everything:
      -- Unicode assigns Default_Ignorable by property, so the fillers are Lo and
      -- the variation selectors are Mn, and those need naming by hand. The hand
      -- list is where a rule decays back into a list -- U+180B..U+180D are Mn and
      -- were never in it, and U+180F was assigned to the same block in Unicode 14
      -- after it was written.
      [ c | c <- ignorables, not (invisible c) ] `shouldBe` []

    it "leaves none of them in the output as itself" $ do
      -- What "invisible" is FOR. An attribute name, a title or a tree path that
      -- differs from another only by something that renders as nothing is a line
      -- a maintainer cannot tell from another line -- and the collision is on a
      -- TERMINAL, not in the Text: the two strings are perfectly distinct data,
      -- which is why "safeText x /= safeText y" is the wrong question and passes
      -- against every one of these bugs. The right question is whether the
      -- character survives into the output at all.
      let raw c = safeText (Text.singleton c) == Text.singleton c
      filter raw ignorables `shouldBe` []

    it "leaves an ordinary character alone, including a space" $ do
      -- The other half, and the reason this is not "escape everything above
      -- ASCII": a space is how words are separated, and Cyrillic is text.
      safeText "a b" `shouldBe` "a b"
      safeText "\1087\1088\1080\1074\1077\1090" `shouldBe` "\1087\1088\1080\1074\1077\1090"
      [ c | c <- ['a'..'z'] <> ['0'..'9'] <> " .,-", invisible c ] `shouldBe` []

    it "asks the same question in the canon projection as in a report" $ do
      -- THE INVARIANT THAT WAS BROKEN. 'sexpStr' tested Char.isControl, which is
      -- category Cc and nothing else, while 'safeText' two hundred lines away
      -- tested four more categories -- so a RIGHT-TO-LEFT OVERRIDE went into the
      -- canon event file raw, and that file is what a maintainer reads with
      -- `git show` while deciding whether to sign. Two escapers, one question:
      -- they may differ in how they SPELL an escape and not in what they escape.
      let sample = ['\0'..'\255'] <> ignorables <> "\1088\1091\1089\1089"
          disagrees c = (sexpOf (Text.singleton c) /= Text.singleton c)
                          /= (invisible c || c `elem` sexpSpecials)
      filter disagrees sample `shouldBe` []

    it "escapes the four a terminal obeys, in the projection too" $ do
      -- Named individually because each was demonstrated to pass through: a
      -- title of "Fix crash<U+202E>rojam a ton" reverses its own tail for
      -- whoever is reading the file.
      let raw c = sexpOf (Text.singleton c) == Text.singleton c
      filter raw ['\x202E', '\x2028', '\x200B', '\x00A0'] `shouldBe` []

  describe "PEP-19 rendering: escaping does not cost the round trip" $ do

    it "re-parses a title holding what the escape was widened for" $ do
      -- Widening the predicate is only free if the file still reads back byte for
      -- byte: \xNNNN\& is a Haskell escape for any code point, so it should be.
      -- Proved through the real parser rather than argued, and on the same
      -- projection PEP-19 writes into the event file.
      owner <- _peerSignPk <$> newCredentials @'HBS2Basic
      let nasty = Text.pack ("Fix crash" <> ['\x202E'] <> "rojam a ton"
                              <> ['\x200B','\x00A0','\x2028','\x180B','\ESC'])
          ac = AOpen owner HubIssue nasty [] (Just nasty) Nothing Nothing 5
          rendered = unwords (fmap (show . pretty) (letterSyntax ac))
      case parseTop rendered of
        Left e -> expectationFailure ("projection does not re-parse: " <> show e)
        Right forms -> do
          let clauses = concat [ xs | List _ xs <- forms ]
          [ t | List _ [SymbolVal "title", LitStrVal t] <- clauses ]
            `shouldBe` [nasty]
          -- ...and none of it is in the file as itself
          [ c | c <- Text.unpack (Text.pack rendered), invisible c, c /= '\n' ]
            `shouldBe` []

    it "keeps a code point above 00FF whole in the escape it writes" $ do
      -- justifyRight pads and does not truncate. Worth pinning: the escape was
      -- written when the only thing it could see was a C0 control, so its width
      -- was two, and a two-digit escape for U+202E would name a different
      -- character.
      sexpOf (Text.singleton '\x202E') `shouldBe` "\\x202e\\&"
      -- and the trailing empty escape is still load-bearing: a hex digit after a
      -- numeric escape would otherwise be swallowed into it
      sexpOf "\ESC5" `shouldBe` "\\x1b\\&5"

    it "spells a control the short way where a short way exists" $ do
      -- Unchanged by the widening, and most of what a body contains.
      sexpOf "a\nb\tc" `shouldBe` "a\\nb\\tc"

    it "still escapes its own delimiter and its own backslash" $ do
      -- Neither is invisible, and both would break the file. They are the two
      -- cases that belong to the SPELLING rather than to the question, which is
      -- what the split between 'invisible' and each escaper's own 'esc' is for:
      -- 'invisible' must NOT claim them, and each escaper must handle them
      -- anyway. A backslash that 'invisible' claimed would be escaped twice by
      -- one caller and once by the other.
      sexpOf "a\"b\\c" `shouldBe` "a\\\"b\\\\c"
      map invisible "\"\\" `shouldBe` [False, False]
      safeText "\\" `shouldSatisfy` (/= "\\")
