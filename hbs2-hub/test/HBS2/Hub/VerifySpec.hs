-- | What @hub verify@ prints and what it exits with (PEP-22).
--
-- The report is the whole product of the verb, and until this file existed
-- nothing exercised a line of it: the rendering lived inside an IO action that
-- ends in @exitWith@, which is not a thing a test can assert on. It is now four
-- pure functions, and this is why.
--
-- Two of the tests below are about a stranger's bytes reaching a terminal. That
-- is not a decorative concern here: a tree path may hold any byte but NUL, and
-- anybody who can get a letter into canon can choose one.
module HBS2.Hub.VerifySpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Canon
import HBS2.Hub.Fold
import HBS2.Hub.Repo
import HBS2.Hub.CLI.Verify

import HBS2.Net.Auth.Credentials

import Data.ByteString (ByteString)
import Data.List (nub,sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextE
import Data.Foldable (for_)
import Data.ByteString qualified as BS
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Test.Hspec

type KP = (HubKey, PrivKey 'Sign HubScheme)

kp :: IO KP
kp = do
  c <- newCredentials @'HBS2Basic
  pure (_peerSignPk c, _peerSignSk c)

-- A literal as the bytes it reads as, not as Char8 truncation.
utf8 :: String -> ByteString
utf8 = TextE.encodeUtf8 . Text.pack

-- A Doc as the one line it is meant to be.
--
-- Unbounded on purpose: the default layout wraps at 80 columns, which would make
-- an assertion about "one line" a statement about the width of the terminal.
render :: Doc ann -> Text
render = renderStrict . layoutPretty (LayoutOptions Unbounded)

-- A canon tree in memory whose paths are BYTES, which is what the report handles
-- and what 'HBS2.Hub.RepoSpec' cannot express: its fixture takes a FilePath.
byPath :: [(ByteString, Text)] -> CanonSource IO
byPath files = CanonSource
  { csCommit  = pure (Right "deadbeef")
  , csEntries = const (pure (Right
      [ TreeEntry p (Blob (oidOf i) (BS.length (TextE.encodeUtf8 t)))
      | (i,(p,t)) <- zip [0 :: Int ..] files ]))
  , csBlob    = \oid -> pure (maybe (BlobRefused "no such object") BlobText (lookup oid byOid))
  }
  where
    oidOf i = Text.pack (show i)
    byOid = [ (oidOf i, t) | (i,(_,t)) <- zip [0 :: Int ..] files ]

-- Every constructor of CanonUnreadable with the code it must exit with. The
-- codes are PINNED, not merely required to differ: a script in somebody's hook
-- branches on the number, so swapping two of them is a breaking change that a
-- distinctness check waves through. This list is the contract, and PEP-22 prints
-- the same table.
--
-- Handwritten, since the type has payloads and cannot be enumerated. The
-- -Werror=incomplete-patterns in the module under test is what makes a new
-- constructor a build failure there; what it cannot catch is a new constructor
-- given a code that is already taken, which is what the distinctness check below
-- is for.
everyRefusal :: [(CanonUnreadable, Int)]
everyRefusal =
  [ (NoCanonRef,                          3)
  , (NoRepository "not a git repository", 4)
  , (RefUnresolved "bad object",          5)
  , (CanonTooNewHere 99,                  6)
  , (VersionUnreadable (FileUnreadable "x"),    7)
  , (CanonTooBig (maxCanonBytes + 1),     8)
  , (TreeUnreadable "missing blob",       9)
  , (CanonTooMany (maxCanonFiles + 1),   10)
  , (CanonListingTooBig maxListingBytes, 11)
  , (ReaderFailed "fork: Resource exhausted", 12)
  ]

spec :: Spec
spec = do

  describe "PEP-22 hub verify" $ do

    it "exits with the exact code each refusal is documented to exit with" $ do
      -- One at a time and by value, so a permutation fails. It used to check only
      -- that the codes differ and are above 2, which swapping 3 and 4 passes: a
      -- hook that treats 3 as "canon has not been fetched here" would then be
      -- told that by a directory that is not a repository at all.
      for_ everyRefusal $ \(u, code) -> codeOf u `shouldBe` code

      let codes = fmap snd everyRefusal
      -- Still distinct, which is what catches a constructor added with a code
      -- that is already taken: the Werror in the module under test forces a case
      -- for it, and nothing forces the case to be a new number.
      sort codes `shouldBe` sort (nub codes)
      -- And all above 2, which is a completed audit that found something, and
      -- above 1, a usage error. A hook tells "could not run" from "ran and found
      -- things" by the number alone.
      filter (< 3) codes `shouldBe` []

    it "tells every refusal what to do about it" $
      -- Two of these used to print a bare complaint through a wildcard: a
      -- directory that is not a repository, and a version file that will not
      -- read. They are the two a reader is least likely to work out unaided.
      for_ everyRefusal $ \(u, _) ->
        length (Text.lines (render (refusalDoc u))) `shouldSatisfy` (> 1)

    it "escapes so that nothing can spell an escape" $ do
      -- The escaping is only worth having if it is injective, and it took three
      -- goes. Each line below is a pair that printed identically at some point.
      --
      -- The field spelling out the notation: the four characters \u{0a} against a
      -- newline. Fixed by escaping the backslash.
      safeText "a\\u{0a}b" `shouldNotBe` safeText "a\nb"
      safeText "a\nb" `shouldBe` "a\\u{0a}b"
      -- The digits running into the text after them: U+0006 then the literal "1c"
      -- against U+061C, when the width was two digits under 256 and four above.
      -- Fixed by delimiting instead of padding.
      safeText "\x0006" <> "1c" `shouldNotBe` safeText "\x061C"
      safeText "\x061C" `shouldBe` "\\u{61c}"
      -- A right-to-left override reverses the tail of a line on a terminal that
      -- honours it, and is not a control character, so isControl does not cover it.
      safeText "ok\x202Elater" `shouldBe` "ok\\u{202e}later"
      -- Ordinary text, including non-Latin text, is left alone: this is an escape
      -- for what breaks a line, not a transliteration.
      safeText "\1090\1077\1084\1072" `shouldBe` "\1090\1077\1084\1072"

    it "keeps a character apart from a raw byte that hexes the same" $ do
      -- The third pair, and the one an escape scheme with a single sigil cannot
      -- separate at all: U+0085 is a control character, so the character branch
      -- escapes it, and the single byte 0x85 is invalid UTF-8, so the byte branch
      -- escapes that. Both are 85 in hex. Two entries in a tree, one line in the
      -- report, and the report is how somebody decides which file to open.
      pathText (utf8 "0001-\x0085-z") `shouldNotBe` pathText "0001-\x85-z"
      pathText (utf8 "0001-\x0085-z") `shouldBe` "0001-\\u{85}-z"
      pathText "0001-\x85-z" `shouldBe` "0001-\\x{85}-z"
      -- Which branch wrote a line is readable off the line, because neither can
      -- write the other's sigil: a backslash is escaped in both.
      pathText "\\" `shouldBe` "\\u{5c}"
      pathText "\\\xff" `shouldBe` "\\x{5c}\\x{ff}"

    it "prints a tool's own complaint as a block, not as a field value" $ do
      -- git's complaints are multi-line on purpose: the remedy for a
      -- dubious-ownership refusal is the safe.directory command on the SECOND
      -- line. Rendered as a value in a one-line field, those newlines went
      -- through safeText and came out as \u{0a}, so the fix arrived spelled as an
      -- escape in the middle of a sentence. The escaping is right; a report line
      -- must stay one line. What was wrong is putting a paragraph in a field.
      let said = "fatal: detected dubious ownership\nrun: git config --global ..."
          ls = Text.lines (render (refusalDoc (NoRepository said)))
      any ("\\u{0a}" `Text.isInfixOf`) ls `shouldBe` False
      any ("run: git config" `Text.isInfixOf`) ls `shouldBe` True
      -- Each line of it on its own line, indented under the complaint.
      length (filter ("  " `Text.isPrefixOf`) ls) `shouldSatisfy` (>= 2)

    it "counts a missing version file as a finding, and prints it" $ do
      owner <- kp
      -- PEP-19 requires the file. It was a parenthesis on the header line and a
      -- zero exit, which is the one thing in the report a reader had to notice
      -- unprompted. Deleting both the line and the exit-code clause used to leave
      -- the whole suite green, which is why this asserts both.
      st <- readCanon (byPath [("threads/t/0001-x", "not an event")]) (fst owner)
              >>= either (fail . show) pure
      stVersion st `shouldBe` Nothing
      reportCode st `shouldBe` 2
      any ("no version file" `Text.isPrefixOf`) (fmap render (reportDoc st))
        `shouldBe` True

    it "counts a file with no version clause of its own as a finding" $ do
      owner <- kp
      alice <- kp
      -- Reported since PEP-19 says the clause is never obeyed but is worth
      -- knowing about, and now counted: stdout listed one line per file and $?
      -- said 0, which PEP-22 defines as an audit that found nothing.
      let repo = fst owner
          ev = mkEvent alice owner
                 (AOpen repo HubIssue "an issue" [] Nothing Nothing Nothing 1000)
                 (\eid -> CanonContent repo eid 1 (Just 1) Nothing Nothing 1 Nothing)
          thr = eventId ev
          -- renderEvent writes the clause; stripping it is what a writer that
          -- does not know about it would produce.
          without = Text.unlines
                      [ l | l <- Text.lines (renderEvent ev)
                          , not ("(hub-event" `Text.isPrefixOf` l) ]
      st <- readCanon (byPath
              [ ("version", renderMeta)
              , (encodePath (threadDir thr <> "/" <> eventFileName 1 thr), without) ]) repo
              >>= either (fail . show) pure
      [ p | (p, Nothing) <- stFileVersions st ] `shouldSatisfy` (not . null)
      reportCode st `shouldBe` 2

    it "keeps two paths that differ in one invalid byte on two distinct lines" $ do
      owner <- kp
      let repo = fst owner
          -- Not UTF-8, and differing in one byte. A lenient decode maps both to
          -- the same replacement character, so the report named one file twice
          -- and the other not at all: whoever read it went looking for a file
          -- that is not there.
          p1 = "threads/t/\xfe-x"
          p2 = "threads/t/\xff-x"
      st <- readCanon (byPath [ ("version", renderMeta)
                              , (p1, "not an event")
                              , (p2, "not an event") ]) repo
              >>= either (fail . show) pure

      sort (fmap fst (stBad st)) `shouldBe` sort [p1, p2]
      let ls = [ l | l <- fmap render (reportDoc st), "unreadable" `Text.isPrefixOf` l ]
      length ls `shouldBe` 2
      nub ls `shouldBe` ls

    it "keeps a path that forges a report line on one line" $ do
      owner <- kp
      let repo = fst owner
          -- A path git will happily carry, chosen to end the report with a lie:
          -- a clean summary line under a real one.
          evil = "threads/t/0001-x\nadmitted 0 dropped 0 anomalies 0 unreadable 0"
      st <- readCanon (byPath [("version", renderMeta), (evil, "not an event")]) repo
              >>= either (fail . show) pure

      let ls = fmap render (reportDoc st)
      -- One Doc per line, and every one of them really one line.
      for_ ls $ \l -> length (Text.lines l) `shouldBe` 1
      -- The newline is spelled out rather than obeyed, and the last line of the
      -- report is still the report's own summary.
      any ("\\u{0a}" `Text.isInfixOf`) ls `shouldBe` True
      fmap (Text.isPrefixOf "admitted") (lastOf ls) `shouldBe` Just True

    it "exits 2 for an audit that found something and 0 for a clean one" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner
                    (AOpen repo HubIssue "an issue" [] Nothing Nothing Nothing 1000)
                    (\eid -> CanonContent repo eid 1 (Just 1) Nothing Nothing 1 Nothing)
          thr = eventId eOpen
          good = [ ("version", renderMeta)
                 , (encodePath (threadDir thr <> "/" <> eventFileName 1 thr), renderEvent eOpen) ]

      clean <- readCanon (byPath good) repo >>= either (fail . show) pure
      reportCode clean `shouldBe` 0

      -- One unreadable file and nothing else wrong. Still 2: a file somebody has
      -- to look at is a finding, and the drops are not the only kind.
      dirty <- readCanon (byPath (good <> [("threads/x/junk", "not an event")])) repo
                 >>= either (fail . show) pure
      frDropped (stFold dirty) `shouldBe` []
      reportCode dirty `shouldBe` 2

  where
    encodePath = TextE.encodeUtf8 . Text.pack
    lastOf xs = if null xs then Nothing else Just (last xs)
