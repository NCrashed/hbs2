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
import HBS2.Hub.Canon (CanonError(..))
import HBS2.Hub.Fold
import HBS2.Hub.Repo
import HBS2.Hub.CLI.Verify

import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))
import HBS2.Prelude.Plated (pretty)

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
  [ (NoCanonRef "/somewhere/.git",        3)
  , (NoRepository "not a git repository", 4)
  , (RefUnresolved "bad object",          5)
  , (CanonTooNewHere 99,                  6)
  , (VersionUnreadable (FileMalformed (BadClause "hub-meta")), 7)
  -- Not the file being wrong but this clone not having it: the ordinary state
  -- after clone --filter=blob:none, and 7 says canon is broken.
  , (VersionUnreadable FileObjectMissing, 13)
  , (VersionUnreadable (FileUnreadable "cannot read"), 13)
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
      -- Still distinct PER CONSTRUCTOR, which is what catches a new one given a
      -- code that is already taken: the Werror in the module under test forces a
      -- case for it and nothing forces the case to be a new number. Two entries
      -- here share 13 on purpose, being two spellings of one constructor.
      sort (nub codes) `shouldBe` nub (sort codes)
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

    it "escapes what is invisible, by category and not by a list" $ do
      -- A list of characters somebody thought of is a list somebody will get
      -- wrong twice: it held twelve, and it was Cf (over 160 members) and then Zs
      -- that got through. Every character below is one a reader cannot see, in a
      -- DIFFERENT category, so a return to enumerating them fails here.
      for_ [ ('\x200B', "Cf zero width space")
           , ('\x00A0', "Zs no-break space")
           , ('\x2028', "Zl line separator")
           , ('\x3164', "Lo hangul filler, default-ignorable")
           , ('\xFE00', "Mn variation selector, default-ignorable")
           , ('\x034F', "Mn combining grapheme joiner")
           , ('\xE0041', "Cf tag latin A, invisible ASCII in a path")
           ] $ \(c, what) ->
        (what, safeText (Text.pack ['a', c, 'b'])) `shouldSatisfy`
          \(_, out) -> out /= Text.pack ['a', c, 'b']

      -- And the ordinary space is NOT escaped: it is the one character in Zs a
      -- reader recognises, and escaping it would wreck every title in the report.
      safeText "a b" `shouldBe` "a b"

      -- Nor is anything ordinary, including scripts nobody on this project reads.
      safeText "\1090\1077\1084\1072 \20320\22909" `shouldBe` "\1090\1077\1084\1072 \20320\22909"

    it "keeps a tab in a quoted tool block and escapes it in a field" $ do
      -- git indents the safe.directory command it offers with a tab. Escaped, the
      -- command arrives as \u{09}git config --global ... and does not paste. In a
      -- block the line structure is already decided, so a tab cannot forge
      -- anything; in a one-line field it can, and there it is still escaped.
      safeWith (== '\t') "a\tb" `shouldBe` "a\tb"
      safeText "a\tb" `shouldBe` "a\\u{09}b"
      -- The exemption is that character and no other.
      safeWith (== '\t') "a\nb" `shouldBe` "a\\u{0a}b"

    it "keeps a character apart from a raw byte that hexes the same" $ do
      -- The third pair, and the one an escape scheme with a single sigil cannot
      -- separate at all: U+0085 is a control character, so the character branch
      -- escapes it, and the single byte 0x85 is invalid UTF-8, so the byte branch
      -- escapes that. Both are 85 in hex. Two entries in a tree, one line in the
      -- report, and the report is how somebody decides which file to open.
      pathText (utf8 "0001-\x0085-z") `shouldNotBe` pathText "0001-\x85-z"
      -- Quoted, so the path is one field: the report prints it before a colon and
      -- a reason, and a path containing ": " supplied its own reason.
      pathText (utf8 "0001-\x0085-z") `shouldBe` "\"0001-\\u{85}-z\""
      pathText "0001-\x85-z" `shouldBe` "\"0001-\\x{85}-z\""
      -- Which branch wrote a line is readable off the line, because neither can
      -- write the other's sigil: a backslash is escaped in both.
      pathText "\\" `shouldBe` "\"\\u{5c}\""
      pathText "\\\xff" `shouldBe` "\"\\x{5c}\\x{ff}\""
      -- And a quote inside is escaped, so the field cannot be closed early.
      pathText "a\"b" `shouldBe` "\"a\\u{22}b\""

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
      -- Both lines of it, each on its own line and each MARKED as quoted. The
      -- marker is what tells a stranger's text from this program's advice, which
      -- is printed at the same indent immediately below it: unmarked, git's "run:
      -- git config --global ..." sat exactly where hbs2-hub's own "run this"
      -- lines sit. An assertion about indentation alone passed without it,
      -- because the advice is indented too.
      filter ("  | " `Text.isPrefixOf`) ls `shouldBe`
        [ "  | fatal: detected dubious ownership"
        , "  | run: git config --global ..." ]
      -- And this program's own advice is still there, unmarked, under it.
      any (\l -> "  " `Text.isPrefixOf` l && not ("  | " `Text.isPrefixOf` l)) ls
        `shouldBe` True

    it "counts a missing version file as a finding, and prints it" $ do
      owner <- kp
      -- PEP-19 requires the file. It was a parenthesis on the header line and a
      -- zero exit, which is the one thing in the report a reader had to notice
      -- unprompted. Deleting both the line and the exit-code clause used to leave
      -- the whole suite green, which is why this asserts both.
      -- An EMPTY tree, so the missing version file is the only finding there is:
      -- with an unreadable file in it as well, the exit code was 2 either way and
      -- deleting the version clause from reportCode left the suite green.
      st <- readCanon (byPath []) (fst owner) >>= either (fail . show) pure
      stVersion st `shouldBe` Nothing
      stBad st `shouldBe` []
      frDropped (stFold st) `shouldBe` []
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

    it "gives every refusal its own words, not just some words" $ do
      -- Pairwise distinct, because "more than one line" is satisfied by any two
      -- texts, including two identical ones: swapping the advice of any two
      -- constructors passed. What a reader gets told is the whole product of a
      -- refusal, and there are twelve of them.
      -- The ADVICE, not the whole doc: the first line is `hub: <pretty u>`, which
      -- differs per constructor for free, so comparing whole docs passed while
      -- three refusals gave word-for-word identical advice.
      let advice u = Text.unlines (drop 1 (Text.lines (render (refusalDoc u))))
          said = [ (codeOf u, advice u) | (u, _) <- everyRefusal ]
          -- The three bounds are allowed to share: compaction really is the one
          -- answer to all three, and pretending otherwise would be padding.
          bounds' = [8, 10, 11]
      for_ [ (x, y) | x <- said, y <- said, fst x < fst y
                    , not (fst x `elem` bounds' && fst y `elem` bounds') ] $
        \((ca, a), (cb, b)) ->
          (ca, cb, a) `shouldSatisfy` \_ -> a /= b

    it "exits 2 for a fold that dropped something, and prints the drop" $ do
      owner <- kp
      mallory <- kp
      -- The verb's whole product, and nothing reached it: every reportCode test
      -- got its 2 from an unreadable FILE, so deleting frDropped and frAnomalies
      -- from the clean check left an audit with fifty-seven refused events exiting
      -- zero, with the suite green. The lines themselves were never rendered.
      let repo = fst owner
          -- Signed by a key that is not the owner and was never delegated: the
          -- fold's rule 3, which is what an audit is for.
          ev = mkEvent mallory mallory
                 (AOpen repo HubIssue "planted" [] Nothing Nothing Nothing 1000)
                 (\eid -> CanonContent repo eid 1 (Just 1) Nothing Nothing 1 Nothing)
          thr = eventId ev
      st <- readCanon (byPath
              [ ("version", renderMeta)
              , (encodePath (threadDir thr <> "/" <> eventFileName 1 thr), renderEvent ev) ])
              repo >>= either (fail . show) pure

      stBad st `shouldBe` []
      length (frDropped (stFold st)) `shouldBe` 1
      reportCode st `shouldBe` 2
      -- And the drop is a line in the report, with a reason a reader can act on.
      let ls = fmap render (reportDoc st)
      any ("not blessed by an authorized key" `Text.isInfixOf`) ls `shouldBe` True
      -- The summary counts it, which it did not: three of the five findings were
      -- printed and two were left out, so a hook saw zeroes beside a non-zero code.
      last ls `shouldSatisfy` Text.isInfixOf "dropped 1"

    it "prints an anomaly, which is admitted canon nobody should have written" $ do
      owner <- kp
      alice <- kp
      -- The other half of what the verb is for, and the half nothing rendered: an
      -- anomaly is fold-LEGAL, so it cannot be a drop, and PEP-19 says to show it
      -- rather than hide it. An unnormalised attribute value is one: the fold
      -- cannot fix it (the author box is signed) and must not drop it.
      let repo = fst owner
          ev = mkEvent alice owner
                 (AOpen repo HubIssue "an issue" [] Nothing Nothing Nothing 1000)
                 (\eid -> CanonContent repo eid 1 (Just 1) Nothing Nothing 1 Nothing)
          thr = eventId ev
          -- Owner-signed, so it is admitted; labels not in canonical form, so it
          -- is an anomaly.
          bad = mkEvent owner owner (ASet thr "labels" "b,a,b" 2000)
                  (\eid -> CanonContent repo eid 2 Nothing Nothing Nothing 2 Nothing)
      st <- readCanon (byPath
              [ ("version", renderMeta)
              , (encodePath (threadDir thr <> "/" <> eventFileName 1 thr), renderEvent ev)
              , (encodePath (threadDir thr <> "/" <> eventFileName 2 (eventId bad))
                , renderEvent bad) ]) repo
              >>= either (fail . show) pure

      frDropped (stFold st) `shouldBe` []
      length (frAnomalies (stFold st)) `shouldBe` 1
      reportCode st `shouldBe` 2
      let ls = fmap render (reportDoc st)
      any ("canonical form" `Text.isInfixOf`) ls `shouldBe` True
      last ls `shouldSatisfy` Text.isInfixOf "anomalies 1"

    it "puts the commit, the tree version and the owner key in the header" $ do
      owner <- kp
      -- The header is the only line that says what was audited, and every part of
      -- it was added after somebody could not tell from a report what it had read.
      st <- readCanon (byPath [("version", renderMeta)]) (fst owner)
              >>= either (fail . show) pure
      let header = head (fmap render (reportDoc st))
      header `shouldSatisfy` Text.isInfixOf "canon deadbeef"
      header `shouldSatisfy` Text.isInfixOf "(hub-meta 1)"
      header `shouldSatisfy`
        Text.isInfixOf (Text.pack (show (pretty (AsBase58 (fst owner)))))

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
      -- The header names the key the audit ran AGAINST. Pasting a fork's key
      -- instead of upstream's gives "admitted 0 dropped 57", which reads exactly
      -- like mass forgery, and the report did not say which key it used.
      head (fmap render (reportDoc clean)) `shouldSatisfy`
        Text.isInfixOf (Text.pack (show (pretty (AsBase58 repo))))

      -- One unreadable file and nothing else wrong. Still 2: a file somebody has
      -- to look at is a finding, and the drops are not the only kind.
      dirty <- readCanon (byPath (good <> [("threads/x/junk", "not an event")])) repo
                 >>= either (fail . show) pure
      frDropped (stFold dirty) `shouldBe` []
      reportCode dirty `shouldBe` 2

  where
    encodePath = TextE.encodeUtf8 . Text.pack
    lastOf xs = if null xs then Nothing else Just (last xs)
