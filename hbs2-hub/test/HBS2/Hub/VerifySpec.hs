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
import HBS2.Prelude.Plated (pretty,fromString)

import Data.ByteString (ByteString)
import Data.List (nub,sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextE
import Data.Foldable (for_)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Data.Char qualified as Char
import Control.Exception (bracket,try)
import GHC.IO.Handle (hDuplicate,hDuplicateTo)
import HBS2.Data.Types.Refs (HashRef(..))
import System.Exit (ExitCode(..))
import System.IO (stdout,stderr,hClose,withFile,IOMode(WriteMode),hSetBuffering,BufferMode(..))
import System.Posix.IO qualified as Posix
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
  , csClose   = pure ()
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
  , (RefUnresolved (ToolSaid "bad object"),          5)
  , (RefUnresolved (ReaderSays "not an object id: zz"), 5)
  , (CanonTooNewHere 99,                  6)
  , (VersionUnreadable (FileMalformed (BadClause "hub-meta")), 7)
  -- Not the file being wrong but this clone not having it: the ordinary state
  -- after clone --filter=blob:none, and 7 says canon is broken.
  , (VersionUnreadable FileObjectMissing, 13)
  , (VersionUnreadable FileListingUnparsed, 13)
  , (VersionUnreadable (FileUnreadable "cannot read"), 13)
  -- The file reads and the number in it is not a version, which is the
  -- publisher's problem and not this clone's: 7, not 13.
  , (VersionUnreadable (FileBadVersion 0), 7)
  , (CanonTooBig (maxCanonBytes + 1),     8)
  , (TreeUnreadable (ToolSaid "missing blob"),   9)
  , (TreeUnreadable (ReaderSays "gave up"),      9)
  , (CanonTooMany (maxCanonFiles + 1),   10)
  , (CanonListingTooBig maxListingBytes, 11)
  , (ReaderFailed "fork: Resource exhausted", 12)
  , (CanonTooSlow "passed 180s", 14)
  , (ToolStalled "git show-ref did not finish in 60s", 15)
  , (BlobsOutOfStep "cat-file --batch answered about another object", 16)
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
      -- No code shared by two DIFFERENT constructors, which is the thing
      -- -Werror=incomplete-patterns cannot catch: it forces a new constructor to
      -- get a case in codeOf and does not force that case to be a new number.
      --
      -- The previous version of this line was `sort (nub codes) == nub (sort
      -- codes)`, which is true of every list there is: it asserted nothing at all
      -- while its comment claimed exactly this property. Two constructors sharing
      -- a code passed under it.
      --
      -- VersionUnreadable appears three times on purpose, twice with 13: one
      -- constructor whose code depends on its payload, which is why the grouping
      -- is by constructor name and not by value.
      let name = takeWhile (/= ' ') . show
          shares = [ (a, b, c) | (ua, c) <- everyRefusal, (ub, c') <- everyRefusal
                               , c == c', let a = name ua, let b = name ub, a /= b ]
      shares `shouldBe` []
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

    it "tells each refusal the thing that is true of IT" $ do
      -- "More than one line" and "no two are word for word the same" are both
      -- satisfied by advice attached to the wrong constructor. Three branches
      -- could be deleted outright and the suite stayed green: the refusals whose
      -- payload goes through toolSaid are multi-line for free, and the pairwise
      -- check tells them apart by that payload rather than by the advice.
      --
      -- So this pins a phrase per constructor. It is deliberately the phrase a
      -- reader ACTS on, not a decoration: what to run, or what not to bother
      -- running.
      let says u phrase =
            (show (pretty u), phrase)
              `shouldSatisfy` \_ -> phrase `Text.isInfixOf`
                                      Text.unlines (drop 1 (Text.lines (render (refusalDoc u))))
      says (NoCanonRef "/x/.git") "git fetch"
      says (NoRepository "x") "safe.directory"
      says (RefUnresolved (ToolSaid "x")) "unshallow"
      says (CanonTooNewHere 99) "Upgrade"
      says (VersionUnreadable (FileMalformed (BadClause "hub-meta"))) "rewrite that file"
      says (VersionUnreadable (FileBadVersion 0)) "rewrite that file"
      says (VersionUnreadable FileObjectMissing) "shallow clone"
      -- These two both say "report the git version", so the phrase alone lets
      -- their bodies be swapped: each would still contain the phrase asserted of
      -- it, and the pairwise check only requires that they differ. So each is
      -- pinned by the words that are true of IT and of nothing else.
      says (VersionUnreadable FileListingUnparsed) "not the file"
      says (CanonTooBig 1) "PEP-19"
      says (TreeUnreadable (ToolSaid "x")) "fetching"
      says (TreeUnreadable (ReaderSays "x")) "Fetching will not help"
      says (CanonTooMany 1) "PEP-19"
      says (CanonListingTooBig 1) "PEP-19"
      says (ReaderFailed "x") "not on PATH"
      says (CanonTooSlow "x") "pre-receive hook"
      -- The two that used to borrow somebody else's advice: a git that is present
      -- and silent got "this is local, retry", and a batch reply out of step got
      -- "compaction". Both are named here so neither can drift back.
      says (ToolStalled "x") "retrying"
      says (BlobsOutOfStep "x") "The tree listed and its files did not read"

    it "exits with what it computed, and 141 when the pipe is gone" $ do
      owner <- kp
      -- THE VERB'S OWN IO, which nothing reached: reportDoc and reportCode are
      -- pure and tested, and the function that prints one and exits with the
      -- other was not. Its whole content is the two things below.
      let repo = fst owner
      clean <- readCanon (byPath [("version", renderMeta)]) repo
                 >>= either (fail . show) pure
      found <- readCanon (byPath [("version", renderMeta), ("threads/t/x", "junk")]) repo
                 >>= either (fail . show) pure
      reportCode clean `shouldBe` 0
      reportCode found `shouldBe` 2

      -- A misnamed file is PRINTED AND COUNTED. "Printed and not counted" is the
      -- defect this one function has had twice already -- the missing tree
      -- version, then the files with no version clause of their own -- and both
      -- times stdout listed findings while the summary said zero and the exit
      -- code said clean. Deleting the misnamed clause from `clean` left the suite
      -- green until this line.
      let misnamed = clean { stMisnamed = [("threads/t/hello", NameNotEventShaped)] }
      reportCode misnamed `shouldBe` 2
      any ("misnamed" `Text.isPrefixOf`) (fmap render (reportDoc misnamed))
        `shouldBe` True
      -- And on the summary line, which is what a hook parses.
      last (fmap render (reportDoc misnamed)) `shouldSatisfy` Text.isInfixOf "misnamed 1"

      -- Quietly, because the point is the exit code and not the text: stdout goes
      -- to /dev/null for the two that print.
      let quietly act = bracket (hDuplicate stdout) (\o -> hDuplicateTo o stdout >> hClose o)
                          (\_ -> withFile "/dev/null" WriteMode $ \n ->
                                   hDuplicateTo n stdout >> act)
      -- A clean audit RETURNS, and does not exit 0 through exitWith: `hub verify`
      -- is one verb in a dispatcher, so exiting on success would stop whatever
      -- else the caller asked for.
      quietly (try (report clean)) `shouldReturn` (Right () :: Either ExitCode ())
      quietly (try (report found)) `shouldReturn` Left (ExitFailure 2)

      -- 141 IS 128 PLUS SIGPIPE, and it used to be 1, which PEP-22 gives to a bad
      -- argument: `hub verify | head -1` on a long report told a hook it had been
      -- called wrongly. Reproduced by writing into a pipe whose read end is shut,
      -- which is what head leaves behind.
      (r, w) <- Posix.createPipe
      Posix.closeFd r
      wh <- Posix.fdToHandle w
      -- UNBUFFERED, so the write reaches the pipe inside the handler's scope. A
      -- buffered handle swallows a short report whole and raises nothing until
      -- the flush at exit, by which time the code is already chosen -- which is
      -- also why a short report piped into `head` legitimately exits 2: nothing
      -- ever failed to write.
      gone <- bracket (hDuplicate stdout)
                (\o -> hDuplicateTo o stdout >> hClose o
                         >> hSetBuffering stdout LineBuffering)
                (\_ -> do hDuplicateTo wh stdout
                          hSetBuffering stdout NoBuffering
                          try (report found))
      hClose wh
      gone `shouldBe` (Left (ExitFailure 141) :: Either ExitCode ())

      -- And the OTHER half of the verb's IO, which nothing joined up: every
      -- check of a refusal's code so far has been of codeOf in isolation, and
      -- nothing asserted that the process actually leaves with it. That is the
      -- one thing the narrowing of the 141 handler was written for -- it is
      -- scoped to the report so that a usage error whose stderr is closed keeps
      -- its own code -- and it was unobserved.
      let onStderr act = bracket (hDuplicate stderr)
                           (\o -> hDuplicateTo o stderr >> hClose o)
                           (\_ -> withFile "/dev/null" WriteMode $ \n ->
                                    hDuplicateTo n stderr >> act)
      onStderr (try (refused (NoRepository "not a git repository")))
        `shouldReturn` (Left (ExitFailure 4) :: Either ExitCode ())
      onStderr (try (refused (ToolStalled "did not finish")))
        `shouldReturn` (Left (ExitFailure 15) :: Either ExitCode ())

    it "says how to spell the verb when the arguments are wrong" $
      -- `hub verify --help` landed on "hbs2-hub: BadFormException
      -- (hub:verify --help)", which names an internal type and a spelling
      -- nobody typed. The dispatcher that reaches this is not testable without
      -- an hbs2-cli context; the text it dies with is.
      Text.unpack (render usage) `shouldContain` "usage: hub verify <repo-key>"

    it "prints a hash-shaped field that is not a hash by its size" $ do
      -- A HashRef takes any length on the wire and validHashRef is applied to the
      -- AUTHOR box only, so a maintainer can stamp a canon box whose origin is
      -- tens of kilobytes. base58 is quadratic (2.5 s per 64 KiB, measured in this
      -- project), and the report can print a thousand of these lines.
      let h = HashRef (fromString (replicate 40000 (Char.chr 122)))
      validHashRef h `shouldBe` False
      let out = render (pretty (DupOrigin h))
      -- Says what is true of it -- its size -- and does not spend the base58.
      Text.unpack out `shouldContain` "not a hash"
      Text.length out `shouldSatisfy` (< 200)

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

    it "stays injective when a path is too long to print" $ do
      owner <- kp
      -- A tree entry may hold a path of any length (measured: one record of
      -- 1 048 638 bytes), and pathText spends up to six characters on a byte, so
      -- one entry could put six megabytes on a terminal. The first attempt at
      -- bounding that cut the escaped text at 300 characters and appended the byte
      -- length -- which made two paths of equal length sharing a 300-character
      -- prefix print as ONE line, undoing the property three rounds of this went
      -- into. The digest is what keeps it.
      let long n = "threads/t/" <> BS.replicate 400 0x61 <> B8.pack (show (n :: Int))
          p1 = long 1
          p2 = long 2
      BS.length p1 `shouldBe` BS.length p2
      BS.take 300 p1 `shouldBe` BS.take 300 p2
      st <- readCanon (byPath [ ("version", renderMeta)
                              , (p1, "not an event"), (p2, "not an event") ])
              (fst owner) >>= either (fail . show) pure
      let ls = [ l | l <- fmap render (reportDoc st), "unreadable" `Text.isPrefixOf` l ]
      length ls `shouldBe` 2
      nub ls `shouldBe` ls
      -- And the line is bounded: not four hundred characters of a.
      for_ ls $ \l -> Text.length l `shouldSatisfy` (< 420)

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
      -- refusal, and there are nineteen entries below covering thirteen of them.
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

      -- And the three that ARE allowed to share say the same thing word for word,
      -- which is the claim the exemption above rests on. Asserted rather than
      -- assumed: with only the exemption, changing one of the three and not the
      -- others is invisible, and the phrase table cannot see it either, since
      -- their one phrase is the one they share.
      let three = [ advice u | (u, c) <- everyRefusal, c `elem` bounds' ]
      length three `shouldBe` 3
      nub three `shouldBe` take 1 three

      -- The filter above compares refusals with DIFFERENT codes, so two sharing
      -- one are never put side by side. Three do: the pairs that matter are named
      -- here. (TreeUnreadable's two are their own example further down.)
      advice (VersionUnreadable FileObjectMissing)
        `shouldNotBe` advice (VersionUnreadable FileListingUnparsed)
      -- These two SHARE, and that is right: a version file with a broken clause
      -- and one whose number is not a version are both the publisher's to
      -- rewrite, and only the line above the advice differs. Pinned so the
      -- sharing is a decision rather than an accident.
      advice (VersionUnreadable (FileMalformed (BadClause "hub-meta")))
        `shouldBe` advice (VersionUnreadable (FileBadVersion 0))
      -- RefUnresolved's two are not compared here: `advice` drops one line, and a
      -- ToolSaid payload is a block of several, so the two differ by the quoted
      -- text rather than by anything advisory. What tells them apart is the
      -- marker, which "quotes git and does not quote itself" below asserts.

    it "quotes git and does not quote itself" $ do
      -- THE HEAD OF THE MATTER, and nothing tested it. `told` is what puts git's
      -- words in a | block and this program's in a sentence, and every existing
      -- check of that block used ToolSaid: replacing told with one that sends
      -- both through toolSaid left the suite green, which means the marker could
      -- go back to sitting exactly where this program's advice sits.
      let markers u = [ l | l <- Text.lines (render (refusalDoc u))
                          , "  | " `Text.isPrefixOf` l ]
      -- git said it: quoted, and the words are inside the block.
      markers (RefUnresolved (ToolSaid "fatal: bad object"))
        `shouldBe` ["  | fatal: bad object"]
      -- This reader said it: no marker anywhere, and the words are still printed.
      markers (RefUnresolved (ReaderSays "not an object id: zz")) `shouldBe` []
      Text.unpack (render (refusalDoc (RefUnresolved (ReaderSays "not an object id: zz"))))
        `shouldContain` "not an object id: zz"
      -- And the same for the one a new user meets first, which is a message from
      -- the runtime about git and not from git.
      markers (ReaderFailed "git: startProcess: does not exist") `shouldBe` []

    it "tells a tree git could not read from one this reader could not parse" $ do
      -- The pair above compares refusals with DIFFERENT codes, so it cannot see
      -- this one: both of these are code 9. The distinction is the whole of what
      -- 'Told' was added for, and nothing in the suite could tell whether it had
      -- been used -- deleting the split left every example green.
      --
      -- What the split buys: git's complaints have several causes and some are
      -- fixed by fetching, while this reader's own refusals (a bound reached, a
      -- format it cannot parse, a batch reply it will not follow) are not fixed
      -- by fetching at all and were being told to fetch.
      let advice u = Text.unlines (drop 1 (Text.lines (render (refusalDoc u))))
          tool = advice (TreeUnreadable (ToolSaid "missing blob"))
          ours = advice (TreeUnreadable (ReaderSays "gave up"))
      tool `shouldNotBe` ours
      Text.unpack tool `shouldContain` "fetching"
      Text.unpack ours `shouldContain` "Fetching will not help"

    it "bounds the report's lines without changing what it counts" $ do
      -- The report had no bound of any kind, and no read bound reaches it: a path
      -- that is not where canon puts things never becomes a blob, so the walk's
      -- budget never sees it, and it is printed all the same. Measured on the
      -- unbounded version: 4555 bytes of git objects, 369 600 298 bytes of
      -- stdout, 661 MB resident, exit 2. Eighty thousand times the input.
      owner <- kp
      let repo = fst owner
          n = 1200
          files = ("version", renderMeta)
                    : [ ( B8.pack ("threads/t/" <> show i <> "-x"), "not an event" )
                      | i <- [1 .. n :: Int] ]
      st <- readCanon (byPath files) repo >>= either (fail . show) pure

      let ls = fmap render (reportDoc st)
          unreadable = [ l | l <- ls, "unreadable " `Text.isPrefixOf` l ]
      -- Cut, and SAID to be cut: a report that silently stops listing reads as a
      -- report that found nothing more.
      length unreadable `shouldBe` 1000
      any (Text.isPrefixOf "and 200 more unreadable lines") ls `shouldBe` True
      -- And the counts are of all of them, because that is what a hook reads and
      -- what the exit code is computed from.
      any (Text.isInfixOf ("unreadable " <> Text.pack (show n))) ls `shouldBe` True
      reportCode st `shouldBe` 2

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
      header `shouldSatisfy` Text.isInfixOf (Text.strip renderMeta)
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
