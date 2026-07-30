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
  , csBlob    = \oid -> pure (lookup oid byOid)
  }
  where
    oidOf i = Text.pack (show i)
    byOid = [ (oidOf i, t) | (i,(_,t)) <- zip [0 :: Int ..] files ]

-- Every constructor of CanonUnreadable, once. Not derived from anything: the
-- point is that adding one to the type does not silently leave it out of the
-- checks below, and the -Werror=incomplete-patterns in the module under test is
-- what makes forgetting it a build failure there.
everyRefusal :: [CanonUnreadable]
everyRefusal =
  [ NoCanonRef
  , NoRepository "not a git repository"
  , RefUnresolved "bad object"
  , TreeUnreadable "missing blob"
  , CanonTooNewHere 99
  , VersionUnreadable FileUnreadable
  , CanonTooBig (maxCanonBytes + 1)
  , CanonTooMany (maxCanonFiles + 1)
  ]

spec :: Spec
spec = do

  describe "PEP-22 hub verify" $ do

    it "gives every refusal its own exit code, all of them above the audit's" $ do
      let codes = fmap codeOf everyRefusal
      -- Distinct, because a script branches on these. Two pairs shared a code:
      -- "the ref is broken" with "the tree will not list", and "too big" with
      -- "too many", which differ in which bound there is to argue with.
      sort codes `shouldBe` sort (nub codes)
      -- Above 2, which is a completed audit that found something, and above 1,
      -- which is a usage error. A hook tells "could not run" from "ran and found
      -- things" by the number alone.
      filter (< 3) codes `shouldBe` []

    it "tells every refusal what to do about it" $
      -- Two of these used to print a bare complaint through a wildcard: a
      -- directory that is not a repository, and a version file that will not
      -- read. They are the two a reader is least likely to work out unaided.
      for_ everyRefusal $ \u ->
        length (Text.lines (render (refusalDoc u))) `shouldSatisfy` (> 1)

    it "escapes so that nothing can spell an escape" $ do
      -- The escaping is only worth having if it is injective. Without escaping the
      -- backslash itself, a title containing the four characters \x0a printed
      -- exactly like a title containing a newline, so a field could spell out the
      -- notation meant to expose it and a reader could not tell which they had.
      safeText "a\\x0ab" `shouldNotBe` safeText "a\nb"
      safeText "a\nb" `shouldBe` "a\\x0ab"
      -- A right-to-left override reverses the tail of a line on a terminal that
      -- honours it, and is not a control character, so isControl does not cover it.
      safeText "ok\x202Elater" `shouldBe` "ok\\x202elater"
      -- Ordinary text, including non-Latin text, is left alone: this is an escape
      -- for what breaks a line, not a transliteration.
      safeText "\1090\1077\1084\1072" `shouldBe` "\1090\1077\1084\1072"

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
      any ("\\x0a" `Text.isInfixOf`) ls `shouldBe` True
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
                 , (encode (threadDir thr <> "/" <> eventFileName 1 thr), renderEvent eOpen) ]

      clean <- readCanon (byPath good) repo >>= either (fail . show) pure
      reportCode clean `shouldBe` 0

      -- One unreadable file and nothing else wrong. Still 2: a file somebody has
      -- to look at is a finding, and the drops are not the only kind.
      dirty <- readCanon (byPath (good <> [("threads/x/junk", "not an event")])) repo
                 >>= either (fail . show) pure
      frDropped (stFold dirty) `shouldBe` []
      reportCode dirty `shouldBe` 2

  where
    encode = TextE.encodeUtf8 . Text.pack
    lastOf xs = if null xs then Nothing else Just (last xs)
