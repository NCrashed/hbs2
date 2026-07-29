module HBS2.Hub.CanonSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Canon
import HBS2.Hub.Fold
import HBS2.Hub.Letter (oversizedField,maxTitle,maxLabel,maxLabels,maxInlineBody,maxRef,maxAttrName,maxAttrValue)
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.GroupKeySymm (typicalKeyLength)
import HBS2.Data.Types.Refs (HashRef)

import Data.Config.Suckless

import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import Test.Hspec

type KP = (HubKey, PrivKey 'Sign HubScheme)

kp :: IO KP
kp = do
  c <- newCredentials @'HBS2Basic
  pure (_peerSignPk c, _peerSignSk c)

someHash :: IO HashRef
someHash = do
  (pk,sk) <- kp
  pure (authorBoxId (signAuthor pk sk (ARevoke pk pk 0)))

secret32 :: PartSecret
secret32 = fromMaybe (error "bad fixture secret")
             (mkPartSecret (BS.replicate typicalKeyLength 0x41))

canon :: RepoRef -> Word64 -> Maybe Word64 -> Maybe HashRef -> Maybe PartSecret
      -> EventId -> CanonContent
canon repo sq num origin sec eid = CanonContent repo eid sq num origin sq sec

coords :: PRCoords
coords = PRCoords (Just "hbs23://fork") "refs/heads/f" "aaaa" "refs/heads/master" "bbbb" Nothing

-- A title nobody would type and anybody can send.
nasty :: Text
nasty = "a \"quoted\" \\ title\nand \1088\1091\1089\1089\1082\1080\1081"

spec :: Spec
spec = do

  describe "PEP-19 canon file" $ do

    it "round-trips an event through the file bytes" $ do
      owner <- kp
      alice <- kp
      origin <- someHash
      part <- someHash
      -- The two boxes are the file. Everything a writer produces has to come
      -- back byte for byte, or canon is whatever the writer happened to emit
      -- and no second implementation can read it.
      let repo = fst owner
          ev = mkEvent alice owner
                 (AOpen repo HubPR nasty ["bug"] (Just nasty) (Just part) (Just coords) 42)
                 (canon repo 7 (Just 3) (Just origin) (Just secret32))
      parseEvent (renderEvent ev) `shouldBe` Right (hubEventVersion, ev)
      -- One clause per line, one clause of each name. Two answers to the same
      -- question is what 'only' refuses, and the format is declared forever.
      case parseTop (renderEvent ev) of
        Left e      -> expectationFailure ("the file does not re-parse: " <> show e)
        Right forms -> do
          let cs = concat [ xs | List _ xs@(List{} : _) <- forms ] <> forms
              named n = length [ () | List _ (SymbolVal m : _) <- cs, m == n ]
          map named ["hub-event","author-box","canon-box","op","seq"]
            `shouldBe` [1,1,1,1,1]

    it "round-trips every op, with and without the optional clauses" $ do
      owner <- kp
      alice <- kp
      target <- someHash
      origin <- someHash
      let repo = fst owner
          contents =
            [ AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1
            , AOpen repo HubPR "t" ["a","b"] (Just "body") Nothing (Just coords) 1
            , AComment target (Just target) (Just "hi") Nothing 2
            , ARevise target coords 3
            , ASet target "labels" "a,b" 4
            , AClose target (Just "done") 5
            , AReopen target Nothing 6
            , AMerge target "cafe" "refs/heads/master" 7
            , ARedact target 8
            , ADelegate repo (fst alice) 9
            , ARevoke repo (fst alice) 10
            ]
          evs = [ mkEvent alice owner c (canon repo 1 Nothing Nothing Nothing) | c <- contents ]
             <> [ mkEvent alice owner c (canon repo 2 (Just 5) (Just origin) (Just secret32))
                | c <- take 1 contents ]
      mapM_ (\ev -> parseEvent (renderEvent ev) `shouldBe` Right (hubEventVersion, ev)) evs

    it "reads back an event standing on every limit the bridge will mint at" $ do
      owner <- kp
      alice <- kp
      part <- someHash
      -- The gap this closes was invisible for the plainest reason: every body in
      -- every fixture was one character long. The bridge would mint a 32 KiB
      -- body, this module would write the file, and the same build would answer
      -- TooLarge when asked to read it back. A reader bound has to be derived
      -- from what the writer admits, and the test for that has to stand on the
      -- limits rather than near them.
      let repo = fst owner
          filled n = Text.replicate n "x"
          -- The tightest axis of the derived bound is the doubling a projection
          -- suffers when every character has to be escaped, so one fixture is
          -- made of the character that does it.
          quoted n = Text.replicate n "\""
          full = AOpen repo HubPR (filled maxTitle)
                   (replicate maxLabels (filled maxLabel))
                   (Just (filled maxInlineBody)) (Just part)
                   (Just (PRCoords (Just (filled maxRef)) (filled maxRef) (filled maxRef)
                            (filled maxRef) (filled maxRef) (Just part)))
                   42
          biggest = mkEvent alice owner full (canon repo 7 (Just 3) Nothing (Just secret32))
          attrs = mkEvent owner owner
                    (ASet part (Text.replicate maxAttrName "a")
                       (Text.replicate maxAttrValue "v") 43)
                    (canon repo 8 Nothing Nothing Nothing)
          note = mkEvent owner owner
                   (AClose part (Just (filled maxInlineBody)) 44)
                   (canon repo 9 Nothing Nothing Nothing)
      oversizedField full `shouldBe` Nothing     -- the bridge would mint this
      parseEvent (renderEvent biggest) `shouldBe` Right (hubEventVersion, biggest)
      parseEvent (renderEvent attrs) `shouldBe` Right (hubEventVersion, attrs)
      parseEvent (renderEvent note) `shouldBe` Right (hubEventVersion, note)
      let escaped = mkEvent alice owner
                      (AOpen repo HubIssue (quoted maxTitle) (replicate maxLabels (quoted maxLabel))
                         (Just (quoted maxInlineBody)) Nothing Nothing 45)
                      (canon repo 10 (Just 4) Nothing Nothing)
      parseEvent (renderEvent escaped) `shouldBe` Right (hubEventVersion, escaped)

    it "keeps the boxes readable under a title written to break the file" $ do
      owner <- kp
      alice <- kp
      -- The projection carries a stranger's words next to the authoritative
      -- boxes. One unescaped quote there would not spoil a display line, it
      -- would make the file unparseable, permanently, with two valid signatures
      -- inside it.
      let repo = fst owner
          hostile = "(canon-box x) " <> nasty
          ev = mkEvent alice owner
                 (AOpen repo HubIssue hostile [] (Just nasty) Nothing Nothing 1)
                 (canon repo 1 (Just 1) Nothing Nothing)
          file = renderEvent ev
      parseEvent file `shouldBe` Right (hubEventVersion, ev)
      -- Reading the boxes back is not enough to know the file survived: they
      -- are written first, so a title that ends its string early damages only
      -- what comes after it. What has to hold is that the file still says what
      -- it was written to say, clause for clause.
      case parseTop file of
        Left e      -> expectationFailure ("the file does not re-parse: " <> show e)
        Right forms -> do
          let cs = concat [ xs | List _ xs@(List{} : _) <- forms ] <> forms
          [ t | List _ [SymbolVal "title", LitStrVal t] <- cs ] `shouldBe` [hostile]
          -- and the clause after it is still a clause, not part of the title
          [ () | List _ (SymbolVal "created" : _) <- cs ] `shouldBe` [()]

    it "keeps the boxes readable under an attribute name written to break the file" $ do
      owner <- kp
      alice <- kp
      -- The same hole one field over, and the one nothing else closes: the fold
      -- admits an attribute name that is not a vocabulary word (it only reports
      -- it), so a name of "a)(canon-box" reaches this file from a letter, and
      -- compaction re-renders canon somebody else wrote.
      target <- someHash
      let name = "a)(canon-box x) \"q"
          repo = fst owner
          ev = mkEvent alice owner (ASet target name "v" 1)
                 (canon repo 1 Nothing Nothing Nothing)
          file = renderEvent ev
      parseEvent file `shouldBe` Right (hubEventVersion, ev)
      case parseTop file of
        Left e      -> expectationFailure ("the file does not re-parse: " <> show e)
        Right forms -> do
          let cs = concat [ xs | List _ xs@(List{} : _) <- forms ] <> forms
          [ (k,v) | List _ [SymbolVal "set", LitStrVal k, LitStrVal v] <- cs ]
            `shouldBe` [(name,"v")]

    it "reports a file from a newer schema without obeying it" $ do
      owner <- kp
      alice <- kp
      -- The version is reported, never obeyed. It vetoes neither the tree, which
      -- would hand a veto to anyone who can write one unsigned line, nor the
      -- file: the boxes are self-describing CBOR and a version this build does
      -- not know does not make them unreadable. Refusing here would throw away
      -- the stamp the fold needs, which is the same mistake one floor up.
      let repo = fst owner
          ev = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                 (canon repo 1 (Just 1) Nothing Nothing)
          file = renderEvent ev
          bumped = Text.replace "(hub-event 1)" "(hub-event 4294967295)" file
      parseEvent bumped `shouldBe` Right (4294967295, ev)
      -- what a newer schema can really do is put content in a box this build
      -- cannot decode, and the fold answers that itself, keeping the stamp
      map drWhy (frDropped (foldEvents repo [ev])) `shouldBe` []

    it "refuses a file too large to be worth reading" $ do
      owner <- kp
      alice <- kp
      -- Reading is the first thing anyone does with a tree and the last thing
      -- they can refuse to do, so the cost of a file has to be bounded by
      -- something cheaper than parsing it. Base58 is quadratic in the token, and
      -- the parser is superlinear in the number of forms, so either one is a way
      -- to stop every fold in every clone with one file.
      let repo = fst owner
          ev = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                 (canon repo 1 (Just 1) Nothing Nothing)
          file = renderEvent ev
      parseEvent (file <> Text.replicate maxEventBytes " ") `shouldBe` Left (TooLarge "file")
      parseEvent (file <> Text.replicate (maxClauses + 1) "()")
        `shouldBe` Left (TooLarge "clauses")
      parseEvent (Text.unlines
        [ "(hub-event 1)"
        , "(author-box " <> Text.replicate (maxTokenBytes + 1) "z" <> ")"
        , "(canon-box zzz)"
        ]) `shouldBe` Left (TooLarge "author-box")
      -- ...and a body full of brackets is a contributor, not an attack: counting
      -- every parenthesis in the file capped an issue at 114 of them.
      let bracketed = mkEvent alice owner
                        (AOpen repo HubIssue "t" [] (Just (Text.replicate 500 "(")) Nothing
                           Nothing 1)
                        (canon repo 1 (Just 1) Nothing Nothing)
      parseEvent (renderEvent bracketed) `shouldBe` Right (hubEventVersion, bracketed)

    it "refuses a file with a clause missing, twice over, or misshapen" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          ev = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                 (canon repo 1 (Just 1) Nothing Nothing)
          file = renderEvent ev
          without name = Text.unlines
            [ l | l <- Text.lines file, not (("(" <> name) `Text.isPrefixOf` l) ]
      parseEvent (without "hub-event") `shouldBe` Left (MissingClause "hub-event")
      parseEvent (without "author-box") `shouldBe` Left (MissingClause "author-box")
      parseEvent (without "canon-box") `shouldBe` Left (MissingClause "canon-box")
      -- Two answers and no rule for choosing: refuse rather than pick.
      parseEvent (file <> "(canon-box zzz)\n") `shouldBe` Left (BadClause "canon-box")
      -- base58 that is not a box
      parseEvent (Text.unlines
        [ "(hub-event 1)", "(author-box zzz)", "(canon-box zzz)" ])
        `shouldBe` Left (BadClause "author-box")
      case parseEvent "(((" of
        Left (NotAnEvent _) -> pure ()
        other -> expectationFailure ("expected a parse failure, got " <> show other)

    it "does not read the projection back" $ do
      owner <- kp
      alice <- kp
      -- The clauses beside the boxes are regenerated for humans. A reader that
      -- believed (seq 5) over the canon box would be trusting an unsigned line
      -- of text that anyone who can write the file can write.
      let repo = fst owner
          ev = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                 (canon repo 1 (Just 1) Nothing Nothing)
          lied = Text.replace "(seq 1)" "(seq 99)" (renderEvent ev)
      parseEvent lied `shouldBe` Right (hubEventVersion, ev)
      -- and what the fold reads is still the box
      let fr = foldEvents repo [ev]
      frMaxSeq fr `shouldBe` 1

    it "writes the tree version and reads it back" $ do
      parseMeta renderMeta `shouldBe` Right hubMetaVersion
      parseMeta "(hub-meta 2)" `shouldBe` Right 2
      parseMeta "" `shouldBe` Left (MissingClause "hub-meta")

    it "round-trips the number index" $ do
      a <- someHash
      b <- someHash
      -- Regenerable and never trusted over the open events, but it still has to
      -- survive a write and a read, since the whole point is reading #42
      -- without folding.
      let ns = [(1,a),(42,b)]
      parseNumberIndex (renderNumberIndex ns) `shouldBe` Right ns
      parseNumberIndex "" `shouldBe` Right []
      parseNumberIndex "(number 1)" `shouldBe` Left (BadClause "number")
      -- Its own bounds, and not the event file's: an index has an entry per
      -- thread, so an event's clause bound would have capped a repository at
      -- 128 issues and returned a hard error on the 129th.
      let many' = [ (fromIntegral n, a) | n <- [1 .. 500 :: Int] ]
      parseNumberIndex (renderNumberIndex many') `shouldBe` Right many'
      -- ...and bounds of its own, checked before the file is split into lines,
      -- since reading a hint must cost less than folding the canon it is a hint
      -- about. Refusing one is not refusing the repository: the index is
      -- regenerable from the open events and never trusted over them.
      parseNumberIndex (Text.replicate (3 * 1024 * 1024) "x")
        `shouldBe` Left (TooLarge "index-file")
      parseNumberIndex ("(number 1 " <> Text.replicate 300 "z" <> ")")
        `shouldBe` Left (TooLarge "index-line")

    it "reads a whole tree, keeping what parsed and naming what did not" $ do
      owner <- kp
      alice <- kp
      -- The seam both hub sync and hub verify go through, so that there is one
      -- of them: a file that cannot be read has no identity but its path, and
      -- the version of every file is what an operator is owed even when the
      -- file read fine.
      let repo = fst owner
          ev n = mkEvent alice owner
                   (AOpen repo HubIssue (Text.pack (show n)) [] Nothing Nothing Nothing n)
                   (canon repo n (Just n) Nothing Nothing)
          files = [ ("threads/a/1", renderEvent (ev 1))
                  , ("threads/a/2", "(hub-event 1)\n(author-box zz)\n")
                  , ("threads/a/3", renderEvent (ev 3))
                  ]
          got = readEventLog files
      crEvents got `shouldBe` [ev 1, ev 3]
      crBad got `shouldBe` [("threads/a/2", BadClause "author-box")]
      crVersions got `shouldBe` [("threads/a/1",1),("threads/a/3",1)]
