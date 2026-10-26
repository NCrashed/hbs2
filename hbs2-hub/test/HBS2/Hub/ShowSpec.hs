-- | @hub inbox show@: the report, and what it may not let through.
--
-- The rendering is what this verb IS -- it decides nothing else -- and every
-- field in it comes out of a signed box a stranger composed. So the tests below
-- are mostly about the two ways that goes wrong: a field that reaches a terminal
-- unescaped, and a field with no bound reaching it at all.
module HBS2.Hub.ShowSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Letter (Disposition(..),LetterError(..),maxMessageParts)
import HBS2.Hub.Ingress
import HBS2.Hub.CLI.Show
import HBS2.Hub.Render (renderDocument)
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject)
import HBS2.Net.Auth.Credentials
import HBS2.Prelude.Plated (Doc,pretty)

import Data.Config.Suckless (Syntax,C)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Aeson (Value(..),toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Foldable (toList)
import Data.HashMap.Strict qualified as HM
import Data.List (isInfixOf)
import Data.List qualified as List
import Data.Text qualified as Text
import Test.Hspec

shown :: [Doc ()] -> String
shown = unlines . fmap show

mh :: BS.ByteString -> HashRef
mh = HashRef . hashObject

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

b58 :: HubKey -> String
b58 = show . pretty . AsBase58

argv :: [String] -> [Syntax C]
argv = fmap argvAtom

-- One letter that opened, with everything a stranger controls left to the
-- caller.
letter :: HubKey -> AuthorContent -> Disposition -> Shown
letter author ac disp =
  Shown (LetterView (mh "m") (Just author) (Right (author, ac, disp)) (Just (mh "e")) [])
        []
        0
        Nothing
        Nothing
        mempty

spec :: Spec
spec = spec1 >> sanitised >> asJson

spec1 :: Spec
spec1 = do

  describe "PEP-22 hub inbox show: the report" $ do

    it "prints the title, the body and what triage makes of it" $ do
      repo <- aKey ; author <- aKey
      let ac = AOpen repo HubIssue "it crashes" [] (Just "on startup") Nothing Nothing 5
          out = shown (showDoc (letter author ac FoldsToCanon))
      out `shouldSatisfy` isInfixOf "title"
      out `shouldSatisfy` isInfixOf "it crashes"
      out `shouldSatisfy` isInfixOf "| on startup"
      out `shouldSatisfy` isInfixOf "would fold"

    -- The one that matters: this is the first verb that prints a stranger's
    -- title and body at a terminal, and an escape sequence in either of them is
    -- a title that repositions the cursor of the person deciding whether to
    -- publish it.
    it "escapes a title and a body that hold terminal escapes" $ do
      repo <- aKey ; author <- aKey
      let nasty = "x\ESC[2Kfake"
          ac = AOpen repo HubIssue nasty [] (Just nasty) Nothing Nothing 5
          out = shown (showDoc (letter author ac FoldsToCanon))
      out `shouldSatisfy` not . isInfixOf "\ESC"

    -- A body is quoted so that it cannot forge a line of the report: without
    -- the marker, a body whose first line reads "canon    not folded" is
    -- indistinguishable from the line this verb prints itself.
    it "quotes every line of a body, including one that mimics a field" $ do
      repo <- aKey ; author <- aKey
      let body = "canon    not folded\nsecond line"
          ac = AOpen repo HubIssue "t" [] (Just body) Nothing Nothing 5
          out = shown (showDoc (letter author ac FoldsToCanon))
      out `shouldSatisfy` isInfixOf "| canon    not folded"
      out `shouldSatisfy` isInfixOf "| second line"
      -- And the report's own canon line is absent, since nothing asked canon.
      out `shouldSatisfy` not . isInfixOf "\ncanon"

    -- 32 KiB of newlines is 32 000 lines, and the sender chooses the body.
    it "bounds a body by lines and says how many it did not print" $ do
      repo <- aKey ; author <- aKey
      let body = Text.replicate 5000 "x\n"
          ac = AOpen repo HubIssue "t" [] (Just body) Nothing Nothing 5
          out = shown (showDoc (letter author ac FoldsToCanon))
      length (lines out) `shouldSatisfy` (< maxBodyLines + 20)
      out `shouldSatisfy` isInfixOf "more line(s) not shown"

    it "tells an empty body from an absent one" $ do
      repo <- aKey ; author <- aKey
      let with b = shown (showDoc (letter author
                                     (AOpen repo HubIssue "t" [] b Nothing Nothing 5)
                                     FoldsToCanon))
      with (Just "") `shouldSatisfy` isInfixOf "(empty)"
      with Nothing `shouldSatisfy` not . isInfixOf "body"

    -- A letter's labels are a REQUEST (PEP-18): the fold does not apply them.
    -- Printed beside the title with no qualifier, they would read as properties
    -- of a thread that does not exist yet.
    it "says a letter's labels are asked for and not applied" $ do
      repo <- aKey ; author <- aKey
      let ac = AOpen repo HubIssue "t" ["bug","ui"] Nothing Nothing Nothing 5
          out = shown (showDoc (letter author ac FoldsToCanon))
      out `shouldSatisfy` isInfixOf "bug, ui"
      out `shouldSatisfy` isInfixOf "requested"

    it "prints an unreadable letter as far as it got" $ do
      author <- aKey
      let sw = Shown (LetterView (mh "m") (Just author) (Left NotForUs) Nothing []) [] 0 Nothing Nothing mempty
          out = shown (showDoc sw)
      -- The envelope key is the one thing a maintainer can act on (hub block),
      -- so it survives every failure path that knows it.
      out `shouldSatisfy` isInfixOf (b58 author)
      out `shouldSatisfy` isInfixOf "unreadable"

    -- A truncated list is a WRONG list, not a shorter one, and this one is
    -- truncated by a number a stranger chose: the letter names the parts, and
    -- walking each costs a tree walk. So the count that was not walked is said
    -- rather than dropped, and it says what an accept will do with the letter,
    -- since that is the next thing a maintainer tries.
    it "says how many attachments it did not walk, and why" $ do
      repo <- aKey ; author <- aKey
      let ac = AOpen repo HubIssue "t" [] Nothing Nothing Nothing 5
          sw = (letter author ac FoldsToCanon)
                 { swParts = [ (mh "a", Right (PartFacts 1024 True)) ]
                 , swPartsUnmeasured = 40
                 }
          out = shown (showDoc sw)
      out `shouldSatisfy` isInfixOf "parts-unmeasured"
      out `shouldSatisfy` isInfixOf "40"
      -- And the number that decides it, so a reader is not left guessing where
      -- the line falls.
      out `shouldSatisfy` isInfixOf (show maxMessageParts)
      out `shouldSatisfy` isInfixOf "refuses"

    -- The ordinary letter says nothing about it. A note that appears when
    -- nothing was left out is a note nobody reads when something was.
    it "says nothing about unwalked attachments when there are none" $ do
      repo <- aKey ; author <- aKey
      let ac = AOpen repo HubIssue "t" [] Nothing Nothing Nothing 5
          out = shown (showDoc (letter author ac FoldsToCanon))
      out `shouldSatisfy` (not . isInfixOf "parts-unmeasured")

    it "reports each part's size and whether the peer has it" $ do
      repo <- aKey ; author <- aKey
      let ac = AOpen repo HubIssue "t" [] Nothing Nothing Nothing 5
          sw = (letter author ac FoldsToCanon)
                 { swParts = [ (mh "a", Right (PartFacts 1024 True))
                             , (mh "b", Right (PartFacts 7 False))
                             , (mh "c", Left PartNotFetchedYet)
                             ] }
          out = shown (showDoc sw)
      out `shouldSatisfy` isInfixOf "1024 bytes here"
      out `shouldSatisfy` isInfixOf "7 bytes not fetched yet"
      out `shouldSatisfy` isInfixOf "has not been fetched"

    it "says whether canon already holds the letter, when it was asked" $ do
      repo <- aKey ; author <- aKey
      let ac = AOpen repo HubIssue "t" [] Nothing Nothing Nothing 5
          with f = shown (showDoc ((letter author ac FoldsToCanon) { swFolded = f }))
      with (Just True) `shouldSatisfy` isInfixOf "already folded"
      with (Just False) `shouldSatisfy` isInfixOf "not folded"
      with Nothing `shouldSatisfy` not . isInfixOf "canon"

  describe "PEP-22 hub inbox show: what it says about itself" $ do

    it "says nothing when the repository was named" $ do
      repo <- aKey ; author <- aKey
      let ac = AOpen repo HubIssue "t" [] Nothing Nothing Nothing 5
      shown (showNotes True (letter author ac FoldsToCanon)) `shouldBe` ""

    -- Canon is the half that is not conditional: a folded letter and an
    -- unfolded one look alike from the mailbox, and accepting the folded one
    -- again is the mistake the answer prevents.
    it "warns that canon was not consulted, whatever the letter asks for" $ do
      repo <- aKey ; author <- aKey
      let ac = AOpen repo HubIssue "t" [] Nothing Nothing Nothing 5
          notes d = shown (showNotes False (letter author ac d))
      notes FoldsToCanon `shouldSatisfy` isInfixOf "canon was not consulted"
      notes OwnerNative `shouldSatisfy` isInfixOf "canon was not consulted"

    -- The deny-list half IS conditional, like the queue's: it qualifies the
    -- line "triage: would fold", and against a letter nothing would fold anyway
    -- it warns about a sentence that is not on screen.
    it "warns about the deny-list only when something would fold" $ do
      repo <- aKey ; author <- aKey
      let ac = AOpen repo HubIssue "t" [] Nothing Nothing Nothing 5
          notes d = shown (showNotes False (letter author ac d))
      notes FoldsToCanon `shouldSatisfy` isInfixOf "no deny-list was applied"
      notes RequestOnly `shouldSatisfy` not . isInfixOf "no deny-list was applied"
      notes OwnerNative `shouldSatisfy` not . isInfixOf "no deny-list was applied"

  describe "PEP-22 hub inbox show: arguments" $ do

    it "reads the flags in any order" $ do
      mbox <- aKey
      let h = mh "a message"
          want = Just (ShowArgs mbox h Nothing False)
      showArgs (argv ["--mailbox", b58 mbox, "--message", show (pretty h)])
        `shouldBe` want
      showArgs (argv ["--message", show (pretty h), "--mailbox", b58 mbox])
        `shouldBe` want

    it "takes the repository when one is named" $ do
      mbox <- aKey ; repo <- aKey
      let h = mh "a message"
      showArgs (argv [ "--mailbox", b58 mbox, "--message", show (pretty h)
                     , "--repo", b58 repo ])
        `shouldBe` Just (ShowArgs mbox h (Just repo) False)

    it "refuses a form with no mailbox or no message" $ do
      mbox <- aKey
      let h = mh "a message"
      showArgs (argv ["--message", show (pretty h)]) `shouldBe` Nothing
      showArgs (argv ["--mailbox", b58 mbox]) `shouldBe` Nothing

    -- Nothing positional, for the reason every verb here has: a key and a hash
    -- are both thirty-two bytes of base58, so position cannot tell them apart.
    it "refuses a positional value and an unknown flag" $ do
      mbox <- aKey
      let h = mh "a message"
      showArgs (argv [b58 mbox, show (pretty h)]) `shouldBe` Nothing
      showArgs (argv [ "--mailbox", b58 mbox, "--message", show (pretty h)
                     , "--verbose" ])
        `shouldBe` Nothing

-- | Every op, through the renderer, with a stranger's bytes in every field.
--
-- WHY A TABLE. 'contentDoc' dispatches on all ten constructors of
-- 'AuthorContent' and one test passed 'AOpen'. The other nine were rendered by
-- nothing -- on the verb that prints a stranger's UNFOLDED letter, which is the
-- surface that produced 054d4dd2 ("sanitise the status too") and is exactly
-- what a maintainer reads while deciding whether to sign it into canon forever.
--
-- One escape in one field of one op is enough to rewrite the lines above it in
-- that maintainer's terminal, and the arm carrying it is chosen by the
-- attacker: they write the letter.
sanitised :: Spec
sanitised =
  describe "PEP-22 show: a stranger's bytes in every field of every op" $ do

    it "puts no escape on the terminal, whichever op the letter carries" $ do
      repo <- aKey ; author <- aKey ; other <- aKey
      let esc = "erase\ESC[2Kme"
          thr = mh "thread"
          coords = PRCoords (Just esc) esc esc esc esc Nothing
          ops =
            [ ("open",     AOpen repo HubIssue esc [esc,esc] (Just esc) Nothing Nothing 1)
            , ("open/pr",  AOpen repo HubPR esc [] (Just esc) Nothing (Just coords) 1)
            , ("comment",  AComment thr (Just (mh "r")) (Just esc) Nothing 1)
            , ("revise",   ARevise thr coords 1)
            , ("set",      ASet thr esc esc 1)
            , ("close",    AClose thr (Just esc) 1)
            , ("reopen",   AReopen thr (Just esc) 1)
            , ("merge",    AMerge thr esc esc 1)
            , ("redact",   ARedact repo (mh "e") 1)
            , ("delegate", ADelegate repo other 1)
            , ("revoke",   ARevoke repo other 1)
            ]

      -- Named in the failure, or a red table says only that one of eleven
      -- rows is wrong.
      for_ ops $ \(name, ac) -> do
        let out = shown (showDoc (letter author ac FoldsToCanon))
        (name, "\ESC" `isInfixOf` out) `shouldBe` (name, False)

    -- The escape is ESCAPED, not dropped: what a stranger wrote is what a
    -- maintainer has to be able to read before deciding about it.
    it "keeps what the escape was made of" $ do
      repo <- aKey ; author <- aKey
      let ac = ASet (mh "thread") "attr" "erase\ESC[2Kme" 1
          out = shown (showDoc (letter author ac FoldsToCanon))
      out `shouldSatisfy` isInfixOf "erase"
      out `shouldSatisfy` isInfixOf "me"

    -- One line per line of the report, whatever a field holds. A newline in an
    -- attribute value forges a field that was never in the letter.
    it "cannot be made to forge a field of the report" $ do
      repo <- aKey ; author <- aKey
      let ac = ASet (mh "thread") "attr" "real\ntriage: would fold" 1
          out = shown (showDoc (letter author ac FoldsToCanon))
      out `shouldSatisfy` (not . isInfixOf "\ntriage: would fold")

-- | @hub inbox show --json@: the letter as a document a program decides on.
--
-- The point of the document is the verdict, so most of these are about which
-- verdict a letter gets and that the reason travels with it as a code a
-- program can branch on rather than a sentence it has to match.
asJson :: Spec
asJson = describe "PEP-22 hub inbox show --json: the letter as a document" $ do

  let doc = letterContract
      verdict sw = at ["triage","verdict"] (doc sw)
      code sw = at ["triage","reason","code"] (doc sw)
      -- Canon consulted, holding the given threads.
      asked repo ths sw = sw { swRepo = Just repo, swFolded = Just False
                             , swCanonThreads = HM.fromList ths }

  it "says which document it is, and under which contract" $ do
    repo <- aKey ; author <- aKey
    let v = doc (letter author (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 5) FoldsToCanon)
    at ["contract"] v `shouldBe` Just (Number 1)
    at ["document"] v `shouldBe` Just (String "letter")
    at ["message"] v `shouldBe` Just (str (show (pretty (mh "m"))))
    at ["envelope"] v `shouldBe` Just (str (b58 author))
    at ["event"] v `shouldBe` Just (str (show (pretty (mh "e"))))

  -- WHOLE AND UNESCAPED. The terminal report bounds a body by lines and
  -- escapes it; a program deciding whether to accept has to see exactly what
  -- it would accept, and JSON carries the escape as \u001b on its own.
  it "carries the whole body, byte for byte" $ do
    repo <- aKey ; author <- aKey
    let body = Text.unlines (replicate (maxBodyLines + 50) "line\ESC[2K")
        ac = AOpen repo HubIssue "it crashes" ["bug"] (Just body) Nothing Nothing 5
        v = doc (letter author ac FoldsToCanon)
    at ["body"] v `shouldBe` Just (String body)
    at ["subject","title"] v `shouldBe` Just (String "it crashes")
    at ["subject","kind"] v `shouldBe` Just (String "issue")
    at ["subject","target"] v `shouldBe` Just (str (b58 repo))
    at ["labels_requested"] v `shouldBe` Just (toJSON ["bug" :: Text.Text])
    at ["op"] v `shouldBe` Just (String "open")
    at ["author"] v `shouldBe` Just (str (b58 author))
    LBS.unpack (renderBytes v) `shouldSatisfy` not . isInfixOf "\ESC"

  -- Every key, whatever the op: a consumer tells "no body" from "a renderer
  -- that forgot the body" by reading.
  it "has the same keys whichever op the letter carries" $ do
    repo <- aKey ; author <- aKey ; other <- aKey
    let thr = mh "thread"
        coords = PRCoords Nothing "r" "tip" "main" "base" Nothing
        ops = [ AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1
              , AComment thr Nothing (Just "b") Nothing 1
              , ARevise thr coords 1, ASet thr "a" "v" 1
              , AClose thr Nothing 1, AReopen thr Nothing 1
              , AMerge thr "c" "i" 1, ARedact repo (mh "e") 1
              , ADelegate repo other 1, ARevoke repo other 1 ]
        keysOf v = case v of { Object o -> Just (List.sort (KM.keys o)) ; _ -> Nothing }
        unreadable = keysOf (doc (Shown (LetterView (mh "m") Nothing (Left NotForUs) Nothing [])
                                        [] 0 Nothing Nothing mempty))
    for_ ops $ \ac ->
      keysOf (doc (letter author ac FoldsToCanon)) `shouldBe` unreadable

  it "names a comment's thread, and its number once canon is asked" $ do
    repo <- aKey ; author <- aKey
    let thr = mh "thread"
        sw = letter author (AComment thr (Just (mh "r")) (Just "+1") Nothing 5) FoldsToCanon
    at ["subject","thread"] (doc sw) `shouldBe` Just (str (show (pretty thr)))
    at ["subject","number"] (doc sw) `shouldBe` Just Null
    at ["subject","in_canon"] (doc sw) `shouldBe` Just Null
    at ["reply_to"] (doc sw) `shouldBe` Just (str (show (pretty (mh "r"))))
    let asked' = asked repo [(thr, Just 7)] sw
    at ["subject","number"] (doc asked') `shouldBe` Just (Number 7)
    at ["subject","in_canon"] (doc asked') `shouldBe` Just (Bool True)
    verdict asked' `shouldBe` Just "would_fold"
    at ["triage","reason"] (doc asked') `shouldBe` Just Null

  -- The thread an open starts is its own event-id.
  it "names the thread an open would start" $ do
    repo <- aKey ; author <- aKey
    let sw = letter author (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 5) FoldsToCanon
    at ["subject","thread"] (doc sw) `shouldBe` Just (str (show (pretty (mh "e"))))

  describe "the verdict" $ do

    it "would fold, with nothing asked, and says nothing was asked" $ do
      repo <- aKey ; author <- aKey
      let sw = letter author (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 5) FoldsToCanon
      verdict sw `shouldBe` Just "would_fold"
      at ["triage","disposition"] (doc sw) `shouldBe` Just "folds"
      at ["triage","deny_list_applied"] (doc sw) `shouldBe` Just (Bool False)
      at ["triage","canon_consulted"] (doc sw) `shouldBe` Just (Bool False)
      at ["folded"] (doc sw) `shouldBe` Just Null
      at ["repo"] (doc sw) `shouldBe` Just Null

    it "is already_folded when canon holds the letter" $ do
      repo <- aKey ; author <- aKey
      let sw = (asked repo [] (letter author (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 5)
                                              FoldsToCanon)) { swFolded = Just True }
      verdict sw `shouldBe` Just "already_folded"
      code sw `shouldBe` Just "already_in_canon"
      at ["folded"] (doc sw) `shouldBe` Just (Bool True)

    it "refuses a request and an owner-only op, with a code each" $ do
      author <- aKey ; repo <- aKey
      let thr = mh "thread"
          req = letter author (AClose thr Nothing 5) RequestOnly
          own = letter author (ARedact repo (mh "x") 5) OwnerNative
      (verdict req, code req) `shouldBe` (Just "refused", Just "request_only")
      (verdict own, code own) `shouldBe` (Just "refused", Just "owner_native")

    it "refuses an open written for another repository, once one is named" $ do
      repo <- aKey ; elsewhere <- aKey ; author <- aKey
      let sw = letter author (AOpen elsewhere HubIssue "t" [] Nothing Nothing Nothing 5) FoldsToCanon
      verdict sw `shouldBe` Just "would_fold"
      code (asked repo [] sw) `shouldBe` Just "wrong_repo"

    it "refuses a reply to a thread canon does not hold, once canon is asked" $ do
      repo <- aKey ; author <- aKey
      let sw = letter author (AComment (mh "nowhere") Nothing (Just "b") Nothing 5) FoldsToCanon
      verdict sw `shouldBe` Just "would_fold"
      code (asked repo [(mh "elsewhere", Just 1)] sw) `shouldBe` Just "unknown_thread"
      at ["subject","in_canon"] (doc (asked repo [] sw)) `shouldBe` Just (Bool False)

    it "is pending while an attachment is not here, and says which" $ do
      repo <- aKey ; author <- aKey
      let ac = AOpen repo HubIssue "t" [] Nothing Nothing Nothing 5
          sw = (letter author ac FoldsToCanon)
                 { swParts = [ (mh "a", Right (PartFacts 1024 True))
                             , (mh "b", Right (PartFacts 7 False))
                             , (mh "c", Left PartNotFetchedYet) ] }
          Just (Array ps) = at ["attachments"] (doc sw)
      verdict sw `shouldBe` Just "pending"
      code sw `shouldBe` Just "part_not_fetched"
      [ (at ["size"] p, at ["here"] p, at ["trouble","code"] p) | p <- toList ps ]
        `shouldBe` [ (Just (Number 1024), Just (Bool True), Nothing)
                   , (Just (Number 7), Just (Bool False), Nothing)
                   , (Just Null, Just (Bool False), Just "not_fetched") ]

    it "refuses a letter naming more parts than an accept walks" $ do
      repo <- aKey ; author <- aKey
      let ac = AOpen repo HubIssue "t" [] Nothing Nothing Nothing 5
          sw = (letter author ac FoldsToCanon) { swPartsUnmeasured = 3 }
      code sw `shouldBe` Just "too_many_parts"
      at ["attachments_unmeasured"] (doc sw) `shouldBe` Just (Number 3)

    it "is unreadable with the reason as a code, and keeps the envelope" $ do
      author <- aKey
      let sw = Shown (LetterView (mh "m") (Just author) (Left NotForUs) Nothing [])
                     [] 0 Nothing Nothing mempty
      verdict sw `shouldBe` Just "unreadable"
      code sw `shouldBe` Just "not_for_us"
      at ["unreadable","code"] (doc sw) `shouldBe` Just "not_for_us"
      at ["envelope"] (doc sw) `shouldBe` Just (str (b58 author))
      at ["author"] (doc sw) `shouldBe` Just Null
      at ["triage","disposition"] (doc sw) `shouldBe` Just Null

    -- A ban arrives as a letter that did not open, and is a refusal rather
    -- than a fault: the letter is fine, its author is not welcome.
    it "refuses a deny-listed author rather than calling the letter unreadable" $ do
      author <- aKey
      let sw = Shown (LetterView (mh "m") (Just author) (Left (BadLetterHere AuthorDenied))
                                 Nothing []) [] 0 Nothing Nothing mempty
      (verdict sw, code sw) `shouldBe` (Just "refused", Just "author_denied")

  describe "arguments" $
    it "reads --json" $ do
      mbox <- aKey
      let h = mh "a message"
      showArgs (argv ["--mailbox", b58 mbox, "--message", show (pretty h), "--json"])
        `shouldBe` Just (ShowArgs mbox h Nothing True)
      showArgs (argv ["--mailbox", b58 mbox, "--message", show (pretty h), "--json", "yes"])
        `shouldBe` Nothing

-- A path into a document.
at :: [Text.Text] -> Value -> Maybe Value
at [] v = Just v
at (k:ks) (Object o) = KM.lookup (Key.fromText k) o >>= at ks
at _ _ = Nothing

str :: String -> Value
str = String . Text.pack

renderBytes :: Value -> LBS.ByteString
renderBytes = renderDocument letterOrder
