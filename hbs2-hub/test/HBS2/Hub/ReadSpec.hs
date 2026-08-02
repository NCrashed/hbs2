-- | The read verbs' decisions (PEP-22 "Read").
--
-- Everything these print is a function of one 'FoldResult', so all of it can
-- be asked about here: what is listed, in what order, what is withheld, and
-- what a filter narrows to. The verbs around them are argument handling and an
-- exit code.
module HBS2.Hub.ReadSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Hub.CLI.Read

import HBS2.Net.Auth.Credentials

import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import Prettyprinter (Doc,pretty)
import Test.Hspec

type KP = (HubKey, PrivKey 'Sign HubScheme)

kp :: IO KP
kp = do
  c <- newCredentials @'HBS2Basic
  pure (_peerSignPk c, _peerSignSk c)

canonOf :: RepoRef -> Word64 -> Maybe Word64 -> EventId -> CanonContent
canonOf repo sq num eid = CanonContent repo eid sq num Nothing Nothing sq Nothing

-- A fold built the way canon is: events, in order, through the real thing.
-- A ThreadState assembled by hand would be a fixture of this test's opinion
-- about the fold rather than of the fold.
foldOf :: KP -> [(Word64, Maybe Word64, AuthorContent)] -> FoldResult
foldOf owner es =
  foldEvents (fst owner)
    [ mkEvent owner owner c (canonOf (fst owner) sq n) | (sq, n, c) <- es ]

anIssue :: RepoRef -> Text -> AuthorContent
anIssue repo t = AOpen repo HubIssue t [] (Just "body") Nothing Nothing 1000

rendered :: [Doc ann] -> String
rendered = unlines . fmap show

spec :: Spec
spec = do

  describe "PEP-22 read: what is listed" $ do

    it "lists by number and not in hash order" $ do
      owner <- kp
      let repo = fst owner
          fr = foldOf owner [ (n, Just n, anIssue repo (Text.pack (show n)))
                            | n <- [1 .. 12] ]
      fmap tsNumber (threadsOf HubIssue noFilter fr)
        `shouldBe` fmap Just [1 .. 12]

    it "keeps the two kinds apart" $ do
      owner <- kp
      let repo = fst owner
          pr = AOpen repo HubPR "a pr" [] Nothing Nothing
                 (Just (PRCoords (Just "hbs23://x") "refs/heads/t"
                                 "tip" "onto" "base" Nothing)) 1000
          fr = foldOf owner [ (1, Just 1, anIssue repo "an issue")
                            , (2, Just 2, pr) ]
      fmap tsNumber (threadsOf HubIssue noFilter fr) `shouldBe` [Just 1]
      fmap tsNumber (threadsOf HubPR noFilter fr) `shouldBe` [Just 2]

    it "narrows by status, and an unclosed thread is open" $ do
      owner <- kp
      let repo = fst owner
          e1 = mkEvent owner owner (anIssue repo "one") (canonOf repo 1 (Just 1))
          fr = foldEvents repo
                 [ e1
                 , mkEvent owner owner (anIssue repo "two") (canonOf repo 2 (Just 2))
                 , mkEvent owner owner (AClose (eventId e1) Nothing 3000)
                     (canonOf repo 3 Nothing) ]
      fmap tsNumber (threadsOf HubIssue (Filter (Just "open") Nothing) fr)
        `shouldBe` [Just 2]
      fmap tsNumber (threadsOf HubIssue (Filter (Just "closed") Nothing) fr)
        `shouldBe` [Just 1]

    -- A label an owner applied is not a label an author asked for. PEP-19 makes
    -- applying one an owner-signed event, so counting the request would let a
    -- stranger label their own issue and have it show up in a filtered list.
    it "filters on the labels an owner applied, not the ones asked for" $ do
      owner <- kp
      let repo = fst owner
          asked = AOpen repo HubIssue "asked" ["bug"] Nothing Nothing Nothing 1000
          e1 = mkEvent owner owner asked (canonOf repo 1 (Just 1))
          e2 = mkEvent owner owner (anIssue repo "applied") (canonOf repo 2 (Just 2))
          fr = foldEvents repo
                 [ e1, e2
                 , mkEvent owner owner
                     (ASet (eventId e2) "labels" (encodeLabels ["bug"]) 3000)
                     (canonOf repo 3 Nothing) ]
      -- the one that ASKED for the label carries it as a request only
      fmap tsLabelsRequested (threadsOf HubIssue noFilter fr)
        `shouldBe` [["bug"], []]
      fmap labelsOf (threadsOf HubIssue noFilter fr)
        `shouldBe` [[], ["bug"]]
      fmap tsNumber (threadsOf HubIssue (Filter Nothing (Just "bug")) fr)
        `shouldBe` [Just 2]

  describe "PEP-22 read: what is printed" $ do

    -- A title arrives in a letter anybody may send and is printed to a
    -- terminal. One row per thread has to survive a title that says otherwise.
    it "cannot be made to forge a second row of the listing" $ do
      owner <- kp
      let repo = fst owner
          nasty = "real\n#2     open   forged"
          fr = foldOf owner [(1, Just 1, anIssue repo nasty)]
          out = rendered (listDoc (threadsOf HubIssue noFilter fr))
      length (lines out) `shouldBe` 1
      out `shouldSatisfy` (not . ("forged" `isInfixOf`) . drop 0 . unlines . drop 1 . lines)

    it "does not put an escape sequence on the terminal" $ do
      owner <- kp
      let repo = fst owner
          fr = foldOf owner [(1, Just 1, anIssue repo "erase\ESC[2Kme")]
          out = rendered (listDoc (threadsOf HubIssue noFilter fr))
      out `shouldSatisfy` (not . ("\ESC" `isInfixOf`))

    it "shows a thread with its comment" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          e1 = mkEvent alice owner (anIssue repo "an issue") (canonOf repo 1 (Just 1))
          fr = foldEvents repo
                 [ e1
                 , mkEvent alice owner
                     (AComment (eventId e1) Nothing (Just "a reply") Nothing 2000)
                     (canonOf repo 2 Nothing) ]
          [t] = threadsOf HubIssue noFilter fr
          out = rendered (showDoc t)
      out `shouldSatisfy` ("an issue" `isInfixOf`)
      out `shouldSatisfy` ("a reply" `isInfixOf`)
      -- the author is named, which the list line leaves out
      out `shouldSatisfy` ("author" `isInfixOf`)

    -- A redaction is display-level (PEP-19): the event stays in canon and every
    -- clone still holds the bytes. What must not happen is this printing them.
    it "withholds a redacted body" $ do
      owner <- kp
      let repo = fst owner
          e1 = mkEvent owner owner
                 (AOpen repo HubIssue "a title" [] (Just "the secret") Nothing Nothing 1000)
                 (canonOf repo 1 (Just 1))
          fr = foldEvents repo
                 [ e1
                 , mkEvent owner owner (ARedact repo (eventId e1) 2000)
                     (canonOf repo 2 Nothing) ]
          [t] = threadsOf HubIssue noFilter fr
          out = rendered (showDoc t)
      out `shouldSatisfy` (not . ("the secret" `isInfixOf`))
      out `shouldSatisfy` ("redacted" `isInfixOf`)

    it "prints the log in seq order" $ do
      owner <- kp
      let repo = fst owner
          e1 = mkEvent owner owner (anIssue repo "one") (canonOf repo 1 (Just 1))
          fr = foldEvents repo
                 [ mkEvent owner owner (AComment (eventId e1) Nothing (Just "c") Nothing 2000)
                     (canonOf repo 2 Nothing)
                 , e1 ]
          out = lines (rendered (logDoc Nothing fr))
      fmap (take 6) out `shouldBe` ["1     ", "2     "]
      out !! 0 `shouldSatisfy` ("open" `isInfixOf`)
      out !! 1 `shouldSatisfy` ("comment" `isInfixOf`)
