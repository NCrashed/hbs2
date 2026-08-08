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
import HBS2.Hub.Render (threadContract,renderContract)
import HBS2.Hub.Repo (threadsNumbered,numberIndexOf)
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..))

import Data.Config.Suckless (C)
import Data.List (isInfixOf)
import Data.List qualified as List
import Data.Maybe (isJust)
import Data.String (fromString)
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

  -- WHAT MAY BE HANDED TO GIT. The coordinates come out of a stranger's signed
  -- box, are bounded by size and by nothing else, and reach canon unverified on
  -- the fork path by design. Joined into one argv word as a range they were an
  -- option, and `git diff --output=<path>` truncates a file and exits 0, so the
  -- contract then reported the diff as available and empty.
  describe "PEP-22 read: the coordinates a diff may be asked for" $ do

    let coords b t = PRCoords Nothing "refs/heads/t" t "refs/heads/master" b Nothing
        sha c = Text.replicate 40 c

    it "asks for two object names, separated from any path" $
      diffArgv (coords (sha "a") (sha "b"))
        `shouldBe` Just [ "diff", "--no-color"
                        , Text.unpack (sha "a"), Text.unpack (sha "b"), "--" ]

    -- The exploit, as a shape: nothing git could read as an option, and no
    -- element holding the range separator the old spelling supplied for free.
    it "keeps the two coordinates apart, and never joins them into a range" $ do
      let Just as = diffArgv (coords (sha "a") (sha "b"))
          -- Everything this build chose is a literal; what came out of canon is
          -- the rest, and that is what must not be read as an option.
          ours = ["diff","--no-color","--"]
          theirs = filter (`notElem` ours) as
      theirs `shouldBe` [Text.unpack (sha "a"), Text.unpack (sha "b")]
      theirs `shouldSatisfy` all (\a -> take 1 a /= "-")
      -- The separator the old spelling supplied for free, and which turned two
      -- harmless-looking halves into one traversal.
      as `shouldSatisfy` all (not . isInfixOf "..")
      -- And the terminator is there, so neither can be taken for a path.
      last as `shouldBe` "--"

    it "refuses a base that is an option" $
      diffArgv (coords "--output=sub/" (sha "b")) `shouldBe` Nothing

    it "refuses a tip that is a path" $
      diffArgv (coords (sha "a") "/victim.txt") `shouldBe` Nothing

    -- The abbreviated sha a person pastes is not an object name either. It is
    -- refused rather than passed, and the caller reports the diff unavailable.
    it "refuses anything that is not the length of a hash" $ do
      diffArgv (coords "abc1234" (sha "b")) `shouldBe` Nothing
      diffArgv (coords (sha "a") "") `shouldBe` Nothing

  -- PLURAL, and the name is the whole of the finding. This verb wrote
  -- "assignee" and read it back, so the terminal was right and nothing else in
  -- the package knew the name: `multiValued` lists the plural, so no value was
  -- ever normalized and `hub verify` raised no UnnormalizedAttr, and the PEP-22
  -- contract reads the plural, so every thread the shipped verb assigned came
  -- out of --json as unassigned. PEP-19 settles the spelling and says why: an
  -- attribute that can hold a set is spelled as one everywhere.
  describe "PEP-22 read: who a thread is assigned to" $ do

    it "shows the key an owner set, and nothing before that" $ do
      owner <- kp ; bob <- kp
      let repo = fst owner
          key = Text.pack (show (pretty (AsBase58 (fst bob))))
          e1 = mkEvent owner owner (anIssue repo "one") (canonOf repo 1 (Just 1))
          before = foldEvents repo [e1]
          after = foldEvents repo
                    [ e1
                    , mkEvent owner owner
                        (ASet (eventId e1) "assignees" (encodeLabels [key]) 3000)
                        (canonOf repo 2 Nothing) ]
      fmap assigneesOf (threadsOf HubIssue noFilter before) `shouldBe` [[]]
      fmap assigneesOf (threadsOf HubIssue noFilter after) `shouldBe` [[key]]

    -- Last-writer-wins like every attribute, so unassigning is the empty set
    -- and not a removal: there is no op that takes an attribute away.
    it "reads an empty value as assigned to nobody a reader should print" $ do
      owner <- kp ; bob <- kp
      let repo = fst owner
          key = Text.pack (show (pretty (AsBase58 (fst bob))))
          e1 = mkEvent owner owner (anIssue repo "one") (canonOf repo 1 (Just 1))
          fr = foldEvents repo
                 [ e1
                 , mkEvent owner owner
                     (ASet (eventId e1) "assignees" (encodeLabels [key]) 3000)
                     (canonOf repo 2 Nothing)
                 , mkEvent owner owner
                     (ASet (eventId e1) "assignees" (encodeLabels []) 4000)
                     (canonOf repo 3 Nothing) ]
      fmap assigneesOf (threadsOf HubIssue noFilter fr) `shouldBe` [[]]
      -- And the report says nothing about it: an empty line reading
      -- "assignees" is worse than no line.
      unwords (fmap show (showDoc (head (threadsOf HubIssue noFilter fr))))
        `shouldSatisfy` not . isInfixOf "assignees"

    -- THE CROSSING TEST, which is what nothing did: the name the shipped verb
    -- writes, read by the PEP-22 contract. Both sides invented their own
    -- spelling, each was self-consistent, each had a test, and both were green
    -- -- so this one goes through 'attrAssignees', the constant `issue assign`
    -- writes with, rather than through a literal of its own. A test that spells
    -- the name itself is the shape that let this happen.
    it "writes the name the render contract reads" $ do
      owner <- kp ; bob <- kp
      let repo = fst owner
          key = Text.pack (show (pretty (AsBase58 (fst bob))))
          e1 = mkEvent owner owner (anIssue repo "one") (canonOf repo 1 (Just 1))
          fr = foldEvents repo
                 [ e1
                 , mkEvent owner owner
                     (ASet (eventId e1) attrAssignees (encodeLabels [key]) 3000)
                     (canonOf repo 2 Nothing) ]
          t = head (threadsOf HubIssue noFilter fr)
          json = show (renderContract (threadContract t Nothing))
      json `shouldSatisfy` isInfixOf (Text.unpack key)
      -- And the terminal reader agrees with both.
      assigneesOf t `shouldBe` [key]

    -- The half that made the drift invisible: an attribute the writer spells
    -- one way and 'multiValued' another is never normalized, so `hub verify`
    -- has nothing to report about it either.
    it "is a name the canonicalizer knows" $ do
      attrAssignees `shouldSatisfy` (`elem` multiValued)
      attrLabels `shouldSatisfy` (`elem` multiValued)

  -- A NUMBER THAT NAMES TWO THREADS. `DupNumber` is an anomaly the fold reports
  -- and does not drop, so canon can hold both -- two maintainers minting from
  -- one view is the case PEP-19 leaves open. Every resolver took the head of an
  -- unordered traversal, which for a writer means an owner-signed event against
  -- whichever the HAMT yielded first, silently, into append-only canon.
  describe "PEP-22 read: resolving a number canon gives twice" $ do

    let twoAt n owner =
          let repo = fst owner
              a = mkEvent owner owner (anIssue repo "one") (canonOf repo 1 (Just n))
              b = mkEvent owner owner (anIssue repo "two") (canonOf repo 2 (Just n))
          in (foldEvents repo [a, b], eventId a, eventId b)

    it "answers with every thread that carries the number" $ do
      owner <- kp
      let (fr, a, b) = twoAt 42 owner
      length (threadsNumbered 42 fr) `shouldBe` 2
      threadsNumbered 42 fr `shouldSatisfy` (\ts -> a `elem` ts && b `elem` ts)

    it "answers with one when canon holds one, and none when it holds none" $ do
      owner <- kp
      let repo = fst owner
          e1 = mkEvent owner owner (anIssue repo "one") (canonOf repo 1 (Just 7))
          fr = foldEvents repo [e1]
      threadsNumbered 7 fr `shouldBe` [eventId e1]
      threadsNumbered 8 fr `shouldBe` []

    -- And the order is the index's, not the HashMap's: two clones folding one
    -- canon wrote different index/number.sexp bytes, and so different tree and
    -- commit ids, because `sortOn` is stable and the tie fell through to hash
    -- order.
    --
    -- FIVE AND NOT TWO. The keys are random, so under the old sort the hash
    -- order coincides with the sorted one about half the time with two threads
    -- -- a guard that catches a regression on a coin flip is not a guard. With
    -- five it is one permutation in 120.
    it "orders a tie by thread-id, so the index is a function of canon" $ do
      owner <- kp
      let repo = fst owner
          fr = foldEvents repo
                 [ mkEvent owner owner (anIssue repo (Text.pack (show i)))
                           (canonOf repo i (Just 42))
                 | i <- [1 .. 5] ]
          ids = threadsNumbered 42 fr
      length ids `shouldBe` 5
      ids `shouldBe` List.sort ids
      fmap snd (numberIndexOf fr) `shouldBe` ids

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
      -- The newline is ESCAPED, not dropped, and asserting that is the point.
      --
      -- What stood here was `not . isInfixOf "forged" . unlines . drop 1 . lines`,
      -- which the line above had already made vacuous: with one line, `drop 1`
      -- is empty and the predicate is constantly True. So the only real
      -- assertion was the row count -- and a renderer that CLIPPED the title at
      -- the newline would pass it while quietly throwing away what a stranger
      -- wrote, which is a different bug and not one this test would have shown.
      out `shouldSatisfy` ("forged" `isInfixOf`)
      out `shouldSatisfy` (not . ("\n#2" `isInfixOf`))

    it "does not put an escape sequence on the terminal" $ do
      owner <- kp
      let repo = fst owner
          fr = foldOf owner [(1, Just 1, anIssue repo "erase\ESC[2Kme")]
          out = rendered (listDoc (threadsOf HubIssue noFilter fr))
      out `shouldSatisfy` (not . ("\ESC" `isInfixOf`))

    -- The test above names the LINE and populated one field of it. The status
    -- sits on the same line and is the same kind of thing -- an attribute value
    -- out of a signed event, four kilobytes of anything -- and it was printed
    -- with a bare pretty while the title beside it was sanitised. Reading a
    -- stranger's canon is what a clone does, so the owner of the repository
    -- being read is not a trusted party here.
    it "does not put one on it through the status either" $ do
      owner <- kp
      let repo = fst owner
          e1 = mkEvent owner owner (anIssue repo "a title") (canonOf repo 1 (Just 1))
          fr = foldEvents repo
                 [ e1
                 , mkEvent owner owner
                     (ASet (eventId e1) "status" "\ESC[2K\ESC[1Aopen" 2000)
                     (canonOf repo 2 Nothing) ]
          [t] = threadsOf HubIssue noFilter fr
      -- The fold really did take it: this is a test about printing, and it
      -- would pass vacuously if the value never arrived.
      statusOf t `shouldSatisfy` ("\ESC" `isInfixOf`) . Text.unpack
      rendered (listDoc [t]) `shouldSatisfy` (not . ("\ESC" `isInfixOf`))
      rendered (showDoc t)   `shouldSatisfy` (not . ("\ESC" `isInfixOf`))

    -- A HashRef takes any width off the wire ('validHashRef' exists for that),
    -- the fold carries reply-to through unvalidated, and base58 is quadratic:
    -- 2.5 s for 64 KiB, measured in this project. One admitted comment is
    -- enough, and `hub pr show` prints one line per comment.
    it "prints a hash-shaped field that is not a hash by its size" $ do
      owner <- kp
      let repo = fst owner
          wide = HashRef (fromString (replicate 40000 'z'))
          e1 = mkEvent owner owner (anIssue repo "a title") (canonOf repo 1 (Just 1))
          fr = foldEvents repo
                 [ e1
                 , mkEvent owner owner
                     (AComment (eventId e1) (Just wide) (Just "a reply") Nothing 2000)
                     (canonOf repo 2 Nothing) ]
          [t] = threadsOf HubIssue noFilter fr
          out = rendered (showDoc t)
      out `shouldSatisfy` ("not a hash" `isInfixOf`)
      -- and the forty thousand characters were never spent
      length out `shouldSatisfy` (< 4000)

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

    -- A filter that silently does not run is worse than one that refuses: the
    -- caller reads the output as filtered. Both of these came back as
    -- `Filter Nothing Nothing`, which is every issue in the tracker with a zero
    -- exit, from a command that named two labels or a numeric one.
    it "refuses a repeated filter rather than dropping it" $ do
      owner <- kp
      let repo = fst owner
          key = argvAtom (show (pretty (AsBase58 repo)))
          call ws = listArgs @C (key : fmap argvAtom ws)
      fmap snd (call []) `shouldBe` Just noFilter
      fmap snd (call ["--label","bug"]) `shouldBe` Just (Filter Nothing (Just "bug"))
      isJust (call ["--label","a","--label","b"]) `shouldBe` False
      isJust (call ["--status","open","--status","closed"]) `shouldBe` False

    -- argvAtom keeps a word that spells a number AS a number, so every
    -- StringLike pattern misses it. A label may be a year.
    it "takes a filter value that spells a number" $ do
      owner <- kp
      let repo = fst owner
          key = argvAtom (show (pretty (AsBase58 repo)))
      fmap snd (listArgs @C [key, argvAtom "--label", argvAtom "2026"])
        `shouldBe` Just (Filter Nothing (Just "2026"))

    -- THE ONE THE HAND-ROLLED READER LET THROUGH. Its `ok` looked at
    -- even-indexed words only, so this passed -- index 0 is a known flag and
    -- the length is even -- and the pairing then bound "--label" as the value
    -- of --status. A listing filtered on a status nobody typed, exit zero.
    it "refuses a flag standing where a value belongs" $ do
      owner <- kp
      let repo = fst owner
          key = argvAtom (show (pretty (AsBase58 repo)))
          call ws = listArgs @C (key : fmap argvAtom ws)
      isJust (call ["--status","--label"]) `shouldBe` False
      isJust (call ["--status"]) `shouldBe` False

    -- And a word nobody claimed. Answering it with an unfiltered listing would
    -- look exactly like a filter that ran.
    it "refuses a stray word and an unknown flag" $ do
      owner <- kp
      let repo = fst owner
          key = argvAtom (show (pretty (AsBase58 repo)))
          call ws = listArgs @C (key : fmap argvAtom ws)
      isJust (call ["--label","bug","extra"]) `shouldBe` False
      isJust (call ["--kind","issue"]) `shouldBe` False

    -- What `issueUsage` has been telling everybody is accepted, and what this
    -- verb refused: the spelling to use when a value begins with a dash.
    it "takes the equals spelling every other verb takes" $ do
      owner <- kp
      let repo = fst owner
          key = argvAtom (show (pretty (AsBase58 repo)))
      fmap snd (listArgs @C [key, argvAtom "--label=bug"])
        `shouldBe` Just (Filter Nothing (Just "bug"))
