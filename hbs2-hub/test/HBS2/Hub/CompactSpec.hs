-- | What a compaction may drop (PEP-19 "Compaction", PEP-21 policy).
--
-- The one rule in this package that REMOVES from append-only canon, so the
-- tests are written as the retain list is: one per reason, because each item
-- survives for a different reason and a rule that dropped any of them would
-- break a different property. The two that are not about size at all -- the
-- delegation events and the redact clause -- are the ones worth being loud
-- about, since dropping them changes what the fold ADMITS rather than what a
-- reader sees.
module HBS2.Hub.CompactSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Compact
import HBS2.Hub.CLI.Compact
import HBS2.Hub.Repo
import HBS2.Hub.Canon (renderMeta)
import HBS2.Hub.CLI.Argv (argvAtom)
import HBS2.Hub.Fold ( foldEvents,frThreads,frAdmitted,frOrigins,frMaintainers
                     , frMaxSeq,tsAttrs )

import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Prelude.Plated (pretty)
import HBS2.Hash (hashObject)

import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text.Encoding qualified as TextE
import Data.Config.Suckless (Syntax,C)
import Data.List (isInfixOf)
import Data.Text qualified as Text
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Data.Word (Word64)
import Test.Hspec

type KP = (HubKey, PrivKey 'Sign HubScheme)

kp :: IO KP
kp = do
  c <- newCredentials @'HBS2Basic
  pure (_peerSignPk c, _peerSignSk c)

canonOf :: RepoRef -> Word64 -> Maybe Word64 -> EventId -> CanonContent
canonOf repo sq num eid = CanonContent repo eid sq num Nothing Nothing sq Nothing

-- | The same, carrying the hash of the letter it folded.
--
-- Its own helper because that field is the difference between an event a
-- compaction may take and one it may not, and a fixture that set it inline
-- would bury the reason.
canonWith :: RepoRef -> Word64 -> Maybe HashRef -> EventId -> CanonContent
canonWith repo sq origin eid = CanonContent repo eid sq Nothing origin Nothing sq Nothing

-- | Every thread's attributes, which is what a reader sees and what a
-- compaction must not change.
attrsOf :: RepoRef -> [Event] -> HM.HashMap ThreadId (HM.HashMap Text Text)
attrsOf repo evs =
  HM.fromList [ (t, tsAttrs s) | (t, s) <- HM.toList (frThreads (foldEvents repo evs)) ]

-- One event at a seq, signed for real: 'compactionOf' resolves every box, so a
-- fixture built by hand would be testing a shape the rule never sees.
ev :: KP -> Word64 -> AuthorContent -> Event
ev owner sq c = mkEvent owner owner c (canonOf (fst owner) sq Nothing)

anOpen :: KP -> Word64 -> Text -> Event
anOpen owner sq t =
  mkEvent owner owner (AOpen (fst owner) HubIssue t [] (Just "body") Nothing Nothing 1000)
          (canonOf (fst owner) sq (Just sq))

argv :: [String] -> [Syntax C]
argv = fmap argvAtom

kept, dropped :: Compaction -> [EventId]
kept = fmap eventId . cpKeep
dropped = fmap eventId . cpDrop

-- | The plan over canon this owner's fold has seen.
--
-- The rule takes the fold, so every case here folds first; a test that passed
-- a bare list would be asking a question the rule no longer answers. That is
-- the point: the fold is what says which events are real, and the two cases
-- that turn on it are below.
planOf :: KP -> [Event] -> Compaction
planOf owner evs = compactionOf (foldEvents (fst owner) evs) evs

-- A canon tree as a list of files, which is what the reader wants and what
-- makes the classification testable without git. The same shape
-- "HBS2.Hub.VerifySpec" uses, kept local because a test fixture shared between
-- two specs is a third thing to keep in step.
byPath :: [(BS.ByteString, Text)] -> CanonSource IO
byPath files = CanonSource
  { csCommit  = pure (Right "deadbeef")
  , csClose   = pure ()
  , csEntries = const (pure (Right
      [ TreeEntry p (Blob (oidOf i) (BS.length (TextE.encodeUtf8 t)))
      | (i,(p,t)) <- zip [0 :: Int ..] files ]))
  , csBlob    = \oid -> pure (maybe (BlobRefused "no such object") BlobText
                                    (lookup oid byOid))
  }
  where
    oidOf i = Text.pack (show (i :: Int))
    byOid = [ (oidOf i, t) | (i,(_,t)) <- zip [0 :: Int ..] files ]

spec :: Spec
spec = do

  -- WHAT COMPACTION MAY NOT TOUCH BECAUSE IT CANNOT READ IT. `stEvents` is only
  -- the files that became an event; everything else is in `stBad`, and the
  -- writer commits the plan and nothing else -- no read-tree -- so a file this
  -- reader could not take was deleted by a compaction that reported only the
  -- events it dropped and exited 0. A shallow clone is the ordinary way in; a
  -- hostile upstream planting one such file, so that compacting launders it out
  -- of this lineage, is the other.
  describe "PEP-19 compaction: what the reader could not take" $ do

    it "refuses a canon holding a file it cannot read, and names it" $ do
      owner <- kp
      let repo = fst owner
          junk = "threads/t/not-an-event"
      st <- readCanon (byPath [("version", renderMeta), (junk, "nonsense")]) repo
              >>= either (fail . show) pure
      fmap fst (stBad st) `shouldBe` [junk]
      case compactable st of
        Left bad -> fmap fst bad `shouldBe` [junk]
        Right () -> expectationFailure "compactable admitted an unreadable canon"

    it "admits a canon whose every file read" $ do
      owner <- kp
      let repo = fst owner
      st <- readCanon (byPath [("version", renderMeta)]) repo
              >>= either (fail . show) pure
      stBad st `shouldBe` []
      compactable st `shouldBe` Right ()

    -- The refusal has to name the file, or the operator is told to run
    -- `hub verify` about a tree they were given no reason to doubt.
    it "says which file, and what to run to see the rest" $ do
      owner <- kp
      let repo = fst owner
          said = show (unreadableDoc repo [("threads/t/junk", FileUnreadable "no such object")])
      said `shouldSatisfy` isInfixOf "threads/t/junk"
      said `shouldSatisfy` isInfixOf "verify"
      said `shouldSatisfy` isInfixOf "Nothing was written"

  describe "PEP-19 compaction: what may go" $ do

    it "drops a set that a later set overwrote" $ do
      owner <- kp
      let o = anOpen owner 1 "one"
          thr = eventId o
          s1 = ev owner 2 (ASet thr "labels" "a" 2000)
          s2 = ev owner 3 (ASet thr "labels" "b" 3000)
          c = planOf owner [o, s1, s2]
      dropped c `shouldBe` [eventId s1]
      kept c `shouldBe` [eventId o, eventId s2]

    -- The winner is per (thread, attribute): two attributes on one thread do
    -- not overwrite each other, and neither do two threads.
    it "does not let one attribute overwrite another" $ do
      owner <- kp
      let o = anOpen owner 1 "one"
          thr = eventId o
          a = ev owner 2 (ASet thr "labels" "a" 2000)
          b = ev owner 3 (ASet thr "milestone" "m" 3000)
          c = planOf owner [o, a, b]
      dropped c `shouldBe` []

    it "does not let one thread overwrite another" $ do
      owner <- kp
      let o1 = anOpen owner 1 "one"
          o2 = anOpen owner 2 "two"
          a = ev owner 3 (ASet (eventId o1) "labels" "a" 3000)
          b = ev owner 4 (ASet (eventId o2) "labels" "b" 4000)
      dropped (planOf owner [o1, o2, a, b]) `shouldBe` []

    -- close and reopen write the status attribute without naming it, so they
    -- supersede a plain `set status` and are superseded by one. A rule that
    -- keyed on the op rather than the attribute would keep every one of them.
    it "treats close, reopen and set status as one attribute" $ do
      owner <- kp
      let o = anOpen owner 1 "one"
          thr = eventId o
          closed = ev owner 2 (AClose thr Nothing 2000)
          reopened = ev owner 3 (AReopen thr Nothing 3000)
          set = ev owner 4 (ASet thr "status" "closed" 4000)
          c = planOf owner [o, closed, reopened, set]
      dropped c `shouldBe` [eventId closed, eventId reopened]
      kept c `shouldBe` [eventId o, eventId set]

    it "keeps the order it was given, in both halves" $ do
      owner <- kp
      let o = anOpen owner 1 "one"
          thr = eventId o
          s1 = ev owner 2 (ASet thr "a" "1" 2000)
          s2 = ev owner 3 (ASet thr "a" "2" 3000)
          s3 = ev owner 4 (ASet thr "b" "1" 4000)
          s4 = ev owner 5 (ASet thr "b" "2" 5000)
          c = planOf owner [s4, s3, s2, s1, o]
      dropped c `shouldBe` [eventId s3, eventId s1]
      kept c `shouldBe` [eventId s4, eventId s2, eventId o]

  describe "PEP-19 compaction: what may not" $ do

    it "keeps every open, comment, revise and merge" $ do
      owner <- kp
      let o = anOpen owner 1 "one"
          thr = eventId o
          cm = ev owner 2 (AComment thr Nothing (Just "said") Nothing 2000)
          mg = ev owner 3 (AMerge thr "abc" "master" 3000)
          -- And a set that IS droppable, so the test proves the rule ran at
          -- all rather than that it dropped nothing.
          s1 = ev owner 4 (ASet thr "a" "1" 4000)
          s2 = ev owner 5 (ASet thr "a" "2" 5000)
          c = planOf owner [o, cm, mg, s1, s2]
      dropped c `shouldBe` [eventId s1]

    -- The one that is not about the reader at all: admission of every event
    -- depends on the maintainer set as of its seq, reconstructed from these.
    -- Dropping one changes which PAST events the fold admits.
    it "keeps every delegate and revoke, superseded or not" $ do
      owner <- kp ; bob <- kp
      let repo = fst owner
          d1 = ev owner 1 (ADelegate repo (fst bob) 1000)
          r1 = ev owner 2 (ARevoke repo (fst bob) 2000)
          d2 = ev owner 3 (ADelegate repo (fst bob) 3000)
      dropped (planOf owner [d1, r1, d2]) `shouldBe` []

    -- A note on a close is authored discussion even when the status it set was
    -- overwritten a minute later.
    it "keeps a close or reopen that carries a note" $ do
      owner <- kp
      let o = anOpen owner 1 "one"
          thr = eventId o
          noted = ev owner 2 (AClose thr (Just "duplicate of #3") 2000)
          later = ev owner 3 (AReopen thr Nothing 3000)
          set = ev owner 4 (ASet thr "status" "closed" 4000)
          c = planOf owner [o, noted, later, set]
      -- The bare reopen goes; the noted close stays despite being superseded
      -- twice over.
      dropped c `shouldBe` [eventId later]

    -- Dropping the target of a redact leaves the redact pointing at nothing:
    -- the fold treats it as an unknown target and drops it, the highest
    -- admitted seq falls, and the bridge reuses a seq already spent.
    it "keeps a superseded set that a redact names" $ do
      owner <- kp
      let o = anOpen owner 1 "one"
          thr = eventId o
          s1 = ev owner 2 (ASet thr "a" "secret" 2000)
          s2 = ev owner 3 (ASet thr "a" "public" 3000)
          rd = ev owner 4 (ARedact (fst owner) (eventId s1) 4000)
          c = planOf owner [o, s1, s2, rd]
      dropped c `shouldBe` []
      kept c `shouldSatisfy` (elem (eventId rd))

    -- Canon can hold two events at one seq: the fold reports it as an anomaly
    -- and orders them by canon-box hash. Neither supersedes the other, and a
    -- rule that dropped either would be choosing on a tie it has no opinion
    -- about.
    it "keeps both of a pair stamped with one seq" $ do
      owner <- kp
      let o = anOpen owner 1 "one"
          thr = eventId o
          a = ev owner 2 (ASet thr "a" "left" 2000)
          b = ev owner 2 (ASet thr "a" "right" 2001)
      dropped (planOf owner [o, a, b]) `shouldBe` []

    -- Not this build's to remove: an event whose boxes will not resolve is
    -- what `hub verify` reports, and a compaction that quietly took it away
    -- would edit the audit rather than bound the size.
    it "keeps an event it cannot resolve" $ do
      owner <- kp ; other <- kp
      let o = anOpen owner 1 "one"
          thr = eventId o
          -- Signed by one key and blessed under another's canon content, which
          -- is the id mismatch `resolve` refuses.
          broken = Event (evAuthorBox (ev owner 2 (ASet thr "a" "1" 2000)))
                         (evCanonBox (ev other 3 (ASet thr "a" "2" 3000)))
          s2 = ev owner 4 (ASet thr "a" "3" 4000)
      dropped (planOf owner [o, broken, s2]) `shouldBe` []

    -- The one the previous rule got wrong, and it is not a hostile case: a
    -- delegate minting from a view built before their revocation produces
    -- exactly this shape, which the fold documents as ordinary. `resolve`
    -- passes it -- two good signatures, matching id -- and only ADMISSION
    -- refuses it, so a rule asking `resolve` alone let it win.
    it "keeps a set the fold refused, though a later set overwrote it" $ do
      owner <- kp ; stranger <- kp
      let repo = fst owner
          o = anOpen owner 1 "one"
          thr = eventId o
          -- Blessed under a canon key this repository never authorized.
          refused = mkEvent stranger stranger (ASet thr "a" "1" 2000)
                            (canonOf repo 2 Nothing)
          s2 = ev owner 3 (ASet thr "a" "2" 3000)
      -- It is genuinely refused, or the case below proves nothing.
      HM.member (eventId refused) (frAdmitted (foldEvents repo [o, refused, s2]))
        `shouldBe` False
      dropped (planOf owner [o, refused, s2]) `shouldBe` []

    -- The other half of the same mistake, and the expensive one: the refused
    -- event was not merely dropped, it DECIDED. At a seq above the real value
    -- it made the owner's own set droppable, so a compaction removed a value
    -- from canon in favour of one no reader will ever see.
    it "does not let a set the fold refused displace one it admitted" $ do
      owner <- kp ; stranger <- kp
      let repo = fst owner
          o = anOpen owner 1 "one"
          thr = eventId o
          mine = ev owner 2 (ASet thr "a" "mine" 2000)
          -- Higher seq, refused all the same.
          refused = mkEvent stranger stranger (ASet thr "a" "theirs" 3000)
                            (canonOf repo 3 Nothing)
          c = planOf owner [o, mine, refused]
      dropped c `shouldBe` []
      -- Said as the property rather than as a list, since that is what breaks:
      -- the attribute a reader sees must survive the rewrite.
      attrsOf repo (cpKeep c) `shouldBe` attrsOf repo [o, mine, refused]

    -- PEP-19 "Compaction": one letter folds to at most one event, and the check
    -- that enforces it reads canon for the letter's hash as an origin. Drop the
    -- event and the same letter, still sitting in a mailbox, folds again.
    it "keeps an overwritten close that folded a letter" $ do
      owner <- kp
      let repo = fst owner
          o = anOpen owner 1 "one"
          thr = eventId o
          letterHash = HashRef (hashObject ("a letter" :: LBS.ByteString))
          -- A note-less close, which is droppable by every other measure, but
          -- honoured from somebody's request and therefore carrying its origin.
          honoured = mkEvent owner owner (AClose thr Nothing 2000)
                             (canonWith repo 2 (Just letterHash))
          -- And overwritten.
          reopened = ev owner 3 (AReopen thr Nothing 3000)
          c = planOf owner [o, honoured, reopened]
      dropped c `shouldBe` []
      -- The property behind it: what stops the letter folding twice survives.
      frOrigins (foldEvents repo (cpKeep c))
        `shouldBe` frOrigins (foldEvents repo [o, honoured, reopened])

  describe "PEP-19 compaction: the property it exists to preserve" $ do

    -- The whole claim: a clone that folds the compacted log computes the same
    -- materialized state. Anything else is a size win that changes what the
    -- repository says.
    it "leaves every thread's attributes exactly as they were" $ do
      owner <- kp
      let repo = fst owner
          o1 = anOpen owner 1 "one"
          o2 = anOpen owner 2 "two"
          t1 = eventId o1
          t2 = eventId o2
          evs = [ o1, o2
                , ev owner 3 (ASet t1 "labels" "a" 3000)
                , ev owner 4 (AClose t1 Nothing 4000)
                , ev owner 5 (ASet t1 "labels" "b" 5000)
                , ev owner 6 (AReopen t1 (Just "not done") 6000)
                , ev owner 7 (ASet t2 "labels" "c" 7000)
                , ev owner 8 (ASet t2 "labels" "d" 8000)
                ]
          attrsOf es = fmap tsAttrs (HM.elems (frThreads (foldEvents repo es)))
          c = planOf owner evs

      -- Something was actually dropped, or the assertion below is vacuous.
      dropped c `shouldSatisfy` (not . null)
      -- Compared as maps keyed by thread, since the fold's own order is a
      -- HashMap's and neither list order is a promise.
      HM.fromList [ (t, tsAttrs s)
                  | (t, s) <- HM.toList (frThreads (foldEvents repo (cpKeep c))) ]
        `shouldBe`
        HM.fromList [ (t, tsAttrs s)
                    | (t, s) <- HM.toList (frThreads (foldEvents repo evs)) ]
      length (attrsOf (cpKeep c)) `shouldBe` length (attrsOf evs)

  describe "PEP-19 compaction: telling a rewrite from a fork" $ do

    -- The check a clone makes when canon diverges. A compaction passes it by
    -- construction, which is the whole reason non-forcing sync and compaction
    -- can coexist.
    it "says a compacted lineage is the same canon" $ do
      owner <- kp
      let repo = fst owner
          o = anOpen owner 1 "one"
          thr = eventId o
          evs = [ o
                , ev owner 2 (ASet thr "labels" "a" 2000)
                , ev owner 3 (ASet thr "labels" "b" 3000) ]
          c = planOf owner evs
      dropped c `shouldSatisfy` (not . null)
      equivalentTo (foldEvents repo evs) (foldEvents repo (cpKeep c)) `shouldBe` True

    it "says a lineage missing a comment is not" $ do
      owner <- kp
      let repo = fst owner
          o = anOpen owner 1 "one"
          cm = ev owner 2 (AComment (eventId o) Nothing (Just "said") Nothing 2000)
      equivalentTo (foldEvents repo [o, cm]) (foldEvents repo [o]) `shouldBe` False

    -- The one a state comparison alone would miss: two canons agree on every
    -- thread while one of them quietly dropped a delegation nobody had used.
    -- Taking that hands the repository a maintainer set its owner did not write.
    it "says a lineage missing an unused delegation is not" $ do
      owner <- kp ; bob <- kp
      let repo = fst owner
          o = anOpen owner 1 "one"
          d = ev owner 2 (ADelegate repo (fst bob) 2000)
          withD = foldEvents repo [o, d]
          without = foldEvents repo [o]
      -- The threads really are identical, which is what makes this the case
      -- worth testing rather than an obvious one.
      frThreads withD `shouldBe` frThreads without
      equivalentTo withD without `shouldBe` False

    it "says a lineage missing a redact is not" $ do
      owner <- kp
      let repo = fst owner
          o = anOpen owner 1 "one"
          cm = ev owner 2 (AComment (eventId o) Nothing (Just "said") Nothing 2000)
          rd = ev owner 3 (ARedact repo (eventId cm) 3000)
      equivalentTo (foldEvents repo [o, cm, rd]) (foldEvents repo [o, cm])
        `shouldBe` False

    -- The same shape as the delegation case and for the same kind of reason:
    -- what is missing is invisible in every thread, and it is what stops a
    -- letter still sitting in a mailbox from being folded a second time.
    it "says a lineage missing the record of a folded letter is not" $ do
      owner <- kp
      let repo = fst owner
          o = anOpen owner 1 "one"
          thr = eventId o
          letterHash = HashRef (hashObject ("a letter" :: LBS.ByteString))
          honoured = mkEvent owner owner (AClose thr Nothing 2000)
                             (canonWith repo 2 (Just letterHash))
          plain    = ev owner 2 (AClose thr Nothing 2000)
          withOrigin = foldEvents repo [o, honoured]
          without    = foldEvents repo [o, plain]
      -- Identical to a reader: same thread, same status, same seq.
      frThreads withOrigin `shouldBe` frThreads without
      equivalentTo withOrigin without `shouldBe` False

    -- The conjunct that had no test at all. It is what stops a clone taking a
    -- rewrite that lowers the cursor and hands the local bridge a seq already
    -- spent; without it, deleting the line from 'equivalentTo' changed nothing
    -- any test could see.
    it "says a lineage that lost the high-water mark is not" $ do
      owner <- kp ; bob <- kp
      let repo = fst owner
          o = anOpen owner 1 "one"
          -- A delegate and its revoke: the maintainer set and every thread end
          -- up identical either way, so seq is the ONLY difference left.
          d = ev owner 2 (ADelegate repo (fst bob) 2000)
          r = ev owner 3 (ARevoke repo (fst bob) 3000)
          full  = foldEvents repo [o, d, r]
          short = foldEvents repo [o]
      frThreads full `shouldBe` frThreads short
      frMaintainers full `shouldBe` frMaintainers short
      frMaxSeq full `shouldSatisfy` (> frMaxSeq short)
      equivalentTo full short `shouldBe` False

  describe "PEP-22 hub compact: arguments" $ do

    it "reads the repository, and the dry run as a switch" $ do
      repo <- kp
      let k = show (pretty (AsBase58 (fst repo)))
      compactArgs (argv ["--repo", k]) `shouldBe` Just (CompactArgs (fst repo) False)
      compactArgs (argv ["--repo", k, "--dry-run"])
        `shouldBe` Just (CompactArgs (fst repo) True)
      -- A switch takes no value: given one, the line does not parse rather
      -- than the value being swallowed as something else's.
      compactArgs (argv ["--repo", k, "--dry-run", "yes"]) `shouldBe` Nothing

    it "refuses a call with no repository, and an unknown flag" $ do
      repo <- kp
      let k = show (pretty (AsBase58 (fst repo)))
      compactArgs (argv []) `shouldBe` Nothing
      compactArgs (argv ["--repo", k, "--force"]) `shouldBe` Nothing

  describe "PEP-22 hub compact: what it shows before it writes" $ do

    -- The dropped events are listed because that is what a person approves;
    -- the retained ones are everything else, and counting them says nothing.
    it "names what would go and counts both halves" $ do
      owner <- kp
      let o = anOpen owner 1 "one"
          thr = eventId o
          s1 = ev owner 2 (ASet thr "a" "1" 2000)
          s2 = ev owner 3 (ASet thr "a" "2" 3000)
          out = unlines (fmap show (compactDoc "abc123" (planOf owner [o, s1, s2])))
      out `shouldSatisfy` isInfixOf "abc123"
      out `shouldSatisfy` isInfixOf "keeping 2 event(s), dropping 1"
      out `shouldSatisfy` isInfixOf (show (pretty (eventId s1)))

    -- Canon a stranger contributed to can hold a great many superseded sets,
    -- and a report that listed all of them is a report a stranger sizes.
    it "bounds the list and says how many it did not print" $ do
      owner <- kp
      let o = anOpen owner 1 "one"
          thr = eventId o
          many' = [ ev owner (n + 1) (ASet thr "a" (Text.pack (show n)) (n * 1000))
                  | n <- [1 .. 80] ]
          out = unlines (fmap show (compactDoc "abc123" (planOf owner (o : many'))))
      length (lines out) `shouldSatisfy` (< 60)
      out `shouldSatisfy` isInfixOf "more"

  describe "PEP-22 hub compact: whose canon this is" $ do

    -- Not an authorization check: compaction signs nothing, so there is no key
    -- to check it against. What this catches is a mistyped --repo, which the
    -- selection rule would happily plan a rewrite for -- it never asks whose
    -- canon it is looking at -- while the number index, derived from the fold,
    -- would come out empty.
    it "accepts a canon the named key blessed" $ do
      owner <- kp
      let repo = fst owner
          evs = [anOpen owner 1 "one", anOpen owner 2 "two"]
      ownsCanon evs (foldEvents repo evs) `shouldBe` True

    it "refuses one where the named key blessed nothing" $ do
      owner <- kp ; stranger <- kp
      let evs = [anOpen owner 1 "one", anOpen owner 2 "two"]
      -- The same events, folded under a key that owns nothing here: every one
      -- of them is dropped as unauthorized, and the index would be empty.
      ownsCanon evs (foldEvents (fst stranger) evs) `shouldBe` False

    -- Some admitted and some dropped is the ordinary state of canon and says
    -- nothing about the key.
    it "says nothing about a canon that merely holds a dropped event" $ do
      owner <- kp ; stranger <- kp
      let repo = fst owner
          good = anOpen owner 1 "one"
          -- Blessed by somebody this repository never delegated to.
          bad = mkEvent stranger stranger
                        (AOpen repo HubIssue "two" [] Nothing Nothing Nothing 2000)
                        (canonOf repo 2 (Just 2))
          evs = [good, bad]
      ownsCanon evs (foldEvents repo evs) `shouldBe` True

    -- An empty canon is not a wrong key: it is a repository nobody has folded
    -- anything into, and there is nothing to compact there anyway.
    it "says nothing about an empty canon" $ do
      owner <- kp
      ownsCanon ([] :: [Event]) (foldEvents (fst owner) []) `shouldBe` True
