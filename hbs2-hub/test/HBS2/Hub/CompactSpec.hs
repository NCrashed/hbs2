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
import HBS2.Hub.CLI.Argv (argvAtom)
import HBS2.Hub.Fold (foldEvents,frThreads,tsAttrs)

import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Prelude.Plated (pretty)
import HBS2.Hash (hashObject)

import Data.ByteString.Lazy.Char8 qualified as LBS
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

spec :: Spec
spec = do

  describe "PEP-19 compaction: what may go" $ do

    it "drops a set that a later set overwrote" $ do
      owner <- kp
      let o = anOpen owner 1 "one"
          thr = eventId o
          s1 = ev owner 2 (ASet thr "labels" "a" 2000)
          s2 = ev owner 3 (ASet thr "labels" "b" 3000)
          c = compactionOf [o, s1, s2]
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
          c = compactionOf [o, a, b]
      dropped c `shouldBe` []

    it "does not let one thread overwrite another" $ do
      owner <- kp
      let o1 = anOpen owner 1 "one"
          o2 = anOpen owner 2 "two"
          a = ev owner 3 (ASet (eventId o1) "labels" "a" 3000)
          b = ev owner 4 (ASet (eventId o2) "labels" "b" 4000)
      dropped (compactionOf [o1, o2, a, b]) `shouldBe` []

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
          c = compactionOf [o, closed, reopened, set]
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
          c = compactionOf [s4, s3, s2, s1, o]
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
          c = compactionOf [o, cm, mg, s1, s2]
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
      dropped (compactionOf [d1, r1, d2]) `shouldBe` []

    -- A note on a close is authored discussion even when the status it set was
    -- overwritten a minute later.
    it "keeps a close or reopen that carries a note" $ do
      owner <- kp
      let o = anOpen owner 1 "one"
          thr = eventId o
          noted = ev owner 2 (AClose thr (Just "duplicate of #3") 2000)
          later = ev owner 3 (AReopen thr Nothing 3000)
          set = ev owner 4 (ASet thr "status" "closed" 4000)
          c = compactionOf [o, noted, later, set]
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
          c = compactionOf [o, s1, s2, rd]
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
      dropped (compactionOf [o, a, b]) `shouldBe` []

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
      dropped (compactionOf [o, broken, s2]) `shouldBe` []

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
          c = compactionOf evs

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
          c = compactionOf evs
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
          out = unlines (fmap show (compactDoc "abc123" (compactionOf [o, s1, s2])))
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
          out = unlines (fmap show (compactDoc "abc123" (compactionOf (o : many'))))
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
