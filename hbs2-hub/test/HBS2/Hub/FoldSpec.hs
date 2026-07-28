module HBS2.Hub.FoldSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.SignedBox

import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import HBS2.Data.Types.Refs (HashRef)
import Data.Text (Text)
import Data.Word (Word64)
import Test.Hspec

type KP = (HubKey, PrivKey 'Sign HubScheme)

kp :: IO KP
kp = do
  c <- newCredentials @'HBS2Basic
  pure (_peerSignPk c, _peerSignSk c)

-- A canon box builder: seq, optional number, folded-at time.
canonAt :: Word64 -> Maybe Word64 -> Word64 -> EventId -> CanonContent
canonAt sq num folded eid = CanonContent eid sq num Nothing folded Nothing

canon :: Word64 -> Maybe Word64 -> EventId -> CanonContent
canon sq num = canonAt sq num sq

coords :: PRCoords
-- A fork-pointer PR: PEP-20 requires one of the two ways to fetch the
-- change, so a coords with neither is refused (reachableCoords).
coords = PRCoords (Just "hbs23://fork") "refs/heads/f" "aaaa" "refs/heads/master" "bbbb" Nothing

-- Everything a fold produces for a thread, so a determinism check compares
-- the whole state rather than a convenient corner of it. ThreadState has no
-- Eq (comments and PR state are records), hence the explicit tuple.
type Summary =
  ( EventId, HubKind, Maybe Word64, HubKey, Word64, Word64
  , [(Text,Text)], [Text], Maybe Text, Maybe HashRef, Maybe ByteString, Maybe HashRef
  , [(EventId,HubKey,Word64,Maybe Text,Maybe HashRef)]
  , Maybe (PRCoords, Maybe ByteString, Maybe (Text,Text)) )

summary :: FoldResult -> [Summary]
summary fr =
  sortOn (\(i,_,_,_,_,_,_,_,_,_,_,_,_,_) -> i)
    [ ( tsId t, tsKind t, tsNumber t, tsAuthor t, tsCreated t, tsUpdated t
      , sortOn fst (HM.toList (tsAttrs t))
      , tsLabelsRequested t, tsBody t, tsBodyPart t, tsPartSecret t, tsOrigin t
      , [ (cId c, cAuthor c, cFoldedTs c, cBody c, cBodyPart c) | c <- tsComments t ]
      , fmap (\p -> (psCoords p, psPartSecret p, psMerge p)) (tsPR t) )
    | t <- HM.elems (frThreads fr) ]

-- The narrow view a few assertions read directly.
titles :: FoldResult -> [(EventId, (Text, Maybe Text, Int))]
titles fr =
  sortOn fst
    [ (tsId t, (tsTitle t, HM.lookup "status" (tsAttrs t), length (tsComments t)))
    | t <- HM.elems (frThreads fr) ]

reasons :: FoldResult -> [DropReason]
reasons = map snd . frDropped

-- Fetch a thread the test asserts must exist.
threadOf :: FoldResult -> ThreadId -> ThreadState
threadOf fr tid = fromMaybe (error "expected thread") (HM.lookup tid (frThreads fr))

-- A hash no event in the fixture has.
someHash :: IO HashRef
someHash = do
  (pk,sk) <- kp
  pure (authorBoxId (signAuthor pk sk (ARevoke pk 0)))

-- A correctly signed box whose content this build cannot decode, which is
-- what an op added by a newer schema looks like from here. SignedBox is
-- phantom in its payload type, so signing another type and reinterpreting
-- reproduces it exactly.
futureBox :: KP -> SignedBox AuthorContent HubScheme
futureBox (pk,sk) =
  case makeSignedBox @HubScheme pk sk (12345 :: Word64) of
    SignedBox p b s -> SignedBox p b s

spec :: Spec
spec = do

  describe "PEP-19 fold" $ do

    it "is deterministic under input reordering" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "hello" [] Nothing Nothing Nothing 100)
                          (canon 1 (Just 1))
          tid   = eventId eOpen
          eCmt  = mkEvent alice owner (AComment tid Nothing (Just "hi") Nothing 200)
                          (canon 2 Nothing)
          eSet  = mkEvent owner owner (AClose tid Nothing 300)
                          (canon 3 Nothing)
          fwd = foldEvents repo [eOpen, eCmt, eSet]
          rev = foldEvents repo [eSet, eCmt, eOpen]
      summary fwd `shouldBe` summary rev
      titles fwd `shouldBe` [(tid, ("hello", Just "closed", 1))]

    it "reports drops in a deterministic order too" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          other = fst alice           -- a different repo key
          bad1 = mkEvent alice owner (AOpen other HubIssue "x" [] Nothing Nothing Nothing 1)
                         (canon 1 (Just 1))
          bad2 = mkEvent alice owner (AOpen other HubIssue "y" [] Nothing Nothing Nothing 2)
                         (canon 2 (Just 2))
          a = frDropped (foldEvents repo [bad1, bad2])
          b = frDropped (foldEvents repo [bad2, bad1])
      a `shouldBe` b

    it "orders unresolvable drops deterministically (they carry no seq)" $ do
      owner <- kp
      alice <- kp
      -- Events that fail *resolve* never get a seq, so they take the
      -- maxBound bucket; that bucket must still be ordered by event-id.
      let repo = fst owner
          mk n = mkEvent alice owner
                   (AOpen repo HubIssue n [] Nothing Nothing Nothing 1) (canon 1 (Just 1))
          forged1 = Event (evAuthorBox (mk "a1")) (evCanonBox (mk "a2"))  -- IdMismatch
          forged2 = Event (evAuthorBox (mk "b1")) (evCanonBox (mk "b2"))  -- IdMismatch
          x = frDropped (foldEvents repo [forged1, forged2])
          y = frDropped (foldEvents repo [forged2, forged1])
      map snd x `shouldBe` [IdMismatch, IdMismatch]
      x `shouldBe` y

    it "drops a comment on a non-admitted thread (dangling)" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          -- a real, well-formed open that we deliberately never fold
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                          (canon 1 (Just 1))
          orphan = eventId eOpen
          eCmt = mkEvent alice owner (AComment orphan Nothing (Just "orphan") Nothing 2)
                         (canon 2 Nothing)
          fr = foldEvents repo [eCmt]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [BadThread]

    it "hides a redacted event and ignores an unknown redact target" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                          (canon 1 (Just 1))
          tid = eventId eOpen
          eCmt = mkEvent alice owner (AComment tid Nothing (Just "oops, a secret") Nothing 2)
                         (canon 2 Nothing)
          cid = eventId eCmt
          eRed = mkEvent owner owner (ARedact cid 3) (canon 3 Nothing)
          -- a redact naming an event that was never admitted
          ghost = mkEvent alice owner (AComment tid Nothing (Just "never folded") Nothing 4)
                          (canon 4 Nothing)
          eGhost = mkEvent owner owner (ARedact (eventId ghost) 5) (canon 5 Nothing)
          fr = foldEvents repo [eOpen, eCmt, eRed, eGhost]
          t = threadOf fr tid
      HS.member cid (frRedacted fr) `shouldBe` True
      -- the event itself stays in canon; only rendering is withheld
      map cId (tsComments t) `shouldBe` [cid]
      reasons fr `shouldBe` [UnknownRedact]

    it "refuses a redact authored by anyone but a maintainer" $ do
      owner <- kp
      alice <- kp
      -- redact is owner-authored (PEP-19 rule 4): checking only the canon
      -- box would let a stranger author a redact and hide any event as soon
      -- as a triage bridge blessed the letter it arrived in.
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                          (canon 1 (Just 1))
          tid = eventId eOpen
          eCmt = mkEvent alice owner (AComment tid Nothing (Just "hi") Nothing 2)
                         (canon 2 Nothing)
          cid = eventId eCmt
          -- authored by alice, blessed by the owner
          eRed = mkEvent alice owner (ARedact cid 3) (canon 3 Nothing)
          fr = foldEvents repo [eOpen, eCmt, eRed]
      HS.member cid (frRedacted fr) `shouldBe` False
      reasons fr `shouldBe` [UnauthorizedCanon]

    it "reports what it admitted but should not have" $ do
      owner <- kp
      alice <- kp
      part <- someHash
      origin <- someHash
      -- None of these is a drop: refusing them would make a clone show less
      -- than canon holds. They exist for hub verify (PEP-22), which is the
      -- only place they can be acted on.
      let repo = fst owner
          o1 = mkEvent alice owner (AOpen repo HubIssue "a" [] Nothing Nothing Nothing 1)
                 (\e -> CanonContent e 1 (Just 5) (Just origin) 5000 Nothing)
          -- number goes backwards, folded-ts goes backwards, and the same
          -- letter is folded a second time
          o2 = mkEvent alice owner (AOpen repo HubIssue "b" [] Nothing Nothing Nothing 2)
                 (\e -> CanonContent e 2 (Just 3) (Just origin) 4000 Nothing)
          -- ...and this one names an encrypted part with no key to it
          o3 = mkEvent alice owner (AOpen repo HubIssue "c" [] Nothing (Just part) Nothing 3)
                 (\e -> CanonContent e 3 (Just 9) Nothing 6000 Nothing)
          -- an unnormalized multi-valued attribute: the same two labels in
          -- another order are other bytes and so another event-id
          o4 = mkEvent owner owner (ASet (eventId o1) "labels" "ui,bug" 4) (canonAt 4 Nothing 7000)
          fr = foldEvents repo [o1,o2,o3,o4]
      frDropped fr `shouldBe` []
      map snd (frAnomalies fr) `shouldBe`
        [ NumberWentBack 5 3, FoldedTsWentBack 5000 4000, DupOrigin origin
        , PartWithoutSecret
        , UnnormalizedAttr "labels"
        ]
      -- in seq order, so a report reads like the log
      map fst (frAnomalies fr) `shouldBe` [eventId o2, eventId o2, eventId o2, eventId o3, eventId o4]

    it "reports a duplicate seq and a duplicate number" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          a = mkEvent alice owner (AOpen repo HubIssue "a" [] Nothing Nothing Nothing 1)
                (canon 1 (Just 1))
          b = mkEvent alice owner (AOpen repo HubIssue "b" [] Nothing Nothing Nothing 2)
                (canon 1 (Just 1))
          fr = foldEvents repo [a,b]
      frDropped fr `shouldBe` []
      map snd (frAnomalies fr) `shouldSatisfy` \as ->
        DupSeq 1 `elem` as && DupNumber 1 `elem` as

    it "collects every part canon references, for retention" $ do
      owner <- kp
      alice <- kp
      body <- someHash
      cmt <- someHash
      bundle <- someHash
      -- A canon-aware purge has to keep the trees canon points at (PEP-21),
      -- and they are otherwise scattered across three fields of two records.
      let repo = fst owner
          ePR = mkEvent alice owner
                  (AOpen repo HubPR "pr" [] Nothing (Just body)
                     (Just coords { prBundle = Just bundle }) 1)
                  (\e -> CanonContent e 1 (Just 1) Nothing 1 (Just "S"))
          eC = mkEvent alice owner (AComment (eventId ePR) Nothing Nothing (Just cmt) 2)
                 (\e -> CanonContent e 2 Nothing Nothing 2 (Just "S"))
          fr = foldEvents repo [ePR, eC]
      frDropped fr `shouldBe` []
      frParts fr `shouldBe` HS.fromList [body, cmt, bundle]

    it "canonicalizes a multi-valued attribute and leaves scalars alone" $ do
      -- The rule PEP-19 states in prose: the same set of labels must always
      -- produce the same bytes, or two maintainers agreeing mint two events.
      normalizeAttr "labels" "ui,bug" `shouldBe` "bug,ui"
      normalizeAttr "labels" "bug,ui,bug" `shouldBe` "bug,ui"
      normalizeAttr "assignees" "b,a" `shouldBe` "a,b"
      normalizeAttr "status" "in,progress" `shouldBe` "in,progress"
      normalizedAttr "labels" "bug,ui" `shouldBe` True
      normalizedAttr "labels" "ui,bug" `shouldBe` False

    it "names an event file so a lexical listing is the fold order" $ do
      owner <- kp
      alice <- kp
      let ac n = AOpen (fst owner) HubIssue n [] Nothing Nothing Nothing 1
          e1 = mkEvent alice owner (ac "a") (canon 1 (Just 1))
          e2 = mkEvent alice owner (ac "b") (canon 10 (Just 2))
      -- The padding is the point: "10" must not sort before "2".
      eventFileName 1 (eventId e1) `shouldSatisfy` (< eventFileName 10 (eventId e2))
      length (eventFileName 1 (eventId e1)) `shouldBe` length (eventFileName 10 (eventId e2))

    it "orders two blessings of one author box by content, not by input" $ do
      owner <- kp
      alice <- kp
      -- The same author box blessed twice at the same seq is ONE event, and
      -- the loser is dropped as a duplicate. Which copy wins decides the
      -- number the thread gets, and therefore the next number a folder mints
      -- ('cursorFrom'), and neither seq nor event-id can see the difference:
      -- all of it lives in the canon box.
      let repo = fst owner
          ac = AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1
          e1 = mkEvent alice owner ac (canonAt 1 (Just 1) 1111)
          e2 = mkEvent alice owner ac (canonAt 1 (Just 7) 2222)
          tid = eventId e1
          shown fr = ( fmap tsNumber (HM.lookup tid (frThreads fr))
                     , fmap tsCreated (HM.lookup tid (frThreads fr))
                     , frMaxNumber fr
                     , map snd (frDropped fr)
                     )
      eventId e2 `shouldBe` tid
      shown (foldEvents repo [e1,e2]) `shouldBe` shown (foldEvents repo [e2,e1])
      reasons (foldEvents repo [e1,e2]) `shouldBe` [DupId]

    it "tells an undecodable box apart from a forged one" $ do
      owner <- kp
      alice <- kp
      -- A correctly signed box whose content this build cannot decode: canon
      -- is newer than the reader. Reporting BadAuthorSig would accuse a
      -- newer, honest sender of forgery.
      let repo = fst owner
          real = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                         (canon 1 (Just 1))
          ev = Event (futureBox alice) (evCanonBox real)
          fr = foldEvents repo [ev]
      map fst (frDropped fr) `shouldBe` [eventId ev]
      case map snd (frDropped fr) of
        [UndecodableAuthor k why] -> do
          -- the reader can still name whose event it could not read
          k `shouldBe` fst alice
          why `shouldBe` Undecodable
        other -> expectationFailure ("unexpected: " <> show other)

    it "records which key blessed each event, not just the thread" $ do
      owner <- kp
      bob <- kp
      alice <- kp
      -- Under a delegation the events of one thread are blessed by different
      -- keys, so the provenance PEP-22 renders is per item, not per thread.
      let repo = fst owner
          eDel = mkEvent owner owner (ADelegate (fst bob) 1) (canon 1 Nothing)
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 2)
                          (canon 2 (Just 1))
          tid = eventId eOpen
          eCmt = mkEvent alice bob (AComment tid Nothing (Just "hi") Nothing 3)
                         (canon 3 Nothing)
          fr = foldEvents repo [eDel, eOpen, eCmt]
          t = threadOf fr tid
      frDropped fr `shouldBe` []
      tsCanonBy t `shouldBe` fst owner
      map cCanonBy (tsComments t) `shouldBe` [fst bob]

    it "keeps the reply-to the author signed, without validating it" $ do
      owner <- kp
      alice <- kp
      ghost <- someHash
      -- PEP-19 admits the comment whatever reply-to says: the target may be a
      -- letter the owner chose not to fold. PEP-22 then renders it flat. What
      -- must not happen is the field going missing, since the author signed it
      -- and the event-id covers it.
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                          (canon 1 (Just 1))
          tid = eventId eOpen
          eC1 = mkEvent alice owner (AComment tid Nothing (Just "first") Nothing 2)
                        (canon 2 Nothing)
          eC2 = mkEvent alice owner (AComment tid (Just (eventId eC1)) (Just "answer") Nothing 3)
                        (canon 3 Nothing)
          eC3 = mkEvent alice owner (AComment tid (Just ghost) (Just "dangling") Nothing 4)
                        (canon 4 Nothing)
          fr = foldEvents repo [eOpen, eC1, eC2, eC3]
      frDropped fr `shouldBe` []
      map cReplyTo (tsComments (threadOf fr tid))
        `shouldBe` [Nothing, Just (eventId eC1), Just ghost]

    it "moves the thread clock when a comment is redacted" $ do
      owner <- kp
      alice <- kp
      -- Moderation changes the thread, so a renderer sorting by tsUpdated
      -- must not show it as untouched since before the redaction.
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                          (canon 1 (Just 1))
          tid = eventId eOpen
          eCmt = mkEvent alice owner (AComment tid Nothing (Just "spam") Nothing 2)
                         (canon 2 Nothing)
          eRed = mkEvent owner owner (ARedact (eventId eCmt) 3) (canonAt 3 Nothing 9000)
          fr = foldEvents repo [eOpen, eCmt, eRed]
      frDropped fr `shouldBe` []
      tsUpdated (threadOf fr tid) `shouldBe` 9000

    it "refuses a number on anything but an open" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                          (canon 1 (Just 1))
          tid = eventId eOpen
          -- a comment stamped with a number: nothing mints these, but a
          -- wrong or hostile maintainer could, and it would raise the
          -- number high-water mark for everyone.
          eCmt = mkEvent alice owner (AComment tid Nothing (Just "hi") Nothing 2)
                         (canon 2 (Just 99))
          fr = foldEvents repo [eOpen, eCmt]
      reasons fr `shouldBe` [BadStamp]
      frMaxNumber fr `shouldBe` 1

    it "refuses a seq or number at the top of the range" $ do
      owner <- kp
      alice <- kp
      -- The next mint is the maximum plus one, so admitting maxBound would
      -- wrap the cursor to zero and poison the repo permanently.
      let repo = fst owner
          eMax = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                         (canon maxBound (Just 1))
          eNum = mkEvent alice owner (AOpen repo HubIssue "u" [] Nothing Nothing Nothing 1)
                         (canon 1 (Just maxBound))
          fr = foldEvents repo [eMax, eNum]
      reasons fr `shouldBe` [BadStamp, BadStamp]
      HM.null (frThreads fr) `shouldBe` True
      frMaxSeq fr `shouldBe` 0

    it "rejects a tampered author box" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          e = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                      (canon 1 (Just 1))
          SignedBox pk bs sig = evAuthorBox e
          tampered = e { evAuthorBox = SignedBox pk (bs <> "x") sig }
          fr = foldEvents repo [tampered]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [BadAuthorSig]

    it "rejects a tampered canon box" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          e = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                      (canon 1 (Just 1))
          SignedBox pk bs sig = evCanonBox e
          tampered = e { evCanonBox = SignedBox pk (bs <> "x") sig }
          fr = foldEvents repo [tampered]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [BadCanonSig]

    it "rejects an open authored for a different repo (cross-repo replay)" $ do
      ownerX <- kp     -- our repo
      ownerY <- kp     -- someone else's repo
      alice  <- kp     -- author who wrote to repo Y
      -- Alice's author box, signed for repo Y, lifted verbatim into repo X
      -- and blessed by X's owner. It must not become a thread in X.
      let stolen = mkEvent alice ownerX
                     (AOpen (fst ownerY) HubIssue "authored elsewhere" [] Nothing Nothing Nothing 1)
                     (canon 1 (Just 1))
          fr = foldEvents (fst ownerX) [stolen]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [WrongTarget]

    it "deduplicates by event-id (rewrap replay)" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                          (canon 1 (Just 1))
          tid   = eventId eOpen
          ac    = AComment tid Nothing (Just "once") Nothing 2
          -- the SAME author box, blessed twice under different seqs
          c1 = mkEvent alice owner ac (canon 2 Nothing)
          c2 = mkEvent alice owner ac (canon 3 Nothing)
          fr = foldEvents repo [eOpen, c1, c2]
          t  = threadOf fr tid
      eventId c1 `shouldBe` eventId c2      -- same content, same id
      length (tsComments t) `shouldBe` 1
      reasons fr `shouldBe` [DupId]

    it "breaks seq ties by event-id, consistently" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                          (canon 1 (Just 1))
          tid   = eventId eOpen
          -- two DIFFERENT sets of the same attribute at the SAME seq
          s1 = mkEvent owner owner (ASet tid "status" "one" 2) (canon 5 Nothing)
          s2 = mkEvent owner owner (ASet tid "status" "two" 3) (canon 5 Nothing)
          fwd = foldEvents repo [eOpen, s1, s2]
          rev = foldEvents repo [eOpen, s2, s1]
      summary fwd `shouldBe` summary rev

    it "treats title as an LWW attribute, not a frozen field" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "old" [] Nothing Nothing Nothing 1)
                          (canon 1 (Just 1))
          tid   = eventId eOpen
          eSet  = mkEvent owner owner (ASet tid "title" "new" 2) (canon 2 Nothing)
          fr = foldEvents repo [eOpen, eSet]
          t  = threadOf fr tid
      tsTitle t `shouldBe` "new"
      HM.lookup "title" (tsAttrs t) `shouldBe` Just "new"

    it "takes times from the canon box, not the author's clock" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                          (canonAt 1 (Just 1) 1000)
          tid   = eventId eOpen
          -- a comment claiming to be from the far future
          eCmt  = mkEvent alice owner (AComment tid Nothing (Just "spoof") Nothing maxBound)
                          (canonAt 2 Nothing 2000)
          fr = foldEvents repo [eOpen, eCmt]
          t  = threadOf fr tid
      tsCreated t `shouldBe` 1000
      tsUpdated t `shouldBe` 2000           -- not maxBound
      tsAuthorTs t `shouldBe` 1             -- declared value still available
      map cAuthorTs (tsComments t) `shouldBe` [maxBound]
      map cFoldedTs (tsComments t) `shouldBe` [2000]

    it "rejects a canon box blessing a different event-id" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          real = mkEvent alice owner (AOpen repo HubIssue "a" [] Nothing Nothing Nothing 1)
                         (canon 1 (Just 1))
          other = mkEvent alice owner (AOpen repo HubIssue "b" [] Nothing Nothing Nothing 2)
                          (canon 2 (Just 2))
          -- graft: real author box, canon box that blesses the other id
          forged = Event (evAuthorBox real) (evCanonBox other)
          fr = foldEvents repo [forged]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [IdMismatch]

  describe "PEP-21 delegation" $ do

    it "rejects delegation escalation by a non-owner (rule 5)" $ do
      owner <- kp
      bob   <- kp     -- legit delegate
      carol <- kp     -- author bob blesses
      dave  <- kp     -- bob illegally tries to delegate dave
      erin  <- kp     -- dave illegally tries to bless erin's open
      let repo = fst owner
          eDelBob = mkEvent owner owner (ADelegate (fst bob) 1) (canon 1 Nothing)
          eOpenC  = mkEvent carol bob (AOpen repo HubIssue "ok" [] Nothing Nothing Nothing 100)
                            (canon 2 (Just 1))
          tidC    = eventId eOpenC
          eDelDave = mkEvent bob bob (ADelegate (fst dave) 3) (canon 3 Nothing)
          eOpenE  = mkEvent erin dave (AOpen repo HubIssue "bad" [] Nothing Nothing Nothing 400)
                            (canon 4 (Just 2))
          tidE    = eventId eOpenE
          fr = foldEvents repo [eDelBob, eOpenC, eDelDave, eOpenE]
      HM.member tidC (frThreads fr) `shouldBe` True
      HM.member tidE (frThreads fr) `shouldBe` False
      reasons fr `shouldContain` [UnauthorizedDelegate]
      reasons fr `shouldContain` [UnauthorizedCanon]

    it "honours revoke going forward but keeps events signed while authorized" $ do
      owner <- kp
      bob   <- kp
      alice <- kp
      let repo = fst owner
          eDel  = mkEvent owner owner (ADelegate (fst bob) 1) (canon 1 Nothing)
          -- bob blesses this while authorized: must survive the later revoke
          eOk   = mkEvent alice bob (AOpen repo HubIssue "before" [] Nothing Nothing Nothing 2)
                          (canon 2 (Just 1))
          tidOk = eventId eOk
          eRev  = mkEvent owner owner (ARevoke (fst bob) 3) (canon 3 Nothing)
          -- after the revoke bob may no longer bless
          eBad  = mkEvent alice bob (AOpen repo HubIssue "after" [] Nothing Nothing Nothing 4)
                          (canon 4 (Just 2))
          tidBad = eventId eBad
          fr = foldEvents repo [eDel, eOk, eRev, eBad]
      HM.member tidOk (frThreads fr) `shouldBe` True
      HM.member tidBad (frThreads fr) `shouldBe` False
      reasons fr `shouldBe` [UnauthorizedCanon]

    it "ignores a revoke of the owner key (root of trust)" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eSelf = mkEvent owner owner (ARevoke (fst owner) 1) (canon 1 Nothing)
          eOpen = mkEvent alice owner (AOpen repo HubIssue "still works" [] Nothing Nothing Nothing 2)
                          (canon 2 (Just 1))
          tid = eventId eOpen
          eClose = mkEvent owner owner (AClose tid Nothing 3) (canon 3 Nothing)
          fr = foldEvents repo [eSelf, eOpen, eClose]
      titles fr `shouldBe` [(tid, ("still works", Just "closed", 0))]
      reasons fr `shouldBe` []

  describe "PEP-20 pull requests" $ do

    it "rejects revise from anyone but the author of record" $ do
      owner <- kp
      alice <- kp
      mallory <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner
                    (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords) 1)
                    (canon 1 (Just 1))
          tid = eventId eOpen
          good = coords { prSourceTip = "cccc" }
          evil = coords { prSourceTip = "dead" }
          eGood = mkEvent alice owner (ARevise tid good 2) (canon 2 Nothing)
          eEvil = mkEvent mallory owner (ARevise tid evil 3) (canon 3 Nothing)
          fr = foldEvents repo [eOpen, eGood, eEvil]
          t  = threadOf fr tid
      fmap (prSourceTip . psCoords) (tsPR t) `shouldBe` Just "cccc"
      reasons fr `shouldBe` [BadRevise]

    it "refuses PR-only ops on an issue thread and never invents coordinates" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "an issue" [] Nothing Nothing Nothing 1)
                          (canon 1 (Just 1))
          tid = eventId eOpen
          eMerge = mkEvent owner owner (AMerge tid "cafe" "refs/heads/master" 2) (canon 2 Nothing)
          fr = foldEvents repo [eOpen, eMerge]
          t  = threadOf fr tid
      tsPR t `shouldBe` Nothing
      HM.lookup "status" (tsAttrs t) `shouldBe` Just "open"
      reasons fr `shouldBe` [BadKind]

    it "rejects a pr open with no coordinates" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubPR "no coords" [] Nothing Nothing Nothing 1)
                          (canon 1 (Just 1))
          fr = foldEvents repo [eOpen]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [BadKind]

    it "encodes a label set to the same bytes whatever the order" $ do
      -- An attribute value is one string, so the same labels in a different
      -- order would otherwise be different bytes and a different event-id:
      -- two maintainers agreeing would mint two events.
      encodeLabels ["ui","bug"] `shouldBe` encodeLabels ["bug","ui"]
      encodeLabels ["bug","ui"] `shouldBe` "bug,ui"
      encodeLabels ["bug","bug"] `shouldBe` "bug"
      encodeLabels [" bug "] `shouldBe` "bug"
      decodeLabels (encodeLabels ["bug","ui"]) `shouldBe` ["bug","ui"]
      decodeLabels "" `shouldBe` []
      -- A comma cannot survive the round trip, so it is not representable.
      validLabel "needs,triage" `shouldBe` False
      encodeLabels ["ok","needs,triage"] `shouldBe` "ok"

    it "refuses a pr whose change cannot be fetched at all" $ do
      owner <- kp
      alice <- kp
      -- PEP-20 ships the diff either as a bundle or as a fork to pull; with
      -- neither, canon would carry a review request with nothing to review.
      let repo = fst owner
          nowhere = coords { prSource = Nothing, prBundle = Nothing }
          eOpen = mkEvent alice owner
                    (AOpen repo HubPR "unfetchable" [] Nothing Nothing (Just nowhere) 1)
                    (canon 1 (Just 1))
          fr = foldEvents repo [eOpen]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [BadKind]

    it "refuses a revise that removes the last way to fetch" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner
                    (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords) 1)
                    (canon 1 (Just 1))
          tid = eventId eOpen
          nowhere = coords { prSource = Nothing, prBundle = Nothing }
          eRev = mkEvent alice owner (ARevise tid nowhere 2) (canon 2 Nothing)
          fr = foldEvents repo [eOpen, eRev]
      reasons fr `shouldBe` [BadKind]
      -- the original coordinates survive
      fmap (prSource . psCoords) (tsPR (threadOf fr tid)) `shouldBe` Just (Just "hbs23://fork")

    it "rejects an issue open carrying pr coordinates" $ do
      owner <- kp
      alice <- kp
      -- The author box says one thing; materializing it as a plain issue
      -- would silently drop the coordinates, so reject it instead.
      let repo = fst owner
          eOpen = mkEvent alice owner
                    (AOpen repo HubIssue "issue with coords" [] Nothing Nothing (Just coords) 1)
                    (canon 1 (Just 1))
          fr = foldEvents repo [eOpen]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [BadKind]
