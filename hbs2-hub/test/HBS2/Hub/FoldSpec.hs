module HBS2.Hub.FoldSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.SignedBox

import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
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
coords = PRCoords Nothing "refs/heads/f" "aaaa" "refs/heads/master" "bbbb" Nothing

-- A comparable projection of the fold: (thread-id, title, status, #comments).
summary :: FoldResult -> [(EventId, (Text, Maybe Text, Int))]
summary fr =
  sortOn fst
    [ (tsId t, (tsTitle t, HM.lookup "status" (tsAttrs t), length (tsComments t)))
    | t <- HM.elems (frThreads fr) ]

reasons :: FoldResult -> [DropReason]
reasons = map snd . frDropped

-- Fetch a thread the test asserts must exist.
threadOf :: FoldResult -> ThreadId -> ThreadState
threadOf fr tid = fromMaybe (error "expected thread") (HM.lookup tid (frThreads fr))

spec :: Spec
spec = do

  describe "PEP-19 fold" $ do

    it "is deterministic under input reordering" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "hello" Nothing Nothing Nothing 100)
                          (canon 1 (Just 1))
          tid   = eventId eOpen
          eCmt  = mkEvent alice owner (AComment tid Nothing (Just "hi") Nothing 200)
                          (canon 2 Nothing)
          eSet  = mkEvent owner owner (AClose tid Nothing 300)
                          (canon 3 Nothing)
          fwd = foldEvents repo [eOpen, eCmt, eSet]
          rev = foldEvents repo [eSet, eCmt, eOpen]
      summary fwd `shouldBe` summary rev
      summary fwd `shouldBe` [(tid, ("hello", Just "closed", 1))]

    it "reports drops in a deterministic order too" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          other = fst alice           -- a different repo key
          bad1 = mkEvent alice owner (AOpen other HubIssue "x" Nothing Nothing Nothing 1)
                         (canon 1 (Just 1))
          bad2 = mkEvent alice owner (AOpen other HubIssue "y" Nothing Nothing Nothing 2)
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
                   (AOpen repo HubIssue n Nothing Nothing Nothing 1) (canon 1 (Just 1))
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
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" Nothing Nothing Nothing 1)
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
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" Nothing Nothing Nothing 1)
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

    it "rejects a tampered author box" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          e = mkEvent alice owner (AOpen repo HubIssue "t" Nothing Nothing Nothing 1)
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
          e = mkEvent alice owner (AOpen repo HubIssue "t" Nothing Nothing Nothing 1)
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
                     (AOpen (fst ownerY) HubIssue "authored elsewhere" Nothing Nothing Nothing 1)
                     (canon 1 (Just 1))
          fr = foldEvents (fst ownerX) [stolen]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [WrongTarget]

    it "deduplicates by event-id (rewrap replay)" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" Nothing Nothing Nothing 1)
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
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" Nothing Nothing Nothing 1)
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
          eOpen = mkEvent alice owner (AOpen repo HubIssue "old" Nothing Nothing Nothing 1)
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
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" Nothing Nothing Nothing 1)
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
          real = mkEvent alice owner (AOpen repo HubIssue "a" Nothing Nothing Nothing 1)
                         (canon 1 (Just 1))
          other = mkEvent alice owner (AOpen repo HubIssue "b" Nothing Nothing Nothing 2)
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
          eOpenC  = mkEvent carol bob (AOpen repo HubIssue "ok" Nothing Nothing Nothing 100)
                            (canon 2 (Just 1))
          tidC    = eventId eOpenC
          eDelDave = mkEvent bob bob (ADelegate (fst dave) 3) (canon 3 Nothing)
          eOpenE  = mkEvent erin dave (AOpen repo HubIssue "bad" Nothing Nothing Nothing 400)
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
          eOk   = mkEvent alice bob (AOpen repo HubIssue "before" Nothing Nothing Nothing 2)
                          (canon 2 (Just 1))
          tidOk = eventId eOk
          eRev  = mkEvent owner owner (ARevoke (fst bob) 3) (canon 3 Nothing)
          -- after the revoke bob may no longer bless
          eBad  = mkEvent alice bob (AOpen repo HubIssue "after" Nothing Nothing Nothing 4)
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
          eOpen = mkEvent alice owner (AOpen repo HubIssue "still works" Nothing Nothing Nothing 2)
                          (canon 2 (Just 1))
          tid = eventId eOpen
          eClose = mkEvent owner owner (AClose tid Nothing 3) (canon 3 Nothing)
          fr = foldEvents repo [eSelf, eOpen, eClose]
      summary fr `shouldBe` [(tid, ("still works", Just "closed", 0))]
      reasons fr `shouldBe` []

  describe "PEP-20 pull requests" $ do

    it "rejects revise from anyone but the author of record" $ do
      owner <- kp
      alice <- kp
      mallory <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner
                    (AOpen repo HubPR "pr" Nothing Nothing (Just coords) 1)
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
          eOpen = mkEvent alice owner (AOpen repo HubIssue "an issue" Nothing Nothing Nothing 1)
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
          eOpen = mkEvent alice owner (AOpen repo HubPR "no coords" Nothing Nothing Nothing 1)
                          (canon 1 (Just 1))
          fr = foldEvents repo [eOpen]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [BadKind]

    it "rejects an issue open carrying pr coordinates" $ do
      owner <- kp
      alice <- kp
      -- The author box says one thing; materializing it as a plain issue
      -- would silently drop the coordinates, so reject it instead.
      let repo = fst owner
          eOpen = mkEvent alice owner
                    (AOpen repo HubIssue "issue with coords" Nothing Nothing (Just coords) 1)
                    (canon 1 (Just 1))
          fr = foldEvents repo [eOpen]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [BadKind]
