module HBS2.Hub.FoldSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Hub.Bridge (cursorFrom,CanonCursor(..))
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.SignedBox

import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Codec.Serialise (Serialise,serialise)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as B8
import HBS2.Hash (hashObject)
import HBS2.Prelude.Plated (fromStringMay,pretty)
import Codec.Serialise qualified as CBOR
import Data.Bits (xor)
import HBS2.Net.Auth.GroupKeySymm (typicalKeyLength)
import Data.ByteString (ByteString)
import HBS2.Data.Types.Refs (HashRef(..))
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
-- The secret is opaque and has no Eq-friendly display, so summaries omit it.
type Summary =
  ( EventId, HubKind, Maybe Word64, HubKey, Word64, Word64
  , [(Text,Text)], [Text], Maybe Text, Maybe HashRef, Maybe PartSecret, Maybe HashRef
  , [(EventId,HubKey,Word64,Maybe Text,Maybe HashRef)]
  , Maybe (PRCoords, Maybe PartSecret, Maybe (Text,Text)) )

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

-- Any other order that is neither the input nor its reverse.
rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs <> [x]

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

-- Sign a payload the way everything OUTSIDE this package does: no domain tag,
-- because nothing else knows about one. This is what a signature lifted from
-- another record looks like.
signBare :: Serialise a => KP -> a -> SignedBox b HubScheme
signBare (pk,sk) a = reinterpret (makeSignedBox pk sk a)

-- The CBOR an author box signs, domain tag included.
domainedBytes :: AuthorContent -> ByteString
domainedBytes = LBS.toStrict . serialise . Domained (domainOf (Nothing @AuthorContent))

-- A box over bytes the caller chose, signed for real.
rawBox :: KP -> ByteString -> SignedBox p HubScheme
rawBox (pk,sk) bs = SignedBox pk bs (makeSign @HubScheme sk bs)

-- The box's payload means whatever the reader decodes it as: that is the point
-- of the domain tag, and what these tests check.
reinterpret :: SignedBox a HubScheme -> SignedBox b HubScheme
reinterpret (SignedBox p b s) = SignedBox p b s

-- Flip the high bit of the last signature byte: the classic malleability
-- probe against a scheme that does not canonicalize S.
mangleSig :: SignedBox p HubScheme -> SignedBox p HubScheme
mangleSig (SignedBox pk bs sig) = SignedBox pk bs (mangle sig)
  where
    mangle s = case CBOR.deserialiseOrFail (bend (serialise s)) of
      Right s' -> s'
      Left e   -> error ("cannot rebuild signature: " <> show e)
    bend lbs = case LBS.unsnoc lbs of
      Just (front, b) -> LBS.snoc front (b `xor` 0x80)
      Nothing         -> lbs

-- A correctly signed box whose content this build cannot decode, which is
-- what an op added by a newer schema looks like from here. SignedBox is
-- phantom in its payload type, so signing another type and reinterpreting
-- reproduces it exactly.
futureBox :: KP -> SignedBox AuthorContent HubScheme
futureBox (pk,sk) =
  -- Tagged with the author domain, because that is what a real v2 sender
  -- produces: domains are never renumbered, so what a newer schema changes is
  -- the payload inside the tag, not the tag.
  signBare (pk,sk) (Domained (domainOf (Nothing @AuthorContent)) (12345 :: Word64))

-- A group secret is raw key bytes of a fixed size, and the constructor checks
-- only that; telling the parts secret from the message secret is what the
-- bridge's 'poMessage' is for.
secret32 :: PartSecret
secret32 = fromMaybe (error "bad fixture secret")
             (mkPartSecret (BS.replicate typicalKeyLength 0x41))

spec :: Spec
spec = do

  describe "PEP-19 fold" $ do

    it "is deterministic under reordering, whole result and all" $ do
      owner <- kp
      alice <- kp
      bob <- kp
      part <- someHash
      origin <- someHash
      -- The existing determinism test compares a projection of frThreads on
      -- three events. This one compares everything the fold returns, on a log
      -- that produces drops, anomalies, parts and a redaction, so a field
      -- added later is covered without anyone remembering to add it.
      let repo = fst owner
          eDel = mkEvent owner owner (ADelegate (fst bob) 1) (canon 1 Nothing)
          eOpen = mkEvent alice owner
                    (AOpen repo HubIssue "t" [] Nothing (Just part) Nothing 2)
                    (\e -> CanonContent e 2 (Just 1) (Just origin) 2 (Just secret32))
          tid = eventId eOpen
          eCmt = mkEvent alice bob (AComment tid Nothing (Just "hi") Nothing 3)
                   (\e -> CanonContent e 3 Nothing (Just origin) 3 Nothing)
          eBad = mkEvent alice alice (AComment tid Nothing (Just "unblessed") Nothing 4)
                   (canon 4 Nothing)
          eRed = mkEvent owner owner (ARedact (eventId eCmt) 5) (canon 5 Nothing)
          evs = [eDel, eOpen, eCmt, eBad, eRed]
          full fr = ( summary fr, frDropped fr, frAnomalies fr
                    , frRedacted fr, frParts fr, frMaxSeq fr, frMaxNumber fr
                    , frLastFolded fr, frOrigins fr, frMaintainers fr
                    , frAdmitted fr )
      full (foldEvents repo evs) `shouldBe` full (foldEvents repo (reverse evs))
      full (foldEvents repo evs) `shouldBe` full (foldEvents repo (rotate evs))
      -- ...and the log is actually exercising all of it
      frDropped (foldEvents repo evs) `shouldSatisfy` (not . null)
      frAnomalies (foldEvents repo evs) `shouldSatisfy` (not . null)

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
                  (\e -> CanonContent e 1 (Just 1) Nothing 1 (Just secret32))
          eC = mkEvent alice owner (AComment (eventId ePR) Nothing Nothing (Just cmt) 2)
                 (\e -> CanonContent e 2 Nothing Nothing 2 (Just secret32))
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
      -- ...which needs the prefix to be fixed width. The name as a whole is
      -- not, since a base58 hash is 43 or 44 characters.
      take 20 (eventFileName 1 (eventId e1)) `shouldBe` "00000000000000000001"
      take 21 (eventFileName 10 (eventId e2)) `shouldBe` "00000000000000000010-"

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

    it "keeps the frozen encoding frozen" $ do
      -- Types.hs declares this encoding append-only and permanent four times
      -- over, and every other test signs and unboxes through the same pair of
      -- functions, which move together: reordering a constructor or inserting
      -- a field mid-record rewrites every event-id and invalidates every
      -- signature ever made, and nothing would notice. These bytes are what
      -- notices. If one of them changes, either the change is wrong or the
      -- consensus version has to be bumped and the old canon migrated.
      let h = HashRef (hashObject ("hbs2-hub golden fixture" :: ByteString)) :: HashRef
          hx :: Serialise a => a -> String
          hx = B8.unpack . B16.encode . LBS.toStrict . serialise
          author = domainOf (Nothing @AuthorContent)
          canonD = domainOf (Nothing @CanonContent)
      -- The fixture hash itself, so a change in the hashing is caught here
      -- rather than as an unexplained diff in the bytes below.
      show (pretty h) `shouldBe` "2v3ubvkrQaWzhBCZW14JDWUW1LGBMnkirWeJJvZxnNUV"
      -- Domain constants: assigned once, never reused, never renumbered.
      (author, canonD) `shouldBe` (0x48423241, 0x48423243)
      hx (Domained author (ARedact h 5)) `shouldBe`
        "83001a4842324183078200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a795726605"
      hx (Domained author (AComment h (Just h) (Just "hi") Nothing 7)) `shouldBe`
        "83001a4842324186018200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a7957266818200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a7957266816268698007"
      hx (Domained canonD (CanonContent h 3 (Just 9) (Just h) 11 (Nothing :: Maybe PartSecret))) `shouldBe`
        "83001a4842324387008200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a7957266038109818200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a79572660b80"
      -- A fully populated open with full coordinates: the two records most
      -- likely to gain a field, and the one place PRCoords is pinned at all.
      let key = fromMaybe (error "fixture key")
                  (fromStringMay "5xhFoc2rEnV87GrAZzkTNGppBNKuAft363uR12Bfbqu8")
          prc = PRCoords (Just "hbs23://f") "refs/heads/f" "aaaa"
                         "refs/heads/master" "bbbb" (Just h)
      hx (Domained author (AOpen key HubPR "title" ["bug","ui"] (Just "body") (Just h) (Just prc) 42))
        `shouldBe` "83001a4842324189008200582049b3357d33655fdfbf481befe9c4e16e1ab94ae85575f0840ece3b3dfa439abf8101657469746c659f63627567627569ff8164626f6479818200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a7957266818700816968627332333a2f2f666c726566732f68656164732f66646161616171726566732f68656164732f6d61737465726462626262818200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a7957266182a"
      -- ...and the two tags at the end of the sum, where a reordering would
      -- otherwise be invisible.
      hx (Domained author (ADelegate key 3)) `shouldBe`
        "83001a4842324183088200582049b3357d33655fdfbf481befe9c4e16e1ab94ae85575f0840ece3b3dfa439abf03"
      hx (Domained author (ARevoke key 4)) `shouldBe`
        "83001a4842324183098200582049b3357d33655fdfbf481befe9c4e16e1ab94ae85575f0840ece3b3dfa439abf04"

    it "refuses a folded-ts at the top of the range" $ do
      owner <- kp
      alice <- kp
      -- The next stamp is clamped to be no lower than this one, so admitting
      -- maxBound pins every later event to it, on every node, for good, and
      -- every time the render contract shows comes from this field.
      let repo = fst owner
          ev = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                 (canonAt 1 (Just 1) maxBound)
      reasons (foldEvents repo [ev]) `shouldBe` [BadStamp]

    it "counts a dropped event's stamp so it cannot be handed out twice" $ do
      owner <- kp
      bob <- kp
      alice <- kp
      -- Admission is not final. A comment dropped because its signer had been
      -- revoked becomes admissible the moment a later delegate restores them,
      -- so a seq that was never counted gets handed to something else and
      -- canon ends up with two events at one seq.
      let repo = fst owner
          eDel = mkEvent owner owner (ADelegate (fst bob) 1) (canon 1 Nothing)
          eOpen = mkEvent alice bob (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 2)
                    (canon 2 (Just 1))
          eRev = mkEvent owner owner (ARevoke (fst bob) 3) (canon 3 Nothing)
          -- bob races the revoke and loses
          eLate = mkEvent alice bob (AComment (eventId eOpen) Nothing (Just "late") Nothing 4)
                    (canon 4 Nothing)
          fr = foldEvents repo [eDel, eOpen, eRev, eLate]
      reasons fr `shouldBe` [UnauthorizedCanon]
      -- the dropped event's seq is spent: the next mint is 5, not 4
      cursorFrom fr `shouldBe` CanonCursor 5 2

    it "reports a key published for nothing" $ do
      owner <- kp
      alice <- kp
      -- Not a leak now that the parts have a secret of their own, but a key
      -- put in public canon for an event with nothing to unlock.
      let repo = fst owner
          ev = mkEvent alice owner (AOpen repo HubIssue "t" [] (Just "inline") Nothing Nothing 1)
                 (\e -> CanonContent e 1 (Just 1) Nothing 1 (Just secret32))
          fr = foldEvents repo [ev]
      frDropped fr `shouldBe` []
      map snd (frAnomalies fr) `shouldBe` [SecretWithoutPart]

    it "refuses an attribute name that is not a name" $ do
      -- Checking the case alone closed one spelling out of an infinite family:
      -- every one of these is a separate attribute that no set canonicalization
      -- touches, that the render contract never reads, and that nothing can
      -- delete, since the ops only insert.
      validAttrName "labels" `shouldBe` True
      validAttrName "needs-triage" `shouldBe` True
      validAttrName "x1" `shouldBe` True
      mapM_ (\k -> (k, validAttrName k) `shouldBe` (k, False))
        ["Labels", "labels ", " labels", "la bels", "", "1label", "labels\n", "labels,"]
      normalizedAttr "labels " "bug" `shouldBe` False
      -- and a value that is a set is still canonicalized under a valid name
      normalizedAttr "labels" "ui,bug" `shouldBe` False

    it "refuses to build a part-secret from the wrong number of bytes" $ do
      mkPartSecret "abc" `shouldBe` Nothing
      mkPartSecret BS.empty `shouldBe` Nothing
      fmap usablePartSecret (mkPartSecret (BS.replicate typicalKeyLength 0x41))
        `shouldBe` Just True

    it "reports a part-secret that cannot be a key" $ do
      owner <- kp
      alice <- kp
      part <- someHash
      -- mkPartSecret guards the writing side and Serialise walks straight past
      -- it, so canon somebody else wrote can carry anything. Dropping the event
      -- would be wrong (the event is intact, the key is not), which leaves
      -- reporting it as the only way hub verify can see it.
      let repo = fst owner
          stunted = fromMaybe (error "no decode")
                      (decodeStrict (LBS.toStrict (serialise ("abc" :: ByteString))))
          ev = mkEvent alice owner
                 (AOpen repo HubIssue "t" [] Nothing (Just part) Nothing 1)
                 (\e -> CanonContent e 1 (Just 1) Nothing 1 (Just stunted))
          fr = foldEvents repo [ev]
      frDropped fr `shouldBe` []
      map snd (frAnomalies fr) `shouldBe` [UnusablePartSecret]

    it "refuses a signature made for another kind of record" $ do
      owner <- kp
      alice <- kp
      target <- someHash
      -- Ed25519 signs bytes, not types. Without a domain inside the signed
      -- bytes, any record of shape [small int, hash, int] signed by the owner
      -- for any purpose is byte for byte a signed redaction: a constructor tag
      -- is an ordinary small CBOR integer.
      let repo = fst owner
          -- byte for byte serialise (ARedact target 5) before the tag existed
          lookalike = signBare owner ((7 :: Word64), target, (5 :: Word64))
          real = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                         (canon 1 (Just 1))
          ev = Event lookalike (evCanonBox real)
      case unboxChecked (evAuthorBox ev) of
        Left (BoxUndecodable k why) -> do
          k `shouldBe` fst owner
          -- The bytes no longer even parse as an author box: the tag is not
          -- there, so the array is the wrong length.
          why `shouldBe` Undecodable
        other -> expectationFailure ("expected a refusal: " <> show (fmap fst other))
      -- And the case the tag exists for: bytes that DO decode as an author
      -- box, tagged for the other domain. That is what a shape collision
      -- between two signed records looks like once both carry a tag, and it is
      -- the only way this can be caught, since the payload parses perfectly.
      let crossed = signBare owner
                      (Domained (domainOf (Nothing @CanonContent)) (ARedact target 5))
      case unboxChecked (crossed :: SignedBox AuthorContent HubScheme) of
        Left (BoxUndecodable _ why) -> why `shouldBe` WrongDomain
        other -> expectationFailure ("expected a domain refusal: " <> show (fmap fst other))
      -- The fold reports it as an undecodable author box, not as a forgery.
      map snd (frDropped (foldEvents repo [ev]))
        `shouldBe` [UndecodableAuthor (fst owner) Undecodable]

    it "refuses a mangled signature rather than a mangled payload" $ do
      owner <- kp
      alice <- kp
      -- An event-id hashes the whole box, signature included, so a scheme
      -- whose signatures can be mutated and still verify breaks dedup by
      -- event-id: one event would have two ids. Ed25519 as libsodium
      -- implements it rejects both the high-bit and the +1 variants, and any
      -- replacement (PEP-13) has to as well.
      let repo = fst owner
          ev = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                       (canon 1 (Just 1))
      unboxChecked (mangleSig (evAuthorBox ev)) `shouldSatisfy` \case
        Left BoxBadSig -> True
        _              -> False
      map snd (frDropped (foldEvents repo [Event (mangleSig (evAuthorBox ev)) (evCanonBox ev)]))
        `shouldBe` [BadAuthorSig]

    it "reports trailing bytes in a box as tampering, not as a newer schema" $ do
      owner <- kp
      alice <- kp
      -- Produced the way an attacker would: the payload is a valid encoding
      -- with junk appended, and the signature covers the junk, so the only
      -- thing that catches it is the decoder refusing to stop early.
      let repo = fst owner
          content = AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1
          padded = rawBox alice (domainedBytes content <> "!")
          real = mkEvent alice owner content (canon 1 (Just 1))
      case unboxChecked padded of
        Left (BoxUndecodable k why) -> do
          k `shouldBe` fst alice
          why `shouldBe` TrailingData
        other -> expectationFailure ("expected trailing data: " <> show (fmap fst other))
      map snd (frDropped (foldEvents repo [Event padded (evCanonBox real)]))
        `shouldBe` [UndecodableAuthor (fst alice) TrailingData]

    it "refuses to fold canon from a newer consensus version" $ do
      owner <- kp
      alice <- kp
      -- The admission rules ARE the format, so folding canon written under
      -- rules this build does not know produces a view that quietly disagrees
      -- with every up-to-date clone.
      let repo = fst owner
          ev = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                       (canon 1 (Just 1))
      either Just (const Nothing) (foldCanon (hubMetaVersion + 1) hubEventVersion repo [ev])
        `shouldBe` Just (MetaTooNew (hubMetaVersion + 1))
      -- ...and separately for the file version, which answers a different
      -- question: a reader can know the admission rules and still not know the
      -- shape of the file an event is in.
      either Just (const Nothing) (foldCanon hubMetaVersion (hubEventVersion + 1) repo [ev])
        `shouldBe` Just (EventTooNew (hubEventVersion + 1))
      either (const []) (HM.keys . frThreads) (foldCanon hubMetaVersion hubEventVersion repo [ev])
        `shouldBe` [eventId ev]

    it "keeps a thread's state moving through merge and reopen" $ do
      owner <- kp
      alice <- kp
      bundle <- someHash
      -- The state machine nobody had walked end to end: revise and reopen
      -- after a merge, and a redact of the opening event.
      let repo = fst owner
          ePR = mkEvent alice owner
                  (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords { prBundle = Just bundle }) 1)
                  (\e -> CanonContent e 1 (Just 1) Nothing 1 (Just secret32))
          tid = eventId ePR
          eMerge = mkEvent owner owner (AMerge tid "cafe" "refs/heads/master" 2) (canon 2 Nothing)
          -- a revise after the merge is still admitted: PEP-19 has no
          -- terminal state, and the coordinates are the author's to update
          eRev = mkEvent alice owner (ARevise tid coords { prSourceTip = "cccc" } 3)
                   (\e -> CanonContent e 3 Nothing Nothing 3 Nothing)
          eReopen = mkEvent owner owner (AReopen tid Nothing 4) (canon 4 Nothing)
          eRed = mkEvent owner owner (ARedact tid 5) (canon 5 Nothing)
          fr = foldEvents repo [ePR, eMerge, eRev, eReopen, eRed]
          t = threadOf fr tid
      frDropped fr `shouldBe` []
      -- reopen wins over merge, because last writer wins on the attribute
      HM.lookup "status" (tsAttrs t) `shouldBe` Just "open"
      -- ...but the merge result is still recorded
      fmap psMerge (tsPR t) `shouldBe` Just (Just ("cafe","refs/heads/master"))
      fmap (prSourceTip . psCoords) (tsPR t) `shouldBe` Just "cccc"
      -- and the redacted open is visible as such on the thread, not only in a
      -- flat set the renderer has to join against
      tsRedacted t `shouldBe` True
      HS.member tid (frRedacted fr) `shouldBe` True

    it "marks a redacted comment on the comment itself" $ do
      owner <- kp
      alice <- kp
      -- What a redaction is usually FOR. A renderer that forgot to join
      -- against the flat set would publish the withdrawn text in every clone
      -- while the redact reported success.
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                          (canon 1 (Just 1))
          tid = eventId eOpen
          keep' = mkEvent alice owner (AComment tid Nothing (Just "fine") Nothing 2)
                    (canon 2 Nothing)
          gone = mkEvent alice owner (AComment tid Nothing (Just "abuse") Nothing 3)
                   (canon 3 Nothing)
          eRed = mkEvent owner owner (ARedact (eventId gone) 4) (canon 4 Nothing)
          fr = foldEvents repo [eOpen, keep', gone, eRed]
          t = threadOf fr tid
      frDropped fr `shouldBe` []
      map cRedacted (tsComments t) `shouldBe` [False, True]
      tsRedacted t `shouldBe` False

    it "names who supplied the current pr coordinates" $ do
      owner <- kp
      alice <- kp
      bob <- kp
      -- PEP-19 lets a maintainer revise someone else's PR, so the tip a
      -- reviewer is looking at may have been chosen by somebody other than the
      -- author the thread is displayed under.
      let repo = fst owner
          eDel = mkEvent owner owner (ADelegate (fst bob) 1) (canon 1 Nothing)
          ePR = mkEvent alice owner (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords) 2)
                  (canon 2 (Just 1))
          tid = eventId ePR
          eRev = mkEvent bob bob (ARevise tid coords { prSource = Just "hbs23://elsewhere" } 3)
                   (canon 3 Nothing)
          fr = foldEvents repo [eDel, ePR, eRev]
          t = threadOf fr tid
      frDropped fr `shouldBe` []
      tsAuthor t `shouldBe` fst alice
      fmap psAuthor (tsPR t) `shouldBe` Just (fst bob)
      fmap psCanonBy (tsPR t) `shouldBe` Just (fst bob)

    it "tells apart which of the two boxes it could not read" $ do
      owner <- kp
      alice <- kp
      -- Split by box because which side is from the future is the useful part:
      -- an author box this reader cannot decode is a newer sender, a canon box
      -- it cannot decode is a newer publisher.
      let repo = fst owner
          real = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                         (canon 1 (Just 1))
          futureCanon = signBare owner
                          (Domained (domainOf (Nothing @CanonContent)) (12345 :: Word64))
          ev = Event (evAuthorBox real) futureCanon
      map snd (frDropped (foldEvents repo [ev]))
        `shouldBe` [UndecodableCanon (fst owner) Undecodable]

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
