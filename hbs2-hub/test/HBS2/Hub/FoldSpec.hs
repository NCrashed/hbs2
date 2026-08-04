module HBS2.Hub.FoldSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Hub.Bridge (cursorFrom,CanonCursor(..))
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.SignedBox

import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Control.Monad (forM_)
import Data.Either (isLeft,isRight)
import Data.List (isInfixOf,sortOn)
import HBS2.Base58 (AsBase58(..))
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
canonAt :: RepoRef -> Word64 -> Maybe Word64 -> Word64 -> EventId -> CanonContent
canonAt repo sq num folded eid = CanonContent repo eid sq num Nothing Nothing folded Nothing

canon :: RepoRef -> Word64 -> Maybe Word64 -> EventId -> CanonContent
canon repo sq num = canonAt repo sq num sq

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
reasons = map drWhy . frDropped

-- Fetch a thread the test asserts must exist.
threadOf :: FoldResult -> ThreadId -> ThreadState
threadOf fr tid = fromMaybe (error "expected thread") (HM.lookup tid (frThreads fr))

-- A hash no event in the fixture has.
someHash :: IO HashRef
someHash = do
  (pk,sk) <- kp
  pure (authorBoxId (signAuthor pk sk (ARevoke pk pk 0)))

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

-- An event from a newer schema: an author box this build cannot decode, and a
-- canon box that blesses that box's own id.
--
-- The binding matters even here, and especially here. The event-id is the hash
-- of the serialised author box, so it is computable without decoding the box,
-- and a stamp counted without checking it would let anyone lift a canon box off
-- one event, staple it to an undecodable box of their own, and spend the seq
-- it names.
ghostEvent :: KP -> KP -> (EventId -> CanonContent) -> Event
ghostEvent alice (cpk,csk) mkCanon =
  let abox = futureBox alice
      eid  = HashRef (hashObject (serialise abox))
  in Event abox (signCanon cpk csk (mkCanon eid))

-- A group secret is raw key bytes of a fixed size, and the constructor checks
-- only that; telling the parts secret from the message secret is what the
-- bridge's 'poMessage' is for.
-- A part reference whose proof really holds. PEP-18 binds a part to its author,
-- so a valid proof is part of a well-formed event now; the fixtures that never
-- reach the check use 'unproven'.
proven :: (HubKey, PrivKey 'Sign HubScheme) -> HashRef -> PartRef
proven who h = PartRef h (partProofFor h secret32 (fst who))

unproven :: HashRef -> PartRef
unproven h = PartRef h (PartProof h)

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
          eDel = mkEvent owner owner (ADelegate repo (fst bob) 1) (canon repo 1 Nothing)
          eOpen = mkEvent alice owner
                    (AOpen repo HubIssue "t" [] Nothing (Just (proven alice part)) Nothing 2)
                    (\e -> CanonContent repo e 2 (Just 1) (Just origin) Nothing 2 (Just secret32))
          tid = eventId eOpen
          eCmt = mkEvent alice bob (AComment tid Nothing (Just "hi") Nothing 3)
                   (\e -> CanonContent repo e 3 Nothing (Just origin) Nothing 3 Nothing)
          eBad = mkEvent alice alice (AComment tid Nothing (Just "unblessed") Nothing 4)
                   (canon repo 4 Nothing)
          eRed = mkEvent owner owner (ARedact repo (eventId eCmt) 5) (canon repo 5 Nothing)
          evs = [eDel, eOpen, eCmt, eBad, eRed]
          -- frHonoured and frOwner belong here as much as the rest: the first
          -- is what the bridge reads to refuse a request carried out twice,
          -- and a whole-result comparison that leaves out the field deciding
          -- that is a comparison of the convenient corner this test exists to
          -- stop being.
          full fr = ( summary fr, frDropped fr, frAnomalies fr
                    , frRedacted fr, frParts fr, frMaxSeq fr, frMaxNumber fr
                    , frLastFolded fr, frOrigins fr, frMaintainers fr
                    , frAdmitted fr, frLog fr, frHonoured fr, frOwner fr )
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
                          (canon repo 1 (Just 1))
          tid   = eventId eOpen
          eCmt  = mkEvent alice owner (AComment tid Nothing (Just "hi") Nothing 200)
                          (canon repo 2 Nothing)
          eSet  = mkEvent owner owner (AClose tid Nothing 300)
                          (canon repo 3 Nothing)
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
                         (canon repo 1 (Just 1))
          bad2 = mkEvent alice owner (AOpen other HubIssue "y" [] Nothing Nothing Nothing 2)
                         (canon repo 2 (Just 2))
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
                   (AOpen repo HubIssue n [] Nothing Nothing Nothing 1) (canon repo 1 (Just 1))
          forged1 = Event (evAuthorBox (mk "a1")) (evCanonBox (mk "a2"))  -- IdMismatch
          forged2 = Event (evAuthorBox (mk "b1")) (evCanonBox (mk "b2"))  -- IdMismatch
          x = frDropped (foldEvents repo [forged1, forged2])
          y = frDropped (foldEvents repo [forged2, forged1])
      map drWhy x `shouldBe` [IdMismatch, IdMismatch]
      x `shouldBe` y

    it "drops a comment on a non-admitted thread (dangling)" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          -- a real, well-formed open that we deliberately never fold
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                          (canon repo 1 (Just 1))
          orphan = eventId eOpen
          eCmt = mkEvent alice owner (AComment orphan Nothing (Just "orphan") Nothing 2)
                         (canon repo 2 Nothing)
          fr = foldEvents repo [eCmt]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [BadThread]

    it "hides a redacted event and ignores an unknown redact target" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                          (canon repo 1 (Just 1))
          tid = eventId eOpen
          eCmt = mkEvent alice owner (AComment tid Nothing (Just "oops, a secret") Nothing 2)
                         (canon repo 2 Nothing)
          cid = eventId eCmt
          eRed = mkEvent owner owner (ARedact repo cid 3) (canon repo 3 Nothing)
          -- a redact naming an event that was never admitted
          ghost = mkEvent alice owner (AComment tid Nothing (Just "never folded") Nothing 4)
                          (canon repo 4 Nothing)
          eGhost = mkEvent owner owner (ARedact repo (eventId ghost) 5) (canon repo 5 Nothing)
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
                          (canon repo 1 (Just 1))
          tid = eventId eOpen
          eCmt = mkEvent alice owner (AComment tid Nothing (Just "hi") Nothing 2)
                         (canon repo 2 Nothing)
          cid = eventId eCmt
          -- authored by alice, blessed by the owner
          eRed = mkEvent alice owner (ARedact repo cid 3) (canon repo 3 Nothing)
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
                 (\e -> CanonContent repo e 1 (Just 5) (Just origin) Nothing 5000 Nothing)
          -- number goes backwards, folded-ts goes backwards, and the same
          -- letter is folded a second time
          o2 = mkEvent alice owner (AOpen repo HubIssue "b" [] Nothing Nothing Nothing 2)
                 (\e -> CanonContent repo e 2 (Just 3) (Just origin) Nothing 4000 Nothing)
          -- ...and this one names an encrypted part with no key to it
          o3 = mkEvent alice owner (AOpen repo HubIssue "c" [] Nothing (Just (unproven part)) Nothing 3)
                 (\e -> CanonContent repo e 3 (Just 9) Nothing Nothing 6000 Nothing)
          -- an unnormalized multi-valued attribute: the same two labels in
          -- another order are other bytes and so another event-id
          o4 = mkEvent owner owner (ASet (eventId o1) "labels" "ui,bug" 4) (canonAt repo 4 Nothing 7000)
          fr = foldEvents repo [o1,o2,o3,o4]
      frDropped fr `shouldBe` []
      map anWhat (frAnomalies fr) `shouldBe`
        [ NumberWentBack 5 3, FoldedTsWentBack 5000 4000, DupOrigin origin
        , PartWithoutSecret
        , UnnormalizedAttr "labels"
        ]
      -- in seq order, so a report reads like the log
      map anEvent (frAnomalies fr) `shouldBe` [eventId o2, eventId o2, eventId o2, eventId o3, eventId o4]

    it "reports a duplicate seq and a duplicate number" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          a = mkEvent alice owner (AOpen repo HubIssue "a" [] Nothing Nothing Nothing 1)
                (canon repo 1 (Just 1))
          b = mkEvent alice owner (AOpen repo HubIssue "b" [] Nothing Nothing Nothing 2)
                (canon repo 1 (Just 1))
          fr = foldEvents repo [a,b]
      frDropped fr `shouldBe` []
      map anWhat (frAnomalies fr) `shouldSatisfy` \as ->
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
                  (AOpen repo HubPR "pr" [] Nothing (Just (proven alice body))
                     (Just coords { prBundle = Just (proven alice bundle) }) 1)
                  (\e -> CanonContent repo e 1 (Just 1) Nothing Nothing 1 (Just secret32))
          eC = mkEvent alice owner (AComment (eventId ePR) Nothing Nothing (Just (proven alice cmt)) 2)
                 (\e -> CanonContent repo e 2 Nothing Nothing Nothing 2 (Just secret32))
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
      let repo = fst owner
          ac n = AOpen repo HubIssue n [] Nothing Nothing Nothing 1
          e1 = mkEvent alice owner (ac "a") (canon repo 1 (Just 1))
          e2 = mkEvent alice owner (ac "b") (canon repo 10 (Just 2))
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
          e1 = mkEvent alice owner ac (canonAt repo 1 (Just 1) 1111)
          e2 = mkEvent alice owner ac (canonAt repo 1 (Just 7) 2222)
          tid = eventId e1
          shown fr = ( fmap tsNumber (HM.lookup tid (frThreads fr))
                     , fmap tsCreated (HM.lookup tid (frThreads fr))
                     , frMaxNumber fr
                     , map drWhy (frDropped fr)
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
          key = fromMaybe (error "fixture key")
                  (fromStringMay "5xhFoc2rEnV87GrAZzkTNGppBNKuAft363uR12Bfbqu8")
          prc = PRCoords (Just "hbs23://f") "refs/heads/f" "aaaa"
                         "refs/heads/master" "bbbb" (Just (unproven h))
      -- The fixture hash itself, so a change in the hashing is caught here
      -- rather than as an unexplained diff in the bytes below.
      show (pretty h) `shouldBe` "2v3ubvkrQaWzhBCZW14JDWUW1LGBMnkirWeJJvZxnNUV"
      -- Domain constants: assigned once, never reused, never renumbered.
      (author, canonD) `shouldBe` (0x48423241, 0x48423243)
      hx (Domained author (ARedact key h 5)) `shouldBe`
        "83001a4842324184078200582049b3357d33655fdfbf481befe9c4e16e1ab94ae85575f0840ece3b3dfa439abf8200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a795726605"
      hx (Domained author (AComment h (Just h) (Just "hi") Nothing 7)) `shouldBe`
        "83001a4842324186018200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a7957266818200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a7957266816268698007"
      hx (Domained canonD (CanonContent key h 3 (Just 9) (Just h) Nothing 11 (Nothing :: Maybe PartSecret))) `shouldBe`
        "83001a4842324389008200582049b3357d33655fdfbf481befe9c4e16e1ab94ae85575f0840ece3b3dfa439abf8200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a7957266038109818200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a7957266800b80"
      -- A fully populated open with full coordinates: the two records most
      -- likely to gain a field, and the one place PRCoords is pinned at all.
      hx (Domained author (AOpen key HubPR "title" ["bug","ui"] (Just "body") (Just (unproven h)) (Just prc) 42))
        `shouldBe` "83001a4842324189008200582049b3357d33655fdfbf481befe9c4e16e1ab94ae85575f0840ece3b3dfa439abf8101657469746c659f63627567627569ff8164626f64798183008200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a795726682008200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a7957266818700816968627332333a2f2f666c726566732f68656164732f66646161616171726566732f68656164732f6d617374657264626262628183008200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a795726682008200820058201c72c1b65434e182da5ebcf1af45322b2afd989579158a86a54c2409a7957266182a"
      -- ...and the two tags at the end of the sum, where a reordering would
      -- otherwise be invisible.
      hx (Domained author (ADelegate key key 3)) `shouldBe`
        "83001a4842324184088200582049b3357d33655fdfbf481befe9c4e16e1ab94ae85575f0840ece3b3dfa439abf8200582049b3357d33655fdfbf481befe9c4e16e1ab94ae85575f0840ece3b3dfa439abf03"
      hx (Domained author (ARevoke key key 4)) `shouldBe`
        "83001a4842324184098200582049b3357d33655fdfbf481befe9c4e16e1ab94ae85575f0840ece3b3dfa439abf8200582049b3357d33655fdfbf481befe9c4e16e1ab94ae85575f0840ece3b3dfa439abf04"

    it "refuses a folded-ts past the ceiling" $ do
      owner <- kp
      alice <- kp
      -- The next stamp is clamped to be no lower than this one, so a stamp
      -- pins every later event to it, on every node, and every time the render
      -- contract shows comes from this field. The top of the range is not a
      -- special case here the way it is for the two counters: those wrap, this
      -- one is carried forward by 'max' from anywhere in the range, so what
      -- bounds it is a ceiling.
      let repo = fst owner
          at t = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                   (canonAt repo 1 (Just 1) t)
      reasons (foldEvents repo [at maxBound]) `shouldBe` [FoldedTsAboveCeiling]
      reasons (foldEvents repo [at (maxFoldedTs + 1)]) `shouldBe` [FoldedTsAboveCeiling]
      -- and the ceiling itself is admitted, so the bridge's gate and this one
      -- are the same boundary
      reasons (foldEvents repo [at maxFoldedTs]) `shouldBe` []

    it "spends the seq of an event from a newer schema" $ do
      owner <- kp
      alice <- kp
      -- The extension path PEP-19 designs for: a new op is an append-only
      -- addition to AuthorContent, which is exactly what an older build cannot
      -- decode. The canon box is a separate signature over separate bytes and
      -- the seq is in that one, so the stamp is still readable and still
      -- spent. A build that skipped it would mint into that seq, and the newer
      -- build reading the result would find two events there with every LWW
      -- attribute between them settled by a hash instead of by time.
      let repo = fst owner
          ev = ghostEvent alice owner (canon repo 9 (Just 3))
          fr = foldEvents repo [ev]
      map drWhy (frDropped fr) `shouldBe` [UndecodableAuthor (fst alice) Undecodable]
      -- the seq, and with it the clock; not the number, which is spent on
      -- admission and would otherwise be counted by the build that cannot read
      -- the event and refused by the build that can
      cursorFrom fr `shouldBe` CanonCursor 10 1
      -- and not the clock, which is the floor the next mint is clamped to: a
      -- refused file stamped at the ceiling would pin every later stamp in the
      -- repository to the year 2100
      frLastFolded fr `shouldBe` 0
      -- ...and nothing is spent when the canon box is not readable either:
      -- then there is no stamp, only bytes claiming to be one.
      let unreadable = Event (evAuthorBox ev) (mangleSig (evCanonBox ev))
          fr2 = foldEvents repo [unreadable]
      map drWhy (frDropped fr2) `shouldBe` [UndecodableAuthor (fst alice) Undecodable]
      cursorFrom fr2 `shouldBe` CanonCursor 1 1
      -- ...nor when the stamp is real but blesses another event: a canon box
      -- lifted off a good event and stapled to an undecodable box of one's own
      -- is the cheapest way to spend a seq, since the id is the hash of the
      -- author box and cannot be forged to match.
      let real = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                         (canon repo 9 (Just 3))
          stapled = Event (futureBox alice) (evCanonBox real)
          fr3 = foldEvents repo [stapled]
      map drWhy (frDropped fr3) `shouldBe` [UndecodableAuthor (fst alice) Undecodable]
      cursorFrom fr3 `shouldBe` CanonCursor 1 1

    it "counts a dropped event's stamp so it cannot be handed out twice" $ do
      owner <- kp
      alice <- kp
      -- Admission is not final: canon arrives a file at a time, and a reply
      -- dropped as dangling is admitted the moment the open it names turns up.
      -- And even where it is final the file sits at that seq, so minting into
      -- it leaves two events there for good, with every LWW attribute between
      -- them settled by a hash instead of by time.
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                    (canon repo 1 (Just 1))
          -- a comment on a thread this fold has not got (yet)
          eDangling = mkEvent alice owner
                        (AComment (eventId eOpen) Nothing (Just "early") Nothing 2)
                        (canon repo 9 Nothing)
          fr = foldEvents repo [eDangling]
      reasons fr `shouldBe` [BadThread]
      -- the dropped event's seq is spent: the next mint is 10, not 1
      cursorFrom fr `shouldBe` CanonCursor 10 1

    it "does not let a key canon never authorized move the cursor" $ do
      owner <- kp
      alice <- kp
      mallory <- kp
      -- The other half of the same rule, and the reason it is not simply "every
      -- event in the tree". A stamp counts because a key this log authorized
      -- wrote it; counting the rest would let anyone who can get one file into
      -- one clone push the cursor to the top of its range, and the owner would
      -- then be unable to mint even the revoke that would stop it.
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                    (canon repo 1 (Just 1))
          intruder = mkEvent alice mallory
                       (AComment (eventId eOpen) Nothing (Just "hello") Nothing 2)
                       (canonAt repo (maxBound - 1) Nothing 2)
          fr = foldEvents repo [eOpen, intruder]
      reasons fr `shouldBe` [UnauthorizedCanon]
      cursorFrom fr `shouldBe` CanonCursor 2 2

    it "does not let a refused file move the clock, or the anomaly" $ do
      owner <- kp
      alice <- kp
      -- Two questions that used to share one number. The floor the next mint is
      -- clamped to is a high-water mark; whether the clock went backwards is
      -- asked of the previous ADMITTED event. Sharing them meant a refused file
      -- raised the bar, so a strictly increasing log reported itself as going
      -- backwards, and a file stamped at the ceiling pinned every future stamp
      -- in the repository to the year 2100.
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                    (canonAt repo 1 (Just 1) 5000)
          tid = eventId eOpen
          -- refused: nobody authorized this key
          eBad = mkEvent alice alice (AComment tid Nothing (Just "no") Nothing 2)
                   (canonAt repo 2 Nothing maxFoldedTs)
          eCmt = mkEvent alice owner (AComment tid Nothing (Just "yes") Nothing 3)
                   (canonAt repo 3 Nothing 6000)
          fr = foldEvents repo [eOpen, eBad, eCmt]
      reasons fr `shouldBe` [UnauthorizedCanon]
      map anWhat (frAnomalies fr) `shouldBe` []
      frLastFolded fr `shouldBe` 6000

    it "remembers a number it could not read, so the collision is visible" $ do
      owner <- kp
      alice <- kp
      -- The build that cannot decode the content does not spend the number,
      -- which is what keeps it minting the same numbers as the build that can.
      -- Silence about the collision is a different thing, and not wanted: the
      -- number was seen, so handing it out again is reportable.
      let repo = fst owner
          ghost = ghostEvent alice owner (canon repo 1 (Just 3))
          mine = mkEvent alice owner (AOpen repo HubIssue "mine" [] Nothing Nothing Nothing 2)
                   (canon repo 2 (Just 3))
          fr = foldEvents repo [ghost, mine]
      map drWhy (frDropped fr) `shouldBe` [UndecodableAuthor (fst alice) Undecodable]
      map anWhat (frAnomalies fr) `shouldBe` [DupNumber 3]

    -- The seq half of the test above, which was missing. A ghost SPENT its seq
    -- and remembered nothing about it, so a genuine collision -- a maintainer on
    -- a newer build minting at N, an owner on this one minting at N because its
    -- cursor had not seen that event -- was invisible on THIS build, the one
    -- that can see it from the stamp alone, while the build that can read the
    -- event reports it. Both orders, because the two sort by event-id between
    -- them, which is to say arbitrarily, and an anomaly that depends on that is
    -- one that shows up in half the clones.
    it "reports a seq two events were stamped with, ghost or not" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          ghost = ghostEvent alice owner (canon repo 7 Nothing)
          mine = mkEvent alice owner (AOpen repo HubIssue "mine" [] Nothing Nothing Nothing 2)
                   (canon repo 7 (Just 1))
      map anWhat (frAnomalies (foldEvents repo [ghost, mine])) `shouldBe` [DupSeq 7]
      map anWhat (frAnomalies (foldEvents repo [mine, ghost])) `shouldBe` [DupSeq 7]
      -- And a ghost alone is not a collision with itself.
      map anWhat (frAnomalies (foldEvents repo [ghost])) `shouldBe` []

    -- An origin is a MESSAGE hash and a message can be rewrapped, so the same
    -- request honoured twice under two envelopes has two origins and one
    -- honours-id: DupOrigin cannot see it. The bridge refuses a second honour
    -- only when the first is already in its view, which is exactly the case
    -- that did not happen when two maintainers honoured at once.
    it "reports one request carried out twice, which two origins hide" $ do
      owner <- kp
      alice <- kp
      req <- someHash
      o1 <- someHash
      o2 <- someHash
      let repo = fst owner
          e1 = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                 (canon repo 1 (Just 1))
          closeAt sq o = mkEvent owner owner (AClose (eventId e1) Nothing sq)
                           (\e -> CanonContent repo e sq Nothing (Just o) (Just req) sq Nothing)
          fr = foldEvents repo [e1, closeAt 2 o1, closeAt 3 o2]
      frDropped fr `shouldBe` []
      map anWhat (frAnomalies fr) `shouldBe` [DupHonours req]

    it "does not spend a stamp blessed for another repository" $ do
      ownerB <- kp
      ownerC <- kp
      bob <- kp
      alice <- kp
      -- One person delegated in two repositories is ordinary, and a file they
      -- signed in one is byte-for-byte valid in the other: both signatures
      -- verify and the canon box references its own author box, so the stamp
      -- used to be spent before anything noticed the content belonged
      -- elsewhere. That handed C's high-water mark to B, and a hostile version
      -- of it hands over maxBound, after which B can mint nothing at all.
      let repoB = fst ownerB
          repoC = fst ownerC
          delegB = mkEvent ownerB ownerB (ADelegate repoB (fst bob) 1) (canon repoB 1 Nothing)
          -- bob's event in C, blessed by bob, dropped into B's tree verbatim
          fromC = mkEvent alice bob
                    (AComment (eventId delegB) Nothing (Just "elsewhere") Nothing 2)
                    (canon repoC (maxBound - 99) Nothing)
          fr = foldEvents repoB [delegB, fromC]
      reasons fr `shouldBe` [WrongTarget]
      cursorFrom fr `shouldBe` CanonCursor 2 1

    it "does not spend a ghost's stamp blessed for another repository" $ do
      ownerB <- kp
      ownerC <- kp
      bob <- kp
      alice <- kp
      -- The other half of the repository binding, and the half nothing reached:
      -- on the readable path the event is dropped before the stamp is even
      -- forced, so the target check inside 'spendable' is only ever asked on a
      -- ghost, and both ghost fixtures reused a canon box from their own repo.
      -- This one is a canon box from C, with content this build cannot read.
      let repoB = fst ownerB
          repoC = fst ownerC
          delegB = mkEvent ownerB ownerB (ADelegate repoB (fst bob) 1)
                     (canon repoB 1 Nothing)
          ghost = ghostEvent alice bob (canon repoC 5000 (Just 9))
          fr = foldEvents repoB [delegB, ghost]
      map drWhy (frDropped fr) `shouldBe` [UndecodableAuthor (fst alice) Undecodable]
      cursorFrom fr `shouldBe` CanonCursor 2 1

    it "refuses a redaction authored for another repository" $ do
      owner <- kp
      other <- kp
      alice <- kp
      -- A redact names an event-id and nothing else, so the transitive binding
      -- through a thread does not reach it: without its own target, one signed
      -- by a maintainer of two repos hides an event in whichever holds that id.
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                    (canon repo 1 (Just 1))
          tid = eventId eOpen
          elsewhere = mkEvent owner owner (ARedact (fst other) tid 2) (canon repo 2 Nothing)
          here = mkEvent owner owner (ARedact repo tid 3) (canon repo 3 Nothing)
      reasons (foldEvents repo [eOpen, elsewhere]) `shouldBe` [WrongTarget]
      HS.member tid (frRedacted (foldEvents repo [eOpen, elsewhere])) `shouldBe` False
      reasons (foldEvents repo [eOpen, here]) `shouldBe` []
      HS.member tid (frRedacted (foldEvents repo [eOpen, here])) `shouldBe` True

    it "can no longer be given a box whose key or signature is the wrong length" $ do
      owner <- kp
      -- This used to BEND an event's author box to carry a key of 0, 31 and 5000
      -- bytes and assert the fold dropped each as BadAuthorSig. The bending
      -- worked because a key and a signature were newtypes over bytes with
      -- DERIVED Serialise instances: the encoding is the constructor tag and the
      -- field, so any length of bytes decoded into one, walking straight past the
      -- length check the crypto library does in its own decoder. libsodium then
      -- read its thirty-two and sixty-four bytes regardless, which is a read out
      -- of bounds for a short key and, for a long one, a value that verifies its
      -- owner's signatures and is equal to nothing on any list.
      --
      -- The fixture has no way to exist now. Those instances are hand-written and
      -- run saltine's own decoder, and saltine does not export the constructor
      -- either, so a key of the wrong length cannot be built at all. The
      -- 'wellFormed' guard in 'unboxChecked' stays as defence in depth and is
      -- simply unreachable from the wire; what is testable, and what actually
      -- closed the hole, is that the bytes are refused before anything is made
      -- out of them.
      let bytes n = serialise ((0 :: Word), BS.replicate n 0x41)
      forM_ [0, 31, 5000] $ \n -> do
        CBOR.deserialiseOrFail @HubKey (bytes n) `shouldSatisfy` isLeft
        CBOR.deserialiseOrFail @(Signature HubScheme) (bytes n) `shouldSatisfy` isLeft

      -- And the encoding is otherwise untouched, which it has to be: these bytes
      -- are inside every signature and every ref key derived from a key.
      CBOR.deserialiseOrFail @HubKey (serialise (fst owner)) `shouldSatisfy` isRight
      validHubKey (fst owner) `shouldBe` True

    it "prints a refusal in words, with keys anyone can look up" $ do
      owner <- kp
      alice <- kp
      -- The renderers exist because the derived Show of a key is a short
      -- internal digest, not the base58 an operator can search for, and because
      -- an attribute name is a stranger's bytes on their way to a terminal.
      let repo = fst owner
          ev = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                 (canon repo 1 (Just 1))
          shown = show (pretty (Dropped (eventId ev) (Just 1) (Just (fst owner))
                                 UnauthorizedCanon))
      shown `shouldSatisfy` isInfixOf (show (pretty (AsBase58 (fst owner))))
      shown `shouldSatisfy` isInfixOf "not blessed by an authorized key"
      -- control bytes are made visible rather than passed through. The escape is
      -- delimited (\u{1b}, not \x1b) so that its digits cannot be continued by
      -- the text after it: two digits for one value and four for another is what
      -- let one field print exactly like another. See 'safeText'.
      show (pretty (UnnormalizedAttr "a\ESCb")) `shouldSatisfy` isInfixOf "\\u{1b}"
      show (pretty (UnnormalizedAttr "a\ESCb")) `shouldSatisfy` (not . isInfixOf "\ESC")

    it "does not remember a number from a stamp it will not count" $ do
      owner <- kp
      alice <- kp
      mallory <- kp
      -- The seq has always been recorded only for stamps that count. The
      -- number was not, so one file with an unreadable author box and a number
      -- in its canon box turned the next honest open into a reported anomaly,
      -- with the honest maintainer's key printed beside it.
      let repo = fst owner
          ghost = ghostEvent alice mallory (canon repo 1 (Just 7))
          mine = mkEvent alice owner (AOpen repo HubIssue "mine" [] Nothing Nothing Nothing 2)
                   (canon repo 2 (Just 7))
          fr = foldEvents repo [ghost, mine]
      map drWhy (frDropped fr) `shouldBe` [UndecodableAuthor (fst alice) Undecodable]
      map anWhat (frAnomalies fr) `shouldBe` []

    it "refuses a delegation authored for another repository" $ do
      owner <- kp
      bob <- kp
      other <- kp
      -- An owner with two repositories signs a delegation naming one of them.
      -- Without the target inside the signed content, the same author box is a
      -- valid delegation in the other, and the only thing standing between that
      -- and a maintainer set nobody agreed to is who may write to the tree,
      -- which is exactly the guarantee this document says not to lean on.
      let repo = fst owner
          elsewhere = mkEvent owner owner (ADelegate (fst other) (fst bob) 1)
                        (canon repo 1 Nothing)
          here = mkEvent owner owner (ADelegate repo (fst bob) 2) (canon repo 2 Nothing)
      reasons (foldEvents repo [elsewhere]) `shouldBe` [WrongTarget]
      HS.member (fst bob) (frMaintainers (foldEvents repo [elsewhere]))
        `shouldBe` False
      -- ...and the one that names this repo does what it says
      reasons (foldEvents repo [here]) `shouldBe` []
      HS.member (fst bob) (frMaintainers (foldEvents repo [here])) `shouldBe` True
      -- the same for a revoke, which is the half that would be silently
      -- ineffective rather than silently effective
      let revElsewhere = mkEvent owner owner (ARevoke (fst other) (fst bob) 3)
                           (canon repo 3 Nothing)
      reasons (foldEvents repo [here, revElsewhere]) `shouldBe` [WrongTarget]
      HS.member (fst bob) (frMaintainers (foldEvents repo [here, revElsewhere]))
        `shouldBe` True

    it "spends a withdrawn delegate's stamp only next to the cursor" $ do
      owner <- kp
      bob <- kp
      alice <- kp
      -- The window is a constant that decides whether a revocation is a remedy,
      -- and nothing tested it: raising it to a million left every test green,
      -- because the mutinous fixture sat at the top of the range and the honest
      -- one sat next to the cursor. These two stand on either side of the line.
      let repo = fst owner
          eDel = mkEvent owner owner (ADelegate repo (fst bob) 1) (canon repo 1 Nothing)
          eOpen = mkEvent alice bob (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 2)
                    (canon repo 2 (Just 1))
          eRev = mkEvent owner owner (ARevoke repo (fst bob) 3) (canon repo 3 Nothing)
          late n = mkEvent alice bob
                     (AComment (eventId eOpen) Nothing (Just "late") Nothing 4)
                     (canonAt repo n Nothing 4)
          upTo n = cursorFrom (foldEvents repo [eDel, eOpen, eRev, late n])
      -- the cursor is at 3 when the late file is reached, and the window is 16
      upTo 19 `shouldBe` CanonCursor 20 2
      upTo 20 `shouldBe` CanonCursor 4 2

    it "does not spend a number on an event it refused" $ do
      owner <- kp
      alice <- kp
      other <- kp
      -- An open aimed at another repository, blessed by a maintainer of this
      -- one: refused, and it never showed anyone a number, so burning one would
      -- leave a gap nothing explains. At the top of the range it would strand
      -- the counter and abort every later triage run.
      let repo = fst owner
          foreign' = mkEvent alice owner
                       (AOpen (fst other) HubIssue "elsewhere" [] Nothing Nothing Nothing 1)
                       (canon repo 1 (Just (maxBound - 1)))
          fr = foldEvents repo [foreign']
      reasons fr `shouldBe` [WrongTarget]
      -- the seq is spent, since the file sits at it; the number is not
      cursorFrom fr `shouldBe` CanonCursor 2 1

    it "still counts a seq a maintainer took before their revocation landed" $ do
      owner <- kp
      bob <- kp
      alice <- kp
      -- The delegate minted from a view built before the revocation and the
      -- publisher wrote the result. The fold refuses the event, but the file is
      -- in the tree at that seq, so handing the seq out again puts two events
      -- there. No ill intent needed, and a key that was a maintainer could
      -- have stranded the cursor while its delegation stood anyway, so nothing
      -- is conceded by counting it now.
      let repo = fst owner
          eDel = mkEvent owner owner (ADelegate repo (fst bob) 1) (canon repo 1 Nothing)
          eOpen = mkEvent alice bob (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 2)
                    (canon repo 2 (Just 1))
          eRev = mkEvent owner owner (ARevoke repo (fst bob) 3) (canon repo 3 Nothing)
          eLate = mkEvent alice bob (AComment (eventId eOpen) Nothing (Just "late") Nothing 4)
                    (canon repo 9 Nothing)
          fr = foldEvents repo [eDel, eOpen, eRev, eLate]
      reasons fr `shouldBe` [UnauthorizedCanon]
      cursorFrom fr `shouldBe` CanonCursor 10 2
      frLastFolded fr `shouldBe` 3
      -- ...and only next to the cursor. A withdrawn delegation that could still
      -- stamp anywhere made the revocation useless as a remedy: the owner had
      -- nothing left to answer a mutiny with but compaction.
      let eMutiny = mkEvent alice bob
                      (AComment (eventId eOpen) Nothing (Just "mutiny") Nothing 5)
                      (canonAt repo (maxBound - 1) Nothing 5)
          fr2 = foldEvents repo [eDel, eOpen, eRev, eMutiny]
      reasons fr2 `shouldBe` [UnauthorizedCanon]
      cursorFrom fr2 `shouldBe` CanonCursor 4 2

    it "hands back the admitted events in order, including the invisible ones" $ do
      owner <- kp
      bob <- kp
      alice <- kp
      -- What 'hub log' is made of. The threads are a map and say nothing about
      -- order, and the events that leave no visible trace (a set, a merge, a
      -- note-less close, a delegate) appear in no other view at all.
      let repo = fst owner
          eDel = mkEvent owner owner (ADelegate repo (fst bob) 1) (canon repo 1 Nothing)
          eOpen = mkEvent alice bob (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 2)
                    (canon repo 2 (Just 1))
          tid = eventId eOpen
          eSet = mkEvent owner owner (ASet tid "labels" "bug" 3) (canon repo 3 Nothing)
          eClose = mkEvent owner owner (AClose tid Nothing 4) (canon repo 4 Nothing)
          -- refused, so it is in the drop report and not here
          eBad = mkEvent alice alice (AComment tid Nothing (Just "no") Nothing 5)
                   (canon repo 5 Nothing)
          fr = foldEvents repo [eSet, eBad, eClose, eOpen, eDel]
      map lgSeq (frLog fr) `shouldBe` [1,2,3,4]
      map lgEvent (frLog fr) `shouldBe` map eventId [eDel, eOpen, eSet, eClose]
      map lgThread (frLog fr) `shouldBe` [Nothing, Just tid, Just tid, Just tid]
      map lgCanonBy (frLog fr) `shouldBe` [fst owner, fst bob, fst owner, fst owner]
      map lgAuthor (frLog fr) `shouldBe` [fst owner, fst alice, fst owner, fst owner]
      map lgContent (frLog fr) `shouldBe`
        [ ADelegate repo (fst bob) 1
        , AOpen repo HubIssue "t" [] Nothing Nothing Nothing 2
        , ASet tid "labels" "bug" 3
        , AClose tid Nothing 4
        ]
      map drWhy (frDropped fr) `shouldBe` [UnauthorizedCanon]

    it "reports a key published for nothing" $ do
      owner <- kp
      alice <- kp
      -- Not a leak now that the parts have a secret of their own, but a key
      -- put in public canon for an event with nothing to unlock.
      let repo = fst owner
          ev = mkEvent alice owner (AOpen repo HubIssue "t" [] (Just "inline") Nothing Nothing 1)
                 (\e -> CanonContent repo e 1 (Just 1) Nothing Nothing 1 (Just secret32))
          fr = foldEvents repo [ev]
      frDropped fr `shouldBe` []
      map anWhat (frAnomalies fr) `shouldBe` [SecretWithoutPart]

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
                 (AOpen repo HubIssue "t" [] Nothing (Just (unproven part)) Nothing 1)
                 (\e -> CanonContent repo e 1 (Just 1) Nothing Nothing 1 (Just stunted))
          fr = foldEvents repo [ev]
      frDropped fr `shouldBe` []
      map anWhat (frAnomalies fr) `shouldBe` [UnusablePartSecret]

    -- The audit half of PEP-18's part proof. The check that stops the theft is
    -- the bridge's, before anything is minted; this is the one every clone can
    -- make for itself out of the two boxes alone, so a maintainer who published
    -- a secret nobody proved cannot have it counted as canon anywhere.
    it "drops an event whose part-secret its author never proved" $ do
      owner <- kp
      alice <- kp
      mallory <- kp
      part <- someHash
      let repo = fst owner
          -- Same part, same published secret, two authors: only the one the
          -- proof is bound to survives.
          ev who = mkEvent who owner
                     (AOpen repo HubIssue "t" [] Nothing (Just (proven alice part)) Nothing 1)
                     (\e -> CanonContent repo e 1 (Just 1) Nothing Nothing 1 (Just secret32))
      frDropped (foldEvents repo [ev alice]) `shouldBe` []
      map drWhy (frDropped (foldEvents repo [ev mallory])) `shouldBe` [PartNotProven]
      -- Nothing is published, so there is nothing to prove and the event stands
      -- (it says so as an anomaly instead).
      let noKey = mkEvent mallory owner
                    (AOpen repo HubIssue "t" [] Nothing (Just (proven alice part)) Nothing 1)
                    (\e -> CanonContent repo e 1 (Just 1) Nothing Nothing 1 Nothing)
      frDropped (foldEvents repo [noKey]) `shouldBe` []

    it "refuses a signature made for another kind of record" $ do
      owner <- kp
      alice <- kp
      target <- someHash
      -- Ed25519 signs bytes, not types. Without a domain inside the signed
      -- bytes, any record of shape [small int, hash, int] signed by the owner
      -- for any purpose is byte for byte a signed redaction: a constructor tag
      -- is an ordinary small CBOR integer.
      let repo = fst owner
          -- byte for byte serialise (ARedact repo target 5) before the tag existed
          lookalike = signBare owner ((7 :: Word64), target, (5 :: Word64))
          real = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                         (canon repo 1 (Just 1))
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
                      (Domained (domainOf (Nothing @CanonContent)) (ARedact repo target 5))
      case unboxChecked (crossed :: SignedBox AuthorContent HubScheme) of
        Left (BoxUndecodable _ why) -> why `shouldBe` WrongDomain
        other -> expectationFailure ("expected a domain refusal: " <> show (fmap fst other))
      -- The fold reports it as an undecodable author box, not as a forgery.
      map drWhy (frDropped (foldEvents repo [ev]))
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
                       (canon repo 1 (Just 1))
      unboxChecked (mangleSig (evAuthorBox ev)) `shouldSatisfy` \case
        Left BoxBadSig -> True
        _              -> False
      map drWhy (frDropped (foldEvents repo [Event (mangleSig (evAuthorBox ev)) (evCanonBox ev)]))
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
          real = mkEvent alice owner content (canon repo 1 (Just 1))
      case unboxChecked padded of
        Left (BoxUndecodable k why) -> do
          k `shouldBe` fst alice
          why `shouldBe` TrailingData
        other -> expectationFailure ("expected trailing data: " <> show (fmap fst other))
      map drWhy (frDropped (foldEvents repo [Event padded (evCanonBox real)]))
        `shouldBe` [UndecodableAuthor (fst alice) TrailingData]

    it "refuses to fold canon from a newer consensus version" $ do
      owner <- kp
      alice <- kp
      -- The admission rules ARE the format, so folding canon written under
      -- rules this build does not know produces a view that quietly disagrees
      -- with every up-to-date clone.
      let repo = fst owner
          ev = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                       (canon repo 1 (Just 1))
      either Just (const Nothing) (foldCanon (hubMetaVersion + 1) repo [ev])
        `shouldBe` Just (MetaTooNew (hubMetaVersion + 1))
      either (const []) (HM.keys . frThreads) (foldCanon hubMetaVersion repo [ev])
        `shouldBe` [eventId ev]
      -- The TREE's version only. A file's own version is one file's problem
      -- and belongs to the reader: taken as a maximum over the tree, it handed
      -- a veto over the whole repository to anyone who could write one
      -- unsigned line ("HBS2.Hub.Canon").

    it "keeps a thread's state moving through merge and reopen" $ do
      owner <- kp
      alice <- kp
      bundle <- someHash
      -- The state machine nobody had walked end to end: revise and reopen
      -- after a merge, and a redact of the opening event.
      let repo = fst owner
          ePR = mkEvent alice owner
                  (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords { prBundle = Just (proven alice bundle) }) 1)
                  (\e -> CanonContent repo e 1 (Just 1) Nothing Nothing 1 (Just secret32))
          tid = eventId ePR
          eMerge = mkEvent owner owner (AMerge tid "cafe" "refs/heads/master" 2) (canon repo 2 Nothing)
          -- a revise after the merge is still admitted: PEP-19 has no
          -- terminal state, and the coordinates are the author's to update
          eRev = mkEvent alice owner (ARevise tid coords { prSourceTip = "cccc" } 3)
                   (\e -> CanonContent repo e 3 Nothing Nothing Nothing 3 Nothing)
          eReopen = mkEvent owner owner (AReopen tid Nothing 4) (canon repo 4 Nothing)
          eRed = mkEvent owner owner (ARedact repo tid 5) (canon repo 5 Nothing)
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
                          (canon repo 1 (Just 1))
          tid = eventId eOpen
          keep' = mkEvent alice owner (AComment tid Nothing (Just "fine") Nothing 2)
                    (canon repo 2 Nothing)
          gone = mkEvent alice owner (AComment tid Nothing (Just "abuse") Nothing 3)
                   (canon repo 3 Nothing)
          eRed = mkEvent owner owner (ARedact repo (eventId gone) 4) (canon repo 4 Nothing)
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
          eDel = mkEvent owner owner (ADelegate repo (fst bob) 1) (canon repo 1 Nothing)
          ePR = mkEvent alice owner (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords) 2)
                  (canon repo 2 (Just 1))
          tid = eventId ePR
          eRev = mkEvent bob bob (ARevise tid coords { prSource = Just "hbs23://elsewhere" } 3)
                   (canon repo 3 Nothing)
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
                         (canon repo 1 (Just 1))
          futureCanon = signBare owner
                          (Domained (domainOf (Nothing @CanonContent)) (12345 :: Word64))
          ev = Event (evAuthorBox real) futureCanon
      map drWhy (frDropped (foldEvents repo [ev]))
        `shouldBe` [UndecodableCanon (fst owner) Undecodable]

    it "tells an undecodable box apart from a forged one" $ do
      owner <- kp
      alice <- kp
      -- A correctly signed box whose content this build cannot decode: canon
      -- is newer than the reader. Reporting BadAuthorSig would accuse a
      -- newer, honest sender of forgery.
      let repo = fst owner
          real = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                         (canon repo 1 (Just 1))
          ev = Event (futureBox alice) (evCanonBox real)
          fr = foldEvents repo [ev]
      map drEvent (frDropped fr) `shouldBe` [eventId ev]
      case map drWhy (frDropped fr) of
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
          eDel = mkEvent owner owner (ADelegate repo (fst bob) 1) (canon repo 1 Nothing)
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 2)
                          (canon repo 2 (Just 1))
          tid = eventId eOpen
          eCmt = mkEvent alice bob (AComment tid Nothing (Just "hi") Nothing 3)
                         (canon repo 3 Nothing)
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
                          (canon repo 1 (Just 1))
          tid = eventId eOpen
          eC1 = mkEvent alice owner (AComment tid Nothing (Just "first") Nothing 2)
                        (canon repo 2 Nothing)
          eC2 = mkEvent alice owner (AComment tid (Just (eventId eC1)) (Just "answer") Nothing 3)
                        (canon repo 3 Nothing)
          eC3 = mkEvent alice owner (AComment tid (Just ghost) (Just "dangling") Nothing 4)
                        (canon repo 4 Nothing)
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
                          (canon repo 1 (Just 1))
          tid = eventId eOpen
          eCmt = mkEvent alice owner (AComment tid Nothing (Just "spam") Nothing 2)
                         (canon repo 2 Nothing)
          eRed = mkEvent owner owner (ARedact repo (eventId eCmt) 3) (canonAt repo 3 Nothing 9000)
          fr = foldEvents repo [eOpen, eCmt, eRed]
      frDropped fr `shouldBe` []
      tsUpdated (threadOf fr tid) `shouldBe` 9000

    it "refuses a number on anything but an open" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                          (canon repo 1 (Just 1))
          tid = eventId eOpen
          -- a comment stamped with a number: nothing mints these, but a
          -- wrong or hostile maintainer could, and it would raise the
          -- number high-water mark for everyone.
          eCmt = mkEvent alice owner (AComment tid Nothing (Just "hi") Nothing 2)
                         (canon repo 2 (Just 99))
          fr = foldEvents repo [eOpen, eCmt]
      reasons fr `shouldBe` [NumberOnNonOpen]
      frMaxNumber fr `shouldBe` 1

    it "refuses a seq or number at the top of the range" $ do
      owner <- kp
      alice <- kp
      -- The next mint is the maximum plus one, so admitting maxBound would
      -- wrap the cursor to zero and poison the repo permanently.
      let repo = fst owner
          eMax = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                         (canon repo maxBound (Just 1))
          eNum = mkEvent alice owner (AOpen repo HubIssue "u" [] Nothing Nothing Nothing 1)
                         (canon repo 1 (Just maxBound))
          fr = foldEvents repo [eMax, eNum]
      -- and each says which counter, since that is the first thing an operator
      -- has to know and nothing in the event recovers it
      reasons fr `shouldBe` [NumberAtTopOfRange, SeqAtTopOfRange]
      HM.null (frThreads fr) `shouldBe` True
      -- Refused, and the stamps are still spent field by field: the usable
      -- half of each of these two is, the out-of-range half is not. Spending
      -- maxBound is the one thing that cannot happen, since the next mint is
      -- the maximum plus one and would wrap to zero.
      frMaxSeq fr `shouldBe` 1
      -- and no number at all: that one is spent on admission, and neither of
      -- these was admitted
      frMaxNumber fr `shouldBe` 0

    it "rejects a tampered author box" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          e = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                      (canon repo 1 (Just 1))
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
                      (canon repo 1 (Just 1))
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
                     (canon (fst ownerX) 1 (Just 1))
          fr = foldEvents (fst ownerX) [stolen]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [WrongTarget]

    it "deduplicates by event-id (rewrap replay)" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                          (canon repo 1 (Just 1))
          tid   = eventId eOpen
          ac    = AComment tid Nothing (Just "once") Nothing 2
          -- the SAME author box, blessed twice under different seqs
          c1 = mkEvent alice owner ac (canon repo 2 Nothing)
          c2 = mkEvent alice owner ac (canon repo 3 Nothing)
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
                          (canon repo 1 (Just 1))
          tid   = eventId eOpen
          -- two DIFFERENT sets of the same attribute at the SAME seq
          s1 = mkEvent owner owner (ASet tid "status" "one" 2) (canon repo 5 Nothing)
          s2 = mkEvent owner owner (ASet tid "status" "two" 3) (canon repo 5 Nothing)
          fwd = foldEvents repo [eOpen, s1, s2]
          rev = foldEvents repo [eOpen, s2, s1]
      summary fwd `shouldBe` summary rev

    it "treats title as an LWW attribute, not a frozen field" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "old" [] Nothing Nothing Nothing 1)
                          (canon repo 1 (Just 1))
          tid   = eventId eOpen
          eSet  = mkEvent owner owner (ASet tid "title" "new" 2) (canon repo 2 Nothing)
          fr = foldEvents repo [eOpen, eSet]
          t  = threadOf fr tid
      tsTitle t `shouldBe` "new"
      HM.lookup "title" (tsAttrs t) `shouldBe` Just "new"

    it "takes times from the canon box, not the author's clock" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                          (canonAt repo 1 (Just 1) 1000)
          tid   = eventId eOpen
          -- a comment claiming to be from the far future
          eCmt  = mkEvent alice owner (AComment tid Nothing (Just "spoof") Nothing maxBound)
                          (canonAt repo 2 Nothing 2000)
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
                         (canon repo 1 (Just 1))
          other = mkEvent alice owner (AOpen repo HubIssue "b" [] Nothing Nothing Nothing 2)
                          (canon repo 2 (Just 2))
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
          eDelBob = mkEvent owner owner (ADelegate repo (fst bob) 1) (canon repo 1 Nothing)
          eOpenC  = mkEvent carol bob (AOpen repo HubIssue "ok" [] Nothing Nothing Nothing 100)
                            (canon repo 2 (Just 1))
          tidC    = eventId eOpenC
          eDelDave = mkEvent bob bob (ADelegate repo (fst dave) 3) (canon repo 3 Nothing)
          eOpenE  = mkEvent erin dave (AOpen repo HubIssue "bad" [] Nothing Nothing Nothing 400)
                            (canon repo 4 (Just 2))
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
          eDel  = mkEvent owner owner (ADelegate repo (fst bob) 1) (canon repo 1 Nothing)
          -- bob blesses this while authorized: must survive the later revoke
          eOk   = mkEvent alice bob (AOpen repo HubIssue "before" [] Nothing Nothing Nothing 2)
                          (canon repo 2 (Just 1))
          tidOk = eventId eOk
          eRev  = mkEvent owner owner (ARevoke repo (fst bob) 3) (canon repo 3 Nothing)
          -- after the revoke bob may no longer bless
          eBad  = mkEvent alice bob (AOpen repo HubIssue "after" [] Nothing Nothing Nothing 4)
                          (canon repo 4 (Just 2))
          tidBad = eventId eBad
          fr = foldEvents repo [eDel, eOk, eRev, eBad]
      HM.member tidOk (frThreads fr) `shouldBe` True
      HM.member tidBad (frThreads fr) `shouldBe` False
      reasons fr `shouldBe` [UnauthorizedCanon]

    it "ignores a revoke of the owner key (root of trust)" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eSelf = mkEvent owner owner (ARevoke (fst owner) (fst owner) 1) (canon repo 1 Nothing)
          eOpen = mkEvent alice owner (AOpen repo HubIssue "still works" [] Nothing Nothing Nothing 2)
                          (canon repo 2 (Just 1))
          tid = eventId eOpen
          eClose = mkEvent owner owner (AClose tid Nothing 3) (canon repo 3 Nothing)
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
                    (canon repo 1 (Just 1))
          tid = eventId eOpen
          good = coords { prSourceTip = "cccc" }
          evil = coords { prSourceTip = "dead" }
          eGood = mkEvent alice owner (ARevise tid good 2) (canon repo 2 Nothing)
          eEvil = mkEvent mallory owner (ARevise tid evil 3) (canon repo 3 Nothing)
          fr = foldEvents repo [eOpen, eGood, eEvil]
          t  = threadOf fr tid
      fmap (prSourceTip . psCoords) (tsPR t) `shouldBe` Just "cccc"
      reasons fr `shouldBe` [BadRevise]

    it "refuses PR-only ops on an issue thread and never invents coordinates" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubIssue "an issue" [] Nothing Nothing Nothing 1)
                          (canon repo 1 (Just 1))
          tid = eventId eOpen
          eMerge = mkEvent owner owner (AMerge tid "cafe" "refs/heads/master" 2) (canon repo 2 Nothing)
          fr = foldEvents repo [eOpen, eMerge]
          t  = threadOf fr tid
      tsPR t `shouldBe` Nothing
      HM.lookup "status" (tsAttrs t) `shouldBe` Just "open"
      reasons fr `shouldBe` [PROnlyOnIssue]

    it "rejects a pr open with no coordinates" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner (AOpen repo HubPR "no coords" [] Nothing Nothing Nothing 1)
                          (canon repo 1 (Just 1))
          fr = foldEvents repo [eOpen]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [PROpenWithoutCoords]

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
                    (canon repo 1 (Just 1))
          fr = foldEvents repo [eOpen]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [CoordsUnreachable]

    it "refuses a revise that removes the last way to fetch" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner
                    (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords) 1)
                    (canon repo 1 (Just 1))
          tid = eventId eOpen
          nowhere = coords { prSource = Nothing, prBundle = Nothing }
          eRev = mkEvent alice owner (ARevise tid nowhere 2) (canon repo 2 Nothing)
          fr = foldEvents repo [eOpen, eRev]
      reasons fr `shouldBe` [CoordsUnreachable]
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
                    (canon repo 1 (Just 1))
          fr = foldEvents repo [eOpen]
      HM.null (frThreads fr) `shouldBe` True
      reasons fr `shouldBe` [IssueOpenWithCoords]
