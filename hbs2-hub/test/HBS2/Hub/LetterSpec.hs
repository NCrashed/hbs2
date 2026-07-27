module HBS2.Hub.LetterSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Fold
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.SignedBox
import HBS2.Data.Types.Refs (HashRef)

import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Test.Hspec

type KP = (HubKey, PrivKey 'Sign HubScheme)

kp :: IO KP
kp = do
  c <- newCredentials @'HBS2Basic
  pure (_peerSignPk c, _peerSignSk c)

canon :: Word64 -> Maybe Word64 -> EventId -> CanonContent
canon sq num eid = CanonContent eid sq num Nothing sq Nothing

threadOf :: FoldResult -> ThreadId -> ThreadState
threadOf fr tid = fromMaybe (error "expected thread") (HM.lookup tid (frThreads fr))

-- The payload types deliberately have no Show (a SignedBox is opaque
-- bytes), so assert on Either by hand rather than via shouldSatisfy.
expectRight :: Either LetterError a -> IO a
expectRight = either (\e -> error ("unexpected error: " <> show e)) pure

expectLeft :: LetterError -> Either LetterError a -> Expectation
expectLeft want = \case
  Left got -> got `shouldBe` want
  Right _  -> expectationFailure ("expected " <> show want)

-- Fold a letter the way the triage bridge will: keep the inner box verbatim,
-- wrap it in an owner-signed canon box.
bless :: KP -> Word64 -> Maybe Word64 -> SignedBox AuthorContent HubScheme -> Event
bless (opk,osk) sq num box =
  Event box (signCanon opk osk (canon sq num (authorBoxId box)))

-- A well-formed hash to stand in for a sigil reference.
someHash :: IO HashRef
someHash = do
  (pk,sk) <- kp
  pure (authorBoxId (signAuthor pk sk (ARevoke pk 0)))

coords :: PRCoords
coords = PRCoords Nothing "refs/heads/f" "aaaa" "refs/heads/master" "bbbb" Nothing

spec :: Spec
spec = do

  describe "PEP-18 letter payload" $ do

    it "round-trips a letter through the payload bytes" $ do
      alice <- kp
      owner <- kp
      sig <- someHash
      let ac = AOpen (fst owner) HubIssue "hello" ["bug","ui"] (Just "body") Nothing Nothing 42
          rc = ReplyTo (fst alice) sig
          sent = makeLetter (fst alice) (snd alice) ac rc
      got <- expectRight (parsePayload (letterPayload sent))
      (_, who, content, chan) <- expectRight (openLetter got)
      who `shouldBe` fst alice
      content `shouldBe` ac
      chan `shouldBe` rc

    it "round-trips a pr letter with coordinates, all the way to canon" $ do
      alice <- kp
      owner <- kp
      let ac = AOpen (fst owner) HubPR "a pull request" [] Nothing Nothing (Just coords) 1
          sent = makeLetter (fst alice) (snd alice) ac noReplyChannel
      got <- expectRight (parsePayload (letterPayload sent))
      (box, _, content, _) <- expectRight (openLetter got)
      content `shouldBe` ac
      let ev = bless owner 1 (Just 1) box
          fr = foldEvents (fst owner) [ev]
          t = threadOf fr (eventId ev)
      tsKind t `shouldBe` HubPR
      fmap psCoords (tsPR t) `shouldBe` Just coords

    it "round-trips an ack, and an ack is not a letter" $ do
      owner <- kp
      alice <- kp
      let ac = AOpen (fst owner) HubIssue "t" [] Nothing Nothing Nothing 1
          tid = authorBoxId (signAuthor (fst alice) (snd alice) ac)
          rec' = AckRecord (fst owner) tid (Just 7) "merged" (Just "cafe")
      got <- expectRight (parsePayload (letterPayload (makeAck rec')))
      openAck (const True) (fst owner) got `shouldBe` Right rec'
      expectLeft NotALetter (openLetter got)

    it "refuses an ack whose envelope signer is not a maintainer" $ do
      owner <- kp
      mallory <- kp
      alice <- kp
      let ac = AOpen (fst owner) HubIssue "t" [] Nothing Nothing Nothing 1
          tid = authorBoxId (signAuthor (fst alice) (snd alice) ac)
          rec' = AckRecord (fst owner) tid (Just 7) "closed" Nothing
          isMaintainer = (== fst owner)
      got <- expectRight (parsePayload (letterPayload (makeAck rec')))
      openAck isMaintainer (fst owner) got `shouldBe` Right rec'
      expectLeft UntrustedAck (openAck isMaintainer (fst mallory) got)

    it "rejects a tampered inner box" $ do
      alice <- kp
      owner <- kp
      let ac = AOpen (fst owner) HubIssue "hello" [] Nothing Nothing Nothing 1
      case mdBody (makeLetter (fst alice) (snd alice) ac noReplyChannel) of
        Ack _ -> expectationFailure "expected a Letter"
        Letter box rc -> do
          let SignedBox pk bs sig = box
              forged = MessageData hubMsgVersion (Letter (SignedBox pk (bs <> "x") sig) rc)
          expectLeft BadInnerSig (openLetter forged)

    it "rejects garbage bytes" $
      expectLeft MalformedPayload (parsePayload "not cbor at all")

    it "rejects valid cbor with trailing garbage" $ do
      alice <- kp
      owner <- kp
      let ac = AOpen (fst owner) HubIssue "t" [] Nothing Nothing Nothing 1
          good = letterPayload (makeLetter (fst alice) (snd alice) ac noReplyChannel)
      expectLeft MalformedPayload (parsePayload (good <> "trailing"))

    it "reports a newer schema as unsupported, not malformed" $ do
      alice <- kp
      owner <- kp
      let ac = AOpen (fst owner) HubIssue "t" [] Nothing Nothing Nothing 1
          future = MessageData (hubMsgVersion + 1)
                     (mdBody (makeLetter (fst alice) (snd alice) ac noReplyChannel))
      expectLeft (UnsupportedVersion (hubMsgVersion + 1))
                 (parsePayload (letterPayload future))

    it "classifies letter ops by what they may become" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          ac0 = AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1
          tid = authorBoxId (signAuthor (fst alice) (snd alice) ac0)
      classify ac0 `shouldBe` FoldsToCanon
      classify (AComment tid Nothing Nothing Nothing 1) `shouldBe` FoldsToCanon
      classify (AClose tid Nothing 1) `shouldBe` RequestOnly
      classify (ASet tid "labels" "bug" 1) `shouldBe` RequestOnly
      classify (AMerge tid "a" "b" 1) `shouldBe` OwnerNative
      classify (ADelegate (fst owner) 1) `shouldBe` OwnerNative

  describe "PEP-18/PEP-19 bridge" $ do

    it "gives the sender the canonical thread-id before delivery" $ do
      alice <- kp
      owner <- kp
      -- The contributor computes the thread-id at send time...
      let ac = AOpen (fst owner) HubIssue "computed early" [] Nothing Nothing Nothing 1
          letter = makeLetter (fst alice) (snd alice) ac noReplyChannel
          predicted = letterThreadId letter
      -- ...the owner later folds that very inner box...
      (box, _, _, _) <- expectRight (openLetter letter)
      let ev = bless owner 1 (Just 1) box
          fr = foldEvents (fst owner) [ev]
      -- ...and canon agrees with what the sender predicted, with no handshake.
      predicted `shouldBe` Just (eventId ev)
      HM.keys (frThreads fr) `shouldBe` [eventId ev]

    it "carries the sender's authorship into canon, blessed by the owner" $ do
      alice <- kp
      owner <- kp
      let ac = AOpen (fst owner) HubIssue "from a stranger" [] Nothing Nothing Nothing 1
          letter = makeLetter (fst alice) (snd alice) ac noReplyChannel
      (box, _, _, _) <- expectRight (openLetter letter)
      let ev = bless owner 1 (Just 1) box
          fr = foldEvents (fst owner) [ev]
          t = threadOf fr (eventId ev)
      -- Authorship stays the stranger's; the owner only admitted and ordered it.
      tsAuthor t `shouldBe` fst alice
      tsTitle t `shouldBe` "from a stranger"

    it "keeps the reply channel out of the signed box" $ do
      alice <- kp
      owner <- kp
      sig <- someHash
      let ac = AOpen (fst owner) HubIssue "t" [] Nothing Nothing Nothing 1
          withChan = makeLetter (fst alice) (snd alice) ac (ReplyTo (fst alice) sig)
          without  = makeLetter (fst alice) (snd alice) ac noReplyChannel
      -- The back-channel sits outside the inner box, so it changes neither
      -- the event-id nor anything that reaches canon.
      letterEventId withChan `shouldBe` letterEventId without

    it "distinguishes a reply's own id from the thread it belongs to" $ do
      alice <- kp
      owner <- kp
      let ac = AOpen (fst owner) HubIssue "thread" [] Nothing Nothing Nothing 1
          opening = makeLetter (fst alice) (snd alice) ac noReplyChannel
          Just tid = letterThreadId opening
          reply = makeLetter (fst alice) (snd alice)
                    (AComment tid Nothing (Just "reply") Nothing 2) noReplyChannel
      -- On an open the two coincide; on a reply they must not, or an
      -- AckRecord (whose akThread is the thread) would never correlate.
      letterEventId opening `shouldBe` Just tid
      letterThreadId reply `shouldBe` Just tid
      letterEventId reply `shouldNotBe` Just tid

    it "drops a deny-listed inner author, however the envelope was signed" $ do
      mallory <- kp
      relay   <- kp
      owner   <- kp
      let ac = AOpen (fst owner) HubIssue "spam" [] Nothing Nothing Nothing 1
          letter = makeLetter (fst mallory) (snd mallory) ac noReplyChannel
          allowed k = k /= fst mallory
      -- rewrapped under a fresh envelope key, the inner author is still banned
      expectLeft AuthorDenied (openLetterAs allowed (fst relay) letter)
      expectLeft AuthorDenied (openLetterAs allowed (fst mallory) letter)

    it "honours the reply channel only from the inner author's own envelope" $ do
      alice <- kp
      relay <- kp
      owner <- kp
      sig <- someHash
      let ac = AOpen (fst owner) HubIssue "t" [] Nothing Nothing Nothing 1
          letter = makeLetter (fst alice) (snd alice) ac (ReplyTo (fst alice) sig)
      (_,_,_,mine) <- expectRight (openLetterAs (const True) (fst alice) letter)
      (_,_,_,relayed) <- expectRight (openLetterAs (const True) (fst relay) letter)
      mine `shouldBe` ReplyTo (fst alice) sig
      -- a rewrapper could have substituted their own, so it is not honoured
      relayed `shouldBe` NoReply

    it "a reply letter names the thread the sender already knows" $ do
      alice <- kp
      bob   <- kp
      owner <- kp
      let ac = AOpen (fst owner) HubIssue "thread" [] Nothing Nothing Nothing 1
          opening = makeLetter (fst alice) (snd alice) ac noReplyChannel
          Just tid = letterThreadId opening
          -- bob read the thread-id from public canon and replies to it
          reply = makeLetter (fst bob) (snd bob)
                    (AComment tid Nothing (Just "me too") Nothing 2) noReplyChannel
      (obox, _, _, _) <- expectRight (openLetter opening)
      (rbox, _, _, _) <- expectRight (openLetter reply)
      let fr = foldEvents (fst owner) [bless owner 1 (Just 1) obox, bless owner 2 Nothing rbox]
          t = threadOf fr tid
      map cAuthor (tsComments t) `shouldBe` [fst bob]
