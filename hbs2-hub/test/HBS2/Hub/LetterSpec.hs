module HBS2.Hub.LetterSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Fold
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.SignedBox
import HBS2.Data.Types.Refs (HashRef)
import HBS2.Prelude.Plated (pretty)

import Data.Config.Suckless
import Data.HashMap.Strict qualified as HM
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.ByteString qualified as BS
import HBS2.Net.Auth.GroupKeySymm (typicalKeyLength)
import Data.Word (Word8,Word64)
import Test.Hspec

type KP = (HubKey, PrivKey 'Sign HubScheme)

kp :: IO KP
kp = do
  c <- newCredentials @'HBS2Basic
  pure (_peerSignPk c, _peerSignSk c)

-- A correctly signed author box whose content this build cannot decode: the
-- sender speaks a newer schema. Reporting a bad signature for one of these
-- would accuse an honest sender of forgery.
futureBox :: KP -> SignedBox AuthorContent HubScheme
futureBox (pk,sk) =
  -- Tagged with the author domain, because that is what a real v2 sender
  -- produces: domains are never renumbered, so a newer schema changes the
  -- payload inside the tag, not the tag.
  case makeSignedBox @HubScheme pk sk
         (Domained (domainOf (Nothing @AuthorContent)) (12345 :: Word64)) of
    SignedBox p b s -> SignedBox p b s

canon :: RepoRef -> Word64 -> Maybe Word64 -> EventId -> CanonContent
canon repo sq num eid = CanonContent repo eid sq num Nothing sq Nothing

threadOf :: FoldResult -> ThreadId -> ThreadState
threadOf fr tid = fromMaybe (error "expected thread") (HM.lookup tid (frThreads fr))

-- The payload types deliberately have no Show (a SignedBox is opaque
-- bytes), so assert on Either by hand rather than via shouldSatisfy.
expectRight :: Either LetterError a -> IO a
expectRight = either (\e -> error ("unexpected error: " <> show e)) pure

expectJust :: Maybe a -> a
expectJust = fromMaybe (error "expected Just")

expectLeft :: LetterError -> Either LetterError a -> Expectation
expectLeft want = \case
  Left got -> got `shouldBe` want
  Right _  -> expectationFailure ("expected " <> show want)

-- Fold a letter the way the triage bridge will: keep the inner box verbatim,
-- wrap it in an owner-signed canon box.
bless :: KP -> Word64 -> Maybe Word64 -> SignedBox AuthorContent HubScheme -> Event
bless (opk,osk) sq num box =
  Event box (signCanon opk osk (canon opk sq num (authorBoxId box)))

-- A well-formed hash to stand in for a sigil reference.
someHash :: IO HashRef
someHash = do
  (pk,sk) <- kp
  pure (authorBoxId (signAuthor pk sk (ARevoke pk pk 0)))

coords :: PRCoords
-- A fork-pointer PR: PEP-20 requires one of the two ways to fetch the
-- change, so a coords with neither is refused (reachableCoords).
coords = PRCoords (Just "hbs23://fork") "refs/heads/f" "aaaa" "refs/heads/master" "bbbb" Nothing

-- A group secret is raw key bytes of a fixed size, and the constructor checks
-- only that; telling the parts secret from the message secret is what the
-- bridge's 'poMessage' is for.
secret32 :: PartSecret
secret32 = secretOf 0x41

-- Two distinct secrets, for the case where each message has its own.
secretA, secretB :: PartSecret
secretA = secretOf 0x01
secretB = secretOf 0x02

secretOf :: Word8 -> PartSecret
secretOf b = fromMaybe (error "bad fixture secret")
               (mkPartSecret (BS.replicate typicalKeyLength b))

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
          rec' = AckRecord (fst owner) tid (Just 7) "merged" (Just "cafe") Nothing
      got <- expectRight (parsePayload (letterPayload (makeAck rec')))
      openAck (\_ _ -> True) (EnvelopeSigner (fst owner)) got `shouldBe` Right rec'
      expectLeft NotALetter (openLetter got)

    it "refuses an ack whose envelope signer is not a maintainer" $ do
      owner <- kp
      mallory <- kp
      alice <- kp
      let ac = AOpen (fst owner) HubIssue "t" [] Nothing Nothing Nothing 1
          tid = authorBoxId (signAuthor (fst alice) (snd alice) ac)
          rec' = AckRecord (fst owner) tid (Just 7) "closed" Nothing Nothing
          isMaintainer _ k = k == fst owner
      got <- expectRight (parsePayload (letterPayload (makeAck rec')))
      openAck isMaintainer (EnvelopeSigner (fst owner)) got `shouldBe` Right rec'
      expectLeft UntrustedAck (openAck isMaintainer (EnvelopeSigner (fst mallory)) got)

    it "refuses an ack about a thread the reader never submitted" $ do
      mine <- kp
      other <- kp
      alice <- kp
      thr <- someHash
      -- A maintainer of their OWN repo naming a thread in someone else's: the
      -- signature and the maintainer check both pass, so only the reader's own
      -- record of what it sent can tell the difference. Correlating by thread
      -- alone would show a stranger's status on the reader's submission.
      let rec' = AckRecord (fst other) thr (Just 1) "closed" Nothing Nothing
          isMaintainer repo k = k == repo
          isMine repo t = repo == fst mine && t == thr
      got <- expectRight (parsePayload (letterPayload (makeAck rec')))
      -- openAck alone takes it
      openAck isMaintainer (EnvelopeSigner (fst other)) got `shouldBe` Right rec'
      expectLeft UnrelatedAck (openAckFor isMaintainer isMine (EnvelopeSigner (fst other)) got)
      -- ...and the reader's own ack still passes
      let ours = AckRecord (fst mine) thr (Just 1) "closed" Nothing Nothing
      oursMd <- expectRight (parsePayload (letterPayload (makeAck ours)))
      openAckFor isMaintainer isMine (EnvelopeSigner (fst mine)) oursMd `shouldBe` Right ours
      -- alice is nobody's maintainer, so the first check still fires first
      expectLeft UntrustedAck (openAckFor isMaintainer isMine (EnvelopeSigner (fst alice)) oursMd)

    it "refuses to read a letter as an ack" $ do
      alice <- kp
      owner <- kp
      -- The mirror of "an ack is not a letter": both directions matter,
      -- because a reader picks the door by what it expected, not by what came.
      let ac = AOpen (fst owner) HubIssue "t" [] Nothing Nothing Nothing 1
          letter = makeLetter (fst alice) (snd alice) ac noReplyChannel
      got <- expectRight (parsePayload (letterPayload letter))
      expectLeft NotAnAck (openAck (\_ _ -> True) (EnvelopeSigner (fst alice)) got)
      expectLeft NotAnAck
        (openAckFor (\_ _ -> True) (\_ _ -> True) (EnvelopeSigner (fst alice)) got)

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

    it "rejects an older version as unsupported, not as v1" $ do
      alice <- kp
      owner <- kp
      -- Version 0 is not "close enough to 1": an unknown lower version is no
      -- more decodable here than an unknown higher one, and guessing is how
      -- a reader ends up interpreting bytes it does not understand.
      let ac = AOpen (fst owner) HubIssue "t" [] Nothing Nothing Nothing 1
          old = MessageData 0 (mdBody (makeLetter (fst alice) (snd alice) ac noReplyChannel))
      expectLeft (UnsupportedVersion 0) (parsePayload (letterPayload old))

    it "checks the version even when a MessageData is built directly" $ do
      alice <- kp
      owner <- kp
      -- The constructor is exported, so a value can reach the open path
      -- without passing parsePayload.
      let ac = AOpen (fst owner) HubIssue "t" [] Nothing Nothing Nothing 1
          future = MessageData (hubMsgVersion + 1)
                     (mdBody (makeLetter (fst alice) (snd alice) ac noReplyChannel))
      expectLeft (UnsupportedVersion (hubMsgVersion + 1)) (openLetter future)

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

    it "reports the version even when the body is undecodable" $ do
      -- The case the version exists for: a v2 sender uses a body shape this
      -- reader has never seen. Decoding body-first would report "malformed"
      -- and lose the one piece of information that explains why.
      let futureBody = LBS.toStrict (serialise (42 :: Word64))  -- not a MessageBody
          v1 = LBS.toStrict (serialise (Envelope hubMsgVersion futureBody))
          v2 = LBS.toStrict (serialise (Envelope (hubMsgVersion + 1) futureBody))
      expectLeft MalformedPayload (parsePayload v1)
      expectLeft (UnsupportedVersion (hubMsgVersion + 1)) (parsePayload v2)

    it "projects a letter to readable s-expressions" $ do
      owner <- kp
      let ac = AOpen (fst owner) HubIssue "a title" ["needs triage"] (Just "b") Nothing Nothing 5
          rendered = show (pretty (mkList @C (letterSyntax ac)))
      -- The projection is regenerated for humans, never signed or parsed
      -- back, but it must at least be well formed and carry the content.
      rendered `shouldSatisfy` (("hub-msg" `isInfixOf`) )
      rendered `shouldSatisfy` (("a title" `isInfixOf`) )
      -- a label with a space must not become a broken symbol
      rendered `shouldSatisfy` (("\"needs triage\"" `isInfixOf`) )
      case parseTop (unwords (fmap (show . pretty) (letterSyntax ac))) of
        Left e  -> expectationFailure ("projection does not re-parse: " <> show e)
        Right _ -> pure ()

    it "projects a title a stranger wrote, whatever is in it" $ do
      owner <- kp
      -- A title comes from anyone, and the same projection is what PEP-19
      -- writes into the event file beside the authoritative boxes. One
      -- unescaped quote there does not spoil a display line: it makes the FILE
      -- unparseable, permanently, with two valid signed boxes inside it.
      let nasty = "a \"quoted\" \\ title\nwith a newline and \1088\1091\1089\1089\1082\1080\1081"
          ac = AOpen (fst owner) HubIssue nasty ["with \"quotes\""] (Just nasty) Nothing Nothing 5
          rendered = unwords (fmap (show . pretty) (letterSyntax ac))
      case parseTop rendered of
        Left e  -> expectationFailure ("projection does not re-parse: " <> show e)
        Right forms -> do
          -- Parsing is not enough. An unescaped quote ends the string early and
          -- what follows is read as whatever it happens to look like, so the
          -- clauses AFTER the title are what a hostile title really attacks.
          let clauses = concat [ xs | List _ xs <- forms ]
          [ t | List _ [SymbolVal "title", LitStrVal t] <- clauses ]
            `shouldBe` [nasty]
          [ () | List _ (SymbolVal "created" : _) <- clauses ] `shouldBe` [()]

    it "projects an acknowledgement the way PEP-18 pins it" $ do
      owner <- kp
      thr <- someHash
      -- A contributor's tooling reads these ('hub updates', PEP-22), so the
      -- shape is part of the interop surface, not a debugging convenience.
      let ack = AckRecord (fst owner) thr (Just 7) "merged" (Just "cafe") (Just "as agreed")
          rendered = fmap (show . pretty) (ackSyntax ack)
          whole = unwords rendered
      whole `shouldSatisfy` isInfixOf "(kind ack)"
      whole `shouldSatisfy` isInfixOf "(number 7)"
      whole `shouldSatisfy` isInfixOf "(merge-commit \"cafe\")"
      -- the maintainer's own words, so a contributor is not sent to canon to
      -- find out why their submission was closed
      whole `shouldSatisfy` isInfixOf "(note \"as agreed\")"
      -- No op: an ack asserts nothing the contributor authored.
      whole `shouldSatisfy` (not . isInfixOf "(op ")
      case parseTop whole of
        Left e  -> expectationFailure ("projection does not re-parse: " <> show e)
        Right _ -> pure ()
      -- The optional halves are simply absent, not rendered empty.
      let bare = unwords (fmap (show . pretty) (ackSyntax (AckRecord (fst owner) thr Nothing "open" Nothing Nothing)))
      bare `shouldSatisfy` (not . isInfixOf "number")
      bare `shouldSatisfy` (not . isInfixOf "merge-commit")
      bare `shouldSatisfy` (not . isInfixOf "note")

    it "classifies letter ops by what they may become" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          ac0 = AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1
          tid = authorBoxId (signAuthor (fst alice) (snd alice) ac0)
      -- All ten, because the bridge dispatches on this to decide whether a
      -- letter is folded as the sender's own, put in front of a maintainer, or
      -- refused outright, and an op missing from the list is an op nothing
      -- says which of the three it is.
      classify ac0 `shouldBe` FoldsToCanon
      classify (AComment tid Nothing Nothing Nothing 1) `shouldBe` FoldsToCanon
      classify (ARevise tid coords 1) `shouldBe` FoldsToCanon
      classify (AClose tid Nothing 1) `shouldBe` RequestOnly
      classify (AReopen tid Nothing 1) `shouldBe` RequestOnly
      classify (ASet tid "labels" "bug" 1) `shouldBe` RequestOnly
      classify (AMerge tid "a" "b" 1) `shouldBe` OwnerNative
      classify (ARedact tid 1) `shouldBe` OwnerNative
      classify (ADelegate (fst owner) (fst owner) 1) `shouldBe` OwnerNative
      classify (ARevoke (fst owner) (fst owner) 1) `shouldBe` OwnerNative

    it "keeps a contributor's reply channel out of a log" $ do
      alice <- kp
      sig <- someHash
      -- The channel lives outside the signed letter precisely so a
      -- contributor's mailbox key never reaches canon, and a derived Show is
      -- the same leak by a shorter route: every refusal carrying a Pending is
      -- something a triage loop logs.
      show (ReplyTo (fst alice) sig) `shouldBe` "ReplyTo <hidden>"
      show NoReply `shouldBe` "NoReply"

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
          tid = expectJust (letterThreadId opening)
          reply = makeLetter (fst alice) (snd alice)
                    (AComment tid Nothing (Just "reply") Nothing 2) noReplyChannel
      -- On an open the two coincide; on a reply they must not, or an
      -- AckRecord (whose akThread is the thread) would never correlate.
      letterEventId opening `shouldBe` Just tid
      letterThreadId reply `shouldBe` Just tid
      letterEventId reply `shouldNotBe` Just tid

    it "materializes what a letter carried: labels, body, attachment, origin" $ do
      alice <- kp
      owner <- kp
      part  <- someHash
      origin <- someHash
      let ac = AOpen (fst owner) HubIssue "with attachments" ["bug","needs triage"]
                     (Just "inline") (Just part) Nothing 7
          letter = makeLetter (fst alice) (snd alice) ac noReplyChannel
      (box, _, _, _) <- expectRight (openLetter letter)
      let cc eid = CanonContent (fst owner) eid 1 (Just 1) (Just origin) 100 (Just secret32)
          ev = Event box (signCanon (fst owner) (snd owner) (cc (authorBoxId box)))
          fr = foldEvents (fst owner) [ev]
          t = threadOf fr (eventId ev)
      -- Requested labels are visible to triage but NOT applied: a stranger
      -- must not label their own submission.
      tsLabelsRequested t `shouldBe` ["bug","needs triage"]
      HM.lookup "labels" (tsAttrs t) `shouldBe` Nothing
      -- The attachment and the secret to decrypt it travel together.
      tsBody t `shouldBe` Just "inline"
      tsBodyPart t `shouldBe` Just part
      tsPartSecret t `shouldBe` Just secret32
      tsOrigin t `shouldBe` Just origin

    it "gives a revised bundle its own secret, not the opening one" $ do
      alice <- kp
      owner <- kp
      b1 <- someHash
      b2 <- someHash
      let coords1 = coords { prBundle = Just b1 }
          coords2 = coords { prSourceTip = "cccc", prBundle = Just b2 }
          acOpen = AOpen (fst owner) HubPR "pr" [] Nothing Nothing (Just coords1) 1
          acRev  = ARevise (expectJust (letterEventId
                     (makeLetter (fst alice) (snd alice) acOpen noReplyChannel))) coords2 2
          lOpen = makeLetter (fst alice) (snd alice) acOpen noReplyChannel
          lRev  = makeLetter (fst alice) (snd alice) acRev noReplyChannel
      (obox,_,_,_) <- expectRight (openLetter lOpen)
      (rbox,_,_,_) <- expectRight (openLetter lRev)
      -- Each message has its own per-message group key, so each bundle has
      -- its own secret; carrying the opening one forward would hand a
      -- reader the wrong key for the new bundle.
      let mk s sq box = Event box (signCanon (fst owner) (snd owner)
                          (CanonContent (fst owner) (authorBoxId box) sq Nothing Nothing sq (Just s)))
          fr = foldEvents (fst owner) [mk secretA 1 obox, mk secretB 2 rbox]
          t = threadOf fr (eventId (mk secretA 1 obox))
      fmap (prBundle . psCoords) (tsPR t) `shouldBe` Just (Just b2)
      fmap psPartSecret (tsPR t) `shouldBe` Just (Just secretB)

    it "drops a deny-listed inner author, however the envelope was signed" $ do
      mallory <- kp
      relay   <- kp
      owner   <- kp
      let ac = AOpen (fst owner) HubIssue "spam" [] Nothing Nothing Nothing 1
          letter = makeLetter (fst mallory) (snd mallory) ac noReplyChannel
          allowed k = k /= fst mallory
      -- rewrapped under a fresh envelope key, the inner author is still banned
      expectLeft AuthorDenied (openLetterAs allowed (EnvelopeSigner (fst relay)) letter)
      expectLeft AuthorDenied (openLetterAs allowed (EnvelopeSigner (fst mallory)) letter)

    it "applies the deny-list to a letter it cannot decode either" $ do
      mallory <- kp
      let md = MessageData hubMsgVersion (Letter (futureBox mallory) noReplyChannel)
          allowed k = k /= fst mallory
      -- The signature checks out, so the author is known; only the content is
      -- from a schema this build does not speak. For an honest newer sender
      -- that is a retry, which is what an unfiltered open reports.
      expectLeft (UndecodableContent (fst mallory) Undecodable)
        (openLetterAs (const True) (EnvelopeSigner (fst mallory)) md)
      -- For a banned one it would be a retry forever: the key is known and
      -- the ban is the only thing that could ever stop it, so the ban has to
      -- reach this case too.
      expectLeft AuthorDenied (openLetterAs allowed (EnvelopeSigner (fst mallory)) md)

    it "honours the reply channel only from the inner author's own envelope" $ do
      alice <- kp
      relay <- kp
      owner <- kp
      sig <- someHash
      let ac = AOpen (fst owner) HubIssue "t" [] Nothing Nothing Nothing 1
          letter = makeLetter (fst alice) (snd alice) ac (ReplyTo (fst alice) sig)
      (_,_,_,mine) <- expectRight (openLetterAs (const True) (EnvelopeSigner (fst alice)) letter)
      (_,_,_,relayed) <- expectRight (openLetterAs (const True) (EnvelopeSigner (fst relay)) letter)
      mine `shouldBe` ReplyTo (fst alice) sig
      -- a rewrapper could have substituted their own, so it is not honoured
      relayed `shouldBe` NoReply

    it "a reply letter names the thread the sender already knows" $ do
      alice <- kp
      bob   <- kp
      owner <- kp
      let ac = AOpen (fst owner) HubIssue "thread" [] Nothing Nothing Nothing 1
          opening = makeLetter (fst alice) (snd alice) ac noReplyChannel
          tid = expectJust (letterThreadId opening)
          -- bob read the thread-id from public canon and replies to it
          reply = makeLetter (fst bob) (snd bob)
                    (AComment tid Nothing (Just "me too") Nothing 2) noReplyChannel
      (obox, _, _, _) <- expectRight (openLetter opening)
      (rbox, _, _, _) <- expectRight (openLetter reply)
      let fr = foldEvents (fst owner) [bless owner 1 (Just 1) obox, bless owner 2 Nothing rbox]
          t = threadOf fr tid
      map cAuthor (tsComments t) `shouldBe` [fst bob]
