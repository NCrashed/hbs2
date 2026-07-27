module HBS2.Hub.BridgeSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Bridge
import HBS2.Hub.Fold
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.Refs (HashRef)

import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromMaybe)
import Test.Hspec

type KP = (HubKey, PrivKey 'Sign HubScheme)

kp :: IO KP
kp = do
  c <- newCredentials @'HBS2Basic
  pure (_peerSignPk c, _peerSignSk c)

someHash :: IO HashRef
someHash = do
  (pk,sk) <- kp
  pure (authorBoxId (signAuthor pk sk (ARevoke pk 0)))

threadOf :: FoldResult -> ThreadId -> ThreadState
threadOf fr tid = fromMaybe (error "expected thread") (HM.lookup tid (frThreads fr))

expectRight :: Show e => Either e a -> IO a
expectRight = either (\e -> error ("unexpected: " <> show e)) pure

-- Event has no Eq, so assert on the error side by hand.
expectErr :: TriageError -> Either TriageError a -> Expectation
expectErr want = \case
  Left got -> got `shouldBe` want
  Right _  -> expectationFailure ("expected " <> show want)

coords :: PRCoords
coords = PRCoords Nothing "refs/heads/f" "aaaa" "refs/heads/master" "bbbb" Nothing

-- The thread an accepted event landed in.
scopeOf :: Accepted -> ThreadId
scopeOf a = case acScope a of
  ThreadScope t -> t
  RepoScope     -> error "expected a thread scope"

-- Everyone is allowed unless a test says otherwise.
anyone :: HubKey -> Bool
anyone = const True

spec :: Spec
spec = do

  describe "PEP-19 triage bridge" $ do

    it "folds an open letter and mints seq and number" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "hello" [] (Just "b") Nothing Nothing 1)
                     noReplyChannel
      acc <- expectRight
        (acceptLetter owner anyone (fst alice) repo emptyView 100 origin Nothing letter)
      -- The sender's inner box is kept verbatim, so the id it computed holds.
      letterEventId letter `shouldBe` Just (eventId (acEvent acc))
      acNumber acc `shouldBe` Just 1
      cvCursor (acView acc) `shouldBe` CanonCursor 2 2
      let fr = foldEvents repo [acEvent acc]
          t = threadOf fr (eventId (acEvent acc))
      tsNumber t `shouldBe` Just 1
      tsAuthor t `shouldBe` fst alice     -- authorship stays the sender's
      tsOrigin t `shouldBe` Just origin
      tsCreated t `shouldBe` 100

    it "numbers threads in order and does not number replies" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          env = fst alice
          open n = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue n [] Nothing Nothing Nothing 1) noReplyChannel
      a1 <- expectRight (acceptLetter owner anyone env repo emptyView 1 origin Nothing (open "a"))
      a2 <- expectRight (acceptLetter owner anyone env repo (acView a1) 2 origin Nothing (open "b"))
      let reply = makeLetter (fst alice) (snd alice)
                    (AComment (scopeOf a1) Nothing (Just "hi") Nothing 3) noReplyChannel
      a3 <- expectRight (acceptLetter owner anyone env repo (acView a2) 3 origin Nothing reply)
      cvCursor (acView a3) `shouldBe` CanonCursor 4 3
      acNumber a3 `shouldBe` Nothing
      let fr = foldEvents repo (map acEvent [a1,a2,a3])
      tsNumber (threadOf fr (scopeOf a1)) `shouldBe` Just 1
      tsNumber (threadOf fr (scopeOf a2)) `shouldBe` Just 2
      length (tsComments (threadOf fr (scopeOf a1))) `shouldBe` 1

    it "refuses a reply whose thread is not in canon yet" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- Minting this would burn a seq on an event the fold can never admit:
      -- the fold is one ascending pass, so a reply needs a higher seq than
      -- its open, and re-folding would never repair it. Fold the open first.
      let repo = fst owner
          opening = makeLetter (fst alice) (snd alice)
                      (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
          tid = fromMaybe (error "no id") (letterEventId opening)
          reply = makeLetter (fst alice) (snd alice)
                    (AComment tid Nothing (Just "early") Nothing 2) noReplyChannel
      expectErr UnknownThread
        (acceptLetter owner anyone (fst alice) repo emptyView 1 origin Nothing reply)
      -- ...and once the open is folded, the same reply is accepted.
      aOpen <- expectRight
        (acceptLetter owner anyone (fst alice) repo emptyView 1 origin Nothing opening)
      aReply <- expectRight
        (acceptLetter owner anyone (fst alice) repo (acView aOpen) 2 origin Nothing reply)
      let fr = foldEvents repo [acEvent aOpen, acEvent aReply]
      frDropped fr `shouldBe` []
      length (tsComments (threadOf fr tid)) `shouldBe` 1

    it "refuses a deny-listed inner author, however the envelope was signed" $ do
      mallory <- kp
      relay <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          allowed k = k /= fst mallory
          letter = makeLetter (fst mallory) (snd mallory)
                     (AOpen repo HubIssue "spam" [] Nothing Nothing Nothing 1) noReplyChannel
      -- Banning by envelope key is evaded by rewrapping, so the ban is on
      -- the inner author and holds under any envelope.
      expectErr (BadLetter AuthorDenied)
        (acceptLetter owner allowed (fst mallory) repo emptyView 1 origin Nothing letter)
      expectErr (BadLetter AuthorDenied)
        (acceptLetter owner allowed (fst relay) repo emptyView 1 origin Nothing letter)

    it "hands back the vetted reply channel and the minted number" $ do
      alice <- kp
      relay <- kp
      owner <- kp
      origin <- someHash
      sigil <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                     (ReplyTo (fst alice) sigil)
      -- Everything an ack needs, without opening the letter twice.
      mine <- expectRight
        (acceptLetter owner anyone (fst alice) repo emptyView 1 origin Nothing letter)
      acReply mine `shouldBe` ReplyTo (fst alice) sigil
      acNumber mine `shouldBe` Just 1
      -- A rewrapper's envelope does not get to redirect the notification.
      relayed <- expectRight
        (acceptLetter owner anyone (fst relay) repo emptyView 1 origin Nothing letter)
      acReply relayed `shouldBe` NoReply

    it "refuses to bless an owner-native op arriving as a letter" $ do
      mallory <- kp
      owner <- kp
      origin <- someHash
      target <- someHash
      -- The reason the bridge checks at all: the fold would drop this, but a
      -- bridge that blessed whatever arrived would have the owner sign a
      -- redact authored by a stranger.
      let repo = fst owner
          evil = makeLetter (fst mallory) (snd mallory) (ARedact target 1) noReplyChannel
      expectErr (NotAcceptable OwnerNative)
        (acceptLetter owner anyone (fst mallory) repo emptyView 1 origin Nothing evil)

    it "refuses to bless a request as the requester's own event" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      thr <- someHash
      let repo = fst owner
          req = makeLetter (fst alice) (snd alice) (AClose thr Nothing 1) noReplyChannel
      expectErr (NotAcceptable RequestOnly)
        (acceptLetter owner anyone (fst alice) repo emptyView 1 origin Nothing req)

    it "honours a request by re-authoring it under the owner key and clock" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter owner anyone (fst alice) repo emptyView 1 origin Nothing letter)
      let tid = scopeOf aOpen
          -- the requester declares an absurd time; the owner must not adopt it
          req = makeLetter (fst alice) (snd alice)
                  (AClose tid (Just "please") maxBound) noReplyChannel
      aClose <- expectRight (honourRequest owner anyone (fst alice) (acView aOpen) 500 origin req)
      let fr = foldEvents repo [acEvent aOpen, acEvent aClose]
          t = threadOf fr tid
      HM.lookup "status" (tsAttrs t) `shouldBe` Just "closed"
      frDropped fr `shouldBe` []
      map cAuthor (tsComments t) `shouldBe` [fst owner]
      -- the note is the owner's, declared at the owner's clock
      map cAuthorTs (tsComments t) `shouldBe` [500]

    it "refuses a request against an unknown thread" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      thr <- someHash
      let req = makeLetter (fst alice) (snd alice) (AClose thr Nothing 1) noReplyChannel
      expectErr UnknownThread (honourRequest owner anyone (fst alice) emptyView 1 origin req)

    it "rejects a letter authored for another repo" $ do
      alice <- kp
      owner <- kp
      other <- kp
      origin <- someHash
      let letter = makeLetter (fst alice) (snd alice)
                     (AOpen (fst other) HubIssue "t" [] Nothing Nothing Nothing 1)
                     noReplyChannel
      expectErr WrongRepo
        (acceptLetter owner anyone (fst alice) (fst owner) emptyView 1 origin Nothing letter)

    it "rejects content the fold would drop, before burning a seq" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          env = fst alice
          prNoCoords = makeLetter (fst alice) (snd alice)
                         (AOpen repo HubPR "pr" [] Nothing Nothing Nothing 1) noReplyChannel
          issueWithCoords = makeLetter (fst alice) (snd alice)
                              (AOpen repo HubIssue "i" [] Nothing Nothing (Just coords) 1)
                              noReplyChannel
      expectErr BadContent
        (acceptLetter owner anyone env repo emptyView 1 origin Nothing prNoCoords)
      expectErr BadContent
        (acceptLetter owner anyone env repo emptyView 1 origin Nothing issueWithCoords)

    it "refuses a revise from anyone but the author of record" $ do
      alice <- kp
      mallory <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          pr = makeLetter (fst alice) (snd alice)
                 (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords) 1) noReplyChannel
      aPR <- expectRight
        (acceptLetter owner anyone (fst alice) repo emptyView 1 origin Nothing pr)
      let evil = makeLetter (fst mallory) (snd mallory)
                   (ARevise (scopeOf aPR) coords { prSourceTip = "dead" } 2) noReplyChannel
      expectErr NotAuthorOfRecord
        (acceptLetter owner anyone (fst mallory) repo (acView aPR) 2 origin Nothing evil)

    it "refuses the same author box twice" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      acc <- expectRight
        (acceptLetter owner anyone (fst alice) repo emptyView 1 origin Nothing letter)
      -- a rewrapped resend of the very same inner box
      expectErr AlreadyInCanon
        (acceptLetter owner anyone (fst alice) repo (acView acc) 2 origin Nothing letter)

    it "refuses an owner redact whose target is not in canon yet" $ do
      owner <- kp
      ghost <- someHash
      expectErr UnknownThread (ownerEvent owner (fst owner) emptyView 1 Nothing (ARedact ghost 1))

    it "can redact an event that left no visible trace" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- A set leaves nothing in the materialized thread beyond its effect,
      -- so a view rebuilt from threads alone would not know it exists and
      -- would refuse to redact it. The fold reports admitted events, so it
      -- does. This only diverges after a restart, which is why the view is
      -- taken from the fold rather than reconstructed.
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter owner anyone (fst alice) repo emptyView 1 origin Nothing letter)
      aSet <- expectRight
        (ownerEvent owner repo (acView aOpen) 2 Nothing (ASet (scopeOf aOpen) "labels" "bug" 2))
      -- rebuild the view the way a restarted folder would
      let fr = foldEvents repo (map acEvent [aOpen, aSet])
          rebuilt = viewOf fr
      aRedact <- expectRight
        (ownerEvent owner repo rebuilt 3 Nothing (ARedact (eventId (acEvent aSet)) 3))
      -- and it lands with the thread it belongs to, not under its target's id
      acScope aRedact `shouldBe` ThreadScope (scopeOf aOpen)
      let fr' = foldEvents repo (map acEvent [aOpen, aSet, aRedact])
      frDropped fr' `shouldBe` []

    it "catches a resent revise after a restart" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- A revise leaves no trace of its own event-id in the thread, so a
      -- reconstructed view would mint it twice and the fold would drop the
      -- second as a duplicate.
      let repo = fst owner
          pr = makeLetter (fst alice) (snd alice)
                 (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords) 1) noReplyChannel
      aPR <- expectRight
        (acceptLetter owner anyone (fst alice) repo emptyView 1 origin Nothing pr)
      let rev = makeLetter (fst alice) (snd alice)
                  (ARevise (scopeOf aPR) coords { prSourceTip = "cccc" } 2) noReplyChannel
      aRev <- expectRight
        (acceptLetter owner anyone (fst alice) repo (acView aPR) 2 origin Nothing rev)
      let rebuilt = viewOf (foldEvents repo (map acEvent [aPR, aRev]))
      expectErr AlreadyInCanon
        (acceptLetter owner anyone (fst alice) repo rebuilt 3 origin Nothing rev)

    it "refuses delegate and revoke from anyone but the owner key" $ do
      owner <- kp
      bob <- kp
      -- Only the LWWRef owner may delegate (PEP-19 rule 5); a delegate
      -- signing one would be dropped, so the bridge refuses it.
      expectErr UnauthorizedForRepo
        (ownerEvent bob (fst owner) emptyView 1 Nothing (ADelegate (fst bob) 1))
      acc <- expectRight
        (ownerEvent owner (fst owner) emptyView 1 Nothing (ADelegate (fst bob) 1))
      acScope acc `shouldBe` RepoScope

    it "refuses a pr-only owner op on an issue thread" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "an issue" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter owner anyone (fst alice) repo emptyView 1 origin Nothing letter)
      expectErr BadContent
        (ownerEvent owner repo (acView aOpen) 2 Nothing
           (AMerge (scopeOf aOpen) "cafe" "refs/heads/master" 2))

    it "refuses honouring the same request twice" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter owner anyone (fst alice) repo emptyView 1 origin Nothing letter)
      let req = makeLetter (fst alice) (snd alice)
                  (AClose (scopeOf aOpen) Nothing 2) noReplyChannel
      aClose <- expectRight (honourRequest owner anyone (fst alice) (acView aOpen) 9 origin req)
      -- same request, same clock: identical bytes, identical id
      expectErr AlreadyInCanon
        (honourRequest owner anyone (fst alice) (acView aClose) 9 origin req)

    it "publishes the group secret only when something is encrypted" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      part <- someHash
      let repo = fst owner
          env = fst alice
          plain = makeLetter (fst alice) (snd alice)
                    (AOpen repo HubIssue "t" [] (Just "inline") Nothing Nothing 1) noReplyChannel
          withPart = makeLetter (fst alice) (snd alice)
                       (AOpen repo HubIssue "t" [] Nothing (Just part) Nothing 1) noReplyChannel
      a1 <- expectRight (acceptLetter owner anyone env repo emptyView 1 origin (Just "S") plain)
      a2 <- expectRight (acceptLetter owner anyone env repo emptyView 1 origin (Just "S") withPart)
      -- No attachment, no secret in canon: publishing one would be noise.
      tsPartSecret (threadOf (foldEvents repo [acEvent a1]) (scopeOf a1)) `shouldBe` Nothing
      tsPartSecret (threadOf (foldEvents repo [acEvent a2]) (scopeOf a2)) `shouldBe` Just "S"

    it "carries the secret for a pr bundle" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      bundle <- someHash
      let repo = fst owner
          pr = makeLetter (fst alice) (snd alice)
                 (AOpen repo HubPR "pr" [] Nothing Nothing
                    (Just coords { prBundle = Just bundle }) 1)
                 noReplyChannel
      acc <- expectRight
        (acceptLetter owner anyone (fst alice) repo emptyView 1 origin (Just "S") pr)
      let t = threadOf (foldEvents repo [acEvent acc]) (scopeOf acc)
      fmap psPartSecret (tsPR t) `shouldBe` Just (Just "S")

    it "derives the next cursor from canon, not from a local counter" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          env = fst alice
          open n = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue n [] Nothing Nothing Nothing 1) noReplyChannel
      a1 <- expectRight (acceptLetter owner anyone env repo emptyView 1 origin Nothing (open "a"))
      a2 <- expectRight (acceptLetter owner anyone env repo (acView a1) 2 origin Nothing (open "b"))
      -- A folder that restarts and rebuilds from canon mints the same next
      -- values as the one that never stopped.
      cursorFrom (foldEvents repo (map acEvent [a1,a2])) `shouldBe` cvCursor (acView a2)

    it "admits everything it mints" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- The bridge's whole job: whatever it accepts, the fold takes. Nothing
      -- it mints should ever end up in frDropped.
      let repo = fst owner
          env = fst alice
          opening = makeLetter (fst alice) (snd alice)
                      (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords) 1) noReplyChannel
      a1 <- expectRight (acceptLetter owner anyone env repo emptyView 1 origin Nothing opening)
      let cmt = makeLetter (fst alice) (snd alice)
                  (AComment (scopeOf a1) Nothing (Just "hi") Nothing 2) noReplyChannel
      a2 <- expectRight (acceptLetter owner anyone env repo (acView a1) 2 origin Nothing cmt)
      let rev = makeLetter (fst alice) (snd alice)
                  (ARevise (scopeOf a1) coords { prSourceTip = "cccc" } 3) noReplyChannel
      a3 <- expectRight (acceptLetter owner anyone env repo (acView a2) 3 origin Nothing rev)
      a4 <- expectRight (ownerEvent owner repo (acView a3) 4 Nothing
                          (AMerge (scopeOf a1) "cafe" "refs/heads/master" 4))
      let fr = foldEvents repo (map acEvent [a1,a2,a3,a4])
      frDropped fr `shouldBe` []
      HM.lookup "status" (tsAttrs (threadOf fr (scopeOf a1))) `shouldBe` Just "merged"
