module HBS2.Hub.BridgeSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Bridge
import HBS2.Hub.Fold
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.GroupKeySymm (typicalKeyLength)
import HBS2.Data.Types.Refs (HashRef(..))

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Codec.Serialise (serialise)
import Data.HashMap.Strict qualified as HM
import Data.List (isInfixOf)
import Data.String (fromString)
import Data.Text qualified as Text
import Data.Maybe (fromMaybe)
import Test.Hspec

type KP = (HubKey, PrivKey 'Sign HubScheme)

kp :: IO KP
kp = do
  c <- newCredentials @'HBS2Basic
  pure (_peerSignPk c, _peerSignSk c)

-- | A fresh hash, used as a Mailbox message hash (an origin) and as a stand-in
-- for an event-id that is not in canon.
--
-- Every letter needs its OWN origin: a message carries one letter and folds to
-- at most one event, so the bridge refuses a second event from an origin canon
-- already records (PEP-19).
someHash :: IO HashRef
someHash = do
  (pk,sk) <- kp
  pure (authorBoxId (signAuthor pk sk (ARevoke pk pk 0)))

threadOf :: FoldResult -> ThreadId -> ThreadState
threadOf fr tid = fromMaybe (error "expected thread") (HM.lookup tid (frThreads fr))

expectRight :: Show e => Either e a -> IO a
expectRight = either (\e -> error ("unexpected: " <> show e)) pure

-- A refusal that carries a request: the caller is meant to rule on it.
expectRequested :: Either TriageError Accepted -> IO Pending
expectRequested = \case
  Left (Requested p) -> pure p
  Left e  -> error ("expected a request, got " <> show e)
  Right _ -> error "expected a request, got an accept"

-- Every refusal from ownerEvent is Composed, because there is no letter on
-- that path: nothing it judges is anything a sender did.
expectOwn :: TriageError -> Either TriageError a -> Expectation
expectOwn want = expectErr (Composed want)

-- Event has no Eq, so assert on the error side by hand.
expectErr :: TriageError -> Either TriageError a -> Expectation
expectErr want = \case
  Left got -> got `shouldBe` want
  Right _  -> expectationFailure ("expected " <> show want)

-- A group secret is raw key bytes of a fixed size, and the constructor checks
-- only that; telling the parts secret from the message secret is what
-- 'poMessage' is for.
secret32 :: PartSecret
secret32 = fromMaybe (error "bad fixture secret")
             (mkPartSecret (BS.replicate typicalKeyLength 0x41))

-- The secret over messageData, which a reader of a letter always has: it read
-- the letter with it. Distinct from the parts secret by construction here, as
-- it must be on the wire (PEP-18).
-- The same bytes as a part secret, which is the mistake the two types exist
-- to make visible: a caller holding one value cannot tell which it is.
sameAs :: PartSecret -> MessageSecret
sameAs = fromMaybe (error "same secret") . mkMessageSecret . partSecretBytes

msgSecret :: MessageSecret
msgSecret = fromMaybe (error "bad fixture secret")
              (mkMessageSecret (BS.replicate typicalKeyLength 0x42))

-- A message that carried no attachments, which is what almost every letter
-- here is. Not the same as a caller with nothing to say about attachments:
-- there is no letter-side value for that any more, because a reader of a
-- letter always holds the secret it read the letter with.
noParts :: LetterParts
noParts = noMessageParts msgSecret

-- A part that is here and small enough to carry. There is no way to say
-- "fetched" without saying how big, which is the number the gate reads.
here :: HashRef -> (HashRef, PartEvidence)
here h = (h, PartHere 1024)

coords :: PRCoords
-- A fork-pointer PR: PEP-20 requires one of the two ways to fetch the
-- change, so a coords with neither is refused (reachableCoords).
coords = PRCoords (Just "hbs23://fork") "refs/heads/f" "aaaa" "refs/heads/master" "bbbb" Nothing

-- The thread an accepted event landed in.
scopeOf :: Accepted -> ThreadId
scopeOf a = case acScope a of
  ThreadScope t -> t
  RepoScope     -> error "expected a thread scope"

-- The fixed part of a triage run: this owner, this repo, everyone allowed.
ctxOf :: KP -> TriageCtx
ctxOf owner = TriageCtx owner (const True) (fst owner)

-- ...and the same for a delegate folding on the owner's repo.
ctxAs :: KP -> RepoRef -> TriageCtx
ctxAs kpr repo = TriageCtx kpr (const True) repo

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
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 100 origin noParts letter)
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
      o1 <- someHash
      o2 <- someHash
      o3 <- someHash
      let repo = fst owner
          env = fst alice
          open n = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue n [] Nothing Nothing Nothing 1) noReplyChannel
      a1 <- expectRight (acceptLetter (ctxOf owner) (EnvelopeSigner env) (emptyView repo) 1 o1 noParts (open "a"))
      a2 <- expectRight (acceptLetter (ctxOf owner) (EnvelopeSigner env) (acView a1) 2 o2 noParts (open "b"))
      let reply = makeLetter (fst alice) (snd alice)
                    (AComment (scopeOf a1) Nothing (Just "hi") Nothing 3) noReplyChannel
      a3 <- expectRight (acceptLetter (ctxOf owner) (EnvelopeSigner env) (acView a2) 3 o3 noParts reply)
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
      origin2 <- someHash
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
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin2 noParts reply)
      -- ...and once the open is folded, the same reply is accepted.
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts opening)
      aReply <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (acView aOpen) 2 origin2 noParts reply)
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
        (acceptLetter (TriageCtx owner allowed repo) (EnvelopeSigner (fst mallory)) (emptyView repo) 1 origin noParts letter)
      expectErr (BadLetter AuthorDenied)
        (acceptLetter (TriageCtx owner allowed repo) (EnvelopeSigner (fst relay)) (emptyView repo) 1 origin noParts letter)

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
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts letter)
      acReply mine `shouldBe` ReplyTo (fst alice) sigil
      acNumber mine `shouldBe` Just 1
      -- A rewrapper's envelope does not get to redirect the notification.
      relayed <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst relay)) (emptyView repo) 1 origin noParts letter)
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
          evil = makeLetter (fst mallory) (snd mallory) (ARedact repo target 1) noReplyChannel
      expectErr (NotAcceptable OwnerNative)
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst mallory)) (emptyView repo) 1 origin noParts evil)

    it "refuses to bless a request as the requester's own event" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts letter)
      origin2 <- someHash
      let req = makeLetter (fst alice) (snd alice) (AClose (scopeOf aOpen) Nothing 2) noReplyChannel
      p <- expectRequested
             (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (acView aOpen) 2 origin2 noParts req)
      pdAuthor p `shouldBe` fst alice

    -- A request whose thread is not in canon is early, exactly as a comment
    -- naming the same thread would be: raising it as a decision would produce
    -- a request that 'honourRequest' refuses.
    it "treats a request naming an unknown thread as early, not as a decision" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      thr <- someHash
      let repo = fst owner
          req = makeLetter (fst alice) (snd alice) (AClose thr Nothing 1) noReplyChannel
          got = acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts req
      expectErr UnknownThread got
      either outcome (const Decide) got `shouldBe` Retry

    it "honours a request by re-authoring it under the owner key and clock" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts letter)
      let tid = scopeOf aOpen
          -- the requester declares an absurd time; the owner must not adopt it
          req = makeLetter (fst alice) (snd alice)
                  (AClose tid Nothing maxBound) noReplyChannel
      origin2 <- someHash
      aClose <- expectRight (honourRequest (ctxOf owner) (EnvelopeSigner (fst alice)) (acView aOpen) 500 origin2 req)
      let fr = foldEvents repo [acEvent aOpen, acEvent aClose]
          t = threadOf fr tid
      HM.lookup "status" (tsAttrs t) `shouldBe` Just "closed"
      frDropped fr `shouldBe` []
      -- the event is the owner's, declared at the owner's clock
      acAuthor aClose `shouldBe` fst owner
      authorTs (acContent aClose) `shouldBe` 500

    it "will not sign a stranger's prose as the owner's own words" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts letter)
      let tid = scopeOf aOpen
          -- a closing note becomes a comment authored by whoever signs the
          -- event, so honouring this verbatim publishes a stranger's text as
          -- the maintainer's, in every clone, forever
          withNote = makeLetter (fst alice) (snd alice)
                       (AClose tid (Just "shipped, obviously") 2) noReplyChannel
          -- and a set is the requester's choice of both name and value
          withAttr = makeLetter (fst alice) (snd alice)
                       (ASet tid "assignees" "alice" 2) noReplyChannel
      origin2 <- someHash
      expectErr NeedsReview
        (honourRequest (ctxOf owner) env (acView aOpen) 2 origin2 withNote)
      expectErr NeedsReview
        (honourRequest (ctxOf owner) env (acView aOpen) 2 origin2 withAttr)
      outcome NeedsReview `shouldBe` Decide
      -- ...and honourWith is how a maintainer says yes in their own words
      acc <- expectRight
        (honourWith (ctxOf owner) env (acView aOpen) 7 origin2
           (AClose tid (Just "agreed, thanks") 2) withNote)
      let t = threadOf (foldEvents repo [acEvent aOpen, acEvent acc]) tid
      map cAuthor (tsComments t) `shouldBe` [fst owner]
      map cBody (tsComments t) `shouldBe` [Just "agreed, thanks"]
      map cAuthorTs (tsComments t) `shouldBe` [7]

    it "refuses a request against an unknown thread" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      thr <- someHash
      let req = makeLetter (fst alice) (snd alice) (AClose thr Nothing 1) noReplyChannel
      -- Wrapped: what the owner would be signing names a thread canon does not
      -- hold, which says nothing about the letter and must not delete it.
      -- The letter named a thread canon does not hold: order, not validity, and
      -- the letter must wait rather than be deleted.
      expectErr UnknownThread
        (honourRequest (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView (fst owner)) 1 origin req)

    it "rejects a letter authored for another repo" $ do
      alice <- kp
      owner <- kp
      other <- kp
      origin <- someHash
      let letter = makeLetter (fst alice) (snd alice)
                     (AOpen (fst other) HubIssue "t" [] Nothing Nothing Nothing 1)
                     noReplyChannel
      expectErr WrongRepo
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView (fst owner)) 1 origin noParts letter)

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
        (acceptLetter (ctxOf owner) (EnvelopeSigner env) (emptyView repo) 1 origin noParts prNoCoords)
      expectErr BadContent
        (acceptLetter (ctxOf owner) (EnvelopeSigner env) (emptyView repo) 1 origin noParts issueWithCoords)

    it "refuses a revise from anyone but the author of record" $ do
      alice <- kp
      mallory <- kp
      owner <- kp
      origin <- someHash
      origin2 <- someHash
      let repo = fst owner
          pr = makeLetter (fst alice) (snd alice)
                 (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords) 1) noReplyChannel
      aPR <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts pr)
      let evil = makeLetter (fst mallory) (snd mallory)
                   (ARevise (scopeOf aPR) coords { prSourceTip = "dead" } 2) noReplyChannel
      expectErr NotAuthorOfRecord
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst mallory)) (acView aPR) 2 origin2 noParts evil)

    it "refuses the same author box twice" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- A rewrap arrives as a NEW Mailbox message, so the origin differs and
      -- only the author box catches the resend.
      origin2 <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      acc <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts letter)
      -- a rewrapped resend of the very same inner box
      expectErr AlreadyInCanon
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (acView acc) 2 origin2 noParts letter)

    it "refuses an owner redact whose target is not in canon yet" $ do
      owner <- kp
      ghost <- someHash
      expectOwn UnknownTarget (ownerEvent (ctxOf owner) (emptyView (fst owner)) 1 noOwnAttachments (ARedact (fst owner) ghost 1))

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
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts letter)
      aSet <- expectRight
        (ownerEvent (ctxOf owner) (acView aOpen) 2 noOwnAttachments (ASet (scopeOf aOpen) "labels" "bug" 2))
      -- rebuild the view the way a restarted folder would
      let fr = foldEvents repo (map acEvent [aOpen, aSet])
          rebuilt = viewOf fr
      aRedact <- expectRight
        (ownerEvent (ctxOf owner) rebuilt 3 noOwnAttachments (ARedact repo (eventId (acEvent aSet)) 3))
      -- and it lands with the thread it belongs to, not under its target's id
      acScope aRedact `shouldBe` ThreadScope (scopeOf aOpen)
      let fr' = foldEvents repo (map acEvent [aOpen, aSet, aRedact])
      frDropped fr' `shouldBe` []

    it "catches a resent revise after a restart" $ do
      o2 <- someHash
      o3 <- someHash
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
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts pr)
      let rev = makeLetter (fst alice) (snd alice)
                  (ARevise (scopeOf aPR) coords { prSourceTip = "cccc" } 2) noReplyChannel
      aRev <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (acView aPR) 2 o2 noParts rev)
      let rebuilt = viewOf (foldEvents repo (map acEvent [aPR, aRev]))
      expectErr AlreadyInCanon
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) rebuilt 3 o3 noParts rev)

    it "refuses to fold under a revoked maintainer key" $ do
      owner <- kp
      bob <- kp
      alice <- kp
      origin <- someHash
      -- A revoked maintainer can still sign; the fold just will not admit
      -- what they signed. Minting would consume a triaged letter and leave
      -- a useless event, and once retention prunes the mailbox copy the
      -- submission is gone.
      let repo = fst owner
          letter n = makeLetter (fst alice) (snd alice)
                       (AOpen repo HubIssue n [] Nothing Nothing Nothing 1) noReplyChannel
      aDel <- expectRight (ownerEvent (ctxOf owner) (emptyView (fst owner)) 1 noOwnAttachments (ADelegate (fst owner) (fst bob) 1))
      -- while delegated, bob may fold
      aOk <- expectRight
        (acceptLetter (ctxAs bob repo) (EnvelopeSigner (fst alice)) (acView aDel) 2 origin noParts (letter "ok"))
      aRev <- expectRight (ownerEvent (ctxOf owner) (acView aOk) 3 noOwnAttachments (ARevoke repo (fst bob) 3))
      -- the view is rebuilt from canon, fully up to date, and bob is out
      let rebuilt = viewOf (foldEvents repo (map acEvent [aDel, aOk, aRev]))
      expectErr UnauthorizedForRepo
        (acceptLetter (ctxAs bob repo) (EnvelopeSigner (fst alice)) rebuilt 4 origin noParts (letter "after"))
      expectOwn UnauthorizedForRepo
        (ownerEvent (ctxAs bob repo) rebuilt 4 noOwnAttachments (ASet (scopeOf aOk) "status" "closed" 4))
      -- and the owner still can
      _ <- expectRight (ownerEvent (ctxOf owner) rebuilt 4 noOwnAttachments
                          (ASet (scopeOf aOk) "status" "closed" 4))
      pure ()

    it "lets a live delegate fold letters and owner events" $ do
      owner <- kp
      bob <- kp
      alice <- kp
      origin <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aDel <- expectRight (ownerEvent (ctxOf owner) (emptyView (fst owner)) 1 noOwnAttachments (ADelegate (fst owner) (fst bob) 1))
      aOpen <- expectRight
        (acceptLetter (ctxAs bob repo) (EnvelopeSigner (fst alice)) (acView aDel) 2 origin noParts letter)
      aSet <- expectRight
        (ownerEvent (ctxAs bob repo) (acView aOpen) 3 noOwnAttachments (ASet (scopeOf aOpen) "status" "closed" 3))
      -- and canon takes all of it
      let fr = foldEvents repo (map acEvent [aDel, aOpen, aSet])
      frDropped fr `shouldBe` []
      HM.lookup "status" (tsAttrs (threadOf fr (scopeOf aOpen))) `shouldBe` Just "closed"

    it "refuses delegate and revoke from a delegate, not just from a stranger" $ do
      owner <- kp
      bob <- kp
      carol <- kp
      -- bob is a real maintainer here, so this exercises the root-of-trust
      -- guard rather than the general authority check.
      aDel <- expectRight (ownerEvent (ctxOf owner) (emptyView (fst owner)) 1 noOwnAttachments (ADelegate (fst owner) (fst bob) 1))
      authorizedCanon (acView aDel) (fst bob) `shouldBe` True
      expectOwn OwnerKeyRequired
        (ownerEvent (ctxAs bob (fst owner)) (acView aDel) 2 noOwnAttachments (ADelegate (fst owner) (fst carol) 2))
      expectOwn OwnerKeyRequired
        (ownerEvent (ctxAs bob (fst owner)) (acView aDel) 2 noOwnAttachments (ARevoke (fst owner) (fst bob) 2))
      -- the owner still may
      acc <- expectRight
        (ownerEvent (ctxOf owner) (acView aDel) 2 noOwnAttachments (ADelegate (fst owner) (fst carol) 2))
      acScope acc `shouldBe` RepoScope

    it "refuses a stranger's delegate too" $ do
      owner <- kp
      bob <- kp
      -- A stranger is not authorized to bless anything at all, so this stops
      -- at the general authority check and never reaches the owner-only rule.
      expectOwn UnauthorizedForRepo
        (ownerEvent (ctxAs bob (fst owner)) (emptyView (fst owner)) 1 noOwnAttachments (ADelegate (fst owner) (fst bob) 1))

    it "puts a redact of a repo-scope event under repo scope" $ do
      owner <- kp
      bob <- kp
      aDel <- expectRight
        (ownerEvent (ctxOf owner) (emptyView (fst owner)) 1 noOwnAttachments (ADelegate (fst owner) (fst bob) 1))
      aRed <- expectRight
        (ownerEvent (ctxOf owner) (acView aDel) 2 noOwnAttachments
           (ARedact (fst owner) (eventId (acEvent aDel)) 2))
      acScope aRed `shouldBe` RepoScope

    it "reports an unknown redact target as such, not as an unknown thread" $ do
      owner <- kp
      ghost <- someHash
      expectOwn UnknownTarget
        (ownerEvent (ctxOf owner) (emptyView (fst owner)) 1 noOwnAttachments (ARedact (fst owner) ghost 1))

    it "reports a revise on an issue thread as bad content" $ do
      origin2 <- someHash
      alice <- kp
      owner <- kp
      origin <- someHash
      -- The author is right; the thread is the wrong kind, and the fold
      -- would say BadKind, so the bridge must not blame the author.
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "an issue" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts letter)
      let rev = makeLetter (fst alice) (snd alice)
                  (ARevise (scopeOf aOpen) coords 2) noReplyChannel
      expectErr BadContent
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (acView aOpen) 2 origin2 noParts rev)

    it "reports seq, author and content for the writer" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          content = AOpen repo HubIssue "t" [] Nothing Nothing Nothing 7
          letter = makeLetter (fst alice) (snd alice) content noReplyChannel
      acc <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts letter)
      -- everything the tree writer needs, without unboxing anything again
      acSeq acc `shouldBe` 1
      acAuthor acc `shouldBe` fst alice
      acContent acc `shouldBe` content

    it "signs what triage decided, not what the requester wrote" $ do
      mallory <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          letter = makeLetter (fst mallory) (snd mallory)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst mallory)) (emptyView repo) 1 origin noParts letter)
      let tid = scopeOf aOpen
          -- a request to set an attribute of the requester's choosing
          req = makeLetter (fst mallory) (snd mallory)
                  (ASet tid "assignees" "mallory" 2) noReplyChannel
          -- triage agrees to something else entirely
          edited = ASet tid "labels" "wontfix" 2
      origin2 <- someHash
      acc <- expectRight
        (honourWith (ctxOf owner) (EnvelopeSigner (fst mallory)) (acView aOpen) 5 origin2 edited req)
      acContent acc `shouldBe` withAuthorTs 5 edited
      let fr = foldEvents repo [acEvent aOpen, acEvent acc]
          t = threadOf fr tid
      HM.lookup "labels" (tsAttrs t) `shouldBe` Just "wontfix"
      HM.lookup "assignees" (tsAttrs t) `shouldBe` Nothing

    it "refuses a pr-only owner op on an issue thread" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "an issue" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts letter)
      expectOwn BadContent
        (ownerEvent (ctxOf owner) (acView aOpen) 2 noOwnAttachments
           (AMerge (scopeOf aOpen) "cafe" "refs/heads/master" 2))

    it "refuses honouring the same request twice" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts letter)
      let req = makeLetter (fst alice) (snd alice)
                  (AClose (scopeOf aOpen) Nothing 2) noReplyChannel
      origin2 <- someHash
      aClose <- expectRight
        (honourRequest (ctxOf owner) (EnvelopeSigner (fst alice)) (acView aOpen) 9 origin2 req)
      -- Same letter at a DIFFERENT clock: the re-authored event gets a fresh
      -- id, so only the recorded origin catches it. This is what a triage
      -- loop hits after a restart, re-reading a mailbox it already drained.
      expectErr AlreadyHonoured
        (honourRequest (ctxOf owner) (EnvelopeSigner (fst alice)) (acView aClose) 99 origin2 req)

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
      a1 <- expectRight (acceptLetter (ctxOf owner) (EnvelopeSigner env) (emptyView repo) 1 origin (attachments secret32 msgSecret []) plain)
      a2 <- expectRight (acceptLetter (ctxOf owner) (EnvelopeSigner env) (emptyView repo) 1 origin (attachments secret32 msgSecret [here part]) withPart)
      -- No attachment, no secret in canon: publishing one would be noise.
      tsPartSecret (threadOf (foldEvents repo [acEvent a1]) (scopeOf a1)) `shouldBe` Nothing
      tsPartSecret (threadOf (foldEvents repo [acEvent a2]) (scopeOf a2)) `shouldBe` Just secret32

    it "refuses to publish an encrypted part with no key to it" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      part <- someHash
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          withPart = makeLetter (fst alice) (snd alice)
                       (AOpen repo HubIssue "t" [] Nothing (Just part) Nothing 1) noReplyChannel
      -- The alternative is canon holding a signed reference to bytes nobody
      -- can ever read: the part cannot be re-encrypted without breaking the
      -- signature, and the secret is gone once the message is deleted.
      --
      -- Four ways to get there, and they are not the same answer. Named but
      -- not carried is a dead reference and always will be.
      let accept po = acceptLetter (ctxOf owner) env (emptyView repo) 1 origin po withPart
      -- A message that carried nothing is not the caller saying nothing.
      -- Reading the two alike let any stranger wedge the loop with a letter
      -- naming a part their message does not have, and there is no longer a
      -- letter-side way to say the second thing: every builder takes the
      -- message secret, which a reader of a letter always holds.
      expectErr (PartNotInMessage part) (accept noParts)
      either outcome (const Decide) (accept noParts) `shouldBe` Discard
      expectErr (PartNotInMessage part) (accept (attachmentsNoKey msgSecret []))
      either outcome (const Decide) (accept (attachmentsNoKey msgSecret []))
        `shouldBe` Discard
      -- The caller reaching for the empty value IS still a stop, on the one
      -- path where it can happen: an owner-native event has no message behind
      -- it, so 'noOwnAttachments' is the only value that says nothing at all.
      expectOwn NoAttachmentsSupplied
        (ownerEvent (ctxOf owner) (emptyView repo) 1 noOwnAttachments
           (AOpen repo HubIssue "t" [] Nothing (Just part) Nothing 1))
      outcome (Composed NoAttachmentsSupplied) `shouldBe` Abort
      -- ...but once it HAS supplied evidence, a part that evidence does not
      -- list is the letter's doing, and anyone can send such a letter. Abort
      -- here would let one stranger wedge triage for good: the letter stays in
      -- the mailbox and every later pass hits it again.
      other <- someHash
      expectErr (PartNotInMessage part) (accept (attachments secret32 msgSecret [here other]))
      either outcome (const Decide) (accept (attachments secret32 msgSecret [here other]))
        `shouldBe` Discard
      -- Carried but not fetched yet is a wait.
      let notYet = attachments secret32 msgSecret [(part, PartPending 1024)]
      expectErr (PartNotFetched part) (accept notYet)
      either outcome (const Decide) (accept notYet) `shouldBe` Retry
      -- Here, but with no key to it, and with a key that cannot be one.

      expectErr MissingPartSecret (accept (attachmentsNoKey msgSecret [here part]))
      either outcome (const Decide) (accept (attachmentsNoKey msgSecret [here part])) `shouldBe` Retry
      -- ...and the one the type exists for: the message's own secret offered
      -- as the parts secret would publish the letter's envelope, and with it
      -- the sender's private reply address, to every clone.
      -- Discard, not Abort: the sender picks this too, since nothing stops
      -- them encrypting their parts with the message key and a Message is a
      -- box anyone can build. Minting is still refused; handing them the loop
      -- as well would not be. Nor is it deleted: the identical shape is what a
      -- caller that wrapped one secret twice produces, and discarding would
      -- then take the only copy of the part secret with it.
      let wrongKey = attachments secret32 (sameAs secret32) [here part]
      expectErr MessageSecretOffered (accept wrongKey)
      either outcome (const Decide) (accept wrongKey) `shouldBe` Park
      -- ...which is why the convenient constructor demands the message secret
      -- rather than defaulting it away. The owner path has its own, because
      -- an owner-native event arrived in no message at all.
      expectErr MessageSecretOffered
        (accept (attachments secret32 (sameAs secret32) [here part]))
      _ <- expectRight (accept (attachments secret32 msgSecret [here part]))
      _ <- expectRight
        (ownerEvent (ctxOf owner) (emptyView repo) 1 (ownAttachments secret32 [here part])
           (AOpen repo HubIssue "mine" [] Nothing (Just part) Nothing 1))
      pure ()
      _ <- expectRight (accept (attachments secret32 msgSecret [here part]))
      -- On the owner path the mistake this type exists for cannot be made at
      -- all: there is no message, so the builder takes the parts secret and
      -- nothing else. What is still catchable there is vouching for nothing.
      expectOwn NoAttachmentsSupplied
        (ownerEvent (ctxOf owner) (emptyView repo) 1 noOwnAttachments
           (AOpen repo HubIssue "mine" [] Nothing (Just part) Nothing 1))

    it "checks wiring and dedup before it asks a human" $ do
      alice <- kp
      owner <- kp
      other <- kp
      origin <- someHash
      -- A request with a note is NeedsReview, but only once it is a request
      -- worth reviewing. A view wired to another repo is a bug to stop on, and
      -- a letter already folded would have the maintainer write a reply that
      -- honourWith then refuses: both must win over the review prompt.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts letter)
      reqOrigin <- someHash
      let req = makeLetter (fst alice) (snd alice)
                  (AClose (scopeOf aOpen) (Just "please close") 2) noReplyChannel
      -- wrong repo first
      expectErr ViewRepoMismatch
        (honourRequest (ctxOf owner) env (emptyView (fst other)) 2 reqOrigin req)
      -- already folded first
      aClose <- expectRight
        (honourWith (ctxOf owner) env (acView aOpen) 2 reqOrigin
           (AClose (scopeOf aOpen) (Just "closing") 2) req)
      expectErr AlreadyHonoured
        (honourRequest (ctxOf owner) env (acView aClose) 3 reqOrigin req)
      -- ...and with neither in the way, the review prompt
      other2 <- someHash
      expectErr NeedsReview
        (honourRequest (ctxOf owner) env (acView aOpen) 2 other2 req)

    it "refuses to sign an attribute value nobody canonicalized" $ do
      owner <- kp
      alice <- kp
      origin <- someHash
      -- The same two labels in another order are other bytes and so another
      -- event-id, which is what the canonical form exists to prevent. The
      -- bridge will not quietly rewrite what the owner signs, and the fold
      -- can neither fix nor drop it, so it has to be refused here.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts letter)
      let tid = scopeOf aOpen
      expectOwn (UnnormalizedValue "labels")
        (ownerEvent (ctxOf owner) (acView aOpen) 2 noOwnAttachments (ASet tid "labels" "ui,bug" 2))
      -- The letter path answers for the sender, who chose those bytes; the
      -- honour path answers for the caller, who did not have to repeat them.
      outcome (UnnormalizedValue "labels") `shouldBe` Discard
      outcome (Composed (UnnormalizedValue "labels")) `shouldBe` Abort
      -- normalized is fine, and so is a scalar attribute
      _ <- expectRight
        (ownerEvent (ctxOf owner) (acView aOpen) 2 noOwnAttachments
           (ASet tid "labels" (normalizeAttr "labels" "ui,bug") 2))
      _ <- expectRight
        (ownerEvent (ctxOf owner) (acView aOpen) 2 noOwnAttachments (ASet tid "status" "b,a" 2))
      -- and honourWith signs owner content too, so it is checked there
      reqOrigin <- someHash
      let req = makeLetter (fst alice) (snd alice) (AClose tid Nothing 2) noReplyChannel
      expectErr (Composed (UnnormalizedValue "labels"))
        (honourWith (ctxOf owner) env (acView aOpen) 2 reqOrigin
           (ASet tid "labels" "ui,bug" 2) req)

    it "parks a letter from a newer schema instead of discarding it" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- End to end, not just the outcome table: a v2 letter reaching the
      -- bridge must come back as something a loop keeps rather than deletes,
      -- and stops re-checking every pass.
      let repo = fst owner
          v2 = MessageData (hubMsgVersion + 1)
                 (mdBody (makeLetter (fst alice) (snd alice)
                            (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                            noReplyChannel))
          got = acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo)
                  1 origin noParts v2
      expectErr (BadLetter (UnsupportedVersion (hubMsgVersion + 1))) got
      either outcome (const Decide) got `shouldBe` Park

    it "refuses to honour a letter that was not a request" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- An open folds on its own; honouring it would re-author a stranger's
      -- submission under the owner's key and lose the authorship the whole
      -- design exists to keep.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts letter)
      origin2 <- someHash
      expectErr (NotAcceptable FoldsToCanon)
        (honourRequest (ctxOf owner) env (acView aOpen) 2 origin2 letter)
      expectErr (NotAcceptable FoldsToCanon)
        (honourWith (ctxOf owner) env (acView aOpen) 2 origin2
           (AClose (scopeOf aOpen) Nothing 2) letter)

    it "does not blame the letter for what triage composed" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- Alice sends a perfectly good close request and the maintainer's tooling
      -- builds the wrong op. Discarding here deletes a valid request over a bug
      -- on the other side of the screen.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          opening = makeLetter (fst alice) (snd alice)
                      (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts opening)
      let tid = scopeOf aOpen
          req = makeLetter (fst alice) (snd alice) (AClose tid Nothing 2) noReplyChannel
      origin2 <- someHash
      expectErr (Composed (NotAcceptable OwnerNative))
        (honourWith (ctxOf owner) env (acView aOpen) 2 origin2 (AMerge tid "abc" "master" 2) req)
      expectErr (Composed (NotAcceptable OwnerNative))
        (honourWith (ctxOf owner) env (acView aOpen) 2 origin2 (ARedact repo tid 2) req)
      either outcome (const Decide)
        (honourWith (ctxOf owner) env (acView aOpen) 2 origin2 (ARedact repo tid 2) req)
        `shouldBe` Abort
      -- ...and the request itself is still there to be honoured properly
      _ <- expectRight
        (honourWith (ctxOf owner) env (acView aOpen) 2 origin2
           (AClose tid (Just "agreed") 2) req)
      pure ()

    it "never stamps a folded-ts below the last one in canon" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      origin2 <- someHash
      -- A maintainer whose clock is corrected backwards would otherwise mint
      -- canon that hub verify complains about forever, and no later event can
      -- fix a signed timestamp. Clamped rather than refused: refusing would
      -- let one fast clock stop every other folder until real time caught up.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          open n = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue n [] Nothing Nothing Nothing 1) noReplyChannel
      a1 <- expectRight (acceptLetter (ctxOf owner) env (emptyView repo) 9000 origin noParts (open "a"))
      a2 <- expectRight (acceptLetter (ctxOf owner) env (acView a1) 1000 origin2 noParts (open "b"))
      let fr = foldEvents repo (map acEvent [a1,a2])
      frDropped fr `shouldBe` []
      frAnomalies fr `shouldBe` []
      tsCreated (threadOf fr (scopeOf a2)) `shouldBe` 9000
      cvLastFolded (acView a2) `shouldBe` 9000

    it "gives an accepted event the path the writer must use" $ do
      alice <- kp
      owner <- kp
      bob <- kp
      origin <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts letter)
      aDel <- expectRight
        (ownerEvent (ctxOf owner) (acView aOpen) 2 noOwnAttachments (ADelegate repo (fst bob) 2))
      eventPath aOpen `shouldBe`
        threadDir (scopeOf aOpen) <> "/" <> eventFileName 1 (eventId (acEvent aOpen))
      eventPath aDel `shouldSatisfy` \p -> take 5 p == "repo/"

    it "refuses to mint once the cursor has nowhere left to go" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- Not "not ready yet" but "this repo needs an operator": another pass
      -- changes nothing, and deleting the letter would lose it.
      let repo = fst owner
          spent = (emptyView repo) { cvCursor = CanonCursor maxBound 1 }
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
          got = acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) spent 1 origin
                  noParts letter
      expectErr CursorExhausted got
      either outcome (const Decide) got `shouldBe` Abort
      -- ...and the same for the number, which only an open mints
      let spentNum = (emptyView repo) { cvCursor = CanonCursor 1 maxBound }
      expectErr CursorExhausted
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) spentNum 1 origin
           noParts letter)

    it "applies the same gates on the owner path as on the letter path" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      part <- someHash
      -- Three fixes landed on the owner paths with tests that only exercised
      -- acceptLetter, so deleting the owner-side checks left the suite green.
      -- These are the assertions that go red.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts letter)
      let tid = scopeOf aOpen
          view = acView aOpen
          req = makeLetter (fst alice) (snd alice) (AClose tid Nothing 2) noReplyChannel
      origin2 <- someHash
      -- size, via ownerEvent
      expectOwn (BodyTooLarge "note")
        (ownerEvent (ctxOf owner) view 2 noOwnAttachments
           (AClose tid (Just (Text.replicate (maxInlineBody + 1) "x")) 2))
      -- size, via honourWith: the owner signs this too
      expectErr (Composed (BodyTooLarge "note"))
        (honourWith (ctxOf owner) env view 2 origin2
           (AClose tid (Just (Text.replicate (maxInlineBody + 1) "x")) 2) req)
      -- an attribute NAME has its own bound, narrower than a value's
      expectOwn (BodyTooLarge "attr")
        (ownerEvent (ctxOf owner) view 2 noOwnAttachments
           (ASet tid (Text.replicate (maxAttrName + 1) "a") "v" 2))
      _ <- expectRight
        (ownerEvent (ctxOf owner) view 2 noOwnAttachments
           (ASet tid "labels" (Text.replicate maxAttrValue "v") 2))
      expectOwn (BodyTooLarge "value")
        (ownerEvent (ctxOf owner) view 2 noOwnAttachments
           (ASet tid "labels" (Text.replicate (maxAttrValue + 1) "v") 2))
      -- a part secret that came back out of canon unchecked, which is the
      -- path compaction takes when it re-stamps
      let stunted = fromMaybe (error "no decode")
                      (decodeStrict (LBS.toStrict (serialise ("abc" :: BS.ByteString))))
      expectOwn BadPartSecret
        (ownerEvent (ctxOf owner) view 2 (ownAttachments stunted [here part])
           (AOpen repo HubIssue "mine" [] Nothing (Just part) Nothing 2))
      either outcome (const Decide)
        (ownerEvent (ctxOf owner) view 2 (ownAttachments stunted [here part])
           (AOpen repo HubIssue "mine" [] Nothing (Just part) Nothing 2))
        `shouldBe` Abort

    it "refuses a letter whose schema it cannot read from a banned envelope" $ do
      mallory <- kp
      owner <- kp
      origin <- someHash
      -- The version is checked before the body, so there is no inner author to
      -- ban: the envelope key is the only one in hand. Without this, a version
      -- number and a few bytes of garbage are the cheapest way to grow the
      -- parked set, cheaper than an oversized body.
      let repo = fst owner
          allowed k = k /= fst mallory
          v999 = MessageData 999
                   (mdBody (makeLetter (fst mallory) (snd mallory)
                              (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                              noReplyChannel))
          ctx = TriageCtx owner allowed repo
          got = acceptLetter ctx (EnvelopeSigner (fst mallory)) (emptyView repo) 1 origin
                  noParts v999
      expectErr (BadLetter AuthorDenied) got
      either outcome (const Decide) got `shouldBe` Discard
      -- ...and an unbanned sender still gets the honest answer
      either outcome (const Decide)
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst mallory)) (emptyView repo) 1 origin
           noParts v999)
        `shouldBe` Park

    it "hands back the message hash the caller must delete by" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- What the caller does next is delete that message, by exactly this
      -- hash. A parallel list zipped against the results is one off-by-one
      -- away from deleting a letter nobody has read.
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      acc <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin
           noParts letter)
      acOrigin acc `shouldBe` Just origin
      -- an owner-native event came from no message, and says so
      own <- expectRight
        (ownerEvent (ctxOf owner) (acView acc) 2 noOwnAttachments
           (AClose (scopeOf acc) Nothing 2))
      acOrigin own `shouldBe` Nothing

    it "refuses a body it will not carry inline" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- messageData is one secretbox over the whole payload, unchunked, and it
      -- rides in a gossiped message, so a huge inline body is a cost every
      -- relaying peer pays. Above the limit it belongs in a body-part.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          big = Text.replicate (maxInlineBody + 1) "x"
          fat = makeLetter (fst alice) (snd alice)
                  (AOpen repo HubIssue "t" [] (Just big) Nothing Nothing 1) noReplyChannel
          longTitle = makeLetter (fst alice) (snd alice)
                  (AOpen repo HubIssue (Text.replicate (maxTitle + 1) "t") []
                     Nothing Nothing Nothing 1) noReplyChannel
      expectErr (BodyTooLarge "body")
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts fat)
      expectErr (BodyTooLarge "title")
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts longTitle)
      -- Park, not Discard: the limit is this hub's policy, not consensus, and
      -- another hub may carry a body this one will not.
      outcome (BodyTooLarge "body") `shouldBe` Park
      -- Coordinates are five more unbounded strings, and a revise is nothing
      -- but those.
      let longRef = Text.replicate (maxRef + 1) "r"
          fatPR = makeLetter (fst alice) (snd alice)
                    (AOpen repo HubPR "pr" [] Nothing Nothing
                       (Just coords { prOnto = longRef }) 1) noReplyChannel
      expectErr (BodyTooLarge "onto")
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts fatPR)
      aPR <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts
           (makeLetter (fst alice) (snd alice)
              (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords) 1) noReplyChannel))
      origin2 <- someHash
      expectErr (BodyTooLarge "source-tip")
        (acceptLetter (ctxOf owner) env (acView aPR) 2 origin2 noParts
           (makeLetter (fst alice) (snd alice)
              (ARevise (scopeOf aPR) coords { prSourceTip = longRef } 2) noReplyChannel))
      -- Labels are advisory, so an unbounded list is free storage.
      let manyLabels = makeLetter (fst alice) (snd alice)
                         (AOpen repo HubIssue "t" (replicate (maxLabels + 1) "l")
                            Nothing Nothing Nothing 1) noReplyChannel
          longLabel = makeLetter (fst alice) (snd alice)
                        (AOpen repo HubIssue "t" [Text.replicate (maxLabel + 1) "l"]
                           Nothing Nothing Nothing 1) noReplyChannel
      expectErr (BodyTooLarge "labels")
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts manyLabels)
      expectErr (BodyTooLarge "label")
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts longLabel)
      -- Exactly at the limit is fine.
      let ok = makeLetter (fst alice) (snd alice)
                 (AOpen repo HubIssue "t" [] (Just (Text.replicate maxInlineBody "x"))
                    Nothing Nothing 1) noReplyChannel
      _ <- expectRight (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts ok)
      pure ()

    it "does not raise a decision on a letter it already folded" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      reqOrigin <- someHash
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts letter)
      let req = makeLetter (fst alice) (snd alice)
                  (AClose (scopeOf aOpen) Nothing 2) noReplyChannel
      -- First pass: a decision for the maintainer.
      _ <- expectRequested (acceptLetter (ctxOf owner) env (acView aOpen) 2 reqOrigin noParts req)
      aClose <- expectRight
        (honourRequest (ctxOf owner) env (acView aOpen) 2 reqOrigin req)
      -- After a restart the loop re-reads the mailbox. Raising the same
      -- request again would put a decision in front of a human that
      -- 'honourRequest' refuses the moment they take it.
      let again = acceptLetter (ctxOf owner) env (acView aClose) 3 reqOrigin noParts req
      expectErr AlreadyHonoured again
      either outcome (const Retry) again `shouldBe` Discard

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
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin (attachments secret32 msgSecret [here bundle]) pr)
      let t = threadOf (foldEvents repo [acEvent acc]) (scopeOf acc)
      fmap psPartSecret (tsPR t) `shouldBe` Just (Just secret32)

    it "derives the next cursor from canon, not from a local counter" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      origin2 <- someHash
      let repo = fst owner
          env = fst alice
          open n = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue n [] Nothing Nothing Nothing 1) noReplyChannel
      a1 <- expectRight (acceptLetter (ctxOf owner) (EnvelopeSigner env) (emptyView repo) 1 origin noParts (open "a"))
      a2 <- expectRight (acceptLetter (ctxOf owner) (EnvelopeSigner env) (acView a1) 2 origin2 noParts (open "b"))
      -- A folder that restarts and rebuilds from canon mints the same next
      -- values as the one that never stopped.
      cursorFrom (foldEvents repo (map acEvent [a1,a2])) `shouldBe` cvCursor (acView a2)

    it "refuses to honour a request onto a different thread" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      originB <- someHash
      -- Editing what the request asked for is the point of honourWith;
      -- redirecting it to another thread is not honouring it, and would
      -- leave an event whose origin points at a letter about something else.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          open n = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue n [] Nothing Nothing Nothing 1) noReplyChannel
      aA <- expectRight (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts (open "a"))
      aB <- expectRight (acceptLetter (ctxOf owner) env (acView aA) 2 originB noParts (open "b"))
      let req = makeLetter (fst alice) (snd alice)
                  (AClose (scopeOf aA) Nothing 3) noReplyChannel
          elsewhere = AClose (scopeOf aB) Nothing 3
      origin2 <- someHash
      expectErr (Composed ThreadMismatch)
        (honourWith (ctxOf owner) env (acView aB) 3 origin2 elsewhere req)
      -- the same edit on the thread that was asked about is fine
      _ <- expectRight
        (honourWith (ctxOf owner) env (acView aB) 3 origin2
           (AClose (scopeOf aA) (Just "agreed") 3) req)
      pure ()

    it "refuses to stamp a folded-ts the fold would drop" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      origin2 <- someHash
      -- The third stamped field, and the only one the caller supplies
      -- directly. The fold refuses it, so minting burns the letter; worse, the
      -- accepted view carries the stamp forward, so every later letter is
      -- clamped up to it and burns too, and a loop that folds then deletes
      -- deletes each of them.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
          at t = acceptLetter (ctxOf owner) env (emptyView repo) t origin noParts letter
      expectErr StampOutOfRange (at maxBound)
      expectErr StampOutOfRange (at (maxFoldedTs + 1))
      either outcome (const Decide) (at maxBound) `shouldBe` Abort
      -- The ceiling itself mints, and the fold admits what was minted: one
      -- boundary, checked on both sides of it.
      acc <- expectRight (at maxFoldedTs)
      frDropped (foldEvents repo [acEvent acc]) `shouldBe` []
      -- The other subject of the same refusal: canon already holds a stamp
      -- above the ceiling, and the clamp cannot go under it. Nothing the
      -- letter did, and nothing another pass fixes.
      let poisoned = (emptyView repo) { cvLastFolded = maxFoldedTs + 1 }
      expectErr StampOutOfRange
        (acceptLetter (ctxOf owner) env poisoned 1 origin2 noParts letter)
      -- ...and the owner path is gated by the same function.
      expectOwn StampOutOfRange
        (ownerEvent (ctxOf owner) (emptyView repo) (maxFoldedTs + 1) noOwnAttachments
           (ADelegate repo (fst alice) 1))

    it "will not build a secret that cannot be a key" $ do
      -- Length is the only thing bytes can be checked for, and a writer that
      -- put something else in the field would produce canon whose attachments
      -- never open. Neither type can check the other thing that matters,
      -- which is which of the two secrets it is holding.
      mkPartSecret (BS.replicate (typicalKeyLength - 1) 0x41) `shouldBe` Nothing
      mkMessageSecret (BS.replicate (typicalKeyLength + 1) 0x41) `shouldBe` Nothing
      -- Same bytes, different types: this is the whole of the check the bridge
      -- has, and it is what stands between a folded attachment and the
      -- sender's private reply address in every clone.
      sameSecret secret32 (sameAs secret32) `shouldBe` True
      sameSecret secret32 msgSecret `shouldBe` False

    it "refuses an oversized request before showing it to a maintainer" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      origin2 <- someHash
      -- A closing note over the limit is refused whoever wrote it, so raising
      -- the request first would put a letter in front of a maintainer that no
      -- answer of theirs can fold.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter c = makeLetter (fst alice) (snd alice) c noReplyChannel
          big = Text.replicate (maxInlineBody + 1) "x"
      acc <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts
           (letter (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)))
      expectErr (BodyTooLarge "note")
        (acceptLetter (ctxOf owner) env (acView acc) 2 origin2 noParts
           (letter (AClose (scopeOf acc) (Just big) 2)))

    it "refuses an open whose change nothing can fetch, on both paths" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- PEP-20 gives two ways to ship a change and the coordinates say which
      -- by leaving the other absent. With neither, canon would carry a PR
      -- nobody can fetch, and the fold drops it, so neither path may mint it.
      let repo = fst owner
          nowhere = coords { prSource = Nothing, prBundle = Nothing }
          content = AOpen repo HubPR "pr" [] Nothing Nothing (Just nowhere) 1
      expectErr BadContent
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin
           noParts (makeLetter (fst alice) (snd alice) content noReplyChannel))
      expectOwn BadContent
        (ownerEvent (ctxOf owner) (emptyView repo) 1 noOwnAttachments content)
      -- and the other two guards on the same op, which the owner path checks
      -- for the same reason: the fold drops all three.
      expectOwn BadContent
        (ownerEvent (ctxOf owner) (emptyView repo) 1 noOwnAttachments
           (AOpen repo HubPR "pr" [] Nothing Nothing Nothing 1))
      expectOwn BadContent
        (ownerEvent (ctxOf owner) (emptyView repo) 1 noOwnAttachments
           (AOpen repo HubIssue "i" [] Nothing Nothing (Just coords) 1))
      expectOwn WrongRepo
        (ownerEvent (ctxOf owner) (emptyView repo) 1 noOwnAttachments
           (AOpen (fst alice) HubIssue "i" [] Nothing Nothing Nothing 1))

    it "refuses an attribute name that is not one" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- Short enough to pass the size gate, so this reaches the normalization
      -- check rather than stopping at 'BodyTooLarge': a name is matched
      -- literally against the multi-valued list, so "Labels" is a separate
      -- attribute that no canonicalization touches and nothing can delete.
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      acc <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin
           noParts letter)
      expectOwn (UnnormalizedValue "Labels")
        (ownerEvent (ctxOf owner) (acView acc) 2 noOwnAttachments
           (ASet (scopeOf acc) "Labels" "bug" 2))
      expectOwn (UnnormalizedValue "labels ")
        (ownerEvent (ctxOf owner) (acView acc) 2 noOwnAttachments
           (ASet (scopeOf acc) "labels " "bug" 2))
      -- ...and the letter carrying one never reaches a maintainer. It would
      -- arrive as an ordinary request, the obvious thing to do with a request
      -- is to hand what it carries to honourWith, and that refuses it as the
      -- caller's doing and stops the loop: one letter naming an attribute
      -- "Labels" would be all it takes.
      origin2 <- someHash
      let asked = makeLetter (fst alice) (snd alice)
                    (ASet (scopeOf acc) "Labels" "bug" 2) noReplyChannel
      expectErr (UnnormalizedValue "Labels")
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (acView acc) 2 origin2
           noParts asked)
      either outcome (const Decide)
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (acView acc) 2 origin2
           noParts asked) `shouldBe` Discard
      -- what the owner may still do is sign a normalized version of their own
      _ <- expectRight
        (honourWith (ctxOf owner) (EnvelopeSigner (fst alice)) (acView acc) 2 origin2
           (ASet (scopeOf acc) "labels" (normalizeAttr "labels" "bug") 2) asked)
      pure ()

    it "honours a request under a delegation, and not once it is withdrawn" $ do
      alice <- kp
      bob <- kp
      owner <- kp
      origin <- someHash
      req <- someHash
      req2 <- someHash
      -- A delegate is a maintainer for the honour path as much as for the
      -- letter path: the fold admits what an authorized key blessed, and
      -- refuses it the moment the delegation is withdrawn. Minting after the
      -- revoke would burn the request and produce nothing.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter c = makeLetter (fst alice) (snd alice) c noReplyChannel
      aDel <- expectRight
        (ownerEvent (ctxOf owner) (emptyView repo) 1 noOwnAttachments (ADelegate repo (fst bob) 1))
      aOpen <- expectRight
        (acceptLetter (ctxAs bob repo) env (acView aDel) 2 origin noParts
           (letter (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 2)))
      let ask = letter (AClose (scopeOf aOpen) Nothing 3)
      aClose <- expectRight
        (honourRequest (ctxAs bob repo) env (acView aOpen) 3 req ask)
      -- what the delegate blessed is admitted
      frDropped (foldEvents repo (map acEvent [aDel,aOpen,aClose])) `shouldBe` []
      aRev <- expectRight
        (ownerEvent (ctxOf owner) (acView aClose) 4 noOwnAttachments (ARevoke repo (fst bob) 4))
      expectErr UnauthorizedForRepo
        (honourRequest (ctxAs bob repo) env (acView aRev) 5 req2 ask)
      -- another folder can still take it, which is why this is a Retry
      outcome UnauthorizedForRepo `shouldBe` Retry
      _ <- expectRight (honourRequest (ctxOf owner) env (acView aRev) 5 req2 ask)
      pure ()

    it "carries a reopen request through triage and back into canon" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      req <- someHash
      req2 <- someHash
      -- The op with no test of its own until now, and the only one whose whole
      -- job is to undo another: a stranger's reopen is a request like a close,
      -- honouring it verbatim is what "agreed as sent" means, and canon has to
      -- end up open again rather than merely not-closed.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter c = makeLetter (fst alice) (snd alice) c noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts
           (letter (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)))
      let thr = scopeOf aOpen
      aClose <- expectRight
        (honourRequest (ctxOf owner) env (acView aOpen) 2 req (letter (AClose thr Nothing 2)))
      pending <- expectRequested
        (acceptLetter (ctxOf owner) env (acView aClose) 3 req2 noParts
           (letter (AReopen thr Nothing 3)))
      pdContent pending `shouldBe` AReopen thr Nothing 3
      aReopen <- expectRight
        (honourRequest (ctxOf owner) env (acView aClose) 3 req2 (letter (AReopen thr Nothing 3)))
      let fr = foldEvents repo (map acEvent [aOpen,aClose,aReopen])
      frDropped fr `shouldBe` []
      HM.lookup "status" (tsAttrs (threadOf fr thr)) `shouldBe` Just "open"

    it "refuses an owner event the fold could not place or stamp" $ do
      owner <- kp
      thr <- someHash
      -- The owner path has the same two ordering gates as the letter path, and
      -- neither has a test of its own: an op on a thread canon does not hold
      -- would be minted and then dropped as dangling, and a number the cursor
      -- cannot produce would be minted and then dropped as a bad stamp.
      let repo = fst owner
      expectOwn UnknownThread
        (ownerEvent (ctxOf owner) (emptyView repo) 1 noOwnAttachments (AClose thr Nothing 1))
      let spent = (emptyView repo) { cvCursor = CanonCursor 1 maxBound }
      expectOwn CursorExhausted
        (ownerEvent (ctxOf owner) spent 1 noOwnAttachments
           (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1))
      -- ...and a comment, which mints no number, is unaffected by that cursor
      expectOwn UnknownThread
        (ownerEvent (ctxOf owner) spent 1 noOwnAttachments
           (AComment thr Nothing (Just "c") Nothing 1))

    it "keeps out of a log what canon must never hold" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      part <- someHash
      sig <- someHash
      -- Every one of these Show instances is hand-written, and a derived one
      -- would compile: what stops that is this test. A triage loop logs a
      -- refusal, and a refusal carries the request, the evidence, and once
      -- accepted the whole event.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          note = "the contributor's own words"
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] (Just note) Nothing Nothing 1)
                     (ReplyTo (fst alice) sig)
      -- the request: the op and the thread, not the prose and not the address
      let pending = show (Pending (fst alice) (AClose origin (Just note) 1)
                            (ReplyTo (fst alice) sig))
      pending `shouldSatisfy` isInfixOf "op = close"
      pending `shouldSatisfy` isInfixOf "reply = present"
      pending `shouldSatisfy` (not . isInfixOf (Text.unpack note))
      pending `shouldSatisfy` (not . isInfixOf (show sig))
      -- the evidence: how many parts, never the key to them
      let evidence = show (attachments secret32 msgSecret [here part])
      evidence `shouldSatisfy` isInfixOf "secret = present"
      evidence `shouldSatisfy` (not . isInfixOf (replicate 8 'A'))
      evidence `shouldSatisfy` (not . isInfixOf (replicate 8 'B'))
      show secret32 `shouldBe` "PartSecret <hidden>"
      show msgSecret `shouldBe` "MessageSecret <hidden>"
      -- and the accepted event: the hash the caller deletes by, not the body
      acc <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts letter)
      show acc `shouldSatisfy` isInfixOf (show origin)
      show acc `shouldSatisfy` (not . isInfixOf (Text.unpack note))

    it "refuses a reply channel naming somebody else's mailbox" $ do
      alice <- kp
      victim <- kp
      carol <- kp
      owner <- kp
      origin <- someHash
      sig <- someHash
      -- The envelope proves who put the letter on the wire, not whose mailbox
      -- the channel names. Without the second check a hub is a reflector: N
      -- letters from one key become N maintainer-signed acks delivered to
      -- somebody who asked for none of them.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letterTo k = makeLetter (fst alice) (snd alice)
                         (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                         (ReplyTo k sig)
      acc <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts
           (letterTo (fst victim)))
      acReply acc `shouldBe` NoReply
      -- The case only the second condition catches, and the one an attacker
      -- would actually use: carol authors, alice puts it on the wire and names
      -- HER OWN mailbox. The envelope signer and the channel agree, so the
      -- rewrap check passes; what fails is that the channel is not the author's.
      origin3 <- someHash
      let rewrapped = makeLetter (fst carol) (snd carol)
                        (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)
                        (ReplyTo (fst alice) sig)
      accR <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin3 noParts rewrapped)
      acReply accR `shouldBe` NoReply
      acAuthor accR `shouldBe` fst carol
      -- ...and the sender's own mailbox is still honoured
      origin2 <- someHash
      acc2 <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin2 noParts
           (letterTo (fst alice)))
      acReply acc2 `shouldBe` ReplyTo (fst alice) sig

    it "measures a body in bytes, not in characters" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- Half as many characters as the limit, of two bytes each, plus one. A
      -- gate written in characters takes this and puts it in every clone, and
      -- the limit is about what a gossiped message costs to relay.
      let repo = fst owner
          body = Text.replicate (maxInlineBody `div` 2 + 1) "\1103"
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] (Just body) Nothing Nothing 1) noReplyChannel
      Text.length body `shouldSatisfy` (< maxInlineBody)
      expectErr (BodyTooLarge "body")
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin
           noParts letter)

    it "refuses an attachment over what this hub will carry" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      part <- someHash
      -- The inline limit pushes everything substantial into an attachment, so
      -- without a limit here there is none: the reference lives inside a signed
      -- author box and every clone that wants the thread keeps the tree.
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing (Just part) Nothing 1) noReplyChannel
          with n = acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo)
                     1 origin (attachments secret32 msgSecret [(part, PartHere n)]) letter
      expectErr (PartTooLarge part) (with (maxPartBytes + 1))
      either outcome (const Decide) (with (maxPartBytes + 1)) `shouldBe` Park
      _ <- expectRight (with maxPartBytes)
      pure ()

    it "hands back the acknowledgement for what it accepted" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      o2 <- someHash
      -- The number and the status belong to the THREAD, not to the event, so a
      -- caller holding an Accepted would otherwise re-fold canon after every
      -- letter or keep a second copy of the last-writer-wins rule.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter c = makeLetter (fst alice) (snd alice) c noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts
           (letter (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords) 1)))
      let thr = scopeOf aOpen
      ackFor aOpen `shouldBe` Just (AckRecord repo thr (Just 1) "open" Nothing Nothing)
      -- a reply carries the thread's number, which the sender cannot compute
      aCmt <- expectRight
        (acceptLetter (ctxOf owner) env (acView aOpen) 2 o2 noParts
           (letter (AComment thr Nothing (Just "hi") Nothing 2)))
      ackFor aCmt `shouldBe` Just (AckRecord repo thr (Just 1) "open" Nothing Nothing)
      -- ...and a merge reports the status the fold will show, and the commit
      aMerge <- expectRight
        (ownerEvent (ctxOf owner) (acView aCmt) 3 noOwnAttachments
           (AMerge thr "cafe" "refs/heads/master" 3))
      ackFor aMerge `shouldBe` Just (AckRecord repo thr (Just 1) "merged" (Just "cafe") Nothing)
      -- a close carries the words the maintainer wrote with it
      aClose <- expectRight
        (ownerEvent (ctxOf owner) (acView aMerge) 5 noOwnAttachments
           (AClose thr (Just "superseded") 5))
      ackFor aClose `shouldBe`
        Just (AckRecord repo thr (Just 1) "closed" Nothing (Just "superseded"))
      -- what the bridge cached is what the fold materializes
      let fr = foldEvents repo (map acEvent [aOpen,aCmt,aMerge])
      HM.lookup "status" (tsAttrs (threadOf fr thr)) `shouldBe` Just "merged"
      acView aMerge `shouldBe` viewOf fr
      -- an event that belongs to no thread acknowledges nothing
      aDel <- expectRight
        (ownerEvent (ctxOf owner) (acView aMerge) 4 noOwnAttachments (ADelegate repo (fst alice) 4))
      ackFor aDel `shouldBe` Nothing

    it "will not let one tick of the clock swallow a request" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      req1 <- someHash
      req2 <- someHash
      req3 <- someHash
      -- close, reopen, close on one tick: the third re-authors to the bytes of
      -- the first, and so to its id. A loop that takes its clock once per batch
      -- is the ordinary implementation. Answering that by discarding would
      -- delete the letter, leave the thread open, and tell triage it had closed
      -- it; it is the caller's clock that is wrong, not the letter.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter c = makeLetter (fst alice) (snd alice) c noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts
           (letter (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)))
      let thr = scopeOf aOpen
      a1 <- expectRight
        (honourRequest (ctxOf owner) env (acView aOpen) 9 req1 (letter (AClose thr Nothing 2)))
      a2 <- expectRight
        (honourRequest (ctxOf owner) env (acView a1) 9 req2 (letter (AReopen thr Nothing 3)))
      expectErr (Composed AlreadyInCanon)
        (honourRequest (ctxOf owner) env (acView a2) 9 req3 (letter (AClose thr Nothing 4)))
      either outcome (const Decide)
        (honourRequest (ctxOf owner) env (acView a2) 9 req3 (letter (AClose thr Nothing 4)))
        `shouldBe` Abort
      -- one tick later it is an ordinary close
      _ <- expectRight
        (honourRequest (ctxOf owner) env (acView a2) 10 req3 (letter (AClose thr Nothing 4)))
      pure ()

    it "refuses a hash-shaped field that is not a hash" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- A HashRef takes any length on the wire and every size gate measures
      -- text, so this went past all of them: fifty kilobytes in a reply-to sits
      -- inside a signed author box, past the box budget the reader's bounds are
      -- derived from, and makes a canon file no reader accepts again. The bytes
      -- are inside the signature and inside the event-id, so there is no
      -- repairing it afterwards.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter c = makeLetter (fst alice) (snd alice) c noReplyChannel
          huge = HashRef (fromString (replicate 50000 'z'))
      acc <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts
           (letter (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)))
      origin2 <- someHash
      expectErr (MalformedRef "reply-to")
        (acceptLetter (ctxOf owner) env (acView acc) 2 origin2 noParts
           (letter (AComment (scopeOf acc) (Just huge) (Just "hi") Nothing 2)))
      either outcome (const Decide)
        (acceptLetter (ctxOf owner) env (acView acc) 2 origin2 noParts
           (letter (AComment (scopeOf acc) (Just huge) (Just "hi") Nothing 2)))
        `shouldBe` Discard
      -- ...and every other hash an event carries, on every path
      expectErr (MalformedRef "body-part")
        (acceptLetter (ctxOf owner) env (acView acc) 2 origin2
           (attachments secret32 msgSecret [here huge])
           (letter (AComment (scopeOf acc) Nothing Nothing (Just huge) 2)))
      expectOwn (MalformedRef "redacts")
        (ownerEvent (ctxOf owner) (acView acc) 2 noOwnAttachments (ARedact repo huge 2))
      expectOwn (MalformedRef "thread")
        (ownerEvent (ctxOf owner) (acView acc) 2 noOwnAttachments
           (AClose huge Nothing 2))
      -- a well-formed one is still fine, which is what says the gate is a gate
      -- and not a wall
      _ <- expectRight
        (acceptLetter (ctxOf owner) env (acView acc) 2 origin2 noParts
           (letter (AComment (scopeOf acc) (Just (scopeOf acc)) (Just "hi") Nothing 2)))
      pure ()

    it "refuses an oversized attachment before fetching it" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      part <- someHash
      -- A tree declares its size in its root block, which is one fetch, so the
      -- limit can be applied before the payload is pulled. Otherwise a stranger
      -- naming a hundred gigabytes has the hub spend the hundred gigabytes and
      -- only then park the letter: what is bounded has to be what triage
      -- spends, not only what canon keeps.
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing (Just part) Nothing 1) noReplyChannel
          pending n = acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice))
                        (emptyView repo) 1 origin
                        (attachments secret32 msgSecret [(part, PartPending n)]) letter
      expectErr (PartTooLarge part) (pending (maxPartBytes + 1))
      -- and one that fits is still simply not here yet
      expectErr (PartNotFetched part) (pending maxPartBytes)

    it "tells the caller what to do with a refusal" $ do
      -- Treating every refusal alike either loses letters or retries spam
      -- forever. PEP-18: an early reply waits in the mailbox, it is not
      -- rejected.
      alice <- kp
      thr <- someHash
      outcome UnknownThread `shouldBe` Retry
      outcome UnknownTarget `shouldBe` Retry
      outcome UnauthorizedForRepo `shouldBe` Retry
      -- A request is not a failure: the caller is meant to rule on it.
      outcome (Requested (Pending (fst alice) (AClose thr Nothing 1) NoReply))
        `shouldBe` Decide
      -- A letter carrying an owner-only op is the letter's fault.
      outcome (NotAcceptable OwnerNative) `shouldBe` Discard
      -- The other two mean the caller used the wrong door: a valid opening
      -- letter handed to honourRequest comes back as FoldsToCanon, and a loop
      -- that folds then deletes would delete it.
      outcome (NotAcceptable FoldsToCanon) `shouldBe` Abort
      outcome (NotAcceptable RequestOnly) `shouldBe` Abort
      -- ...and what the maintainer's own tooling composed is judged apart from
      -- the letter, whatever it happens to be.
      outcome (Composed (NotAcceptable OwnerNative)) `shouldBe` Abort
      outcome (Composed (BodyTooLarge "note")) `shouldBe` Abort
      outcome (Composed ThreadMismatch) `shouldBe` Abort
      outcome (Composed UnknownThread) `shouldBe` Abort
      outcome (Composed (PartNotInMessage thr)) `shouldBe` Abort
      outcome WrongRepo `shouldBe` Discard
      outcome (BadLetter AuthorDenied) `shouldBe` Discard
      outcome AlreadyInCanon `shouldBe` Discard
      -- A letter from a newer schema is valid; this build is behind.
      -- Discarding it would strand a submission an upgrade could fold, which
      -- is exactly what happens to letters already sitting in mailboxes.
      -- Park, not Retry: a loop that re-checked these every pass would
      -- re-verify the signature of every piece of signed garbage an attacker
      -- cares to send.
      outcome (BadLetter (UnsupportedVersion 2)) `shouldBe` Park
      outcome (BadLetter (UndecodableContent (fst alice) Undecodable)) `shouldBe` Park
      -- Trailing bytes are not what an honest newer sender produces.
      outcome (BadLetter (UndecodableContent (fst alice) TrailingData)) `shouldBe` Discard
      -- Nor is a payload signed for another domain, and for a different
      -- reason than its neighbours: domains are never renumbered, so a newer
      -- sender fails on its payload, not on the tag.
      outcome (BadLetter (UndecodableContent (fst alice) WrongDomain)) `shouldBe` Discard
      outcome NotAuthorOfRecord `shouldBe` Discard
      outcome BadContent `shouldBe` Discard
      outcome ThreadMismatch `shouldBe` Discard
      outcome AlreadyHonoured `shouldBe` Discard
      -- A dead reference in a letter anyone can send, so not a stop: one
      -- stranger would otherwise wedge triage with a single letter.
      outcome (PartNotInMessage thr) `shouldBe` Discard
      -- The sender picks this one too, for the same reason: a Message is a box
      -- anyone can build, and nothing stops them encrypting their own parts
      -- with the message key.
      outcome MessageSecretOffered `shouldBe` Park
      -- The letter is intact and the caller can go and get what is missing.
      outcome MissingPartSecret `shouldBe` Retry
      outcome (PartNotFetched thr) `shouldBe` Retry
      -- Local policy, not consensus: another hub may carry a body this one
      -- will not, so deleting the letter over it would be the wrong kind of
      -- permanent.
      outcome (BodyTooLarge "body") `shouldBe` Park
      -- The maintainer has to look at these two.
      outcome NeedsReview `shouldBe` Decide
      -- Wiring bugs, an exhausted cursor and a stamp past the ceiling are
      -- neither the letter's fault nor fixable by another pass: stop, do not
      -- delete anything.
      outcome CursorExhausted `shouldBe` Abort
      outcome StampOutOfRange `shouldBe` Abort
      outcome ViewRepoMismatch `shouldBe` Abort
      outcome OwnerKeyRequired `shouldBe` Abort
      outcome NoAttachmentsSupplied `shouldBe` Abort
      outcome BadPartSecret `shouldBe` Abort
      outcome (UnnormalizedValue "labels") `shouldBe` Discard

    it "remembers a thread's number, so a reply can be acknowledged" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      origin2 <- someHash
      -- acNumber only carries on an open, but an ack for a comment reports
      -- the thread's number, which the sender cannot compute.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts letter)
      let reply = makeLetter (fst alice) (snd alice)
                    (AComment (scopeOf aOpen) Nothing (Just "hi") Nothing 2) noReplyChannel
      aCmt <- expectRight (acceptLetter (ctxOf owner) env (acView aOpen) 2 origin2 noParts reply)
      acNumber aCmt `shouldBe` Nothing
      fmap tfNumber (HM.lookup (scopeOf aOpen) (cvThreads (acView aCmt)))
        `shouldBe` Just (Just 1)

    it "refuses an owner revise whose coordinates fetch nothing" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      -- The fold lets a maintainer revise, and it checks the coordinates, so
      -- this path is reachable: without the check the owner mints an event
      -- the fold then drops, burning a seq and losing the action silently.
      let repo = fst owner
          pr = makeLetter (fst alice) (snd alice)
                 (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords) 1) noReplyChannel
      aPR <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts pr)
      let nowhere = coords { prSource = Nothing, prBundle = Nothing }
      expectOwn BadContent
        (ownerEvent (ctxOf owner) (acView aPR) 2 noOwnAttachments (ARevise (scopeOf aPR) nowhere 2))
      -- a revise that keeps a way to fetch is fine
      _ <- expectRight
        (ownerEvent (ctxOf owner) (acView aPR) 2 noOwnAttachments
           (ARevise (scopeOf aPR) coords { prSourceTip = "cccc" } 2))
      pure ()

    it "refuses a view built for another repo" $ do
      owner <- kp
      other <- kp
      alice <- kp
      origin <- someHash
      -- RepoRef and HubKey are the same type, so a view of the wrong repo is
      -- otherwise indistinguishable from the right one.
      let letter = makeLetter (fst alice) (snd alice)
                     (AOpen (fst owner) HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      expectErr ViewRepoMismatch
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice))
           (emptyView (fst other)) 1 origin noParts letter)

    it "keeps the accumulated view equal to the rebuilt one" $ do
      alice <- kp
      bob <- kp
      carol <- kp
      owner <- kp
      origin <- someHash
      origin2 <- someHash
      part <- someHash
      -- The view carried across accepts is a cache of what the fold would
      -- say, so after a mixed batch the two must agree field for field. One
      -- equality covers every field, including the ones no test reads yet.
      let repo = fst owner
          env = fst alice
          letter c = makeLetter (fst alice) (snd alice) c noReplyChannel
      a1 <- expectRight (ownerEvent (ctxOf owner) (emptyView repo) 1 noOwnAttachments
                           (ADelegate repo (fst bob) 1))
      a2 <- expectRight (acceptLetter (ctxAs bob repo) (EnvelopeSigner env) (acView a1) 2 origin noParts
                           (letter (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords) 2)))
      a3 <- expectRight (acceptLetter (ctxOf owner) (EnvelopeSigner env) (acView a2) 3 origin2 (attachments secret32 msgSecret [here part])
                           (letter (AComment (scopeOf a2) Nothing Nothing (Just part) 3)))
      a4 <- expectRight (ownerEvent (ctxOf owner) (acView a3) 4 noOwnAttachments
                           (ASet (scopeOf a2) "labels" "bug" 4))
      a5 <- expectRight (ownerEvent (ctxOf owner) (acView a4) 5 noOwnAttachments
                           (ARedact repo (eventId (acEvent a4)) 5))
      a6 <- expectRight (ownerEvent (ctxOf owner) (acView a5) 6 noOwnAttachments
                           (ADelegate repo (fst carol) 6))
      a7 <- expectRight (ownerEvent (ctxOf owner) (acView a6) 7 noOwnAttachments
                           (ARevoke repo (fst bob) 7))
      -- including the case the fold treats as a no-op
      a8 <- expectRight (ownerEvent (ctxOf owner) (acView a7) 8 noOwnAttachments
                           (ARevoke repo repo 8))
      let evs = map acEvent [a1,a2,a3,a4,a5,a6,a7,a8]
          fr = foldEvents repo evs
      frDropped fr `shouldBe` []
      acView a8 `shouldBe` viewOf fr

    it "admits everything it mints" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      o2 <- someHash
      o3 <- someHash
      -- The bridge's whole job: whatever it accepts, the fold takes. Nothing
      -- it mints should ever end up in frDropped.
      let repo = fst owner
          env = fst alice
          opening = makeLetter (fst alice) (snd alice)
                      (AOpen repo HubPR "pr" [] Nothing Nothing (Just coords) 1) noReplyChannel
      a1 <- expectRight (acceptLetter (ctxOf owner) (EnvelopeSigner env) (emptyView repo) 1 origin noParts opening)
      let cmt = makeLetter (fst alice) (snd alice)
                  (AComment (scopeOf a1) Nothing (Just "hi") Nothing 2) noReplyChannel
      a2 <- expectRight (acceptLetter (ctxOf owner) (EnvelopeSigner env) (acView a1) 2 o2 noParts cmt)
      let rev = makeLetter (fst alice) (snd alice)
                  (ARevise (scopeOf a1) coords { prSourceTip = "cccc" } 3) noReplyChannel
      a3 <- expectRight (acceptLetter (ctxOf owner) (EnvelopeSigner env) (acView a2) 3 o3 noParts rev)
      a4 <- expectRight (ownerEvent (ctxOf owner) (acView a3) 4 noOwnAttachments
                          (AMerge (scopeOf a1) "cafe" "refs/heads/master" 4))
      let fr = foldEvents repo (map acEvent [a1,a2,a3,a4])
      frDropped fr `shouldBe` []
      HM.lookup "status" (tsAttrs (threadOf fr (scopeOf a1))) `shouldBe` Just "merged"
