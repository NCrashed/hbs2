module HBS2.Hub.BridgeSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Bridge
import HBS2.Hub.Fold
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.GroupKeySymm (typicalKeyLength)
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Prelude.Plated (pretty)

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Codec.Serialise (serialise)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List (isInfixOf,nub)
import Data.Char qualified as Char
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

-- A part that is here, small enough to carry, and opened with the secret this
-- letter goes on to publish. There is no way to say "opened" without saying how
-- big and with what, which are the two things the gate reads and the two a
-- caller cannot have without having looked.
-- A part reference whose proof really holds, for the fixtures whose subject is
-- something else. PEP-18 binds a part to its author, so a valid proof is as
-- much a part of a well-formed letter now as a valid signature is; the tests
-- that are ABOUT the proof build a wrong one on purpose.
-- And one whose proof is merely a hash, for the fixtures that refuse before
-- the proof is ever looked at.
unproven :: HashRef -> PartRef
unproven h = PartRef h (PartProof h)

proven :: HubKey -> HashRef -> PartRef
proven = provenWith secret32

provenWith :: PartSecret -> HubKey -> HashRef -> PartRef
provenWith sec who h = PartRef h (partProofFor h sec who)

here :: HashRef -> (HashRef, PartEvidence)
here h = (h, PartOpened 1024 secret32)

-- | A message declaring exactly the parts it carries evidence about, which is
-- what an honest sender builds. The case where the two sets differ is a
-- refusal with a test of its own; every fixture here is the agreeing shape.
carrying :: MessageSecret -> [(HashRef, PartEvidence)] -> LetterParts
carrying msg evs = attachments msg (fmap fst evs) evs

-- Object names are FORTY HEX, and this fixture used to say "aaaa".
--
-- A coordinate is a ref name or an object name ('malformedName'), so a toy
-- value is one canon cannot hold -- and a fixture built out of values canon
-- cannot hold tests a letter no hub would ever fold. Spelled out here rather
-- than generated, so the shapes are visible in the fixture that uses them.
forkTip, forkBase :: Text.Text
forkTip  = Text.replicate 40 "a"
forkBase = Text.replicate 40 "b"

coords :: PRCoords
-- A fork-pointer PR: PEP-20 requires one of the two ways to fetch the
-- change, so a coords with neither is refused (reachableCoords).
coords = PRCoords (Just "hbs23://fork") "refs/heads/f" forkTip "refs/heads/master" forkBase Nothing

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
spec = promise >> do

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
      expectErr (BadContent PROpenWithoutCoords)
        (acceptLetter (ctxOf owner) (EnvelopeSigner env) (emptyView repo) 1 origin noParts prNoCoords)
      expectErr (BadContent IssueOpenWithCoords)
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
                   (ARevise (scopeOf aPR) coords { prSourceTip = Text.replicate 40 "d" } 2) noReplyChannel
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

    -- WHAT REDACTING SOMETHING HAS TO DO: hide something. 'frRedacted' is
    -- consulted in one place, which sets the flag on a thread and on its
    -- comments, so a redact naming a `set` was admitted, spent a seq, moved the
    -- thread's `updated`, was counted by `hub verify`, was retained forever by
    -- compaction -- and changed no rendering anywhere, while the verb printed an
    -- event id, a seq and a commit. This test used to assert that it worked.
    it "refuses to redact an event no reader would hide anything of" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts letter)
      aSet <- expectRight
        (ownerEvent (ctxOf owner) (acView aOpen) 2 noOwnAttachments (ASet (scopeOf aOpen) "labels" "bug" 2))
      let rebuilt = viewOf (foldEvents repo (map acEvent [aOpen, aSet]))
      expectOwn NotRedactable
        (ownerEvent (ctxOf owner) rebuilt 3 noOwnAttachments
           (ARedact repo (eventId (acEvent aSet)) 3))

    -- ...and the property the case above was written for, which is a different
    -- one and still holds: a view rebuilt from threads alone would not know a
    -- `set` exists at all, so it is taken from the fold. What proves the view
    -- knows is that a RESENT set is caught as already in canon -- which only
    -- diverges after a restart, and is why this rebuilds the view.
    it "knows about an event that left no visible trace, after a restart" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts letter)
      aSet <- expectRight
        (ownerEvent (ctxOf owner) (acView aOpen) 2 noOwnAttachments (ASet (scopeOf aOpen) "labels" "bug" 2))
      let rebuilt = viewOf (foldEvents repo (map acEvent [aOpen, aSet]))
      expectOwn AlreadyInCanon
        (ownerEvent (ctxOf owner) rebuilt 3 noOwnAttachments
           (ASet (scopeOf aOpen) "labels" "bug" 2))

    -- A comment IS hideable, so redacting one is admitted and lands with its
    -- thread rather than under an id of its own.
    it "redacts a comment, and it lands with the thread it belongs to" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      origin2 <- someHash
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts letter)
      let reply = makeLetter (fst alice) (snd alice)
                    (AComment (scopeOf aOpen) Nothing (Just "hi") Nothing 2) noReplyChannel
      aCm <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (acView aOpen) 2 origin2 noParts reply)
      let rebuilt = viewOf (foldEvents repo (map acEvent [aOpen, aCm]))
      aRedact <- expectRight
        (ownerEvent (ctxOf owner) rebuilt 3 noOwnAttachments
           (ARedact repo (eventId (acEvent aCm)) 3))
      acScope aRedact `shouldBe` ThreadScope (scopeOf aOpen)
      frDropped (foldEvents repo (map acEvent [aOpen, aCm, aRedact])) `shouldBe` []

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
                  (ARevise (scopeOf aPR) coords { prSourceTip = Text.replicate 40 "c" } 2) noReplyChannel
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

    -- A delegate is repo-scope, and it is also an event no reader shows the
    -- content of, so redacting one is refused before the scope question comes
    -- up. This case used to assert the scope of an owner-signed no-op.
    it "refuses to redact a repo-scope event, which hides nothing" $ do
      owner <- kp
      bob <- kp
      aDel <- expectRight
        (ownerEvent (ctxOf owner) (emptyView (fst owner)) 1 noOwnAttachments (ADelegate (fst owner) (fst bob) 1))
      expectOwn NotRedactable
        (ownerEvent (ctxOf owner) (acView aDel) 2 noOwnAttachments
           (ARedact (fst owner) (eventId (acEvent aDel)) 2))

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
      -- would say PROnlyOnIssue, so the bridge must not blame the author.
      let repo = fst owner
          letter = makeLetter (fst alice) (snd alice)
                     (AOpen repo HubIssue "an issue" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin noParts letter)
      let rev = makeLetter (fst alice) (snd alice)
                  (ARevise (scopeOf aOpen) coords 2) noReplyChannel
      expectErr (BadContent PROnlyOnIssue)
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
      expectOwn (BadContent PROnlyOnIssue)
        (ownerEvent (ctxOf owner) (acView aOpen) 2 noOwnAttachments
           (AMerge (scopeOf aOpen) (Text.replicate 40 "e") "refs/heads/master" 2))

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
                       (AOpen repo HubIssue "t" [] Nothing (Just (proven (fst alice) part)) Nothing 1) noReplyChannel
      a1 <- expectRight (acceptLetter (ctxOf owner) (EnvelopeSigner env) (emptyView repo) 1 origin (carrying msgSecret []) plain)
      a2 <- expectRight (acceptLetter (ctxOf owner) (EnvelopeSigner env) (emptyView repo) 1 origin (carrying msgSecret [here part]) withPart)
      -- No attachment, no secret in canon: publishing one would be noise.
      tsPartSecret (threadOf (foldEvents repo [acEvent a1]) (scopeOf a1)) `shouldBe` Nothing
      tsPartSecret (threadOf (foldEvents repo [acEvent a2]) (scopeOf a2)) `shouldBe` Just secret32

    -- THE THEFT THIS GATE EXISTS FOR. MessageContent is signed and not
    -- encrypted, so every part hash in a mailbox is public; the maintainer's
    -- node opens whatever a letter names, because the tree is wrapped for the
    -- recipient key it holds; and folding publishes the secret. So a letter
    -- naming somebody else's part hash published a stranger's attachment, and
    -- every other attachment on that letter with it, in public append-only
    -- canon. Nothing in the evidence can tell the two letters apart: the part
    -- opens either way. What tells them apart is the proof.
    it "refuses a letter claiming a part its author cannot prove is theirs" $ do
      alice <- kp
      mallory <- kp
      owner <- kp
      origin <- someHash
      part <- someHash
      let repo = fst owner
          -- Alice's letter, and Mallory's, over the same part hash. Mallory's
          -- carries the proof he could lift out of hers, which is the best he
          -- can do without the secret.
          hers = AOpen repo HubIssue "mine" [] Nothing (Just (proven (fst alice) part)) Nothing 1
          stolen = AOpen repo HubIssue "yours now" [] Nothing
                     (Just (proven (fst alice) part)) Nothing 1
          accept who c = acceptLetter (ctxOf owner) (EnvelopeSigner (fst who))
                           (emptyView repo) 1 origin
                           (carrying msgSecret [here part])
                           (makeLetter (fst who) (snd who) c noReplyChannel)
      _ <- expectRight (accept alice hers)
      expectErr (PartUnproven part) (accept mallory stolen)
      -- And re-proving it under his own key is what he cannot do: the proof is
      -- over the secret, and any value he can compute without it is this one.
      expectErr (PartUnproven part)
        (accept mallory (AOpen repo HubIssue "yours now" [] Nothing
                           (Just (unproven part)) Nothing 1))
      -- A letter of his own, over a part he really did create, is ordinary.
      _ <- expectRight
        (accept mallory (AOpen repo HubIssue "mine too" [] Nothing
                           (Just (proven (fst mallory) part)) Nothing 1))
      -- Discard, not Park: no later state of this node changes what he signed.
      either outcome (const Decide) (accept mallory stolen) `shouldBe` Discard

    it "refuses to publish an encrypted part with no key to it" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      part <- someHash
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          withPart = makeLetter (fst alice) (snd alice)
                       (AOpen repo HubIssue "t" [] Nothing (Just (proven (fst alice) part)) Nothing 1) noReplyChannel
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
      expectErr (PartNotInMessage part) (accept (carrying msgSecret []))
      either outcome (const Decide) (accept (carrying msgSecret []))
        `shouldBe` Discard
      -- The caller reaching for the empty value IS still a stop, on the one
      -- path where it can happen: an owner-native event has no message behind
      -- it, so 'noOwnAttachments' is the only value that says nothing at all.
      expectOwn NoAttachmentsSupplied
        (ownerEvent (ctxOf owner) (emptyView repo) 1 noOwnAttachments
           (AOpen repo HubIssue "t" [] Nothing (Just (proven (fst owner) part)) Nothing 1))
      outcome (Composed NoAttachmentsSupplied) `shouldBe` Abort
      -- ...but once it HAS supplied evidence, a part that evidence does not
      -- list is the letter's doing, and anyone can send such a letter. Abort
      -- here would let one stranger wedge triage for good: the letter stays in
      -- the mailbox and every later pass hits it again.
      other <- someHash
      expectErr (PartNotInMessage part) (accept (carrying msgSecret [here other]))
      either outcome (const Decide) (accept (carrying msgSecret [here other]))
        `shouldBe` Discard
      -- Carried but not fetched yet is a wait.
      let notYet = carrying msgSecret [(part, PartPending 1024)]
      expectErr (PartNotFetched part) (accept notYet)
      either outcome (const Decide) (accept notYet) `shouldBe` Retry
      -- Fetched, but with no key to it.
      let locked = carrying msgSecret [(part, PartLocked 1024)]
      expectErr (MissingPartSecret part) (accept locked)
      either outcome (const Decide) (accept locked) `shouldBe` Retry
      -- ...and the one the type exists for: the message's own secret offered
      -- as the parts secret would publish the letter's envelope, and with it
      -- the sender's private reply address, to every clone.
      -- Discard, not Abort: the sender picks this too, since nothing stops
      -- them encrypting their parts with the message key and a Message is a
      -- box anyone can build. Minting is still refused; handing them the loop
      -- as well would not be. Nor is it deleted: the identical shape is what a
      -- caller that wrapped one secret twice produces, and discarding would
      -- then take the only copy of the part secret with it.
      let wrongKey = carrying (sameAs secret32) [here part]
      expectErr (MessageSecretOffered part) (accept wrongKey)
      either outcome (const Decide) (accept wrongKey) `shouldBe` Park
      -- ...which is why the convenient constructor demands the message secret
      -- rather than defaulting it away. The owner path has its own, because
      -- an owner-native event arrived in no message at all.
      expectErr (MessageSecretOffered part)
        (accept (carrying (sameAs secret32) [here part]))
      _ <- expectRight (accept (carrying msgSecret [here part]))
      _ <- expectRight
        (ownerEvent (ctxOf owner) (emptyView repo) 1 (ownAttachments [here part])
           (AOpen repo HubIssue "mine" [] Nothing (Just (proven (fst owner) part)) Nothing 1))
      pure ()
      _ <- expectRight (accept (carrying msgSecret [here part]))
      -- On the owner path the mistake this type exists for cannot be made at
      -- all: there is no message, so there is no message secret to confuse the
      -- parts secret with. What is still catchable there is vouching for
      -- nothing.
      expectOwn NoAttachmentsSupplied
        (ownerEvent (ctxOf owner) (emptyView repo) 1 noOwnAttachments
           (AOpen repo HubIssue "mine" [] Nothing (Just (proven (fst owner) part)) Nothing 1))

    -- ONE MESSAGE, ONE KEY. PEP-18 argues the secret is safe to publish
    -- because it opens what the event points at; the two sets were never
    -- compared, so a letter naming one part in a message carrying sixteen had
    -- the owner sign the key to all sixteen into public canon forever. The
    -- extras need not be, and now are not, opened by anybody: this is decided
    -- from what the message DECLARES.
    it "refuses a message carrying an attachment the letter never names" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      part <- someHash
      extra <- someHash
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          withPart = makeLetter (fst alice) (snd alice)
                       (AOpen repo HubIssue "t" [] Nothing
                              (Just (proven (fst alice) part)) Nothing 1) noReplyChannel
          -- Declared: both. Evidence: only the one the letter names, which is
          -- all a caller has any reason to open.
          po = attachments msgSecret [part, extra] [here part]
      expectErr (PartNotReferenced extra)
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin po withPart)
      -- The sender built and signed the message that way and no later state of
      -- this node accounts for the extra, so it is not something to keep
      -- retrying.
      either outcome (const Decide)
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin po withPart)
        `shouldBe` Discard
      -- The agreeing shape still folds, which is what says the rule is about
      -- the disagreement and not about carrying attachments at all.
      _ <- expectRight (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin
                          (carrying msgSecret [here part]) withPart)
      pure ()

    -- The worst shape of the same thing: a letter that names NO attachment,
    -- whose message carries them anyway. Nothing in the walk looks at those,
    -- so before this the letter folded, the event carried a part-secret for
    -- attachments it did not mention, and the fold reported the leftover as
    -- 'SecretWithoutPart' -- an anomaly, after the key was public.
    it "refuses one whose letter names nothing at all" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      extra <- someHash
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          plain = makeLetter (fst alice) (snd alice)
                    (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      expectErr (PartNotReferenced extra)
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin
           (attachments msgSecret [extra] []) plain)
      -- and with no attachments declared it is the ordinary letter it looks like
      _ <- expectRight (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin
                          (attachments msgSecret [] []) plain)
      pure ()

    it "publishes the secret a part was opened with, not one handed in beside it" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      part <- someHash
      other <- someHash
      -- The gap the evidence used to leave open. A secret sat beside the parts
      -- as a field of its own, so it was a claim about nothing: well-formed,
      -- usable, not the message secret, and opening none of the parts it was
      -- published next to. Every check passed and canon got a permanent
      -- reference to bytes nobody can read. A pure function cannot verify a
      -- decryption; what it can refuse is a secret that is not attached to an
      -- act of opening a named part, and now that is the only way to supply one.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          content = AOpen repo HubIssue "att" [] Nothing (Just (provenWith otherSecret (fst alice) part)) Nothing 1
          letter = makeLetter (fst alice) (snd alice) content noReplyChannel
          accept po = acceptLetter (ctxOf owner) env (emptyView repo) 1 origin po letter
          otherSecret = fromMaybe (error "bad fixture secret")
                          (mkPartSecret (BS.replicate typicalKeyLength 0x43))
      -- what is published is the secret named in the evidence for THIS part
      ok <- expectRight (accept (carrying msgSecret [(part, PartOpened 1024 otherSecret)]))
      case unboxChecked (evCanonBox (acEvent ok)) of
        Left e        -> expectationFailure ("cannot read the canon box: " <> show e)
        Right (_, cc) -> ccPartSecret cc `shouldBe` Just otherSecret
      -- ...and a secret named for some other part is not published for this
      -- one: the evidence for the part the content references is missing, so
      -- there is nothing to publish and nothing to mint
      expectErr (PartNotInMessage part)
        (accept (carrying msgSecret [(other, PartOpened 1024 secret32)]))
      -- two parts opened with two different keys cannot both be carried, since
      -- canon has one field for it, and the refusal names the one that differed
      let two = AOpen repo HubPR "att" [] Nothing (Just (proven (fst alice) part))
                  (Just (PRCoords Nothing "refs/heads/f" forkTip "refs/heads/master" forkBase
                           (Just (provenWith otherSecret (fst alice) other)))) 1
          twoLetter = makeLetter (fst alice) (snd alice) two noReplyChannel
      expectErr (PartSecretsDiffer other)
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin
           (carrying msgSecret [ (part, PartOpened 1024 secret32)
                                  , (other, PartOpened 1024 otherSecret) ])
           twoLetter)
      -- and two parts opened with the same one are ordinary. Its own letter,
      -- because a proof is over the secret: the letter above claims one part
      -- under each secret, and claiming both under one is a different claim.
      let same = AOpen repo HubPR "att" [] Nothing (Just (proven (fst alice) part))
                   (Just (PRCoords Nothing "refs/heads/f" forkTip "refs/heads/master" forkBase
                            (Just (proven (fst alice) other)))) 1
      _ <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin
           (carrying msgSecret [ (part, PartOpened 1024 secret32)
                                  , (other, PartOpened 1024 secret32) ])
           (makeLetter (fst alice) (snd alice) same noReplyChannel))
      pure ()

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
        (honourWith (ctxOf owner) env (acView aOpen) 2 origin2 (AMerge tid (Text.replicate 40 "e") "master" 2) req)
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

    -- AND THE MIRROR, which the wrapping above used to swallow. honourRequest
    -- signs the letter verbatim, so it passes the letter's own content as the
    -- caller's too, and every check below that line then read a stranger's bytes
    -- and reported them as the maintainer's wiring bug -- whose outcome is a
    -- stop. One AClose naming a fifty-kilobyte hash, and the triage loop is told
    -- to leave the letter alone and go fix its own code.
    it "does not blame triage for what the letter composed" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      origin2 <- someHash
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          opening = makeLetter (fst alice) (snd alice)
                      (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts opening)
      let wide = HashRef (fromString (replicate 40000 (Char.chr 122)))
          bad = makeLetter (fst alice) (snd alice) (AClose wide Nothing 2) noReplyChannel
          r = honourRequest (ctxOf owner) env (acView aOpen) 2 origin2 bad
      expectErr (MalformedRef "thread") r
      -- Discard: the letter is at fault and no later state of this node changes
      -- what it says. Abort would hand one stranger the whole queue.
      either outcome (const Decide) r `shouldBe` Discard

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
          opened = ownAttachments [(part, PartOpened 1024 stunted)]
      expectOwn (BadPartSecret part)
        (ownerEvent (ctxOf owner) view 2 opened
           (AOpen repo HubIssue "mine" [] Nothing (Just (proven (fst owner) part)) Nothing 2))
      either outcome (const Decide)
        (ownerEvent (ctxOf owner) view 2 opened
           (AOpen repo HubIssue "mine" [] Nothing (Just (proven (fst owner) part)) Nothing 2))
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

    -- AND THE SHAPE, which the size gate above cannot see. A coordinate is a
    -- ref name or an object name (PEP-20); `--output=sub/` is a git option,
    -- well under every size bound, and it was admissible canon -- in every
    -- clone forever, waiting for a reader that put it on a command line.
    --
    -- HERE AND NOT ONLY IN LetterSpec. That module asks 'malformedName' what it
    -- answers; this asks whether the bridge CALLS it, which is the half that
    -- unwiring the gate would leave green.
    it "refuses a coordinate that is not a git name" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letterWith c = makeLetter (fst alice) (snd alice)
                           (AOpen repo HubPR "pr" [] Nothing Nothing (Just c) 1)
                           noReplyChannel
      expectErr (MalformedName "base")
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts
           (letterWith coords { prBase = "--output=sub/" }))
      expectErr (MalformedName "onto")
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts
           (letterWith coords { prOnto = "-x" }))
      -- Discard and not Retry: no later pass changes what the sender signed,
      -- and no hub anywhere would fold it.
      outcome (MalformedName "base") `shouldBe` Discard

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
                    (Just coords { prBundle = Just (proven (fst alice) bundle) }) 1)
                 noReplyChannel
      acc <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin (carrying msgSecret [here bundle]) pr)
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
      expectErr (BadContent CoordsUnreachable)
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo) 1 origin
           noParts (makeLetter (fst alice) (snd alice) content noReplyChannel))
      expectOwn (BadContent CoordsUnreachable)
        (ownerEvent (ctxOf owner) (emptyView repo) 1 noOwnAttachments content)
      -- and the other two guards on the same op, which the owner path checks
      -- for the same reason: the fold drops all three.
      expectOwn (BadContent PROpenWithoutCoords)
        (ownerEvent (ctxOf owner) (emptyView repo) 1 noOwnAttachments
           (AOpen repo HubPR "pr" [] Nothing Nothing Nothing 1))
      expectOwn (BadContent IssueOpenWithCoords)
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
      -- a second request, because the first one has been honoured and a
      -- request is honoured once whatever envelope carries it
      let ask2 = letter (AReopen (scopeOf aOpen) Nothing 5)
      expectErr UnauthorizedForRepo
        (honourRequest (ctxAs bob repo) env (acView aRev) 5 req2 ask2)
      -- another folder can still take it, which is why this is a Retry
      outcome UnauthorizedForRepo `shouldBe` Retry
      _ <- expectRight (honourRequest (ctxOf owner) env (acView aRev) 5 req2 ask2)
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
      let evidence = show (carrying msgSecret [here part])
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
                     (AOpen repo HubIssue "t" [] Nothing (Just (proven (fst alice) part)) Nothing 1) noReplyChannel
          with n = acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice)) (emptyView repo)
                     1 origin (carrying msgSecret [(part, PartOpened n secret32)]) letter
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
           (AMerge thr (Text.replicate 40 "e") "refs/heads/master" 3))
      ackFor aMerge `shouldBe`
        Just (AckRecord repo thr (Just 1) "merged" (Just (Text.replicate 40 "e")) Nothing)
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
      -- is the ordinary implementation, not a bug to report. Refusing meant
      -- either deleting a letter whose request was never carried out or stopping
      -- the loop over a millisecond, so the stamp moves forward instead: it is
      -- the caller's clock that is short, and one millisecond later is still the
      -- owner's own clock.
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
      a3 <- expectRight
        (honourRequest (ctxOf owner) env (acView a2) 9 req3 (letter (AClose thr Nothing 4)))
      -- the third is a distinct event, stamped one millisecond on
      acSeq a3 `shouldBe` 4
      -- and all three fold, in the order they were minted, with the thread
      -- ending closed: the whole point of not discarding the third
      let fr = foldEvents repo (map acEvent [aOpen,a1,a2,a3])
      frDropped fr `shouldBe` []
      frLastFolded fr `shouldBe` 10
      map anWhat (frAnomalies fr) `shouldBe` []
      fmap (HM.lookup "status" . tsAttrs) (HM.lookup thr (frThreads fr))
        `shouldBe` Just (Just "closed")
      pure ()

    it "will not nudge the clock past the ceiling to make room" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      req1 <- someHash
      req2 <- someHash
      -- The nudge above runs AFTER 'requireCanon' has judged the clock, so it
      -- walks past a gate it then has to keep itself. At the ceiling there is no
      -- room: a stamp one millisecond on is one the fold drops, and the bridge
      -- would have minted it. Refusing is right here and only here, because the
      -- clock genuinely has nowhere to go and an operator has to be told.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter c = makeLetter (fst alice) (snd alice) c noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) maxFoldedTs origin noParts
           (letter (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)))
      let thr = scopeOf aOpen
      a1 <- expectRight
        (honourRequest (ctxOf owner) env (acView aOpen) maxFoldedTs req1
           (letter (AClose thr Nothing 2)))
      a2 <- expectRight
        (honourRequest (ctxOf owner) env (acView a1) maxFoldedTs req2
           (letter (AReopen thr Nothing 3)))
      -- the third would re-author to the first's bytes, and there is no
      -- millisecond left to move to
      req3 <- someHash
      expectErr StampOutOfRange
        (honourRequest (ctxOf owner) env (acView a2) maxFoldedTs req3
           (letter (AClose thr Nothing 4)))
      -- an operator's to fix, so the loop stops rather than deleting the letter
      outcome StampOutOfRange `shouldBe` Abort
      -- and what did get minted is inside the ceiling, so the fold takes it all
      let fr = foldEvents repo (map acEvent [aOpen,a1,a2])
      frDropped fr `shouldBe` []
      frLastFolded fr `shouldBe` maxFoldedTs

    it "honours a request once, however many envelopes carry it" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      req1 <- someHash
      req2 <- someHash
      -- A rewrap: the identical request re-encrypted to the same mailbox is a
      -- new message with a new hash, so the origin does not catch it, and
      -- honouring re-authors under the owner's clock, so the event-id does not
      -- either. Without the box, anyone who kept the ciphertext could have the
      -- close applied again on every resend, and a triage loop would show it as
      -- a fresh request every time.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter c = makeLetter (fst alice) (snd alice) c noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts
           (letter (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)))
      let ask = letter (AClose (scopeOf aOpen) Nothing 2)
      aClose <- expectRight
        (honourRequest (ctxOf owner) env (acView aOpen) 2 req1 ask)
      -- resent under a different message hash, and a later clock, so nothing
      -- but the requester's own box ties the two together
      expectErr AlreadyHonoured
        (honourRequest (ctxOf owner) env (acView aClose) 99 req2 ask)
      -- triage refuses it too, so the maintainer is never shown it
      expectErr AlreadyHonoured
        (acceptLetter (ctxOf owner) env (acView aClose) 99 req2 noParts ask)
      -- and the refusal survives a restart, because canon carries it: a view
      -- rebuilt from the fold knows as much as the accumulated one
      let rebuilt = viewOf (foldEvents repo (map acEvent [aOpen,aClose]))
      expectErr AlreadyHonoured
        (honourRequest (ctxOf owner) env rebuilt 99 req2 ask)
      -- Discard, because the request has been carried out: deleting the resent
      -- copy is the correct thing to do with it
      outcome AlreadyHonoured `shouldBe` Discard

    -- AN ORIGIN IS A CLAIM AND NOTHING BINDS IT TO A MESSAGE THAT EXISTS. The
    -- fold records it for any admitted event and cannot do better: it has no
    -- mailbox and never sees a message. So an authorized key could mint ONE
    -- ordinary event carrying `origin = H` for any H it liked, and the letter
    -- in message H was refused as already folded from then on -- forever, since
    -- the set never shrinks and revoking the key does not free it, and silently,
    -- since DupOrigin fires only on a SECOND event with that origin.
    it "does not let one event's origin claim block a letter it did not carry" $ do
      alice <- kp
      owner <- kp
      mallory <- kp
      victim <- someHash
      other <- someHash
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter c = makeLetter (fst alice) (snd alice) c noReplyChannel
          theirs c = makeLetter (fst mallory) (snd mallory) c noReplyChannel

      -- A delegate folds an unrelated letter of their own, and stamps it with
      -- the hash of a message they want kept out.
      seeded <- expectRight
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst mallory)) (emptyView repo)
           1 victim noParts
           (theirs (AOpen repo HubIssue "mine" [] Nothing Nothing Nothing 1)))
      HS.member victim (cvOrigins (acView seeded)) `shouldBe` True

      -- The letter that message really carries still folds: canon's claim on
      -- that hash holds neither this letter's event nor an honour of it, so it
      -- did not come from this letter, whoever wrote it and whatever they meant.
      _ <- expectRight
        (acceptLetter (ctxOf owner) env (acView seeded) 2 victim noParts
           (letter (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 2)))

      -- ...and the gate still does the job it exists for. A triage loop
      -- re-reading a mailbox after a restart must not fold one letter twice,
      -- and there canon DOES hold the letter's own event.
      real <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 other noParts
           (letter (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)))
      expectErr AlreadyHonoured
        (acceptLetter (ctxOf owner) env (acView real) 2 other noParts
           (letter (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)))

    -- The predicate on its own, because the whole of the fix is which two sets
    -- it asks and a caller elsewhere asks it too.
    it "counts a folded letter and an honoured request, and nothing else" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      req <- someHash
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter c = makeLetter (fst alice) (snd alice) c noReplyChannel
          ac = AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1
      folded <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts (letter ac))
      -- the event it folded to
      originFits (acView folded) (authorBoxId (signAuthor (fst alice) (snd alice) ac))
        `shouldBe` True
      -- and a request the owner carried out, which is re-authored and so has a
      -- different event-id: only the requester's own box ties the two
      let ask = letter (AClose (scopeOf folded) Nothing 2)
      done <- expectRight
        (honourRequest (ctxOf owner) env (acView folded) 2 req ask)
      originFits (acView done) (authorBoxId (signAuthor (fst alice) (snd alice)
                                               (AClose (scopeOf folded) Nothing 2)))
        `shouldBe` True
      -- something canon has never seen fits nothing
      originFits (acView done) (authorBoxId (signAuthor (fst alice) (snd alice)
                                               (AOpen repo HubIssue "never" [] Nothing Nothing Nothing 9)))
        `shouldBe` False

    it "refuses an owner event lifted back out of canon" $ do
      alice <- kp
      owner <- kp
      origin <- someHash
      replay <- someHash
      -- Canon is public. Anyone can lift the owner's own signed close out of an
      -- event file, wrap it in a message under their own envelope key, and send
      -- it: the signature is genuine, the author IS the owner, and every check
      -- about the content passes. Triage would then show a maintainer their own
      -- past decision as a fresh request, once per resend, forever.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
          letter c = makeLetter (fst alice) (snd alice) c noReplyChannel
      aOpen <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts
           (letter (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1)))
      aClose <- expectRight
        (ownerEvent (ctxOf owner) (acView aOpen) 2 noOwnAttachments
           (AClose (scopeOf aOpen) Nothing 2))
      -- the owner's box, verbatim, in a letter a stranger's envelope carries
      let lifted = MessageData hubMsgVersion
                     (Letter (evAuthorBox (acEvent aClose)) noReplyChannel)
      expectErr AlreadyInCanon
        (acceptLetter (ctxOf owner) env (acView aClose) 3 replay noParts lifted)
      -- and after a restart, from canon rather than from the accumulated view
      let rebuilt = viewOf (foldEvents repo (map acEvent [aOpen,aClose]))
      expectErr AlreadyInCanon
        (acceptLetter (ctxOf owner) env rebuilt 3 replay noParts lifted)
      outcome AlreadyInCanon `shouldBe` Discard

    it "refuses to mint a redaction for another repository" $ do
      owner <- kp
      other <- kp
      alice <- kp
      origin <- someHash
      -- The bridge's half of the binding the fold checks: a redact names an
      -- event-id and nothing else, so it is the one op no thread binds to a
      -- repository for it.
      let repo = fst owner
          env = EnvelopeSigner (fst alice)
      acc <- expectRight
        (acceptLetter (ctxOf owner) env (emptyView repo) 1 origin noParts
           (makeLetter (fst alice) (snd alice)
              (AOpen repo HubIssue "t" [] Nothing Nothing Nothing 1) noReplyChannel))
      expectOwn WrongRepo
        (ownerEvent (ctxOf owner) (acView acc) 2 noOwnAttachments
           (ARedact (fst other) (scopeOf acc) 2))
      _ <- expectRight
        (ownerEvent (ctxOf owner) (acView acc) 2 noOwnAttachments
           (ARedact repo (scopeOf acc) 2))
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
           (carrying msgSecret [here huge])
           (letter (AComment (scopeOf acc) Nothing Nothing (Just (unproven huge)) 2)))
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
                     (AOpen repo HubIssue "t" [] Nothing (Just (proven (fst alice) part)) Nothing 1) noReplyChannel
          pending n = acceptLetter (ctxOf owner) (EnvelopeSigner (fst alice))
                        (emptyView repo) 1 origin
                        (carrying msgSecret [(part, PartPending n)]) letter
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
      -- All four payload reasons, because the reason is now a field and a
      -- Discard that held for one of them would say nothing about the others.
      -- The letter is what it is: no state of this node makes any of these
      -- foldable later.
      outcome (BadContent PROpenWithoutCoords) `shouldBe` Discard
      outcome (BadContent IssueOpenWithCoords) `shouldBe` Discard
      outcome (BadContent CoordsUnreachable)   `shouldBe` Discard
      outcome (BadContent PROnlyOnIssue)       `shouldBe` Discard
      -- The caller's content, not the letter's, so it stops the loop -- and it
      -- means that bare as well as inside Composed.
      outcome NotOnAThread `shouldBe` Abort
      outcome (Composed NotOnAThread) `shouldBe` Abort
      outcome ThreadMismatch `shouldBe` Discard
      outcome AlreadyHonoured `shouldBe` Discard
      -- A dead reference in a letter anyone can send, so not a stop: one
      -- stranger would otherwise wedge triage with a single letter.
      outcome (PartNotInMessage thr) `shouldBe` Discard
      -- The sender picks this one too, for the same reason: a Message is a box
      -- anyone can build, and nothing stops them encrypting their own parts
      -- with the message key.
      outcome (MessageSecretOffered thr) `shouldBe` Park
      -- ...and parts opened with two different keys is the letter's doing as
      -- well, but unfoldable rather than merely refused here: canon has one
      -- part-secret field, so no hub can carry both.
      outcome (PartSecretsDiffer thr) `shouldBe` Discard
      -- The letter is intact and the caller can go and get what is missing.
      outcome (MissingPartSecret thr) `shouldBe` Retry
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
      outcome (BadPartSecret thr) `shouldBe` Abort
      outcome (UnnormalizedValue "labels") `shouldBe` Discard

    it "names which disagreement, in the fold's own words" $ do
      -- Pairwise distinct, because carrying the reason is worth nothing if
      -- every reason prints the same sentence -- which is what this was: one
      -- nullary constructor and "kind and payload disagree" for all four.
      let says   = show . pretty . BadContent
          four   = [PROpenWithoutCoords, IssueOpenWithCoords, CoordsUnreachable, PROnlyOnIssue]
          said   = fmap says four
      nub said `shouldBe` said
      -- And the fold's words, not a second set of them. The bridge refuses
      -- because the fold would drop, so an operator who reads both must not be
      -- handed two vocabularies for one decision.
      said `shouldBe` fmap (show . pretty) four
      -- The sentence they used to share, kept here so it cannot come back:
      -- under it, "the pull request arrived with nothing to fetch" -- the one
      -- of the four a SENDER can fix -- was indistinguishable from three
      -- refusals they can do nothing about.
      said `shouldSatisfy` notElem "kind and payload disagree"

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
      expectOwn (BadContent CoordsUnreachable)
        (ownerEvent (ctxOf owner) (acView aPR) 2 noOwnAttachments (ARevise (scopeOf aPR) nowhere 2))
      -- a revise that keeps a way to fetch is fine
      _ <- expectRight
        (ownerEvent (ctxOf owner) (acView aPR) 2 noOwnAttachments
           (ARevise (scopeOf aPR) coords { prSourceTip = Text.replicate 40 "c" } 2))
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
      a3 <- expectRight (acceptLetter (ctxOf owner) (EnvelopeSigner env) (acView a2) 3 origin2 (carrying msgSecret [here part])
                           (letter (AComment (scopeOf a2) Nothing Nothing (Just (proven (fst alice) part)) 3)))
      a4 <- expectRight (ownerEvent (ctxOf owner) (acView a3) 4 noOwnAttachments
                           (ASet (scopeOf a2) "labels" "bug" 4))
      -- Redacting the COMMENT, not the set: a redact hides a thread's or a
      -- comment's content, and one naming an event no reader shows is refused
      -- before it is signed. The batch keeps a redact in it because the point
      -- here is that the accumulated view agrees with the fold after every
      -- shape of op, this one included.
      a5 <- expectRight (ownerEvent (ctxOf owner) (acView a4) 5 noOwnAttachments
                           (ARedact repo (eventId (acEvent a3)) 5))
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
                  (ARevise (scopeOf a1) coords { prSourceTip = Text.replicate 40 "c" } 3) noReplyChannel
      a3 <- expectRight (acceptLetter (ctxOf owner) (EnvelopeSigner env) (acView a2) 3 o3 noParts rev)
      a4 <- expectRight (ownerEvent (ctxOf owner) (acView a3) 4 noOwnAttachments
                          (AMerge (scopeOf a1) (Text.replicate 40 "e") "refs/heads/master" 4))
      let fr = foldEvents repo (map acEvent [a1,a2,a3,a4])
      frDropped fr `shouldBe` []
      HM.lookup "status" (tsAttrs (threadOf fr (scopeOf a1))) `shouldBe` Just "merged"

-- | THE PROMISE, AS SOMETHING THE COMPILER CHECKS.
--
-- The bridge owes its caller that it never mints an event the fold would drop,
-- and that holds by enumeration over 'DropReason'. The enumeration lived in a
-- header paragraph and went stale twice: once naming two constructors that had
-- been split into eight, once two short of the list (@PartNotProven@, which is
-- the gate a stranger's attachment turns on, and @NumberTooFarAhead@). It is a
-- total function now, in a module with @-Werror=incomplete-patterns@, so a
-- reason added to the fold does not build until somebody says what keeps the
-- bridge from minting it.
--
-- What a test can add is that the answers are not vacuous, and that the two
-- carrying a security argument say what the code does.
promise :: Spec
promise =
  describe "PEP-19 what keeps the bridge from minting a droppable event" $ do

    -- Named as the refusal raised instead, which is a constructor of the
    -- module and therefore greppable -- and, for the four payload reasons, is
    -- the refusal that carries the DropReason itself.
    it "answers with the refusal the gate raises" $ do
      keptOut PartNotProven `shouldBe` ByGate "PartUnproven"
      keptOut UnauthorizedCanon `shouldBe` ByGate "UnauthorizedForRepo"
      keptOut CoordsUnreachable `shouldBe` ByGate "BadContent"
      keptOut DupId `shouldBe` ByGate "AlreadyInCanon"

    -- And where there is no gate, because no gate is needed: the author box is
    -- carried out of the letter verbatim, and a number is minted only for an
    -- open and only one above the highest canon holds.
    it "answers construction where nothing built here can have the shape" $ do
      let byConstruction = \case { ByConstruction _ -> True ; ByGate _ -> False }
      keptOut BadAuthorSig `shouldSatisfy` byConstruction
      keptOut IdMismatch `shouldSatisfy` byConstruction
      keptOut NumberOnNonOpen `shouldSatisfy` byConstruction
      keptOut NumberTooFarAhead `shouldSatisfy` byConstruction

    -- The gate the first assertion names, doing it: the test above pins the
    -- sentence, this pins that the sentence is about something real. Both, or
    -- the function is prose with a type.
    it "raises that refusal when the proof is missing" $ do
      alice <- kp
      mallory <- kp
      owner <- kp
      origin <- someHash
      part <- someHash
      let repo = fst owner
          stolen = AOpen repo HubIssue "yours now" [] Nothing
                     (Just (proven (fst alice) part)) Nothing 1
      expectErr (PartUnproven part)
        (acceptLetter (ctxOf owner) (EnvelopeSigner (fst mallory))
           (emptyView repo) 1 origin (carrying msgSecret [here part])
           (makeLetter (fst mallory) (snd mallory) stolen noReplyChannel))
