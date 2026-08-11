module HBS2.Hub.IngressSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Ingress
import HBS2.Hub.Bridge (PartEvidence(..))
import HBS2.Hub.CLI.Accept (partEvidence)
import HBS2.Hub.Letter (maxPartBytes)
import HBS2.Hub.Letter (LetterError(..),makeLetter,letterPayload,noReplyChannel
                       ,Envelope(..),hubMsgWrite)

import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject,Hash,HbSync)
import HBS2.Merkle ( MTree(..), MTreeAnn(..), AnnMetaData(..)
                   , MTreeEncryption(..), newMNode )
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.EncryptedBox
import HBS2.Data.Types.SignedBox
import HBS2.Data.Types.SmallEncryptedBlock
import HBS2.Net.Auth.GroupKeySymm (GroupKey,GroupSecret,generateGroupKey,encryptBlock)
import HBS2.Storage (AnyStorage)
import Crypto.Saltine.Core.SecretBox qualified as Encrypt
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Entry
import HBS2.Prelude.Plated (Pretty(..))

import Control.Monad.IO.Class (liftIO)
import Codec.Serialise (serialise)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.HashSet qualified as HS
import Crypto.Saltine.Core.SecretBox qualified as SK
import Crypto.Saltine.Class qualified as Saltine
import Data.IORef
import System.Timeout (timeout)
import Test.Hspec

-- Distinct message hashes, cheaply.
mh :: ByteString -> HashRef
mh = HashRef . hashObject

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

-- An ingress wired to nothing: every call answers the least interesting thing.
-- Each test overrides only the field it is about, which is the whole reason the
-- record holds functions rather than a service caller.
stub :: Ingress IO
stub = Ingress
  { igBlock   = const (pure Nothing)
    -- Both of these would BOTTOM if a test reached them, which is what a
    -- missing field in a record construction does and is not what a stub
    -- should be. The answers below are the honest ones for a node that holds
    -- nothing.
  , igSize    = const (pure Nothing)
  , igOpenPart = const (pure (Left "nothing is here"))
  , igStatus  = const (pure (Just ()))
  , igFetch   = const (pure ())
  , igRoot    = const (pure Nothing)
  , igPause   = const (pure ())
  , igAllowed = const True
  , igSecret  = ReadMessageServices (const (pure Nothing))
  }

-- A Message with an envelope this sender really signed and a payload that is
-- rubbish. What varies between the tests is the group key: by reference, or
-- inline for a secret lookup to answer.
message :: PeerCredentials 'HBS2Basic
        -> Either HashRef (GroupKey 'Symm 'HBS2Basic)
        -> Message 'HBS2Basic
message creds gk =
  MessageBasic (makeSignedBox @'HBS2Basic (_peerSignPk creds) (_peerSignSk creds) content)
  where
    content = MessageContent (MessageFlags1 (MessageTimestamp 0) Nothing Nothing Nothing)
                mempty gk mempty
                (SmallEncryptedBlock (mh "gk")
                   (B8.replicate 24 'n')
                   (EncryptedBox (B8.pack "not a secretbox")))

-- Flip a bit of the envelope signature. The classic probe, and the only way to
-- reach the one OpenError that is an accusation.
mangle :: Message 'HBS2Basic -> Message 'HBS2Basic
mangle (MessageBasic (SignedBox pk bs sig)) = MessageBasic (SignedBox pk (bs <> "x") sig)

-- Serve exactly one message, whatever hash is asked for.
served :: Message 'HBS2Basic -> Ingress IO -> Ingress IO
served msg ig = ig { igBlock = const (pure (Just (serialise msg))) }

spec :: Spec
spec = spec1 >> spec2 >> spec3 >> partSeam

spec1 :: Spec
spec1 = do

  describe "PEP-18 mailbox ingress" $ do

    it "reads what a mailbox holds as a difference of two sets" $ do
      let a = mh "a"
          b = mh "b"
          c = mh "c"
          -- An append-only log: the same message appears as Exists and later as
          -- Deleted, and the entries carry no ordering information at all (both
          -- proofs are a Maybe HashRef and nothing else). So a difference is the
          -- only defined answer; there is no "latest" to prefer.
          es = [ Exists mempty a, Exists mempty b, Deleted mempty b
               , Exists mempty c, Deleted mempty c, Exists mempty c
               ]
      liveMessages es `shouldBe` HS.fromList [a]

    it "lets deletion win however the tree is walked" $ do
      let a = mh "a"
          -- Two orders of the same log. A reader that preferred the last entry
          -- seen would answer differently for these two, and which order a
          -- merkle walk produces is not part of the format.
          up   = [ Exists mempty a, Deleted mempty a ]
          down = [ Deleted mempty a, Exists mempty a ]
      liveMessages up `shouldBe` liveMessages down
      liveMessages up `shouldBe` HS.empty

    it "keeps a message the mailbox never held out of the answer" $ do
      -- A Deleted for something no Exists ever named. Not an error and not a
      -- ghost entry: the difference just does not mention it. Worth pinning
      -- because the peer's merge admits a Deleted whose box is signed by the
      -- mailbox key without checking that the message was ever there.
      liveMessages [ Deleted mempty (mh "gone") ] `shouldBe` HS.empty

    it "refuses a mailbox the peer does not hold, rather than calling it empty" $ do
      k <- aKey
      -- The peer only asks the network about mailboxes in its own database, so
      -- an unknown key reads as empty on this run and on every future one. A
      -- reader that cannot tell those apart reports silence as an answer.
      readInbox stub { igStatus = const (pure Nothing) } k
        `shouldThrow` \(MailboxUnknown _) -> True
      -- ...and a mailbox it does hold with nothing in it is not an error
      r <- readInbox stub k
      irLetters r `shouldBe` []
      -- ...but it is not a SETTLED answer either, and that distinction is the
      -- whole of what a caller can act on here. See the test below.
      irSettled r `shouldBe` False

    it "does not call a mailbox it never saw a root for settled" $ do
      k <- aKey
      -- THE ONE THAT MATTERS. The peer writes the mailbox ref only when a merge
      -- lands, so "this peer has no ref" and "this peer has not finished
      -- downloading" are the same observation from here. The loop compared the
      -- absence against the absence it started with and called the SECOND look
      -- settled -- 2.5 s into a download whose own path pauses for ten -- so
      -- "settled" was very nearly a constant True, and an empty answer on a first
      -- run against a live mailbox was indistinguishable from a complete one.
      calls <- newIORef (0 :: Int)
      let ig = stub { igRoot = const (liftIO (modifyIORef' calls succ >> pure Nothing)) }
      (root, settled) <- awaitMailbox ig k
      root `shouldBe` Nothing
      settled `shouldBe` False
      -- and it waited the full bound before saying so, rather than one round
      readIORef calls `shouldReturn` maxFetchRounds

    it "settles on a root that stops changing, on the second look" $ do
      k <- aKey
      -- The other half: a mailbox that IS here must not pay the full wait. A
      -- root that is present and unchanged across one round is as settled as a
      -- local reader can establish, and it costs one pause.
      calls <- newIORef (0 :: Int)
      let ig = stub { igRoot = const (liftIO (modifyIORef' calls succ
                                                >> pure (Just (mh "r")))) }
      (root, settled) <- awaitMailbox ig k
      root `shouldBe` Just (mh "r")
      settled `shouldBe` True
      readIORef calls `shouldReturn` 2

    it "does not fetch a mailbox before asking whether the peer has it" $ do
      k <- aKey
      calls <- newIORef (0 :: Int)
      let ig = stub { igStatus = const (pure Nothing)
                    , igFetch = const (liftIO (modifyIORef calls succ))
                    }
      readInbox ig k `shouldThrow` \(MailboxUnknown _) -> True
      -- Fetching a mailbox the peer does not have is a no-op it will not report,
      -- so the order is what makes the refusal above possible at all.
      readIORef calls `shouldReturn` 0

    it "waits for the root to stop changing, and says when it did not" $ do
      k <- aKey
      -- The peer's fetch is fire-and-forget: it queues the key and returns, then
      -- gossips, then downloads on its own poll. A reader that looked once was
      -- racing that download.
      roots <- newIORef [Just (mh "r1"), Just (mh "r2"), Just (mh "r2")]
      let next = liftIO $ atomicModifyIORef' roots $ \case
                   (x:xs) -> (xs, x)
                   []     -> ([], Nothing)
      (root, settled) <- awaitMailbox stub { igRoot = const next } k
      root `shouldBe` Just (mh "r2")
      settled `shouldBe` True

    it "gives up after a bounded number of rounds, and reports that it did" $ do
      k <- aKey
      -- A mailbox that never settles must not hang the reader: the sender may
      -- simply be offline. The answer is what was there plus a flag saying it
      -- was still moving, which is the difference between a short list and a
      -- wrong one.
      n <- newIORef (0 :: Int)
      let next = liftIO $ do
            i <- atomicModifyIORef' n (\x -> (x + 1, x))
            pure (Just (mh (B8.pack (show i))))
      (root, settled) <- awaitMailbox stub { igRoot = const next } k
      settled `shouldBe` False
      root `shouldSatisfy` (/= Nothing)
      -- exactly the bound, so a change to the constant cannot silently become
      -- an unbounded wait
      readIORef n `shouldReturn` maxFetchRounds

    it "counts a hole in the mailbox tree instead of swallowing it" $ do
      k <- aKey
      -- A root the peer named and whose block is not here. Answering with an
      -- empty list and a zero exit would be reporting a WRONG inbox as an empty
      -- one: the missing chunk could carry either Exists entries, which makes
      -- letters vanish, or Deleted ones, which puts folded letters back in the
      -- queue.
      r <- readInbox stub { igRoot = const (pure (Just (mh "tree"))) } k
      irMissing r `shouldBe` [mh "tree"]
      irLetters r `shouldBe` []

    -- The queue is bounded and the mailbox is not, and this is where the two
    -- part company. `hub inbox accept` asked the opened prefix whether a letter
    -- was in the mailbox, so a thousand messages ground to low hashes (a few
    -- thousand signatures) sorted every honest letter past the cut and made it
    -- permanently unacceptable: no flag raises the bound and this build has no
    -- DeleteMessages to clear the junk with.
    it "holds every live message, not only the page it opened" $ do
      k <- aKey
      let n = maxInboxLetters + 5
          msgs = [ mh (B8.pack ("m" <> show i)) | i <- [1 .. n] ]
          entries = [ (m, serialise (Exists mempty m)) | m <- msgs ]
          treeB = serialise (MLeaf (fmap fst entries) :: MTree [HashRef])
          treeH = HashRef (hashObject treeB)
          store = (treeH, treeB) : entries
          ig = stub { igRoot = const (pure (Just treeH))
                    , igBlock = \h -> pure (lookup h store)
                    }
      r <- readInbox ig k
      -- Bounded where it costs: a body opened per letter.
      length (irLetters r) `shouldBe` maxInboxLetters
      irOmitted r `shouldBe` 5
      -- Whole where it is a fact about the mailbox.
      HS.size (irLive r) `shouldBe` n
      -- And specifically: the ones past the cut are in it. That is the property
      -- the accept path reads, and asking irLetters for it was the bug.
      let opened = HS.fromList (fmap lvMessage (irLetters r))
          missed = [ m | m <- msgs, not (HS.member m opened) ]
      length missed `shouldBe` 5
      all (`HS.member` irLive r) missed `shouldBe` True

    -- A REWRAP NEEDS NO KEY. 'SignedBox' keeps its payload as opaque bytes and
    -- 'MessageContent' has no sender field, so anyone who saw the ciphertext can
    -- re-sign it under a fresh key and produce a new message hash for a letter
    -- they never read. Canon is not at risk (an event-id is the hash of the
    -- inner box, so the copy is 'AlreadyInCanon'), but everything a HUMAN does
    -- was one-shot: a line, a decision and a tombstone per copy, for a letter
    -- read once.
    --
    -- What makes the grouping possible without any key of ours: a rewrap that
    -- still delivers the letter must carry the ciphertext byte for byte,
    -- because producing a different valid ciphertext of one plaintext needs the
    -- plaintext.
    it "shows one line for a letter rewrapped under many envelopes" $ do
      alice <- newCredentials @'HBS2Basic
      owner <- aKey
      let ac = AOpen owner HubIssue "t" [] Nothing Nothing Nothing 1
      (msg, gks) <- opened alice
                      (letterPayload (makeLetter (_peerSignPk alice) (_peerSignSk alice)
                                                 ac noReplyChannel))
      -- Three strangers, none of whom can read it, each re-signing the same
      -- bytes. And a fourth message that is a different letter, to say the
      -- grouping is about the ciphertext and not about "everything collapses".
      m1 <- rewrap msg <$> newCredentials @'HBS2Basic
      m2 <- rewrap msg <$> newCredentials @'HBS2Basic
      bob <- newCredentials @'HBS2Basic
      (other, _) <- opened bob
                      (letterPayload (makeLetter (_peerSignPk bob) (_peerSignSk bob)
                                                 ac noReplyChannel))

      let ig = servingAll [msg, m1, m2, other] gks
      r <- readInbox ig owner

      -- Two lines: one letter, and the other letter.
      length (irLetters r) `shouldBe` 2
      -- ...and all four messages are still live, because grouping is a decision
      -- about the QUEUE and not a claim about the mailbox.
      HS.size (irLive r) `shouldBe` 4

      let copiesShown = concatMap lvCopies (irLetters r)
      length copiesShown `shouldBe` 2
      -- the representative is not listed among its own copies
      all (`notElem` fmap lvMessage (irLetters r)) copiesShown `shouldBe` True

    -- And the verb that acts on the decision acts on the same set: a tombstone
    -- is per message hash, so without this rejecting one copy rejected one copy.
    it "answers with the copies of a letter, for the verb that drops them" $ do
      alice <- newCredentials @'HBS2Basic
      owner <- aKey
      let ac = AOpen owner HubIssue "t" [] Nothing Nothing Nothing 1
      (msg, gks) <- opened alice
                      (letterPayload (makeLetter (_peerSignPk alice) (_peerSignSk alice)
                                                 ac noReplyChannel))
      m1 <- rewrap msg <$> newCredentials @'HBS2Basic
      let ig = servingAll [msg, m1] gks
          hashOf m = HashRef (hashObject (serialise m))
      cs <- copiesOf ig owner (hashOf msg)
      cs `shouldBe` [hashOf m1]
      -- asked from the other end it answers the other one, so the relation is
      -- symmetric and neither copy is privileged
      copiesOf ig owner (hashOf m1) `shouldReturn` [hashOf msg]
      -- a message this mailbox does not hold has no copies here, rather than
      -- an answer invented from nothing
      copiesOf ig owner (mh "elsewhere") `shouldReturn` []

    it "tells a block that has not arrived from one that is not a message" $ do
      -- These were one answer. "not fetched yet" is a WAIT, and it was what a
      -- block the peer holds and cannot decode also said -- a wait for something
      -- that will never change, retried forever, with a zero exit. readEntries
      -- draws exactly this distinction for tree blocks fifteen lines above the
      -- site that got it wrong.
      absent <- openMessage stub (mh "m")
      lvLetter absent `shouldBe` Left NotFetched

      let junk ig = ig { igBlock = const (pure (Just "not a message at all")) }
      present <- openMessage (junk stub) (mh "m")
      lvLetter present `shouldBe` Left NotAMessage
      -- and nobody is named for it: the envelope is inside the bytes that did
      -- not decode.
      lvEnvelope present `shouldBe` Nothing

    it "answers rather than throwing when a message decrypts to rubbish" $ do
      -- The bug this commit is named for, and one nothing could reach while the
      -- mapping lived inside readInbox's where clause. A key IS found for this
      -- message and the ciphertext does not open, which the decrypt step raises
      -- as OperationError: a different type from readMessage's own errors, so a
      -- catch naming only those let it escape and take the whole listing with
      -- it. Anyone can send it, since recipients' encryption keys are public in
      -- their sigils.
      sender <- newCredentials @'HBS2Basic
      gk <- generateGroupKey @'HBS2Basic Nothing []
      sec <- SK.newKey
      let ig = stub { igSecret = ReadMessageServices (const (pure (Just sec))) }
      lv <- openMessage (served (message sender (Right gk)) ig) (mh "m")
      lvLetter lv `shouldBe` Left Undecipherable
      -- ...and the envelope signer is still named, because its signature is what
      -- established it and that part was fine. This is the key `hub block` takes,
      -- and on a public mailbox most of the queue arrives on paths like this one.
      lvEnvelope lv `shouldBe` Just (_peerSignPk sender)

    it "calls a forged envelope forged, and names nobody for it" $ do
      -- The one case that is an accusation rather than a wait or a shrug, so it
      -- must not share an answer with them; and the one case with no signer to
      -- name, since the signature is what would have established it.
      sender <- newCredentials @'HBS2Basic
      gk <- generateGroupKey @'HBS2Basic Nothing []
      lv <- openMessage (served (mangle (message sender (Right gk))) stub) (mh "m")
      lvLetter lv `shouldBe` Left BadEnvelopeSig
      lvEnvelope lv `shouldBe` Nothing

    it "tells a group key it cannot resolve from one not meant for it" $ do
      sender <- newCredentials @'HBS2Basic
      gk <- generateGroupKey @'HBS2Basic Nothing []
      -- By reference: this build does not resolve those (the peer's own
      -- TODO: support-groupkey-by-reference). Reported as NotForUs until now,
      -- which is a claim about something never examined, since the letter may
      -- well be addressed here.
      byRef <- openMessage (served (message sender (Left (mh "gk"))) stub) (mh "m")
      lvLetter byRef `shouldBe` Left GroupKeyByRef
      -- Inline, and no key of ours in it: that one really is not for us, and the
      -- stub's secret lookup answering Nothing is exactly that condition.
      inline <- openMessage (served (message sender (Right gk)) stub) (mh "m")
      lvLetter inline `shouldBe` Left NotForUs

    it "names every way a message does not become a letter, and says it differently" $ do
      -- These were one constructor. The five call for five different things:
      -- wait, ignore, wait for a feature, suspect corruption, block the sender.
      -- Asserting they print differently is asserting the caller can act.
      let es = [NotFetched, NotAMessage, NotForUs, GroupKeyByRef, Undecipherable, BadEnvelopeSig]
          said = fmap (show . pretty) es
      length (HS.fromList said) `shouldBe` length es
      -- and none of them is the sentence the letter layer uses for decrypted
      -- bytes, which is what three of them used to claim
      said `shouldNotContain` [show (pretty (BadLetterHere MalformedPayload))]

-- Measuring a part is how PEP-18's size gate learns what an attachment costs
-- before anything is paid for it. Its two ways of learning nothing call for
-- opposite things: a wait, and a refusal that will never change. Elsewhere in
-- this module that distinction was drawn only after it had been got wrong.
spec2 :: Spec
spec2 =
  describe "PEP-18 attachments: measuring one" $ do

    it "says a part whose root is not here has not been fetched" $ do
      r <- measurePart stub (mh "absent")
      r `shouldBe` Left PartNotFetchedYet

    it "tells that apart from a root that is here and is not a tree" $ do
      let ig = stub { igBlock = const (pure (Just "not a merkle tree at all")) }
      r <- measurePart ig (mh "present")
      case r of
        Left (PartNotATree _) -> pure ()
        other -> expectationFailure ("expected PartNotATree, got " <> show other)

    -- A part whose every node names its one child TWICE. Well formed, about a
    -- kilobyte, and 2^depth to walk: HBS2.Merkle measures the same shape at nine
    -- seconds for twenty-three blocks and does not finish at forty-one. It
    -- arrives as a hash in the messageParts of any letter, and it is measured on
    -- the accept path, where somebody is waiting.
    it "refuses a tree that costs more reads than it will spend" $ do
      reads' <- newIORef (0 :: Int)
      let (top, chain) = bomb 40
          root = MTreeAnn NoMetaData
                   (EncryptGroupNaClSymm1 (hashObject ("gk" :: LBS.ByteString))
                                          (B8.replicate 24 'n'))
                   (newMNode 1 [top, top])
          rootB = serialise (root :: MTreeAnn [HashRef])
          store = (HashRef (hashObject rootB), rootB) : chain
          ig = stub { igBlock = \h -> do
                        modifyIORef' reads' succ
                        pure (lookup h store)
                    , igSize = const (pure (Just 1))
                    }
      -- Bounded rather than merely finished: without a bound this call does not
      -- return at all, and a test that hangs is a test nobody runs twice.
      r <- timeout (30 * 1000000) (measurePart ig (HashRef (hashObject rootB)))
      r `shouldBe` Just (Left (PartTooManyBlocks maxPartBlocks))
      n <- readIORef reads'
      n `shouldSatisfy` (<= succ maxPartBlocks)

    -- The regression guard for the OTHER way to bound the walk. Entering each
    -- node once bounds it too, and is what the mailbox reader next door does,
    -- and here it would be wrong: this measures content, a file of repeated
    -- bytes really does list one block many times, and a part measured smaller
    -- than it reads is a hole in the gate this feeds.
    it "counts a block the tree lists twice as twice the bytes" $ do
      let leafB = serialise (MLeaf [mh "d"] :: MTree [HashRef])
          leafH = hashObject leafB
          gkB   = "a group key, not a tree" :: LBS.ByteString
          gkH   = hashObject gkB
          root = MTreeAnn NoMetaData
                   (EncryptGroupNaClSymm1 gkH (B8.replicate 24 'n'))
                   (newMNode 1 [leafH, leafH])
          rootB = serialise (root :: MTreeAnn [HashRef])
          store = [ (HashRef (hashObject rootB), rootB)
                  , (HashRef leafH, leafB)
                  , (HashRef gkH, gkB) ]
          ig = stub { igBlock = \h -> pure (lookup h store)
                    , igSize  = const (pure (Just 100))
                    }
      r <- measurePart ig (HashRef (hashObject rootB))
      -- 200 for the leaf the node lists twice, which is the regression guard: a
      -- walk that entered it once would say 100. Plus 100 for the key block,
      -- which is the other half of what opening this part costs.
      r `shouldBe` Right (PartFacts 300 True)

    -- The half the gate never looked at. An encrypted tree names its data AND
    -- the group key that opens it, 'igOpenPart' reads both, and the measurement
    -- covered the first: a part could be one small leaf, pass maxPartBytes as
    -- cheap, and name a key of any size or shape at all.
    it "measures the key that opens the part, not only its data" $ do
      reads' <- newIORef (0 :: Int)
      let (top, chain) = bomb 40
          leafB = serialise (MLeaf [mh "d"] :: MTree [HashRef])
          leafH = hashObject leafB
          -- A tiny part whose KEY is the duplicated-edge chain.
          root = MTreeAnn NoMetaData
                   (EncryptGroupNaClSymm1 (hashObject (serialise (newMNode 1 [top,top] :: MTree [HashRef])))
                                          (B8.replicate 24 'n'))
                   (MLeaf [mh "d"])
          keyB  = serialise (newMNode 1 [top, top] :: MTree [HashRef])
          rootB = serialise (root :: MTreeAnn [HashRef])
          store = (HashRef (hashObject rootB), rootB)
                : (HashRef (hashObject keyB), keyB)
                : (HashRef leafH, leafB)
                : chain
          ig = stub { igBlock = \h -> do
                        modifyIORef' reads' succ
                        pure (lookup h store)
                    , igSize = const (pure (Just 1))
                    }
      r <- timeout (30 * 1000000) (measurePart ig (HashRef (hashObject rootB)))
      r `shouldBe` Just (Left (PartTooManyBlocks maxPartBlocks))

    it "says a part whose key is not here is not here, rather than unreadable" $ do
      let leafB = serialise (MLeaf [mh "d"] :: MTree [HashRef])
          leafH = hashObject leafB
          root = MTreeAnn NoMetaData
                   (EncryptGroupNaClSymm1 (hashObject ("absent" :: LBS.ByteString))
                                          (B8.replicate 24 'n'))
                   (newMNode 1 [leafH])
          rootB = serialise (root :: MTreeAnn [HashRef])
          store = [ (HashRef (hashObject rootB), rootB), (HashRef leafH, leafB) ]
          ig = stub { igBlock = \h -> pure (lookup h store)
                    , igSize  = \h -> pure (if h == HashRef leafH then Just 100 else Just 100)
                    }
      -- Still downloading is a reason to come back, not a refusal that will
      -- never change, and the two travel as different answers.
      r <- measurePart ig (HashRef (hashObject rootB))
      fmap pfHere r `shouldBe` Right False

-- A chain of @depth@ node blocks, each naming the next one twice, over a leaf
-- that carries nothing. Answers the top hash and every block by hash.
bomb :: Int -> (Hash HbSync, [(HashRef, LBS.ByteString)])
bomb depth = foldl add (leafH, [(HashRef leafH, leafB)]) [1..depth]
  where
    leafB = serialise (MLeaf [] :: MTree [HashRef])
    leafH = hashObject leafB
    add (h, bs) (_ :: Int) =
      let b = serialise (newMNode 1 [h, h] :: MTree [HashRef])
      in (hashObject b, (HashRef (hashObject b), b) : bs)

-- | A message this node can really open.
--
-- Every openMessage assertion in this module used to end in a Left: the fixture
-- carried "not a secretbox" as its payload, so openWith, openLetterAs, classify
-- and the deny-list were reached by nothing. What decides whether a stranger's
-- letter becomes an event was the untested half of the module whose whole job
-- is deciding that.
--
-- Encrypted through the REAL primitive rather than by assembling a secretbox
-- here, because a fixture that encrypts the way the test thinks the code does
-- is a test of that opinion. The storage argument is never forced: with the
-- group key passed by hash ('Left'), encryptBlock's only use of it is the
-- branch that writes an inline key, which this does not take.
opened :: PeerCredentials 'HBS2Basic -> ByteString -> IO (Message 'HBS2Basic, GroupSecret)
opened creds payload = do
  gks <- Encrypt.newKey
  gk  <- generateGroupKey @'HBS2Basic (Just gks) []
  seb <- encryptBlock (error "the group key is passed by hash, so storage is unused"
                          :: AnyStorage)
                      gks
                      (Left (mh "gk") :: Either HashRef (GroupKey 'Symm 'HBS2Basic))
                      Nothing payload
  let content = MessageContent (MessageFlags1 (MessageTimestamp 0) Nothing Nothing Nothing)
                  mempty (Right gk) mempty seb
  pure ( MessageBasic (makeSignedBox @'HBS2Basic (_peerSignPk creds) (_peerSignSk creds) content)
       , gks )

-- An ingress that serves one message and holds the key to it.
holding :: Message 'HBS2Basic -> GroupSecret -> Ingress IO -> Ingress IO
holding msg gks ig = ig
  { igBlock  = const (pure (Just (serialise msg)))
  , igSecret = ReadMessageServices (const (pure (Just gks)))
  }

-- Opening a letter is the half of this module that decides whether a
-- stranger's words become an event, and it was the half no test reached.
spec3 :: Spec
spec3 =
  describe "PEP-18 mailbox ingress: a letter this node can open" $ do

    it "opens a real letter and reports what it turned out to be" $ do
      alice <- newCredentials @'HBS2Basic
      owner <- aKey
      let ac = AOpen owner HubIssue "a real issue" ["bug"] (Just "the body") Nothing Nothing 7
      (msg, gks) <- opened alice (letterPayload (makeLetter (_peerSignPk alice) (_peerSignSk alice) ac noReplyChannel))
      lv <- openMessage (holding msg gks stub) (mh "m")
      case lvLetter lv of
        Left e -> expectationFailure ("expected a letter, got " <> show e)
        Right (who, content, _) -> do
          -- The envelope key and the inner author are the same person here, and
          -- both are reported: the inner one is the authorship claim canon
          -- publishes, the envelope one is the key a maintainer blocks by.
          lvEnvelope lv `shouldBe` Just (_peerSignPk alice)
          who `shouldBe` _peerSignPk alice
          content `shouldBe` ac

    -- The deny-list is applied where the queue is BUILT, not only at accept, so
    -- that a banned author's letter is not in front of a human at all. Nothing
    -- reached it: igAllowed is a field of the record and every fixture left it
    -- const True while every letter failed earlier.
    it "drops a letter from a denied author, however the envelope was signed" $ do
      alice <- newCredentials @'HBS2Basic
      owner <- aKey
      let ac = AOpen owner HubIssue "t" [] Nothing Nothing Nothing 1
      (msg, gks) <- opened alice (letterPayload (makeLetter (_peerSignPk alice) (_peerSignSk alice) ac noReplyChannel))
      let denied = (holding msg gks stub) { igAllowed = const False }
      lv <- openMessage denied (mh "m")
      case lvLetter lv of
        Left (BadLetterHere _) -> pure ()
        other -> expectationFailure ("expected a refusal by the deny-list, got " <> show other)
      -- and with the same bytes and the list empty it opens, so the refusal
      -- above is the LIST and not the letter
      lv2 <- openMessage (holding msg gks stub) (mh "m")
      lvLetter lv2 `shouldSatisfy` either (const False) (const True)

    -- AND FOR A LETTER OF A SCHEMA THIS BUILD CANNOT READ, which is the one
    -- case where the envelope key is all there is to ask about. 'openLetterAs'
    -- has the branch written out with its reasoning and nothing could reach it:
    -- 'parsePayload' checks the version first, so such a letter never got as
    -- far as the function that would ban its sender. The tests that "covered"
    -- it built a MessageData by hand, so they were green over a path production
    -- does not take.
    --
    -- The cost of the gap: an unreadable-version letter is a WAIT, not a
    -- discard -- rightly, since a newer build folds it -- so one banned sender
    -- could park one in the queue and every pass would open it again forever.
    it "bans a letter of a version it cannot read, by the only key it has" $ do
      alice <- newCredentials @'HBS2Basic
      -- A payload whose envelope parses and whose version is not one this
      -- build speaks. The body never has to be decodable: the version is
      -- checked first, which is exactly why the ban had nowhere to run.
      let ahead = serialise (Envelope (hubMsgWrite + 7) (B8.pack "a body from the future"))
      (msg, gks) <- opened alice (LBS.toStrict ahead)

      lvOk <- openMessage (holding msg gks stub) (mh "m")
      case lvLetter lvOk of
        Left (BadLetterHere (UnsupportedVersion _)) -> pure ()
        other -> expectationFailure ("expected an unsupported version, got " <> show other)

      let denied = (holding msg gks stub) { igAllowed = const False }
      lv <- openMessage denied (mh "m")
      case lvLetter lv of
        Left (BadLetterHere AuthorDenied) -> pure ()
        other -> expectationFailure ("expected the deny-list, got " <> show other)

    -- rawMessage is what the ACCEPT path reads: the same bytes as the queue,
    -- and it hands the bridge the message secret and the part list. It had no
    -- test at all, and "a letter the queue can show and this cannot is a bug,
    -- not a state" was a claim nothing checked.
    it "reads the same letter for the accept path, with its parts and secret" $ do
      alice <- newCredentials @'HBS2Basic
      owner <- aKey
      let ac = AOpen owner HubIssue "t" [] (Just "body") Nothing Nothing 1
      (msg, gks) <- opened alice (letterPayload (makeLetter (_peerSignPk alice) (_peerSignSk alice) ac noReplyChannel))
      r <- rawMessage (holding msg gks stub) (mh "m")
      case r of
        Left e -> expectationFailure ("expected a letter, got " <> show e)
        Right raw -> do
          lrEnvelope raw `shouldBe` _peerSignPk alice
          -- No attachments on this one, and that is the answer rather than an
          -- absence: the accept path measures every part this list names.
          lrParts raw `shouldBe` []

-- | The seam between measuring a part and opening it.
--
-- 'igOpenPart' turns a fetched encrypted tree into the 'PartSecret' the owner
-- publishes into canon FOREVER, and until this existed it was reached by
-- nothing: the stub answers Left, so every PartOpened in the whole suite was a
-- fixture literal. 'measurePart' was tested at one end and 'requireParts' at
-- the other, and the thing that joins them was run by no test at all.
partSeam :: Spec
partSeam =
  describe "PEP-18 attachments: what the accept learns about each part" $ do

    let leafB = serialise (MLeaf [mh "d"] :: MTree [HashRef])
        leafH = hashObject leafB
        gkB   = "a group key, not a tree" :: LBS.ByteString
        gkH   = hashObject gkB
        root  = MTreeAnn NoMetaData
                  (EncryptGroupNaClSymm1 gkH (B8.replicate 24 'n'))
                  (newMNode 1 [leafH])
        rootB = serialise (root :: MTreeAnn [HashRef])
        part  = HashRef (hashObject rootB)
        store = [ (part, rootB), (HashRef leafH, leafB), (HashRef gkH, gkB) ]
        sized n = stub { igBlock = \h -> pure (lookup h store)
                       , igSize  = const (pure (Just n))
                       }

    -- The arm nothing ran. A real secret comes back and has to arrive in the
    -- evidence UNCHANGED: this is the value the owner publishes so that every
    -- clone can decrypt the attachment, and canon is append-only.
    it "carries the secret the part was opened with, and its measured size" $ do
      key <- Encrypt.newKey
      let ig = (sized 40) { igOpenPart = const (pure (Right ("plaintext", Saltine.encode key))) }
      r <- partEvidence ig [part]
      case r of
        [(h, (PartOpened n s, Just bs))] -> do
          h `shouldBe` part
          -- 80: the leaf and the key block, which is what measurePart counts.
          n `shouldBe` 80
          Just s `shouldBe` mkPartSecret (Saltine.encode key)
          bs `shouldBe` "plaintext"
        other -> expectationFailure ("expected one opened part, got " <> show other)

    -- A secret of the wrong length is not a key. Publishing it would put a
    -- value in canon that opens nothing, permanently.
    it "will not call a secret of the wrong length a secret" $ do
      let ig = (sized 40) { igOpenPart = const (pure (Right ("plaintext", "short"))) }
      r <- partEvidence ig [part]
      fmap (fmap fst) r `shouldBe` [(part, PartLocked 80)]

    -- Sealed to another maintainer: ordinary, and not a wait. The bridge turns
    -- this into a refusal, not a Retry.
    it "reports a part this node cannot open as locked" $ do
      r <- partEvidence (sized 40) [part]
      fmap (fmap fst) r `shouldBe` [(part, PartLocked 80)]

    -- NOT OPENED AT ALL when the size is over the bound: opening means
    -- decrypting, and decrypting what the gate is about to refuse is the spend
    -- the gate exists to prevent.
    it "does not open a part it is going to refuse for its size" $ do
      touched <- newIORef (0 :: Int)
      let ig = (sized (fromIntegral (maxPartBytes `div` 2 + 1)))
                 { igOpenPart = \_ -> modifyIORef' touched succ
                                        >> pure (Right ("plaintext", "short")) }
      r <- partEvidence ig [part]
      fmap (fmap fst) r `shouldBe` [(part, PartLocked (maxPartBytes + 2))]
      readIORef touched >>= (`shouldBe` 0)

    -- The distinction the accept turns into Retry rather than a refusal: a leaf
    -- whose size nothing has answered yet is a part still arriving.
    it "tells a part still arriving from one it will not take" $ do
      -- The size is asked of the DATA HASHES the leaf lists, not of the leaf:
      -- see the walk above. So this is one data block nothing has answered for.
      let ig = stub { igBlock = \h -> pure (lookup h store)
                    , igSize  = \h -> pure (if h == mh "d" then Nothing else Just 40)
                    }
      r <- partEvidence ig [part]
      -- 40 and not 0: what WAS measured is reported. Zero is the other
      -- pending, the one where the tree could not be measured at all, and the
      -- accept turns both into a Retry rather than a refusal.
      fmap (fmap fst) r `shouldBe` [(part, PartPending 40)]

-- | A rewrap: the same 'MessageContent', re-signed by somebody who never read
-- it. The whole attack in four lines, which is the point of writing it out.
rewrap :: Message 'HBS2Basic -> PeerCredentials 'HBS2Basic -> Message 'HBS2Basic
rewrap (MessageBasic box) creds =
  case unboxSignedBox0 @(MessageContent 'HBS2Basic) box of
    Just (_, content) ->
      MessageBasic (makeSignedBox @'HBS2Basic (_peerSignPk creds) (_peerSignSk creds) content)
    Nothing -> error "the fixture built an envelope that will not open"

-- | A mailbox holding these messages, each at its own hash, with one group key
-- that opens all of them.
servingAll :: [Message 'HBS2Basic] -> GroupSecret -> Ingress IO
servingAll msgs gks = stub
  { igRoot   = const (pure (Just treeH))
  , igBlock  = \h -> pure (lookup h store)
  , igSecret = ReadMessageServices (const (pure (Just gks)))
  }
  where
    -- The entry and the message it names live at DIFFERENT hashes, unlike the
    -- fixture above where nothing decodes the message and the two were allowed
    -- to coincide. Here they must not: a store keyed by hash would serve the
    -- entry where the message was asked for.
    blobs   = [ (HashRef (hashObject b), b) | b <- fmap serialise msgs ]
    entries = [ (HashRef (hashObject e), e)
              | (h, _) <- blobs, let e = serialise (Exists mempty h) ]
    treeB   = serialise (MLeaf (fmap fst entries) :: MTree [HashRef])
    treeH   = HashRef (hashObject treeB)
    store   = (treeH, treeB) : entries <> blobs
