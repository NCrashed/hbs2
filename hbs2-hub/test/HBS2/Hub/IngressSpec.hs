module HBS2.Hub.IngressSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Ingress
import HBS2.Hub.Letter (LetterError(..))

import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject)
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.EncryptedBox
import HBS2.Data.Types.SignedBox
import HBS2.Data.Types.SmallEncryptedBlock
import HBS2.Net.Auth.GroupKeySymm (GroupKey,generateGroupKey)
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
import Data.IORef
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
spec = spec1 >> spec2

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
