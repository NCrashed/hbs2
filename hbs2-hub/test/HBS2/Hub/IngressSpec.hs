module HBS2.Hub.IngressSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Ingress
import HBS2.Hub.Letter (LetterError(..))

import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject)
import HBS2.Net.Auth.Credentials
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Entry
import HBS2.Prelude.Plated (Pretty(..))

import Control.Monad.IO.Class (liftIO)
import Codec.Serialise (serialise)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.HashSet qualified as HS
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
  , igStatus  = const (pure (Just ()))
  , igFetch   = const (pure ())
  , igRoot    = const (pure Nothing)
  , igPause   = const (pure ())
  , igAllowed = const True
  , igSecret  = ReadMessageServices (const (pure Nothing))
  }

spec :: Spec
spec = do

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

    it "names every way a message does not become a letter, and says it differently" $ do
      -- These were one constructor. The five call for five different things:
      -- wait, ignore, wait for a feature, suspect corruption, block the sender.
      -- Asserting they print differently is asserting the caller can act.
      let es = [NotFetched, NotForUs, GroupKeyByRef, Undecipherable, BadEnvelopeSig]
          said = fmap (show . pretty) es
      length (HS.fromList said) `shouldBe` length es
      -- and none of them is the sentence the letter layer uses for decrypted
      -- bytes, which is what three of them used to claim
      said `shouldNotContain` [show (pretty (BadLetterHere MalformedPayload))]
