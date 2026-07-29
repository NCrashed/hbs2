module HBS2.Hub.IngressSpec (spec) where

import HBS2.Hub.Ingress

import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject)
import HBS2.Prelude.Plated (Pretty(..))
import HBS2.Peer.Proto.Mailbox.Entry

import Data.ByteString (ByteString)
import Data.HashSet qualified as HS
import Test.Hspec

-- Distinct message hashes, cheaply.
mh :: ByteString -> HashRef
mh = HashRef . hashObject

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

    it "tells the four ways a message does not become a letter apart" $ do
      -- These were one constructor, and the four call for four different things:
      -- wait, ignore, block the sender, and report the letter's own fault. The
      -- order also matters to nobody, which is why this asserts on Eq and not on
      -- Ord: 'BadEnvelopeSig' is not "worse" than 'NotFetched' in any way a
      -- reader should sort by.
      length (HS.fromList (fmap show [NotFetched, NotForUs, BadEnvelopeSig]))
        `shouldBe` 3
      -- and each says something a human can act on, rather than one shared
      -- sentence about malformed bytes
      show (pretty NotFetched)     `shouldSatisfy` (/= show (pretty NotForUs))
      show (pretty BadEnvelopeSig) `shouldSatisfy` (/= show (pretty NotFetched))

    it "waits a bounded number of rounds, and not zero" $ do
      -- The peer's fetch is fire-and-forget: it queues the key and returns, then
      -- gossips, then downloads on a two-second poll. So a reader that looked at
      -- storage immediately was racing the download, and one that looped forever
      -- would hang on a mailbox whose sender is offline.
      maxFetchRounds `shouldSatisfy` (> 1)
      -- and no faster than the poll it is waiting on, or it asks a question that
      -- cannot have been answered yet
      fetchRound `shouldSatisfy` (>= 2)
