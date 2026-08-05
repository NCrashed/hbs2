-- | @hub updates@: what a contributor is shown, and what they are not.
--
-- The mailbox this reads is the reader's own and it is public, so its contents
-- are a stranger's choice. The decisions under test are therefore the two that
-- keep a stranger's message from reading as a maintainer's word, and the notes
-- that keep a filtered report from reading as an empty mailbox.
module HBS2.Hub.UpdatesSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.CLI.Updates
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject)
import HBS2.Net.Auth.Credentials
import HBS2.Prelude.Plated (Doc,pretty)

import Data.Config.Suckless (Syntax,C)
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Text qualified as Text
import Test.Hspec

shown :: [Doc ()] -> String
shown = unlines . fmap show

mh :: BS.ByteString -> HashRef
mh = HashRef . hashObject

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

b58 :: HubKey -> String
b58 = show . pretty . AsBase58

argv :: [String] -> [Syntax C]
argv = fmap argvAtom

ack :: HubKey -> AckRecord
ack repo = AckRecord repo (mh "thread") (Just 7) "open" Nothing Nothing

spec :: Spec
spec = do

  describe "PEP-18 hub updates: the report" $ do

    it "prints the thread, the number and the status" $ do
      repo <- aKey
      let out = shown (updatesDoc False [Acked (mh "m") (ack repo)])
      out `shouldSatisfy` isInfixOf "#7"
      out `shouldSatisfy` isInfixOf "status open"
      out `shouldSatisfy` isInfixOf (show (pretty (mh "thread")))

    -- A status and a note are a MAINTAINER's words, arriving over the network,
    -- printed at a terminal. The same rule the queue and `inbox show` follow.
    it "escapes a status and a note that hold terminal escapes" $ do
      repo <- aKey
      let nasty = "x\ESC[2Kfake"
          a = (ack repo) { akStatus = nasty, akNote = Just nasty }
          out = shown (updatesDoc False [Acked (mh "m") a])
      out `shouldSatisfy` not . isInfixOf "\ESC"

    it "shows a note when there is one, and nothing when there is not" $ do
      repo <- aKey
      let with n = shown (updatesDoc False [Acked (mh "m") ((ack repo) { akNote = n })])
      with (Just "duplicate of #3") `shouldSatisfy` isInfixOf "note: duplicate of #3"
      with Nothing `shouldSatisfy` not . isInfixOf "note:"

    -- Refused and letter messages are counted, never printed: what they contain
    -- is chosen by whoever put them in a public mailbox.
    it "prints nothing at all for a message it does not believe" $ do
      repo <- aKey
      shown (updatesDoc False [Refused, NotAnAckHere]) `shouldBe` ""
      shown (updatesDoc False [Refused, Acked (mh "m") (ack repo), NotAnAckHere])
        `shouldSatisfy` ((== 1) . length . lines)

  describe "PEP-18 hub updates: what it says about itself" $ do

    -- The case that reads as silence and is not: every ack was refused, so the
    -- mailbox has things in it and the report has nothing.
    it "says an empty report over a non-empty mailbox is not an empty mailbox" $ do
      let notes = shown (updatesNotes False False [Refused, Refused])
      notes `shouldSatisfy` isInfixOf "not an empty mailbox"

    it "says why everything is refused when the clone has no canon" $ do
      let notes = shown (updatesNotes False True [Refused])
      notes `shouldSatisfy` isInfixOf "no canon"
      notes `shouldSatisfy` isInfixOf "refs/hbs2/meta"

    it "counts what it did not show beside what it did" $ do
      repo <- aKey
      let notes = shown (updatesNotes False False [Acked (mh "m") (ack repo), Refused])
      notes `shouldSatisfy` isInfixOf "1 message(s) were not shown"

    it "says letters are here and are somebody else's verb" $ do
      let notes = shown (updatesNotes False False [NotAnAckHere])
      notes `shouldSatisfy` isInfixOf "hub inbox"

    -- The reason an ack is worth showing at all, said whenever one is: it is a
    -- notification, and a reader who takes it for the record is reading a
    -- message that replays verbatim.
    it "says an ack is a courtesy, whenever it showed one" $ do
      repo <- aKey
      shown (updatesNotes False False [Acked (mh "m") (ack repo)])
        `shouldSatisfy` isInfixOf "courtesy"
      shown (updatesNotes False False []) `shouldSatisfy` not . isInfixOf "courtesy"
      -- And under --all, where what was shown is the set-aside ones.
      shown (updatesNotes True False [Unrelated (mh "m") (ack repo)])
        `shouldSatisfy` isInfixOf "courtesy"

    -- The set-aside pile is its own note and not folded into the refusals: a
    -- refused message is a stranger's, and this is a real ack that did not
    -- match the log, which is a thing the reader can act on.
    it "says why an ack was set aside, and how to see it anyway" $ do
      repo <- aKey
      let notes = shown (updatesNotes False False [Unrelated (mh "m") (ack repo)])
      notes `shouldSatisfy` isInfixOf "no record of sending"
      notes `shouldSatisfy` isInfixOf "--all"
      -- Not repeated once the reader has asked for them.
      shown (updatesNotes True False [Unrelated (mh "m") (ack repo)])
        `shouldSatisfy` not . isInfixOf "no record of sending"

  describe "PEP-18 hub updates: arguments" $ do

    it "reads both flags, in any order" $ do
      mbox <- aKey ; repo <- aKey
      updatesArgs (argv ["--mailbox", b58 mbox, "--repo", b58 repo])
        `shouldBe` Just (UpdatesArgs mbox repo False)
      updatesArgs (argv ["--repo", b58 repo, "--mailbox", b58 mbox])
        `shouldBe` Just (UpdatesArgs mbox repo False)

    -- --repo is optional on every other verb that takes one and required here:
    -- it is what an ack is checked against, and an unchecked ack is a sentence
    -- any stranger can put in your mailbox.
    it "refuses a call with no repository" $ do
      mbox <- aKey
      updatesArgs (argv ["--mailbox", b58 mbox]) `shouldBe` Nothing

    it "refuses a call with no mailbox, and an unknown flag" $ do
      repo <- aKey
      updatesArgs (argv ["--repo", b58 repo]) `shouldBe` Nothing
      updatesArgs (argv ["--mailbox", b58 repo, "--repo", b58 repo, "--every"])
        `shouldBe` Nothing

    it "reads --all as a switch that takes no value" $ do
      mbox <- aKey ; repo <- aKey
      let base = ["--mailbox", b58 mbox, "--repo", b58 repo]
      fmap uaAll (updatesArgs (argv base)) `shouldBe` Just False
      fmap uaAll (updatesArgs (argv (base <> ["--all"]))) `shouldBe` Just True
      updatesArgs (argv (base <> ["--all", "yes"])) `shouldBe` Nothing

    it "refuses two mailboxes rather than reading one of them" $ do
      mbox <- aKey ; other <- aKey ; repo <- aKey
      updatesArgs (argv [ "--mailbox", b58 mbox, "--mailbox", b58 other
                        , "--repo", b58 repo ])
        `shouldBe` Nothing

  describe "PEP-18 hub updates: an ack that is not about you" $ do

    -- Shown only when asked for, and marked when shown: a report read with
    -- --all and one read without must not be confusable.
    it "hides a set-aside ack, and marks it when --all is given" $ do
      repo <- aKey
      let us = [Unrelated (mh "m") (ack repo)]
      shown (updatesDoc False us) `shouldBe` ""
      shown (updatesDoc True us) `shouldSatisfy` isInfixOf "not a thread this node sent"

    it "counts a set-aside ack as a message it did not show" $ do
      repo <- aKey
      let notes = shown (updatesNotes False False [Unrelated (mh "m") (ack repo)])
      notes `shouldSatisfy` isInfixOf "not an empty mailbox"
