{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | How a mailbox names the entry for a message, and the marker that says the
-- entry is already in.
--
-- Both are STORAGE KEYS, so they are a format and not an implementation detail:
-- change either derivation by a byte and every marker already written stops
-- being found. Nothing would fail. The merge path would simply re-merge entries
-- it has already merged, and the cheap early-out in @mailboxInQ@ -- which is
-- what makes accepting a message twice affordable, and therefore what makes a
-- policy refusal recoverable -- would never fire again. Slower, and silent.
--
-- So the two derivations are named functions with one definition each, used by
-- the accept path and by the merge path alike, and the golden hashes below are
-- what stops them drifting.
module MailboxEntry (mailboxEntryTests) where

import HBS2.Hash
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Peer.Proto.Mailbox.Entry
import HBS2.Prelude.Plated (pretty)

import Data.ByteString (ByteString)

import Test.Tasty
import Test.Tasty.HUnit

mh :: ByteString -> HashRef
mh = HashRef . hashObject

mailboxEntryTests :: TestTree
mailboxEntryTests = testGroup "mailbox entry derivations"
  [ testCase "an exists-entry is a function of the message and nothing else" $ do
      -- No clock, no policy, no mailbox: which is what lets the accept path
      -- compute the entry hash WITHOUT storing anything, and so ask "is this
      -- already in?" for the price of one lookup.
      existsEntryHash (mh "a") @?= existsEntryHash (mh "a")
      assertBool "different messages, different entries"
        (existsEntryHash (mh "a") /= existsEntryHash (mh "b"))

    -- The merged marker used to be derived here too, and is now a row in the
    -- mailbox database. "MailboxMergedSpec" holds the same three facts about it
    -- (two mailboxes are two facts, two entries are two facts, asking twice
    -- answers the same), plus the one that made it move: a stranger cannot
    -- write it.

  , testCase "a deleted-entry is a function of the proof and the target" $ do
      -- Both halves matter and for different reasons. The TARGET is what issue
      -- #15 was about: a proof that does not name the message the entry removes
      -- is not a proof of anything. The PROOF BOX is what makes a replayed
      -- delete cheap to recognise, since the accept path can derive this hash
      -- without writing a block and ask whether it is already merged.
      let p = mh "proof"
          t = mh "target"
      deletedEntryHash p t @?= deletedEntryHash p t
      assertBool "another proof, another entry"
        (deletedEntryHash (mh "other") t /= deletedEntryHash p t)
      assertBool "another target, another entry"
        (deletedEntryHash p (mh "other") /= deletedEntryHash p t)
      -- And it is not the same thing as saying the message exists.
      assertBool "a delete is not an exists"
        (deletedEntryHash p t /= existsEntryHash t)

  , testCase "the derivations are the ones already on disk" $ do
      -- GOLDEN, and the reason is that these are storage keys. A change here is
      -- not a refactor: every marker written by every previous build stops being
      -- found, the merge path silently redoes work it has already done, and the
      -- early-out that makes a re-accepted message cheap never fires again.
      -- Nothing breaks loudly, which is exactly why it is pinned.
      show (pretty (existsEntryHash (mh "a")))
        @?= "FsRJKCQewTby4JUgy57tLtJVNvTdr15frMKZMod9Hpxy"
      show (pretty (deletedEntryHash (mh "proof") (mh "target")))
        @?= "BJh8zGhawTtCqx5GCCA5AVvK119vY3PaQcJpAmZZkN7"
  ]
