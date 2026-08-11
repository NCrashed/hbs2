-- | Tests for the mailbox merge decision (issue #15).
--
-- All pure. 'admitDeleted' takes the bytes of a proof block and answers, so
-- there is no storage, no peer and no clock here, which is the whole reason the
-- decision was lifted out of @mailboxMergeQ@: while it lived inside a
-- 'runMaybeT' inside a stream inside a poll, nothing could ask it anything, and
-- it went years looking like a check it was not.
module MailboxMerge (mailboxMergeTests) where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Data.Types.SignedBox
import HBS2.Net.Auth.Credentials
import HBS2.Peer.Proto.Mailbox.Merge
import HBS2.Peer.Proto.Mailbox.Ref
import HBS2.Peer.Proto.Mailbox.Types

import Codec.Serialise (serialise)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List (foldl')

import Test.Tasty
import Test.Tasty.HUnit

type S = 'HBS2Basic

-- Distinct message hashes, cheaply.
mh :: ByteString -> HashRef
mh = HashRef . hashObject

-- A delete payload that authorises removing exactly one message. The only shape
-- either path honours.
deleteOf :: HashRef -> DeleteMessagesPayload S
deleteOf h = DeleteMessagesPayload (MailboxMessagePredicate1 (Op (MessageHashEq h)))

-- The bytes of a proof block: a delete payload signed by some key.
proofBytes :: PeerCredentials S -> DeleteMessagesPayload S -> LBS.ByteString
proofBytes creds dmp =
  serialise (makeSignedBox @S (_peerSignPk creds) (_peerSignSk creds) dmp)

mailboxOf :: PeerCredentials S -> MailboxRefKey S
mailboxOf = MailboxRefKey . _peerSignPk

mailboxMergeTests :: TestTree
mailboxMergeTests = testGroup "mailbox merge (issue #15)"

  [ testCase "accepts a proof that names the message the entry removes" do
      owner <- newCredentials @S
      let victim = mh "m1"
      admitDeleted (mailboxOf owner) victim (proofBytes owner (deleteOf victim))
        @?= MergeAccept

  , testCase "refuses a proof for this mailbox that names another message" do
      -- THE REGRESSION TEST FOR #15, and it must fail against the code before
      -- the fix. The merge matched the entry's target as `_` and never read the
      -- payload's predicate at all, so it established that the proof was a
      -- delete box signed for this mailbox and not that it authorised deleting
      -- THIS message.
      --
      -- Every delete box the owner has ever issued is public: the DeleteMessages
      -- branch gossips it and stores the block, and PEP-21's fold-then-delete
      -- makes issuing them the steady state. So this is one legitimate box,
      -- stapled by a stranger to an entry naming somebody else's letter.
      owner <- newCredentials @S
      let alreadyDeleted = mh "an old letter the owner really did delete"
          victim         = mh "a letter the stranger wants gone"
      admitDeleted (mailboxOf owner) victim (proofBytes owner (deleteOf alreadyDeleted))
        @?= MergeWrongTarget

  , testCase "refuses a proof signed for another mailbox" do
      -- The check that DID exist. Kept under test so that lifting the decision
      -- out did not lose it.
      owner     <- newCredentials @S
      somebody  <- newCredentials @S
      let victim = mh "m1"
      admitDeleted (mailboxOf owner) victim (proofBytes somebody (deleteOf victim))
        @?= MergeWrongMailbox

  , testCase "refuses a proof whose signature does not verify" do
      owner <- newCredentials @S
      let victim = mh "m1"
          forged :: SignedBox (DeleteMessagesPayload S) S
          forged = case makeSignedBox @S (_peerSignPk owner) (_peerSignSk owner) (deleteOf victim) of
                     SignedBox pk bs sig -> SignedBox pk (bs <> "x") sig
      admitDeleted (mailboxOf owner) victim (serialise forged)
        @?= MergeUnsigned

  , testCase "refuses bytes that are not a delete payload at all" do
      owner <- newCredentials @S
      -- Refused rather than retried: the block is content-addressed, so these
      -- bytes will not become something else on a later poll.
      admitDeleted (mailboxOf owner) (mh "m1") (serialise (mh "not a signed box"))
        @?= MergeBadProofBlock

  , testCase "refuses a predicate this build does not implement" do
      -- Compound predicates are in the wire format and honoured nowhere. An
      -- older reader must not guess what a newer one meant by a delete, and it
      -- must not confuse this with the wrong-target case: those are an old build
      -- and an attack, and they used to be one answer when the target check was
      -- written as a guard that fell through.
      owner <- newCredentials @S
      let victim = mh "m1"
          nop     = DeleteMessagesPayload (MailboxMessagePredicate1 (Op Nop))
          bothOf  = DeleteMessagesPayload
                      (MailboxMessagePredicate1
                        (And (Op (MessageHashEq victim)) (Op (MessageHashEq (mh "m2")))))
      admitDeleted (mailboxOf owner) victim (proofBytes owner nop)
        @?= MergeUnsupportedPred
      admitDeleted (mailboxOf owner) victim (proofBytes owner bothOf)
        @?= MergeUnsupportedPred

  , testCase "keeps every entry of a tree in the merge queue" do
      -- The second defect in the same twenty lines. This runs inside the loop
      -- over a downloaded tree's entries, and a plain HM.insert replaced the
      -- whole set on every iteration, so a tree carrying N Deleted entries
      -- contributed at most one. The loss was permanent: the surrounding code
      -- counts fetch failures only, a clobbered entry is not one, so the
      -- download was dropped from the queue as complete and never retried.
      owner <- newCredentials @S
      let box  = mailboxOf owner
          hs   = [ mh (fromString (show i)) | i <- [1 :: Int .. 32] ]
          q    = foldl' (\m h -> enqueueMerge box h m) mempty hs
      fmap HS.size (HM.lookup box q) @?= Just (length hs)

  , testCase "keeps the mailboxes of a queue apart" do
      -- Accumulating must not mean merging two mailboxes into one set.
      one <- newCredentials @S
      two <- newCredentials @S
      let q = enqueueMerge (mailboxOf two) (mh "b")
                (enqueueMerge (mailboxOf one) (mh "a") mempty)
      fmap HS.toList (HM.lookup (mailboxOf one) q) @?= Just [mh "a"]
      fmap HS.toList (HM.lookup (mailboxOf two) q) @?= Just [mh "b"]

  -- AND STOPS ACCUMULATING SOMEWHERE. One of the three call sites walks a
  -- downloaded tree and enqueues every Deleted entry in it with no validation;
  -- another puts an entry back for as long as its proof block is absent. So a
  -- tree a stranger uploaded bought one permanent queue entry per entry in it,
  -- and behind each a wip entry the downloader sweeps only on completion and a
  -- brains row that survives a restart.
  , testCase "stops taking entries for one mailbox at the cap" do
      owner <- newCredentials @S
      let box = mailboxOf owner
          hs  = [ mh (fromString (show i)) | i <- [1 .. maxMergeQueue + 500] ]
          q   = foldl' (\m h -> enqueueMerge box h m) mempty hs
      fmap HS.size (HM.lookup box q) @?= Just maxMergeQueue

  -- The cap is per mailbox and not global: a busy one must not stop this peer
  -- merging anything for the others it hosts.
  , testCase "counts one mailbox's entries against that mailbox alone" do
      one <- newCredentials @S
      two <- newCredentials @S
      let full = foldl' (\m h -> enqueueMerge (mailboxOf one) h m) mempty
                   [ mh (fromString (show i)) | i <- [1 .. maxMergeQueue + 10] ]
          q    = enqueueMerge (mailboxOf two) (mh "b") full
      fmap HS.toList (HM.lookup (mailboxOf two) q) @?= Just [mh "b"]

  -- An entry already queued is not a new one, or the cap would be spent by one
  -- hash re-offered: the re-enqueue path offers the same entry on every poll
  -- for as long as its proof is missing, which is exactly the loop this bounds.
  , testCase "does not spend the cap on an entry it already holds" do
      owner <- newCredentials @S
      let box = mailboxOf owner
          q   = foldl' (\m _ -> enqueueMerge box (mh "same") m) mempty [1 .. 100 :: Int]
      fmap HS.size (HM.lookup box q) @?= Just 1
  ]
