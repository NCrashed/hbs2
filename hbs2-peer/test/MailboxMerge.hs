-- | Tests for the mailbox merge decision (issue #15).
--
-- All pure. 'admitDeleted' takes the bytes of a proof block and answers, so
-- there is no storage, no peer and no clock here, which is the whole reason the
-- decision was lifted out of @mailboxMergeQ@: while it lived inside a
-- 'runMaybeT' inside a stream inside a poll, nothing could ask it anything, and
-- it went years looking like a check it was not.
module MailboxMerge (mailboxMergeTests) where

import HBS2.Prelude.Plated
import HBS2.Defaults (defMaxDatagram)
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

-- A delete payload that authorises removing exactly one message. A bare leaf,
-- with no spine around it, which is what every delete looked like before a
-- payload could name a set.
deleteOf :: HashRef -> DeleteMessagesPayload S
deleteOf h = DeleteMessagesPayload (MailboxMessagePredicate1 (Op (MessageHashEq h)))

-- A delete payload naming a set, built here rather than by 'deleteNaming' so
-- that a test can write a shape the builder would never emit: over the cap, with
-- a node the reader refuses, or leaning the other way.
deleteOfMany :: [HashRef] -> DeleteMessagesPayload S
deleteOfMany hs =
  DeleteMessagesPayload (MailboxMessagePredicate1 (foldr (Or . Op . MessageHashEq) End hs))

-- The same set the other way round. A reader must not care: the shape a payload
-- happens to have is not something its signer promised anybody.
deleteOfManyLeft :: [HashRef] -> DeleteMessagesPayload S
deleteOfManyLeft hs =
  DeleteMessagesPayload
    (MailboxMessagePredicate1 (foldl (\acc h -> Or acc (Op (MessageHashEq h))) End hs))

-- Distinct message hashes, n of them.
mhs :: Int -> [HashRef]
mhs n = [ mh (fromString (show i)) | i <- [1 .. n] ]

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

  -- A DELETE NAMES A SET, spelled with the Or the wire format
  -- already had. `hub drop` on a triage queue was one packet, one signature and
  -- (after the stamp) one grind per letter; a set makes it one per batch.
  , testCase "accepts a proof whose set contains the message the entry removes" do
      owner <- newCredentials @S
      let batch  = mhs 5
          victim = batch !! 2
      admitDeleted (mailboxOf owner) victim (proofBytes owner (deleteOfMany batch))
        @?= MergeAccept

  , testCase "refuses a proof whose set does not contain it" do
      -- The property #15 is about, restated for a set: what a box authorises is
      -- what its signer wrote down, and a set widens that by the messages named
      -- in it and by nothing else.
      owner <- newCredentials @S
      let victim = mh "a letter the stranger wants gone"
      admitDeleted (mailboxOf owner) victim (proofBytes owner (deleteOfMany (mhs 5)))
        @?= MergeWrongTarget

  , testCase "reads the same set out of either spine" do
      -- The tree shape is the builder's business. A reader that accepted only a
      -- right-leaning spine would refuse a legitimate delete from any other
      -- implementation for a reason nothing on the wire states.
      owner <- newCredentials @S
      let batch  = mhs 4
          victim = head batch
      admitDeleted (mailboxOf owner) victim (proofBytes owner (deleteOfManyLeft batch))
        @?= MergeAccept
      deleteTargets (deleteOfManyLeft batch) @?= deleteTargets (deleteOfMany batch)

  , testCase "accepts a set at exactly the cap" do
      -- The boundary, from below: the cap is what the builder is allowed to
      -- emit, so a full batch has to survive the reader that receives it.
      owner <- newCredentials @S
      let batch = mhs maxDeleteTargets
      admitDeleted (mailboxOf owner) (last batch) (proofBytes owner (deleteOfMany batch))
        @?= MergeAccept

  , testCase "refuses a set over the cap" do
      -- The merge path reads proofs out of the block store, not off the wire, so
      -- a payload is bounded by the block size and not by a datagram. Without
      -- this, one signature buys thousands of entries and merge-queue slots.
      owner <- newCredentials @S
      let batch = mhs (maxDeleteTargets + 1)
      admitDeleted (mailboxOf owner) (head batch) (proofBytes owner (deleteOfMany batch))
        @?= MergeTooManyTargets

  , testCase "counts leaves and not distinct hashes" do
      -- Deduplication is a courtesy to an honest builder, not a discount on the
      -- bound: the walk is what costs, and a payload naming one hash a thousand
      -- times has still made a reader visit a thousand leaves.
      owner <- newCredentials @S
      let victim = mh "m1"
          batch  = replicate (maxDeleteTargets + 1) victim
      admitDeleted (mailboxOf owner) victim (proofBytes owner (deleteOfMany batch))
        @?= MergeTooManyTargets

  , testCase "refuses a set with an unsupported node anywhere in it" do
      -- An old build must not act on part of a predicate it only partly
      -- understands. Both of these would be ACCEPTED by a reader that simply
      -- collected the MessageHashEq leaves it recognised and ignored the rest.
      owner <- newCredentials @S
      let victim  = mh "m1"
          leaf    = Op (MessageHashEq victim)
          withNop = DeleteMessagesPayload
                      (MailboxMessagePredicate1 (Or leaf (Op Nop)))
          withAnd = DeleteMessagesPayload
                      (MailboxMessagePredicate1
                        (Or leaf (And leaf (Op (MessageHashEq (mh "m2"))))))
      admitDeleted (mailboxOf owner) victim (proofBytes owner withNop)
        @?= MergeUnsupportedPred
      admitDeleted (mailboxOf owner) victim (proofBytes owner withAnd)
        @?= MergeUnsupportedPred

  , testCase "what the builder writes is what the reader reads" do
      -- The two halves are in one module so they cannot drift, and this is the
      -- assertion that says so. A builder in the hub package, which the reader
      -- does not depend on, could disagree about the spine, the terminator or
      -- the batch size and nothing would notice until a delete stopped working.
      let batch    = mhs (maxDeleteTargets * 2 + 7)
          payloads = deleteNaming @S batch

      length payloads @?= 3

      mapM_ (\p -> case deleteTargets p of
                Left v   -> assertFailure ("the reader refuses a payload the builder"
                                             <> " wrote: " <> show v)
                Right ts -> assertBool "a batch over the cap"
                                       (HS.size ts <= maxDeleteTargets))
            payloads

      -- and nothing is lost or invented between the batches
      mconcat [ ts | Right ts <- fmap deleteTargets payloads ] @?= HS.fromList batch

  , testCase "the builder names a message once however often it is asked" do
      -- The reject path drops a letter and every rewrapped copy of it, and the
      -- copies are found by a walk that can offer the same hash twice.
      let victim = mh "m1"
      case deleteNaming @S [victim, victim, victim] of
        [p] -> deleteTargets p @?= Right (HS.singleton victim)
        ps  -> assertFailure ("expected one payload, got " <> show (length ps))

  , testCase "nothing to drop is nothing to sign" do
      -- Not one payload authorising nothing, which the reader would refuse and
      -- which would cost a signature and a packet to be refused.
      assertBool "an empty drop built a payload" (null (deleteNaming @S []))

  , testCase "a full batch still fits a datagram" do
      -- WHAT PINS 'maxDeleteTargets', so the number is not a guess anybody has
      -- to re-derive. A delete is gossiped, gossip goes over UDP, and a packet
      -- larger than 'defMaxDatagram' is one nobody receives -- silently, since
      -- an oversized datagram is not an error anything here reports.
      --
      -- THE WHOLE WIRE VALUE and not just the signed box, which is what this
      -- measured first and which stopped being the packet the moment a delete
      -- could carry a stamp: the box alone is 2931 bytes and the packet around
      -- it is 2982. Measuring the part rather than the whole is how a bound
      -- ends up justifying a number it no longer covers.
      --
      -- The stamped form is the larger of the two, so it is the one measured.
      -- The margin left over covers only what is outside this value: the
      -- protocol framing, which is the (id, bytes) tuple.
      owner <- newCredentials @S
      let payload = deleteOfMany (mhs maxDeleteTargets)
          box     = makeSignedBox @S (_peerSignPk owner) (_peerSignSk owner) payload
          stamp   = MessageStamp1 (_peerSignPk owner) maxBound
          packet  = serialise (MailBoxProtoV1 @S @() (DeleteMessagesStamped box stamp))
          full    = LBS.length packet
      assertBool ("a full stamped batch encodes to " <> show full <> " bytes")
                 (full < fromIntegral defMaxDatagram - 128)

  , testCase "refuses a predicate that names no message at all" do
      -- Well formed and authorising nothing. It is not an attack and not a
      -- target mismatch, so it must not be reported as one.
      owner <- newCredentials @S
      let empty = DeleteMessagesPayload (MailboxMessagePredicate1 End)
      admitDeleted (mailboxOf owner) (mh "m1") (proofBytes owner empty)
        @?= MergeUnsupportedPred
      deleteTargets (deleteOfMany []) @?= Left MergeUnsupportedPred

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
