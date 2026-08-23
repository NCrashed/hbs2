{-# Language BangPatterns #-}
-- | Whether an entry somebody else's tree carries may be merged into ours.
--
-- Split out of @mailboxMergeQ@ because the decision was buried inside a
-- 'runMaybeT' inside an @S.toList_@ inside a @polling@, and while it was there
-- it looked for years like a check it was not: it established that a @Deleted@
-- entry's proof is /a/ delete box signed by the mailbox key, and every reader
-- downstream took that to mean the proof authorised deleting the message the
-- entry names. It did not, and one public delete box worked as a proof for
-- anything else in the same mailbox. See issue #15.
--
-- Nothing here touches storage, the network or a clock, so every answer is a
-- one-line test. What is deliberately NOT here is fetching: "this node does not
-- have the proof block yet" is a download, not a judgement, and the worker keeps
-- it so that a proof still in flight is retried rather than refused.
module HBS2.Peer.Proto.Mailbox.Merge
  ( MergeVerdict(..)
  , admitDeleted
  , deleteTargets
  , deleteNaming
  , maxDeleteTargets
  , enqueueMerge
  , maxMergeQueue
  ) where

import HBS2.Prelude

import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Peer.Proto.Mailbox.Ref

import HBS2.Data.Types.Refs (HashRef)
import HBS2.Data.Types.SignedBox

import Codec.Serialise (deserialiseOrFail)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)

-- | Add one entry to a mailbox's pending merge set.
--
-- A named function for one call to 'HM.insertWith', because the two call sites
-- that need it were spelled with 'HM.insert' and one of them is INSIDE the loop
-- over a downloaded tree's entries: it replaced the whole set on every
-- iteration, so a tree carrying N @Deleted@ entries contributed at most one of
-- them. The loss was permanent rather than merely racy, because the surrounding
-- code counts fetch failures only, and a clobbered entry is not a fetch failure,
-- so the download was dropped from the queue as complete.
--
-- Accumulating is the whole content of it, and that is what the test asserts:
-- the interesting property is not what 'HM.insertWith' does, it is that this is
-- what the callers do.
--
-- AND BOUNDED, which it was not. Three call sites feed this queue and one of
-- them walks a DOWNLOADED TREE, enqueuing every @Deleted@ entry in it with no
-- validation; a second puts an entry back whenever its proof block is absent,
-- for as long as it stays absent. So a tree a stranger uploaded -- about ten
-- megabytes for a hundred thousand entries naming proofs that do not exist --
-- bought a hundred thousand permanent queue entries, and behind each of them a
-- 'wip' entry the downloader's sweeper never removes (it removes on completion)
-- and a row in the brains database that SURVIVES A RESTART, plus one storage
-- read per entry per poll, forever.
--
-- The comment on the re-enqueue bounds WHICH MAILBOX may put entries here --
-- only ones this peer hosts -- and says nothing about how many. That is the
-- same mistake the download queue had: the key there is a (version, hash) pair
-- and the hash is a value the announcer invents.
--
-- NOTHING IS LOST OVER THE CAP, which is what makes a cap the right answer
-- rather than a lesser evil: an entry that does not fit is still in the tree it
-- came from, and the next status walks that tree again. What the cap buys is
-- that the three unbounded stores behind this one stop growing.
enqueueMerge
  :: (Eq k, Hashable k)
  => k
  -> HashRef
  -> HashMap k (HashSet HashRef)
  -> HashMap k (HashSet HashRef)
enqueueMerge k h m
  | HS.member h here      = m
  | HS.size here >= maxMergeQueue = m
  | otherwise             = HM.insertWith (<>) k (HS.singleton h) m
  where
    here = HM.lookupDefault mempty k m

-- | The most entries one mailbox's merge queue will hold at a time.
--
-- A judgement, like 'maxMailboxDownloads'. Deletes are rare next to messages,
-- and this queue is drained every two seconds, so a real mailbox never has
-- thousands of them outstanding; what the number has to be is comfortably above
-- any honest burst and small enough that a poll over it stays cheap.
maxMergeQueue :: Int
maxMergeQueue = 4096

-- | The most messages one delete payload may name (PEP-23 step B).
--
-- A gossiped packet has to fit 'defMaxDatagram', which is 4096 bytes, and a
-- datagram over that is one nobody receives -- silently, since nothing here
-- reports an oversized send. A full batch measures 2931 bytes, which a test in
-- @MailboxMerge@ asserts rather than leaving in this sentence, so the number
-- below cannot drift away from the thing that justifies it.
--
-- IT IS ALSO A BOUND ON WORK, and that is the half a datagram does not cover.
-- The merge path reads a proof out of the block store rather than off the wire,
-- and a block is up to 256 KiB, so a hostile payload could name thousands of
-- messages and buy that many entries and merge-queue slots for one signature.
-- 'deleteTargets' stops counting here, whichever path the payload arrived on.
maxDeleteTargets :: Int
maxDeleteTargets = 64

-- | The messages a delete payload authorises removing.
--
-- ONE READING OF A PREDICATE, and there used to be two: this module carried a
-- @PlainMessageDelete@ pattern for the single-hash shape and the worker matched
-- on it separately. Two spellings of "what does this delete say" are two places
-- to teach about a new shape, and the shape below is new.
--
-- WHAT IS HONOURED: 'Or' spines and 'End', so a payload denotes the set of
-- hashes its @Op (MessageHashEq h)@ leaves name. 'Or' was already in the wire
-- format and decodes on every deployed peer, which is why a set is spelled with
-- it rather than with a new 'SimplePredicate' constructor: this type lives
-- INSIDE the signed box, so a new constructor would make an older peer fail in
-- 'unboxSignedBox0' and drop the delete whole, with no verdict and no
-- diagnostic, instead of answering 'MergeUnsupportedPred' and relaying it on.
--
-- WHAT IS NOT: 'And', which has no meaning over a set of hashes and must not be
-- given one by guesswork; 'Nop', whose meaning is unspecified, so a newer build
-- may mean something by it that an older one must not act on; and a predicate
-- that names no message at all, which authorises nothing. All three are the
-- same answer to a reader: this build cannot tell what you meant.
deleteTargets :: forall s . DeleteMessagesPayload s
              -> Either MergeVerdict (HashSet HashRef)
deleteTargets (DeleteMessagesPayload (MailboxMessagePredicate1 e0)) = go 0 mempty [e0]
  where
    -- Counting LEAVES VISITED and not the size of the set, for two reasons.
    -- 'HS.size' is a walk, so asking it per node would make this quadratic in a
    -- value a stranger chooses the size of. And a payload that names one hash a
    -- thousand times has still named a thousand targets: deduplication is a
    -- courtesy to the honest caller, not a discount on the bound.
    go :: Int -> HashSet HashRef -> [SimplePredicateExpr]
       -> Either MergeVerdict (HashSet HashRef)
    go !n _ _ | n > maxDeleteTargets = Left MergeTooManyTargets
    go _ acc [] | HS.null acc = Left MergeUnsupportedPred
                | otherwise   = Right acc
    go !n acc (x:xs) = case x of
      End                  -> go n acc xs
      Or a b               -> go n acc (a:b:xs)
      Op (MessageHashEq h) -> go (n+1) (HS.insert h acc) xs
      Op Nop               -> Left MergeUnsupportedPred
      And{}                -> Left MergeUnsupportedPred

-- | The delete payloads that authorise removing these messages.
--
-- THE WRITER'S HALF OF 'deleteTargets', and here so that it cannot drift from
-- it. A builder living in the hub would be a second opinion about what a delete
-- says, in a package the reader does not depend on, and the two would be free to
-- disagree about the spine shape, the terminator or the batch size.
--
-- Answers a LIST because the batch size is not the caller's to choose:
-- 'maxDeleteTargets' is what a reader will accept, so a caller with more
-- messages than that gets more payloads, each of which it signs and sends
-- separately. An empty input is no payloads at all, not one authorising nothing.
--
-- The spine is right-leaning and terminated by 'End'. Nothing requires that of a
-- writer -- 'deleteTargets' reads any shape -- so it is a choice about being
-- boring rather than a promise anybody may rely on.
deleteNaming :: forall s . [HashRef] -> [DeleteMessagesPayload s]
deleteNaming =
  fmap (DeleteMessagesPayload . MailboxMessagePredicate1 . spine) . chunk . dedup
  where
    spine = foldr (Or . Op . MessageHashEq) End

    chunk [] = []
    chunk xs = let (a,b) = splitAt maxDeleteTargets xs in a : chunk b

    -- Order-preserving, so the same input builds the same payloads twice. The
    -- set 'deleteTargets' reads back does not care, but a caller comparing two
    -- runs, or an operator reading a log, does.
    dedup = go mempty
      where
        go _ [] = []
        go seen (x:xs) | HS.member x seen = go seen xs
                       | otherwise        = x : go (HS.insert x seen) xs

-- | Why a @Deleted@ entry was or was not merged.
--
-- Seven answers and not a Bool, because a refusal goes in the log and the
-- reasons call for different things: three of them are somebody's bug, one is a
-- build that is too old, and two are an attack.
data MergeVerdict =
    -- | The proof is signed by this mailbox's key and names this entry's target.
    MergeAccept
    -- | The block the proof points at is not a signed delete payload at all.
    --
    -- Permanent: the block is content-addressed, so those bytes will not become
    -- something else. Refused rather than retried.
  | MergeBadProofBlock
    -- | It is one, and the signature does not verify.
  | MergeUnsigned
    -- | It verifies, for a different mailbox. Somebody's proof, not ours.
  | MergeWrongMailbox
    -- | It verifies, for THIS mailbox, and authorises deleting a different
    -- message than the entry it is attached to.
    --
    -- THE ONE THIS MODULE EXISTS FOR. Every delete box the owner has ever issued
    -- is public: it is gossiped by the @DeleteMessages@ branch and stored as a
    -- block. Without this case, any peer that has finished a handshake can take
    -- one of them, staple it to an entry naming somebody else's message, serve a
    -- tree holding that entry, and have the message disappear from the mailbox
    -- with no missing block, no unsettled state and no diagnostic anywhere.
  | MergeWrongTarget
    -- | A predicate this build does not implement.
    --
    -- The predicate language is wider than any reader implements, so this is a
    -- refusal and not a failure: an older reader must not guess what a newer one
    -- meant by a delete. Since PEP-23 step B, @Or@ IS honoured, and what is left
    -- here is @And@, @Nop@, and a predicate naming no message at all. See
    -- 'deleteTargets'.
  | MergeUnsupportedPred
    -- | It names more messages than 'maxDeleteTargets'.
    --
    -- Its own answer and not folded into the one above, because they call for
    -- different things: that one is a build too old to understand a delete,
    -- this one is a payload no honest builder produces. The merge path reads
    -- proofs out of the block store, where a value is up to 256 KiB rather than
    -- one datagram, so this is the ceiling on how many entries and merge-queue
    -- slots one signature can buy.
  | MergeTooManyTargets
  deriving stock (Eq,Show,Generic)

instance Pretty MergeVerdict where
  pretty = \case
    MergeAccept          -> "accepted"
    MergeBadProofBlock   -> "the block its proof names is not a signed delete payload"
    MergeUnsigned        -> "the delete payload's signature does not verify"
    MergeWrongMailbox    -> "the delete payload is signed for another mailbox"
    MergeWrongTarget     -> "the delete payload is for this mailbox and authorises another message"
    MergeUnsupportedPred -> "the delete payload carries a predicate this build does not implement"
    MergeTooManyTargets  -> "the delete payload names more messages than"
                              <+> pretty maxDeleteTargets

-- | May this @Deleted@ entry be merged into this mailbox?
--
-- Takes the bytes of the block the entry's proof points at, rather than the
-- block's hash, because everything from there on is a judgement: deserialising
-- them, verifying the signature, and -- the part that was missing -- checking
-- that what the signature authorises is what the entry does.
--
-- The two hashes that must agree are the entry's target and the predicate's
-- argument. Both used to be discarded at the match site: the entry's in a @_@
-- pattern, the payload's by never reading @dmpPredicate@ on this path at all.
--
-- SINCE PEP-23 STEP B a payload names a SET (see 'deleteTargets'), and the
-- property this function exists to protect is unchanged by that: the proof must
-- still name the message the entry deletes, so a public delete box stapled to
-- somebody else's letter is still 'MergeWrongTarget'. A set widens what one box
-- authorises to what its signer actually wrote down, and not by one message.
admitDeleted
  :: forall s . ForMailbox s
  => MailboxRefKey s      -- ^ the mailbox being merged into
  -> HashRef              -- ^ the entry's target: the message it removes
  -> LBS.ByteString       -- ^ the bytes of the block the entry's proof names
  -> MergeVerdict

admitDeleted (MailboxRefKey want) what bs =
  case deserialiseOrFail @(SignedBox (DeleteMessagesPayload s) s) bs of
    Left{}    -> MergeBadProofBlock
    Right box -> case unboxSignedBox0 box of
      Nothing -> MergeUnsigned
      Just (pk, payload)
        | pk /= want -> MergeWrongMailbox
        -- The refusal 'deleteTargets' returns is passed through as it stands,
        -- because it has already picked which of the reasons this is. Collapsing
        -- it into one answer here would put an old build and a hostile payload
        -- back under the same word, which is the mistake the verdict type exists
        -- to prevent.
        | otherwise -> case deleteTargets payload of
            Left verdict -> verdict
            Right targets
              | HS.member what targets -> MergeAccept
              | otherwise              -> MergeWrongTarget
