-- The dispatch over 'AuthorContent' below is total on purpose, for the reason
-- the library modules give: this runs on content an attacker composed, inside
-- the triage loop, so a constructor added without a case here is a crash in that
-- loop rather than a build error.
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | Reading a hub's ingress mailbox (PEP-18 "Mailbox ingress", PEP-22 @hub
-- inbox@).
--
-- The seam between the Mailbox protocol and the pure library: it turns a mailbox
-- key into a list of verified letters, and nothing else. It does not fold, does
-- not mint, does not delete, and does not need canon. Everything it decides
-- about a letter comes out of "HBS2.Hub.Letter".
--
-- Read-only, and deliberately so: the accept path is irreversible, because an
-- event minted into canon is in every clone forever. Composing and sending a
-- letter lives next door in "HBS2.Hub.CLI.Compose", which is also read-only with
-- respect to canon: it puts a letter in a mailbox, and a mailbox is retractable.
--
-- What this module must never do is throw on one message. A mailbox is public,
-- anybody can put anything in it, and a reader that stops at the first thing it
-- cannot open is a queue one stranger can close. Every failure is a value; see
-- 'OpenError'.
module HBS2.Hub.Ingress
  ( Ingress(..)
  , OpenError(..)
  , LetterView(..)
  , InboxRead(..)
  , MailboxUnknown(..)
  , liveMessages
  , readInbox
  , readInboxFrom
  , MailboxLive(..)
  , mailboxLive
  , openMessage
  , openBlock
  , copyOf
  , copiesOf
  , awaitMailbox
  , maxFetchRounds
  , maxInboxLetters
  , maxMailboxBlocks
  , fetchRound
  , rpcTimeout
  , bounded
  , PeerSilent(..)
  , LetterRaw(..)
  , rawMessage
  , PartFacts(..)
  , PartTrouble(..)
  , measurePart
  , maxPartBlocks
  , TreeWeight(..)
  , weighTree
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter

import HBS2.CLI.Prelude

import HBS2.Base58 (AsBase58(..))
import HBS2.Merkle ( walkMerkleUnique,walkMerkleTree,walkMerkle,MTreeAnn(..),MTree(..)
                   , pattern EncryptGroupNaClSymm )
import HBS2.Data.Detect (tryDetect,BlobType(..))
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject,Hash,HbSync)
import HBS2.Data.Types.SignedBox (unboxSignedBox0)
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Entry
import HBS2.Storage.Operations.Class (OperationError)

import Codec.Serialise (deserialiseOrFail,serialise)
import Crypto.Saltine.Class qualified as Saltine
import Data.Coerce (coerce)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Set qualified as Set
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.List qualified as List
import Data.Word (Word64)
import Data.Text qualified as Text
import Data.Maybe (isJust,listToMaybe,fromMaybe)
import Streaming.Prelude qualified as S
import Control.Monad.Except (runExceptT,throwError)

-- | What reading an inbox needs from the outside, gathered so the walk below
-- can be read without knowing how any of it is served.
--
-- A record rather than a class, following 'ReadMessageServices' next door: this
-- has exactly one implementation and the alternative is a class with one
-- instance, which is the same thing with more places to look.
data Ingress m = Ingress
  { -- | A block from local storage. Lazy bytes, because that is what the storage
    -- gives and what every reader here consumes: a strict field made the wiring
    -- toStrict on the way in and all three readers fromStrict on the way back.
    igBlock    :: HashRef -> m (Maybe LBS.ByteString)
    -- | How big a block is, without fetching it. 'Nothing' means it is not here.
    --
    -- Its own call rather than @length <$> igBlock@, and that is the whole
    -- point of it: measuring an attachment by fetching it is what the size gate
    -- exists to avoid (PEP-18), so the one place that needs a size before
    -- deciding anything must be able to ask for the size alone.
  , igSize     :: HashRef -> m (Maybe Integer)
    -- | Decrypt a part, and hand back the secret that opened it.
    --
    -- The secret comes back because canon needs it and cannot derive it: at
    -- fold the owner publishes it beside the event so that a public clone can
    -- decrypt an attachment it can see referenced (PEP-18 "Attachments in
    -- public canon"). A reader that only returned the bytes would leave the
    -- fold with a reference nobody but the maintainers can follow.
    --
    -- 'Left' is "this node cannot open it", which is a state and not a
    -- failure: a part sealed to another maintainer is ordinary.
  , igOpenPart :: HashRef -> m (Either Text (LBS.ByteString, BS.ByteString))
    -- | Does the peer hold this mailbox at all? 'Nothing' means it does not.
    --
    -- A function rather than the 'ServiceCaller' this used to be, and so is the
    -- one below. The whole reason the ingress moved out of the executable was
    -- that its decisions should be testable, and with a service caller in the
    -- record they were not: the wait loop and every 'OpenError' still needed a
    -- live peer. The seam stopped one call short of where it had to be.
  , igStatus   :: HubKey -> m (Maybe ())
    -- | Ask the peer to fetch a mailbox. Returns nothing: the peer's own answer
    -- is nothing, since it queues the key and returns before anything is
    -- fetched, which is the reason 'awaitMailbox' exists.
  , igFetch    :: HubKey -> m ()
    -- | The root of the peer's copy of the mailbox tree, right now.
  , igRoot     :: HubKey -> m (Maybe HashRef)
    -- | Wait. An effect on the outside world like the others, and here for the
    -- same reason: with 'pause' written into the loop, the one test that proves
    -- the wait is bounded had to actually wait, which put twenty seconds into a
    -- unit suite and made the bound expensive to assert rather than cheap.
  , igPause    :: Timeout 'Seconds -> m ()
    -- | Whether a letter from this key may be folded at all (PEP-21). Asked
    -- here as well as at accept time, because triage is a queue a human reads
    -- and a banned author's letter should not be in it.
  , igAllowed  :: HubKey -> Bool
    -- | Resolve the group secret for a message.
    --
    -- The one call here whose failures are NOT turned into an 'OpenError', and
    -- that is a choice rather than an omission. In the real wiring this is a
    -- keyman lookup over sqlite, so it can fail for reasons that have nothing to
    -- do with the message: a locked database, a missing keyring. Those are the
    -- reader's own problem, they are the same for every message in the queue, and
    -- reporting them once per letter would bury the letters. So they propagate.
    --
    -- What must never propagate is anything a SENDER can cause, and none of that
    -- arrives here: the two exception types 'openMessage' catches are the whole
    -- of what a message's own bytes can raise.
  , igSecret   :: ReadMessageServices HBS2Basic
  }

-- | Why a message in the mailbox did not become a letter.
--
-- Five cases and not one, because they call for five different things from
-- whoever is reading the queue, and the difference is not recoverable later.
-- They were collapsed into a single "malformed payload", which said the wrong
-- thing about four of them: 'MalformedPayload' is a claim about DECRYPTED bytes,
-- and these are the ways of never reaching any.
data OpenError =
    -- | The message block is not in local storage yet. A wait, not a fault: the
    -- peer downloads mailbox contents on its own schedule and this is the
    -- ordinary state of a mailbox that was just fetched.
    NotFetched
    -- | The block IS here, and it is not a message.
    --
    -- Split out of 'NotFetched', which said "not fetched yet" about it: a wait,
    -- for something that will never change. An operator retried forever and the
    -- exit code stayed 0. 'readEntries', fifteen lines above the site that got
    -- this wrong, already draws exactly this distinction for tree blocks ("a
    -- block that is here but does not decode as an entry counts as a miss too"),
    -- so the module knew the difference and did not apply it here.
    --
    -- Nobody is named: the envelope is inside the bytes that did not decode.
  | NotAMessage
    -- | No key this node holds appears in the message's group key. Ordinary: a
    -- mailbox accepts messages sealed to anybody, and one addressed to another
    -- maintainer is a line in the queue rather than a problem.
  | NotForUs
    -- | A group key this build cannot resolve, because it was passed by
    -- reference rather than inline and that is not implemented yet (the peer's
    -- own @TODO: support-groupkey-by-reference@).
    --
    -- Not 'NotForUs', which is what this used to report, and the difference is
    -- the same one this module exists to keep: the letter may well be addressed
    -- to us, and nothing here has checked. Saying "not sealed to any key this
    -- node holds" would be a claim about something never examined.
  | GroupKeyByRef
    -- | A key WAS found and the ciphertext did not open with it, or opened to
    -- bytes that are not a payload. Either the message is corrupt or somebody
    -- built a group key naming this node and put rubbish inside it.
    --
    -- Anyone can do that: mailbox recipients' encryption keys are public in
    -- their sigils, so a sender can produce a group key that resolves here. It
    -- is therefore a line in the queue and not an exception, which is exactly
    -- what it used to be: this arrives as 'OperationError', a different type
    -- from the one the catch named, so it escaped the loop and took the whole
    -- listing with it. One stranger could close the queue.
  | Undecipherable
    -- | The envelope signature does not verify. The one that is an accusation,
    -- and the one somebody acts on ('hub block'), so it must not share a
    -- constructor with the others.
  | BadEnvelopeSig
    -- | The envelope opened and the letter inside did not. Carried whole,
    -- because the letter layer already draws the distinctions that matter here
    -- (a forged inner box is not a sender from a newer schema) and flattening
    -- them would undo that work.
  | BadLetterHere LetterError
  deriving stock (Eq,Show)

instance Pretty OpenError where
  pretty = \case
    NotFetched      -> "not fetched yet"
    NotAMessage     -> "the block is here and is not a message"
    NotForUs        -> "not sealed to any key this node holds"
    GroupKeyByRef   -> "group key by reference, which this build cannot resolve"
    Undecipherable  -> "a key resolved for it and the ciphertext did not open"
    BadEnvelopeSig  -> "the envelope signature does not verify"
    BadLetterHere e -> pretty e

-- | One letter as triage sees it before anything is decided about it.
--
-- The message hash is first because it is the letter's identity everywhere
-- else: it is the @origin@ a folded event carries, the argument @hub inbox
-- show@ takes, and the handle the retention rule deletes by.
data LetterView = LetterView
  { lvMessage  :: HashRef            -- ^ the Mailbox message this came in
    -- | Who signed the envelope, when the message opened far enough to say.
    -- Absent rather than a placeholder, and printed even on the failure paths
    -- where it IS known: it is the key a maintainer blocks by.
  , lvEnvelope :: Maybe HubKey
    -- | What the letter turned out to be, or why it could not be read. A
    -- 'Left' here is not an error to stop on: a mailbox is public, anybody can
    -- send anything to it, and a queue that stops at the first unreadable
    -- message is a queue one stranger can close.
  , lvLetter   :: Either OpenError (HubKey, AuthorContent, Disposition)
    -- | The event-id this letter would fold to, which is the hash of the inner
    -- box and therefore computable by the sender before any maintainer has
    -- looked (PEP-18). Present whenever the box could be read at all, even for
    -- a letter that will be refused: it is how a sender correlates the
    -- acknowledgement they get back with the thread they asked for.
  , lvEventId  :: Maybe EventId
    -- | Other messages in this mailbox carrying THE SAME LETTER, by hash.
    --
    -- A rewrap needs no key at all: 'SignedBox' keeps its payload as opaque
    -- bytes and 'MessageContent' has no sender field, so anyone who saw the
    -- ciphertext can re-sign it under a fresh key and produce a new message
    -- hash for a letter they never read. Canon is not at risk from that -- an
    -- event-id is the hash of the inner box, so the second copy is refused as
    -- 'AlreadyInCanon', and a request honoured twice is caught by
    -- @ccHonours@ -- but everything a HUMAN does was one-shot: one queue line
    -- per copy, one decision per copy, one tombstone per copy.
    --
    -- WHAT MAKES THE GROUPING POSSIBLE WITHOUT A KEY: a rewrap that still
    -- delivers the same letter must carry @messageData@ byte for byte, because
    -- producing a different valid ciphertext of one plaintext needs the
    -- plaintext. So the hash of that block is an identity a rewrap cannot
    -- change and this node can compute before a keyman lookup, before a
    -- secretbox open, before anything.
    --
    -- A copy CAN be made distinct by padding the ciphertext, and that is not a
    -- hole in this: such a message no longer opens, so it is a line in the
    -- queue rather than a decision, which is the cheaper of the two.
  , lvCopies   :: [HashRef]
  }
  deriving stock (Eq,Show)

-- | What reading a whole inbox produced.
--
-- The misses are a field rather than a log line. A mailbox tree read with holes
-- in it is not a shorter inbox, it is a WRONG one, and wrong in both directions:
-- a chunk carrying @Deleted@ entries that never arrived puts already-folded
-- letters back in front of a maintainer, and a chunk carrying @Exists@ entries
-- makes letters vanish. The peer treats the same condition as a failure and
-- refuses to mark the state downloaded; a reader that printed to stderr and
-- returned a list looked complete to everything downstream of it.
data InboxRead = InboxRead
  { irLetters :: [LetterView]
    -- | Tree blocks the walk could not read, by hash.
    --
    -- The hashes and not only a count: a hash is the one thing anybody can act
    -- on (@hbs2-peer download@ takes one), and the first version of this threw
    -- them at stderr, the second threw them away to gain a counter. Both.
  , irMissing :: [HashRef]
    -- | Whether the walk gave up at 'maxMailboxBlocks' before reaching the end
    -- of the tree. 'False' for every ordinary read.
    --
    -- NOT 'irOmitted', and the difference is what a caller does next. Omitted
    -- letters are behind the page: they exist, this reader knows their hashes,
    -- and @--after@ walks to them. This says the walk itself stopped, so the
    -- live set is a prefix of the mailbox and no cursor reaches past it -- every
    -- page of a truncated read is a page of the same partial set.
  , irTruncated :: Bool
    -- | Whether the peer's copy of the mailbox had stopped changing by the time
    -- it was read. 'False' means the letters below are a snapshot of something
    -- still arriving: not an error, and not something to keep quiet about
    -- either, which is what happened while this was computed and discarded.
    --
    -- 'False' with an EMPTY 'irLetters' is the case worth naming, because it is
    -- the one that used to lie: it means the peer never produced a root for this
    -- mailbox in the time allowed, and a mailbox with nothing in it and a
    -- mailbox still downloading look exactly alike from here (see
    -- 'awaitMailbox'). An empty inbox is therefore only believable when this is
    -- 'True'.
  , irSettled :: Bool
    -- | How many live messages were left unopened because 'maxInboxLetters' was
    -- reached. Zero for every ordinary read.
    --
    -- A field and not a log line, for the reason 'irMissing' is one: a truncated
    -- queue is a WRONG queue, not a shorter one, and one a stranger can cause. A
    -- mailbox is public, so the number of letters in it is chosen by whoever
    -- writes to it, and this reader used to open every single one and hold every
    -- 'LetterView' -- each carrying a body of up to 'maxInlineBody' -- resident
    -- before printing a line. Ten thousand letters is one RPC per tree node,
    -- one per entry, one per message, a keyman lookup and a secretbox open each,
    -- and a few hundred megabytes. The canon reader next door carries three
    -- bounds and its own refusal for each; this had none.
  , irOmitted :: Int
    -- | How many of the letters this page opened were from a denied author.
    --
    -- Counted here and kept OUT of 'irLetters', which is what the deny-list was
    -- supposed to do all along: 'igAllowed' is documented as "asked here as well
    -- as at accept time, because triage is a queue a human reads and a banned
    -- author's letter should not be in it", and it was in it -- one line each,
    -- indistinguishable from a letter that failed to decrypt.
    --
    -- The COUNT stays, because a queue that hides them silently is a queue that
    -- lies about how much of it a stranger is producing, and because a
    -- maintainer who banned somebody by mistake would have no way to see that
    -- anything was happening.
    --
    -- What this does NOT do is give back the slot: the ban is on the inner
    -- author, so the letter has to be decrypted before anybody knows whose it
    -- is. Only 'iaAfter' gets past a page a stranger filled.
  , irDenied  :: Int
    -- | The last live message this page looked at, or 'Nothing' for a page that
    -- looked at none.
    --
    -- What @--after@ takes, and it is the last one READ rather than the last one
    -- shown: a page whose every letter was denied or grouped away would
    -- otherwise hand back a cursor that does not advance, and paging would loop
    -- on it forever.
  , irCursor  :: Maybe HashRef
    -- | Every live message the tree holds, opened or not.
    --
    -- The queue above is bounded and this is not, and the difference is what a
    -- caller is asking about. "Show me a page of this mailbox" is bounded by
    -- what a person can read; "is this letter in this mailbox" is a question
    -- about the mailbox, and answering it out of the bounded prefix made the
    -- answer a function of how many letters a stranger had sent.
    --
    -- It costs nothing to carry: the walk materializes this set to compute the
    -- prefix in the first place, so what was bounded was never the set, only
    -- the bodies opened out of it.
    --
    -- Bounded by the WALK, which is a different bound and a weaker promise:
    -- 'maxMailboxBlocks' caps what the tree costs to read, so "every live
    -- message" means every one the walk reached. 'irTruncated' says when that
    -- is not all of them.
  , irLive :: HashSet HashRef
  }
  deriving stock (Eq,Show)

-- | The most letters one @hub inbox@ will open.
--
-- Not a guess about mailboxes, a guess about PEOPLE: this is a triage queue that
-- somebody reads, and a thousand lines is already past what anybody works
-- through in a sitting. It bounds the resident cost at roughly a thousand bodies
-- rather than however many a stranger cared to send, and what is left out is
-- counted and said rather than dropped.
maxInboxLetters :: Int
maxInboxLetters = 1000

-- | The peer does not have this mailbox, so there is nothing to read and there
-- never will be.
--
-- Not an empty inbox, and the difference is not cosmetic. A peer only gossips a
-- @CheckMailbox@ for a mailbox it holds locally (@mailboxFetchQ@ skips the rest
-- with no diagnostic), so reading an arbitrary mailbox key returns nothing on
-- this run and on every future one. Reported as a distinct failure because the
-- fix is @hbs2-peer mailbox create@ and no amount of waiting is it.
newtype MailboxUnknown = MailboxUnknown HubKey

-- Hand-written, because the derived one prints the saltine internals
-- (@Sign.PublicKey {hashesTo = ...}@) and this is a message with an action in it:
-- the key has to be the base58 the caller typed, and the fix has to be named,
-- since no amount of retrying is it.
instance Show MailboxUnknown where
  show (MailboxUnknown k) =
    "this peer does not have the mailbox " <> show (pretty (AsBase58 k))
      <> ": it only asks the network about mailboxes it holds locally, so this"
      <> " would read as empty on every run. Create it with"
      <> " `hbs2-peer mailbox create --key " <> show (pretty (AsBase58 k)) <> " hub`."

instance Exception MailboxUnknown

-- | The messages a mailbox currently holds.
--
-- A mailbox tree is an append-only log of 'Exists' and 'Deleted' entries for
-- the same message hash, so "what is in the mailbox" is a difference of two
-- sets rather than a list. The entries carry no ordering information at all
-- (both proofs are a 'Maybe HashRef' and nothing else), so a difference is the
-- only answer that is defined: there is no "latest" to prefer.
--
-- Deletion winning is therefore not a policy choice here, and what makes it
-- safe is a check in the peer that this comment used to describe wrongly. It
-- said a @Deleted@ entry enters the tree only if its payload box is signed by
-- the mailbox's own key, and concluded that a stranger cannot hide a letter
-- from triage. The first half was true and the conclusion did not follow: the
-- merge never compared what the signed box AUTHORISED with what the entry
-- deleted, so any one of the owner's delete boxes -- all of them public, all of
-- them gossiped -- worked as a proof for anything else in the same mailbox
-- (issue #15).
--
-- 'HBS2.Peer.Proto.Mailbox.Merge.admitDeleted' now compares the two, so the
-- claim holds as written. It holds going FORWARD: the merge carries entries
-- already in a tree across without re-checking them, so a peer that accepted a
-- forgery before the fix still serves it. Reading a mailbox whose history
-- predates the fix is reading a tree that could have been poisoned, and the
-- remedy for that is to recreate the mailbox rather than anything this module
-- can do.
liveMessages :: [MailboxEntry] -> HashSet HashRef
liveMessages es = HS.difference exists deleted
  where
    exists  = HS.fromList [ h | Exists _ h  <- es ]
    deleted = HS.fromList [ h | Deleted _ h <- es ]

-- | How many rounds 'awaitMailbox' waits for the peer's copy of a mailbox to
-- stop growing.
--
-- Bounded, because the alternative is waiting forever on a mailbox whose sender
-- is offline, and short, because the thing being waited for is a local download
-- of a tree the peer already knows the root of. What it costs to be wrong is one
-- more @hub inbox@.
maxFetchRounds :: Int
maxFetchRounds = 8

-- | And how long between them. The peer's own download poll runs every two
-- seconds, so anything faster is asking a question it cannot have answered yet.
fetchRound :: Timeout 'Seconds
fetchRound = 2.5

-- | How long to wait on one RPC call to the peer.
--
-- Named rather than written at each call site: these are calls to a process on
-- the same machine over a unix socket, so the number is about the peer being
-- wedged and not about the network, and two callers that disagreed about it
-- would be two different opinions on what "wedged" means.
rpcTimeout :: Timeout 'Seconds
rpcTimeout = 10

-- | Ask the peer to fetch a mailbox, and wait until its copy settles.
--
-- 'RpcMailboxFetch' returns as soon as the key is queued: the gossip, the
-- replies and the download all happen afterwards, on the peer's own poll. So
-- reading storage straight after it is a race with the download, and on a first
-- run against a live mailbox it returns nothing or a prefix. This waits for the
-- root to stop changing instead, and says so when it has not.
--
-- Returns the root and whether it had settled. Settling is not proof of
-- completeness (a message can arrive a second later) and is not treated as any:
-- it is the difference between "read what was there" and "read while it was
-- arriving".
awaitMailbox :: MonadUnliftIO m => Ingress m -> HubKey -> m (Maybe HashRef, Bool)
awaitMailbox ig mbox = do
  -- The status call first, because it is the one that distinguishes a mailbox
  -- the peer does not have from one that is empty, and fetching a mailbox the
  -- peer does not have is a no-op it will not report.
  igStatus ig mbox >>= \case
    Nothing -> throwIO (MailboxUnknown mbox)
    Just () -> pure ()

  igFetch ig mbox

  go maxFetchRounds Nothing

  where
    go 0 seen = pure (seen, False)
    go n seen = do
      root <- igRoot ig mbox
      case root of
        -- NO ROOT IS NOT A SETTLED STATE, and this is the whole of the
        -- correction. The peer writes the mailbox ref only when a merge lands,
        -- so "this peer has no ref for the mailbox" and "this peer has not
        -- finished downloading it" are the same observation from here; there is
        -- nothing local that tells them apart. The loop used to compare the
        -- absence against the absence it started with and call the second look
        -- settled -- 2.5 s into a download whose own path pauses for ten
        -- (@mailboxInQ@), which made "settled" very nearly a constant True and
        -- made an empty answer on a first run indistinguishable from a complete
        -- one. Waiting the rounds out and then saying so is the honest answer,
        -- and it costs a wait only on the mailbox that had nothing to show.
        -- The pause is BEFORE the next look and not after the last one: waiting
        -- on the final round buys an observation nobody makes, and it is 2.5 s
        -- of a wait a person is watching.
        Nothing | n <= 1    -> pure (Nothing, False)
                | otherwise -> igPause ig fetchRound >> go (n - 1) Nothing
        -- Unchanged across a round, and not the first look, since @seen@ starts
        -- as Nothing and this branch has a root: as settled as a local reader
        -- can establish.
        Just _ | root == seen -> pure (root, True)
               | n <= 1       -> pure (root, False)
               | otherwise    -> igPause ig fetchRound >> go (n - 1) root

-- | Fetch, walk and read a mailbox: every live message, opened as far as it
-- will open.
-- | A page of the queue: the first 'maxInboxLetters' of the mailbox.
--
-- The shape every caller wanted before there was anywhere else to start from.
readInbox :: MonadUnliftIO m => Ingress m -> HubKey -> m InboxRead
readInbox ig mbox = readInboxFrom ig mbox Nothing maxInboxLetters

-- | And a page starting after a message, of a chosen size.
--
-- WHY THERE IS A CURSOR AT ALL. The page is the first N of the live set sorted
-- by hash, and a mailbox is public, so which N those are is a function of what
-- strangers have sent. Grinding message hashes below the honest ones is a few
-- thousand signatures and displaces every real letter off the only screen a
-- maintainer triages from -- permanently, since nothing raised the bound and
-- the remedy was rejecting junk one hash at a time. @--after@ is what walks
-- past it.
--
-- BY HASH AND NOT BY OFFSET, because the set changes underneath: an offset
-- names a position in a list that grows, so a letter arriving between two pages
-- shifts everything after it and a page is skipped. A hash names a place in an
-- order that does not move.
--
-- The bound stays a bound: 'limit' is clamped to 'maxInboxLetters', so a caller
-- cannot ask for the whole mailbox and pay a keyman lookup and a secretbox open
-- for every letter a stranger sent.
readInboxFrom :: MonadUnliftIO m
              => Ingress m
              -> HubKey
              -> Maybe HashRef  -- ^ start after this message
              -> Int            -- ^ how many to open, clamped
              -> m InboxRead
readInboxFrom ig mbox after limit = do
  ml <- mailboxLive ig mbox
  -- Sorted explicitly. This used to be HS.toList with a comment claiming it
  -- was sorted by hash: that is the HAMT traversal order, which is a property
  -- of the container's implementation and not a promise it makes. A triage
  -- queue that reorders itself on a dependency bump is one a maintainer cannot
  -- work through a page at a time.
  -- Sorted BEFORE the bound is applied, so which thousand you get is a function
  -- of the mailbox and not of the order the walk happened to return: a
  -- truncated queue that reshuffles between runs would be worse than a
  -- truncated one.
  let all' = sort (HS.toList (mlLive ml))
      -- STRICTLY after, so a cursor handed back by the previous page does not
      -- repeat its last letter -- and so that paging terminates even when the
      -- cursor names a message that has since been deleted, since the
      -- comparison is on the order and not on membership.
      live = maybe all' (\h -> List.dropWhile (<= h) all') after
      (taken, left) = List.splitAt (max 1 (min maxInboxLetters limit)) live

  -- ONE BLOCK READ EACH, then group, then open. A rewrap costs nothing to make
  -- -- see 'lvCopies' -- and every copy used to be its own queue line, its own
  -- keyman lookup, its own secretbox open and its own decision, all for a
  -- letter the maintainer had already read. The grouping key is the hash of the
  -- ciphertext, which needs no key of ours and which a rewrap cannot change.
  --
  -- Only the first of each group is opened. The others were the same bytes, so
  -- the answer would have been the same answer.
  blocks <- for taken $ \h -> (,) h <$> igBlock ig h

  let keyed  = [ (h, copyOf =<< blk, blk) | (h, blk) <- blocks ]
      -- Grouped by the copy id, in the order the sorted page had them, so a
      -- queue read twice is the same queue. A message with no copy id (not a
      -- message, or an envelope that will not open) groups with nothing: there
      -- is nothing to compare, and a guess would put two letters on one line.
      groups = [ (h, blk, [ o | (o, k', _) <- keyed, Just k <- [key], k' == Just k, o /= h ])
               | (h, key, blk) <- keyed
               , maybe True (\k -> firstOf k == Just h) key
               ]
      firstOf k = listToMaybe [ h | (h, k', _) <- keyed, k' == Just k ]

  opened <- for groups $ \(h, blk, copies) ->
              (\lv -> lv { lvCopies = copies }) <$> openBlock ig h blk

  -- AND A DENIED AUTHOR'S LETTER LEAVES THE QUEUE, which is what 'igAllowed'
  -- has always claimed to do here: "triage is a queue a human reads and a
  -- banned author's letter should not be in it". It was in it, as one more
  -- unreadable line, indistinguishable from a letter that would not decrypt.
  --
  -- Counted rather than dropped in silence: a queue that hides them says
  -- nothing about how much of itself a stranger is producing, and a maintainer
  -- who banned the wrong key would see no sign of it.
  let denied lv = case lvLetter lv of
                    Left (BadLetterHere AuthorDenied) -> True
                    _                                 -> False
      lvs = [ lv | lv <- opened, not (denied lv) ]

  pure InboxRead
    { irLetters = lvs
    , irMissing = mlMissing ml
    , irTruncated = mlTruncated ml
    , irSettled = mlSettled ml
    , irOmitted = length left
    , irDenied  = length [ () | lv <- opened, denied lv ]
      -- The last one READ, not the last one shown: see 'irCursor'.
    , irCursor  = listToMaybe (reverse taken)
    , irLive    = mlLive ml
    }

-- | What a mailbox HOLDS, without opening any of it.
--
-- Split out of 'readInbox' because two callers ask only this, and asking it
-- through the queue made them pay for a page of the queue: @hub inbox accept@
-- and @hub inbox show@ name one message and need to know it is in this mailbox
-- (a hash is not a claim about where the bytes came from), and both were
-- decrypting up to 'maxInboxLetters' letters to answer a set membership.
--
-- The walk itself is the same one and its cost is the tree's, which is a
-- stranger's; what is skipped is the keyman lookup, the secretbox open and the
-- parse, per letter, per accept.
data MailboxLive = MailboxLive
  { mlLive      :: HashSet HashRef  -- ^ every live message the walk reached
  , mlMissing   :: [HashRef]        -- ^ tree blocks the walk could not read
  , mlSettled   :: Bool             -- ^ whether the peer's copy had stopped changing
    -- | Whether the walk ran out of 'maxMailboxBlocks' before it finished.
    --
    -- What it means for the two fields above: 'mlLive' is then a SUBSET of the
    -- mailbox and not the mailbox, so a membership answer over it is wrong in
    -- both directions, exactly as a hole is -- an unread chunk of @Exists@
    -- entries hides letters, an unread chunk of @Deleted@ entries brings
    -- settled ones back. 'mlMissing' is partial for the same reason, which is
    -- why 'mailboxDoubt' reports this one FIRST.
  , mlTruncated :: Bool
  }
  deriving stock (Eq,Show)

-- | The most blocks one mailbox walk will read from the peer.
--
-- 'walkMerkleUnique' fixed the SHAPE attack -- a node naming its one child twice
-- costs 2^depth for depth+1 blocks -- and nothing fixed the SIZE. A mailbox is
-- public by design: how many entries its tree holds is a number chosen by
-- whoever writes to it, and this walk runs in full on @hub inbox@, on @hub
-- inbox show@ and on @hub inbox accept@, every time. Bounding the letters
-- OPENED ('maxInboxLetters') bounded the keyman lookups and the secretbox
-- opens; it left one RPC per tree node and one per entry, at the tree's count.
--
-- ONE budget over blocks, because a block read is the unit a stranger makes
-- this pay and both halves of the walk spend it: charging entries alone would
-- leave a tree of a million empty leaves free, and charging nodes alone would
-- leave a leaf naming a million entries free. Charged when a leaf is entered
-- rather than as its entries are read, so a node listing more than the budget
-- has left is stopped before its entries are fetched.
--
-- Running out is not an error and not silence: the walk keeps what it read,
-- says so through 'mlTruncated', and the verbs that ask a membership question
-- refuse on it. There is nothing else honest available -- the answer really is
-- partial, and the only thing that makes it whole again is a smaller mailbox.
--
-- 200 times 'maxInboxLetters', which is the line between a triage queue and a
-- flood rather than where this reader gets tired: a walk of that many blocks is
-- already minutes of sequential round trips, so a mailbox past it had stopped
-- being readable as a queue before the bound was reached. What actually keeps a
-- mailbox under it is retention, which PEP-21 defers: nothing here prunes one,
-- and triage GROWS it, since rejecting a letter writes a @Deleted@ entry beside
-- the @Exists@ entry it settles.
maxMailboxBlocks :: Int
maxMailboxBlocks = 200000

mailboxLive :: MonadUnliftIO m => Ingress m -> HubKey -> m MailboxLive
mailboxLive ig mbox = do
  (root, settled) <- awaitMailbox ig mbox
  case root of
    Nothing   -> pure (MailboxLive mempty [] settled False)
    Just tree -> do
      (entries, misses, cut) <- readEntries tree
      pure (MailboxLive (liveMessages entries) misses settled cut)

  where
    readEntries tree = do
      acc  <- newTVarIO []
      bad  <- newTVarIO []
      -- One budget for the whole walk, spent by the node reads inside it and by
      -- the entry reads below, and 'runExceptT' rather than a flag the loop
      -- checks: 'walkMerkleUnique' has no early exit, so the only way to stop
      -- it is to leave. What is accumulated so far is kept -- the TVars are
      -- outside -- and the caller is told the answer is a prefix.
      left <- newTVarIO maxMailboxBlocks
      let spend n = atomically do
            k <- readTVar left
            if k < n then pure False else True <$ writeTVar left (k - n)
          charge n = lift (spend n) >>= \ok -> unless ok (throwError ())
          look h = charge 1 >> lift (igBlock ig (HashRef h))
      -- walkMerkleUnique, because this tree is a stranger's: the mailbox is
      -- public by design and its root is whatever the peer merged. A plain walk
      -- follows every edge, so a chain of nodes each naming its one child twice
      -- costs 2^depth for depth+1 blocks, and about a kilobyte of well-formed
      -- blocks is nine seconds at depth 22 and does not finish at depth 40. What
      -- this reader wants is the set of entries the tree holds -- the result goes
      -- straight into liveMessages, which is a set difference -- so entering a
      -- node once is the answer to the question being asked.
      cut <- runExceptT $
        walkMerkleUnique @[HashRef] (coerce tree) look $ \case
          Left miss -> atomically $ modifyTVar bad (HashRef miss :)
          Right hs -> do
            charge (length hs)
            es <- lift $ forM hs $ \h -> (h,) <$> readEntry h
            -- A block that is here but does not decode as an entry counts as a
            -- miss too: the effect on the answer is the same as not having it,
            -- and the hash is equally the only thing to act on.
            atomically do
              modifyTVar acc ([ e | (_, Just e) <- es ] <>)
              modifyTVar bad ([ h | (h, Nothing) <- es ] <>)
      (,,) <$> readTVarIO acc
           <*> (sort <$> readTVarIO bad)
           <*> pure (either (const True) (const False) cut)

    readEntry h = igBlock ig h
                    <&> (>>= either (const Nothing) Just
                           . deserialiseOrFail @MailboxEntry)

-- | What one message in a mailbox turns out to be.
--
-- Top-level rather than local to 'readInbox', and that is what makes the mapping
-- below testable at all: it is a function of the 'Ingress' record and a hash, so
-- each of the six answers can be produced without a peer, a mailbox or a merkle
-- tree. The bug this shape keeps out (an exception type escaping the loop) had
-- been fixed once already, and while this lived inside a @where@ clause nothing
-- could have caught it coming back.
openMessage :: MonadUnliftIO m => Ingress m -> HashRef -> m LetterView
openMessage ig mh = igBlock ig mh >>= openBlock ig mh

-- | The identity a rewrap cannot change, for a block already in hand.
--
-- Not a hash of the whole message: that is what a rewrap DOES change, by
-- construction. The ciphertext of the letter is what it cannot, because
-- producing a different valid ciphertext of one plaintext needs the plaintext,
-- and the whole point of the attack is that the rewrapper never had it.
--
-- 'Nothing' for a block that is not a message, or whose envelope will not open:
-- there is nothing to compare, and grouping on a guess would put two different
-- letters on one line, which is worse than the copies it would collapse.
copyOf :: LBS.ByteString -> Maybe HashRef
copyOf blk = do
  msg <- either (const Nothing) Just (deserialiseOrFail @(Message HBS2Basic) blk)
  (_, content) <- unboxSignedBox0 @(MessageContent HBS2Basic) (messageContent msg)
  pure (HashRef (hashObject (serialise (messageData content))))

-- | Which other messages in this mailbox carry the same letter as the one named.
--
-- The grouping 'readInbox' does, asked about one message rather than the whole
-- page, because the verb that acts on a decision has to act on the same set the
-- verb that showed it grouped. A tombstone is written per message hash, so
-- without this rejecting one copy of a rewrapped letter rejected one copy, and
-- the maintainer who read a letter once decided about it N times.
--
-- BOUNDED BY THE PAGE, which is not a shortcut: it is the set this can know
-- without an unbounded walk, and it is the set the operator was looking at. A
-- message from outside it answers with no copies, which is right -- nothing here
-- has seen the others.
copiesOf :: MonadUnliftIO m => Ingress m -> HubKey -> HashRef -> m [HashRef]
copiesOf ig mbox msg = do
  ml <- mailboxLive ig mbox
  let live = List.take maxInboxLetters (sort (HS.toList (mlLive ml)))
  keyed <- for live $ \h -> (,) h . (>>= copyOf) <$> igBlock ig h
  pure $ case List.lookup msg keyed of
    Just (Just k) -> [ h | (h, k') <- keyed, k' == Just k, h /= msg ]
    _             -> []

-- | The reading half of 'openMessage', over a block the caller already fetched.
--
-- Split out so the queue can group copies without paying for the block twice:
-- 'readInbox' reads each block once, works out which of them carry one letter,
-- and only then decrypts -- one keyman lookup and one secretbox open per
-- LETTER rather than per message.
openBlock :: MonadUnliftIO m
          => Ingress m -> HashRef -> Maybe LBS.ByteString -> m LetterView
openBlock ig mh blk = do
  -- Two answers, not one. These used to be the same `Nothing`, so "the peer has
  -- not downloaded this yet" and "the peer has it and it is not a message" both
  -- printed "not fetched yet" and left with 0: the first is a wait and the
  -- second never changes, and only one of them is worth retrying.
  case blk >>= either (const Nothing) Just . deserialiseOrFail @(Message HBS2Basic) of
    Nothing  -> pure (LetterView mh Nothing (Left (unreadable blk)) Nothing [])
    Just msg -> do
      -- The envelope signer is recovered separately from the decryption, and
      -- that is the point. It is authenticated by this call alone, so on every
      -- path below except a bad signature the key is KNOWN, and on a public
      -- mailbox the bulk of the queue is letters sealed to somebody else. Taking
      -- the key from readMessage's success value left all of them anonymous,
      -- which is a queue with no key to block by.
      --
      -- CHECKED FOR BEING A KEY, which 'unboxSignedBox0' does not do and
      -- 'unboxChecked' does.
      --
      -- When this check was added it was load-bearing: the Serialise instances
      -- for a signing key and a signature were derived, so they took a payload
      -- of any length, and libsodium reads its thirty-two bytes and ignores the
      -- rest. A sender who appended one byte to their own public key produced an
      -- envelope that verified under a key equal to nothing anybody has on a
      -- list, so 'openLetterAs's one envelope-level check -- @allowed
      -- envelopeSigner@ -- never fired, and each padding changed the message
      -- hash, so one letter became N stored blocks and N queue lines printing as
      -- @(not a key: N bytes)@.
      --
      -- Those instances are hand-written now and refuse a wrong length, so this
      -- is defence in depth rather than the only defence. It stays: it is one
      -- round trip through the same decoder, and this reader should not have to
      -- know which instance produced the bytes it was handed.
      let envelope = case unboxSignedBox0 @(MessageContent HBS2Basic) (messageContent msg) of
            Just (k, _) | validHubKey k -> Just k
            _                           -> Nothing

      opened <- tryOpen msg
      case opened of
        Left e -> pure (LetterView mh (envelopeFor e envelope) (Left e) Nothing [])
        Right (_, _, payload) -> do
          let md = parsePayload payload
          pure LetterView
            { lvMessage  = mh
            , lvEnvelope = envelope
            , lvLetter   = case (envelope, md) of
                -- THE ONE CASE WHERE THE ENVELOPE KEY IS ALL THERE IS, and the
                -- only one where a ban may be asked of it. 'openLetterAs' has
                -- this branch written out with its reasoning, and it is dead
                -- code: 'parsePayload' checks the version first, so a letter
                -- from a schema this build cannot read never reaches the
                -- function that would ban its sender. The tests reached it by
                -- building a 'MessageData' by hand, so they were green and the
                -- guarantee was not there.
                --
                -- What that cost: such a letter is a wait, not a discard --
                -- rightly, since a newer build folds it -- so a banned sender
                -- could park one in the queue and every later pass would open
                -- it again, forever, with the ban being the only mechanism that
                -- would have stopped it and no way to reach the ban.
                --
                -- ASKED OF THE ENVELOPE KEY ONLY HERE. Everywhere else the list
                -- is a list of inner AUTHORS, and it has to stay that way:
                -- denying content because a banned key relayed it would let
                -- anyone on the list censor other people's letters by rewrapping
                -- them. Here there is no inner author to ask about, because
                -- there is no body this build can open.
                (Just who, Left (UnsupportedVersion v))
                  | not (igAllowed ig who) -> Left (BadLetterHere AuthorDenied)
                  | otherwise -> Left (BadLetterHere (UnsupportedVersion v))
                (_, Left e)           -> Left (BadLetterHere e)
                -- REACHABLE, and it was not before the key above was checked for
                -- being a key: readMessage succeeding means the same unbox
                -- succeeded, so the only way here is an envelope whose signature
                -- verifies under something that is not a key. A key that is not
                -- one is not an identity, so there is nobody to attribute the
                -- letter to and nothing for a deny list to match, and refusing it
                -- is the only answer that does not invent a sender.
                (Nothing, _)          -> Left BadEnvelopeSig
                (Just who, Right md') -> openWith who md'
            , lvEventId  = either (const Nothing) letterEventId md
              -- Filled in by 'readInbox', which is the only caller that knows
              -- what else is in the mailbox. One message read on its own has
              -- nothing to compare against and says so.
            , lvCopies   = []
            }

  where
    -- WHICH of the two "no message here" answers this is. They were one, so a
    -- block the peer holds and cannot be a message printed "not fetched yet":
    -- a wait, for something that will never change, retried forever with a zero
    -- exit. 'readEntries' fifteen lines above draws exactly this distinction for
    -- tree blocks, so the module knew it and did not apply it here.
    unreadable = maybe NotFetched (const NotAMessage)

    -- Two exception types, not one, and this is where a stranger could close the
    -- queue. readMessage's own errors are ReadMessageError, but its last step
    -- decrypts, and that raises OperationError: DecryptionError for ciphertext
    -- that does not open, UnsupportedFormat for bytes that are not a payload. A
    -- catch naming only the first let the second escape here, and through mapM
    -- and readInbox, so one message with a resolvable group key and rubbish
    -- inside it printed no queue at all. Anyone can send that, since recipients'
    -- encryption keys are public in their sigils.
    tryOpen msg =
      (Right <$> readMessage (igSecret ig) msg)
        `catch` (pure . Left . readErr)
        `catch` (\(_ :: OperationError) -> pure (Left Undecipherable))

    readErr = \case
      ReadSignCheckFailed  -> BadEnvelopeSig
      ReadNoGroupKey       -> GroupKeyByRef
      ReadNoGroupKeyAccess -> NotForUs

    -- A forged envelope names nobody: the signature is what would have
    -- established the key, and it did not. Every other case is spelled out
    -- rather than wildcarded, so a new OpenError has to say whether its signer
    -- is known instead of inheriting an answer.
    envelopeFor e k = case e of
      BadEnvelopeSig   -> Nothing
      -- Nobody to name either: the envelope is inside the bytes that did not
      -- decode. Spelled out rather than wildcarded, like the rest of these.
      NotAMessage      -> Nothing
      NotFetched       -> k
      NotForUs         -> k
      GroupKeyByRef    -> k
      Undecipherable   -> k
      BadLetterHere{}  -> k

    openWith who md = do
      (_, author, content, _) <-
        either (Left . BadLetterHere) Right
          (openLetterAs (igAllowed ig) (EnvelopeSigner who) md)
      pure (author, content, classify content)

-- | One message, opened far enough for the bridge (PEP-18 "Folding").
--
-- 'openMessage' answers the QUEUE's question, which is what a letter asks for,
-- and drops three things the accept path cannot do without: the letter as it
-- was sent, the key that signed the envelope, and the secret the message body
-- was encrypted with, which the bridge compares a published part secret
-- against.
--
-- A second read rather than three more fields on 'LetterView'. A queue is a
-- page of letters and an accept is one of them, so carrying a secret per line
-- would hold key material for a thousand letters nobody is going to accept,
-- and 'Show' on the view would have to start hiding fields.
data LetterRaw = LetterRaw
  { lrEnvelope :: HubKey
    -- | The parts the MESSAGE carries.
    --
    -- Named here because the accept path has to produce evidence about each of
    -- them, and the message is the only thing that says which they are: what
    -- the letter references is the sender's claim, and a letter naming a part
    -- its message does not carry is a case the bridge answers rather than one
    -- this should paper over.
  , lrParts    :: [HashRef]
  , lrData     :: MessageData
  , lrSecret   :: MessageSecret
  }

-- | Read one message for the bridge, or say why it could not be read.
--
-- The same errors 'openMessage' reports, because it is the same reading; what
-- differs is only what is kept. A letter the queue could show and this cannot
-- is a bug, not a state.
rawMessage :: MonadUnliftIO m => Ingress m -> HashRef -> m (Either OpenError LetterRaw)
rawMessage ig mh = do
  blk <- igBlock ig mh
  case blk >>= either (const Nothing) Just . deserialiseOrFail @(Message HBS2Basic) of
    Nothing -> pure (Left (maybe NotFetched (const NotAMessage) blk))
    Just msg ->
      (do
        (who, co, payload) <- readMessage (igSecret ig) msg
        -- The secret again, and from the same place readMessage took it. It is
        -- not in what readMessage returns, and asking for the group key twice
        -- is cheaper than widening that signature for one caller.
        gk  <- messageGK0 co & orThrow ReadNoGroupKey
        gks <- rmsFindGKS (igSecret ig) gk >>= orThrow ReadNoGroupKeyAccess
        -- A key that is not the length a key is cannot be a message secret.
        -- Unreachable through libsodium, and the constructor refuses rather
        -- than truncating for the reason every other length check here does.
        sec <- mkMessageSecret (Saltine.encode gks) & orThrow ReadNoGroupKeyAccess
        unless (validHubKey who) (throwIO ReadSignCheckFailed)
        case parsePayload payload of
          Left e   -> pure (Left (BadLetterHere e))
          Right md -> pure (Right (LetterRaw who (Set.toList (messageParts co)) md sec)))
      `catch` (pure . Left . readErr')
      `catch` (\(_ :: OperationError) -> pure (Left Undecipherable))
  where
    readErr' = \case
      ReadSignCheckFailed  -> BadEnvelopeSig
      ReadNoGroupKey       -> GroupKeyByRef
      ReadNoGroupKeyAccess -> NotForUs

-- | What a part turned out to be, without pulling its payload.
--
-- Both numbers are about the CIPHERTEXT, which is what the peer stores and
-- what a transfer costs. That is the honest unit for a bound: the plaintext is
-- a little smaller and is not knowable until it has been paid for.
data PartFacts = PartFacts
  { pfSize    :: Word64
    -- | Every block of it is in local storage. 'False' is not an error: it is
    -- the peer still downloading, which is 'HBS2.Hub.Bridge.PartPending' and a
    -- reason to come back rather than to refuse.
  , pfHere    :: Bool
  }
  deriving stock (Eq,Show)

-- | Two halves of one attachment: what it costs is both of them.
--
-- Sizes add and presence is a conjunction, because a part whose data is here
-- and whose key is not cannot be opened, which is what 'pfHere' is asked.
instance Semigroup PartFacts where
  a <> b = PartFacts (pfSize a + pfSize b) (pfHere a && pfHere b)

instance Monoid PartFacts where
  mempty = PartFacts 0 True

-- | Why a part could not even be measured.
data PartTrouble =
    PartNotFetchedYet     -- ^ its root block is not here, so nothing is known
  | PartNotATree Text     -- ^ the root is here and is not an encrypted tree
  | PartUnreadable Text   -- ^ it is a tree and this could not follow it
  | PartTooManyBlocks Int -- ^ it is a tree and measuring it costs more than this
  deriving stock (Eq,Show)

instance Pretty PartTrouble where
  pretty = \case
    PartNotFetchedYet -> "the part has not been fetched yet"
    PartNotATree e    -> "not an encrypted tree:" <+> pretty (safeText e)
    PartUnreadable e  -> "cannot read the part's tree:" <+> pretty (safeText e)
    PartTooManyBlocks n ->
      "the part's tree asks for more than" <+> pretty n <+> "reads to measure"

-- | The most reads one part measurement will spend.
--
-- A bound on the WALK, and it needs one for the reason 'readInbox' next door
-- carries 'walkMerkleUnique': the tree is a stranger's, a node lists its
-- children with nothing stopping two of them being equal, and following every
-- edge costs 2^depth for depth+1 blocks. About a kilobyte of well-formed blocks
-- is 2^40 visits and does not finish. Reached from a letter naming the tree as
-- a part, measured on the accept path, which is where the operator is waiting.
--
-- NOT solved the way the mailbox tree solves it, and that is the whole reason
-- this constant exists. 'walkMerkleUnique' enters each node once, which is the
-- right answer to "which blocks does this graph mention" and the wrong one
-- here: this walk measures CONTENT, and a file of repeated bytes genuinely does
-- list one leaf block many times over. HBS2.Merkle's own note declines to
-- deduplicate for exactly that reason. Entering a repeated node once would
-- report a part smaller than the one 'igOpenPart' then reads, which is a hole
-- in the gate this function exists to feed. So every edge is still followed and
-- what is bounded is the spend.
--
-- The number counts CALLS, not bytes, because calls are what a stranger makes
-- this pay: one read per tree node, and one 'igSize' per leaf entry below. A
-- part at 'maxPartBytes' is 64 MiB and a block is 'defBlockSize' = 256 KiB, so
-- the largest part this hub carries measures in 256 entries plus a handful of
-- nodes. This is a hundred times that, which leaves room for a tree written
-- with much smaller blocks and is still nothing next to what a bomb needs.
maxPartBlocks :: Int
maxPartBlocks = 32768

-- | Measure a part by walking its tree, without pulling the payload.
--
-- The size PEP-18's gate wants before it decides anything, and it is derived
-- rather than believed. A tree of these does not declare its size anywhere
-- (its metadata carries a file name and a mime type and nothing else), so the
-- alternative to walking would be a number the sender wrote, which is the one
-- thing a bound against a stranger must not be.
--
-- The walk reads the tree's NODES and asks the size of each leaf. A node is
-- small and a size is not a fetch, so this costs one call per block and no
-- payload: a part naming a hundred gigabytes is refused having transferred
-- none of them, which is what the gate is for.
--
-- Bounded by 'maxPartBlocks', which is what keeps "one call per block" a
-- statement about this reader rather than about the tree: how many blocks a
-- stranger's tree mentions is a number the stranger chooses.
measurePart :: MonadUnliftIO m => Ingress m -> HashRef -> m (Either PartTrouble PartFacts)
measurePart ig h = do
  root <- igBlock ig h
  case root of
    Nothing -> pure (Left PartNotFetchedYet)
    Just bs -> case tryDetect (coerce h) bs of
      MerkleAnn ann@MTreeAnn{ _mtaCrypt = EncryptGroupNaClSymm gkh _ } -> walk ann gkh
      MerkleAnn MTreeAnn{} -> pure (Left (PartNotATree "the tree is not encrypted"))
      _ -> pure (Left (PartNotATree "the root block is not a merkle tree"))
  where
    walk ann gkh = do
      -- ONE budget for the whole measurement, spent by the node reads inside
      -- the walk and by the 'igSize' calls the leaves below turn into. Charged
      -- for a leaf's entries when the leaf is entered, so a single node naming
      -- a hundred thousand blocks is refused before its list is held, not after.
      left <- newTVarIO maxPartBlocks
      let spend n = atomically do
            k <- readTVar left
            if k < n then pure False else True <$ writeTVar left (k - n)
      -- BOTH TREES, and the second one is the reason this takes two walks.
      --
      -- An encrypted tree names two things: its data and, in '_mtaCrypt', the
      -- GROUP KEY that opens it. The gate measured the first and 'igOpenPart'
      -- then read both, so a part could be one hundred-byte leaf, sail through
      -- 'maxPartBytes' as cheap, and name a key tree of any size at all --
      -- which the reader concatenates into memory, with the same duplicated-edge
      -- blow-up the data walk is bounded against. A bound that covers the half a
      -- caller has already decided to pay for is not a bound.
      dat <- measureTree spend (Right (_mtaTree ann))
      gkey <- measureTree spend (Left gkh) <&> \case
        -- A key block that is not here is the peer still downloading, and
        -- 'pfHere' is the field that says so. Only the DATA tree calls a missing
        -- block a failure to measure, because a size it cannot see is a size
        -- the gate cannot use; what the gate wants from the key is that it be
        -- bounded, and an absent one is bounded by definition.
        Left (PartUnreadable _) -> Right (PartFacts 0 False)
        other                   -> other
      pure ((<>) <$> dat <*> gkey)

    -- The walk's error is a PartTrouble and not a Text, so that the budget's
    -- refusal arrives as ITSELF. Told "cannot read the part's tree", an
    -- operator goes looking for a peer that is still downloading, and a tree
    -- this reader declined to walk is the opposite of that.
    --
    -- From a parsed tree or from a hash, because the two references are spelled
    -- differently: the data tree is already decoded inside the annotation, and
    -- the key is a hash of a block that has to be read to find out what it is.
    measureTree spend from = do
      let charge n = lift (lift (spend n))
                       >>= \ok -> unless ok (lift (throwError (PartTooManyBlocks maxPartBlocks)))
          look hx = charge 1 >> lift (lift (igBlock ig (HashRef hx)))
          sink = \case
            Left miss -> lift (throwError (PartUnreadable (Text.pack (show (pretty miss)))))
            Right hs  -> charge (length hs) >> S.each (hs :: [HashRef])
      leaves <- runExceptT $ S.toList_ $ case from of
        Right t -> walkMerkleTree t look sink
        -- A group key is written like any other blob, so it is a tree of the
        -- same shape; a walk that finds none is a block that is not one, and
        -- that is the annotation naming something that cannot be a key. Its own
        -- size still counts, since it is a block this node holds either way.
        Left  k -> walkMerkle @[HashRef] k look sink
      case leaves of
        Left e   -> pure (Left e)
        Right [] | Left k <- from -> do
          s <- igSize ig (HashRef k)
          pure (Right (PartFacts (maybe 0 fromIntegral s) (isJust s)))
        Right hs -> do
          sizes <- traverse (igSize ig) hs
          pure $ Right PartFacts
            { pfSize = fromIntegral (sum [ s | Just s <- sizes ])
            , pfHere = all isJust sizes
            }

-- | Whether a tree is small enough to read, decided before a byte of it moves.
data TreeWeight
  = TreeFits
  | TreeTooBig             -- ^ over the byte bound or the block bound
  | TreeUnreadable Text    -- ^ a block of it is not here, or it is not a tree
  deriving stock (Eq,Show)

-- | Weigh a tree without pulling it.
--
-- THE GATE TWO READERS ALREADY THOUGHT THEY HAD. A manifest and a mailbox
-- policy are both fetched from a merkle tree somebody else published, and both
-- carried a byte bound -- 'maxManifestBytes', 'maxPolicyBytes' -- that was an
-- argument to the PARSER. By the time the parser saw it the tree had been
-- concatenated into memory and copied strict, so the bound said what would be
-- parsed and nothing said what would be fetched. A manifest is reached by
-- @issue new@, @pr new@, @comment@ and @whoami --repo@ from a key the caller
-- typed; a policy is read per recipient on every send.
--
-- SIZES AND NOT CONTENT. A leaf's size is one 'hasBlock', which is a lookup and
-- not a transfer, so a tree naming a hundred gigabytes is refused having moved
-- none of them. The same shape 'measurePart' uses on the attachment path.
--
-- BOTH BOUNDS COME OUT OF ONE WALK, and the block budget is not decoration: how
-- many blocks a tree spreads itself over is as much a stranger's choice as how
-- many bytes it holds, and a million empty leaves weigh nothing and cost a
-- million lookups.
--
-- FUNCTIONS AND NOT A STORAGE, like everything else here that has to be
-- testable without a peer -- and it is what lets this live below the two
-- readers that need it rather than beside either.
--
-- An encrypted tree is weighed by its CIPHERTEXT, which is what a reader can
-- see before it holds a key, and the group key tree below the annotation is not
-- weighed. That is a residue and it is written down: reaching it needs a tree
-- sealed to a group this node is in, which is not the anonymous key that makes
-- the rest of this reachable. 'measurePart', whose input IS always encrypted,
-- weighs both.
weighTree :: forall m . MonadUnliftIO m
          => (Hash HbSync -> m (Maybe LBS.ByteString))  -- ^ a block, if it is here
          -> (Hash HbSync -> m (Maybe Integer))         -- ^ its size, without reading it
          -> Int                                        -- ^ the most bytes
          -> Int                                        -- ^ the most blocks
          -> HashRef
          -> m TreeWeight
weighTree look size maxBytes maxBlocks href = do
  root <- look (coerce href)
  case root of
    Nothing -> pure (TreeUnreadable (nameOf href <> " is not here yet"))
    Just bs -> case tryDetect (coerce href) bs of
      Merkle t                           -> weigh t
      MerkleAnn MTreeAnn{ _mtaTree = t } -> weigh t
      -- A blob, a link or a sequential ref. The readers below answer
      -- @UnsupportedFormat@ for all three, and saying so before the fetch
      -- costs nothing.
      _ -> pure (TreeUnreadable (nameOf href <> " does not name a tree"))

  where
    nameOf :: Pretty a => a -> Text
    nameOf = fromString . show . pretty

    weigh :: MTree [HashRef] -> m TreeWeight
    weigh t = do
      -- The root above is read already, which is what the budget starts at.
      blocks <- newTVarIO (1 :: Int)
      bytes  <- newTVarIO (0 :: Integer)
      let spend = do
            n <- atomically (modifyTVar' blocks (+1) >> readTVar blocks)
            when (n > maxBlocks) (throwError TreeTooBig)
          look' h = spend >> lift (look h)
          sink = \case
            -- A node of the tree that is not here. It cannot be weighed, so it
            -- is not read either: the reader below would fail on the same
            -- block, one full fetch later.
            Left miss -> throwError (TreeUnreadable ("block " <> nameOf miss
                                                       <> " of it is not here yet"))
            Right hs -> for_ hs $ \h -> do
              spend
              -- A leaf nothing has answered for weighs nothing here and fails
              -- the read below. This is a bound, not a completeness check.
              s <- lift (size (coerce h))
              n <- atomically (modifyTVar' bytes (+ fromMaybe 0 s) >> readTVar bytes)
              when (n > fromIntegral maxBytes) (throwError TreeTooBig)
      runExceptT (walkMerkleTree t look' sink) <&> either id (const TreeFits)

-- | Bound one call to the peer, and say which call it was if it does not answer.
--
-- Top-level rather than a @where@ binding, because this is the fix a whole
-- rewrite was about and nothing could reach it: the storage client's 'getBlock'
-- calls 'callService' raw, which blocks on a TQueue with no timeout at all.
--
-- ONE CALL, and the haddock says so because the first version of it did not:
-- this is 10 s per block, not 10 s for the walk. A peer answering every merkle
-- node just inside the bound still takes as long as the mailbox is big. What it
-- rules out is the wedge -- a peer that answers the mailbox service and then
-- stops -- which is the state that hung the verb forever.
--
-- HERE rather than beside the verb that first needed it, because the manifest
-- reader needs it too and lives below the CLI: a module that reads a peer's
-- answers cannot import the module that prints refusals.
bounded :: MonadUnliftIO m => Timeout 'Seconds -> String -> m a -> m a
bounded t what act =
  race (pause t) act >>= either (\() -> throwIO (PeerSilent what)) pure

-- | The peer is up and stopped answering.
--
-- Distinct from "no peer" (which fails before any verb runs) and from a usage
-- error, because the three call for different things: start it, fix the command,
-- and look at why it is stuck. It used to arrive as @user error (...)@ with exit
-- 1, under the ten GHC call stacks the socket layer logs on the way.
newtype PeerSilent = PeerSilent String

instance Show PeerSilent where
  show (PeerSilent what) =
    "the peer did not answer for " <> show what <> " within "
      <> show (pretty rpcTimeout) <> ". It is running, so this is not a"
      <> " connection problem: check `hbs2-peer poke` and the peer's log."

instance Exception PeerSilent
