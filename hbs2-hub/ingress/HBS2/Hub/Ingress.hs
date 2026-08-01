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
  , openMessage
  , awaitMailbox
  , maxFetchRounds
  , fetchRound
  , rpcTimeout
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter

import HBS2.CLI.Prelude

import HBS2.Base58 (AsBase58(..))
import HBS2.Merkle (walkMerkle)
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Data.Types.SignedBox (unboxSignedBox0)
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Entry
import HBS2.Storage.Operations.Class (OperationError)

import Codec.Serialise (deserialiseOrFail)
import Data.Coerce (coerce)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)

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
  }
  deriving stock (Eq,Show)

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
readInbox :: MonadUnliftIO m => Ingress m -> HubKey -> m InboxRead
readInbox ig mbox = do
  (root, settled) <- awaitMailbox ig mbox
  case root of
    Nothing   -> pure (InboxRead [] [] settled)
    Just tree -> do
      (entries, misses) <- readEntries tree
      -- Sorted explicitly. This used to be HS.toList with a comment claiming it
      -- was sorted by hash: that is the HAMT traversal order, which is a
      -- property of the container's implementation and not a promise it makes.
      -- A triage queue that reorders itself on a dependency bump is one a
      -- maintainer cannot work through a page at a time.
      lvs <- mapM (openMessage ig) (sort (HS.toList (liveMessages entries)))
      pure (InboxRead lvs misses settled)

  where
    readEntries tree = do
      acc <- newTVarIO []
      bad <- newTVarIO []
      walkMerkle @[HashRef] (coerce tree) (igBlock ig . HashRef) $ \case
        Left miss -> atomically $ modifyTVar bad (HashRef miss :)
        Right hs -> do
          es <- forM hs $ \h -> (h,) <$> readEntry h
          -- A block that is here but does not decode as an entry counts as a
          -- miss too: the effect on the answer is the same as not having it, and
          -- the hash is equally the only thing to act on.
          atomically do
            modifyTVar acc ([ e | (_, Just e) <- es ] <>)
            modifyTVar bad ([ h | (h, Nothing) <- es ] <>)
      (,) <$> readTVarIO acc <*> (sort <$> readTVarIO bad)

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
openMessage ig mh = do
  blk <- igBlock ig mh
  case blk >>= either (const Nothing) Just . deserialiseOrFail @(Message HBS2Basic) of
    Nothing  -> pure (LetterView mh Nothing (Left NotFetched) Nothing)
    Just msg -> do
      -- The envelope signer is recovered separately from the decryption, and
      -- that is the point. It is authenticated by this call alone, so on every
      -- path below except a bad signature the key is KNOWN, and on a public
      -- mailbox the bulk of the queue is letters sealed to somebody else. Taking
      -- the key from readMessage's success value left all of them anonymous,
      -- which is a queue with no key to block by.
      let envelope = fmap fst
                       (unboxSignedBox0 @(MessageContent HBS2Basic) (messageContent msg))

      opened <- tryOpen msg
      case opened of
        Left e -> pure (LetterView mh (envelopeFor e envelope) (Left e) Nothing)
        Right (_, _, payload) -> do
          let md = parsePayload payload
          pure LetterView
            { lvMessage  = mh
            , lvEnvelope = envelope
            , lvLetter   = case (envelope, md) of
                (_, Left e)           -> Left (BadLetterHere e)
                -- Unreachable: readMessage succeeding means the same unbox
                -- succeeded. Answered rather than asserted, because the one
                -- thing this function must not do is throw.
                (Nothing, _)          -> Left BadEnvelopeSig
                (Just who, Right md') -> openWith who md'
            , lvEventId  = either (const Nothing) letterEventId md
            }

  where
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
