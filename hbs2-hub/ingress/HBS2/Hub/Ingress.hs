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
-- Read-only on purpose, and that is worth stating rather than implying. The
-- accept path is irreversible twice over: an event minted into canon is in
-- every clone forever, and the retention rule (PEP-21) deletes the letter
-- afterwards, taking the only copy of the part secret with it. So the first
-- thing built here is the one that can be run against a live mailbox without
-- any of that being at stake.
module HBS2.Hub.Ingress
  ( Ingress(..)
  , OpenError(..)
  , LetterView(..)
  , InboxRead(..)
  , MailboxUnknown(..)
  , liveMessages
  , readInbox
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
import HBS2.Data.Types.SignedBox (unboxSignedBox0)
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Entry
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage.Operations.Class (OperationError)

import Codec.Serialise (deserialiseOrFail)
import Data.Coerce (coerce)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)

-- | What reading an inbox needs from the outside, gathered so the walk below
-- can be read without knowing how any of it is served.
--
-- A record rather than a class, following 'ReadMessageServices' next door: this
-- has exactly one implementation and the alternative is a class with one
-- instance, which is the same thing with more places to look.
data Ingress m = Ingress
  { igBlock    :: HashRef -> m (Maybe ByteString)
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
-- Deletion winning is therefore not a policy choice here, and it is safe for a
-- reason worth writing down: a @Deleted@ entry only enters the tree if its
-- payload box is signed by the mailbox's own key (@guard (MailboxRefKey pk ==
-- r)@ in the peer's merge). A stranger cannot hide a letter from triage by
-- sending a deletion for it.
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
      if root == seen && n < maxFetchRounds
        -- Unchanged across a round, and not the first look: as settled as a
        -- local reader can establish.
        then pure (root, True)
        else igPause ig fetchRound >> go (n - 1) root

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
      lvs <- mapM readOne (sort (HS.toList (liveMessages entries)))
      pure (InboxRead lvs misses settled)

  where
    readEntries tree = do
      acc <- newTVarIO []
      bad <- newTVarIO []
      walkMerkle @[HashRef] (coerce tree) (fmap (fmap LBS.fromStrict) . igBlock ig . HashRef) $ \case
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
                           . deserialiseOrFail @MailboxEntry . LBS.fromStrict)

    readOne mh = do
      blk <- igBlock ig mh
      case blk >>= either (const Nothing) Just
                   . deserialiseOrFail @(Message HBS2Basic) . LBS.fromStrict of
        Nothing  -> pure (LetterView mh Nothing (Left NotFetched) Nothing)
        Just msg -> do
          -- The envelope signer is recovered separately from the decryption, and
          -- that is the point. It is authenticated by this call alone, so on
          -- every path below except a bad signature the key is KNOWN, and on a
          -- public mailbox the bulk of the queue is letters sealed to somebody
          -- else. Taking the key from readMessage's success value left all of
          -- them anonymous, which is the queue with no key to block by.
          let envelope = fmap fst
                           (unboxSignedBox0 @(MessageContent HBS2Basic) (messageContent msg))

          -- Two exception types, not one, and this is where a stranger could
          -- close the queue. readMessage's own errors are ReadMessageError, but
          -- its last step decrypts, and that throws OperationError
          -- (DecryptionError for ciphertext that does not open, UnsupportedFormat
          -- for bytes that are not a payload). A catch naming only the first let
          -- the second escape readOne, mapM and readInbox, so one message with a
          -- resolvable group key and rubbish inside it printed no queue at all.
          -- Anyone can send that: recipients' encryption keys are public.
          opened <- tryOpen msg
          case opened of
            Left e -> pure (LetterView mh (envelopeFor e envelope) (Left e) Nothing)
            Right (_, _, payload) -> do
              let md = parsePayload payload
              pure LetterView
                { lvMessage  = mh
                , lvEnvelope = envelope
                , lvLetter   = case (envelope, md) of
                    (_, Left e)          -> Left (BadLetterHere e)
                    -- Unreachable: readMessage succeeding means the same unbox
                    -- succeeded. Answered rather than asserted, because the one
                    -- thing this loop must not do is throw.
                    (Nothing, _)         -> Left BadEnvelopeSig
                    (Just who, Right md') -> openWith who md'
                , lvEventId  = either (const Nothing) letterEventId md
                }

    -- A forged envelope names nobody: the signature is what would have
    -- established the key, and it did not.
    envelopeFor BadEnvelopeSig _ = Nothing
    envelopeFor _ k = k

    tryOpen msg =
      (Right <$> readMessage (igSecret ig) msg)
        `catch` (pure . Left . readErr)
        -- Everything the decrypt step can raise, which is a DIFFERENT type from
        -- the one above. Both of its cases mean the same thing to a reader (a key
        -- resolved and the bytes behind it are not a message), and neither is a
        -- reason to stop reading the mailbox.
        `catch` (\(_ :: OperationError) -> pure (Left Undecipherable))

    readErr = \case
      ReadSignCheckFailed  -> BadEnvelopeSig
      ReadNoGroupKey       -> GroupKeyByRef
      ReadNoGroupKeyAccess -> NotForUs

    openWith who md = do
      (_, author, content, _) <-
        either (Left . BadLetterHere) Right
          (openLetterAs (igAllowed ig) (EnvelopeSigner who) md)
      pure (author, content, classify content)
