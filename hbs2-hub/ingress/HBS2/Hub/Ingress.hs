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
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Entry
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import Codec.Serialise (deserialiseOrFail)
import Data.Coerce (coerce)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (sortOn)
import Data.Maybe (catMaybes)

-- | What reading an inbox needs from the outside, gathered so the walk below
-- can be read without knowing how any of it is served.
--
-- A record rather than a class, following 'ReadMessageServices' next door: this
-- has exactly one implementation and the alternative is a class with one
-- instance, which is the same thing with more places to look.
data Ingress = Ingress
  { igStorage  :: AnyStorage
  , igMailbox  :: ServiceCaller MailboxAPI UNIX
    -- | Whether a letter from this key may be folded at all (PEP-21). Asked
    -- here as well as at accept time, because triage is a queue a human reads
    -- and a banned author's letter should not be in it.
  , igAllowed  :: HubKey -> Bool
    -- | Resolve the group secret for a message. Separate so a caller can
    -- supply a fake without a keyman running.
  , igSecret   :: ReadMessageServices HBS2Basic
  , igTimeout  :: Timeout 'Seconds
  }

-- | Why a message in the mailbox did not become a letter.
--
-- Four cases and not one, because they call for four different things from
-- whoever is reading the queue, and the difference is not recoverable later.
-- They were collapsed into a single "malformed payload", which said the wrong
-- thing about three of them: 'MalformedPayload' is a claim about DECRYPTED
-- bytes, and these are the ways of never reaching any.
data OpenError =
    -- | The message block is not in local storage yet. A wait, not a fault: the
    -- peer downloads mailbox contents on its own schedule and this is the
    -- ordinary state of a mailbox that was just fetched.
    NotFetched
    -- | The message decrypted to nothing this node holds a key for. Also
    -- ordinary: a mailbox accepts messages sealed to anybody, and one addressed
    -- to another maintainer is a line in the queue rather than a problem.
  | NotForUs
    -- | The envelope signature does not verify. This is the one that is an
    -- accusation, and the one somebody acts on ('hub block'), so it must not
    -- share a constructor with the two above.
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
  , irMissing :: Int          -- ^ tree blocks the walk could not read
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
awaitMailbox :: MonadUnliftIO m => Ingress -> HubKey -> m (Maybe HashRef, Bool)
awaitMailbox ig mbox = do
  -- The status call first, because it is the one that distinguishes a mailbox
  -- the peer does not have from one that is empty, and fetching a mailbox the
  -- peer does not have is a no-op it will not report.
  st <- callRpcWaitMay @RpcMailboxGetStatus (igTimeout ig) (igMailbox ig) mbox
          >>= orThrowUser "cannot reach the peer's mailbox service"

  case st of
    Left e         -> orThrowUser ("mailbox service:" <+> viaShow e) (Nothing @())
    Right Nothing  -> throwIO (MailboxUnknown mbox)
    Right (Just _) -> pure ()

  void $ callRpcWaitMay @RpcMailboxFetch (igTimeout ig) (igMailbox ig) mbox

  go maxFetchRounds Nothing

  where
    go 0 seen = pure (seen, False)
    go n seen = do
      root <- callRpcWaitMay @RpcMailboxGet (igTimeout ig) (igMailbox ig) mbox
                >>= orThrowUser "cannot reach the peer's mailbox service"
      if root == seen && n < maxFetchRounds
        -- Unchanged across a round, and not the first look: as settled as a
        -- local reader can establish.
        then pure (root, True)
        else pause fetchRound >> go (n - 1) root

-- | Fetch, walk and read a mailbox: every live message, opened as far as it
-- will open.
readInbox :: MonadUnliftIO m => Ingress -> HubKey -> m InboxRead
readInbox ig mbox = do
  (root, _settled) <- awaitMailbox ig mbox
  case root of
    Nothing   -> pure (InboxRead [] 0)
    Just tree -> do
      (entries, misses) <- readEntries tree
      -- Sorted explicitly. This used to be HS.toList with a comment claiming it
      -- was sorted by hash: that is the HAMT traversal order, which is a
      -- property of the container's implementation and not a promise it makes.
      -- A triage queue that reorders itself on a dependency bump is one a
      -- maintainer cannot work through a page at a time.
      lvs <- mapM readOne (sortOn (show . pretty) (HS.toList (liveMessages entries)))
      pure (InboxRead lvs misses)

  where
    sto = igStorage ig

    readEntries tree = do
      acc <- newTVarIO []
      bad <- newTVarIO (0 :: Int)
      walkMerkle @[HashRef] (coerce tree) (liftIO . getBlock sto) $ \case
        Left _   -> atomically $ modifyTVar bad succ
        Right hs -> do
          es <- forM hs $ \h ->
                  getBlock sto (coerce h)
                    <&> (>>= either (const Nothing) Just . deserialiseOrFail @MailboxEntry)
          -- A block that is here but does not decode as an entry counts as a
          -- miss too: the effect on the answer is the same as not having it.
          atomically do
            modifyTVar acc (catMaybes es <>)
            modifyTVar bad (+ length [ () | Nothing <- es ])
      (,) <$> readTVarIO acc <*> readTVarIO bad

    readOne mh = do
      blk <- getBlock sto (coerce mh)
      case blk >>= either (const Nothing) Just . deserialiseOrFail @(Message HBS2Basic) of
        Nothing  -> pure (LetterView mh Nothing (Left NotFetched) Nothing)
        Just msg -> do
          -- readMessage throws, and which exception it throws is the whole of
          -- what separates "not addressed to us" from "forged". Catching
          -- SomeException and reporting one thing threw that away.
          opened <- try @_ @ReadMessageError (readMessage (igSecret ig) msg)
          case opened of
            Left ReadSignCheckFailed  -> pure (LetterView mh Nothing (Left BadEnvelopeSig) Nothing)
            Left ReadNoGroupKey       -> pure (LetterView mh Nothing (Left NotForUs) Nothing)
            Left ReadNoGroupKeyAccess -> pure (LetterView mh Nothing (Left NotForUs) Nothing)
            Right (envelope, _, payload) -> do
              let md = parsePayload payload
              pure LetterView
                { lvMessage  = mh
                , lvEnvelope = Just envelope
                , lvLetter   = either (Left . BadLetterHere) Right (opened' envelope =<< md)
                , lvEventId  = either (const Nothing) letterEventId md
                }

    opened' envelope md = do
      (_, author, content, _) <- openLetterAs (igAllowed ig) (EnvelopeSigner envelope) md
      pure (author, content, classify content)
