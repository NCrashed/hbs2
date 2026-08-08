-- | The nonces this peer puts in its own @CheckMailbox@ requests, and the rule
-- that decides whether a @MailboxStatus@ is an answer to one of them.
--
-- What this replaces. @CheckMailbox@ has carried a @Maybe Word64@ since it was
-- written, the requester filled it with its own clock reading and the responder
-- threw it away and stamped the reply with ITS clock. Freshness was then a
-- comparison of two independent clocks inside a ten-second window, which is not
-- a proof that the status answers anything: it says only that somebody's clock
-- is close to ours, so a status recorded from an earlier exchange replays for as
-- long as that window lasts. It is also fragile in the ordinary case -- see
-- 'clockSkew', whose haddock records what a 'Word64' subtraction did to two
-- honest peers.
--
-- Echoing the requester's nonce turns the same two fields into a
-- challenge-response, with no change to the wire format. The requester compares
-- what came back against what it sent, and it never has to trust a clock but its
-- own.
--
-- What it is NOT. @CheckMailbox@ is gossiped, so every peer we are connected to
-- is handed the nonce as a matter of course, and any of them can echo it. The
-- rule here answers "is this a reply to something I asked recently"; it does not
-- answer "was this peer entitled to reply", which is what policy is for. The
-- value being unguessable therefore only shuts out somebody who did NOT receive
-- the request.
--
-- Nothing here touches the network, and both decisions are pure functions -- one
-- over a map, one over three arguments -- so they can be asked questions, which
-- the expression they replace, buried in a @ContT@ inside the wire handler,
-- could not be.
module HBS2.Peer.Proto.Mailbox.Nonce
  ( CheckNonces
  , checkNonceTTL
  , newCheckNonces
  , issueCheckNonce
  , acceptCheckNonce
  , insertCheckNonce
  , lookupCheckNonce
  , pruneCheckNonces
  , StatusUse(..)
  , StatusOrigin(..)
  , statusUse
  ) where

import HBS2.Prelude

import HBS2.Peer.Proto.Mailbox.Types (clockSkew)

import Crypto.Saltine.Core.Utils (randomByteString)
import Data.ByteString qualified as BS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Word
import UnliftIO

-- | Nonces issued for our own outgoing @CheckMailbox@ requests, each with the
-- time it was issued.
--
-- Keyed by the mailbox as well as the nonce. A @CheckMailbox@ is gossiped, so
-- every peer we are connected to sees the nonce; binding it to the mailbox it
-- was asked about means a peer cannot take a nonce it was shown and answer with
-- a status for a different mailbox.
newtype CheckNonces k = CheckNonces (TVar (HashMap (k, Word64) Word64))

-- | How long a nonce we issued stays acceptable, in seconds.
--
-- This bounds how long a LEGITIMATE answer may take to arrive, not how much
-- clock drift we tolerate, so it does not have to be the old ten-second window.
-- It should not be much larger either: a nonce we still accept is a window in
-- which a genuine status captured off the wire can be replayed at us, and under
-- the rule this replaces that window was ten seconds. Sixty is six times the
-- round trip the old code demanded and still far less than the ten minutes
-- @mailboxCheckQ@ waits before asking again (@600@, at the @set _2 600@ that
-- feeds its @polling@), so nothing accumulates between checks.
checkNonceTTL :: Word64
checkNonceTTL = 60

newCheckNonces :: (Eq k, Hashable k, MonadIO m) => m (CheckNonces k)
newCheckNonces = CheckNonces <$> newTVarIO HM.empty

-- | Draw a fresh nonce for a mailbox and remember it.
--
-- From libsodium's @randombytes_buf@, which is the generator the rest of this
-- project's nonces come from. NOT @System.Random@: that is splitmix, whose
-- @mix64@ is invertible, so two observed outputs give the whole sequence -- and
-- every value here is broadcast to every connected peer.
issueCheckNonce :: (Eq k, Hashable k, MonadIO m) => CheckNonces k -> k -> m Word64
issueCheckNonce (CheckNonces t) k = do
  now   <- liftIO $ getPOSIXTime <&> round
  nonce <- liftIO $ randomByteString 8 <&> BS.foldl' (\a w -> a * 256 + fromIntegral w) 0
  atomically $ modifyTVar' t (insertCheckNonce now k nonce)
  pure nonce

-- | Did we issue this nonce, for this mailbox, recently enough?
--
-- The nonce is NOT consumed. @CheckMailbox@ goes out by gossip and every peer
-- that holds the mailbox may answer it; taking the first answer and refusing the
-- rest would throw away the only reason to ask more than one peer.
acceptCheckNonce :: (Eq k, Hashable k, MonadIO m) => CheckNonces k -> k -> Word64 -> m Bool
acceptCheckNonce (CheckNonces t) k nonce = do
  now <- liftIO $ getPOSIXTime <&> round
  readTVarIO t <&> lookupCheckNonce now k nonce

-- | Record a nonce as issued, dropping whatever has expired.
--
-- Pruning on insert is what keeps the map bounded without a sweeper. Its cost is
-- a pass over the live nonces, so the map's size is also its insert cost: the
-- periodic check contributes at most one live nonce per mailbox we host, but the
-- other issuer is driven by local RPC and is not rate limited, so a caller that
-- asked for a fetch in a tight loop would pay for it quadratically. Nothing in
-- this tree does that, and the socket is local, which is why there is no cap
-- here rather than because none could be wanted.
insertCheckNonce :: (Eq k, Hashable k)
                 => Word64 -- ^ now
                 -> k
                 -> Word64 -- ^ nonce
                 -> HashMap (k, Word64) Word64
                 -> HashMap (k, Word64) Word64
insertCheckNonce now k nonce = HM.insert (k, nonce) now . pruneCheckNonces now

-- | Drop every nonce whose issue time is further from now than the TTL.
--
-- Distance rather than difference, for the reason 'clockSkew' exists, and it
-- also disposes of the case where the local clock jumps backwards: an entry
-- stamped in what is now the future expires like any other instead of living
-- until the clock catches up.
pruneCheckNonces :: Word64 -> HashMap (k, Word64) Word64 -> HashMap (k, Word64) Word64
pruneCheckNonces now = HM.filter (\issued -> clockSkew now issued < checkNonceTTL)

lookupCheckNonce :: (Eq k, Hashable k)
                 => Word64 -- ^ now
                 -> k
                 -> Word64 -- ^ nonce as it came back
                 -> HashMap (k, Word64) Word64
                 -> Bool
lookupCheckNonce now k nonce =
  maybe False (\issued -> clockSkew now issued < checkNonceTTL) . HM.lookup (k, nonce)

-- THE CLOCK WINDOW IS GONE, and so is the rule that used it.
--
-- What stood here: a status counted as fresh if the nonce came back OR its
-- timestamp was within ten seconds of the reader's clock. The second half was
-- transitional, written for a responder that does not echo, and it made the
-- first half decide nothing -- the timestamp is written by whoever sends the
-- status, so any peer with a working clock satisfied it. 'StatusUnasked' was
-- unreachable, 'useTree' was true for everybody, and the split shipped in
-- 9c3822af never applied once.
--
-- What that cost is in the accept path: a forged status makes the reader fetch
-- and MERGE a tree the announcer built, and the @Replicated@ branch of the
-- drain checks no work at all under @(pow 0)@, which is the default. For an
-- open inbox that is free writing into somebody else's mailbox, around the
-- queue and around the work an honest sender pays.
--
-- What it breaks: a peer will not take a tree from one that does not echo,
-- which is every released version -- the echo landed after 0.25.5.0. Deliberate
-- (owner, 2026-08-08): there is no compatibility promise across 0.25.x, and the
-- network being revived has no co-hosting to preserve. Policy still propagates,
-- since an unsolicited status carries it; what stops is tree sync, and the
-- accept path says so at 'warn'.
--
-- 'clockSkew' stays: the nonce map still expires by it.

-- | What a peer may act on from one status it has been sent.
--
-- A status carries two things and they are not equally trustworthy, which the
-- accept path used to miss by deciding both at once.
--
-- The POLICY inside it authenticates itself: it is a 'SetPolicyPayload' signed
-- by the mailbox key, naming its own mailbox, and admitted only at a strictly
-- greater version. Nothing a freshness check adds can make that safer, and
-- requiring freshness of it costs something real -- it is the only reason
-- @mailboxSetPolicy@'s unsolicited broadcast depended on the clock window at
-- all.
--
-- The TREE does not authenticate itself in any way: it is a hash somebody
-- announced, and acting on it means fetching and merging whatever is under it.
-- That is what the nonce is for, and what the poisoning note in the accept path
-- is about.
--
-- So they are separated: an answer moves both, and a status nobody asked for
-- moves only the policy.
data StatusUse = StatusUse
  { useTree   :: Bool
  , usePolicy :: Bool
  }
  deriving stock (Eq,Show)

-- | Where a status came from, as far as this peer can tell.
data StatusOrigin =
    -- | It echoed a nonce this peer issued for this mailbox -- or, while the
    -- transitional window lives, it is stamped with a plausible clock.
    StatusAnswered
    -- | Nobody asked for it: the owner's broadcast after a policy change, or a
    -- stranger announcing a tree.
  | StatusUnasked
  deriving stock (Eq,Show)

-- | The rule, in one place.
statusUse :: StatusOrigin -> StatusUse
statusUse = \case
  StatusAnswered -> StatusUse { useTree = True,  usePolicy = True }
  StatusUnasked  -> StatusUse { useTree = False, usePolicy = True }
