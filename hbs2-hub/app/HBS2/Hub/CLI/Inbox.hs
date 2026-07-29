-- | Reading a hub's ingress mailbox (PEP-18 "Mailbox ingress", PEP-22 @hub
-- inbox@).
--
-- This is the seam between the Mailbox protocol and the pure library: it turns
-- a mailbox key into a list of verified letters, and nothing else. It does not
-- fold, does not mint, does not delete, and does not need canon. Everything it
-- decides comes out of 'HBS2.Hub.Letter'.
--
-- Read-only on purpose, and that is worth stating rather than implying. The
-- accept path is irreversible twice over: an event minted into canon is in
-- every clone forever, and the retention rule (PEP-21) deletes the letter
-- afterwards, taking the only copy of the part secret with it. So the first
-- thing built here is the one that can be run against a live mailbox without
-- any of that being at stake.
module HBS2.Hub.CLI.Inbox
  ( inboxEntries
  , Ingress(..)
  , LetterView(..)
  , liveMessages
  , readInbox
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter

import HBS2.CLI.Prelude hiding (mapMaybe)
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Merkle (walkMerkle)
import HBS2.Net.Auth.Credentials
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Entry
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,extractGroupKeySecret)

import Codec.Serialise (deserialiseOrFail)
import Data.Coerce (coerce)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe (mapMaybe)

-- | What reading an inbox needs from the outside, gathered so the walk below
-- can be read without knowing how any of it is served.
--
-- A record rather than a class, following 'ReadMessageServices' next door: this
-- has exactly one implementation and the alternative is a class with one
-- instance, which is the same thing with more places to look.
data Ingress m = Ingress
  { igStorage  :: AnyStorage
  , igMailbox  :: ServiceCaller MailboxAPI UNIX
    -- | Whether a letter from this key may be folded at all (PEP-21). Asked
    -- here as well as at accept time, because triage is a queue a human reads
    -- and a banned author's letter should not be in it.
  , igAllowed  :: HubKey -> Bool
    -- | Resolve the group secret for a message. Separate so a caller can
    -- supply a fake in a test without a keyman running.
  , igSecret   :: ReadMessageServices HBS2Basic
  , igTimeout  :: Timeout 'Seconds
  }

-- | One letter as triage sees it before anything is decided about it.
--
-- The message hash is first because it is the letter's identity everywhere
-- else: it is the @origin@ a folded event carries, the argument @hub inbox
-- show@ takes, and the handle the retention rule deletes by.
data LetterView = LetterView
  { lvMessage  :: HashRef            -- ^ the Mailbox message this came in
  -- | Who signed the envelope, when the message opened far enough to say.
  -- Absent rather than a placeholder: a mailbox takes messages sealed to
  -- anybody, so "this maintainer holds no key for it" is an ordinary line in
  -- the queue and there is no key to name on it.
  , lvEnvelope :: Maybe HubKey
    -- | What the letter turned out to be, or why it could not be read. A
    -- 'Left' here is not an error to stop on: a mailbox is public, anybody can
    -- send anything to it, and a queue that stops at the first unreadable
    -- message is a queue one stranger can close.
  , lvLetter   :: Either LetterError (HubKey, AuthorContent, Disposition)
  }

-- | The messages a mailbox currently holds.
--
-- A mailbox tree is an append-only log of 'Exists' and 'Deleted' entries for
-- the same message hash, so "what is in the mailbox" is a difference of two
-- sets rather than a list. Deletion wins whenever both appear, regardless of
-- the order the tree is walked: the entries carry timestamps, but a message
-- deleted and then re-sent is a new letter to triage, not a resurrection of
-- the old one, and treating it as one would let a sender un-delete their own
-- letter by racing the walk.
liveMessages :: [MailboxEntry] -> HashSet HashRef
liveMessages es = HS.difference exists deleted
  where
    exists  = HS.fromList [ h | Exists _ h  <- es ]
    deleted = HS.fromList [ h | Deleted _ h <- es ]

-- | Fetch, walk and read a mailbox: every live message, opened as far as it
-- will open.
--
-- The fetch first, because a mailbox key with nothing behind it locally reads
-- as an empty inbox, which is indistinguishable from a mailbox with no
-- submissions and is the wrong answer to give a maintainer.
readInbox :: MonadUnliftIO m => Ingress m -> HubKey -> m [LetterView]
readInbox ig mbox = do
  void $ callRpcWaitMay @RpcMailboxFetch (igTimeout ig) (igMailbox ig) mbox

  root <- callRpcWaitMay @RpcMailboxGet (igTimeout ig) (igMailbox ig) mbox
            >>= orThrowUser "cannot reach the peer's mailbox service"

  case root of
    Nothing -> pure []
    Just tree -> do
      entries <- readEntries tree
      let live = liveMessages entries
      -- Sorted by hash, which is arbitrary but stable: a triage queue that
      -- reorders itself between two runs over the same mailbox is one a
      -- maintainer cannot work through a page at a time.
      mapM readOne (HS.toList live)

  where
    sto = igStorage ig

    readEntries tree = do
      acc <- newTVarIO []
      walkMerkle @[HashRef] (coerce tree) (liftIO . getBlock sto) $ \case
        Left miss -> err ("missed block in the mailbox tree:" <+> pretty miss)
        Right hs  -> do
          es <- forM hs $ \h ->
                  getBlock sto (coerce h)
                    <&> (>>= either (const Nothing) Just . deserialiseOrFail @MailboxEntry)
          atomically $ modifyTVar acc (mapMaybe id es <>)
      readTVarIO acc

    readOne mh = do
      blk <- getBlock sto (coerce mh)
      case blk >>= either (const Nothing) Just . deserialiseOrFail @(Message HBS2Basic) of
        Nothing -> pure (LetterView mh Nothing (Left MalformedPayload))
        Just msg -> do
          -- readMessage throws when the group key is not ours, which for an
          -- inbox is ordinary rather than exceptional: a mailbox accepts
          -- messages sealed to anybody, and one this maintainer cannot open is
          -- a line in the queue, not a reason to stop reading the queue.
          opened <- try @_ @SomeException (readMessage (igSecret ig) msg)
          case opened of
            Left _ -> pure (LetterView mh Nothing (Left MalformedPayload))
            Right (envelope, _, payload) ->
              pure (LetterView mh (Just envelope) (openOne envelope payload))

    openOne envelope payload = do
      md <- parsePayload payload
      (_, author, content, _) <- openLetterAs (igAllowed ig) (EnvelopeSigner envelope) md
      pure (author, content, classify content)

-- | @hub inbox@ and friends.
inboxEntries :: forall c m . ( IsContext c
                             , MonadUnliftIO m
                             , HasStorage m
                             , HasClientAPI MailboxAPI UNIX m
                             , Exception (BadFormException c)
                             ) => MakeDictM c m ()
inboxEntries = do

  brief "list the letters waiting in a hub's ingress mailbox"
    $ args [arg "string" "mailbox-key"]
    $ desc ( "Read-only. Fetches the mailbox, opens every message this"
             <> line <> "maintainer holds a key for, and reports what each one"
             <> line <> "asks for. Nothing is folded, minted or deleted." )
    $ entry $ bindMatch "hub:inbox" $ nil_ \case
        [ SignPubKeyLike mbox ] -> lift do
          sto <- getStorage
          api <- getClientAPI @MailboxAPI @UNIX
          let ig = Ingress { igStorage = sto
                           , igMailbox = api
                             -- No deny-list wired yet: PEP-21 policy lives in
                             -- the repo manifest, which this verb does not read
                             -- because it takes a mailbox key rather than a
                             -- repo. Allowing everything here is honest, since
                             -- listing is not accepting; the accept path must
                             -- not inherit this default.
                           , igAllowed = const True
                           , igSecret = ReadMessageServices
                               (liftIO . runKeymanClientRO . extractGroupKeySecret)
                           , igTimeout = TimeoutSec 10
                           }
          readInbox ig mbox >>= liftIO . mapM_ (print . render)

        _ -> throwIO (BadFormException @c nil)

-- One line per letter, in the order the fields matter to somebody deciding
-- what to do with it.
render :: LetterView -> Doc ann
render lv = pretty (lvMessage lv) <+> body
  where
    body = case lvLetter lv of
      Left e -> "unreadable:" <+> pretty e
      Right (author, content, disp) ->
        maybe "?" (pretty . AsBase58) (lvEnvelope lv)
          <+> "author" <+> pretty (AsBase58 author)
          <+> opOf content
          <+> maybe "-" (\t -> "on" <+> pretty t) (authorThread content)
          <+> dispOf disp

    opOf = \case
      AOpen{} -> "open"; AComment{} -> "comment"; ARevise{} -> "revise"
      ASet{} -> "set"; AClose{} -> "close"; AReopen{} -> "reopen"
      AMerge{} -> "merge"; ARedact{} -> "redact"
      ADelegate{} -> "delegate"; ARevoke{} -> "revoke"

    dispOf = \case
      FoldsToCanon -> "(folds)"
      RequestOnly  -> "(request)"
      OwnerNative  -> "(owner-only: not acceptable from a letter)"
