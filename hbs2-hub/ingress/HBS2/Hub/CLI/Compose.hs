-- | Composing and sending a Tier B letter (PEP-18, PEP-22 "Contribute").
--
-- The mirror of "HBS2.Hub.CLI.Inbox": that module opens letters, this one seals
-- them. Both are thin, and deliberately so. What a letter IS lives in
-- 'HBS2.Hub.Letter'; what this adds is a signed envelope, encryption to the
-- maintainers, and a peer to hand it to.
--
-- The sender signs the inner box with their own key, and that signature is what
-- canon publishes verbatim if the letter is ever folded. So the key named here
-- is not a transport detail: it is the authorship claim, permanently, and it is
-- taken as an explicit argument rather than defaulted from whatever the keyman
-- happens to hold first.
module HBS2.Hub.CLI.Compose
  ( composeEntries
  , Outbound(..)
  , sendLetter
  , sendLetterWith
  , sendPayload
  , stampsFor
  , checkReplyChannel
  , attachToLetter
  , issueUsage
  , issueArgs
  , readBody
  , letterBody
  , codeNoKey
  , NotStored(..)
  , codeNotStored
  , PoWTooHard(..)
  , codeNoWork
  , codeWrongSigil
  , codeUnsendable
  , unsendable
  , queuedNext
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Ingress (rpcTimeout,PeerSilent(..))
import HBS2.Hub.Sent (Sent(..),recordSent)
import HBS2.Hub.CLI.Argv (badArgs,flagsOf,flagOnce,flagEvery,flagMaybe,repoFlags,flagRepo)
import HBS2.Hub.CLI.Common (refuse,codePeerSilent,saying,manifestCode,signerFor,signingPair
                           ,askingKeyman)
import HBS2.Hub.Repo.Manifest (sigilFor)
import HBS2.Hub.CLI.Policy (readPolicyWith,PolicyReader,PolicyGone(..))

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.SignedBox (SignedBox,unboxSignedBox0)
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil (Sigil,loadSigil)
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Policy (policyPoW)
import HBS2.Peer.Proto.Mailbox.PoW (solveStamp)
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,loadKeyRingEntry)

import Crypto.Saltine.Class qualified as Saltine

import Data.ByteString.Lazy qualified as LBS
import Data.Char (isSpace)
import Data.Maybe (catMaybes,fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import System.Exit (die)

-- | What sending a letter needs from the outside.
data Outbound = Outbound
  { obStorage :: AnyStorage
  , obMailbox :: ServiceCaller MailboxAPI UNIX
  , obTimeout :: Timeout 'Seconds
  }

-- | Seal a letter to its recipients and hand it to the peer.
--
-- The recipients are given as sigils rather than as keys, because a sigil is
-- what carries the ENCRYPTION key: a mailbox is addressed by a sign key, and
-- there is no service that resolves one to the other, which is the whole reason
-- PEP-18 puts the sigil hash in the repo manifest. Resolving it from there is
-- the next verb up; this one takes it directly, so the primitive is testable
-- before the manifest lookup exists.
--
-- Which mailbox it lands in is therefore not a separate argument. A sigil binds
-- one sign key to one encryption key, and 'createMessage' puts that sign key in
-- the recipient set, so the sigil already says where the letter goes. Taking the
-- mailbox key as well would be taking a value that can disagree with the sigil,
-- and the sigil is the one that would win.
sendLetter
  :: MonadUnliftIO m
  => Outbound
  -> HashRef                  -- ^ sender sigil
  -> [HashRef]                -- ^ recipient sigils: every maintainer reading the inbox
  -> SignedBox AuthorContent HubScheme  -- ^ the inner box, already signed by the author
  -> ReplyChannel
  -> m HashRef
sendLetter ob sender rcpts = sendLetterWith ob sender rcpts []

-- | Store a letter's attachments, and say what they are called.
--
-- The first half of sending a letter that references its own attachment, and
-- it has to be a separate call for the reason the peer's 'createAttachments'
-- does: a part is named by the hash of its encrypted tree, PEP-18 puts that
-- hash inside the signed inner box (@body-part@, @bundle-part@), so the tree
-- exists before the box that names it is signed.
--
-- The answer is in the order the parts were given, and each part comes back
-- WITH ITS PROOF: naming a part is claiming it, and a claim a maintainer will
-- act on by publishing the group secret has to be one only its owner can make
-- (PEP-18 'PartRef'). The proof is over the author key, so it is made here,
-- where the key that will sign the box is known, rather than left to a caller
-- to remember.
attachToLetter
  :: MonadUnliftIO m
  => Outbound
  -> HubKey                   -- ^ the key that will sign the letter
  -> HashRef                  -- ^ sender sigil
  -> [HashRef]                -- ^ recipient sigils
  -> [([(Text, Text)], m LBS.ByteString)]
  -> m [PartRef]
attachToLetter ob author sender rcpts parts = do
  (hs, sec) <- createAttachments (services ob) (Left sender) (fmap Left rcpts) parts
  -- A generated key is the right length or libsodium is broken, and the
  -- alternative to checking is signing a proof over a truncated secret that
  -- nothing on the reading side can ever match.
  psec <- mkPartSecret (Saltine.encode sec)
            & maybe (liftIO (die "the attachment key is not a key")) pure
  pure [ PartRef h (partProofFor h psec author) | h <- hs ]

-- | Seal a letter around parts that already exist, and hand it to the peer.
sendLetterWith
  :: MonadUnliftIO m
  => Outbound
  -> HashRef
  -> [HashRef]
  -> [HashRef]                -- ^ parts, from 'attachToLetter'
  -> SignedBox AuthorContent HubScheme
  -> ReplyChannel
  -> m HashRef
sendLetterWith ob sender rcpts parts box reply = do
  checkReplyChannel (loadSigil @HubScheme (obStorage ob)) reply
  sendPayload ob (Left sender) rcpts parts
    (MessageData hubMsgWrite (Letter box reply))

-- | Refuse a letter whose answer could never reach whoever sent it.
--
-- The reply channel is an author key and a sigil, and a sigil that names
-- SOMEBODY ELSE's key is the one combination that fails invisibly: the letter
-- is well-formed, the maintainer folds it, and the hub then declines to ack
-- because it will not seal a maintainer's word to a key the sender does not
-- hold ('ackTarget'). Every one of those steps is correct and none of them
-- happens where the person who can fix it is standing.
--
-- So the same rule is asked here, at the last moment it is cheap: the sender's
-- own machine, before anything is signed, sent or charged for. It is one block
-- read, and the sigil is about to be read anyway to seal the message.
--
-- HERE AND NOT IN THE FOUR VERBS. `issue new`, `pr new`, `pr revise` and the
-- comment verbs all arrive through this function, and a check per verb is four
-- chances for the fifth verb to skip it.
--
-- 'Nothing' PROCEEDS, matching 'ackTarget' on the other side: a sigil this node
-- cannot read is not evidence of anything, and the send fails on its own if the
-- bytes are really missing, which is a better answer than an accusation.
--
-- TAKES THE SIGIL READER for the reason 'stampsFor' takes the policy one: what
-- this decides is a refusal a contributor meets, and with the storage inlined
-- it could only be reached with a peer.
checkReplyChannel :: MonadUnliftIO m
                  => (HashRef -> m (Maybe (Sigil HubScheme))) -> ReplyChannel -> m ()
checkReplyChannel _ NoReply = pure ()
checkReplyChannel sigilAt (ReplyTo k href) = do
  named <- sigilNames k <$> sigilAt href
  when (named == Just False) $ liftIO $ refuse
    ( show ( "the sender sigil does not name the author key" <> line
               <> "  --author" <+> pretty (AsBase58 k) <> line
               <> "  --sender" <+> pretty href <> line
               <> "  That pair is what an acknowledgement is sealed to, so this"
               <+> "letter would be" <> line
               <> "  folded and never answered. `hub whoami --author <key>"
               <+> "--sender <hash>`" <> line <> "  says which of the two is"
               <+> "the odd one." <> line
               <> "  Nothing was sent." ) )
    codeWrongSigil

-- | What a letter whose reply channel cannot work exits with.
--
-- Its own code, because it is the one refusal on this path that is neither the
-- peer's fault nor the letter's content: two arguments that are each valid and
-- do not go together.
codeWrongSigil :: Int
codeWrongSigil = 47

-- | Seal any hub payload and hand it to the peer.
--
-- Split out of 'sendLetterWith' when the second kind of payload appeared: an
-- acknowledgement is not a letter (no inner box, never folded, PEP-18), and
-- everything below the payload -- the flags, the group key, storing the bytes
-- before sending, the stamps, the four ways it can fail -- is the same for
-- both. Two copies of that would be two answers to what a hub asks a peer for.
--
-- The sender may be a sigil that already exists (a contributor's, published
-- and fetchable) or one made on the spot from credentials, which is what a hub
-- acking a letter does: it has the canon key and no reason to have published a
-- sigil for it.
sendPayload
  :: MonadUnliftIO m
  => Outbound
  -> Either HashRef (Sigil HBS2Basic)   -- ^ sender
  -> [HashRef]                          -- ^ recipient sigils
  -> [HashRef]                          -- ^ parts, from 'attachToLetter'
  -> MessageData
  -> m HashRef
sendPayload ob sender rcpts parts payload = do
  flags <- defMessageFlags

  -- CAUGHT, because it used to leave through the RTS: 'CreateMessageError' is
  -- an exception, nothing on this path handled it, and a sigil this node has
  -- not fetched came out as a derived Show at exit 1.
  msg <- createMessageWith (services ob) flags Nothing sender (fmap Left rcpts)
           parts
           (letterPayload payload)
           `catch` \(e :: CreateMessageError) ->
             liftIO (refuse (show (unsendable e)) (codeFor' e))

  -- Stored before it is sent, and this is not belt-and-braces. The peer takes a
  -- Message by value, but everything downstream of it refers to the letter by
  -- the hash of these bytes: the inbox lists that hash, a folded event carries
  -- it as its origin, and retention deletes by it. A sender that cannot name
  -- what it sent cannot correlate the acknowledgement it gets back.
  h <- putBlock (obStorage ob) (serialise msg)
         >>= maybe (throwIO (NotStored "the peer would not store the message")) pure

  -- One send per stamp, and a plain one when nothing is charged.
  --
  -- A letter names several recipients and a stamp is work for ONE mailbox
  -- (PEP-21), so a mailbox that charges can only be paid by a copy that names
  -- it. The copies are the same bytes -- the stamp rides beside the message,
  -- not inside it -- so what multiplies is the gossip, not the storage, and a
  -- mailbox that has already taken one copy drops the rest as merged.
  --
  -- THAT IS ALSO WHY THE PEER'S QUEUE KEYS ITS DEDUP ON RECIPIENTS. The copies
  -- hash alike, so a queue that remembered only "a copy of this letter is
  -- already in flight, and it pays" kept whichever stamp arrived first and
  -- dropped the rest -- and a letter to two charging mailboxes reached one of
  -- them. Nothing hostile was involved: it is what this loop sends.
  floorD <- relayFloor ob
  stamps <- stampsFor floorD (readPolicyWith (obStorage ob) (obMailbox ob)) msg

  -- Checked, not voided. callRpcWaitMay answers Nothing on a timeout, and
  -- discarding that printed a message hash and left with a zero exit having sent
  -- nothing at all. The read side spent a whole round being taught not to report
  -- silence as an answer; on the write side the same mistake is worse, because
  -- the caller believes a letter is in a mailbox.
  --
  -- What this establishes and NOTHING MORE: the RPC round-tripped, and the peer
  -- did not refuse. @Output RpcMailboxSend@ carries an
  -- @Either MailboxServiceError ()@ now -- it was @()@, and the handler was
  -- @void $ mailboxSendMessage@, so every refusal read as delivery -- but the
  -- service function behind it STILL answers @Right ()@ unconditionally: it
  -- fires the protocol inside @deferred@ and returns. So what can be reported
  -- here are the refusals the peer reaches before that point, and delivery is
  -- not among them. That is why the verb answers @queued@ and not @sent@, and
  -- the word is still the honest one.
  --
  -- Typed exceptions rather than 'orThrowUser', and the difference is the exit
  -- code. `orThrowUser` leaves through the RTS with 1, which PEP-22 gives to
  -- usage errors, so "you mistyped a flag" and "your letter is in nobody's
  -- mailbox" arrived as one number -- the exact confusion codes 17 and 18 were
  -- added to end on the read side, and worse here, because the caller stops
  -- watching for a letter that was never sent.
  for_ (case stamps of { [] -> [Nothing] ; ss -> fmap Just ss }) $ \stamp ->
    callRpcWaitMay @RpcMailboxSend (obTimeout ob) (obMailbox ob) (stamp, msg)
      >>= maybe (throwIO (PeerSilent "the mailbox send")) pure
      >>= either (throwIO . NotStored . show
                    . ("the peer refused the message:" <+>) . viaShow)
                 pure

  pure (HashRef h)

-- | What the sender's own peer says a letter must pay to leave (PEP-23 step D).
--
-- SILENCE IS ZERO, deliberately. A peer older than this method answers
-- 'ErrorMethodNotFound', which arrives here as 'Nothing', and so does a peer
-- that is not answering at all. Both mean "no evidence of a price", and reading
-- them as a price would either refuse to send or grind work nobody asked for.
-- The send that follows is then exactly the send this tool made before the
-- method existed, which is the behaviour to fall back to.
--
-- Asked once per letter and not once per recipient: the number is a property of
-- the road out of this machine, not of who the letter is addressed to.
relayFloor :: MonadUnliftIO m => Outbound -> m PoWDifficulty
relayFloor ob =
  callRpcWaitMay @RpcMailboxPoWFloor (obTimeout ob) (obMailbox ob) ()
    <&> fromMaybe 0

-- | Solve what the recipients charge, and what the road out charges.
--
-- WHAT IT CAN AND CANNOT SEE. The difficulty lives in the mailbox's own signed
-- policy, and this peer has that policy only for mailboxes it holds. Writing to
-- somebody else's hub is exactly the case where it holds none, and then this
-- answers "nothing is charged" -- which is right in the only sense available
-- (it has no evidence of a charge) and wrong in the one that matters (the hub
-- may drop the letter for want of work, in silence). Sending to a mailbox that
-- charges therefore wants the peer to hold it first, and PEP-21 says plainly
-- that the sender gets no signal either way.
--
-- A policy that will not read is NOT the same as one that charges nothing, and
-- it says so on stderr: the letter still goes, because refusing to send over a
-- broken policy file on somebody else's peer would be a worse failure than
-- sending work-free into a mailbox that may want work.
--
-- THE RELAY FLOOR IS THE SECOND PRICE (PEP-23 step D), and it answers a
-- different question from the first: what a mailbox charges decides whether the
-- letter is STORED, what a peer charges decides whether it is CARRIED. The
-- floor comes from 'RpcMailboxPoWFloor' and is the sender's own peer plus the
-- neighbours it can see; nothing further out is knowable. Two consequences,
-- both of them the point:
--
--   * a mailbox that charges nothing can still need a stamp, because somebody
--     on the way will not carry a letter that pays nothing. That is the case
--     this function used to answer with no stamp at all;
--   * a mailbox that charges less than the floor gets the floor, since a letter
--     that satisfies its destination and never arrives is not delivered.
--
-- At the default floor of zero this is exactly what it was: no letter is
-- stamped that would not have been, and none is stamped harder.
--
-- TAKES THE POLICY READER AND THE FLOOR, and does not take an 'Outbound'.
-- Everything else here needs 'createMessage', which needs a storage and a
-- keyman, so no test can reach it; this one needs the ANSWERS, and with the
-- handles inlined the whole proof-of-work path could only be run against a
-- peer. Skipping every grind left the suite green, and what that costs is
-- letters dropped for want of work with no signal to the sender.
stampsFor :: MonadUnliftIO m
          => PoWDifficulty       -- ^ what the road out of this machine charges
          -> PolicyReader m
          -> Message HubScheme
          -> m [MessageStamp HubScheme]
stampsFor floorD policyFor msg = do

  -- From the message rather than from the sigils it was built out of: a stamp
  -- is checked against `messageRecipients`, so what it must name is what ended
  -- up in there.
  let rcpts = Set.toList $ maybe mempty (messageRecipients . snd)
                             (unboxSignedBox0 (messageContent msg))

  charging <- fmap catMaybes $ for rcpts $ \mbox ->
    policyFor mbox >>= \case
      Left (PolicyNotHere _) -> pure Nothing
      Left e -> do
        liftIO $ saying ( "hbs2-hub: cannot tell what"
                            <+> pretty (AsBase58 mbox) <+> "charges:"
                            <+> pretty e <> line
                            <> "  sending without proof-of-work" <> line )
        pure Nothing
      -- A mailbox with no policy at all charges nothing, and it is not the same
      -- reading as the deny/deny fallback: the fallback is what the peer admits
      -- a message under, and its pow is zero either way, so the answer here is
      -- the same and the reason for it is not.
      Right Nothing -> pure Nothing
      Right (Just (_, p)) -> do
        d <- policyPoW @HBS2Basic p
        pure $ if d == 0 then Nothing else Just (mbox, max d floorD)

  case charging of
    -- ONE STAMP FOR THE ROAD, not one per recipient, and this is why the two
    -- cases are not written as one. A stamp buys two different things: a
    -- mailbox's policy is satisfied only by a stamp NAMING that mailbox, so a
    -- charging mailbox needs its own copy; but a relay only asks that the
    -- packet carry enough work for SOME recipient of it, so one copy carries
    -- the letter to every host that charges nothing. Solving per recipient
    -- there would multiply the gossip by the recipient count and buy nothing.
    [] | floorD > 0, (mbox:_) <- rcpts -> do
           liftIO $ saying ( "hbs2-hub: solving" <+> pretty floorD
                               <+> "bits of work to leave this peer" <> line )
           pure <$> solveWithin powBudget floorD mbox msg
       | otherwise -> pure []

    _ -> for charging $ \(mbox, d) -> do
           liftIO $ saying ( "hbs2-hub: solving" <+> pretty d <+> "bits of work for"
                               <+> pretty (AsBase58 mbox) <> line )
           solveWithin powBudget d mbox msg

-- | How long a grind is allowed to take before it is called impossible.
--
-- A bound and not a difficulty limit, because what a difficulty costs is not
-- knowable from the number alone: it is the sender's machine, and the search is
-- probabilistic, so the same D takes a different time twice. Time is the thing
-- the person waiting actually has an opinion about.
powBudget :: Timeout 'Seconds
powBudget = 300

-- | Grind, or give up saying so.
--
-- 'solveStamp' is pure and unbounded by design (it cannot know what the caller
-- will sit through), so the bound belongs here. Forcing to WHNF is the whole
-- search: the constructor does not exist until a nonce has been found.
solveWithin :: MonadUnliftIO m
            => Timeout 'Seconds
            -> PoWDifficulty
            -> HubKey
            -> Message HubScheme
            -> m (MessageStamp HubScheme)
solveWithin t d mbox msg =
  race (pause t) (liftIO (evaluate (solveStamp d mbox msg)))
    >>= either (\() -> throwIO (PoWTooHard (show ( pretty d <+> "bits for"
                                                     <+> pretty (AsBase58 mbox)))))
               pure

-- Where the message builder gets its keys. One definition, because the two
-- halves of sending must resolve the same credentials: the parts key and the
-- message key are wrapped for one set of recipients.
services :: Outbound -> CreateMessageServices HubScheme
services ob = CreateMessageServices
  (obStorage ob)
  signerFor
  -- Through 'askingKeyman' like the signer above it: a machine with no key
  -- database answers neither, and the sentence is the same one.
  (askingKeyman . loadKeyRingEntry)

-- | The peer answered and would not take the message.
--
-- Distinct from 'PeerSilent': the peer is answering, so the remedy is what it
-- said rather than whether it is alive. Distinct from a usage error for the
-- reason every code here is: nothing the caller could retype changes it.
newtype NotStored = NotStored String

instance Show NotStored where
  show (NotStored what) =
    what <> ". The letter is in no mailbox: check the peer's log."

instance Exception NotStored

-- | No signing key for the author this letter claims.
--
-- Above 'codePeerSilent', and added rather than folded into 1, because a hook
-- that cannot tell "this key is not in the keyman here" from "you mistyped a
-- flag" cannot act on either.
codeNoKey :: Int
codeNoKey = 19

-- | And what a peer that would not store the message exits with.
codeNotStored :: Int
codeNotStored = 20

-- | The letter was not composed, so nothing was sent.
--
-- ONE CODE FOR FIVE WAYS, for the reason 'HBS2.Hub.CLI.Common.oneStop' gives:
-- they all mean the same thing to whoever ran the verb -- no letter exists, no
-- mailbox has anything, and the repository is untouched. The five sentences are
-- in 'unsendable' and they do differ; the two that ARE about a missing key keep
-- 'codeNoKey', because that one has a hook-visible remedy of its own.
codeUnsendable :: Int
codeUnsendable = 52

-- | Which code a 'CreateMessageError' leaves with.
codeFor' :: CreateMessageError -> Int
codeFor' = \case
  NoCredentialsFound{} -> codeNoKey
  NoKeyringFound{}     -> codeNoKey
  _                    -> codeUnsendable

-- | And what it says, which was a derived 'Show' escaping through the RTS.
--
-- @hbs2-hub issue new@ with a sigil this node has not fetched printed
-- @SigilNotFound 5Kd3...@ and exited 1 -- the code PEP-22 gives to a mistyped
-- flag -- on a contributor's first command. Every one of these is a real state
-- with a real remedy, and none of the remedies is "check your arguments".
unsendable :: CreateMessageError -> Doc ann
unsendable = \case
  SigilNotFound h ->
    "this node does not hold the sigil" <+> pretty h
      <> line <> "  nothing was sent. A sigil is a block like any other:"
      <+> "`hbs2-peer download" <+> pretty h <> "`"
      <> line <> "  fetches it, and `hbs2-hub whoami --sender" <+> pretty h
      <+> "` says what it names."
  MalformedSigil h ->
    "that block is here and is not a sigil" <> maybe mempty ((":" <+>) . pretty) h
      <> line <> "  nothing was sent. A hash that names something else parses"
      <+> "as far as this and no further."
  SenderNoAccesToGroupKey ->
    "the sender cannot open the group key this letter would be sealed with"
      <> line <> "  nothing was sent, and a letter sent anyway would be one"
      <+> "the sender could not read back."
  NoCredentialsFound s ->
    "no signing key here for" <+> pretty s
      <> line <> "  nothing was sent. `hbs2-hub whoami` lists what this machine"
      <+> "can sign as."
  NoKeyringFound s ->
    "no keyring here for" <+> pretty s
      <> line <> "  nothing was sent. The key is known and the file that holds"
      <+> "it is not where keyman says."
  -- The two the CLI cannot produce: every verb here fills both in before it
  -- gets this far. Said plainly rather than dressed up as a user error,
  -- because if one is ever seen it is this tool's bug and not the caller's.
  SenderNotSet ->
    "no sender was set on the letter, which is this tool's bug: nothing was sent"
  RecipientsNotSet ->
    "no recipient was set on the letter, which is this tool's bug:"
      <+> "nothing was sent"

-- | The mailbox charges more work than this run would spend.
--
-- Its own exception rather than a 'NotStored', because no peer was involved:
-- nothing was sent, nothing was refused, and "check the peer's log" would send
-- somebody to read about an event that never reached a peer. The remedy is on
-- this side -- wait longer, or write to a mailbox that charges less.
newtype PoWTooHard = PoWTooHard String

instance Show PoWTooHard where
  show (PoWTooHard what) =
    "the proof-of-work this mailbox charges did not come out in the time"
      <> " allowed (" <> what <> "). Nothing was sent."

instance Exception PoWTooHard

-- | And what giving up on the work exits with.
codeNoWork :: Int
codeNoWork = 37

-- | @hub issue new@ and the other compose verbs.
composeEntries :: forall c m . ( IsContext c
                               , MonadUnliftIO m
                               , HasStorage m
                               , HasClientAPI MailboxAPI UNIX m
                               , HasClientAPI LWWRefAPI UNIX m
                               , Exception (BadFormException c)
                               ) => MakeDictM c m ()
composeEntries = do

  brief "open an issue: compose a Tier B letter and send it to a hub mailbox"
    $ args [ arg "string" "--repo repo-key"
           , arg "string" "--sender sender-sigil"
           , arg "string" "--author author-key", arg "string" "--title title"
           -- BOTH OPTIONAL, and both were absent or wrong here: --recipient is
           -- read from the repository manifest when it is left out (the
           -- paragraph below says so, and the docs say so), and --body was not
           -- in the synopsis at all though it is how a body gets in.
           , arg "string" "[--recipient recipient-sigil]"
           , arg "string" "[--body text | -]"
           , arg "string" "[--label label]..." ]
    $ desc ( "Every value is behind a flag, and they may be given in any"
             <> line <> "order. There is no positional form: two of the four values"
             <> line <> "are keys and two are sigils, so a swap within either pair"
             <> line <> "sent a correctly signed letter claiming the wrong author,"
             <> line <> "with no error and a zero exit -- and an authorship claim"
             <> line <> "inside a signed box cannot be taken back."
             <> line
             <> line <> "Signs the author box with author-key and seals it to"
             <> line <> "recipient-sigil, which is also what says which mailbox"
             <> line <> "it lands in."
             <> line
             <> line <> "The body comes from --body, and --body - reads it from"
             <> line <> "stdin. Nothing is read from stdin otherwise, and that is"
             <> line <> "not tidiness: git hands a hook <old> <new> <ref-name> on"
             <> line <> "stdin, so a hook filing an issue used to sign those bytes"
             <> line <> "as the body, inside the event-id, where canon is"
             <> line <> "append-only and nothing can repair it."
             <> line
             <> line <> "Prints the message hash, which is the letter's identity"
             <> line <> "everywhere else (the inbox lists it, a folded event carries"
             <> line <> "it as its origin), and the event-id the thread will have." )
    $ entry $ bindMatch "hub:issue:new" \case
        (issueArgs -> Just (repo, senderSigil, mrcpt, author, title, labels, mbody)) -> lift do

          body <- liftIO (readBody mbody)

          -- Where it goes, before anything is signed: a letter this node cannot
          -- address is one it must not mint an event-id for.
          rcptSigil <- sigilFor mrcpt repo
                         >>= either (\e -> liftIO (refuse (show (pretty e))
                                                          (manifestCode e)))
                                    pure

          -- 'signerFor' and not 'loadCredentials': keyman resolves a key to the
          -- FILE that holds it and answers with that file's PRIMARY
          -- credentials, so an author key that is a secondary in its keyring
          -- signed the inner box with somebody else's secret while declaring
          -- itself. The letter folds nowhere and the sender is told it was
          -- queued.
          creds <- signerFor author
                     >>= maybe (liftIO (refuse (show ( "cannot sign as"
                                                        <+> pretty (AsBase58 author)
                                                        <> line
                                                        <> "  no keyring here holds it as its"
                                                        <+> "own signing key" ))
                                               codeNoKey))
                               pure

          -- Milliseconds, because that is what the declared timestamp is
          -- (PEP-19): in seconds, a close and a reopen in the same second
          -- collapse to one event-id.
          now <- liftIO getPOSIXTime <&> floor . (* 1000)

          -- Every field the letter layer bounds is bounded before anything is
          -- signed, because an oversized field in a signed box is a letter no
          -- hub will fold and the signature cannot be redone over less.
          -- The labels the author ASKS for, which is all an author can do with a
          -- label: PEP-19 makes applying one an owner-signed set event, and
          -- PEP-22 renders these as `labels_requested` and never as `labels`,
          -- "showing these as labels would let a stranger label their own
          -- issue". They were hard-coded empty here, so the field the render
          -- contract specifies could never be populated by the tool that is
          -- supposed to populate it.
          let content = AOpen repo HubIssue (fromString title) (fmap fromString labels)
                          (bodyOf body) Nothing Nothing now

          -- Through 'refuse', like the same check in Comment and Pr. A throwIO
          -- here exits 1 through the RTS, which loses the exit code the
          -- refusal was supposed to carry -- the defect 'refuse''s own haddock is
          -- a paragraph about.
          for_ (oversizedField content) $ \f ->
            liftIO (refuse (show ("over the size limit for a letter:" <+> pretty f)) 1)

          sto <- getStorage
          api <- getClientAPI @MailboxAPI @UNIX

          let box = uncurry signAuthor (signingPair creds) content
              ob = Outbound sto api rpcTimeout

          h <- sendLetter ob senderSigil [rcptSigil] box (ReplyTo author senderSigil)
                 `catch` (\(e :: PeerSilent) -> liftIO (refuse (show e) codePeerSilent))
                 `catch` (\(e :: NotStored)  -> liftIO (refuse (show e) codeNotStored))
                 `catch` (\(e :: PoWTooHard) -> liftIO (refuse (show e) codeNoWork))

          -- AFTER it is queued, and not before: a log of letters that were not
          -- sent is a log that makes an unrelated ack look like an answer to
          -- one of them. An open is its own thread (PEP-18 threading), which is
          -- why the two hashes below are the same value.
          recordSent Sent { seThread = authorBoxId box
                          , seEvent = authorBoxId box
                          , seMessage = h
                          , seRepo = Just repo
                          , seAuthor = author
                          , seAt = now
                          , seWhat = "issue new"
                          , seTitle = Just (fromString title)
                          }

          -- Both hashes, because they answer different questions and only one
          -- of them is guessable from the other. The message hash is how the
          -- mailbox and retention name this letter; the event-id is what canon
          -- will call the thread, and the sender can compute it now, before any
          -- maintainer has looked (PEP-18 "the sender can compute the thread-id
          -- at send time without any handshake").
          --
          -- PRINTED, and in the same three lines its three siblings print. This
          -- was an s-expression -- @(queued (message H) (thread T))@ -- returned
          -- to the script runtime, so it came out with no trailing newline and
          -- the shell prompt landed on it, in a vocabulary nothing else in the
          -- tool uses. The verb is typed by people; `hub pr new`, `hub pr
          -- revise` and `hub comment` all print, and this is the one a
          -- contributor reaches first.
          liftIO $ print $ vcat
            [ "queued" <+> hashDoc h
            , "thread" <+> hashDoc (authorBoxId box)
            , queuedNext
            ]
          pure (List noContext [])

        -- Its own message, for the reason `hub verify` has one: BadFormException
        -- names an internal Haskell type and a spelling the caller did not type,
        -- and its 'show' renders the whole form -- so a wrong-arity call printed
        -- the caller's argv raw, control characters included.
        other -> liftIO (badArgs (issueUsage :: Doc ()) other)

  where
    bodyOf = letterBody

-- | The bytes a body is, once the shell has had its say.
--
-- A trailing newline from the shell is not part of the body, and an empty body
-- is absent rather than a zero-length one: the fold reports a reference to a
-- part with no body, and @""@ would look like content.
--
-- TRAILING only. This was 'Text.strip', which also removes LEADING whitespace,
-- so a body whose first line is an indented code block or a quoted diff was
-- signed as different bytes from the file that was piped in -- and the body is
-- inside the author box, so it is inside the event-id and cannot be corrected
-- afterwards. The comment on it described the trailing half and the code did
-- both.
--
-- ONE definition, exported, because there were two: this one and a copy in the
-- pull-request verb that trimmed newlines only. Two answers to "what did the
-- author write", both feeding signed boxes, differing on a body that ends in a
-- space -- which is what a shell here-doc and an editor both produce.
letterBody :: String -> Maybe Text
letterBody s = case Text.dropWhileEnd isSpace (Text.pack s) of
  t | Text.null t -> Nothing
    | otherwise   -> Just t

-- | The arguments to @hub issue new@. EVERY value behind a flag.
--
-- THE POSITIONAL FORM IS GONE, and this is why it could not stay. It was four
-- base58 blobs in a row and TWO PAIRS OF THEM ARE INTERCHANGEABLE AT THE
-- PATTERN LEVEL: repo-key and author-key are both 'SignPubKeyLike', the two
-- sigils are both 'HashLike'. So swapping either pair produced a valid, signed,
-- DELIVERED letter -- authored by the repository key, targeting the author
-- key -- with no error and a zero exit. An authorship claim lives inside a
-- signed box, so nothing afterwards can take it back.
--
-- This comment used to end "the positional form stays because it is what exists
-- and what the tests drive", which is a reason to keep a form and not a reason
-- it is safe. It is removed before a release rather than after, because
-- afterwards it is a break: the shape is well-typed either way round, so the
-- only defence a caller has is that the tool will not take it.
--
-- PEP-22 spells the verb with flags, and the sibling verb states the rule for
-- itself: "a form the spec names has to be accepted under that name or the
-- divergence has merely moved". @--title@'s argument is the only free text
-- here, and it is the one the argv reader must hand over verbatim.
--
-- Total, and exported so that a test can ask what a command line means without
-- a peer, a keyman or a dictionary.
issueArgs :: forall c . IsContext c
          => [Syntax c]
          -> Maybe (RepoRef, HashRef, Maybe HashRef, HubKey, String, [String], Maybe String)
issueArgs = named
  where
    -- A title is TEXT, and a person is allowed to call an issue "2026".
    --
    -- The argv reader keeps a word that spells a number as a number, on purpose,
    -- because half the inherited dictionary matches on integers -- and every
    -- title pattern here is 'StringLike', which matches a symbol or a string and
    -- not a number. So `--title 2026` died with a usage message that said
    -- nothing about why. Rendering the atom back is lossless by construction:
    -- the reader kept it as a number only BECAUSE rendering it gives back the
    -- characters that were typed.
    titleOf = \case
      StringLike t           -> Just t
      x@(LitIntVal _)        -> Just (show (pretty x))
      x@(LitScientificVal _) -> Just (show (pretty x))
      x@(LitBoolVal _)       -> Just (show (pretty x))
      _                      -> Nothing
    -- 'flagsOf' does the pairing, the missing-value guard and the unknown-flag
    -- guard; it lives in HBS2.Hub.CLI.Argv because five other verbs had the
    -- same scan without either guard. What stays here is which flags this verb
    -- has and what each value must be.
    named ss = do
      kvs    <- flagsOf knownFlags ss
      repo   <- flagRepo signKey kvs
      sender <- flagOnce kvs "--sender"    >>= hashOf
      -- Optional: without it the --repo repository is asked what sigil it
      -- publishes for its ingress mailbox (PEP-18). Named by hand it wins and
      -- costs no lookup.
      rcpt   <- flagMaybe kvs "--recipient" hashOf
      author <- flagOnce kvs "--author"    >>= signKey
      title  <- flagOnce kvs "--title"     >>= titleOf
      -- Repeatable, unlike every other flag here, and so read with 'flagEvery'
      -- rather than 'flagOnce'. Bounds are not applied at this layer:
      -- 'oversizedField' owns them, and it is the same check the fold will make.
      labels <- traverse titleOf (flagEvery kvs "--label")
      -- The BODY IS NAMED, and it used to be whatever was on stdin.
      --
      -- Reading stdin because it is not a terminal is the ordinary convenience,
      -- and it is the wrong one for a verb that signs. git hands a hook
      -- `<old> <new> <ref-name>` on stdin, so a hook that files an issue signed
      -- those bytes as the body, into the author box and therefore into the
      -- event-id, where canon is append-only and nothing can repair it. The same
      -- module removed the same shape of hazard from the interpreter path
      -- (app/Main.hs) and left it here.
      --
      -- So: no --body, no body. `--body -` is the pipeline spelling and says out
      -- loud that stdin is the source.
      body   <- flagMaybe kvs "--body" titleOf
      pure (repo, sender, rcpt, author, title, labels, body)

    knownFlags = repoFlags <> [ "--sender","--recipient","--author","--title"
                 , "--label","--body" ]

    signKey = \case
      SignPubKeyLike x -> Just x
      _                -> Nothing

    hashOf = \case
      HashLike x -> Just x
      _          -> Nothing

-- | What this verb takes, in the words somebody typing it would use.
issueUsage :: Doc ann
issueUsage = "usage: hub issue new --repo <repo-key> --sender <sender-sigil>"
          <> line <> "                     --recipient <recipient-sigil>"
          <> line <> "                     --author <author-key> --title <title>"
          <> line <> "                     [--label <label>]..."
          <> line
          <> line <> "  --label may be given more than once. A label here is a"
          <> line <> "  REQUEST: applying one is the owner's to sign (PEP-19), so"
          <> line <> "  these are rendered as labels_requested and never as labels."
          <> line
          <> line <> "  --flag=value is accepted too, and is the spelling to use"
          <> line <> "  when a value begins with a dash: `--title --draft` is a"
          <> line <> "  missing title, not a title of `--draft`, and is refused."
          <> line
          <> line <> "  --recipient is OPTIONAL. Left out, the sigil is read from"
          <> line <> "  --repo's own manifest, which is the whole reason a"
          <> line <> "  contributor does not have to go and find a 44-character"
          <> line <> "  hash before they can file anything."
          <> line
          <> line <> "  Given BOTH, they are not cross-checked: the sigil says"
          <> line <> "  which mailbox the letter lands in, --repo says which"
          <> line <> "  repository it is about, and nothing here makes them agree."
          <> line <> "  A letter naming one repo and sent to another's hub is"
          <> line <> "  signed, delivered and then dropped at fold time as"
          <> line <> "  WrongTarget, with no reply path to tell you."
          <> line
          <> line <> "  --body <text>, or --body - to read it from stdin."
          <> line <> "  Every value is behind a flag and there is no positional"
          <> line <> "  form. The two keys and the two sigils are interchangeable"
          <> line <> "  by shape, so a swap sent a signed letter"
          <> line <> "  claiming the wrong author, which cannot be taken back."
          <> line <> "  `hub help issue new` says more."

-- | Where a letter's body comes from, and it is never a surprise.
--
-- 'Nothing' is no @--body@ at all, which is an empty body and an untouched
-- stdin. @-@ is the pipeline spelling. Anything else is the text itself.
--
-- Reading stdin merely because it is not a terminal is the ordinary
-- convenience and the wrong one for a verb that signs: git hands a hook
-- @\<old\> \<new\> \<ref-name\>@ on stdin, so a hook that files an issue signed
-- those bytes as its body, inside the author box and therefore inside the
-- event-id, which canon cannot correct.
readBody :: Maybe String -> IO String
readBody = \case
  Nothing  -> pure ""
  Just "-" -> getContents
  Just t   -> pure t

-- | What happens to a letter after the peer takes it, said by every verb that
-- queues one.
--
-- NONE OF THE FOUR SAID IT. A contributor's `hub issue new` printed two hashes
-- and exited 0, and the next thing that happens is a maintainer folding it on
-- another machine, at another time -- so the two questions the report leaves are
-- "is it there yet" and "how would I know", and neither had an answer anywhere
-- in the tool's output.
--
-- SAID ONCE, because four verbs saying it four ways is how `pr new` came to
-- call the same value @thread@ and `pr revise` @event@.
queuedNext :: Doc ann
queuedNext =
  "the peer has it, and nothing is in canon until a maintainer folds it;"
    <> line <> "  `hbs2-hub updates --repo <key> --mailbox <your-mailbox>` is"
    <+> "what comes back."
