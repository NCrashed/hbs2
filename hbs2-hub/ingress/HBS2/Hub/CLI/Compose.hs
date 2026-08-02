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
  , issueUsage
  , issueArgs
  , codeNoKey
  , codeNotStored
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Ingress (rpcTimeout)
import HBS2.Hub.CLI.Inbox (PeerSilent(..),refuse,codePeerSilent)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.SignedBox (SignedBox)
import HBS2.Net.Auth.Credentials
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,loadCredentials,loadKeyRingEntry)

import Data.Char (isSpace)
import Data.List qualified as List
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
sendLetter ob sender rcpts box reply = do
  let cms = CreateMessageServices
              (obStorage ob)
              (runKeymanClientRO . loadCredentials)
              (runKeymanClientRO . loadKeyRingEntry)

  flags <- defMessageFlags

  -- No attachments here: an inline body is what this verb sends, and a
  -- body-part is a separate decision with its own size rule (PEP-18). Passing
  -- 'mempty' rather than an empty list of readers keeps the parts secret from
  -- being generated for nothing.
  msg <- createMessage cms flags Nothing (Left sender) (fmap Left rcpts) mempty
           (letterPayload (MessageData hubMsgVersion (Letter box reply)))

  -- Stored before it is sent, and this is not belt-and-braces. The peer takes a
  -- Message by value, but everything downstream of it refers to the letter by
  -- the hash of these bytes: the inbox lists that hash, a folded event carries
  -- it as its origin, and retention deletes by it. A sender that cannot name
  -- what it sent cannot correlate the acknowledgement it gets back.
  h <- putBlock (obStorage ob) (serialise msg)
         >>= maybe (throwIO (NotStored "the peer would not store the message")) pure

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
  callRpcWaitMay @RpcMailboxSend (obTimeout ob) (obMailbox ob) msg
    >>= maybe (throwIO (PeerSilent "the mailbox send")) pure
    >>= either (throwIO . NotStored . show
                  . ("the peer refused the message:" <+>) . viaShow)
               pure

  pure (HashRef h)

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

-- | @hub issue new@ and the other compose verbs.
composeEntries :: forall c m . ( IsContext c
                               , MonadUnliftIO m
                               , HasStorage m
                               , HasClientAPI MailboxAPI UNIX m
                               , Exception (BadFormException c)
                               ) => MakeDictM c m ()
composeEntries = do

  brief "open an issue: compose a Tier B letter and send it to a hub mailbox"
    $ args [ arg "string" "--target repo-key"
           , arg "string" "--sender sender-sigil"
           , arg "string" "--recipient recipient-sigil"
           , arg "string" "--author author-key", arg "string" "--title title" ]
    $ desc ( "The flags may be given in any order, and the same five values"
             <> line <> "are also accepted positionally in the order above. Prefer"
             <> line <> "the flags: the two keys are interchangeable positionally,"
             <> line <> "and so are the two sigils, so a swap sends a correctly"
             <> line <> "signed letter claiming the wrong author."
             <> line
             <> line <> "Signs the author box with author-key and seals it to"
             <> line <> "recipient-sigil, which is also what says which mailbox"
             <> line <> "it lands in."
             <> line
             <> line <> "The body is read from stdin when stdin is a pipe or a"
             <> line <> "file, and left empty when it is a terminal: PEP-22 writes"
             <> line <> "it as optional, and a verb that blocked on an unprompted"
             <> line <> "read would look like a hang."
             <> line
             <> line <> "Prints the message hash, which is the letter's identity"
             <> line <> "everywhere else (the inbox lists it, a folded event carries"
             <> line <> "it as its origin), and the event-id the thread will have." )
    $ entry $ bindMatch "hub:issue:new" \case
        (issueArgs -> Just (repo, senderSigil, rcptSigil, author, title, labels)) -> lift do

          -- Only when there is something to read. On a terminal this used to
          -- block with no prompt, which reads as a hang rather than as a
          -- request for input.
          body <- liftIO $ hIsTerminalDevice stdin >>= \case
                    True  -> pure ""
                    False -> getContents

          creds <- runKeymanClientRO (loadCredentials author)
                     >>= maybe (liftIO (refuse (show ("no signing key here for"
                                                       <+> pretty (AsBase58 author)))
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

          for_ (oversizedField content) $ \f ->
            throwIO (userError (show ("over the size limit for a letter:" <+> pretty f)))

          sto <- getStorage
          api <- getClientAPI @MailboxAPI @UNIX

          let box = signAuthor author (_peerSignSk creds) content
              ob = Outbound sto api rpcTimeout

          h <- sendLetter ob senderSigil [rcptSigil] box noReplyChannel
                 `catch` (\(e :: PeerSilent) -> liftIO (refuse (show e) codePeerSilent))
                 `catch` (\(e :: NotStored)  -> liftIO (refuse (show e) codeNotStored))

          -- Both hashes, because they answer different questions and only one
          -- of them is guessable from the other. The message hash is how the
          -- mailbox and retention name this letter; the event-id is what canon
          -- will call the thread, and the sender can compute it now, before any
          -- maintainer has looked (PEP-18 "the sender can compute the thread-id
          -- at send time without any handshake").
          -- Strings, not symbols: a hash is data here, and the argv reader on the
          -- way back in lexes a bare base58 word as a symbol only by accident of
          -- it having no punctuation. One convention for hashes across the tool.
          -- "queued", not "sent", and the word is the finding. The peer's send
          -- RPC answers @()@ and its handler discards the protocol's own result,
          -- so what a zero exit here establishes is that the peer took the
          -- message off our hands -- not that any mailbox accepted it, and not
          -- that it was delivered. `sent` was a claim about the network that
          -- nothing in the round trip supports, printed to somebody who would
          -- then stop watching for it.
          pure $ mkForm "queued" [ mkForm "message" [mkStr (show (pretty h))]
                                 , mkForm "thread"  [mkStr (show (pretty (authorBoxId box)))]
                                 ]

        -- Its own message, for the reason `hub verify` has one: BadFormException
        -- names an internal Haskell type and a spelling the caller did not type,
        -- and its 'show' renders the whole form -- so a wrong-arity call printed
        -- the caller's argv raw, control characters included.
        _ -> liftIO (die (show (issueUsage :: Doc ())))

  where
    -- A trailing newline from the shell is not part of the body, and an empty
    -- body is absent rather than a zero-length one: the fold reports a
    -- reference to a part with no body, and "" would look like content.
    --
    -- TRAILING only. This was 'Text.strip', which also removes LEADING
    -- whitespace, so a body whose first line is an indented code block or a
    -- quoted diff was signed as different bytes from the file that was piped in
    -- -- and the body is inside the author box, so it is inside the event-id and
    -- cannot be corrected afterwards. The comment above described the trailing
    -- half and the code did both.
    bodyOf s = case Text.dropWhileEnd isSpace (Text.pack s) of
      t | Text.null t -> Nothing
        | otherwise   -> Just t

-- | The arguments to @hub issue new@, positionally or by name.
--
-- The named form exists because the positional one is four base58 blobs in a
-- row and TWO PAIRS OF THEM ARE INTERCHANGEABLE AT THE PATTERN LEVEL: repo-key
-- and author-key are both 'SignPubKeyLike', the two sigils are both 'HashLike'.
-- Swapping the keys produced a valid, signed, delivered letter authored by the
-- repository key and targeting the author key, with no error and a zero exit --
-- and an authorship claim inside a signed box is permanent. A name cannot be
-- swapped silently.
--
-- PEP-22 spells the verb with flags, and this is also the rule the sibling verb
-- states for itself: "a form the spec names has to be accepted under that name
-- or the divergence has merely moved". The positional form stays because it is
-- what exists and what the tests drive; --title's argument is the only free text
-- here, and it is the one the argv reader must hand over verbatim.
--
-- Total, and exported so that a test can ask what a command line means without
-- a peer, a keyman or a dictionary.
issueArgs :: forall c . IsContext c
          => [Syntax c] -> Maybe (RepoRef, HashRef, HashRef, HubKey, String, [String])
issueArgs = \case
  [ SignPubKeyLike repo, HashLike sender, HashLike rcpt
    , SignPubKeyLike author, (titleOf -> Just title) ] ->
      -- No labels positionally: a repeatable value has no position, and the
      -- positional form stays only because it is what exists.
      Just (repo, sender, rcpt, author, title, [])
  ss -> named ss
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
    named ss = do
      kvs <- pairs ss
      -- Every flag known, so a typo is a refusal rather than a default. Without
      -- this, `--titel X` would be dropped on the floor and the letter would go
      -- out under whatever the other flags said.
      guard (all ((`elem` knownFlags) . fst) kvs)
      repo   <- one kvs "--target"    >>= signKey
      sender <- one kvs "--sender"    >>= hashOf
      rcpt   <- one kvs "--recipient" >>= hashOf
      author <- one kvs "--author"    >>= signKey
      title  <- one kvs "--title"     >>= titleOf
      -- Repeatable, unlike every other flag here, and so read with 'every'
      -- rather than 'one'. Bounds are not applied at this layer: 'oversizedField'
      -- owns them, and it is the same check the fold will make.
      labels <- traverse titleOf (every kvs "--label")
      pure (repo, sender, rcpt, author, title, labels)

    knownFlags = ["--target","--sender","--recipient","--author","--title","--label"]

    -- Exactly one, so that `--title a --title b` is a refusal rather than a
    -- silent choice between them.
    one kvs k = case every kvs k of
      [v] -> Just v
      _   -> Nothing

    every kvs k = [ v | (k',v) <- kvs, k' == k ]

    pairs :: [Syntax c] -> Maybe [(String, Syntax c)]
    pairs = \case
      [] -> Just []
      -- `--flag=value`, which was accepted nowhere and named in no usage text:
      -- `--title=t` fell through to the case below, paired `--title=t` with the
      -- NEXT word, and printed a usage message that did not say what was wrong.
      -- It is also the spelling that lets a value legitimately begin with a
      -- dash, which is what makes the refusal below affordable.
      (StringLike k : rest)
        | Just (k', v) <- splitFlag k -> ((k', mkStr @c v) :) <$> pairs rest
      (StringLike k : v : rest)
        | "--" `List.isPrefixOf` k, not (flagLike v) -> ((k,v) :) <$> pairs rest
      _ -> Nothing

    -- A flag where a value belongs is a MISSING value, not a value. Without
    -- this, `hub issue new ... --title --draft` parsed cleanly and signed the
    -- string `--draft` as the title -- into the author box and therefore into
    -- the event-id, which is the hash of that box, so it cannot be corrected
    -- afterwards: canon is append-only. That is the same hazard the named form
    -- was introduced for, reached by a different route.
    flagLike = \case
      StringLike s -> "--" `List.isPrefixOf` s
      _            -> False

    -- Split on the FIRST '=' only, so a value may contain one.
    splitFlag s = case List.break (== '=') s of
      (k, '=' : v) | "--" `List.isPrefixOf` k, length k > 2 -> Just (k, v)
      _                                                     -> Nothing

    signKey = \case
      SignPubKeyLike x -> Just x
      _                -> Nothing

    hashOf = \case
      HashLike x -> Just x
      _          -> Nothing

-- | What this verb takes, in the words somebody typing it would use.
issueUsage :: Doc ann
issueUsage = "usage: hub issue new --target <repo-key> --sender <sender-sigil>"
          <> line <> "                     --recipient <recipient-sigil>"
          <> line <> "                     --author <author-key> --title <title>"
          <> line <> "                     [--label <label>]..."
          <> line <> "   or: hub issue new <repo-key> <sender-sigil> <recipient-sigil>"
          <> line <> "                     <author-key> <title>"
          <> line
          <> line <> "  --label may be given more than once. A label here is a"
          <> line <> "  REQUEST: applying one is the owner's to sign (PEP-19), so"
          <> line <> "  these are rendered as labels_requested and never as labels."
          <> line
          <> line <> "  --flag=value is accepted too, and is the spelling to use"
          <> line <> "  when a value begins with a dash: `--title --draft` is a"
          <> line <> "  missing title, not a title of `--draft`, and is refused."
          <> line
          <> line <> "  --target and --recipient are NOT cross-checked, and cannot"
          <> line <> "  be here: the sigil says which mailbox the letter lands in,"
          <> line <> "  --target says which repository it is about, and resolving"
          <> line <> "  one from the other needs that repository's manifest, which"
          <> line <> "  this verb does not read. A letter naming one repo and sent"
          <> line <> "  to another's hub is signed, delivered and then dropped at"
          <> line <> "  fold time as WrongTarget, with no reply path to tell you."
          <> line
          <> line <> "  The body is read from stdin when stdin is not a terminal."
          <> line <> "  Prefer the named form: the two keys and the two sigils are"
          <> line <> "  interchangeable positionally, and a swap sends a correctly"
          <> line <> "  signed letter claiming the wrong author, which cannot be"
          <> line <> "  taken back."
          <> line <> "  `hub help issue new` says more."
