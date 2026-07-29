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
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Ingress (rpcTimeout)

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

import Data.Text qualified as Text

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
         >>= orThrowUser "cannot store the message"

  -- Checked, not voided. callRpcWaitMay answers Nothing on a timeout, and
  -- discarding that printed a message hash and left with a zero exit having sent
  -- nothing at all. The read side spent a whole round being taught not to report
  -- silence as an answer; on the write side the same mistake is worse, because
  -- the caller believes a letter is in a mailbox.
  callRpcWaitMay @RpcMailboxSend (obTimeout ob) (obMailbox ob) msg
    >>= orThrowUser "the peer did not accept the message for delivery"

  pure (HashRef h)

-- | @hub issue new@ and the other compose verbs.
composeEntries :: forall c m . ( IsContext c
                               , MonadUnliftIO m
                               , HasStorage m
                               , HasClientAPI MailboxAPI UNIX m
                               , Exception (BadFormException c)
                               ) => MakeDictM c m ()
composeEntries = do

  brief "open an issue: compose a Tier B letter and send it to a hub mailbox"
    $ args [ arg "string" "repo-key"
           , arg "string" "sender-sigil", arg "string" "recipient-sigil"
           , arg "string" "author-key", arg "string" "title" ]
    $ desc ( "Signs the author box with author-key and seals it to"
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
        [ SignPubKeyLike repo
          , HashLike senderSigil, HashLike rcptSigil
          , SignPubKeyLike author, StringLike title ] -> lift do

          -- Only when there is something to read. On a terminal this used to
          -- block with no prompt, which reads as a hang rather than as a
          -- request for input.
          body <- liftIO $ hIsTerminalDevice stdin >>= \case
                    True  -> pure ""
                    False -> getContents

          creds <- runKeymanClientRO (loadCredentials author)
                     >>= orThrowUser ("no credentials for" <+> pretty (AsBase58 author))

          -- Milliseconds, because that is what the declared timestamp is
          -- (PEP-19): in seconds, a close and a reopen in the same second
          -- collapse to one event-id.
          now <- liftIO getPOSIXTime <&> floor . (* 1000)

          -- Every field the letter layer bounds is bounded before anything is
          -- signed, because an oversized field in a signed box is a letter no
          -- hub will fold and the signature cannot be redone over less.
          let content = AOpen repo HubIssue (fromString title) []
                          (bodyOf body) Nothing Nothing now

          for_ (oversizedField content) $ \f ->
            throwIO (userError (show ("over the size limit for a letter:" <+> pretty f)))

          sto <- getStorage
          api <- getClientAPI @MailboxAPI @UNIX

          let box = signAuthor author (_peerSignSk creds) content
              ob = Outbound sto api rpcTimeout

          h <- sendLetter ob senderSigil [rcptSigil] box noReplyChannel

          -- Both hashes, because they answer different questions and only one
          -- of them is guessable from the other. The message hash is how the
          -- mailbox and retention name this letter; the event-id is what canon
          -- will call the thread, and the sender can compute it now, before any
          -- maintainer has looked (PEP-18 "the sender can compute the thread-id
          -- at send time without any handshake").
          -- Strings, not symbols: a hash is data here, and the argv reader on the
          -- way back in lexes a bare base58 word as a symbol only by accident of
          -- it having no punctuation. One convention for hashes across the tool.
          pure $ mkForm "sent" [ mkForm "message" [mkStr (show (pretty h))]
                               , mkForm "thread"  [mkStr (show (pretty (authorBoxId box)))]
                               ]

        _ -> throwIO (BadFormException @c nil)

  where
    -- A trailing newline from the shell is not part of the body, and an empty
    -- body is absent rather than a zero-length one: the fold reports a
    -- reference to a part with no body, and "" would look like content.
    bodyOf s = case Text.strip (Text.pack s) of
      t | Text.null t -> Nothing
        | otherwise   -> Just t
