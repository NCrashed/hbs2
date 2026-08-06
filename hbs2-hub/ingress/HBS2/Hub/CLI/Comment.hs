-- | @hub issue comment@ and @hub pr comment@ (PEP-18, PEP-22 "Contribute").
--
-- The reply. Until this existed a contributor could open a thread and never say
-- another word in it: the fold has carried comments since PEP-19, the bridge
-- has admitted them, the queue renders them, and nothing composed one.
--
-- ONE IMPLEMENTATION UNDER TWO NAMES, because a comment is a comment: the op
-- names a thread and says nothing about what kind of thread it is (PEP-18's
-- @AComment@ carries no kind and no repository), and which of the two nouns a
-- person types is which noun they have in their head. Binding only one of them
-- would make the other a spelling that fails for no reason a caller can see.
--
-- NO REPOSITORY IS NAMED, and none can be. A comment names a thread, a thread
-- is an open, and the open names the repository; the binding is transitive and
-- the fold makes it (rule 4). What decides which mailbox the letter lands in is
-- the recipient sigil, exactly as it does for an open.
--
-- AN EMPTY COMMENT IS REFUSED rather than signed. A body is the whole content
-- of this op, so a comment without one is an event that says nothing, spends a
-- seq, and is in canon forever.
module HBS2.Hub.CLI.Comment
  ( commentEntries
  , commentUsage
  , CommentArgs(..)
  , commentArgs
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Ingress (rpcTimeout,PeerSilent(..))
import HBS2.Hub.Sent (Sent(..),recordSent)
import HBS2.Hub.CLI.Argv (flagsOf,flagOnce,flagMaybe,flagText,repoFlags,flagRepo,flagRepoMaybe)
import HBS2.Hub.CLI.Inbox (refuse,codePeerSilent,manifestCode)
import HBS2.Hub.Repo.Manifest (sigilFor)
import HBS2.Hub.CLI.Compose (Outbound(..),sendLetter,letterBody,readBody,codeNoKey
                            ,NotStored(..),codeNotStored,PoWTooHard(..),codeNoWork)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Net.Auth.Credentials (_peerSignSk)
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,loadCredentials)

import Data.Maybe (isNothing)
import Data.Text qualified as Text
import System.Exit (die)

-- | What one comment was asked to say, and to whom.
data CommentArgs = CommentArgs
  { cmSender :: HashRef        -- ^ the contributor's sigil
    -- | The hub's sigil, which is what picks the mailbox. Absent: read it from
    -- --repo's manifest.
  , cmRcpt   :: Maybe HashRef
    -- | Where to send it, for addressing only.
    --
    -- A comment names a thread and carries no repository (PEP-18), and this
    -- flag does not change that: it never enters the letter, and the fold binds
    -- the comment to a repository through the thread as it always did. It is
    -- here because the manifest is keyed by repository and a sigil has to come
    -- from somewhere; one of it and --recipient has to be given.
  , cmTarget :: Maybe RepoRef
  , cmAuthor :: HubKey         -- ^ the key that signs the inner box
  , cmThread :: ThreadId       -- ^ the thread being replied to
    -- | The event this answers, when it answers one in particular.
    --
    -- Advisory: the fold records it and renders it, and admits a comment whose
    -- reply-to names nothing, because the alternative is a thread that loses
    -- replies when the event they answered is redacted.
  , cmReplyTo :: Maybe EventId
  , cmBody   :: Maybe Text     -- ^ @-@ means stdin
  }
  deriving stock (Eq,Show)

commentUsage :: Doc ()
commentUsage =
  "usage: hbs2-hub issue|pr comment --sender <sigil> --author <key>"
    <> line <> "       --thread <thread-id> [--reply-to <event-id>] --body <text> | --body -"
    <> line <> "       and one of --recipient <sigil> | --repo <repo-key>"

commentEntries :: forall c m . ( IsContext c
                               , MonadUnliftIO m
                               , HasStorage m
                               , HasClientAPI MailboxAPI UNIX m
                               , HasClientAPI LWWRefAPI UNIX m
                               , Exception (BadFormException c)
                               ) => MakeDictM c m ()
commentEntries = do

  commentVerb "hub:issue:comment"
  commentVerb "hub:pr:comment"

  where

    commentVerb name =
      brief "reply in a thread: compose a Tier B letter and send it to a hub mailbox"
        $ args [ arg "string" "--sender sender-sigil"
               , arg "string" "--recipient recipient-sigil"
               , arg "string" "--author author-key"
               , arg "string" "--thread thread-id"
               , arg "string" "--body text" ]
        $ desc ( "Signs the comment with --author and seals it to --recipient,"
                 <> line <> "which is also what says which mailbox it lands in."
                 <> line
                 <> line <> "No repository is named and none can be: a comment names"
                 <> line <> "a thread, the thread names the repository, and the fold"
                 <> line <> "makes that binding. A thread-id from another repository"
                 <> line <> "is dropped at fold time as an unknown thread."
                 <> line
                 <> line <> "issue comment and pr comment are the same verb. The op"
                 <> line <> "carries no kind, so which noun you type is which noun"
                 <> line <> "you have in mind."
                 <> line
                 <> line <> "--reply-to names the event being answered. It is"
                 <> line <> "advisory: a comment whose reply-to names nothing is"
                 <> line <> "still admitted, so redacting an event does not take the"
                 <> line <> "replies to it out of the thread."
                 <> line
                 <> line <> "The body comes from --body, and --body - reads it from"
                 <> line <> "stdin. Nothing is read from stdin otherwise: git hands a"
                 <> line <> "hook <old> <new> <ref-name> there, and a body goes inside"
                 <> line <> "the signature and the event-id, where canon cannot fix"
                 <> line <> "it. A comment with no body is refused rather than sent."
                 <> line
                 <> line <> "Prints the message hash and the event-id this comment"
                 <> line <> "will have, which the sender can compute before any"
                 <> line <> "maintainer has looked." )
        $ entry $ bindMatch name $ nil_ \case
            (commentArgs -> Just cm) -> lift (comment cm)
            _ -> liftIO (die (show commentUsage))

    comment cm = do
      body <- liftIO (readBody (fmap Text.unpack (cmBody cm)))

      -- Before the key is loaded and before anything is sealed: this is the
      -- caller's mistake and the cheapest place to say so.
      when (isNothing (letterBody body)) $
        liftIO $ refuse (show ( "a comment with no body says nothing, and canon is"
                                  <+> "append-only" <> line
                                  <> "  " <> commentUsage ))
                        1

      rcpt <- case (cmRcpt cm, cmTarget cm) of
                (Just h, _) -> pure h
                -- Named by hand it wins and costs no lookup; otherwise the
                -- repository is asked what sigil it publishes (PEP-18).
                (Nothing, Just repo) ->
                  sigilFor Nothing repo
                    >>= either (\e -> liftIO (refuse (show (pretty e)) (manifestCode e)))
                               pure
                (Nothing, Nothing) -> liftIO (die (show commentUsage))

      creds <- runKeymanClientRO (loadCredentials (cmAuthor cm))
                 >>= maybe (liftIO (refuse (show ("no signing key here for"
                                                   <+> pretty (AsBase58 (cmAuthor cm))))
                                           codeNoKey))
                           pure

      -- Milliseconds, the unit every declared timestamp in this package is in
      -- (PEP-19): in seconds, two comments in one tick collapse to one
      -- event-id and the second is a duplicate nobody meant to send.
      now <- liftIO getPOSIXTime <&> floor . (* 1000)

      let content = AComment (cmThread cm) (cmReplyTo cm) (letterBody body) Nothing now

      -- The same bound the fold will apply, applied before anything is signed:
      -- an oversized field inside a signed box is a letter no hub will fold,
      -- and the signature cannot be redone over less.
      for_ (oversizedField content) $ \f ->
        liftIO $ refuse (show ("over the size limit for a letter:" <+> pretty f)) 1

      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX

      let ob = Outbound sto api rpcTimeout
          box = signAuthor (cmAuthor cm) (_peerSignSk creds) content

      h <- sendLetter ob (cmSender cm) [rcpt] box
             (ReplyTo (cmAuthor cm) (cmSender cm))
             `catch` (\(e :: PeerSilent) -> liftIO (refuse (show e) codePeerSilent))
             `catch` (\(e :: NotStored)  -> liftIO (refuse (show e) codeNotStored))
             `catch` (\(e :: PoWTooHard) -> liftIO (refuse (show e) codeNoWork))

      -- "queued" and not "sent", like the sibling verbs: the peer's send RPC
      -- answers unit, so a zero exit says the peer took the message off our
      -- hands and nothing about any mailbox having accepted it.
      recordSent Sent { seThread = cmThread cm
                      , seEvent = authorBoxId box
                      , seMessage = h
                      -- A comment names a thread and nothing else (PEP-18), so
                      -- there is no repository to record and none is invented.
                      , seRepo = Nothing
                      , seAuthor = cmAuthor cm
                      , seAt = now
                      , seWhat = "comment"
                      , seTitle = Nothing
                      }

      liftIO $ print $ vcat
        [ "queued" <+> hashDoc h
        , "event" <+> hashDoc (authorBoxId box)
        , "on" <+> hashDoc (cmThread cm)
        ]

-- | Every value behind a flag.
--
-- Two sigils and a key, which is two 'HashLike' values and one
-- 'SignPubKeyLike', and the thread-id is a third hash: nothing here can be told
-- apart by position, and a swap sends a correctly signed comment into somebody
-- else's thread.
commentArgs :: forall c . IsContext c => [Syntax c] -> Maybe CommentArgs
commentArgs syn = do
  kvs    <- flagsOf (repoFlags <> ["--sender","--recipient","--author","--thread"
                                  ,"--reply-to","--body"]) syn
  sender <- flagOnce kvs "--sender"    >>= asHash
  rcpt   <- flagMaybe kvs "--recipient" asHash
  target <- flagRepoMaybe asKey kvs
  author <- flagOnce kvs "--author"    >>= asKey
  thread <- flagOnce kvs "--thread"    >>= asHash
  replyTo <- flagMaybe kvs "--reply-to" asHash
  -- Through 'flagText', so a body that spells a number is one: `--body 2026`
  -- is a body, and a StringLike pattern misses it.
  body   <- flagMaybe kvs "--body" (fmap Text.pack . flagText)
  pure (CommentArgs sender rcpt target author thread replyTo body)
  where
    asKey  = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    asHash = \case { HashLike h -> Just h ; _ -> Nothing }
