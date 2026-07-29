-- 'opOf' below dispatches on every constructor of 'AuthorContent' with no
-- wildcard, on content an attacker composed, inside the triage loop. The library
-- modules set this for the same reason: a constructor added without a case here
-- is a crash in that loop rather than a build error.
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | @hub inbox@: the triage queue, rendered (PEP-22 "Maintain").
--
-- Only the presentation and the argument handling. What reading a mailbox MEANS
-- is "HBS2.Hub.Ingress", which a test can reach.
module HBS2.Hub.CLI.Inbox
  ( inboxEntries
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Ingress

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client (HasClientAPI(..))
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,extractGroupKeySecret)

import System.Exit (exitWith,ExitCode(..))

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
    $ desc ( "Read-only. Waits for the peer's copy of the mailbox to settle,"
             <> line <> "opens every message this node holds a key for, and reports"
             <> line <> "what each one asks for. Nothing is folded, minted or deleted."
             <> line
             <> line <> "The peer must already hold this mailbox: it only asks the"
             <> line <> "network about mailboxes it has locally, so an arbitrary key"
             <> line <> "reads as empty forever. Create one with"
             <> line <> "'hbs2-peer mailbox create --key KEY hub'."
             <> line
             <> line <> "Takes the mailbox key directly. PEP-22 specifies the form"
             <> line <> "that reads it from the repo manifest instead; that needs a"
             <> line <> "manifest reader, which does not exist yet, and it is also"
             <> line <> "what would supply the PEP-21 deny-list that this form has"
             <> line <> "no source for." )
    $ entry $ bindMatch "hub:inbox" $ nil_ \case
        [ SignPubKeyLike mbox ] -> lift do
          sto <- getStorage
          api <- getClientAPI @MailboxAPI @UNIX
          let ig = Ingress { igStorage = sto
                           , igMailbox = api
                             -- No deny-list: PEP-21 policy lives in the repo
                             -- manifest and this verb takes a mailbox key rather
                             -- than a repo. Allowing everything is honest here,
                             -- since listing is not accepting; the accept path
                             -- must not inherit this default.
                           , igAllowed = const True
                           , igSecret = ReadMessageServices
                               (liftIO . runKeymanClientRO . extractGroupKeySecret)
                           , igTimeout = rpcTimeout
                           }

          r <- readInbox ig mbox

          liftIO $ mapM_ (print . render) (irLetters r)

          -- A tree read with holes in it is a wrong answer, not a short one, so
          -- it does not leave through a zero exit. Said once at the end rather
          -- than per block: the number is what matters, and a page of identical
          -- warnings buries the letters above it.
          when (irMissing r > 0) $ liftIO do
            hPutDoc stderr $ "hub:" <+> pretty (irMissing r)
              <+> "block(s) of this mailbox tree could not be read;"
              <+> "the list above is incomplete in both directions" <> line
            exitWith (ExitFailure 2)

        _ -> throwIO (BadFormException @c nil)

-- One line per letter, in the order the fields matter to somebody deciding what
-- to do with it.
--
-- The envelope key is printed on the failure paths too, wherever it is known: it
-- is the key 'hub block' takes, so a forged envelope that does not name its
-- signer is a report nobody can act on.
render :: LetterView -> Doc ann
render lv = pretty (lvMessage lv) <+> maybe "-" (pretty . AsBase58) (lvEnvelope lv)
              <+> body
  where
    body = case lvLetter lv of
      Left e -> "unreadable:" <+> pretty e
      Right (author, content, disp) ->
        "author" <+> pretty (AsBase58 author)
          <+> "event" <+> maybe "?" pretty (lvEventId lv)
          -- The author's own clock, advisory and unverifiable (PEP-19), which is
          -- why it is shown and not sorted on: sorting the queue by it would let
          -- a sender choose where in the queue they appear.
          <+> "at" <+> pretty (authorTs content)
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
