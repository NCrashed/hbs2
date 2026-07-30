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
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,extractGroupKeySecret)

import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale,formatTime)
import Data.Word (Word64)
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
    $ args [arg "string" "[--mailbox] mailbox-key"]
    $ desc ( "Read-only. Waits for the peer's copy of the mailbox to settle,"
             <> line <> "opens every message this node holds a key for, and reports"
             <> line <> "what each one asks for. Nothing is folded, minted or deleted."
             <> line
             <> line <> "The peer must already hold this mailbox: it only asks the"
             <> line <> "network about mailboxes it has locally, so an arbitrary key"
             <> line <> "reads as empty forever. Create one with"
             <> line <> "'hbs2-peer mailbox create --key KEY hub'."
             <> line
             <> line <> "Takes the mailbox key directly, which is the form PEP-22"
             <> line <> "spells --mailbox. The form that reads the key from the repo"
             <> line <> "manifest needs a manifest reader, which does not exist yet,"
             <> line <> "and that manifest is also what would supply the PEP-21"
             <> line <> "deny-list this form has no source for: a queue read this"
             <> line <> "way shows letters from banned authors." )
    -- Both spellings, because PEP-22 specifies the flag: the bare key is what
    -- the spec calls `--mailbox <key>`, and a form the spec names has to be
    -- accepted under that name or the divergence has merely moved.
    $ entry $ bindMatch "hub:inbox" $ nil_ \case
        [ SignPubKeyLike mbox ]                          -> lift (listInbox mbox)
        [ StringLike "--mailbox", SignPubKeyLike mbox ]   -> lift (listInbox mbox)
        _ -> throwIO (BadFormException @c nil)

  where
    listInbox mbox = do
      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX

      r <- readInbox (overRpc sto api) mbox

      liftIO $ mapM_ (print . render) (irLetters r)

      -- Two things can be said about the list above, and only one of them makes
      -- it WRONG. Said once at the end: the counts are what matter, and a page
      -- of identical warnings buries the letters.
      liftIO do
        -- Not an error, and not silence either. Every letter listed is real; the
        -- mailbox was simply still arriving, which is the routine state of a
        -- first read of a big one, since the peer rewrites the mailbox ref on
        -- every merged batch. A non-zero exit here would fire on ordinary use
        -- and teach a caller to ignore the code.
        unless (irSettled r) $
          hPutDoc stderr $ "hub: the peer's copy of this mailbox was still"
            <+> "changing after" <+> pretty maxFetchRounds <+> "rounds, so more"
            <+> "letters may follow. Everything listed above is real." <> line

        -- This one IS wrong, in both directions: a missing chunk carrying Exists
        -- entries makes letters vanish, one carrying Deleted entries puts folded
        -- letters back in the queue. So it does not leave through a zero exit.
        unless (List.null (irMissing r)) do
          hPutDoc stderr $ "hub:" <+> pretty (length (irMissing r))
            <+> "block(s) of this mailbox tree could not be read, so the list"
            <+> "above is incomplete in both directions."
            <+> "Missing:" <+> hsep (fmap pretty (irMissing r)) <> line
          exitWith (ExitFailure 2)

    -- The one place the ingress is wired to a peer. Everything above it is a
    -- function of these five, which is what lets the wait loop and every
    -- OpenError be tested without one.
    overRpc sto api = Ingress
      { igBlock  = liftIO . getBlock sto . coerce
      , igStatus = \k ->
          callRpcWaitMay @RpcMailboxGetStatus rpcTimeout api k
            >>= orThrowUser "cannot reach the peer's mailbox service"
            >>= either badService (pure . void)
      , igFetch  = void . callRpcWaitMay @RpcMailboxFetch rpcTimeout api
      , igRoot   = \k ->
          callRpcWaitMay @RpcMailboxGet rpcTimeout api k
            >>= orThrowUser "cannot reach the peer's mailbox service"
      , igPause  = pause
        -- No deny-list: PEP-21 policy lives in the repo manifest and this verb
        -- takes a mailbox key rather than a repo. Allowing everything is honest
        -- here, since listing is not accepting; the accept path must not
        -- inherit this default.
      , igAllowed = const True
      , igSecret = ReadMessageServices
          (liftIO . runKeymanClientRO . extractGroupKeySecret)
      }

badService :: MonadUnliftIO m => MailboxServiceError -> m a
badService e = throwIO (userError (show ("mailbox service:" <+> viaShow e)))

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
          <+> "at" <+> pretty (utcOf (authorTs content))
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

-- The author's declared time as something a human reads. Epoch milliseconds are
-- what the field IS (PEP-19) and what any tooling should parse, but a triage
-- queue is read by a person, and a column of thirteen-digit integers is a column
-- nobody compares.
utcOf :: Word64 -> String
utcOf ms
  -- Clamped at the ceiling canon admits, because the value is the SENDER's and
  -- unverifiable (PEP-19): maxBound formats as the year 584 million, which does
  -- not crash but does let a sender wreck the column alignment of the queue a
  -- maintainer is reading.
  | ms > maxFoldedTs = "after " <> utcOf maxFoldedTs
  | otherwise = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
                  (posixSecondsToUTCTime (fromIntegral ms / 1000))
