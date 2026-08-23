-- | Dropping one letter from a mailbox (PEP-21 "Retention").
--
-- Its own module because there are two callers with opposite meanings and one
-- act: @hub inbox reject@ drops a letter INSTEAD of folding it, and @hub inbox
-- accept@ drops it BECAUSE it folded it. Two copies of a signed delete would be
-- two answers to what a hub asks a peer for, drifting apart at whichever of them
-- a later fix touches.
--
-- WHAT "DROP" MEANS HERE, exactly, because the word promises more than the
-- system does. It writes a @Deleted@ entry into the mailbox, which is a
-- tombstone: 'liveMessages' stops reporting the message and the triage queue
-- stops showing it. The blocks stay in storage. Nothing in this project walks a
-- mailbox and frees bytes, so no disk is reclaimed by this and none is claimed
-- to be.
--
-- THE MAILBOX KEY SIGNS IT, not the repo key and not a canon key. The peer
-- takes the signer of the payload AS the mailbox being deleted from
-- (@mailboxAcceptDelete adapter (MailboxRefKey mbox)@), so a delete signed by
-- anything else is a delete against a mailbox nobody has. A delegate holding
-- only a canon key therefore cannot drop anything, which is the same
-- signing-is-not-publishing boundary the maintainer verbs describe, and is why
-- this answers with a value rather than exiting: one caller refuses on it and
-- the other has already written canon and only reports it.
--
-- A LIST OF MESSAGES AND NO PREDICATE. A sweep is a thing an operator may want,
-- and the predicate language has the Or to express one, but a function that took
-- a PREDICATE would take one matching more than the caller read. So the caller
-- names the letters it decided about, and 'deleteNaming' turns that list into as
-- many payloads as a reader will accept -- which since PEP-23 step B is up to
-- 'maxDeleteTargets' letters per signature instead of one.
module HBS2.Hub.CLI.Drop
  ( dropMessage
  , dropMessages
  , DropTrouble(..)
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Ingress (rpcTimeout)
import HBS2.Hub.CLI.Common (signerFor,signingPair)

import HBS2.CLI.Prelude

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.SignedBox (makeSignedBox)
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Merge (deleteNaming)
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)


import Data.Text qualified as Text

-- | Why a letter is still in the mailbox.
--
-- Three answers and not one, because the remedies differ: install a key, look
-- at the peer, read what the peer said. The first is also the routine one -- a
-- delegate accepting on the owner's behalf holds a canon key and not the
-- mailbox key -- so it must not read as a fault.
data DropTrouble =
    DropNoKey HubKey     -- ^ no signing key here for the mailbox itself
  | DropPeerSilent       -- ^ the peer is up and did not answer in time
  | DropRefused Text     -- ^ the peer answered and would not
  deriving stock (Eq,Show)

instance Pretty DropTrouble where
  pretty = \case
    DropNoKey k ->
      "no signing key here for" <+> pretty (AsBase58 k) <> line
        <> "  the mailbox's own key signs a delete; a canon key cannot"
    DropPeerSilent ->
      "the peer did not answer the mailbox delete within"
        <+> pretty rpcTimeout
    -- Through 'safeText' like everything else this tool prints that it did not
    -- author: the error carries what the peer said about a payload a stranger
    -- can influence.
    DropRefused e -> "the peer refused the delete:" <+> pretty (safeText e)

-- | Write the tombstone, or say why there is none.
dropMessage :: forall m . ( MonadUnliftIO m
                          , HasClientAPI MailboxAPI UNIX m
                          )
            => HubKey -> HashRef -> m (Either DropTrouble ())
dropMessage mbox msg = dropMessages mbox [msg]

-- | Write the tombstones for a set of letters, or say why there are none.
--
-- ONE SIGNATURE PER BATCH, not per letter, which is the whole of PEP-23 step B
-- at this end. A reject drops the letter and every rewrapped copy of it, and an
-- inbox sweep drops a page at a time, so the one-per-letter shape this replaced
-- meant one packet flooded to every neighbour per letter, and after step A it
-- would have meant one proof-of-work grind per letter too.
--
-- STOPS AT THE FIRST BATCH THAT FAILS, and reports that batch's answer. The
-- alternative is to carry on and report the last failure, which would leave the
-- caller unable to tell one refused batch from all of them; and the three
-- answers 'DropTrouble' has are about the peer and the keys, so a second batch
-- failing differently from the first is not a case that arises.
--
-- What HAS happened when it answers 'Left' is the batches before the failing
-- one, and that is safe to re-run: a delete is a tombstone keyed by the entry,
-- so re-issuing it is a lookup on the peer and no work.
dropMessages :: forall m . ( MonadUnliftIO m
                           , HasClientAPI MailboxAPI UNIX m
                           )
             => HubKey -> [HashRef] -> m (Either DropTrouble ())
dropMessages _ [] = pure (Right ())
dropMessages mbox msgs =
  -- 'signerFor' and not 'loadCredentials': keyman answers with the credentials
  -- of the FILE holding a key, whose sign key is that file's primary one, so a
  -- mailbox key that is a secondary in its keyring produced a delete box signed
  -- by somebody else and refused by the peer as not the mailbox's own. Here
  -- that is the same answer as holding no key at all, which is what
  -- 'DropNoKey' already means.
  signerFor mbox >>= \case
    Nothing -> pure (Left (DropNoKey mbox))
    Just creds -> do
      api <- getClientAPI @MailboxAPI @UNIX

      let send payload = do
            let box = uncurry (makeSignedBox @HubScheme) (signingPair creds) payload
            callRpcWaitMay @RpcMailboxDeleteMessages rpcTimeout api box >>= \case
              Nothing        -> pure (Left DropPeerSilent)
              Just (Left e)  -> pure (Left (DropRefused (Text.pack (show e))))
              Just (Right _) -> pure (Right ())

      -- The batching is 'deleteNaming's and not this module's: how many messages
      -- one payload may name is what the READER accepts, and that number lives
      -- beside the reader.
      foldM (\acc payload -> either (pure . Left) (const (send payload)) acc)
            (Right ())
            (deleteNaming msgs)
