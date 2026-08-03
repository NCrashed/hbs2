-- | @hub inbox reject@ (PEP-21 "Retention", PEP-22 "Maintain").
--
-- The first verb in this build that deletes anything, and therefore the first
-- that has to obey the rule A2 settles: a hub does not delete an attachment
-- canon references. The fold already computes that set ('frParts'); this is the
-- first caller with a reason to consult it.
--
-- WHAT "DELETE" MEANS HERE, exactly, because the word promises more than the
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
-- only a canon key therefore cannot reject, which is the same
-- signing-is-not-publishing boundary the maintainer verbs describe.
--
-- REJECTING IS NOT CLOSING. A letter that was never folded has no canon thread,
-- so there is nothing to close and no event is written (PEP-22). Closing a
-- folded thread is @hub issue close@, and is a different act on a different
-- object.
module HBS2.Hub.CLI.Reject
  ( rejectEntries
  , rejectUsage
  , rejectArgs
  , Reject(..)
  , codeAlreadyFolded
  , codeNotRejected
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.Ingress (rpcTimeout)
import HBS2.Hub.CLI.Inbox (refuse,codePeerSilent,PeerSilent(..))
import HBS2.Hub.CLI.Verify (codeOf)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (pattern HashLike)
import HBS2.Data.Types.SignedBox (makeSignedBox)
import HBS2.Net.Auth.Credentials (_peerSignSk)
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)

import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,loadCredentials)

import Data.HashSet qualified as HS
import System.Exit (die)

-- | The letter is already in canon, so its parts are not this hub's to drop.
--
-- Its own code because it is the one refusal that is about canon rather than
-- about the mailbox, and because a script sweeping a queue wants to skip these
-- rather than stop.
codeAlreadyFolded :: Int
codeAlreadyFolded = 32

-- | Nothing was deleted.
codeNotRejected :: Int
codeNotRejected = 33

-- | What @hub inbox reject@ was asked to drop.
data Reject = Reject
  { rjMailbox :: HubKey
  , rjMessage :: HashRef
    -- | The repository whose canon to check first.
    --
    -- Optional, and the help says what skipping it costs: without a repo there
    -- is nothing to ask whether this letter was folded, so the check that
    -- protects canon's attachments cannot run.
  , rjRepo    :: Maybe RepoRef
  }
  deriving stock (Eq,Show)

rejectUsage :: Doc ()
rejectUsage =
  "usage: hbs2-hub inbox reject --mailbox <key> --message <hash> [--repo <key>]"

rejectEntries :: forall c m . ( IsContext c
                              , MonadUnliftIO m
                              , HasClientAPI MailboxAPI UNIX m
                              , Exception (BadFormException c)
                              ) => MakeDictM c m ()
rejectEntries = do

  brief "drop a letter from the ingress mailbox without folding it"
    $ args [ arg "string" "--mailbox mailbox-key"
           , arg "string" "--message message-hash" ]
    $ desc ( "Writes a tombstone into the mailbox: the queue stops showing"
             <> line <> "the letter. It does NOT free disk. Nothing in this build"
             <> line <> "walks a mailbox and deletes blocks, so the bytes stay"
             <> line <> "where they are and no space is reclaimed."
             <> line
             <> line <> "The MAILBOX key signs it. The peer takes the signer as"
             <> line <> "the mailbox being deleted from, so a repo key or a"
             <> line <> "delegate's canon key produces a delete against a mailbox"
             <> line <> "nobody has."
             <> line
             <> line <> "--repo is optional and worth giving: with it, a letter"
             <> line <> "already folded into canon is refused. Canon references"
             <> line <> "that letter's attachments forever and publishes the key"
             <> line <> "to them, so a hub that dropped them would leave every"
             <> line <> "clone with a reference it can see and cannot follow."
             <> line
             <> line <> "Rejecting is not closing. An unfolded letter has no canon"
             <> line <> "thread, so no event is written; closing a folded thread is"
             <> line <> "a different act on a different object." )
    $ entry $ bindMatch "hub:inbox:reject" $ nil_ \case
        (rejectArgs -> Just rj) -> lift (reject rj)
        _ -> liftIO (die (show rejectUsage))

  where

    reject rj = do
      -- Canon first, when there is a canon to ask. A letter already folded is
      -- refused before anything is signed: the refusal is about what canon
      -- promises, and asking after would mean having signed a delete for it.
      for_ (rjRepo rj) $ \repo ->
        withGitCanon (\cs -> readCanon cs repo) >>= \case
          Left NoCanonRef{} -> pure ()
          Left e -> liftIO (refuse (show (pretty e)) (codeOf e))
          Right st -> do
            let fr = stFold st
            when (HS.member (rjMessage rj) (frOrigins fr)) $
              liftIO $ refuse (show ( "this letter is already in canon; its"
                                        <+> "attachments are referenced there and"
                                        <+> "the key to them is published"
                                        <> line <> "  nothing was deleted" ))
                              codeAlreadyFolded

      creds <- runKeymanClientRO (loadCredentials (rjMailbox rj))
                 >>= maybe (liftIO (refuse (show ( "no signing key here for"
                                                     <+> pretty (AsBase58 (rjMailbox rj))
                                                     <> line
                                                     <> "  the mailbox's own key signs a"
                                                     <+> "delete; see --help" ))
                                           codeNotRejected))
                           pure

      api <- getClientAPI @MailboxAPI @UNIX

      -- One message and no more. The predicate language has And/Or, and a
      -- sweep is a thing an operator may want, but a verb that took a
      -- predicate would take one that can match more than the caller read.
      let payload = DeleteMessagesPayload
                      (MailboxMessagePredicate1 (Op (MessageHashEq (rjMessage rj))))
          box = makeSignedBox @HubScheme (rjMailbox rj) (_peerSignSk creds) payload

      callRpcWaitMay @RpcMailboxDeleteMessages rpcTimeout api box
        >>= maybe (liftIO (refuse (show (PeerSilent "the mailbox delete")) codePeerSilent))
                  pure
        >>= either (\e -> liftIO (refuse (show ("the peer refused the delete:"
                                                  <+> viaShow e))
                                         codeNotRejected))
                   pure

      liftIO $ print $ vcat
        [ "rejected" <+> pretty (rjMessage rj)
        , "a tombstone was written; the blocks are still on disk"
        , maybe ("canon was not consulted: no --repo was given")
                (const "canon does not hold this letter")
                (rjRepo rj)
        ]

-- Every value behind a flag, and the two keys are one type again.
rejectArgs :: forall c . [Syntax c] -> Maybe Reject
rejectArgs syn = do
  mbox <- flagged "--mailbox" asKey
  h    <- flagged "--message" asHash
  pure (Reject mbox h (flagged "--repo" asKey))
  where
    flagged :: forall v . String -> (Syntax c -> Maybe v) -> Maybe v
    flagged n f = case [ v | (StringLike n', f -> Just v) <- zip syn (drop 1 syn)
                           , n' == n ] of
                    [v] -> Just v
                    _   -> Nothing

    asKey  = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    asHash = \case { HashLike x -> Just x ; _ -> Nothing }
