-- | @hub inbox reject@ (PEP-21 "Retention", PEP-22 "Maintain").
--
-- The verb for a letter a maintainer will not fold: it drops it from the
-- mailbox and writes nothing to canon, which is the whole difference between it
-- and @hub inbox accept@.
--
-- WHAT "DELETE" MEANS HERE, and who signs it, is "HBS2.Hub.CLI.Drop": this verb
-- is that one act plus the two questions worth asking before it.
--
-- REJECTING IS NOT CLOSING. A letter that was never folded has no canon thread,
-- so there is nothing to close and no event is written (PEP-22). Closing a
-- folded thread is @hub issue close@, and is a different act on a different
-- object.
--
-- AND REJECTING IS NOT ACCEPTING'S CLEANUP. A folded letter is refused here
-- (exit 32) while @hub inbox accept@ drops the same letter itself, and the two
-- are not in tension: the tombstone is identical, the meaning is not. Rejecting
-- says this was not taken, and saying that about something canon holds is a
-- claim the mailbox cannot carry and the operator probably did not mean.
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
import HBS2.Hub.CLI.Drop (dropMessage,DropTrouble(..))
import HBS2.Hub.Ingress (PeerSilent(..),copiesOf,openMessage,LetterView(..))
import HBS2.Hub.Bridge (originFits,viewOf)
import HBS2.Hub.CLI.Common (overRpc)
import HBS2.Hub.CLI.Common (refuse,codePeerSilent,withCanon,OnMissing(..))
import HBS2.Hub.CLI.Argv (flagsOf,flagOnce,repoFlags,flagRepo)
import HBS2.Hub.CLI.Verify (codeOf)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Storage
import HBS2.Peer.RPC.Client.Unix (UNIX)


import Data.HashSet qualified as HS
import Data.List qualified as List
import System.Exit (die)

-- | The letter is already in canon, so "rejected" is not what happened to it.
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
    -- REQUIRED, and it was optional. Without it there is nothing to ask whether
    -- this letter was already folded, so the one check this verb makes ran only
    -- when the caller happened to pass a flag they were told was optional -- and
    -- rejecting says the letter was not taken, which is a false sentence about
    -- something canon holds. `hub inbox accept` has always required it; the two
    -- verbs decide about the same letter and had different standards for it.
    --
    -- Made required before a release rather than after, because afterwards it
    -- is a break.
  , rjRepo    :: RepoRef
  }
  deriving stock (Eq,Show)

rejectUsage :: Doc ()
rejectUsage =
  "usage: hbs2-hub inbox reject --mailbox <key> --message <hash> [--repo <key>]"

rejectEntries :: forall c m . ( IsContext c
                              , MonadUnliftIO m
                              , HasStorage m
                              , HasClientAPI MailboxAPI UNIX m
                              , Exception (BadFormException c)
                              ) => MakeDictM c m ()
rejectEntries = do

  brief "drop a letter from the ingress mailbox without folding it"
    $ args [ arg "string" "--repo repo-key"
           , arg "string" "--mailbox mailbox-key"
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
             <> line <> "--repo is REQUIRED: a letter already folded into canon"
             <> line <> "is refused here. Not because the tombstone would differ"
             <> line <> "(accept writes the same one), but because rejecting says"
             <> line <> "the letter was not taken, and canon says it was. It was"
             <> line <> "optional, which made the one check this verb makes run"
             <> line <> "only when the caller happened to pass the flag."
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
      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX
      let ig = overRpc sto api

      do
        let repo = rjRepo rj
        -- TreatAsEmpty, like `inbox show`: the question is whether canon already
        -- holds this letter, and no canon is no.
        withCanon TreatAsEmpty repo withGitCanon >>= \(_, fr) -> do
          -- AND THE CLAIM HAS TO FIT THIS LETTER. `origin` is unverifiable (see
          -- 'originFits'), so an authorized key could put any message hash into
          -- canon and every verb that reads the set would then say the letter in
          -- it was folded. Here that is a sentence rather than a block, and the
          -- sentence would be false: the accept path stopped believing such a
          -- claim, so this must not go on believing it either, or the two verbs
          -- disagree about one letter.
          mine <- lvEventId <$> openMessage ig (rjMessage rj)
          do
            when (HS.member (rjMessage rj) (frOrigins fr)
                    && maybe False (originFits (viewOf fr)) mine) $
              liftIO $ refuse (show ( "this letter is already in canon: rejecting"
                                        <+> "is not what happened to it"
                                        <> line <> "  nothing was deleted."
                                        <+> "`hub inbox accept` drops a letter when"
                                        <+> "it folds it; if this one stayed (--keep,"
                                        <+> "or no mailbox key at the time) it is a"
                                        <+> "queue entry and not a decision:"
                                        <> line <> "    hbs2-peer mailbox delete:message"
                                        <+> pretty (AsBase58 (rjMailbox rj))
                                        <+> pretty (rjMessage rj) ))
                              codeAlreadyFolded

      -- AND THE COPIES OF IT, which used to be a decision each. A rewrap needs
      -- no key at all -- 'lvCopies' has the mechanism -- so the same letter
      -- arrives under any number of envelopes, and a tombstone is written by
      -- message hash, so rejecting one copy rejected one copy. The maintainer
      -- read the letter once and should decide about it once.
      --
      -- Bounded by the page the queue shows, because that is the set this can
      -- know without an unbounded walk, and because it is the set the operator
      -- was looking at when they typed this.
      copies <- copiesOf ig (rjMailbox rj) (rjMessage rj)

      for_ (rjMessage rj : copies) $ \h ->
        -- The peer is silent for the reason 'PeerSilent' says and NOT for the
        -- reason a missing key is, so the three answers keep their own codes.
        dropMessage (rjMailbox rj) h >>= \case
          Right () -> pure ()
          Left DropPeerSilent -> liftIO (refuse (show (PeerSilent "the mailbox delete"))
                                                codePeerSilent)
          Left e -> liftIO (refuse (show (pretty e)) codeNotRejected)

      liftIO $ print $ vcat
        ( [ "rejected" <+> pretty (rjMessage rj) ]
       <> [ "and" <+> pretty (length copies)
              <+> "copy(ies) of it under other envelopes:"
              <> line <> indent 2 (vcat (fmap pretty copies))
          | not (List.null copies) ]
       <> [ "a tombstone was written; the blocks are still on disk"
          , "canon does not hold this letter"
          ] )

-- Every value behind a flag, and the two keys are one type again.
rejectArgs :: forall c . IsContext c => [Syntax c] -> Maybe Reject
rejectArgs syn = do
  kvs  <- flagsOf (repoFlags <> ["--mailbox","--message"]) syn
  mbox <- flagOnce kvs "--mailbox" >>= asKey
  h    <- flagOnce kvs "--message" >>= asHash
  repo <- flagRepo asKey kvs
  pure (Reject mbox h repo)
  where
    asKey  = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    asHash = \case { HashLike x -> Just x ; _ -> Nothing }
