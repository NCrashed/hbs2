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
import HBS2.Hub.CLI.Drop (dropMessages,DropTrouble(..))
import HBS2.Hub.Ingress (PeerSilent(..),copiesOf,openMessage,LetterView(..)
                        ,rawMessage,LetterRaw(..),rpcTimeout)
import HBS2.Hub.Letter (AckRecord(..),openLetterAs,EnvelopeSigner(..))
import HBS2.Hub.CLI.Ack (sendAck,AckTrouble(..))
import HBS2.Hub.CLI.Compose (Outbound(..))
import HBS2.Hub.Bridge (originFits,viewOf)
import HBS2.Hub.CLI.Common (overRpc,refuse,saying,signerFor,codePeerSilent
                           ,withCanon,OnMissing(..))
import HBS2.Hub.CLI.Argv (badArgs,flagOnce,flagMaybe,flagsAndSwitches,flagSwitch
                         ,repoFlags,flagRepo)
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
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
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
    -- | The canon key that signs the acknowledgement. Defaults to the repo key.
  , rjAs      :: Maybe HubKey
    -- | Do not tell the sender.
    --
    -- The escape for the case rejecting is most often FOR: a letter nobody
    -- wants answered. An ack confirms the address is live and that somebody
    -- read it, which is the one thing a flood is looking for.
  , rjSilent  :: Bool
  }
  deriving stock (Eq,Show)

rejectUsage :: Doc ()
rejectUsage =
  "usage: hbs2-hub inbox reject --repo <key> --mailbox <key> --message <hash> [--as <key>] [--silent]"

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
           , arg "string" "--message message-hash"
           , arg "string" "[--as canon-key]", arg "string" "[--silent]" ]
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
             <> line <> "a different act on a different object."
             <> line
             <> line <> "THE SENDER IS TOLD, when their letter asked for an answer"
             <> line <> "and named a mailbox of their own. Without it a refusal"
             <> line <> "and a letter nobody has looked at are the same silence"
             <> line <> "on their side, forever: canon they can fetch says"
             <> line <> "nothing about a letter that never entered it."
             <> line
             <> line <> "That acknowledgement cannot be checked against anything."
             <> line <> "An accept's can: whoever gets it can fold canon and see"
             <> line <> "the event. This one is a claim about something canon"
             <> line <> "does not hold, so all a reader establishes is that a"
             <> line <> "maintainer of the repository signed it."
             <> line
             <> line <> "--silent skips it, which is the case rejecting is most"
             <> line <> "often for: an ack confirms the address is live and that"
             <> line <> "somebody read it, and a flood is looking for exactly"
             <> line <> "that. --as names the canon key that signs it (PEP-21);"
             <> line <> "the MAILBOX key still signs the delete." )
    $ entry $ bindMatch "hub:inbox:reject" $ nil_ \case
        (rejectArgs -> Just rj) -> lift (reject rj)
        other -> liftIO (badArgs rejectUsage other)

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

      -- ONE DELETE FOR THE WHOLE SET, and it used to be one per copy. A delete
      -- names a set since PEP-23 step B, so a letter arriving under a dozen
      -- envelopes is a dozen tombstones bought with one signature and one packet
      -- rather than a dozen of each.
      --
      -- The peer is silent for the reason 'PeerSilent' says and NOT for the
      -- reason a missing key is, so the three answers keep their own codes.
      dropMessages (rjMailbox rj) (rjMessage rj : copies) >>= \case
        Right () -> pure ()
        Left DropPeerSilent -> liftIO (refuse (show (PeerSilent "the mailbox delete"))
                                              codePeerSilent)
        Left e -> liftIO (refuse (show (pretty e)) codeNotRejected)

      -- AND THE CONTRIBUTOR IS TOLD, which is the half of a decision this tool
      -- did not have. `sendAck` had one caller -- the accept -- so a letter that
      -- was refused and a letter nobody had looked at were the same silence on
      -- the sender's side, forever: there is no state on their machine that
      -- changes, and canon they can fetch says nothing about a letter that
      -- never entered it.
      --
      -- UNVERIFIABLE BY CONSTRUCTION, and that is worth being plain about. An
      -- accept's ack can be checked against canon by whoever gets it; this one
      -- is a claim about something canon does not hold, so all a reader can
      -- establish is that a maintainer of the repository signed it. It is a
      -- courtesy notification, like the other one, and canon remains the
      -- authority for everything canon has.
      --
      -- AFTER the drop, for the reason the accept's ack is after the commit:
      -- the work is done and a notification that does not go out costs a
      -- notification.
      acked <- if rjSilent rj then pure (Left AckNotAsked) else tell rj
      liftIO $ for_ (either Just (const Nothing) acked) $ \why ->
        saying ("the sender was not told:" <+> pretty why <> line)

      liftIO $ print $ vcat
        ( [ "rejected" <+> pretty (rjMessage rj) ]
       <> [ "and" <+> pretty (length copies)
              <+> "copy(ies) of it under other envelopes:"
              <> line <> indent 2 (vcat (fmap pretty copies))
          | not (List.null copies) ]
       <> [ "a tombstone was written; the blocks are still on disk"
          , "canon does not hold this letter"
          ]
       <> [ "the sender was told:" <+> pretty h | Right h <- [acked] ] )

    -- The acknowledgement itself, which needs a key of its own: the DROP above
    -- is signed by the mailbox key, and an ack is signed by the canon key,
    -- because what makes it checkable is that a maintainer of the repository
    -- signed it (PEP-18). Two keys, two questions, and a node that holds the
    -- first and not the second still rejects -- it just cannot say so.
    tell rj = do
      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX
      let ig = overRpc sto api
          canonKey = fromMaybe (rjRepo rj) (rjAs rj)

      creds <- signerFor canonKey
      raw <- rawMessage ig (rjMessage rj)

      case (creds, raw) of
        (Nothing, _) -> pure (Left (AckNotSent ("no signing key here for "
                                                  <> tshow (pretty (AsBase58 canonKey)))))
        (_, Left e)  -> pure (Left (AckNotSent (tshow (pretty e))))
        (Just c, Right lr) ->
          case openLetterAs (const True) (EnvelopeSigner (lrEnvelope lr)) (lrData lr) of
            Left e -> pure (Left (AckNotSent (tshow (pretty e))))
            Right (box, _author, content, reply) -> do
              -- The thread this letter is about: the one it names, or -- for an
              -- open, which names none -- the thread it would have started.
              -- Both are what the sender computed before sending (PEP-18), so
              -- both are values they can correlate on.
              let thr = fromMaybe (authorBoxId box) (authorThread content)
                  rec' = AckRecord { akTarget = rjRepo rj
                                   , akThread = thr
                                     -- No number: numbers are minted by canon
                                     -- and this letter never reached it.
                                   , akNumber = Nothing
                                   , akStatus = "rejected"
                                   , akMergeCommit = Nothing
                                   , akNote = Nothing
                                   }
              sendAck (Outbound sto api rpcTimeout) canonKey c reply rec'

    tshow :: Show a => a -> Text
    tshow = Text.pack . show

-- Every value behind a flag, and the two keys are one type again.
rejectArgs :: forall c . IsContext c => [Syntax c] -> Maybe Reject
rejectArgs syn = do
  kvs  <- flagsAndSwitches (repoFlags <> ["--mailbox","--message","--as"])
                           ["--silent"] syn
  mbox <- flagOnce kvs "--mailbox" >>= asKey
  h    <- flagOnce kvs "--message" >>= asHash
  repo <- flagRepo asKey kvs
  as   <- flagMaybe kvs "--as" asKey
  q    <- flagSwitch kvs "--silent"
  pure (Reject mbox h repo as q)
  where
    asKey  = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    asHash = \case { HashLike x -> Just x ; _ -> Nothing }
