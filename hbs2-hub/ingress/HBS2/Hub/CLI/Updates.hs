-- | @hub updates@: the contributor's end of the back-channel (PEP-18, PEP-22).
--
-- The other half of what @hub inbox accept@ now sends. A contributor puts a
-- letter in somebody else's mailbox and can see nothing afterwards: canon is
-- the authority and canon is in the maintainer's repository, so until they
-- fetch it, "not folded yet" and "refused" look the same. This reads their own
-- mailbox and prints what came back.
--
-- WHAT AN ACK IS WORTH, exactly. It carries no inner box and is never folded,
-- so the only trust available is who signed the envelope: a maintainer of the
-- repository the ack names, checked against that repository's canon in the
-- clone this runs in ('openAckNoPolicy'). An ack signed by anybody else is
-- shown as refused and counted, because a mailbox is public and a stranger can
-- send anything into it. Even a trusted one is a courtesy: canon is what a
-- reader believes if the two disagree.
--
-- AND THE OTHER HALF PEP-18 NAMES: whether the thread an ack talks about is one
-- this node submitted ('openAckFor'). Nothing in the ack ties its thread to its
-- target -- a thread-id is the hash of an author box the reader may never have
-- seen -- so a maintainer of any repository can sign a valid ack about a thread
-- in it that you never touched. The set comes from "HBS2.Hub.Sent", the log the
-- compose verbs write.
--
-- An ack that fails THAT check is not refused, it is set aside: the signer is a
-- maintainer, so it is a real statement about a real thread, and it is simply
-- not about you. @--all@ shows those too, for the reader who has just lost their
-- log or is reading a mailbox from another machine.
module HBS2.Hub.CLI.Updates
  ( updatesEntries
  , updatesUsage
    -- * The parts that decide something
  , Update(..)
  , updatesDoc
  , updatesNotes
  , UpdatesArgs(..)
  , updatesArgs
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold (frMaintainers)
import HBS2.Hub.Letter
import HBS2.Hub.Repo (readCanon,stFold)
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.Ingress
import HBS2.Hub.Sent (loadSent,submittedBy,codeNoSentLog)
import HBS2.Hub.CLI.Argv (flagsAndSwitches,flagOnce,flagSwitch,repoFlags,flagRepo,flagRepoMaybe)
import HBS2.Hub.CLI.Common (overRpc,refuse,saying,codeMailboxUnknown,codePeerSilent,withCanon,OnMissing(..))
import HBS2.Hub.CLI.Verify (codeOf)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import Data.HashSet qualified as HS
import System.Exit (die,exitWith,ExitCode(..))
import System.IO.Error (isResourceVanishedError)

-- | One message in the reader's own mailbox, as this verb sees it.
data Update =
    -- | An acknowledgement signed by a maintainer of the repository it names,
    -- about a thread this node's log says it submitted.
    Acked HashRef AckRecord
    -- | The same, about a thread the log does not have. A real statement by a
    -- real maintainer that is not about you (PEP-18 'openAckFor'), or an ack
    -- about a letter sent from another machine, or one whose log was lost.
  | Unrelated HashRef AckRecord
    -- | A message that is an ack and is not one this reader will believe: the
    -- signer is not a maintainer, the version is not one this build reads, or
    -- the bytes did not open at all. Counted rather than printed, because a
    -- public mailbox is a place anybody can put anything and a report that
    -- listed every one of them would be a report a stranger controls.
  | Refused
    -- | A letter, in a mailbox this reader uses for replies. Not an error:
    -- somebody may perfectly well write to you directly.
  | NotAnAckHere
  deriving stock (Eq,Show)

updatesUsage :: Doc ()
updatesUsage =
  "usage: hbs2-hub updates --mailbox <own-key> --repo <repo-key> [--all]"

updatesEntries :: forall c m . ( IsContext c
                               , MonadUnliftIO m
                               , HasStorage m
                               , HasClientAPI MailboxAPI UNIX m
                               , Exception (BadFormException c)
                               ) => MakeDictM c m ()
updatesEntries = do

  brief "read the acknowledgements a hub sent back to you"
    $ args [ arg "string" "--mailbox own-mailbox-key"
           , arg "string" "--repo repo-key" ]
    $ desc ( "Read-only. Opens your own mailbox and prints the"
             <> line <> "acknowledgements in it: the number a thread got, its"
             <> line <> "status, and the maintainer's note when there was one."
             <> line
             <> line <> "--repo is not optional and cannot be: an ack is trusted"
             <> line <> "only when a maintainer of the repository it names signed"
             <> line <> "it, and the maintainer set comes from that repository's"
             <> line <> "canon, in the clone you are standing in. Without one"
             <> line <> "there is nothing to check against, and an unchecked ack"
             <> line <> "is a sentence any stranger can put in your mailbox."
             <> line
             <> line <> "An ack is a COURTESY, not authority. Canon is the record;"
             <> line <> "this is the maintainer telling you what it says, so that"
             <> line <> "you do not have to fetch to find out. If the two ever"
             <> line <> "disagree, canon wins and the ack was noise."
             <> line
             <> line <> "An ack about a thread this node has no record of sending"
             <> line <> "is set aside rather than shown: nothing in an ack ties its"
             <> line <> "thread to its target, so a maintainer of this repository"
             <> line <> "can sign a true one about somebody else's submission. The"
             <> line <> "record is the log the compose verbs write. --all shows"
             <> line <> "those too, which is what you want after losing the log or"
             <> line <> "when reading this mailbox from another machine." )
    $ entry $ bindMatch "hub:updates" $ nil_ \case
        (updatesArgs -> Just ua) -> lift (updates ua)
        _ -> liftIO (die (show updatesUsage))

  where

    updates ua = do
      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX

      -- The maintainer set, out of the repository this runs in. Read BEFORE the
      -- mailbox: a run that cannot check an ack should say so instead of
      -- printing a queue and deciding what to do about it afterwards.
      -- TreatAsEmpty: refusing to run would refuse to show the one thing a
      -- contributor waiting for a first answer wants. What this used to do was
      -- branch on @codeOf e == 3@ -- control flow keyed on a number out of the
      -- PRESENTATION table, whose own comment says the numbers are a contract
      -- about stability, not a set of constructors. Reorder that table and this
      -- breaks silently.
      --
      -- It also used to answer @mempty@, and the fold of no events does not: the
      -- owner is a maintainer BY DEFINITION rather than by any event, so the
      -- repository key is in the set. That is the honest answer and a small
      -- behaviour change -- an ack signed by the owner of a repository whose
      -- canon this node has never fetched now validates, which it should: the
      -- owner key is the authority the ack is checked against.
      maint <- frMaintainers . snd <$> withCanon TreatAsEmpty (uaRepo ua) withGitCanon

      -- The log this node keeps of its own letters. A damaged one stops the
      -- run, unlike a missing one: reading on with an empty log would set every
      -- ack aside as somebody else's, which is a report that looks like an
      -- answer ("nothing for you") and is not one.
      sent <- loadSent
                >>= either (\e -> liftIO (refuse (show ("the sent log will not read:"
                                                          <+> pretty (safeText e)))
                                                 codeNoSentLog))
                           pure

      let ig = overRpc sto api

      r <- readInbox ig (uaMailbox ua)
             `catch` (\(e :: MailboxUnknown) -> liftIO (refuse (show e) codeMailboxUnknown))
             `catch` (\(e :: PeerSilent)     -> liftIO (refuse (show e) codePeerSilent))

      -- One read of each message, opened as an ack rather than as a letter.
      -- 'readInbox' already opened them as letters, which for an ack fails with
      -- NotALetter; what is wanted here is the other reading of the same bytes.
      us <- for (irLetters r) $ \lv -> do
              raw <- rawMessage ig (lvMessage lv)
              pure (updateOf maint (submittedBy sent) lv raw)

      liftIO $ handleJust
        (\e -> if isResourceVanishedError e then Just () else Nothing)
        (\_ -> exitWith (ExitFailure 141))
        (mapM_ print (updatesDoc (uaAll ua) us))

      liftIO $ for_ (updatesNotes (uaAll ua) (HS.null maint) us) (saying . (<> line))

-- | What one message turned out to be.
--
-- Pure and top level for the reason every decision in this package is: this one
-- says whether a stranger's message is shown as a maintainer's word.
updateOf :: HS.HashSet HubKey
         -> (RepoRef -> ThreadId -> Bool)   -- ^ did this node submit that thread?
         -> LetterView
         -> Either OpenError LetterRaw
         -> Update
updateOf maint isMine lv = \case
  Left _ -> Refused
  Right raw ->
    case openAckFor isMaintainer isMine (EnvelopeSigner (lrEnvelope raw)) (lrData raw) of
      Right a -> Acked (lvMessage lv) a
      -- Told apart because they are three different things happening: a letter
      -- in your mailbox is somebody writing to you, an unrelated ack is a
      -- maintainer's true statement about somebody else's thread, and the rest
      -- is a message this reader will not believe.
      Left NotAnAck -> NotAnAckHere
      Left UnrelatedAck ->
        -- Opened AGAIN, without the relation check, purely to carry the record
        -- for --all. It costs nothing (the bytes are in hand and the check is
        -- a set lookup) and the alternative is a flag that can only report a
        -- count.
        case openAckNoPolicy isMaintainer (EnvelopeSigner (lrEnvelope raw)) (lrData raw) of
          Right a -> Unrelated (lvMessage lv) a
          Left _  -> Refused
      Left _ -> Refused
  where
    -- The repo the ack names is checked against the maintainer set of the repo
    -- the CALLER named, which is the only set this node read. An ack about
    -- another repository is refused rather than trusted, because its maintainer
    -- set is not in front of us.
    isMaintainer _ k = HS.member k maint

-- | The acks, for stdout.
--
-- The flag widens what is PRINTED and changes nothing about what was checked:
-- an unrelated ack is marked as unrelated on its own line, so a report read
-- with @--all@ and one read without cannot be confused for each other.
updatesDoc :: Bool -> [Update] -> [Doc ann]
updatesDoc showAll us =
     [ one m a mempty | Acked m a <- us ]
  <> [ one m a (" (not a thread this node sent; --all)") | showAll, Unrelated m a <- us ]
  where
    one m a mark =
      hashDoc m
        <+> "thread" <+> hashDoc (akThread a)
        <+> maybe "-" (\n -> "#" <> pretty n) (akNumber a)
        -- Through 'safeText': the status and the note are a maintainer's words
        -- and this is a terminal, and a maintainer is not this node.
        <+> "status" <+> pretty (safeText (akStatus a))
        <> maybe mempty (\c -> " merged" <+> pretty (safeText c)) (akMergeCommit a)
        <> mark
        <> maybe mempty (\n -> line <> "  note:" <+> pretty (safeText n)) (akNote a)

-- | What is true about that list, for stderr.
updatesNotes :: Bool -> Bool -> [Update] -> [Doc ann]
updatesNotes showAll noMaintainers us =
  emptyNote <> maintNote <> unrelatedNote <> refusedNote <> letterNote <> alwaysNote
  where
    acked = length [ () | Acked{} <- us ]
    unrelated = length [ () | Unrelated{} <- us ]
    refused = length [ () | Refused <- us ]
    letters = length [ () | NotAnAckHere <- us ]

    -- The case that looks like silence and is not: nothing was shown, and the
    -- mailbox has things in it.
    emptyNote
      | acked == 0, refused + unrelated > 0 =
          [ "hub: nothing above, and" <+> pretty (refused + unrelated)
              <+> "message(s) in this mailbox were not shown. That is not an"
              <+> "empty mailbox." ]
      | otherwise = []

    -- Its own note and not folded into the refusals, because it is the one a
    -- reader can act on: these are real acks and the log is what did not match.
    unrelatedNote
      | unrelated > 0, not showAll =
          [ "hub:" <+> pretty unrelated <+> "acknowledgement(s) are about threads"
              <+> "this node has no record of sending, so they were set aside"
              <+> "(PEP-18: an ack does not tie its thread to its target, so a"
              <+> "maintainer can sign a true one about somebody else's). If you"
              <+> "sent from another machine, or lost ~/.local/share/hbs2-hub/sent,"
              <+> "that is what this looks like: --all shows them." ]
      | otherwise = []

    maintNote
      | noMaintainers =
          [ "hub: this clone has no canon for that repository, so it knows no"
              <+> "maintainers and every acknowledgement is refused. Fetch"
              <+> "refs/hbs2/meta first." ]
      | otherwise = []

    refusedNote
      | refused > 0, acked > 0 =
          [ "hub:" <+> pretty refused <+> "message(s) were not shown: an"
              <+> "acknowledgement this reader will not believe (signed by a key"
              <+> "that is not a maintainer of that repository, or a version this"
              <+> "build does not read). A mailbox is public." ]
      | otherwise = []

    letterNote
      | letters > 0 =
          [ "hub:" <+> pretty letters <+> "letter(s) are in this mailbox, which"
              <+> "this verb does not show. `hub inbox` reads letters." ]
      | otherwise = []

    -- Said whenever anything was shown, because it is the thing a reader will
    -- otherwise assume the opposite of: this is a notification and not a
    -- record, and the record is a fetch away.
    alwaysNote
      | acked + (if showAll then unrelated else 0) > 0 =
          [ "hub: an acknowledgement is a courtesy and canon is the record. An"
              <+> "old one replays verbatim -- it carries no clock and no counter"
              <+> "-- so what it is good for is knowing when to go and look." ]
      | otherwise = []

-- | @--mailbox <key> --repo <key> [--all]@.
data UpdatesArgs = UpdatesArgs
  { uaMailbox :: HubKey    -- ^ the reader's OWN mailbox
  , uaRepo    :: RepoRef   -- ^ whose maintainer set to check acks against
    -- | Show acks about threads this node has no record of sending.
  , uaAll     :: Bool
  }
  deriving stock (Eq,Show)

updatesArgs :: forall c . IsContext c => [Syntax c] -> Maybe UpdatesArgs
updatesArgs syn = do
  kvs  <- flagsAndSwitches (repoFlags <> ["--mailbox"]) ["--all"] syn
  mbox <- flagOnce kvs "--mailbox" >>= asKey
  -- Required, unlike everywhere else this flag appears: it is what an ack is
  -- checked against, and an unchecked ack is not worth printing.
  repo <- flagRepo asKey kvs
  showAll <- flagSwitch kvs "--all"
  pure (UpdatesArgs mbox repo showAll)
  where
    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
