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
-- WHAT THIS BUILD CANNOT CHECK is the other half PEP-18 names: whether the
-- thread an ack talks about is one this node ever submitted ('openAckFor').
-- That needs a record of what was sent, and the compose verbs keep none -- they
-- print the two hashes and forget. So a maintainer of a repository you follow
-- can tell you about a thread you never opened, and the honest answer is to say
-- so on stderr rather than to imply a check that did not run.
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
import HBS2.Hub.CLI.Argv (flagsOf,flagOnce)
import HBS2.Hub.CLI.Inbox (overRpc,refuse,saying,codeMailboxUnknown,codePeerSilent
                          ,PeerSilent)
import HBS2.Hub.CLI.Verify (codeOf)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import Data.HashSet qualified as HS
import Data.List qualified as List
import System.Exit (die,exitWith,ExitCode(..))
import System.IO.Error (isResourceVanishedError)

-- | One message in the reader's own mailbox, as this verb sees it.
data Update =
    -- | An acknowledgement signed by a maintainer of the repository it names.
    Acked HashRef AckRecord
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
  "usage: hbs2-hub updates --mailbox <own-key> --repo <repo-key>"

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
             <> line <> "What this cannot check: whether the thread an ack talks"
             <> line <> "about is one you submitted. That needs a record of what"
             <> line <> "you sent, and this build keeps none." )
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
      maint <- withGitCanon (\cs -> readCanon cs (uaRepo ua)) >>= \case
        Right st -> pure (frMaintainers (stFold st))
        -- Not a failure: a repository whose canon nobody has fetched has an
        -- empty maintainer set, so every ack is refused and the note below says
        -- why. Refusing to run would be refusing to show the one thing a
        -- contributor waiting for a first answer would want.
        Left e | codeOf e == 3 -> pure mempty
        Left e -> liftIO (refuse (show (pretty e)) (codeOf e))

      let ig = overRpc sto api

      r <- readInbox ig (uaMailbox ua)
             `catch` (\(e :: MailboxUnknown) -> liftIO (refuse (show e) codeMailboxUnknown))
             `catch` (\(e :: PeerSilent)     -> liftIO (refuse (show e) codePeerSilent))

      -- One read of each message, opened as an ack rather than as a letter.
      -- 'readInbox' already opened them as letters, which for an ack fails with
      -- NotALetter; what is wanted here is the other reading of the same bytes.
      us <- for (irLetters r) $ \lv -> do
              raw <- rawMessage ig (lvMessage lv)
              pure (updateOf maint lv raw)

      liftIO $ handleJust
        (\e -> if isResourceVanishedError e then Just () else Nothing)
        (\_ -> exitWith (ExitFailure 141))
        (mapM_ print (updatesDoc us))

      liftIO $ for_ (updatesNotes (HS.null maint) us) (saying . (<> line))

-- | What one message turned out to be.
--
-- Pure and top level for the reason every decision in this package is: this one
-- says whether a stranger's message is shown as a maintainer's word.
updateOf :: HS.HashSet HubKey -> LetterView -> Either OpenError LetterRaw -> Update
updateOf maint lv = \case
  Left _ -> Refused
  Right raw ->
    case openAckNoPolicy isMaintainer (EnvelopeSigner (lrEnvelope raw)) (lrData raw) of
      Right a -> Acked (lvMessage lv) a
      -- Told apart because they are different things happening: a letter in
      -- your mailbox is somebody writing to you, and everything else is a
      -- message this reader will not believe.
      Left NotAnAck -> NotAnAckHere
      Left _ -> Refused
  where
    -- The repo the ack names is checked against the maintainer set of the repo
    -- the CALLER named, which is the only set this node read. An ack about
    -- another repository is refused rather than trusted, because its maintainer
    -- set is not in front of us.
    isMaintainer _ k = HS.member k maint

-- | The acks, for stdout.
updatesDoc :: [Update] -> [Doc ann]
updatesDoc us = [ one m a | Acked m a <- us ]
  where
    one m a =
      hashDoc m
        <+> "thread" <+> hashDoc (akThread a)
        <+> maybe "-" (\n -> "#" <> pretty n) (akNumber a)
        -- Through 'safeText': the status and the note are a maintainer's words
        -- and this is a terminal, and a maintainer is not this node.
        <+> "status" <+> pretty (safeText (akStatus a))
        <> maybe mempty (\c -> " merged" <+> pretty (safeText c)) (akMergeCommit a)
        <> maybe mempty (\n -> line <> "  note:" <+> pretty (safeText n)) (akNote a)

-- | What is true about that list, for stderr.
updatesNotes :: Bool -> [Update] -> [Doc ann]
updatesNotes noMaintainers us = emptyNote <> maintNote <> refusedNote <> letterNote <> alwaysNote
  where
    acked = length [ () | Acked{} <- us ]
    refused = length [ () | Refused <- us ]
    letters = length [ () | NotAnAckHere <- us ]

    -- The case that looks like silence and is not: every ack was refused, so
    -- the mailbox is not empty and the report is.
    emptyNote
      | acked == 0, refused > 0 =
          [ "hub: nothing above, and" <+> pretty refused <+> "message(s) in this"
              <+> "mailbox were not shown. That is not an empty mailbox." ]
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

    -- Said every time, including when everything above is quiet, because it is
    -- the thing a reader will otherwise assume the opposite of.
    alwaysNote
      | acked > 0 =
          [ "hub: an acknowledgement is a courtesy and canon is the record. This"
              <+> "does not check that a thread it names is one you submitted:"
              <+> "that needs a log of what you sent, which this build does not"
              <+> "keep." ]
      | otherwise = []

-- | @--mailbox <key> --repo <key>@.
data UpdatesArgs = UpdatesArgs
  { uaMailbox :: HubKey    -- ^ the reader's OWN mailbox
  , uaRepo    :: RepoRef   -- ^ whose maintainer set to check acks against
  }
  deriving stock (Eq,Show)

updatesArgs :: forall c . IsContext c => [Syntax c] -> Maybe UpdatesArgs
updatesArgs syn = do
  kvs  <- flagsOf ["--mailbox","--repo"] syn
  mbox <- flagOnce kvs "--mailbox" >>= asKey
  -- Required, unlike everywhere else this flag appears: it is what an ack is
  -- checked against, and an unchecked ack is not worth printing.
  repo <- flagOnce kvs "--repo" >>= asKey
  pure (UpdatesArgs mbox repo)
  where
    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
