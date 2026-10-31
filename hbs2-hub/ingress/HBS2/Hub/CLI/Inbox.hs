-- The rendering below dispatches on attacker-composed content inside the
-- triage loop. A constructor added without a case is a build error rather than
-- a crash in that loop.
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | @hub inbox@: the triage queue, rendered (PEP-22 "Maintain").
--
-- Only the presentation and the argument handling. What reading a mailbox MEANS
-- is "HBS2.Hub.Ingress", which a test can reach.
--
-- Everything here that DECIDES something is exported and pure, for the reason
-- the ingress split out of the executable in the first place: while the
-- rendering and the exit code lived inside the verb, nothing tested them, and
-- both were wrong. A stranger's thread-id was printed with a bare 'pretty'
-- (quadratic base58 on a field nothing bounds), and a wrong-arity call answered
-- with a Haskell constructor name carrying the caller's unescaped bytes.
module HBS2.Hub.CLI.Inbox
  ( inboxEntries
    -- * The parts that decide something
    --
    -- Exported the way "HBS2.Hub.CLI.Verify" exports 'report' and 'refused',
    -- and for the same reason: a decision inside a @where@ clause is a decision
    -- nothing can ask about. The exit codes below are a contract PEP-22 writes
    -- down and a hook branches on, so the code that reaches them has to be
    -- reachable from a test too.
  , render
  , inboxDoc
  , queueContract
  , dispositionName
  , inboxNotes
  , inboxArgs
  , freshRows
  , FollowStopped(..)
  , FollowSession(..)
  , Follower(..)
  , FollowState(..)
  , followLoop
  , followGone
  , followBack
  , followBackoff
  , InboxArgs(..)
  , Width(..)
  , utcOf
  , inboxCode
  , inboxUsage
  , refuse
  , saying
  , codeMailboxUnknown
  , codePeerSilent
  , maxMissingLines
  , overRpc
  , manifestCode
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Render (contractVersion,renderContract,opName,declaredAt,b58,key58)
import HBS2.Hub.Ingress
import HBS2.Hub.PeerLink (PeerGone(..),withPeerLink,socketOf)
import HBS2.Hub.Deny (denyingFor,denyUnreadable,codeNoBanList)
import HBS2.Hub.Repo.Manifest (mailboxFor,ManifestGone(..),codeNoManifest)
import HBS2.Hub.CLI.Argv (badArgs,flagsOf,flagOnce,flagMaybe,flagWord,repoFlags,flagRepo,flagRepoMaybe
                         ,flagsAndSwitches,flagSwitch)

import HBS2.Hub.CLI.Common

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import HBS2.Net.Auth.GroupKeySymm (pattern ToDecryptBS)
import HBS2.Storage.Operations.Class (readFromMerkle)
import Crypto.Saltine.Class qualified as Saltine
import Control.Monad.Except (runExceptT)
import Data.Text qualified as Text

import HBS2.Base58 (AsBase58(..))
import Data.Aeson (Value,object,(.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Maybe (isJust,fromMaybe)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale,formatTime)
import Data.Word (Word64)
import System.Exit (die,exitWith,exitSuccess,ExitCode(..))
import System.IO.Error (isResourceVanishedError)
import System.Posix.Signals (installHandler,Handler(..),sigINT,sigTERM)
import Control.Concurrent (myThreadId)
import Control.Exception qualified as E
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS

-- | @hub inbox@ and friends.
inboxEntries :: forall c m . ( IsContext c
                             , MonadUnliftIO m
                             , HasStorage m
                             , HasClientAPI MailboxAPI UNIX m
                             , HasClientAPI LWWRefAPI UNIX m
                             , Exception (BadFormException c)
                             ) => MakeDictM c m ()
inboxEntries = do

  brief "list the letters waiting in a hub's ingress mailbox"
    $ args [ arg "string" "[--mailbox] mailbox-key", arg "string" "--repo repo-key"
           , arg "string" "[--after message-hash]", arg "string" "[--limit n]"
           , arg "string" "[--long]", arg "string" "[--json]"
           , arg "string" "[-f|--follow]" ]
    $ desc ( "Read-only. Waits for the peer's copy of the mailbox to settle,"
             <> line <> "opens every message this node holds a key for, and reports"
             <> line <> "what each one asks for. Nothing is folded, minted or deleted."
             <> line
             <> line <> "The wait costs one round (about 2.5s) when the peer already"
             <> line <> "has the mailbox, and also when nothing was ever merged into"
             <> line <> "it and the peer has nothing for it in flight: that mailbox"
             <> line <> "is empty and the list is empty. The peer writes a mailbox"
             <> line <> "ref only once a merge lands, so it is the peer that tells"
             <> line <> "'empty' from 'still arriving'. While a copy is still"
             <> line <> "downloading, and with a peer too old to say, the wait is up"
             <> line <> "to seven more rounds and an empty answer then says it could"
             <> line <> "not tell."
             <> line
             <> line <> "The peer must already hold this mailbox: it only asks the"
             <> line <> "network about mailboxes it has locally, so an arbitrary key"
             <> line <> "reads as empty forever. Create one with"
             <> line <> "'hbs2-peer mailbox create --key KEY hub'."
             <> line
             <> line <> "--mailbox names the key directly. Without it, --repo is"
             <> line <> "read: the repository manifest declares its ingress mailbox"
             <> line <> "(PEP-18), so a maintainer who knows the repository does not"
             <> line <> "have to know the mailbox. One of the two has to be given."
             <> line
             <> line <> "--repo names whose deny-list to apply (hub ban), which is"
             <> line <> "this node's own state keyed by repository and not anything"
             <> line <> "the mailbox knows. Without it the queue is unfiltered and"
             <> line <> "says so: a banned author's letter is in the list with the"
             <> line <> "same \"(folds)\" as anyone else's. With it, a letter whose"
             <> line <> "author IS on the list is left out and counted."
             <> line
             <> line <> "THE LIST IS A PAGE, in hash order, and a mailbox is public:"
             <> line <> "which letters land on the first page is a stranger's choice,"
             <> line <> "since grinding hashes below the honest ones is cheap. When"
             <> line <> "the page runs out the report prints the --after value that"
             <> line <> "continues it. --limit lowers how many are opened and cannot"
             <> line <> "raise it past what one read will take."
             <> line
             <> line <> "--long expands each letter into a labelled block with whole"
             <> line <> "identifiers and, when it can be accepted, a ready-to-copy"
             <> line <> "'hbs2-hub inbox accept --message ...' command. --json is the"
             <> line <> "stable document form for scripts."
             <> line
             <> line <> "--follow (-f) prints the queue and keeps running. It asks"
             <> line <> "the peer for the mailbox root every few seconds, asks it to"
             <> line <> "fetch once a minute, and reads the queue again only when"
             <> line <> "the root moved. Each read prints the rows not printed"
             <> line <> "before in this run, and a row again when what it says"
             <> line <> "changed: a letter that had not downloaded opens, or another"
             <> line <> "copy of it turns up. A letter that leaves the queue prints"
             <> line <> "nothing. Every line is flushed as it is written, and with"
             <> line <> "--json each read is one document on one line, holding only"
             <> line <> "those letters. Still read-only. A note on stderr is said"
             <> line <> "when it first applies, not on every read. SIGINT and"
             <> line <> "SIGTERM end it with 0; a peer that stops answering or a"
             <> line <> "mailbox it no longer holds end it with their own codes."
             <> line <> "A peer that restarts does not: it says once on stderr that"
             <> line <> "the peer is gone, reconnects with a backoff, says once that"
             <> line <> "it is back, and reads the queue again, printing only what"
             <> line <> "it has not printed." )
    -- Both spellings, because PEP-22 specifies the flag: the bare key is what
    -- the spec calls `--mailbox <key>`, and a form the spec names has to be
    -- accepted under that name or the divergence has merely moved.
    $ entry $ bindMatch "hub:inbox" $ nil_ \case
        [ SignPubKeyLike mbox ] ->
          lift (listInbox (InboxArgs (Just mbox) Nothing Nothing Nothing Short False False))
        (inboxArgs -> Just ia)
          -- One of the two, and the reader cannot say which: a queue with
          -- neither a mailbox nor a repository is a queue nobody named.
          | isJust (iaMailbox ia) || isJust (iaRepo ia) -> lift (listInbox ia)
        -- Its own message, not a BadFormException. Two things were wrong with
        -- that: it names an internal Haskell type and a spelling the caller did
        -- not type, which is the defect `hub verify` was already fixed for; and
        -- its 'show' renders the whole form, so a wrong-arity call printed the
        -- caller's argv RAW. `hub inbox <key> $'x\ESC[2K...'` put a live
        -- erase-line sequence on the terminal, while the sibling path thirty
        -- lines away in Main.hs sent the same bytes through 'safeText'.
        other -> liftIO (badArgs (inboxUsage :: Doc ()) other)

  where
    listInbox ia = do
      let mmbox = iaMailbox ia
          mrepo = iaRepo ia
      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX

      -- The mailbox named, or the one the repository declares. Costs nothing
      -- when it was named: `mailboxFor` makes no call in that case.
      mbox <- case mrepo of
        Nothing -> maybe (liftIO (die (show (inboxUsage :: Doc ())))) pure mmbox
        Just repo -> mailboxFor mmbox repo
                       >>= either (\e -> liftIO (refuse (show (pretty e)) (manifestCode e)))
                                  pure

      -- THE DENY-LIST, when the caller says which repository's. It is this
      -- node's own state keyed by repo (PEP-21 "Two enforcement layers"), not
      -- anything the mailbox knows, so a queue read by mailbox key alone has no
      -- source for it and says so below. Applied HERE and not only at accept,
      -- because triage is a queue a human reads and a banned author's letter
      -- should not be in front of them at all.
      --
      -- The same refusal `hub inbox accept` makes: a deny-list that reads as
      -- empty when it is damaged stops working silently, which is the failure
      -- this layer exists to prevent.
      --
      -- The deny-list is read again on every read, so that a `hub ban` made
      -- while a follower runs applies from its next read on: a follower that
      -- kept the list it started with would go on showing the letters the ban
      -- was made to take out of the queue.
      let readRaw ig = do
            allowed <- denyingFor mrepo
                         >>= either (\e -> liftIO (refuse (show (denyUnreadable e)) codeNoBanList))
                                    pure

            -- Each with its own exit code. PEP-22 gives 1 to "a bad argument, an
            -- unknown verb", and neither of these is one: the key was well formed
            -- both times, and the fix is a command in one case and an
            -- investigation in the other. While both left with 1, a hook could not
            -- tell either from a typo.
            --
            -- Chained, so the PeerSilent handler in 'readOnce' covers this one
            -- as well as the body. That is harmless: 'refuse' leaves through
            -- 'exitWith', whose exception is an ExitCode and is caught by
            -- neither.
            readInboxFrom (ig { igAllowed = allowed }) mbox
                          (iaAfter ia) (fromMaybe maxInboxLetters (iaLimit ia))
              `catch` (\(e :: MailboxUnknown) -> liftIO (refuse (show e) codeMailboxUnknown))

          -- PeerSilent is caught out here and not in 'readRaw', because the
          -- follower has to see it first: a call that timed out on a socket the
          -- peer had closed is a restart to wait out, not a refusal.
          readOnce = readRaw (overRpc sto api)
              `catch` (\(e :: PeerSilent)     -> liftIO (refuse (show e) codePeerSilent))

          -- 128 plus SIGPIPE, what a shell reports for a program a pipe killed.
          -- `hub inbox <key> | head` used to report SUCCESS on a truncated
          -- queue, which is the one answer a caller must not get from a queue it
          -- only saw part of. Scoped to the printing, like the same handler in
          -- Verify: over the whole verb it would turn a documented refusal whose
          -- stderr had been closed into a silent 141.
          printing = liftIO . handleJust
            (\e -> if isResourceVanishedError e then Just () else Nothing)
            (\_ -> exitWith (ExitFailure 141))

      if iaFollow ia then followInbox api mbox readRaw printing else do

        r <- readOnce

        printing
          (if iaJson ia
             -- The whole document or nothing: a JSON consumer that got half a
             -- document because a pipe closed would parse it as a shorter
             -- mailbox, which is the one failure the counts in it exist to make
             -- impossible.
             then LBS.putStr (renderContract (queueContract r))
             else mapM_ print (inboxDoc (iaWidth ia) r))

        -- What can be said ABOUT the list, said once at the end rather than once
        -- per line: the counts are what matter, and a page of identical warnings
        -- buries the letters.
        --
        -- Through 'saying', so that a stderr nobody is reading cannot cost the
        -- exit code below. `hub inbox K 2>&1 | head` closes both, and an
        -- unguarded write then leaves through the RTS with 1 -- the code PEP-22
        -- gives to usage errors, which is exactly the confusion 17 and 18 were
        -- added to end.
        liftIO $ for_ (inboxNotes (isJust mrepo) r) (saying . (<> line))

        liftIO $ case inboxCode r of
          0 -> pure ()
          n -> exitWith (ExitFailure n)

      where
        -- THE FOLLOWER. The same read as above, done again whenever the peer's
        -- root moves, printing only what this run has not printed yet.
        --
        -- No exit code from 'inboxCode' here, because there is no end of the
        -- queue to attach one to: a page that was incomplete on one read can be
        -- whole on the next. The notes still say it, each time it starts to
        -- apply.
        --
        -- OVER ITS OWN CONNECTION, opened again whenever the peer comes back:
        -- "HBS2.Hub.PeerLink" says why the one this verb was handed cannot be
        -- used for that. The loop itself is 'followLoop'.
        followInbox api mb readRaw printing = do
          me <- liftIO myThreadId
          liftIO $ for_ [sigINT, sigTERM] $ \s ->
            installHandler s (Catch (throwTo me FollowStopped)) Nothing

          -- The messaging layer's own warnings are off for as long as this
          -- runs. The connection this verb was handed stays open and unused,
          -- and while the peer is away it retries every second with a warning
          -- and an error each time, which a watcher merging stderr reads as
          -- events. The follower says once that the peer is gone and once that
          -- it is back. Nothing in the hub itself logs through these levels.
          liftIO $ setLoggingOff @WARN >> setLoggingOff @ERROR

          let silent (e :: PeerSilent) = liftIO (refuse (show e) codePeerSilent)
              unknown (e :: MailboxUnknown) = liftIO (refuse (show e) codeMailboxUnknown)

              openLink away k = withPeerLink (socketOf api) $ \sto mbapi over -> do
                let ig = overRpc sto mbapi
                k FollowSession
                    -- The first call on a new connection is the one that says
                    -- whether the peer is back, and after an outage a peer
                    -- whose socket is up before its services are is not back
                    -- yet. Every other silence is the peer stuck, as before.
                    { ssRoot   = over away (igRoot ig mb) `catch` silent
                    , ssRead   = over False (readRaw ig) `catch` silent
                    , ssChange = \seen -> over False (awaitRootChange ig mb seen)
                                            `catch` unknown `catch` silent
                    }

              emit first r fresh = printing $ do
                if iaJson ia
                  then when (first || not (List.null fresh)) $
                         LBS.putStr (Aeson.encode (queueContract r { irLetters = fresh }) <> "\n")
                  else for_ fresh $ \lv -> print (render (iaWidth ia) lv) >> hFlush stdout
                hFlush stdout

              follower = Follower
                { fwLink  = openLink
                , fwEmit  = emit
                , fwNotes = \r -> fmap show (inboxNotes (isJust (iaRepo ia)) r :: [Doc ()])
                , fwSay   = \n -> liftIO (saying (pretty n <> line))
                , fwPause = pause
                }

          withRunInIO $ \unlift ->
            E.handle (\FollowStopped -> E.handle (\(_ :: IOException) -> pure ()) (hFlush stdout) >> exitSuccess)
                   (unlift (followLoop follower))

-- | What this verb takes, in the words somebody typing it would use.
inboxUsage :: Doc ann
inboxUsage = "usage: hub inbox [--mailbox <key>] [--repo <key>]" <> line
          <> "  one of the two: the mailbox key in base58, or the repository"
          <> line <> "  whose manifest declares it. The peer must already hold the"
          <> line <> "  mailbox: `hbs2-peer mailbox create --key KEY hub`."
          <> line <> "  `hub help inbox` says more."

-- | The queue itself: one line per letter, on stdout.
inboxDoc :: Width -> InboxRead -> [Doc ann]
inboxDoc w = fmap (render w) . irLetters

-- | Which rows of a read a follower has not printed yet, and what it has printed
-- once it prints them.
--
-- KEYED BY WHAT THE ROW SAYS, and not only by which message it is. A follower
-- that printed a message once and never again would leave the first thing it
-- said about a letter as the last: a letter whose block had not downloaded yet
-- reads "unreadable: not fetched" on the read that first sees it and opens on
-- the next, and the opened row is the one a maintainer needs. A rewrap moves
-- the row too, by its copy count or by which copy sorts first and so leads the
-- row, and the count is the fact 'render' prints it for.
--
-- Every name the row covers is remembered, the copies as well as the message
-- that leads it, so a new copy that sorts first is a changed row and not a new
-- letter. The whole row is compared ('Long', whatever the caller asked to see),
-- because a shortened one could stay the same while what it shortens changed.
--
-- Forgotten when the message leaves the mailbox, which keeps this the size of
-- the mailbox and not of the run: a deleted message cannot come back (deletion
-- wins, see 'liveMessages'). Not when the walk was cut short or had holes,
-- since 'irLive' is then a part of the mailbox and forgetting by it would print
-- rows again that nothing changed.
freshRows :: HashMap HashRef String -> InboxRead -> ([LetterView], HashMap HashRef String)
freshRows shown r = (fresh, kept)
  where
    rows   = [ (lv, show (render Long lv :: Doc ())) | lv <- irLetters r ]
    names lv = lvMessage lv : lvCopies lv
    said lv = [ s | n <- names lv, Just s <- [HM.lookup n shown] ]
    fresh  = [ lv | (lv, row) <- rows, row `notElem` said lv ]
    shown' = HM.union (HM.fromList [ (n, row) | (lv, row) <- rows, n <- names lv ]) shown
    kept | irTruncated r || not (List.null (irMissing r)) = shown'
         | otherwise = HM.filterWithKey (\k _ -> HS.member k (irLive r)) shown'

-- | What a follower asks the peer over one connection. Each answer is
-- 'Left' 'PeerGone' when the connection dropped under the call.
data FollowSession m = FollowSession
  { ssRoot   :: m (Either PeerGone (Maybe HashRef))
    -- ^ the mailbox root now; the first call on every connection
  , ssRead   :: m (Either PeerGone InboxRead)
    -- ^ one read of the queue
  , ssChange :: Maybe HashRef -> m (Either PeerGone (Maybe HashRef))
    -- ^ wait until the root is not this one
  }

-- | Everything 'followLoop' does to the outside world, so that what it prints
-- across a peer restart can be asked about without a peer.
data Follower m = Follower
  { fwLink  :: Bool
            -> (FollowSession m -> m (FollowState, Bool))
            -> m (Maybe (FollowState, Bool))
    -- ^ Open a connection and run the body over it; 'Nothing' when there was
    -- nothing to connect to. The flag is whether the peer was away, which
    -- makes the first call lenient (see 'HBS2.Hub.PeerLink.Over').
  , fwEmit  :: Bool -> InboxRead -> [LetterView] -> m ()
    -- ^ print the fresh rows of a read; the flag is whether nothing has been
    -- printed yet, which @--json@ answers with a document even when empty
  , fwNotes :: InboxRead -> [String]
    -- ^ what is true about a read, for stderr
  , fwSay   :: String -> m ()
    -- ^ one line on stderr
  , fwPause :: Timeout 'Seconds -> m ()
  }

-- | What a follower carries from one read to the next, and from one connection
-- to the next.
data FollowState = FollowState
  { fsShown :: HashMap HashRef String   -- ^ see 'freshRows'
  , fsSaid  :: [String]                 -- ^ the notes the last read said
  , fsFirst :: Bool                     -- ^ nothing printed yet
  }

-- | The line a follower prints when the peer goes, once per outage.
followGone :: String
followGone = "the peer is gone; waiting for it to come back"

-- | And when it is back and the follower is reading again.
followBack :: String
followBack = "the peer is back; following again"

-- | How long a follower waits between two attempts to reach a peer that went
-- away: quickly at first, since a restart takes seconds, and then every
-- fifteen seconds for as long as it takes.
followBackoff :: [Timeout 'Seconds]
followBackoff = [1, 2, 4, 8] <> repeat 15

-- | THE FOLLOWER. Read, print what is new, wait for the root to move, again;
-- and when the peer goes away, wait for it and go on from where it was.
--
-- WHAT IT HAS PRINTED OUTLIVES THE CONNECTION. The state 'freshRows' keeps is
-- carried across a reconnect, and the first thing done on a new connection is
-- a whole read, not a wait for the root to move: a letter that arrived while
-- the peer was away is printed then, and one printed before the outage is not
-- printed again. The root is taken BEFORE each read and not after: taken
-- after, a letter landing between the two would move a root this loop then
-- calls seen, and it would wait for the next one.
--
-- ONE LINE EACH WAY. 'followGone' when a connection drops or the first one
-- cannot be made, then nothing however many attempts it takes, then
-- 'followBack' once a new connection has answered. Both through 'fwSay',
-- which is stderr: a JSON stream on stdout carries documents and nothing else.
--
-- Never returns. It is stopped from outside, by a signal or by a refusal.
followLoop :: Monad m => Follower m -> m a
followLoop fw = attempt (FollowState mempty [] True) False followBackoff
  where
    attempt st away delays = fwLink fw away (session st away) >>= \case
      Just (st', True) -> do
        fwSay fw followGone
        retry st' followBackoff
      Just (st', False) -> waiting st'
      Nothing           -> waiting st
      where
        waiting s = do
          unless away (fwSay fw followGone)
          retry s delays

    retry st ds = do
      let (d, ds') = case ds of
                       (x:xs) -> (x, xs)
                       []     -> (15, [])
      fwPause fw d
      attempt st True ds'

    -- (state, whether the connection worked before it dropped)
    session st away s = ssRoot s >>= \case
      Left PeerGone -> pure (st, False)
      Right root -> do
        when away (fwSay fw followBack)
        go st root
      where
        go st0 seen = ssRead s >>= \case
          Left PeerGone -> pure (st0, True)
          Right r -> do
            let (fresh, shown') = freshRows (fsShown st0) r
                notes = fwNotes fw r
            fwEmit fw (fsFirst st0) r fresh
            for_ [ n | n <- notes, n `notElem` fsSaid st0 ] (fwSay fw)
            let st1 = FollowState shown' notes False
            ssChange s seen >>= \case
              Left PeerGone -> pure (st1, True)
              Right seen'   -> go st1 seen'

-- | How a follower is told to stop: SIGINT or SIGTERM, delivered to the thread
-- that runs it.
--
-- Asynchronous, so that nothing between the signal and the handler at the top
-- of the follower mistakes it for a failure of its own and carries on: a
-- @catchAny@ in a reader would otherwise swallow the request to stop.
data FollowStopped = FollowStopped
  deriving stock (Show)

instance Exception FollowStopped where
  toException   = E.asyncExceptionToException
  fromException = E.asyncExceptionFromException

-- | Whether a row carries whole identifiers or the front of them.
--
-- WHY A QUEUE HAS THIS AT ALL. A row printed four full base58 values -- the
-- message hash, the envelope key, the author key and the event id -- which is
-- about a hundred and eighty characters before any of the words, on the one
-- screen a maintainer triages from. Nothing could be compared against the row
-- above it and nothing fitted a terminal, so the fields that say what the letter
-- IS were pushed off the end.
--
-- 'Long' is what a script pipes and what a copy-paste needs; 'Short' is what a
-- person reads. Short is the default, which is the way round `git log` has it.
data Width = Short | Long
  deriving stock (Eq,Show)

-- | What is true about the list above, for stderr.
--
-- A list rather than an action, so that a test can ask what this run would say
-- without capturing a handle.
inboxNotes :: Bool -> InboxRead -> [Doc ann]
inboxNotes listed r =
  settledNote <> missingNote <> partialNote <> omittedNote <> deniedNote
    <> keymanNote <> policyNote
  where
    -- BEFORE the omitted one, because it changes what that one means. "N more
    -- letters were not opened, here is the cursor" invites a caller to page to
    -- the end; when the walk stopped early there is no end to page to, and the
    -- pages are pages of a prefix of the mailbox.
    partialNote
      | not (irTruncated r) = []
      | otherwise =
          [ "hbs2-hub: this mailbox holds more than" <+> pretty maxMailboxBlocks
              <+> "blocks, so the walk stopped before the end of it and the"
              <+> "letters above are part of the mailbox and not the mailbox."
              <+> "--after pages within what was read and does not reach past"
              <+> "it, and no fetch changes that: every run stops at the same"
              <+> "place. `hub inbox accept` refuses on this until --incomplete." ]

    -- WHAT THE DENY-LIST TOOK OUT, counted. 'igAllowed' has always said the
    -- queue applies it "because triage is a queue a human reads and a banned
    -- author's letter should not be in it", and until now such a letter was in
    -- it as one more unreadable line. Dropping them SILENTLY would be the other
    -- mistake: the number is how a maintainer sees that somebody is still
    -- sending, and how they notice a key they banned in error.
    deniedNote
      | irDenied r <= 0 = []
      | otherwise =
          [ "hbs2-hub:" <+> pretty (irDenied r) <+> "letter(s) were left out because"
              <+> "their author is on this repository's deny-list (hub ban)."
              <+> "They still arrived and still take disk: a ban bounds what is"
              <+> "folded, not what is stored." ]

    -- Truncation, said with a number. A mailbox is public, so how many letters
    -- are in it is a stranger's choice; this reader opens at most
    -- 'maxInboxLetters' of them and used to open all of them and hold every body
    -- resident. What it will not do is truncate quietly: a list that is missing
    -- letters is wrong, so it says how many and leaves non-zero.
    omittedNote
      | irOmitted r <= 0 = []
      | otherwise =
          [ "hbs2-hub:" <+> pretty (irOmitted r) <+> "more letter(s) in this mailbox"
              <+> "were not opened:" <+> pretty maxInboxLetters <+> "is the most"
              <+> "one read will take. The list above is a page, in hash order,"
              <+> "and is therefore incomplete."
              -- AND HOW TO SEE THE REST, which is the whole of what was
              -- missing. The page is the first N by hash and a mailbox is
              -- public, so which N those are is a stranger's choice: grinding
              -- hashes below the honest ones is a few thousand signatures and
              -- displaces every real letter off the only screen a maintainer
              -- triages from. There was no flag that raised the bound and no
              -- way past the prefix, so the remedy was rejecting junk one hash
              -- at a time.
              <> (case irCursor r of
                    Nothing -> mempty
                    Just h -> line <> "  next page: --after" <+> hashDoc h) ]

    -- The one this reader CANNOT tell apart, said out loud rather than implied.
    --
    -- 'NotForUs' comes from 'ReadNoGroupKeyAccess', and the keyman answers
    -- Nothing for "no key of mine is a recipient" AND for an index that was
    -- never updated, a key file it cannot read, and credentials that do not
    -- parse. So a node with a keyman that cannot be consulted shows a full queue
    -- in which every single line says "not sealed to any key this node holds",
    -- once per letter, with a zero exit -- which reads as "none of this is mine"
    -- and is indistinguishable from it.
    --
    -- Only when EVERY letter says it, because that is the shape a broken keyman
    -- makes and a mailbox where some letters are ours does not.
    keymanNote
      | not (List.null (irLetters r))
      , all notForUs (irLetters r) =
          [ "hub: all" <+> pretty (length (irLetters r)) <+> "letters report"
              <+> "\"not sealed to any key this node holds\". That is also what a"
              <+> "keyman this node cannot consult looks like from here -- an"
              <+> "index never updated, an unreadable key file, credentials that"
              <+> "do not parse all answer the same way. If some of these should"
              <+> "be yours, check `hbs2-keyman list` before concluding they are"
              <+> "not." ]
      | otherwise = []

    notForUs lv = case lvLetter lv of
      Left NotForUs -> True
      _             -> False

    -- Not an error, and not silence either. Every letter listed is real; the
    -- mailbox was simply still arriving, which is the routine state of a first
    -- read of a big one, since the peer rewrites the mailbox ref on every merged
    -- batch. A non-zero exit here would fire on ordinary use and teach a caller
    -- to ignore the code.
    settledNote
      | irSettled r = []
      -- The empty case is its own sentence, because it is the one that used to
      -- be a lie: no root at all and an empty mailbox are the same observation
      -- from here, and the wait loop called the second look settled 2.5 seconds
      -- in. "Nothing is waiting" and "nothing arrived in twenty seconds" are
      -- different answers and only one of them is about the mailbox.
      --
      -- Rarer since the peer can say it is idle, which settles an empty
      -- mailbox in one round: what is left is a peer that was still fetching
      -- something for the mailbox, or one too old to say.
      | List.null (irLetters r) =
          [ "hub: the peer produced no copy of this mailbox within"
              <+> pretty maxFetchRounds <+> "rounds, and it was still fetching"
              <+> "one or is too old to say whether it was. That is NOT the same"
              <+> "as an empty mailbox: an empty one answers in a round."
              <+> "Try again in a moment." ]
      | otherwise =
          [ "hub: the peer's copy of this mailbox was still changing after"
              <+> pretty maxFetchRounds <+> "rounds, so more letters may follow."
              <+> "Everything listed above is real." ]

    -- This one IS wrong, in both directions: a missing chunk carrying Exists
    -- entries makes letters vanish, one carrying Deleted entries puts folded
    -- letters back in the queue. So it does not leave through a zero exit.
    missingNote
      | List.null (irMissing r) = []
      | otherwise =
          [ "hbs2-hub:" <+> pretty (length (irMissing r))
              <+> "block(s) of this mailbox tree could not be read, so the list"
              <+> "above is incomplete in both directions."
              <+> "Missing:" <+> hsep (capped (fmap hashDoc (irMissing r))) ]

    -- PEP-22: "anything built on it must not treat 'it was in the queue' as
    -- 'it may be folded'". The queue prints "(folds)" against every letter the
    -- admission rules would take, and read alone that is permission. It is not:
    -- this form has no deny-list to apply, so a banned author's letter is in the
    -- list with the same marker as anyone else's. Once, and only when there is
    -- something for it to be about.
    policyNote
      | listed = []
      | any folds (irLetters r) =
          [ "hub: no deny-list was applied (this form takes a mailbox key, and"
              <+> "the list is kept per repository), so \"(folds)\" means the"
              <+> "admission rules would take it, not that its author is allowed"
              <+> "to send. Name the repository with --repo to apply it." ]
      | otherwise = []

    folds lv = case lvLetter lv of
      Right (_, _, FoldsToCanon) -> True
      _                          -> False

    -- One line of hashes with no bound was the shape `hub verify` already had
    -- its cap added for, after a measured 369 MB of stdout. irMissing grows with
    -- the mailbox tree, which a stranger can grow.
    capped xs
      | length xs <= maxMissingLines = xs
      | otherwise = List.take maxMissingLines xs
          <> [ "and" <+> pretty (length xs - maxMissingLines) <+> "more" ]

-- | How many missing-block hashes are worth printing.
maxMissingLines :: Int
maxMissingLines = 100

-- | 0, or what the caller has to act on.
--
-- Three ways this list is not the mailbox, and deliberately not a fourth: an
-- unsettled read is a SHORTER answer and these are WRONG ones. A hole in the
-- tree loses letters and resurrects folded ones; a walk that ran out of
-- 'maxMailboxBlocks' does both over a larger piece of the tree, and no fetch
-- fixes that one; a page that stopped at 'maxInboxLetters' simply does not
-- contain letters that are in the mailbox. All are 2, because the remedy is the
-- same -- do not treat this list as the mailbox -- and the numbers are a
-- contract that may be added to, not reassigned.
inboxCode :: InboxRead -> Int
inboxCode r
  | not (List.null (irMissing r)) = 2
  | irTruncated r                 = 2
  | irOmitted r > 0               = 2
  | otherwise = 0

-- One line per letter, in the order the fields matter to somebody deciding what
-- to do with it.
--
-- The envelope key is printed on the failure paths too, wherever it is known: it
-- is the key a maintainer would block by, so a forged envelope that does not
-- name its signer is a report nobody can act on. Note that for a letter that DID
-- open, the subject PEP-21 says to act on is the inner author, further along the
-- line: an envelope ban is evadable by rewrapping.
--
-- EVERY hash and EVERY key here goes through 'hashDoc'/'keyDoc' and not through
-- 'pretty'. None of them is bounded to its natural width by anything: a HashRef
-- and a HubKey are both newtypes over ByteString with generic Serialise
-- instances, and all of these come out of a stranger's bytes -- the message hash
-- from a mailbox entry, the thread-id and the author from a signed box, the
-- envelope key from a box that verified because libsodium reads 32 bytes without
-- asking how many there are. base58 is Integer base conversion, so it is
-- quadratic: a 48 KiB field is 0.7 s of CPU and 67 000 characters, per line, in
-- a queue anybody can write to.
render :: Width -> LetterView -> Doc ann
render Short = renderShort
render Long  = renderLong

renderShort :: LetterView -> Doc ann
renderShort lv = shortRow
  where
    short = briefly 8
    shortRow = short (hashDoc (lvMessage lv))
                 <+> maybe "-" (short . keyDoc) (lvEnvelope lv)
                 <+> body <> copies

    -- SAID, not silently collapsed. A rewrap needs no key (see 'lvCopies'), so
    -- one letter can arrive under any number of envelopes, and the queue used
    -- to give each of them a line and a decision. Showing one line is the point;
    -- showing it WITHOUT the count would hide from the maintainer that somebody
    -- is doing this, which is the one fact they can act on.
    copies = case lvCopies lv of
      [] -> mempty
      cs -> " +" <> pretty (length cs) <+> "copy(ies) of the same letter,"
              <+> "under other envelopes"

    body = case lvLetter lv of
      Left e -> "unreadable:" <+> pretty e
      Right (author, content, disp) ->
        "author" <+> short (keyDoc author)
          <+> "event" <+> maybe "?" (short . hashDoc) (lvEventId lv)
          -- The author's own clock, advisory and unverifiable (PEP-19), which is
          -- why it is shown and not sorted on: sorting the queue by it would let
          -- a sender choose where in the queue they appear.
          <+> "at" <+> pretty (utcOf (authorTs content))
          <+> pretty (opName content)
          <+> maybe "-" (\t -> "on" <+> short (hashDoc t)) (authorThread content)
          <+> dispOf disp
          <> shortTitle content

-- | @--long@ is an inspection and copy-paste view, not the compact queue with
-- its abbreviations removed.  Label every key: an author key and a message hash
-- have the same base58 shape, and the old positional row invited maintainers to
-- paste the former into @--message@.  Put the command itself in the document so
-- the next valid action does not have to be inferred from field order.
renderLong :: LetterView -> Doc ann
renderLong lv = vcat (header <> copies <> details)
  where
    header =
      [ "Message:" <+> hashDoc (lvMessage lv)
      , "Envelope:" <+> maybe "-" keyDoc (lvEnvelope lv)
      ]
    copies = case lvCopies lv of
      [] -> []
      cs -> [ "Copies:" <+> pretty (length cs) <+> "under other envelopes" ]
    details = case lvLetter lv of
      Left e -> [ "Status: unreadable:" <+> pretty e ]
      Right (author, content, disp) ->
        [ "Author:" <+> keyDoc author
        , "Event:" <+> maybe "?" hashDoc (lvEventId lv)
        , "At:" <+> pretty (utcOf (authorTs content))
        , "Action:" <+> pretty (opName content)
        , "Thread:" <+> maybe "-" hashDoc (authorThread content)
        , "Disposition:" <+> pretty (dispositionName disp)
        ]
        <> titleLine content
        <> acceptLine disp

    titleLine = \case
      AOpen _ _ t _ _ _ _ _ -> [ "Title:" <+> dquotes (pretty (safeText t)) ]
      _                       -> []
    acceptLine = \case
      OwnerNative -> []
      _ -> [ "Accept:" <+> "hbs2-hub inbox accept --message"
                         <+> hashDoc (lvMessage lv) ]

-- THE SUBJECT, which the compact queue did not carry at all before this
-- renderer grew it: deciding what to inspect meant opening every letter in
-- turn.  It is last because it is the only unbounded prose field a stranger
-- controls; every field a maintainer compares stays aligned before it.
shortTitle :: AuthorContent -> Doc ann
shortTitle = \case
  AOpen _ _ t _ _ _ _ _ -> " " <> dquotes (briefly 60 (pretty (safeText t)))
  _                       -> mempty

dispOf :: Disposition -> Doc ann
dispOf OwnerNative = "(owner-only: not acceptable from a letter)"
dispOf d           = "(" <> pretty (dispositionName d) <> ")"

-- | @--mailbox <key> [--repo <key>]@.
--
-- The repository is optional and is not about which mailbox to read: it names
-- whose deny-list to apply, which is this node's own state keyed by repo
-- (PEP-21). Without it the queue is honest but unfiltered, and 'inboxNotes'
-- says so rather than leaving "(folds)" to be read as permission.
--
-- Exported and pure, like every other argument reader here.
-- | What one @hub inbox@ was asked to show.
data InboxArgs = InboxArgs
  { iaMailbox :: Maybe HubKey
  , iaRepo    :: Maybe RepoRef
    -- | Start the page after this message, in hash order.
    --
    -- The page is the first N of the live set sorted by hash, and a mailbox is
    -- public, so which N those are is a stranger's choice: grinding hashes below
    -- the honest ones displaces every real letter off the only screen a
    -- maintainer triages from, and until this there was no way past the prefix
    -- and no flag that raised the bound.
    --
    -- A HASH AND NOT AN OFFSET, because the set changes underneath: an offset
    -- names a position in a list that grows, so a letter arriving between two
    -- pages shifts everything after it and a page is skipped.
  , iaAfter   :: Maybe HashRef
    -- | How many to open. Clamped to 'maxInboxLetters' by the reader, so this
    -- lowers the cost of a look and cannot raise it past the bound.
  , iaLimit   :: Maybe Int
    -- | Whole identifiers, or the front of them. See 'Width'.
  , iaWidth   :: Width
    -- | The queue as a document rather than as rows. See 'queueContract'.
  , iaJson    :: Bool
    -- | Keep running and print what arrives. See 'freshRows'.
  , iaFollow  :: Bool
  }
  deriving stock (Eq,Show)

inboxArgs :: forall c . IsContext c => [Syntax c] -> Maybe InboxArgs
inboxArgs syn = do
  kvs  <- flagsAndSwitches (repoFlags <> ["--mailbox","--after","--limit"])
                           ["--long","--json","--follow","-f"] syn
  -- BOTH optional here and not in the verb: a queue named by repository alone
  -- resolves its mailbox from the manifest, and one named by neither is a
  -- usage error the verb reports with its own words.
  mbox <- flagMaybe kvs "--mailbox" asKey
  repo <- flagRepoMaybe asKey kvs
  aft  <- flagMaybe kvs "--after" asHash
  lim  <- fmap (fmap fromIntegral) (flagMaybe kvs "--limit" flagWord)
  long <- flagSwitch kvs "--long"
  asJson <- flagSwitch kvs "--json"
  follow <- (||) <$> flagSwitch kvs "--follow" <*> flagSwitch kvs "-f"
  pure (InboxArgs mbox repo aft lim (if long then Long else Short) asJson follow)
  where
    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    asHash = \case { HashLike h -> Just h ; _ -> Nothing }


-- | The queue as a document (PEP-22 "Scripting").
--
-- WHY THIS IS A FOURTH DOCUMENT AND NOT A NEW CONTRACT. 'threadContract' put a
-- @document@ field in the envelope from the start, for exactly this: one
-- counter over several shapes, so that two documents cannot both call
-- themselves @contract 1@ and mean different things.
--
-- IT IS NOT DERIVED FROM CANON, and that is the difference from every other
-- document here. A thread contract is a function of the fold, so two clones
-- produce the same bytes; this is a function of a mailbox read at a moment, by
-- a node that holds particular keys, filtered by a deny-list that is local and
-- unsigned. Two maintainers reading the same mailbox will not agree, and
-- neither of them is wrong. Nothing downstream should treat a row here as
-- evidence about anything but this node's own view.
--
-- THE COUNTS AND THE CURSOR ARE PART OF IT. A consumer that read @letters@ and
-- ignored @omitted@ would be a consumer that silently works on a prefix of a
-- mailbox, which is the failure 'inboxCode' exists to make loud on the terminal
-- side.
queueContract :: InboxRead -> Value
queueContract r = object
  [ "contract" .= contractVersion
  , "document" .= ("queue" :: Text)
  , "settled"  .= irSettled r
  , "denied"   .= irDenied r
  , "omitted"  .= irOmitted r
  , "cursor"   .= fmap b58 (irCursor r)
  , "missing"  .= fmap b58 (irMissing r)
    -- Separate from @omitted@ and from @missing@ because it invalidates them
    -- both: a consumer that pages on @cursor@ until @omitted@ is zero has
    -- finished reading a prefix of the mailbox, and @missing@ counts only the
    -- holes in the part that was walked.
  , "truncated" .= irTruncated r
  , "letters"  .= fmap letter (irLetters r)
  ]
  where
    letter lv = object $
      [ "message"  .= b58 (lvMessage lv)
      , "envelope" .= fmap key58 (lvEnvelope lv)
      , "event"    .= fmap b58 (lvEventId lv)
      , "copies"   .= fmap b58 (lvCopies lv)
      ]
      <> case lvLetter lv of
           -- SAID AS A FIELD rather than by leaving the others out: a consumer
           -- that has to infer "unreadable" from an absence is a consumer that
           -- treats a bug in this renderer as a readable letter.
           Left e -> [ "unreadable" .= (Text.pack (show (pretty e)) :: Text) ]
           Right (author, content, disp) ->
             [ "unreadable"  .= (Nothing :: Maybe Text)
             , "author"      .= key58 author
             , "op"          .= opName content
             , "thread"      .= fmap b58 (authorThread content)
               -- Clamped like the thread contract clamps it ('declaredAt'): the
               -- author's own clock, which nothing bounds, on its way to a
               -- reader whose JSON numbers may be doubles.
             , "declared_at" .= declaredAt (authorTs content)
             , "disposition" .= dispositionName disp
             ]
             <> [ "title" .= t | AOpen _ _ t _ _ _ _ _ <- [content] ]

-- | A disposition, in the spelling the queue and the letter documents share.
dispositionName :: Disposition -> Text
dispositionName = \case
  FoldsToCanon -> "folds"
  RequestOnly  -> "request"
  OwnerNative  -> "owner-native"
