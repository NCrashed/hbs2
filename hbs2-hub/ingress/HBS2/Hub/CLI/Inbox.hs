-- 'opOf' below dispatches on every constructor of 'AuthorContent' with no
-- wildcard, on content an attacker composed, inside the triage loop. The library
-- modules set this for the same reason: a constructor added without a case here
-- is a crash in that loop rather than a build error.
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
  , inboxNotes
  , inboxCode
  , inboxUsage
  , refuse
  , saying
  , bounded
  , PeerSilent(..)
  , codeMailboxUnknown
  , codePeerSilent
  , maxMissingLines
  , overRpc
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Ingress

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,extractGroupKeySecret)
import HBS2.Net.Auth.GroupKeySymm (ToDecrypt,pattern ToDecryptBS)
import HBS2.Storage.Operations.Class (readFromMerkle)
import Crypto.Saltine.Class qualified as Saltine
import Control.Monad.Except (runExceptT)
import Data.Text qualified as Text

import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale,formatTime)
import Data.Word (Word64)
import System.Exit (die,exitWith,ExitCode(..))
import System.IO.Error (isResourceVanishedError)

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
             <> line <> "The wait costs one round (about 2.5s) when the peer already"
             <> line <> "has the mailbox, and up to seven more when it does not:"
             <> line <> "the peer writes a mailbox ref only once a merge lands, so"
             <> line <> "'no ref yet' and 'nothing in it' cannot be told apart from"
             <> line <> "here, and an empty answer is only believable after the wait."
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
        -- Its own message, not a BadFormException. Two things were wrong with
        -- that: it names an internal Haskell type and a spelling the caller did
        -- not type, which is the defect `hub verify` was already fixed for; and
        -- its 'show' renders the whole form, so a wrong-arity call printed the
        -- caller's argv RAW. `hub inbox <key> $'x\ESC[2K...'` put a live
        -- erase-line sequence on the terminal, while the sibling path thirty
        -- lines away in Main.hs sent the same bytes through 'safeText'.
        _ -> liftIO (die (show (inboxUsage :: Doc ())))

  where
    listInbox mbox = do
      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX

      -- Each with its own exit code. PEP-22 gives 1 to "a bad argument, an
      -- unknown verb", and neither of these is one: the key was well formed both
      -- times, and the fix is a command in one case and an investigation in the
      -- other. While both left with 1, a hook could not tell either from a typo.
      --
      -- Chained, so the second handler covers the first as well as the body.
      -- That is harmless here: 'refuse' leaves through 'exitWith', whose
      -- exception is an ExitCode and is caught by neither.
      r <- readInbox (overRpc sto api) mbox
             `catch` (\(e :: MailboxUnknown) -> liftIO (refuse (show e) codeMailboxUnknown))
             `catch` (\(e :: PeerSilent)     -> liftIO (refuse (show e) codePeerSilent))

      liftIO $ handleJust
        (\e -> if isResourceVanishedError e then Just () else Nothing)
        -- 128 plus SIGPIPE, what a shell reports for a program a pipe killed.
        -- `hub inbox <key> | head` used to report SUCCESS on a truncated queue,
        -- which is the one answer a caller must not get from a queue it only saw
        -- part of. Scoped to the printing, like the same handler in Verify: over
        -- the whole verb it would turn a documented refusal whose stderr had
        -- been closed into a silent 141.
        (\_ -> exitWith (ExitFailure 141))
        (mapM_ print (inboxDoc r))

      -- What can be said ABOUT the list, said once at the end rather than once
      -- per line: the counts are what matter, and a page of identical warnings
      -- buries the letters.
      --
      -- Through 'saying', so that a stderr nobody is reading cannot cost the
      -- exit code below. `hub inbox K 2>&1 | head` closes both, and an
      -- unguarded write then leaves through the RTS with 1 -- the code PEP-22
      -- gives to usage errors, which is exactly the confusion 17 and 18 were
      -- added to end.
      liftIO $ for_ (inboxNotes r) (saying . (<> line))

      liftIO $ case inboxCode r of
        0 -> pure ()
        n -> exitWith (ExitFailure n)

-- | Bound one call to the peer, and say which call it was if it does not answer.
--
-- Top-level rather than a @where@ binding, because this is the fix the whole
-- rewrite is about and nothing could reach it: the storage client's 'getBlock'
-- calls 'callService' raw, which blocks on a TQueue with no timeout at all.
--
-- ONE CALL, and the haddock says so because the first version of it did not:
-- this is 10 s per block, not 10 s for the walk. A peer answering every merkle
-- node just inside the bound still takes as long as the mailbox is big. What it
-- rules out is the wedge -- a peer that answers the mailbox service and then
-- stops -- which is the state that hung the verb forever.
bounded :: MonadUnliftIO m => Timeout 'Seconds -> String -> m a -> m a
bounded t what act =
  race (pause t) act >>= either (\() -> throwIO (PeerSilent what)) pure

-- | Say why, on stderr, and leave with the code that says which why.
--
-- The write is GUARDED and the exit is not. A closed stderr is not a reason to
-- lose the exit code: `hub inbox K 2>&1 | head` closes both handles, and an
-- unguarded 'hPutDoc' then leaves through the RTS with 1, which PEP-22 gives to
-- usage errors -- so the hook that 17 and 18 were added for saw the one code
-- they were added to be told apart from.
refuse :: String -> Int -> IO a
refuse msg n = do
  saying ("hbs2-hub:" <+> pretty msg <> line)
  exitWith (ExitFailure n)

-- | Write to stderr, or do not, but do not take the exit code with you.
saying :: Doc AnsiStyle -> IO ()
saying d =
  handleJust (\e -> if isResourceVanishedError e then Just () else Nothing)
             (\_ -> pure ())
             (hPutDoc stderr d)

-- | The peer is up and stopped answering.
--
-- Distinct from "no peer" (which fails before any verb runs) and from a usage
-- error, because the three call for different things: start it, fix the command,
-- and look at why it is stuck. It used to arrive as @user error (...)@ with exit
-- 1, under the ten GHC call stacks the socket layer logs on the way.
newtype PeerSilent = PeerSilent String

instance Show PeerSilent where
  show (PeerSilent what) =
    "the peer did not answer for " <> show what <> " within "
      <> show (pretty rpcTimeout) <> ". It is running, so this is not a"
      <> " connection problem: check `hbs2-peer poke` and the peer's log."

instance Exception PeerSilent

-- | What the peer not holding the mailbox exits with.
--
-- Above the range PEP-22 assigns to `hub verify`'s own refusals (3..16), and
-- added to that table rather than reusing one of them: the numbers are a
-- contract a hook branches on, so they may be added to and not reassigned.
codeMailboxUnknown :: Int
codeMailboxUnknown = 17

-- | And what a peer that stopped answering exits with.
codePeerSilent :: Int
codePeerSilent = 18

-- | What this verb takes, in the words somebody typing it would use.
inboxUsage :: Doc ann
inboxUsage = "usage: hub inbox [--mailbox] <mailbox-key>" <> line
          <> "  the hub's ingress mailbox key in base58. The peer must already"
          <> line <> "  hold it: `hbs2-peer mailbox create --key KEY hub`."
          <> line <> "  `hub help inbox` says more."

-- | The queue itself: one line per letter, on stdout.
inboxDoc :: InboxRead -> [Doc ann]
inboxDoc = fmap render . irLetters

-- | What is true about the list above, for stderr.
--
-- A list rather than an action, so that a test can ask what this run would say
-- without capturing a handle.
inboxNotes :: InboxRead -> [Doc ann]
inboxNotes r = settledNote <> missingNote <> omittedNote <> keymanNote <> policyNote
  where
    -- Truncation, said with a number. A mailbox is public, so how many letters
    -- are in it is a stranger's choice; this reader opens at most
    -- 'maxInboxLetters' of them and used to open all of them and hold every body
    -- resident. What it will not do is truncate quietly: a list that is missing
    -- letters is wrong, so it says how many and leaves non-zero.
    omittedNote
      | irOmitted r <= 0 = []
      | otherwise =
          [ "hub:" <+> pretty (irOmitted r) <+> "more letter(s) in this mailbox"
              <+> "were not opened:" <+> pretty maxInboxLetters <+> "is the most"
              <+> "one read will take. The list above is a prefix, in hash order,"
              <+> "and is therefore incomplete." ]

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
      | List.null (irLetters r) =
          [ "hub: the peer produced no copy of this mailbox within"
              <+> pretty maxFetchRounds <+> "rounds. That is NOT the same as an"
              <+> "empty mailbox: a mailbox with nothing in it and one still"
              <+> "downloading look alike from here. Try again in a moment." ]
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
          [ "hub:" <+> pretty (length (irMissing r))
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
      | any folds (irLetters r) =
          [ "hub: no deny-list was applied (this form takes a mailbox key, and"
              <+> "PEP-21 policy lives in the repo manifest), so \"(folds)\""
              <+> "means the admission rules would take it, not that its author"
              <+> "is allowed to send." ]
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
-- Missing blocks and a truncated read, and deliberately not an unsettled one: an
-- unsettled read is a SHORTER answer, and both of these are WRONG ones. A hole
-- in the tree loses letters and resurrects folded ones; a read that stopped at
-- 'maxInboxLetters' simply does not contain letters that are in the mailbox.
-- Both are 2, because the remedy is the same -- do not treat this list as the
-- mailbox -- and the numbers are a contract that may be added to, not
-- reassigned.
inboxCode :: InboxRead -> Int
inboxCode r
  | not (List.null (irMissing r)) = 2
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
render :: LetterView -> Doc ann
render lv = hashDoc (lvMessage lv) <+> maybe "-" keyDoc (lvEnvelope lv) <+> body
  where
    body = case lvLetter lv of
      Left e -> "unreadable:" <+> pretty e
      Right (author, content, disp) ->
        "author" <+> keyDoc author
          <+> "event" <+> maybe "?" hashDoc (lvEventId lv)
          -- The author's own clock, advisory and unverifiable (PEP-19), which is
          -- why it is shown and not sorted on: sorting the queue by it would let
          -- a sender choose where in the queue they appear.
          <+> "at" <+> pretty (utcOf (authorTs content))
          <+> opOf content
          <+> maybe "-" (\t -> "on" <+> hashDoc t) (authorThread content)
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

badService :: MonadUnliftIO m => MailboxServiceError -> m a
badService e = throwIO (userError (show ("mailbox service:" <+> viaShow e)))

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

-- | The one place the ingress is wired to a peer.
--
-- Top level and exported, because there are two callers now: listing a queue
-- and accepting one letter out of it. Two copies of this would be two answers
-- to what a hub asks a peer for, drifting apart at whichever of the six a
-- later fix touches.
--
-- Everything above it is a function of these six, which is what lets the wait
-- loop and every OpenError be tested without a peer.
overRpc :: (MonadUnliftIO m) => AnyStorage -> ServiceCaller MailboxAPI UNIX -> Ingress m
overRpc sto api = Ingress
  { -- BOUNDED, and it is the only one of these that was not. The other three
    -- go through 'callRpcWaitMay', which is 'race' against a pause; this one
    -- is the storage client's 'getBlock', which calls 'callService' raw, and
    -- that blocks on a TQueue with no timeout at all. Every merkle node and
    -- every message body comes through here, so the bulk of what `hub inbox`
    -- asks the peer for was the part with no bound: a peer that answered the
    -- mailbox service and then stalled on storage hung the verb forever,
    -- after all three timed calls had succeeded. Per block, not per walk --
    -- see 'bounded'.
    igBlock  = \h -> bounded rpcTimeout "a block of the mailbox"
                       (liftIO (getBlock sto (coerce h)))
    -- The size alone, which is the whole reason this is not `length <$>
    -- igBlock`: the gate that refuses an oversized attachment has to fire
    -- before the attachment is paid for.
  , igSize   = \h -> bounded rpcTimeout "the size of a block"
                       (liftIO (hasBlock sto (coerce h)))
    -- THE SECRET IS CAUGHT ON THE WAY PAST, in an IORef, and that is not a
    -- trick for want of a better one: 'ToDecryptBS' takes the resolver as a
    -- function and returns only the plaintext, so the value canon needs is
    -- visible exactly once, inside the call the reader makes. Widening the
    -- reader's result is the alternative and it is in hbs2-core, used by
    -- everything that reads an encrypted tree.
    --
    -- Called once per read by construction (the reader resolves one group key
    -- for the whole tree), so there is no last-writer question to answer.
  , igOpenPart = \h -> do
      seen <- liftIO (newIORef Nothing)
      -- Inline, because 'findSecret' is rank-2: a named binding would be
      -- monomorphic in the monad the reader instantiates it at.
      r <- liftIO $ runExceptT $ readFromMerkle sto
             (ToDecryptBS (coerce h) (\gk -> liftIO do
                 s <- runKeymanClientRO (extractGroupKeySecret gk)
                 for_ s (writeIORef seen . Just)
                 pure s))
      case r of
        Left e -> pure (Left (Text.pack (show e)))
        Right bs -> liftIO (readIORef seen) >>= \case
          Just s  -> pure (Right (bs, Saltine.encode s))
          -- Unreachable through the reader above, which throws when it cannot
          -- resolve. Answered rather than asserted because the alternative is
          -- publishing a part-secret this build invented.
          Nothing -> pure (Left "the part opened and its secret was not seen")
  , igStatus = \k ->
      callRpcWaitMay @RpcMailboxGetStatus rpcTimeout api k
        >>= silent "the mailbox service"
        >>= either badService (pure . void)
    -- Checked, not voided. 'void' threw away the Maybe, and the Maybe IS the
    -- timeout signal: a fetch that never reached the peer left the wait loop
    -- to conclude, correctly and uselessly, that a mailbox it had never
    -- asked for had not arrived.
  , igFetch  = \k ->
      void (callRpcWaitMay @RpcMailboxFetch rpcTimeout api k
              >>= silent "the mailbox service")
  , igRoot   = \k ->
      callRpcWaitMay @RpcMailboxGet rpcTimeout api k
        >>= silent "the mailbox service"
  , igPause  = pause
    -- No deny-list: PEP-21 policy lives in the repo manifest and this verb
    -- takes a mailbox key rather than a repo. Allowing everything is honest
    -- here, since listing is not accepting; the accept path must not
    -- inherit this default, and 'inboxNotes' says so on stderr whenever the
    -- queue actually contains something a reader might take for permission.
  , igAllowed = const True
  , igSecret = ReadMessageServices
      (liftIO . runKeymanClientRO . extractGroupKeySecret)
  }

-- A call that answered nothing in the time allowed is not an answer.
silent :: MonadUnliftIO m' => String -> Maybe a -> m' a
silent what = maybe (throwIO (PeerSilent what)) pure
