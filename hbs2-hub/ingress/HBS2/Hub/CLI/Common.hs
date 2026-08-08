-- | What every verb in this tool needs and no verb owns.
--
-- WHY THIS EXISTS. All of it lived in "HBS2.Hub.CLI.Inbox" -- the module that
-- implements @hub inbox@ -- and twelve other verb modules imported it from
-- there for their exit codes, their stderr writer and their peer wiring. So
-- @hub compact@ depended on the queue verb in order to know how to refuse, and
-- "HBS2.Hub.Deny" carries a note about routing around the near-cycle that
-- caused. None of what is here is about listing a queue.
--
-- THE EXIT CODES ARE A CONTRACT (PEP-22): a hook branches on them, so they may
-- be added to and never reassigned. Keeping them in one file is the point --
-- four separate tables in four verbs is four places to forget when one is
-- added.
module HBS2.Hub.CLI.Common
  ( refuse
  , saying
  , utcOf
  , overRpc
  , manifestCode
  , codeMailboxUnknown
  , codePeerSilent
  , OnMissing(..)
  , withCanon
  , withCanonState
  , blessed
  , WriteStop(..)
  , oneStop
  , committing
  ) where

import HBS2.Hub.Types (maxFoldedTs)
import HBS2.Hub.Ingress
import HBS2.Hub.Repo.Manifest (ManifestGone(..),codeNoManifest)
import HBS2.Hub.Repo
import HBS2.Hub.Fold (FoldResult,foldEvents)
import HBS2.Hub.Types (RepoRef,Event,ThreadId)
import HBS2.Hub.CLI.Verify (refusalDoc,codeOf)
import HBS2.Hub.Repo.GitWrite (withGitSink)
import HBS2.Hub.Bridge (TriageError)

import HBS2.CLI.Prelude

import HBS2.Data.Types.Refs (HashRef)
import HBS2.Net.Auth.GroupKeySymm
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage
import HBS2.Storage.Operations.Class (readFromMerkle)
import HBS2.Storage.Operations.ByteString
import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,extractGroupKeySecret)
import Control.Monad.Except (runExceptT)

import Crypto.Saltine.Class qualified as Saltine

import Data.Coerce (coerce)
import Data.Text qualified as Text
import Data.Word (Word64)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime,defaultTimeLocale)
import System.Exit (exitWith,ExitCode(..))
import System.IO.Error (isResourceVanishedError)

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


-- | The author's declared time as something a human reads. Epoch milliseconds are
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

-- | What a verb that could not resolve a mailbox should exit with.
--
-- Here rather than beside 'ManifestGone', because an exit code is this layer's
-- business and the reader below the CLI cannot import the module that owns
-- them. Silence keeps its own number: it is the one case where trying again is
-- the answer.
manifestCode :: ManifestGone -> Int
manifestCode = \case
  ManifestPeerSilent -> codePeerSilent
  _                  -> codeNoManifest

-- | Take what the bridge blessed, or stop with what the bridge said.
--
-- THE WORDS ARE THE POINT. Three of the four verbs that mint an event printed
-- @viaShow e@, which is the derived 'Show': a reader whose pull request arrived
-- with nothing to fetch was told @BadContent CoordsUnreachable@. 'TriageError'
-- has a hand-written 'Pretty' instance for exactly this moment, written so that
-- the refusal is a sentence a person can act on -- and it names, among other
-- things, the one refusal in that family a SENDER can fix.
--
-- One function rather than four spellings of it, so a fifth verb cannot get
-- this wrong a fifth time.
blessed :: MonadUnliftIO m => Int -> Either TriageError a -> m a
blessed code = either (\e -> liftIO (refuse (show ("refused:" <+> pretty e)) code)) pure

-- | What to exit with when a canon write does not happen.
--
-- Two codes and not one because they are two different things: an event that
-- will not render to a file is this build's bug, and a commit git refuses is
-- the repository's state. A record and not two positional 'Int's, because two
-- adjacent arguments of one type are two arguments that can be swapped in
-- silence.
data WriteStop =
  WriteStop
  { wsUnplannable :: Int  -- ^ the event will not render to a canon file
  , wsUnwritable  :: Int  -- ^ git would not write the commit
  }

-- | One code for every way of stopping.
--
-- Not a shortcut: @hub pr merge@ and the maintainer verbs each say in their own
-- words that every way of stopping means the same thing to whoever ran them --
-- nothing was published and the repository is as it was. This is that decision
-- spelled once instead of being lost in a pair of identical arguments.
oneStop :: Int -> WriteStop
oneStop c = WriteStop c c

-- | Plan a canon write and commit it: the tail of every verb that writes.
--
-- Four copies of it, and the order they share is the load-bearing part.
-- Minting and planning write NOTHING, so a refusal anywhere above the commit
-- leaves canon exactly as it was, and the commit is the only step that
-- publishes. A fifth verb that wrote this out again could get the order right
-- and the codes wrong, or the codes right and the order wrong; there is no
-- reason for it to write it out at all.
committing :: MonadUnliftIO m
           => WriteStop
           -> Maybe Text            -- ^ the parent, and what the ref must still hold
           -> [(FilePath, Event)]
           -> [(Word64, ThreadId)]  -- ^ the number index to regenerate
           -> Text                  -- ^ the commit message
           -> Word64                -- ^ now, in milliseconds
           -> m Text                -- ^ the commit written
committing WriteStop{..} parent files numbers message now = do
  plan <- either (\e -> liftIO (refuse (show (pretty e)) wsUnplannable)) pure
            (planCanon files numbers)

  commit <- withGitSink (\sk -> skCommit sk (CanonWrite parent (cwFiles plan) message now))
              >>= either (\e -> liftIO (refuse (show (pretty e)) wsUnwritable)) pure

  -- A TRUNCATED NUMBER INDEX IS SAID OUT LOUD BY EVERY VERB THAT WRITES ONE.
  --
  -- Every canon write regenerates @index/number.sexp@ from the whole fold, so
  -- every one of them can overflow it -- but only `hub inbox accept` mentioned
  -- it. The other three rewrote a truncated index in silence, which is the
  -- half of "not an error and not silent" that was missing.
  --
  -- On stderr, because it is advice about a convenience map and not part of
  -- what the verb produced. The report a verb prints on stdout is the event,
  -- the seq and the commit.
  when (cwIndexOmitted plan > 0) $ liftIO $ saying
    ( "note:" <+> pretty (cwIndexOmitted plan)
        <+> "number(s) did not fit index/number.sexp;"
        <+> "it is a convenience map and is regenerable" <> line )

  pure commit

badService :: MonadUnliftIO m => MailboxServiceError -> m a
badService e = throwIO (userError (show ("mailbox service:" <+> viaShow e)))

silent :: MonadUnliftIO m' => String -> Maybe a -> m' a
silent what = maybe (throwIO (PeerSilent what)) pure

-- | What a verb does when this repository has no canon yet.
--
-- The rule the eleven canon reads followed and none of them stated. It has two
-- halves and they divide cleanly: a verb that needs canon to HOLD something
-- refuses, and a verb that ASKS canon a question answers "no". @inbox accept@
-- folds the first event into a repository that has none, so it treats the
-- absence as empty; @pr merge@ needs a thread that is already there, so it does
-- not.
--
-- Written down here because it was not written anywhere: each verb spelled its
-- own answer, and `hub issue close` in a repository nobody has folded into
-- refused with "canon is unreadable" while `hub inbox accept` in the same one
-- started from an empty fold, and nothing said which was intended.
data OnMissing =
    -- | No canon is an empty canon. For the verbs that CREATE it.
    TreatAsEmpty
    -- | No canon is a refusal, with the reader's own code and its remedy.
  | Refuse
    -- | The same, with a code and a sentence of the verb's own. One caller:
    -- @hub compact@, for which "there is no canon" is the same event as
    -- "there is nothing superseded to drop", and a scheduled run should read
    -- one number for both.
  | RefuseWith Int (Doc AnsiStyle)

-- | Canon for this repository: the commit it is at, and the fold over it.
--
-- 'Nothing' for the commit means canon does not exist yet, which only
-- 'TreatAsEmpty' can produce -- the fold beside it is then the fold of no
-- events, which is what a first accept mints against.
--
-- EVERY refusal goes through 'refusalDoc', which is the other half of what
-- this collapses: ten of the eleven printed a bare `pretty e` and only the
-- listing verb printed the REMEDY. A plain clone does not fetch
-- @refs\/hbs2\/meta@, so "no canon here" is the first thing a new reader meets
-- and the fetch line is the one sentence they need.
withCanon :: (MonadUnliftIO m)
          => OnMissing
          -> RepoRef
          -> (forall a . (CanonSource m -> m a) -> m a)  -- ^ usually 'withGitCanon'
          -> m (Maybe Text, FoldResult)
withCanon onMissing repo runIt = runIt (\cs -> readCanon cs repo) >>= \case
  Right st -> pure (Just (stCommit st), stFold st)
  Left NoCanonRef{} | TreatAsEmpty <- onMissing -> pure (Nothing, foldEvents repo [])
  Left NoCanonRef{} | RefuseWith n d <- onMissing -> liftIO do
    saying (d <> line)
    exitWith (ExitFailure n)
  Left e -> refusedBy e
  where
    refusedBy e = liftIO do
      saying (refusalDoc e <> line)
      exitWith (ExitFailure (codeOf e))

-- | The same, for the one caller that needs the FILES and not just the fold.
--
-- @hub compact@ writes the retained events back under the names the tree held
-- them at, so it needs 'stEvents', which the fold does not carry. Its own entry
-- point rather than a wider return type for the other ten: a caller handed a
-- 'CanonState' it does not need is a caller that can reach into it.
withCanonState :: (MonadUnliftIO m)
               => OnMissing
               -> RepoRef
               -> (forall a . (CanonSource m -> m a) -> m a)
               -> m CanonState
withCanonState onMissing repo runIt = runIt (\cs -> readCanon cs repo) >>= \case
  Right st -> pure st
  Left NoCanonRef{} | RefuseWith n d <- onMissing -> liftIO do
    saying (d <> line)
    exitWith (ExitFailure n)
  Left e -> liftIO do
    saying (refusalDoc e <> line)
    exitWith (ExitFailure (codeOf e))
