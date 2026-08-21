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
  , mailboxHoles
  , holesDoc
  , codeMailboxIncomplete
  , codeMailboxUnknown
  , codePeerSilent
  , signerFor
  , signerOf
  , askingKeyman
  , codeNoKeyman
  , signingPair
  , OnMissing(..)
  , withCanon
  , withCanonState
  , blessed
  , WriteStop(..)
  , Writing(..)
  , writingOf
  , dryRunHelp
  , rehearsalDoc
  , oneStop
  , committing
  ) where

import HBS2.Hub.Types (maxFoldedTs,HubKey,HubScheme)
import Data.Word (Word32)
import HBS2.Hub.Ingress
import HBS2.Hub.Repo.Manifest (ManifestGone(..),codeNoManifest)
import HBS2.Hub.Repo
import HBS2.Hub.Fold (FoldResult,foldEvents)
import HBS2.Hub.Types (RepoRef,Event,ThreadId,eventId)
import HBS2.Hub.CLI.Verify (refusalDoc,codeOf)
import HBS2.Hub.CLI.Say (saying,refuse)
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
import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,extractGroupKeySecret,loadCredentials
                               ,KeyManClient)
import HBS2.Net.Auth.Credentials (PeerCredentials,_peerSignPk,_peerSignSk)
import Control.Monad.Except (runExceptT)

import Crypto.Saltine.Class qualified as Saltine

import Data.ByteString.Char8 qualified as BS8
import Data.Coerce (coerce)
import Data.List qualified as List
import HBS2.Base58 (AsBase58(..))
import Data.Text qualified as Text
import Data.Word (Word64)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime,defaultTimeLocale)
import System.Exit (exitWith,ExitCode(..))

-- The two writers every verb leaves through are re-exported from here, because
-- every verb already imports this module for them. They LIVE in
-- "HBS2.Hub.CLI.Say", which is below this one: the module that prints `hub
-- verify`'s refusals could not import this (this imports it) and so wrote to
-- stderr by hand, along with two others. See that module for the two rules a
-- hand-written write has to remember, and had not.

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

-- | The secret key for EXACTLY the key named, and nothing otherwise.
--
-- WHY THIS IS NOT 'loadCredentials'. keyman resolves a key to the FILE that
-- holds it and returns that file's credentials, whose sign key is the file's
-- PRIMARY one:
--
-- > select f.file from keytype t join keyfile f on t.key = f.key
-- >   where t.key = ? and t.type = 'sign' order by w.weight desc limit 1
--
-- For a key that is a SECONDARY in its keyring, @_peerSignSk@ of that record is
-- somebody else's secret. Every canon writer here used it and then declared the
-- key it had asked for, so the event was signed by one key and attributed to
-- another: the verb printed an event id, a seq and a commit, exited 0, and the
-- fold dropped it as 'BadAuthorSig' in every clone including this one. Nothing
-- downstream could catch it, because the bridge is handed a secret key and told
-- whose it is -- which is exactly why the check belongs on this side of that
-- boundary and in one place.
--
-- Answers rather than refuses. Four verbs want an exit code and one
-- ("HBS2.Hub.CLI.Drop") wants a value, and the three states are not the same
-- refusal: no key at all, a key this keyring cannot sign as, and a signable
-- key. The middle one is the one that used to be invisible.
--
-- Returns the whole record because one caller needs the keyring to seal an
-- acknowledgement; 'signingPair' is how the other four get what they wanted.
signerFor :: (MonadUnliftIO m) => HubKey -> m (Maybe (PeerCredentials HubScheme))
signerFor k = signerOf k <$> askingKeyman (loadCredentials k)

-- | What this node exits with when it has no key database at all.
--
-- Distinct from every "no key" code, because it is a different sentence with a
-- different remedy: a key that is not in the database is asked for by name, and
-- a database that is not there is the fresh install.
codeNoKeyman :: Int
codeNoKeyman = 51

-- | Ask keyman, or say why there was nothing to ask.
--
-- WHAT THIS CATCHES. keyman is a SQLite file under XDG, created by
-- @hbs2-keyman@; on a machine that has never run it, every call here throws
-- whatever the sqlite bindings throw, and it left through the RTS as
-- @SQLite3 returned ErrorCan't open database file@ at exit 1 -- the code PEP-22
-- reserves for usage errors. That is `hub whoami`, the first command in the
-- manual, in the one case the manual is written for.
--
-- BROAD ON PURPOSE, and this is the argument for it: everything downstream of
-- this call is "what keyman answered", and there is no answer this tool can act
-- on when the call itself did not return. The alternative is naming the sqlite
-- exception types here, which ties the hub to the bindings a dependency of a
-- dependency happens to use.
askingKeyman :: MonadUnliftIO m => KeyManClient m a -> m a
askingKeyman act =
  tryAny (runKeymanClientRO act) >>= \case
    Right a -> pure a
    Left _  -> liftIO $ refuse (show ( "no key database on this machine"
                                         <> line <> "  hbs2-keyman keeps the keys this"
                                         <+> "tool signs and reads with, and has"
                                         <> line <> "  not been run here. `hbs2-keyman"
                                         <+> "list` creates it and says what it holds;"
                                         <> line <> "  `hbs2-hub whoami` then says what"
                                         <+> "of it this tool can use." ))
                                codeNoKeyman

-- | The rule itself, without the keyman call around it.
--
-- Separate and exported because the call site is unreachable from a test (it
-- wants a key database) while the decision is the whole of the fix, and a fix
-- nothing asserts is a fix somebody removes as a redundant comparison.
signerOf :: HubKey
         -> Maybe (PeerCredentials HubScheme)  -- ^ what keyman answered
         -> Maybe (PeerCredentials HubScheme)
signerOf k = \case
  Just creds | _peerSignPk creds == k -> Just creds
  _                                   -> Nothing

-- | The pair a 'HBS2.Hub.Bridge.TriageCtx' takes, from one record.
--
-- Both halves out of the same credentials, which is the point: assembled by
-- hand at each call site, the public half came from the caller's argument and
-- the private half from keyman, and nothing said they had to be the same
-- identity. See 'signerFor'.
signingPair :: PeerCredentials HubScheme -> (HubKey, PrivKey 'Sign HubScheme)
signingPair c = (_peerSignPk c, _peerSignSk c)


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

-- | The holes in a mailbox tree, when a membership answer over it has any.
--
-- 'hub inbox' has said for a long time that a hole makes its list wrong in BOTH
-- directions -- a missing chunk of @Exists@ entries makes letters vanish, one of
-- @Deleted@ entries puts settled letters back -- and exits 2 for it. The two
-- verbs that ask a MEMBERSHIP question over the same walk read only 'mlLive'
-- and 'mlSettled', so the reasoning stopped at the verb that only lists and did
-- not reach the one that publishes to every clone forever.
--
-- One function because the two verbs must not answer this differently, and
-- 'Maybe' rather than a Bool so the caller has the hashes to print: a hash is
-- the one thing anybody can act on.
mailboxHoles :: MailboxLive -> Maybe [HashRef]
mailboxHoles ml
  | List.null (mlMissing ml) = Nothing
  | otherwise                = Just (mlMissing ml)

-- | And what to say about them.
--
-- Bounded, on the rule every report over a stranger's bytes here follows: the
-- tree is a stranger's and so is the length of this list.
holesDoc :: HubKey -> HashRef -> [HashRef] -> Doc AnsiStyle
holesDoc mbox msg hs =
  pretty (length hs) <+> "block(s) of mailbox" <+> pretty (AsBase58 mbox)
    <+> "could not be read," <> line
    <> "  so whether" <+> pretty msg <+> "is in it cannot be answered either way:"
    <> line
    <> "  a missing chunk of Exists entries hides letters, one of Deleted"
      <+> "entries" <> line
    <> "  brings back letters that were settled." <> line
    <> indent 2 (vsep (fmap pretty (take 10 hs))) <> line
    <> "  `hbs2-peer download <hash>` asks for one. Nothing was written."

-- | A membership answer that cannot be believed, over a tree with holes in it.
--
-- Its own code, and NOT 'codeLetterUnreadable': nothing is wrong with the
-- letter, nothing is wrong with this node, and the remedy is neither retyping
-- the command nor giving up -- it is fetching some blocks. A script sweeping a
-- queue wants to come back to this one rather than treat it as decided.
codeMailboxIncomplete :: Int
codeMailboxIncomplete = 50

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

-- | Whether a canon write actually writes.
--
-- WHAT --dry-run IS FOR HERE, and it is not the usual caution about a risky
-- command. Canon is append-only: a wrong event cannot be taken back, only
-- answered by another event, and every verb below mints one from an owner or
-- maintainer signature over arguments that are thirty-two bytes of base58 each.
-- The rehearsal is how somebody sees which thread they are about to close, or
-- which key they are about to delegate, before the signature exists.
--
-- Named rather than 'Bool', because the argument next to it is also a flag and
-- @committing stop True parent@ would compile whichever way round they went.
data Writing = ForReal | DryRun
  deriving stock (Eq,Show)

-- | What @--dry-run@ means, for the help of every verb that takes it.
--
-- ONE WORDING IN ONE PLACE. The switch does the same thing in each of them
-- (see 'Writing'), and seven help texts written one at a time would be seven
-- wordings that drift, of which some would end up describing a rehearsal that
-- skips a check it does not skip.
dryRunHelp :: Doc ann
dryRunHelp =
  line
    <> line <> "--dry-run signs nothing and writes nothing. It prints the event"
    <> line <> "these arguments would mint and the file it would go into, after"
    <> line <> "running every check the real thing runs. Canon is append-only:"
    <> line <> "a wrong event is answered by another event, never withdrawn."

-- | A parsed @--dry-run@ as the thing it means.
--
-- One line, in one place, because every writing verb parses the same switch and
-- @if dry then ForReal else DryRun@ is a mistake that reads correctly.
writingOf :: Bool -> Writing
writingOf dry = if dry then DryRun else ForReal

-- | One code for every way of stopping.
--
-- Not a shortcut: @hub pr merge@ and the maintainer verbs each say in their own
-- words that every way of stopping means the same thing to whoever ran them --
-- nothing was published and the repository is as it was. This is that decision
-- spelled once instead of being lost in a pair of identical arguments.
oneStop :: Int -> WriteStop
oneStop c = WriteStop c c

-- | What a rehearsal says.
--
-- THE EVENT AND THE FILES, which is the half the operator could not work out
-- from their own command line: the event id is a hash over what was minted, and
-- the path carries the seq canon gave it. A report that read back the arguments
-- would tell them what they typed.
--
-- Pure and separate for the reason 'signerOf' is: the branch it belongs to sits
-- directly above a git write in the ambient repository, so a test that called
-- 'committing' would be one broken branch away from committing into whatever
-- clone the suite is standing in.
rehearsalDoc :: Maybe Text            -- ^ the parent it would commit onto
             -> [(FilePath, Event)]   -- ^ what it would mint
             -> Text                  -- ^ the commit message
             -> CanonCommit           -- ^ and the whole tree that would be written
             -> [Doc ann]
rehearsalDoc parent files message plan =
  [ "dry run: nothing was written, and canon is as it was" ]
    <> [ maybe "this would be the first commit on refs/hbs2/meta"
               (("onto" <+>) . pretty) parent ]
    <> [ "would mint" <+> pretty (eventId e) <+> "at" <+> pretty p
       | (p, e) <- files ]
    <> [ "would write" <+> pretty (length (cwFiles plan)) <+> "file(s):" ]
    <> [ "  " <> pretty (BS8.unpack p) | (p, _) <- cwFiles plan ]
    <> [ "message" <+> pretty message ]

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
           -> Writing               -- ^ or a rehearsal of it
           -> Maybe Text            -- ^ the parent, and what the ref must still hold
           -> Maybe Word32          -- ^ the version the tree already declared
           -> [(FilePath, Event)]
           -> [(Word64, ThreadId)]  -- ^ the number index to regenerate
           -> Text                  -- ^ the commit message
           -> Word64                -- ^ now, in milliseconds
           -> m Text                -- ^ the commit written
committing WriteStop{..} writing parent declared files numbers message now = do
  plan <- either (\e -> liftIO (refuse (show (pretty e)) wsUnplannable)) pure
            -- Fresh events, so no file has a version to preserve: they are
            -- being written under this build's rules and say so.
            (planCanon declared (const Nothing) files numbers)

  -- HERE, WHICH IS AS LATE AS A REHEARSAL CAN STOP AND STILL BE ONE.
  --
  -- Everything above this has already happened: the caller has read canon,
  -- decided, signed, and the event is planned into files. None of it wrote --
  -- the commit below is the only step that publishes -- so the rehearsal has
  -- run every check the real thing runs, including the ones that refuse, and
  -- the operator sees the event id their arguments produced rather than a
  -- restatement of the arguments.
  --
  -- Exits rather than returning, because a verb's remaining work is downstream
  -- of the commit: `hub inbox accept` acks the letter and drops it, `hub pr
  -- merge` has a branch to move. Returning a commit that does not exist would
  -- make every caller decide again what a dry run means, and one of them would
  -- decide wrong.
  case writing of
    ForReal -> pure ()
    DryRun  -> liftIO do
      mapM_ print (rehearsalDoc parent files message plan)
      when (cwIndexOmitted plan > 0) $
        saying ( "note:" <+> pretty (cwIndexOmitted plan)
                   <+> "number(s) would not fit index/number.sexp" <> line )
      exitWith ExitSuccess

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
