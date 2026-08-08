-- | @hub issue close|reopen|label|assign@ and @hub redact@ (PEP-19, PEP-22
-- "Maintain").
--
-- The owner-native ops on a thread that is already in canon. Everything they
-- decide is decided elsewhere -- 'ownerEvent' says whether the key may sign,
-- the fold says whether the event will be admitted -- and what is here is the
-- argument reading, the thread lookup and the refusals.
--
-- WHY THEY ARE SEPARATE FROM THE LETTER PATH. A stranger may ASK for any of
-- these (PEP-18 carries @close@, @reopen@ and @set@ as letter ops), and what
-- arrives is a request: the bridge refuses to bless it as the requester's own
-- event, and honouring it is the owner re-authoring it under their own key.
-- These verbs are the other end, where the owner acts without being asked.
--
-- A NUMBER AND NOT A THREAD-ID, because the number is what a person has: it is
-- what @hub issue list@ prints and what an issue is called in conversation.
-- The id is derived from it through the fold, which is the same lookup every
-- reader makes, so a number that names nothing is a refusal here rather than an
-- event minted against a thread canon does not hold.
module HBS2.Hub.CLI.Own
  ( ownEntries
  , ownUsage
  , redactUsage
  , OwnArgs(..)
  , ownArgs
  , RedactArgs(..)
  , redactArgs
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Bridge
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.CLI.Argv ( flagsOf,flagsAndSwitches,flagOnce,flagEvery,flagMaybe
                         , repoFlags,flagRepo,flagRepoMaybe
                         , flagSwitch,flagText,flagWord )
import HBS2.Hub.CLI.Publish (notPublishedYet)
import HBS2.Hub.CLI.Common (refuse,saying,withCanon,OnMissing(..)
                           ,blessed,committing,oneStop,signerFor,signingPair)
import HBS2.Hub.CLI.Read (codeNoSuchThread)
import HBS2.Hub.CLI.Accept (codeNoCanonKey,codeTriageRefused,codeCanonUnwritable)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (pattern HashLike)

import Data.List qualified as List
import Data.Maybe (fromMaybe,listToMaybe)
import Data.Text qualified as Text
import Data.Word (Word64)
import System.Exit (die)

-- | What a close, reopen, label or assign was asked to do.
data OwnArgs = OwnArgs
  { owRepo   :: RepoRef
  , owNumber :: Word64
  , owNote   :: Maybe Text     -- ^ close/reopen only
  , owLabels :: [Text]         -- ^ label only
  , owClear  :: Bool           -- ^ label and assign: the empty value, said out loud
    -- | Whom to assign to. Assign only.
  , owTo     :: Maybe HubKey
  , owAs     :: Maybe HubKey   -- ^ a delegate's key; defaults to the repo key
  }
  deriving stock (Eq,Show)

-- | What a redact was asked to hide.
data RedactArgs = RedactArgs
  { rdRepo  :: RepoRef
  , rdEvent :: EventId
  , rdAs    :: Maybe HubKey
  }
  deriving stock (Eq,Show)

ownUsage :: Doc ()
-- Both nouns, because both are bound: a thread is an issue or a pull request
-- and these ops care about neither.
ownUsage =
  "usage: hbs2-hub issue|pr close|reopen --repo <key> --number <n> [--note <text>] [--as <key>]"
    <> line <> "       hbs2-hub issue|pr label --repo <key> --number <n> --label <l>... [--as <key>]"
    <> line <> "       hbs2-hub issue|pr label --repo <key> --number <n> --clear"
    <> line <> "       hbs2-hub issue|pr assign --repo <key> --number <n> --to <key> | --clear"

redactUsage :: Doc ()
redactUsage =
  "usage: hbs2-hub redact --repo <key> --event <event-id> [--as <key>]"

ownEntries :: forall c m . ( IsContext c
                           , MonadUnliftIO m
                           , Exception (BadFormException c)
                           ) => MakeDictM c m ()
ownEntries = do

  -- BOTH NOUNS, and it is the same verb under each.
  --
  -- A thread is an issue or a pull request and these ops care about neither:
  -- they name a thread by number and write an attribute, and @AClose@ carries
  -- no kind. Binding only @issue@ meant closing a pull request was spelled
  -- `hub issue close --number <pr number>` -- which works, since the number
  -- index does not filter on kind, and which nobody guesses. `hub pr comment`
  -- was already bound both ways for exactly this reason; the other four were
  -- not.
  for_ ["issue","pr"] $ \noun -> do

    statusVerb (verb noun "close") "Marks a thread closed."
      "close a thread that is in canon"
      (\thr ow now -> AClose thr (owNote ow) now)

    statusVerb (verb noun "reopen") "Marks a closed thread open again."
      "reopen a thread that is in canon"
      (\thr ow now -> AReopen thr (owNote ow) now)

    labelVerb (verb noun "label")
    assignVerb (verb noun "assign")

  brief "hide an event's content in every clone that folds this canon"
    $ args [ arg "string" "--repo repo-key", arg "string" "--event event-id" ]
    $ desc ( "DISPLAY-LEVEL, and PEP-19 says so: the event stays in canon and"
             <> line <> "every clone still holds the bytes. What changes is that"
             <> line <> "readers stop showing the body. It is the answer to a"
             <> line <> "mistake or to abuse, not to a secret: a secret that"
             <> line <> "reached canon has been published."
             <> line
             <> line <> "The event-id, not a number: a redact names one event, and"
             <> line <> "the thing worth hiding is as often a comment as an open." )
    $ entry $ bindMatch "hub:redact" $ nil_ \case
        (redactArgs -> Just rd) -> lift (redactIt rd)
        _ -> liftIO (die (show redactUsage))

  where

    -- `hub:<noun>:<op>`, built rather than written twice.
    verb noun op = fromString ("hub:" <> noun <> ":" <> op)

    labelVerb name =
      brief "set the labels of a thread that is in canon"
        $ args [ arg "string" "--repo repo-key", arg "string" "--number n"
               , arg "string" "--label label | --clear"
               , arg "string" "[--as canon-key]" ]
        $ desc ( "Writes an owner-signed set event. The labels REPLACE what the"
                 <> line <> "thread had: PEP-19 makes an attribute last-writer-wins,"
                 <> line <> "and labels are one value, so adding to a set means naming"
                 <> line <> "the whole set."
                 <> line
                 <> line <> "That is also why removing them all needs --clear rather"
                 <> line <> "than an omitted --label: a verb that cleared the labels"
                 <> line <> "because somebody forgot an argument would be a verb that"
                 <> line <> "publishes a mistake into append-only canon."
                 <> line
                 <> line <> "A LABEL AN AUTHOR ASKED FOR IS NOT A LABEL. A letter's"
                 <> line <> "labels are a request (PEP-18); this is the owner applying"
                 <> line <> "one, and it is the only thing a reader shows as a label." )
        $ entry $ bindMatch name $ nil_ \case
            (ownArgs -> Just ow)
              | owClear ow || not (List.null (owLabels ow)) -> lift (labelIt ow)
            _ -> liftIO (die (show ownUsage))

    assignVerb name =
      brief "say who is looking at a thread that is in canon"
        $ args [ arg "string" "--repo repo-key", arg "string" "--number n"
               , arg "string" "--to key | --clear"
               , arg "string" "[--as canon-key]" ]
        $ desc ( "Writes an owner-signed set event on the assignee attribute,"
                 <> line <> "which is last-writer-wins like every other (PEP-19): one"
                 <> line <> "thread has one assignee, and assigning replaces."
                 <> line
                 <> line <> "A KEY AND NOT A NAME. A person here is a key; a name would"
                 <> line <> "be a second identity with nothing behind it, and canon has"
                 <> line <> "no notion of one. It is not checked against the maintainer"
                 <> line <> "set either: assigning somebody is a note about who is"
                 <> line <> "looking, not a grant of anything, and PEP-21 delegation is"
                 <> line <> "what grants."
                 <> line
                 <> line <> "--clear unassigns, and is spelled out for the reason it is"
                 <> line <> "on labels: a verb that unassigned because somebody forgot"
                 <> line <> "an argument would publish that into append-only canon." )
        $ entry $ bindMatch name $ nil_ \case
            (ownArgs -> Just ow)
              | owClear ow, Nothing <- owTo ow -> lift (assignIt ow)
              | Just _ <- owTo ow, not (owClear ow) -> lift (assignIt ow)
            _ -> liftIO (die (show ownUsage))

    statusVerb name lead what mk =
      brief what
        $ args [ arg "string" "--repo repo-key", arg "string" "--number n"
               , arg "string" "[--note text]", arg "string" "[--as canon-key]" ]
        $ desc ( lead
                 <> line
                 <> line <> "Writes an owner-signed event onto canon. The status follows"
                 <> line <> "from the op itself (PEP-19), so no separate set is"
                 <> line <> "written and none should be: canon would claim the"
                 <> line <> "thread was open until the second event arrived."
                 <> line
                 <> line <> "--note is published as a comment attached to the"
                 <> line <> "status change, in the owner's own words. A note a"
                 <> line <> "stranger phrased is not signed here: that is what"
                 <> line <> "honouring a request is for."
                 <> line
                 <> line <> "--as names a delegate's key (PEP-21); it defaults to"
                 <> line <> "the repository key." )
        $ entry $ bindMatch name $ nil_ \case
            (ownArgs -> Just ow) -> lift (statusIt mk ow)
            _ -> liftIO (die (show ownUsage))

    canonOf repo =
      -- Refuse: every verb here names a thread by number, and a repository with
      -- no canon has none. Its sibling in Maintainer treats the absence as
      -- empty, because a delegation CAN be the first event canon holds.
      withCanon Refuse repo withGitCanon

    -- The number to the thread it names, through the fold's own index. A
    -- number nobody minted is a refusal and not an event: minting against a
    -- thread canon does not hold is exactly what the fold drops as BadThread,
    -- with the seq already spent.
    threadOfNumber fr n =
      listToMaybe [ t | (n', t) <- numberIndexOf fr, n' == n ]
        & maybe (liftIO (refuse (show ("canon holds no thread numbered"
                                         <+> pretty n))
                                codeNoSuchThread))
                pure

    statusIt mk ow = do
      (parent, fr) <- canonOf (owRepo ow)
      thr <- threadOfNumber fr (owNumber ow)
      writeOwn (owRepo ow) (owAs ow) parent fr
        (\now -> mk thr ow now) "hub: status"

    labelIt ow = do
      (parent, fr) <- canonOf (owRepo ow)
      thr <- threadOfNumber fr (owNumber ow)
      -- Through 'encodeLabels', which is what makes the value canonical: the
      -- bridge refuses an unnormalized one, and two maintainers applying the
      -- same labels in a different order must produce the same event.
      let value = encodeLabels (if owClear ow then [] else owLabels ow)
      writeOwn (owRepo ow) (owAs ow) parent fr
        (ASet thr "labels" value) "hub: labels"

    assignIt ow = do
      (parent, fr) <- canonOf (owRepo ow)
      thr <- threadOfNumber fr (owNumber ow)
      -- Base58, which is how every other key in canon is written, and empty for
      -- a clear: the attribute is last-writer-wins and there is no way to
      -- remove one, so an empty value IS the absence a reader shows as none.
      let value = maybe "" (Text.pack . show . pretty . AsBase58) (owTo ow)
      writeOwn (owRepo ow) (owAs ow) parent fr
        (ASet thr "assignee" value) "hub: assignee"

    redactIt rd = do
      (parent, fr) <- canonOf (rdRepo rd)
      writeOwn (rdRepo rd) (rdAs rd) parent fr
        (ARedact (rdRepo rd) (rdEvent rd)) "hub: redact"

    -- The shared tail: sign, mint, plan, commit. The same order 'hub inbox
    -- accept' uses, and for the same reason -- minting writes nothing, so a
    -- refusal anywhere above the commit leaves canon untouched.
    writeOwn repo mas parent fr mk message = do
      let signer = fromMaybe repo mas

      creds <- signerFor signer
               >>= maybe (liftIO (refuse (show ( "cannot sign as"
                                                   <+> pretty (AsBase58 signer)
                                                   <> line
                                                   <> "  no keyring here holds it as its own"
                                                   <+> "signing key" ))
                                         codeNoCanonKey))
                         pure

      now <- liftIO getPOSIXTime <&> floor . (* 1000)

      let ctx = TriageCtx (signingPair creds) (const True) repo

      acc <- blessed codeTriageRefused
               (ownerEvent ctx (viewOf fr) now noOwnAttachments (mk now))

      commit <- committing (oneStop codeCanonUnwritable) parent
                  [(eventPath acc, acEvent acc)] (numberIndexOf fr) message now

      liftIO $ print $ vcat
        [ "event" <+> pretty (eventId (acEvent acc))
        , "seq" <+> pretty (acSeq acc)
        , "commit" <+> pretty commit
        ]

      liftIO (saying (notPublishedYet <> line))

-- | @--repo K --number N [--note T] [--label L]... [--clear] [--as K]@.
--
-- One reader for the three thread verbs, because they differ in which optional
-- values they use and not in how a value is spelled. Every one of them is
-- behind a flag for the reason the whole package's are: a repo key and a
-- delegate key are the same thirty-two bytes of base58.
ownArgs :: forall c . IsContext c => [Syntax c] -> Maybe OwnArgs
ownArgs syn = do
  kvs   <- flagsAndSwitches (repoFlags <> ["--number","--note","--label","--to","--as"])
                            ["--clear"] syn
  repo  <- flagRepo asKey kvs
  n     <- flagOnce kvs "--number" >>= flagWord
  note  <- flagMaybe kvs "--note" (fmap Text.pack . flagText)
  as    <- flagMaybe kvs "--as" asKey
  ls    <- traverse (fmap Text.pack . flagText) (flagEvery kvs "--label")
  -- A switch, so it takes no value and cannot swallow the next word.
  clear <- flagSwitch kvs "--clear"
  to    <- flagMaybe kvs "--to" asKey
  pure (OwnArgs repo n note ls clear to as)
  where
    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }

redactArgs :: forall c . IsContext c => [Syntax c] -> Maybe RedactArgs
redactArgs syn = do
  kvs  <- flagsOf (repoFlags <> ["--event","--as"]) syn
  repo <- flagRepo asKey kvs
  e    <- flagOnce kvs "--event" >>= asHash
  as   <- flagMaybe kvs "--as" asKey
  pure (RedactArgs repo e as)
  where
    asKey  = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    asHash = \case { HashLike h -> Just h ; _ -> Nothing }
