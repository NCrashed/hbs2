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
  , ownArgsFor

  , RedactArgs(..)
  , redactArgs
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Bridge
import HBS2.Hub.Repo
import HBS2.Hub.Fold (frMeta)
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.CLI.Argv ( flagsOf,flagsAndSwitches,flagOnce,flagEvery,flagMaybe
                         , repoFlags,flagRepo,flagRepoMaybe
                         , flagSwitch,flagText,flagWord 
                         ,flagOneOfMaybe,assigneeFlags)
import HBS2.Hub.CLI.Publish (notPublishedYet)
import HBS2.Hub.CLI.Common (refuse,saying,withCanon,OnMissing(..)
                           ,blessed,committing,oneStop,signerFor,signingPair
                           ,Writing,writingOf,dryRunHelp)
import HBS2.Hub.CLI.Read (codeNoSuchThread,oneNumbered)
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
  , owDry    :: Writing        -- ^ --dry-run
  }
  deriving stock (Eq,Show)

-- | What a redact was asked to hide.
data RedactArgs = RedactArgs
  { rdRepo  :: RepoRef
  , rdEvent :: EventId
  , rdAs    :: Maybe HubKey
  , rdDry   :: Writing
  }
  deriving stock (Eq,Show)

ownUsage :: Doc ()
-- Both nouns, because both are bound: a thread is an issue or a pull request
-- and these ops care about neither.
ownUsage =
  "usage: hbs2-hub issue|pr close|reopen --repo <key> --number <n> [--note <text>] [--as <key>] [--dry-run]"
    <> line <> "       hbs2-hub issue|pr label --repo <key> --number <n> --label <l>... [--as <key>] [--dry-run]"
    <> line <> "       hbs2-hub issue|pr label --repo <key> --number <n> --clear [--dry-run]"
    <> line <> "       hbs2-hub issue|pr assign --repo <key> --number <n> --assignee <key> | --clear [--dry-run]"

redactUsage :: Doc ()
redactUsage =
  "usage: hbs2-hub redact --repo <key> --event <event-id> [--as <key>] [--dry-run]"

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
    $ args [ arg "string" "--repo repo-key", arg "string" "--event event-id"
           , arg "string" "[--dry-run]" ]
    $ desc ( "DISPLAY-LEVEL, and PEP-19 says so: the event stays in canon and"
             <> line <> "every clone still holds the bytes. What changes is that"
             <> line <> "readers stop showing the body. It is the answer to a"
             <> line <> "mistake or to abuse, not to a secret: a secret that"
             <> line <> "reached canon has been published."
             <> line
             <> line <> "The event-id, not a number: a redact names one event, and"
             <> line <> "the thing worth hiding is as often a comment as an open."
             <> dryRunHelp )
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
               , arg "string" "[--as canon-key]", arg "string" "[--dry-run]" ]
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
                 <> line <> "one, and it is the only thing a reader shows as a label."
                 <> dryRunHelp )
        $ entry $ bindMatch name $ nil_ \case
            -- BOTH AT ONCE IS REFUSED, and it used not to be: this admitted the
            -- pair and 'labelIt' then resolved it in favour of the clear, so
            -- `issue label --label bug --clear` published an owner-signed event
            -- that REMOVED every label when the operator had asked to add one,
            -- into canon that cannot take it back. The sibling verb refuses the
            -- same shape two functions down, and the paragraph above says the
            -- whole reason --clear is spelled out is not to publish a mistake.
            (ownArgsFor ["--label"] ["--clear"] -> Just ow)
              | owClear ow, List.null (owLabels ow) -> lift (labelIt ow)
              | not (owClear ow), not (List.null (owLabels ow)) -> lift (labelIt ow)
            _ -> liftIO (die (show ownUsage))

    assignVerb name =
      brief "say who is looking at a thread that is in canon"
        $ args [ arg "string" "--repo repo-key", arg "string" "--number n"
               , arg "string" "--assignee key | --clear"
               , arg "string" "[--as canon-key]", arg "string" "[--dry-run]" ]
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
                 <> line <> "an argument would publish that into append-only canon."
                 <> dryRunHelp )
        $ entry $ bindMatch name $ nil_ \case
            (ownArgsFor assigneeFlags ["--clear"] -> Just ow)
              | owClear ow, Nothing <- owTo ow -> lift (assignIt ow)
              | Just _ <- owTo ow, not (owClear ow) -> lift (assignIt ow)
            _ -> liftIO (die (show ownUsage))

    statusVerb name lead what mk =
      brief what
        $ args [ arg "string" "--repo repo-key", arg "string" "--number n"
               , arg "string" "[--note text]", arg "string" "[--as canon-key]"
               , arg "string" "[--dry-run]" ]
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
                 <> line <> "the repository key."
                 <> dryRunHelp )
        $ entry $ bindMatch name $ nil_ \case
            (ownArgsFor ["--note"] [] -> Just ow) -> lift (statusIt mk ow)
            _ -> liftIO (die (show ownUsage))

    canonOf repo =
      -- Refuse: every verb here names a thread by number, and a repository with
      -- no canon has none. Its sibling in Maintainer treats the absence as
      -- empty, because a delegation CAN be the first event canon holds.
      withCanon Refuse repo withGitCanon

    -- The number to the thread it names, through 'oneNumbered'. A number
    -- nobody minted is a refusal and not an event: minting against a thread
    -- canon does not hold is exactly what the fold drops as BadThread, with the
    -- seq already spent. A number canon gives TWICE is a refusal for a sharper
    -- reason, which is on 'oneNumbered'.
    threadOfNumber fr n = oneNumbered n fr

    statusIt mk ow = do
      (parent, fr) <- canonOf (owRepo ow)
      thr <- threadOfNumber fr (owNumber ow)
      writeOwn (owRepo ow) (owAs ow) (owDry ow) parent fr
        (\now -> mk thr ow now) "hub: status"

    labelIt ow = do
      (parent, fr) <- canonOf (owRepo ow)
      thr <- threadOfNumber fr (owNumber ow)
      -- Through 'encodeLabels', which is what makes the value canonical: the
      -- bridge refuses an unnormalized one, and two maintainers applying the
      -- same labels in a different order must produce the same event.
      let value = encodeLabels (if owClear ow then [] else owLabels ow)
      writeOwn (owRepo ow) (owAs ow) (owDry ow) parent fr
        (ASet thr attrLabels value) "hub: labels"

    assignIt ow = do
      (parent, fr) <- canonOf (owRepo ow)
      thr <- threadOfNumber fr (owNumber ow)
      -- PLURAL, and through 'encodeLabels' like every other set-valued
      -- attribute. PEP-19 states the rule and states why: "an attribute that
      -- can hold a set is spelled as one everywhere, so nothing has to remember
      -- which spelling normalizes". This verb wrote the singular, so the name it
      -- wrote was in no reader's vocabulary but its own: 'multiValued' lists the
      -- plural, so 'normalizeAttr' left the value alone and `hub verify` raised
      -- no UnnormalizedAttr; and the PEP-22 render contract reads the plural, so
      -- every thread this verb ever assigned came out of `--json` with
      -- "assignees": [] while the terminal showed an assignee. Canon is
      -- append-only, which is why this had to move before anybody published one.
      --
      -- One key still, and the empty set is a clear: last-writer-wins has no way
      -- to remove an attribute, so the empty value IS the absence a reader shows
      -- as none. The shape leaves room for --to to become repeatable, which is
      -- additive; the singular name would not have.
      let value = encodeLabels
                    (maybe [] (pure . Text.pack . show . pretty . AsBase58) (owTo ow))
      writeOwn (owRepo ow) (owAs ow) (owDry ow) parent fr
        (ASet thr attrAssignees value) "hub: assignees"

    redactIt rd = do
      (parent, fr) <- canonOf (rdRepo rd)
      writeOwn (rdRepo rd) (rdAs rd) (rdDry rd) parent fr
        (ARedact (rdRepo rd) (rdEvent rd)) "hub: redact"

    -- The shared tail: sign, mint, plan, commit. The same order 'hub inbox
    -- accept' uses, and for the same reason -- minting writes nothing, so a
    -- refusal anywhere above the commit leaves canon untouched.
    writeOwn repo mas dry parent fr mk message = do
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

      commit <- committing (oneStop codeCanonUnwritable) dry parent
                  (frMeta fr) [(eventPath acc, acEvent acc)] (numberIndexOf fr) message now

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
ownArgs = ownArgsFor (["--note","--label"] <> assigneeFlags) ["--clear"]

-- | The same, told which optional flags THIS verb has.
--
-- WHY THE UNION WAS WRONG. One reader served four verbs and its known set was
-- everything any of them takes, so a flag the verb in hand does not use was not
-- refused -- it was dropped. `issue close --to <key>` was accepted and the
-- assignment discarded; so was `issue close --label bug`. "HBS2.Hub.CLI.Argv"
-- states the rule this broke: nothing here can tell an operator's mistake from
-- an intention, so the only safe answer to a word nobody claimed is to stop.
--
-- Which matters most for the pair that used to be resolved silently in favour
-- of the destructive one: see 'labelVerb'.
ownArgsFor :: forall c . IsContext c
           => [String] -> [String] -> [Syntax c] -> Maybe OwnArgs
ownArgsFor extra switches syn = do
  kvs   <- flagsAndSwitches (repoFlags <> ["--number","--as"] <> extra)
                            (switches <> ["--dry-run"]) syn
  repo  <- flagRepo asKey kvs
  n     <- flagOnce kvs "--number" >>= flagWord
  note  <- flagMaybe kvs "--note" (fmap Text.pack . flagText)
  as    <- flagMaybe kvs "--as" asKey
  ls    <- traverse (fmap Text.pack . flagText) (flagEvery kvs "--label")
  -- A switch, so it takes no value and cannot swallow the next word.
  clear <- flagSwitch kvs "--clear"
  -- Through 'assigneeFlags': --to named a PERSON while --key named three
  -- different kinds of key elsewhere, and all four are thirty-two bytes of
  -- base58. --assignee says which of the four this is.
  to    <- flagOneOfMaybe asKey assigneeFlags kvs
  dry   <- flagSwitch kvs "--dry-run"
  pure (OwnArgs repo n note ls clear to as (writingOf dry))
  where
    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }

redactArgs :: forall c . IsContext c => [Syntax c] -> Maybe RedactArgs
redactArgs syn = do
  kvs  <- flagsAndSwitches (repoFlags <> ["--event","--as"]) ["--dry-run"] syn
  repo <- flagRepo asKey kvs
  e    <- flagOnce kvs "--event" >>= asHash
  as   <- flagMaybe kvs "--as" asKey
  dry  <- flagSwitch kvs "--dry-run"
  pure (RedactArgs repo e as (writingOf dry))
  where
    asKey  = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    asHash = \case { HashLike h -> Just h ; _ -> Nothing }
