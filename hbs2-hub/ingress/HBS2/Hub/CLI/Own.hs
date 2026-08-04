-- | @hub issue close|reopen|label@ and @hub redact@ (PEP-19, PEP-22 "Maintain").
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
import HBS2.Hub.Repo.GitWrite (withGitSink)
import HBS2.Hub.CLI.Argv ( flagsOf,flagsAndSwitches,flagOnce,flagEvery,flagMaybe
                         , flagSwitch,flagText,flagWord )
import HBS2.Hub.CLI.Inbox (refuse)
import HBS2.Hub.CLI.Read (codeNoSuchThread)
import HBS2.Hub.CLI.Accept (codeNoCanonKey,codeTriageRefused,codeCanonUnwritable)
import HBS2.Hub.CLI.Verify (codeOf)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (pattern HashLike)
import HBS2.Net.Auth.Credentials (_peerSignSk)
import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,loadCredentials)

import Data.List qualified as List
import Data.Maybe (fromMaybe,listToMaybe)
import Data.Text qualified as Text
import Data.Word (Word64)
import System.Exit (die)

-- | What a close, reopen or label was asked to do.
data OwnArgs = OwnArgs
  { owRepo   :: RepoRef
  , owNumber :: Word64
  , owNote   :: Maybe Text     -- ^ close/reopen only
  , owLabels :: [Text]         -- ^ label only
  , owClear  :: Bool           -- ^ label only: the empty set, said out loud
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
ownUsage =
  "usage: hbs2-hub issue close|reopen --repo <key> --number <n> [--note <text>] [--as <key>]"
    <> line <> "       hbs2-hub issue label --repo <key> --number <n> --label <l>... [--as <key>]"
    <> line <> "       hbs2-hub issue label --repo <key> --number <n> --clear"

redactUsage :: Doc ()
redactUsage =
  "usage: hbs2-hub redact --repo <key> --event <event-id> [--as <key>]"

ownEntries :: forall c m . ( IsContext c
                           , MonadUnliftIO m
                           , Exception (BadFormException c)
                           ) => MakeDictM c m ()
ownEntries = do

  statusVerb "hub:issue:close" "close a thread that is in canon"
    (\thr ow now -> AClose thr (owNote ow) now)

  statusVerb "hub:issue:reopen" "reopen a thread that is in canon"
    (\thr ow now -> AReopen thr (owNote ow) now)

  brief "set the labels of a thread that is in canon"
    $ args [ arg "string" "--repo repo-key", arg "string" "--number n"
           , arg "string" "--label label" ]
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
    $ entry $ bindMatch "hub:issue:label" $ nil_ \case
        (ownArgs -> Just ow)
          | owClear ow || not (List.null (owLabels ow)) -> lift (labelIt ow)
        _ -> liftIO (die (show ownUsage))

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

    statusVerb name what mk =
      brief what
        $ args [ arg "string" "--repo repo-key", arg "string" "--number n"
               , arg "string" "--note text" ]
        $ desc ( "Writes an owner-signed event onto canon. The status follows"
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
      withGitCanon (\cs -> readCanon cs repo) >>= \case
        Right st -> pure (Just (stCommit st), stFold st)
        Left e -> liftIO (refuse (show (pretty e)) (codeOf e))

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

    redactIt rd = do
      (parent, fr) <- canonOf (rdRepo rd)
      writeOwn (rdRepo rd) (rdAs rd) parent fr
        (ARedact (rdRepo rd) (rdEvent rd)) "hub: redact"

    -- The shared tail: sign, mint, plan, commit. The same order 'hub inbox
    -- accept' uses, and for the same reason -- minting writes nothing, so a
    -- refusal anywhere above the commit leaves canon untouched.
    writeOwn repo mas parent fr mk message = do
      let signer = fromMaybe repo mas

      creds <- runKeymanClientRO (loadCredentials signer)
                 >>= maybe (liftIO (refuse (show ( "no signing key here for"
                                                     <+> pretty (AsBase58 signer) ))
                                           codeNoCanonKey))
                           pure

      now <- liftIO getPOSIXTime <&> floor . (* 1000)

      let ctx = TriageCtx (signer, _peerSignSk creds) (const True) repo

      acc <- either (\e -> liftIO (refuse (show ("refused:" <+> pretty e))
                                          codeTriageRefused))
                    pure
               (ownerEvent ctx (viewOf fr) now noOwnAttachments (mk now))

      plan <- either (\e -> liftIO (refuse (show (pretty e)) codeCanonUnwritable)) pure
                (planCanon [(eventPath acc, acEvent acc)] (numberIndexOf fr))

      commit <- withGitSink (\sk -> skCommit sk (CanonWrite parent (cwFiles plan)
                                                   message now))
                  >>= either (\e -> liftIO (refuse (show (pretty e)) codeCanonUnwritable))
                             pure

      liftIO $ print $ vcat
        [ "event" <+> pretty (eventId (acEvent acc))
        , "seq" <+> pretty (acSeq acc)
        , "commit" <+> pretty commit
        ]

-- | @--repo K --number N [--note T] [--label L]... [--clear] [--as K]@.
--
-- One reader for the three thread verbs, because they differ in which optional
-- values they use and not in how a value is spelled. Every one of them is
-- behind a flag for the reason the whole package's are: a repo key and a
-- delegate key are the same thirty-two bytes of base58.
ownArgs :: forall c . IsContext c => [Syntax c] -> Maybe OwnArgs
ownArgs syn = do
  kvs   <- flagsAndSwitches ["--repo","--number","--note","--label","--as"]
                            ["--clear"] syn
  repo  <- flagOnce kvs "--repo" >>= asKey
  n     <- flagOnce kvs "--number" >>= flagWord
  note  <- flagMaybe kvs "--note" (fmap Text.pack . flagText)
  as    <- flagMaybe kvs "--as" asKey
  ls    <- traverse (fmap Text.pack . flagText) (flagEvery kvs "--label")
  -- A switch, so it takes no value and cannot swallow the next word.
  clear <- flagSwitch kvs "--clear"
  pure (OwnArgs repo n note ls clear as)
  where
    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }

redactArgs :: forall c . IsContext c => [Syntax c] -> Maybe RedactArgs
redactArgs syn = do
  kvs  <- flagsOf ["--repo","--event","--as"] syn
  repo <- flagOnce kvs "--repo" >>= asKey
  e    <- flagOnce kvs "--event" >>= asHash
  as   <- flagMaybe kvs "--as" asKey
  pure (RedactArgs repo e as)
  where
    asKey  = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    asHash = \case { HashLike h -> Just h ; _ -> Nothing }
