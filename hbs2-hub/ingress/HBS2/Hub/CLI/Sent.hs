-- | @hub sent@: what this node has sent (PEP-22 "Contribute").
--
-- WHY THIS EXISTS. "HBS2.Hub.Sent" writes the log and spends its header on why
-- seeing it matters -- a letter is fire-and-forget, the answer comes back to a
-- different mailbox at a different time, and the message hash is how the two
-- are correlated. Its only reader was @hub updates@, as a filter: the log was
-- kept so that an acknowledgement could be matched against it, and there was no
-- way to look at the thing being matched.
--
-- So a contributor whose letter had not been answered could not tell whether
-- they had sent it. `hub issue new` prints two hashes once, to a terminal that
-- is now scrolled away.
--
-- READ-ONLY AND PEERLESS, like the canon readers: the log is a file this node
-- wrote about its own actions. It says nothing about whether anything arrived,
-- and the report says so rather than letting a row be read as delivery.
module HBS2.Hub.CLI.Sent
  ( sentEntries
  , sentUsage
    -- * The parts that decide something
  , sentDoc
  , sentNote
  , SentArgs(..)
  , sentArgs
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Sent (Sent(..),loadSent,codeNoSentLog)
import HBS2.Hub.CLI.Argv (badArgs,flagsAndSwitches,flagSwitch,repoFlags,flagRepoMaybe)
import HBS2.Hub.CLI.Common (refuse,saying,utcOf)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))

import Data.List qualified as List

-- | What one @hub sent@ was asked to show.
data SentArgs = SentArgs
  { snRepo :: Maybe RepoRef
    -- | Whole identifiers instead of the front of them.
    --
    -- The same switch `hub inbox` has, and for the same reason: a row is read
    -- by a person and piped by a script, and those want different widths.
  , snLong :: Bool
  }
  deriving stock (Eq,Show)

sentUsage :: Doc ()
sentUsage = "usage: hbs2-hub sent [--repo <key>] [--long]"

-- | One line per letter, newest last.
--
-- NEWEST LAST, which is the order the log is written in and the order a
-- terminal reads: the last line is the one still on the screen. `hub updates`
-- sorts the other way because an acknowledgement is news; this is a record.
--
-- Pure and exported, like every other renderer in this package: what a report
-- says is the whole of what the verb does, and a Doc built inside an IO action
-- that ends in exitWith cannot be asserted on.
sentDoc :: Bool -> [Sent] -> [Doc ann]
sentDoc long xs =
  [ hsep [ pretty (utcOf (seAt s))
         , short (hashDoc (seMessage s))
         , fill 8 (pretty (safeText (seWhat s)))
         , short (hashDoc (seThread s))
         , maybe "-" (short . keyDoc) (seRepo s)
         , maybe mempty (dquotes . title) (seTitle s)
         ]
  | s <- xs ]
  where
    short = if long then id else briefly 8
    -- A stranger's bytes only in the sense that this node wrote them down from
    -- its own argv, and bounded for the reason the queue's are: it is the one
    -- field of unbounded width on the row.
    title t = (if long then id else briefly 60) (pretty (safeText t))

-- | What is true ABOUT the list, for stderr.
--
-- A LOG IS NOT A DELIVERY RECEIPT, and the word this verb prints has to keep
-- saying so: an entry is written when the peer took the message off this
-- node's hands, which is not that any mailbox accepted it and not that it was
-- read. `hub updates` is the other end.
sentNote :: Int -> Doc ann
sentNote 0 = "nothing has been sent from this node"
sentNote n =
  pretty n <+> "letter(s) this node handed to its peer."
    <> line <> "  That is what the log records: not that a mailbox took them,"
    <+> "and not that anybody read them."
    <> line <> "  `hbs2-hub updates --repo <key> --mailbox <your-mailbox>`"
    <+> "is what came back."

sentEntries :: forall c m . ( IsContext c
                            , MonadUnliftIO m
                            , Exception (BadFormException c)
                            ) => MakeDictM c m ()
sentEntries = do

  brief "list the letters this node has sent"
    $ args [ arg "string" "[--repo repo-key]", arg "string" "[--long]" ]
    $ desc ( "Read-only and peerless: the log is a file this node wrote"
             <> line <> "about its own actions."
             <> line
             <> line <> "One line per letter: when, the message hash, the verb,"
             <> line <> "the thread it opens or replies in, the repository when"
             <> line <> "the letter named one, and the title when it had one."
             <> line
             <> line <> "A comment names no repository -- PEP-18 gives it a"
             <> line <> "thread, and the thread names the repository -- so"
             <> line <> "--repo cannot filter one in without inventing the"
             <> line <> "binding. It filters the rows that DO name one, and the"
             <> line <> "rest are shown either way rather than hidden."
             <> line
             <> line <> "AN ENTRY IS NOT A DELIVERY. It is written when the peer"
             <> line <> "took the message off this node's hands, which is not"
             <> line <> "that any mailbox accepted it. `hub updates` is what"
             <> line <> "came back." )
    $ entry $ bindMatch "hub:sent" $ nil_ \case
        (sentArgs -> Just sa) -> lift (listSent sa)
        other -> liftIO (badArgs sentUsage other)

  where

    listSent sa = do
      xs <- loadSent
              >>= either (\e -> liftIO (refuse (show (pretty e)) codeNoSentLog)) pure

      -- A row with no repository is KEPT under --repo, and that is the whole
      -- of what the flag can honestly do: a comment carries a thread and not a
      -- target, so dropping the rows that name nothing would hide exactly the
      -- letters somebody filtering by repository is looking for.
      let mine = case snRepo sa of
                   Nothing -> xs
                   Just k  -> [ s | s <- xs, maybe True (== k) (seRepo s) ]

      liftIO (mapM_ print (sentDoc (snLong sa) mine))
      liftIO (saying (sentNote (length mine) <> line))

-- | @[--repo <key>] [--long]@.
sentArgs :: forall c . IsContext c => [Syntax c] -> Maybe SentArgs
sentArgs syn = do
  kvs  <- flagsAndSwitches repoFlags ["--long"] syn
  repo <- flagRepoMaybe asKey kvs
  long <- flagSwitch kvs "--long"
  pure (SentArgs repo long)
  where
    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
