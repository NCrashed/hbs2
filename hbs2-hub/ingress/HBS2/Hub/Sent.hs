-- | What this node sent, as a store (PEP-18 "Acknowledgement letter").
--
-- A hub-side deny-list has an obvious home and this did not, which is why it
-- did not exist: a contributor's letters go into somebody else's mailbox and
-- leave nothing behind on their own machine. The compose verbs printed the
-- message hash and the thread-id and forgot them, so the node had no memory of
-- its own submissions, and 'openAckFor' -- the check PEP-18 requires of anyone
-- reading an acknowledgement -- had no set to ask.
--
-- WHAT IT IS FOR, in order of how much it matters:
--
--   * The correlation check. An ack ties a thread to a status, and a maintainer
--     of a repository can sign a valid ack about a thread in it that this node
--     never touched. Without a record of what was sent, a reader either shows
--     it (and calls somebody else's business your own) or shows nothing.
--
--   * Being able to see what you sent at all. A thread-id is not memorable and
--     a message hash is less so, and both scroll past in a terminal.
--
-- LOCAL, UNSIGNED AND NOT CANON. Nothing here is published, nothing depends on
-- it being true, and losing the file costs the check above and nothing else --
-- which is the failure mode to want: an ack about a thread this node cannot
-- find in the log is shown as unrelated rather than believed.
--
-- OUTSIDE THE WORKING TREE for the reason the deny-list is: a file under the
-- repository looks like something that travels, and eventually goes out with
-- somebody's @git add -A@. This one names a contributor's threads and their
-- own author key.
module HBS2.Hub.Sent
  ( Sent(..)
  , sentPath
  , renderSent
  , parseSent
  , loadSent
  , recordSent
  , submittedBy
  , codeNoSentLog
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter (sexpStr)

import HBS2.CLI.Prelude

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef,pattern HashLike)

import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Word (Word64)
import System.Directory (createDirectoryIfMissing,doesFileExist,getXdgDirectory
                        ,XdgDirectory(..))
import System.FilePath ((</>),takeDirectory)

-- | The log is here and will not read.
--
-- Its own code, and NOT shared with the deny-list's: they fail for the same
-- reasons and mean opposite things. A damaged deny-list must stop an accept,
-- because carrying on folds exactly what somebody took the trouble to ban. A
-- damaged sent log stops nothing -- it is a display and a filter -- so a reader
-- can be told to look at it and carry on.
codeNoSentLog :: Int
codeNoSentLog = 38

-- | One letter this node put in somebody's mailbox.
data Sent = Sent
  { seThread  :: ThreadId        -- ^ the thread it opens or replies in
  , seEvent   :: EventId         -- ^ this letter's own event-id
  , seMessage :: HashRef         -- ^ the Mailbox message, which is how it is named
    -- | The repository, when the letter said which.
    --
    -- A comment does not: PEP-18's @AComment@ names a thread and the thread
    -- names the repository, so a reply carries no target and this node cannot
    -- invent one. The correlation check below is written to accept that.
  , seRepo    :: Maybe RepoRef
  , seAuthor  :: HubKey          -- ^ the key that signed it
    -- | This node's own clock, in milliseconds. Advisory and local: nothing
    -- reads it but a person, and it is not the timestamp inside the letter.
  , seAt      :: Word64
  , seWhat    :: Text            -- ^ the verb, for a person reading the log
  , seTitle   :: Maybe Text      -- ^ the title, when the letter had one
  }
  deriving stock (Eq,Show)

-- | Where this node keeps it.
--
-- ONE FILE and not one per repository, unlike the deny-list, because half the
-- records have no repository to key on: a comment names a thread only. Keying
-- what can be keyed and dropping the rest would leave exactly the replies out
-- of the check that wants them.
sentPath :: MonadIO m => m FilePath
sentPath = do
  dir <- liftIO (getXdgDirectory XdgData "hbs2-hub")
  pure (dir </> "sent")

-- | One record as one line.
--
-- Through 'sexpStr' for every text, which is the escaper canon uses: a title is
-- whatever somebody typed, and this file is read with @cat@ more often than
-- with a parser. A raw escape sequence in it rewrites what the reader sees.
-- ONE LINE, assembled rather than pretty-printed. A rendered form wraps at
-- whatever width the printer likes, and this file is appended to by processes
-- that may be running at the same time: one line is one write, and a wrapped
-- record is several. It is also what makes the file greppable, which is most of
-- what a log is for.
renderSent :: Sent -> Text
renderSent s = "(sent " <> Text.intercalate " " (fixed <> optional) <> ")"
  where
    fixed =
      [ form "thread"  (href (seThread s))
      , form "event"   (href (seEvent s))
      , form "message" (href (seMessage s))
      , form "author"  (key (seAuthor s))
      , form "at"      (Text.pack (show (seAt s)))
      , form "what"    (str (seWhat s))
      ]

    optional =
         [ form "repo" (key r) | Just r <- [seRepo s] ]
      <> [ form "title" (str t) | Just t <- [seTitle s] ]

    form k v = "(" <> k <> " " <> v <> ")"
    href = Text.pack . show . pretty
    key = Text.pack . show . pretty . AsBase58
    -- Through 'sexpStr', which is the escaper canon uses, and then rendered:
    -- it answers with a Syntax whose printed form is the quoted literal, which
    -- holds no newline whatever the text did.
    str = Text.pack . show . pretty . sexpStr

-- | And back.
--
-- A malformed RECORD is an error, like a malformed ban clause: a log that
-- quietly drops what it cannot read is a log that shortens itself, and this one
-- decides whether an acknowledgement is shown as yours.
--
-- An unknown CLAUSE inside a well-formed record is ignored, which is the
-- opposite decision and the right one for the opposite reason: this file is
-- written by one version of this tool and read by the next, and a field added
-- later must not make every record before it unreadable.
parseSent :: Text -> Either Text [Sent]
parseSent txt = do
  syn <- either (const (Left "the file is not a list of clauses")) Right
           (parseTop (Text.unpack txt))
  traverse one syn
  where
    one s@(ListVal (SymbolVal "sent" : clauses)) = do
      let clause k = List.lookup k [ (n, vs) | ListVal (SymbolVal (Id n) : vs) <- clauses ]

          -- Signed, because it is used at seven different result types and
          -- without one the first use fixes it for the rest.
          bad :: Text -> Either Text a
          bad what = Left (what <> ": " <> safeText (Text.pack (show (pretty s))))

      thread  <- maybe (bad "no thread") Right (clause "thread" >>= hashOf)
      event   <- maybe (bad "no event") Right (clause "event" >>= hashOf)
      message <- maybe (bad "no message") Right (clause "message" >>= hashOf)
      author  <- maybe (bad "no author") Right (clause "author" >>= keyOf)
      at      <- maybe (bad "no timestamp") Right (clause "at" >>= intOf)
      what    <- maybe (bad "no verb") Right (clause "what" >>= textOf)

      -- Absent and unreadable are told apart: a repo clause that is there and
      -- is not a key is a damaged record, not a comment.
      repo <- case clause "repo" of
                Nothing -> Right Nothing
                Just vs -> maybe (bad "the repo is not a key") (Right . Just) (keyOf vs)
      title <- case clause "title" of
                 Nothing -> Right Nothing
                 Just vs -> maybe (bad "the title is not text") (Right . Just) (textOf vs)

      pure (Sent thread event message repo author at what title)

    one other = Left ("not a sent record: "
                        <> safeText (Text.pack (show (pretty other))))

    hashOf = \case { [HashLike h] -> Just h ; _ -> Nothing }
    keyOf  = \case { [SignPubKeyLike k] -> Just k ; _ -> Nothing }
    textOf = \case { [StringLike t] -> Just (Text.pack t) ; _ -> Nothing }
    intOf  = \case { [LitIntVal n] | n >= 0 -> Just (fromIntegral n) ; _ -> Nothing }

-- | Everything this node remembers sending, or why it could not be read.
--
-- A missing file is an empty log, which is what "this node has sent nothing"
-- looks like and is the state every new install is in.
loadSent :: MonadIO m => m (Either Text [Sent])
loadSent = do
  p <- sentPath
  here <- liftIO (doesFileExist p)
  if not here then pure (Right [])
    else liftIO (Text.readFile p) <&> parseSent

-- | Remember one.
--
-- Appends, and appends a whole line at once, because two hbs2-hub processes
-- sending letters at the same time is an ordinary thing for a script to do and
-- a read-modify-write would lose one of them. Nothing here dedups: sending the
-- same letter twice is two letters, and the log says so.
recordSent :: MonadIO m => Sent -> m ()
recordSent s = do
  p <- sentPath
  liftIO do
    createDirectoryIfMissing True (takeDirectory p)
    Text.appendFile p (renderSent s <> "\n")

-- | The check PEP-18 asks a reader of an acknowledgement to make.
--
-- Takes the log rather than reading it, so the decision is a function and the
-- one place that reads a file is not also the one that decides.
--
-- A record with NO repository (a comment) matches on the thread alone, and that
-- is deliberate rather than lax. The alternative readings are both worse: drop
-- those records and every ack about a thread you replied in reads as somebody
-- else's, or infer a repository the letter never named. What the thread-id
-- proves is weaker than a signature and stronger than nothing -- it is the hash
-- of an author box this node saw -- and the whole predicate is a display filter
-- over messages an unrelated party can send but cannot forge.
submittedBy :: [Sent] -> RepoRef -> ThreadId -> Bool
submittedBy log' repo thread =
  not (HS.null (HS.fromList [ () | s <- log', seThread s == thread, matches s ]))
  where
    matches s = maybe True (== repo) (seRepo s)
