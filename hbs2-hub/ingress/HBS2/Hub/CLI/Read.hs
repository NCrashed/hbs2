-- | @hub issue list@, @hub issue show@, @hub pr list@, @hub pr show@ and
-- @hub log@ (PEP-22 "Read").
--
-- The read side of the tracker. Everything here is a function of one
-- 'FoldResult', so none of it needs a peer, a key or a network: canon is a git
-- ref in this repository and the fold over it is what these print.
--
-- Everything that DECIDES something is exported and pure, the way
-- "HBS2.Hub.CLI.Verify" exports 'reportDoc' and "HBS2.Hub.CLI.Inbox" exports
-- 'render'. Both of those modules had a bug in exactly the part that lived
-- inside a @where@ clause and could not be asked about.
--
-- A TITLE, A LABEL AND A BODY ARE A STRANGER'S BYTES. They arrive in a letter
-- anybody may send, and they are printed to a terminal, so every one of them
-- goes through 'safeText' for the reason the rest of this package does: a
-- title holding an erase-line sequence rewrites the line above it, and a title
-- holding a newline forges a second row in the listing.
--
-- ORDER IS IMPOSED, never taken from a container. 'frThreads' is a HashMap and
-- iterates in hash order, which is stable for one build and is not a promise;
-- a tracker whose issues reorder themselves on a dependency bump is one nobody
-- can work through a page at a time. Threads sort by number (PEP-22), events
-- by @seq@.
--
-- WHAT IS NOT HERE: the query DSL PEP-22 inherits from fixme-new. It compiles
-- to SQL over the materialized cache, and there is no cache in this build (the
-- fold is in memory). Two flags stand in for it, and the help says which one
-- this is.
module HBS2.Hub.CLI.Read
  ( readEntries
    -- * The parts that decide something
  , Filter(..)
  , noFilter
  , threadsOf
  , listDoc
  , showDoc
  , logDoc
  , statusOf
  , listArgs
  , labelsOf
  , assigneeOf
  , codeNoSuchThread
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.CLI.Argv (flagsOf,flagMaybe,flagText)
import HBS2.Hub.CLI.Verify (codeOf, refusalDoc)

import HBS2.CLI.Prelude hiding (null)
import HBS2.CLI.Run.Internal

import HBS2.Data.Types.Refs (pattern HashLike)

import Data.HashMap.Strict qualified as HM
import Data.List (sortOn)
import Data.Maybe (fromMaybe,isJust)
import System.IO.Error (isResourceVanishedError)
import Data.Text qualified as Text
import Data.Word (Word64)
import System.Exit (die,exitWith,ExitCode(..))

-- | Asked for a thread canon does not hold.
--
-- Its own code rather than 1: a number that is not there is not a usage error,
-- and a script that polls an issue until it appears has to tell the two apart.
codeNoSuchThread :: Int
codeNoSuchThread = 26

-- | What a listing was narrowed to.
--
-- Two fields and not a query language. Both are exact matches on the
-- attributes the fold maintains, which is all that can be done honestly
-- without the cache PEP-22's DSL compiles against.
data Filter = Filter
  { fStatus :: Maybe Text
  , fLabel  :: Maybe Text
  }
  deriving stock (Eq,Show)

noFilter :: Filter
noFilter = Filter Nothing Nothing

-- | A thread's status: @open@, @closed@ or @merged@.
--
-- An LWW attribute like the title, seeded @open@ at the open event, so the
-- default is a fact about canon rather than a guess made here.
statusOf :: ThreadState -> Text
statusOf = fromMaybe "open" . HM.lookup "status" . tsAttrs

-- | The same, on its way to a terminal.
--
-- A status is an ATTRIBUTE VALUE, which is to say a stranger's bytes: it is
-- whatever a @set@ event carried, and @requireNormalized@ asks that the value
-- be in canonical form, not that it be a word. Four kilobytes of it are allowed
-- ('maxAttrValue'), the fold raises no anomaly, and this printed it with a bare
-- 'pretty' on the same line as a title that goes through 'safeText' -- so a
-- repository whose canon says the status is @\\ESC[2K\\ESC[1Aopen@ rewrote the
-- rows above it in every reader's terminal, once per thread. Reading a
-- stranger's canon is the ordinary case for this verb: it is what a clone does.
--
-- Its own function so that both callers cannot drift, which is how the title
-- and the status came to be printed by different rules in the first place.
statusDoc :: ThreadState -> Doc ann
statusDoc = pretty . safeText . statusOf

-- | The labels an owner actually applied.
--
-- NOT 'tsLabelsRequested', which is what the author asked for: PEP-19 makes
-- applying a label an owner-signed @set@, and printing the two as one would
-- let a stranger label their own issue.
labelsOf :: ThreadState -> [Text]
labelsOf = maybe [] decodeLabels . HM.lookup "labels" . tsAttrs

-- | Who a thread is assigned to, when an owner has said so.
--
-- An ordinary LWW attribute like the status, and read like one. It holds a key
-- in base58 because a person here is a key: a name would be a second identity
-- with nothing behind it, and the fold has no notion of one.
--
-- Kept as TEXT rather than parsed back into a key, and that is the same
-- decision 'statusDoc' records: an attribute value is a stranger's bytes,
-- whatever a set event carried, and a reader that insisted on parsing it would
-- show nothing at all for a thread whose canon says something else.
assigneeOf :: ThreadState -> Maybe Text
assigneeOf = HM.lookup "assignee" . tsAttrs

-- | The threads of one kind, filtered, in the order PEP-22 prints them.
--
-- Sorted by number, and by thread id where a thread has none. A thread has no
-- number only if canon holds one minted without a stamp, which the fold
-- refuses, so this is a total order rather than a tie-break in practice; it is
-- written down because the alternative to imposing one is printing hash order.
threadsOf :: HubKind -> Filter -> FoldResult -> [ThreadState]
threadsOf kind f fr =
  sortOn key [ t | t <- HM.elems (frThreads fr)
             , tsKind t == kind
             , maybe True (== statusOf t) (fStatus f)
             , maybe True (`elem` labelsOf t) (fLabel f)
             ]
  where key t = (tsNumber t, show (pretty (tsId t)))

-- | One line per thread.
--
-- The number, the status, the labels and the title. Not the author: a base58
-- key is forty-odd columns and would push every title off the line, and
-- @issue show@ prints it.
listDoc :: [ThreadState] -> [Doc ann]
listDoc ts =
  [ hsep [ num (tsNumber t)
         , fill 6 (statusDoc t)
         , title t
         , labels t ]
  | t <- ts ]
  where
    num = fill 5 . maybe "-" (("#" <>) . pretty)

    -- A redacted thread keeps its row: it is in canon and a reader who cannot
    -- see it in the list would conclude the numbering has a hole.
    title t | tsRedacted t = "(redacted)"
            | otherwise    = pretty (safeText (tsTitle t))

    labels t = case labelsOf t of
      [] -> mempty
      ls -> brackets (hsep (punctuate comma (fmap (pretty . safeText) ls)))

-- | One thread, with its comments.
showDoc :: ThreadState -> [Doc ann]
showDoc t =
  [ hsep [ maybe "(no number)" (("#" <>) . pretty) (tsNumber t)
         , statusDoc t
         , if tsRedacted t then "(redacted)" else pretty (safeText (tsTitle t)) ]
  , "thread" <+> hashDoc (tsId t)
  , "kind" <+> viaShow (tsKind t)
  , "author" <+> keyDoc (tsAuthor t)
  , "blessed-by" <+> keyDoc (tsCanonBy t)
  , "created" <+> pretty (tsCreated t) <+> "updated" <+> pretty (tsUpdated t)
  ]
  -- Only when there is one: an assignee is cleared by setting the attribute
  -- to the empty string (last-writer-wins has no way to remove one), and a
  -- line reading "assignee" with nothing after it says the opposite of what
  -- canon holds.
  <> [ "assignee" <+> pretty (safeText a)
     | Just a <- [assigneeOf t], not (Text.null a) ]
  <> [ "labels" <+> hsep (punctuate comma (fmap (pretty . safeText) ls))
     | ls <- [labelsOf t], not (null ls) ]
  -- Said only when there is something to say, and said as a REQUEST: the
  -- author asked, nobody applied it.
  <> [ "labels-requested" <+> hsep (punctuate comma (fmap (pretty . safeText) ls))
     | ls <- [tsLabelsRequested t], not (null ls) ]
  -- THROUGH hashDoc, every one of them, and that is a rule about the renderer
  -- and not about these four fields. A HashRef is a newtype over a ByteString
  -- with a derived Serialise instance, so it takes any width off the wire;
  -- 'validHashRef' exists for that, the fold does not re-run it (Fold.hs calls
  -- reply-to "carried through unvalidated"), and base58 is Integer base
  -- conversion, i.e. quadratic. A 48 KiB reply-to inside 'maxBoxBytes' is
  -- admitted canon and half a second of CPU per line. 'hashDoc' says what is
  -- true of it instead, and 'HBS2.Hub.CLI.Inbox' next door already prints its
  -- hashes this way.
  <> [ "origin" <+> hashDoc o | Just o <- [tsOrigin t] ]
  -- The body's hash and the secret's presence, not the secret: a reader needs
  -- to know the body is fetchable and this is a terminal.
  <> [ "body-part" <+> hashDoc h
         <+> (if isJust (tsPartSecret t) then "(secret published)" else "(no secret)")
     | Just h <- [tsBodyPart t] ]
  <> [ coords p | Just p <- [tsPR t] ]
  <> body t
  <> concatMap comment (tsComments t)
  where
    body x | tsRedacted x = ["", "(body redacted)"]
           | otherwise    = maybe [] (\b -> ["", pretty (safeText b)]) (tsBody x)

    coords p = "pr" <+> viaShow (psCoords p)

    comment c =
      [ ""
      , "---" <+> hashDoc (cId c)
          <+> "by" <+> keyDoc (cAuthor c)
          <+> "at" <+> pretty (cFoldedTs c)
          <> maybe mempty (\r -> " in reply to" <+> hashDoc r) (cReplyTo c)
      ]
      <> ( if cRedacted c
             then ["(redacted)"]
             else maybe [] (\b -> [pretty (safeText b)]) (cBody c) )
      <> [ "body-part" <+> hashDoc h | Just h <- [cBodyPart c] ]

-- | The surviving events, oldest first.
--
-- "Surviving" is the word PEP-22 uses and it is load-bearing: compaction drops
-- superseded @set@ events, so this is what canon still holds and not
-- everything that ever happened.
logDoc :: Maybe Word64 -> FoldResult -> [Doc ann]
logDoc mnum fr =
  [ hsep [ fill 6 (pretty (lgSeq e))
         , pretty (opOf (lgContent e))
         , hashDoc (lgEvent e)
         , "by" <+> keyDoc (lgAuthor e) ]
  | e <- frLog fr
  , maybe True (\n -> lgThread e `elem` fmap Just (threadsNumbered n)) mnum
  ]
  where
    threadsNumbered n = [ tsId t | t <- HM.elems (frThreads fr), tsNumber t == Just n ]

    opOf = \case
      AOpen{}     -> "open"     ; AComment{}  -> "comment"
      ARevise{}   -> "revise"   ; ASet{}      -> "set"
      AClose{}    -> "close"    ; AReopen{}   -> "reopen"
      AMerge{}    -> "merge"    ; ARedact{}   -> "redact"
      ADelegate{} -> "delegate" ; ARevoke{}   -> "revoke"

-- | The read verbs.
readEntries :: forall c m . ( IsContext c
                            , MonadUnliftIO m
                            , Exception (BadFormException c)
                            ) => MakeDictM c m ()
readEntries = do

  listVerb "hub:issue:list" HubIssue "issues"
  listVerb "hub:pr:list" HubPR "pull requests"

  showVerb "hub:issue:show" HubIssue
  showVerb "hub:pr:show" HubPR

  brief "print the surviving events of this repository's canon"
    $ args [arg "string" "repo-key"]
    $ desc ( "Read-only and peerless, like every other read verb: canon is a"
             <> line <> "git ref in this repository."
             <> line
             <> line <> "'Surviving' is not decoration. Compaction drops superseded"
             <> line <> "set events (PEP-19), so this is what canon still holds and"
             <> line <> "not a history of everything that ever happened."
             <> line
             <> line <> "Give a number to see one thread's events." )
    $ entry $ bindMatch "hub:log" $ nil_ \case
        [ SignPubKeyLike repo ] -> lift (withFold repo (out . logDoc Nothing))
        [ SignPubKeyLike repo, LitIntVal n ] ->
          lift (withFold repo (out . logDoc (Just (fromIntegral n))))
        _ -> liftIO (die "usage: hub log <repo-key> [<number>]")

  where

    listVerb name kind what =
      brief (fromString ("list this repository's folded " <> what))
        $ args [arg "string" "repo-key"]
        $ desc ( "Read-only and peerless: canon is a git ref in this"
                 <> line <> "repository, so this needs neither a peer nor a key."
                 <> line
                 <> line <> "--status open|closed|merged and --label <label> narrow"
                 <> line <> "the list. They are exact matches, not the query DSL"
                 <> line <> "PEP-22 inherits from fixme-new: that one compiles to SQL"
                 <> line <> "over a materialized cache, and this build folds canon in"
                 <> line <> "memory and has no cache."
                 <> line
                 <> line <> "A label here is one an owner APPLIED. What an author asked"
                 <> line <> "for on open is shown by 'show' and never counted as a"
                 <> line <> "label: applying one is an owner-signed event (PEP-19), so"
                 <> line <> "merging the two would let a stranger label their own"
                 <> line <> "issue." )
        $ entry $ bindMatch name $ nil_ \case
            (listArgs -> Just (repo, f)) ->
              lift (withFold repo (out . listDoc . threadsOf kind f))
            _ -> liftIO (die ("usage: hub " <> spelled name
                                <> " <repo-key> [--status S] [--label L]"))

    showVerb name kind =
      brief "show one folded thread with its comments"
        $ args [arg "string" "repo-key", arg "string" "number"]
        $ desc ( "Read-only and peerless."
                 <> line
                 <> line <> "Takes the number, or --thread with the thread-id. The id"
                 <> line <> "is behind a flag because a thread-id and a repository key"
                 <> line <> "are both thirty-two bytes of base58 and nothing can tell"
                 <> line <> "them apart positionally."
                 <> line
                 <> line <> "A body shipped as an encrypted tree is named, not fetched:"
                 <> line <> "this verb talks to no peer." )
        $ entry $ bindMatch name $ nil_ \case
            [ SignPubKeyLike repo, LitIntVal n ] ->
              lift (withFold repo (pick kind (byNumber (fromIntegral n))))
            [ SignPubKeyLike repo, StringLike "--thread", HashLike h ] ->
              lift (withFold repo (pick kind ((== h) . tsId)))
            _ -> liftIO (die ("usage: hub " <> spelled name
                                <> " <repo-key> (<number> | --thread <id>)"))

    -- "hub:issue:list" as somebody types it.
    spelled n = fmap (\c -> if c == ':' then ' ' else c) (drop 4 (show (pretty n)))

    pick kind p fr = case [ t | t <- HM.elems (frThreads fr), p t, tsKind t == kind ] of
      (t:_) -> out (showDoc t)
      -- Not an empty page. A thread that is not there and a thread with
      -- nothing in it are different answers, and only one of them is worth
      -- retrying.
      []    -> liftIO $ do
                 hPutDoc stderr ("no such thread in canon" <> line)
                 exitWith (ExitFailure codeNoSuchThread)

    byNumber n t = tsNumber t == Just n

    -- Canon, or the same refusal and the same exit code `hub verify` gives.
    -- One table, so a script branches on one set of numbers whichever read
    -- verb it ran.
    withFold repo act =
      withGitCanon (\cs -> readCanon cs repo) >>= \case
        Right st -> act (stFold st)
        Left u -> liftIO $ do
          hPutDoc stderr (refusalDoc u <> line)
          exitWith (ExitFailure (codeOf u))

    -- 141 is 128 plus SIGPIPE, what a shell reports for a program a pipe
    -- killed. `hub issue list K | head` exiting 1 would say "bad argument"
    -- about a listing that was fine.
    out ds = liftIO $ handleJust
      (\e -> if isResourceVanishedError e then Just () else Nothing)
      (\_ -> exitWith (ExitFailure 141))
      (mapM_ print ds)

-- | The repository key, then the two flags, in any order.
--
-- Exported, like everything else here that decides something. It was not, and
-- the module header is about exactly that: what lives in a @where@ clause
-- cannot be asked a question, and this is what a listing means.
--
-- The bug that cost: a repeated flag made @flag@ answer Nothing while @ok@
-- still passed (it looks at even-indexed words, and both @--label@s are in
-- even positions), so `hub issue list K --label a --label b` came back as
-- @Filter Nothing Nothing@ -- every issue in the tracker, exit zero, from a
-- command that asked for two labels. `--label 2026` did the same by another
-- route, since 'argvAtom' keeps a numeric word as a number and @StringLike@
-- does not match one. A filter that silently does not run is worse than one
-- that refuses: the caller reads the output as filtered.
-- THROUGH 'flagsOf', like every other reader in this package, and it was the
-- last one that was not. The hand-rolled pairing had the hole the shared reader
-- exists to close: @ok@ looked at even-indexed words only, so
-- @hub issue list K --status --label@ passed it (index 0 is a known flag, the
-- length is even) and @flag "--status"@ then zipped it against the word after,
-- answering @Filter (Just "--label") Nothing@ -- a listing filtered on a status
-- nobody typed, exit zero. That is exactly the "a flag where a value belongs"
-- case @c40ed994@ removed from six other readers, and this one was missed.
--
-- It also brings @--flag=value@, which 'issueUsage' has been telling everybody
-- is accepted and which five verbs refused.
listArgs :: forall c . IsContext c => [Syntax c] -> Maybe (HubKey, Filter)
listArgs syn = case syn of
  (SignPubKeyLike repo : rest) -> do
    kvs <- flagsOf ["--status","--label"] rest
    -- Through 'flagText', the same reading the compose verbs use: a status or
    -- a label that spells a number is the word that was typed, and 'argvAtom'
    -- keeps a numeric word as a number.
    st <- flagMaybe kvs "--status" (fmap Text.pack . flagText)
    lb <- flagMaybe kvs "--label"  (fmap Text.pack . flagText)
    pure (repo, Filter st lb)
  _ -> Nothing
