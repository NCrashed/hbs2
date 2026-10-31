{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
-- | @hub issue list@, @hub issue show@, @hub pr list@, @hub pr show@ and
-- @hub log@ (PEP-22 "Read").
--
-- The read side of the tracker. Thread content is a function of one
-- 'FoldResult', so none of it needs a peer, a key or a network: canon is a git
-- ref in this repository and the fold over it is what these print. Pull request
-- rows also carry a read-only observation of the ordinary git refs in this
-- clone: whether the folded proposal tip merges into its target as they stand.
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
  , emptyListing
  , listDoc
  , listDocChecked
  , showDoc
  , showDocChecked
  , logDoc
  , logContract
  , logOrder
  , statusOf
  , listArgs
  , listJsonArgs
  , indexContract
  , indexContractChecked
  , mergeabilityOf
  , showArgs
  , logArgs
  , Which(..)
  , labelsOf
  , assigneesOf
  , diffArgv
  , codeNoSuchThread
  , codeAmbiguousNumber
  , oneNumbered
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Render
import HBS2.Hub.Fold
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git (withGitCanon,gitRun,GitTrouble(..))
import HBS2.Hub.Canon (takeBytes)
import HBS2.Hub.Repo.GitBundle (refTip,mergeTree,Merged(..))
import HBS2.Hub.CLI.Argv (badArgs,flagsAndSwitches,flagSwitch,flagMaybe,flagText,flagWord
                         ,repoFlags,flagRepo,repoAndFlags)
import HBS2.Hub.CLI.Common (withCanon,OnMissing(..),refuse,saying,utcOf)

import HBS2.CLI.Prelude hiding (null)
import HBS2.CLI.Run.Internal

import HBS2.Data.Types.Refs (pattern HashLike, HashRef)

import Data.Aeson (Value,object,(.=))
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (sortOn)
import Data.List qualified as List
import Data.Maybe (fromMaybe,isJust)
import Control.Applicative ((<|>))
import System.IO.Error (isResourceVanishedError)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Word (Word64)
import System.Exit (exitWith,ExitCode(..))

-- | Asked for a thread canon does not hold.
--
-- Its own code rather than 1: a number that is not there is not a usage error,
-- and a script that polls an issue until it appears has to tell the two apart.
codeNoSuchThread :: Int
codeNoSuchThread = 26

-- | Canon holds more than one thread with the number given.
--
-- Its own code because the remedy is not 'codeNoSuchThread''s: the number is
-- real and names two threads, and what resolves it is naming the thread by id.
-- @hub verify@ reports the same state as a @DupNumber@ anomaly.
codeAmbiguousNumber :: Int
codeAmbiguousNumber = 49

-- | The one thread canon gives that number, or a refusal saying which way.
--
-- HERE AND NOT IN FOUR VERBS. `DupNumber` is an anomaly the fold reports and
-- does not drop, so canon can legitimately hold two threads numbered alike --
-- two maintainers minting from one view is the case PEP-19 leaves open. Every
-- resolver in the package took the head of an unordered traversal: for a reader
-- that is an arbitrary answer, and for a writer it is worse, since
-- `hub issue close --number 42` minted an owner-signed close against whichever
-- thread the HAMT yielded first, said nothing about the other, and put it in
-- append-only canon. Choosing between two threads to sign against is not a
-- decision a tool makes for a maintainer.
--
-- Four verbs asked this question and each spelled its own answer, which is how
-- three of them came to spell it wrong.
oneNumbered :: MonadUnliftIO m => Word64 -> FoldResult -> m ThreadId
oneNumbered n fr = case threadsNumbered n fr of
  [t] -> pure t
  []  -> liftIO (refuse (show ("canon holds no thread numbered" <+> pretty n))
                        codeNoSuchThread)
  ts  -> liftIO $ refuse
           (show ( "canon holds" <+> pretty (length ts) <+> "threads numbered"
                     <+> pretty n <> ", so this cannot say which you mean:"
                   <> line
                   <> indent 2 (vcat (fmap hashDoc ts))
                   <> line
                   <> "  `hbs2-hub verify <repo-key>` reports it as a duplicate"
                     <+> "number." ))
           codeAmbiguousNumber

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
labelsOf = maybe [] decodeLabels . HM.lookup attrLabels . tsAttrs

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
--
-- PLURAL and set-valued, like 'labelsOf' and for the same reason: PEP-19 spells
-- an attribute that can hold a set as one everywhere. This read the singular,
-- which the writing verb also wrote and which nothing else in the package knew
-- -- not 'multiValued', so no value was ever normalized, and not the PEP-22
-- contract, so `--json` reported every assignment as none.
assigneesOf :: ThreadState -> [Text]
assigneesOf = maybe [] decodeLabels . HM.lookup attrAssignees . tsAttrs

-- | What git may be asked for the diff of these coordinates, if anything.
--
-- THE ONE PLACE IN THIS PACKAGE THAT HANDED CANON'S TEXT TO GIT UNCHECKED, and
-- the shape it used made that fatal rather than merely sloppy. Both coordinates
-- are a stranger's: 'oversizedField' bounds them by SIZE and by nothing else,
-- and a fork-path proposal reaches canon with no verification at all and says
-- so out loud. Joined into one argv word as a range, they are an option --
-- @--output=sub/@ and @/victim.txt@ meet through the range's own @..@ to make
-- @--output=sub\/..\/victim.txt@ -- and @git diff@ then truncates whatever that
-- resolves to, exits 0 and prints nothing, so the render contract reports the
-- diff as available and empty. Reading a stranger's canon is the ordinary case
-- for this verb.
--
-- TWO WORDS AND A @--@, so neither coordinate can be read as an option or as a
-- path, and 'validSha' first, because both are documented as object names and
-- that is the predicate the rest of the package already applies to every
-- coordinate it hands git ("HBS2.Hub.Repo.GitBundle"). Coordinates that are not
-- object names are not a diff this build can ask for, which the caller reports
-- as unavailable.
--
-- Top level and exported because it decides something, which is this module's
-- own rule: the two bugs it records having had were both in a @where@ clause
-- nothing could ask about.
diffArgv :: PRCoords -> Maybe [String]
diffArgv co
  | validSha (prBase co) && validSha (prSourceTip co) =
      Just [ "diff", "--no-color"
           , Text.unpack (prBase co), Text.unpack (prSourceTip co), "--" ]
  | otherwise = Nothing

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

-- | What an empty listing says, and it used to say nothing at all.
--
-- AN EMPTY PROJECT AND A TYPO LOOK THE SAME on stdout, and one of them is much
-- more likely: @--status@ and @--label@ are matched literally against attribute
-- values, which are a stranger's bytes and extensible by design, so there is no
-- vocabulary to check a value against -- @--status closd@ is a well-formed
-- filter that matches nothing, and so is @--status Open@.
--
-- What CAN be said is what canon holds, which is why this takes the fold: the
-- values in the answer are the ones a filter would have matched. Nothing here
-- is a hard-coded list of statuses; a repository that invents one gets it named
-- back the same way.
--
-- ON STDERR at the call site, and exit 0: an empty answer to a well-formed
-- question is an answer. Pure and exported so a test can ask what it says.
emptyListing :: HubKind -> Filter -> FoldResult -> Doc ann
emptyListing kind f fr
  | List.null ofKind = "canon holds no" <+> plural <+> "at all"
  | otherwise =
      "no" <+> pretty (kindOf kind) <+> "matches" <+> asked
        <> line <> "  canon holds" <+> pretty (length ofKind) <+> plural
        <> "; the statuses in it are:"
        <+> hsep (punctuate comma (fmap (pretty . safeText) statuses))
        <> line <> "  a filter is matched literally against what canon says,"
        <+> "and canon says whatever an owner set."
  where
    ofKind = [ t | t <- HM.elems (frThreads fr), tsKind t == kind ]
    plural = case kind of { HubIssue -> "issues" ; HubPR -> "pull requests" }
    statuses = List.nub (sortOn id (fmap statusOf ofKind))
    asked = hsep ( [ "--status" <+> pretty (safeText s) | Just s <- [fStatus f] ]
                <> [ "--label" <+> pretty (safeText l) | Just l <- [fLabel f] ] )

-- | One line per thread.
--
-- The number, the status, the labels and the title. Not the author: a base58
-- key is forty-odd columns and would push every title off the line, and
-- @issue show@ prints it.
listDoc :: [ThreadState] -> [Doc ann]
listDoc = listDocChecked . fmap (, Nothing)

-- | One line per thread, including the result of the read-only merge check
-- when the caller made one.  The short state belongs in the list; the paths
-- belong in 'showDocChecked', where they do not turn one row into a page.
listDocChecked :: [(ThreadState, Maybe PRMergeability)] -> [Doc ann]
listDocChecked ts =
  [ hsep [ num (tsNumber t)
         , fill 6 (statusDoc t)
         , title t
         , labels t
         , mergeState m ]
  | (t, m) <- ts ]
  where
    num = fill 5 . maybe "-" (("#" <>) . pretty)

    -- A redacted thread keeps its row: it is in canon and a reader who cannot
    -- see it in the list would conclude the numbering has a hole.
    title t | tsRedacted t = "(redacted)"
            | otherwise    = pretty (safeText (tsTitle t))

    labels t = case labelsOf t of
      [] -> mempty
      ls -> brackets (hsep (punctuate comma (fmap (pretty . safeText) ls)))

    mergeState = \case
      Just MergeClean{}       -> "[mergeable]"
      Just MergeConflicting{} -> "[conflicts]"
      Just MergeUnknown{}     -> "[mergeability unknown]"
      Nothing                 -> mempty

-- | One thread, with its comments.
showDoc :: ThreadState -> [Doc ann]
showDoc t =
  [ hsep [ maybe "(no number)" (("#" <>) . pretty) (tsNumber t)
         , statusDoc t
         , if tsRedacted t then "(redacted)" else pretty (safeText (tsTitle t)) ]
  , "thread" <+> hashDoc (tsId t)
  , "kind" <+> pretty (kindOf (tsKind t))
  , "author" <+> keyDoc (tsAuthor t)
  , "blessed-by" <+> keyDoc (tsCanonBy t)
  -- THROUGH 'utcOf', both of them. They were epoch milliseconds, which is what
  -- the field IS (PEP-19) and what --json carries; this report is read by a
  -- person, and thirteen digits is not a date anybody compares to another. The
  -- queue next door has printed them this way from the start.
  , "created" <+> pretty (utcOf (tsCreated t))
      <+> "updated" <+> pretty (utcOf (tsUpdated t))
  ]
  -- Only when there is one: an assignment is cleared by setting the attribute
  -- to the empty set (last-writer-wins has no way to remove one), and a line
  -- reading "assignees" with nothing after it says the opposite of what canon
  -- holds.
  <> [ "assignees" <+> hsep (punctuate comma (fmap (pretty . safeText) as))
     | as@(_:_) <- [assigneesOf t] ]
  <> [ "labels" <+> hsep (punctuate comma (fmap (pretty . safeText) ls))
     | ls <- [labelsOf t], not (null ls) ]
  -- Said only when there is something to say, and said as a REQUEST: the
  -- author asked, nobody applied it. WITHHELD on a redacted thread, like the
  -- body: it is the same stranger's text, up to 32 labels of 128 bytes, on the
  -- one event a redact of an open is usually aimed at.
  <> [ "labels-requested" <+> hsep (punctuate comma (fmap (pretty . safeText) ls))
     | not (tsRedacted t), ls <- [tsLabelsRequested t], not (null ls) ]
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
  -- to know the body is fetchable and this is a terminal. Not on a redacted
  -- thread: "secret published" beside a withheld body tells a reader exactly
  -- how to go and read it, which the two renderers used to disagree about.
  <> [ "body-part" <+> hashDoc h
         <+> (if isJust (tsPartSecret t) then "(secret published)" else "(no secret)")
     | not (tsRedacted t), Just h <- [tsBodyPart t] ]
  <> [ coords p | not (tsRedacted t), Just p <- [tsPR t] ]
  <> body t
  <> concatMap comment (tsComments t)
  where
    body x | tsRedacted x = ["", "(body redacted)"]
           | otherwise    = maybe [] (\b -> ["", pretty (safeText b)]) (tsBody x)

    -- THE FIELDS, not the record. This was the derived Show of 'PRCoords', on
    -- the most important line of a pull request: what a maintainer wants is the
    -- branch it lands on and the commit being proposed, and what they got was
    -- a Haskell constructor with Maybe and Just in it, three quoted strings
    -- deep. The words are `hub pr show`'s, so the two reports agree.
    coords p = vcat
      ( [ "onto" <+> pretty (safeText (prOnto c))
        , "from" <+> pretty (safeText (prSourceRef c))
        , "tip"  <+> pretty (safeText (prSourceTip c))
        , "base" <+> pretty (safeText (prBase c))
        ]
     <> [ "source" <+> pretty (safeText s) | Just s <- [prSource c] ]
     <> [ "bundle" <+> hashDoc (ptPart b) | Just b <- [prBundle c] ] )
      where c = psCoords p

    comment c =
      [ ""
      , "---" <+> hashDoc (cId c)
          <+> "by" <+> keyDoc (cAuthor c)
          <+> "at" <+> pretty (utcOf (cFoldedTs c))
          <> maybe mempty (\r -> " in reply to" <+> hashDoc r) (cReplyTo c)
      ]
      <> ( if cRedacted c
             then ["(redacted)"]
             else maybe [] (\b -> [pretty (safeText b)]) (cBody c) )
      -- OUTSIDE the branch above, which is what it was, so a redacted comment
      -- printed "(redacted)" and then the hash of the body it had just
      -- withheld. The JSON contract hides it; two renderers disagreeing about
      -- what a redaction covers is the same defect twice.
      <> [ "body-part" <+> hashDoc h | not (cRedacted c), Just h <- [cBodyPart c] ]

-- | One thread and the mergeability observation made for it. Conflict paths
-- are stranger-controlled git paths, so they pass through 'safeText'. Kept as
-- a small overlay on the established renderer: adding a git observation does
-- not give the thread's canon-derived projection a second implementation.
showDocChecked :: ThreadState -> Maybe PRMergeability -> [Doc ann]
showDocChecked t mmerge = showDoc t <> mergeabilityDocs mmerge

mergeabilityDocs :: Maybe PRMergeability -> [Doc ann]
mergeabilityDocs = \case
  Nothing -> []
  Just (MergeClean target head') ->
    [ "mergeability clean"
    , "checked-target" <+> pretty target
    , "checked-head" <+> pretty head' ]
  Just (MergeConflicting target head' paths) ->
    [ "mergeability conflicts"
    , "checked-target" <+> pretty target
    , "checked-head" <+> pretty head'
    , "conflicting-paths" <> line <> indent 2 (vcat (fmap (pretty . safeText) paths)) ]
  Just (MergeUnknown why) -> ["mergeability unknown:" <+> pretty (safeText why)]

-- | The surviving events, oldest first.
--
-- "Surviving" is the word PEP-22 uses and it is load-bearing: compaction drops
-- superseded @set@ events, so this is what canon still holds and not
-- everything that ever happened.
--
-- THE THREADS AND NOT THE NUMBER, which is the whole of the fix. This took the
-- number and resolved it inside the comprehension's guard, so the traversal of
-- every thread in canon ran once per LOG ENTRY -- over a tree somebody else
-- published, bounded by 'maxCanonFiles' at 200000 in each direction. Taking the
-- resolved set instead makes the quadratic version unwritable rather than
-- merely absent: there is no number here to resolve and no fold to resolve it
-- against, so the guard is a hash lookup and nothing else.
--
-- A SET, because how many threads share a number is not this reader's choice:
-- @DupNumber@ is an anomaly the fold reports and does not drop, so a hostile
-- canon can number every thread alike and a list membership would be the same
-- defect one layer down.
--
-- An event with no thread is not in a filtered log. It never was -- the old
-- guard compared against @Just@ ids -- and it is said here because the shape
-- that said it went away.
logDoc :: Maybe (HashSet ThreadId) -> FoldResult -> [Doc ann]
logDoc only fr =
  [ hsep [ fill 6 (pretty (lgSeq e))
         , pretty (opOf (lgContent e))
         , hashDoc (lgEvent e)
         , "by" <+> keyDoc (lgAuthor e) ]
  | e <- frLog fr
  , wanted (lgThread e)
  ]
  where
    wanted = case only of
      Nothing  -> const True
      Just ids -> maybe False (`HS.member` ids)

    opOf = \case
      AOpen{}     -> "open"     ; AComment{}  -> "comment"
      ARevise{}   -> "revise"   ; ASet{}      -> "set"
      AClose{}    -> "close"    ; AReopen{}   -> "reopen"
      AMerge{}    -> "merge"    ; ARedact{}   -> "redact"
      ADelegate{} -> "delegate" ; ARevoke{}   -> "revoke"

-- | The read verbs.
-- | How a thread comes out: for a person, or for a program.
--
-- A constructor and not a Bool, because `pick kind True p` at the call site
-- says nothing about which of the two True is.
data Shape = AsDoc | AsJson

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
    $ args [arg "string" "repo-key", arg "string" "[number]", arg "string" "[--json]"]
    $ desc ( "Read-only and peerless, like every other read verb: canon is a"
             <> line <> "git ref in this repository."
             <> line
             <> line <> "'Surviving' is not decoration. Compaction drops superseded"
             <> line <> "set events (PEP-19), so this is what canon still holds and"
             <> line <> "not a history of everything that ever happened."
             <> line
             <> line <> "Give a number to see one thread's events."
             <> line
             <> line <> "--json prints one `log` document (contract 1): the same"
             <> line <> "events in seq order, each with its op, thread, number,"
             <> line <> "author, the key that blessed it, its folded-at time and"
             <> line <> "the letter it came from. No text: titles and bodies are"
             <> line <> "`issue show --json`'s, which withholds the redacted ones." )
    $ entry $ bindMatch "hub:log" $ nil_ \case
        (logArgs -> Just (repo, n, asJson)) -> lift $ withFold repo \fr -> do
          -- A NUMBER THAT NAMES NO THREAD IS A REFUSAL, like it is in `show`.
          -- Filtering an empty answer out of the log left this exiting 0 with
          -- nothing on stdout, which is what a thread with no events would look
          -- like -- so `hub log K 999` and a real but silent thread were the
          -- same answer, and the sibling verb already exits 26 for the first.
          --
          -- RESOLVED ONCE, HERE, and by the function every other resolver in
          -- the package uses. The refusal and the filter are one question asked
          -- once instead of two answers spelled two ways: this walked the whole
          -- thread map to decide whether to refuse, and then the renderer
          -- walked it again per log entry with a second copy of the rule.
          only <- for n $ \want -> case threadsNumbered want fr of
            []  -> liftIO (refuse (show ("canon holds no thread numbered"
                                           <+> pretty want))
                                  codeNoSuchThread)
            ids -> pure (HS.fromList ids)
          if asJson
            then emit (renderDocument logOrder (logContract repo n only fr))
            else out (logDoc only fr)
        other -> liftIO (badArgs ("usage: hub log <repo-key> [<number>] [--json]"
                                    <> line <> "   or: hub log --repo <key> [--number <n>] [--json]")
                                 other)

  where

    listVerb name kind what =
      brief (fromString ("list this repository's folded " <> what))
        $ args [arg "string" "[--repo] repo-key", arg "string" "[--json]"]
        $ desc ( "Read-only and peerless: canon is a git ref in this"
                 <> line <> "repository, so this needs neither a peer nor a key."
                 <> line
                 <> line <> "--status open|closed|merged and --label <label> narrow"
                 <> line <> "the list. They are exact matches, not the query DSL"
                 <> line <> "PEP-22 inherits from fixme-new: that one compiles to SQL"
                 <> line <> "over a materialized cache, and this build folds canon in"
                 <> line <> "memory and has no cache."
                 <> line
                 <> line <> "Open pull requests are checked against their current target"
                 <> line <> "without changing the worktree. A list checks at most 64; a"
                 <> line <> "row past that says not checked, and `pr show` checks it."
                 <> line
                 <> line <> "A label here is one an owner APPLIED. What an author asked"
                 <> line <> "for on open is shown by 'show' and never counted as a"
                 <> line <> "label: applying one is an owner-signed event (PEP-19), so"
                 <> line <> "merging the two would let a stranger label their own"
                 <> line <> "issue."
                 <> line
                 <> line <> "--json prints one `index` document (contract 1, PEP-24):"
                 <> line <> "per thread its number, kind, id, title, status, applied"
                 <> line <> "labels, author, comment count, folded times and, for an open"
                 <> line <> "pull request, mergeability against this clone's target. A redacted"
                 <> line <> "thread keeps its row with a null title." )
        $ entry $ bindMatch name $ nil_ \case
            (listJsonArgs -> Just ((repo, f), asJson)) ->
              lift $ withFold repo \fr -> do
                let ts = threadsOf kind f fr
                checked <- checkListing ts
                if asJson
                  then emit (renderDocument indexOrder (indexContractChecked repo kind f checked))
                  else do
                    out (listDocChecked checked)
                    -- On stderr and exit 0: an empty answer to a well-formed
                    -- question is an answer, and this is advice about it.
                    when (List.null ts) $
                      liftIO (saying (emptyListing kind f fr <> line))
            other -> liftIO (badArgs ( "usage: hub " <> pretty (spelled name)
                                         <> " <repo-key> [--status S] [--label L] [--json]"
                                         <> line <> "   or: hub " <> pretty (spelled name)
                                         <> " --repo <key> [--status S] [--label L] [--json]" )
                                     other)

    showVerb name kind =
      brief "show one folded thread with its comments"
        $ args [arg "string" "repo-key", arg "string" "number", arg "string" "[--json]"]
        $ desc ( "Read-only and peerless."
                 <> line
                 <> line <> "Takes the number, or --thread with the thread-id. The id"
                 <> line <> "is behind a flag because a thread-id and a repository key"
                 <> line <> "are both thirty-two bytes of base58 and nothing can tell"
                 <> line <> "them apart positionally."
                 <> line
                 <> line <> "A body shipped as an encrypted tree is named, not fetched:"
                 <> line <> "this verb talks to no peer."
                 <> line
                 <> line <> "--json prints the PEP-22 render contract instead: the"
                 <> line <> "same thread as a versioned, documented JSON object that"
                 <> line <> "a web layer or any other renderer reads without touching"
                 <> line <> "hbs2, crypto or the event log. There is no diff in it"
                 <> line <> "here, and it says so ('unavailable'), because building"
                 <> line <> "one needs git. For an open pull request it also reports the"
                 <> line <> "read-only mergeability check against this clone's target." )
        $ entry $ bindMatch name $ nil_ \case
            (showArgs -> Just (repo, which, asJson)) ->
              lift (withFold repo (pick kind (if asJson then AsJson else AsDoc)
                                             (matches which)))
            other -> liftIO (badArgs ( "usage: hub " <> pretty (spelled name)
                                         <> " <repo-key> (<number> | --thread <id>) [--json]"
                                         <> line <> "   or: hub " <> pretty (spelled name)
                                         <> " --repo <key> (--number <n> | --thread <id>) [--json]" )
                                     other)

    matches = \case
      ByNumber n -> byNumber n
      ByThread h -> (== h) . tsId

    -- "hub:issue:list" as somebody types it.
    spelled n = fmap (\c -> if c == ':' then ' ' else c) (drop 4 (show (pretty n)))

    pick kind how p fr = case [ t | t <- HM.elems (frThreads fr), p t, tsKind t == kind ] of
      (t:_) -> do
               m <- mergeabilityOf Nothing t
               case how of
                 AsDoc  -> out (showDocChecked t m)
                 AsJson -> do
                   d <- diffOf t
                   liftIO (LBS.putStr (renderContract (threadContractChecked t d m)))
      -- Not an empty page. A thread that is not there and a thread with
      -- nothing in it are different answers, and only one of them is worth
      -- retrying.
      []    -> liftIO $ do
                 saying ("no such thread in canon" <> line)
                 exitWith (ExitFailure codeNoSuchThread)

    byNumber n t = tsNumber t == Just n

    -- | The diff of a pull request, so a static renderer needs no git.
    --
    -- PEP-22 puts this in the contract precisely so that whoever renders it
    -- does not have to be standing in a repository. Computing it belongs HERE
    -- and not in the serializer, which is pure and testable without one; this
    -- is the layer that already has git.
    --
    -- THREE ANSWERS, and the middle one is the reason it is not a Maybe.
    -- `available` is the objects being here. `reconstructable` is the objects
    -- being gone while the bundle attachment that would rebuild them is still
    -- named by canon, which is what a rejected pull request looks like after
    -- its staged ref was dropped: a renderer can offer to rebuild rather than
    -- pretending there was never anything to see. `unavailable` is neither.
    --
    -- Bounded, because a diff is a stranger's proposal and this is a byte
    -- stream a web layer will embed. Over the bound it is truncated and SAYS
    -- so, and a renderer that wants the whole thing has the two commits.
    diffOf t = case tsPR t of
      Nothing -> pure Nothing
      Just pr -> do
        let co = psCoords pr
        -- BOUNDED AT THE PIPE, and the bound is this contract's own. It used to
        -- be taken whole -- `gitRun` kept stdout with no ceiling at all -- and
        -- truncated afterwards, so a proposal of a hundred megabytes of
        -- generated files was a hundred megabytes held in memory to print 256
        -- KiB of it. What comes back over the bound is the prefix, which is
        -- exactly what this rendered before, and 'GitTooMuch' is what says the
        -- flag must be set.
        r <- case diffArgv co of
               Nothing -> pure (Left ())
               Just as -> Right <$> gitRun Nothing [] 30 maxDiffBytes
                                          "the diff of a proposal" as mempty
        pure $ Just $ case r of
          Right (Right (ExitSuccess, o, _)) ->
            PRDiff DiffAvailable False (Text.decodeUtf8With Text.lenientDecode o)
          Right (Left (GitTooMuch _ _ kept)) ->
            PRDiff DiffAvailable True
                   (takeBytes maxDiffBytes (Text.decodeUtf8With Text.lenientDecode kept))
          -- git could not answer, or was never asked because the coordinates
          -- are not object names. Whether the objects can be got back is what
          -- canon says: a bundle part is a way, a fork pointer is not one this
          -- build can follow.
          _ | Just{} <- prBundle co -> PRDiff DiffReconstructable False ""
            | otherwise             -> PRDiff DiffUnavailable False ""

    -- | What a diff is allowed to weigh in the contract.
    --
    -- A judgement, and on the generous side: this is one thread's JSON, read by
    -- a renderer that asked for it, not something gossiped. What it bounds is a
    -- contributor proposing a hundred megabytes of generated files and a web
    -- layer embedding it in a page.
    maxDiffBytes :: Int
    maxDiffBytes = 256 * 1024

    -- A canon may hold hundreds of thousands of threads. One git child per
    -- open proposal without a ceiling would turn a read into an unbounded
    -- process factory; rows past the ceiling say they were not checked and a
    -- detail view still checks that proposal on its own.
    checkListing = go (0 :: Int)
      where
        go _ [] = pure []
        go n (t:rest)
          | checkable t && n >= maxListMergeChecks =
              ((t, Just (MergeUnknown "not checked in this listing; open the pull request to check it")) :)
                <$> go n rest
          | otherwise = do
              m <- mergeabilityOf Nothing t
              let n' = if isJust m then n + 1 else n
              ((t, m) :) <$> go n' rest

        checkable t = tsKind t == HubPR && not (tsRedacted t) && statusOf t == "open"

    maxListMergeChecks = 64

    -- Canon, or the same refusal and the same exit code `hub verify` gives.
    -- One table, so a script branches on one set of numbers whichever read
    -- verb it ran.
    withFold repo act =
      -- Refuse: a listing of a repository with no canon is not an empty
      -- listing, it is a question this clone cannot answer, and the remedy
      -- (fetch the ref, which a plain clone does not) is what refusalDoc adds.
      withCanon Refuse repo withGitCanon >>= act . snd

    -- 141 is 128 plus SIGPIPE, what a shell reports for a program a pipe
    -- killed. `hub issue list K | head` exiting 1 would say "bad argument"
    -- about a listing that was fine.
    out ds = liftIO $ handleJust
      (\e -> if isResourceVanishedError e then Just () else Nothing)
      (\_ -> exitWith (ExitFailure 141))
      (mapM_ print ds)

    -- A document, under the same guard: the whole of it or a 141.
    emit bs = liftIO $ handleJust
      (\e -> if isResourceVanishedError e then Just () else Nothing)
      (\_ -> exitWith (ExitFailure 141))
      (LBS.putStr bs)

-- | Read-only mergeability at the two commits this clone currently has.
-- Nothing means "not an open, visible pull request"; failures are data rather
-- than refusals because a list must still show the proposal whose objects or
-- target have not arrived yet.  @hub pr land@ repeats the same plumbing before
-- any write, so this is a preview and never an authorization decision.
mergeabilityOf :: MonadUnliftIO m
               => Maybe FilePath -> ThreadState -> m (Maybe PRMergeability)
mergeabilityOf cwd t
  | tsKind t /= HubPR || tsRedacted t || statusOf t /= "open" = pure Nothing
  | otherwise = traverse (checkMergeability cwd . psCoords) (tsPR t)

checkMergeability :: MonadUnliftIO m
                  => Maybe FilePath -> PRCoords -> m PRMergeability
checkMergeability cwd co = do
  let head' = prSourceTip co
      targetRef = targetBranch (prOnto co)
  refTip cwd targetRef >>= \case
    Left _ -> pure (MergeUnknown "target ref could not be read")
    Right Nothing -> pure (MergeUnknown ("target ref is missing: " <> targetRef))
    Right (Just target) -> checkedMerge cwd target head'

targetBranch :: Text -> Text
targetBranch ref
  | "refs/heads/" `Text.isPrefixOf` ref = ref
  | otherwise = "refs/heads/" <> ref

checkedMerge :: MonadUnliftIO m
             => Maybe FilePath -> Text -> Text -> m PRMergeability
checkedMerge cwd target head' = mergeTree cwd target head' <&> \case
  Right (MergedTree _)      -> MergeClean target head'
  Right (MergeConflicts ps) -> MergeConflicting target head' ps
  Left _                    -> MergeUnknown "git could not compare the target and proposal"

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
listArgs syn = do
  -- Positionally or behind --repo, which is the spelling every verb that WRITES
  -- takes. See 'repoAndFlags': accepting it is additive now and removing the
  -- positional form after a release is not.
  (repo, kvs) <- repoAndFlags asKey ["--status","--label"] syn
  -- Through 'flagText', the same reading the compose verbs use: a status or
  -- a label that spells a number is the word that was typed, and 'argvAtom'
  -- keeps a numeric word as a number.
  st <- flagMaybe kvs "--status" (fmap Text.pack . flagText)
  lb <- flagMaybe kvs "--label"  (fmap Text.pack . flagText)
  pure (repo, Filter st lb)
  where
    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }

-- | Which thread @issue show@ / @pr show@ was asked about.
data Which = ByNumber Word64 | ByThread HashRef
  deriving stock (Eq,Show)

-- | @<repo> (<n> | --thread <id>) [--json]@, or the same behind flags.
--
-- The positional form is what the verb has always taken; the flag form exists
-- because @--repo@ is what every writing verb takes and a reader who learned it
-- there met a usage error here. See 'repoAndFlags'.
--
-- EXACTLY ONE of the two ways of naming a thread, in both forms: a line giving
-- a number and a thread-id is a line somebody edited half way, and picking one
-- is the guess this reader exists to refuse.
showArgs :: forall c . IsContext c => [Syntax c] -> Maybe (HubKey, Which, Bool)
showArgs syn = positional syn <|> flagged syn
  where
    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }

    positional = \case
      [ SignPubKeyLike repo, (flagWord -> Just n) ] -> Just (repo, ByNumber n, False)
      [ SignPubKeyLike repo, (flagWord -> Just n), StringLike "--json" ] ->
        Just (repo, ByNumber n, True)
      [ SignPubKeyLike repo, StringLike "--thread", HashLike h ] ->
        Just (repo, ByThread h, False)
      [ SignPubKeyLike repo, StringLike "--thread", HashLike h, StringLike "--json" ] ->
        Just (repo, ByThread h, True)
      _ -> Nothing

    flagged s = do
      kvs <- flagsAndSwitches (repoFlags <> ["--number","--thread"]) ["--json"] s
      repo <- flagRepo asKey kvs
      n <- flagMaybe kvs "--number" flagWord
      t <- flagMaybe kvs "--thread" (\case { HashLike h -> Just h ; _ -> Nothing })
      j <- flagSwitch kvs "--json"
      which <- case (n, t) of
                 (Just k, Nothing)  -> Just (ByNumber k)
                 (Nothing, Just h)  -> Just (ByThread h)
                 _                  -> Nothing
      pure (repo, which, j)

-- | @<repo> [<n>] [--json]@, or the same behind flags.
logArgs :: forall c . IsContext c => [Syntax c] -> Maybe (HubKey, Maybe Word64, Bool)
logArgs syn = positional syn <|> flagged syn
  where
    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }

    -- The switch last, in the positional form, the way 'showArgs' takes it.
    positional s = case s of
      [ SignPubKeyLike repo ] -> Just (repo, Nothing, False)
      -- Through 'flagWord', which guards BOTH ends. A bare fromIntegral on the
      -- Integer a LitIntVal carries guards neither: flagWord's own haddock is
      -- the report of that bug -- `--number 18446744073709551617` wrapped to 1
      -- and the verb answered about a thread nobody named.
      [ SignPubKeyLike repo, (flagWord -> Just n) ] -> Just (repo, Just n, False)
      [ SignPubKeyLike repo, StringLike "--json" ] -> Just (repo, Nothing, True)
      [ SignPubKeyLike repo, (flagWord -> Just n), StringLike "--json" ] ->
        Just (repo, Just n, True)
      _ -> Nothing

    flagged s = do
      kvs <- flagsAndSwitches (repoFlags <> ["--number"]) ["--json"] s
      repo <- flagRepo asKey kvs
      n <- flagMaybe kvs "--number" flagWord
      j <- flagSwitch kvs "--json"
      pure (repo, n, j)

-- | The surviving events as a document (@hub log --json@).
--
-- DERIVED FROM CANON, like the thread document and unlike the queue: two
-- clones of one canon produce the same bytes, which is why the order is the
-- fold's @seq@ and nothing else.
--
-- NO TEXT. A log entry's content holds a title and a body, and the thread
-- document is where those are shown, because it is where a redaction withholds
-- them. A second document carrying the same text would be a second place a
-- redaction has to be honoured, and the one somebody forgets. What is here is
-- the provenance: who, blessed by whom, when, about what, and from which
-- letter.
--
-- @filter_number@ is the filter that was asked for. @number@ on an event is
-- its thread's number, null for the repository-scope ops and for a thread
-- canon numbered none.
logContract :: RepoRef -> Maybe Word64 -> Maybe (HashSet ThreadId) -> FoldResult -> Value
logContract repo n only fr = object
  [ "contract" .= contractVersion
  , "document" .= ("log" :: Text)
  , "repo"     .= key58 repo
  , "filter_number" .= n
  , "events"   .= fmap one [ e | e <- frLog fr, wanted (lgThread e) ]
  ]
  where
    wanted = case only of
      Nothing  -> const True
      Just ids -> maybe False (`HS.member` ids)

    numberOf t = HM.lookup t (frThreads fr) >>= tsNumber

    one e = object
      [ "seq"       .= lgSeq e
      , "event_id"  .= b58 (lgEvent e)
      , "op"        .= opName (lgContent e)
      , "thread_id" .= fmap b58 (lgThread e)
      , "number"    .= (lgThread e >>= numberOf)
      , "author"    .= key58 (lgAuthor e)
      , "canon_by"  .= key58 (lgCanonBy e)
      , "folded_at" .= lgFoldedTs e
      , "origin"    .= fmap b58 (lgOrigin e)
      ]

logOrder :: [Text]
logOrder =
  [ "repo", "filter_number", "events", "seq", "event_id", "op", "thread_id", "number"
  , "author", "canon_by", "folded_at", "origin" ]

-- | 'listArgs', and whether @--json@ was asked for.
--
-- The switch is taken out before the rest is read rather than added to
-- 'listArgs', so the filter and the repository keep the one reading they have
-- and its tests; a list verb has no flag whose value could be @--json@.
listJsonArgs :: forall c . IsContext c => [Syntax c] -> Maybe ((HubKey, Filter), Bool)
listJsonArgs syn = (, asJson) <$> listArgs rest
  where
    isJson = \case { StringLike "--json" -> True ; _ -> False }
    asJson = any isJson syn
    rest = List.filter (not . isJson) syn

-- | The @index@ document (PEP-24): a list view's worth of each thread.
--
-- What `issue list` prints, as data: enough to draw a row and to decide which
-- thread to fetch with `show`, and nothing that needs the thread's text beyond
-- its title. Same rules as the thread document: derived from canon only, folded
-- times in milliseconds, and a redacted thread keeps its row with a null title,
-- for the reason 'listDoc' keeps it.
indexContract :: RepoRef -> HubKind -> Filter -> [ThreadState] -> Value
indexContract repo kind f = indexContractChecked repo kind f . fmap (, Nothing)

-- | The index with mergeability observations made against this clone's refs.
indexContractChecked :: RepoRef -> HubKind -> Filter
                     -> [(ThreadState, Maybe PRMergeability)] -> Value
indexContractChecked repo kind f ts = object
  [ "contract" .= contractVersion
  , "document" .= ("index" :: Text)
  , "repo"     .= key58 repo
  , "kind"     .= kindOf kind
  , "filter"   .= object [ "status" .= fStatus f, "label" .= fLabel f ]
  , "threads"  .= fmap one ts
    -- No paging yet: a whole list is one document. The field is here from the
    -- start so a reader that pages later need not treat it as optional.
  , "cursor"   .= (Nothing :: Maybe Text)
  ]
  where
    one (t, mmerge) = object $
      [ "number"     .= tsNumber t
      , "kind"       .= kindOf (tsKind t)
      , "thread_id"  .= b58 (tsId t)
      , "title"      .= (if tsRedacted t then Nothing else Just (tsTitle t))
      , "status"     .= statusOf t
      , "labels"     .= labelsOf t
      , "author"     .= key58 (tsAuthor t)
      , "comments"   .= length (tsComments t)
      , "created_at" .= tsCreated t
      , "updated_at" .= tsUpdated t
      , "redacted"   .= tsRedacted t
      ]
      <> [ "mergeability" .= mergeabilityContract mmerge | tsKind t == HubPR ]

indexOrder :: [Text]
indexOrder =
  -- One order for every level, so a key that occurs at two levels sits where it
  -- reads best in both: a thread's number before its kind, and the document's
  -- kind before its filter.
  [ "repo", "number", "kind", "filter", "threads", "cursor"
  , "thread_id", "title", "status", "label", "labels", "author", "comments"
  , "created_at", "updated_at", "redacted" ]
  <> ["mergeability", "state", "target", "head", "conflicts", "reason"]
