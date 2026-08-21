{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
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
  , emptyListing
  , listDoc
  , showDoc
  , logDoc
  , statusOf
  , listArgs
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
import HBS2.Hub.Repo.Git (withGitCanon,gitRun)
import HBS2.Hub.Canon (utf8Length,takeBytes)
import HBS2.Hub.Repo.GitBundle (validSha)
import HBS2.Hub.CLI.Argv (badArgs,flagsOf,flagsAndSwitches,flagSwitch,flagMaybe,flagText,flagWord
                         ,repoFlags,flagRepo,repoAndFlags)
import HBS2.Hub.CLI.Common (withCanon,OnMissing(..),refuse,saying,utcOf)
import HBS2.Hub.CLI.Verify (codeOf, refusalDoc)

import HBS2.CLI.Prelude hiding (null)
import HBS2.CLI.Run.Internal

import HBS2.Data.Types.Refs (pattern HashLike, HashRef)

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
import System.Exit (die,exitWith,ExitCode(..))

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
        (logArgs -> Just (repo, n)) -> lift $ withFold repo \fr -> do
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
          out (logDoc only fr)
        other -> liftIO (badArgs ("usage: hub log <repo-key> [<number>]"
                                    <> line <> "   or: hub log --repo <key> [--number <n>]")
                                 other)

  where

    listVerb name kind what =
      brief (fromString ("list this repository's folded " <> what))
        $ args [arg "string" "[--repo] repo-key"]
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
              lift $ withFold repo \fr -> do
                let ts = threadsOf kind f fr
                out (listDoc ts)
                -- On stderr and exit 0: an empty answer to a well-formed
                -- question is an answer, and this is advice about it.
                when (List.null ts) $
                  liftIO (saying (emptyListing kind f fr <> line))
            other -> liftIO (badArgs ( "usage: hub " <> pretty (spelled name)
                                         <> " <repo-key> [--status S] [--label L]"
                                         <> line <> "   or: hub " <> pretty (spelled name)
                                         <> " --repo <key> [--status S] [--label L]" )
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
                 <> line <> "one needs git and this verb needs nothing." )
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
      (t:_) -> case how of
                 AsDoc  -> out (showDoc t)
                 AsJson -> do
                   d <- diffOf t
                   liftIO (LBS.putStr (renderContract (threadContract t d)))
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
        r <- case diffArgv co of
               Nothing -> pure (Left ())
               Just as -> Right <$> gitRun Nothing [] 30 "the diff of a proposal" as mempty
        pure $ Just $ case r of
          Right (Right (ExitSuccess, o, _)) ->
            let txt = Text.decodeUtf8With Text.lenientDecode o
            in if utf8Length txt > maxDiffBytes
                 then PRDiff DiffAvailable True (takeBytes maxDiffBytes txt)
                 else PRDiff DiffAvailable False txt
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

-- | @<repo> [<n>]@, or the same behind flags.
logArgs :: forall c . IsContext c => [Syntax c] -> Maybe (HubKey, Maybe Word64)
logArgs syn = positional syn <|> flagged syn
  where
    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }

    positional = \case
      [ SignPubKeyLike repo ] -> Just (repo, Nothing)
      -- Through 'flagWord', which guards BOTH ends. A bare fromIntegral on the
      -- Integer a LitIntVal carries guards neither: flagWord's own haddock is
      -- the report of that bug -- `--number 18446744073709551617` wrapped to 1
      -- and the verb answered about a thread nobody named.
      [ SignPubKeyLike repo, (flagWord -> Just n) ] -> Just (repo, Just n)
      _ -> Nothing

    flagged s = do
      kvs <- flagsOf (repoFlags <> ["--number"]) s
      repo <- flagRepo asKey kvs
      n <- flagMaybe kvs "--number" flagWord
      pure (repo, n)
