{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
-- | Canon as a tree of files (PEP-19 "Tree layout").
--
-- Canon is an orphan git commit chain under @refs\/hbs2\/meta@ whose trees hold
-- one file per event. This turns such a tree into a 'FoldResult', and nothing
-- more: it decides nothing about the events, which is "HBS2.Hub.Fold"'s job.
--
-- Pure, and here rather than beside the git plumbing that feeds it, because the
-- writer belongs next to it: 'eventPath' is already in "HBS2.Hub.Bridge", and a
-- reader and a writer of one format on opposite sides of a package boundary is a
-- format with two owners. What talks to git is a 'CanonSource'.
--
-- A path is BYTES. A git tree path may hold any byte but NUL, so it is not text
-- in any encoding, and treating it as text lost real cases: two paths differing
-- only in an invalid byte decoded to one string and the second blob was read for
-- both, and a path with a non-ASCII byte failed to round-trip through a process
-- argument under the C locale, turning a healthy event file into an unreadable
-- one. Blobs are therefore fetched by object id, which is hex, and a path is only
-- ever a label to report.
--
-- Everything that can go wrong is a value. A tree that cannot be read is not an
-- empty tree, a version this build does not know is not a version it may fold
-- under, and a file it cannot parse is not a file that is not there.
module HBS2.Hub.Repo
  ( CanonSource(..)
  , CanonState(..)
  , CanonUnreadable(..)
  , FileProblem(..)
  , TreeEntry(..)
  , readCanon
  , readCanonAt
  , metaRef
  , sortCanon
  , maxCanonBytes
  , maxCanonFiles
  , maxListingBytes
  , versionPath
  , assumedMetaVersion
  , EntryKind(..)
  , BlobResult(..)
  , Told(..)
  , NameProblem(..)
  , told
  , planCanon
  , numberIndexOf
  , CanonCommit(..)
  , WriteRefused(..)
  , CanonSink(..)
  , CanonWrite(..)
  , CanonUnwritable(..)
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Canon
import HBS2.Hub.Fold

import HBS2.Prelude.Plated (Pretty(..),(<+>),Doc,nest,vsep,line)

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.List (sortOn)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Word (Word32,Word64)

-- | One entry a tree listing reported.
data TreeEntry = TreeEntry
  { teePath :: ByteString
  , teeKind :: EntryKind
  }
  deriving stock (Eq,Show)

-- | What a listing said an entry is.
--
-- Four cases, and every one of them was once folded into "not a blob". The one
-- that mattered: a listing gives a blob whose object is not present the size
-- @BAD@, not a number, so in a blobless or partial clone every event file read
-- as a submodule and the fetch that would have brought the object was never tried.
-- That is the partial-clone case this module has a name for, arriving under the
-- wrong one.
data EntryKind =
    -- | A blob that is here: object id, and size in bytes. The id is what a blob
    -- is fetched by, so a path never reaches a process argument; the size comes
    -- from the listing, so a bound can refuse a blob before anything pays for it.
    Blob Text Int
    -- | A blob whose object the listing could not size, which is how it says the
    -- object is not in this clone.
  | BlobMissing Text
    -- | Any other type: a submodule, or a tree. Something somebody put in canon.
  | NotABlob
    -- | The listing RECORD could not be read. Kept rather than dropped, and this
    -- is the one that is about this reader rather than about the tree: if the
    -- listing format ever shifts, every entry becomes this, which is a visible
    -- failure instead of a clean empty audit.
  | Unparsed
  deriving stock (Eq,Show)

-- | Where the canon files come from.
--
-- A record of functions rather than a git call, so the fold over a tree can be
-- exercised against a tree built in memory, and so the git-facing half can be
-- exercised on its own.
data CanonSource m = CanonSource
  { csCommit  :: m (Either CanonUnreadable Text)
    -- ^ what @refs\/hbs2\/meta@ points at
  , csEntries :: Text -> m (Either CanonUnreadable [TreeEntry])
    -- | One blob by object id. Not by path: see the note on paths above.
  , csBlob    :: Text -> m BlobResult
    -- | Release whatever the source is holding.
    --
    -- The git source keeps ONE @cat-file --batch@ for the whole walk, because a
    -- process per blob is what made a 172 KB tree cost 82 seconds, so there is
    -- something to close (a git, its three pipes and the thread reading its
    -- stderr) and a caller that forgets leaks all of it. 'withGitCanon' is the
    -- shape that does not forget; this field is what it calls.
  , csClose   :: m ()
  }

-- | What asking for one blob comes back as.
--
-- Three answers and not @Maybe@, because two of them are not about the blob at
-- all and were being reported as if they were. A fork that fails on EAGAIN under
-- a pids limit, or on EMFILE, is a local resource failure; folded into "no blob"
-- it became an unreadable FILE, which exits 2, the code PEP-22 defines as "the
-- audit ran and found something". It did not run. On the @version@ file the same
-- failure exited 7 and advised whoever published canon to rewrite it.
--
-- The distinction is not free to make and this is where it has to be made: only
-- the thing that ran the tool knows whether the tool ran.
data BlobResult =
    BlobText Text
    -- | The source ran and would not give it, with what it said. Not only "gone":
    -- a gc or a prune between two calls, a corrupt pack, EACCES on an object
    -- file, an ownership complaint. git exits 128 for all of those and this
    -- reader cannot tell them apart, so it keeps the words instead of choosing a
    -- story. Reported as 'FileUnreadable', which is the honest shape: what is
    -- broken is the read.
  | BlobRefused Text
    -- | The source could not run, or ran and stopped running. Says nothing about
    -- canon.
  | BlobUnavailable Text
    -- | The source ran, answered, and the answer is not one this reader can
    -- follow: a reply about an object it did not ask for, a type it did not ask
    -- for, a shape it cannot parse. Not a fact about any file, and not a local
    -- failure either. Reported as 'BlobsOutOfStep', whose advice is to report
    -- the git version.
    --
    -- Its own constructor because the two it borrowed both lied:
    -- 'BlobUnavailable' says the tool could not be run, about a git that had
    -- answered promptly, and 'TreeUnreadable' says the tree will not list, about
    -- a listing already read whole and parsed.
  | BlobProtocol Text
    -- | The source ran and stopped saying anything. Distinct from 'BlobProtocol'
    -- (it said something wrong) and from 'BlobUnavailable' (it could not be run),
    -- because those are the two answers a silent git used to get, and neither is
    -- true of it.
  | BlobStalled Text
    -- | The blob's announced size is larger than this reader will hold, carrying
    -- THE ANNOUNCED SIZE: nothing was read, which is the point of refusing on the
    -- announcement.
    --
    -- Reported as 'FileTooLarge', the same as the listing's own size column,
    -- because to whoever reads the report they are one fact: this file is too
    -- big. They are two different numbers, though, and that is why the read has
    -- a ceiling at all -- the column is the AUDITED TREE's word for it and a
    -- loose object can be self-consistent and lie, saying ten over two megabytes.
  | BlobOversize Int
    -- | The reader has spent as much as it will, and stopped. Usually about the
    -- WALK -- its clock, or the bytes it has handed back -- and sometimes about
    -- one object, when that object alone took longer than the reader will wait
    -- while never going quiet enough to be called stalled. The message says
    -- which; the answers differ, and the advice for the code names both.
  | BlobBudget Text
  deriving stock (Eq,Show)

-- | Why there is no canon to fold.
--
-- One type for the answers about the whole tree, because they are what a caller
-- branches on, and they were once several spellings of nothing: an unfetched ref,
-- a broken repository, an unlistable tree and canon from the future all arrived
-- as an empty fold with a zero exit.
data CanonUnreadable =
    -- | @refs\/hbs2\/meta@ is not here, and WHERE it is not: the absolute path of
    -- the repository this reader ended up in.
    --
    -- The path is not decoration. This verb obeys an inherited GIT_DIR on purpose,
    -- because a hook should audit the repository the hook was invoked for, and a
    -- hook is also where GIT_DIR is set to somewhere the operator is not standing.
    -- Without the path the message is "no refs\/hbs2\/meta in this repository", so
    -- they fetch canon into the tree they can see, run it again, and get the same
    -- sentence for ever.
    NoCanonRef ByteString
    -- | Not a git repository. Distinct from the above because the advice differs:
    -- fetching canon into a directory that is not a repository will not help.
    --
    -- Always GIT'S WORDS, whole, and so a plain 'Text' rather than a 'Told': git
    -- ran and exited non-zero to produce this. A git that could not be run is
    -- 'ReaderFailed', on the first call as much as on any later one -- it was
    -- this constructor on the first call, which is how "git is not installed"
    -- came out as a fact about somebody's repository, with advice about
    -- safe.directory.
  | NoRepository Text
    -- | The ref is here and does not resolve to a commit: a pruned object, a
    -- partial clone, or a ref pointing at something else. Distinct from a missing
    -- ref for the same reason, and it used to share its answer.
  | RefUnresolved Told
    -- | The commit is here and its tree cannot be listed, and WHO SAID SO.
    --
    -- The distinction is not decoration: the report prints a tool's words as an
    -- indented block with each line marked @|@, precisely so a stranger's text
    -- cannot be read as this program's instruction. Both messages came through
    -- that block, so "git ls-tree did not finish: it sent 77 bytes..." -- written
    -- here, about this reader's own bound -- was printed as though git had said
    -- it.
  | TreeUnreadable Told
    -- | The tree's @(hub-meta N)@ is newer than this build's rules. Folding
    -- anyway would produce a view under rules this build does not implement,
    -- which is how one clone quietly disagrees with every other one.
  | CanonTooNewHere Word32
    -- | The @version@ file is listed and could not be read as a version, with
    -- the reason. It governs the admission rules, so guessing is not available;
    -- and it is not an event, so nothing else in the report can mention it.
  | VersionUnreadable FileProblem
    -- | The event files total more than this reader will hold, or there are more
    -- of them than it will fetch. See 'maxCanonBytes'.
  | CanonTooBig Int
  | CanonTooMany Int
    -- | The LISTING is larger than this reader will read, which it finds out
    -- while reading it rather than after. Carries the bound, not the bytes seen:
    -- reading stops mid-chunk, so the count at that moment is an artefact of the
    -- chunk size and is neither the bound nor the size of the tree.
    --
    -- Its own constructor, not a 'TreeUnreadable': the other two bounds have
    -- their own codes precisely so a script knows which bound to argue with, and
    -- wrapped as a tree that will not list it also drew the advice for a pruned
    -- object, printed directly under a message about compaction.
  | CanonListingTooBig Int
    -- | Reading this tree cost more than the reader will spend: time on the walk,
    -- bytes handed back by it, or one object that took too long on its own.
    --
    -- A budget on the WALK, which no listing bound can give, because the listing
    -- of an expensive tree is small. The tree that made this necessary was 45000
    -- paths pointing at one shared subtree: five objects, 164 KB on disk, a 3.6 MB
    -- listing against a 102 MB bound and 45000 records against 200000. Every bound
    -- passed, and then the walk read it a path at a time.
    --
    -- That particular cost is gone -- blobs go through one batch process now, and
    -- the same tree reads in seconds -- and this is not a leftover. A bound and a
    -- fast path answer different questions: the fast path decides what a walk
    -- usually costs, and this decides what it may cost at worst, on a tree chosen
    -- by whoever is being audited, in a verb meant to run inside a pre-receive
    -- hook.
  | CanonTooSlow Text
    -- | The reader could not run the tool at all: no process slots, no file
    -- descriptors, the tool gone from PATH mid-audit. Nothing is known about
    -- canon, which is why this is not a finding about it.
    --
    -- NO ANSWER OUT OF GIT is the distinction from 'NoRepository', which is git
    -- having run and exited non-zero WITH SOMETHING TO SAY. Two shapes reach
    -- here, and narrowing the words to one of them was a mistake made once
    -- already: exec failing (git not on PATH, no process slots, no file
    -- descriptors) and a git that ran and then vanished mid-answer, which is what
    -- an OOM killer inside a hook's cgroup looks like from this end.
    --
    -- Not about WHEN, either: git absent at the first call is this, and so is a
    -- fork refused two hundred thousand blobs into a tree. The earlier rule was
    -- "the first call means it could be either", which held while the caller
    -- could not tell a process that never started from one that started and
    -- failed; it can now.
  | ReaderFailed Text
    -- | git RAN and did not answer. Not the same thing as 'ReaderFailed', which
    -- was the answer to both, and the difference is the whole of the advice: that
    -- one is documented as the single refusal worth retrying, and a retry against
    -- a git that is sitting there buys another minute and another stuck child.
    --
    -- Reachable without any exotica: a FIFO at @.git\/refs\/hbs2\/meta@ leaves
    -- @rev-parse@ answering and @show-ref@ blocked on open, which exited 12 after
    -- a minute with "no process slots, no file descriptors" and an invitation to
    -- try again.
    --
    -- The listing has had this distinction since it grew bounds of its own; these
    -- are the three small calls, which did not.
  | ToolStalled Text
    -- | The listing parsed and the walk over its files did not: git answered a
    -- request about an object nobody asked for, with a type nobody asked for, or
    -- with a size this reader will not read into an Int.
    --
    -- Its own constructor because the two it borrowed both lied. 'ReaderFailed'
    -- says the tool could not be run, about a git that answered promptly.
    -- 'TreeUnreadable' says the tree will not list, printed about a listing that
    -- had already been read whole and parsed, under advice recommending
    -- compaction, which has nothing to do with a batch reply going out of step.
  | BlobsOutOfStep Text
  deriving stock (Eq,Show)

-- | The tool's own words, as their own indented block rather than a value in a
-- one-line field, and MARKED as quoted.
--
-- The marker is not decoration. Advice from this program is printed at the same
-- indent directly underneath, so an unmarked block put a stranger's text in the
-- exact position of a line telling the reader what command to run. A tool's
-- complaint and this program's instruction have to be told apart at a glance.
--
-- git's complaints are multi-line on purpose: the remedy for a dubious-ownership
-- refusal is the safe.directory command on the LAST line, after a blank one and
-- indented with a tab (git's own template: "detected dubious ownership...", then
-- "To add an exception for this directory, call:", then the command). Rendered
-- as a field
-- value those newlines went through 'safeText' and came out as \\u{0a}, so the
-- fix arrived spelled as an escape in the middle of a sentence. The escaping is
-- right (one line of a report must stay one line); what was wrong is putting a
-- block where a line goes.
toolSaid :: Text -> Doc ann
toolSaid e
  -- Reachable: the stderr read is bounded in time, so a tool that exits without
  -- its message being collected leaves nothing here. A bare indent under "read
  -- the message above" is worse than saying there is none.
  | Text.null (Text.strip e) = " (it said nothing)"
  -- TAB is kept, alone among the control characters: the line structure is
  -- already decided here (one Doc per line of the message), so a tab cannot forge
  -- anything, and escaped it broke the one thing these blocks exist to carry.
  -- git's dubious-ownership refusal indents the safe.directory command with one,
  -- and it arrived as \\u{09}git config --global ..., which does not paste.
  | otherwise = nest 2 (line <> vsep [ "|" <+> pretty (safeWith (== '\t') l)
                                     | l <- Text.lines e ])

instance Pretty CanonUnreadable where
  pretty = \case
    -- Through pathText, because a git directory is a PATH: a byte in it that is
    -- not UTF-8 came out as U+FFFD, which is a name that does not exist and does
    -- not paste back into a shell, and that is the whole reason pathText is not
    -- decodeUtf8Lenient.
    NoCanonRef w      -> "no" <+> pretty metaRef <+> "in" <+> pretty (pathText w)
    NoRepository e    -> "cannot read this git repository:" <> toolSaid e
    RefUnresolved t   -> pretty metaRef <+> "does not resolve to a commit:" <> told t
    TreeUnreadable t  -> "cannot list the canon tree:" <> told t
    ToolStalled e     -> "git ran and did not answer:" <+> pretty (safeText e)
    BlobsOutOfStep e  -> "reading the canon files went out of step with git:"
                           <+> pretty (safeText e)
    CanonTooNewHere n -> "canon was folded under rules newer than this build:"
                           <+> "hub-meta" <+> pretty n
    -- ONE PREFIX THAT IS TRUE OF EVERY PAYLOAD, rather than one that is true of
    -- most and special cases for the rest. "Listed and does not read" was false
    -- three ways: of a version that parses and holds a number that is not a
    -- version, of a DIRECTORY where the file goes (nothing is listed at that
    -- path, which is the whole distinction), and of a listing record this build
    -- could not parse, which the advice underneath then corrected in the next
    -- sentence. What is true in all of them is that no version came out of it.
    VersionUnreadable p -> "the version file gave this reader no version:"
                             <+> pretty p
    CanonTooBig n     -> "the event files total" <+> pretty n
                           <+> "bytes, past what this reader will hold"
    CanonTooMany n    -> "there are" <+> pretty n
                           <+> "files in the canon tree, past what this reader"
                           <+> "will take in"
    CanonListingTooBig n -> "the canon tree listing is over" <+> pretty n
                              <+> "bytes, past what this reader will read"
    CanonTooSlow e    -> "this reader gave up on canon:" <+> pretty (safeText e)
    -- A SENTENCE, not a quoted block. Nothing here is ever git's: this
    -- constructor is for git not having run, so what it carries is a message
    -- written here, which names this reader before quoting whatever the runtime
    -- said about the failed exec. Printed through toolSaid, "git: startProcess:
    -- exec: does not exist" appeared under a | marker as though git had said it,
    -- which is the exact confusion the markers exist to prevent, on the first
    -- refusal a new user meets.
    --
    -- The quoted half is the runtime's and differs between platforms -- that
    -- same text is "startProcess: find_executable: failed (Unknown error: -2)"
    -- on aarch64-osx -- so it is evidence for a human and never something to
    -- assert on.
    ReaderFailed e    -> "this reader got no answer out of git:" <+> pretty (safeText e)

-- | Whose words a message is.
--
-- On every constructor that can carry either, which is every one whose payload
-- sometimes comes from git's stderr and sometimes from this reader. Three of them
-- were left as bare 'Text' when this type was added and went on printing this
-- reader's own sentences inside the block that means "a stranger said this".
data Told =
    ToolSaid Text     -- ^ git's, printed as a quoted block
  | ReaderSays Text   -- ^ this program's, printed as its own sentence
  deriving stock (Eq,Show)

-- | A 'Told' as the report prints it: a block for the tool, a sentence for us.
told :: Told -> Doc ann
told = \case
  ToolSaid e   -> toolSaid e
  ReaderSays e -> " " <> pretty (safeText e)

-- | Why one file in the tree did not become an event.
data FileProblem =
    FileMalformed CanonError  -- ^ read, and not an event
    -- | Listed and its blob will not read, with what the source said. Not the same
    -- as malformed, and reported all the same: what is broken is the read, not
    -- the file. The message is kept because the causes range from a pruned object
    -- to a permission error, and the difference is the whole of what to do next.
  | FileUnreadable Text
    -- | Bigger than 'maxEventBytes', refused from the listing unfetched.
  | FileTooLarge Int
    -- | Listed and not a blob: a submodule. Named rather than filtered, because
    -- something under @threads\/@ that is not an event file is something somebody
    -- put in canon.
    --
    -- A symlink is NOT this: git records one as a blob holding its target, so it
    -- arrives here as a very small event file and is refused as malformed. That
    -- is the right answer and it is worth knowing it is not this one.
  | FileNotABlob
    -- | A blob the listing could not size, which git spells BAD.
    --
    -- USUALLY the object is not in this clone (a blobless or partial clone, or a
    -- pruned object), and then fetching is the answer. Not always: a corrupt zlib
    -- stream and an object file this user may not read both print BAD too, with
    -- git's reason on a stderr that this reader threw away because ls-tree still
    -- exited zero. So the wording hedges, and the advice with it.
  | FileObjectMissing
    -- | The listing record itself could not be read. See 'Unparsed'.
  | FileListingUnparsed
    -- | The path is a DIRECTORY in the tree. Its own problem, because ls-tree -r
    -- recurses: a tree at @version@ produces @version\/x@ entries and no
    -- @version@ entry at all, so the reader saw "no version file" and printed it,
    -- and when
    -- that was fixed the nearest constructor to hand said "not a blob: a
    -- submodule", sending somebody to look for a gitlink that is not there.
  | FileIsADirectory
    -- | Somewhere the tree layout does not put files. PEP-19 fixes the layout, so
    -- a path outside it is somebody's addition, and a reader that only looked
    -- under the two event directories could not see it at all.
  | FileUnexpected
    -- | The tree lists this path more than once, which git fsck calls
    -- duplicateEntries and which a hand-built tree can carry.
    --
    -- It matters most for the version file: taking the head of the list meant the
    -- rules canon is folded under were chosen by the ORDER of entries in somebody
    -- else's tree, silently, with the audit exiting zero. Everywhere else in this
    -- package an ambiguity is refused rather than resolved, and this is that.
  | FileDuplicated
    -- | The file reads, parses, and declares a version that is not one: today
    -- only zero, since anything above this build's is 'CanonTooNewHere'.
    --
    -- Not 'FileMalformed': the clause is spelled correctly and the file is a
    -- perfectly good s-expression, so "malformed clause hub-meta" sent whoever
    -- read it looking for a syntax error that is not there. What is wrong is the
    -- number, and only the number.
  | FileBadVersion Word32
  deriving stock (Eq,Ord,Show)

instance Pretty FileProblem where
  pretty = \case
    FileMalformed e     -> pretty e
    -- Escaped onto ONE line, not blocked like the refusals above: this is a field
    -- in a per-file line of the report, and 'reportDoc' promises one Doc per
    -- line. A block here put a paragraph in the middle of a list.
    FileUnreadable e    -> "listed in the tree and its blob does not read:"
                             <+> pretty (safeText e)
    FileTooLarge n      -> "over the bound this reader will read:" <+> pretty n
                             <+> "bytes"
    FileNotABlob        -> "not a blob: a submodule, in a canon tree"
    FileObjectMissing   -> "the listing could not size this object: absent from"
                             <+> "this clone, corrupt, or unreadable"
    FileListingUnparsed -> "this reader could not parse the tree listing record"
    FileIsADirectory    -> "a directory in the tree, where a file belongs"
    FileUnexpected      -> "not a path the canon tree layout has"
    FileDuplicated      -> "listed more than once in the tree"
    FileBadVersion n    -> "declares hub-meta" <+> pretty n <> ", and there is no"
                             <+> "version" <+> pretty n

-- | How an event file's name disagrees with the event in it.
--
-- PEP-19 "Tree layout" makes the name normative: twenty digits of @seq@, a
-- hyphen, then the event-id. Nothing checked it, so a file could be named for one
-- event and hold another, and a tool that trusts names (a hook grepping the tree,
-- a human reading @ls@) would be looking at a different event from the one the
-- fold admits.
--
-- REPORTED, NEVER OBEYED, like the per-file version clause: the fold works off
-- signed content and never reads a path, so dropping a misnamed file would make
-- this clone show less than canon holds over a byte no signature covers. What is
-- wrong with the tree is said, and the tree is still folded.
--
-- Two things are checked here: the shape, and the event-id against the file's own
-- content. THREE are not -- @seq@, the scope (a repo-scope event under
-- @threads\/@), and the thread directory a file sits in -- and the reason is cost
-- rather than reach. They are ordinary fields of the canon box, and opening one
-- verifies a signature the fold verifies again a moment later, so checking them
-- here means verifying canon twice in a verb built for a pre-receive hook. The
-- place that has already paid is the fold, and the fold is not shown a path.
--
-- An earlier version of this note said the canon box is one "only the fold
-- opens", which is not true of this tree: 'unboxChecked' is exported and is
-- already called elsewhere. PEP-22 has the corrected version.
data NameProblem =
    -- | Not @\<20 digits>-\<something>@ at all.
    NameNotEventShaped
    -- | Shaped right and naming another event, with the id the file holds.
  | NameWrongEvent EventId
  deriving stock (Eq,Show)

instance Pretty NameProblem where
  pretty = \case
    NameNotEventShaped -> "not named as PEP-19 names an event file:"
                            <+> "20 digits of seq, a hyphen, the event-id"
    NameWrongEvent e   -> "named for another event; this file holds" <+> pretty e

-- | What reading canon produced.
data CanonState = CanonState
  { stCommit  :: Text
    -- | Every event the tree held, with the path it held it at.
    --
    -- The fold below is what a reader wants; this is what a REWRITE wants, and
    -- nothing else asks for it: a compaction keeps a subset of these files and
    -- has to put them back under the names they had.
  , stEvents  :: [(ByteString, Event)]
  , stVersion :: Maybe Word32
    -- ^ the tree's own rules version, absent when the tree has no @version@ file
  , stBad     :: [(ByteString, FileProblem)]
  , stMisnamed :: [(ByteString, NameProblem)]
    -- ^ files whose name is not the one PEP-19 gives them, reported and folded
  , stFileVersions :: [(ByteString, Maybe Word32)]
    -- ^ what each file declared, reported and never obeyed (PEP-19)
  , stFold    :: FoldResult
  }

-- | The ref canon lives under. One definition, because a second spelling of it
-- somewhere else is a clone that reads a different tracker.
metaRef :: Text
metaRef = "refs/hbs2/meta"

-- | The tree's own rules version, by path.
versionPath :: ByteString
versionPath = "version"

-- | What version a tree with no version file is folded as.
--
-- The OLDEST this build knows, not the newest. Today they are the same number,
-- and the day hub-meta becomes 2 they stop being: taken as "whatever this build
-- implements", deleting one unsigned file would be a way to have canon folded
-- under newer rules. A tree that does not say cannot claim newer.
--
-- What this number DOES, exactly, is decide the gate: it is compared against
-- 'hubMetaVersion' and nothing else looks at it. The fold itself does not take a
-- version and does not vary with one, so this is not yet a choice of rules; it is
-- the placeholder that becomes one the first time the rules fork, and the value
-- is chosen now so that the fork does not have to remember to choose it.
assumedMetaVersion :: Word32
assumedMetaVersion = 1

-- | How much of a tree this reader will take in, and how many files.
--
-- Three bounds, because they bound three different costs.
--
-- 'maxCanonBytes' bounds what the reader HOLDS: the fold sorts the whole log, so
-- every admitted event is resident at once, and a per-file bound does not imply a
-- total one. It is measured over the event files only, and only over those whose
-- size the listing gave: a README somebody committed next to canon is not
-- something this reader fetches, so counting it would refuse an audit over a file
-- the audit does not read. It is checked before a byte is fetched, which is what
-- makes it a bound on the reader rather than a note about it.
--
-- 'maxCanonFiles' bounds the LISTING, every entry of it, and not the event files
-- alone. Each entry is parsed, sorted and possibly printed whether or not this
-- reader would fetch it, and each entry's path is a slice of one listing buffer,
-- so holding any one of them holds the whole. It is checked before the entries
-- exist, on the bytes, for that reason.
--
-- 'maxListingBytes' bounds what reading that listing PEAKS at. Necessary because
-- the other two are computed from the listing and so cannot refuse it, and
-- sufficient only if it is enforced while the bytes arrive; see 'HBS2.Hub.Repo.Git'.
-- 512 bytes an entry is about twice the width of a canon path. It is a bound on
-- MEMORY and not a stand-in for the file count: a listing of minimal 63-byte
-- records fits 1.6M of them under it, eight times 'maxCanonFiles', so the count
-- has to be enforced on the raw bytes before any entry is built. The git source
-- does that by counting record terminators; 'readCanon' keeps its own count over
-- the parsed entries as the backstop for a source that does not.
maxCanonBytes :: Int
maxCanonBytes = 256 * 1024 * 1024

maxCanonFiles :: Int
maxCanonFiles = 200000

maxListingBytes :: Int
maxListingBytes = maxCanonFiles * 512

-- | Split a listing into the event files to read and the entries to refuse.
--
-- PEP-19 fixes the tree layout, so every path is one of four things: the version
-- file, the number index, an event under @threads\/@ or @repo\/@, or somebody's
-- addition. The last is reported. A reader that looked only under the two event
-- directories was blind to the rest of the tree, which is the reader that lets a
-- tree carry things it pretends not to see.
-- A path listed twice is reported once, as well as being handled: git fsck calls
-- it duplicateEntries, a hand-built tree can carry it, and nothing downstream can
-- see it, since every later step works one entry at a time.
sortCanon :: [TreeEntry] -> ([(ByteString, Text, Int)], [(ByteString, FileProblem)])
sortCanon entries = (evs, dups <> bad)
  where
    (evs, bad) = go [] [] (dedup HS.empty entries)

    -- Every path with more than one entry, and what those entries said. Built
    -- once, in one pass, and then used for both the report and the dedup: an
    -- earlier version walked a growing list with elem, which is O(n^2) and was
    -- measured at 0.3 s for 5000 files, 4.6 s for 20000 and 20 s for 40000 --
    -- about eight minutes of CPU at maxCanonFiles, before a single blob is read,
    -- on a tree this reader calls acceptable.
    listed :: HM.HashMap ByteString [EntryKind]
    listed = HM.fromListWith (<>) [ (teePath e, [teeKind e]) | e <- entries ]

    duplicated = HM.filter (\ks -> length ks > 1) listed

    dups = sortOn fst [ (p, FileDuplicated) | p <- HM.keys duplicated ]

    -- A path listed twice with DIFFERENT entries is dropped entirely, and listed
    -- twice with the same entry collapses to one.
    --
    -- Keeping the first was resolving the ambiguity, which is what this package
    -- refuses to do everywhere else: with two different object ids, the ORDER of
    -- entries in somebody else's tree chose which of two signed events the fold
    -- would see, and the other was never read, never folded and never named. The
    -- version file is refused outright for exactly this, and the comment here
    -- used to claim event files followed the same rule while the code did not.
    conflicting = HS.fromList
      [ p | (p, ks) <- HM.toList duplicated, not (allSame ks) ]
      where allSame ks = all (== head ks) ks

    dedup _ [] = []
    dedup seen (e:rest)
      | teePath e `HS.member` conflicting = dedup seen rest
      | teePath e `HS.member` seen        = dedup seen rest
      | otherwise = e : dedup (HS.insert (teePath e) seen) rest

    go es bs [] = (reverse es, reverse bs)
    go es bs (e:rest) = case teeKind e of
      -- Before the layout question, because a record nobody could read has no
      -- reliable path to ask it about.
      Unparsed -> go es ((teePath e, FileListingUnparsed) : bs) rest
      kind
        | not (isEvent (teePath e)) ->
            -- The version file and the index have their own readers; anything
            -- else outside the layout is somebody's addition and is reported.
            --
            -- Skipped only when it IS a blob. A gitlink at index/number.sexp was
            -- invisible while the same gitlink under threads/ was reported: the
            -- skip was by path and did not look at what the entry is, so the one
            -- path in the tree that nothing else reads was also the one place a
            -- submodule could sit unnamed.
            if teePath e `elem` [versionPath, B8.pack numberIndexPath]
              then case kind of
                     Blob{} -> go es bs rest
                     _      -> go es ((teePath e, problemOf kind) : bs) rest
              else go es ((teePath e, FileUnexpected) : bs) rest
        | otherwise -> case kind of
            Blob oid n
              | n > maxEventBytes -> go es ((teePath e, FileTooLarge n) : bs) rest
              | otherwise -> go ((teePath e, oid, n) : es) bs rest
            _ -> go es ((teePath e, problemOf kind) : bs) rest

    -- What a non-blob entry is a problem of. Total, and Blob is in it so that a
    -- constructor added to EntryKind is a build failure here rather than a
    -- wildcard that quietly calls it a submodule, which is how the missing-object
    -- case spent a round being reported as one.
    problemOf = \case
      NotABlob      -> FileNotABlob
      BlobMissing _ -> FileObjectMissing
      Unparsed      -> FileListingUnparsed
      Blob{}        -> FileNotABlob

    -- A path under threads/ must name a thread directory and a file in it, and a
    -- path under repo/ a file. A bare prefix match folded /x@ as an event
    -- and then reported it as malformed, where it is really a path the layout does
    -- not have.
    isEvent p = under "threads/" 2 p || under "repo/" 1 p
      where
        under pfx depth q =
          B8.pack pfx `B8.isPrefixOf` q
            && ( let cs = Prelude.filter (not . B8.null)
                            (B8.split '/' (B8.drop (length pfx) q))
                 in length cs == depth && all plain cs )

        -- A COMPONENT IS A NAME, and @.@ and @..@ are not names.
        --
        -- Counting components was not enough: @threads/../x@ has two of them
        -- and is not a path this layout has. What made that worth refusing
        -- rather than reporting is the WRITE side -- a compaction puts back the
        -- names the tree already had, and git's index answers a path like that
        -- with "Ignoring path" and exit zero, so the event was dropped from the
        -- rewrite and the verb said it had succeeded. That half is closed in
        -- "HBS2.Hub.Repo.GitWrite" too, and independently: this one stops such
        -- a file being an event, that one stops any planned file going missing
        -- for any reason.
        --
        -- Not folded is not ignored: a path under these prefixes that is not an
        -- event is reported by the reader like any other file it will not take.
        plain c = c /= "." && c /= ".."

-- | Read canon and fold it.
--
-- The repository key is passed in rather than taken from the tree, because the
-- tree cannot be trusted to say whose it is: the owner key is the root of the
-- trust chain (PEP-19 rule 3), so canon that named its own owner would be canon
-- that could rename it.
readCanon :: Monad m
          => CanonSource m -> RepoRef -> m (Either CanonUnreadable CanonState)
readCanon cs owner = csCommit cs >>= \case
  Left e -> pure (Left e)
  Right commit -> readCanonAt cs owner commit

-- | The same, over a commit the caller names rather than the one the ref holds.
--
-- Split out when a second lineage had to be folded: `hub sync` fetches what a
-- remote published into FETCH_HEAD and has to know whether it materializes the
-- same state before it moves the ref onto it (PEP-19 compaction). Everything
-- below is what 'readCanon' always did; only where the commit comes from is
-- new, and it is deliberately NOT re-read from the ref, since the whole point
-- is to fold something the ref does not point at.
readCanonAt :: Monad m
            => CanonSource m -> RepoRef -> Text
            -> m (Either CanonUnreadable CanonState)
readCanonAt cs owner commit = csEntries cs commit >>= \case
    Left e -> pure (Left e)
    -- The COUNT below is over the whole listing, and the bytes over the event
    -- files. Two different costs: every entry becomes a TreeEntry, is sorted and
    -- is printed, so ten million paths outside the layout cost ten million lines
    -- whether or not this reader would fetch any of them, and the paths are
    -- slices of one listing buffer, so holding any of them holds all of it. The
    -- bytes are what the reader will HOLD, which only the event files are.
    Right entries -> do
        -- THE VERSION FIRST, ahead of both bounds on size, because the two
        -- answers are not equal in rank. A bound says "this canon is bigger than
        -- this reader will take, go and compact it", which is work for whoever
        -- owns the tree; the version says "this build cannot fold this tree at
        -- all, upgrade", which is work for whoever is reading it and is true
        -- whatever the size. Measured the other way round: a 300 MB tree stamped
        -- (hub-meta 99) was answered with code 8 and advice to compact, and
        -- compacting it would have produced a small tree this build still cannot
        -- read.
        --
        -- It costs one linear scan of a listing already in memory, plus one blob
        -- bounded by maxEventBytes. The COUNT bound still runs before 'sortCanon'
        -- below, which is the expensive pass and the one it exists to protect.
        --
        -- The version decides whether the rest may be folded at all, so its
        -- absence and its unreadability are different answers. The listing is
        -- what tells them apart: a file the tree does not list is absent, and one
        -- it lists whose blob will not read is a version this reader does not
        -- have. Before the listing was consulted, a pruned version blob read as
        -- "no version file" and the tree was folded under this build's rules,
        -- which is the gate below being skipped in silence.
        -- More than one is refused, not resolved. Taking the head let the ORDER
        -- of entries in somebody else's tree choose the rules canon is folded
        -- under, with nothing said and a zero exit.
        ver <- case [ teeKind e | e <- entries, teePath e == versionPath ] of
                 -- A DIRECTORY where the version file goes is not "no version
                 -- file": ls-tree -r recurses, so a tree there yields version/x
                 -- and no version at all, and one nothing wore the other's face.
                 -- Its children are reported as unexpected paths either way; this
                 -- says which nothing it is.
                 [] | any ((B8.pack "version/" `B8.isPrefixOf`) . teePath) entries ->
                        pure (Left (Right FileIsADirectory))
                 [] -> pure (Right Nothing)
                 -- Two entries for one path. Identical ones collapse, as they do
                 -- for events: it is one file listed twice, and the tree is
                 -- malformed either way (reported by sortCanon). Different ones
                 -- are refused, because choosing would let the order of entries
                 -- pick the rules canon is folded under.
                 (k : ks) | not (all (== k) ks) -> pure (Left (Right FileDuplicated))
                 (NotABlob : _)      -> pure (Left (Right FileNotABlob))
                 (Unparsed : _)      -> pure (Left (Right FileListingUnparsed))
                 (BlobMissing _ : _) -> pure (Left (Right FileObjectMissing))
                 (Blob oid n : _)
                   | n > maxEventBytes -> pure (Left (Right (FileTooLarge n)))
                   | otherwise -> csBlob cs oid >>= \case
                       BlobUnavailable e -> pure (Left (Left (ReaderFailed e)))
                       BlobStalled e -> pure (Left (Left (ToolStalled e)))
                       BlobProtocol e -> pure (Left (Left (BlobsOutOfStep e)))
                       BlobBudget e -> pure (Left (Left (CanonTooSlow e)))
                       BlobOversize big -> pure (Left (Right (FileTooLarge big)))
                       BlobRefused e -> pure (Left (Right (FileUnreadable e)))
                       BlobText t -> pure (either (Left . Right . FileMalformed)
                                                  (Right . Just)
                                                  (parseMeta t))

        case ver of
          -- A tool that would not run is not a version file that will not read.
          Left (Left u) -> pure (Left u)
          Left (Right p) -> pure (Left (VersionUnreadable p))
          -- Zero is not a version this or any build implements, and it read as
          -- one: the gate only ever looked for NEWER, so (hub-meta 0) folded
          -- silently under today's rules with a clean report.
          Right (Just 0) -> pure (Left (VersionUnreadable (FileBadVersion 0)))
          -- BEFORE the blobs, not after them. The gate lives inside foldCanon,
          -- which runs last, so a tree stamped newer than this build was read in
          -- full first: measured at one cat-file per event, all of it thrown away.
          -- Worse, a fork failing anywhere in that loop turned the answer into
          -- ReaderFailed and "nothing was learned about canon", when the version
          -- file had been read and the answer was already known.
          Right (Just n) | n > hubMetaVersion -> pure (Left (CanonTooNewHere n))
          -- Now the counting, and only now: see the note over the version read.
          _ | length entries > maxCanonFiles ->
                pure (Left (CanonTooMany (length entries)))
          Right declared -> do
           let (evEntries, refused) = sortCanon entries
               bytes = sum [ n | (_,_,n) <- evEntries ]
           if bytes > maxCanonBytes then pure (Left (CanonTooBig bytes)) else do

            -- Parsed inside the loop, so a file's TEXT dies as soon as it has
            -- become an event or a problem. Binding the texts first and using
            -- that list twice kept all of them alive at once, which made the peak
            -- the size of canon rather than of its largest file. What survives is
            -- the events, which the fold needs all of at once, since it sorts.
            -- Left the moment the tool stops running, and not a file-shaped
            -- problem per remaining file: 200000 failed forks are one local
            -- failure, and a report listing them all as unreadable files is a
            -- report about canon that is not about canon.
            readAll <- foldM readOne (Right ([],[],[],[])) evEntries

            case readAll of
             Left u -> pure (Left u)
             Right (evs, vers, bad2, mis) -> do
              let bad = sortOn fst (refused <> bad2)

              -- foldCanon and not foldEvents: the tree's version governs the
              -- admission rules, which is the whole reason it is a tree-level file.
              pure $ case foldCanon (maybe assumedMetaVersion id declared) owner (fmap snd evs) of
                Left (MetaTooNew n) -> Left (CanonTooNewHere n)
                Right fr -> Right CanonState
                  { stCommit  = commit
                  , stEvents  = reverse evs
                  , stVersion = declared
                  , stBad     = bad
                  , stFileVersions = sortOn fst vers
                  , stMisnamed = sortOn fst mis
                  , stFold    = fr
                  }

  where
    -- The leaf of a path against the name PEP-19 gives this event. See
    -- 'NameProblem' for what is checked and what cannot be.
    nameProblem p ev
      | not shaped = Just NameNotEventShaped
      | named /= mine = Just (NameWrongEvent (eventId ev))
      | otherwise = Nothing
      where
        -- The last component. Safe on any path this reaches: isEvent has already
        -- said it has two components under threads/ or one under repo/.
        base = B8.takeWhileEnd (/= '/') p
        (digits, rest) = B8.splitAt 20 base
        shaped = B8.length digits == 20
                   && B8.all (\c -> c >= '0' && c <= '9') digits
                   && not (B8.null rest) && B8.head rest == '-'
        named = B8.drop 1 rest
        mine = B8.pack (show (pretty (eventId ev)))

    readOne acc@(Left _) _ = pure acc
    readOne (Right (evs, vers, bad, mis)) (p, oid, _) = csBlob cs oid >>= \case
      BlobUnavailable e -> pure (Left (ReaderFailed e))
      -- The ones that are NOT about this file end the walk: a budget spent is
      -- spent, and a source that has stopped answering or stopped making sense
      -- will not do better on the next object. The two that are about it do not:
      -- an oversized file and an unreadable one are findings, and a report that
      -- stops at the first of them is a report about one file.
      BlobStalled e -> pure (Left (ToolStalled e))
      BlobProtocol e -> pure (Left (BlobsOutOfStep e))
      BlobBudget e -> pure (Left (CanonTooSlow e))
      BlobOversize n -> pure (Right (evs, vers, (p, FileTooLarge n) : bad, mis))
      BlobRefused e -> pure (Right (evs, vers, (p, FileUnreadable e) : bad, mis))
      BlobText t -> pure $ case parseEvent t of
        Left e        -> Right (evs, vers, (p, FileMalformed e) : bad, mis)
        -- REPORTED AND STILL FOLDED, like the per-file version clause above it.
        -- The name is normative (PEP-19 "Tree layout") and nothing checked it, so
        -- a file could name one event and hold another; but the fold works off
        -- signed content and never looks at a path, so dropping the event here
        -- would make this reader show less than canon holds and disagree with
        -- every other clone over a byte no signature covers.
        -- The name check is FORCED here, not left as a thunk in the accumulator.
        -- It hashes the author box, which is a full serialisation of somebody's
        -- event, and three of the four accumulators are already strict in their
        -- spine; leaving this one lazy piles two hundred thousand of those up to
        -- be paid at once when the report is printed, which is after every budget
        -- has stopped applying.
        --
        -- It is still one hash per admitted event, and the fold computes the same
        -- one for its sort key, so the walk hashes canon twice. Bounded by the
        -- same maxCanonBytes the read is, and worth saying rather than hiding.
        Right (v, ev) ->
          let !mis' = maybe mis (\np -> (p, np) : mis) (nameProblem p ev)
          -- WITH ITS PATH, because a rewrite has to put it back where it was.
          -- Deriving the path from the event instead would silently repair a
          -- misnamed file, which is a finding this reader reports and not a
          -- thing a compaction is entitled to fix.
          in Right ((p, ev) : evs, (p, v) : vers, bad, mis')

-- | What a commit to canon must put in the tree.
--
-- Files only. A commit message, a parent and a ref are git's business and are
-- the sink's; what canon IS stops here, which is the same line 'readCanon'
-- draws from the other side.
data CanonCommit = CanonCommit
  { cwFiles :: [(ByteString, ByteString)]
    -- ^ path to contents, in path order, no duplicates
  , cwIndexOmitted :: Int
    -- | How many numbers @index\/number.sexp@ could not hold.
    --
    -- Not an error and not silent. The index is a convenience map that PEP-19
    -- says is regenerable from the @open@ events and never trusted over them,
    -- so a writer that reaches its bound truncates rather than refusing the
    -- commit: a prefix is still correct for every number in it, and refusing
    -- an index would be refusing the repository. But a cap nobody is told
    -- about reads as "everything fit", so the count comes out and the caller
    -- says it.
  }
  deriving stock (Eq,Show)

-- | Why a set of events could not be turned into a commit.
data WriteRefused =
    -- | The file rendered, and this build could not read it back.
    --
    -- Refused rather than written. The reader's bounds are derived from the
    -- ones the bridge mints under ('maxEventBytes' explains the derivation),
    -- so this should be unreachable, and that is exactly why it is checked:
    -- an unreachable case that becomes reachable writes canon this build folds
    -- with one event missing, in every clone, permanently. A refused commit
    -- costs an operator a message.
    EventUnwritable FilePath CanonError
    -- | The file read back as a different event than the one written.
    --
    -- Cannot happen through 'renderEvent', which puts both boxes in the file
    -- verbatim; it is here because the alternative to checking is trusting,
    -- and what is trusted is a projection layer that already truncates.
  | EventNotItself FilePath EventId EventId
    -- | Two events want one path, so writing both would keep one.
    --
    -- The path is @seq@ plus event-id, so a collision means the caller minted
    -- one author box twice at one @seq@, which the bridge refuses as
    -- @AlreadyInCanon@. A batch assembled by hand can still do it.
  | PathCollision FilePath
  deriving stock (Eq,Show)

instance Pretty WriteRefused where
  pretty = \case
    EventUnwritable p e ->
      nest 2 $ vsep [ "cannot write" <+> pretty (pathText (B8.pack p))
                        <+> ": this build could not read back what it rendered"
                    , pretty e ]
    EventNotItself p want got ->
      nest 2 $ vsep [ "cannot write" <+> pretty (pathText (B8.pack p))
                        <+> ": the file reads back as another event"
                    , "wanted" <+> pretty want
                    , "got" <+> pretty got ]
    PathCollision p ->
      "two events claim" <+> pretty (pathText (B8.pack p))
        <+> ": writing both would keep one"

-- | The number map PEP-19 puts in @index\/number.sexp@, from a fold.
--
-- Derived from the fold and never from a previous index, because the index is
-- the one file in the tree that no signature covers: regenerating it is what
-- keeps it from drifting, and PEP-19 requires exactly that.
numberIndexOf :: FoldResult -> [(Word64, ThreadId)]
numberIndexOf fr =
  sortOn fst [ (n, t) | (t, ts) <- HM.toList (frThreads fr), Just n <- [tsNumber ts] ]

-- | The files that put these events in canon (PEP-19 "Tree layout").
--
-- The paths come from 'HBS2.Hub.Bridge.eventPath', which is where the layout
-- is decided; this renders, checks and assembles. Taking a path rather than an
-- 'HBS2.Hub.Bridge.Accepted' keeps the format's two halves free of the triage
-- layer, which neither of them needs to know about.
--
-- Every commit rewrites @version@ and @index\/number.sexp@ whole and adds one
-- file per event. The two rewritten files are content-stable, so git stores
-- one object for them however many commits repeat them, and the event files
-- are immutable by construction; this is the incremental update PEP-19
-- describes, spelled as a whole-tree write because the caller then hands the
-- sink a complete set and does not have to reason about what it is replacing.
--
-- Each file is READ BACK before it is accepted, with the reader this package
-- ships. Being able to write what cannot be read is the failure that costs
-- most here (canon is in every clone and cannot be un-published), the reader
-- has three bounds of its own that a size check would not cover, and the cost
-- is one parse per event written, which is one event per accept.
planCanon :: [(FilePath, Event)]
          -> [(Word64, ThreadId)]
          -> Either WriteRefused CanonCommit
planCanon evs numbers = do
  files <- traverse one evs
  case duplicates (fmap fst files) of
    (p:_) -> Left (PathCollision p)
    []    -> pure ()
  let idx = renderNumberIndex numbers
      -- What the index HELD, against what it was given. renderNumberIndex
      -- truncates silently and returns text; counting its lines is the only
      -- way back to the number, and it is cheaper than the render was.
      held = length (filter (not . Text.null) (Text.lines idx))
  pure CanonCommit
    { cwFiles = sortOn fst
        (  [ (versionPath, TE.encodeUtf8 renderMeta)
           , (B8.pack numberIndexPath, TE.encodeUtf8 idx) ]
        <> fmap (\(p,t) -> (B8.pack p, t)) files )
    , cwIndexOmitted = max 0 (length numbers - held)
    }
  where
    one (p, ev) =
      let txt = renderEvent ev
      in case parseEvent txt of
           Left e -> Left (EventUnwritable p e)
           Right (_, back)
             | eventId back /= eventId ev ->
                 Left (EventNotItself p (eventId ev) (eventId back))
             | otherwise -> Right (p, TE.encodeUtf8 txt)

    -- Over a sorted list of paths, so this is linear and does not need Ord on
    -- anything the rest of the module lacks it for.
    duplicates ps = [ a | (a,b) <- zip s (drop 1 s), a == b ]
      where s = sortOn id ps

-- | Where a canon commit goes.
--
-- The mirror of 'CanonSource', and a record of functions for the same reason:
-- what a commit IS can then be exercised without git, and the git-facing half
-- without the format.
--
-- Two operations and not three. Reading the ref and writing the commit are
-- separate because the caller needs the parent long before it has anything to
-- write (it folds canon first, and mints from that fold), but moving the ref
-- is NOT separate from writing: the parent read at the start is the value the
-- move must compare against, and a caller holding the two apart is a caller
-- who can forget to.
data CanonSink m = CanonSink
  { skParent :: m (Either CanonUnwritable (Maybe Text))
    -- ^ what @refs\/hbs2\/meta@ points at, 'Nothing' when canon does not exist yet
  , skCommit :: CanonWrite -> m (Either CanonUnwritable Text)
    -- | The same, writing a ROOT commit: the tree is exactly the files given
    -- and the history before it is gone.
    --
    -- Its own method rather than a flag, because the two are different acts and
    -- the type should say which one a caller performs. Everything else is
    -- identical, 'cnParent' included -- it still names what the ref must hold,
    -- since a rewrite that landed on a canon somebody has moved under it would
    -- drop whatever they folded.
    --
    -- This is what a compaction publishes (PEP-19): a new lineage holding the
    -- retained events, replacing one whose history is what it traded for size.
  , skRewrite :: CanonWrite -> m (Either CanonUnwritable Text)
    -- ^ write the files, commit them, move the ref; the new commit
  , skClose  :: m ()
  }

-- | One commit to canon.
data CanonWrite = CanonWrite
  { cnParent  :: Maybe Text
    -- | The commit's parent AND the value the ref must still have.
    --
    -- One field for both because they are one fact: canon is a chain, so
    -- committing onto a parent that is no longer the tip would fork it, and
    -- the fork that loses is a triage decision the operator already made.
  , cnFiles   :: [(ByteString, ByteString)]
  , cnMessage :: Text
  , cnWhen    :: Word64
    -- | MILLISECONDS, the unit everything in this package stamps in.
    --
    -- Used for the commit's author and committer dates, so the commit id is a
    -- function of what canon holds and not of when the writer ran: folding the
    -- same event at the same stamp twice produces the same commit, which is
    -- what makes a retry after a lost answer cheap instead of forking history.
  }
  deriving stock (Eq,Show)

-- | Why canon could not be written.
--
-- The same four-way split 'BlobResult' draws, for the same reason: a git that
-- could not start says nothing about the repository, a git that ran and
-- refused says everything, and a ref that moved under us is neither.
data CanonUnwritable =
    WriterFailed Text
    -- ^ git could not be started, or stopped being there. Worth retrying.
  | WriterStalled Text
    -- ^ git ran and did not finish. Not worth retrying at the same bound.
  | WriterRefused Text Told
    -- ^ git ran and said no, with which command and what it said
  | RefMoved (Maybe Text) (Maybe Text)
    -- | Canon moved between the fold and the commit, so the event was minted
    -- against a canon that is no longer the tip.
    --
    -- The commit itself may well have been written; what did not happen is the
    -- ref move, which is the only step that publishes anything. Nothing is
    -- lost and nothing is half-done: the caller folds again and re-mints,
    -- which is exactly what the bridge's @AlreadyInCanon@ is there to make
    -- safe.
    -- | git built a tree that does not hold a file it was given, and nothing
    -- was published.
    --
    -- Carries how many were asked for and one of the paths that did not land.
    --
    -- WHY THIS CAN HAPPEN AT ALL: @update-index --index-info@ answers a path it
    -- will not take with @Ignoring path ...@ on stderr and EXIT ZERO, so
    -- @write-tree@, @commit-tree@ and @update-ref@ all succeed on a tree with
    -- the file missing, and the verb reports the commit it made. The path that
    -- provoked it does not have to be this build's: a compaction writes back
    -- the names the tree already had, so one file somebody else put under
    -- @threads\/..\/x@ is enough, and what the compaction then publishes is
    -- canon with an event silently gone.
    --
    -- The reader refuses to fold such a path (see 'isEvent'), which stops it
    -- being an event here. This is the other half, and it is the one that does
    -- not depend on knowing which paths are bad: what was planned either landed
    -- or nothing is published.
  | WriterDropped Int ByteString
  deriving stock (Eq,Show)

instance Pretty CanonUnwritable where
  pretty = \case
    WriterFailed e  -> "could not run git:" <+> pretty e
    WriterStalled e -> "git did not finish:" <+> pretty e
    WriterDropped n p ->
      nest 2 $ vsep
        [ "git did not take every file it was given"
        , "planned" <+> pretty n <> ", and this one is not in the tree:"
        , "  " <> pretty (pathText p)
        , "nothing was published" ]
    WriterRefused what t ->
      nest 2 $ vsep [ "git" <+> pretty what <+> "refused", told t ]
    RefMoved want got ->
      nest 2 $ vsep
        [ pretty metaRef <+> "moved while this was being prepared"
        , "expected" <+> maybe "no ref" pretty want
        , "found" <+> maybe "no ref" pretty got
        , "nothing was published; fold again and retry" ]
