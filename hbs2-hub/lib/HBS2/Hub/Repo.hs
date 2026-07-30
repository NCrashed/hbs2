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
  , metaRef
  , sortCanon
  , maxCanonBytes
  , maxCanonFiles
  , maxListingBytes
  , versionPath
  , assumedMetaVersion
  , EntryKind(..)
  , BlobResult(..)
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Canon
import HBS2.Hub.Fold

import HBS2.Prelude.Plated (Pretty(..),(<+>),Doc,nest,vsep,line)

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.List (sortOn)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32)

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
    -- | The source could not run. Says nothing about canon.
  | BlobUnavailable Text
  deriving stock (Eq,Show)

-- | Why there is no canon to fold.
--
-- One type for the answers about the whole tree, because they are what a caller
-- branches on, and they were once several spellings of nothing: an unfetched ref,
-- a broken repository, an unlistable tree and canon from the future all arrived
-- as an empty fold with a zero exit.
data CanonUnreadable =
    -- | @refs\/hbs2\/meta@ is not here. The ordinary state of a fresh clone:
    -- git's default refspec covers heads and tags, so canon is fetched
    -- explicitly. Not an error, and not an empty tracker either.
    NoCanonRef
    -- | Not a git repository, or git could not be run. Distinct from the above
    -- because the advice differs: fetching canon into a directory that is not a
    -- repository will not help. Carries what the tool said, whole.
  | NoRepository Text
    -- | The ref is here and does not resolve to a commit: a pruned object, a
    -- partial clone, or a ref pointing at something else. Distinct from a missing
    -- ref for the same reason, and it used to share its answer.
  | RefUnresolved Text
    -- | The commit is here and its tree cannot be listed. Carries the tool's own
    -- words, because the reason ranges from a pruned object to a repository this
    -- user is not allowed to read, and only one of those is fixed by fetching.
  | TreeUnreadable Text
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
    -- | The reader could not run the tool at all: no process slots, no file
    -- descriptors, the tool gone from PATH mid-audit. Nothing is known about
    -- canon, which is why this is not a finding about it.
    --
    -- MID-AUDIT is the distinction from 'NoRepository'. A tool that will not run
    -- on the very first call cannot be told from a directory that is not a
    -- repository, and 'NoRepository' says both and advises both. This one is for
    -- a tool that ran, answered, and then stopped running, which is what a fork
    -- limit reached two hundred thousand blobs into a tree looks like.
  | ReaderFailed Text
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
-- refusal is the safe.directory command on the SECOND line. Rendered as a field
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
    NoCanonRef        -> "no" <+> pretty metaRef <+> "in this repository"
    NoRepository e    -> "cannot read this git repository:" <> toolSaid e
    RefUnresolved e   -> pretty metaRef <+> "does not resolve to a commit:"
                           <> toolSaid e
    TreeUnreadable e  -> "cannot list the canon tree:" <> toolSaid e
    CanonTooNewHere n -> "canon was folded under rules newer than this build:"
                           <+> "hub-meta" <+> pretty n
    VersionUnreadable p -> "the version file is listed and does not read:"
                             <+> pretty p
    CanonTooBig n     -> "the event files total" <+> pretty n
                           <+> "bytes, past what this reader will hold"
    CanonTooMany n    -> "there are" <+> pretty n
                           <+> "files in the canon tree, past what this reader"
                           <+> "will take in"
    CanonListingTooBig n -> "the canon tree listing is over" <+> pretty n
                              <+> "bytes, past what this reader will read"
    ReaderFailed e    -> "this reader could not run git:" <> toolSaid e

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
    -- | A blob whose object this clone does not have: a blobless or partial
    -- clone, or a pruned object. Distinct from the two above because it is the
    -- one that fetching fixes, and it used to report as a submodule.
  | FileObjectMissing
    -- | The listing record itself could not be read. See 'Unparsed'.
  | FileListingUnparsed
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
    FileObjectMissing   -> "the object is not in this clone; fetch, or unshallow"
    FileListingUnparsed -> "this reader could not parse the tree listing record"
    FileUnexpected      -> "not a path the canon tree layout has"
    FileDuplicated      -> "listed more than once in the tree"

-- | What reading canon produced.
data CanonState = CanonState
  { stCommit  :: Text
  , stVersion :: Maybe Word32
    -- ^ the tree's own rules version, absent when the tree has no @version@ file
  , stBad     :: [(ByteString, FileProblem)]
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

-- | What rules a tree with no version file was folded under.
--
-- The OLDEST, not this build's. They are the same number today, and the day
-- hub-meta becomes 2 they stop being: reading an unstamped tree as "whatever
-- this build implements" would make deleting one unsigned file a way to choose
-- the rules canon is folded under, and hub verify would report that choice as a
-- parenthesis and a zero exit. A tree that does not say cannot claim newer.
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
    (evs, bad) = go [] [] (nubPaths [] entries)

    dups = [ (p, FileDuplicated)
           | p : _ : _ <- List.group (List.sort (fmap teePath entries)) ]

    -- The SECOND entry for a path is dropped, not merely reported. Kept, both
    -- were fetched, both were parsed, both went into the fold and the byte bound
    -- counted the object twice; with the same oid on both, the report said
    -- "listed more than once" and then dropped one as a duplicate event-id, which
    -- is an accusation about canon that the reader had manufactured. The version
    -- file is refused outright for the same ambiguity, and this is the rest of
    -- that rule.
    nubPaths _ [] = []
    nubPaths seen (e:rest)
      | teePath e `elem` seen = nubPaths seen rest
      | otherwise = e : nubPaths (teePath e : seen) rest

    go evs bad [] = (reverse evs, reverse bad)
    go evs bad (e:rest) = case teeKind e of
      -- Before the layout question, because a record nobody could read has no
      -- reliable path to ask it about.
      Unparsed -> go evs ((teePath e, FileListingUnparsed) : bad) rest
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
                     Blob{} -> go evs bad rest
                     _      -> go evs ((teePath e, problemOf kind) : bad) rest
              else go evs ((teePath e, FileUnexpected) : bad) rest
        | otherwise -> case kind of
            Blob oid n
              | n > maxEventBytes -> go evs ((teePath e, FileTooLarge n) : bad) rest
              | otherwise -> go ((teePath e, oid, n) : evs) bad rest
            _ -> go evs ((teePath e, problemOf kind) : bad) rest

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
            && length (Prelude.filter (not . B8.null)
                        (B8.split '/' (B8.drop (length pfx) q))) == depth

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
  Right commit -> csEntries cs commit >>= \case
    Left e -> pure (Left e)
    Right entries
      -- The COUNT is over the whole listing, and the bytes over the event files.
      -- Two different costs: every entry becomes a TreeEntry, is sorted and is
      -- printed, so ten million paths outside the layout cost ten million lines
      -- whether or not this reader would fetch any of them, and the paths are
      -- slices of one listing buffer, so holding any of them holds all of it. The
      -- bytes are what the reader will HOLD, which only the event files are.
      | length entries > maxCanonFiles ->
          pure (Left (CanonTooMany (length entries)))
    Right entries -> do
      let (evEntries, refused) = sortCanon entries
          bytes = sum [ n | (_,_,n) <- evEntries ]

      -- Before any blob is fetched.
      case (if bytes > maxCanonBytes then Just (CanonTooBig bytes) else Nothing) of
       Just refusal -> pure (Left refusal)
       Nothing -> do
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
                 [] -> pure (Right Nothing)
                 (_ : _ : _) -> pure (Left (Right FileDuplicated))
                 (NotABlob : _)      -> pure (Left (Right FileNotABlob))
                 (Unparsed : _)      -> pure (Left (Right FileListingUnparsed))
                 (BlobMissing _ : _) -> pure (Left (Right FileObjectMissing))
                 (Blob oid n : _)
                   | n > maxEventBytes -> pure (Left (Right (FileTooLarge n)))
                   | otherwise -> csBlob cs oid >>= \case
                       BlobUnavailable e -> pure (Left (Left (ReaderFailed e)))
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
          Right (Just 0) -> pure (Left (VersionUnreadable (FileMalformed
                              (BadClause "hub-meta"))))
          -- BEFORE the blobs, not after them. The gate lives inside foldCanon,
          -- which runs last, so a tree stamped newer than this build was read in
          -- full first: measured at one cat-file per event, all of it thrown away.
          -- Worse, a fork failing anywhere in that loop turned the answer into
          -- ReaderFailed and "nothing was learned about canon", when the version
          -- file had been read and the answer was already known.
          Right (Just n) | n > hubMetaVersion -> pure (Left (CanonTooNewHere n))
          Right declared -> do

            -- Parsed inside the loop, so a file's TEXT dies as soon as it has
            -- become an event or a problem. Binding the texts first and using
            -- that list twice kept all of them alive at once, which made the peak
            -- the size of canon rather than of its largest file. What survives is
            -- the events, which the fold needs all of at once, since it sorts.
            -- Left the moment the tool stops running, and not a file-shaped
            -- problem per remaining file: 200000 failed forks are one local
            -- failure, and a report listing them all as unreadable files is a
            -- report about canon that is not about canon.
            readAll <- foldM readOne (Right ([],[],[])) evEntries

            case readAll of
             Left u -> pure (Left u)
             Right (evs, vers, bad2) -> do
              let bad = sortOn fst (refused <> bad2)

              -- foldCanon and not foldEvents: the tree's version governs the
              -- admission rules, which is the whole reason it is a tree-level file.
              pure $ case foldCanon (maybe assumedMetaVersion id declared) owner evs of
                Left (MetaTooNew n) -> Left (CanonTooNewHere n)
                Right fr -> Right CanonState
                  { stCommit  = commit
                  , stVersion = declared
                  , stBad     = bad
                  , stFileVersions = sortOn fst vers
                  , stFold    = fr
                  }

  where
    readOne acc@(Left _) _ = pure acc
    readOne (Right (evs, vers, bad)) (p, oid, _) = csBlob cs oid >>= \case
      BlobUnavailable e -> pure (Left (ReaderFailed e))
      BlobRefused e -> pure (Right (evs, vers, (p, FileUnreadable e) : bad))
      BlobText t -> pure $ case parseEvent t of
        Left e        -> Right (evs, vers, (p, FileMalformed e) : bad)
        Right (v, ev) -> Right (ev : evs, (p, v) : vers, bad)
