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
  , versionPath
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Canon
import HBS2.Hub.Fold

import HBS2.Prelude.Plated (Pretty(..),(<+>))

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word32)

-- | One entry a tree listing reported.
--
-- The object id is what a blob is fetched by, so a path never reaches a process
-- argument; the size comes from the listing, so a bound can refuse a blob before
-- anything pays for it.
--
-- Both are 'Nothing' for an entry that is not a readable blob: a submodule, which
-- has no size in a listing, or a record the listing parser could not read at all.
-- Neither is dropped. A gitlink under @threads\/@ is something somebody put in
-- canon, and a record nobody can parse is the one thing a reader must not swallow
-- silently: dropping those would empty the listing and report a clean, empty
-- audit, which is what a broken read looks like when nobody is checking.
data TreeEntry = TreeEntry
  { teePath :: ByteString
  , teeBlob :: Maybe (Text, Int)   -- ^ (object id, size in bytes)
  }
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
  , csBlob    :: Text -> m (Maybe Text)
  }

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
  deriving stock (Eq,Show)

instance Pretty CanonUnreadable where
  pretty = \case
    NoCanonRef        -> "no" <+> pretty metaRef <+> "in this repository"
    NoRepository e    -> "cannot read this git repository:" <+> pretty (safeText e)
    RefUnresolved e   -> pretty metaRef <+> "does not resolve to a commit:"
                           <+> pretty (safeText e)
    TreeUnreadable e  -> "cannot list the canon tree:" <+> pretty (safeText e)
    CanonTooNewHere n -> "canon was folded under rules newer than this build:"
                           <+> "hub-meta" <+> pretty n
    VersionUnreadable p -> "the version file is listed and does not read:"
                             <+> pretty p
    CanonTooBig n     -> "the event files total" <+> pretty n
                           <+> "bytes, past what this reader will hold"
    CanonTooMany n    -> "there are" <+> pretty n
                           <+> "event files, past what this reader will fetch"

-- | Why one file in the tree did not become an event.
data FileProblem =
    FileMalformed CanonError  -- ^ read, and not an event
    -- | Listed and its blob will not read. Not the same as malformed, and
    -- reported all the same: what is broken is the read, not the file.
  | FileUnreadable
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
    -- | The listing record itself could not be read. The one problem that is
    -- about this reader rather than about the tree, and reported loudly for that
    -- reason: if the listing format ever shifts, every entry becomes this, which
    -- is a visible failure instead of an empty audit.
  | FileListingUnparsed
    -- | Somewhere the tree layout does not put files. PEP-19 fixes the layout, so
    -- a path outside it is somebody's addition, and a reader that only looked
    -- under the two event directories could not see it at all.
  | FileUnexpected
  deriving stock (Eq,Ord,Show)

instance Pretty FileProblem where
  pretty = \case
    FileMalformed e     -> pretty e
    FileUnreadable      -> "listed in the tree and its blob does not read"
    FileTooLarge n      -> "over the bound this reader will read:" <+> pretty n
                             <+> "bytes"
    FileNotABlob        -> "not a blob: a submodule, in a canon tree"
    FileListingUnparsed -> "this reader could not parse the tree listing record"
    FileUnexpected      -> "not a path the canon tree layout has"

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

-- | How much of a tree this reader will take in, and how many files.
--
-- Two bounds, because they bound different costs. The bytes bound what the reader
-- holds: the fold sorts the whole log, so every admitted event is resident at
-- once, and a per-file bound does not imply a total one. The count bounds how
-- many times a 'CanonSource' is asked for a blob.
--
-- Both are measured over the EVENT files only, and only over those whose size the
-- listing gave. A README somebody committed next to canon is not something this
-- reader fetches, so counting it towards a bound would refuse an audit over a
-- file the audit does not read. And both are checked before a byte is fetched,
-- which is what makes them bounds on the reader rather than notes about it.
maxCanonBytes :: Int
maxCanonBytes = 256 * 1024 * 1024

maxCanonFiles :: Int
maxCanonFiles = 200000

-- | Split a listing into the event files to read and the entries to refuse.
--
-- PEP-19 fixes the tree layout, so every path is one of four things: the version
-- file, the number index, an event under @threads\/@ or @repo\/@, or somebody's
-- addition. The last is reported. A reader that looked only under the two event
-- directories was blind to the rest of the tree, which is the reader that lets a
-- tree carry things it pretends not to see.
sortCanon :: [TreeEntry] -> ([(ByteString, Text, Int)], [(ByteString, FileProblem)])
sortCanon = go [] []
  where
    go evs bad [] = (reverse evs, reverse bad)
    go evs bad (e:rest)
      | not (isEvent (teePath e)) =
          -- The version file and the index have their own readers; anything else
          -- outside the layout is reported.
          if teePath e == versionPath || teePath e == B8.pack numberIndexPath
            then go evs bad rest
            else go evs ((teePath e, FileUnexpected) : bad) rest
      | otherwise = case teeBlob e of
          Nothing -> go evs ((teePath e, notBlob (teePath e)) : bad) rest
          Just (oid, n)
            | n > maxEventBytes -> go evs ((teePath e, FileTooLarge n) : bad) rest
            | otherwise -> go ((teePath e, oid, n) : evs) bad rest

    -- A listing this reader could not parse has neither an id nor a size, and so
    -- does a submodule. The listing parser is expected to mark the difference; a
    -- path with no separator at all is the record it could not read.
    notBlob p | B8.null p = FileListingUnparsed
              | otherwise = FileNotABlob

    isEvent p = B8.pack "threads/" `B8.isPrefixOf` p
             || B8.pack "repo/" `B8.isPrefixOf` p

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
    Right entries -> do
      let (evEntries, refused) = sortCanon entries
          bytes = sum [ n | (_,_,n) <- evEntries ]

      -- Both bounds before any blob is fetched.
      case bounds bytes (length evEntries) of
       Just refusal -> pure (Left refusal)
       Nothing -> do
        -- The version decides whether the rest may be folded at all, so its
        -- absence and its unreadability are different answers. The listing is
        -- what tells them apart: a file the tree does not list is absent, and one
        -- it lists whose blob will not read is a version this reader does not
        -- have. Before the listing was consulted, a pruned version blob read as
        -- "no version file" and the tree was folded under this build's rules,
        -- which is the gate below being skipped in silence.
        ver <- case [ b | e <- entries, teePath e == versionPath, let b = teeBlob e ] of
                 [] -> pure (Right Nothing)
                 (Nothing : _) -> pure (Left FileNotABlob)
                 (Just (oid, n) : _)
                   | n > maxEventBytes -> pure (Left (FileTooLarge n))
                   | otherwise -> csBlob cs oid >>= \case
                       Nothing -> pure (Left FileUnreadable)
                       Just t  -> pure (either (Left . FileMalformed) (Right . Just)
                                          (parseMeta t))

        case ver of
          Left p -> pure (Left (VersionUnreadable p))
          Right declared -> do

            -- Parsed inside the loop, so a file's TEXT dies as soon as it has
            -- become an event or a problem. Binding the texts first and using
            -- that list twice kept all of them alive at once, which made the peak
            -- the size of canon rather than of its largest file. What survives is
            -- the events, which the fold needs all of at once, since it sorts.
            (evs, vers, bad') <- foldM readOne ([],[],[]) evEntries

            let bad = sortOn fst (refused <> bad')

            -- foldCanon and not foldEvents: the tree's version governs the
            -- admission rules, which is the whole reason it is a tree-level file.
            pure $ case foldCanon (maybe hubMetaVersion id declared) owner evs of
              Left (MetaTooNew n) -> Left (CanonTooNewHere n)
              Right fr -> Right CanonState
                { stCommit  = commit
                , stVersion = declared
                , stBad     = bad
                , stFileVersions = sortOn fst vers
                , stFold    = fr
                }

  where
    bounds bytes n
      | bytes > maxCanonBytes = Just (CanonTooBig bytes)
      | n > maxCanonFiles     = Just (CanonTooMany n)
      | otherwise             = Nothing

    readOne (evs, vers, bad) (p, oid, _) = csBlob cs oid >>= \case
      Nothing -> pure (evs, vers, (p, FileUnreadable) : bad)
      Just t  -> pure $ case parseEvent t of
        Left e        -> (evs, vers, (p, FileMalformed e) : bad)
        Right (v, ev) -> (ev : evs, (p, v) : vers, bad)
