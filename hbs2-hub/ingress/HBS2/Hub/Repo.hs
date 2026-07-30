-- | Reading canon out of a git tree (PEP-19 "Tree layout").
--
-- Canon is an orphan commit chain under @refs/hbs2/meta@ whose trees hold one
-- file per event. This turns that into a 'FoldResult', and nothing more: it does
-- not decide anything about the events, which is "HBS2.Hub.Fold"'s job, and it
-- does not write.
--
-- Git is reached by running @git@, not through hbs2-git3. Two reasons. The ref is
-- an ordinary git ref that PEP-19 says is fetched and pushed by the mechanism
-- code branches already use, so plain git is the honest tool for reading it; and
-- @gitReadTree@ next door dedups its entries by object hash, which would silently
-- collapse two paths whose blobs happen to be identical.
--
-- The three calls are a record so the fold-over-a-tree can be tested against a
-- tree built in memory. That is not a hypothetical convenience: nothing has
-- written canon yet, so a reader wired straight to a repository would have had
-- nothing to read and no way to be checked.
module HBS2.Hub.Repo
  ( CanonSource(..)
  , CanonState(..)
  , gitCanon
  , readCanon
  , metaRef
  , eventPaths
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Canon
import HBS2.Hub.Fold

import HBS2.CLI.Prelude hiding (filter)

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List (isPrefixOf,sort)
import Data.Word (Word32)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.Process.Typed

-- | Where the canon files come from.
data CanonSource m = CanonSource
  { csCommit  :: m (Maybe Text)
    -- ^ what @refs/hbs2/meta@ points at, or 'Nothing' if the ref is absent
  , csPaths   :: Text -> m [FilePath]
    -- ^ every blob path under that commit, in any order
  , csBlob    :: Text -> FilePath -> m (Maybe Text)
  }

-- | What reading canon produced.
data CanonState = CanonState
  { stCommit  :: Maybe Text          -- ^ absent when the ref is not there
    -- | The tree's own @(hub-meta N)@, which DOES govern (unlike a file's own
    -- version): it names the admission rules that produced this canon. Absent
    -- when the tree has no @version@ file, which is a tree no writer here made.
  , stVersion :: Maybe Word32
    -- | Files that could not be read as events, by path. Reported rather than
    -- skipped: a canon file this build cannot parse is either corruption or a
    -- newer schema, and both are things an operator has to see.
  , stBad     :: [(FilePath, CanonError)]
    -- | The version each file declared. Reported and never obeyed, per PEP-19,
    -- and 'Nothing' for a file whose version clause was missing or unreadable.
  , stFileVersions :: [(FilePath, Maybe Word32)]
  , stFold    :: FoldResult
  }

-- | The ref canon lives under. One definition, because a second spelling of it
-- somewhere else is a clone that reads a different tracker.
metaRef :: Text
metaRef = "refs/hbs2/meta"

-- | Which paths in the tree are event files.
--
-- @threads\/\<thread-id>\/\<seq>-\<event-id>@ and @repo\/\<seq>-\<event-id>@, and
-- nothing else: the @version@ file and @index\/number.sexp@ are read by their own
-- readers and would fail an event parse, so handing them to one would report two
-- corrupt files in every healthy tree.
--
-- A prefix test rather than a shape test on purpose. A file under @threads/@ that
-- does not look like an event is a file somebody put there, and reporting it is
-- the point; silently ignoring anything unrecognised would let a tree carry
-- events this reader pretends not to see.
eventPaths :: [FilePath] -> [FilePath]
eventPaths = sort . filter isEvent
  where
    isEvent p = "threads/" `isPrefixOf` p || "repo/" `isPrefixOf` p

-- | Read canon and fold it.
--
-- The repository key is passed in rather than taken from the tree, because the
-- tree cannot be trusted to say whose it is: the owner key is the root of the
-- whole trust chain (PEP-19 rule 3), and a canon that named its own owner would
-- be canon that could rename it.
readCanon :: Monad m => CanonSource m -> RepoRef -> m CanonState
readCanon cs owner = do
  csCommit cs >>= \case
    Nothing -> pure (CanonState Nothing Nothing [] [] (foldEvents owner []))
    Just commit -> do
      paths <- csPaths cs commit

      ver <- csBlob cs commit "version" <&> \case
               Nothing -> Nothing
               Just t  -> either (const Nothing) Just (parseMeta t)

      files <- forM (eventPaths paths) $ \p ->
                 csBlob cs commit p <&> fmap (p,)

      let got = readEventLog [ f | Just f <- files ]
          -- A path the tree lists and whose blob cannot be read is not the same
          -- as a malformed file, and both have to be reported: the first is a
          -- broken read, the second is a file somebody wrote.
          absent = [ (p, MissingClause "blob") | (p, Nothing) <- zip (eventPaths paths) files ]

      pure CanonState
        { stCommit  = Just commit
        , stVersion = ver
        , stBad     = sort (crBad got <> absent)
        , stFileVersions = sort (crVersions got)
        , stFold    = foldEvents owner (crEvents got)
        }

-- | A 'CanonSource' backed by the git repository in the current directory.
--
-- @git@ is run three times per read, not once per file: @cat-file --batch@ would
-- be fewer round trips, and it is the obvious next step if a tree ever gets big
-- enough to notice. It is not done yet because a wrong answer from a batch parser
-- is a canon file silently misread, and one process per blob is a cost that shows
-- up in a profile rather than in a fold.
gitCanon :: MonadIO m => CanonSource m
gitCanon = CanonSource
  { csCommit = do
      -- rev-parse --verify, so an absent ref is an exit code rather than the
      -- string "refs/hbs2/meta" handed on as if it were a commit.
      git ["rev-parse", "--verify", "--quiet", Text.unpack metaRef <> "^{commit}"]
        <&> fmap (Text.strip . decode)

  , csPaths = \commit ->
      -- -z, because a path in a git tree may contain anything but NUL, and
      -- splitting on newlines would break on the first one that contains a
      -- newline. --name-only with -r gives blobs only, which is what this wants.
      git ["ls-tree", "-r", "-z", "--name-only", Text.unpack commit]
        <&> maybe [] (fmap Text.unpack . filter (not . Text.null)
                       . Text.split (== '\0') . decode)

  , csBlob = \commit path ->
      git ["cat-file", "blob", Text.unpack commit <> ":" <> path] <&> fmap decode
  }
  where
    decode = Text.decodeUtf8Lenient . LBS8.toStrict

    -- Nothing for a non-zero exit, which is how git reports "no such ref" and
    -- "no such path" alike. Neither is exceptional here: an unfetched canon and
    -- a tree without an index are both ordinary states.
    git args = do
      (code, out, _) <- readProcess (setStdin closed (proc "git" args))
      pure $ case code of
        ExitSuccess -> Just out
        _           -> Nothing
