-- | Canon as a tree of files (PEP-19 "Tree layout").
--
-- Canon is an orphan git commit chain under @refs\/hbs2\/meta@ whose trees hold
-- one file per event. This turns such a tree into a 'FoldResult', and nothing
-- more: it decides nothing about the events, which is "HBS2.Hub.Fold"'s job.
--
-- Pure, and here rather than beside the git plumbing that feeds it, because the
-- writer belongs next to it: 'eventPath' below is already in "HBS2.Hub.Bridge",
-- and a reader and a writer of one format on opposite sides of a package
-- boundary is a format with two owners. What talks to git is a 'CanonSource',
-- which the CLI supplies.
--
-- Everything that can go wrong is a value. A tree that cannot be read is not an
-- empty tree, a version this build does not know is not a version it may fold
-- under, and a file it cannot parse is not a file that is not there; each of
-- those was once the same observation as the benign one it sits next to, and
-- each of them is something an operator has to act on differently.
module HBS2.Hub.Repo
  ( CanonSource(..)
  , CanonState(..)
  , CanonUnreadable(..)
  , FileProblem(..)
  , TreeEntry(..)
  , readCanon
  , metaRef
  , eventEntries
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Canon
import HBS2.Hub.Fold

import HBS2.Prelude.Plated (Pretty(..),(<+>))

import Data.Functor ((<&>))
import Data.List (isPrefixOf,partition,sortOn)
import Data.Text (Text)
import Data.Word (Word32)

-- | One blob the tree lists: its path and its size.
--
-- The size comes from the tree listing, so it is known before anything is read,
-- which is the only order in which a size bound bounds what a READER spends. A
-- reader that fetched the blob and then compared it against 'maxEventBytes' has
-- already paid for it: a tree of twenty thousand files just under the limit is
-- inside every per-file bound and is gigabytes of resident text.
data TreeEntry = TreeEntry
  { teePath :: FilePath
  , teeSize :: Int
  }
  deriving stock (Eq,Show)

-- | Where the canon files come from.
--
-- A record of functions rather than a git call, so the fold over a tree can be
-- exercised against a tree built in memory. That was not a hypothetical when it
-- was written: nothing had produced canon yet, so a reader wired straight to a
-- repository would have had nothing to read.
data CanonSource m = CanonSource
  { csCommit  :: m (Either CanonUnreadable Text)
    -- ^ what @refs\/hbs2\/meta@ points at
  , csEntries :: Text -> m (Maybe [TreeEntry])
    -- ^ every blob under that commit, or 'Nothing' if the tree cannot be listed
  , csBlob    :: Text -> FilePath -> m (Maybe Text)
  }

-- | Why there is no canon to fold.
--
-- One type for the answers that are about the whole tree, because they are what
-- a caller branches on and they used to be three different spellings of nothing:
-- an unfetched ref, a broken repository and an unlistable tree all arrived as an
-- empty fold with a zero exit.
data CanonUnreadable =
    -- | @refs\/hbs2\/meta@ is not here. The ordinary state of a fresh clone:
    -- git's default refspec covers heads and tags, so canon is fetched
    -- explicitly. Not an error, and not an empty tracker either.
    NoCanonRef
    -- | Not a git repository, or git could not be run at all. Distinct from the
    -- above because the advice is different: fetching canon into a directory
    -- that is not a repository will not help.
  | NoRepository Text
    -- | The commit is here and its tree is not: a partial clone, a pruned
    -- object, a killed process. Empty canon is the one answer this must not
    -- give, since it is indistinguishable from a tracker with nothing in it.
  | TreeUnreadable Text
    -- | The tree's @(hub-meta N)@ is newer than this build's rules. Folding
    -- anyway would produce a view under rules this build does not implement,
    -- which is how one clone quietly disagrees with every other one.
  | CanonTooNewHere Word32
    -- | The @version@ file is here and does not read. The one file in the tree
    -- whose unreadability nothing else can report, since it is not an event, so
    -- swallowing it made a corrupt stamp and an absent one the same observation.
  | VersionUnreadable
  deriving stock (Eq,Show)

instance Pretty CanonUnreadable where
  pretty = \case
    NoCanonRef        -> "no" <+> pretty metaRef <+> "in this repository"
    NoRepository e    -> "cannot read this git repository:" <+> pretty (safeText e)
    TreeUnreadable c  -> "the tree of commit" <+> pretty (safeText c)
                           <+> "cannot be listed"
    CanonTooNewHere n -> "canon was folded under rules newer than this build:"
                           <+> "hub-meta" <+> pretty n
    VersionUnreadable -> "the version file is present and does not read"

-- | Why one file in the tree did not become an event.
data FileProblem =
    FileMalformed CanonError  -- ^ read, and not an event
    -- | The tree lists it and its blob will not read. Not the same as malformed,
    -- and reported all the same: the path is what @hbs2-peer download@ takes.
  | FileUnreadable
    -- | Bigger than 'maxEventBytes', refused from the tree listing without ever
    -- being fetched.
  | FileTooLarge Int
  deriving stock (Eq,Ord,Show)

instance Pretty FileProblem where
  pretty = \case
    FileMalformed e -> pretty e
    FileUnreadable  -> "listed in the tree and its blob does not read"
    FileTooLarge n  -> "over the bound this reader will read:" <+> pretty n
                         <+> "bytes"

-- | What reading canon produced.
data CanonState = CanonState
  { stCommit  :: Text
  , stVersion :: Maybe Word32
    -- ^ the tree's own rules version, absent when the tree has no @version@ file
  , stBad     :: [(FilePath, FileProblem)]
  , stFileVersions :: [(FilePath, Maybe Word32)]
    -- ^ what each file declared, reported and never obeyed (PEP-19)
  , stFold    :: FoldResult
  }

-- | The ref canon lives under. One definition, because a second spelling of it
-- somewhere else is a clone that reads a different tracker.
metaRef :: Text
metaRef = "refs/hbs2/meta"

-- | Which entries in the tree are event files, and which are refused unread.
--
-- @threads\/\<thread-id>\/\<seq>-\<event-id>@ and @repo\/\<seq>-\<event-id>@, and
-- nothing else: the @version@ file and the number index are read by their own
-- readers and would fail an event parse, so handing them to one would report two
-- corrupt files in every healthy tree.
--
-- A prefix test rather than a shape test on purpose. A file under @threads\/@
-- that does not look like an event is a file somebody put there, and reporting
-- it is the point; silently ignoring anything unrecognised would let a tree
-- carry events this reader pretends not to see.
eventEntries :: [TreeEntry] -> ([TreeEntry], [(FilePath, FileProblem)])
eventEntries es = (small, [ (teePath e, FileTooLarge (teeSize e)) | e <- big ])
  where
    (big, small) = partition ((> maxEventBytes) . teeSize) (filter isEvent es)
    isEvent e = "threads/" `isPrefixOf` teePath e || "repo/" `isPrefixOf` teePath e

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
    Nothing -> pure (Left (TreeUnreadable commit))
    Just entries -> do
      -- The version first, because it decides whether the rest may be folded at
      -- all. An absent file is not a refusal (a tree written by something else
      -- is still readable); one that is present and malformed is.
      ver <- csBlob cs commit "version" <&> fmap parseMeta
      case ver of
        Just (Left _) -> pure (Left VersionUnreadable)
        _ -> do
          -- ver is Nothing or Just (Right n) here: the malformed case left above.
          let declared = case ver of
                           Just (Right n) -> Just n
                           _              -> Nothing
              (ok, tooBig) = eventEntries entries
              paths = fmap teePath ok

          files <- traverse (\p -> csBlob cs commit p <&> (p,)) paths

          let got = readEventLog [ (p,t) | (p, Just t) <- files ]
              absent = [ (p, FileUnreadable) | (p, Nothing) <- files ]
              bad = sortOn fst $ tooBig <> absent
                      <> [ (p, FileMalformed e) | (p,e) <- crBad got ]

          -- foldCanon and not foldEvents: the tree's version governs the
          -- admission rules, which is the whole reason it is a tree-level file
          -- rather than a per-file one, and folding a newer tree under this
          -- build's rules is the divergence PEP-19 versions canon to prevent.
          let rules = maybe hubMetaVersion id declared
          pure $ case foldCanon rules owner (crEvents got) of
            Left (MetaTooNew n) -> Left (CanonTooNewHere n)
            Right fr -> Right CanonState
              { stCommit  = commit
              , stVersion = declared
              , stBad     = bad
              , stFileVersions = sortOn fst (crVersions got)
              , stFold    = fr
              }
