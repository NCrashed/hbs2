{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
-- | A 'CanonSink' backed by a git repository (PEP-19 "Tree layout").
--
-- The write half of "HBS2.Hub.Repo.Git", and separate from it because the two
-- guard against opposite things. The reader walks a tree somebody else wrote
-- and is hostile to it throughout: every size is a bound, every silence is a
-- case, a path is bytes it will not hand to a shell. The writer's input is
-- what this build just rendered and 'planCanon' just read back, so what it has
-- to get right is not the data but the effect: which repository it lands in,
-- what it does to the index, and whether the ref it moves is still the one it
-- read.
--
-- Five commands, in one order:
--
-- > hash-object -w --stdin        one per file, content on stdin
-- > read-tree <parent>            only when canon already exists
-- > update-index --index-info     the new paths, on stdin
-- > write-tree                    then commit-tree, message on stdin
-- > update-ref <ref> <new> <old>  compare-and-swap, which is the publish
--
-- A PATH NEVER REACHES A PROCESS ARGUMENT, for the reason the reader gives at
-- length: under the C locale a path with any byte above 127 does not survive
-- argv, and a thread whose id is not the issue is a directory whose name is
-- not Latin-1. @--index-info@ takes them on stdin, and @hash-object --stdin@
-- needs no path at all.
--
-- THE INDEX IS NOT THE REPOSITORY'S. @GIT_INDEX_FILE@ points at a file of this
-- writer's own for the length of one commit: building canon through the
-- caller's index would stage their working tree's changes into a canon commit
-- and leave their @git status@ rearranged by a tool they ran to accept an
-- issue.
module HBS2.Hub.Repo.GitWrite
  ( withGitSink
  , withGitSinkIn
  , canonAuthor
  ) where

import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git (gitRun,GitTrouble(..))

import HBS2.CLI.Prelude hiding (filter)

import Control.Monad.Except (ExceptT(..),runExceptT,throwError)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Bifunctor (first)
import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import System.FilePath ((</>))
import System.Process.Typed

-- | Who a canon commit is by, as far as git is concerned.
--
-- FIXED, and not the operator's @user.name@. Authorship in canon is the pair of
-- signed boxes inside the event files; the commit around them is plumbing, and
-- signing it with a person's name would claim, in the one place a git tool
-- looks, that they wrote what a stranger sent. It also makes the commit
-- reproducible: same tree, same parent, same stamp, same commit id, on any
-- machine, including one with no git identity configured at all, where
-- @commit-tree@ otherwise refuses outright.
--
-- The address is under @.invalid@, which exists for exactly this and can never
-- resolve.
canonAuthor :: (String, String)
canonAuthor = ("hbs2-hub", "canon@hbs2.invalid")

-- | A sink for the repository discovered from the current directory.
withGitSink :: MonadUnliftIO m => (CanonSink m -> m a) -> m a
withGitSink = withGitSinkIn Nothing

-- | A sink for a named repository, or for the discovered one when unnamed.
--
-- The bracket shape rather than a bare constructor, matching
-- 'HBS2.Hub.Repo.Git.withGitCanon': the temporary index lives for the length
-- of the scope, and a caller that forgets to release it leaves a file behind
-- in every accept.
withGitSinkIn :: MonadUnliftIO m => Maybe FilePath -> (CanonSink m -> m a) -> m a
withGitSinkIn cwd act =
  withSystemTempDirectory "hbs2-hub-canon" $ \tmp ->
    act (sink cwd (tmp </> "index"))

-- How long one git call may take. Every command here is local and touches a
-- handful of objects, so this is not a budget to spend but a bound on being
-- stuck: an update-ref waiting on a lock somebody else holds is the realistic
-- case, and waiting a minute for it is longer than any of them takes.
callSeconds :: Int
callSeconds = 60

sink :: forall m . MonadUnliftIO m => Maybe FilePath -> FilePath -> CanonSink m
sink cwd indexFile = CanonSink
  { skParent = parentOf
  , skClose = pure ()
  , skCommit = \cw -> commitWith (cnParent cw) cw
  -- The parent is NOT what the ref must hold here: a rewrite writes a root and
  -- still swaps against the canon it compacted (PEP-19).
  , skRewrite = commitWith Nothing
  }
  where

  commitWith parent cw = runExceptT do
      -- The ref is re-read HERE and compared before anything is written, and
      -- again by update-ref at the end. The first check costs one rev-parse
      -- and turns the common case of a stale caller into a refusal that wrote
      -- nothing; the second is the one that is atomic, and is what actually
      -- holds when two accepts race.
      now <- ExceptT parentOf
      when (now /= cnParent cw) $
        throwError (RefMoved (cnParent cw) now)

      let whenMs = cnWhen cw

      oids <- traverse (ExceptT . blobOf whenMs) (cnFiles cw)

      -- read-tree FIRST and into an index that does not exist yet, which is
      -- how the new commit keeps every file the parent had. Without a parent
      -- there is nothing to seed from and the index starts empty, which is the
      -- orphan root PEP-19 asks for.
      _ <- case parent of
             Nothing -> pure ""
             -- AND ITS FAILURE IS ITS OWN. read-tree fails on the WHOLE
             -- parent when one path in it is unindexable, and every verb that
             -- writes canon starts here -- so the state is "this repository
             -- cannot be written until somebody rewrites canon" and not "this
             -- command failed". See 'WriterUnindexable'.
             Just p  -> ExceptT ( run whenMs "read-tree" ["read-tree", Text.unpack p] mempty
                                    <&> first unindexable )

      _ <- ExceptT (run whenMs "update-index" ["update-index", "-z", "--index-info"]
                        (LBS.fromStrict (indexInfo (zip (fmap fst (cnFiles cw)) oids))))

      tree <- ExceptT (run whenMs "write-tree" ["write-tree"] mempty) <&> Text.strip . dec

      -- WHAT WAS PLANNED EITHER LANDED OR NOTHING IS PUBLISHED.
      --
      -- update-index answers a path it will not take with "Ignoring path ..."
      -- on stderr and EXIT ZERO, so everything below here succeeds on a tree
      -- with the file missing and the verb reports the commit it made.
      -- Reproduced on git 2.46 with `threads/../x`: the entry is dropped and
      -- write-tree returns the empty tree.
      --
      -- The path does not have to be one this build chose. A compaction writes
      -- back the names the tree already had, deliberately -- deriving them
      -- instead would quietly rename a misnamed file -- so one file somebody
      -- else put under such a name is enough, and what a compaction then
      -- publishes is canon with an event gone and a success on stdout.
      --
      -- Asked of the TREE and not of the index, because the tree is what the
      -- commit will hold.
      landed <- ExceptT (run whenMs "ls-tree"
                             ["ls-tree", "-r", "--name-only", "-z", Text.unpack tree]
                             mempty)
                  <&> HS.fromList . filter (not . BS.null) . BS.split 0x00

      for_ (fmap fst (cnFiles cw)) $ \p ->
        unless (HS.member p landed) $
          throwError (WriterDropped (length (cnFiles cw)) p)

      -- AND WHAT THE PARENT HELD IS STILL THERE, which is the direction this
      -- did not check. It said inclusion was enough because "read-tree seeded
      -- the index from the parent, so everything canon already had is in here
      -- too", and that is the assumption update-index breaks: an entry is
      -- added with OK_TO_REPLACE, so writing threads/<id>/1.event into an index
      -- holding a FILE at threads/<id> REMOVES that file. Exit zero, nothing on
      -- stderr, and the planned path lands -- so the loop above is satisfied
      -- and the publish goes ahead with a file of canon gone. Reproduced on git
      -- 2.46 with read-tree of a tree holding `threads/abc` and --index-info
      -- with `threads/abc/1.event`: write-tree returns a tree of one file.
      --
      -- One more ls-tree per canon commit, which is the price of the guarantee
      -- the haddock above was already claiming.
      --
      -- ONLY WHERE THERE IS A PARENT TO KEEP. A rewrite legitimately drops
      -- files and gets its own emptiness honestly: skRewrite passes Nothing
      -- here, so no read-tree ran and there is nothing to have kept.
      inherited <- case parent of
        Nothing -> pure mempty
        Just p  -> ExceptT (run whenMs "ls-tree"
                                ["ls-tree", "-r", "--name-only", "-z", Text.unpack p]
                                mempty)
                     <&> HS.fromList . filter (not . BS.null) . BS.split 0x00

      -- Sorted, so which path a report names is a function of the trees and not
      -- of a HashSet's traversal order.
      for_ (List.sort (HS.toList inherited)) $ \p ->
        unless (HS.member p landed) $
          throwError (WriterLost (HS.size inherited) p)

      -- -c commit.gpgsign=false, because this machine's owner may well sign
      -- their own commits and a canon commit is not theirs to sign. Unattended
      -- triage must not stop on a pinentry, and a signature over plumbing that
      -- wraps somebody else's signed boxes would say something untrue.
      commit <- ExceptT (run whenMs "commit-tree"
                             ( [ "-c", "commit.gpgsign=false", "commit-tree"
                               , Text.unpack tree ]
                               <> concat [ ["-p", Text.unpack p] | Just p <- [parent] ] )
                             (LBS.fromStrict (Text.encodeUtf8 (cnMessage cw))))
                  <&> Text.strip . dec

      -- THE PUBLISH, and the only step that is. Everything above wrote objects
      -- git will collect if this fails; this is where canon changes for
      -- everyone who fetches it.
      --
      -- The old value is given, so this is a compare-and-swap: an empty string
      -- means the ref must not exist, which is the first-accept case. Two
      -- accepts racing therefore leave one refusal and one commit, rather than
      -- one of them silently winning with an event minted against a canon that
      -- had already moved.
      -- A REFUSAL HERE IS REPORTED AS WHAT IT IS. The compare-and-swap failing
      -- means somebody else published between the check above and this line,
      -- which is the case this argument exists for -- and it came back as a
      -- quoted git error, the same shape as "git is not installed". A caller
      -- cannot tell "retry against the canon that is there now" from "this
      -- machine is broken" out of that. So the ref is re-read and, if it has
      -- moved, the answer is the same 'RefMoved' the pre-check gives; anything
      -- else keeps git's own words, because then it is not a move.
      moved <- lift (run whenMs "update-ref"
                         [ "update-ref", Text.unpack metaRef, Text.unpack commit
                         , maybe "" Text.unpack (cnParent cw) ]
                         mempty)
      case moved of
        Right _ -> pure ()
        Left e  -> do
          now' <- lift parentOf
          case now' of
            Right n | n /= cnParent cw -> throwError (RefMoved (cnParent cw) n)
            _                          -> throwError e

      pure commit

  dec = Text.decodeUtf8Lenient

  -- Two calls, because canon that does not exist yet and canon that will not
  -- resolve are not the same answer and the writer must do different things
  -- with them: the first is the FIRST accept and commits an orphan root, the
  -- second is a repository whose canon is broken and must not be committed
  -- onto at all.
  --
  -- One call could not tell them apart. @rev-parse --verify --quiet@ exits 1
  -- for an absent ref, for a garbage loose ref, for a ref naming a blob and
  -- for a ref whose object is gone alike (the reader's own measured table says
  -- so), and taking all four as "no canon yet" would have this writer commit a
  -- second root and then fail the ref move with a message about a ref it said
  -- was missing.
  parentOf =
    raw 0 "show-ref" ["show-ref", "--verify", "--quiet", Text.unpack metaRef] mempty >>= \case
      Left e -> pure (Left e)
      Right (ExitFailure 1, _, _) -> pure (Right Nothing)
      Right (ExitFailure c, _, said) -> pure (Left (refusal "show-ref" c said))
      Right (ExitSuccess, _, _) ->
        raw 0 "rev-parse" ["rev-parse", "--verify", Text.unpack metaRef <> "^{commit}"] mempty
          <&> \case
            Left e -> Left e
            Right (ExitSuccess, out, _) -> Right (Just (Text.strip (dec out)))
            Right (ExitFailure c, _, said) -> Left (refusal "rev-parse" c said)

  blobOf whenMs (_, content) =
    run whenMs "hash-object" ["hash-object", "-w", "--stdin"] (LBS.fromStrict content)
      <&> fmap (Text.strip . dec)

  -- The stdin update-index reads: mode, object id, path, NUL-terminated.
  --
  -- Assembled as BYTES from the path's own bytes. The mode is 100644 for every
  -- file: canon holds text this build renders, and nothing in PEP-19's layout
  -- is executable, a symlink or a submodule.
  indexInfo entries = BS.concat
    [ "100644 " <> Text.encodeUtf8 oid <> "\t" <> path <> "\0"
    | (path, oid) <- entries ]

  -- What each git call is given beyond the inherited environment.
  --
  -- The index is this writer's; the identity is 'canonAuthor'; the dates come
  -- from the caller's stamp so the commit id is a function of canon rather than
  -- of the clock. All of them go through 'gitIn', so the rules about which
  -- repository a command lands in stay in one place.
  extraEnv when' =
    [ ("GIT_INDEX_FILE", indexFile)
    , ("GIT_AUTHOR_NAME", fst canonAuthor)
    , ("GIT_AUTHOR_EMAIL", snd canonAuthor)
    , ("GIT_COMMITTER_NAME", fst canonAuthor)
    , ("GIT_COMMITTER_EMAIL", snd canonAuthor)
    , ("GIT_AUTHOR_DATE", stamp when')
    , ("GIT_COMMITTER_DATE", stamp when')
    ]

  -- Milliseconds in, git's raw date out. UTC, always: an offset taken from the
  -- machine would put two clones' commits at different ids for one canon.
  --
  -- THE @ IS NOT DECORATION. Without it git parses the digits as a date string
  -- rather than as an epoch: a small number is "invalid date format" and a
  -- ten-digit one is accepted as something else entirely, so the failure is
  -- loud for a fixture and silent for a real stamp.
  stamp ms = "@" <> show (ms `div` 1000) <> " +0000"

  -- The common case: git ran, and a non-zero exit is a refusal.
  run :: Word64 -> Text -> [String] -> LBS.ByteString -> m (Either CanonUnwritable ByteString)
  run whenMs what args input = raw whenMs what args input <&> (>>= done what)

  done what = \case
    (ExitSuccess, out, _) -> Right out
    (ExitFailure c, _, e0) -> Left (refusal what c e0)

  -- What git said, and when it said nothing, that it said nothing and with
  -- which code. An empty quoted block is a refusal that names no reason, which
  -- is how "canon does not exist yet" first arrived here.
  refusal what c e0
    | Text.null said = WriterRefused what
        (ReaderSays ("exited " <> Text.pack (show c) <> " and said nothing"))
    | otherwise = WriterRefused what (ToolSaid said)
    where said = Text.dropWhileEnd (`elem` ("\r\n" :: String)) (dec e0)

  -- Git ran, and this is what it did. The two answers that are not about the
  -- command come from 'gitRun', which the bundle plumbing shares; a non-zero
  -- exit is the caller's to read, because for one caller here it is not a
  -- failure at all.
  raw :: Word64 -> Text -> [String] -> LBS.ByteString
      -> m (Either CanonUnwritable (ExitCode, ByteString, ByteString))
  raw whenMs what args input =
    gitRun cwd (extraEnv whenMs) callSeconds maxListingBytes what args input <&> \case
      Left (GitUnstartable e) -> Left (WriterFailed e)
      Left (GitStalled e)     -> Left (WriterStalled e)
      -- A listing of canon longer than the reader will take. Its own sentence
      -- and not a git refusal: git did what it was asked and this build is the
      -- one that stopped, which is what ReaderSays is for.
      Left (GitTooMuch w n _) -> Left (WriterRefused w (ReaderSays
            ("answered with more than " <> Text.pack (show n) <> " bytes")))
      Right r                 -> Right r

-- | read-tree's refusal, as the state it actually describes.
--
-- Only that ONE command's, and only its refusal: a read-tree that could not be
-- started or that stalled is what it says it is, and neither of those is a
-- statement about what canon holds.
unindexable :: CanonUnwritable -> CanonUnwritable
unindexable = \case
  WriterRefused _ t -> WriterUnindexable t
  e                 -> e
