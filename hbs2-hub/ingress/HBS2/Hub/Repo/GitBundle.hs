{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
-- | The git side of what a hub does (PEP-20 "Two submission paths", "Fetch and
-- verify"; PEP-19 for the refs).
--
-- The delta path, as far as git is concerned: build a bundle of
-- @base..source-ref@, take a stranger's bundle in, check that the signed base
-- really is an ancestor of the signed tip, and stage the proposed tip under
-- @refs\/hbs2\/pulls\/<n>\/head@.
--
-- And, since they are the same plumbing, the two operations that move those
-- refs about afterwards: putting a staged proposal on a branch, and fetching
-- code and canon from a remote. They live here rather than in a module of their
-- own because the timeouts, the refusal type and the name checks are here, and
-- a second copy of those is a second answer to what this build will hand git.
--
-- WHAT ARRIVES HERE IS SIGNED, NOT TRUSTED. A ref name and a sha come out of a
-- contributor's inner box: the signature says who wrote them, and says nothing
-- about what they are. Both reach a git command line, and git reads a leading
-- dash as an option, so @source-ref@ of @--upload-pack=sh -c ...@ would be a
-- signed letter that runs a program. Every one of them is checked against a
-- shape before it is passed, and the check is deliberately narrower than what
-- git would accept: this is the set of names a branch has, not the set a ref
-- may have.
--
-- FSCK IS ON, and it is off by default in git. The objects in a bundle are
-- bytes a stranger produced, and git's own history has malformed-object
-- vulnerabilities in it; @transfer.fsckObjects@ is what makes it refuse them
-- rather than write them into the maintainer's repository.
module HBS2.Hub.Repo.GitBundle
  ( bundleRange
  , acceptBundle
  , isAncestor
  , mergeBase
  , stagePull
  , pullTip
  , refTip
  , checkoutBranch
  , syncFrom
  , Synced(..)
  , SyncedCanon(..)
  , pullRef
  , Bundled(..)
  , BundleError(..)
  , validRefName
  , validSha
  ) where

import HBS2.Hub.Types (safeText)
import HBS2.Hub.Repo (Told(..), told)
import HBS2.Hub.Repo.Git (gitRun, GitTrouble(..))

import HBS2.CLI.Prelude hiding (filter)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Control.Monad.Except (ExceptT(..),runExceptT,throwError)
import Data.Char (isHexDigit)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word8,Word64)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process.Typed (ExitCode(..))

-- | A bundle, and the claim it can be checked against.
data Bundled = Bundled
  { bnBytes :: ByteString
    -- | The tip the bundle's recorded ref points at.
    --
    -- Reported rather than left for the caller to look up, because this is the
    -- value that goes into the signed box as @source-tip@ and the value the
    -- maintainer checks the fetched tip against. Deriving it twice, once here
    -- and once there, is how the two come to disagree.
  , bnTip   :: Text
  }
  deriving stock (Eq,Show)

data BundleError =
    BundleUnstartable Text
  | BundleStalled Text
    -- | git ran and refused, with which command and what it said.
  | BundleRefused Text Told
    -- | There is nothing between @base@ and @source-ref@.
    --
    -- Its own case because git says it in prose ("Refusing to create empty
    -- bundle") and because it is the ordinary mistake, not a failure: a
    -- contributor who has not committed yet, or who named the branch they are
    -- already on as the base.
  | BundleEmpty
    -- | A name or a sha that this will not hand to git. Carries what was
    -- offered, escaped by the printer.
  | BundleBadName Text Text
    -- | The bundle fetched, and its tip is not the one the letter signed.
  | BundleTipMismatch Text Text
  deriving stock (Eq,Show)

instance Pretty BundleError where
  pretty = \case
    BundleUnstartable e -> "could not run git:" <+> pretty e
    BundleStalled e     -> "git did not finish:" <+> pretty e
    BundleRefused what t -> nest 2 $ vsep [ "git" <+> pretty what <+> "refused", told t ]
    BundleEmpty ->
      nest 2 $ vsep [ "there is nothing between the base and the source ref"
                    , "git will not build an empty bundle; commit first, or name"
                        <+> "the base the branch actually forked from" ]
    BundleBadName what v ->
      nest 2 $ vsep [ "not a" <+> pretty what <+> "this will pass to git:"
                        <+> pretty (safeText v)
                    , "a signed letter says who wrote a name, not what it is,"
                        <+> "and git reads a leading dash as an option" ]
    BundleTipMismatch want got ->
      nest 2 $ vsep [ "the bundle does not carry what the letter signed for"
                    , "signed" <+> pretty want
                    , "fetched" <+> pretty got
                    , "the objects are not the ones the contributor put their"
                        <+> "name to; do not stage them" ]

-- | Where PEP-19 stages a proposed tip.
pullRef :: Word64 -> Text
pullRef n = "refs/hbs2/pulls/" <> Text.pack (show n) <> "/head"

-- | A ref name this will pass to git.
--
-- Narrower than @git check-ref-format@ on purpose. That accepts a great deal,
-- including names this has no reason to carry, and running it would mean
-- putting the name on a command line to find out whether the name may go on a
-- command line. This is the shape a branch or a tag has.
validRefName :: Text -> Bool
validRefName r =
  not (Text.null r)
    && Text.length r <= 255
    && Text.all ok r
    && not ("-" `Text.isPrefixOf` r)
    && not ("/" `Text.isPrefixOf` r)
    && not (".." `Text.isInfixOf` r)
    && not ("//" `Text.isInfixOf` r)
    && not ("." `Text.isSuffixOf` r)
    && not (".lock" `Text.isSuffixOf` r)
  where ok c = c `elem` ("/_-." :: String)
                 || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
                 || (c >= '0' && c <= '9')

-- | An object name this will pass to git: hex, and the length a hash is.
--
-- Both lengths, because a repository may be sha1 or sha256 and refusing the
-- second would refuse a whole class of repository over a number.
validSha :: Text -> Bool
validSha s = Text.length s `elem` [40, 64] && Text.all isHexDigit s

-- | Build a bundle of @base..source-ref@ (PEP-20 delta path).
--
-- The RANGE USES A REF NAME and not the sha, which is not a preference: @git
-- bundle@ refuses to build a bundle that records no ref, and a bare sha is not
-- a ref, so @base..source-tip@ fails outright. The recorded ref is what the
-- maintainer's fetch names on the other side.
bundleRange :: MonadUnliftIO m
            => Maybe FilePath -> Text -> Text -> m (Either BundleError Bundled)
bundleRange cwd base ref = runExceptT do
  checked "object name" validSha base
  checked "ref name" validRefName ref

  -- To stdout, so the bytes never touch a file this would then have to clean
  -- up. A bundle of a real range is megabytes, which is what the caller is
  -- about to encrypt and put in a message anyway.
  -- LC_ALL=C, and only here. 'orEmpty' below reads git's PROSE, which is the
  -- only signal git gives for an empty range (there is no exit code for it),
  -- and prose is translated: the same refusal is "Refusing to create empty
  -- bundle" under C, "Отклонение создания пустого пакета" under ru_RU and
  -- "Erstellung eines leeren Pakets zurückgewiesen" under de_DE, so a
  -- maintainer with a localised shell got a raw fatal: line instead of the
  -- advice this error carries. Pinned for this ONE call rather than in
  -- 'forcedEnv', because everywhere else git's own words are shown to a human
  -- and translating them for that human is the right behaviour.
  out <- ExceptT $ callWith [("LC_ALL","C")] cwd bundleSeconds "bundle create"
           ["bundle", "create", "-", Text.unpack base <> ".." <> Text.unpack ref]
           `orEmpty` "empty bundle"

  tip <- ExceptT $ oneLine cwd "rev-parse" ["rev-parse", Text.unpack ref <> "^{commit}"]
  pure (Bundled out tip)

-- | Take a stranger's bundle into this repository, against the signed claim.
--
-- Two calls before the fetch, and both matter. @bundle verify@ is what says
-- whether the prerequisites are present: without it a bundle that forked from
-- a commit this repository does not have fails inside the fetch, with a
-- message about the fetch. And fsck is turned on explicitly, twice, because
-- git leaves it off and these are a stranger's objects.
--
-- THE SIGNED TIP IS AN ARGUMENT, not something the caller checks afterwards.
-- It is step 2 of PEP-20's "Fetch and verify" and it is the step the whole
-- delta path rests on: git's object hashing binds the content to the tip, so
-- a bundle that produces the signed tip is the objects the contributor put
-- their name to, and one that does not is somebody else's. A check the caller
-- performs is a check the caller can omit, and the caller that omits it stages
-- unsigned objects under a number.
acceptBundle :: MonadUnliftIO m
             => Maybe FilePath
             -> ByteString      -- ^ the bundle
             -> Text            -- ^ @source-ref@, from the signed box
             -> Text            -- ^ @source-tip@, from the signed box
             -> m (Either BundleError Text)
acceptBundle cwd bytes ref signedTip = runExceptT do
  checked "ref name" validRefName ref
  checked "object name" validSha signedTip

  -- git will not read a bundle from a pipe: it seeks in it. So it goes to a
  -- file, in a directory of this call's own that goes away with it.
  ExceptT $ withSystemTempDirectory "hbs2-hub-bundle" $ \tmp -> runExceptT do
    let path = tmp </> "pr.bundle"
    liftIO (BS.writeFile path bytes)

    _ <- ExceptT $ call cwd bundleSeconds "bundle verify" ["bundle", "verify", path]

    -- THE FETCH LANDS IN A QUARANTINE FIRST, and this is not tidiness. A fetch
    -- that fails its connectivity check, and a bundle whose tip is not the one
    -- the letter signed, both leave their pack in the maintainer's object store
    -- anyway: git writes the objects and then refuses, and unreachable objects
    -- outlive `git gc` by its two-week grace. So every refused pull request was
    -- disk a stranger chose, for a fortnight, in somebody else's repository.
    --
    -- GIT_OBJECT_DIRECTORY is where new objects go and the repository's own
    -- store is an alternate, so the prerequisites still resolve and nothing
    -- written here is visible to anybody else. When it all checks out the fetch
    -- is repeated without the quarantine, which costs one re-index of a pack
    -- whose size is the delta and is the price of never writing a refused one.
    objects <- ExceptT $ oneLine cwd "rev-parse" ["rev-parse", "--git-path", "objects"]
    let quarantine = tmp </> "objects"
        walled = [ ("GIT_OBJECT_DIRECTORY", quarantine)
                 , ("GIT_ALTERNATE_OBJECT_DIRECTORIES", Text.unpack objects)
                 ]
        fetch env = callWith env cwd bundleSeconds "fetch"
                      [ "-c", "transfer.fsckObjects=true"
                      , "-c", "fetch.fsckObjects=true"
                      , "fetch", path, Text.unpack ref ]

    liftIO (createDirectoryIfMissing True quarantine)

    _ <- ExceptT (fetch walled)

    -- FETCH_HEAD, which is what the fetch above just wrote, and not the ref
    -- name: fetching a bundle does not create a local branch, and resolving
    -- the name would find whatever this repository happens to have under it.
    -- That is the whole difference between checking what arrived and checking
    -- what was already here.
    --
    -- Resolved INSIDE the quarantine, because that is where the objects it
    -- names are; outside it the commit does not exist yet.
    got <- ExceptT $ callWith walled cwd smallSeconds "rev-parse"
                       ["rev-parse", "FETCH_HEAD^{commit}"]
             <&> fmap (Text.strip . Text.decodeUtf8Lenient)

    when (got /= signedTip) $ throwError (BundleTipMismatch signedTip got)

    -- Only now, into the repository proper.
    _ <- ExceptT (fetch [])
    pure got

-- | Where two refs forked, which is the @base@ a PR is a delta against.
--
-- Computed rather than asked for, because a contributor who has to name it
-- names it wrong: too old makes the bundle the whole history, too new makes a
-- bundle the maintainer cannot apply. It is still overridable, since a
-- contributor rebasing onto something the maintainer does not have yet knows
-- something git does not.
mergeBase :: MonadUnliftIO m
          => Maybe FilePath -> Text -> Text -> m (Either BundleError Text)
mergeBase cwd a b = runExceptT do
  checked "ref name" validRefName a
  checked "ref name" validRefName b
  ExceptT $ oneLine cwd "merge-base" ["merge-base", Text.unpack a, Text.unpack b]

-- | Is @a@ an ancestor of @b@?
--
-- On the delta path the bundle's construction guarantees it and this is a
-- second opinion; on the fork path nothing guarantees it, and PEP-20 requires
-- the check there. Exit 1 is the answer "no", not a failure, which is the
-- distinction that makes this its own function rather than a call site.
isAncestor :: MonadUnliftIO m
           => Maybe FilePath -> Text -> Text -> m (Either BundleError Bool)
isAncestor cwd a b = runExceptT do
  checked "object name" validSha a
  checked "object name" validSha b
  r <- ExceptT (run cwd smallSeconds "merge-base"
                    ["merge-base", "--is-ancestor", Text.unpack a, Text.unpack b])
  case r of
    (ExitSuccess, _, _)     -> pure True
    (ExitFailure 1, _, _)   -> pure False
    (ExitFailure c, _, e0) -> throwError (refusal "merge-base" c e0)

-- | What @refs\/hbs2\/pulls\/\<n\>\/head@ points at, or nothing if it is not
-- there.
--
-- The old side of 'stagePull's compare-and-swap, for the case that HAS one. An
-- @open@ stages a ref that must not exist yet and passes 'Nothing'; a @revise@
-- MOVES a ref that does, and 'Nothing' there is git's "must not exist", so the
-- revise could not have staged whatever else was right.
--
-- Absent is an ANSWER and not a failure: it is the ordinary state before a
-- first stage, and it is what @--quiet@ is for.
pullTip :: MonadUnliftIO m
        => Maybe FilePath -> Word64 -> m (Either BundleError (Maybe Text))
pullTip cwd n = refTip cwd (pullRef n)

-- | What any ref points at, or nothing if it is not there.
--
-- 'pullTip' generalized when a second caller appeared: checking out a proposal
-- has to ask what a BRANCH holds before it will touch it, and that is the same
-- question about a different name.
--
-- Absent is an ANSWER and not a failure -- the ordinary state of a branch
-- nobody has made yet -- which is what @--quiet@ is for.
refTip :: MonadUnliftIO m
       => Maybe FilePath -> Text -> m (Either BundleError (Maybe Text))
refTip cwd ref = runExceptT do
  checked "ref name" validRefName ref
  r <- ExceptT (run cwd smallSeconds "rev-parse"
                    [ "rev-parse", "--verify", "--quiet"
                    , Text.unpack ref <> "^{commit}" ])
  case r of
    (ExitSuccess, out, _) -> pure (nonEmpty (Text.strip (Text.decodeUtf8Lenient out)))
    (ExitFailure 1, _, _)  -> pure Nothing
    (ExitFailure c, _, e0) -> throwError (refusal "rev-parse" c e0)
  where
    nonEmpty t = if Text.null t then Nothing else Just t

-- | Put a branch at a commit and switch to it, without discarding anything.
--
-- THREE STATES AND ONLY ONE OF THEM WRITES A BRANCH, because the fourth thing
-- git offers -- @checkout -B@, which moves a branch wherever you say -- would
-- silently discard commits somebody made on that name. A reviewer's local work
-- is not this verb's to throw away, and a name it will not move is a name they
-- can pick again with a flag.
--
-- The switch itself is git's to refuse: an uncommitted change that would be
-- clobbered stops it, and that refusal is the caller's to print.
checkoutBranch :: MonadUnliftIO m
               => Maybe FilePath -> Text -> Text -> m (Either BundleError ())
checkoutBranch cwd branch tip = runExceptT do
  checked "ref name" validRefName branch
  checked "object name" validSha tip
  here <- ExceptT (refTip cwd branch)
  case here of
    Nothing ->
      void $ ExceptT $ call cwd smallSeconds "checkout"
        ["checkout", "-b", Text.unpack branch, Text.unpack tip]
    Just was | was == tip ->
      void $ ExceptT $ call cwd smallSeconds "checkout"
        ["checkout", Text.unpack branch]
    Just was -> throwError (BundleTipMismatch tip was)

-- | What one sync did (PEP-19 "refs/hbs2/meta", PEP-22 @hub sync@).
--
-- A record rather than a line of output, so the rendering is somebody else's
-- and a test can ask what happened without reading a terminal.
data Synced = Synced
  { syCanon :: SyncedCanon
    -- | Whether the pull refs were fetched. False only when the fetch itself
    -- failed, which is reported as an error instead.
  , syPulls :: Bool
  }
  deriving stock (Eq,Show)

-- | What happened to this clone's copy of canon.
data SyncedCanon =
    CanonNone              -- ^ the remote has none: nobody has folded anything
  | CanonSame              -- ^ the same commit is already here
  | CanonMoved Text Text   -- ^ from, to: a fast-forward
    -- | The two have diverged and NOTHING was written. Local, then remote.
    --
    -- The case this whole function exists to make visible: a maintainer who has
    -- accepted a letter holds canon the remote has not seen, and a forced fetch
    -- would replace it. The events survive in the object store either way, but
    -- the ref is what every reader follows.
  | CanonDiverged Text Text
  deriving stock (Eq,Show)

-- | Fetch code and canon from a remote.
--
-- THE CODE FETCH AND THE HUB REFS ARE TWO CALLS because giving git a refspec
-- replaces the configured one: a single call with @refs/hbs2/*@ in it fetches
-- the hub namespace and no branches at all.
--
-- CANON IS NOT FORCED, which is where this stops being a wrapper. PEP-22 writes
-- the refspec with a plus, and a plus here replaces the canon of whoever runs
-- it: a maintainer who has just accepted a letter holds a commit the remote has
-- not seen. So the remote's tip is fetched into FETCH_HEAD, compared, and the
-- ref is moved only when the move is a fast-forward. A divergence is reported
-- and left alone.
--
-- THE PULL REFS ARE forced, and the asymmetry is the point: they are a staging
-- cache of what a maintainer published, nobody commits on them, and the verb
-- that writes them prints the command to redo one.
syncFrom :: MonadUnliftIO m
         => Maybe FilePath -> Text -> m (Either BundleError Synced)
syncFrom cwd remote = runExceptT do
  checked "remote name" validRefName remote

  -- The branches, by whatever refspec the remote is configured with.
  _ <- ExceptT $ call cwd fetchSeconds "fetch" ["fetch", Text.unpack remote]

  -- ASKED FOR FIRST, because git fails outright on a NAMED ref the remote does
  -- not have ("couldn't find remote ref"), and a repository where nobody has
  -- folded anything yet is the ordinary state of a new one rather than an
  -- error. A wildcard would be quiet, and would also mirror whatever else ever
  -- appears under refs/hbs2; ls-remote asks the question that is being asked.
  probe <- ExceptT $ call cwd fetchSeconds "ls-remote"
             ["ls-remote", Text.unpack remote, "refs/hbs2/meta"]

  here <- ExceptT (refTip cwd "refs/hbs2/meta")

  canon <- if BS.null (BS.filter (not . isSpace8) probe) then pure CanonNone else do
    -- Canon, into FETCH_HEAD only: a source-only refspec updates no local ref,
    -- which is what makes the comparison below possible at all.
    _ <- ExceptT $ call cwd fetchSeconds "fetch"
           ["fetch", Text.unpack remote, "refs/hbs2/meta"]

    there <- ExceptT (refTip cwd "FETCH_HEAD")

    case (here, there) of
      -- ls-remote said it was there and FETCH_HEAD says it is not, which is a
      -- ref that vanished between two calls. Reported as absent because that
      -- is what this clone can see; the next run will say the same or move on.
      (_, Nothing) -> pure CanonNone
      (Nothing, Just t) -> do
        _ <- ExceptT $ call cwd smallSeconds "update-ref"
               ["update-ref", "refs/hbs2/meta", Text.unpack t]
        pure (CanonMoved "" t)
      (Just a, Just b) | a == b -> pure CanonSame
      (Just a, Just b) -> do
        ff <- ExceptT (isAncestor cwd a b)
        if not ff then pure (CanonDiverged a b)
          else do
            -- Through what it currently holds, like every other ref this
            -- package moves: between the read and the write another process
            -- may have folded something.
            _ <- ExceptT $ call cwd smallSeconds "update-ref"
                   ["update-ref", "refs/hbs2/meta", Text.unpack b, Text.unpack a]
            pure (CanonMoved a b)

  -- And the staged proposals. A wildcard, so a remote with none is quiet
  -- rather than an error: git fails on a named ref it cannot find and says
  -- nothing about a pattern that matches nothing.
  _ <- ExceptT $ call cwd fetchSeconds "fetch"
         [ "fetch", Text.unpack remote, "+refs/hbs2/pulls/*:refs/hbs2/pulls/*" ]

  pure (Synced canon True)

-- | Stage a proposed tip where PEP-19 puts it.
--
-- Compare-and-swap, like the canon ref and for the same reason: a pull ref
-- that moved under us means somebody else staged this number, and overwriting
-- it would replace one contributor's proposal with another's under one number.
--
-- @old@ is what the ref is expected to hold: 'Nothing' means it must not exist
-- (a first stage), and 'Just' is a move (see 'pullTip').
stagePull :: MonadUnliftIO m
          => Maybe FilePath -> Word64 -> Text -> Maybe Text
          -> m (Either BundleError ())
stagePull cwd n tip old = runExceptT do
  checked "object name" validSha tip
  for_ old (checked "object name" validSha)
  _ <- ExceptT $ call cwd smallSeconds "update-ref"
         [ "update-ref", Text.unpack (pullRef n), Text.unpack tip
         , maybe "" Text.unpack old ]
  pure ()

-- Bundling and fetching walk history; the rest are single lookups.
bundleSeconds, fetchSeconds, smallSeconds :: Int
bundleSeconds = 600
-- A fetch is a network operation over whatever transport the remote names,
-- which for hbs23:// is a peer talking to the network. Bounded like the bundle
-- and for the same reason: the alternative is a verb that hangs.
fetchSeconds  = 600
smallSeconds  = 60

-- A value that will not go to git is refused before anything runs.
checked :: Monad m => Text -> (Text -> Bool) -> Text -> ExceptT BundleError m ()
checked what ok v = unless (ok v) (throwError (BundleBadName what v))

run :: MonadUnliftIO m
    => Maybe FilePath -> Int -> Text -> [String]
    -> m (Either BundleError (ExitCode, ByteString, ByteString))
run = runWith []

-- | The same, with environment this call needs and the others must not have.
runWith :: MonadUnliftIO m
        => [(String,String)] -> Maybe FilePath -> Int -> Text -> [String]
        -> m (Either BundleError (ExitCode, ByteString, ByteString))
runWith env cwd secs what args =
  gitRun cwd env secs what args mempty <&> \case
    Left (GitUnstartable e) -> Left (BundleUnstartable e)
    Left (GitStalled e)     -> Left (BundleStalled e)
    Right r                 -> Right r

-- The common case: a non-zero exit is a refusal, and stdout is the answer.
call :: MonadUnliftIO m
     => Maybe FilePath -> Int -> Text -> [String] -> m (Either BundleError ByteString)
call = callWith []

callWith :: MonadUnliftIO m
         => [(String,String)] -> Maybe FilePath -> Int -> Text -> [String]
         -> m (Either BundleError ByteString)
callWith env cwd secs what args =
  runWith env cwd secs what args <&> \case
    Left e -> Left e
    Right (ExitSuccess, out, _)     -> Right out
    Right (ExitFailure c, _, e0)    -> Left (refusal what c e0)

-- The same, for a command whose answer is one line.
oneLine :: MonadUnliftIO m
        => Maybe FilePath -> Text -> [String] -> m (Either BundleError Text)
oneLine cwd what args =
  call cwd smallSeconds what args <&> fmap (Text.strip . Text.decodeUtf8Lenient)

-- git says this one in prose, so it is read out of the prose. Narrowly: only
-- when the words are there, so a different refusal keeps its own words.
orEmpty :: Functor f => f (Either BundleError a) -> Text -> f (Either BundleError a)
orEmpty act needle = fmap f act
  where
    f (Left (BundleRefused _ (ToolSaid said)))
      | needle `Text.isInfixOf` Text.toLower said = Left BundleEmpty
    f other = other

refusal :: Text -> Int -> ByteString -> BundleError
refusal what c e0
  | Text.null said = BundleRefused what
      (ReaderSays ("exited " <> Text.pack (show c) <> " and said nothing"))
  | otherwise = BundleRefused what (ToolSaid said)
  where said = Text.dropWhileEnd (`elem` ("\r\n" :: String)) (Text.decodeUtf8Lenient e0)

-- Whitespace, as a byte. git answers ls-remote with either nothing or a line,
-- and "nothing" arrives as an empty string or a bare newline depending on the
-- transport.
isSpace8 :: Word8 -> Bool
isSpace8 w = w `elem` [0x20, 0x09, 0x0a, 0x0d]
