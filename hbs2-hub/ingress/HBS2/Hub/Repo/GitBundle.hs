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
  , takeCanon
  , Synced(..)
  , SyncedCanon(..)
  , publishTo
  , remoteCanon
  , Published(..)
  , PublishedCanon(..)
  , PublishedPulls(..)
  , pullRef
  , Bundled(..)
  , BundleError(..)
  , validRefName
  , validSha
  , validAbbrevSha
  , resolveCommit
  , addedBytes
  ) where

import HBS2.Hub.Types (safeText,validRefName,validSha,validAbbrevSha)
import HBS2.Hub.Repo (Told(..), told)
import HBS2.Hub.Repo.Git (gitRun, GitTrouble(..))

import HBS2.CLI.Prelude hiding (filter)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Control.Monad.Except (ExceptT(..),runExceptT,throwError)
import Data.Either (isLeft)
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
    -- | The bundle records a tip that is not the one the letter signed.
    --
    -- Read from the bundle HEADER, before anything is fetched, so a mismatched
    -- bundle writes nothing at all -- not even into the quarantine.
    --
    -- Carries the signed tip and then the recorded one.
  | BundleTipMismatch Text Text
    -- | The bundle records the signed tip and carries none of its objects.
    --
    -- Its own case because it is the one refusal here that is not a mistake. A
    -- v2 header naming any commit the maintainer already has, followed by an
    -- empty pack, is 106 bytes that passes @bundle verify@ ("records a complete
    -- history"), fetches with exit 0, and resolves to exactly the signed tip
    -- through the repository's own store. What it proposes is somebody else's
    -- work, or the maintainer's own unpublished commits, and staging it puts
    -- them under a number and pushes them on the next publish.
  | BundleNoObjects Text
    -- | The branch fetch moved @refs\/hbs2\/meta@ out from under this verb.
    --
    -- The first thing `hub sync` does is fetch the code, and it does that by
    -- whatever refspec the remote is configured with, which this verb does not
    -- choose. A mirror clone, or any remote configured @+refs\/*:refs\/*@, has
    -- canon inside that refspec -- so the careful never-force logic below it ran
    -- against a ref the fetch had already forced, and reported @CanonSame@
    -- about a rollback. With @fetch.prune@ and no canon on the remote the ref is
    -- DELETED, and the verb then says "the remote has none, so nothing here
    -- folds yet" having just removed the only copy.
    --
    -- Carries what it was and what it became, because the way back is a ref
    -- update to the first of those and nothing else is lost: the objects are
    -- still in the repository.
  | BundleCanonClobbered Text (Maybe Text)
    -- | The signed base is not an ancestor of the signed tip.
    --
    -- The letter says its bundle is the range @base..source-ref@, and this is
    -- that claim checked rather than believed: the construction is the
    -- CONTRIBUTOR's, and a range that is not one puts objects under a number
    -- that no fork point explains.
    --
    -- Carries both, because the pair is the claim.
  | BundleNotAncestor Text Text
    -- | The bundle records something other than one ref.
    --
    -- PEP-20's delta path is a bundle of @base..source-ref@, and git records
    -- the one ref that names. Its own case because the alternative -- matching
    -- the letter's short name against git's fully-qualified one to pick among
    -- several -- is this build guessing at git's refspec rules, on a value a
    -- stranger chose.
  | BundleNotOneRef Int
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
                    , "signed  " <+> pretty want
                    , "recorded" <+> pretty got
                    , "the objects are not the ones the contributor put their"
                        <+> "name to; do not stage them" ]
    BundleCanonClobbered was now ->
      nest 2 $ vsep [ "fetching the code moved canon, and this verb did not"
                    , "was  " <+> pretty was
                    , "now  " <+> maybe "gone" pretty now
                    , "this remote's refspec covers refs/hbs2/*, so the branch"
                        <+> "fetch forced or pruned"
                    , "the ref before anything here could compare it. Nothing"
                        <+> "else was written and the"
                    , "objects are still here. Put it back with:"
                    , "  git update-ref refs/hbs2/meta" <+> pretty was
                    , "then narrow the refspec, or fetch canon with hbs2-hub"
                        <+> "alone." ]
    BundleNoObjects tip ->
      nest 2 $ vsep [ "the bundle names a commit it does not carry"
                    , "signed" <+> pretty (safeText tip)
                    , "that commit is already in this repository, and the"
                        <+> "bundle brought nothing:"
                    , "the proposal is somebody else's work, or your own"
                        <+> "unpublished commits. Do not stage it." ]
    BundleNotOneRef n ->
      nest 2 $ vsep [ "the bundle records" <+> pretty n <+> "refs, and a"
                        <+> "proposal is one"
                    , "a delta is a bundle of base..source-ref, which records"
                        <+> "exactly one; this is not that shape and nothing"
                    , "was taken into this repository." ]
    BundleNotAncestor base tip ->
      nest 2 $ vsep [ "the signed base is not an ancestor of the signed tip"
                    , "base" <+> pretty (safeText base)
                    , "tip " <+> pretty (safeText tip)
                    , "the range is not what the letter says it is; nothing"
                        <+> "was taken into this repository." ]

-- | Where PEP-19 stages a proposed tip.
pullRef :: Word64 -> Text
pullRef n = "refs/hbs2/pulls/" <> Text.pack (show n) <> "/head"

-- The three name shapes moved to "HBS2.Hub.Types" and are re-exported here,
-- because every caller in this module already imports it from here. They moved
-- because the gate that decides what a signed LETTER may carry is in the pure
-- library and could not reach them: see 'HBS2.Hub.Letter.malformedName'.

-- | Turn one of those into the whole object name it means.
--
-- @^{commit}@ rather than a bare rev-parse, for the reason 'acceptBundle' peels
-- the same way: a tag or a tree resolves happily and is not a commit, and the
-- caller is about to assert that something is an ancestor of it.
--
-- Ambiguity is git's to report and it does, with exit 128 and prose naming the
-- candidates, which is more use than anything this could say.
resolveCommit :: MonadUnliftIO m
              => Maybe FilePath -> Text -> m (Either BundleError Text)
resolveCommit cwd name = runExceptT do
  checked "object name" validAbbrevSha name
  out <- ExceptT $ call cwd smallSeconds "rev-parse"
           ["rev-parse", "--verify", "--end-of-options"
           , Text.unpack name <> "^{commit}"]
  let full = Text.strip (Text.decodeUtf8Lenient out)
  if validSha full then pure full else throwError (BundleBadName "object name" full)

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
-- THE SIGNED TIP AND THE SIGNED BASE ARE ARGUMENTS, not something the caller
-- checks afterwards.
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
             -> Text            -- ^ @base@, the fork point the letter signed
             -> m (Either BundleError Text)
acceptBundle cwd bytes ref signedTip base = runExceptT do
  checked "ref name" validRefName ref
  checked "object name" validSha signedTip
  checked "object name" validSha base

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

    -- WHAT THE BUNDLE RECORDS, read out of the file itself.
    --
    -- This was @rev-parse FETCH_HEAD@ after the fetch, and FETCH_HEAD is the
    -- problem: the quarantine covers OBJECTS, and FETCH_HEAD is a file in the
    -- real git dir that every fetch in this repository rewrites. Between the
    -- fetch and the read sat a second process -- a concurrent `hub inbox
    -- accept`, a `hub sync`, or the operator's own @git fetch@ in another
    -- terminal -- and nothing in this package takes a lock. What the read
    -- returned was then somebody else's ref, compared against this letter's
    -- signed tip.
    --
    -- The bundle file is in a temporary directory of this call's own, so
    -- reading the claim from THERE is not shared with anything. It is also
    -- earlier: a bundle whose tip is not the signed one is now refused before
    -- a single object is written, quarantine or not.
    --
    -- EXACTLY ONE RECORDED REF, which is the shape PEP-20's delta path
    -- produces: 'bundleRange' builds @base..source-ref@ and git records the
    -- one ref that names. A bundle recording several is not that shape, and
    -- picking one of them by matching the letter's short name against git's
    -- fully-qualified one would be this build guessing at git's own refspec
    -- rules.
    heads <- ExceptT $ call cwd smallSeconds "bundle list-heads"
                         ["bundle", "list-heads", path]

    got <- case recordedTips heads of
             [t] -> pure t
             ts  -> throwError (BundleNotOneRef (length ts))

    when (got /= signedTip) $ throwError (BundleTipMismatch signedTip got)

    -- AND THAT THE OBJECTS ARRIVED, which the check above does not establish
    -- and the module header used to claim it did. Reading the tip out of the
    -- bundle header says what the bundle CLAIMS, and a claim costs nothing to
    -- make: a bundle with an empty pack, whose only content is a v2 header
    -- naming a commit this repository already holds, passes
    -- @bundle verify@, fetches with exit 0, and produces exactly the signed tip
    -- having transferred nothing. What it proposes is then whatever the
    -- attacker named -- another contributor's accepted tip, or a merge commit
    -- the maintainer made locally and has not pushed, which `hub publish` will
    -- push because staging a ref pushes everything reachable from it.
    --
    -- So the question is asked of the QUARANTINE ALONE, with no alternate: is
    -- the object one this fetch wrote. An honest bundle always answers yes,
    -- since git packs the objects in @base..ref@ and refuses to build an empty
    -- bundle at all; a re-sent bundle for objects already here answers no, and
    -- that is a refusal worth having rather than a silent pass.
    let alone = [ ("GIT_OBJECT_DIRECTORY", quarantine) ]
    brought <- lift $ callWith alone cwd smallSeconds "cat-file"
                        ["cat-file", "-e", Text.unpack signedTip <> "^{commit}"]
    when (isLeft brought) $ throwError (BundleNoObjects signedTip)

    -- AND THE RANGE IS THE RANGE THE LETTER SIGNED, asked HERE and not by the
    -- caller.
    --
    -- The check itself is not new; where it ran was the defect. It sat after
    -- this function returned, which is after the second fetch, so a bundle
    -- whose base is not an ancestor of its tip had already been written into
    -- the maintainer's object store -- and unreachable objects outlive `git
    -- gc` by its two-week grace. Every refused proposal was a fortnight of
    -- disk a stranger chose, which is the exact cost the quarantine above
    -- exists to avoid, paid on the one path that reaches it.
    --
    -- Inside the quarantine, because the tip is only there; the base resolves
    -- through the alternate, since it is a commit this repository already has.
    -- See 'isAncestorWith'.
    anc <- ExceptT (isAncestorWith walled cwd base signedTip)
    unless anc $ throwError (BundleNotAncestor base signedTip)

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
isAncestor = isAncestorWith []

-- | The same, in an object store the caller names.
--
-- 'acceptBundle' asks this INSIDE the quarantine, where the tip exists and the
-- repository's own store is an alternate, so the base still resolves. Outside
-- it the answer would be about a commit that is not there yet.
isAncestorWith :: MonadUnliftIO m
               => [(String,String)] -> Maybe FilePath -> Text -> Text
               -> m (Either BundleError Bool)
isAncestorWith env cwd a b = runExceptT do
  checked "object name" validSha a
  checked "object name" validSha b
  r <- ExceptT (runWith env cwd smallSeconds "merge-base"
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
-- not seen. So the remote's tip is fetched by a source-only refspec, compared
-- against what the remote advertised, and the ref is moved only when the move is
-- a fast-forward. A divergence is reported
-- and left alone.
--
-- THE PULL REFS ARE forced, and the asymmetry is the point: they are a staging
-- cache of what a maintainer published, nobody commits on them, and the verb
-- that writes them prints the command to redo one.
syncFrom :: MonadUnliftIO m
         => Maybe FilePath -> Text -> m (Either BundleError Synced)
syncFrom cwd remote = runExceptT do
  checked "remote name" validRefName remote

  -- FSCK ON, on all three fetches below.
  --
  -- The same flags 'acceptBundle' sets and for the same reason it sets them:
  -- what arrives is a stranger's objects, git's own history has
  -- malformed-object vulnerabilities in it, and fsck is off by default. This
  -- path did not set them, which made it the one way into the object store
  -- that skipped the check -- and its objects are the ones @hub pr checkout@
  -- puts into a reviewer's WORKING TREE. A hostile upstream publishes both the
  -- canon coordinates and the pull ref, so the mismatch check @prCheckout@ does
  -- passes and only git's checkout-time defences are left.
  --
  -- Named once here rather than per call, since three fetches differing in
  -- whether they verify what they take would be three chances to pick the
  -- wrong one.
  let fetch as = call cwd fetchSeconds "fetch"
                   ( [ "-c", "transfer.fsckObjects=true"
                     , "-c", "fetch.fsckObjects=true"
                     , "fetch" ] <> as )

  -- WHAT CANON WAS BEFORE THE BRANCH FETCH. Read here rather than after it,
  -- because the fetch below runs under the remote's own refspec and this verb
  -- does not choose that: on a mirror clone, or any remote configured
  -- @+refs/*:refs/*@, it covers refs/hbs2/* and forces the ref that everything
  -- underneath is careful never to force. Reading afterwards meant comparing
  -- the remote against itself and answering CanonSame about a rollback.
  before <- ExceptT (refTip cwd "refs/hbs2/meta")

  -- The branches, by whatever refspec the remote is configured with.
  _ <- ExceptT $ fetch [Text.unpack remote]

  -- And whether it survived that. A FAST-FORWARD IS FINE and is left alone: on
  -- a mirror that fetch is how canon legitimately arrives, and refusing it would
  -- make this verb useless there. Anything else -- a rollback, a sideways move,
  -- or a prune that deleted the ref -- is the case worth stopping on, and it is
  -- stopped on before a single other ref is touched.
  for_ before $ \was -> do
    now <- ExceptT (refTip cwd "refs/hbs2/meta")
    kept <- case now of
              Nothing -> pure False
              Just b | b == was  -> pure True
                     | otherwise -> ExceptT (isAncestor cwd was b)
    unless kept $ throwError (BundleCanonClobbered was now)

  -- ASKED FOR FIRST, because git fails outright on a NAMED ref the remote does
  -- not have ("couldn't find remote ref"), and a repository where nobody has
  -- folded anything yet is the ordinary state of a new one rather than an
  -- error. A wildcard would be quiet, and would also mirror whatever else ever
  -- appears under refs/hbs2; ls-remote asks the question that is being asked.
  probe <- ExceptT $ call cwd fetchSeconds "ls-remote"
             ["ls-remote", Text.unpack remote, "refs/hbs2/meta"]

  here <- ExceptT (refTip cwd "refs/hbs2/meta")

  canon <- if BS.null (BS.filter (not . isSpace8) probe) then pure CanonNone else do
    -- Canon, by a source-only refspec: it updates no local ref, which is what
    -- makes the comparison below possible at all.
    _ <- ExceptT $ fetch [Text.unpack remote, "refs/hbs2/meta"]

    -- WHAT THE REMOTE ADVERTISED, and not what FETCH_HEAD says.
    --
    -- FETCH_HEAD is a file in the real git dir that every fetch in this
    -- repository rewrites, and nothing in this package takes a lock -- so a
    -- concurrent `hub inbox accept`, a second `hub sync`, or the operator's own
    -- @git fetch@ in another terminal lands between the fetch above and the
    -- read. Here that was the sharpest of the three: on a clone with no canon
    -- yet, the branch below does an unconditional @update-ref refs\/hbs2\/meta@
    -- on whatever the file happened to hold.
    --
    -- The probe is already in hand and was being thrown away after a test for
    -- emptiness. It is the same question asked of the same remote, one call
    -- earlier.
    --
    -- If the remote MOVED between the two calls, this names a commit the fetch
    -- may not have brought, and @update-ref@ then refuses an object that is not
    -- there -- loudly, with nothing written. A run behind is the worst honest
    -- answer here, and the next sync closes it.
    let there = firstField probe

    case (here, there) of
      -- The probe was not empty and holds no object name, which is a line git
      -- printed in a shape this does not read. Reported as absent because that
      -- is what this clone can establish; the next run will say the same or
      -- move on.
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
  _ <- ExceptT $ fetch [ Text.unpack remote, "+refs/hbs2/pulls/*:refs/hbs2/pulls/*" ]

  pure (Synced canon True)

-- | What a publish did.
data Published = Published
  { pbCanon :: PublishedCanon
  , pbPulls :: PublishedPulls
  }
  deriving stock (Eq,Show)

-- | And what happened to the remote's copy of the staged proposals.
--
-- Three states rather than a Bool, because the Bool could not say the third
-- one and the report was false as a result: canon could be refused, with
-- @nothing was written@ on stdout, while this verb had just FORCED
-- @refs\/hbs2\/pulls\/*@ over the remote's copies in the same run.
data PublishedPulls =
    -- | There were none here to push, which is the ordinary state of a
    -- repository nobody has proposed anything to. A failure is an error rather
    -- than a 'PullsNone'.
    PullsNone
    -- | Pushed.
  | PullsMoved
    -- | There were some, and they were left where they are, because canon was
    -- refused.
    --
    -- These refs are DERIVED FROM CANON -- the number in the name comes out of
    -- the fold -- so publishing them against a remote whose canon this clone
    -- does not contain writes names built from an accounting the remote has
    -- already moved past. And the force is what makes it matter: the remote's
    -- own copies, staged against the canon it actually holds, would be
    -- overwritten by ours. The sync the canon line asks for fixes both.
  | PullsHeld
  deriving stock (Eq,Show)

-- | What happened to the remote's copy of canon.
data PublishedCanon =
    -- | There is no canon in this clone: nothing has been folded here.
    PublishedNone
    -- | The remote already had exactly this.
  | PublishedSame Text
    -- | Pushed. What the remote now holds.
  | PublishedMoved Text
    -- | The remote holds canon this clone does not contain, so the push would
    -- have replaced it. NOTHING WAS WRITTEN.
    --
    -- The mirror of 'CanonDiverged' and the reason this push is not forced: a
    -- second maintainer's folds live only in their ref until somebody fetches
    -- them, and a plus here would drop them on the floor. The answer is the
    -- same as on the read side -- @hub sync@, which folds both and takes the
    -- rewrite when it is one.
  | PublishedRefused Text
  deriving stock (Eq,Show)

-- | Push canon and the staged proposals to a remote.
--
-- The counterpart of 'syncFrom', and it exists because until it did a
-- maintainer could do a full day's correct work that nobody would ever see:
-- @refs\/hbs2\/meta@ is an ordinary git ref in the repository they are standing
-- in, every accept and merge and compaction writes it locally, and each of them
-- exits zero having published nothing.
--
-- CANON IS NOT FORCED, for the reason 'syncFrom' does not force its fetch: a
-- plus here replaces canon the remote holds and this clone has not seen, which
-- is exactly what a second maintainer's folds look like. git refuses the
-- non-fast-forward on its own; this asks first so the answer is a sentence
-- rather than git's.
--
-- THE PULL REFS ARE FORCED, and the asymmetry is not an oversight. A staged
-- proposal head moves when the contributor revises the pull request, which is
-- a rewrite by construction and not a fast-forward, and 'syncFrom' already
-- takes them with a plus -- so without one here a revised proposal could be
-- staged locally and never published. They are also derived from canon rather
-- than authored: the number comes from the fold, and PEP-21's A1 has one
-- publisher holding the reflog key, so there is no second author for these to
-- race with.
publishTo :: MonadUnliftIO m
          => Maybe FilePath -> Text -> m (Either BundleError Published)
publishTo cwd remote = runExceptT do
  checked "remote name" validRefName remote

  here <- ExceptT (refTip cwd "refs/hbs2/meta")

  canon <- case here of
    Nothing -> pure PublishedNone
    Just mine -> do
      -- What the remote has, before anything is pushed. Asked rather than
      -- inferred from the push failing, because "the remote is ahead" and "the
      -- remote refused for some other reason" are two different sentences and
      -- git says them both the same way.
      theirs0 <- ExceptT (remoteCanon cwd remote)

      case theirs0 of
        Nothing -> push mine
        Just theirs
          | theirs == mine -> pure (PublishedSame mine)
          | otherwise -> do
              -- A REMOTE CHOSE THIS STRING, so it is checked before it reaches
              -- a command line, on the rule the rest of this module follows.
              -- 'isAncestor' checks its own arguments; the point of doing it
              -- here is that there are now two commands downstream of it.
              checked "object name" validSha theirs

              -- AND THIS CLONE MAY SIMPLY NOT HAVE IT, which was the ordinary
              -- case and not a rare one: not having the remote's canon is what
              -- "the remote is ahead" MEANS when nobody has fetched. Asking
              -- @merge-base --is-ancestor@ about an object git does not hold
              -- gets "fatal: Not a valid commit name", so the default path of
              -- the one scenario this verb is shaped around answered with a raw
              -- git error at exit 46, and the sentence written for it appeared
              -- only after fetching by hand.
              --
              -- It is the same answer, not a different one: an object this
              -- repository does not contain is the strongest form of canon this
              -- clone does not contain, and the remedy the report names -- sync,
              -- which fetches and folds both -- is the remedy either way.
              have <- lift $ call cwd smallSeconds "cat-file"
                        ["cat-file", "-e", Text.unpack theirs <> "^{commit}"]

              if isLeft have
                then pure (PublishedRefused theirs)
                else do
                  -- Contains, not equals: the remote's commit has to be an
                  -- ancestor of ours, which is the same question the fetch side
                  -- asks in the other direction.
                  ff <- ExceptT (isAncestor cwd theirs mine)
                  if ff then push mine else pure (PublishedRefused theirs)

  -- And the staged proposals, when there are any. A push with no matching
  -- source refspec is an error from git, and a repository nobody has proposed
  -- anything to is not an error, so the question is asked first.
  staged <- ExceptT $ call cwd smallSeconds "for-each-ref"
              ["for-each-ref", "--format=%(refname)", "refs/hbs2/pulls/"]

  let anyStaged = not (BS.null (BS.filter (not . isSpace8) staged))

  pulls <- case canon of
    _ | not anyStaged      -> pure PullsNone
    PublishedRefused{}     -> pure PullsHeld
    PublishedNone          -> movePulls
    PublishedSame{}        -> movePulls
    PublishedMoved{}       -> movePulls

  pure (Published canon pulls)

  where
    push mine = do
      _ <- ExceptT $ call cwd fetchSeconds "push"
             [ "push", Text.unpack remote, "refs/hbs2/meta:refs/hbs2/meta" ]
      pure (PublishedMoved mine)

    movePulls = do
      _ <- ExceptT $ call cwd fetchSeconds "push"
             [ "push", Text.unpack remote, "+refs/hbs2/pulls/*:refs/hbs2/pulls/*" ]
      pure PullsMoved

    -- ls-remote answers a line of "<sha>\t<ref>", or nothing at all.
-- | Move the canon ref, through what it currently holds.
--
-- The one write 'hub sync' makes, and it is a compare-and-swap for the reason
-- every other ref move in this package is: between deciding and writing,
-- another process may have folded a letter here, and taking a remote lineage
-- over that would drop it.
--
-- Named rather than folded into 'syncFrom', because the decision to take a
-- rewritten lineage is not git's to make: it needs both canons folded, which
-- happens a layer up.
takeCanon :: MonadUnliftIO m
          => Maybe FilePath -> Text -> Text -> m (Either BundleError ())
takeCanon cwd new old = runExceptT do
  checked "object name" validSha new
  checked "object name" validSha old
  _ <- ExceptT $ call cwd smallSeconds "update-ref"
         ["update-ref", "refs/hbs2/meta", Text.unpack new, Text.unpack old]
  pure ()

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

-- | What a remote holds for @refs\/hbs2\/meta@, without writing anything.
--
-- SPLIT OUT OF 'publishTo' rather than reimplemented beside it: `hub status`
-- wants the same probe and must not push to get it, and two ls-remote parsers
-- would be two answers to "is this published". 'publishTo' calls this and then
-- decides; this only asks.
--
-- 'Nothing' is the remote holding no canon, which is not an error: a remote
-- nobody has published to is the ordinary state of a fresh repository.
remoteCanon :: MonadUnliftIO m
            => Maybe FilePath -> Text -> m (Either BundleError (Maybe Text))
remoteCanon cwd remote = runExceptT do
  -- A REMOTE NAME REACHES A COMMAND LINE, so it is checked here as well as in
  -- the caller: this is exported and the rule in this module is that every
  -- string on a command line was checked by whoever put it there.
  checked "remote name" validRefName remote
  probe <- ExceptT $ call cwd fetchSeconds "ls-remote"
             ["ls-remote", Text.unpack remote, "refs/hbs2/meta"]
  pure (firstField probe)

-- The sha at the head of an ls-remote line, which is tab-separated.
firstField :: BS.ByteString -> Maybe Text
firstField bs = case BS.split 0x09 (BS.takeWhile (/= 0x0a) bs) of
  (sha : _) | not (BS.null (BS.filter (not . isSpace8) sha)) ->
    Just (Text.strip (Text.decodeUtf8Lenient sha))
  _ -> Nothing

-- | The object names @git bundle list-heads@ printed, one per recorded ref.
--
-- Its own function, and total, because what it parses is the answer this whole
-- path rests on: git prints @<sha> <refname>@ per line, and a line that is not
-- that shape must not silently become a tip. A line with no sha is dropped
-- rather than guessed at; what the caller does with a count that is not one is
-- the caller's rule, not this one's.
recordedTips :: ByteString -> [Text]
recordedTips out =
  [ sha
  | l <- BS.split 0x0a out
  , sha : _ <- [Text.words (Text.decodeUtf8Lenient l)]
  , validSha sha
  ]

-- | What a proposal adds, in bytes, once git is not compressing it.
--
-- WHY A SIZE IS WORTH ASKING FOR AT ALL. A bundle is a pack, and a pack is
-- compressed: 512 MiB of zeros bundles to about 522 KiB, a ratio near 1000:1.
-- The fetch is cheap, because git keeps the pack as it arrived -- so nothing
-- upstream of a checkout notices, and @hub pr checkout@ is where the tree is
-- materialised into somebody's working directory. An attachment at the bound
-- this hub accepts, at that ratio, is tens of gigabytes.
--
-- THE OBJECTS THIS PROPOSAL ADDS, and not the tree at the tip. A repository is
-- legitimately large and its maintainer expects a working tree that size; what
-- nobody expects is one proposal adding more than the repository holds. Asking
-- @base..tip@ makes the number about the contributor's contribution, so the
-- bound below means the same thing in a small repository and in a large one.
--
-- An UPPER bound on what a checkout writes: it counts every new object, and a
-- working tree materialises only the blobs still present at the tip. Bounding
-- from above is the right direction for a gate, and the exact figure would cost
-- a second walk to compute.
--
-- Two calls and no shell: @rev-list --objects@ names them, @cat-file
-- --batch-check@ weighs them, and the second reads the first's output as stdin.
-- Both are bounded by the size of the DELTA rather than of the repository,
-- which is what makes this affordable to run before every checkout.
addedBytes :: MonadUnliftIO m
           => Maybe FilePath -> Text -> Text -> m (Either BundleError Integer)
addedBytes cwd base tip = runExceptT do
  checked "object name" validSha base
  checked "object name" validSha tip

  objs <- ExceptT $ call cwd bundleSeconds "rev-list"
            [ "rev-list", "--objects", "--end-of-options"
            , Text.unpack base <> ".." <> Text.unpack tip ]

  -- THE FIRST TOKEN OF EACH LINE. `rev-list --objects` prints the object name
  -- and then, for a blob or a tree, the path it was found at -- and a path is a
  -- stranger's bytes, which must not reach cat-file's stdin as though it were
  -- an object name. Not `--no-object-names`, which says the same thing and was
  -- added in git 2.26: this parse is stable across every version that has
  -- `--objects` at all.
  let names = [ n | l <- BS.split 0x0a objs, n : _ <- [B8.words l] ]

  if Prelude.null names then pure 0 else do
    sizes <- ExceptT $ stdinTo cwd bundleSeconds "cat-file"
               [ "cat-file", "--batch-check=%(objectsize)" ]
               (LBS.fromStrict (BS.intercalate "\n" names <> "\n"))
    pure (sum [ n | l <- BS.split 0x0a sizes, Just n <- [readSize l] ])
  where
    readSize l = case B8.words l of
      [w] | not (BS.null w), BS.all (\c -> c >= 0x30 && c <= 0x39) w ->
        Just (read (Text.unpack (Text.decodeUtf8Lenient w)) :: Integer)
      _ -> Nothing

-- The same as 'call', for a command that reads its work from stdin.
stdinTo :: MonadUnliftIO m
        => Maybe FilePath -> Int -> Text -> [String] -> LBS.ByteString
        -> m (Either BundleError ByteString)
stdinTo cwd secs what args input =
  gitRun cwd [] secs what args input <&> \case
    Left (GitUnstartable e) -> Left (BundleUnstartable e)
    Left (GitStalled e)     -> Left (BundleStalled e)
    Right (ExitSuccess, out, _)  -> Right out
    Right (ExitFailure c, _, e0) -> Left (refusal what c e0)
