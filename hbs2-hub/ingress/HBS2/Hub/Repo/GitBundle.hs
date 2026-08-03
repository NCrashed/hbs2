{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
-- | The git side of a pull request (PEP-20 "Two submission paths", "Fetch and
-- verify").
--
-- Four operations, and between them they are the whole delta path as far as
-- git is concerned: build a bundle of @base..source-ref@, take a stranger's
-- bundle in, check that the signed base really is an ancestor of the signed
-- tip, and stage the proposed tip under @refs\/hbs2\/pulls\/<n>\/head@.
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
  , stagePull
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
import Data.Word (Word64)
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
  out <- ExceptT $ call cwd bundleSeconds "bundle create"
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

    _ <- ExceptT $ call cwd bundleSeconds "fetch"
           [ "-c", "transfer.fsckObjects=true"
           , "-c", "fetch.fsckObjects=true"
           , "fetch", path, Text.unpack ref ]

    -- FETCH_HEAD, which is what the fetch above just wrote, and not the ref
    -- name: fetching a bundle does not create a local branch, and resolving
    -- the name would find whatever this repository happens to have under it.
    -- That is the whole difference between checking what arrived and checking
    -- what was already here.
    got <- ExceptT $ oneLine cwd "rev-parse" ["rev-parse", "FETCH_HEAD^{commit}"]

    when (got /= signedTip) $ throwError (BundleTipMismatch signedTip got)
    pure got

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

-- | Stage a proposed tip where PEP-19 puts it.
--
-- Compare-and-swap, like the canon ref and for the same reason: a pull ref
-- that moved under us means somebody else staged this number, and overwriting
-- it would replace one contributor's proposal with another's under one number.
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
bundleSeconds, smallSeconds :: Int
bundleSeconds = 600
smallSeconds  = 60

-- A value that will not go to git is refused before anything runs.
checked :: Monad m => Text -> (Text -> Bool) -> Text -> ExceptT BundleError m ()
checked what ok v = unless (ok v) (throwError (BundleBadName what v))

run :: MonadUnliftIO m
    => Maybe FilePath -> Int -> Text -> [String]
    -> m (Either BundleError (ExitCode, ByteString, ByteString))
run cwd secs what args =
  gitRun cwd [] secs what args mempty <&> \case
    Left (GitUnstartable e) -> Left (BundleUnstartable e)
    Left (GitStalled e)     -> Left (BundleStalled e)
    Right r                 -> Right r

-- The common case: a non-zero exit is a refusal, and stdout is the answer.
call :: MonadUnliftIO m
     => Maybe FilePath -> Int -> Text -> [String] -> m (Either BundleError ByteString)
call cwd secs what args =
  run cwd secs what args <&> \case
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
