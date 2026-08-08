-- | @hub publish@ (PEP-22): canon and the staged proposals, out.
--
-- Two halves, tested apart. The REPORT is a pure function of the outcome, so
-- these ask it directly; the PUSH is git, so it is asked with two real
-- repositories and a real remote.
--
-- The case worth the fixture is the refusal. A push that overwrote canon the
-- remote holds and this clone has not seen would delete a second maintainer's
-- folds, and there is nothing to notice afterwards: their events survive in the
-- object store and the ref every reader follows does not point at them.
module HBS2.Hub.PublishSpec (spec) where

import HBS2.Hub.CLI.Publish
import HBS2.Hub.Repo.GitBundle (publishTo,Published(..),PublishedCanon(..),PublishedPulls(..))

import HBS2.Prelude.Plated (Doc)

import Control.Monad (void)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (createDirectoryIfMissing)
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed
import Test.Hspec

shown :: [Doc ann] -> String
shown = unlines . fmap show

-- | git in a directory, with the ambient configuration sealed out.
--
-- The same sealing 'GitRepoSpec' uses and for the same reason: what is under
-- test runs git, and a developer's global config can decide whether it works.
git :: FilePath -> [String] -> IO String
git dir as = do
  (_, out, _) <- readProcess
    ( setEnv [ ("GIT_CONFIG_GLOBAL", "/dev/null")
             , ("GIT_CONFIG_SYSTEM", "/dev/null")
             , ("GIT_AUTHOR_NAME", "t"), ("GIT_AUTHOR_EMAIL", "t@t")
             , ("GIT_COMMITTER_NAME", "t"), ("GIT_COMMITTER_EMAIL", "t@t")
             , ("LC_ALL", "C")
             ]
      (setWorkingDir dir (proc "git" as)) )
  pure (trim (show out))
  where
    trim = takeWhile (`notElem` ("\\\"" :: String)) . drop 1

-- | A clone with a canon ref, and a bare remote it can push to.
--
-- Canon here is any commit: what is under test is the ref plumbing, and the
-- fold has its own suite. Building a real canon tree would test that instead.
withRemote :: (FilePath -> FilePath -> IO a) -> IO a
withRemote act = withSystemTempDirectory "hub-publish" $ \root -> do
  let bare = root <> "/remote.git"
      work = root <> "/work"
  createDirectoryIfMissing True work
  void $ git root ["init", "-q", "--bare", "remote.git"]
  void $ git work ["init", "-q", "."]
  writeFile (work <> "/a.txt") "one\n"
  void $ git work ["add", "a.txt"]
  void $ git work ["commit", "-q", "-m", "base"]
  void $ git work ["remote", "add", "origin", bare]
  act work bare

-- | Put HEAD on refs/hbs2/meta in this repository.
canonHere :: FilePath -> IO Text
canonHere dir = do
  h <- Text.pack <$> git dir ["rev-parse", "HEAD"]
  void $ git dir ["update-ref", "refs/hbs2/meta", Text.unpack h]
  pure h

spec :: Spec
spec = do

  describe "PEP-22 hub publish: what it says" $ do

    it "says nothing was folded here, rather than reporting a failure" $ do
      -- A young repository, not an error: the verb exits zero and the report
      -- says which of the two nothings this is.
      let out = shown (publishDoc "origin" (Published PublishedNone PullsNone))
      out `shouldSatisfy` isInfixOf "nothing has been folded"
      publishCode (Published PublishedNone PullsNone) `shouldBe` 0

    it "tells an unchanged remote from one it moved" $ do
      shown (publishDoc "origin" (Published (PublishedSame "abc") PullsNone))
        `shouldSatisfy` isInfixOf "already there"
      shown (publishDoc "origin" (Published (PublishedMoved "abc") PullsNone))
        `shouldSatisfy` isInfixOf "published"

    -- The one that needs a person. It has to say what to do, because what to
    -- do is not guessable: the answer is a sync, and a sync with the repository
    -- key, since only a fold can tell a rewrite from a fork.
    it "says what to do when the remote is ahead, and exits its own code" $ do
      let p = Published (PublishedRefused "deadbeef") PullsNone
          out = shown (publishDoc "origin" p)
      out `shouldSatisfy` isInfixOf "NOT published"
      out `shouldSatisfy` isInfixOf "nothing was written"
      out `shouldSatisfy` isInfixOf "sync"
      publishCode p `shouldBe` codeNotPublished
      -- And not the code a git failure gets: a person reading a script's exit
      -- has to be able to tell "somebody else folded something" from "git
      -- would not run".
      codeNotPublished `shouldSatisfy` (/= codePublishFailed)

    -- Said rather than left out: a maintainer who has just staged a proposal
    -- and sees no line about it cannot tell "there were none" from "this verb
    -- does not do that".
    it "says either way whether there were staged proposals" $ do
      shown (publishDoc "origin" (Published PublishedNone PullsMoved))
        `shouldSatisfy` isInfixOf "staged proposals published"
      shown (publishDoc "origin" (Published PublishedNone PullsNone))
        `shouldSatisfy` isInfixOf "no staged proposals"

  describe "PEP-22 hub publish: what it does" $ do

    it "pushes canon to a remote that has none" $ withRemote $ \work bare -> do
      mine <- canonHere work
      r <- either (fail . show) pure =<< publishTo (Just work) "origin"
      pbCanon r `shouldBe` PublishedMoved mine
      -- And it is really there, asked of the remote rather than of the pusher.
      there <- git bare ["rev-parse", "refs/hbs2/meta"]
      there `shouldBe` Text.unpack mine

    it "says the remote already has it, and pushes nothing" $ withRemote $ \work _ -> do
      mine <- canonHere work
      _ <- either (fail . show) pure =<< publishTo (Just work) "origin"
      r <- either (fail . show) pure =<< publishTo (Just work) "origin"
      pbCanon r `shouldBe` PublishedSame mine

    -- THE ONE THIS VERB IS SHAPED AROUND. The remote holds canon this clone
    -- does not contain, which is what a second maintainer's folds look like. A
    -- forced push would take them out of the ref every reader follows, and
    -- nothing afterwards would say so.
    it "refuses to publish over canon this clone does not contain" $
      withRemote $ \work bare -> do
        _ <- canonHere work
        _ <- either (fail . show) pure =<< publishTo (Just work) "origin"

        -- Somebody else folds something and publishes it.
        writeFile (work <> "/b.txt") "two\n"
        void $ git work ["add", "b.txt"]
        void $ git work ["commit", "-q", "-m", "theirs"]
        theirs <- canonHere work
        _ <- either (fail . show) pure =<< publishTo (Just work) "origin"

        -- And this clone goes back to what it had, which is what a maintainer
        -- who never fetched is holding.
        void $ git work ["update-ref", "refs/hbs2/meta", "HEAD~1"]

        r <- either (fail . show) pure =<< publishTo (Just work) "origin"
        pbCanon r `shouldBe` PublishedRefused theirs
        -- Nothing was written: the remote still holds theirs.
        still <- git bare ["rev-parse", "refs/hbs2/meta"]
        still `shouldBe` Text.unpack theirs

    it "pushes the staged proposals, and says when there are none" $
      withRemote $ \work bare -> do
        _ <- canonHere work
        r0 <- either (fail . show) pure =<< publishTo (Just work) "origin"
        pbPulls r0 `shouldBe` PullsNone

        h <- git work ["rev-parse", "HEAD"]
        void $ git work ["update-ref", "refs/hbs2/pulls/1/head", h]
        r1 <- either (fail . show) pure =<< publishTo (Just work) "origin"
        pbPulls r1 `shouldBe` PullsMoved
        there <- git bare ["rev-parse", "refs/hbs2/pulls/1/head"]
        there `shouldBe` h

    -- THE OTHER HALF OF THE REFUSAL, and it was missing: the canon push is a
    -- fast-forward check and the pulls push is a FORCE, so a run that refused
    -- canon and printed "nothing was written" went on to replace the remote's
    -- staged proposals in the same breath. They are numbered out of canon, and
    -- the canon they are numbered out of is the one this clone has not got.
    it "holds the staged proposals back when it refused canon" $
      withRemote $ \work bare -> do
        _ <- canonHere work
        base <- git work ["rev-parse", "HEAD"]
        void $ git work ["update-ref", "refs/hbs2/pulls/1/head", base]
        _ <- either (fail . show) pure =<< publishTo (Just work) "origin"

        -- Somebody else folds, and stages a different proposal under the same
        -- number, and publishes both.
        writeFile (work <> "/b.txt") "two\n"
        void $ git work ["add", "b.txt"]
        void $ git work ["commit", "-q", "-m", "theirs"]
        theirs <- canonHere work
        void $ git work ["update-ref", "refs/hbs2/pulls/1/head", Text.unpack theirs]
        _ <- either (fail . show) pure =<< publishTo (Just work) "origin"

        -- And this clone is back where a maintainer who never fetched is: an
        -- older canon, and its own idea of what proposal 1 is.
        void $ git work ["update-ref", "refs/hbs2/meta", "HEAD~1"]
        void $ git work ["update-ref", "refs/hbs2/pulls/1/head", base]

        r <- either (fail . show) pure =<< publishTo (Just work) "origin"
        pbCanon r `shouldBe` PublishedRefused theirs
        pbPulls r `shouldBe` PullsHeld

        -- Asked of the remote: the other maintainer's proposal is still there.
        still <- git bare ["rev-parse", "refs/hbs2/pulls/1/head"]
        still `shouldBe` Text.unpack theirs

        -- And the report says so, rather than leaving a reader to infer it
        -- from the canon line.
        shown (publishDoc "origin" r) `shouldSatisfy` isInfixOf "NOT published either"
