-- | The deadline every wait for a repository now has.
--
-- These are the two loops 'waitRepo' is built from, run against probes that
-- answer on a schedule, so what is asserted is when they give up and when
-- they must not: a value that exists has to be found even if it lands at the
-- last moment, and a repository that keeps arriving must never be cut off.
-- The peer is not needed for any of it, which is the point of the split.
module HBS2.Git3.State.WaitSpec (spec) where

import HBS2.Git3.State.Internal.Wait

import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema ()
import HBS2.Prelude.Plated (fromStringMay)

import Control.Concurrent (threadDelay)
import Data.IORef
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Test.Hspec

aKey :: PubKey 'Sign 'HBS2Basic
aKey = fromMaybe (error "not a key")
         (fromStringMay "2v3ubvkrQaWzhBCZW14JDWUW1LGBMnkirWeJJvZxnNUV")

-- | Seconds a computation took, and its result.
timed :: IO a -> IO (Double, a)
timed io = do
  t0 <- getCurrentTime
  r  <- io
  t1 <- getCurrentTime
  pure (realToFrac (diffUTCTime t1 t0), r)

-- | A probe that answers Nothing this many times, then the value.
answersAfter :: Int -> a -> IO (IO (Maybe a), IORef Int)
answersAfter n x = do
  calls <- newIORef 0
  let probe = do
        i <- atomicModifyIORef' calls (\c -> (c + 1, c))
        pure (if i >= n then Just x else Nothing)
  pure (probe, calls)

spec :: Spec
spec = do

  describe "pollUntil" do

    it "returns a value as soon as it is there" do
      (probe, calls) <- answersAfter 2 'x'
      (dt, r) <- timed (pollUntil 30 probe)
      r `shouldBe` Just 'x'
      readIORef calls >>= (`shouldBe` 3)
      dt `shouldSatisfy` (< 5)

    it "gives up at the limit, and not long after it" do
      calls <- newIORef (0 :: Int)
      (dt, r) <- timed $ pollUntil 1 (modifyIORef' calls succ >> pure (Nothing :: Maybe ()))
      r `shouldBe` Nothing
      dt `shouldSatisfy` (>= 1)
      dt `shouldSatisfy` (< 2)
      readIORef calls >>= (`shouldSatisfy` (>= 2))

    it "still sees a value that lands during the last pause" do
      t0 <- getCurrentTime
      let probe = do
            now <- getCurrentTime
            pure $ if realToFrac (diffUTCTime now t0) >= (0.9 :: Double)
                     then Just () else Nothing
      pollUntil 0.9 probe >>= (`shouldBe` Just ())

    -- An RPC that times out is a probe that took its whole timeout, and that
    -- time is part of the wait, or a dead peer would stretch every limit.
    it "counts the time a slow probe takes against the limit" do
      (probe0, calls) <- answersAfter 1 'y'
      let probe = threadDelay 300000 >> probe0
      pollUntil 0.2 probe >>= (`shouldBe` Nothing)
      readIORef calls >>= (`shouldBe` 1)

  describe "pollProgress" do

    it "is never cut off while what is missing keeps going down" do
      left <- newIORef (8 :: Int)
      let probe = do
            n <- atomicModifyIORef' left (\c -> (c - 1, c))
            pure (if n <= 0 then Right () else Left n)
      (dt, r) <- timed (pollProgress 0.3 probe)
      r `shouldBe` Right ()
      -- eight pauses of up to 0.3s each: well past a limit counted once
      dt `shouldSatisfy` (> 0.3 * 4)

    it "gives up once it stops going down, with the last count" do
      left <- newIORef (5 :: Int)
      let probe = do
            n <- atomicModifyIORef' left (\c -> (max 3 (c - 1), c))
            pure (Left n :: Either Int ())
      (dt, r) <- timed (pollProgress 0.5 probe)
      r `shouldBe` Left 3
      dt `shouldSatisfy` (< 3)

  describe "the limit" do

    it "is a positive number of seconds" do
      parseRepoWait "90"  `shouldBe` Just 90
      parseRepoWait "1.5" `shouldBe` Just 1.5
      parseRepoWait "0"   `shouldBe` Nothing
      parseRepoWait "-5"  `shouldBe` Nothing
      parseRepoWait "ten" `shouldBe` Nothing
      parseRepoWait ""    `shouldBe` Nothing

    it "defaults to a minute" do
      defRepoWaitTimeout `shouldBe` 60

  describe "what a user is told" do

    let k = "2v3ubvkrQaWzhBCZW14JDWUW1LGBMnkirWeJJvZxnNUV" :: String

    it "names the key and says it was not found" do
      let s = show (RepoNotFound aKey 60)
      s `shouldSatisfy` isInfixOf ("repository " <> k <> " not found")
      s `shouldSatisfy` isInfixOf "its lwwref has no value on this peer or its neighbours"
      s `shouldSatisfy` isInfixOf "waited 60s"
      s `shouldSatisfy` isInfixOf repoWaitEnvVar

    it "names the value whose manifest it could not read" do
      show (RepoManifestUnreadable aKey aHashRef 60)
        `shouldSatisfy` isInfixOf "points at 8yqJyq5jxKmDdMwzGvQhx3srKuc1FqmxCYSZKz5yzXWJ"

    it "says some blocks when it does not know how many" do
      show (RepoRefLogIncomplete aKey aKey maxBound 60) `shouldSatisfy` isInfixOf ": some block(s)"
      show (RepoRefLogIncomplete aKey aKey 7 60) `shouldSatisfy` isInfixOf ": 7 block(s)"

  where
    aHashRef = fromMaybe (error "not a hash")
                 (fromStringMay "8yqJyq5jxKmDdMwzGvQhx3srKuc1FqmxCYSZKz5yzXWJ")
