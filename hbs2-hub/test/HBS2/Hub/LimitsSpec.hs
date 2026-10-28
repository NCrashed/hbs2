-- | What bounds a server's work (PEP-25): the gate a call waits at and the
-- cache a public answer is reused from.
module HBS2.Hub.LimitsSpec (spec) where

import HBS2.Hub.Serve.Limits

import Control.Concurrent
import Control.Exception qualified
import Control.Monad (replicateM,replicateM_)
import Data.Aeson (Value(..))
import Data.Aeson.KeyMap qualified as KM
import Data.IORef
import Test.Hspec
import UnliftIO.Async (concurrently,forConcurrently,replicateConcurrently)

spec :: Spec
spec = describe "PEP-25 serve: the gate and the answer cache" $ do

  it "lets a call past the busy limit wait for a slot instead of refusing it" $ do
    -- The bug this replaces: one page asks six things at once, a public
    -- server ran four, and the other two were refused and left blank.
    g <- newGate 2 8 5
    running <- newIORef (0 :: Int)
    most <- newIORef (0 :: Int)
    let one = withGate g $ do
          n <- atomicModifyIORef' running (\x -> (x + 1, x + 1))
          atomicModifyIORef' most (\m -> (max m n, ()))
          threadDelay 20000
          atomicModifyIORef' running (\x -> (x - 1, ()))
    rs <- replicateConcurrently 6 one
    rs `shouldBe` replicate 6 (Right ())
    readIORef most `shouldReturn` 2

  it "refuses past the queue's depth, at once" $ do
    g <- newGate 1 1 5
    hold <- newEmptyMVar
    started <- newEmptyMVar
    _ <- forkIO (() <$ withGate g (putMVar started () >> takeMVar hold))
    takeMVar started
    _ <- forkIO (() <$ withGate g (pure ()))
    threadDelay 20000
    withGate g (pure ()) `shouldReturn` Left QueueFull
    putMVar hold ()

  it "gives up on a slot that does not come free in time" $ do
    g <- newGate 1 4 0
    hold <- newEmptyMVar
    started <- newEmptyMVar
    _ <- forkIO (() <$ withGate g (putMVar started () >> takeMVar hold))
    takeMVar started
    withGate g (pure ()) `shouldReturn` Left WaitedOut
    putMVar hold ()
    -- and the slot is there again once the holder is done
    threadDelay 20000
    withGate g (pure 'x') `shouldReturn` Right 'x'

  it "answers a repeated call from the cache" $ do
    ac <- newAnswerCache @() 30
    runs <- newIORef (0 :: Int)
    let run = atomicModifyIORef' runs (\n -> (n + 1, ())) >> pure (Right (String "a"))
        k = cacheKey "issue_list" (KM.fromList [("repo", String "K")])
    replicateM_ 3 (cachedAnswer ac k run)
    readIORef runs `shouldReturn` 1
    -- a different call is a different key
    _ <- cachedAnswer ac (cacheKey "pr_list" (KM.fromList [("repo", String "K")])) run
    readIORef runs `shouldReturn` 2

  it "runs a call asked many times at once only once" $ do
    ac <- newAnswerCache @() 30
    runs <- newIORef (0 :: Int)
    let run = do
          atomicModifyIORef' runs (\n -> (n + 1, ()))
          threadDelay 50000
          pure (Right (String "a"))
    rs <- forConcurrently [1 .. 8 :: Int] (const (cachedAnswer ac "k" run))
    rs `shouldBe` replicate 8 (Right (String "a"))
    readIORef runs `shouldReturn` 1

  it "keeps no refusal, so the next call runs again" $ do
    ac <- newAnswerCache @String 30
    runs <- newIORef (0 :: Int)
    let run = atomicModifyIORef' runs (\n -> (n + 1, ())) >> pure (Left "busy")
    _ <- replicateM 2 (cachedAnswer ac "k" run)
    readIORef runs `shouldReturn` 2

  it "lets the others run when the run they waited on failed" $ do
    ac <- newAnswerCache @() 30
    gate <- newEmptyMVar
    let boom = takeMVar gate >> ioError (userError "boom")
        fine = pure (Right (String "b"))
    (a, b) <- concurrently
      (tryIt (cachedAnswer ac "k" boom))
      (threadDelay 20000 >> putMVar gate () >> cachedAnswer ac "k" fine)
    either (const True) (const False) a `shouldBe` True
    b `shouldBe` Right (String "b")

  it "caches nothing when it is off" $ do
    ac <- newAnswerCache @() 0
    runs <- newIORef (0 :: Int)
    let run = atomicModifyIORef' runs (\n -> (n + 1, ())) >> pure (Right Null)
    replicateM_ 3 (cachedAnswer ac "k" run)
    readIORef runs `shouldReturn` 3
  where
    tryIt :: IO a -> IO (Either IOError a)
    tryIt act = (Right <$> act) `catchIO` (pure . Left)
    catchIO :: IO a -> (IOError -> IO a) -> IO a
    catchIO = Control.Exception.catch
