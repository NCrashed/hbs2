{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import HBS2.Git3.Prelude
import HBS2.Git3.Run
import HBS2.Git3.State
import HBS2.Git3.Logger

import Data.Config.Suckless.Script
import Data.Version (showVersion)
import Paths_hbs2_git3 qualified as Pkg
import System.Environment qualified as E

main :: IO ()
main = do
  rawArgs <- E.getArgs
  if rawArgs == ["--version"]
    then putStrLn $ showVersion Pkg.version
    else runMain rawArgs

runMain :: [String] -> IO ()
runMain rawArgs = flip runContT pure do

  setupLogger

  ContT $ bracket none $ const do
    silence

  let (discovery, argz) = case rawArgs of
        [command, "--help"] -> (True, ["--help", command])
        "--help" : _ -> (True, rawArgs)
        "help" : _ -> (True, rawArgs)
        _ -> (False, rawArgs)

  cli <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  tvd <- newTVarIO theDict
  env' <- nullGit3Env
  let env = env' { gitRuntimeDict = Just (RuntimeDict tvd) }

  void $ lift $ withGit3Env env do
    conf <- if discovery then pure mempty else readLocalConf
    recover $ setupLogger >> runEval tvd (conf <> cli)
      `finally` silence
