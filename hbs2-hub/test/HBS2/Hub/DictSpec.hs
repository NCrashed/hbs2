-- | WHAT THE TOOL ANSWERS TO (PEP-22).
--
-- The verb layer was not linked into this suite at all: no test built a
-- dictionary, so every @*Entries@ function was unreferenced and the two
-- DECISIONS in the dispatch -- which verbs exist, and which inherited builtins
-- are kept out -- were checked by running the binary and by nothing else.
--
-- Building the dictionary needs no peer. 'HBS2Cli' carries its environment in a
-- TVar that starts empty and is filled by @recover@, so the constraints are
-- satisfied by the type and only RUNNING a verb would want a connection. That
-- is the whole reason this file can exist.
module HBS2.Hub.DictSpec (spec) where

import HBS2.Hub.CLI.Dict

import HBS2.CLI.Prelude
import HBS2.CLI.Run
import HBS2.CLI.Run.Internal (HBS2Cli)

import Data.HashMap.Strict qualified as HM
import Data.Text qualified as Text
import Test.Hspec

-- The dictionary the binary answers to, built as @main@ builds it. The help
-- entries are not here: they close over the finished dictionary, so they stay
-- in @main@ and so does whatever they get wrong.
theDict :: Dict C (HBS2Cli IO)
theDict = HM.filterWithKey (\k _ -> ours k) $ makeDict do
  internalEntries
  hubEntries

spec :: Spec
spec = do

  describe "PEP-22 the dictionary this tool answers to" $ do

    -- THE CHECK THAT USED TO BE A `die` AT STARTUP, and it belongs here: a name
    -- in the list that is not a verb means the real verb is not listed, so it
    -- runs with no RPC clients and the first thing it asks for throws -- which
    -- the user reads as their own configuration. A build error is cheaper than
    -- a first-run error, and `main` keeps its check for a binary built from
    -- somewhere else.
    it "binds every verb that is listed as needing a peer" $
      [ k | k <- peerFulNames, not (HM.member k theDict) ] `shouldBe` []

    -- The allowlist, from the other side. The dictionary inherits about 154
    -- builtins from suckless-conf -- `rm`, `mv`, `cp`, `cd`, `setenv` and the
    -- whole `run:proc:*` family -- and `hbs2-hub rm victim.txt` deleted the file
    -- and exited 0. A tool that files issues does not need to delete files.
    it "answers to none of the builtins it inherits" $ do
      let inherited = makeDict @C @(HBS2Cli IO) internalEntries
      -- The fixture is real: these ARE bound upstream, so the test is about the
      -- filter and not about a name nobody defines.
      [ k | k <- shellish, not (HM.member k inherited) ] `shouldBe` []
      [ k | k <- shellish, HM.member k theDict ] `shouldBe` []

    it "keeps the four names that are about the tool rather than a forge" $ do
      -- These come from `internalEntries` or from `main`'s own bindings; what
      -- is asserted here is that the filter does not eat them.
      Prelude.filter (not . ours) ["help", "--help", "--version", "--run"] `shouldBe` []
      -- ...and that it eats everything else, whatever the upstream set grows to
      Prelude.filter ours shellish `shouldBe` []

    -- Every verb this tool defines is under one prefix, which is what makes the
    -- allowlist statable at all.
    it "puts every verb it defines under hub:" $ do
      let mine = makeDict @C @(HBS2Cli IO) hubEntries
      [ k | k@(Id t) <- HM.keys mine, not ("hub:" `Text.isPrefixOf` t) ]
        `shouldBe` []
      -- and there really are some, so the assertion above is not vacuous
      HM.size mine `shouldSatisfy` (> 20)

-- Builtins that touch the filesystem or spawn a process. Named literally,
-- because what is being tested is that a name somebody could type does not
-- reach the code behind it.
shellish :: [Id]
shellish = ["rm", "mv", "cp", "cd", "touch", "mkdir", "setenv"
           ,"proc:pipe", "run:proc:attached", "run:proc:quiet"]
