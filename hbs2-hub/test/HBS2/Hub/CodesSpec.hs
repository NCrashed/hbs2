-- | The exit codes, which PEP-22 calls a contract (PEP-22 "Scripting").
--
-- A hook branches on them, so they may be added to and never reassigned. They
-- were defined in fifteen modules, four of them documented nowhere, and the next
-- number was chosen by grepping for the last one -- which is how two of them
-- would eventually be chosen at once.
module HBS2.Hub.CodesSpec (spec) where

import HBS2.Hub.CLI.Codes
import HBS2.Hub.CLI.Verify (codeOf)
import HBS2.Hub.CLI.Sync (codeDiverged,codeSyncFailed)
import HBS2.Hub.CLI.Publish (codePublishFailed)
import HBS2.Hub.Repo (CanonUnreadable(..))

import Data.List (sort,group,isInfixOf)
import Data.Text qualified as Text
import Prettyprinter (Doc)
import Test.Hspec

shown :: [Doc ()] -> String
shown = unlines . fmap show

spec :: Spec
spec =
  describe "PEP-22 exit codes: the contract" $ do

    -- THE ONE THAT MATTERS. Two refusals sharing a number means a hook cannot
    -- tell them apart, and the numbers cannot be moved afterwards to fix it.
    it "gives every refusal a number of its own" $ do
      let ns = fmap codeFor refusals
          dups = [ n | (n:_:_) <- group (sort ns) ]
      dups `shouldBe` []

    -- 0, 1 and 2 are spoken for (success, usage, `verify` found something) and
    -- 3..16 belong to `hub verify`'s own reasons. A refusal landing in either
    -- range would be read as something else entirely by a script.
    it "keeps out of the ranges that already mean something" $ do
      [ r | r <- refusals, codeFor r <= 16 ] `shouldBe` []

    -- And the other side of that: verify's codes stay inside the range the
    -- table hands them, or the table's sentence about 3..16 becomes false.
    it "keeps verify's codes inside the range the table gives them" $ do
      let vs = [ codeOf e | e <- [ NoCanonRef "x", NoRepository "x"
                                 , CanonTooNewHere 9
                                 , CanonTooBig 1, CanonTooMany 1
                                 , CanonListingTooBig 1 ] ]
      [ v | v <- vs, v < 3 || v > 16 ] `shouldBe` []

    -- The one deliberate sharing, asserted as deliberate. `sync` and `publish`
    -- both mean "git would not do it", and the code is one binding rather than
    -- two equal literals precisely so that this is visible.
    it "shares one code between sync and publish, on purpose" $ do
      codeSyncFailed `shouldBe` codePublishFailed
      -- ...and sync's OTHER code is its own, which is the one a caller acts on
      codeDiverged `shouldSatisfy` (/= codePublishFailed)

    -- WHAT THE TOOL PRINTS IS WHAT IT EXITS WITH, because the table is
    -- generated from the constants rather than typed a second time. A table in
    -- the manual would be the second copy that disagrees.
    it "prints every refusal, with its number and a sentence" $ do
      let out = shown codesDoc
      [ r | r <- refusals, not (show (codeFor r) `isInfixOf` out) ] `shouldBe` []
      [ r | r <- refusals, not (Text.unpack (meaning r) `isInfixOf` out) ] `shouldBe` []
      -- and the three that are not refusals at all
      out `shouldSatisfy` isInfixOf "0   success"
      out `shouldSatisfy` isInfixOf "3..16"

    -- A sentence per code, and not the constant's own haddock: a caller
    -- branching on a number wants to know what it means, in one line.
    it "gives every meaning some words" $ do
      [ r | r <- refusals, Text.length (meaning r) < 12 ] `shouldBe` []
