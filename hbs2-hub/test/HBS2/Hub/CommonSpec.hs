-- | What a verb does when this repository has no canon yet (PEP-19, PEP-22).
--
-- WHY THIS IS A RULE AND NOT ELEVEN ANSWERS. Every verb that reads canon used
-- to spell its own answer to a missing @refs\/hbs2\/meta@, and the two answers
-- in the tree were both defensible and nowhere written down: `hub issue close`
-- in a repository nobody has folded into refused with "canon is unreadable"
-- while `hub inbox accept` in the same one started from an empty fold. Nothing
-- said which was intended, so nothing could say which verb had it wrong.
--
-- The rule: a verb that needs canon to HOLD something refuses; a verb that ASKS
-- canon a question answers "no". 'withCanon' takes it as an argument, which is
-- the whole point -- the decision is now at the call site of a shared reader
-- rather than in eleven hand-written cases.
--
-- Testable because 'withCanon' takes the SOURCE as a parameter: no git, no
-- repository, no ref. The 'Refuse' half is not asserted here, and cannot be --
-- it ends in 'exitWith'; what it prints is 'refusalDoc', which "HBS2.Hub.Verify"
-- covers.
module HBS2.Hub.CommonSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Hub.Repo
import HBS2.Hub.CLI.Common (withCanon,OnMissing(..))

import HBS2.Net.Auth.Credentials

import Data.HashSet qualified as HS
import Test.Hspec

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

-- A source that has no canon, which is what a plain clone is: nothing fetches
-- refs/hbs2/meta by default.
noCanon :: CanonSource IO
noCanon = CanonSource
  { csCommit  = pure (Left (NoCanonRef "/somewhere/.git"))
  , csEntries = const (pure (Right []))
  , csBlob    = const (pure (BlobText ""))
  , csClose   = pure ()
  }

-- And one whose ref is there and whose tree is empty, which is a different
-- thing: canon exists and holds nothing.
emptyCanon :: CanonSource IO
emptyCanon = noCanon { csCommit = pure (Right "deadbeef") }

runWith :: CanonSource IO -> (forall a . (CanonSource IO -> IO a) -> IO a)
runWith cs act = act cs

spec :: Spec
spec =
  describe "PEP-19 canon: what a verb does when there is none" $ do

    -- The half that lets a verb CREATE canon. accept has always done this; as
    -- of the same change so does `maintainer add`, because naming a
    -- co-maintainer before anybody has filed an issue is an ordinary order of
    -- work and refusing it was an accident of two verbs writing the answer
    -- separately.
    it "hands a verb that creates canon no parent and an empty fold" $ do
      repo <- aKey
      (parent, fr) <- withCanon TreatAsEmpty repo (runWith noCanon)
      parent `shouldBe` Nothing
      frThreads fr `shouldBe` mempty
      frMaxSeq fr `shouldBe` 0

    -- NOT mempty, and this is the behaviour change worth pinning: the owner is
    -- a maintainer BY DEFINITION rather than by any event, so the fold of no
    -- events still has the repository key in it. `hub updates` used to answer
    -- an empty set here, which refused even an ack signed by the owner of a
    -- repository whose canon this node had never fetched.
    it "counts the owner as a maintainer of a canon that does not exist" $ do
      repo <- aKey
      (_, fr) <- withCanon TreatAsEmpty repo (runWith noCanon)
      frMaintainers fr `shouldBe` HS.singleton repo

    -- A ref that is there and a tree that is empty is not the same state, and
    -- the reader must not collapse them: this one HAS a parent to commit onto.
    it "tells canon that does not exist from canon that is empty" $ do
      repo <- aKey
      (parent, _) <- withCanon TreatAsEmpty repo (runWith emptyCanon)
      parent `shouldBe` Just "deadbeef"

    it "gives a verb that only reads the same fold either way" $ do
      repo <- aKey
      (_, a) <- withCanon TreatAsEmpty repo (runWith noCanon)
      (_, b) <- withCanon TreatAsEmpty repo (runWith emptyCanon)
      frThreads a `shouldBe` frThreads b
      frMaintainers a `shouldBe` frMaintainers b
