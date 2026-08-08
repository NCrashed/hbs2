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
import HBS2.Hub.CLI.Common (withCanon,OnMissing(..),blessed,oneStop,WriteStop(..)
                           ,signerOf,signingPair)
import HBS2.Hub.Bridge (TriageError(..))

import HBS2.Net.Auth.Credentials

import Data.HashSet qualified as HS
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Control.Monad (void)
import Control.Exception (try,bracket)
import GHC.IO.Handle (hDuplicate,hDuplicateTo)
import System.Exit (ExitCode(..))
import System.IO
import System.Posix.IO qualified as Posix
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
spec = spec1 >> spec2 >> spec3

-- | The check between keyman's answer and the key a verb asked about.
--
-- WHY THIS IS HERE AT ALL. @loadCredentials@ resolves a key to the FILE that
-- holds it and hands back that file's PRIMARY credentials, so for a key that is
-- a secondary in its keyring the secret returned belongs to somebody else.
-- Every canon writer signed with it and then declared the key it had asked for:
-- the verb printed an event, a seq and a commit, exited 0, and the fold dropped
-- the event as 'BadAuthorSig' in every clone. Nothing downstream could catch it,
-- because the bridge is handed a secret key and told whose it is.
--
-- Asserted on 'signerOf' rather than 'signerFor' because the latter wants a key
-- database. The comparison IS the fix, so it is the thing pinned: delete it and
-- the second case below goes green-to-red, which is what nothing did before.
spec3 :: Spec
spec3 =
  describe "keyman: the credentials answered are the credentials asked for" $ do

    -- On the ANSWER's shape and not on the record: 'PeerCredentials' has no
    -- Show, and a secret key is not a thing a failing test should print.
    it "takes a record whose sign key is the key asked about" $ do
      alice <- newCredentials @'HBS2Basic
      isJust (signerOf (_peerSignPk alice) (Just alice)) `shouldBe` True

    it "refuses a record for a DIFFERENT key, which is the keyring case" $ do
      alice <- newCredentials @'HBS2Basic
      bob   <- newCredentials @'HBS2Basic
      -- What keyman does when alice's key is a secondary in bob's keyring: it
      -- finds the file, and the file's own sign key is bob's.
      isJust (signerOf (_peerSignPk alice) (Just bob)) `shouldBe` False

    it "refuses when keyman found nothing" $ do
      alice <- newCredentials @'HBS2Basic
      isJust (signerOf (_peerSignPk alice) Nothing) `shouldBe` False

    it "takes both halves of a signing pair out of one record" $ do
      alice <- newCredentials @'HBS2Basic
      -- The declared key is the record's own, which is what makes a
      -- TriageCtx built from it unable to disagree with itself.
      fst (signingPair alice) `shouldBe` _peerSignPk alice

spec1 :: Spec
spec1 =
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

-- | What a verb says when the bridge would not bless the event.
--
-- WHY THIS IS ASSERTED ON THE TEXT. Three of the four verbs that mint printed
-- @viaShow e@, the derived 'Show', so a contributor whose pull request arrived
-- with nothing to fetch was told @BadContent CoordsUnreachable@ -- and that is
-- the one refusal in the family they could have acted on. 'TriageError' has a
-- hand-written 'Pretty' instance written for exactly this moment. A test on the
-- exit code alone passes either way, which is how it went unnoticed.
spec2 :: Spec
spec2 =
  describe "PEP-22: what a verb says when the bridge refuses" $ do

    it "prints the sentence the bridge wrote, not its constructor" $ do
      (code, said) <- saidBy (blessed 23 (Left (BadContent CoordsUnreachable))
                                :: IO ())
      code `shouldBe` Just (ExitFailure 23)
      said `shouldSatisfy` isInfixOf "coordinates with nothing to fetch"
      -- The spelling it used to have. Named so that going back to viaShow is a
      -- failing test rather than a silent regression in what a stranger reads.
      said `shouldSatisfy` (not . isInfixOf "CoordsUnreachable")

    it "carries the verb's own exit code, not one of its own" $ do
      -- `hub pr merge` and the maintainer verbs each decided that every way of
      -- stopping means one thing to whoever ran them. The shared helper must
      -- not overrule that.
      (code, _) <- saidBy (blessed 31 (Left (BadContent PROnlyOnIssue)) :: IO ())
      code `shouldBe` Just (ExitFailure 31)

    it "says nothing at all when the bridge blessed it" $ do
      (code, said) <- saidBy (blessed 23 (Right 'x'))
      code `shouldBe` Nothing
      said `shouldBe` ""

    it "keeps one code for every way of stopping when that is the decision" $ do
      -- Two fields and not one, and the two verbs that want them equal say so
      -- once here instead of passing the same number twice at the call site.
      wsUnplannable (oneStop 30) `shouldBe` (30 :: Int)
      wsUnwritable (oneStop 30) `shouldBe` 30

-- | Run something that may exit, with stderr caught, and give back both.
--
-- 'Nothing' for the code means it did not exit. The pipe is fine for these:
-- what a refusal writes is a line, not a buffer's worth.
saidBy :: IO a -> IO (Maybe ExitCode, String)
saidBy act = do
  (r, w) <- Posix.createPipe
  rh <- Posix.fdToHandle r
  wh <- Posix.fdToHandle w
  got <- bracket (hDuplicate stderr)
           (\o -> hDuplicateTo o stderr >> hClose o)
           (\_ -> do hDuplicateTo wh stderr
                     hSetBuffering stderr NoBuffering
                     try @ExitCode (void act))
  hClose wh
  said <- hGetContents' rh
  hClose rh
  pure (either Just (const Nothing) got, said)
