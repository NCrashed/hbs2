-- | The triage deny-list (PEP-21 "Deny-lists").
--
-- The layer that decides what enters canon, and the one thing here that is
-- deliberately NOT canon: PEP-21 defers a published ban to hub-meta 2, so this
-- is local, unsigned state. What can be asked of it is what it holds, what it
-- refuses to read, and who it lets through.
module HBS2.Hub.BanSpec (spec) where

import HBS2.Hub.Types (HubKey)
import HBS2.Hub.CLI.Ban
import HBS2.Hub.Deny
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))

import Data.Config.Suckless
import Data.HashSet qualified as HS
import Data.List (isInfixOf)
import Prettyprinter (pretty)
import Data.Text qualified as Text
import Data.Time.Clock (getCurrentTime,diffUTCTime)
import Control.Exception (bracket)
import Data.Text.IO qualified as Text
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv,setEnv,unsetEnv)
import System.FilePath (takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

argv :: [String] -> [Syntax C]
argv = fmap argvAtom

b58 :: HubKey -> String
b58 = show . pretty . AsBase58

spec :: Spec
spec = keyNames >> theWire >> do

  describe "PEP-21 triage layer: the list" $ do

    it "round-trips through the file it is stored as" $ do
      a <- aKey ; b <- aKey
      let ks = HS.fromList [a,b]
      parseBans (renderBans ks) `shouldBe` Right ks

    it "writes one file whatever order the set was built in" $ do
      a <- aKey ; b <- aKey
      renderBans (HS.fromList [a,b]) `shouldBe` renderBans (HS.fromList [b,a])

    it "reads an empty file as nobody" $
      parseBans "" `shouldBe` Right HS.empty

    -- A deny-list that quietly drops what it cannot read is one an attacker
    -- shortens by writing something odd into it. Every line has to parse or
    -- the whole file is refused.
    it "refuses a file it cannot read rather than reading part of it" $ do
      a <- aKey
      let good = renderBans (HS.fromList [a])
      parseBans (good <> "(ban not-a-key)\n") `shouldSatisfy` isLeft
      parseBans (good <> "(banish x)\n") `shouldSatisfy` isLeft
      parseBans "«" `shouldSatisfy` isLeft

    -- WHY THE READER TAKES A LINE AT A TIME. `parseTop` is superlinear in the
    -- number of forms handed to it at once: this file measured 42 ms at 1024
    -- bans, 136 ms at 2048 and 1.97 s at 8192, and `loadBans` runs on every
    -- accept, so a hub that bans steadily paid a quadratic for it. Line by line
    -- it is linear: 0.41 s at 16384, where the whole-file reader would be
    -- around eight seconds.
    --
    -- A clock in a test is a blunt instrument, so the threshold sits between
    -- the two by a wide margin in both directions: what it catches is the shape
    -- going back, not a machine having a bad day.
    it "reads a long deny-list in time linear in its length" $ do
      a <- aKey
      let txt = Text.concat (replicate 16384 (Text.pack ("(ban " <> b58 a <> ")\n")))
      t0 <- getCurrentTime
      parseBans txt `shouldBe` Right (HS.fromList [a])
      t1 <- getCurrentTime
      diffUTCTime t1 t0 `shouldSatisfy` (< 3)

    -- The bound is on the LINE and not on the file, which is the difference
    -- between this list and a stranger's manifest: it is the operator's own and
    -- grows by them using the verb that writes it, so a bound on the file would
    -- eventually stop an accept over a list nobody did anything wrong with.
    it "reads several bans on one line, and refuses a line past the bound" $ do
      a <- aKey ; b <- aKey
      let one k = Text.pack ("(ban " <> b58 k <> ")")
      parseBans (one a <> " " <> one b) `shouldBe` Right (HS.fromList [a,b])
      -- Over the byte bound: still every clause well formed, and still refused,
      -- with the reason named rather than "the file will not read".
      case parseBans (one a <> " ; " <> Text.replicate 300 "x") of
        Left e -> Text.unpack e `shouldSatisfy` isInfixOf "over the bound"
        Right _ -> expectationFailure "a line past the byte bound was read"

    it "lets everybody through when nobody is banned" $ do
      a <- aKey
      allowedBy HS.empty a `shouldBe` True

    it "stops exactly the keys it holds" $ do
      a <- aKey ; b <- aKey
      let bans = HS.fromList [a]
      allowedBy bans a `shouldBe` False
      allowedBy bans b `shouldBe` True

  describe "PEP-21 triage layer: where it lives" $

    -- Outside the working tree, so that a list this build cannot publish does
    -- not get committed by somebody's `git add -A`. Keyed by repository,
    -- because one node may serve two and must not confuse them.
    it "keys the file by repository, outside any repository" $ do
      a <- aKey ; b <- aKey
      pa <- banPath a
      pb <- banPath b
      pa `shouldSatisfy` (/= pb)
      pa `shouldSatisfy` (b58 a `isInfixOf`)
      pa `shouldSatisfy` ("hbs2-hub" `isInfixOf`)

  describe "PEP-21 triage layer: arguments" $ do

    it "reads the repo alone, for list" $ do
      repo <- aKey
      banArgs (argv ["--repo", b58 repo]) `shouldBe` Just (BanArgs repo Nothing)

    it "reads the author when one is given" $ do
      repo <- aKey ; k <- aKey
      banArgs (argv ["--repo", b58 repo, "--key", b58 k])
        `shouldBe` Just (BanArgs repo (Just k))

    it "refuses a form with no repository" $ do
      k <- aKey
      banArgs (argv ["--key", b58 k]) `shouldBe` Nothing

    it "refuses a repeated flag rather than choosing one" $ do
      repo <- aKey ; other <- aKey
      banArgs (argv ["--repo", b58 repo, "--repo", b58 other]) `shouldBe` Nothing

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

-- | @--key@ MEANT FOUR DIFFERENT KEYS.
--
-- An inner author at `ban`, an envelope key at `block`, a canon signer at
-- `maintainer add`, a person at `assign --to` -- all thirty-two bytes of base58,
-- so every swap between them was well-typed and silent. The verbs keep their
-- names, which say what they DO, and the flag says which layer, which is the
-- half that was missing: a reader who has the flag right cannot have the layer
-- wrong.
--
-- Both spellings, for one release. The old one still works and is no longer
-- printed, exactly as `--target` is handled.
keyNames :: Spec
keyNames =
  describe "PEP-22 the four keys that were all called --key" $ do

    it "reads a ban under its own name and under the old one" $ do
      repo <- aKey ; who <- aKey
      let want = Just (BanArgs repo (Just who))
      banArgs (argv ["--repo", b58 repo, "--author-key", b58 who]) `shouldBe` want
      banArgs (argv ["--repo", b58 repo, "--key", b58 who]) `shouldBe` want

    -- The repository's flags come from 'repoFlags' now, and this is the bug
    -- that fixes: `ban` built its flag list by hand, so --target worked on
    -- `ban list` and not on `ban`, one verb apart.
    it "takes --target on the verb as well as on the listing" $ do
      repo <- aKey ; who <- aKey
      banArgs (argv ["--target", b58 repo, "--author-key", b58 who])
        `shouldBe` Just (BanArgs repo (Just who))

    -- Two spellings of one value, so a line carrying both is a line somebody
    -- edited half way, and choosing between them is a guess.
    it "refuses both spellings at once" $ do
      repo <- aKey ; who <- aKey ; other <- aKey
      banArgs (argv ["--repo", b58 repo, "--author-key", b58 who, "--key", b58 other])
        `shouldBe` Nothing
      banArgs (argv ["--repo", b58 repo, "--target", b58 other, "--author-key", b58 who])
        `shouldBe` Nothing

-- | THE WIRE, FROM THE FILE TO THE ANSWER.
--
-- Every case above proves that a PREDICATE refuses a banned author, and none
-- of them proved the predicate is ever built out of the file: `loadBans` could
-- be made to answer "nobody is banned" with the whole suite green. That is the
-- one thing PEP-21's banning consists of, in three verbs, unasserted.
--
-- Against a real filesystem, under a temporary XDG root, because the path is
-- part of the wire: two hubs serving one repository keep separate lists, and
-- one hub serving two repositories must not confuse them.
theWire :: Spec
theWire =
  describe "PEP-21 triage layer: from the file to the answer" $ do

    it "builds the predicate the triage loop applies out of the file on disk" $
      inXdg $ \_ -> do
        repo <- aKey ; alice <- aKey ; bob <- aKey
        _ <- saveBans repo (HS.fromList [alice])
        allowed <- denyingFor (Just repo) >>= either (fail . Text.unpack) pure
        allowed alice `shouldBe` False
        allowed bob `shouldBe` True

    -- Keyed by repository, which is what the path is for: a node may serve two
    -- and must not answer one repository's question with the other's list.
    it "keeps two repositories' lists apart" $ inXdg $ \_ -> do
      one <- aKey ; two <- aKey ; alice <- aKey
      _ <- saveBans one (HS.fromList [alice])
      here  <- denyingFor (Just one) >>= either (fail . Text.unpack) pure
      there <- denyingFor (Just two) >>= either (fail . Text.unpack) pure
      here alice `shouldBe` False
      there alice `shouldBe` True

    -- A missing file is "nobody has banned anybody here", and a damaged one is
    -- NOT: a deny-list that reads as empty when it is broken is a deny-list
    -- that stops working silently, which is the failure this layer exists to
    -- prevent.
    it "reads a missing list as empty and refuses a damaged one" $ inXdg $ \_ -> do
      repo <- aKey ; alice <- aKey
      denyingFor (Just repo) >>= \case
        Right allowed -> allowed alice `shouldBe` True
        Left e -> expectationFailure ("a missing list refused: " <> Text.unpack e)
      p <- banPath repo
      createDirectoryIfMissing True (takeDirectory p)
      Text.writeFile p "(ban not-a-key)\n"
      denyingFor (Just repo) >>= \case
        Left _  -> pure ()
        Right _ -> expectationFailure "a damaged list read as a predicate"

    -- No repository named is not an empty list: it is "there is no list to
    -- apply", which is the form that reads a mailbox by key alone.
    it "applies nothing when no repository was named" $ do
      alice <- aKey
      denyingFor Nothing >>= either (fail . Text.unpack) (\f -> f alice `shouldBe` True)

-- The XDG root the store reads, pointed at a temporary directory for the
-- length of one case and put back afterwards.
inXdg :: (FilePath -> IO a) -> IO a
inXdg act =
  withSystemTempDirectory "hub-ban" $ \dir ->
    bracket (lookupEnv "XDG_DATA_HOME")
            (maybe (unsetEnv "XDG_DATA_HOME") (setEnv "XDG_DATA_HOME"))
            (const (setEnv "XDG_DATA_HOME" dir >> act dir))
