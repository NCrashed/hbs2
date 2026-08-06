-- | The argument reading of the owner-native verbs (PEP-22 "Maintain").
--
-- Its own module for the reason @hub pr new@'s is: these verbs sign, and what
-- they sign is published into append-only canon. Two of their values are keys
-- of the same thirty-two bytes of base58 (the repository and a delegate), and
-- one is a number that names a thread nobody can rename afterwards.
module HBS2.Hub.OwnSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.CLI.Own
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))
import HBS2.Hash (hashObject)
import HBS2.Data.Types.Refs (HashRef(..))

import Data.Config.Suckless
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Maybe (isJust)
import Prettyprinter (pretty)
import Test.Hspec

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

b58 :: HubKey -> String
b58 = show . pretty . AsBase58

argv :: [String] -> [Syntax C]
argv = fmap argvAtom

spec :: Spec
spec = do

  describe "PEP-22 hub issue close|reopen|label: arguments" $ do

    it "reads a complete call, in any order" $ do
      repo <- aKey
      let want = Just (OwnArgs repo 7 (Just "done") [] False Nothing Nothing)
      ownArgs (argv ["--repo", b58 repo, "--number", "7", "--note", "done"])
        `shouldBe` want
      ownArgs (argv ["--note", "done", "--number", "7", "--repo", b58 repo])
        `shouldBe` want

    it "takes a delegate's key when one is named" $ do
      repo <- aKey ; bob <- aKey
      fmap owAs (ownArgs (argv [ "--repo", b58 repo, "--number", "1"
                               , "--as", b58 bob ]))
        `shouldBe` Just (Just bob)

    it "refuses a call with no repository or no number" $ do
      repo <- aKey
      ownArgs (argv ["--number", "7"]) `shouldBe` Nothing
      ownArgs (argv ["--repo", b58 repo]) `shouldBe` Nothing

    -- A number is a Word64 in canon, and the guard against a negative is not
    -- the only end that matters: this used to be the shape that wrapped.
    it "refuses a number that is not one" $ do
      repo <- aKey
      let with n = ownArgs (argv ["--repo", b58 repo, "--number", n])
      with "-1" `shouldBe` Nothing
      with "seven" `shouldBe` Nothing
      with "18446744073709551617" `shouldBe` Nothing

    it "takes labels, as many as are given, in the order given" $ do
      repo <- aKey
      fmap owLabels (ownArgs (argv [ "--repo", b58 repo, "--number", "1"
                                   , "--label", "bug", "--label", "ui" ]))
        `shouldBe` Just ["bug","ui"]

    -- The labels REPLACE the set, so an omitted --label would be a verb that
    -- clears them because somebody forgot an argument. --clear says it.
    it "tells an empty label set from an absent one" $ do
      repo <- aKey
      let base = ["--repo", b58 repo, "--number", "1"]
      fmap owClear (ownArgs (argv base)) `shouldBe` Just False
      fmap owClear (ownArgs (argv (base <> ["--clear"]))) `shouldBe` Just True
      -- A switch takes no value: given one, the line does not parse rather than
      -- the value being swallowed as something else's.
      ownArgs (argv (base <> ["--clear", "yes"])) `shouldBe` Nothing

    it "refuses a flag it does not know instead of dropping it" $ do
      repo <- aKey
      ownArgs (argv ["--repo", b58 repo, "--number", "1", "--dry-run"])
        `shouldBe` Nothing
      ownArgs (argv ["--repo", b58 repo, "--number", "1", "--nete", "x"])
        `shouldBe` Nothing

    it "refuses a flag standing where a value belongs" $ do
      repo <- aKey
      ownArgs (argv ["--repo", b58 repo, "--number", "--note", "done"])
        `shouldBe` Nothing

    -- A note is free text, and argvAtom keeps a numeric word as a number.
    it "takes a note that spells a number" $ do
      repo <- aKey
      fmap owNote (ownArgs (argv [ "--repo", b58 repo, "--number", "1"
                                 , "--note", "2026" ]))
        `shouldBe` Just (Just "2026")

  describe "PEP-22 hub issue assign: arguments" $ do

    it "reads a key to assign to" $ do
      repo <- aKey ; bob <- aKey
      fmap owTo (ownArgs (argv [ "--repo", b58 repo, "--number", "7"
                               , "--to", b58 bob ]))
        `shouldBe` Just (Just bob)

    -- The attribute is last-writer-wins and there is no way to remove one, so
    -- an empty value IS the absence. Saying it out loud is the same rule the
    -- labels follow: an unassign because somebody forgot an argument would be
    -- published into append-only canon.
    it "tells unassigning from forgetting the key" $ do
      repo <- aKey
      let base = ["--repo", b58 repo, "--number", "7"]
      fmap (\a -> (owTo a, owClear a)) (ownArgs (argv base))
        `shouldBe` Just (Nothing, False)
      fmap (\a -> (owTo a, owClear a)) (ownArgs (argv (base <> ["--clear"])))
        `shouldBe` Just (Nothing, True)

    it "refuses a --to that is not a key rather than ignoring it" $ do
      repo <- aKey
      ownArgs (argv ["--repo", b58 repo, "--number", "7", "--to", "bob"])
        `shouldBe` Nothing

  describe "PEP-19 hub redact: arguments" $ do

    it "reads a complete call" $ do
      repo <- aKey
      let e = HashRef (hashObject (LBS.pack "an event"))
      redactArgs (argv ["--repo", b58 repo, "--event", show (pretty e)])
        `shouldBe` Just (RedactArgs repo e Nothing)

    it "refuses a call with no event, and one whose event is not a hash" $ do
      repo <- aKey
      redactArgs (argv ["--repo", b58 repo]) `shouldBe` Nothing
      redactArgs (argv ["--repo", b58 repo, "--event", "not-a-hash"])
        `shouldBe` Nothing

    -- An event-id and a key are both thirty-two bytes of base58, so nothing
    -- here can be told apart by shape and the flags are what say which is
    -- which. This records that the parser cannot catch a swap.
    it "cannot tell a key from a hash, which is why every value is flagged" $ do
      repo <- aKey
      isJust (redactArgs (argv ["--repo", b58 repo, "--event", b58 repo]))
        `shouldBe` True
