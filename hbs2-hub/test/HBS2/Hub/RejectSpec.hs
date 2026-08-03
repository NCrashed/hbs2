-- | The argument reading of @hub inbox reject@ (PEP-21 "Retention").
--
-- The first verb in this build that deletes anything. Two of its three values
-- are a key and a hash, which spell the same, and the third decides whether
-- the check that protects canon's attachments runs at all: a call missing
-- @--repo@ deletes without asking canon, which is allowed and has to be
-- visible in what the verb answers.
module HBS2.Hub.RejectSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.CLI.Reject
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))
import HBS2.Hash (hashObject)
import HBS2.Data.Types.Refs (HashRef(..))

import Data.Config.Suckless
import Data.ByteString.Lazy.Char8 qualified as LBS
import Prettyprinter (pretty)
import Test.Hspec

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

argv :: [String] -> [Syntax C]
argv = fmap argvAtom

b58 :: HubKey -> String
b58 = show . pretty . AsBase58

aHash :: String -> HashRef
aHash s = HashRef (hashObject (LBS.pack s))

spec :: Spec
spec =

  describe "PEP-21 hub inbox reject: arguments" $ do

    it "reads the mailbox and the message, in either order" $ do
      mbox <- aKey
      let h = aHash "a letter"
      rejectArgs (argv ["--mailbox", b58 mbox, "--message", show (pretty h)])
        `shouldBe` Just (Reject mbox h Nothing)
      rejectArgs (argv ["--message", show (pretty h), "--mailbox", b58 mbox])
        `shouldBe` Just (Reject mbox h Nothing)

    -- Optional, and its absence is a real difference rather than a default:
    -- without a repository there is no canon to ask whether this letter was
    -- folded, so the check that keeps a hub from dropping an attachment canon
    -- references cannot run.
    it "takes a repository when one is given, and does without" $ do
      mbox <- aKey
      repo <- aKey
      let h = aHash "a letter"
      fmap rjRepo (rejectArgs (argv [ "--mailbox", b58 mbox
                                    , "--message", show (pretty h)
                                    , "--repo", b58 repo ]))
        `shouldBe` Just (Just repo)
      fmap rjRepo (rejectArgs (argv ["--mailbox", b58 mbox, "--message", show (pretty h)]))
        `shouldBe` Just Nothing

    it "refuses a call with either required value missing" $ do
      k <- aKey
      let h = aHash "a letter"
      rejectArgs (argv ["--mailbox", b58 k]) `shouldBe` Nothing
      rejectArgs (argv ["--message", show (pretty h)]) `shouldBe` Nothing
      rejectArgs (argv [b58 k, show (pretty h)]) `shouldBe` Nothing

    it "refuses two messages rather than deleting one of them" $ do
      mbox <- aKey
      rejectArgs (argv [ "--mailbox", b58 mbox
                       , "--message", show (pretty (aHash "one"))
                       , "--message", show (pretty (aHash "two")) ])
        `shouldBe` Nothing

    it "refuses a repeated mailbox rather than choosing one" $ do
      mbox <- aKey
      other <- aKey
      rejectArgs (argv [ "--mailbox", b58 mbox, "--mailbox", b58 other
                       , "--message", show (pretty (aHash "one")) ])
        `shouldBe` Nothing
