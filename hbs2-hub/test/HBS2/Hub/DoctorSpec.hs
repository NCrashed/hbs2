-- | The product-level doctor is a pure report over facts gathered by the hub.
module HBS2.Hub.DoctorSpec (spec) where

import HBS2.Hub.CLI.Doctor
import HBS2.Peer.CLI.Doctor

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.List (find)
import Data.Text qualified as Text
import Test.Hspec

spec :: Spec
spec = do
  describe "D4 forge doctor" $ do
    it "adds the four forge boundaries after the peer report" $ do
      let report = combineDoctor baseDoctor healthyForge
      fmap doctorCheckName (doctorChecks report)
        `shouldBe` [ "rpc", "forge-mailbox", "forge-policy"
                   , "forge-sender", "forge-delivery" ]
      doctorPassed report `shouldBe` True

    it "stops at a repository with no forge mailbox" $ do
      let report = combineDoctor baseDoctor healthyForge
            { forgeMailbox = ForgeMailboxMissing }
      statusOf "forge-mailbox" report `shouldBe` Just CheckFailure
      statusOf "forge-policy" report `shouldBe` Just CheckSkipped
      statusOf "forge-sender" report `shouldBe` Just CheckSkipped
      statusOf "forge-delivery" report `shouldBe` Just CheckSkipped

    it "warns when the destination policy is not locally observable" $ do
      let report = combineDoctor baseDoctor healthyForge
            { forgePolicy = ForgePolicyUnavailable "this peer does not hold it" }
      statusOf "forge-policy" report `shouldBe` Just CheckWarning
      statusOf "forge-sender" report `shouldBe` Just CheckOK
      doctorPassed report `shouldBe` True

    it "fails a mailbox with no policy before sender claims" $ do
      let report = combineDoctor baseDoctor healthyForge
            { forgePolicy = ForgePolicyMissing }
      statusOf "forge-policy" report `shouldBe` Just CheckFailure
      statusOf "forge-sender" report `shouldBe` Just CheckSkipped

    it "reports every local sender problem" $ do
      let report = combineDoctor baseDoctor healthyForge
            { forgeSender = ForgeSenderChecked
                ["no signing key", "sender sigil names another key"] []
                "author-a with sender-sigil" }
          sender = checkNamed "forge-sender" report
      doctorCheckStatus sender `shouldBe` CheckFailure
      doctorCheckMessage sender `shouldSatisfy`
        Text.isInfixOf "sender sigil names another key"

    it "keeps an excessive outbound PoW floor as a warning" $ do
      let report = combineDoctor baseDoctor healthyForge
            { forgeSender = ForgeSenderChecked []
                ["outbound road asks 25 bits; the client cap is 22"]
                "author-a with sender-sigil" }
      statusOf "forge-sender" report `shouldBe` Just CheckWarning
      doctorPassed report `shouldBe` True

    it "calls local handoff local and never delivery" $ do
      let report = combineDoctor baseDoctor healthyForge
            { forgeDelivery = ForgeLocalHandoff "message-hash" 1772323200000 }
          delivery = checkNamed "forge-delivery" report
      doctorCheckStatus delivery `shouldBe` CheckWarning
      doctorCheckMessage delivery `shouldSatisfy` Text.isInfixOf "local-peer"
      doctorCheckMessage delivery `shouldSatisfy` Text.isInfixOf "not delivery"

    it "recognises canon as stronger evidence" $ do
      let report = combineDoctor baseDoctor healthyForge
            { forgeDelivery = ForgeCanonPublished "message-hash" "event-hash" }
      statusOf "forge-delivery" report `shouldBe` Just CheckOK
      doctorCheckMessage (checkNamed "forge-delivery" report)
        `shouldSatisfy` Text.isInfixOf "canon-published"

  describe "the lower-level doctor JSON seam" $ do
    it "reads contract 1 without losing statuses or remedies" $ do
      let bytes = Aeson.encode (doctorJSON baseDoctor)
      decodePeerDoctor bytes `shouldBe` Right baseDoctor

    it "refuses a document that is not a doctor report" $ do
      decodePeerDoctor (LBS.pack "{\"contract\":1,\"document\":\"sent\",\"checks\":[]}")
        `shouldSatisfy` either (Text.isInfixOf "document") (const False)

baseDoctor :: DoctorReport
baseDoctor = DoctorReport
  [DoctorCheck "rpc" CheckOK "peer answered" Nothing]

healthyForge :: ForgeFacts
healthyForge = ForgeFacts
  { forgeMailbox = ForgeMailboxFound "mailbox-a" (Just "public")
  , forgePolicy = ForgePolicyCurrent 3 18
  , forgeSender = ForgeSenderChecked [] [] "author-a with sender-sigil"
  , forgeDelivery = ForgeNothingHanded
  }

statusOf :: Text.Text -> DoctorReport -> Maybe CheckStatus
statusOf name = fmap doctorCheckStatus . findCheck name

findCheck :: Text.Text -> DoctorReport -> Maybe DoctorCheck
findCheck name = find ((== name) . doctorCheckName) . doctorChecks

checkNamed :: Text.Text -> DoctorReport -> DoctorCheck
checkNamed name report = case findCheck name report of
  Just check -> check
  Nothing -> error ("missing doctor check: " <> Text.unpack name)
