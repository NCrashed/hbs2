module HBS2.Hub.ManifestSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Manifest
import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))
import HBS2.Prelude.Plated (pretty)

import Data.Config.Suckless
import Data.List (isInfixOf)
import Test.Hspec

kp :: IO HubKey
kp = _peerSignPk <$> newCredentials @'HBS2Basic

spec :: Spec
spec = do

  describe "PEP-18 manifest clauses" $ do

    it "reads a single hub mailbox" $ do
      k <- kp
      let syn = [mailboxClause (HubMailbox k hubRole Nothing)]
      hubMailboxes syn `shouldBe` [HubMailbox k hubRole Nothing]

    it "survives a text round-trip" $ do
      k <- kp
      let emitted = show (pretty (mailboxClause (HubMailbox k hubRole Nothing)))
      case parseTop emitted of
        Left e    -> expectationFailure (show e)
        Right syn -> hubMailboxes syn `shouldBe` [HubMailbox k hubRole Nothing]

    it "keeps tiers apart and defaults to the untiered inbox" $ do
      open   <- kp
      known  <- kp
      let syn = [ mailboxClause (HubMailbox open hubRole Nothing)
                , mailboxClause (HubMailbox known hubRole (Just "known"))
                ]
      map mbKey (hubMailboxes syn) `shouldBe` [open, known]
      fmap mbKey (mailboxByTier Nothing syn) `shouldBe` Just open
      fmap mbKey (mailboxByTier (Just "known") syn) `shouldBe` Just known
      mailboxByTier (Just "nope") syn `shouldBe` Nothing

    it "falls back to the public tier when no untiered inbox exists" $ do
      known  <- kp
      public <- kp
      let syn = [ mailboxClause (HubMailbox known hubRole (Just "known"))
                , mailboxClause (HubMailbox public hubRole (Just publicTier))
                ]
      -- A stranger naming no tier must still find somewhere to submit.
      fmap mbKey (mailboxByTier Nothing syn) `shouldBe` Just public

    it "falls back to the first hub mailbox when there is no public tier" $ do
      a <- kp
      b <- kp
      let syn = [ mailboxClause (HubMailbox a hubRole (Just "known"))
                , mailboxClause (HubMailbox b hubRole (Just "trusted"))
                ]
      -- Neither untiered nor public: a default submitter still needs an
      -- address, so the first declared inbox is it.
      fmap mbKey (mailboxByTier Nothing syn) `shouldBe` Just a

    it "writes the role and tier bare, as the spec does" $ do
      -- PEP-18 shows (mailbox KEY hub known); the parser takes a string too,
      -- but an emitter that always quoted would not match its own examples.
      k <- kp
      let emitted = show (pretty (mailboxClause (HubMailbox k hubRole (Just "known"))))
      emitted `shouldSatisfy` isInfixOf "hub known"
      -- ...and a value that could not be read back as a symbol is quoted.
      let quoted = show (pretty (mailboxClause (HubMailbox k hubRole (Just "2 tiers"))))
      quoted `shouldSatisfy` isInfixOf "\"2 tiers\""

    it "keeps a tier containing a space intact" $ do
      k <- kp
      let emitted = show (pretty (mailboxClause (HubMailbox k hubRole (Just "known good"))))
      case parseTop emitted of
        Left e    -> expectationFailure (show e)
        Right syn -> hubMailboxes syn `shouldBe` [HubMailbox k hubRole (Just "known good")]

    it "keeps a tier written to break the manifest intact" $ do
      k <- kp
      -- The quoted branch used to hand the printer the text unchanged, and the
      -- printer wraps a literal in quotes and does nothing else. A tier with a
      -- quote in it would close the string early and everything after it in the
      -- manifest would read back as whatever it looked like, including the
      -- mailbox clauses that say where submissions go.
      let nasty = "a\"b\\c\nd"
          emitted = show (pretty (mailboxClause (HubMailbox k hubRole (Just nasty))))
      case parseTop emitted of
        Left e    -> expectationFailure (show e)
        Right syn -> do
          -- one clause, not two, and the tier comes back exactly as it went in
          length (syn :: [Syntax C]) `shouldBe` 1
          hubMailboxes syn `shouldBe` [HubMailbox k hubRole (Just nasty)]

    it "accepts string literals as well as symbols" $ do
      k <- kp
      let b58 = show (pretty (AsBase58 k))
      case parseTop ("(mailbox \"" <> b58 <> "\" \"hub\" \"known\")") of
        Left e    -> expectationFailure (show e)
        Right syn -> hubMailboxes syn `shouldBe` [HubMailbox k hubRole (Just "known")]

    it "ignores mailboxes with another role" $ do
      hub   <- kp
      other <- kp
      let syn = [ mailboxClause (HubMailbox hub hubRole Nothing)
                , mailboxClause (HubMailbox other "backup" Nothing)
                ]
      map mbKey (mailboxes syn) `shouldBe` [hub, other]
      map mbKey (hubMailboxes syn) `shouldBe` [hub]

    it "round-trips sigil clauses through text" $ do
      mb <- kp
      let h = "5KKfLTe5aDgvC7fBdmiRr5FMYVUsvJqBNSNK2R6MYo2A"
          emitted = show (pretty (mailboxSigilClause (MailboxSigil mb h)))
      case parseTop emitted of
        Left e    -> expectationFailure (show e)
        Right syn -> sigils syn `shouldBe` [MailboxSigil mb h]

    it "binds each sigil to its own mailbox" $ do
      mb1 <- kp
      mb2 <- kp
      let h1 = "5KKfLTe5aDgvC7fBdmiRr5FMYVUsvJqBNSNK2R6MYo2A"
          h2 = "8yqJyq5jxKmDdMwzGvQhx3srKuc1FqmxCYSZKz5yzXWJ"
          h3 = "CGaG9zbBaPPd5vgvVaKcXbhTnGmDCLqELx4WwCsNRAgc"
          syn = [ mailboxSigilClause (MailboxSigil mb1 h1)
                , mailboxSigilClause (MailboxSigil mb1 h2)
                , mailboxSigilClause (MailboxSigil mb2 h3)
                ]
      -- two maintainers read mb1, so a sender seals to both
      sigilsFor mb1 syn `shouldBe` [h1,h2]
      sigilsFor mb2 syn `shouldBe` [h3]

    it "ignores unrelated clauses" $ do
      k <- kp
      case parseTop ("(hbs2-git 3)\n(seed 42)\n(public)\n"
                      <> show (pretty (mailboxClause (HubMailbox k hubRole Nothing)))) of
        Left e    -> expectationFailure (show e)
        Right syn -> do
          map mbKey (hubMailboxes syn) `shouldBe` [k]
          sigils syn `shouldBe` []
