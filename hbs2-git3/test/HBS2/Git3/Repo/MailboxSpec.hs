-- | The manifest clauses that say where a repository takes contributions.
--
-- WHAT THESE ARE GUARDING. The clause is spelled in this package and read in
-- another one (@hbs2-hub@, "HBS2.Hub.Manifest"), and nothing but agreement
-- links the two. So the assertions below are on the TEXT the manifest will
-- hold, not on the value: what the hub eventually parses is bytes read out of a
-- tree, and a clause that is a fine 'Syntax' value and renders to something
-- else is exactly the failure the round trip exists to catch.
--
-- The literal strings are therefore the point of this file, and the same ones
-- are asserted from the other side in @hbs2-hub@'s ManifestSpec, as what its
-- reader ACCEPTS. Changing one should be as loud as changing a wire format,
-- because between two packages that is what it is.
module HBS2.Git3.Repo.MailboxSpec (spec) where

import HBS2.Git3.Repo.Mailbox

import HBS2.Data.Types.Refs (HashRef)
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema ()
import HBS2.Prelude.Plated (fromStringMay,fromString)

import Data.Config.Suckless.Script

import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Test.Hspec

-- Fixed keys, so the expected text below can be written out in full.
aKey :: PubKey 'Sign 'HBS2Basic
aKey = aKeyOf "2v3ubvkrQaWzhBCZW14JDWUW1LGBMnkirWeJJvZxnNUV"

bKey :: PubKey 'Sign 'HBS2Basic
bKey = aKeyOf "5KKfLTe5aDgvC7fBdmiRr5FMYVUsvJqBNSNK2R6MYo2A"

aKeyOf :: String -> PubKey 'Sign 'HBS2Basic
aKeyOf s = fromMaybe (error ("not a key: " <> s)) (fromStringMay s)

aHash :: HashRef
aHash = fromMaybe (error "not a hash")
          (fromStringMay "8yqJyq5jxKmDdMwzGvQhx3srKuc1FqmxCYSZKz5yzXWJ")

-- What repo:init leaves behind, near enough: the clauses an edit must not
-- disturb.
base :: [Syntax C]
base =
  [ mkForm (fromString "hbs2-git" :: Id) [mkInt (3 :: Int)]
  , mkForm (fromString "seed" :: Id) [mkInt (42 :: Int)]
  , mkForm (fromString "public" :: Id) []
  ]

setOK :: PubKey 'Sign 'HBS2Basic -> Maybe Text -> [Syntax C] -> [Syntax C]
setOK k t mf = either (error . show) id (setMailbox k t mf)

-- | The rendered clause with its line breaks taken out.
--
-- THE TOKENS ARE THE FORMAT; THE LINE BREAKS ARE THE PRINTER'S. A clause wider
-- than the layout's page really is stored wrapped -- the sigil one is, since
-- two base58 keys and a name are over eighty columns -- and an S-expression
-- spans lines perfectly well, which the round-trip cases at the bottom prove
-- against the same bytes. Pinning where the printer chose to break would be
-- pinning something no reader depends on and every layout change would move.
oneLine :: [Syntax C] -> String
oneLine = unwords . words . renderManifest

spec :: Spec
spec = do

  describe "PEP-18: the mailbox clause a repository declares" $ do

    it "renders the shape the hub reads" $ do
      oneLine [either (error . show) id (mailboxClause aKey Nothing)]
        `shouldBe` "(mailbox 2v3ubvkrQaWzhBCZW14JDWUW1LGBMnkirWeJJvZxnNUV hub)"

    it "renders a tier as a fourth field" $ do
      oneLine [either (error . show) id (mailboxClause aKey (Just "public"))]
        `shouldBe` "(mailbox 2v3ubvkrQaWzhBCZW14JDWUW1LGBMnkirWeJJvZxnNUV hub public)"

    it "refuses an empty tier rather than emitting a clause that loses a field" $ do
      -- (mailbox KEY hub "") reads back as a mailbox with NO tier, which is a
      -- different mailbox: the untiered one is the inbox a stranger is sent to.
      -- There is no spelling to escape into, so this is a refusal and not a
      -- best effort.
      isLeft (mailboxClause aKey (Just "")) `shouldBe` True

    it "quotes a tier that would not lex back as itself" $ do
      -- A leading digit lexes back as a number, a space as two atoms. Both
      -- would change which mailbox the clause names.
      oneLine [either (error . show) id (mailboxClause aKey (Just "2nd tier"))]
        `shouldBe` "(mailbox 2v3ubvkrQaWzhBCZW14JDWUW1LGBMnkirWeJJvZxnNUV hub \"2nd tier\")"

    it "renders the sigil clause the hub reads" $ do
      oneLine [mailboxSigilClause aKey aHash]
        `shouldBe` "(mailbox-sigil 2v3ubvkrQaWzhBCZW14JDWUW1LGBMnkirWeJJvZxnNUV \
                   \8yqJyq5jxKmDdMwzGvQhx3srKuc1FqmxCYSZKz5yzXWJ)"

  describe "PEP-18: editing a manifest" $ do

    it "leaves the clauses repo:init wrote alone" $ do
      renderManifest (take (length base) (setOK aKey Nothing base))
        `shouldBe` renderManifest base

    it "adds the mailbox at the end" $ do
      declaredMailboxes (setOK aKey Nothing base)
        `shouldBe` [(aKey, "hub", Nothing)]

    it "edits its own clause rather than adding a second" $ do
      -- Two (mailbox K hub ...) for one key would make the tier lookup answer
      -- from whichever came first, which is not an answer.
      let mf = setOK aKey (Just "trusted") (setOK aKey Nothing base)
      declaredMailboxes mf `shouldBe` [(aKey, "hub", Just "trusted")]

    it "keeps a second mailbox, which is a different inbox" $ do
      let mf = setOK bKey (Just "trusted") (setOK aKey Nothing base)
      declaredMailboxes mf `shouldBe` [(aKey, "hub", Nothing), (bKey, "hub", Just "trusted")]

    it "keeps every sigil for one mailbox" $ do
      -- A sigil binds exactly one encryption key, so a mailbox read by two
      -- maintainers has two.
      let mf = addMailboxSigil aKey aHash (setOK aKey Nothing base)
      declaredSigils mf `shouldBe` [(aKey, aHash)]

    it "does not publish the same sigil twice" $ do
      let mf = addMailboxSigil aKey aHash (addMailboxSigil aKey aHash (setOK aKey Nothing base))
      declaredSigils mf `shouldBe` [(aKey, aHash)]

    it "takes a sigil out and leaves the mailbox" $ do
      let mf = dropMailboxSigil aKey aHash (addMailboxSigil aKey aHash (setOK aKey Nothing base))
      declaredSigils mf `shouldBe` []
      declaredMailboxes mf `shouldBe` [(aKey, "hub", Nothing)]

    it "takes a mailbox out WITH its sigils" $ do
      -- A sigil left behind describes an inbox the repository no longer
      -- declares: a published encryption key with no stated purpose.
      let mf = dropMailbox aKey (addMailboxSigil aKey aHash (setOK aKey Nothing base))
      declaredSigils mf `shouldBe` []
      declaredMailboxes mf `shouldBe` []
      renderManifest mf `shouldBe` renderManifest base

    it "leaves another mailbox's sigil alone when dropping one" $ do
      let mf0 = addMailboxSigil bKey aHash (setOK bKey Nothing (setOK aKey Nothing base))
          mf  = dropMailbox aKey mf0
      declaredMailboxes mf `shouldBe` [(bKey, "hub", Nothing)]
      declaredSigils mf `shouldBe` [(bKey, aHash)]

  describe "PEP-18: the round trip the two packages meet on" $ do

    it "says yes for a manifest that will read back" $ do
      writesBackAsMailbox aKey Nothing (setOK aKey Nothing base) `shouldBe` True
      writesBackAsMailbox aKey (Just "public") (setOK aKey (Just "public") base)
        `shouldBe` True

    it "says yes for a sigil that will read back" $ do
      writesBackAsSigil aKey aHash (addMailboxSigil aKey aHash (setOK aKey Nothing base))
        `shouldBe` True

    it "says no when the clause is not there at all" $ do
      writesBackAsMailbox aKey Nothing base `shouldBe` False
      writesBackAsSigil aKey aHash base `shouldBe` False

    it "says no when the tier is not the one asked for" $ do
      -- The case a lost field produces, and the reason the guard compares the
      -- whole triple rather than just looking for the key.
      writesBackAsMailbox aKey (Just "public") (setOK aKey Nothing base) `shouldBe` False
      writesBackAsMailbox aKey Nothing (setOK aKey (Just "public") base) `shouldBe` False

    it "says no when the key is a different one" $ do
      writesBackAsMailbox bKey Nothing (setOK aKey Nothing base) `shouldBe` False

    it "survives a quoted tier, which is where an escaping fault would live" $ do
      writesBackAsMailbox aKey (Just "2nd tier") (setOK aKey (Just "2nd tier") base)
        `shouldBe` True
