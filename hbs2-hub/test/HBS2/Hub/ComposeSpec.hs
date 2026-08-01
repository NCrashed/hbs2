module HBS2.Hub.ComposeSpec (spec) where

import HBS2.Hub.Types ()
import HBS2.Hub.CLI.Compose (issueArgs,issueUsage)

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject)
import HBS2.Net.Auth.Credentials
import HBS2.Prelude.Plated (Doc,pretty)

import Data.Config.Suckless

import Data.ByteString (ByteString)
import Data.List (isInfixOf)
import Test.Hspec

shown :: Doc () -> String
shown = show

-- A key or a hash as the word somebody would type at a shell.
word :: Show a => a -> Syntax C
word = mkStr @C . show

keyWord :: HubKeyOf -> Syntax C
keyWord k = mkStr @C (show (pretty (AsBase58 k)))

type HubKeyOf = PubKey 'Sign 'HBS2Basic

href :: ByteString -> HashRef
href = HashRef . hashObject

aKey :: IO HubKeyOf
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

spec :: Spec
spec = do

  describe "PEP-22 hub issue new: which value is which" $ do

    it "reads the five positional arguments it always read" $ do
      repo <- aKey
      author <- aKey
      let s = href "sender"
          r = href "rcpt"
          argv = [ keyWord repo, word (pretty s), word (pretty r), keyWord author
                 , mkStr @C "a title" ]
      issueArgs argv `shouldBe` Just (repo, s, r, author, "a title")

    it "reads them by name, in any order" $ do
      -- The reason the named form exists: FOUR base58 blobs in a row, and two
      -- pairs of them are interchangeable at the pattern level -- repo-key and
      -- author-key are both SignPubKeyLike, the two sigils are both HashLike. A
      -- swap produced a valid, signed, DELIVERED letter authored by the
      -- repository key, with no error and a zero exit, and an authorship claim
      -- inside a signed box is permanent. A name cannot be swapped silently.
      repo <- aKey
      author <- aKey
      let s = href "sender"
          r = href "rcpt"
          named = [ mkStr @C "--target", keyWord repo
                  , mkStr @C "--sender", word (pretty s)
                  , mkStr @C "--recipient", word (pretty r)
                  , mkStr @C "--author", keyWord author
                  , mkStr @C "--title", mkStr @C "a title" ]
          shuffled = [ mkStr @C "--title", mkStr @C "a title"
                     , mkStr @C "--author", keyWord author
                     , mkStr @C "--recipient", word (pretty r)
                     , mkStr @C "--sender", word (pretty s)
                     , mkStr @C "--target", keyWord repo ]
      issueArgs named `shouldBe` Just (repo, s, r, author, "a title")
      -- The property, rather than two examples of it: order carries no meaning
      -- in the named form, which is the whole point of having it.
      issueArgs shuffled `shouldBe` issueArgs named

    it "refuses a flag it does not know instead of defaulting past it" $ do
      -- Without this, `--titel X` is dropped on the floor and the letter goes out
      -- under whatever the other flags said -- which, for a letter, means signed
      -- and gone.
      repo <- aKey
      author <- aKey
      let with k = [ mkStr @C "--target", keyWord repo
                   , mkStr @C "--sender", word (pretty (href "s"))
                   , mkStr @C "--recipient", word (pretty (href "r"))
                   , mkStr @C "--author", keyWord author
                   , mkStr @C "--title", mkStr @C "t"
                   , mkStr @C k, mkStr @C "x" ]
      issueArgs (with "--titel") `shouldBe` Nothing
      issueArgs (with "--label") `shouldBe` Nothing

    it "refuses a flag given twice instead of choosing" $ do
      repo <- aKey
      author <- aKey
      issueArgs [ mkStr @C "--target", keyWord repo
                , mkStr @C "--sender", word (pretty (href "s"))
                , mkStr @C "--recipient", word (pretty (href "r"))
                , mkStr @C "--author", keyWord author
                , mkStr @C "--title", mkStr @C "one"
                , mkStr @C "--title", mkStr @C "two" ] `shouldBe` Nothing

    it "refuses a flag with nothing after it" $ do
      repo <- aKey
      issueArgs [ mkStr @C "--target", keyWord repo, mkStr @C "--title" ]
        `shouldBe` Nothing

    it "refuses a partial named form rather than filling a blank" $ do
      repo <- aKey
      issueArgs [ mkStr @C "--target", keyWord repo ] `shouldBe` Nothing
      issueArgs [] `shouldBe` Nothing

    it "refuses a value that is not the kind the name asks for" $ do
      -- A sigil is a hash and a target is a key; neither is "whatever parses".
      author <- aKey
      issueArgs [ mkStr @C "--target", mkStr @C "not-a-key"
                , mkStr @C "--sender", word (pretty (href "s"))
                , mkStr @C "--recipient", word (pretty (href "r"))
                , mkStr @C "--author", keyWord author
                , mkStr @C "--title", mkStr @C "t" ] `shouldBe` Nothing

    it "lets an issue be called 2026" $ do
      -- A title is text, and the argv reader keeps a word that spells a number
      -- AS a number on purpose, because half the inherited dictionary matches on
      -- integers. Every title pattern here is StringLike, which matches a symbol
      -- or a string and not a number, so `--title 2026` died with a usage
      -- message that said nothing about why. Rendering the atom back is lossless
      -- by construction: the reader kept it as a number only because rendering
      -- it gives back the characters that were typed.
      repo <- aKey
      author <- aKey
      let titled t = [ mkStr @C "--target", keyWord repo
                     , mkStr @C "--sender", word (pretty (href "s"))
                     , mkStr @C "--recipient", word (pretty (href "r"))
                     , mkStr @C "--author", keyWord author
                     , mkStr @C "--title", t ]
          titleOf = fmap (\(_,_,_,_,t) -> t) . issueArgs
      titleOf (titled (mkInt @C (2026 :: Int))) `shouldBe` Just "2026"
      titleOf (titled (mkStr @C "2026"))        `shouldBe` Just "2026"
      -- ...and a list is still not a title
      titleOf (titled (mkList @C [mkStr @C "a"])) `shouldBe` Nothing

    it "says what it takes, and why the names are worth typing" $ do
      shown issueUsage `shouldSatisfy` isInfixOf "usage: hub issue new --target"
      -- The consequence, not a fragment of the sentence carrying it: the whole
      -- reason to prefer the flags is that a positional swap is signed and gone.
      shown issueUsage `shouldSatisfy` isInfixOf "claiming the wrong author"
