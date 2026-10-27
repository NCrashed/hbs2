-- | The code view's own decisions (PEP-25): which refs and paths a page may
-- name, and how git's NUL-separated answers become documents.
module HBS2.Hub.CodeSpec (spec) where

import HBS2.Hub.CLI.Code

import Data.Aeson (Value(..),object,(.=))
import Data.ByteString.Char8 qualified as B8
import Data.Text.Encoding qualified as Text
import Test.Hspec

spec :: Spec
spec = describe "PEP-25 hub code" $ do

  it "names a branch, a tag, a sha or HEAD, and no expression" $ do
    mapM_ (\r -> validCodeRef r `shouldBe` True) ["HEAD", "master", "release/0.26", "v0.26.0.2", "3795fb0", "3795fb0aa"]
    mapM_ (\r -> validCodeRef r `shouldBe` False) ["HEAD~1", "main^", "A..B", ":/fix", "-oops", "@{1}", ""]

  it "names a path inside the tree and nothing outside it" $ do
    mapM_ (\p -> validCodePath p `shouldBe` True) ["", "src", "src/lib/a b.ts", "\1044\1086\1082.md"]
    mapM_ (\p -> validCodePath p `shouldBe` False) ["..", "src/../..", "./x", "a//b", "a\0b"]

  it "finds the README GitHub would render, markdown first" $ do
    readmeOf ["LICENSE", "README.txt", "README.md"] `shouldBe` Just "README.md"
    readmeOf ["readme"] `shouldBe` Just "readme"
    readmeOf ["READMEFIRST.md", "src"] `shouldBe` Nothing

  it "reads ls-tree -z -l, directories first, names as given" $ do
    let out = B8.concat
          [ "100644 blob aaaa       12\tz.txt\0"
          , "040000 tree bbbb        -\tsrc\0"
          , "100644 blob cccc        3\t", Text.encodeUtf8 "a b \1044.md", "\0" ]
    treeEntries out `shouldBe`
      [ object ["name" .= ("src" :: String), "type" .= ("tree" :: String), "mode" .= ("040000" :: String), "oid" .= ("bbbb" :: String), "size" .= (Nothing :: Maybe Integer)]
      , object ["name" .= ("a b \1044.md" :: String), "type" .= ("blob" :: String), "mode" .= ("100644" :: String), "oid" .= ("cccc" :: String), "size" .= Just (3 :: Integer)]
      , object ["name" .= ("z.txt" :: String), "type" .= ("blob" :: String), "mode" .= ("100644" :: String), "oid" .= ("aaaa" :: String), "size" .= Just (12 :: Integer)] ]

  it "reads log -z, and leaves out a record that is short of fields" $ do
    let rec = B8.intercalate "\x1f" ["abc", "p1 p2", "Ann", "a@x", "10", "20", "Merge it"]
        cs = commitsOf (rec <> "\0" <> "short\x1fone\0")
    length cs `shouldBe` 1
    case cs of
      [Object _] -> pure ()
      other -> expectationFailure (show other)
