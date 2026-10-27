-- | What `hub serve` decides before anything runs (PEP-25): which files it
-- serves, which requests reach the API, and what it listens on.
module HBS2.Hub.ServeSpec (spec) where

import HBS2.Hub.Serve
import HBS2.Hub.CLI.Serve (splitListen)

import Test.Hspec

spec :: Spec
spec = describe "PEP-25 hub serve" $ do

  it "serves a plain name under the pages and nothing above them" $ do
    assetPath [] `shouldBe` Just "index.html"
    assetPath ["pages", "thread.html"] `shouldBe` Just "pages/thread.html"
    assetPath ["..", "etc", "passwd"] `shouldBe` Nothing
    assetPath ["pages", ".."] `shouldBe` Nothing
    assetPath [".git", "config"] `shouldBe` Nothing
    assetPath ["a\\b"] `shouldBe` Nothing
    assetPath ["a/b"] `shouldBe` Nothing
    assetPath ["pages", ""] `shouldBe` Nothing

  it "serves only the file types the pages are made of" $ do
    contentTypeFor "pages/repo.js" `shouldBe` Just "text/javascript; charset=utf-8"
    contentTypeFor "x.sh" `shouldBe` Nothing
    contentTypeFor "README" `shouldBe` Nothing

  it "answers to its loopback address only, against DNS rebinding" $ do
    hostAllowed 8484 (Just "127.0.0.1:8484") `shouldBe` True
    hostAllowed 8484 (Just "localhost:8484") `shouldBe` True
    hostAllowed 8484 (Just "evil.example:8484") `shouldBe` False
    hostAllowed 8484 (Just "127.0.0.1:9999") `shouldBe` False
    hostAllowed 8484 Nothing `shouldBe` False

  it "takes a POST from its own origin or from no browser" $ do
    originAllowed 8484 Nothing `shouldBe` True
    originAllowed 8484 (Just "http://127.0.0.1:8484") `shouldBe` True
    originAllowed 8484 (Just "http://evil.example") `shouldBe` False
    originAllowed 8484 (Just "null") `shouldBe` False

  it "compares the token whole" $ do
    sameToken "abc123" "abc123" `shouldBe` True
    sameToken "abc123" "abc124" `shouldBe` False
    sameToken "abc123" "abc12" `shouldBe` False
    sameToken "abc123" "" `shouldBe` False

  it "reads a listen address with the port last" $ do
    splitListen "127.0.0.1:8484" `shouldBe` Just ("127.0.0.1", 8484)
    splitListen "::1:8484" `shouldBe` Just ("::1", 8484)
    splitListen "0.0.0.0:99999" `shouldBe` Nothing
    splitListen "8484" `shouldBe` Nothing
