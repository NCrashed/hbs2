-- | A repository's name and description in its manifest (PEP-25): what is
-- written, what is refused, and that what is written reads back as text.
module HBS2.Git3.Repo.AboutSpec (spec) where

import HBS2.Git3.Repo.About
import HBS2.Git3.Repo.Mailbox (renderManifest)

import Data.Config.Suckless.Script
import Data.Either (isLeft)
import Data.Text qualified as Text
import Test.Hspec

base :: [Syntax C]
base = either (error . show) id (parseTop "(hbs2-git 3)\n(seed 1)\n(public)\n")

spec :: Spec
spec = describe "PEP-25 a repository's name and description" $ do

  it "adds both, and reads them back from the rendered bytes" $ do
    let desc = "hbs2-hub's pages, for GitHub and without it: \1080\1089\1089\1091\1077 (issues)."
        Right mf = setAbout "name" (Just "hbs2-hub-browser") base >>= setAbout "description" (Just desc)
    aboutOf mf `shouldBe` About (Just "hbs2-hub-browser") (Just desc)
    writesBackAsAbout (aboutOf mf) mf `shouldBe` True
    "(name \"hbs2-hub-browser\")" `Text.isInfixOf` Text.pack (renderManifest mf) `shouldBe` True

  it "replaces a field where it is, and keeps the rest of the manifest in order" $ do
    let Right one = setAbout "name" (Just "a") base
        Right two = setAbout "name" (Just "b") one
    aboutOf two `shouldBe` About (Just "b") Nothing
    length two `shouldBe` length one
    renderManifest (take (length base) two) `shouldBe` renderManifest base

  it "takes a field out" $ do
    let Right one = setAbout "name" (Just "a") base
    fmap renderManifest (setAbout "name" Nothing one) `shouldBe` Right (renderManifest base)

  it "refuses what it cannot write or should not show" $ do
    isLeft (setAbout "name" (Just "   ") base) `shouldBe` True
    isLeft (setAbout "name" (Just "two\nlines") base) `shouldBe` True
    isLeft (setAbout "description" (Just "two\nlines") base) `shouldBe` True
    isLeft (setAbout "name" (Just (Text.replicate (maxNameChars + 1) "x")) base) `shouldBe` True
    isLeft (setAbout "description" (Just (Text.replicate (maxDescriptionChars + 1) "x")) base) `shouldBe` True
    isLeft (setAbout "description" (Just "a bell \a here") base) `shouldBe` True

  it "writes a quote and a backslash, and keeps them through a second edit" $ do
    -- Finding 20: the renderer wrote a string between quotes as it is, so
    -- a quote ended it early. Escaped on the way out now, and the clauses
    -- in memory stay as the parser gives them, so an edit of a manifest
    -- read back escapes once and not twice.
    let name = "a \"quoted\" C:\\path"
        Right mf = setAbout "name" (Just name) base
        Right back = parseTop (renderManifest mf)
        Right again = setAbout "description" (Just "and a \\ second") back
        Right twice = parseTop (renderManifest again)
    aboutOf back `shouldBe` About (Just name) Nothing
    aboutOf twice `shouldBe` About (Just name) (Just "and a \\ second")
    writesBackAsAbout (aboutOf again) again `shouldBe` True

  it "escapes every string in the manifest, whoever put it there" $ do
    let odd' = "tab\there, bell \a, nul \0 then 5, del \DEL"
        clause = mkForm @C "note" [mkStr (odd' :: Text.Text)]
        Right back = parseTop (renderManifest (base <> [clause]))
    [ s | ListVal [SymbolVal "note", LitStrVal s] <- back ] `shouldBe` [odd']
    -- and nothing raw that a terminal would act on
    any (< ' ') (filter (/= '\n') (renderManifest (base <> [clause]))) `shouldBe` False
