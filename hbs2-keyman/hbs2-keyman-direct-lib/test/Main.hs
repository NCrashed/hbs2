module Main (main) where

import HBS2.KeyMan.Config

import Data.Config.Suckless (C, Syntax, parseTop)
import Data.Either (fromRight)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "peer keyring discovery"
    [ testCase "resolves the generated relative key against the peer config" do
        peerKeyFileFromConfig "/home/alice/.config/hbs2-peer/config"
            (config "key \"./default.key\"")
            @?= Just "/home/alice/.config/hbs2-peer/default.key"
    , testCase "keeps a custom absolute key path" do
        peerKeyFileFromConfig "/home/alice/.config/hbs2-peer/config"
            (config "key \"/run/keys/peer.key\"")
            @?= Just "/run/keys/peer.key"
    , testCase "uses the effective last key and ignores unrelated paths" do
        peerKeyFileFromConfig "/home/alice/.config/hbs2-peer/config"
            (config "key \"old.key\"\nstorage \"arbitrary-home-tree\"\nkey \"keys/current.key\"")
            @?= Just "/home/alice/.config/hbs2-peer/keys/current.key"
    , testCase "does not invent a broad scan when no key is configured" do
        peerKeyFileFromConfig "/home/alice/.config/hbs2-peer/config"
            (config "storage \"somewhere\"")
            @?= Nothing
    ]

config :: String -> [Syntax C]
config = fromRight [] . parseTop
