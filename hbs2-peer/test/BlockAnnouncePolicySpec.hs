module BlockAnnouncePolicySpec (tests) where

import BlockAnnouncePolicy
import PeerConfig

import Data.Either (fromRight)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "block announce policy"
  [ testCase "wildcard accepts a contributor that is not a known peer" do
      let conf = PeerConfig (fromRight mempty (parseTop "(accept-block-announce *)"))
      announceAllowed (announcePolicy conf) Nothing @?= True

  , testCase "an explicit allow-list still rejects an unknown peer" do
      announceAllowed (AcceptAnnounceFrom mempty) Nothing @?= False
  ]
