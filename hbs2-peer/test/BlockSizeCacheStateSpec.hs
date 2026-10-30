module BlockSizeCacheStateSpec (tests) where

import BlockSizeCacheState

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "block-size cache"
  [ testCase "negative answer expires when a block is announced again" do
      lookupBlockSize 2 (Just (1, Nothing)) @?= BlockSizeUnknown

  , testCase "negative answer remains valid within one announcement revision" do
      lookupBlockSize 1 (Just (1, Nothing)) @?= BlockSizeAbsent

  , testCase "positive size remains useful after another announcement" do
      lookupBlockSize 2 (Just (1, Just 42)) @?= BlockSizeKnown 42

  , testCase "missing cache entry is unknown" do
      lookupBlockSize 1 Nothing @?= BlockSizeUnknown
  ]
