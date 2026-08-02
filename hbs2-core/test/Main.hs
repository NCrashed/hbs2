module Main where

import TestFakeMessaging
import TestActors
import TestFileLogger
import TestScheduled
import TestDerivedKey
import TestMerkleWalk
import TestKeyEncoding

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "root"
      [
        testCase "testFakeMessaging1" testFakeMessaging1
      , testCase "testActorsBasic" testActorsBasic
      , testCase "testFileLogger" testFileLogger
      , testCase "testScheduledActions" testScheduled
      , testCase "testDerivedKeys1" testDerivedKeys1
      , testGroup "merkle walk over a graph somebody else chose"
          [ testCase "enters each node once when asked about a set"
              testMerkleWalkBounded
          , testCase "still repeats a leaf that a tree names twice"
              testMerkleWalkKeepsDuplicates
          ]
      , testGroup "a key on the wire"
          [ testCase "encodes exactly as it always did"
              testKeyEncodingUnchanged
          , testCase "is refused when it is not the length its scheme wants"
              testKeyEncodingRefusesBadLength
          ]
      ]


