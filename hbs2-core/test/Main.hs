module Main where

import TestFakeMessaging
import TestActors
import TestFileLogger
import TestScheduled
import TestDerivedKey
import TestMerkleWalk
import TestStorageInventory
import TestKeyEncoding
import TestByPassFlow
import TestEncryptedTree
import TestFrameBound
import TestBlockSizeBound
import TestTCPRedial
import TestPutBlockAsked

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
      , testGroup "canonical storage inventory"
          [ testCase "is a deterministic set of distinct blocks"
              testInventoryDeterministic
          , testCase "separates missing and corrupt frontier blocks"
              testInventoryReportsBadFrontier
          , testCase "stops explicitly at configured work bounds"
              testInventoryBoundsWork
          ]
      , testGroup "a key on the wire"
          [ testCase "encodes exactly as it always did"
              testKeyEncodingUnchanged
          , testCase "is refused when it is not the length its scheme wants"
              testKeyEncodingRefusesBadLength
          ]
      , testGroup "a stream frame is as long as the far end says"
          [ testCase "refuses a length no frame this build produces could have"
              testFrameRefusesHugeDeclaredLength
          , testCase "reads a frame the size this build actually sends"
              testFrameTakesWhatThisBuildSends
          , testCase "ends with the stream when a promised frame stops arriving"
              testFrameDoesNotAllocateWhatWasPromised
          ]
      , testGroup "a block is as big as the far end says"
          [ testCase "the ceiling admits anything this build writes"
              testBlockSizeCeilingIsAboveAnythingWritten
          , testCase "and keeps the work a declared size buys finite"
              testBlockSizeCeilingBoundsTheWork
          ]
      , testGroup "a block is stored only if it is the one that was asked for"
          [ testCase "keeps a block it held when a peer sends it instead"
              testPutAskedKeepsWhatWasAlreadyHeld
          , testCase "writes nothing it was not asked for"
              testPutAskedWritesNothingItWasNotAskedFor
          , testCase "stores the block that was asked for"
              testPutAskedStoresWhatWasAskedFor
          ]
      , testGroup "an encrypted flow belongs to the peer, not to the flow key"
          [ testCase "is not taken over by a stranger who names the nonce first"
              testByPassFlowNotStolen
          , testCase "stays readable when two peers share a flow key"
              testByPassFlowSharedIsReadable
          ]
      , testGroup "an encrypted tree separates two payloads under one group key"
          [ testCase "gives back what it was given"
              testEncryptedTreeRoundTrip
          , testCase "does not reuse a nonce when two payloads share a megabyte"
              testEncryptedTreeSharedPrefixNoNonceReuse
          , testCase "still charges an append only for the blocks that changed"
              testEncryptedTreeAppendStillDedups
          , testCase "reads a tree written before the nonce moved into the block"
              testEncryptedTreeReadsLegacyMethod1
          ]
      , testGroup "a TCP peer that was down when we dialled it"
          [ testCase "is dialled again once it is back"
              testTCPRedialsAfterFailedDial
          ]
      ]

