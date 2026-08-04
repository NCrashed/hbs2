-- | A sender naming its own attachment in the payload it signs.
--
-- The one thing 'createMessage' alone cannot do, and the reason it is now two
-- functions. A part is named by the hash of its encrypted tree, so the tree
-- has to exist before the payload that names it is written, and the payload is
-- signed. A caller that could only hand 'createMessage' a producer learned the
-- name afterwards, with the signature already over bytes that did not mention
-- it. PEP-18's letters (@body-part@, @bundle-part@) do exactly this.
--
-- The assertions are the two halves of that: the message really carries the
-- tree the sender hashed, and the payload really comes back naming it.
module MessageParts (messagePartsTests) where

import HBS2.Prelude.Plated
import HBS2.Peer.Proto.Mailbox.Message
import HBS2.Peer.Proto.Mailbox.Types

import MailboxFixture

import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Set qualified as Set

import Test.Tasty
import Test.Tasty.HUnit

messagePartsTests :: TestTree
messagePartsTests = testGroup "message parts"

  [ testCase "a sender can name its own attachment in the payload it signs" $
      withStore $ \sto -> do
        (alice, aliceS) <- aPeer
        (bob, bobS)     <- aPeer
        let cms = services sto [alice, bob]

        -- ONE: the attachment exists and has a name.
        (parts, _) <- createAttachments cms (Right aliceS) [Right bobS]
                        [([("file-name","pr.bundle")], pure (LBS8.pack "the bundle bytes"))]
        part <- case parts of
                  [p] -> pure p
                  _   -> assertFailure "expected one part" >> fail "unreachable"

        -- TWO: the payload names it, and the signature goes over that.
        let payload = B8.pack ("bundle-part " <> show (pretty part))

        flags <- defMessageFlags
        msg <- createMessageWith cms flags Nothing (Right aliceS) [Right bobS]
                 [part] payload

        -- The message carries the tree the sender hashed, not one built for it
        -- afterwards. This is the half that used to be impossible.
        content <- contentOf msg
        Set.toList (messageParts content) @?= [part]

        -- And the payload comes back saying so, which is what was signed.
        (_, _, back) <- readMessage (reader bob) msg
        back @?= payload

  , testCase "createMessage is still the two of them in a row" $
      withStore $ \sto -> do
        (alice, aliceS) <- aPeer
        (bob, bobS)     <- aPeer
        let cms = services sto [alice, bob]
        flags <- defMessageFlags
        msg <- createMessage cms flags Nothing (Right aliceS) [Right bobS]
                 [([("file-name","a.txt")], pure (LBS8.pack "contents"))]
                 (B8.pack "the payload")

        content <- contentOf msg
        length (Set.toList (messageParts content)) @?= 1

        (_, _, back) <- readMessage (reader bob) msg
        back @?= B8.pack "the payload"

  -- A message with nothing attached still builds, and carries no parts. The
  -- split made this the case where 'createAttachments' is called with an empty
  -- list, which is every caller in this repository today.
  , testCase "a message with no attachments carries none" $
      withStore $ \sto -> do
        (alice, aliceS) <- aPeer
        (bob, bobS)     <- aPeer
        let cms = services sto [alice, bob]
        flags <- defMessageFlags
        msg <- createMessage cms flags Nothing (Right aliceS) [Right bobS]
                 mempty (B8.pack "no attachments here")

        content <- contentOf msg
        Set.toList (messageParts content) @?= []
        (_, _, back) <- readMessage (reader bob) msg
        back @?= B8.pack "no attachments here"
  ]
