module HBS2.Hub.InboxSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Letter (Disposition(..))
import HBS2.Hub.Ingress
import HBS2.Hub.CLI.Inbox

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject)
import HBS2.Net.Auth.Credentials
import HBS2.Prelude.Plated (Doc,fromString,pretty)

import HBS2.Clock (Timeout(..),TimeoutKind(..),pause)

import Codec.Serialise (serialise)
import Codec.Serialise qualified as CBOR
import Control.Exception (bracket,try)
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import GHC.IO.Handle (hDuplicate,hDuplicateTo)
import System.Exit (ExitCode(..))
import System.IO
import System.Posix.IO qualified as Posix
import Test.Hspec

shown :: Doc () -> String
shown = show

mh :: BS.ByteString -> HashRef
mh = HashRef . hashObject

-- A hash-shaped field that is not a hash. A HashRef is a newtype over a
-- ByteString with a generic Serialise instance, so it takes any length off the
-- wire; nothing about the type says 32 bytes.
fatRef :: Int -> HashRef
fatRef n = HashRef (fromString (replicate n 'z'))

-- The same for a key, built the way FoldSpec builds one: through the generic
-- encoding, because Saltine's own decode is what refuses these.
fatKey :: Int -> HubKey
fatKey n = fromMaybe (error "cannot build a stunted key")
             (either (const Nothing) Just
                (CBOR.deserialiseOrFail (serialise ((0 :: Word), BS.replicate n 0x41))))

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

-- One queue line, with everything a stranger controls left to the caller.
view :: HashRef -> Maybe HubKey -> Either OpenError (HubKey, AuthorContent, Disposition) -> LetterView
view m env letter = LetterView m env letter Nothing

spec :: Spec
spec = do

  describe "PEP-22 hub inbox: the queue line" $ do

    it "prints a thread-id that is not a hash by its size" $ do
      -- THE ONE THAT MATTERS HERE. The thread-id comes out of a stranger's
      -- signed author box, 'malformedRef' is applied on the ACCEPT path only,
      -- and base58 is Integer base conversion, so it is quadratic: 48 KiB is
      -- 0.7 s of CPU and 67 000 characters, per line, in a queue anybody can
      -- write to. The guard existed, was documented and was tested -- in
      -- HBS2.Hub.Fold, where this renderer could not reach it.
      repo <- aKey
      author <- aKey
      let thread = fatRef 40000
          ac = AComment thread Nothing (Just "hi") Nothing 5
          line = shown (render (view (mh "m") (Just repo) (Right (author, ac, RequestOnly))))
      line `shouldSatisfy` isInfixOf "not a hash"
      -- and the cost was not paid and then thrown away
      length line `shouldSatisfy` (< 400)

    it "prints an envelope key that is not a key by its size" $ do
      -- The envelope key is recovered by unboxSignedBox0, which does not run the
      -- length check 'unboxChecked' adds for the inner box: libsodium reads its
      -- 32 bytes without asking how many there are, so a key whose first 32 bytes
      -- are real and whose serialised form carries another fifty kilobytes
      -- VERIFIES -- and this line prints on every failure path, NotForUs
      -- included, which is most of a public mailbox.
      let line = shown (render (view (mh "m") (Just (fatKey 40000)) (Left NotForUs)))
      line `shouldSatisfy` isInfixOf "not a key"
      length line `shouldSatisfy` (< 400)

    it "prints a real hash and a real key as themselves" $ do
      -- The other half: the guard must not turn the ordinary case into a size.
      -- Pinned to the EXACT rendering rather than to "does not say not-a-key",
      -- because the interesting regression is not the guard misfiring, it is
      -- keyDoc losing the base58: a bare `pretty` on a HubKey prints the saltine
      -- internals (Sign.PublicKey {hashesTo = ...}), which is the defect
      -- 'MailboxUnknown's hand-written Show exists to avoid, and it says
      -- nothing about "not a key" on the way past.
      k <- aKey
      let line = shown (render (view (mh "m") (Just k) (Left NotFetched)))
      take 2 (words line) `shouldBe` [show (pretty (mh "m")), show (pretty (AsBase58 k))]
      line `shouldSatisfy` isInfixOf "not fetched yet"

    it "names nobody when the envelope named nobody" $ do
      -- A forged envelope has no signer to name: the signature is what would
      -- have established one. Checked BY POSITION rather than by "contains a
      -- dash", which a line about anything at all would satisfy.
      let line = shown (render (view (mh "m") Nothing (Left BadEnvelopeSig)))
      take 2 (words line) `shouldBe` [show (pretty (mh "m")), "-"]

  describe "PEP-22 hub inbox: what it says about the list" $ do

    it "does not call an empty answer an empty mailbox" $ do
      -- The peer writes the mailbox ref only when a merge lands, so "no ref" and
      -- "not downloaded yet" are one observation. Reporting nothing, quietly and
      -- with a zero exit, was the answer that made a first run against a live
      -- mailbox look like a mailbox with nothing in it.
      let notes = fmap shown (inboxNotes (InboxRead [] [] False))
      notes `shouldSatisfy` any (isInfixOf "NOT the same as an empty mailbox")

    it "says a settled empty mailbox is empty, by saying nothing" $ do
      fmap shown (inboxNotes (InboxRead [] [] True)) `shouldBe` []

    it "tells a still-arriving queue from an incomplete one" $ do
      -- Different notes because they call for different things, and only one of
      -- them makes the list WRONG. BOTH HALVES of each, positive and negative:
      -- asserting only that each says its own sentence is satisfied by a version
      -- that says both every time, which is the confusion the name promises to
      -- prevent.
      k <- aKey
      let one = [view (mh "m") (Just k) (Left NotForUs)]
          arriving = fmap shown (inboxNotes (InboxRead one [] False))
          holed    = fmap shown (inboxNotes (InboxRead one [mh "x"] True))
      arriving `shouldSatisfy` any (isInfixOf "more letters may follow")
      arriving `shouldSatisfy` not . any (isInfixOf "incomplete in both directions")
      holed    `shouldSatisfy` any (isInfixOf "incomplete in both directions")
      holed    `shouldSatisfy` not . any (isInfixOf "more letters may follow")

    it "bounds the list of unreadable blocks it prints" $ do
      -- irMissing grows with the mailbox tree, which a stranger can grow. `hub
      -- verify` got its cap after a measured 369 MB of stdout; this printed one
      -- unbounded line of 45-character hashes.
      let many' = [ mh (fromString (show i)) | i <- [1 :: Int .. maxMissingLines * 3] ]
          note = concatMap shown (inboxNotes (InboxRead [] many' True))
      -- The COUNT of what was left out, not just the word "more", which the
      -- still-arriving note also contains and which a cap of any size satisfies.
      note `shouldSatisfy` isInfixOf ("and " <> show (maxMissingLines * 2) <> " more")
      length note `shouldSatisfy` (< maxMissingLines * 80)

    it "prints a missing block hash that is not a hash by its size, too" $ do
      -- These come out of a mailbox entry, which is a stranger's bytes like
      -- everything else in the tree.
      let note = concatMap shown (inboxNotes (InboxRead [] [fatRef 40000] True))
      note `shouldSatisfy` isInfixOf "not a hash"

    it "warns that a queue line is not permission" $ do
      -- PEP-22: "anything built on it must not treat 'it was in the queue' as
      -- 'it may be folded'". This form has no deny-list to apply, and every
      -- admissible letter is marked "(folds)", which read alone is permission.
      k <- aKey
      let ac = AOpen k HubIssue "t" [] Nothing Nothing Nothing 1
          folds = [view (mh "m") (Just k) (Right (k, ac, FoldsToCanon))]
          other = [view (mh "m") (Just k) (Right (k, ac, RequestOnly))]
      fmap shown (inboxNotes (InboxRead folds [] True))
        `shouldSatisfy` any (isInfixOf "no deny-list was applied")
      -- ...and not when there is nothing for it to be about
      fmap shown (inboxNotes (InboxRead other [] True)) `shouldBe` []

  describe "PEP-22 hub inbox: the exit code" $ do

    it "leaves non-zero only when the list is wrong" $ do
      -- A hole in the tree makes the list wrong in BOTH directions: a missing
      -- chunk of Exists entries makes letters vanish, one of Deleted entries puts
      -- folded letters back in the queue.
      inboxCode (InboxRead [] [mh "x"] True) `shouldBe` 2
      -- Still arriving is a SHORTER answer, not a wrong one. A non-zero exit here
      -- would fire on ordinary use and teach a caller to ignore the code.
      inboxCode (InboxRead [] [] False) `shouldBe` 0
      inboxCode (InboxRead [] [] True) `shouldBe` 0

    it "keeps its own codes out of the range hub verify owns" $ do
      -- The numbers are a contract a hook branches on: PEP-22 says they may be
      -- added to and not reassigned, and 3..16 are already spoken for.
      codeMailboxUnknown `shouldSatisfy` (> 16)
      codePeerSilent `shouldSatisfy` (> 16)
      codeMailboxUnknown `shouldNotBe` codePeerSilent

    it "says what it takes, in the words somebody would type" $ do
      -- Not a BadFormException. That named an internal Haskell type and a
      -- spelling the caller did not type, and its show rendered the whole form --
      -- so a wrong-arity call printed the caller's argv raw, escape sequences
      -- included.
      shown inboxUsage `shouldSatisfy` isInfixOf "usage: hub inbox"
      shown inboxUsage `shouldSatisfy` isInfixOf "hbs2-peer mailbox create"

    it "says a wedged peer is wedged, and which call it was" $ do
      show (PeerSilent "a block of the mailbox")
        `shouldSatisfy` isInfixOf "a block of the mailbox"
      show (PeerSilent "x") `shouldSatisfy` isInfixOf "did not answer"

    it "actually leaves with the code, not just defines it" $ do
      -- Every check above is of a CONSTANT. Nothing said the process reaches it,
      -- which is the half that matters to a hook and the half `hub verify` has
      -- had tested since it was written. stderr goes to /dev/null so the suite
      -- stays readable.
      onStderr (try (refuse "no such mailbox" codeMailboxUnknown))
        `shouldReturn` (Left (ExitFailure 17) :: Either ExitCode ())
      onStderr (try (refuse "the peer stopped answering" codePeerSilent))
        `shouldReturn` (Left (ExitFailure 18) :: Either ExitCode ())

    it "keeps the code when there is nowhere left to say why" $ do
      -- THE ONE THAT MATTERS HERE. `hub inbox K 2>&1 | head` closes stderr, and
      -- an unguarded hPutDoc then leaves through the RTS with 1 -- which PEP-22
      -- gives to usage errors, so the hook sees the one code 17 and 18 were
      -- added to be told apart from. Reproduced by writing into a pipe whose
      -- read end is shut, which is what head leaves behind.
      (r, w) <- Posix.createPipe
      Posix.closeFd r
      wh <- Posix.fdToHandle w
      got <- bracket (hDuplicate stderr)
               (\o -> hDuplicateTo o stderr >> hClose o)
               (\_ -> do hDuplicateTo wh stderr
                         -- UNBUFFERED, so the write reaches the dead pipe inside
                         -- the guard's scope rather than at the flush on exit.
                         hSetBuffering stderr NoBuffering
                         try (refuse "no such mailbox" codeMailboxUnknown))
      hClose wh
      got `shouldBe` (Left (ExitFailure 17) :: Either ExitCode ())

  describe "PEP-22 hub inbox: the bound on one call to the peer" $ do

    it "gives the answer back when the peer answers" $ do
      -- The bound must not become the answer.
      bounded (0.5 :: Timeout 'Seconds) "a block" (pure (Just "x"))
        `shouldReturn` Just ("x" :: String)

    it "names the call that did not answer, rather than hanging on it" $ do
      -- The fix this module is mostly about: the storage client's getBlock calls
      -- callService raw, which blocks on a TQueue with NO timeout, and every
      -- merkle node and every message body goes through it. A peer that answered
      -- the mailbox service and then stalled on storage hung the verb forever,
      -- after all three timed calls had succeeded.
      let stalls = pause (10 :: Timeout 'Seconds) >> pure ()
      bounded (0.2 :: Timeout 'Seconds) "a block of the mailbox" stalls
        `shouldThrow` \(PeerSilent what) -> what == "a block of the mailbox"

-- stderr sent somewhere nobody is looking, so that a test about an exit code
-- does not print the message it is about.
onStderr :: IO a -> IO a
onStderr act = bracket (hDuplicate stderr)
                 (\o -> hDuplicateTo o stderr >> hClose o)
                 (\_ -> withFile "/dev/null" WriteMode $ \n ->
                          hDuplicateTo n stderr >> act)
