module HBS2.Hub.InboxSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Letter (Disposition(..))
import HBS2.Hub.Ingress
import HBS2.Hub.CLI.Inbox
import HBS2.Hub.CLI.Argv (argvAtom)
import Data.Config.Suckless (Syntax,C)

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
import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.List (isInfixOf)
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

-- There is no fatKey beside it any more. A key used to be buildable the same
-- way, through the generic encoding, and that is exactly what was fixed: the
-- Serialise instances are hand-written now and run saltine's own decoder, and
-- saltine does not export the constructor. A HashRef still has no length rule,
-- which is why the one above is still needed.

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

argv :: [String] -> [Syntax C]
argv = fmap argvAtom

b58 :: HubKey -> String
b58 = show . pretty . AsBase58

-- One queue line, with everything a stranger controls left to the caller.
view :: HashRef -> Maybe HubKey -> Either OpenError (HubKey, AuthorContent, Disposition) -> LetterView
view m env letter = LetterView m env letter Nothing []

spec :: Spec
spec = spec1 >> spec2

spec1 :: Spec
spec1 = do

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

    it "can no longer be given an envelope key that is not a key" $ do
      -- This used to build a 40 KiB "key" and assert the line said "not a key"
      -- rather than paying quadratic base58 for it. The envelope key is
      -- recovered by unboxSignedBox0, which does not run the length check
      -- 'unboxChecked' adds, and this line prints on every failure path,
      -- NotForUs included, which is most of a public mailbox.
      --
      -- The fixture cannot be built now: a key was a newtype over bytes with a
      -- DERIVED Serialise instance, so any length of bytes decoded into one, and
      -- those instances are hand-written and run saltine's own decoder. saltine
      -- does not export the constructor, so there is no other door. 'keyDoc's
      -- guard stays as defence in depth and is no longer reachable from here, so
      -- what is pinned is the thing that closed it.
      forM_ [0, 31, 40000] $ \n ->
        CBOR.deserialiseOrFail @HubKey (serialise ((0 :: Word), BS.replicate n 0x41))
          `shouldSatisfy` isLeft

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
      let notes = fmap shown (inboxNotes False (InboxRead [] [] False 0 mempty))
      notes `shouldSatisfy` any (isInfixOf "NOT the same as an empty mailbox")

    it "says a settled empty mailbox is empty, by saying nothing" $ do
      fmap shown (inboxNotes False (InboxRead [] [] True 0 mempty)) `shouldBe` []

    it "tells a still-arriving queue from an incomplete one" $ do
      -- Different notes because they call for different things, and only one of
      -- them makes the list WRONG. BOTH HALVES of each, positive and negative:
      -- asserting only that each says its own sentence is satisfied by a version
      -- that says both every time, which is the confusion the name promises to
      -- prevent.
      k <- aKey
      let one = [view (mh "m") (Just k) (Left NotForUs)]
          arriving = fmap shown (inboxNotes False (InboxRead one [] False 0 mempty))
          holed    = fmap shown (inboxNotes False (InboxRead one [mh "x"] True 0 mempty))
      arriving `shouldSatisfy` any (isInfixOf "more letters may follow")
      arriving `shouldSatisfy` not . any (isInfixOf "incomplete in both directions")
      holed    `shouldSatisfy` any (isInfixOf "incomplete in both directions")
      holed    `shouldSatisfy` not . any (isInfixOf "more letters may follow")

    it "says how many letters it did not open, and leaves non-zero for it" $ do
      -- A mailbox is public, so how many letters are in it is a stranger's
      -- choice; this reader opens at most maxInboxLetters and used to open all
      -- of them, holding every body resident. Truncating is allowed. Truncating
      -- QUIETLY is not: a list missing letters is wrong, not short.
      k <- aKey
      let one = [view (mh "m") (Just k) (Left NotForUs)]
          cut = fmap shown (inboxNotes False (InboxRead one [] True 42 mempty))
      cut `shouldSatisfy` any (isInfixOf "42 more letter(s)")
      cut `shouldSatisfy` any (isInfixOf "incomplete")
      -- ...and not said when nothing was left out
      fmap shown (inboxNotes False (InboxRead one [] True 0 mempty))
        `shouldSatisfy` not . any (isInfixOf "were not opened")
      -- The remedy is the same as for a hole in the tree -- do not treat this as
      -- the mailbox -- so it is the same code.
      inboxCode (InboxRead one [] True 42 mempty) `shouldBe` 2
      inboxCode (InboxRead one [] True 0 mempty)  `shouldBe` 0

    it "warns when every letter says not-for-us, which a broken keyman also says" $ do
      -- ReadNoGroupKeyAccess is what an unindexed keyman, an unreadable key file
      -- and credentials that do not parse ALL come back as, and it becomes
      -- NotForUs -- once per letter, with a zero exit. That reads as "none of
      -- this is mine" and is indistinguishable from it, so the one thing this
      -- reader can honestly do is name the other possibility.
      k <- aKey
      let mine  = [view (mh "a") (Just k) (Left NotForUs)]
          mixed = mine <> [view (mh "b") (Just k) (Left NotFetched)]
      fmap shown (inboxNotes False (InboxRead mine [] True 0 mempty))
        `shouldSatisfy` any (isInfixOf "hbs2-keyman list")
      -- Only when EVERY letter says it: that is the shape a broken keyman makes,
      -- and a mailbox where some letters are ours does not.
      fmap shown (inboxNotes False (InboxRead mixed [] True 0 mempty))
        `shouldSatisfy` not . any (isInfixOf "hbs2-keyman list")
      -- and never on an empty queue, which says nothing about the keyman at all
      fmap shown (inboxNotes False (InboxRead [] [] True 0 mempty))
        `shouldSatisfy` not . any (isInfixOf "hbs2-keyman list")

    it "bounds the list of unreadable blocks it prints" $ do
      -- irMissing grows with the mailbox tree, which a stranger can grow. `hub
      -- verify` got its cap after a measured 369 MB of stdout; this printed one
      -- unbounded line of 45-character hashes.
      let many' = [ mh (fromString (show i)) | i <- [1 :: Int .. maxMissingLines * 3] ]
          note = concatMap shown (inboxNotes False (InboxRead [] many' True 0 mempty))
      -- The COUNT of what was left out, not just the word "more", which the
      -- still-arriving note also contains and which a cap of any size satisfies.
      note `shouldSatisfy` isInfixOf ("and " <> show (maxMissingLines * 2) <> " more")
      length note `shouldSatisfy` (< maxMissingLines * 80)

    it "prints a missing block hash that is not a hash by its size, too" $ do
      -- These come out of a mailbox entry, which is a stranger's bytes like
      -- everything else in the tree.
      let note = concatMap shown (inboxNotes False (InboxRead [] [fatRef 40000] True 0 mempty))
      note `shouldSatisfy` isInfixOf "not a hash"

    it "warns that a queue line is not permission" $ do
      -- PEP-22: "anything built on it must not treat 'it was in the queue' as
      -- 'it may be folded'". This form has no deny-list to apply, and every
      -- admissible letter is marked "(folds)", which read alone is permission.
      k <- aKey
      let ac = AOpen k HubIssue "t" [] Nothing Nothing Nothing 1
          folds = [view (mh "m") (Just k) (Right (k, ac, FoldsToCanon))]
          other = [view (mh "m") (Just k) (Right (k, ac, RequestOnly))]
      fmap shown (inboxNotes False (InboxRead folds [] True 0 mempty))
        `shouldSatisfy` any (isInfixOf "no deny-list was applied")
      -- ...and not when there is nothing for it to be about
      fmap shown (inboxNotes False (InboxRead other [] True 0 mempty)) `shouldBe` []

    -- And it stops as soon as a list HAS been applied: a warning that never
    -- goes away is one a reader learns to skip, and this one is the difference
    -- between "the rules would take it" and "its author is allowed to send".
    it "stops warning about the deny-list once one has been applied" $ do
      k <- aKey
      let ac = AOpen k HubIssue "t" [] Nothing Nothing Nothing 1
          one = [view (mh "m") (Just k) (Right (k, ac, FoldsToCanon))]
          notes listed = concatMap shown (inboxNotes listed (InboxRead one [] True 0 mempty))
      notes False `shouldSatisfy` isInfixOf "no deny-list"
      notes True `shouldSatisfy` (not . isInfixOf "no deny-list")

  describe "PEP-22 hub inbox: the exit code" $ do

    it "leaves non-zero only when the list is wrong" $ do
      -- A hole in the tree makes the list wrong in BOTH directions: a missing
      -- chunk of Exists entries makes letters vanish, one of Deleted entries puts
      -- folded letters back in the queue.
      inboxCode (InboxRead [] [mh "x"] True 0 mempty) `shouldBe` 2
      -- Still arriving is a SHORTER answer, not a wrong one. A non-zero exit here
      -- would fire on ordinary use and teach a caller to ignore the code.
      inboxCode (InboxRead [] [] False 0 mempty) `shouldBe` 0
      inboxCode (InboxRead [] [] True 0 mempty) `shouldBe` 0

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


spec2 :: Spec
spec2 =
  describe "PEP-22 hub inbox: arguments" $ do

    it "reads either flag, or both, in any order" $ do
      mbox <- aKey ; repo <- aKey
      inboxArgs (argv ["--mailbox", b58 mbox]) `shouldBe` Just (Just mbox, Nothing)
      inboxArgs (argv ["--repo", b58 repo]) `shouldBe` Just (Nothing, Just repo)
      inboxArgs (argv ["--mailbox", b58 mbox, "--repo", b58 repo])
        `shouldBe` Just (Just mbox, Just repo)
      inboxArgs (argv ["--repo", b58 repo, "--mailbox", b58 mbox])
        `shouldBe` Just (Just mbox, Just repo)

    -- BOTH are optional to the reader and not to the verb: a repository alone
    -- resolves its mailbox from the manifest (PEP-18), and neither is a queue
    -- nobody named. The reader cannot tell those apart, so it parses and the
    -- verb refuses.
    it "parses a line naming neither, which the verb then refuses" $ do
      inboxArgs (argv []) `shouldBe` Just (Nothing, Nothing)

    it "refuses a positional key, an unknown flag and a repeat" $ do
      mbox <- aKey ; other <- aKey
      inboxArgs (argv [b58 mbox]) `shouldBe` Nothing
      inboxArgs (argv ["--mailbox", b58 mbox, "--all"]) `shouldBe` Nothing
      inboxArgs (argv ["--mailbox", b58 mbox, "--mailbox", b58 other])
        `shouldBe` Nothing
