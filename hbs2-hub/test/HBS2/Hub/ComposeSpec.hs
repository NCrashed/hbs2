module HBS2.Hub.ComposeSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.CLI.Compose (issueArgs,issueUsage,stampsFor)
import HBS2.Hub.CLI.Policy (PolicyReader,PolicyGone(..),withPoW)

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject)
import HBS2.Net.Auth.Credentials
import HBS2.Prelude.Plated (Doc,pretty)

import HBS2.Data.Types.SignedBox (makeSignedBox)
import HBS2.Data.Types.EncryptedBox
import HBS2.Data.Types.SmallEncryptedBlock
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.PoW (stampOk)
import HBS2.Peer.Proto.Mailbox.Policy.Basic (defaultBasicPolicy)

import Data.Config.Suckless

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Set qualified as Set
import Data.List (isInfixOf)
import Test.Hspec

shown :: Doc () -> String
shown = show

-- A key or a hash as the word somebody would type at a shell.
word :: Show a => a -> Syntax C
word = mkStr @C . show

keyWord :: HubKeyOf -> Syntax C
keyWord k = mkStr @C (show (pretty (AsBase58 k)))

type HubKeyOf = PubKey 'Sign 'HBS2Basic

href :: ByteString -> HashRef
href = HashRef . hashObject

aKey :: IO HubKeyOf
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

spec :: Spec
spec = work >> do

  describe "PEP-22 hub issue new: which value is which" $ do

    -- THE POSITIONAL FORM IS GONE, and this test was the one that drove it.
    --
    -- It was four base58 blobs in a row and two PAIRS of them are
    -- interchangeable at the pattern level: repo-key and author-key are both
    -- SignPubKeyLike, the two sigils are both HashLike. Swapping either pair
    -- produced a valid, signed, DELIVERED letter authored by the repository key,
    -- with no error and a zero exit -- and an authorship claim lives inside a
    -- signed box, so nothing afterwards takes it back.
    --
    -- The old comment here said the form stays "because it is what exists and
    -- what the tests drive", which is a reason to keep a form and not a reason
    -- it is safe. Removed before a release rather than after, because the shape
    -- is well-typed either way round: the only defence a caller has is that the
    -- tool will not take it.
    it "refuses the five positional arguments it used to read" $ do
      repo <- aKey
      author <- aKey
      let s = href "sender"
          r = href "rcpt"
          argv = [ keyWord repo, word (pretty s), word (pretty r), keyWord author
                 , mkStr @C "a title" ]
      issueArgs argv `shouldBe` Nothing
      -- And the swap it used to accept in silence, which is the whole reason:
      -- same shape, two of the four values exchanged.
      issueArgs [ keyWord author, word (pretty r), word (pretty s), keyWord repo
                , mkStr @C "a title" ]
        `shouldBe` Nothing
      -- The usage no longer offers it either, or the refusal above would send a
      -- reader straight back to the form that was just taken away.
      show (issueUsage :: Doc ()) `shouldSatisfy` not . isInfixOf "<repo-key> <sender-sigil>"

    it "reads them by name, in any order" $ do
      -- The reason the named form exists: FOUR base58 blobs in a row, and two
      -- pairs of them are interchangeable at the pattern level -- repo-key and
      -- author-key are both SignPubKeyLike, the two sigils are both HashLike. A
      -- swap produced a valid, signed, DELIVERED letter authored by the
      -- repository key, with no error and a zero exit, and an authorship claim
      -- inside a signed box is permanent. A name cannot be swapped silently.
      repo <- aKey
      author <- aKey
      let s = href "sender"
          r = href "rcpt"
          named = [ mkStr @C "--target", keyWord repo
                  , mkStr @C "--sender", word (pretty s)
                  , mkStr @C "--recipient", word (pretty r)
                  , mkStr @C "--author", keyWord author
                  , mkStr @C "--title", mkStr @C "a title" ]
          shuffled = [ mkStr @C "--title", mkStr @C "a title"
                     , mkStr @C "--author", keyWord author
                     , mkStr @C "--recipient", word (pretty r)
                     , mkStr @C "--sender", word (pretty s)
                     , mkStr @C "--target", keyWord repo ]
      issueArgs named `shouldBe` Just (repo, s, Just r, author, "a title", [], Nothing)
      -- The property, rather than two examples of it: order carries no meaning
      -- in the named form, which is the whole point of having it.
      issueArgs shuffled `shouldBe` issueArgs named

    it "refuses a flag it does not know instead of defaulting past it" $ do
      -- Without this, `--titel X` is dropped on the floor and the letter goes out
      -- under whatever the other flags said -- which, for a letter, means signed
      -- and gone.
      repo <- aKey
      author <- aKey
      let with k = [ mkStr @C "--target", keyWord repo
                   , mkStr @C "--sender", word (pretty (href "s"))
                   , mkStr @C "--recipient", word (pretty (href "r"))
                   , mkStr @C "--author", keyWord author
                   , mkStr @C "--title", mkStr @C "t"
                   , mkStr @C k, mkStr @C "x" ]
      issueArgs (with "--titel") `shouldBe` Nothing
      issueArgs (with "--draft") `shouldBe` Nothing

    it "takes the labels PEP-22 says it takes, as many as are given" $ do
      -- PEP-22:280 spells the verb `hub issue new --target <repo> --title ...
      -- [--label ...]`, and this used to REFUSE the flag and hard-code the
      -- requested labels to []. So `labels_requested` in the render contract
      -- (PEP-22:410) could never be populated by the tool that populates it, and
      -- a person following the spec got a usage message that does not mention
      -- labels.
      repo <- aKey
      author <- aKey
      let labelled ls = [ mkStr @C "--target", keyWord repo
                        , mkStr @C "--sender", word (pretty (href "s"))
                        , mkStr @C "--recipient", word (pretty (href "r"))
                        , mkStr @C "--author", keyWord author
                        , mkStr @C "--title", mkStr @C "t" ]
                        <> concat [ [mkStr @C "--label", mkStr @C l] | l <- ls ]
          labelsOf = fmap (\(_,_,_,_,_,ls,_) -> ls) . issueArgs
      labelsOf (labelled [])             `shouldBe` Just []
      labelsOf (labelled ["bug"])        `shouldBe` Just ["bug"]
      -- Repeatable, and in the order given: unlike every other flag here, a
      -- second --label is not a contradiction to refuse.
      labelsOf (labelled ["bug","ui"])   `shouldBe` Just ["bug","ui"]

    it "will not take a flag as the value of a flag" $ do
      -- `--title --draft` parsed cleanly and signed the string `--draft` as the
      -- title. The title goes inside the author box, and the event-id is the
      -- hash of that box, so it cannot be corrected afterwards: canon is
      -- append-only. That is the hazard the named form exists to prevent,
      -- reached by another route.
      repo <- aKey
      author <- aKey
      let titled t = [ mkStr @C "--target", keyWord repo
                     , mkStr @C "--sender", word (pretty (href "s"))
                     , mkStr @C "--recipient", word (pretty (href "r"))
                     , mkStr @C "--author", keyWord author
                     , mkStr @C "--title", t ]
      issueArgs (titled (mkStr @C "--draft")) `shouldBe` Nothing

    it "accepts the --flag=value spelling, which was accepted nowhere" $ do
      -- `--title=t` used to fall through and pair the whole word `--title=t`
      -- with the NEXT one, then print a usage message that did not say what was
      -- wrong. It is also the escape hatch that makes refusing `--title --draft`
      -- affordable: a title that begins with a dash is still expressible.
      repo <- aKey
      author <- aKey
      let argvEq t = [ mkStr @C ("--target=" <> show (pretty (AsBase58 repo)))
                     , mkStr @C ("--sender=" <> show (pretty (href "s")))
                     , mkStr @C ("--recipient=" <> show (pretty (href "r")))
                     , mkStr @C ("--author=" <> show (pretty (AsBase58 author)))
                     , mkStr @C ("--title=" <> t)
                     , mkStr @C "--label=bug" ]
      issueArgs (argvEq "a title")
        `shouldBe` Just (repo, href "s", Just (href "r"), author, "a title", ["bug"], Nothing)
      -- A value may hold an '=' of its own: the split is on the first one only.
      fmap (\(_,_,_,_,t,_,_) -> t) (issueArgs (argvEq "a=b")) `shouldBe` Just "a=b"
      -- And the dash case the refusal above makes necessary.
      fmap (\(_,_,_,_,t,_,_) -> t) (issueArgs (argvEq "--draft")) `shouldBe` Just "--draft"

    it "refuses a flag given twice instead of choosing" $ do
      repo <- aKey
      author <- aKey
      issueArgs [ mkStr @C "--target", keyWord repo
                , mkStr @C "--sender", word (pretty (href "s"))
                , mkStr @C "--recipient", word (pretty (href "r"))
                , mkStr @C "--author", keyWord author
                , mkStr @C "--title", mkStr @C "one"
                , mkStr @C "--title", mkStr @C "two" ] `shouldBe` Nothing

    it "refuses a flag with nothing after it" $ do
      repo <- aKey
      issueArgs [ mkStr @C "--target", keyWord repo, mkStr @C "--title" ]
        `shouldBe` Nothing

    it "refuses a partial named form rather than filling a blank" $ do
      repo <- aKey
      issueArgs [ mkStr @C "--target", keyWord repo ] `shouldBe` Nothing
      issueArgs ([] :: [Syntax C]) `shouldBe` Nothing

    it "refuses a value that is not the kind the name asks for" $ do
      -- A sigil is a hash and a target is a key; neither is "whatever parses".
      author <- aKey
      issueArgs [ mkStr @C "--target", mkStr @C "not-a-key"
                , mkStr @C "--sender", word (pretty (href "s"))
                , mkStr @C "--recipient", word (pretty (href "r"))
                , mkStr @C "--author", keyWord author
                , mkStr @C "--title", mkStr @C "t" ] `shouldBe` Nothing

    it "lets an issue be called 2026" $ do
      -- A title is text, and the argv reader keeps a word that spells a number
      -- AS a number on purpose, because half the inherited dictionary matches on
      -- integers. Every title pattern here is StringLike, which matches a symbol
      -- or a string and not a number, so `--title 2026` died with a usage
      -- message that said nothing about why. Rendering the atom back is lossless
      -- by construction: the reader kept it as a number only because rendering
      -- it gives back the characters that were typed.
      repo <- aKey
      author <- aKey
      let titled t = [ mkStr @C "--target", keyWord repo
                     , mkStr @C "--sender", word (pretty (href "s"))
                     , mkStr @C "--recipient", word (pretty (href "r"))
                     , mkStr @C "--author", keyWord author
                     , mkStr @C "--title", t ]
          titleOf = fmap (\(_,_,_,_,t,_,_) -> t) . issueArgs
      titleOf (titled (mkInt @C (2026 :: Int))) `shouldBe` Just "2026"
      titleOf (titled (mkStr @C "2026"))        `shouldBe` Just "2026"
      -- ...and a list is still not a title
      titleOf (titled (mkList @C [mkStr @C "a"])) `shouldBe` Nothing

    it "says what it takes, and why the names are worth typing" $ do
      shown issueUsage `shouldSatisfy` isInfixOf "usage: hub issue new --repo"
      -- The consequence, not a fragment of the sentence carrying it: the whole
      -- reason to prefer the flags is that a positional swap is signed and gone.
      shown issueUsage `shouldSatisfy` isInfixOf "claiming the wrong author"

    -- git hands a hook `<old> <new> <ref-name>` on stdin, and a body goes into
    -- the author box and therefore into the event-id, where canon is
    -- append-only. So the body is NAMED now: no --body, no body, and stdin is
    -- untouched. `--body -` is the pipeline spelling and says so out loud.
    it "takes the body from a flag, and stdin only when asked" $ do
      repo <- aKey
      author <- aKey
      let with b = [ mkStr @C "--target", keyWord repo
                   , mkStr @C "--sender", word (pretty (href "s"))
                   , mkStr @C "--recipient", word (pretty (href "r"))
                   , mkStr @C "--author", keyWord author
                   , mkStr @C "--title", mkStr @C "t" ] <> b
          bodyOf' = fmap (\(_,_,_,_,_,_,b) -> b) . issueArgs
      bodyOf' (with []) `shouldBe` Just Nothing
      bodyOf' (with [mkStr @C "--body", mkStr @C "the body"])
        `shouldBe` Just (Just "the body")
      bodyOf' (with [mkStr @C "--body", mkStr @C "-"]) `shouldBe` Just (Just "-")
      -- And it is a value like any other: a flag where it belongs is missing.
      issueArgs (with [mkStr @C "--body", mkStr @C "--label"]) `shouldBe` Nothing

-- | THE WORK A SENDER PAYS, AND WHEN IT PAYS NONE.
--
-- `stampsFor` could be made to skip every grind with the suite green, and what
-- that costs is letters dropped by a hub for want of work with no signal to the
-- sender (PEP-21 says plainly that the sender gets none). It was unreachable
-- because it took the storage and the service caller that read the policy; it
-- takes the READER now, so the whole path runs here with no peer.
work :: Spec
work =
  describe "PEP-21 the work a sender pays before sending" $ do

    -- Three bits, because the test grinds for real: what is asserted is not
    -- that a stamp object came back but that it satisfies the peer's own
    -- checker, for THAT mailbox and THAT message.
    it "solves what a charging mailbox asks, for that mailbox and that message" $ do
      creds <- newCredentials @'HBS2Basic
      mbox  <- aKey
      let msg = messageTo creds [mbox]
      stampsFor (charging 3) msg >>= \case
        [s]   -> stampOk 3 mbox msg s `shouldBe` True
        other -> expectationFailure ("expected one stamp, got " <> show (length other))

    -- A stamp is work for ONE mailbox (PEP-21), so a letter to two that charge
    -- is paid twice, and each stamp names its own.
    it "pays each charging recipient separately" $ do
      creds <- newCredentials @'HBS2Basic
      one <- aKey ; two <- aKey
      let msg = messageTo creds [one, two]
      stamps <- stampsFor (charging 2) msg
      length stamps `shouldBe` 2
      [ () | s <- stamps, m <- [one, two], stampOk 2 m msg s ] `shouldSatisfy` ((>= 2) . length)

    it "does no work for a mailbox that charges nothing" $ do
      creds <- newCredentials @'HBS2Basic
      mbox <- aKey
      let msg = messageTo creds [mbox]
      fmap length (stampsFor (charging 0) msg) >>= (`shouldBe` 0)
      -- ...nor for one with no policy at all, which is not the deny/deny
      -- fallback and charges nothing either way.
      fmap length (stampsFor (const (pure (Right Nothing))) msg) >>= (`shouldBe` 0)

    -- The ordinary case for a letter addressed to somebody else's hub: this
    -- peer holds no policy for it, which is no evidence of a charge and not a
    -- complaint either.
    it "says nothing about a mailbox this peer does not hold" $ do
      creds <- newCredentials @'HBS2Basic
      mbox <- aKey
      let msg = messageTo creds [mbox]
      fmap length (stampsFor (const (pure (Left (PolicyNotHere mbox)))) msg) >>= (`shouldBe` 0)

    -- And a policy that will NOT read is not a policy that charges nothing:
    -- the letter still goes -- refusing over a broken file on somebody else's
    -- peer would be the worse failure -- and the sender is told.
    it "sends without work over a policy it cannot read" $ do
      creds <- newCredentials @'HBS2Basic
      mbox <- aKey
      let msg = messageTo creds [mbox]
      fmap length (stampsFor (const (pure (Left PolicyUnparsed))) msg) >>= (`shouldBe` 0)

-- A policy that charges this many bits and says nothing else.
charging :: Applicative m => PoWDifficulty -> PolicyReader m
charging d _ = pure (Right (Just (0, withPoW d (defaultBasicPolicy @'HBS2Basic))))

-- A message addressed to these mailboxes. Built by hand rather than through
-- 'createMessage', which needs a storage and a keyman: what the work is over is
-- the message's own bytes and the recipient set inside its signed content.
messageTo :: PeerCredentials 'HBS2Basic -> [HubKey] -> Message 'HBS2Basic
messageTo creds rcpts =
  MessageBasic (makeSignedBox @'HBS2Basic (_peerSignPk creds) (_peerSignSk creds) content)
  where
    content = MessageContent (MessageFlags1 (MessageTimestamp 0) Nothing Nothing Nothing)
                (Set.fromList rcpts)
                (Left (HashRef (hashObject ("a group key" :: ByteString))))
                mempty
                (SmallEncryptedBlock (HashRef (hashObject ("gk" :: ByteString)))
                                     (BS.replicate 24 0x6e)
                                     (EncryptedBox "not a secretbox"))
