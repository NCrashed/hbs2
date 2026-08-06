-- | @hub issue comment@ / @hub pr comment@: what a command line means.
--
-- Four of this verb's five values are thirty-two bytes of base58 -- two sigils,
-- an author key and a thread-id -- and what they name is a signed reply in
-- somebody's thread. So the tests are about the swaps a parser can catch and the
-- one it cannot.
module HBS2.Hub.CommentSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.CLI.Comment
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject)
import HBS2.Net.Auth.Credentials
import HBS2.Prelude.Plated (pretty)

import Data.Config.Suckless (Syntax,C)
import Data.ByteString qualified as BS
import Data.Maybe (isJust)
import Test.Hspec

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

b58 :: HubKey -> String
b58 = show . pretty . AsBase58

mh :: BS.ByteString -> HashRef
mh = HashRef . hashObject

href :: HashRef -> String
href = show . pretty

argv :: [String] -> [Syntax C]
argv = fmap argvAtom

spec :: Spec
spec = do

  describe "PEP-22 hub issue|pr comment: arguments" $ do

    it "reads a complete call, in any order" $ do
      author <- aKey
      let s = mh "sender" ; r = mh "rcpt" ; t = mh "thread"
          want = Just (CommentArgs s (Just r) Nothing author t Nothing (Just "hi"))
      commentArgs (argv [ "--sender", href s, "--recipient", href r
                        , "--author", b58 author, "--thread", href t
                        , "--body", "hi" ])
        `shouldBe` want
      commentArgs (argv [ "--body", "hi", "--thread", href t
                        , "--author", b58 author, "--recipient", href r
                        , "--sender", href s ])
        `shouldBe` want

    it "takes the event being answered when one is named" $ do
      author <- aKey
      let s = mh "sender" ; r = mh "rcpt" ; t = mh "thread" ; e = mh "event"
      fmap cmReplyTo (commentArgs (argv [ "--sender", href s, "--recipient", href r
                                        , "--author", b58 author, "--thread", href t
                                        , "--reply-to", href e, "--body", "hi" ]))
        `shouldBe` Just (Just e)

    -- --target says WHERE to send it and never enters the letter: a comment
    -- names a thread and carries no repository (PEP-18), and the fold binds it
    -- to one through the thread. It is here because a sigil has to come from
    -- somewhere and the manifest is keyed by repository.
    it "takes a target for addressing, which is not part of the letter" $ do
      author <- aKey ; repo <- aKey
      let s = mh "sender" ; t = mh "thread"
      fmap (\a -> (cmTarget a, cmRcpt a))
           (commentArgs (argv [ "--sender", href s, "--target", b58 repo
                              , "--author", b58 author, "--thread", href t
                              , "--body", "hi" ]))
        `shouldBe` Just (Just repo, Nothing)

    -- Either names where it goes; neither is a letter with no address, which
    -- the verb refuses rather than the reader.
    it "parses with neither address, which the verb then refuses" $ do
      author <- aKey
      let s = mh "sender" ; t = mh "thread"
      fmap cmRcpt (commentArgs (argv [ "--sender", href s
                                     , "--author", b58 author, "--thread", href t
                                     , "--body", "hi" ]))
        `shouldBe` Just Nothing

    it "refuses a call missing any of the four it needs" $ do
      author <- aKey
      let s = mh "sender" ; r = mh "rcpt" ; t = mh "thread"
          full = [ "--sender", href s, "--recipient", href r
                 , "--author", b58 author, "--thread", href t ]
      -- The sender, the author and the thread are required; the recipient is
      -- the one that may be resolved instead.
      commentArgs (argv (drop 2 full)) `shouldBe` Nothing
      commentArgs (argv (take 4 full <> drop 6 full)) `shouldBe` Nothing
      commentArgs (argv (take 6 full)) `shouldBe` Nothing

    -- The body is optional to the PARSER and refused by the verb: an absent
    -- --body is a caller who forgot one, and the difference between that and an
    -- empty body is not something the reader can decide.
    it "parses without a body, which the verb then refuses" $ do
      author <- aKey
      let s = mh "sender" ; r = mh "rcpt" ; t = mh "thread"
      fmap cmBody (commentArgs (argv [ "--sender", href s, "--recipient", href r
                                     , "--author", b58 author, "--thread", href t ]))
        `shouldBe` Just Nothing

    -- argvAtom keeps a numeric word as a number, and every StringLike pattern
    -- then misses it.
    it "takes a body that spells a number" $ do
      author <- aKey
      let s = mh "sender" ; r = mh "rcpt" ; t = mh "thread"
      fmap cmBody (commentArgs (argv [ "--sender", href s, "--recipient", href r
                                     , "--author", b58 author, "--thread", href t
                                     , "--body", "2026" ]))
        `shouldBe` Just (Just "2026")

    it "refuses an unknown flag and a flag standing where a value belongs" $ do
      author <- aKey
      let s = mh "sender" ; r = mh "rcpt" ; t = mh "thread"
          full = [ "--sender", href s, "--recipient", href r
                 , "--author", b58 author, "--thread", href t ]
      commentArgs (argv (full <> ["--body", "hi", "--draft"])) `shouldBe` Nothing
      commentArgs (argv (full <> ["--body", "--reply-to", href t])) `shouldBe` Nothing

    -- The swap no parser can catch, recorded: the two sigils are both hashes,
    -- so sender and recipient are interchangeable at the pattern level and only
    -- the flag names tell them apart.
    it "cannot tell one sigil from the other, which is why both are flagged" $ do
      author <- aKey
      let s = mh "sender"
      isJust (commentArgs (argv [ "--sender", href s, "--recipient", href s
                                , "--author", b58 author, "--thread", href s
                                , "--body", "hi" ]))
        `shouldBe` True
