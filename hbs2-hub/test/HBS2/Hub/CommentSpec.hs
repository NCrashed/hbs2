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
          want = Just (CommentArgs s r author t Nothing (Just "hi"))
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

    -- No repository flag exists and none should: a comment names a thread, the
    -- thread names the repository, and a --target here would be a second
    -- answer to a question the fold already answers.
    it "refuses a repository flag, since a thread already names one" $ do
      author <- aKey
      let s = mh "sender" ; r = mh "rcpt" ; t = mh "thread"
      commentArgs (argv [ "--sender", href s, "--recipient", href r
                        , "--author", b58 author, "--thread", href t
                        , "--target", b58 author, "--body", "hi" ])
        `shouldBe` Nothing

    it "refuses a call missing any of the four it needs" $ do
      author <- aKey
      let s = mh "sender" ; r = mh "rcpt" ; t = mh "thread"
          full = [ "--sender", href s, "--recipient", href r
                 , "--author", b58 author, "--thread", href t ]
      -- Each flag dropped in turn, with its value.
      commentArgs (argv (drop 2 full)) `shouldBe` Nothing
      commentArgs (argv (take 2 full <> drop 4 full)) `shouldBe` Nothing
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
