-- | The argument reading of @hub pr new@ (PEP-20, PEP-22 "Contribute").
--
-- Its own module for the reason @hub inbox accept@'s is: this verb signs, and
-- what it signs is published. Three of its values are keys or hashes, which
-- are the same thirty-two bytes of base58 as each other, so nothing here can
-- be told apart by position and a swap would be a well-typed pull request
-- against the wrong repository, from the wrong author, into the wrong mailbox.
module HBS2.Hub.PrSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.CLI.Pr
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))
import HBS2.Hash (hashObject)
import HBS2.Data.Types.Refs (HashRef(..))

import Data.Config.Suckless
import Data.Text qualified as Text
import Data.ByteString.Lazy.Char8 qualified as LBS
import Prettyprinter (pretty)
import Test.Hspec

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

argv :: [String] -> [Syntax C]
argv = fmap argvAtom

b58 :: HubKey -> String
b58 = show . pretty . AsBase58

aHash :: String -> HashRef
aHash s = HashRef (hashObject (LBS.pack s))

-- A well-formed call, so a test can change one thing about it.
full :: HubKey -> HashRef -> HashRef -> HubKey -> [String]
full repo sender rcpt author =
  [ "--target", b58 repo
  , "--sender", show (pretty sender)
  , "--recipient", show (pretty rcpt)
  , "--author", b58 author
  , "--title", "make it work"
  , "--onto", "refs/heads/master"
  , "--from", "refs/heads/feature"
  ]

spec :: Spec
spec = spec1 >> spec2

spec1 :: Spec
spec1 = do

  describe "PEP-20 hub pr new: arguments" $ do

    it "reads a complete call, in any order" $ do
      repo <- aKey ; author <- aKey
      let sender = aHash "sender" ; rcpt = aHash "rcpt"
          want = Just (PrNew repo sender rcpt author "make it work"
                             "refs/heads/master" "refs/heads/feature" Nothing)
      prNewArgs (argv (full repo sender rcpt author)) `shouldBe` want
      -- The same call with the flag PAIRS in the opposite order, which is what
      -- "in any order" means: a flag and its value stay adjacent.
      prNewArgs (argv (pairsOf (full repo sender rcpt author))) `shouldBe` want

    -- The default is git's answer, and naming it is for the case where git's
    -- answer is wrong: a rebase onto something the maintainer does not have.
    it "takes an explicit fork point when one is given" $ do
      repo <- aKey ; author <- aKey
      let sender = aHash "sender" ; rcpt = aHash "rcpt"
          base = "0123456789abcdef0123456789abcdef01234567"
      fmap pnBase (prNewArgs (argv (full repo sender rcpt author <> ["--base", base])))
        `shouldBe` Just (Just (Text.pack base))

    it "refuses a call missing any of the seven" $ do
      repo <- aKey ; author <- aKey
      let sender = aHash "sender" ; rcpt = aHash "rcpt"
          whole = full repo sender rcpt author
      -- Drop each flag and its value in turn. Every one of them is required,
      -- and a form this verb cannot fill in must not be acted on: it signs.
      sequence_ [ prNewArgs (argv (dropPair i whole)) `shouldBe` Nothing
                | i <- [0, 2 .. length whole - 2] ]

    it "refuses a repeated flag rather than resolving it" $ do
      repo <- aKey ; author <- aKey ; other <- aKey
      let sender = aHash "sender" ; rcpt = aHash "rcpt"
      prNewArgs (argv (full repo sender rcpt author <> ["--target", b58 other]))
        `shouldBe` Nothing

    -- A sigil is a hash and a repo key is a key, and both spell as thirty-two
    -- bytes of base58: putting a sigil where the target belongs is a form the
    -- parser must refuse rather than one it can detect by shape.
    it "requires the sender and recipient to be hashes and the keys to be keys" $ do
      repo <- aKey ; author <- aKey
      let sender = aHash "sender" ; rcpt = aHash "rcpt"
          swapped = [ "--target", show (pretty sender)     -- a hash where a key goes
                    , "--sender", show (pretty sender)
                    , "--recipient", show (pretty rcpt)
                    , "--author", b58 author
                    , "--title", "t", "--onto", "a", "--from", "b" ]
      -- It parses, because the two are indistinguishable. That is the fact
      -- this test records, and the reason every value is behind a flag: the
      -- flag is the only thing that says which is which.
      prNewArgs (argv swapped) `shouldSatisfy` (/= Nothing)
      prNewArgs (argv (full repo sender rcpt author)) `shouldSatisfy` (/= Nothing)

    -- The test above drops a flag AND its value, which keeps the line aligned
    -- and can only ever say "flag absent". What it could not say is "value
    -- absent", and that is the case that signs: `--title --onto
    -- refs/heads/master` parsed cleanly, bound the title `--onto`, and put it in
    -- the author box and therefore in the event-id.
    it "refuses a flag standing where a value belongs" $ do
      repo <- aKey ; author <- aKey
      let sender = aHash "sender" ; rcpt = aHash "rcpt"
          line' = [ "--target", b58 repo, "--sender", show (pretty sender)
                  , "--recipient", show (pretty rcpt), "--author", b58 author
                  , "--title", "--onto", "refs/heads/master"
                  , "--from", "refs/heads/feature" ]
      prNewArgs (argv line') `shouldBe` Nothing

    it "refuses a flag it does not know instead of dropping it" $ do
      repo <- aKey ; author <- aKey
      let sender = aHash "sender" ; rcpt = aHash "rcpt"
      prNewArgs (argv (full repo sender rcpt author <> ["--draft"]))
        `shouldBe` Nothing
      prNewArgs (argv (full repo sender rcpt author <> ["--titel","x"]))
        `shouldBe` Nothing

    -- argvAtom keeps a word that spells a number AS a number, deliberately, and
    -- every StringLike pattern misses it. A branch may be called 2026.
    it "takes a title and a branch that spell a number" $ do
      repo <- aKey ; author <- aKey
      let sender = aHash "sender" ; rcpt = aHash "rcpt"
          line' = [ "--target", b58 repo, "--sender", show (pretty sender)
                  , "--recipient", show (pretty rcpt), "--author", b58 author
                  , "--title", "2026", "--onto", "refs/heads/master"
                  , "--from", "2026" ]
      fmap (\p -> (pnTitle p, pnFrom p)) (prNewArgs (argv line'))
        `shouldBe` Just ("2026", "2026")

  where
    pairsOf xs = concat (reverse (chunk2 xs))
    chunk2 (a:b:rest) = [a,b] : chunk2 rest
    chunk2 _ = []
    dropPair i xs = take i xs <> drop (i + 2) xs

-- Recording a merge publishes a sentence about history, and canon is
-- append-only. The arguments are what that sentence is made of.
spec2 :: Spec
spec2 =
  describe "PEP-20 hub pr merge: arguments" $ do

    it "reads a complete call and defaults the canon key to the repo" $ do
      repo <- aKey
      prMergeArgs (argv [ "--repo", b58 repo, "--number", "7"
                        , "--commit", "abc", "--into", "refs/heads/master" ])
        `shouldBe` Just (PrMerge repo 7 "abc" "refs/heads/master" repo)

    it "takes a delegate's key when one is named" $ do
      repo <- aKey ; bob <- aKey
      fmap pmAs (prMergeArgs (argv [ "--repo", b58 repo, "--number", "7"
                                   , "--commit", "abc", "--into", "master"
                                   , "--as", b58 bob ]))
        `shouldBe` Just bob

    it "refuses a call missing any of the four" $ do
      repo <- aKey
      let whole = [ "--repo", b58 repo, "--number", "7"
                  , "--commit", "abc", "--into", "master" ]
      sequence_ [ prMergeArgs (argv (take i whole <> drop (i + 2) whole))
                    `shouldBe` Nothing
                | i <- [0, 2 .. length whole - 2] ]

    -- An issue number is a Word64 in canon. A negative literal would wrap into
    -- one nobody minted, and the thread lookup would then answer "no such
    -- number" about a number the caller never typed.
    it "refuses a number that is not one" $ do
      repo <- aKey
      let with n = prMergeArgs (argv [ "--repo", b58 repo, "--number", n
                                     , "--commit", "abc", "--into", "master" ])
      with "-1" `shouldBe` Nothing
      with "seven" `shouldBe` Nothing
      -- And the other end, which the guard against a negative left open: this
      -- wrapped to 1, so the verb looked up pull request #1 and named a number
      -- nobody typed in the commit message it wrote.
      with "18446744073709551617" `shouldBe` Nothing

    -- `--dry-run` is not a flag this verb has, and it was dropped on the floor:
    -- the merge was minted, signed and committed onto append-only canon by a
    -- command whose author believed it would not be.
    it "refuses a flag it does not know instead of recording the merge" $ do
      repo <- aKey
      prMergeArgs (argv [ "--repo", b58 repo, "--number", "7"
                        , "--commit", "abc", "--into", "master", "--dry-run" ])
        `shouldBe` Nothing

    -- About one in twenty-seven seven-character abbreviated shas is all digits.
    it "takes a commit that spells a number" $ do
      repo <- aKey
      fmap pmCommit (prMergeArgs (argv [ "--repo", b58 repo, "--number", "7"
                                       , "--commit", "1234567", "--into", "master" ]))
        `shouldBe` Just "1234567"

    -- --as names the key canon is BLESSED with. Answering "not given" for
    -- "given, and not a key" fell back to the repo key and signed under a key
    -- the caller did not name.
    it "refuses an --as that is not a key rather than defaulting" $ do
      repo <- aKey
      prMergeArgs (argv [ "--repo", b58 repo, "--number", "7"
                        , "--commit", "abc", "--into", "master"
                        , "--as", "not-a-key" ])
        `shouldBe` Nothing

    it "refuses a repeated flag rather than resolving it" $ do
      repo <- aKey
      prMergeArgs (argv [ "--repo", b58 repo, "--number", "7", "--number", "8"
                        , "--commit", "abc", "--into", "master" ])
        `shouldBe` Nothing
