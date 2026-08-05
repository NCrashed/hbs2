-- | The log of what this node sent (PEP-18 "Acknowledgement letter").
--
-- Its job is to answer one question -- is this thread one we submitted -- and
-- the answers matter in opposite directions: a false yes shows somebody else's
-- business as your own, a false no hides your own answer. So the tests are the
-- round trip (a title survives being written and read back), the refusals (a
-- record that will not read stops the reader rather than shortening the log),
-- and the predicate's one deliberate looseness.
module HBS2.Hub.SentSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Sent

import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject)
import HBS2.Net.Auth.Credentials

import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Text qualified as Text
import Test.Hspec

mh :: BS.ByteString -> HashRef
mh = HashRef . hashObject

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

opened :: HubKey -> HubKey -> Sent
opened repo author = Sent
  { seThread = mh "thread"
  , seEvent = mh "thread"
  , seMessage = mh "message"
  , seRepo = Just repo
  , seAuthor = author
  , seAt = 1754400000000
  , seWhat = "issue new"
  , seTitle = Just "it crashes"
  }

spec :: Spec
spec = do

  describe "PEP-18 sent log: the file" $ do

    it "reads back what it wrote" $ do
      repo <- aKey ; author <- aKey
      let s = opened repo author
      parseSent (renderSent s) `shouldBe` Right [s]

    it "reads back a record with no repository and no title" $ do
      repo <- aKey ; author <- aKey
      let s = (opened repo author) { seRepo = Nothing, seTitle = Nothing }
      parseSent (renderSent s) `shouldBe` Right [s]

    it "keeps several records, in the order they were appended" $ do
      repo <- aKey ; author <- aKey
      let a = opened repo author
          b = a { seThread = mh "other", seWhat = "comment" }
      parseSent (renderSent a <> "\n" <> renderSent b) `shouldBe` Right [a, b]

    -- A title is whatever somebody typed and this file is read with cat far
    -- more often than with a parser: a raw escape sequence in it rewrites what
    -- the reader sees. It still has to survive the round trip byte for byte,
    -- because the point of writing it down is to show it back.
    it "survives a title holding quotes, newlines and an escape" $ do
      repo <- aKey ; author <- aKey
      let nasty = "a \"quoted\" title\nwith \ESC[2K in it"
          s = (opened repo author) { seTitle = Just nasty }
      Text.any (== '\ESC') (renderSent s) `shouldBe` False
      fmap (fmap seTitle) (parseSent (renderSent s)) `shouldBe` Right [Just nasty]

    -- The opposite decision from the deny-list's, on purpose: this file is
    -- written by one version of the tool and read by the next.
    it "ignores a clause it does not know rather than refusing the record" $ do
      repo <- aKey ; author <- aKey
      let s = renderSent (opened repo author)
          withExtra = Text.dropEnd 1 s <> " (mood \"cheerful\"))"
      fmap (fmap seThread) (parseSent withExtra) `shouldBe` Right [mh "thread"]

    -- And this one keeps the deny-list's: a log that quietly drops what it
    -- cannot read is a log that shortens itself, and it decides whether an
    -- acknowledgement is shown as yours.
    it "refuses a record that is damaged rather than skipping it" $ do
      repo <- aKey ; author <- aKey
      let s = renderSent (opened repo author)
      parseSent (Text.replace "(thread" "(thraed" s) `shouldSatisfy` isLeft
      parseSent (Text.replace "(at 1754400000000)" "(at -1)" s) `shouldSatisfy` isLeft
      parseSent "(sent)" `shouldSatisfy` isLeft
      parseSent "hello" `shouldSatisfy` isLeft

    -- A clause that is present and unreadable is a damaged record, not an
    -- absent field: the two cases mean different things and only one of them
    -- is a comment.
    it "tells an absent repository from one that will not read" $ do
      repo <- aKey ; author <- aKey
      let s = renderSent (opened repo author)
      parseSent (Text.replace "(repo " "(repo x" s) `shouldSatisfy` isLeft

  describe "PEP-18 sent log: did this node submit that thread" $ do

    it "matches on the thread and the repository together" $ do
      repo <- aKey ; other <- aKey ; author <- aKey
      let log' = [opened repo author]
      submittedBy log' repo (mh "thread") `shouldBe` True
      submittedBy log' other (mh "thread") `shouldBe` False
      submittedBy log' repo (mh "elsewhere") `shouldBe` False
      submittedBy [] repo (mh "thread") `shouldBe` False

    -- The deliberate looseness: a comment names a thread and no repository
    -- (PEP-18), so a record with none matches on the thread alone. Dropping
    -- those would leave every ack about a thread you replied in unmatched.
    it "matches a record with no repository on the thread alone" $ do
      repo <- aKey ; other <- aKey ; author <- aKey
      let log' = [(opened repo author) { seRepo = Nothing }]
      submittedBy log' repo (mh "thread") `shouldBe` True
      submittedBy log' other (mh "thread") `shouldBe` True
      submittedBy log' repo (mh "elsewhere") `shouldBe` False
