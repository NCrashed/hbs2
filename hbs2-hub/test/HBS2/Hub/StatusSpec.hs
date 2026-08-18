-- | What @hub status@ and @hub sent@ say (PEP-22).
--
-- Both verbs gather their facts from five places -- a keyring, git, a remote, a
-- manifest and a peer -- and neither of those is reachable from here. What IS
-- reachable is the whole of what they decide: the report is a function of a
-- record, and the record is what the verb spent its work building.
module HBS2.Hub.StatusSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Sent (Sent(..))
import HBS2.Hub.CLI.Status
import HBS2.Hub.CLI.Sent (sentDoc,sentNote,SentArgs(..),sentArgs)
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject)
import HBS2.Net.Auth.Credentials
import HBS2.Prelude.Plated (Doc,pretty)

import Data.Config.Suckless (C,Syntax)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.List (isInfixOf)
import Test.Hspec

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

mh :: String -> HashRef
mh = HashRef . hashObject . LBS.pack

argv :: [String] -> [Syntax C]
argv = fmap argvAtom

b58 :: HubKey -> String
b58 = show . pretty . AsBase58

said :: [Doc ()] -> String
said = unlines . fmap show

-- A standing with nothing interesting in it, for the cases that vary one field.
plain :: RepoRef -> Standing
plain repo = Standing
  { stRepo = repo, stCanSign = True, stCanon = Just "cafe1234"
  , stThreads = 3, stEvents = 9, stKeys = 2
  , stPublished = Level, stMailbox = Nothing, stWaiting = Right 0
  }

spec :: Spec
spec = do

  describe "PEP-22 hub status: what it says" $ do

    it "names the repository and whether this machine can sign for it" $ do
      repo <- aKey
      said (statusDoc (plain repo)) `shouldSatisfy` isInfixOf (b58 repo)
      said (statusDoc (plain repo) ) `shouldSatisfy` isInfixOf "signing"
      said (statusDoc (plain repo) { stCanSign = False })
        `shouldSatisfy` isInfixOf "cannot author owner events"

    -- The four publication states are four sentences on purpose: "not
    -- published" covers cases a maintainer would act on differently.
    it "tells the four ways canon can stand against a remote apart" $ do
      repo <- aKey
      let says p = said (statusDoc (plain repo) { stPublished = p })
      says Level        `shouldSatisfy` isInfixOf "yes:"
      says RemoteEmpty  `shouldSatisfy` isInfixOf "publish"
      says (Diverged "beef") `shouldSatisfy` isInfixOf "beef"
      says (Diverged "beef") `shouldSatisfy` isInfixOf "sync"
      says (RemoteSilent "no such remote") `shouldSatisfy` isInfixOf "no such remote"

    it "says there is nothing to publish when nothing was folded here" $ do
      repo <- aKey
      said (statusDoc (plain repo) { stCanon = Nothing, stPublished = NothingHere })
        `shouldSatisfy` isInfixOf "none here yet"

    -- THE ABSENCE THAT MATTERS. A report that left the line out when the peer
    -- would not answer reads as "nothing waiting", which is the answer somebody
    -- acts on by going away.
    it "says a mailbox would not read, rather than leaving it out" $ do
      repo <- aKey
      mbox <- aKey
      let says w = said (statusDoc (plain repo) { stMailbox = Just mbox, stWaiting = w })
      says (Left "the peer is not running") `shouldSatisfy` isInfixOf "not read"
      says (Left "the peer is not running") `shouldSatisfy` isInfixOf "not running"
      says (Right 0) `shouldSatisfy` isInfixOf "nothing waiting"
      says (Right 4) `shouldSatisfy` isInfixOf "4 letter(s) waiting"

    it "says a repository that declares no mailbox declares none" $ do
      repo <- aKey
      said (statusDoc (plain repo)) `shouldSatisfy` isInfixOf "none declared"

    -- A remote name and a commit both reach a terminal, and a remote chose one
    -- of them.
    it "escapes what a remote said" $ do
      repo <- aKey
      said (statusDoc (plain repo) { stPublished = RemoteSilent "\ESC[2Kgone" })
        `shouldSatisfy` (not . isInfixOf "\ESC")

  describe "PEP-22 hub status: arguments" $ do

    it "takes the repository, and defaults the remote to origin" $ do
      repo <- aKey
      statusArgs (argv ["--repo", b58 repo])
        `shouldBe` Just (StatusArgs repo "origin")
      statusArgs (argv ["--repo", b58 repo, "--remote", "upstream"])
        `shouldBe` Just (StatusArgs repo "upstream")

    it "refuses a call with no repository, and a flag it does not know" $ do
      repo <- aKey
      statusArgs (argv []) `shouldBe` Nothing
      statusArgs (argv [b58 repo]) `shouldBe` Nothing
      statusArgs (argv ["--repo", b58 repo, "--long"]) `shouldBe` Nothing

  describe "PEP-22 hub sent: what it says" $ do

    let one repo = Sent { seThread = mh "t", seEvent = mh "e", seMessage = mh "m"
                        , seRepo = Just repo, seAuthor = repo, seAt = 1000
                        , seWhat = "issue new", seTitle = Just "the tests hang"
                        }

    it "puts the message, the verb and the title on the row" $ do
      repo <- aKey
      let row = said (sentDoc False [one repo])
      row `shouldSatisfy` isInfixOf "issue new"
      row `shouldSatisfy` isInfixOf "the tests hang"
      -- Elided like the queue's, and for the same reason.
      row `shouldSatisfy` isInfixOf (take 8 (show (pretty (mh "m"))) <> "..")
      row `shouldSatisfy` (not . isInfixOf (show (pretty (mh "m"))))

    it "prints whole identifiers when asked to" $ do
      repo <- aKey
      said (sentDoc True [one repo]) `shouldSatisfy` isInfixOf (show (pretty (mh "m")))

    -- A LOG IS NOT A DELIVERY RECEIPT, and the note is the only thing that says
    -- so: a row on its own reads like a letter that arrived.
    it "will not let a row be read as delivery" $ do
      show (sentNote 3) `shouldSatisfy` isInfixOf "not that a mailbox took them"
      show (sentNote 3) `shouldSatisfy` isInfixOf "updates"
      show (sentNote 0) `shouldSatisfy` isInfixOf "nothing has been sent"

    it "reads its two flags" $ do
      repo <- aKey
      sentArgs (argv []) `shouldBe` Just (SentArgs Nothing False)
      sentArgs (argv ["--repo", b58 repo]) `shouldBe` Just (SentArgs (Just repo) False)
      sentArgs (argv ["--long"]) `shouldBe` Just (SentArgs Nothing True)
      sentArgs (argv ["--long", "yes"]) `shouldBe` Nothing
