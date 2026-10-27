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
import HBS2.Hub.CLI.Signer (Signer(..),SignerSource(..),SignerTrouble(..))
import HBS2.Hub.CLI.Sent (sentDoc,sentNote,SentArgs(..),sentArgs,sentContract)
import HBS2.Hub.CLI.Argv (argvAtom)

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject)
import HBS2.Net.Auth.Credentials
import HBS2.Prelude.Plated (Doc,pretty)

import Data.Config.Suckless (C,Syntax)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Aeson (Value(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.Text qualified as Text
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
  , stSigner = Right (Signer repo RepoKeyDefault), stMaintains = True
  , stThreads = 3, stEvents = 9, stKeys = 2
  , stPublished = Right [("origin", Level)], stMailbox = Nothing, stWaiting = Right 0
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
      let says p = said (statusDoc (plain repo) { stPublished = Right [("origin", p)] })
      says Level        `shouldSatisfy` isInfixOf "yes:"
      says RemoteEmpty  `shouldSatisfy` isInfixOf "publish"
      says (Diverged "beef") `shouldSatisfy` isInfixOf "beef"
      says (Diverged "beef") `shouldSatisfy` isInfixOf "sync"
      says (RemoteSilent "no such remote") `shouldSatisfy` isInfixOf "no such remote"

    -- THE BUG THIS SHAPE FIXES. `hub publish --remote practice-forum`
    -- succeeded, and status probed origin alone and said "the remote holds no
    -- canon at all" without saying which remote.
    it "names the remote each publication line is about" $ do
      repo <- aKey
      let s = said (statusDoc (plain repo)
                      { stPublished = Right [ ("origin", RemoteEmpty)
                                            , ("practice-forum", Level) ] })
      s `shouldSatisfy` isInfixOf "published origin: NO"
      s `shouldSatisfy` isInfixOf "publish --remote origin"
      s `shouldSatisfy` isInfixOf "published practice-forum: yes"
      said (statusDoc (plain repo) { stPublished = Right [("up", Diverged "beef")] })
        `shouldSatisfy` isInfixOf "sync --remote up"

    it "says a clone with no remotes has nowhere it was published" $ do
      repo <- aKey
      said (statusDoc (plain repo) { stPublished = Right [] })
        `shouldSatisfy` isInfixOf "no git remote"
      said (statusDoc (plain repo) { stPublished = Left "not a git repository" })
        `shouldSatisfy` isInfixOf "not a git repository"

    it "says there is nothing to publish when nothing was folded here" $ do
      repo <- aKey
      said (statusDoc (plain repo) { stCanon = Nothing, stPublished = Right [] })
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
      said (statusDoc (plain repo) { stPublished = Right [("origin", RemoteSilent "\ESC[2Kgone")] })
        `shouldSatisfy` (not . isInfixOf "\ESC")

  describe "PEP-22 hub status: arguments" $ do

    it "takes the repository, and probes every remote unless one is named" $ do
      repo <- aKey
      statusArgs (argv ["--repo", b58 repo])
        `shouldBe` Just (StatusArgs repo Nothing False False)
      statusArgs (argv ["--repo", b58 repo, "--remote", "upstream"])
        `shouldBe` Just (StatusArgs repo (Just "upstream") False False)

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
      sentArgs (argv []) `shouldBe` Just (SentArgs Nothing False False)
      sentArgs (argv ["--repo", b58 repo]) `shouldBe` Just (SentArgs (Just repo) False False)
      sentArgs (argv ["--long"]) `shouldBe` Just (SentArgs Nothing True False)
      sentArgs (argv ["--long", "yes"]) `shouldBe` Nothing

    it "reads --json" $ do
      sentArgs (argv ["--json"]) `shouldBe` Just (SentArgs Nothing False True)
      sentArgs (argv ["--json", "--json"]) `shouldBe` Nothing

  describe "PEP-22 hub sent --json: the log as a document" $ do

    let one repo = Sent { seThread = mh "t", seEvent = mh "e", seMessage = mh "m"
                        , seRepo = Just repo, seAuthor = repo, seAt = 1000
                        , seWhat = "issue new", seTitle = Just "x\ESC[2Kfake"
                        }
        reply repo = (one repo) { seRepo = Nothing, seTitle = Nothing, seWhat = "comment" }

    it "says which document it is, and under which contract" $ do
      repo <- aKey
      let v = sentContract Nothing [one repo]
      at ["contract"] v `shouldBe` Just (Number 1)
      at ["document"] v `shouldBe` Just (String "sent")
      at ["filter_repo"] v `shouldBe` Just Null

    -- Whole identifiers, and the title exactly as written: the escape is a
    -- \u001b in the bytes, which no terminal interprets.
    it "carries whole identifiers and the title byte for byte" $ do
      repo <- aKey
      let [row] = rows (sentContract (Just repo) [one repo])
      at ["message"] row `shouldBe` Just (str (show (pretty (mh "m"))))
      at ["event"] row `shouldBe` Just (str (show (pretty (mh "e"))))
      at ["thread"] row `shouldBe` Just (str (show (pretty (mh "t"))))
      at ["repo"] row `shouldBe` Just (str (b58 repo))
      at ["handed_at"] row `shouldBe` Just (Number 1000)
      at ["title"] row `shouldBe` Just (String "x\ESC[2Kfake")
      LBS.unpack (Aeson.encode row) `shouldSatisfy` not . isInfixOf "\ESC"

    it "keeps every key on a row that names no repository" $ do
      repo <- aKey
      let [row] = rows (sentContract Nothing [reply repo])
      at ["repo"] row `shouldBe` Just Null
      at ["title"] row `shouldBe` Just Null
      at ["what"] row `shouldBe` Just (String "comment")

  describe "PEP-22 hub status --json: the standing as a document" $ do

    it "says which document it is, and carries the canon" $ do
      repo <- aKey
      let v = statusContract (plain repo)
      at ["document"] v `shouldBe` Just (String "status")
      at ["repo"] v `shouldBe` Just (str (b58 repo))
      at ["signing"] v `shouldBe` Just (Bool True)
      at ["canon","commit"] v `shouldBe` Just (String "cafe1234")
      at ["canon","threads"] v `shouldBe` Just (Number 3)

    -- The same question the text report's signing line answers: which key,
    -- and from where, not only whether.
    it "names the key that would sign and where it came from" $ do
      repo <- aKey ; deleg <- aKey
      let dflt = statusContract (plain repo)
          env = statusContract (plain repo) { stSigner = Right (Signer deleg FromEnv)
                                            , stMaintains = False }
          cfg = statusContract (plain repo) { stSigner = Right (Signer deleg FromGitConfig) }
          bad = statusContract (plain repo) { stSigner = Left (NotAKey FromEnv "nope") }
      at ["signer","key"] dflt `shouldBe` Just (str (b58 repo))
      at ["signer","source"] dflt `shouldBe` Just (String "repo_key")
      at ["signer","maintainer"] dflt `shouldBe` Just (Bool True)
      at ["signer","error"] dflt `shouldBe` Just Null
      at ["signer","key"] env `shouldBe` Just (str (b58 deleg))
      at ["signer","source"] env `shouldBe` Just (String "env")
      at ["signer","maintainer"] env `shouldBe` Just (Bool False)
      at ["signer","source"] cfg `shouldBe` Just (String "git_config")
      at ["signer","key"] bad `shouldBe` Just Null
      at ["signer","error"] bad `shouldSatisfy` \case
        Just (String e) -> "HBS2_HUB_AS" `Text.isInfixOf` e
        _ -> False

    it "names each remote and what it answered" $ do
      repo <- aKey
      let v = statusContract (plain repo)
                { stPublished = Right [ ("origin", Level), ("forum", Diverged "beef")
                                      , ("gone", RemoteSilent "no route") ] }
          Just (Array rs) = at ["published","remotes"] v
          states = [ (at ["name"] r, at ["state"] r) | r <- toList rs ]
      states `shouldBe` [ (Just "origin", Just "level"), (Just "forum", Just "diverged")
                        , (Just "gone", Just "silent") ]
      at ["published","error"] v `shouldBe` Just Null

    -- Nothing to have published is not "no remotes", and the two must not read
    -- alike.
    it "tells no canon from no remotes" $ do
      repo <- aKey
      at ["published"] (statusContract (plain repo) { stCanon = Nothing })
        `shouldBe` Just Null
      at ["published","remotes"] (statusContract (plain repo) { stPublished = Right [] })
        `shouldBe` Just (Array mempty)

    -- The absence the text report says out loud: a mailbox that would not read
    -- is a null count beside an error, never a zero.
    it "never reads a failed mailbox as nothing waiting" $ do
      repo <- aKey ; mbox <- aKey
      let failed = statusContract (plain repo) { stMailbox = Just mbox
                                               , stWaiting = Left "peer silent" }
          fine = statusContract (plain repo) { stMailbox = Just mbox, stWaiting = Right 4 }
      at ["mailbox","waiting"] failed `shouldBe` Just Null
      at ["mailbox","error"] failed `shouldBe` Just (String "peer silent")
      at ["mailbox","waiting"] fine `shouldBe` Just (Number 4)
      at ["mailbox","key"] fine `shouldBe` Just (str (b58 mbox))
      at ["mailbox","key"] (statusContract (plain repo)) `shouldBe` Just Null

    it "reads --json" $ do
      repo <- aKey
      statusArgs (argv ["--repo", b58 repo, "--json"])
        `shouldBe` Just (StatusArgs repo Nothing True False)

-- A path into a document.
at :: [Text.Text] -> Value -> Maybe Value
at [] v = Just v
at (k:ks) (Object o) = KM.lookup (Key.fromText k) o >>= at ks
at _ _ = Nothing

rows :: Value -> [Value]
rows v = case at ["letters"] v of
  Just (Array xs) -> toList xs
  _ -> []

str :: String -> Value
str = String . Text.pack
