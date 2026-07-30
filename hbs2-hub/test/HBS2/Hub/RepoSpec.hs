module HBS2.Hub.RepoSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Canon
import HBS2.Hub.Fold
import HBS2.Hub.Repo

import HBS2.Net.Auth.Credentials

import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import Test.Hspec

type KP = (HubKey, PrivKey 'Sign HubScheme)

kp :: IO KP
kp = do
  c <- newCredentials @'HBS2Basic
  pure (_peerSignPk c, _peerSignSk c)

canon :: RepoRef -> Word64 -> Maybe Word64 -> EventId -> CanonContent
canon repo sq num eid = CanonContent repo eid sq num Nothing Nothing sq Nothing

-- A canon tree in memory: the same files a writer will put in git, served from
-- a list. This is why 'CanonSource' is a record of functions, not a git call.
inMemory :: [(FilePath, Text)] -> CanonSource IO
inMemory files = CanonSource
  { csCommit = pure (Just "deadbeef")
  , csPaths  = const (pure (fmap fst files))
  , csBlob   = \_ p -> pure (lookup p files)
  }

spec :: Spec
spec = do

  describe "PEP-19 canon tree" $ do

    it "reads a tree back into the fold that wrote it" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner
                    (AOpen repo HubIssue "an issue" [] (Just "body") Nothing Nothing 1000)
                    (canon repo 1 (Just 1))
          thr = eventId eOpen
          eCom = mkEvent alice owner (AComment thr Nothing (Just "a reply") Nothing 2000)
                   (canon repo 2 Nothing)
          files = [ ("version", renderMeta)
                  , (threadDir thr <> "/" <> eventFileName 1 thr, renderEvent eOpen)
                  , (threadDir thr <> "/" <> eventFileName 2 (eventId eCom), renderEvent eCom)
                  ]

      st <- readCanon (inMemory files) repo
      stVersion st `shouldBe` Just hubMetaVersion
      stBad st `shouldBe` []
      frDropped (stFold st) `shouldBe` []
      -- the thread came back with its reply, which is the whole round trip:
      -- render, tree, parse, fold
      fmap (length . tsComments) (HM.lookup thr (frThreads (stFold st)))
        `shouldBe` Just 1

    it "reports a file it cannot read instead of leaving it out" $ do
      owner <- kp
      alice <- kp
      let repo = fst owner
          eOpen = mkEvent alice owner
                    (AOpen repo HubIssue "an issue" [] Nothing Nothing Nothing 1000)
                    (canon repo 1 (Just 1))
          thr = eventId eOpen
          files = [ ("version", renderMeta)
                  , (threadDir thr <> "/" <> eventFileName 1 thr, renderEvent eOpen)
                    -- Somebody put a file under threads/. Skipping it silently
                    -- would let a tree carry events a reader pretends not to
                    -- see; the fold cannot mention it, since it never became an
                    -- event, so this is the only place it can be reported.
                  , (threadDir thr <> "/junk", "not an event at all")
                  ]
      st <- readCanon (inMemory files) repo
      fmap fst (stBad st) `shouldBe` [threadDir thr <> "/junk"]
      -- and the good file still folded
      frDropped (stFold st) `shouldBe` []
      HM.size (frThreads (stFold st)) `shouldBe` 1

    it "does not hand the version file or the index to the event reader" $ do
      -- Both are read by their own readers and would fail an event parse, so a
      -- reader that fed every path to one would report two corrupt files in
      -- every healthy tree.
      eventPaths ["version", numberIndexPath, "repo/1-x", "threads/t/2-y"]
        `shouldBe` ["repo/1-x", "threads/t/2-y"]

    it "answers an absent ref as absent, not as empty canon" $ do
      owner <- kp
      -- A repository whose canon has not been fetched, which is every clone
      -- until somebody asks for it: git's default refspec covers heads and tags
      -- only. Reporting an empty fold would tell a maintainer their tracker is
      -- empty when it is merely not here.
      let noRef = CanonSource (pure Nothing) (const (pure [])) (\_ _ -> pure Nothing)
      st <- readCanon noRef (fst owner)
      stCommit st `shouldBe` Nothing
      -- and the fold it carries is the empty one, so a caller that ignores the
      -- commit gets a defined answer rather than a crash
      frDropped (stFold st) `shouldBe` []

    it "reads a tree with no version file, and says the version is missing" $ do
      owner <- kp
      alice <- kp
      -- The tree version DOES govern (unlike a file's own), so a tree without
      -- one is a tree no writer here made. It is still read: refusing would hand
      -- a veto to whoever deleted one line, which is the same argument the file
      -- version already lost.
      let repo = fst owner
          e = mkEvent alice owner
                (AOpen repo HubIssue "an issue" [] Nothing Nothing Nothing 1000)
                (canon repo 1 (Just 1))
          thr = eventId e
      let files = [(threadDir thr <> "/" <> eventFileName 1 thr, renderEvent e)]
      st <- readCanon (inMemory files) repo
      stVersion st `shouldBe` Nothing
      HM.size (frThreads (stFold st)) `shouldBe` 1

    it "folds repo-scope events alongside thread events" $ do
      owner <- kp
      bob <- kp
      alice <- kp
      -- delegate/revoke live under repo/ and must be seen in seq order with the
      -- thread events, or the maintainer set is wrong at the events between
      -- them. A reader that only walked threads/ would drop everything bob
      -- blessed.
      let repo = fst owner
          deleg = mkEvent owner owner (ADelegate repo (fst bob) 1) (canon repo 1 Nothing)
          eOpen = mkEvent alice bob
                    (AOpen repo HubIssue "by the delegate" [] Nothing Nothing Nothing 2000)
                    (canon repo 2 (Just 1))
          thr = eventId eOpen
          files = [ ("version", renderMeta)
                  , (repoDir <> "/" <> eventFileName 1 (eventId deleg), renderEvent deleg)
                  , (threadDir thr <> "/" <> eventFileName 2 thr, renderEvent eOpen)
                  ]
      st <- readCanon (inMemory files) repo
      fmap drWhy (frDropped (stFold st)) `shouldBe` []
      HM.size (frThreads (stFold st)) `shouldBe` 1
      -- ...and the same tree read without the repo/ file drops the open, which
      -- is what makes the assertion above about ordering and not about luck
      let onlyThread = [ f | f@(p,_) <- files, not (Text.isPrefixOf "repo/" (Text.pack p)) ]
      st' <- readCanon (inMemory onlyThread) repo
      fmap drWhy (frDropped (stFold st')) `shouldBe` [UnauthorizedCanon]

    it "keeps the paths a caller has to act on" $ do
      owner <- kp
      -- A path the tree lists whose blob will not read. Different from a
      -- malformed file and reported all the same: the first is a broken read of
      -- something that exists, and the path is what `hbs2-peer download` takes.
      let repo = fst owner
          listed = CanonSource (pure (Just "deadbeef"))
                     (const (pure ["threads/t/00000000000000000001-x"]))
                     (\_ _ -> pure Nothing)
      st <- readCanon listed repo
      fmap fst (stBad st) `shouldBe` ["threads/t/00000000000000000001-x"]

