module HBS2.Hub.RepoSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Canon
import HBS2.Hub.Fold
import HBS2.Hub.Repo

import HBS2.Net.Auth.Credentials

import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Data.IORef
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
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
  { csCommit  = pure (Right "deadbeef")
    -- UTF-8 bytes, because that is the unit git reports and the unit the bounds
    -- are in. Text.length counts characters, so a fixture measured that way is
    -- measured in a different unit from production.
  , csEntries = const (pure (Just [ TreeEntry p (Just (utf8Len t)) | (p,t) <- files ]))
  , csBlob    = \_ p -> pure (lookup p files)
  }
  where utf8Len = BS.length . Text.encodeUtf8

-- Read a tree that is expected to read.
readOk :: CanonSource IO -> RepoRef -> IO CanonState
readOk cs repo = readCanon cs repo >>= either (fail . show) pure

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

      st <- readOk (inMemory files) repo
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
      st <- readOk (inMemory files) repo
      fmap fst (stBad st) `shouldBe` [threadDir thr <> "/junk"]
      -- and the good file still folded
      frDropped (stFold st) `shouldBe` []
      HM.size (frThreads (stFold st)) `shouldBe` 1

    it "does not hand the version file or the index to the event reader" $ do
      -- Both are read by their own readers and would fail an event parse, so a
      -- reader that fed every path to one would report two corrupt files in
      -- every healthy tree.
      let entries = [ TreeEntry p (Just 10) | p <- ["version", numberIndexPath
                                                  , "repo/1-x", "threads/t/2-y"] ]
      fst (eventEntries entries) `shouldBe` ["repo/1-x", "threads/t/2-y"]

    it "answers an absent ref as absent, not as empty canon" $ do
      owner <- kp
      -- A repository whose canon has not been fetched, which is every clone
      -- until somebody asks for it: git's default refspec covers heads and tags
      -- only. Reporting an empty fold would tell a maintainer their tracker is
      -- empty when it is merely not here.
      let noRef = CanonSource (pure (Left NoCanonRef))
                    (const (pure (Just []))) (\_ _ -> pure Nothing)
      readCanon noRef (fst owner) >>= \r ->
        fmap (const ()) r `shouldBe` Left NoCanonRef

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
      st <- readOk (inMemory files) repo
      stVersion st `shouldBe` Nothing
      HM.size (frThreads (stFold st)) `shouldBe` 1

    it "folds repo-scope events alongside thread events" $ do
      owner <- kp
      bob <- kp
      alice <- kp
      -- delegate/revoke live under repo/, and a reader that walked only threads/
      -- would drop everything the delegate blessed. Ordering is not what this
      -- test is about, and an earlier version of this comment claimed it was:
      -- the fold sorts the whole set itself, so the only thing a reader has to
      -- get right is reading BOTH directories.
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
      st <- readOk (inMemory files) repo
      fmap drWhy (frDropped (stFold st)) `shouldBe` []
      HM.size (frThreads (stFold st)) `shouldBe` 1
      -- ...and the same tree without the repo/ file drops the open, which is what
      -- makes the assertion above about reading repo/ rather than about luck
      let onlyThread = [ f | f@(p,_) <- files, not (Text.isPrefixOf "repo/" (Text.pack p)) ]
      st' <- readOk (inMemory onlyThread) repo
      fmap drWhy (frDropped (stFold st')) `shouldBe` [UnauthorizedCanon]

    it "keeps the paths a caller has to act on" $ do
      owner <- kp
      -- A path the tree lists whose blob will not read. Different from a
      -- malformed file and reported all the same: the first is a broken read of
      -- something that exists, and the path is what `hbs2-peer download` takes.
      let repo = fst owner
          p = "threads/t/00000000000000000001-x"
          listed = CanonSource (pure (Right "deadbeef"))
                     (const (pure (Just [TreeEntry p (Just 10)])))
                     (\_ _ -> pure Nothing)
      st <- readOk listed repo
      stBad st `shouldBe` [(p, FileUnreadable)]

    it "refuses a tree it cannot list, rather than reporting empty canon" $ do
      owner <- kp
      -- A commit that is here and a tree that is not: a partial clone, a pruned
      -- object, a killed ls-tree. This used to answer an empty fold and a zero
      -- exit, which is the one answer indistinguishable from a tracker that has
      -- nothing in it.
      let noTree = CanonSource (pure (Right "deadbeef"))
                     (const (pure Nothing)) (\_ _ -> pure Nothing)
      readCanon noTree (fst owner)
        >>= \r -> fmap (const ()) r `shouldBe` Left (TreeUnreadable "deadbeef")

    it "will not fold a tree whose rules are newer than this build" $ do
      owner <- kp
      alice <- kp
      -- The tree version governs the admission rules, which is the whole reason
      -- it is a tree-level file. Folding it here anyway printed the version and
      -- then a drop list computed under v1 rules, and for the accept path that
      -- reuses this reader it would be minting into a view built from rules this
      -- build does not implement.
      let repo = fst owner
          e = mkEvent alice owner
                (AOpen repo HubIssue "an issue" [] Nothing Nothing Nothing 1000)
                (canon repo 1 (Just 1))
          thr = eventId e
          files = [ ("version", "(hub-meta " <> Text.pack (show (hubMetaVersion + 1)) <> ")\n")
                  , (threadDir thr <> "/" <> eventFileName 1 thr, renderEvent e)
                  ]
      readCanon (inMemory files) repo >>= \r ->
        fmap (const ()) r `shouldBe` Left (CanonTooNewHere (hubMetaVersion + 1))

    it "refuses a version file that is present and unreadable" $ do
      owner <- kp
      -- The one file in the tree whose unreadability nothing else can report: it
      -- is not an event, so it never reaches the fold, and swallowing the parse
      -- error made a corrupt stamp and an absent one the same observation.
      readCanon (inMemory [("version", "(hub-meta not-a-number)")]) (fst owner)
        >>= \r -> fmap (const ()) r `shouldBe` Left VersionUnreadable

    it "refuses an oversized blob from the listing, without fetching it" $ do
      owner <- kp
      fetched <- newIORef (0 :: Int)
      -- The size comes from the tree, so the bound bounds what a READER spends.
      -- Comparing after the fetch is comparing after paying: a tree of files just
      -- under the limit is inside every per-file bound and is gigabytes of text.
      let repo = fst owner
          huge = "threads/t/00000000000000000001-x"
          src = CanonSource (pure (Right "deadbeef"))
                  (const (pure (Just [TreeEntry huge (Just (maxEventBytes + 1))])))
                  (\_ p -> do modifyIORef fetched succ
                              pure (if p == "version" then Just renderMeta
                                                      else Just "whatever"))
      st <- readOk src repo
      stBad st `shouldBe` [(huge, FileTooLarge (maxEventBytes + 1))]
      -- Nothing was fetched at all. Not even the version file: it is looked up in
      -- the listing, which this tree does not have one in, so the reader does not
      -- ask for a blob whose size it has no way to know.
      readIORef fetched `shouldReturn` 0

