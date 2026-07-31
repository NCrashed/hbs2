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
import Data.ByteString.Char8 qualified as B8
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

-- A path as bytes, the way git reports it.
--
-- NOT B8.pack, which truncates each character to one byte: a path with anything
-- above U+00FF became different bytes from the ones production would see, and the
-- multibyte test below is what caught it.
encPath :: FilePath -> BS.ByteString
encPath = Text.encodeUtf8 . Text.pack

-- A canon tree in memory: the same files a writer will put in git, served from
-- a list. This is why 'CanonSource' is a record of functions, not a git call.
--
-- The object id stands in for a path, exactly as it does in production: a blob is
-- fetched by id and a path is only a label. Here the id is the path's index, which
-- is enough to be distinct.
inMemory :: [(FilePath, Text)] -> CanonSource IO
inMemory files = CanonSource
  { csCommit  = pure (Right "deadbeef")
    -- UTF-8 bytes, because that is the unit git reports and the unit the bounds
    -- are in. Text.length counts characters, so a fixture measured that way is
    -- measured in a different unit from production.
  , csEntries = const (pure (Right
      [ TreeEntry (encPath p) (Blob (oidOf i) (utf8Len t))
      | (i,(p,t)) <- zip [0 :: Int ..] files ]))
  , csBlob    = \oid -> pure (maybe (BlobRefused "no such object") BlobText (lookup oid byOid))
  }
  where
    utf8Len = BS.length . Text.encodeUtf8
    oidOf i = Text.pack (show i)
    byOid = [ (oidOf i, t) | (i,(_,t)) <- zip [0 :: Int ..] files ]

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
      fmap fst (stBad st) `shouldBe` [encPath (threadDir thr <> "/junk")]
      -- and the good file still folded
      frDropped (stFold st) `shouldBe` []
      HM.size (frThreads (stFold st)) `shouldBe` 1

    it "does not hand the version file or the index to the event reader" $ do
      -- Both are read by their own readers and would fail an event parse, so a
      -- reader that fed every path to one would report two corrupt files in
      -- every healthy tree.
      let entries = [ TreeEntry (B8.pack p) (Blob "oid" 10)
                    | p <- ["version", numberIndexPath
                           , "repo/1-x", "threads/t/2-y"] ]
          (evs, bad) = sortCanon entries
      [ p | (p,_,_) <- evs ] `shouldBe` ["repo/1-x", "threads/t/2-y"]
      -- ...and neither of the two is reported as an intruder either: they have
      -- their own readers, and a report naming them would name two files in every
      -- healthy tree.
      bad `shouldBe` []

    it "answers an absent ref as absent, not as empty canon" $ do
      owner <- kp
      -- A repository whose canon has not been fetched, which is every clone
      -- until somebody asks for it: git's default refspec covers heads and tags
      -- only. Reporting an empty fold would tell a maintainer their tracker is
      -- empty when it is merely not here.
      let noRef = CanonSource (pure (Left NoCanonRef))
                    (const (pure (Right []))) (const (pure (BlobRefused "no such object")))
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
          p = B8.pack "threads/t/00000000000000000001-x"
          listed = CanonSource (pure (Right "deadbeef"))
                     (const (pure (Right [TreeEntry p (Blob "oid" 10)])))
                     (const (pure (BlobRefused "no such object")))
      st <- readOk listed repo
      stBad st `shouldBe` [(p, FileUnreadable "no such object")]

    it "refuses a tree it cannot list, rather than reporting empty canon" $ do
      owner <- kp
      -- A commit that is here and a tree that is not: a partial clone, a pruned
      -- object, a killed ls-tree. This used to answer an empty fold and a zero
      -- exit, which is the one answer indistinguishable from a tracker that has
      -- nothing in it.
      let noTree = CanonSource (pure (Right "deadbeef"))
                     (const (pure (Left (TreeUnreadable "gone")))) (const (pure (BlobRefused "no such object")))
      readCanon noTree (fst owner)
        >>= \r -> fmap (const ()) r `shouldBe` Left (TreeUnreadable "gone")

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
        >>= \r -> fmap (const ()) r
                    `shouldBe` Left (VersionUnreadable (FileMalformed (BadClause "hub-meta")))

    it "refuses an oversized blob from the listing, without fetching it" $ do
      owner <- kp
      fetched <- newIORef (0 :: Int)
      -- The size comes from the tree, so the bound bounds what a READER spends.
      -- Comparing after the fetch is comparing after paying: a tree of files just
      -- under the limit is inside every per-file bound and is gigabytes of text.
      let repo = fst owner
          huge = B8.pack "threads/t/00000000000000000001-x"
          src = CanonSource (pure (Right "deadbeef"))
                  (const (pure (Right
                    [TreeEntry huge (Blob "oid" (maxEventBytes + 1))])))
                  (\_ -> do modifyIORef fetched succ
                            pure (BlobText "whatever"))
      st <- readOk src repo
      stBad st `shouldBe` [(huge, FileTooLarge (maxEventBytes + 1))]
      -- Nothing was fetched at all. Not even the version file: it is looked up in
      -- the listing, which this tree does not have one in, so the reader does not
      -- ask for a blob whose size it has no way to know.
      readIORef fetched `shouldReturn` 0


    it "refuses an oversized version file without fetching it" $ do
      owner <- kp
      fetched <- newIORef (0 :: Int)
      -- The version file was the last blob read blind: fetched by name, so its
      -- size was unknown until it was resident. A 200 MB one peaked at 658 MiB
      -- before parseMeta said it was not a version.
      let big = CanonSource (pure (Right "deadbeef"))
                  (const (pure (Right
                    [TreeEntry versionPath (Blob "oid" (maxEventBytes + 1))])))
                  (\_ -> modifyIORef fetched succ >> pure (BlobText renderMeta))
      readCanon big (fst owner) >>= \r ->
        fmap (const ()) r
          `shouldBe` Left (VersionUnreadable (FileTooLarge (maxEventBytes + 1)))
      readIORef fetched `shouldReturn` 0

    it "tells an absent version file from one it could not read" $ do
      owner <- kp
      -- These were the same observation, and the difference decides whether the
      -- hub-meta gate runs at all: a pruned version blob read as "no version
      -- file", the tree was folded under this build's rules, and canon written
      -- under newer ones would have been folded in silence. The listing is what
      -- tells them apart.
      let listed = CanonSource (pure (Right "deadbeef"))
                     (const (pure (Right [TreeEntry versionPath (Blob "oid" 13)])))
                     (const (pure (BlobRefused "no such object")))
      readCanon listed (fst owner) >>= \r ->
        fmap (const ()) r `shouldBe` Left (VersionUnreadable (FileUnreadable "no such object"))
      -- ...and a tree that simply does not list one is read
      st <- readOk (inMemory []) (fst owner)
      stVersion st `shouldBe` Nothing

    it "refuses a tree past either bound, counting only the files it reads" $ do
      owner <- kp
      let ev n = TreeEntry (B8.pack ("threads/t/" <> show n))
                   (Blob (Text.pack (show n)) maxEventBytes)
          src es = CanonSource (pure (Right "deadbeef"))
                     (const (pure (Right es))) (const (pure (BlobText "x")))
          n = maxCanonBytes `div` maxEventBytes + 1
      readCanon (src (fmap ev [1..n])) (fst owner) >>= \r ->
        fmap (const ()) r `shouldBe` Left (CanonTooBig (n * maxEventBytes))
      -- A stranger's file next to canon does not count towards a bound over the
      -- files this reader fetches: it is reported, not fetched, so refusing the
      -- whole audit over its size would refuse an audit over something the audit
      -- does not read.
      let readme = TreeEntry "README" (Blob "r" maxCanonBytes)
      st <- readOk (src [readme]) (fst owner)
      stBad st `shouldBe` [("README", FileUnexpected)]

    it "counts the whole listing, not the files it would fetch" $ do
      owner <- kp
      -- The bytes bound and the count bound are over different things, because
      -- they bound different costs. Every entry in a listing is parsed, sorted and
      -- possibly printed whether or not this reader fetches it, and every path is
      -- a slice of one buffer, so holding one holds all: a tree of ten million
      -- paths outside the layout cost ten million lines and the whole buffer while
      -- a count over event files alone said it was empty.
      let junk n = TreeEntry (B8.pack ("junk/" <> show n)) (Blob "x" 1)
          n = maxCanonFiles + 1
          src es = CanonSource (pure (Right "deadbeef"))
                     (const (pure (Right es))) (const (pure (BlobText "x")))
      readCanon (src (fmap junk [1..n])) (fst owner) >>= \r ->
        fmap (const ()) r `shouldBe` Left (CanonTooMany n)

    it "carries each listing kind through to its own problem" $ do
      owner <- kp
      -- The composition, which is where the last round's bug lived: GitRepoSpec
      -- proves git's output becomes the right EntryKind, and the tests above
      -- prove readCanon folds a Blob, and between those two the three other kinds
      -- were unmentioned in either file. A missing object was reported as a
      -- submodule for a whole round with both halves passing.
      let src = CanonSource (pure (Right "deadbeef"))
                  (const (pure (Right
                    [ TreeEntry versionPath (Blob "v" 13)
                    , TreeEntry "threads/t/0001-gone" (BlobMissing "abc")
                    , TreeEntry "threads/t/0001-sub" NotABlob
                    , TreeEntry "100644 blob abc 5 extra" Unparsed
                      -- The index has its own reader and is skipped, but only when
                      -- it IS a blob: a gitlink here was the one entry in the tree
                      -- nothing looked at, because the skip was by path alone.
                    , TreeEntry (B8.pack numberIndexPath) NotABlob
                    ])))
                  (\oid -> pure (BlobText (if oid == "v" then renderMeta else "junk")))
      st <- readOk src (fst owner)
      stBad st `shouldBe`
        [ ("100644 blob abc 5 extra", FileListingUnparsed)
        , (B8.pack numberIndexPath, FileNotABlob)
        , ("threads/t/0001-gone", FileObjectMissing)
        , ("threads/t/0001-sub", FileNotABlob)
        ]

    it "refuses a tree newer than this build BEFORE reading its blobs" $ do
      owner <- kp
      fetched <- newIORef (0 :: Int)
      -- The gate lives inside foldCanon, which runs last, so the whole tree was
      -- read and then thrown away: measured at one cat-file per event. And a fork
      -- failing anywhere in that loop turned the answer into ReaderFailed, "nothing
      -- was learned about canon", when the version file had already answered.
      let newer = Text.pack "(hub-meta 99)\n"
          src = CanonSource (pure (Right "deadbeef"))
                  (const (pure (Right
                    ( TreeEntry versionPath (Blob "v" 14)
                    : [ TreeEntry (B8.pack ("threads/t/0000" <> show i <> "-x"))
                                  (Blob (Text.pack (show i)) 10)
                      | i <- [1 :: Int .. 5] ] ))))
                  (\oid -> do modifyIORef fetched succ
                              pure (BlobText (if oid == "v" then newer else "junk")))
      readCanon src (fst owner) >>= \r ->
        fmap (const ()) r `shouldBe` Left (CanonTooNewHere 99)
      -- One: the version file. Not six.
      readIORef fetched `shouldReturn` 1

    it "refuses version zero rather than folding it under today's rules" $ do
      owner <- kp
      -- The gate only ever looked for NEWER, so (hub-meta 0) folded silently under
      -- this build's rules and the report was clean. Zero is not a version any
      -- build implements.
      let src = CanonSource (pure (Right "deadbeef"))
                  (const (pure (Right [TreeEntry versionPath (Blob "v" 13)])))
                  (const (pure (BlobText "(hub-meta 0)\n")))
      readCanon src (fst owner) >>= \r ->
        fmap (const ()) r `shouldBe`
          Left (VersionUnreadable (FileMalformed (BadClause "hub-meta")))

    it "reads neither copy of a path listed twice with different objects" $ do
      owner <- kp
      -- Keeping the FIRST resolved the ambiguity, and resolving it is what this
      -- package refuses to do: with two different object ids, the order of entries
      -- in somebody else's tree chose which of two signed events the fold saw, and
      -- the other was never read, never folded and never named.
      fetched <- newIORef ([] :: [Text])
      let src = CanonSource (pure (Right "deadbeef"))
                  (const (pure (Right
                    [ TreeEntry versionPath (Blob "v" 13)
                    , TreeEntry "threads/t/0001-x" (Blob "a" 4)
                    , TreeEntry "threads/t/0001-x" (Blob "b" 4)
                    ])))
                  (\oid -> do modifyIORef fetched (oid:)
                              pure (BlobText (if oid == "v" then renderMeta else "junk")))
      st <- readOk src (fst owner)
      ("threads/t/0001-x", FileDuplicated) `elem` stBad st `shouldBe` True
      -- Neither object was read: not "a", which was first, and not "b".
      readIORef fetched `shouldReturn` ["v"]

    it "collapses a path listed twice with the SAME object" $ do
      owner <- kp
      -- The other half: identical entries are one entry, and are still named,
      -- because git fsck calls a duplicate an error whatever it holds.
      fetched <- newIORef (0 :: Int)
      let src = CanonSource (pure (Right "deadbeef"))
                  (const (pure (Right
                    [ TreeEntry versionPath (Blob "v" 13)
                    , TreeEntry "threads/t/0001-x" (Blob "a" 4)
                    , TreeEntry "threads/t/0001-x" (Blob "a" 4)
                    ])))
                  (\oid -> do modifyIORef fetched succ
                              pure (BlobText (if oid == "v" then renderMeta else "junk")))
      st <- readOk src (fst owner)
      ("threads/t/0001-x", FileDuplicated) `elem` stBad st `shouldBe` True
      -- The version file and the event once, not twice.
      readIORef fetched `shouldReturn` 2

    it "refuses a tree that lists the version file twice" $ do
      owner <- kp
      -- git fsck calls it duplicateEntries and a hand-built tree can carry it.
      -- Taking the head of the list let the ORDER of entries in somebody else's
      -- tree pick the rules canon is folded under, silently, exit 0.
      let src = CanonSource (pure (Right "deadbeef"))
                  (const (pure (Right
                    [ TreeEntry versionPath (Blob "a" 13)
                    , TreeEntry versionPath (Blob "b" 13)
                    ])))
                  (const (pure (BlobText renderMeta)))
      readCanon src (fst owner) >>= \r ->
        fmap (const ()) r `shouldBe` Left (VersionUnreadable FileDuplicated)

    it "reports a path under threads/ that is not a thread and a file" $ do
      owner <- kp
      -- A bare prefix match folded threads/x as an event and then reported it as
      -- malformed, where it is really a path the layout does not have. The depth
      -- is what tells those apart, and both directions matter.
      let src = CanonSource (pure (Right "deadbeef"))
                  (const (pure (Right
                    [ TreeEntry versionPath (Blob "v" 13)
                    , TreeEntry "threads/x" (Blob "a" 4)
                    , TreeEntry "threads/t/sub/0001-x" (Blob "b" 4)
                    , TreeEntry "repo/a/b" (Blob "c" 4)
                    ])))
                  (\oid -> pure (BlobText (if oid == "v" then renderMeta else "junk")))
      st <- readOk src (fst owner)
      stBad st `shouldBe`
        [ ("repo/a/b", FileUnexpected)
        , ("threads/t/sub/0001-x", FileUnexpected)
        , ("threads/x", FileUnexpected)
        ]

    it "says a version file is missing rather than only implying it" $ do
      owner <- kp
      -- PEP-19 requires the file. The tree still folds, under the oldest rules,
      -- because refusing would hand a veto to whoever deleted one unsigned line;
      -- but the absence is a finding, and it used to be a parenthesis on the
      -- header line and a zero exit.
      st <- readOk (inMemory []) (fst owner)
      stVersion st `shouldBe` Nothing

    it "says the tool failed rather than blaming the file it was reading" $ do
      owner <- kp
      -- A fork that fails under a pids limit, or on EMFILE, or git removed from
      -- PATH mid-audit. Reported as an unreadable FILE it exited 2, which is the
      -- code for an audit that ran and found something in somebody's canon.
      let src = CanonSource (pure (Right "deadbeef"))
                  (const (pure (Right [TreeEntry "threads/t/0001-x" (Blob "o" 5)])))
                  (const (pure (BlobUnavailable "fork: Resource exhausted")))
      readCanon src (fst owner) >>= \r ->
        fmap (const ()) r `shouldBe` Left (ReaderFailed "fork: Resource exhausted")

      -- And on the version file, where it used to exit 7 and tell whoever
      -- published canon to rewrite a file this reader never managed to look at.
      let onVersion = CanonSource (pure (Right "deadbeef"))
                        (const (pure (Right [TreeEntry versionPath (Blob "v" 13)])))
                        (const (pure (BlobUnavailable "fork: Resource exhausted")))
      readCanon onVersion (fst owner) >>= \r ->
        fmap (const ()) r `shouldBe` Left (ReaderFailed "fork: Resource exhausted")

    it "reports a path the tree layout does not have" $ do
      owner <- kp
      -- PEP-19 fixes the layout, so anything else is somebody's addition. A
      -- reader that looked only under threads/ and repo/ could not see it at all,
      -- which is the reader that lets a tree carry what it pretends not to see.
      let src = CanonSource (pure (Right "deadbeef"))
                  (const (pure (Right
                    [ TreeEntry versionPath (Blob "v" 13)
                    , TreeEntry "evil/plans" (Blob "e" 4)
                    , TreeEntry (B8.pack numberIndexPath) (Blob "i" 4)
                    ])))
                  (\oid -> pure (BlobText (if oid == "v" then renderMeta else "junk")))
      st <- readOk src (fst owner)
      stBad st `shouldBe` [("evil/plans", FileUnexpected)]

    it "keeps a multibyte path and reports the bytes, not the characters" $ do
      owner <- kp
      alice <- kp
      -- A path is bytes. The fixture measures UTF-8 like git does, and this is
      -- the one that would catch a return to counting characters: a thread name
      -- in Cyrillic is two bytes a letter.
      let repo = fst owner
          e = mkEvent alice owner
                (AOpen repo HubIssue "\1090\1077\1084\1072" [] Nothing Nothing Nothing 1000)
                (canon repo 1 (Just 1))
          thr = eventId e
          p = "threads/\1090\1077\1084\1072/" <> eventFileName 1 thr
      st <- readOk (inMemory [("version", renderMeta), (p, renderEvent e)]) repo
      -- read as an event, and its path came back as the bytes it is
      fmap fst (stFileVersions st) `shouldBe` [Text.encodeUtf8 (Text.pack p)]
      stBad st `shouldBe` []
