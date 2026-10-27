-- | @hub code@: a repository's code, as documents a page draws (PEP-25).
--
-- What GitHub showed a project that has left it: the branches and tags, a
-- directory, a file, the history, a commit with its diff, and the name and
-- description the manifest declares. Read from the clone a verb runs in, the
-- working clone or the server's cache one, through the same bounded runner as
-- every other git read here ('gitRun': a time limit, a cap on what is read,
-- git's environment hardened).
--
-- EVERYTHING IN A REPOSITORY IS A STRANGER'S. A ref and a path come from a
-- page, so both are checked before they reach git, and both reach it after
-- @--end-of-options@. A file's bytes are data in a JSON string, never a document
-- of their own, and a file over 'maxBlobBytes' is cut there and says so.
module HBS2.Hub.CLI.Code
  ( codeEntries
    -- * The parts that decide something
  , validCodeRef
  , validCodePath
  , readmeOf
  , treeEntries
  , commitsOf
  , maxBlobBytes
  , maxDiffBytes
  ) where

import HBS2.Hub.Types (RepoRef,validRefName,validAbbrevSha)
import HBS2.Hub.Repo.Git (gitRun,GitTrouble(..))
import HBS2.Hub.Repo.Manifest (readManifest)
import HBS2.Hub.CLI.Argv (badArgs,flagsAndSwitches,flagMaybe,flagText,flagWord,flagRepo,repoFlags)
import HBS2.Hub.CLI.Common (refuse)
import HBS2.Hub.Render (contractVersion,key58)

import HBS2.CLI.Prelude hiding (filter,map,mapMaybe,null,toList)
import HBS2.CLI.Run.Internal

import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import Data.Aeson (Value(..),object,(.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.List qualified as List
import Data.Maybe (fromMaybe,mapMaybe,listToMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import System.Exit (ExitCode(..))
import Text.Read (readMaybe)

-- | What one file may weigh on a page, and one diff. The native host's
-- messages are 1 MB at most, and a page is not where a vendored archive is read.
maxBlobBytes, maxDiffBytes :: Int
maxBlobBytes = 512 * 1024
maxDiffBytes = 512 * 1024

-- | A ref a page may name: a branch or a tag as git spells it, a sha, or
-- @HEAD@. Never an expression: @main~3@, @A..B@ and @:/text@ mean things a
-- link should not.
validCodeRef :: Text -> Bool
validCodeRef r = r == "HEAD" || validAbbrevSha r || (validRefName r && Text.all (`notElem` ("~^:@{" :: String)) r)

-- | A path inside a tree: plain names joined by @/@, none of them empty, @.@
-- or @..@, and no NUL. The empty path is the root.
validCodePath :: Text -> Bool
validCodePath p = Text.null p || (Text.length p <= 4096 && all ok (Text.splitOn "/" p))
  where ok s = not (Text.null s) && s /= "." && s /= ".." && Text.all (/= '\0') s

-- | The file a directory's page renders under its listing, as GitHub does.
readmeOf :: [Text] -> Maybe Text
readmeOf names = case List.sortOn rank (filter ((< 9) . rank) names) of
  (n:_) -> Just n
  [] -> Nothing
  where
    rank n = case map toLower (Text.unpack n) of
      "readme.md" -> 0 :: Int
      "readme.markdown" -> 1
      "readme" -> 2
      "readme.txt" -> 3
      "readme.rst" -> 4
      _ -> 9

-- | @git ls-tree -z -l@, as entries: directories first, then by name.
treeEntries :: BS.ByteString -> [Value]
treeEntries out =
  fmap snd $ List.sortOn fst
    [ ((if ty == "tree" then 0 else 1 :: Int, name), object
        [ "name" .= name, "type" .= ty, "mode" .= mode, "oid" .= oid
        , "size" .= (readMaybe (Text.unpack sz) :: Maybe Integer) ])
    | rec <- BS.split 0 out, not (BS.null rec)
    , let (meta, rest) = BS.break (== 9) rec
    , not (BS.null rest)
    , let name = Text.decodeUtf8Lenient (BS.drop 1 rest)
    , [mode, ty, oid, sz] <- [Text.words (Text.decodeUtf8Lenient meta)] ]

-- | @git log -z@ with 'logFormat', as commits. A record that does not have
-- every field is left out rather than shown wrong.
commitsOf :: BS.ByteString -> [Value]
commitsOf out = mapMaybe one (filter (not . BS.null) (BS.split 0 out))
  where
    one rec = case Text.splitOn "\x1f" (Text.decodeUtf8Lenient rec) of
      (oid : parents : an : ae : authored : ct : rest) -> Just $ object
        [ "oid" .= oid, "parents" .= Text.words parents
        , "author" .= object ["name" .= an, "email" .= ae]
        , "authored_at" .= ms authored, "committed_at" .= ms ct
        , "subject" .= Text.intercalate "\x1f" rest ]
      _ -> Nothing
    ms t = maybe Null (\s -> Number (fromInteger (s * 1000))) (readMaybe (Text.unpack t) :: Maybe Integer)

logFormat :: String
logFormat = "--format=%H%x1f%P%x1f%an%x1f%ae%x1f%at%x1f%ct%x1f%s"

data Code = Refs | Tree Text Text | Blob Text Text | Log Text Text Word64 Word64 | Show Text

codeEntries :: forall c m . ( IsContext c
                            , MonadUnliftIO m
                            , HasStorage m
                            , HasClientAPI LWWRefAPI UNIX m
                            , Exception (BadFormException c)
                            ) => MakeDictM c m ()
codeEntries = do
  verb "hub:code:refs" "a repository's branches and tags, and what its manifest calls it" [] []
    (\_ -> Just Refs)
  verb "hub:code:tree" "the entries of a directory at a ref" ["--ref","--path"] []
    (\kvs -> Tree <$> refOf kvs <*> pathOf kvs)
  verb "hub:code:blob" "a file at a ref, up to 512 KiB of it" ["--ref","--path"] []
    (\kvs -> Blob <$> refOf kvs <*> pathOf kvs)
  verb "hub:code:log" "the commits that reach a ref, or touched a path" ["--ref","--path","--skip","--limit"] []
    (\kvs -> Log <$> refOf kvs <*> pathOf kvs
               <*> (fromMaybe 0 <$> flagMaybe kvs "--skip" flagWord)
               <*> (min 100 . fromMaybe 30 <$> flagMaybe kvs "--limit" flagWord))
  verb "hub:code:show" "a commit: who, when, its message and its diff" ["--commit"] []
    (\kvs -> Show <$> (flagMaybe kvs "--commit" textOf >>= maybe Nothing Just))
  where
    textOf = fmap Text.pack . flagText
    refOf kvs = fromMaybe "HEAD" <$> flagMaybe kvs "--ref" textOf
    pathOf kvs = Text.dropWhileEnd (== '/') . Text.dropWhile (== '/') . fromMaybe "" <$> flagMaybe kvs "--path" textOf

    verb name what (flags :: [String]) (switches :: [String]) parse =
      brief what
        $ args (arg "string" "--repo repo-key" : fmap (\f -> arg "string" ("[" <> Text.pack f <> " value]")) flags
                <> [arg "string" "[--json]"])
        $ desc ( "Read-only, from this clone, as a JSON document (PEP-25)."
                 <> line <> "--ref is a branch, a tag, a sha or HEAD (the default);"
                 <> line <> "--path a path inside the tree, the root when left out." )
        $ entry $ bindMatch name $ nil_ $ \syn ->
            case flagsAndSwitches (repoFlags <> flags) ("--json" : switches) syn >>= \kvs ->
                   (,) <$> flagRepo asKey kvs <*> parse kvs of
              Just (repo, c) -> lift (runCode repo c)
              Nothing -> liftIO (badArgs (pretty ("usage: hbs2-hub " <> Text.unpack (Text.replace ":" " " (Text.drop 4 (idText name)))
                                                  <> " --repo <key> " <> unwords (fmap (\f -> "[" <> f <> " ...]") flags) :: String)) syn)

    asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    idText (Id t) = t

    emit v = liftIO (LBS.putStr (Aeson.encode v) >> putStrLn "")
    doc d repo fields = object (["contract" .= contractVersion, "document" .= (d :: Text), "repo" .= key58 repo] <> fields)

    checked r p = do
      unless (validCodeRef r) $ liftIO (refuse ("not a ref this can show: " <> Text.unpack (Text.take 80 r)) 2)
      unless (validCodePath p) $ liftIO (refuse ("not a path inside a tree: " <> Text.unpack (Text.take 200 p)) 2)

    git :: Text -> Int -> [String] -> m (Either GitTrouble (ExitCode, BS.ByteString, BS.ByteString))
    git what keep argv = gitRun Nothing [] 30 keep what argv mempty

    -- One answer, or the verb's refusal in git's words.
    must what keep argv = git what keep argv >>= \case
      Right (ExitSuccess, out, _) -> pure out
      Right (_, _, e0) -> liftIO (refuse (Text.unpack what <> ": " <> Text.unpack (Text.strip (Text.decodeUtf8Lenient e0))) 44)
      Left t -> liftIO (refuse (Text.unpack what <> ": " <> show t) 44)

    commitOf r = Text.strip . Text.decodeUtf8Lenient
      <$> must "the ref" 4096 ["rev-parse", "--verify", "--quiet", "--end-of-options", Text.unpack r <> "^{commit}"]

    runCode :: RepoRef -> Code -> m ()
    runCode repo = \case
      Refs -> do
        mf <- readManifest repo <&> either (const []) id
        let field f = headMay [ Text.pack s | ListVal [SymbolVal f', StringLike s] <- mf, f' == f ]
        hd <- git "HEAD" 4096 ["symbolic-ref", "-q", "--short", "HEAD"] <&> \case
          Right (ExitSuccess, out, _) -> Just (Text.strip (Text.decodeUtf8Lenient out))
          _ -> Nothing
        refs <- must "the refs" (4 * 1024 * 1024)
                  ["for-each-ref", "--format=%(refname)%00%(objectname)", "refs/heads", "refs/remotes", "refs/tags"]
        let pairs = [ (Text.decodeUtf8Lenient n, Text.decodeUtf8Lenient o)
                    | l <- BS.split 10 refs, let (n, o0) = BS.break (== 0) l, not (BS.null o0), let o = BS.drop 1 o0 ]
            branch n = case Text.stripPrefix "refs/heads/" n of
              Just b -> Just b
              Nothing -> case Text.splitOn "/" <$> Text.stripPrefix "refs/remotes/" n of
                Just (_remote : rest@(_:_)) | rest /= ["HEAD"] -> Just (Text.intercalate "/" rest)
                _ -> Nothing
            branches = List.nubBy (\a b -> fst a == fst b) [ (b, o) | (n, o) <- pairs, Just b <- [branch n] ]
            tags = [ (t, o) | (n, o) <- pairs, Just t <- [Text.stripPrefix "refs/tags/" n] ]
        emit $ doc "refs" repo
          [ "name" .= field "name", "description" .= field "description", "head" .= hd
          , "branches" .= [ object ["name" .= b, "commit" .= o] | (b, o) <- List.sortOn fst branches ]
          , "tags" .= [ object ["name" .= t, "commit" .= o] | (t, o) <- List.sortOn fst tags ] ]

      Tree r p -> do
        checked r p
        c <- commitOf r
        out <- must "the tree" (8 * 1024 * 1024)
                 ["ls-tree", "-z", "-l", "--end-of-options", Text.unpack c <> (if Text.null p then "^{tree}" else ":" <> Text.unpack p)]
        let es = treeEntries out
            names = [ n | Object o <- es, Just (String n) <- [KM.lookup "name" o], Just (String "blob") <- [KM.lookup "type" o] ]
        emit $ doc "tree" repo
          [ "ref" .= r, "commit" .= c, "path" .= p, "entries" .= es, "readme" .= readmeOf names ]

      Blob r p -> do
        checked r p
        when (Text.null p) $ liftIO (refuse "a file needs --path" 2)
        c <- commitOf r
        oid <- Text.strip . Text.decodeUtf8Lenient
                 <$> must "the file" 4096 ["rev-parse", "--verify", "--end-of-options", Text.unpack c <> ":" <> Text.unpack p]
        ty <- Text.strip . Text.decodeUtf8Lenient <$> must "the file" 64 ["cat-file", "-t", Text.unpack oid]
        unless (ty == "blob") $ liftIO (refuse (Text.unpack p <> " is a " <> Text.unpack ty <> ", not a file") 2)
        sz <- Text.strip . Text.decodeUtf8Lenient <$> must "the file" 64 ["cat-file", "-s", Text.unpack oid]
        (bytes, truncated) <- git "the file" maxBlobBytes ["cat-file", "blob", Text.unpack oid] >>= \case
          Right (ExitSuccess, out, _) -> pure (out, False)
          Left (GitTooMuch _ _ prefix) -> pure (prefix, True)
          Right (_, _, e0) -> liftIO (refuse ("the file: " <> Text.unpack (Text.decodeUtf8Lenient e0)) 44)
          Left t -> liftIO (refuse ("the file: " <> show t) 44)
        let binary = BS.elem 0 (BS.take 8000 bytes)
        emit $ doc "blob" repo
          [ "ref" .= r, "commit" .= c, "path" .= p, "oid" .= oid
          , "size" .= (readMaybe (Text.unpack sz) :: Maybe Integer)
          , "binary" .= binary, "truncated" .= truncated
          , "text" .= (if binary then Nothing else Just (Text.decodeUtf8Lenient bytes)) ]

      Log r p skip limit -> do
        checked r p
        c <- commitOf r
        out <- must "the history" (4 * 1024 * 1024)
                 ( ["log", "-z", "--no-color", logFormat, "--skip=" <> show skip, "--max-count=" <> show limit
                   , "--end-of-options", Text.unpack c] <> (if Text.null p then [] else ["--", Text.unpack p]) )
        emit $ doc "commits" repo
          [ "ref" .= r, "commit" .= c, "path" .= p, "skip" .= skip, "limit" .= limit, "commits" .= commitsOf out ]

      Show sha -> do
        unless (validAbbrevSha sha) $ liftIO (refuse "--commit is a sha" 2)
        c <- commitOf sha
        hdr <- must "the commit" (1024 * 1024)
                 ["log", "-1", "-z", "--no-color", "--format=%H%x1f%P%x1f%an%x1f%ae%x1f%at%x1f%cn%x1f%ce%x1f%ct%x1f%B", "--end-of-options", Text.unpack c]
        (diff, truncated) <- git "the diff" maxDiffBytes
            ["show", "--no-color", "--format=", "--patch", "--find-renames", "--diff-merges=first-parent", "--end-of-options", Text.unpack c] >>= \case
          Right (ExitSuccess, out, _) -> pure (out, False)
          Left (GitTooMuch _ _ prefix) -> pure (prefix, True)
          _ -> pure (mempty, False)
        let fields = Text.splitOn "\x1f" (Text.dropWhileEnd (== '\0') (Text.decodeUtf8Lenient hdr))
            field i = fromMaybe "" (listToMaybe (drop i fields))
            ms t = maybe Null (\s -> Number (fromInteger (s * 1000))) (readMaybe (Text.unpack t) :: Maybe Integer)
        emit $ doc "commit" repo
          [ "oid" .= field 0, "parents" .= Text.words (field 1)
          , "author" .= object ["name" .= field 2, "email" .= field 3], "authored_at" .= ms (field 4)
          , "committer" .= object ["name" .= field 5, "email" .= field 6], "committed_at" .= ms (field 7)
          , "message" .= Text.intercalate "\x1f" (drop 8 fields)
          , "diff" .= object ["text" .= Text.decodeUtf8Lenient diff, "truncated" .= truncated] ]
