-- | @hbs2-hub serve@: the hub's pages and its tool table over HTTP (PEP-25).
--
-- The same calls the native host answers (see "HBS2.Hub.Host"), for a browser
-- with no extension: a project that has left GitHub. Two modes.
--
--   * LOCAL, on loopback, for the person at the machine, reading and
--     writing with their keys. Every @/api@ request carries the token this
--     run printed; the Host header must be the loopback address it listens
--     on, which is what stops a page on another site from reaching it through
--     a DNS name pointed at 127.0.0.1; a cross-origin POST is refused by its
--     Origin. No CORS header is ever sent, so no other origin reads an answer.
--   * PUBLIC, read-only, for anyone: the read tools only, the repositories
--     the operator named only, cache clones only, and a bounded number of
--     verbs at once.
--
-- The pages are files in a directory, served as they are; nothing in a
-- repository is ever served as a document of this origin.
module HBS2.Hub.Serve
  ( ServeConfig(..)
  , Mode(..)
  , serveHttp
    -- * The parts that decide something
  , contentTypeFor
  , assetPath
  , hostAllowed
  , originAllowed
  , sameToken
  , securityHeaders
  ) where

import HBS2.Hub.Host
import HBS2.Hub.Mcp (toolJson)

import Control.Concurrent.MVar
import Control.Exception qualified as E
import Data.Aeson (Value(..),object,(.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Bits (xor,(.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (doesFileExist)
import System.FilePath ((</>),takeExtension,joinPath)
import Data.String (fromString)
import System.Process.Typed (readProcess,proc)

data Mode = Local | Public
  deriving stock (Eq,Show)

data ServeConfig = ServeConfig
  { scHost  :: String        -- ^ the address to listen on
  , scPort  :: Int
  , scWeb   :: FilePath      -- ^ the built pages
  , scMode  :: Mode
  , scToken :: Text          -- ^ local mode: what every @/api@ request carries
  , scBusy  :: Int           -- ^ verbs at once; a request past it is told to wait
  }

-- | What every answer carries. The pages run nothing from elsewhere, are
-- framed by nothing, post nowhere, and are never sniffed into another type.
securityHeaders :: [Header]
securityHeaders =
  [ ("Content-Security-Policy", "default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; connect-src 'self'; object-src 'none'; base-uri 'none'; frame-ancestors 'none'; form-action 'none'")
  , ("X-Content-Type-Options", "nosniff")
  , ("X-Frame-Options", "DENY")
  , ("Referrer-Policy", "no-referrer")
  , ("Cross-Origin-Opener-Policy", "same-origin")
  , ("Cross-Origin-Resource-Policy", "same-origin") ]

-- | The file types the pages are made of, and nothing else: a name outside
-- this list is not served at all.
contentTypeFor :: FilePath -> Maybe BS.ByteString
contentTypeFor f = List.lookup (takeExtension f)
  [ (".html", "text/html; charset=utf-8"), (".js", "text/javascript; charset=utf-8")
  , (".css", "text/css; charset=utf-8"), (".png", "image/png"), (".svg", "image/svg+xml")
  , (".ico", "image/x-icon"), (".json", "application/json"), (".woff2", "font/woff2") ]

-- | A request path as a file under the pages, or Nothing. Every segment is a
-- plain name: no empty one, no dot first (which also refuses @..@), no
-- separator of either kind, no NUL.
assetPath :: [Text] -> Maybe FilePath
assetPath segs = case segs of
  [] -> Just "index.html"
  [""] -> Just "index.html"
  _ | all ok segs -> Just (joinPath (fmap Text.unpack segs))
    | otherwise -> Nothing
  where ok s = not (Text.null s) && Text.head s /= '.' && Text.all (`notElem` ("/\\\0" :: String)) s

-- | The Host header a loopback server answers to. Anything else is a page
-- that reached 127.0.0.1 through a name of its own (DNS rebinding).
hostAllowed :: Int -> Maybe BS.ByteString -> Bool
hostAllowed port = \case
  Just h -> h `elem` [ "127.0.0.1:" <> p, "localhost:" <> p ]
  Nothing -> False
  where p = B8.pack (show port)

-- | A POST from this origin, or from no browser at all.
originAllowed :: Int -> Maybe BS.ByteString -> Bool
originAllowed port = \case
  Nothing -> True
  Just o -> o `elem` [ "http://127.0.0.1:" <> p, "http://localhost:" <> p ]
  where p = B8.pack (show port)

-- | The token compared in time that does not depend on where it differs.
sameToken :: Text -> BS.ByteString -> Bool
sameToken want got =
  let w = Text.encodeUtf8 want
  in BS.length w == BS.length got && BS.foldl' (.|.) 0 (BS.packZipWith xor w got) == 0

serveHttp :: ServeConfig -> Hub -> IO ()
serveHttp sc hub = do
  running <- newMVar (0 :: Int)
  let settings = Warp.setHost (fromString (scHost sc)) (Warp.setPort (scPort sc) Warp.defaultSettings)
  Warp.runSettings settings (app running)
  where
    misdirected = mkStatus 421 "Misdirected Request"

    app running req respond = do
      let path = pathInfo req
          meth = requestMethod req
          local = scMode sc == Local
      if local && not (hostAllowed (scPort sc) (requestHeaderHost req))
        then respond (plain misdirected "this server answers to its loopback address only")
        else case path of
          ("api" : rest) -> api running req respond rest
          _ | meth /= methodGet -> respond (plain status405 "GET only")
            | otherwise -> static path respond

    api running req respond rest = do
      let local = scMode sc == Local
          token = lookup "X-Hub-Token" (requestHeaders req)
          authed = not local || maybe False (sameToken (scToken sc)) token
      let crossOrigin = requestMethod req == methodPost && local
                        && not (originAllowed (scPort sc) (lookup "Origin" (requestHeaders req)))
      if not authed then respond (json status401 (err "unauthorized" "this request does not carry the token this server printed"))
      else if crossOrigin then respond (json status403 (err "cross-origin" "a request from another origin"))
      else case (requestMethod req, rest) of
             ("GET", ["hello"]) -> do
               -- The repositories to show on the home page: what a public
               -- server was told to show, or the working clones the user named.
               repos <- if local then workingKeys else pure (maybe [] (List.sort . HS.toList) (hcAllow (hubConfig hub)))
               respond $ json status200 $ case hostHello hub of
                 Object h -> Object (KM.insert "mode" (String (if local then "local" else "public"))
                                       (KM.insert "repos" (Aeson.toJSON repos) h))
                 v -> v
             ("GET", ["tools"]) -> respond (json status200 (object ["tools" .= fmap toolJson (hubTools hub)]))
             ("POST", ["call"]) -> do
               body <- boundedBody req (1024 * 1024)
               case body >>= Aeson.decodeStrict of
                 Just (Object o)
                   | Just (String name) <- KM.lookup "tool" o
                   , args <- case KM.lookup "args" o of { Just (Object a) -> a ; _ -> KM.empty } ->
                       call running name args respond
                 _ -> respond (json status400 (err "bad-request" "a call is {\"tool\": ..., \"args\": {...}} and at most 1 MB"))
             _ -> respond (json status404 (err "not-found" "no such API"))

    -- Progress and then the answer, one JSON object a line, so a page can
    -- show what a long call is doing. Past the limit of verbs at once the
    -- request is told to wait rather than queued without bound.
    call running name args respond = do
      admitted <- modifyMVar running $ \n -> pure (if n < scBusy sc then (n + 1, True) else (n, False))
      if not admitted
        then respond (json status503 (err "busy" "too many calls at once; try again shortly"))
        else (`E.finally` modifyMVar_ running (pure . subtract 1)) $
          respond $ responseStream status200 (ndjson <> securityHeaders) $ \write flush -> do
            let line v = write (B.lazyByteString (Aeson.encode v) <> B.char7 '\n') >> flush
            r <- E.try (hubCall hub (\p -> line (object ["progress" .= p])) name args)
            case r of
              Right (Right v) -> line (object ["ok" .= True, "result" .= v])
              Right (Left e) -> line (object ["ok" .= False, "error" .= hostErrorJson e])
              Left (e :: E.SomeException) -> line (object ["ok" .= False, "error" .= hostErrorJson
                                                  (HostError Null "internal" (Text.pack (show e)))])

    static path respond = case assetPath path of
      Nothing -> respond (plain status404 "not found")
      Just rel -> case contentTypeFor rel of
        Nothing -> respond (plain status404 "not found")
        Just ct -> do
          let f = scWeb sc </> rel
          have <- doesFileExist f
          if have
            then respond (responseFile status200 (("Content-Type", ct) : securityHeaders) f Nothing)
            else respond (plain status404 "not found")

    workingKeys = do
      (_, out, _) <- readProcess (proc "git" ["config", "--global", "--name-only", "--get-regexp", "^hbs2-hub\\..*\\.clone$"])
      pure (List.sort [ k | l <- Text.lines (Text.decodeUtf8 (LBS.toStrict out))
                          , Just rest <- [Text.stripPrefix "hbs2-hub." l]
                          , Just k <- [Text.stripSuffix ".clone" rest] ])

    boundedBody req limit = go mempty 0
      where
        go acc n = do
          chunk <- getRequestBodyChunk req
          if BS.null chunk then pure (Just (LBS.toStrict (B.toLazyByteString acc)))
          else if n + BS.length chunk > limit then pure Nothing
          else go (acc <> B.byteString chunk) (n + BS.length chunk)

    ndjson = [("Content-Type", "application/x-ndjson"), ("Cache-Control", "no-store")]
    json st v = responseLBS st (("Content-Type", "application/json") : ("Cache-Control", "no-store") : securityHeaders) (Aeson.encode v)
    plain st t = responseLBS st (("Content-Type", "text/plain; charset=utf-8") : securityHeaders) (LBS.fromStrict (Text.encodeUtf8 t))
    err meaning msg = object ["ok" .= False, "error" .= object ["code" .= Null, "meaning" .= (meaning :: Text), "message" .= (msg :: Text)]]
