-- | The PEP-22 render contract: the JSON a web layer reads.
--
-- This is a WIRE FORMAT, so the assertions are on the bytes and on the field
-- names. A projection that is correct as a value and spells a key differently
-- is a projection no renderer can read, and a version number that does not move
-- when a field changes meaning is worse than no version number.
--
-- The one that is not merely a schema check is the redaction case. PEP-22 says
-- a redacted item renders as withheld, and this withholds it in the CONTRACT
-- rather than marking it and trusting whoever prints it: a body shipped beside
-- a boolean has been published to every renderer that forgets to read the
-- boolean, which is the whole failure a redaction exists to prevent.
module HBS2.Hub.RenderSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Fold (ThreadState(..),Comment(..),PRState(..))
import HBS2.Hub.Render

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Hash (hashObject)
import HBS2.Net.Auth.Credentials
import HBS2.Prelude.Plated (pretty,fromString)

import Data.Aeson (Value)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.HashMap.Strict qualified as HM
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Test.Hspec

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

mh :: BS.ByteString -> HashRef
mh = HashRef . hashObject

b58 :: HubKey -> Text
b58 = fromString . show . pretty . AsBase58

-- A thread with everything filled in, so a test can take one field away.
aThread :: HubKey -> HubKey -> ThreadState
aThread author canonBy = ThreadState
  { tsId       = mh "thread"
  , tsKind     = HubIssue
  , tsNumber   = Just 42
  , tsAuthor   = author
  , tsCanonBy  = canonBy
  , tsAuthorTs = 1737763100000
  , tsCreated  = 1737763200000
  , tsUpdated  = 1737849600000
  , tsAttrs    = HM.fromList [ ("title","the tests hang")
                             , ("status","open")
                             , ("labels", encodeLabels ["bug","aarch64"])
                             , ("assignees", encodeLabels ["alice"]) ]
  , tsComments = []
  , tsRedacted = False
  , tsPR       = Nothing
  , tsLabelsRequested = ["ui"]
  , tsBody       = Just "it hangs"
  , tsBodyPart   = Just (mh "body")
  , tsPartSecret = mkPartSecret (BS.replicate 32 7)
  , tsOrigin     = Just (mh "letter")
  }

-- Coordinates with every field filled in, so a redaction case can look for
-- each one by name rather than by whichever the fixture happened to set.
aCoords :: PRCoords
aCoords = PRCoords
  { prSource    = Just "hbs23://fork"
  , prSourceRef = "refs/heads/topic"
  , prSourceTip = "cafe"
  , prOnto      = "refs/heads/master"
  , prBase      = "beef"
  , prBundle    = Just (PartRef (mh "bundle") (PartProof (mh "proof")))
  }

-- A pull request whose own secret is the thread's, which is what the fold
-- produces for an opening event: both come from `ccPartSecret` of one canon
-- box. The two used to be withheld by different rules, and that is what made
-- one document carry the same 32 bytes twice.
aPR :: HubKey -> HubKey -> PRState
aPR author canonBy =
  PRState aCoords (mkPartSecret (BS.replicate 32 7)) Nothing author canonBy

aComment :: HubKey -> HubKey -> Comment
aComment author canonBy = Comment
  { cId         = mh "comment"
  , cAuthor     = author
  , cCanonBy    = canonBy
  , cReplyTo    = Just (mh "thread")
  , cAuthorTs   = 1737766700000
  , cFoldedTs   = 1737766800000
  , cBody       = Just "me too"
  , cBodyPart   = Just (mh "cbody")
  , cPartSecret = mkPartSecret (BS.replicate 32 9)
  , cOrigin     = Just (mh "cletter")
  , cRedacted   = False
  }

-- Read one field back out of the object, so a test names what it means.
at :: Value -> Text -> Maybe Value
at (A.Object o) k = KM.lookup (fromString (show (pretty k))) o
at _ _ = Nothing

field :: Value -> Text -> Value
field v k = fromMaybe A.Null (at v k)

spec :: Spec
spec = do

  describe "PEP-22 render contract: the shape" $ do

    it "carries the version a renderer checks first" $ do
      k <- aKey
      field (threadContract (aThread k k) Nothing) "contract"
        `shouldBe` A.Number 1

    it "names the kind the way the contract spells it" $ do
      k <- aKey
      field (threadContract (aThread k k) Nothing) "kind" `shouldBe` A.String "issue"
      let pr = (aThread k k) { tsKind = HubPR }
      field (threadContract pr Nothing) "kind" `shouldBe` A.String "pr"

    it "splits the multi-valued attributes into arrays" $ do
      -- Canon stores labels as one encoded value, and a renderer must not be
      -- made to parse that: it is this project's own encoding and nothing
      -- outside it knows the rules.
      --
      -- SORTED, and that is canon's doing rather than this module's: a set has
      -- a canonical rendering ('normalizeAttr') so that two writers who applied
      -- the same labels in a different order produce the same attribute value
      -- and not two events that disagree. The contract carries what canon
      -- holds, which is the sorted set.
      k <- aKey
      field (threadContract (aThread k k) Nothing) "labels"
        `shouldBe` A.toJSON (["aarch64","bug"] :: [Text])
      field (threadContract (aThread k k) Nothing) "assignees"
        `shouldBe` A.toJSON (["alice"] :: [Text])

    it "keeps what the author ASKED for apart from what an owner applied" $ do
      -- Merging the two would let a stranger label their own issue, which is
      -- the reason PEP-19 makes applying one an owner-signed event.
      k <- aKey
      field (threadContract (aThread k k) Nothing) "labels_requested"
        `shouldBe` A.toJSON (["ui"] :: [Text])

    it "carries provenance per item, not per thread" $ do
      -- Under delegation the events of one thread are blessed by different
      -- keys, so a single thread-level canon_by would be lossy.
      author <- aKey
      opener <- aKey
      later  <- aKey
      let t = (aThread author opener) { tsComments = [aComment author later] }
          v = threadContract t Nothing
      field v "author"   `shouldBe` A.String (b58 author)
      field v "canon_by" `shouldBe` A.String (b58 opener)
      case field v "comments" of
        A.Array cs | [c] <- foldr (:) [] cs -> do
          field c "author"   `shouldBe` A.String (b58 author)
          field c "canon_by" `shouldBe` A.String (b58 later)
        x -> expectationFailure ("expected one comment, got " <> show x)

    it "has no verified flag, and must not grow one" $ do
      -- Everything here passed the PEP-19 admission check: a bad signature or
      -- an unauthorized canon key is dropped by the fold and never
      -- materialized, so presence IS verification. A flag would imply the
      -- contract can carry something unverified, which it cannot.
      k <- aKey
      at (threadContract (aThread k k) Nothing) "verified" `shouldBe` Nothing

    it "carries both clocks, and says which is which by name" $ do
      k <- aKey
      let v = threadContract (aThread k k) Nothing
      field v "created_at"  `shouldBe` A.Number 1737763200000
      field v "declared_at" `shouldBe` A.Number 1737763100000
      field v "updated_at"  `shouldBe` A.Number 1737849600000

  describe "PEP-22 render contract: a redacted item is withheld" $ do

    it "withholds a redacted thread's title, body and attachment" $ do
      k <- aKey
      let v = threadContract (aThread k k) { tsRedacted = True } Nothing
      field v "redacted"    `shouldBe` A.Bool True
      field v "title"       `shouldBe` A.Null
      field v "body"        `shouldBe` A.Null
      field v "body_part"   `shouldBe` A.Null
      field v "part_secret" `shouldBe` A.Null

    it "withholds a redacted comment's body and leaves the rest of the thread" $ do
      k <- aKey
      let t = (aThread k k) { tsComments = [ (aComment k k) { cRedacted = True } ] }
          v = threadContract t Nothing
      field v "body" `shouldBe` A.String "it hangs"
      case field v "comments" of
        A.Array cs | [c] <- foldr (:) [] cs -> do
          field c "redacted"    `shouldBe` A.Bool True
          field c "body"        `shouldBe` A.Null
          field c "body_part"   `shouldBe` A.Null
          field c "part_secret" `shouldBe` A.Null
          -- Withheld, not removed: a renderer still says something was here.
          field c "event_id"    `shouldBe` A.String (fromString (show (pretty (mh "comment"))))
        x -> expectationFailure ("expected one comment, got " <> show x)

    it "does not put the withheld text anywhere else in the bytes" $ do
      -- The assertion that survives a refactor: whatever the shape becomes, a
      -- redacted body must not be IN the file.
      k <- aKey
      let t = (aThread k k) { tsRedacted = True
                            , tsBody = Just "the-secret-text"
                            , tsAttrs = HM.insert "title" "the-secret-title"
                                          (tsAttrs (aThread k k)) }
          out = LBS8.unpack (renderContract (threadContract t Nothing))
      out `shouldSatisfy` (not . isInfixOf "the-secret-text")
      out `shouldSatisfy` (not . isInfixOf "the-secret-title")

    -- THE FIXTURE THIS TEST DID NOT HAVE. The three cases above ran against a
    -- thread with `tsPR = Nothing`, so the whole pull-request half of the
    -- document went unasserted -- and that half took no redaction argument at
    -- all. For a PR's opening event the two secrets are one value (the fold
    -- fills both from `ccPartSecret` of the same canon box), so a document said
    -- "part_secret": null and then printed the same 32 bytes two lines later.
    it "withholds a redacted pull request's secret and coordinates" $ do
      k <- aKey
      let t = (aThread k k) { tsRedacted = True, tsKind = HubPR
                            , tsPR = Just (aPR k k) }
          v = threadContract t Nothing
          pr = field v "pr"
      field v "part_secret" `shouldBe` A.Null
      -- The one that made this sharp: the SAME bytes the thread just withheld.
      field pr "part_secret" `shouldBe` A.Null
      field pr "onto"        `shouldBe` A.Null
      field pr "base"        `shouldBe` A.Null
      field pr "tip"         `shouldBe` A.Null
      field pr "source_ref"  `shouldBe` A.Null
      field pr "source"      `shouldBe` A.Null
      -- The hash stays: it is not text somebody wrote, and a renderer offering
      -- to rebuild a withdrawn proposal still has to say there was one.
      field pr "bundle_part" `shouldSatisfy` (/= A.Null)

    it "withholds the labels an author asked for" $ do
      k <- aKey
      let t = (aThread k k) { tsRedacted = True
                            , tsLabelsRequested = ["the-secret-label"] }
          out = LBS8.unpack (renderContract (threadContract t Nothing))
      field (threadContract t Nothing) "labels_requested" `shouldBe` A.Array mempty
      out `shouldSatisfy` (not . isInfixOf "the-secret-label")

    -- And the same assertion over the PR half, in bytes, which is what
    -- survives whatever the shape becomes.
    it "does not put a redacted proposal's text anywhere in the bytes" $ do
      k <- aKey
      let co = (aCoords) { prOnto = "the-secret-branch", prSourceRef = "the-secret-ref" }
          t = (aThread k k) { tsRedacted = True, tsKind = HubPR
                            , tsPR = Just (aPR k k) { psCoords = co } }
          out = LBS8.unpack (renderContract (threadContract t Nothing))
      out `shouldSatisfy` (not . isInfixOf "the-secret-branch")
      out `shouldSatisfy` (not . isInfixOf "the-secret-ref")

  describe "PEP-22 render contract: the bytes" $ do

    it "escapes a control character instead of emitting it" $ do
      -- A title is a stranger's text and travels verbatim through canon. JSON
      -- gives the right answer for free, and this pins it: the ESC survives as
      -- an escape and no terminal ever sees one.
      k <- aKey
      let t = (aThread k k) { tsAttrs = HM.insert "title" "a\ESC[2Jb"
                                          (tsAttrs (aThread k k)) }
          out = LBS8.unpack (renderContract (threadContract t Nothing))
      out `shouldSatisfy` isInfixOf "\\u001b"
      out `shouldSatisfy` (not . isInfixOf "\ESC")

    it "puts the fields in a fixed order, so two clones write one file" $ do
      k <- aKey
      let out = LBS8.unpack (renderContract (threadContract (aThread k k) Nothing))
      -- Not the hash order aeson would choose, which is neither stable across
      -- versions nor readable.
      out `shouldSatisfy` \s ->
        maybe False id (do i <- idx "\"contract\"" s
                           j <- idx "\"kind\"" s
                           l <- idx "\"comments\"" s
                           pure (i < j && j < l))

    it "ends with a newline, so a file is a line-oriented tool's file too" $ do
      k <- aKey
      LBS8.unpack (renderContract (threadContract (aThread k k) Nothing))
        `shouldSatisfy` \s -> not (null s) && last s == '\n'

  describe "PEP-22 render contract: a pull request" $ do

    it "carries the coordinates and who supplied them" $ do
      author <- aKey
      reviser <- aKey
      canonBy <- aKey
      let co = PRCoords { prSource = Just "hbs23://x", prSourceRef = "refs/heads/topic"
                        , prSourceTip = "cafe", prOnto = "refs/heads/master"
                        , prBase = "beef", prBundle = Nothing }
          t = (aThread author canonBy)
                { tsKind = HubPR
                , tsPR = Just (PRState co Nothing (Just ("dead","refs/heads/master"))
                                       reviser canonBy) }
          v = field (threadContract t Nothing) "pr"
      field v "onto"            `shouldBe` A.String "refs/heads/master"
      field v "base"            `shouldBe` A.String "beef"
      field v "tip"             `shouldBe` A.String "cafe"
      field v "source"          `shouldBe` A.String "hbs23://x"
      field v "source_ref"      `shouldBe` A.String "refs/heads/topic"
      field v "merge_commit"    `shouldBe` A.String "dead"
      -- Not the thread's author: PEP-19 lets a maintainer revise a pull
      -- request, so a thread attributed to one person can point at a tip
      -- another one chose.
      field v "coords_author"   `shouldBe` A.String (b58 reviser)
      field v "coords_canon_by" `shouldBe` A.String (b58 canonBy)

    it "says the diff is unavailable when nobody looked, rather than omitting it" $ do
      -- A missing key and a key saying nothing was found are different facts,
      -- and a renderer can only act on the second.
      k <- aKey
      let co = PRCoords Nothing "refs/heads/t" "cafe" "refs/heads/master" "beef" Nothing
          t = (aThread k k) { tsKind = HubPR
                            , tsPR = Just (PRState co Nothing Nothing k k) }
          d = field (field (threadContract t Nothing) "pr") "diff"
      field d "availability" `shouldBe` A.String "unavailable"
      field d "truncated"    `shouldBe` A.Bool False

    it "reports a diff the caller did compute" $ do
      k <- aKey
      let co = PRCoords Nothing "refs/heads/t" "cafe" "refs/heads/master" "beef" Nothing
          t = (aThread k k) { tsKind = HubPR
                            , tsPR = Just (PRState co Nothing Nothing k k) }
          d = field (field (threadContract t (Just (PRDiff DiffAvailable True "@@")))
                       "pr") "diff"
      field d "availability" `shouldBe` A.String "available"
      field d "truncated"    `shouldBe` A.Bool True
      field d "text"         `shouldBe` A.String "@@"

    it "has no pr object on an issue" $ do
      k <- aKey
      at (threadContract (aThread k k) Nothing) "pr" `shouldBe` Nothing

idx :: String -> String -> Maybe Int
idx needle hay = go 0 hay
  where
    go _ [] = Nothing
    go n s@(_:rest) | needle `isPrefixOf'` s = Just n
                    | otherwise = go (n+1) rest
    isPrefixOf' a b = take (length a) b == a
