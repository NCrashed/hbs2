-- | The argument reading of the owner-native verbs (PEP-22 "Maintain").
--
-- Its own module for the reason @hub pr new@'s is: these verbs sign, and what
-- they sign is published into append-only canon. Two of their values are keys
-- of the same thirty-two bytes of base58 (the repository and a delegate), and
-- one is a number that names a thread nobody can rename afterwards.
module HBS2.Hub.OwnSpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.CLI.Own
import HBS2.Hub.CLI.Argv (argvAtom)
import HBS2.Hub.CLI.Common (Writing(..))

import HBS2.Net.Auth.Credentials
import HBS2.Base58 (AsBase58(..))
import HBS2.Hash (hashObject)
import HBS2.Data.Types.Refs (HashRef(..))

import HBS2.Hub.Fold (foldEvents,FoldResult(..),ThreadState(..),Comment(..))
import HBS2.Hub.Render (threadContract)
import HBS2.Hub.Bridge (ownerEvent,TriageCtx(..),TriageError(..),Accepted(..),viewOf,noOwnAttachments,eventPath)
import HBS2.Hub.Repo (CanonSource(..),TreeEntry(..),EntryKind(..),BlobResult(..),readCanon,stFold)
import HBS2.Hub.Canon (renderMeta,metaAt,renderEvent)
import HBS2.Hub.CLI.Verify (reportCode)
import HBS2.Hub.CLI.Read (showDoc)
import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as TextE
import Data.List (isInfixOf)

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Config.Suckless
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Maybe (fromMaybe,isJust)
import Prettyprinter (pretty)
import Test.Hspec

aKey :: IO HubKey
aKey = _peerSignPk <$> newCredentials @'HBS2Basic

b58 :: HubKey -> String
b58 = show . pretty . AsBase58

argv :: [String] -> [Syntax C]
argv = fmap argvAtom

spec :: Spec
spec = crossing >> replying >> do

  describe "PEP-22 hub issue close|reopen|label: arguments" $ do

    it "reads a complete call, in any order" $ do
      repo <- aKey
      let want = Just (OwnArgs repo 7 (Just "done") [] False Nothing Nothing ForReal)
      ownArgs (argv ["--repo", b58 repo, "--number", "7", "--note", "done"])
        `shouldBe` want
      ownArgs (argv ["--note", "done", "--number", "7", "--repo", b58 repo])
        `shouldBe` want

    it "takes a delegate's key when one is named" $ do
      repo <- aKey ; bob <- aKey
      fmap owAs (ownArgs (argv [ "--repo", b58 repo, "--number", "1"
                               , "--as", b58 bob ]))
        `shouldBe` Just (Just bob)

    it "refuses a call with no repository or no number" $ do
      repo <- aKey
      ownArgs (argv ["--number", "7"]) `shouldBe` Nothing
      ownArgs (argv ["--repo", b58 repo]) `shouldBe` Nothing

    -- A number is a Word64 in canon, and the guard against a negative is not
    -- the only end that matters: this used to be the shape that wrapped.
    it "refuses a number that is not one" $ do
      repo <- aKey
      let with n = ownArgs (argv ["--repo", b58 repo, "--number", n])
      with "-1" `shouldBe` Nothing
      with "seven" `shouldBe` Nothing
      with "18446744073709551617" `shouldBe` Nothing

    it "takes labels, as many as are given, in the order given" $ do
      repo <- aKey
      fmap owLabels (ownArgs (argv [ "--repo", b58 repo, "--number", "1"
                                   , "--label", "bug", "--label", "ui" ]))
        `shouldBe` Just ["bug","ui"]

    -- The labels REPLACE the set, so an omitted --label would be a verb that
    -- clears them because somebody forgot an argument. --clear says it.
    it "tells an empty label set from an absent one" $ do
      repo <- aKey
      let base = ["--repo", b58 repo, "--number", "1"]
      fmap owClear (ownArgs (argv base)) `shouldBe` Just False
      fmap owClear (ownArgs (argv (base <> ["--clear"]))) `shouldBe` Just True
      -- A switch takes no value: given one, the line does not parse rather than
      -- the value being swallowed as something else's.
      ownArgs (argv (base <> ["--clear", "yes"])) `shouldBe` Nothing

    it "refuses a flag it does not know instead of dropping it" $ do
      repo <- aKey
      ownArgs (argv ["--repo", b58 repo, "--number", "1", "--rehearse"])
        `shouldBe` Nothing
      ownArgs (argv ["--repo", b58 repo, "--number", "1", "--nete", "x"])
        `shouldBe` Nothing

    -- It is a flag this verb HAS now, and this used to be the case above: it
    -- was dropped on the floor, so an owner-signed event went into append-only
    -- canon from a command whose author believed it would not.
    it "rehearses when asked to" $ do
      repo <- aKey
      fmap owDry (ownArgs (argv ["--repo", b58 repo, "--number", "1"]))
        `shouldBe` Just ForReal
      fmap owDry (ownArgs (argv ["--repo", b58 repo, "--number", "1", "--dry-run"]))
        `shouldBe` Just DryRun
      -- A switch, so it cannot swallow the word after it.
      ownArgs (argv ["--repo", b58 repo, "--number", "1", "--dry-run", "yes"])
        `shouldBe` Nothing

    it "refuses a flag standing where a value belongs" $ do
      repo <- aKey
      ownArgs (argv ["--repo", b58 repo, "--number", "--note", "done"])
        `shouldBe` Nothing

    -- A note is free text, and argvAtom keeps a numeric word as a number.
    it "takes a note that spells a number" $ do
      repo <- aKey
      fmap owNote (ownArgs (argv [ "--repo", b58 repo, "--number", "1"
                                 , "--note", "2026" ]))
        `shouldBe` Just (Just "2026")

  describe "PEP-22 hub issue assign: arguments" $ do

    it "reads a key to assign to" $ do
      repo <- aKey ; bob <- aKey
      fmap owTo (ownArgs (argv [ "--repo", b58 repo, "--number", "7"
                               , "--to", b58 bob ]))
        `shouldBe` Just (Just bob)

    -- The attribute is last-writer-wins and there is no way to remove one, so
    -- an empty value IS the absence. Saying it out loud is the same rule the
    -- labels follow: an unassign because somebody forgot an argument would be
    -- published into append-only canon.
    it "tells unassigning from forgetting the key" $ do
      repo <- aKey
      let base = ["--repo", b58 repo, "--number", "7"]
      fmap (\a -> (owTo a, owClear a)) (ownArgs (argv base))
        `shouldBe` Just (Nothing, False)
      fmap (\a -> (owTo a, owClear a)) (ownArgs (argv (base <> ["--clear"])))
        `shouldBe` Just (Nothing, True)

    it "refuses a --to that is not a key rather than ignoring it" $ do
      repo <- aKey
      ownArgs (argv ["--repo", b58 repo, "--number", "7", "--to", "bob"])
        `shouldBe` Nothing

  describe "PEP-19 hub redact: arguments" $ do

    it "reads a complete call" $ do
      repo <- aKey
      let e = HashRef (hashObject (LBS.pack "an event"))
      redactArgs (argv ["--repo", b58 repo, "--event", show (pretty e)])
        `shouldBe` Just (RedactArgs repo e Nothing ForReal)

    it "refuses a call with no event, and one whose event is not a hash" $ do
      repo <- aKey
      redactArgs (argv ["--repo", b58 repo]) `shouldBe` Nothing
      redactArgs (argv ["--repo", b58 repo, "--event", "not-a-hash"])
        `shouldBe` Nothing

    -- An event-id and a key are both thirty-two bytes of base58, so nothing
    -- here can be told apart by shape and the flags are what say which is
    -- which. This records that the parser cannot catch a swap.
    it "cannot tell a key from a hash, which is why every value is flagged" $ do
      repo <- aKey
      isJust (redactArgs (argv ["--repo", b58 repo, "--event", b58 repo]))
        `shouldBe` True

  -- EACH VERB'S OWN FLAGS, and this is the half that was silently permissive.
  -- One reader served four verbs with the union of everything any of them
  -- takes, so a flag the verb in hand does not use was not refused but DROPPED:
  -- `issue close --to <key>` was accepted and the assignment discarded.
  -- "HBS2.Hub.CLI.Argv" states the rule that broke -- nothing here can tell an
  -- operator's mistake from an intention, so the only safe answer to a word
  -- nobody claimed is to stop.
  describe "PEP-22 hub issue: a verb refuses a flag it does not have" $ do

    it "close takes --note and refuses the other two verbs' flags" $ do
      repo <- aKey ; bob <- aKey
      let close = ownArgsFor ["--note"] []
          base = ["--repo", b58 repo, "--number", "7"]
      isJust (close (argv (base <> ["--note","done"]))) `shouldBe` True
      close (argv (base <> ["--to", b58 bob])) `shouldBe` Nothing
      close (argv (base <> ["--label","bug"])) `shouldBe` Nothing
      close (argv (base <> ["--clear"])) `shouldBe` Nothing

    it "label takes --label and --clear and refuses --to and --note" $ do
      repo <- aKey ; bob <- aKey
      let label = ownArgsFor ["--label"] ["--clear"]
          base = ["--repo", b58 repo, "--number", "7"]
      isJust (label (argv (base <> ["--label","bug"]))) `shouldBe` True
      isJust (label (argv (base <> ["--clear"]))) `shouldBe` True
      label (argv (base <> ["--to", b58 bob])) `shouldBe` Nothing
      label (argv (base <> ["--note","done"])) `shouldBe` Nothing

    it "assign takes --to and --clear and refuses --label and --note" $ do
      repo <- aKey ; bob <- aKey
      let assign = ownArgsFor ["--to"] ["--clear"]
          base = ["--repo", b58 repo, "--number", "7"]
      isJust (assign (argv (base <> ["--to", b58 bob]))) `shouldBe` True
      assign (argv (base <> ["--label","bug"])) `shouldBe` Nothing
      assign (argv (base <> ["--note","done"])) `shouldBe` Nothing

    -- The pair the label verb used to admit and then resolve in favour of the
    -- destructive one, publishing a clear when the operator asked to add. The
    -- READER still accepts it, because both flags are that verb's; what refuses
    -- it is the verb's own guard, which no test can reach (the bindMatch body
    -- is not linked into this suite). Recorded here so that whoever makes it
    -- reachable knows what to assert.
    it "reads --label with --clear, which the verb is what refuses" $ do
      repo <- aKey
      let label = ownArgsFor ["--label"] ["--clear"]
          got = label (argv ["--repo", b58 repo, "--number", "7"
                            ,"--label","bug","--clear"])
      fmap owClear got `shouldBe` Just True
      fmap owLabels got `shouldBe` Just ["bug"]

-- | THE ONE TEST THAT CROSSES THREE MODULES IN THE PRODUCT'S DIRECTION.
--
-- What `hub assign` writes, what the fold makes of it, and what a renderer
-- reads back out of the PEP-22 contract. Every one of those was tested on its
-- own and the AGREEMENT between them was tested by nothing -- which is exactly
-- how the singular `assignee` shipped: the verb wrote a name in no reader's
-- vocabulary but its own, `hub verify` raised nothing (the normalizer lists the
-- plural), and every thread ever assigned came out of `--json` as unassigned
-- while the terminal showed an assignee.
crossing :: Spec
crossing =
  describe "PEP-19/22 what an owner writes is what a renderer reads" $ do

    it "carries an assignee from the verb through the fold into the contract" $ do
      owner <- kp
      alice <- aKey
      let repo = fst owner
          o = anOpen owner 1 "one"
          thr = eventId o
          -- The value the verb writes, taken from the verb rather than spelled
          -- again here: a fixture that spells it is a test of this test's
          -- opinion, and the opinion is what was wrong.
          set = uncurry (ASet thr) (assignSet False (Just alice)) 2000
          fr = foldEvents repo [o, ev owner 2 set]
          [t] = HM.elems (frThreads fr)
      case field (threadContract t Nothing) "assignees" of
        A.Array vs -> foldr (:) [] vs `shouldBe` [A.String (Text.pack (b58 alice))]
        other      -> expectationFailure ("not an array: " <> show other)

    -- And the clear, which is the same wire: last-writer-wins cannot remove an
    -- attribute, so the empty value IS the absence a reader shows as none.
    it "carries a clear through as the empty set" $ do
      owner <- kp
      alice <- aKey
      let repo = fst owner
          o = anOpen owner 1 "one"
          thr = eventId o
          fr = foldEvents repo
                 [ o
                 , ev owner 2 (uncurry (ASet thr) (assignSet False (Just alice)) 2000)
                 , ev owner 3 (uncurry (ASet thr) (assignSet True Nothing) 3000) ]
          [t] = HM.elems (frThreads fr)
      field (threadContract t Nothing) "assignees" `shouldBe` A.Array mempty

    -- The same for labels, which is the attribute the assignee one was supposed
    -- to be spelled like.
    it "carries labels through, canonically ordered" $ do
      owner <- kp
      let repo = fst owner
          o = anOpen owner 1 "one"
          thr = eventId o
          set = uncurry (ASet thr) (labelSet False ["ui","bug"]) 2000
          fr = foldEvents repo [o, ev owner 2 set]
          [t] = HM.elems (frThreads fr)
      case field (threadContract t Nothing) "labels" of
        A.Array vs -> foldr (:) [] vs `shouldBe` [A.String "bug", A.String "ui"]
        other      -> expectationFailure ("not an array: " <> show other)

-- The pieces the crossing needs, and they are the product's own: an event
-- signed for real, and a fold over it.
type KP = (HubKey, PrivKey 'Sign HubScheme)

kp :: IO KP
kp = do
  c <- newCredentials @'HBS2Basic
  pure (_peerSignPk c, _peerSignSk c)

canonOf :: RepoRef -> Word64 -> Maybe Word64 -> EventId -> CanonContent
canonOf repo sq num eid = CanonContent repo eid sq num Nothing Nothing sq Nothing

ev :: KP -> Word64 -> AuthorContent -> Event
ev owner sq c = mkEvent owner owner c (canonOf (fst owner) sq Nothing)

anOpen :: KP -> Word64 -> Text -> Event
anOpen owner sq t =
  mkEvent owner owner (AOpen (fst owner) HubIssue t [] (Just "body") Nothing Nothing 1000)
          (canonOf (fst owner) sq (Just sq))

field :: A.Value -> Text -> A.Value
field v k = case v of
  A.Object o -> fromMaybe A.Null (A.lookup (A.fromText k) o)
  _          -> A.Null

-- | @hub issue reply@: a maintainer's comment written straight onto canon.
--
-- The verb is argument reading, 'ownerEvent' and a commit, and the commit is
-- the part every owner verb shares. What is particular to this one is that a
-- COMMENT goes through 'ownerEvent' at all, and what the readers make of it:
-- so these cases run the content the verb builds through the bridge, the fold,
-- the canon reader that @hub verify@ uses, and both renderings of a thread.
replying :: Spec
replying = do

  describe "PEP-22 hub issue|pr reply: arguments" $ do

    it "reads a call by number and a call by thread-id" $ do
      repo <- aKey
      let thr = HashRef (hashObject (LBS.pack "a thread"))
      replyArgs (argv ["--repo", b58 repo, "--number", "7", "--body", "on it"])
        `shouldBe` Just (ReplyArgs repo (Right 7) Nothing "on it" Nothing ForReal)
      replyArgs (argv [ "--body", "on it", "--thread", show (pretty thr)
                      , "--repo", b58 repo, "--dry-run" ])
        `shouldBe` Just (ReplyArgs repo (Left thr) Nothing "on it" Nothing DryRun)

    it "takes a reply-to, a delegate's key, and a body that spells a number" $ do
      repo <- aKey ; bob <- aKey
      let e = HashRef (hashObject (LBS.pack "an event"))
          got = replyArgs (argv [ "--repo", b58 repo, "--number", "1"
                                , "--body", "2026", "--reply-to", show (pretty e)
                                , "--as", b58 bob ])
      fmap rpReplyTo got `shouldBe` Just (Just e)
      fmap rpAs got `shouldBe` Just (Just bob)
      fmap rpBody got `shouldBe` Just "2026"

    it "wants exactly one of --number and --thread" $ do
      repo <- aKey
      let thr = HashRef (hashObject (LBS.pack "a thread"))
          base = ["--repo", b58 repo, "--body", "x"]
      replyArgs (argv base) `shouldBe` Nothing
      replyArgs (argv (base <> ["--number", "1", "--thread", show (pretty thr)]))
        `shouldBe` Nothing

    it "refuses a call with no body, no repository, or a flag it does not have" $ do
      repo <- aKey ; bob <- aKey
      replyArgs (argv ["--repo", b58 repo, "--number", "1"]) `shouldBe` Nothing
      replyArgs (argv ["--number", "1", "--body", "x"]) `shouldBe` Nothing
      -- The letter verb's flags, which mean nothing here: a sender and an
      -- author are the letter path, and dropping them would be signing as
      -- somebody other than the caller named.
      replyArgs (argv [ "--repo", b58 repo, "--number", "1", "--body", "x"
                      , "--author", b58 bob ]) `shouldBe` Nothing
      replyArgs (argv [ "--repo", b58 repo, "--number", "1", "--body", "x"
                      , "--note", "y" ]) `shouldBe` Nothing

  describe "PEP-19/22 hub issue reply: what a maintainer writes is a comment" $ do

    it "is admitted, verifies clean, and renders like any other comment" $ do
      owner <- kp
      let repo = fst owner
          o = anOpen owner 1 "one"
          thr = eventId o
          prior = foldEvents repo [o]
          content = replyContent thr (Just thr) "looking at it" 2000
      acc <- either (fail . show) pure
               (ownerEvent (TriageCtx owner (const True) repo) (viewOf prior)
                           2000 noOwnAttachments content)
      let c = acEvent acc
          cid = eventId c
          fr = foldEvents repo [o, c]

      -- Admitted by the fold, with nothing dropped and nothing odd.
      HM.member cid (frAdmitted fr) `shouldBe` True
      frDropped fr `shouldSatisfy` null
      frAnomalies fr `shouldSatisfy` null

      -- `hub verify` over a tree holding it in the file the verb writes.
      st <- readCanon (byPath [ ("version", renderMeta (metaAt hubMetaVersion))
                              , (fromPath (threadDir thr <> "/" <> eventFileName 1 thr), renderEvent o)
                              , (fromPath (eventPath acc), renderEvent c) ])
                      repo
              >>= either (fail . show) pure
      reportCode st `shouldBe` 0
      HM.member cid (frAdmitted (stFold st)) `shouldBe` True

      -- The thread carries it, by the maintainer, with no origin.
      let [t] = HM.elems (frThreads fr)
      fmap (\x -> (cId x, cAuthor x, cBody x, cReplyTo x, cOrigin x)) (tsComments t)
        `shouldBe` [(cid, repo, Just "looking at it", Just thr, Nothing)]

      -- `issue show --json`...
      case field (threadContract t Nothing) "comments" of
        A.Array vs -> case foldr (:) [] vs of
          [v] -> do
            field v "event_id" `shouldBe` A.String (Text.pack (show (pretty cid)))
            field v "author"   `shouldBe` A.String (Text.pack (b58 repo))
            field v "body"     `shouldBe` A.String "looking at it"
            field v "origin"   `shouldBe` A.Null
          other -> expectationFailure ("not one comment: " <> show other)
        other -> expectationFailure ("not an array: " <> show other)

      -- ...and `issue show`.
      unwords (fmap show (showDoc t)) `shouldSatisfy` ("looking at it" `isInfixOf`)

    it "is admitted from a delegate the owner named" $ do
      owner <- kp ; bob <- kp
      let repo = fst owner
          o = anOpen owner 1 "one"
          thr = eventId o
          d = ev owner 2 (ADelegate repo (fst bob) 1500)
          prior = foldEvents repo [o, d]
      acc <- either (fail . show) pure
               (ownerEvent (TriageCtx bob (const True) repo) (viewOf prior)
                           3000 noOwnAttachments (replyContent thr Nothing "mine" 3000))
      let fr = foldEvents repo [o, d, acEvent acc]
          [t] = HM.elems (frThreads fr)
      frDropped fr `shouldSatisfy` null
      fmap (\x -> (cAuthor x, cCanonBy x)) (tsComments t) `shouldBe` [(fst bob, fst bob)]

    -- The refusal the verb exits with codeTriageRefused on, prior anything is
    -- signed into canon: a key that may not bless canon cannot comment here,
    -- and has the letter path instead.
    it "refuses a key that is not a maintainer" $ do
      owner <- kp ; mallory <- kp
      let repo = fst owner
          o = anOpen owner 1 "one"
          prior = foldEvents repo [o]
      case ownerEvent (TriageCtx mallory (const True) repo) (viewOf prior)
                      2000 noOwnAttachments (replyContent (eventId o) Nothing "hi" 2000) of
        Left e  -> e `shouldBe` Composed UnauthorizedForRepo
        Right _ -> expectationFailure "a stranger's key blessed a comment"

    it "refuses a thread canon does not hold" $ do
      owner <- kp
      let repo = fst owner
          prior = foldEvents repo [anOpen owner 1 "one"]
          ghost = HashRef (hashObject (LBS.pack "no such thread"))
      case ownerEvent (TriageCtx owner (const True) repo) (viewOf prior)
                      2000 noOwnAttachments (replyContent ghost Nothing "hi" 2000) of
        Left e  -> e `shouldBe` Composed UnknownThread
        Right _ -> expectationFailure "a comment on no thread was blessed"

fromPath :: FilePath -> BS.ByteString
fromPath = TextE.encodeUtf8 . Text.pack

-- A canon tree in memory, as "HBS2.Hub.VerifySpec" builds one; kept local for
-- the reason that one gives.
byPath :: [(BS.ByteString, Text)] -> CanonSource IO
byPath files = CanonSource
  { csCommit  = pure (Right "deadbeef")
  , csClose   = pure ()
  , csEntries = const (pure (Right
      [ TreeEntry p (Blob (oidOf i) (BS.length (TextE.encodeUtf8 t)))
      | (i,(p,t)) <- zip [0 :: Int ..] files ]))
  , csBlob    = \oid -> pure (maybe (BlobRefused "no such object") BlobText
                                    (lookup oid byOid))
  }
  where
    oidOf i = Text.pack (show (i :: Int))
    byOid = [ (oidOf i, t) | (i,(_,t)) <- zip [0 :: Int ..] files ]
