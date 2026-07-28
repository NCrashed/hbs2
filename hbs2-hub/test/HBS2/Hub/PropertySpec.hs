module HBS2.Hub.PropertySpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Letter hiding (classify)
import HBS2.Hub.Bridge
import HBS2.Hub.Fold
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.GroupKeySymm (typicalKeyLength)
import HBS2.Data.Types.Refs (HashRef)
import HBS2.Data.Types.SignedBox (SignedBox(..))

import Control.Monad ((<=<))
import Data.ByteString qualified as BS
import Data.HashSet qualified as HS
import Data.List (foldl')
import Data.Maybe (fromMaybe,mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO,run,assert,monitor)

type KP = (HubKey, PrivKey 'Sign HubScheme)

-- | One thing a triage loop might do next.
--
-- Deliberately includes moves that should be refused (replying before the
-- thread exists, honouring twice, revising with coordinates nothing can
-- fetch, folding under a revoked delegation, reviving a stale view): the
-- property is that the bridge either refuses or mints something the fold
-- admits, never something in between. Every op the bridge can mint has a
-- step, because an op with no step is an op the property does not cover.
data Step =
    StepOpen Word64 HubKind
    -- | A maintainer files it themselves, with no letter behind it. The only
    -- other op that mints a number, and one an all-letters generator never
    -- reaches.
  | StepOwnerOpen Word64 HubKind
  | StepComment Int Word64      -- ^ index into the threads opened so far
  | StepClose Int Word64        -- ^ a request, so always refused as such
  | StepHonour Int Word64       -- ^ honour the request letter made by StepClose
  | StepOwnerSet Int Word64
  | StepRevise Int Word64 Bool  -- ^ the flag: coordinates the fold can fetch
  | StepMerge Int Word64
  | StepRedact Int Word64       -- ^ index into the events minted so far
  | StepDelegate Word64         -- ^ authorize the second key
  | StepRevoke Word64           -- ^ withdraw it again
  | StepAsDelegate Int Word64   -- ^ the second key folds a comment letter
  | StepStaleView Int Word64    -- ^ mint against an older view, then discard
    -- | A multi-valued attribute, canonical or not: the same labels in another
    -- order are other bytes and so another event-id.
  | StepSetLabels Int Word64 Bool
  | StepHonourWith Int Word64   -- ^ the maintainer signs their own note
  | StepAttach Int Word64 Attach
  | StepBigBody Word64          -- ^ an inline body over what triage carries
    -- | A letter from the second contributor, rewrapped by someone else. The
    -- envelope signer is not the author, so the reply channel must be dropped
    -- and the letter must still fold.
  | StepRewrapped Word64 HubKind
  | StepFromDenied Word64       -- ^ a letter from a deny-listed author (PEP-21)
  | StepCorrupt Word64          -- ^ a letter whose inner box does not verify
  deriving stock (Show)

-- | What the caller can say about an attachment, and what it costs to be
-- wrong. Each of the four is a different answer, and only one mints.
data Attach = Ready | NotFetched | NotCarried | NoKey
  deriving stock (Eq,Show)

instance Arbitrary Step where
  arbitrary = frequency
    [ (3, StepOpen <$> genTs <*> elements [HubIssue,HubPR])
    , (2, StepOwnerOpen <$> genTs <*> elements [HubIssue,HubPR])
    , (3, StepComment <$> genIx <*> genTs)
    , (1, StepClose <$> genIx <*> genTs)
    , (2, StepHonour <$> genIx <*> genTs)
    , (1, StepOwnerSet <$> genIx <*> genTs)
    , (2, StepRevise <$> genIx <*> genTs <*> arbitrary)
    , (1, StepMerge <$> genIx <*> genTs)
    , (1, StepRedact <$> genIx <*> genTs)
    , (2, StepDelegate <$> genTs)
    , (1, StepRevoke <$> genTs)
    , (2, StepAsDelegate <$> genIx <*> genTs)
    , (1, StepStaleView <$> genIx <*> genTs)
    , (2, StepSetLabels <$> genIx <*> genTs <*> arbitrary)
    , (2, StepHonourWith <$> genIx <*> genTs)
    , (2, StepAttach <$> genIx <*> genTs
            <*> elements [Ready,NotFetched,NotCarried,NoKey])
    , (1, StepBigBody <$> genTs)
    , (2, StepRewrapped <$> genTs <*> elements [HubIssue,HubPR])
    , (1, StepFromDenied <$> genTs)
    , (1, StepCorrupt <$> genTs)
    ]

  -- Without a shrink a counterexample is the whole generated script with
  -- four-digit timestamps, which says nothing about which three steps
  -- mattered. Shrinking indices and clocks towards zero is what turns the
  -- double-honour case into [StepOpen 1, StepHonour 0 1, StepHonour 0 2].
  shrink = \case
    StepOpen ts k       -> [StepOpen ts' k | ts' <- shrinkTs ts]
                        <> [StepOpen ts HubIssue | k /= HubIssue]
    StepOwnerOpen ts k  -> [StepOwnerOpen ts' k | ts' <- shrinkTs ts]
                        <> [StepOwnerOpen ts HubIssue | k /= HubIssue]
    StepComment i ts    -> both StepComment i ts
    StepClose i ts      -> both StepClose i ts
    StepHonour i ts     -> both StepHonour i ts
    StepOwnerSet i ts   -> both StepOwnerSet i ts
    StepMerge i ts      -> both StepMerge i ts
    StepRedact i ts     -> both StepRedact i ts
    StepAsDelegate i ts -> both StepAsDelegate i ts
    StepStaleView i ts  -> both StepStaleView i ts
    StepHonourWith i ts -> both StepHonourWith i ts
    StepBigBody ts      -> [StepBigBody ts' | ts' <- shrinkTs ts]
    StepFromDenied ts   -> [StepFromDenied ts' | ts' <- shrinkTs ts]
    StepCorrupt ts      -> [StepCorrupt ts' | ts' <- shrinkTs ts]
    StepRewrapped ts k  -> [StepRewrapped ts' k | ts' <- shrinkTs ts]
                        <> [StepRewrapped ts HubIssue | k /= HubIssue]
    StepSetLabels i ts ok -> [StepSetLabels i' ts ok | i' <- shrinkIx i]
                          <> [StepSetLabels i ts' ok | ts' <- shrinkTs ts]
                          <> [StepSetLabels i ts False | ok]
    StepAttach i ts a   -> [StepAttach i' ts a | i' <- shrinkIx i]
                        <> [StepAttach i ts' a | ts' <- shrinkTs ts]
                        <> [StepAttach i ts Ready | a /= Ready]
    StepDelegate ts     -> [StepDelegate ts' | ts' <- shrinkTs ts]
    StepRevoke ts       -> [StepRevoke ts' | ts' <- shrinkTs ts]
    StepRevise i ts ok  -> [StepRevise i' ts ok | i' <- shrinkIx i]
                        <> [StepRevise i ts' ok | ts' <- shrinkTs ts]
                        <> [StepRevise i ts False | ok]
    where
      both f i ts = [f i' ts | i' <- shrinkIx i] <> [f i ts' | ts' <- shrinkTs ts]

-- Small indices, so a step usually names a thread that exists.
genIx :: Gen Int
genIx = choose (0,4)

genTs :: Gen Word64
genTs = choose (1,1000)

shrinkIx :: Int -> [Int]
shrinkIx = filter (>= 0) . shrink

shrinkTs :: Word64 -> [Word64]
shrinkTs = filter (>= 1) . shrink

-- | A script that opens a thread first.
--
-- Without this most runs mint nothing at all: every other step names a
-- thread, and with no thread in canon they all refuse, so the invariants hold
-- vacuously and the generator spends its budget on the refusal paths.
newtype Script = Script [Step]
  deriving stock (Show)

instance Arbitrary Script where
  arbitrary = Script <$> ((:) <$> firstOpen <*> arbitrary)
    where firstOpen = StepOpen <$> genTs <*> elements [HubIssue,HubPR]
  shrink (Script s) = Script <$> shrink s

-- | What a run minted, for coverage. The invariants cannot see this: a run
-- that only ever refuses satisfies all of them.
data Tag =
    TOpen | TOwnerOpen | TComment | TRevise | TMerge | TRedact | TSet | THonour
  | TDelegate | TRevoke
  | TByDelegate      -- ^ the second key minted an event
  | TRevokedRefused  -- ^ ...and was refused after its delegation was withdrawn
  | TStaleDiscarded  -- ^ a mint against an older view, thrown away
  | TSetLabels | THonourWith | TAttached | TBigBody | TRewrapped
  | TDeniedRefused   -- ^ a deny-listed author was turned away
  | TCorruptRefused  -- ^ ...and so was a letter whose signature does not hold
  deriving stock (Eq,Show)

-- A group secret is raw key bytes of a fixed size, and the constructor checks
-- only that; telling the parts secret from the message secret is what
-- 'poMessage' is for.
secret32 :: PartSecret
secret32 = fromMaybe (error "bad fixture secret")
             (mkPartSecret (BS.replicate typicalKeyLength 0x41))

-- The secret over messageData: distinct by construction, as it must be.
msgSecret :: MessageSecret
msgSecret = fromMaybe (error "bad fixture secret")
              (mkMessageSecret (BS.replicate typicalKeyLength 0x42))

-- An inline body one byte over what triage will carry.
bigBody :: Text
bigBody = Text.replicate (maxInlineBody + 1) "x"

coords :: Bool -> PRCoords
-- A fork-pointer PR. PEP-20 requires one of the two ways to fetch the
-- change, so the unreachable variant (neither fork nor bundle) must be
-- refused rather than minted (reachableCoords).
coords ok = PRCoords (if ok then Just "hbs23://fork" else Nothing)
              "refs/heads/f" "aaaa" "refs/heads/master" "bbbb" Nothing

-- | The keys a run is played with, and the two folding contexts built from
-- them. A record rather than five positional arguments, three of which are
-- the same type: that is how the wrong key ends up in the wrong slot.
data Cast = Cast
  { castRepo  :: RepoRef     -- ^ = the owner's public key
  , castOwner :: TriageCtx   -- ^ folding as the owner
  , castDeleg :: TriageCtx   -- ^ folding as the second maintainer
  , castAlice :: KP          -- ^ the contributor most letters come from
  , castCarol :: KP          -- ^ a second contributor, whose letters arrive rewrapped
  , castMallory :: KP        -- ^ deny-listed (PEP-21)
  , castDelegKey :: HubKey
  }

-- The interpreter's state: the current view, every event minted so far, the
-- threads opened, and a few older views to replay against.
data St = St
  { stView    :: CanonView
  , stEvents  :: [Event]
  , stThreads :: [ThreadId]
  , stOld     :: [CanonView]
    -- Each letter needs its own origin, as each is a distinct message.
  , stOrigins :: [HashRef]
  , stStep    :: Int
    -- Origins for the request letters a honour step answers, indexed by
    -- thread rather than by step, so honouring the same request twice at
    -- different clocks is reachable.
  , stReqOrigins :: [HashRef]
  , stTags    :: [Tag]
  , stRefused :: [TriageError]
    -- The seq of every event minted against a stale view and then discarded.
  , stStale   :: [Word64]
  }

-- | What one run produced.
data Run = Run
  { rDropped  :: [(EventId,DropReason)]
  , rAnoms    :: [(EventId,Anomaly)]
  , rAgrees   :: Bool
  , rOnePer   :: Bool
  , rStaleDup :: Bool
  , rTags     :: [Tag]
  , rRefused  :: [TriageError]
  }

spec :: Spec
spec =
  describe "bridge/fold agreement" $
    it "never mints anything the fold drops, whatever the sequence" $
      -- More than the default 100: the rarest case below (a delegate refused
      -- after its delegation was withdrawn) turns up in about one run in
      -- twenty, and at 100 runs the coverage check would occasionally miss it
      -- through nothing but luck. The whole suite still takes about a second.
      property $ checkCoverage $ withMaxSuccess 300 $ \(Script steps) -> monadicIO $ do
        r <- run (runSteps steps)
        -- Coverage, not assertions. Every invariant below holds trivially for
        -- a run that minted nothing, so a generator that stopped reaching an
        -- op would keep passing in silence. The thresholds are deliberately
        -- far below the observed rates: they are there to catch a generator
        -- that stops reaching an op entirely, not to pin its distribution.
        monitor (tabulate "minted" (map show (rTags r)))
        monitor (classify (TStaleDiscarded `elem` rTags r) "discarded a stale mint")
        monitor (classify (TMerge `elem` rTags r) "merged a PR")
        monitor (cover 5 (TRevise `elem` rTags r) "revised a PR")
        monitor (cover 5 (TOwnerOpen `elem` rTags r) "opened a thread as the owner")
        monitor (cover 5 (TAttached `elem` rTags r) "folded an attachment")
        monitor (cover 5 (TSetLabels `elem` rTags r) "set a multi-valued attribute")
        monitor (cover 5 (THonourWith `elem` rTags r) "honoured a request in its own words")
        -- The hostile steps assert inside the interpreter rather than here, so
        -- without these three they would pass by never running.
        monitor (cover 5 (TRewrapped `elem` rTags r) "folded a rewrapped letter")
        monitor (cover 5 (TDeniedRefused `elem` rTags r) "turned away a deny-listed author")
        monitor (cover 5 (TCorruptRefused `elem` rTags r) "turned away a forged letter")
        monitor (cover 2 (TRedact `elem` rTags r) "redacted an event")
        monitor (cover 2 (TByDelegate `elem` rTags r) "folded under a delegation")
        monitor (cover 1 (TRevokedRefused `elem` rTags r) "refused a revoked delegate")
        monitor (cover 1 (any isDoubleHonour (rRefused r)) "refused a second honour")
        -- Five invariants. The fold admits everything the bridge minted; it
        -- reports no anomaly in what the bridge minted either, which is the
        -- stronger half (an anomaly is fold-legal, so nothing else here would
        -- notice); the view carried across the run still equals a rebuild from
        -- the canon that resulted; no letter produced two canon events, which
        -- none of the others can see (two close events with different ids are a
        -- valid canon, just a wrong one); and a mint against a stale view
        -- really would have collided, which is why the caller has to discard
        -- it.
        assert (  null (rDropped r) && null (rAnoms r)
               && rAgrees r && rOnePer r && rStaleDup r )
  where
    isDoubleHonour = \case
      AlreadyHonoured -> True
      _               -> False

runSteps :: [Step] -> IO Run
runSteps steps = do
  owner <- newKp
  alice <- newKp
  deleg <- newKp
  carol <- newKp
  mallory <- newKp
  origins <- mapM (const someHash) [0 .. length steps + 1]
  reqOrigins <- mapM (const someHash) [0 .. (7 :: Int)]
  let repo = fst owner
      -- The deny-list is on for the whole run: a ban is a property of the
      -- repo, not of a step, and the interesting part is that it holds under
      -- any envelope.
      allowed k = k /= fst mallory
      cast = Cast
        { castRepo  = repo
        , castOwner = TriageCtx owner allowed repo
        , castDeleg = TriageCtx deleg allowed repo
        , castAlice = alice
        , castCarol = carol
        , castMallory = mallory
        , castDelegKey = fst deleg
        }
      st0 = St (emptyView repo) [] [] [] origins 0 reqOrigins [] [] []
      st  = foldl' (\s sp -> step cast s { stStep = stStep s + 1 } sp) st0 steps
      fr  = foldEvents repo (stEvents st)
      -- One canon event per letter: count the origins the events carry.
      origs = mapMaybe (ccOrigin <=< canonOf) (stEvents st)
      live  = HS.fromList (mapMaybe (fmap ccSeq . canonOf) (stEvents st))
  pure Run
    { rDropped  = frDropped fr
    , rAnoms    = frAnomalies fr
    , rAgrees   = stView st == viewOf fr
    , rOnePer   = length origs == HS.size (HS.fromList origs)
    , rStaleDup = all (`HS.member` live) (stStale st)
    , rTags     = stTags st
    , rRefused  = stRefused st
    }

-- | The folder's clock, deliberately jumping around: the steps hand it whatever
-- timestamp they were generated with. A real folder's clock can be corrected
-- backwards, and the bridge clamps the stamp to what canon already holds, so a
-- monotonic harness clock here would be testing the harness.
clockOf :: St -> Word64
clockOf st = fromIntegral (stStep st `mod` 7) * 1000

step :: Cast -> St -> Step -> St
step cast st = \case
  StepOpen ts kind ->
    let content = AOpen repo kind "t" [] Nothing Nothing
                    (if kind == HubPR then Just (coords True) else Nothing) ts
    in accept TOpen (letterOf content)

  -- The other half of the number path, and the one that puts an
  -- owner-authored thread in front of the letter ops: a revise on one is
  -- refused, since the owner is its author of record.
  StepOwnerOpen ts kind ->
    mint TOwnerOpen (AOpen repo kind "mine" [] Nothing Nothing
                       (if kind == HubPR then Just (coords True) else Nothing) ts) ts

  StepComment i ts -> withThread i $ \thr ->
    accept TComment (letterOf (AComment thr Nothing (Just "c") Nothing ts))

  -- A close from a stranger is a request, so this is always refused; the
  -- point is that it is refused rather than half-applied.
  StepClose i ts -> withThread i $ \thr ->
    accept TSet (letterOf (AClose thr Nothing ts))

  StepRevise i ts ok -> withThread i $ \thr ->
    accept TRevise (letterOf (ARevise thr (coords ok) ts))

  -- The same key folds the same letter under the delegated context, so an
  -- event minted here is admitted only while the delegation stands. This is
  -- where a revoked maintainer would slip through.
  StepAsDelegate i ts -> withThread i $ \thr ->
    case acceptLetter (castDeleg cast) (EnvelopeSigner alicePk) (stView st) (clockOf st)
           (originOf (stStep st)) noAttachments
           (letterOf (AComment thr Nothing (Just "d") Nothing ts)) of
      Right acc -> keep TByDelegate acc
      -- Refused after minting earlier in this run: the delegation was
      -- withdrawn in between, which is the case worth counting.
      Left e
        | TByDelegate `elem` stTags st ->
            (refuse e) { stTags = TRevokedRefused : stTags st }
        | otherwise -> refuse e

  StepHonour i ts -> withThread i $ \thr ->
    let req = letterOf (AClose thr Nothing ts)
    in case honourRequest (castOwner cast) (EnvelopeSigner alicePk) (stView st) (clockOf st)
              (reqOriginOf i) req of
         Right acc -> keep THonour acc
         Left e    -> refuse e

  StepOwnerSet i ts -> withThread i $ \thr ->
    mint TSet (ASet thr "status" "closed" ts) ts

  StepMerge i ts -> withThread i $ \thr ->
    mint TMerge (AMerge thr "deadbeef" "refs/heads/master" ts) ts

  -- Out of range on purpose half the time: a redact naming an event that is
  -- not in canon has to be refused, not minted for the fold to drop.
  StepRedact i ts ->
    let target = case drop i (stEvents st) of
                   (e:_) -> eventId e
                   []    -> originOf (stStep st)  -- not an event id at all
    in mint TRedact (ARedact target ts) ts

  StepDelegate ts -> mint TDelegate (ADelegate (castDelegKey cast) ts) ts
  StepRevoke ts   -> mint TRevoke (ARevoke (castDelegKey cast) ts) ts

  -- Mint against a view from earlier in the run and then throw the result
  -- away, which is what a caller must do when the write fails: the event
  -- never reaches canon and the view is not advanced. What makes discarding
  -- mandatory is that the stale cursor hands out a seq canon has already
  -- issued, so the run records it and checks exactly that.
  StepSetLabels i ts ok -> withThread i $ \thr ->
    mint TSetLabels (ASet thr "labels" (if ok then "bug,ui" else "ui,bug") ts) ts

  -- The reviewed half of the honour path, sharing the request origin with
  -- StepHonour so that honouring one letter both ways is caught.
  StepHonourWith i ts -> withThread i $ \thr ->
    let req = letterOf (AClose thr Nothing ts)
    in case honourWith (castOwner cast) (EnvelopeSigner alicePk) (stView st)
              (clockOf st) (reqOriginOf i) (AClose thr (Just "reviewed") ts) req of
         Right acc -> keep THonourWith acc
         Left e    -> refuse e

  -- Only Ready may mint. The other three are the ways a caller can be wrong
  -- about an attachment, and every one of them would be permanent in canon.
  StepAttach i ts att ->
    let part = originOf (stStep st + i + 1)
        po = case att of
               Ready      -> attachments secret32 msgSecret [part]
               NotFetched -> attachmentsPending secret32 msgSecret [part]
               NotCarried -> noAttachments
               NoKey      -> attachmentsNoKey msgSecret [part]
        content = AOpen repo HubIssue "att" [] Nothing (Just part) Nothing ts
    in acceptWith po TAttached (letterOf content)

  StepBigBody ts ->
    accept TBigBody (letterOf (AOpen repo HubIssue "t" [] (Just bigBody) Nothing Nothing ts))

  -- Authored by carol, put on the wire by alice. Rewrapping is how an
  -- envelope-key ban is evaded (PEP-18), so what must survive it is the
  -- authorship, and what must not is the reply channel.
  StepRewrapped ts kind ->
    let (cpk,csk) = castCarol cast
        content = AOpen repo kind "rewrapped" [] Nothing Nothing
                    (if kind == HubPR then Just (coords True) else Nothing) ts
        letter = makeLetter cpk csk content (ReplyTo cpk (originOf (stStep st)))
    in case acceptLetter (castOwner cast) (EnvelopeSigner alicePk) (stView st) ts
              (originOf (stStep st)) noAttachments letter of
         Right acc | acReply acc /= NoReply ->
           -- Not an invariant of the fold, so it would go unnoticed there: a
           -- rewrapper must not be able to redirect the notification.
           error "a rewrapped letter kept its reply channel"
         Right acc -> keep TRewrapped acc
         Left e    -> refuse e

  StepFromDenied ts ->
    let (mpk,msk) = castMallory cast
        content = AOpen repo HubIssue "spam" [] Nothing Nothing Nothing ts
    in case acceptLetter (castOwner cast) (EnvelopeSigner alicePk) (stView st) ts
              (originOf (stStep st)) noAttachments (makeLetter mpk msk content noReplyChannel) of
         Right _ -> error "a deny-listed author was folded"
         Left e  -> (refuse e) { stTags = TDeniedRefused : stTags st }

  -- A letter whose signed bytes were edited after signing. Nothing about it
  -- can be trusted, including the author it names.
  StepCorrupt ts ->
    let content = AOpen repo HubIssue "t" [] Nothing Nothing Nothing ts
        md = letterOf content
        broken = case mdBody md of
                   Letter (SignedBox pk bs sig) rc ->
                     MessageData hubMsgVersion (Letter (SignedBox pk (bs <> "x") sig) rc)
                   ack -> MessageData hubMsgVersion ack
    in case acceptLetter (castOwner cast) (EnvelopeSigner alicePk) (stView st) ts
              (originOf (stStep st)) noAttachments broken of
         Right _ -> error "a forged letter was folded"
         Left e  -> (refuse e) { stTags = TCorruptRefused : stTags st }

  StepStaleView i ts -> case drop i (stOld st) of
    []      -> st
    (old:_) ->
      let content = AOpen repo HubIssue "stale" [] Nothing Nothing Nothing ts
      in case acceptLetter (castOwner cast) (EnvelopeSigner alicePk) old (clockOf st)
                (originOf (stStep st)) noAttachments (letterOf content) of
           Left e    -> refuse e
           Right acc -> case canonOf (acEvent acc) of
             Nothing -> st
             Just cc -> st { stStale = ccSeq cc : stStale st
                           , stTags  = TStaleDiscarded : stTags st
                           }
  where
    repo = castRepo cast
    (alicePk,aliceSk) = castAlice cast

    letterOf content = makeLetter alicePk aliceSk content noReplyChannel

    originOf n = case drop (n `mod` length (stOrigins st)) (stOrigins st) of
      (o:_) -> o
      []    -> head (stOrigins st)

    reqOriginOf n = case drop (n `mod` length (stReqOrigins st)) (stReqOrigins st) of
      (o:_) -> o
      []    -> head (stReqOrigins st)

    withThread i k = case drop i (stThreads st) of
      (thr:_) -> k thr
      []      -> st

    accept tag letter = acceptWith noAttachments tag letter

    acceptWith po tag letter =
      case acceptLetter (castOwner cast) (EnvelopeSigner alicePk) (stView st)
             (clockOf st) (originOf (stStep st)) po letter of
        Right acc -> keep tag acc
        Left e    -> refuse e

    mint tag content _ts =
      case ownerEvent (castOwner cast) (stView st) (clockOf st) noOwnAttachments content of
        Right acc -> keep tag acc
        Left e    -> refuse e

    -- A refusal is normal: most steps name a thread that does not exist, or
    -- carry an op the letter path must not. Only the two refusals a tag says
    -- something about add one, so 'stTags' stays a record of what was minted.
    refuse e = st { stRefused = e : stRefused st }

    keep tag acc = st
      { stView   = acView acc
      , stEvents = acEvent acc : stEvents st
      , stOld    = stView st : stOld st
      , stTags   = tag : stTags st
      , stThreads = case acScope acc of
          ThreadScope t | isOpen (acContent acc) -> stThreads st <> [t]
          _ -> stThreads st
      }

    isOpen = \case
      AOpen{} -> True
      _       -> False

newKp :: IO KP
newKp = do
  c <- newCredentials @'HBS2Basic
  pure (_peerSignPk c, _peerSignSk c)

-- A hash no event will have: a throwaway key signing a throwaway op.
someHash :: IO HashRef
someHash = do
  (pk,sk) <- newKp
  pure (authorBoxId (signAuthor pk sk (ARevoke pk 0)))

-- The canon content of an event, if this build can read it.
canonOf :: Event -> Maybe CanonContent
canonOf e = either (const Nothing) (Just . snd) (unboxChecked (evCanonBox e))
