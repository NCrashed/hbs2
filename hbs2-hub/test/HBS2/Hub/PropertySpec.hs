module HBS2.Hub.PropertySpec (spec) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Bridge
import HBS2.Hub.Fold
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.Refs (HashRef)

import Data.HashSet qualified as HS
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Word (Word64)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO,run,assert)

type KP = (HubKey, PrivKey 'Sign HubScheme)

-- | One thing a triage loop might do next.
--
-- Deliberately includes moves that should be refused (replying before the
-- thread exists, honouring twice, reviving a stale view): the property is
-- that the bridge either refuses or mints something the fold admits, never
-- something in between.
data Step =
    StepOpen Word64 HubKind
  | StepComment Int Word64      -- ^ index into the threads opened so far
  | StepClose Int Word64
  | StepHonour Int Word64       -- ^ honour the request letter made by StepClose
  | StepOwnerSet Int Word64
  | StepStaleView Int Word64    -- ^ replay a step against an older view
  deriving stock (Show)

instance Arbitrary Step where
  arbitrary = do
    n <- choose (0,3)
    t <- choose (1,1000)
    oneof [ StepOpen t <$> elements [HubIssue,HubPR]
          , pure (StepComment n t)
          , pure (StepClose n t)
          , pure (StepHonour n t)
          , pure (StepOwnerSet n t)
          , pure (StepStaleView n t)
          ]

coords :: PRCoords
coords = PRCoords (Just "hbs23://fork") "refs/heads/f" "aaaa" "refs/heads/master" "bbbb" Nothing

-- The interpreter's state: the current view, every event minted so far, the
-- threads opened, and a few older views to replay against.
data St = St
  { stView    :: CanonView
  , stEvents  :: [Event]
  , stThreads :: [ThreadId]
  , stOld     :: [CanonView]
  , stOrigins :: [HashRef]
    -- Each letter needs its own origin, as each is a distinct message.
  , stStep    :: Int
    -- Origins for the request letters a honour step answers, indexed by
    -- thread rather than by step, so honouring the same request twice at
    -- different clocks is reachable.
  , stReqOrigins :: [HashRef]
  }

spec :: Spec
spec =
  describe "bridge/fold agreement" $
    it "never mints anything the fold drops, whatever the sequence" $
      property $ \steps -> monadicIO $ do
        (dropped, agreed, oneEventPerLetter) <- run (runSteps steps)
        -- Three invariants. The fold admits everything the bridge minted;
        -- the view carried across the run still equals a rebuild from the
        -- canon that resulted; and no letter produced two canon events,
        -- which the first two cannot see (two close events with different
        -- ids are a valid canon, just a wrong one).
        assert (null dropped && agreed && oneEventPerLetter)

runSteps :: [Step] -> IO ([(EventId,DropReason)], Bool, Bool)
runSteps steps = do
  owner <- newKp
  alice <- newKp
  origins <- mapM (const (someHash alice)) [0 .. length steps + 1]
  reqOrigins <- mapM (const (someHash alice)) [0 .. (7 :: Int)]
  let repo = fst owner
      ctx  = TriageCtx owner (const True) repo
      st0  = St (emptyView repo) [] [] [] origins 0 reqOrigins
      st   = foldl' (\s sp -> step ctx owner alice repo s { stStep = stStep s + 1 } sp) st0 steps
      fr   = foldEvents repo (stEvents st)
      -- One canon event per letter: count the origins the events carry.
      origs = mapMaybe (fmap ccOrigin . canonOf) (stEvents st)
      seen = length (mapMaybe id origs)
  pure ( frDropped fr
       , stView st == viewOf repo fr
       , seen == HS.size (HS.fromList (mapMaybe id origs)) )

step :: TriageCtx -> KP -> KP -> RepoRef -> St -> Step -> St
step ctx owner alice repo st = \case
  StepOpen ts kind ->
    let content = AOpen repo kind "t" [] Nothing Nothing
                    (if kind == HubPR then Just coords else Nothing) ts
    in accept (makeLetter (fst alice) (snd alice) content noReplyChannel)

  StepComment i ts -> withThread i $ \thr ->
    accept (makeLetter (fst alice) (snd alice)
             (AComment thr Nothing (Just "c") Nothing ts) noReplyChannel)

  -- A close from a stranger is a request, so this is always refused; the
  -- point is that it is refused rather than half-applied.
  StepClose i ts -> withThread i $ \thr ->
    accept (makeLetter (fst alice) (snd alice) (AClose thr Nothing ts) noReplyChannel)

  StepHonour i ts -> withThread i $ \thr ->
    let req = makeLetter (fst alice) (snd alice) (AClose thr Nothing ts) noReplyChannel
    in case honourRequest ctx (EnvelopeSigner (fst alice)) (stView st) ts (reqOriginOf i) req of
         Left _    -> st
         Right acc -> keep acc

  StepOwnerSet i ts -> withThread i $ \thr ->
    case ownerEvent ctx (stView st) ts Nothing (ASet thr "status" "closed" ts) of
      Left _    -> st
      Right acc -> keep acc

  -- Mint against a view from earlier in the run and then throw the result
  -- away, which is what a caller must do when the write fails: the event
  -- never reaches canon and the view is not advanced. The step is here to
  -- check that doing so leaves no trace, since a stale view would otherwise
  -- mint a duplicate seq and number.
  StepStaleView i ts -> case drop i (stOld st) of
    []      -> st
    (old:_) ->
      let content = AOpen repo HubIssue "stale" [] Nothing Nothing Nothing ts
          letter = makeLetter (fst alice) (snd alice) content noReplyChannel
      in seq (acceptLetter ctx (EnvelopeSigner (fst alice)) old ts (originOf (stStep st)) Nothing letter) st
  where
    _ = owner

    originOf n = case drop (n `mod` length (stOrigins st)) (stOrigins st) of
      (o:_) -> o
      []    -> head (stOrigins st)

    reqOriginOf n = case drop (n `mod` length (stReqOrigins st)) (stReqOrigins st) of
      (o:_) -> o
      []    -> head (stReqOrigins st)

    withThread i k = case drop i (stThreads st) of
      (thr:_) -> k thr
      []      -> st

    accept letter =
      case acceptLetter ctx (EnvelopeSigner (fst alice)) (stView st) 1 (originOf (stStep st)) Nothing letter of
        Left _    -> st
        Right acc -> keep acc

    keep acc = st
      { stView   = acView acc
      , stEvents = acEvent acc : stEvents st
      , stOld    = stView st : stOld st
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

someHash :: KP -> IO HashRef
someHash _ = do
  (pk,sk) <- newKp
  pure (authorBoxId (signAuthor pk sk (ARevoke pk 0)))

-- The canon content of an event, if this build can read it.
canonOf :: Event -> Maybe CanonContent
canonOf e = either (const Nothing) (Just . snd) (unboxChecked (evCanonBox e))
