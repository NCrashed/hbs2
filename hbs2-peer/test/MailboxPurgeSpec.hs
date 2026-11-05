module MailboxPurgeSpec (tests) where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.Data.Detect (readLog)
import HBS2.Merkle
import HBS2.Peer.Proto.Mailbox.Retention
import HBS2.Storage
import MailboxFixture (withStore)
import MailboxPurge

import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "mailbox physical purge"
  [ testCase "storage shrinks while a canon-protected tree survives" $
      withStore $ \storage -> do
        victim <- oneLeafTree storage "victim payload"
        protected <- oneLeafTree storage "canon payload"
        victimClosure <- mailboxObjectClosure storage (Set.singleton victim)
        protectedClosure <- mailboxObjectClosure storage (Set.singleton protected)
        let candidates = mailboxPurgeCandidates
              mempty victimClosure mempty mempty protectedClosure
        before <- present storage (victimClosure <> protectedClosure)
        order <- mailboxObjectOrder storage (Set.singleton victim)
        last order @?= victim
        (deleted,failures) <- deleteMailboxBlocks storage
          (filter (`Set.member` candidates) order)
        afterCount <- present storage (victimClosure <> protectedClosure)
        failures @?= []
        deleted @?= Set.size victimClosure
        assertBool "physical block count decreases" (afterCount < before)
        protectedAfter <- present storage protectedClosure
        protectedAfter @?= Set.size protectedClosure
  , testCase "compaction can represent a mailbox with no remaining entries" $
      withStore $ \storage -> do
        root <- makeMerkle 0
          (toPTree (MaxSize 6000) (MaxNum 1024) ([] :: [HashRef]))
          (\(_,_,bytes) -> void (putBlock storage bytes))
        entries <- readLog (getBlock storage) (HashRef root)
        entries @?= ([] :: [HashRef])
  ]

oneLeafTree :: AnyStorage -> LBS.ByteString -> IO HashRef
oneLeafTree storage payload = do
  leaf <- putBlock storage payload >>= \case
    Nothing -> ioError (userError "put leaf failed")
    Just hash -> pure hash
  root <- makeMerkle 0
    (toPTree (MaxSize 6000) (MaxNum 1024) [HashRef leaf])
    (\(_,_,bytes) -> void (putBlock storage bytes))
  pure (HashRef root)

present :: AnyStorage -> Set.Set HashRef -> IO Int
present storage blocks = length . catMaybes <$>
  traverse (hasBlock storage . fromHashRef) (Set.toList blocks)
