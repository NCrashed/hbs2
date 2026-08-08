-- | The parsed policy a mailbox is answered under, and when it stops counting.
--
-- Two claims are being made and both need pinning. The first is that the work
-- stops repeating: reading a policy is a getBlock, a signature check, a merkle
-- read and two parses, and it sat on two paths a stranger drives at whatever
-- rate they like. The second is that the saving is not bought with staleness:
-- the hash is the stamp, so a policy the owner has replaced can never be the
-- one applied.
--
-- The counted action is what makes the first claim testable at all. A cache
-- that answers correctly and re-reads every time passes every test written
-- about its ANSWERS, and that is the exact bug this module exists to prevent.
module MailboxPolicyCacheSpec (tests) where

import HBS2.Peer.Proto.Mailbox.PolicyCache

import HBS2.Hash
import HBS2.Data.Types.Refs (HashRef(..))

import Data.ByteString.Char8 qualified as B8
import Data.HashMap.Strict qualified as HM
import UnliftIO

import Test.Tasty
import Test.Tasty.HUnit

-- | A stand-in for a policy hash.
mh :: Int -> HashRef
mh = HashRef . hashObject . B8.pack . show

-- | A stand-in for a mailbox key.
type Mbox = String

-- | A reader that says how many times it actually ran.
counting :: IO (IORef Int, v -> IO v)
counting = do
  n <- newIORef 0
  pure (n, \v -> modifyIORef' n (+1) >> pure v)

tests :: TestTree
tests = testGroup "mailbox: the parsed policy cache"
  [ testCase "an empty cache holds no answer" $
      cachedAt "a" (mh 1) (mempty @(PolicyMemory Mbox String)) @?= Nothing

  , testCase "what was remembered under a hash is answered under it" $
      cachedAt "a" (mh 1) (rememberPolicy "a" (mh 1) "deny" mempty) @?= Just "deny"

  , testCase "a different hash is NOT a worse answer, it is no answer" $ do
      -- The whole safety argument. The owner has rewritten the policy, the
      -- row's hash has moved, and the entry under the old hash describes a
      -- policy that has been withdrawn. Serving it would be applying a rule
      -- nobody holds any more, which is worse than any amount of re-parsing.
      cachedAt "a" (mh 2) (rememberPolicy "a" (mh 1) "allow" mempty) @?= Nothing

  , testCase "one mailbox is not another" $
      cachedAt "b" (mh 1) (rememberPolicy "a" (mh 1) "allow" mempty) @?= Nothing

  , testCase "rewriting a policy replaces the entry, it does not add one" $ do
      -- The bound. Keyed by the hash instead, an owner who edits their policy
      -- hourly would leave a day's worth of parsed policies behind, and there
      -- is nothing that would ever remove them.
      let m = rememberPolicy "a" (mh 3) "third"
                (rememberPolicy "a" (mh 2) "second"
                  (rememberPolicy "a" (mh 1) "first" mempty))
      HM.size m @?= 1
      cachedAt "a" (mh 3) m @?= Just "third"
      cachedAt "a" (mh 1) m @?= Nothing

  , testCase "the second request does not read the policy again" $ do
      -- What the module is for. Without this, an unmetered CheckMailbox buys a
      -- merkle read and two parses out of the deferred pool every other
      -- protocol shares.
      c <- newPolicyCache @IO @Mbox @String
      (n, read') <- counting
      a <- policyAt c "a" (mh 1) (read' "deny")
      b <- policyAt c "a" (mh 1) (read' "deny")
      a @?= "deny"
      b @?= "deny"
      readIORef n >>= (@?= 1)

  , testCase "a thousand requests are still one read" $ do
      -- Stated separately from the case above, because "twice is once" is
      -- satisfied by a cache of one entry that a third request evicts, and
      -- what a stranger sends is not two packets.
      c <- newPolicyCache @IO @Mbox @String
      (n, read') <- counting
      mapM_ (\_ -> policyAt c "a" (mh 1) (read' "deny")) [1..1000::Int]
      readIORef n >>= (@?= 1)

  , testCase "a changed policy IS read again" $ do
      -- The other half, and the one that would make this module dangerous if
      -- it failed: the saving must not survive the thing it was taken under.
      c <- newPolicyCache @IO @Mbox @String
      (n, read') <- counting
      _ <- policyAt c "a" (mh 1) (read' "allow")
      v <- policyAt c "a" (mh 2) (read' "deny")
      v @?= "deny"
      readIORef n >>= (@?= 2)

  , testCase "and the old policy is not answered with afterwards" $ do
      -- Belt and braces on the case above: re-reading is not enough if the
      -- entry under the old hash also survives to answer a stray request.
      c <- newPolicyCache @IO @Mbox @String
      (_, read') <- counting
      _ <- policyAt c "a" (mh 1) (read' "allow")
      _ <- policyAt c "a" (mh 2) (read' "deny")
      (n2, read2) <- counting
      v <- policyAt c "a" (mh 1) (read2 "allow-again")
      v @?= "allow-again"
      readIORef n2 >>= (@?= 1)

  , testCase "two mailboxes are read once each, not once between them" $ do
      c <- newPolicyCache @IO @Mbox @String
      (n, read') <- counting
      a <- policyAt c "a" (mh 1) (read' "a-policy")
      b <- policyAt c "b" (mh 1) (read' "b-policy")
      -- The same hash for both, which is the case a key that ignored the
      -- mailbox would get right by accident and a key that ignored the hash
      -- would get wrong: these must be two entries and two reads.
      a @?= "a-policy"
      b @?= "b-policy"
      readIORef n >>= (@?= 2)
      a' <- policyAt c "a" (mh 1) (read' "not-this")
      a' @?= "a-policy"
      readIORef n >>= (@?= 2)
  ]
