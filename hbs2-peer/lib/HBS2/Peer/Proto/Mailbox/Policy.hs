{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2.Peer.Proto.Mailbox.Policy where

import HBS2.Prelude.Plated

import HBS2.Peer.Proto.Mailbox.Types
-- import HBS2.Peer.Proto.Mailbox


class ForMailbox s => IsAcceptPolicy s a where

  policyAcceptPeer :: forall m . MonadIO m
                   => a
                   -> PubKey 'Sign s -- ^ peer
                   -> m Bool


  policyAcceptSender :: forall m . MonadIO m
                     => a
                     -> PubKey 'Sign s -- ^ sender
                     -> m Bool

  policyAcceptMessage :: forall m . MonadIO m
                      => a
                      -> Sender s
                      -> MessageContent s
                      -> m Bool

  -- | How much proof-of-work this policy charges, in leading zero bits.
  --
  -- Declared here and CHECKED BY THE CALLER, which is deliberate. Verifying a
  -- stamp needs the mailbox key and the message bytes, and neither is the
  -- policy's business: a policy says what it wants, the peer holding the
  -- message finds out whether it got it. Rate limits and quotas will land the
  -- same way, for the same reason -- the state they count lives in the peer.
  --
  -- Zero by default, so a policy written before any of this existed charges
  -- nothing and needs no change.
  policyPoW :: forall m . MonadIO m => a -> m PoWDifficulty
  policyPoW _ = pure 0


data AnyPolicy s = forall a . (ForMailbox s, IsAcceptPolicy s a) => AnyPolicy { thePolicy :: a }

instance ForMailbox s => IsAcceptPolicy s (AnyPolicy s) where
  policyAcceptPeer  (AnyPolicy p) = policyAcceptPeer @s p
  policyAcceptSender (AnyPolicy p) = policyAcceptSender @s p
  policyAcceptMessage (AnyPolicy p) = policyAcceptMessage @s p
  policyPoW (AnyPolicy p) = policyPoW @s p

