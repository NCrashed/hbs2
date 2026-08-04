{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.Mailbox where

import HBS2.Peer.Prelude

import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.Actors.Peer
import HBS2.Data.Types.SignedBox
import HBS2.Peer.Proto
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Ref
import HBS2.Peer.Proto.Mailbox.Types

import HBS2.Storage
import HBS2.Net.Messaging.Unix
import HBS2.Misc.PrettyStuff

import HBS2.Peer.RPC.API.Peer

import PeerTypes

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.Mailbox

import Lens.Micro.Platform
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

type ForMailboxRPC m = ( MonadIO m
                       , HasRpcContext MailboxAPI RPC2Context m
                       )

instance (MonadIO m) => HandleMethod m RpcMailboxPoke where

  handleMethod key = do
    debug "rpc.RpcMailboxPoke"

instance Monad m => HasRpcContext MailboxAPI RPC2Context (ResponseM UNIX (ReaderT RPC2Context m)) where
  getRpcContext = lift ask

instance (ForMailboxRPC m) => HandleMethod m RpcMailboxCreate where

  handleMethod (puk, t) = do
    AnyMailboxService mbs <- getRpcContext @MailboxAPI @RPC2Context <&> rpcMailboxService
    debug $ "rpc.RpcMailboxCreate" <+> pretty (AsBase58 puk) <+> pretty t
    mailboxCreate @HBS2Basic mbs t puk

instance (ForMailboxRPC m) => HandleMethod m RpcMailboxSetPolicy where

  handleMethod (puk, sbox) = do
    AnyMailboxService mbs <- getRpcContext @MailboxAPI @RPC2Context <&> rpcMailboxService
    debug $ "rpc.RpcMailboxSetPolicy" <+> pretty (AsBase58 puk)
    -- puk was taken and ignored entirely: the mailbox came from the box alone,
    -- so `mailbox set-policy KEY ... file` set the policy of whatever the BOX
    -- named and reported success for KEY. It costs one signature check to say so
    -- instead, which is affordable on a method an operator invokes by hand.
    case unboxSignedBox0 @(SetPolicyPayload HBS2Basic) sbox of
      Nothing -> pure (Left (MailboxAuthError "invalid signature"))
      Just (_, spp)
        | sppMailboxKey spp /= puk ->
            pure (Left (MailboxAuthError "the policy is signed for another mailbox"))
        | otherwise -> mailboxSetPolicy @HBS2Basic mbs sbox

instance (ForMailboxRPC m) => HandleMethod m RpcMailboxGetStatus where

  handleMethod puk = do
    AnyMailboxService mbs <- getRpcContext @MailboxAPI @RPC2Context <&> rpcMailboxService
    debug $ "rpc.RpcMailboxGetStatus" <+> pretty (AsBase58 puk)
    mailboxGetStatus @HBS2Basic mbs (MailboxRefKey puk)

instance (ForMailboxRPC m) => HandleMethod m RpcMailboxFetch where

  handleMethod puk = do
    AnyMailboxService mbs <- getRpcContext @MailboxAPI @RPC2Context <&> rpcMailboxService
    debug $ "rpc.RpcMailboxFetch" <+> pretty (AsBase58 puk)
    mailboxFetch @HBS2Basic mbs (MailboxRefKey puk)

instance (ForMailboxRPC m) => HandleMethod m RpcMailboxDelete where

  handleMethod puk = do
    AnyMailboxService mbs <- getRpcContext @MailboxAPI @RPC2Context <&> rpcMailboxService
    debug $ "rpc.RpcMailboxDelete" <+> pretty (AsBase58 puk)
    mailboxDelete @HBS2Basic mbs puk

instance (ForMailboxRPC m) => HandleMethod m RpcMailboxList where

  handleMethod _ = do
    AnyMailboxService mbs <- getRpcContext @MailboxAPI @RPC2Context <&> rpcMailboxService
    mailboxListBasic @HBS2Basic mbs

instance (ForMailboxRPC m) => HandleMethod m RpcMailboxSend where

  handleMethod (stamp, mess) = do
    co <- getRpcContext @MailboxAPI @RPC2Context
    let w = rpcMailboxService co
    debug $ "rpc.RpcMailboxSend"
    mailboxSendMessage w stamp mess

instance (ForMailboxRPC m) => HandleMethod m RpcMailboxDeleteMessages where

  handleMethod sbox = do
    co <- getRpcContext @MailboxAPI @RPC2Context
    let w = rpcMailboxService co
    debug $ "rpc.RpcMailboxDeleteMessages"
    mailboxSendDelete w sbox

instance (ForMailboxRPC m) => HandleMethod m RpcMailboxGet where

  handleMethod mbox = do
    RPC2Context{..} <- getRpcContext @MailboxAPI @RPC2Context
    debug $ "rpc.RpcMailboxGet"
    getRef rpcStorage (MailboxRefKey @HBS2Basic mbox)
             <&> fmap HashRef


