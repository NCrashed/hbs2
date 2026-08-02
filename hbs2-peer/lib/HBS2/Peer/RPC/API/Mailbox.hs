{-# Language UndecidableInstances #-}
module HBS2.Peer.RPC.API.Mailbox where

import HBS2.Peer.Prelude
import HBS2.Net.Proto.Service
import HBS2.Net.Messaging.Unix (UNIX)
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Data.Types.SignedBox

import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Peer.Proto.Mailbox

import Data.ByteString.Lazy ( ByteString )
import Data.ByteString qualified as BS
import Codec.Serialise

data RpcMailboxPoke
data RpcMailboxCreate
data RpcMailboxSetPolicy
data RpcMailboxDelete
data RpcMailboxGetStatus
data RpcMailboxFetch
data RpcMailboxList
data RpcMailboxSend
data RpcMailboxDeleteMessages
data RpcMailboxGet

type MailboxAPI = '[ RpcMailboxPoke
                   , RpcMailboxCreate
                   , RpcMailboxSetPolicy
                   , RpcMailboxDelete
                   , RpcMailboxGetStatus
                   , RpcMailboxFetch
                   , RpcMailboxList
                   , RpcMailboxSend
                   , RpcMailboxDeleteMessages
                   , RpcMailboxGet
                   ]

-- Bumped when four methods stopped answering () for everything (see below). The
-- request side did not change, so a peer would still have acted on an old
-- client's call and only the REPLY would have failed to decode: the client would
-- report a failure for something that happened. A new id makes the mismatch
-- refuse to connect instead, which is the failure worth having between binaries
-- that ship in one release.
type MailboxAPIProto =  0x056091510d3b2eca


instance HasProtocol UNIX  (ServiceProto MailboxAPI UNIX) where
  type instance ProtocolId (ServiceProto MailboxAPI UNIX) = MailboxAPIProto
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

type instance Input RpcMailboxPoke   = ()
type instance Output RpcMailboxPoke  = ()

-- The four Outputs below were () and [..], and every one of them had an
-- Either MailboxServiceError behind it in the service class that the handler
-- threw away with `void` or `fromRight mempty`. So `hbs2-peer mailbox create`
-- printed () and exited 0 whether the row was written, the worker's database was
-- not ready, or SQLite threw; `mailbox list` printed an empty list for "no
-- mailboxes" and for "database not ready" alike; and `mailbox send` reported
-- success for a message whose signature does not verify. The information was
-- there and was discarded at the boundary.
type instance Input RpcMailboxCreate   = (PubKey 'Sign HBS2Basic, MailboxType)
type instance Output RpcMailboxCreate  = Either MailboxServiceError ()

type instance Input RpcMailboxSetPolicy = (PubKey 'Sign HBS2Basic, SignedBox (SetPolicyPayload HBS2Basic) HBS2Basic)
type instance Output RpcMailboxSetPolicy  = Either MailboxServiceError HashRef

type instance Input RpcMailboxDelete   = (PubKey 'Sign HBS2Basic)
type instance Output RpcMailboxDelete  = Either MailboxServiceError ()

type instance Input RpcMailboxGetStatus  = (PubKey 'Sign HBS2Basic)
type instance Output RpcMailboxGetStatus = Either MailboxServiceError (Maybe (MailBoxStatusPayload 'HBS2Basic))

type instance Input RpcMailboxFetch = (PubKey 'Sign HBS2Basic)
type instance Output RpcMailboxFetch = Either MailboxServiceError ()

type instance Input RpcMailboxList   = ()
type instance Output RpcMailboxList  = Either MailboxServiceError [(MailboxRefKey 'HBS2Basic, MailboxType)]

type instance Input RpcMailboxSend  = (Message HBS2Basic)
type instance Output RpcMailboxSend = Either MailboxServiceError ()

type instance Input RpcMailboxDeleteMessages  = (SignedBox (DeleteMessagesPayload HBS2Basic) HBS2Basic)
type instance Output RpcMailboxDeleteMessages = (Either MailboxServiceError ())

type instance Input RpcMailboxGet = (PubKey 'Sign HBS2Basic)
type instance Output RpcMailboxGet = (Maybe HashRef)

