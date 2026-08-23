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
data RpcMailboxPoWFloor

-- APPENDED, AND NEW ENTRIES GO AT THE END. A call is addressed by the method's
-- INDEX in this list ('findMethodIndex'), so inserting one above renumbers
-- everything below it and a client would call the wrong handler with the right
-- bytes.
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
                   , RpcMailboxPoWFloor
                   ]

-- Bumped when four methods stopped answering () for everything (see below). The
-- request side did not change, so a peer would still have acted on an old
-- client's call and only the REPLY would have failed to decode: the client would
-- report a failure for something that happened. A new id makes the mismatch
-- refuse to connect instead, which is the failure worth having between binaries
-- that ship in one release.
--
-- Bumped again for the proof-of-work stamp on 'RpcMailboxSend', where the
-- REQUEST changed: an old client's message would arrive as an undecodable
-- input, and a new client's call would reach an old peer that has no idea a
-- stamp exists and would send the letter without one.
--
-- NOT bumped for 'RpcMailboxPoWFloor' (PEP-23 step D), and the difference is
-- what a bump is for. Both bumps above changed the meaning of bytes an existing
-- method already exchanged, so silence was the only safe outcome. Appending a
-- method changes nothing that exists: an old peer answers a new client's call
-- with 'ErrorMethodNotFound', which 'callRpcWaitMay' reports as 'Nothing', and
-- the caller reads no floor and sends exactly what it sends today. Refusing to
-- connect over that would trade a benign degradation for an outage.
type MailboxAPIProto =  0x056091510d3b2ecb


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

-- The stamp rides beside the message rather than inside it, for the reason
-- 'SendMessageStamped' exists: the peer stores the message by value, and a
-- witness inside it would change the bytes it is named by.
type instance Input RpcMailboxSend  = (Maybe (MessageStamp HBS2Basic), Message HBS2Basic)
type instance Output RpcMailboxSend = Either MailboxServiceError ()

type instance Input RpcMailboxDeleteMessages  = (SignedBox (DeleteMessagesPayload HBS2Basic) HBS2Basic)
type instance Output RpcMailboxDeleteMessages = (Either MailboxServiceError ())

type instance Input RpcMailboxGet = (PubKey 'Sign HBS2Basic)
type instance Output RpcMailboxGet = (Maybe HashRef)

-- | The least work a message must carry to leave this machine (PEP-23 step D).
--
-- WHY A CLIENT NEEDS TO ASK. A sender solves what the destination MAILBOX
-- charges, which is in that mailbox's signed policy. Whether a peer CARRIES the
-- packet is a different number -- that peer's own @hbs2:mailbox:pow-min@ -- and
-- it is in nobody's policy. The first peer to apply it is the sender's own,
-- since a submission over this RPC runs the same forwarding rule, so a letter
-- can fail to leave the machine it was composed on.
--
-- The answer is the maximum of this peer's floor and the largest one its
-- neighbours have published in their meta (PEP-23 step C). It reaches one hop:
-- a relay further out with a higher floor is still invisible, and nothing short
-- of end-to-end feedback would change that.
--
-- Not an @Either MailboxServiceError@: there is no question to refuse. A peer
-- that cannot answer at all -- an older build without this method -- is
-- 'Nothing' at the call site, which a caller reads as no floor and sends what
-- it would have sent anyway.
type instance Input RpcMailboxPoWFloor = ()
type instance Output RpcMailboxPoWFloor = PoWDifficulty

