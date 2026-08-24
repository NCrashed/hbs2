{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.Proto.Mailbox.Types
  ( ForMailbox
  , MailboxKey
  , MailboxType(..)
  , MailBoxStatusPayload(..)
  , MailboxServiceError(..)
  , Recipient
  , Sender
  , PolicyVersion
  , MailboxMessagePredicate(..)
  , SimplePredicateExpr(..)
  , SimplePredicate(..)
  , MailBoxProto(..)
  , MailBoxProtoMessage(..)
  , Message(..)
  , MessageContent(..)
  , MessageCompression(..)
  , MessageFlags(..)
  , MessageOrigin(..)
  , MessageStamp(..)
  , MessageTimestamp(..)
  , MessageTTL(..)
  , PoWDifficulty
  , DeleteMessagesPayload(..)
  , SetPolicyPayload(..)
  , clockSkew
  , module HBS2.Net.Proto.Types
  , HashRef
  ) where

import HBS2.Prelude.Plated

import HBS2.Base58
import HBS2.Hash
import HBS2.Net.Proto.Types
import HBS2.Data.Types.Refs (HashRef(..))

import HBS2.Data.Types.SignedBox
import HBS2.Data.Types.SmallEncryptedBlock(SmallEncryptedBlock(..))
import HBS2.Net.Auth.GroupKeySymm

import Codec.Serialise
import Control.Exception
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Set
import Data.Set qualified as Set
import Data.Word

data MailboxType =
  MailboxHub | MailboxRelay
  deriving stock (Eq,Ord,Show,Generic)

instance Serialise MailboxType

instance Pretty MailboxType where
  pretty = \case
    MailboxHub -> "hub"
    MailboxRelay -> "relay"

instance FromStringMaybe MailboxType where
  fromStringMay = \case
    "hub"   -> Just MailboxHub
    "relay" -> Just MailboxRelay
    _       -> Nothing

instance IsString MailboxType where
  fromString s = fromMaybe (error "invalid MailboxType value") (fromStringMay s)

type MailboxKey s = PubKey 'Sign s

type Sender s = PubKey 'Sign s

type Recipient s = PubKey 'Sign s

type PolicyVersion = Word32

-- | Proof-of-work difficulty, in leading zero bits of the stamp hash.
--
-- Zero means none is required, which is what every mailbox says until its
-- owner writes @(pow D)@ into the policy. See "HBS2.Peer.Proto.Mailbox.PoW".
type PoWDifficulty = Word8

type ForMailbox s = ( ForGroupKeySymm s
                    , Ord (PubKey 'Sign s)
                    , ForSignedBox s
                    , Pretty (AsBase58 (PubKey 'Sign s))
                    )

data SimplePredicateExpr =
    And SimplePredicateExpr SimplePredicateExpr
  | Or  SimplePredicateExpr SimplePredicateExpr
  | Op  SimplePredicate
  | End
  deriving stock (Generic)

data SimplePredicate =
    Nop
  | MessageHashEq HashRef
  deriving stock (Generic)

data MailboxMessagePredicate =
  MailboxMessagePredicate1 SimplePredicateExpr
  deriving stock (Generic)


instance Serialise SimplePredicate
instance Serialise SimplePredicateExpr
instance Serialise MailboxMessagePredicate

newtype MessageTimestamp =
  MessageTimestamp Word64
  deriving newtype (Eq,Ord,Num,Enum,Integral,Real,Pretty,Show,Hashable)
  deriving stock Generic


newtype MessageTTL = MessageTTL Word32
  deriving newtype (Eq,Ord,Num,Enum,Integral,Real,Pretty,Show,Hashable)
  deriving stock Generic


data MessageCompression = GZip
  deriving stock (Eq,Ord,Generic,Show)

data MessageFlags =
  MessageFlags1
  { messageCreated     :: MessageTimestamp
  , messageTTL         :: Maybe MessageTTL
  , messageCompression :: Maybe MessageCompression
  , messageSchema      :: Maybe HashRef -- reserved
  }
  deriving stock (Eq,Ord,Generic,Show)

type MessageRecipient s = PubKey 'Sign s

data SetPolicyPayload s =
  SetPolicyPayload
  { sppMailboxKey    :: MailboxKey s
  , sppPolicyVersion :: PolicyVersion
  , sppPolicyRef     :: HashRef -- ^ merkle tree hash of policy description file
  }
  deriving stock (Generic)

-- for Hashable
deriving instance ForMailbox s => Eq (SetPolicyPayload s)

data MailBoxStatusPayload s =
  MailBoxStatusPayload
  { mbsMailboxPayloadNonce  :: Word64
  , mbsMailboxKey           :: MailboxKey s
  , mbsMailboxType          :: MailboxType
  , mbsMailboxHash          :: Maybe HashRef
  , mbsMailboxPolicy        :: Maybe (SignedBox (SetPolicyPayload s) s)
  }
  deriving stock (Generic)

-- | How far apart two clock readings are, in whichever direction.
--
-- A named function over 'Word64' for what used to be spelled
-- @abs (now - nonce)@ at the one place that compares our clock against the nonce
-- in a 'MailBoxStatusPayload'. On an UNSIGNED type 'abs' is the identity and the
-- subtraction wraps, so a responder whose clock was one second AHEAD produced a
-- difference of about 2^64 and had its status dropped, silently. The window read
-- as ten seconds either way and was ten seconds in one direction only, which is
-- two honest peers never syncing a mailbox and nothing saying why.
--
-- Symmetric by construction, so it cannot be got wrong by passing the arguments
-- the other way round, which is the other half of why it is a function and not
-- an expression at the call site.
clockSkew :: Word64 -> Word64 -> Word64
clockSkew a b | a >= b    = a - b
              | otherwise = b - a

data DeleteMessagesPayload (s :: CryptoScheme) =
  DeleteMessagesPayload
  { dmpPredicate     :: MailboxMessagePredicate
  }
  deriving stock (Generic)

data MailBoxProtoMessage s e =
    SendMessage      (Message s) -- already has signed box
  | CheckMailbox     (Maybe Word64) (MailboxKey s)
  | MailboxStatus    (SignedBox (MailBoxStatusPayload s) s) -- signed by peer
  | DeleteMessages   (SignedBox (DeleteMessagesPayload s ) s)
  -- | The same message, carrying a proof-of-work witness (PEP-21).
  --
  -- APPENDED LAST, AND IT HAS TO STAY LAST. The derived 'Serialise' tags
  -- constructors by position, so inserting anything above this line renumbers
  -- 'DeleteMessages' and every deployed peer starts mis-reading a message that
  -- has nothing to do with proof-of-work.
  --
  -- A constructor rather than a field on 'Message', for two reasons. A field
  -- changes that record's arity, and an old peer would then fail to decode ALL
  -- mailbox traffic instead of only the stamped part of it. And the peer stores
  -- @serialise (msg :: Message s)@, so keeping the witness out of 'Message'
  -- keeps the stored bytes and their hash identical whether the message arrived
  -- stamped or plain; the witness gates submission and is not part of what
  -- replicates between a mailbox's hosts.
  --
  -- What it costs, and it is not small: relaying re-encodes the decoded value,
  -- so a peer that cannot parse this constructor does not forward it either. A
  -- stamped message travels only over a path of upgraded peers, and an old one
  -- in the path drops it in silence.
  | SendMessageStamped (Message s) (MessageStamp s)
  -- | The same delete, carrying a proof of work.
  --
  -- APPENDED LAST, and the rule above applies to it in turn: the next
  -- constructor goes below this one, not above it.
  --
  -- WHY A DELETE PAYS AT ALL, since the signer is the mailbox owner and is the
  -- one party here who can be authenticated. Because the signature proves
  -- ownership of A KEY and not of a mailbox anybody hosts: the key is RECOVERED
  -- from the signature rather than read off the wire, so "signed by the mailbox
  -- key" is satisfied by a keypair minted for the occasion, and one signature
  -- bought a flood at fan-out per hop through peers that host no mailbox. What
  -- the field is worth against that, and what it is not, is in
  -- "HBS2.Peer.Proto.Mailbox.PoW".
  --
  -- The stamp rides BESIDE the box and not inside it, for the reason it rides
  -- beside a message above, plus one of its own: the bytes stored as the delete
  -- proof are @serialise box@, which @admitDeleted@ reads back and checks the
  -- signer of, so they must be identical whether the delete arrived stamped or
  -- plain.
  --
  -- Same deployment cost as 'SendMessageStamped' and for the same reason.
  | DeleteMessagesStamped (SignedBox (DeleteMessagesPayload s) s) (MessageStamp s)
  deriving stock (Generic)

data MailBoxProto s e =
  MailBoxProtoV1 { mailBoxProtoPayload :: MailBoxProtoMessage s e }
  deriving stock (Generic)

instance ForMailbox s => Serialise (MailBoxStatusPayload s)
instance ForMailbox s => Serialise (SetPolicyPayload s)
instance ForMailbox s => Serialise (DeleteMessagesPayload s)
instance ForMailbox s => Serialise (MailBoxProtoMessage s e)
instance ForMailbox s => Serialise (MailBoxProto s e)

instance ForMailbox s => Pretty (MailBoxStatusPayload s) where
  pretty MailBoxStatusPayload{..} =
    parens $ "mailbox-status" <> line <> st
    where
      st = indent 2 $
             brackets $
             align $ vcat
                  [ parens ("nonce" <+> pretty mbsMailboxPayloadNonce)
                  , parens ("key"   <+> pretty (AsBase58 mbsMailboxKey))
                  , parens ("type"  <+> pretty mbsMailboxType)
                  , element "mailbox-tree"   mbsMailboxHash
                  , element "set-policy-payload-hash" (HashRef . hashObject . serialise <$> mbsMailboxPolicy)
                  , maybe mempty pretty spp
                  ]

      element el = maybe mempty ( \v -> parens (el <+> pretty v) )

      spp = mbsMailboxPolicy >>= unboxSignedBox0 <&> snd


instance ForMailbox s => Pretty (SetPolicyPayload s) where
  pretty SetPolicyPayload{..} = parens ( "set-policy-payload" <> line <> indent 2 (brackets w) )
    where
      w = align $
            vcat [ parens ( "version" <+> pretty sppPolicyVersion )
                 , parens ( "ref" <+> pretty sppPolicyRef )
                 ]


data MessageContent s =
  MessageContent
  { messageFlags      :: MessageFlags
  , messageRecipients :: Set (MessageRecipient s)
  , messageGK0        :: Either HashRef (GroupKey 'Symm s)
  , messageParts      :: Set HashRef
  , messageData       :: SmallEncryptedBlock ByteString
  }
  deriving stock Generic

data Message s =
  MessageBasic
  { messageContent :: SignedBox (MessageContent s) s
  }
  deriving stock Generic

-- | The proof-of-work witness for one message and one mailbox (PEP-21).
--
-- Unsigned, and it needs no authenticity of its own: it is self-verifying
-- against the message it names, and a stamp that does not verify is simply not
-- a stamp.
--
-- It names the mailbox because a message names several recipients and the work
-- is bound to one of them; a verifier cannot tell which without being told, and
-- the peer that checks the difficulty floor before gossiping has no policy for
-- any of them. One stamp is work for one mailbox: a sender who needs two
-- mailboxes that both charge has to solve twice.
--
-- It does NOT carry the difficulty it claims. The verifier counts the zero bits
-- itself, so a claim would be a field it never reads and a sender can lie in.
data MessageStamp s =
  MessageStamp1
  { msMailbox :: MailboxKey s
  , msNonce   :: Word64
  }
  deriving stock (Generic)

-- | How a message reached this peer, which is what decides whether it owes work.
--
-- NOT a wire type, and it cannot become one: it is what the peer knows about a
-- message from the path it arrived on, not something a sender gets to declare.
--
-- It exists because there are two such paths and only one of them can carry a
-- stamp. A stamp is not stored in the mailbox tree (see 'MessageStamp'), so a
-- host replicating a tree has none to offer for messages it did not receive
-- itself, and a mailbox charging @(pow D)@ would refuse everything its own
-- co-hosts hand it -- which is to say it would stop replicating.
data MessageOrigin s =
    -- | Submitted over gossip, with whatever work came with it. The path a
    -- stranger uses, and the one @(pow D)@ is charged on.
    Submitted (Maybe (MessageStamp s))
    -- | Taken out of another host's mailbox tree, where no stamp lives. Bounded
    -- instead by @(peer allow|deny)@: a status is only downloaded for a mailbox
    -- this peer hosts and only from a peer that policy accepts, so what this
    -- skips the work check for is a message a trusted co-host already holds.
  | Replicated

deriving stock instance ForMailbox s => Eq (MessageContent s)
deriving stock instance ForMailbox s => Eq (Message s)
deriving stock instance ForMailbox s => Eq (MessageStamp s)

instance Serialise MessageTimestamp
instance Serialise MessageTTL
instance Serialise MessageCompression
instance Serialise MessageFlags
instance ForMailbox s => Serialise (MessageContent s)
instance ForMailbox s => Serialise (Message s)
instance ForMailbox s => Serialise (MessageStamp s)


data MailboxServiceError =
    MailboxCreateFailed String
  | MailboxOperationError String
  | MailboxSetPolicyFailed String
  | MailboxAuthError String
  deriving stock (Typeable,Show,Generic)

instance Serialise MailboxServiceError
instance Exception MailboxServiceError


