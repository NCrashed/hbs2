# Protocol

This document describes the hbs2 wire protocol at the structural
level: what messages exist, what they contain, and how they fit
together. It is meant to be sufficient for an engineer who wants to
understand the protocol shape, audit interoperability, or
re-implement a peer in another language.

It is **not** a byte-exact specification. CBOR encoding details for
each field type are inherited from the `serialise` Haskell library
and would require an independent audit before being treated as
normative. Sections below cite source files for each definition so
the canonical form can be consulted directly.

## Status

This is the first written version of the protocol spec. It is
derived from the current source tree, not from a separate design
document. Where the source has multiple versions of a message (v0,
v1, v2), all are listed; the wire format must support all of them.
Areas that need deeper verification before being treated as
normative are tagged **(verify)**.

## Foundations

### Encoding

All wire messages are encoded with CBOR via the
[`serialise`](https://hackage.haskell.org/package/serialise)
package. Every protocol type derives `Serialise` (directly or via
`Generic`).

The CBOR encoding for each type follows `serialise`'s default
generic derivation rules. A non-Haskell implementation must match
those rules; the
[`serialise` source](https://hackage.haskell.org/package/serialise)
documents them.

Only one protocol type ships with a hand-rolled `Serialise` instance:
`GroupKey 'Symm s` in
[`hbs2-core/lib/HBS2/Net/Auth/GroupKeySymm.hs:194`](hbs2-core/lib/HBS2/Net/Auth/GroupKeySymm.hs#L194).
It carries a forward-compatibility scheme: the encoder writes a
`GroupKeySymmV1` payload, then a version tag (currently `2`), then
extension data (group-key id scheme, id, and timestamp). The decoder
reads V1, checks for remaining bytes, then optionally decodes the
extension. Other implementations must match this layout for symmetric
group keys; everything else uses default Generic derivation.

### Hashing

The base hash type is `Hash HbSync`, defined in
[`hbs2-core/lib/HBS2/Hash.hs`](hbs2-core/lib/HBS2/Hash.hs).

- Algorithm: **Blake2b-256**. Plain (no salt, no personalisation, no
  domain separation), applied to the raw byte string. The type alias
  is defined as `HashType HbSync = Blake2b_256`
  ([`Hash.hs:39-40`](hbs2-core/lib/HBS2/Hash.hs#L39-L40)) and the
  `Hashed HbSync ByteString` instance computes
  `hash bytes :: Digest Blake2b_256` from `cryptonite`'s
  `Crypto.Hash`.
- Wire representation: 32-byte `ByteString`.
- Display form: base58 (used in URLs like `hbs2://<base58>`).

`HashRef` (in `hbs2-core/lib/HBS2/Data/Types/Refs.hs`) is a thin
newtype around `Hash HbSync` used for references to merkle trees and
other named hashes.

### Asymmetric cryptography

Two key kinds, both backed by NaCl (libsodium) bindings via
`saltine`:

- **Signing keys**: Ed25519. Type tag `'Sign`. Used for peer identity,
  refchan writers, reflog owners, LWWRef writers.
- **Encryption keys**: Curve25519. Type tag `'Encrypt`. Used for
  group key recipient lists.

Public keys appear in messages as raw byte strings of the appropriate
length. Signatures are detached Ed25519 signatures (64 bytes).

### Signed envelope

`SignedBox p s` in
[`hbs2-core/lib/HBS2/Data/Types/SignedBox.hs`](hbs2-core/lib/HBS2/Data/Types/SignedBox.hs)
is the standard signed-payload envelope. Logically:

```
SignedBox p s = (PubKey 'Sign s, ByteString, Signature s)
```

where `ByteString` is the CBOR-encoded payload of type `p`, and the
signature is over that byte string. Verification checks the
signature against the public key.

This envelope is used everywhere a peer publishes something that
others must authenticate: reflog updates, refchan proposals, refchan
accepts, LWWRef updates, peer handshake responses.

### Encrypted envelope

`EncryptedBox t` in
[`hbs2-core/lib/HBS2/Data/Types/EncryptedBox.hs`](hbs2-core/lib/HBS2/Data/Types/EncryptedBox.hs)
is an opaque encrypted byte string. The decryption procedure depends
on the surrounding context (group key kind, recipient identity).

## Merkle trees

Large objects are represented as merkle trees. The wire shape is in
[`hbs2-core/lib/HBS2/Merkle.hs`](hbs2-core/lib/HBS2/Merkle.hs).

A `MTree a` is a recursive structure:

- `MNode MNodeData [Hash HbSync]` for an internal node, holding
  metadata and the hashes of its children.
- `MLeaf a` for a leaf, carrying a payload of type `a`.

`MTreeAnn a` annotates a merkle tree with metadata and an encryption
descriptor:

```
MTreeAnn
  { _mtaMeta  :: AnnMetaData
  , _mtaCrypt :: MTreeEncryption
  , _mtaTree  :: MTree a
  }
```

`AnnMetaData` carries optional application-defined metadata (a short
text blob or a reference to another block).

`MTreeEncryption` records how the tree contents are encrypted (see
[Encryption](#encryption)):

- `NullEncryption`
- `CryptAccessKeyNaClAsymm` (asymmetric group key)
- `EncryptGroupNaClSymm1` (symmetric group key, v1)
- `EncryptGroupNaClSymm2` (symmetric group key, v2 with options)

## Protocols

Each protocol is implemented as a module in
[`hbs2-peer/lib/HBS2/Peer/Proto/`](hbs2-peer/lib/HBS2/Peer/Proto).
Each module exports one or more sum types whose constructors are
the wire messages.

The protocol stack runs on top of a generic messaging layer that
multiplexes by protocol identifier. The identifier is a `Nat`
assigned to each protocol's `HasProtocol` instance; the layer
dispatches incoming bytes to the correct decoder by reading the
identifier first. **(verify)** the exact framing.

### Peer handshake

File: `Peer.hs`. Establishes that two peers can reach each other and
that each holds the private key it claims.

```
data PeerHandshake e
  = PeerPing PingNonce
  | PeerPong PingNonce (Signature (Encryption e)) (PeerData e)
```

`PeerPing` carries a random nonce. The recipient responds with
`PeerPong` containing the same nonce, a signature over it (proving
ownership of the peer's signing key), and the peer's identity data
(`PeerData`: signing key plus the peer's own nonce).

### Peer discovery

#### PeerAnnounce

File: `PeerAnnounce.hs`. Multicast advertisement of presence.

```
data PeerAnnounce e = PeerAnnounce PeerNonce
```

Sent over UDP multicast on the LAN. Receivers respond by initiating
a handshake.

#### PeerExchange (PEX)

File: `PeerExchange.hs`. Peer address swap; two protocol versions.

```
data PeerExchange e
  = PeerExchangeGet    (Nonce (PeerExchange e))
  | PeerExchangePeers  (Nonce (PeerExchange e)) [IPAddrPort e]
  | PeerExchangeGet2   (Nonce (PeerExchange e))
  | PeerExchangePeers2 (Nonce (PeerExchange e)) [PeerAddr e]
```

`Get` requests a peer list; `Peers` returns it. The v2 variant
returns structured peer addresses (`PeerAddr`) that carry more than
just IP and port.

#### PeerMeta

File: `PeerMeta.hs`. Capability and version exchange.

```
data PeerMetaProto e
  = GetPeerMeta
  | ThePeerMeta AnnMetaData
```

Reuses `AnnMetaData` from the merkle-tree metadata machinery.

### Block plane

The block plane is the data-replication layer. Three protocols.

#### BlockInfo

File: `BlockInfo.hs`. Block-size query.

```
data BlockInfo e
  = GetBlockSize (Hash HbSync)
  | NoBlock      (Hash HbSync)
  | BlockSize    (Hash HbSync) Integer
```

A peer asks whether another peer holds a block and how large it is.

#### BlockAnnounce

File: `BlockAnnounce.hs`. Block-availability gossip.

```
data BlockInfoMeta
  = NoBlockInfoMeta
  | BlockInfoMetaShort ByteString
  | BlockInfoMetaRef   (Hash HbSync)

data BlockAnnounceInfo e = BlockAnnounceInfo
  { _biNonce :: BlockInfoNonce  -- Word64
  , _biMeta  :: BlockInfoMeta
  , _biSize  :: Integer
  , _biHash  :: Hash HbSync
  }

data BlockAnnounce e
  = BlockAnnounce PeerNonce (BlockAnnounceInfo e)
```

Sent by a peer when it acquires a new block, so that interested
peers know to fetch it.

#### BlockChunks

File: `BlockChunks.hs`. Reliable block transfer.

```
data BlockChunksProto e
  = BlockGetAllChunks (Hash HbSync) ChunkSize
  | BlockGetChunks    (Hash HbSync) ChunkSize Word32 Word32
  | BlockNoChunks
  | BlockChunk        ChunkNum ByteString
  | BlockLost

data BlockChunks e = BlockChunks (Cookie e) (BlockChunksProto e)
```

A block is fetched in chunks of `ChunkSize` bytes. The cookie
correlates request and response messages on the same transport.

### Reference plane

The reference plane builds mutable named handles on top of the
immutable block plane. Three flavors with very different semantics.

#### RefLog

File: `RefLog.hs`. Append-only signed log; one writer, many readers.

```
data RefLogRequest e
  = RefLogRequest  { refLog      :: PubKey 'Sign (Encryption e) }
  | RefLogResponse { refLog      :: PubKey 'Sign (Encryption e)
                   , refLogValue :: Hash HbSync
                   }

data RefLogUpdate e = RefLogUpdate
  { _refLogId       :: PubKey 'Sign (Encryption e)
  , _refLogUpdNonce :: Nonce (RefLogUpdate e)
  , _refLogUpdData  :: ByteString
  , _refLogUpdSign  :: Signature (Encryption e)
  }
```

An update is identified by the public signing key (`_refLogId`),
carries an opaque payload (`_refLogUpdData`), a per-update nonce,
and a signature over the nonce and data. Anyone with the public key
can verify; only the holder of the matching private key can produce
valid updates. Used by `hbs2-git`.

#### RefChan

Files: `RefChan.hs`, `RefChan/*.hs`. Quorum-based reference channel.

The channel is configured by a *head block* that names voting peers,
authors, readers, and parameters. There are three head-block
versions on the wire:

```
data RefChanHeadBlock e
  = RefChanHeadBlockSmall
      { _refChanHeadVersion    :: Integer
      , _refChanHeadQuorum     :: Integer
      , _refChanHeadWaitAccept :: Integer
      , _refChanHeadPeers      :: HashMap (PubKey 'Sign s) Weight
      , _refChanHeadAuthors    :: HashSet (PubKey 'Sign s)
      }
  | RefChanHeadBlock1   { ... , _refChanHeadReaders'   :: HashSet (PubKey 'Encrypt s) }
  | RefChanHeadBlock2   { ... , _refChanHeadNotifiers' :: HashSet (PubKey 'Sign s) }
```

v0 covers writers, voters, weights, and quorum. v1 adds an explicit
list of reader (encryption) keys for encrypted channels. v2 adds
notifier keys that can receive notifications without being voters.

Update flow is a two-phase commit:

```
data RefChanUpdate e
  = Propose (RefChanId e) (SignedBox (ProposeTran e) (Encryption e))
  | Accept  (RefChanId e) (SignedBox (AcceptTran  e) (Encryption e))

data ProposeTran e
  = ProposeTran HashRef (SignedBox ByteString (Encryption e))

data AcceptTran e
  = AcceptTran1                  HashRef HashRef
  | AcceptTran2 (Maybe AcceptTime) HashRef HashRef
```

An author broadcasts `Propose` carrying a signed payload. Voting
peers verify and broadcast `Accept` referencing the proposal hash
and the head hash. When the quorum threshold is met within
`_refChanHeadWaitAccept` seconds, the proposal is considered applied.

`AcceptTran` has two versions: v1 carries proposal hash and head
hash, v2 adds an optional timestamp.

Channels also support notifications:

```
data RefChanNotify e
  = Notify        (RefChanId e) (SignedBox ByteString (Encryption e))
  | ActionRequest (RefChanId e) RefChanActionRequest
```

`Notify` broadcasts an application-defined signed payload to
subscribers. `ActionRequest` is a control message with exactly two
constructors
([`RefChan/Types.hs:87-90`](hbs2-peer/lib/HBS2/Peer/Proto/RefChan/Types.hs#L87-L90)):

```
data RefChanActionRequest
  = RefChanAnnounceBlock HashRef
  | RefChanFetch         HashRef
```

`RefChanAnnounceBlock` asks subscribers to (re-)announce a block;
`RefChanFetch` asks them to fetch a block they may be missing.

#### LWWRef

File: `LWWRef.hs`. Lightweight last-writer-wins register.

```
data LWWRef s = LWWRef
  { lwwSeq   :: Word64
  , lwwValue :: HashRef
  , lwwProof :: Maybe HashRef
  }

data LWWRefProtoReq s
  = LWWProtoGet (LWWRefKey s)
  | LWWProtoSet (LWWRefKey s) (SignedBox (LWWRef s) s)

newtype LWWRefKey s = LWWRefKey { fromLwwRefKey :: PubKey 'Sign s }
```

The reference identity is the signing key; the value is a hash plus
a monotonic sequence number. A higher `lwwSeq` wins on merge. Used
for cheap pointer-to-the-real-thing where a full reflog or refchan
would be overkill.

#### AnyRef

File: `AnyRef.hs`. Polymorphic reference key wrapper used in generic
ref operations.

```
newtype AnyRefKey t s = AnyRefKey (PubKey 'Sign s)
```

The type tag `t` is a phantom type that exists only at the Haskell
type level. There is no explicit `Serialise` instance, so the
newtype encodes identically to its inner `PubKey 'Sign s`; the tag
does **not** appear on the wire and cannot be recovered from a wire
message alone. Consuming code knows the tag from context (which RPC
endpoint or which protocol message carried the key).

The tag does, however, affect **content addressing**: the
`Hashed HbSync (AnyRefKey t s)` instance prefixes the serialised
pubkey with the literal byte string `anyref|` before hashing
([`AnyRef.hs:23-24`](hbs2-peer/lib/HBS2/Peer/Proto/AnyRef.hs#L23-L24)),
so the content hash of an `AnyRefKey` differs from the content hash
of the bare pubkey. The tag still does not leak through, since the
prefix is constant across all tag values.

## Encryption

Two group-key schemes are supported.

### Asymmetric group key

File: `hbs2-core/lib/HBS2/Net/Auth/GroupKeyAsymm.hs`.

```
data AccessKey s = AccessKeyNaClAsymm
  { permitted :: [(PubKey 'Encrypt s, EncryptedBox (KeyringEntry s))]
  }

data GroupKey 'Asymm s = GroupKeyNaClAsymm
  { recipientPk :: PubKey 'Encrypt s
  , accessKey   :: AccessKey s
  }
```

A group key consists of a list of (recipient public key, payload
encrypted to that recipient). Each recipient decrypts the entry
addressed to them and recovers a `KeyringEntry` which holds the
actual signing/encryption keys for the group. Used for cases where
the recipient set is small and explicit.

### Symmetric group key

File: `hbs2-core/lib/HBS2/Net/Auth/GroupKeySymm.hs`.

A random symmetric secret is generated and encrypted under each
authorized reader's public encryption key. Readers decrypt the
secret and use it to decrypt the data. Wraps a `MTreeAnn` payload
whose `_mtaCrypt` field indicates `EncryptGroupNaClSymm1` or
`EncryptGroupNaClSymm2`.

The symmetric primitive is **NaCl `crypto_secretbox`**
(XSalsa20-Poly1305), invoked via saltine's
`Crypto.Saltine.Core.SecretBox.secretbox` / `secretboxOpen` in
[`GroupKeySymm.hs`](hbs2-core/lib/HBS2/Net/Auth/GroupKeySymm.hs).
The `EncryptGroupNaClSymm2` variant additionally derives per-block
keys via SipHash-based block indexing
(`EncryptGroupNaClSymmBlockSIP`) so that different blocks of the
same tree are encrypted under different nonces and keys.

### Transport-level encryption

The messaging layer at
[`hbs2-core/lib/HBS2/Net/Messaging/Encrypted/`](hbs2-core/lib/HBS2/Net/Messaging/Encrypted)
wraps any transport (UDP, TCP, Unix) with peer-to-peer encryption.
Two pieces:

- **`ByPass.hs`** uses NaCl `crypto_box` (X25519 ECDH + XSalsa20 +
  Poly1305) via saltine's `Crypto.Saltine.Core.Box`. Each pair of
  peers precomputes a shared key (`CombinedKey`) once and then uses
  it to seal and open messages. Includes SipHash-based keyed
  indexing of trusted peers.
- **`RandomPrefix.hs`** is a small bytecode-driven obfuscation layer
  prepended to encrypted packets. The "bytecode" is a sequence of
  primitive operations (`NOP`, `LOADB`, `SKIPBI`, `ANDBI`, `ORBI`,
  `XORBI`, `ADDBI`, `SUBBI`, `MULTBI`, `REPEAT`, `RET`) interpreted
  to produce a per-packet variable-length prefix. The goal is to
  hinder naive traffic analysis and protocol fingerprinting.

This is layered below the protocol stack and is independent of the
group-key schemes used at the data layer.

## Auxiliary types

`PeerNonce` is a 64-bit unsigned integer used to identify a peer
session. `Cookie e` is a transport-level identifier (typically 32
bits) used to correlate request and response messages on the same
connection. `Nonce` types specific to a protocol family carry their
own structure (see each protocol's definition).

## What is intentionally out of scope here

- **RPC over Unix socket between hbs2-peer and local clients.** That
  is a service contract internal to a single machine, not a wire
  protocol between peers. See the
  [`hbs2-peer/lib/HBS2/Peer/RPC/API/`](hbs2-peer/lib/HBS2/Peer/RPC/API)
  modules.
- **Storage on-disk format (NCQ3).** Persistence layout, not wire
  format.
- **Git remote helper protocol.** The interaction between `git` and
  `git-remote-hbs2` is git's documented remote-helper protocol, not
  an hbs2 invention.

## Open verification items

The items tagged **(verify)** above should be confirmed against the
source before this document is treated as normative. A short list:

- Exact framing of the messaging layer that demultiplexes protocols
  by identifier.

Closing these is a good first deeper-protocol review for someone who
wants to maintain hbs2 long-term, or who is implementing a peer in
another language.
