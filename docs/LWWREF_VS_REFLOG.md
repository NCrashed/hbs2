# LWWRef vs RefLog: Understanding HBS2 Reference Types

HBS2 provides two types of mutable references for distributed systems: **LWWRef** and **RefLog**. Both serve similar purposes but have different consistency models and use cases.

## Quick Comparison

| Feature | RefLog | LWWRef |
|---------|--------|--------|
| **Full Name** | Reference Log | Last-Write-Wins Reference |
| **Consistency Model** | Sequential (ordered transactions) | Last-Write-Wins (conflict resolution by timestamp) |
| **Conflict Resolution** | Sequential numbers enforce order | Higher sequence number wins |
| **Use Case** | Single writer with strict ordering | Multiple writers with eventual consistency |
| **Git Implementation** | Used by hbs2-git3 for repository state | Used for distributed mutable pointers |

---

## RefLog (Reference Log)

### Definition

**RefLog** is a **permanent mutable reference** with a permanent ID corresponding to a public signing key. Its value is calculated from a set of cryptographically signed "reference update" transactions.

### Key Characteristics

1. **Single Writer**: Only transactions signed by the owner's private key are accepted
2. **Sequential Ordering**: Each transaction has a Sequential Number establishing order
3. **Transaction Log**: Maintains a log of all signed transactions
4. **Eventually Consistent**: All peers will eventually have the same value (if there's only one writer)

### Data Structure

```haskell
newtype RefLogKey s = RefLogKey { fromRefLogKey :: PubKey 'Sign s }

data RefLogUpdate e = RefLogUpdate
  { _refLogId       :: PubKey 'Sign (Encryption e)
  , _refLogUpdNonce :: Nonce (RefLogUpdate e)
  , _refLogUpdData  :: ByteString           -- Transaction payload
  , _refLogUpdSign  :: Signature (Encryption e)
  }
```

### How It Works

1. **Identity**: RefLog is identified by a public signing key
2. **Updates**: Owner creates signed transactions with sequential numbers
3. **Verification**: Peers verify signature before accepting transaction
4. **State Calculation**: Current value is computed from the set of valid transactions
5. **Ordering**: Sequential numbers determine transaction order

### Use in hbs2-git3

RefLog stores a reference to a Merkle tree containing:
- All repository branches
- All git objects accessible from those branches
- Repository metadata (group keys for encryption, etc.)

Example from git repository manifest:
```
(manifest
  (hbs2-git 3)
  (seed 12345)
  (public)
  (reflog "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP")
)
```

### Configuration Example

```
; Subscribe to a reflog
poll reflog 1 "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"
```

---

## LWWRef (Last-Write-Wins Reference)

### Definition

**LWWRef** is a **Last-Write-Wins** mutable reference that resolves conflicts using sequence numbers and timestamps. The reference with the highest sequence number is considered current.

### Key Characteristics

1. **Conflict Resolution**: Higher sequence number automatically wins
2. **Optional Proof**: Can include proof of previous state
3. **Simpler Protocol**: Less overhead than RefLog's transaction log
4. **Eventually Consistent**: Converges to the value with highest sequence

### Data Structure

```haskell
newtype LWWRefKey s = LWWRefKey { fromLwwRefKey :: PubKey 'Sign s }

data LWWRef (s :: CryptoScheme) = LWWRef
  { lwwSeq      :: Word64        -- Sequence number
  , lwwValue    :: HashRef       -- Current value
  , lwwProof    :: Maybe HashRef -- Optional proof of previous state
  }
```

The entire `LWWRef` is wrapped in a `SignedBox` for cryptographic verification.

### How It Works

1. **Identity**: LWWRef is identified by a public signing key
2. **Updates**: Writer creates signed box with incremented sequence number
3. **Conflict Resolution**: When multiple values exist, highest `lwwSeq` wins
4. **Gossip Protocol**: Updates spread through the network via gossip
5. **Verification**: Signature ensures authenticity

### Use Cases

- **Distributed Pointers**: Mutable references that can be updated by owner
- **Repository Metadata**: Points to the "head" of a repository structure
- **Configuration References**: Distributed configuration that needs updates

### Configuration Example

```
; Subscribe to an LWWRef
poll lwwref 1 "3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC"
```

---

## Detailed Comparison

### 1. Consistency Model

**RefLog:**
- Strong sequential consistency
- All transactions are ordered
- State is deterministic from transaction log
- Good for audit trails

**LWWRef:**
- Eventual consistency
- Latest sequence wins
- No transaction history
- Good for simple mutable pointers

### 2. Storage Overhead

**RefLog:**
- Stores all transactions (higher storage cost)
- Requires transaction log processing
- History is preserved

**LWWRef:**
- Stores only current value (lower storage cost)
- Simple replacement on update
- History is discarded (unless using proof chain)

### 3. Network Protocol

**RefLog:**
```haskell
data RefLogRequest e =
    RefLogRequest  { refLog :: PubKey 'Sign (Encryption e) }
  | RefLogResponse { refLog :: PubKey 'Sign (Encryption e)
                   , refLogValue :: Hash HbSync }

data RefLogUpdate e =
  RefLogUpdate
    { _refLogId       :: PubKey 'Sign (Encryption e)
    , _refLogUpdNonce :: Nonce (RefLogUpdate e)
    , _refLogUpdData  :: ByteString
    , _refLogUpdSign  :: Signature (Encryption e)
    }
```

**LWWRef:**
```haskell
data LWWRefProtoReq (s :: CryptoScheme) =
    LWWProtoGet (LWWRefKey s)
  | LWWProtoSet (LWWRefKey s) (SignedBox (LWWRef s) s)
```

### 4. Update Process

**RefLog:**
1. Create new transaction with incremented sequence number
2. Sign transaction with private key
3. Broadcast transaction to network
4. Peers verify signature and sequence
5. Peers add transaction to their log
6. Peers recalculate reference value

**LWWRef:**
1. Increment sequence number
2. Create new LWWRef with updated value
3. Sign entire structure in SignedBox
4. Broadcast via gossip protocol
5. Peers compare sequence numbers
6. Peers keep version with highest sequence

---

## When to Use Each

### Use RefLog When:

- ✅ You need strict ordering of updates
- ✅ You want audit trail of all changes
- ✅ Single writer scenario
- ✅ Sequential consistency is required
- ✅ Using hbs2-git (it uses RefLog internally)

**Example:** Git repository state, where each push is a transaction that must be ordered.

### Use LWWRef When:

- ✅ You need simple mutable pointer
- ✅ Latest value is all that matters
- ✅ Lower storage overhead preferred
- ✅ Simple last-write-wins is acceptable
- ✅ Pointing to repository manifests or configuration

**Example:** Pointer to the current head of a data structure, where history doesn't matter.

---

## In hbs2-git3 Context

### Repository Structure

```
LWWRef (Repository Key)
  ↓ points to
LWWRef Value (Manifest Hash)
  ↓ contains
Repository Manifest
  ├── reflog: "BTThPdHKF8X..." (RefLog for git transactions)
  ├── gk: [group keys for encryption]
  └── metadata
      ↓
RefLog (Git Transactions)
  ↓ points to
Merkle Tree (Repository State)
  ├── refs/heads/master → commit hash
  ├── refs/heads/dev → commit hash
  └── [all git objects]
```

### Key Storage

Private keys are stored in `~/.hbs2-keyman/keys/`:

```bash
# LWWRef key (repository identifier)
~/.hbs2-keyman/keys/3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC-lwwref.key

# RefLog key (derived from LWWRef key with seed)
# Generated internally by hbs2-git3 using derivedKey
```

---

## Practical Examples

### Subscribing to RefLog

```bash
# In hbs2-peer config
poll reflog 1 "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"
```

This tells the peer to:
1. Subscribe to RefLog updates
2. Download all transactions
3. Verify signatures
4. Maintain current state

### Subscribing to LWWRef

```bash
# In hbs2-peer config
poll lwwref 1 "3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC"
```

This tells the peer to:
1. Subscribe to LWWRef updates
2. Keep latest value
3. Fetch data pointed to by lwwValue

### Using Both Together

```nix
services.hbs2-peer = {
  extraConfig = ''
    ; Subscribe to repository's LWWRef (main pointer)
    poll lwwref 1 "3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC"

    ; Subscribe to repository's RefLog (git transactions)
    poll reflog 1 "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"
  '';
};
```

---

## Summary

- **RefLog** = Ordered transaction log with sequential consistency (used for git operations)
- **LWWRef** = Simple last-write-wins pointer with eventual consistency (used for repository metadata)

Both are cryptographically signed, identified by public keys, and propagate through the P2P network. Choose based on your consistency requirements and whether you need transaction history.
