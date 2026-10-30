# PEP-06: repository lease, canonical inventory, and pinning

Status: draft; provider-local lease MVP implemented 2026-10-30, including
provider-signed local receipts on 2026-10-31. Bilateral wire contracts,
advertisements, and barter accounting are not part of this implementation.

Author: NCrashed (Anton Gushcha)

Depends on: the current hbs2-git3 LWWRef/RefLog repository model and
`HBS2.Storage`.

Required by: PEP-07 retrieval evidence, PEP-08 barter accounting, and PEP-10
replica advertisements.

Umbrella: PEP-16 availability-qualified barter storage.


# Summary

PEP-06 defines the thing a storage provider promises to keep: a complete,
bounded snapshot of a mutable hbs2 repository.  It specifies:

- how a repository state is identified;
- which content-addressed blocks belong to that state;
- how shared blocks are counted;
- how missing, corrupt, or over-limit graphs remain incomplete;
- which state transitions may publish a receipt;
- which local roots must protect an active replica from garbage collection.

This PEP does not negotiate barter, score availability, or discover providers.
Those layers must consume the snapshot and completeness definitions here
instead of inventing their own.


# Motivation

The current `repo:relay-only` path already performs most of the data-plane
work.  It polls the repository LWWRef, reads the repository-head graph, learns
the RefLog key from the manifest, polls that RefLog, and waits until the RefLog
Merkle graph is locally complete.

That is not yet a lease protocol:

- there is no stable snapshot identifier joining the LWWRef and RefLog state;
- there is no canonical unique block inventory or exact logical size;
- a manifest or RefLog root being readable does not prove every transaction
  payload is retained;
- there is no lease-owned pin set with crash-recovery semantics;
- there is no atomic boundary between partial sync and a complete advertised
  replica.

PEP-06 adds those boundaries without replacing the existing repository
transport.


# Current repository graph

The v1 design follows the formats that exist today:

1. A repository key is the signing key of an `LWWRefKey`.
2. The local LWWRef alias names a block containing a signed `LWWRef`.
3. `lwwValue` is the root of a Merkle log.  Its first item is the repository
   manifest content tree; a later item may be the group-key journal.
4. The manifest names the repository RefLog signing key.
5. The local RefLog alias names a Merkle graph whose leaves name signed
   `RefLogUpdate` blocks.
6. A valid RefLog update contains an annotated or sequential reference to a
   `GitTx` root.
7. A `GitTx` is a segment or checkpoint tree whose reachable blocks carry the
   repository history needed by hbs2-git3.

Reference aliases are mutable local names, not CAS blocks.  They are inputs to
snapshot discovery but are not charged as stored bytes.  The signed blocks to
which they resolve are part of the inventory.


# Normative vocabulary

The words MUST, MUST NOT, SHOULD, SHOULD NOT, and MAY are normative.

**Repository state**
  The pair of verified LWWRef and RefLog values observed for one repository.

**Snapshot descriptor**
  A versioned immutable statement naming an exact repository state and its
  retention policy.

**Inventory**
  The ordered set of distinct CAS block hashes and exact stored byte lengths
  reachable under the snapshot's protocol-aware traversal rules.

**Complete snapshot**
  A snapshot for which traversal finished inside its bounds, every inventory
  block is present under the expected hash, and the resulting inventory is
  protected by a durable lease pin.

**Frontier**
  A referenced hash which could not be traversed because its block was absent
  or corrupt.  Descendants behind an absent internal node are unknown; they
  MUST NOT be treated as an empty subtree.


# Retention policy v1

The only interoperable v1 policy is `FullRefLogV1`:

- retain the exact signed LWWRef block;
- retain the complete repository-head graph rooted at `lwwValue`;
- retain the exact signed RefLog root and its full Merkle graph;
- retain every valid RefLog update named by that root;
- retain every Git transaction root named by those updates and its complete
  reachable graph;
- retain metadata and encryption/key blocks explicitly referenced by the
  typed structures above.

This is intentionally conservative.  A provider cannot safely infer that old
transactions are dispensable merely because a later checkpoint exists.  A
future retention policy may define checkpoint or time-window pruning only
after hbs2-git3 can demonstrate that a fresh clone and every supported recovery
operation work from that reduced graph.

Unknown policies MUST be rejected, not interpreted as `FullRefLogV1`.


# Snapshot descriptor and identity

The semantic v1 descriptor is:

```
RepositorySnapshotV1
  repository-key
  lwwref-block
  lww-sequence
  repository-head-root
  reflog-key
  reflog-root
  retention-policy        ; FullRefLogV1
```

`lwwref-block` is the hash stored under the LWWRef alias, not merely
`repository-head-root`.  It binds the signed sequence and any future proof
field.  `reflog-root` is the exact value observed after parsing and verifying
the manifest named by that LWWRef.

The wire encoding MUST be versioned and use a fixed-order CBOR array.  Map
encoding is forbidden for the signed descriptor.  The snapshot identifier is:

```
snapshot-id = HbSync(
  "hbs2-repository-snapshot-v1\0" || canonical-descriptor-bytes
)
```

The concrete Haskell type and golden encoding vector are an M0 deliverable.
No receipt or lease message may be declared wire-stable before that vector is
checked into the test suite.


# Consistent observation

LWWRef and RefLog are independently mutable.  Reading each once can construct
a pair which never represented a stable observable repository state.  Snapshot
discovery therefore uses this loop:

1. Read and verify LWWRef block `L1`.
2. Traverse its repository-head graph and parse its manifest.
3. Read and verify the named RefLog root `R1`.
4. Traverse and validate the candidate inventory.
5. Read the LWWRef alias again as `L2`.
6. Read the RefLog alias again as `R2`.
7. Accept the descriptor only if `L1 == L2` and `R1 == R2`.

If either value changed, the candidate may remain in the local CAS and benefit
deduplication, but it MUST NOT become `Active`.  Discovery restarts with the
new values.  Implementations MUST bound retries and surface a repository which
changes faster than it can be inventoried as `Degraded`, not loop forever.


# Protocol-aware traversal

Canonical inventory MUST NOT use `tryDetect` as its edge authority.  Several
hbs2 values are untagged CBOR and a byte string can decode as more than one
shape.  In addition, a `RefLogUpdate` is opaque to generic Merkle detection even
though its signed payload names a Git transaction root.

Traversal instead carries an expected block role:

```
LWWRef alias
  -> signed LWWRef block
  -> repository-head Merkle root
  -> manifest content root
  -> parsed manifest
  -> RefLog alias
  -> RefLog Merkle root
  -> signed RefLogUpdate blocks
  -> decoded GitTx roots
  -> segment/checkpoint graphs
```

Each decoder MUST:

- verify the block hash before parsing it;
- verify signatures before following signed payloads;
- reject a value which is invalid for its expected role;
- return explicit child roles rather than a bag of guessed hashes;
- bound nesting, block count, and logical bytes using the accepted lease;
- visit a distinct hash once for storage accounting even when several roles or
  paths name it.

A block shared by the manifest, two transactions, or two paths appears once in
one snapshot inventory.  Separate leases reserve their contracted logical
capacity independently as required by PEP-16; physical deduplication does not
reduce the provider's obligation.


# Canonical inventory

For every valid distinct block, inventory records:

```
InventoryEntry
  block-hash
  stored-byte-length
```

Entries are ordered by the raw `HbSync` hash bytes ascending.  The logical size
is the sum of stored byte lengths with each distinct hash counted once.  It is
not the size of reconstructed plaintext, Git objects, or filesystem usage.

The inventory walker returns four independent facts:

- valid blocks and their lengths;
- missing frontier hashes;
- corrupt hashes for which returned bytes do not match the requested hash;
- hashes whose bytes are malformed for their expected protocol role;
- completion or the first configured limit which stopped traversal.

Only a completed traversal with empty missing, corrupt, and malformed sets can
be sealed as a canonical inventory.  Partial results are diagnostic evidence
and MUST NOT be hashed into a smaller apparently valid inventory.

`HBS2.Storage.Inventory` implements the protocol-neutral bounded set walker.
Repository-role decoders live in `HBS2.Peer.RepositoryLease`, next to the
LWWRef and RefLog formats they verify, so that hbs2-core does not acquire
knowledge of repository protocols.


# Inventory commitment

The provider stores a compact commitment alongside the durable lease record:

```
InventoryCommitmentV1
  snapshot-id
  block-count
  logical-bytes
  entries-root
```

`entries-root` is a Merkle tree over the fixed-order encoding of
`InventoryEntry`.  The tree permits bounded audit sampling without keeping one
large in-memory list.  Its tree blocks are local accounting metadata and are
not charged to the consumer's repository logical size.

The exact tree fanout and golden vector remain an M0 decision.  Until those
exist, implementations MAY persist the ordered entry list locally but MUST NOT
publish an interoperable inventory commitment.


# Completeness and receipts

A provider may issue `ReplicaReceiptV1` only after all of these are true:

1. LWWRef and RefLog signatures are valid.
2. The consistent-observation loop succeeded.
3. Inventory traversal completed inside `max-logical-bytes` and the configured
   block-count bound.
4. Missing and corrupt sets are empty.
5. The exact inventory commitment is durable.
6. The lease pin protecting that inventory is durable.
7. Recovery has enough information to reconstruct the same lease and pin after
   a process restart.

The receipt names at least the lease id, snapshot id, inventory commitment,
completion time, and provider identity.  A receipt is evidence of this
transition; it is not a promise that the provider will remain online forever.

`Syncing`, over-limit, stale, and partial states MUST NOT emit a receipt or a
replica advertisement.

Implementation note (2026-10-31): `ReplicaReceiptV1` contains the lease,
repository, provider, snapshot and inventory identities, exact block and byte
counts, completion time, and issue time.  The peer signs it with its node
identity and stores the signed box in CAS behind the durable
`RepositoryLeaseReceipt` root.  Recovery accepts `Active` only when the
snapshot pin and a matching valid receipt are both present; a missing,
corrupt, mismatched, or differently signed receipt is replaced before the
lease can return to `Active`.  Termination and expiry remove both roots.
`repo:lease:receipt` verifies and prints the local receipt.

This provider-local encoding is not yet declared an interoperable wire
contract.  The snapshot, inventory entry, and receipt golden vectors required
by M0 remain the compatibility gate for that declaration.


# Pinning and garbage collection

PEP-06 requires an explicit local root namespace owned by the lease subsystem.
Conceptually:

```
barter/lease/<contract-id>/active -> InventoryCommitmentV1
barter/lease/<contract-id>/next   -> InventoryCommitmentV1
```

The concrete NCQ3 representation may differ, but it MUST provide these
properties:

- pins are durable before a snapshot becomes active;
- GC treats blocks reachable from `active` and `next` as live;
- replacing a snapshot installs and verifies `next` before switching `active`;
- the old active pin is released only after the switch and lease policy allow;
- restart recovery cannot observe an `Active` lease without its pin;
- deleting an expired lease is idempotent and crash-safe.

The update sequence is make-before-break:

1. reserve capacity for the candidate;
2. sync and inventory the candidate;
3. persist its commitment and `next` pin;
4. atomically make it `active` and persist the receipt;
5. keep the previous pin through the signed grace/retention boundary;
6. remove the previous pin and let ordinary GC reclaim unshared blocks.

If NCQ3 cannot yet express a durable root set, Milestone 1 remains blocked.
Keeping a process alive or relying on recently-read blocks is not pinning.


# Capacity and safety bounds

Every scan has both:

- `max-logical-bytes`, negotiated in the lease; and
- `max-inventory-blocks`, selected by the provider and included in admission
  policy.

The second bound prevents a graph of tiny blocks from consuming unbounded RAM,
CPU, or local metadata while staying under a byte limit.  Implementations
SHOULD also bound decoder nesting and wall-clock work per scheduling slice.

Limit exhaustion is a normal incomplete result.  It MUST NOT crash the peer,
silently truncate the inventory, or evict the previous complete snapshot.


# M0 fixtures

The following fixtures are required before the wire types are frozen:

| Fixture | Property it fixes |
|---|---|
| Empty public repository | Minimum valid LWWRef/manifest/RefLog graph |
| One segment and one checkpoint | GitTx role decoding and retention |
| Shared blocks and duplicate edges | Unique accounting, deterministic order |
| Repository-head rewrite with same RefLog | Snapshot identity changes correctly |
| RefLog update during inventory | Consistent-observation retry |
| Missing internal Merkle node | Unknown descendants do not become empty |
| Wrong bytes returned for a hash | Corruption differs from absence |
| Repository above byte limit | Previous complete snapshot survives |
| Tiny-block graph above block limit | Bounded work and explicit incomplete state |
| Group-key journal | Key metadata is retained without requiring decryption |
| Encrypted repository | Ciphertext graph inventories without plaintext access |
| hbs2-hub canon and staged proposal refs | Application refs travel with retained Git history |

Every fixture records expected ordered hashes, block count, logical bytes,
snapshot id, and missing/corrupt frontier.  At least two independent traversal
paths must agree: the hbs2-git3 implementation and a small fixture verifier.


# Provider-local implementation

The first executable vertical slice is now present:

1. `HBS2.Storage.Inventory` performs deterministic, role-aware, bounded
   traversal and separately reports missing, corrupt, and malformed blocks.
2. `HBS2.Peer.RepositoryLease` verifies the signed LWWRef, manifest, signed
   RefLog updates, and content trees, then repeats both mutable ref reads before
   accepting a snapshot.
3. The peer stores versioned lease records in its durable Brains SQLite
   database and exposes append-only PeerAPI methods for put/get/list.
4. A peer worker restores every lease after restart, durably registers the
   LWWRef and discovered RefLog pollers, refreshes each repository, and changes
   state only after a complete scan.
5. Sealing writes a canonical Merkle commitment over ordered `(hash,length)`
   entries and a separate Merkle pin tree over plain hashes. The lease ref
   points to the latter so a storage root walker reaches the repository data.
6. A failed or over-limit update changes an existing lease to `Degraded` while
   retaining its previous complete snapshot and pin. Expiry or explicit
   termination removes the pin idempotently.
7. `hbs2-git3` provides `repo:lease:add`, `repo:lease:list`,
   `repo:lease:show`/`status`, `repo:lease:terminate`, and
   `repo:lease:inventory`. `repo:purge` refuses to delete a repository protected
   by a non-terminated lease.

`repo:lease:add` accepts `--max-bytes`, `--max-blocks`, and optional `--ttl`.
The provider-local default limits are 10 GiB and 10,000,000 blocks. This slice
does not publish a replica advertisement: knowing the provider remains
necessary until PEP-10 is implemented.


# Open questions

1. Does every supported encrypted repository expose enough ciphertext-tree
   structure to inventory it without a group key?  The fixture must answer this
   before it becomes a requirement.
2. Which exact checkpoint semantics permit safe RefLog pruning in a future
   retention policy?
3. Should `max-inventory-blocks` be signed in the lease or remain a stricter
   provider-local admission limit?
4. Can NCQ3 expose a transactional root set, or must the lease layer journal
   pin transitions and replay them before GC starts?
5. Should inventory commitments use the existing hbs2 Merkle encoding or a
   smaller purpose-specific fixed-fanout tree?
6. Which repository changes should debounce snapshot creation so that frequent
   pushes do not keep a provider permanently in `Syncing`?


# Acceptance criteria

PEP-06 is ready for wire implementation when:

- all M0 fixtures have deterministic expected inventories;
- snapshot and entry encodings have golden vectors;
- public and encrypted repository scans are measured and bounded;
- a three-node test can distinguish `Syncing` from a complete pinned replica;
- restart and GC tests demonstrate that an active snapshot survives;
- a repository changing during scan cannot produce a mixed-state receipt.
