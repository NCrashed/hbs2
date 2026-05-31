# Architecture

This document is a tour of how hbs2 is put together. It targets a
Haskell-capable engineer who has not seen the codebase before. It is
not a tutorial (see [`QUICKSTART.md`](QUICKSTART.md)) and it is not a
wire-format specification (see [`PROTOCOL.md`](PROTOCOL.md)).

## Mental model

hbs2 is a peer-to-peer content-addressable store with a thin layer of
named references on top.

- Data is split into fixed-size **blocks**, each addressed by the
  hash of its contents. Larger objects are represented as **merkle
  trees** of block hashes.
- Peers replicate blocks among themselves. Any peer holding a block
  can serve it; there is no central server.
- Mutability is layered on top of immutable blocks via **references**.
  A reference has a stable identity (a public key) and a current
  value (the hash of the block or tree it points to). The value
  changes by appending signed transactions, which other peers verify
  and apply.

Applications built on hbs2 (git remote, file sync) treat references
as the mutable handles they care about and let hbs2 take care of
distributing the underlying blocks.

## Component map

| Package | Role |
|---|---|
| `hbs2-core` | Foundation library. Hashes, merkle trees, references, crypto, messaging, storage interface, peer protocol types. Everything else depends on this. |
| `hbs2-log-structured` | Log-structured-storage primitives (cuckoo hash, structured-data layout). The substrate the NCQ3 storage backend is built on. |
| `hbs2-storage-ncq` | Primary on-disk storage backend. Provides the current NCQ3 log-structured format and a legacy NCQ module retained for migration. Implements the storage interface from `hbs2-core`. |
| `hbs2-storage-simple` | Older on-disk storage backend. Still maintained for tests and small deployments; new installs use `hbs2-storage-ncq`. |
| `hbs2-peer` | The P2P daemon. Listens on UDP/TCP/Unix, runs the protocols (block announcement, download, reflog, refchan, lwwref), exposes an RPC API for local clients. |
| `hbs2-keyman` | Key manager service. Tracks where local private keys live and serves them to local clients that need to sign or decrypt. The `hbs2-keyman-direct-lib` subpackage is the in-process client API. |
| `hbs2-cli` | Primary command-line interface. Talks to a running `hbs2-peer` over its RPC socket. Handles keyring generation, refchan and lwwref inspection, storage operations, peer status. Replaces most subcommands of the legacy `hbs2` monolith. |
| `hbs2-git3` | Git remote helper. Provides the `hbs2://` protocol for git (via `git-remote-hbs23`), the `git hbs2 ...` subcommand surface, and repository initialisation. Replaces the legacy `hbs2-git` package. |
| `hbs2-sync` | Directory synchronization tool. Uses a refchan as the shared substrate for keeping a folder in sync between several machines. |
| `hbs2-tests` | Test harness: integration tests, network probes, storage benchmarks. |
| `miscellaneous/` | Vendored forks of upstream libraries with project-specific patches. Being de-vendored over time. |
| `bf6/`, `scripts/` | Small bf6 (Scheme-like DSL) scripts and shell helpers used during development. `bf6/hbs2` and `hbs2-git3/bf6/git-hbs2` ship as user-facing wrappers around `hbs2-cli` and `hbs2-git3`. |

`hbs2-peer` is the only long-running service. Everything else is a
short-lived command-line tool that connects to the peer when it
needs to do something on the network.

## How it fits: git push, walked through

A useful way to understand the layering is to trace what happens
when you run `git push hbs2`.

1. Git invokes `git-remote-hbs23` (a binary from `hbs2-git3`). This is
   a standard "remote helper": git speaks a documented stdin/stdout
   protocol to it.
2. `git-remote-hbs23` connects to the local `hbs2-peer` over its RPC
   socket. The peer must already be running.
3. The helper computes the merkle tree for the git objects that need
   to be pushed and asks the peer to write the underlying blocks
   into storage. Blocks are written via the storage RPC API.
4. The helper produces a signed reflog transaction that points the
   repository's signing key at the new merkle root. To sign the
   transaction, it asks `hbs2-keyman` for the private key that
   corresponds to the repository's public key. Keyman finds it
   among the keyring files it has been told to watch.
5. The transaction goes to the peer's reflog protocol handler. The
   peer validates the signature and appends the transaction to its
   local reflog state.
6. The peer announces the new blocks to other peers it knows about.
   Other peers that are subscribed to this reflog pull the blocks
   and append the same transaction to their own state.

A `git clone hbs2://<KEY>` is the same flow in reverse: helper asks
the peer for the current reflog value, reads the merkle tree, asks
for blocks, reconstructs git objects.

## Core primitives

These types live in `hbs2-core/lib/HBS2/`.

### Hashes

`HBS2.Hash` defines the project hash type. All content addressing is
by this hash. Blocks are byte strings; the hash of a byte string is
its identifier in storage.

### Merkle trees

`HBS2.Merkle` defines the merkle tree used to represent objects
larger than a single block. A `MTree` is a tree of hashes; leaves
hold the hashes of data blocks, internal nodes hold the hashes of
their children. The hash of the root is the canonical handle for
the whole tree.

`HBS2.Merkle.MetaData` annotates merkle trees with additional
information (file paths, modes, encryption envelopes). Different
applications use the metadata slot differently.

### References

Three flavors of mutable reference, each with its own protocol module
in `hbs2-peer`:

- **`RefLog`**. An append-only log of signed transactions. One
  designated signer (the owner of the corresponding private key)
  can append. Identity is the public signing key. Transactions
  carry an application-defined payload (for git, the new merkle
  root of the repository state). Used by `hbs2-git3`.

- **`RefChan`** (reference channel). A mutable reference with an
  ACL: a list of writers (signing keys), a quorum requirement, and
  a list of authorized peers that can relay updates. Transactions
  carry an application-defined payload. Used by `hbs2-sync`.

- **`LWWRef`** (last writer wins). A simple register where the most
  recent signed update wins, ordered by a sequence number. Lighter
  weight than reflog or refchan. Used as a thin pointer-to-the-real
  thing in several places, including the repository handle by
  `hbs2-git3`.

Each protocol has both a wire-level part (how transactions propagate
between peers) and a storage part (how state is persisted).

### Peer identity

A peer is identified by an Ed25519 public signing key, generated
into a keyring file by `hbs2-cli hbs2:keyring:new`. The keyring file
contains both halves; `hbs2-cli hbs2:keyring:show` extracts the
public half. The private half is used to sign block announcements
and to participate in protocols that authenticate the peer.

### Group keys

Two encryption modes for repository or refchan content:

- **Symmetric group key** (`GroupKeySymm`). A shared secret is
  generated, encrypted under each authorized reader's public
  encryption key, and stored alongside the data. Readers decrypt
  the secret with their private key and then read the data. Used
  for most current encryption.

- **Asymmetric group key** (`GroupKeyAsymm`). Direct asymmetric
  encryption of payloads under each reader's public key. Heavier
  on payload size when there are many readers but simpler in
  semantics.

In both forms, a payload's encryption metadata travels alongside the
payload as a merkle tree annotation.

## Storage layer

`hbs2-storage-ncq` is the primary storage backend. It implements the
`Storage` interface from `hbs2-core/lib/HBS2/Storage.hs` and exposes
two modules:

- `HBS2.Storage.NCQ3` — the current production format. Blocks are
  appended to log-structured segment files; an in-memory index is
  rebuilt from the logs on startup. Built on the primitives in
  `hbs2-log-structured` (cuckoo hash, structured-data layout).
- `HBS2.Storage.NCQ` — the legacy NCQv1 format, retained so that
  existing on-disk data can be migrated. The `ncq3` executable from
  `hbs2-storage-ncq` provides migration tooling. The `scripts/`
  directory has a `ncq-migrate.ss` helper for end-to-end migration.

`hbs2-storage-simple` is the older single-file storage backend. It
still works and is used by parts of the test suite and by deployments
that have not migrated to NCQ3. New installs should use NCQ3.

The `Storage` interface itself is small: put/get blocks by hash,
enumerate, delete (for garbage collection). All higher-level merkle
tree and reference operations are built on top of it in `hbs2-core`.

Storage path defaults to `$HOME/.local/share/hbs2`. Each peer
instance owns its own storage directory.

## Network transport

Messaging types live under `HBS2.Net.Messaging.*` in `hbs2-core`.
The peer can listen on any combination of:

- **UDP**. The default for peer-to-peer block announcement and
  small messages. Supports multicast for LAN peer discovery.
- **TCP**. Used between peers for larger transfers and where UDP is
  unreliable. Optional SOCKS5 wrapping.
- **Unix sockets**. Used for the local RPC between `hbs2-peer` and
  client tools (`hbs2-cli`, `git-remote-hbs23`, `hbs2-sync`,
  `hbs2-keyman` consumers).

There is an encrypted overlay (`HBS2.Net.Messaging.Encrypted`) that
wraps any of these transports with peer-to-peer encryption.

Peer discovery is configured rather than automatic in the general
case:

- `known-peer <ip:port>` in the config pins a specific address.
- `bootstrap-dns <domain>` looks up TXT records at that domain and
  treats their content as peer addresses. The hardcoded default is
  `bootstrap.hbs2.app`; the bootstrap node behind that name is not
  yet deployed, so multi-machine setups should pin specific peers
  with `known-peer` for now.
- Once any peer is reached, the peer exchange protocol (PEX) shares
  addresses of others.

## Processes and how they talk

```
                 +------------------+
                 |   hbs2-peer      |
                 |  (long-running   |
                 |   daemon)        |
                 +--------+---------+
                          |
              Unix socket | RPC
                          |
   +---------+-------+----+-------+--------+
   |         |       |            |        |
   v         v       v            v        v
 hbs2-cli  git-     hbs2-sync   hbs2-     (any other
           remote-              keyman     client)
           hbs23
```

`hbs2-peer` owns the network and the storage. Everything else is a
short-lived client process that connects over a local Unix socket
when it needs something, and then exits.

`hbs2-keyman` is unusual: it has commands of its own (`add-mask`,
`update`, `list`) for managing the local key store, but it also acts
as a service that other tools query when they need a private key.
The keyring directory is shared filesystem state; keyman scans it
and serves results to whoever asks.

The user-facing `git hbs2 ...` command resolves to the `git-hbs2`
wrapper installed by the Nix build (or a one-line shim under the
cabal install path), which dispatches to `hbs2-git3`.

## Notes for future maintainers

Things that surprised the current maintainers and are worth a heads-up.

- **The wire protocol and the type system are deeply coupled.**
  `HBS2.Net.Proto.Types` uses GADTs, type families with functional
  dependencies, and associated types to model the protocol stack.
  Changing the protocol almost always means changing types in
  `hbs2-core` and rippling out. Read the existing protocol modules
  (`RefLog`, `RefChan`, `LWWRef`, `BlockAnnounce`, `BlockChunks`,
  `BlockInfo`) before introducing a new one.

- **Wire serialization is CBOR via the `serialise` library.** It is
  language-agnostic and stable. A non-Haskell implementation of an
  hbs2 peer is feasible.

- **Many TODO/FIXME comments are tuning knobs, not bugs.** Search
  hits include things like timeouts, queue sizes, and retry counts
  that have working defaults but were never tuned. Treat with mild
  suspicion, not alarm.

- **`hbs2-peer` is not designed for NAT traversal.** It assumes
  peers can either reach each other directly or that you point one
  peer at another's public address. Hole-punching is not in scope.

## Where to look in the code

A few entry points for code reading:

- `hbs2-peer/app/PeerMain.hs` is the daemon's main. Big file but
  follows a clear "wire up all the things and run" shape.
- `hbs2-git3/lib/HBS2/Git3/` has the git remote helper logic.
  `Run.hs` registers the `repo:init`, `reflog:*`, `repo:*` commands.
  `Repo/Init.hs` has the new-repository setup. The interface to
  `hbs2-peer` is via the storage, reflog, and lwwref RPC APIs.
- `hbs2-sync/src/HBS2/Sync/Internal.hs` has the sync command surface.
  `State.hs` in the same directory holds the conflict-resolution
  logic.
- `hbs2-core/lib/HBS2/Net/Proto/` has the protocol type machinery.
  Start with `Types.hs`.
- `hbs2-storage-ncq/lib/HBS2/Storage/NCQ3.hs` is the entry point of
  the NCQ3 storage backend; the deeper implementation modules live
  under `hbs2-storage-ncq/lib/HBS2/Storage/NCQ3/`.
- `hbs2-log-structured/lib/HBS2/` has the cuckoo-hash and
  structured-data primitives NCQ3 builds on.

For protocol-level detail beyond this overview, see
[`PROTOCOL.md`](PROTOCOL.md). The `docs/` directory contains older
design notes, CLI migration tables, and mirror-setup guidance.
