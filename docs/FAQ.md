# FAQ

Short answers to questions that come up often. For the long versions,
see [`ARCHITECTURE.md`](../ARCHITECTURE.md) and
[`PROTOCOL.md`](../PROTOCOL.md).

## What is hbs2?

A peer-to-peer content-addressable storage system. It stores data as
hash-indexed blocks, groups them into Merkle trees, and replicates them
between subscribed peers without a central server. Two applications run
on top of that substrate: a distributed git remote (`hbs2-git3`) and a
file synchronization tool (`hbs2-sync`).

See [`README.md`](../README.md) for the overview and
[`ARCHITECTURE.md`](../ARCHITECTURE.md) for the component map.

## What crypto primitives does hbs2 use?

- **Hashing**: BLAKE2b-256.
- **Signatures**: Ed25519.
- **Authenticated encryption**: libsodium primitives (`crypto_box`,
  `crypto_secretbox`).

These are used uniformly for block addressing, signed references, and
the group-key envelopes that wrap encrypted content.

## How does hbs2 compare to Syncthing?

hbs2 was partly inspired by Syncthing but differs on three axes that
matter for the use cases it targets:

- **Host-addressed vs content-addressed.** Syncthing identifies things
  by host key; to receive a folder you need the device ID of someone
  who has it. hbs2 identifies things by their content hash or by a
  signed reference; any peer holding the blocks can serve them, and
  the receiver verifies by hash without trusting the sender.
- **Pure CAS vs mutable directory.** Syncthing synchronises arbitrary
  filesystem state, including in-place mutations. That model does not
  hold up well for git repositories, whose `.git/` directory contains
  files that must not be partially-applied across machines. hbs2's
  CAS sidesteps this: a repository state is a single immutable Merkle
  reference, updated atomically.
- **End-to-end encryption at the storage layer.** Syncthing encrypts
  in transit and supports untrusted relay nodes, but the data model
  itself is plaintext. hbs2 encrypts at the block level under a
  group key, so peers can store and forward encrypted blocks they
  cannot read.

## How does hbs2 compare to Radicle?

Both target self-hosted distributed git. They take different
approaches to the storage substrate.

- Radicle uses git's own object store as the replication unit. That
  is a natural fit for code but does not handle large binary assets
  efficiently and ties the protocol's evolution to git's.
- hbs2 stores arbitrary content (not only git objects) in a separate
  CAS, and exposes git via a remote helper (`hbs23://` scheme) that
  packs and unpacks repositories on top of that CAS. The same storage
  layer carries non-git data (files, encrypted media) without special
  cases.

If your only use case is small-to-medium source repositories with a
small team, Radicle is more mature and more polished. If you need the
same substrate to also distribute binaries, encrypted folders, or
arbitrary signed references, hbs2's split makes that easier.

## How does hbs2 compare to IPFS?

Both are content-addressed P2P storage systems. The relevant
differences for hbs2's design:

- **Smaller protocol surface.** hbs2 ships a fixed set of protocols
  for what it does (peer discovery, block transfer, reflog, refchan,
  lwwref). It does not aim to be a general substrate for arbitrary
  decentralised applications.
- **No global DHT.** Peer and content discovery is done over local
  multicast, a configured peer list, and PEX. There is no
  Kademlia-style global lookup. This is a deliberate fit for small
  trusted peer sets (your devices, a friend's server) rather than
  open public swarms.
- **First-class mutable references.** hbs2 has signed `RefLog` and
  `LWWRef` primitives in the protocol. IPFS leaves mutable-reference
  semantics to higher layers (IPNS, application-specific).

## Why no JSON anywhere?

hbs2 uses CBOR for wire protocols and serialised data, and
S-expressions for configuration and the CLI command language.

> JSON is both bulky and ambiguous, yet somehow still limited.

CBOR is compact, has unambiguous binary encoding, supports the types
hbs2 actually needs (raw bytes without base64), and is friendly to
cryptographic signing. Compactness matters because hbs2 runs over both
UDP (where MTU is hard) and TCP. S-expressions are used wherever
configuration and command composition are involved because they
nest cleanly, parse trivially, and fit the scripting style of the
existing tools (`hbs2-cli`, `bf6`).

If you need JSON for interop, `hbs2-cli` can produce and consume it
via `json:stdin` and related entries.

## Is hbs2 written in Lisp?

No, it is written in Haskell. The parentheses you see in `hbs2-cli`
arguments are S-expressions used as the command syntax, not Lisp
source. The S-expression interpreter (`suckless-conf`) is a small
embedded DSL for composing commands and writing configuration.
