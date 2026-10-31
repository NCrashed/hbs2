# Product readiness review and roadmap

Status: accepted direction, implementation in progress.

Last reviewed: 2026-10-31 against release 0.26.0.6 and the
`pep-16-barter-storage-roadmap` development branch.

## The product promise

The useful promise is deliberately smaller than "a global decentralized
network":

> Given a repository key, a new user can clone the complete repository and
> send a contribution through the configured hbs2 network. Every step either
> completes with verifiable evidence or reports the failed boundary and an
> actionable remedy.

hbs2 already provides the cryptographic and storage primitives behind this
promise. The remaining work is the product contract connecting setup,
discovery, retention, delivery, and diagnosis.

## Review result

The core is useful today to an operator who controls the peers and can inspect
LWWRefs, manifests, sigils, mailbox policy, RPC sockets, and daemon logs. It is
not yet a dependable repository-key-only experience for a new user.

The review found five release-critical gaps:

1. A block announcement, a reachable peer, and a complete durable replica are
   different states. The released network has no end-to-end availability
   receipt or provider lookup.
2. CLI/daemon version skew, the selected RPC socket, bootstrap reachability,
   repository discovery, and mailbox readiness have no unified diagnostic.
3. `hbs2-hub sent` records local handoff, not mailbox acceptance. A contributor
   cannot follow one authoritative delivery state from composition to canon.
4. The peer has no protocol-level multi-node release harness for restart,
   retry, ordering, stale caches, relay policy, and mixed-version scenarios.
5. Private keys are plaintext at rest, and file sync still has unresolved
   destructive-race and recovery work.

The 0.26.0.4 through 0.26.0.6 fixes illustrate the first four gaps as one user
journey: setup did not announce a required sigil, a public relay rejected an
unknown contributor before applying its wildcard policy, and a repeated
announcement did not invalidate an earlier negative block-size answer. Each
fix was correct; the missing part was a release test and a diagnostic spanning
the whole journey.

## Priorities

### P0: a verifiable happy path

#### 1. `doctor`

Build one read-only report over the boundaries a user otherwise diagnoses with
several programs and daemon logs.

Delivery slices:

- **D1, local runtime:** config path, selected RPC socket, daemon response,
  CLI/daemon version agreement, and connected peer count.
- **D2, network:** bootstrap resolution, configured relay reachability, peer
  identity, advertised transports, and policy-relevant peer metadata.
- **D3, repository:** LWWRef, manifest, RefLog, complete snapshot/provider, and
  required sigils for a repository key.
- **D4, forge:** declared mailbox, current policy version and PoW, local sender
  readiness, and the last known delivery state.

Every check has a stable name, severity, explanation, remedy, and JSON form.
Warnings do not make automation fail; failed required boundaries do.

Acceptance scenario:

```text
hbs2-peer doctor
hbs2-peer doctor --json
hbs2-peer doctor --repo <repository-key>
```

A mismatched daemon names both versions. A missing RPC peer names the socket
and the command that starts it. Repository and mailbox failures name the first
unmet boundary rather than surfacing as a later missing block.

#### 2. Durable repository availability

Finish the repository-lease milestones in PEP-06/PEP-16 in this order:

- complete the three-node offline-owner M1 scenario;
- freeze and test the snapshot/inventory/receipt encodings (completed
  2026-10-31 for the provider-local v1 wire types);
- publish and retrieve signed replica advertisements (wire type, verified
  provider-local lifecycle, and worker renewal completed 2026-10-31; network
  lookup envelope, authenticated handler, signature-verifying TTL relay cache,
  bounded gossip, periodic publication, and peer-bound expiring client query
  fan-out completed 2026-10-31; clone-path consumption remains open);
- find a complete provider from the repository key;
- maintain a replica target with make-before-break replacement.

Acceptance scenario: A publishes, B holds a verified complete lease, A shuts
down, and a clean C discovers B and clones the complete repository using only
the repository key and the normal bootstrap configuration.

#### 3. Mail delivery receipts

Expose a single state progression:

```text
created -> local-peer -> relay-stored -> mailbox-accepted
        -> maintainer-seen -> folded/rejected -> canon-published
```

The network states require signed receipts. Local handoff remains a distinct
fact and must never be labelled delivery.

#### 4. Protocol release harness

Gate releases on at least three peers and cover:

- fresh contributor known only by repository key;
- public relay and unknown first-contact peer;
- packet loss, repeated announce, and stale negative cache;
- peer and relay restart during delivery;
- mailbox policy update and PoW;
- supported old/new daemon and CLI combinations;
- publisher offline during clone.

### P1: safe onboarding

1. Add a top-level setup/service UX over the specialised binaries.
2. Encrypt private key material at rest and use a session agent.
3. Provide a tested recovery/backup story for user and service keys.
4. Qualify `hbs2-sync` with destructive-race, large-file, concurrent-writer,
   and recovery tests, or label it experimental until those pass.
5. Implement mailbox retention, compaction, and physical purge after a safe
   synchronization grace period.

### P2: product surface

- searchable/indexed forge views and activity documents;
- relay metrics for replica completeness, update lag, downloads, and mailbox
  rejection;
- automatic canon synchronization/publication policies;
- release-versioned documentation and generated CLI reference.

## Explicit non-priorities

A global DHT is not required for the supported small-peer-set model. Barter,
reputation, and broader marketplace work must not outrun the basic proof that a
complete repository remains discoverable while its publisher is offline.

## Release-level outcomes

- **R1, diagnosable network:** D1-D2 doctor, version compatibility reporting,
  and the multi-node relay release test.
- **R2, dependable repository:** complete leases, receipts, advertisements,
  provider lookup, and offline-owner clone.
- **R3, dependable collaboration:** mailbox delivery receipts, restart-safe
  queue state, retention, and end-to-end contribution status.
- **R4, safe daily use:** encrypted/recoverable keys and qualified file sync.
