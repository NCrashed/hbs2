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

- **D1, local runtime (implemented):** config path, selected RPC socket,
  daemon response, CLI/daemon version agreement, and connected peer count.
- **D2, network (implemented):** bootstrap resolution, configured relay
  reachability, signed peer identity and network classes, advertised
  endpoints, and policy-relevant peer metadata.
- **D3, repository (implemented):** LWWRef, manifest, RefLog, complete local
  snapshot/provider evidence, and required sigils for a repository key.
- **D4, forge (implemented):** declared mailbox, current policy version and
  PoW, local sender readiness, and the strongest locally provable delivery
  state.

Every check has a stable name, severity, explanation, remedy, and JSON form.
Warnings do not make automation fail; failed required boundaries do.

Available now:

```text
hbs2-peer doctor
hbs2-peer doctor --json
hbs2-peer doctor --config <config-directory>
hbs2-peer doctor --rpc <socket-path>
hbs2-peer doctor --repo <repository-key>
hbs2-hub doctor --repo <repository-key>
hbs2-hub doctor --repo <repository-key> --json
```

A mismatched daemon names both versions. A missing RPC peer names the socket
and the command that starts it. See [`doctor.md`](doctor.md) for the user guide.

Repository failures name the first unmet boundary rather than surfacing as a
later missing block. D3 proves completeness on the selected peer; signed
remote provider advertisements and provider lookup remain the next durable
availability milestone. The hub-level D4 report extends those checks through
forge and mailbox boundaries. It deliberately calls a sent-log row a local
handoff rather than delivery; the intermediate network states still require
the signed receipts described below.

#### 2. Durable repository availability

Finish the repository-lease milestones in PEP-06/PEP-16 in this order:

- complete the three-node offline-owner M1 scenario;
- freeze and test the snapshot/inventory/receipt encodings (completed
  2026-10-31 for the provider-local v1 wire types);
- publish and retrieve signed replica advertisements (wire type, verified
  provider-local lifecycle, and worker renewal completed 2026-10-31; network
  lookup envelope, authenticated handler, signature-verifying TTL relay cache,
  bounded gossip, periodic publication, and peer-bound expiring client query
  fan-out completed 2026-10-31; authenticated provider selection and
  clone-path consumption completed 2026-10-31; external owner/provider/relay/
  client acceptance completed 2026-10-31);
- prove repository-key-only discovery in the external multi-process
  offline-owner acceptance scenario (completed 2026-10-31);
- maintain a replica target with make-before-break replacement.

Acceptance scenario: A publishes, B holds a verified complete lease, A shuts
down, and a clean C discovers B and clones the complete repository using only
the repository key and the normal bootstrap configuration. The executable test
uses a separate rendezvous R as C's only configured peer, so direct provider
configuration cannot make the scenario pass without replica-advert lookup.

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

#### `hbs2-hub serve` UX backlog

- Make inbox attention independent of opening the Inbox route. Both the
  browser extension and the standalone UI must start or subscribe to a
  lightweight mailbox-status source in the background and show an unseen-mail
  badge in their persistent chrome. A user who never opens Inbox must still
  learn that a letter arrived.
- Define `unseen` as a persisted, mailbox-scoped cursor rather than the number
  returned by the last Inbox page load. Opening the list may advance that
  cursor, but merely refreshing application data must not silently mark mail
  seen. Distinguish a confirmed zero from `checking`, `stale`, and `failed`,
  and expose the last successful check so an offline client never looks
  deceptively empty.
- Let the extension update its toolbar badge from its background worker, and
  let the standalone navigation update before the Inbox view mounts. Poll or
  subscribe with bounded backoff instead of issuing network work on every
  render. Desktop notifications are a separate opt-in; the in-product badge
  is the default attention mechanism.
- Add a standalone activity dashboard for recent user-relevant events:
  incoming letters, accepted or folded proposals, canon sync/publication,
  and failed operations that need attention. Each item shows time, repository
  or mailbox, outcome, and a direct action. Persist a bounded, deduplicated
  history and give loading, empty, stale, and error states explicit UI.
- Back both surfaces with the same inbox-status and activity-event model so
  the extension badge, standalone badge, Inbox list, and dashboard cannot
  invent conflicting meanings for the same event.

Acceptance scenario: with neither Inbox view open, deliver a letter while the
extension and standalone UI are running. Both surfaces show an unseen badge;
opening Inbox clears it according to the same persisted cursor. After restart
the state remains consistent. If mailbox refresh fails, both surfaces show a
stale/error state rather than zero. The standalone dashboard retains one
deduplicated arrival event and links it to the letter.

- Render the supported safe Markdown consistently, including GitHub-style
  tables, build-status badges, and ordinary external images. Unsupported or
  blocked content must keep its alt text and an explicit placeholder instead
  of disappearing.
- Treat every external image as a privacy boundary. The first page that wants
  one asks before making any request; the initial state is therefore blocked,
  not an implicit opt-in. The prompt offers to load external images or keep
  them blocked and persists the chosen default.
- Add a Settings page where that decision can be changed later. Keep a global
  default of `ask`, `allow`, or `block`, with per-origin exceptions so that
  trusting a badge host does not silently trust every image host.
- External resources are data only, never remote code. Fetch them without
  credentials or a referrer, enforce content-type and size limits, and make
  the blocked origin visible before the user grants access.

Acceptance scenario: open a README or thread containing a Markdown table, a
build badge, and an external image in a fresh browser profile. The table is
readable immediately, no request reaches either image host before consent,
the selected policy survives restart, and changing it on the Settings page
changes subsequent rendering.

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
