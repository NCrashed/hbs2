# PEP-16: availability-qualified barter storage

Status: draft, discussion started 2026-06-04, substantially revised
2026-10-30.
Author: NCrashed (Anton Gushcha)

This is the umbrella vision.  The concrete work is split into PEP-06..12
below.  This revision replaces the original static-tree barter model with
repository replication leases, measured availability, explicit replica
discovery, and repair.


# Summary

Today an hbs2 repository remains available only while at least one operator
who subscribed to it is online.  Subscribing a mirror is a manual operation:
the operator names a repository, polls its LWWRef and RefLog, and budgets the
space by hand.  There is no protocol by which a node can offer spare storage,
accept repositories automatically, or tell a future clone where a live copy
can be found.

PEP-16 turns that manual mirror arrangement into a friendnet service:

1. An operator advertises a bounded amount of space it is willing to donate.
2. Two peers create a signed lease for a mutable repository, not one static
   tree.
3. The provider follows the repository LWWRef and RefLog, fetches the complete
   reachable graph, and pins the last complete snapshot against garbage
   collection.
4. The provider publishes a short-lived signed replica advertisement only
   after the snapshot is complete.
5. The consumer measures reachability, retrievability, freshness, and service
   rate separately.
6. Credit accrues in availability-qualified byte-hours.  A peer that is online
   half the time contributes less effective capacity than an always-on peer.
7. When the replica set falls below its target, a replacement is made complete
   before an old replica becomes eligible for deletion.

The first useful vertical slice is therefore a repository lease and a
discoverable, complete mirror.  Proof-of-custody is supporting evidence, not
the first product milestone.


# Goal

The user-visible promise is that a publisher can go offline and a new user who
knows only the repository key can still find a live replica and clone the
repository.  Operators who make that possible receive proportional storage
service in return.

This includes future updates.  A lease covers the repository identity and its
authenticated stream of states:

- the repository LWWRef and manifest;
- the RefLog and repository transactions it names;
- the block graph reachable from the retained transactions;
- repository refs carried by those transactions, including hbs2-hub canon and
  staged proposals when they have been published into the repository.

The precise retained-history policy is part of a lease.  An implementation
must not claim to mirror a repository merely because it has one historical
tree or its current LWWRef box.


# Why this is not Filecoin, Sia, or Storj

Open anonymous storage markets need globally enforceable accounting,
provider-independent proofs, and often monetary settlement.  That leads to
blockchains, sealing, proof-of-replication, proof-of-spacetime, or a trusted
coordinator.

hbs2 targets a friendnet: personal deployments, teams, and friends of friends.
Decisions and balances remain local.  There is no token, global ledger, global
reputation, or claim that a cheap audit proves a unique physical replica.

This narrower trust model does not remove the need for careful measurement.
In particular, bytes on a node which is rarely reachable are not equivalent
to bytes on a long-running public node.  PEP-16 accounts for that difference
without introducing a global market.


# Terminology and service definition

**Consumer**
  The peer buying replication service with its bilateral credit.  It need not
  own the repository key; public repositories may be sponsored by others.

**Provider**
  The peer reserving local capacity, following repository updates, pinning
  complete snapshots, and serving blocks.

**Repository lease**
  A signed, time-bounded agreement to maintain a repository subject to size,
  freshness, availability, and retention limits.

**Snapshot**
  A particular authenticated repository state identified by at least the
  repository key, LWWRef sequence/value, manifest, and RefLog head.

**Inventory**
  A canonical, unique list of blocks required by a snapshot under its
  retention policy.  Its exact encoding is specified by PEP-06.

**Complete replica**
  A provider has every block in the snapshot inventory and those blocks are
  pinned by an active lease.  A partial download is not a replica.

**Replica receipt**
  The provider's signed statement that a snapshot became complete.  It is
  evidence and an accounting boundary, not proof that the provider will keep
  the data forever.

**Replica advertisement**
  A short-lived, signed hint that a provider is willing to serve a repository.
  It is verified by retrieval; it is not trusted merely because it is signed.

**Effective byte-hour**
  One reserved logical byte for one hour, discounted by measured service
  quality.

**Failure domain**
  A grouping of replicas likely to fail together: operator, machine, site,
  network, power source, or another locally meaningful tag.


# Non-goals

- No blockchain, token, monetary settlement, or global consensus.
- No global reputation number.
- No promise to prove a unique physical copy with the first audit protocol.
- No public permissionless market in the first implementation.  Partner
  admission remains explicit or introduction-based.
- No erasure coding in the first implementation.
- No promise that a self-reported address, capacity, uptime, or failure domain
  is true.  Local observations take precedence.
- No requirement that private contract metadata or the social graph be
  published.  Replica advertisements are opt-in.


# Architectural principles

1. **The unit of service is a mutable repository lease.**  Blocks and trees
   are implementation units, not what the user buys.

2. **Complete before advertise.**  A provider must never publish an incomplete
   download as a replica.

3. **Make before break.**  Repair creates and verifies a replacement before an
   old lease is released or its blocks become garbage-collectable.

4. **Separate observations.**  Reachability, custody, freshness, throughput,
   and operator trust are different facts.  They must not be collapsed into a
   single score at storage time.

5. **Time matters.**  Accounting uses byte-hours, not a one-time byte count.
   Availability qualifies those byte-hours.

6. **Local decisions and bounded exposure.**  Every peer chooses partners,
   budgets, priors, thresholds, and credit limits locally.

7. **Reuse the working data plane.**  Repository replication should drive the
   existing `repo:relay-only`, LWWRef/RefLog polling, block download, NCQ3, and
   signed peer identity machinery rather than create a second repository
   transport.

8. **Discovery is part of availability.**  Stored data which a fresh clone
   cannot locate does not satisfy the primary goal.

9. **Conservative capacity accounting.**  Deduplication is a provider
   optimisation.  It must not allow a provider to over-promise space needed by
   active contracts.

10. **Old complete state beats new partial state.**  During an update, the
    last complete snapshot remains served and pinned until the new one is
    complete.


# Repository lease

The concrete encoding belongs to PEP-06.  The umbrella requires the following
semantic fields:

```
RepositoryLease
  version
  contract-id
  consumer-peer
  provider-peer
  repository-key
  retention-policy
  max-logical-bytes
  max-update-lag
  not-before
  not-after
  grace-period
  availability-class
  audit-policy
  failure-domain-hints
  consumer-signature
  provider-signature
```

The repository key authenticates repository state; the peer keys authenticate
the service agreement.  They are intentionally different authorities.

`max-logical-bytes` is checked against the canonical snapshot inventory.  The
provider also reserves at least that much contract capacity even when NCQ3
deduplicates some blocks with another lease.  This is conservative but makes
admission deterministic and prevents deduplication from becoming an
overcommitment loophole.

An update which exceeds the limit is not partially accepted.  The provider
keeps serving the previous complete snapshot, marks the lease `Degraded`, and
requests a larger limit or a replacement provider.


# Lease state machine

```
Offered -> Accepted -> Syncing -> Active -> Degraded -> Grace -> Expired
               |          |         |          |          |
               +----------+---------+----------+----------+-> Terminated

Expired or Terminated -> GC-eligible only after retention/grace rules permit it
```

Required invariants:

- `Active` implies a complete pinned snapshot and a signed receipt.
- `Syncing` and partial snapshots are never advertised.
- At least one complete snapshot remains pinned while an update is in flight.
- An active lease cannot be evicted merely because another peer has a higher
  reputation or an older lease.
- Capacity pressure first stops new admission.  It does not silently delete
  active data.
- Expiry is determined by the signed lease and its grace period, not by a few
  minutes of peer unreachability.
- Graceful termination names the last time through which service and credit
  accrue.
- Recovery after process restart reconstructs the same state from the durable
  contract log and pin set.


# Observations and availability

The local evidence journal records events, not a mutable scalar score:

```
Observation
  contract-id
  provider-peer
  snapshot-id
  time-window
  kind                 ; reachability | retrieval | freshness | throughput
  outcome              ; success | offline | missing | corrupt | stale | slow
  measured-value
  challenge-id
  observer
  signature?           ; required when transferred to another peer
```

The first implementation uses equal-duration time windows over a rolling
tracking period.  Each window receives equal weight regardless of the number
of probes in it.  This prevents a highly active contract from being punished
or rewarded merely because it was sampled more often.

If the observer itself was offline for a whole window, the provider's state is
`unknown`, not `offline`.  A bilateral observer therefore measures overlap
availability.  Claims about absolute public availability require independent
witnesses and are deferred; the data model leaves room for their signed
observations.

The implementation should retain both:

- a rolling aggregate for accounting and admission; and
- a coarse time-of-week profile for replica placement.

The second matters for intermittent nodes.  Two peers which are each online
half the week may provide almost full coverage if their schedules complement
one another, or almost no improvement if their outages coincide.

New peers use a conservative prior or a lower confidence bound.  One successful
probe must not create a 100% availability rating.  PEP-07 chooses a concrete
Wilson/Beta calculation, window duration, tracking period, and minimum sample
count after measurements on the real network.


# Availability-qualified accounting

The bilateral ledger is denominated in effective byte-hours:

```
effective-byte-hours =
    reserved-logical-bytes
  * active-hours
  * availability-LCB
  * custody-LCB
  * freshness-factor
  * bandwidth-factor
```

Where:

- `availability-LCB` is the conservative reachability estimate over completed
  time windows;
- `custody-LCB` is the success rate of content checks conditional on the peer
  being reachable;
- `freshness-factor` is one while the replica is inside `max-update-lag`, and
  zero or a specified decay while it is stale;
- `bandwidth-factor` is capped at one, for example
  `min(1, measured-rate / contracted-rate)`.

PEP-08 fixes the exact arithmetic and rounding.  It must be deterministic for
the two parties given the observations they both acknowledge, while each side
may use stricter private estimates for future admission.

Example: service with quality 0.40 must provide 2.375 GiB-hours to balance
1 GiB-hour of service with quality 0.95.  This makes a useful but intermittent
home node worth less than an always-on VPS without excluding it entirely.

Balances are bilateral and non-transferable in the first version.  Each peer
sets a maximum negative balance, limiting how much service it will provide
before the counterparty contributes in return.  This avoids both a global
currency and the deadlock of requiring two repositories of exactly equal size
at contract creation.


# Audit model

The original proposal asked for a hash of a random byte range.  That is not a
Merkle proof: an hbs2 block hash commits to the full block and cannot verify an
arbitrary range without the bytes or a separate sub-block commitment.

The first audit protocol therefore measures retrievability with existing CAS
primitives:

```
ReplicaProbeRequest
  version
  contract-id
  snapshot-id
  challenge-id
  nonce
  expires-at
  sampled-block-hashes
  requester-signature

ReplicaProbeReply
  version
  contract-id
  snapshot-id
  challenge-id
  nonce
  result
  response-metadata
  provider-signature
```

For each requested hash the provider returns the complete block, or a protocol
response which lets the requester fetch that block from this provider.  The
auditor verifies it with the normal hbs2 content hash.  Challenges are
unpredictable, bound to the contract and snapshot, expire, and cannot be
replayed into another protocol.

If a fraction `d` of an inventory is absent, `k` independent samples detect the
loss with probability:

```
1 - (1 - d)^k
```

Sampling therefore does not replace an occasional full inventory/deep scan.
Audit rate is adaptive and bounded.  Only an authenticated counterparty or an
explicitly accepted witness may issue probes, and every contract has a probe
bandwidth limit to prevent audit amplification.

A provider may answer a challenge by fetching the block from another replica.
A deadline makes that harder but cannot prove a unique physical copy.  The
first version calls this what it is: proof of retrieval service.  If experience
shows that proof of independent custody is required, a later PEP can introduce
provider-specific encoding, salt, or a real proof-of-retrievability scheme at
the cost of deduplication and implementation complexity.


# Capacity, admission, and eviction

Operator configuration includes at least:

```
barter-budget-bytes
barter-reserve-bytes
barter-max-contracts
barter-max-repo-bytes
barter-credit-limit
barter-min-availability-class
barter-admission-policy
```

The budget is donated capacity, not a function of how much content the
operator owns.  Requiring "own signed content" is not an anti-Sybil mechanism:
arbitrary signed bytes are cheap to generate.

Admission considers:

- currently reserved capacity and headroom for updates;
- bilateral credit and exposure limit;
- locally observed availability and custody history;
- introducer and operator policy;
- failure-domain diversity of the repository's existing replicas;
- current sync queue and expected ingress cost.

When capacity becomes tight, the order is:

1. refuse new offers;
2. stop expanding leases beyond their signed limit;
3. renew fewer leases and notify counterparties;
4. create replacement replicas where required;
5. expire whole leases after grace;
6. allow their pins to be collected.

Partial eviction of an active snapshot is a contract failure, not a normal
space-management policy.


# Sybil resistance and introductions

An identity receives no storage merely by existing or publishing bytes.  It
receives only the bounded credit a local peer chooses to expose, and earns more
by delivering measured service.  Splitting one machine into many keys does not
create additional effective byte-hours.

Introductions are admission hints, not transferable reputation balances:

```
BarterIntroduction
  introducer
  target-peer
  scope
  expires-at
  recommended-credit-limit
  failure-domain-hints
  signature
```

The receiver caps total exposure attributable to one introducer and may group
apparently related identities into one failure domain.  A recommendation never
overrides direct negative observations.  It expires and may be revoked.

Signed observations may be shared later, but a universal scalar score is out
of scope: availability depends on observer location, time, transport, and the
specific repository service.


# Replica discovery

Partner discovery and replica discovery are separate problems:

- **partner discovery** finds peers willing to negotiate new leases;
- **replica discovery** finds peers currently serving a known repository.

Partner discovery may remain introduction-only in the first deployment.
Replica discovery is required for the core offline-clone goal.

An active provider periodically emits a short-lived signed hint:

```
ReplicaAdvert
  version
  repository-key
  provider-peer
  snapshot-id
  reachable-via
  capabilities
  issued-at
  expires-at
  signature
```

Advertisements are emitted only for complete active snapshots.  They expire
unless renewed, may be withdrawn, and are never treated as proof of custody.

PEP-10 evaluates delivery over bounded gossip, PEX metadata, a RefChan/index,
and hbs2-hub rendezvous.  hbs2-hub may collect and render adverts, but it is an
optional index rather than an authority: a client verifies signatures and
performs retrieval itself.

Advertising every block is unnecessary.  Advertising repository roots keeps
the index small; once connected, the ordinary hbs2 block plane serves the
reachable graph.


# Replica placement and repair

A repository policy may state either a minimum replica count or an availability
target.  The scheduler prefers peers which improve the observed ensemble, not
merely peers with the highest individual score.

For independent replicas with availability `a_i`, the familiar estimate is:

```
P(at least one online) = 1 - product(1 - a_i)
```

The independence assumption is optimistic.  Placement also considers declared
failure domains and the empirical time windows in which all current replicas
were unavailable.  Failure-domain hints are advisory, but identical operator,
site, address range, schedule, or introducer should prevent the scheduler from
counting replicas as fully independent.

Repair is triggered when a lease expires, a replica becomes incomplete or too
stale, an advertisement stops renewing, or the ensemble falls below target.
Repair follows make-before-break:

1. choose and accept a replacement provider;
2. sync and verify a complete snapshot;
3. publish its receipt and advertisement;
4. confirm the target is restored;
5. only then release the failed or expiring lease where possible.

If no replacement exists, the old provider remains in `Grace` when policy and
capacity permit it, and the operator is told that the repository is at risk.


# Threat model

**Free rider**
  A peer consumes more service than it supplies.  Bilateral credit limits stop
  further admission; delivered byte-hours, not identity or promises, increase
  the balance.

**Silent eviction or corruption**
  A provider deletes or corrupts active data.  Retrieval probes, full scans,
  freshness checks, and clone failures produce separate observations and
  trigger repair.

**Proxying an audit**
  A provider fetches challenged data elsewhere.  The first protocol measures
  timely retrievability and does not claim unique physical custody.  Tight
  deadlines and multi-block samples raise the cost but are not a proof.

**Sybil**
  Many keys attempt to multiply budget or apparent diversity.  There is no
  identity-granted budget; exposure is locally capped, introductions are
  bounded, and correlated identities may be one failure domain.

**Capacity overcommitment**
  A provider signs more leases than it can preserve.  Capacity is reserved per
  lease using logical limits, active pins are durable, and pressure stops new
  admission before eviction.

**Stale mirror**
  A provider serves a valid old repository state.  Receipts and advertisements
  name the snapshot and LWWRef sequence; credit decays after `max-update-lag`.

**Discovery poisoning**
  A peer advertises repositories it does not serve.  Adverts are signed,
  short-lived hints and are verified by retrieval.  Repeated false adverts
  reduce local admission trust.

**Audit denial of service**
  A consumer sends expensive challenges.  Only contracted identities or
  accepted witnesses can probe, with per-contract rate and bandwidth limits.

**Correlated failure**
  Several replicas share one operator, machine, site, or online schedule.
  Placement uses failure-domain hints and empirical joint availability.

**Clock manipulation**
  A peer lies about time to gain credit or extend a lease.  Signed terms use
  bounded wall-clock intervals; accounting is reconciled over acknowledged
  windows, and unreasonable skew is a protocol error.

**Metadata privacy**
  Offers and adverts reveal relationships and repository interest.  Public
  advertising is opt-in.  Contract negotiation may use PEP-05 onion transport;
  encrypted repositories still leak keys, sizes, timing, and provider links.


# Breakdown into sub-proposals

The original draft ordered proof, trust, and contracts before defining the
replication service.  This revision assigns PEP-06..12 by implementation
dependency instead.


## PEP-06: Repository lease, inventory, and pinning

Defines `RepositoryLease`, snapshot identity, canonical inventory, receipts,
the durable state machine, NCQ3 pin integration, restart recovery, size limits,
retention, and the `repo:relay-only` integration.

The detailed draft is [PEP-06](pep-06-repository-lease.md).

This is the first implementation target.


## PEP-07: Observation ledger and retrieval audit

Defines observation records, equal-duration availability windows, confidence
bounds, freshness checks, full-block retrieval challenges, adaptive sampling,
rate limits, and CLI inspection.  Reachability, custody, freshness, and
throughput remain separate dimensions.


## PEP-08: Bilateral contracts and credit

Defines offer/accept/decline/renew/terminate messages, bilateral effective
byte-hour accounting, credit limits, reconciliation, grace, and disagreement
handling.  No transferable currency is introduced.


## PEP-09: Capacity, placement, and repair

Defines operator budgets, capacity reservation, admission, replica targets,
failure-domain-aware placement, renewal policy, make-before-break repair, and
safe expiry/GC.


## PEP-10: Replica advertisements and lookup

Defines `ReplicaAdvert`, expiry and withdrawal, bounded lookup, routing over
the friendnet, and optional hbs2-hub indexing/rendering.  This PEP is required
before the feature can claim to support a fresh clone while the owner is
offline.


## PEP-11: Introductions and partner matching

Defines scoped expiring introductions, bounded initial exposure, revocation,
manual/automatic offer matching, and privacy rules.  It does not define a
global web of trust.


## PEP-12: N-party settlement and erasure coding

Explores barter cycles, non-bilateral settlement, erasure-coded shares, repair
thresholds, and long-tail allocation.  It is not required for the first useful
deployment.


# Implementation roadmap

Each milestone is independently demonstrable.  The exit criteria are part of
the milestone; completing code without the scenario does not complete it.

| Milestone | Outcome | Unlocks |
|-----------|---------|---------|
| M0 | Measured graph and deterministic inventory | A stable lease contract |
| M1 | One manually leased complete mirror | Offline-owner clone from a known provider |
| M2 | Availability and retrieval evidence | Quality-qualified accounting |
| M3 | Bilateral barter and credit | Reciprocal storage without dummy files |
| M4 | Replica adverts, lookup, and hub UX | Clone by repo key while owner is offline |
| M5 | Placement and make-before-break repair | Maintained replica targets |
| M6 | Scoped introductions and matching | Less manual partner selection |
| M7 | N-party and erasure experiments | Optional utilization improvements |

Implementation note (2026-10-30): the provider-local core of M1 is available:
durable manual leases, polling recovery, typed complete-snapshot inventory,
make-before-break local pinning, limits, expiry, CLI lifecycle commands, and
purge protection. M1 is not yet complete by the exit criteria below: signed
receipts, explicit grace/update-lag and aggregate capacity admission, plus the
three-node offline-owner clone scenario remain. M2 and later are unchanged.


## Milestone 0: Measurement and design fixtures

Deliverables:

- record baseline repository graph sizes, update rates, clone times, NCQ3
  physical growth, and poll latency for representative public and encrypted
  repositories;
- specify the canonical inventory and snapshot identifier in PEP-06;
- add deterministic fixtures for an empty repo, shared blocks, rewritten refs,
  tags, hbs2-hub canon, staged proposals, and encrypted data;
- decide what repository history each retention mode keeps.

Exit criteria:

- two implementations of inventory traversal produce the same ordered block
  set and logical size for every fixture;
- the expected physical/logical size difference is measured rather than
  guessed.


## Milestone 1: Local repository lease MVP

Deliverables:

- durable local lease database and state machine;
- manual `lease add/status/terminate` commands;
- integration with existing LWWRef/RefLog polling and `repo:relay-only`;
- lease-owned pin set which survives restart;
- complete-snapshot verification and signed local receipt;
- size limit, update lag, grace, and capacity reservation.

Exit criteria:

- in a three-node test, A publishes, B accepts a lease, A shuts down, and C can
  clone the complete repository from B;
- B restarts during `Syncing` and resumes without advertising a partial copy;
- NCQ3 GC cannot remove blocks pinned by an active lease;
- an oversized update leaves the previous complete snapshot available.


## Milestone 2: Observation and availability MVP

Deliverables:

- observation journal;
- signed reachability probes reusing the peer handshake where possible;
- full-block randomized retrieval audits and periodic full inventory scans;
- equal-duration rolling windows, conservative confidence bounds, freshness,
  RTT, and throughput metrics;
- CLI and hbs2-hub read-only rendering of evidence and derived quality.

Exit criteria:

- fault injection distinguishes offline, missing, corrupt, stale, and slow;
- a peer online 50% of measured windows converges near 50% availability rather
  than 100% because it happened to answer one probe;
- windows in which the observer is offline remain unknown;
- audit bandwidth remains inside its configured limit.


## Milestone 3: Bilateral barter

Deliverables:

- versioned offer, accept, decline, renewal, termination, and receipt messages;
- bilateral effective-byte-hour ledger;
- credit exposure limits and probation;
- durable reconciliation after peers reconnect;
- admission and renewal driven by capacity plus local evidence.

Exit criteria:

- two peers with unequal repository sizes can trade without dummy content;
- a 40%-availability provider accrues approximately 40% of the availability
  component earned by an otherwise equal always-on provider;
- creating additional peer keys does not increase a counterparty's configured
  exposure;
- neither side can gain credit for `Syncing`, expired, missing, or excessively
  stale data.


## Milestone 4: Discoverable replicas and hub UX

Deliverables:

- signed, expiring replica adverts and withdrawals;
- bounded lookup by repository key;
- at least one decentralized delivery path and optional hbs2-hub indexing;
- clone path which tries verified live providers;
- hub UI for replication target, complete replicas, freshness, observed
  availability, and at-risk state;
- operator UI/config for donated capacity and active leases.

Exit criteria:

- a fresh node which knows only the repo key and normal bootstrap peers can
  find B and clone while owner A is offline;
- incomplete, expired, and withdrawn adverts are not returned as live;
- a malicious advert causes a failed attempt but cannot forge repository data
  or become durable state without verification.

This is the first milestone which closes the primary user problem stated by
this PEP.


## Milestone 5: Placement and automatic repair

Deliverables:

- minimum replica count and availability targets;
- time-profile and failure-domain-aware provider selection;
- health monitor and make-before-break replacement;
- renewal throttling and safe expiry under disk pressure;
- explicit at-risk alerts when no replacement exists.

Exit criteria:

- killing one provider causes a new complete replica to appear before the old
  lease is collected when capacity is available;
- replicas in one declared failure domain do not satisfy a multi-domain target;
- correlated offline schedules are not scored as independent availability;
- disk-full tests never partially evict an active complete snapshot.


## Milestone 6: Introductions and automatic matching

Deliverables:

- scoped, expiring, revocable introductions;
- bounded probationary capacity;
- want/offer matching by size, availability class, privacy, and failure domain;
- controls against one introducer multiplying exposure through many identities.

Exit criteria:

- an introduced node can earn its way out of probation without operator file
  edits;
- revocation stops new contracts but does not destroy live leases early;
- direct observations override recommendations.


## Milestone 7: N-party and erasure-code experiments

Only after production measurements from Milestones 1-6:

- compare whole-copy replication with `k-of-n` shares;
- model repair traffic, correlated churn, and small-friendnet sizes;
- decide whether non-bilateral settlement is worth the complexity;
- write PEP-12 with measured parameters before implementation.


# Cross-milestone test matrix

Every milestone which changes lifecycle or accounting is tested against:

- owner offline before, during, and after initial sync;
- provider restart and crash during every state transition;
- network partition and asymmetric connectivity;
- stale LWWRef/RefLog followed by catch-up;
- corrupt, absent, and slow blocks;
- repository growth across its signed limit;
- NCQ3 garbage collection and compaction;
- shared blocks across multiple contracts;
- lease renewal, graceful termination, hard expiry, and clock skew;
- disk full and reduced operator budget;
- encrypted repository storage without decryption authority;
- multiple peer identities on one failure domain;
- old protocol peers which ignore the new ProtocolIds.


# Operational interface sketch

Names are non-normative, but the supported operations are not:

```
hbs2-peer barter budget show|set
hbs2-peer barter offer|accept|decline
hbs2-peer barter lease list|show|terminate
hbs2-peer barter audit show
hbs2-peer barter credit show
hbs2-peer barter replicas <repo-key>
```

hbs2-hub should render repository-level outcomes rather than a mysterious
global score:

- complete replicas and their snapshot age;
- target versus current replica count/availability;
- jointly unavailable windows;
- failure-domain warnings;
- active repair and at-risk state;
- local donated, reserved, and effective capacity.


# Open questions

1. **Canonical inventory.**  Can all public and encrypted repository graphs be
   traversed and sized without repository decryption authority, or does the
   publisher need to supply a signed inventory tree?

2. **Retention modes.**  Current snapshot only, a time window, tagged history,
   and full RefLog retention have very different costs and recovery semantics.

3. **Witnesses.**  Bilateral probes measure availability while the observer is
   online.  Which existing hubs or introduced peers may witness public
   availability without becoming trusted authorities?

4. **Advert routing.**  Bounded gossip, a repository RefChan, PEX metadata, and
   hub indexes have different privacy and scaling properties.  PEP-10 must
   choose and measure at least one path.

5. **Failure-domain evidence.**  Which hints are useful without pretending that
   IP address, ASN, site, or operator declarations are Sybil-proof?

6. **Shared-block accounting.**  Full logical reservation is safe but
   conservative.  Production data may justify a less conservative rule later,
   provided it cannot overcommit active leases.

7. **Probe parameters.**  Window length, sample count, audit frequency,
   bandwidth ceiling, and confidence priors must come from Milestone 0 and 2
   measurements.

8. **Physical custody.**  Is timely retrieval sufficient for the friendnet, or
   do any users require proof that every provider keeps an independent local
   copy?  The latter needs a stronger and more expensive protocol.

9. **Public versus private adverts.**  Public repositories benefit from broad
   discovery; encrypted repositories may need scoped adverts and onion-only
   endpoints.


# Comparison with prior work

| Approach | Relevant lesson for hbs2 |
|---|---|
| Samsara | Closest prior art: storage claims, periodic checks, transient-failure handling, and provider-specific replicas without money or certified identity. |
| Proofs of Retrievability / PDP | A range hash is not a retrievability proof; strong proofs require authenticators/encoding and explicit extraction assumptions. |
| Filecoin | Unique physical replication requires provider-specific sealing and substantially heavier machinery than this friendnet chooses. |
| Storj | Keep online and content-audit outcomes separate; use time windows so audit frequency does not dominate uptime. |
| IPFS Cluster | Minimum/maximum replication, expiring health metrics, balanced failure-domain allocation, and automatic repinning are useful control-plane patterns. |
| IPFS provider records | Persistence and discoverability are separate; provider hints need expiry and periodic renewal. |
| Tahoe-LAFS | Count independent servers/failure domains, not just copies; erasure coding is useful only with a repair and placement model. |
| Pastiche | Content overlap can reduce transfer and physical cost, but should remain a placement optimisation rather than an entitlement. |
| BitTorrent | Short-term tit-for-tat does not directly enforce long-term seeding and can be exploited by strategic clients. |

Primary references:

- Cox and Noble, *Samsara: Honor Among Thieves in Peer-to-Peer Storage*:
  https://web.eecs.umich.edu/~zmao/eecs589/papers/samsara.pdf
- Bowers, Juels, and Oprea, *Proofs of Retrievability: Theory and
  Implementation*: https://eprint.iacr.org/2008/175.pdf
- Filecoin proof-of-storage specification: https://spec.filecoin.io/
- Storj downtime tracking with audit windows:
  https://github.com/storj/storj/blob/c2a97aeb143791dd7edd8bea5bb43558a95b57de/docs/blueprints/storage-node-downtime-tracking-with-audits.md
- IPFS Cluster pin allocation and replication factors:
  https://ipfscluster.io/documentation/guides/pinning/
- Tahoe-LAFS architecture and servers-of-happiness:
  https://github.com/tahoe-lafs/tahoe-lafs/blob/master/docs/architecture.rst
- Piatek et al., *Do Incentives Build Robustness in BitTorrent?*:
  https://www.usenix.org/conference/nsdi-07/do-incentives-build-robustness-bittorrent


# Relation to other PEPs

- **PEP-05 (Tor transport).**  Contract negotiation and scoped adverts may use
  onion transport.  The data structures must not assume an IP endpoint.
- **PEP-02 (ACB).**  Admission policy may restrict counterparties to keys or
  groups named by access-control blocks.
- **PEP-17..22 (hbs2-hub).**  Hub canon and staged proposals are repository
  data covered by retention policy.  hbs2-hub may render local replication
  state and act as an optional advert rendezvous, but does not become a global
  settlement or reputation authority.
- **NCQ3 storage.**  PEP-06 needs an explicit lease pin/root set and must define
  its interaction with sweep, compaction, recovery, and storage accounting.


# Decision gate

Implementation starts with PEP-06 and Milestone 0.  Wire-level barter offers
must not be implemented before the repository lease, inventory, pinning, and
crash-recovery invariants are executable locally.  Public claims that hbs2
solves offline repository availability start only after Milestone 4 demonstrates
a fresh clone finding a complete replica while the publisher is offline.
