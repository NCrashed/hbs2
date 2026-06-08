Barter storage: overall vision and breakdown into PEPs

Status: draft, discussion started 2026-06-04, local, not published.
Author: NCrashed (Anton Gushcha)


Goal
====

Today in hbs2 the decision about which data a given node stores is
made by hand: the operator must explicitly subscribe to refs
(`hbs2-peer poll add ...`), explicitly fetch blocks
(`hbs2-peer fetch`), explicitly tell the peer about known addresses
(`known-peer ...`). There is no automatic replication between peers
with different payloads.

This is fine for the small-trusted-group scenario ("I know Boris, we
share a git repo between our VPSes"), but it does not scale to the
next step:

1. I want to publish something. Who will store it besides my node? If
   only me, it is not P2P. If I want to cooperate with someone on
   storage, I have to negotiate by hand.
2. Hubs with a large storage budget have nothing to spend it on except
   explicitly subscribing to specific refs.
3. Small nodes with limited storage need a way to say: "I am willing
   to store N bytes of yours, provided N is proportional to what you
   store of mine."

The idea is a system of automatic barter, where storage capacity is
exchanged between peers by mutual consent, with continuous
verification and local reputation.


Why this is not Filecoin/Sia/Storj
==================================

Existing decentralised-storage systems solve a similar problem, but
through monetisation + blockchain + heavy crypto
(proof-of-spacetime). That is the right path for an **open anonymous**
network, but not for the hbs2 scenario.

hbs2 targets small-trusted-group setups - friends-of-friends, a few
teams, personal deployments. This is a **friendnet**. There is no need
for a "pay per byte" crypto-economy here; what is needed is careful
automation of what already happens by hand: "I store yours, you store
mine." No tokens, no blockchain, no global consensus.

The closest analogue in spirit is BitTorrent with tit-for-tat
reciprocity, but there reciprocity works **only while downloading a
single torrent**. After seeding completes there is no incentive. Our
idea is to extend reciprocity to post-download long-term storage.


Threat model
============

- **Free rider.** A neighbour gets its blocks stored by me but quietly
  does not store its obligations. Mitigation: continuous probing with
  cryptographic proof, reputation downgrade on failure.
- **Sybil.** An attacker creates N fake identities to get an N-fold
  storage budget. Mitigation: identity cost is not PoW (we are not a
  blockchain) but **proof of own contribution** - you must have your
  own signed content before you can demand that yours be stored. Plus
  bootstrap only through an introduction from an existing trusted peer.
- **Storage cheat.** A neighbour claims to store a block but on a
  probe fetches it on the fly from the real holder. Mitigation: probes
  must be verifiable by content (a Merkle proof of a random byte
  range), and probe latency is bounded.
- **Eviction griefing.** A neighbour silently deletes my data after
  the contract is signed. Mitigation: same as free rider - probes +
  reputation.
- **Metadata privacy.** The mere fact that "Alice barters with Bob"
  reveals the social graph. May be OK for a small group, bad for an
  adversarial one. Integration with PEP-05 Tor to hide such links is
  possible. **Deferred until a concrete use case.**


Non-goals
=========

- No blockchain / global consensus.
- No monetisation (no tokens, payments, escrow).
- No proof-of-spacetime in the crypto-economic sense (only local
  probing checks).
- No global reputation. Reputation is local to each node;
  recommendations are transferable but the decision always rests with
  the recipient.
- No automatic discovery of new barter partners over the public
  network. Only through introductions.
- No erasure coding in the first stage. Maybe later.


Architectural principles
========================

1. **Locality of decisions.** Each node decides for itself whom to
   barter with and how much to store. No global rules.

2. **Reputation = local CRDT.** A peer's score is stored locally,
   updated by probe results. Recommendations from others are signed,
   advisory, not binding.

3. **Minimum of new primitives.** Reuse the existing storage layer
   (NCQ3), signed envelopes (PROTOCOL.md), and the ProtocolId
   mechanism for the wire format.

4. **Capacity-proportional budget.** The size of the barter budget is
   tied to the amount of one's own content. A big hub with terabytes
   of its own can barter terabytes of others'; a phone peer with
   megabytes of its own is limited to megabytes.

5. **Incrementality.** We do not try to describe the whole system at
   once. The probing protocol is useful on its own, barter is the next
   step, recommendations are the third. Each step is closeable and
   testable without the next ones.


Breakdown into PEPs
===================

In dependency order (each next one builds on the previous):


PEP-06: Proof-of-custody probing
--------------------------------

A standalone module. No barter, no reputation, no budget - **only the
mechanics of "prove you store block X".**

In scope:
- Wire protocol: `BlockProbeRequest`, `BlockProbeReply`.
- Request: block hash + a random byte range [a, b].
- Reply: SHA256 of the contents of that range + a peer-key signature.
- Latency is bounded; expired replies are discarded.
- Only for blocks larger than minSize (for small ones it proves
  nothing - they can be loaded on the fly).

Not in scope: barter, reputation, what the peer does with the result.

Useful on its own: an operator can manually verify that a peer
honestly stores what it promised. Low implementation cost, does not
break backward compatibility (a new ProtocolId; peers without support
silently ignore it).


PEP-07: Local trust ledger
--------------------------

A local store of per-peer reputations. No network.

In scope:
- Record structure: `(peer_pubkey, score, last_probe_time, history)`.
- The file is kept in the peer's storage directory; not replicated.
- Score evolves from probing failures (PEP-06) and explicit operator
  input ("I trust this peer more").
- A CLI to view/edit: `hbs2-cli hbs2:trust:list`, `hbs2:trust:set`,
  `hbs2:trust:bump`.

Not in scope: reputation gossip, recommendations, contracts.

Used as a data block for decisions in PEP-08+.


PEP-08: Bilateral barter contracts
----------------------------------

A two-sided contract between peers: "I store your tree X of size Y
bytes for TTL T, you store my tree X' of size Y' bytes".

In scope:
- Wire protocol: `BarterOffer`, `BarterAccept`, `BarterDecline`.
- Contract structure: signatures of both sides, tree hashes, sizes,
  TTL, protocol version.
- Local storage of active contracts (a per-peer log).
- Renewal: automatic if both nodes are online and reputation is >=
  threshold.
- Termination: graceful (a signed termination message), and timeout
  (if the counterparty does not respond for T minutes, consider it
  terminated).
- Probing based on PEP-06 runs automatically on a schedule for each
  active contract.

Not in scope: discovery of new partners (Phase 1 is explicit peer
setup), N-party (Phase 1 is bilateral only), pools.

This PEP gives the barter MVP. Enough for a proof of concept on 2-3
nodes.


PEP-09: Capacity budget
-----------------------

Managing "how much of others' I am willing to store".

In scope:
- Peer config: `barter-budget-bytes`, `barter-redundancy-cap` (for
  example "at most 20x of my own content").
- Accounting of space occupied by barter.
- Eviction policy on overflow: low-reputation evicts first, then
  oldest.
- Notify the counterparty of graceful termination on eviction.

Not in scope: dynamic budget adjustment (the user changes the config
by hand).


PEP-10: Introductions and transitive trust
-------------------------------------------

Solving the bootstrap problem. A new node with no reputation gets it
through signed recommendations from existing trusted peers.

In scope:
- Format: `BarterIntroduction { introducer: PeerKey, target: PeerKey,
  recommendation_score: 0..1, signature }`.
- Wire protocol: requesting/offering introductions.
- Effective trust computation: `min(introducer_trust * recommendation,
  cap)`.
- Transitive depth limit (1-2 levels, no more).

Not in scope: web-of-trust algorithms (PageRank-like). Simple
multiplicative propagation in the first iteration.


PEP-11: Discovery (optional, future)
------------------------------------

Automatic search for new barter partners via PEX or
introduction-broadcast. Deferred until there is feedback from using
PEP-08..10 in practice.


PEP-12: N-party pools (optional, future)
----------------------------------------

Solving the long-tail problem (popular blocks get hyper-redundancy,
rare ones get zero). A pool of 3+ peers that accept data not directly
from each other but through rotation. More complex, not blocking.


Open questions
==============

1. **Probing granularity.** At what block size does probing become
   "honest"? For 256KB blocks (the default block size in hbs2),
   loading from another node takes milliseconds, and any "proof"
   proves nothing. Probing makes sense from the tree level and up. A
   concrete lower bound is needed (1 MB? 10 MB?).

2. **Barter metadata privacy.** The social graph of "who barters with
   whom" leaks to any node on the network that sees the wire protocol.
   May be acceptable for friend-of-friend, bad for adversarial. Full
   hiding requires Tor (PEP-05). It may be enough to onion-route only
   Offer/Accept and do probes in the clear - a simplification.

3. **Probing cost vs frequency.** On a mobile node with 200 contracts,
   probes every 30 minutes is a lot of traffic and battery. Adaptive
   probing (probabilistic probes with an exponential backoff on stable
   contracts) is a must, but the exact formula is open.

4. **Identity hardness.** What stops me from creating 100 key pairs
   and demanding 100x barter? The current defence is "an identity must
   have its own content". But how to measure "own content"? Just the
   presence of trees signed with one's key? Size? Age? This is part of
   PEP-08 that is not yet thought through.

5. **Erasure coding.** Currently a contract = "one node = one copy".
   With 3 counterparties I have 3x redundancy. Instead one could do
   5-of-3 erasure coding - each node stores a bit less, but any 3 of 5
   reconstruct. That is a different PEP, not blocking.

6. **Anti-Sybil via the social graph.** An alternative to
   identity-hardness via own-content: introduction-only joining. Only
   someone who received an introduction from an already trusted peer
   may participate. Stronger against Sybil, but turns hbs2 into a
   jealous friendnet. This may be the right choice for our scenario -
   to be discussed.


What we do first
================

PEP-06 (proof-of-custody probing). Narrow, testable, useful on its
own. Once it works on the current network, we can move toward PEP-07
and PEP-08 in parallel (they do not depend on each other).

PEP-09 makes sense only after PEP-08 gives an MVP and we understand
how storage is really loaded. PEP-10 is needed only when the first
external users beyond the first-movers appear.


Comparison with alternatives
============================

| Approach | Their weakness | What hbs2 takes |
|---|---|---|
| Filecoin | Heavy crypto-economy, blockchain | Not used |
| Sia/Storj | Same + payments | The erasure-coding idea, later (PEP-12+) |
| IPFS Cluster | Manual allowlist | What we have now |
| Tahoe-LAFS | Client/server split | Not a fit |
| BitTorrent | Reciprocity only during download | Extend to post-download |
| Tor onion services | Not about storage | Take the introduction-points idea (PEP-10) |


Relation to other PEPs
======================

- **PEP-05 (Tor transport).** Barter can optionally run over onion
  routing for metadata privacy. Not blocking, but worth keeping
  compatibility in mind.
- **PEP-02 (ACB).** Access control blocks may be needed at the "whom
  am I willing to barter with" decision layer - for example "only
  those in my refchan X". To consider.
- **PEP-03/04.** Intersections not investigated; check separately.
