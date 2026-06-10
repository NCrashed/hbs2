PEP-05: tor-transport-and-pex-policy

Goal
====

Let operators deploy an hbs2 peer behind a Tor onion service
(`*.onion`), in order to:

1. Traverse NAT without host-network port forwarding.
2. Hide the node's physical location and real IP from peers and from
   an observer on the local network (a local ISP, an active attacker
   on the link).

Baseline assumption: onion peers are usually reachable *only* over
Tor. Connecting to such a peer from clearnet is impossible (and that
is fine: the anonymity is precisely in that). PEX and the handshake
must account for this: an onion peer's address must not be disclosed
to peers that cannot reach it, and the onion peer itself must not
advertise its address where it is useless.

I2P is out of scope for this PEP.

Threat model
============

- **Local ISP / passive observer.** Sees the peer's TCP traffic. Must
  not be able to learn from the address whom the peer talks to in the
  hbs2 network. Tor covers this for outbound; for inbound, the
  listening socket must be on 127.0.0.1 with only the HiddenService
  exposed outward.
- **Active on-path attacker.** Can delay, drop, modify messages. Tor
  partially covers this (traffic is encrypted, hops do not trivially
  correlate). Padding and timing obfuscation against targeted
  correlation are a separate PEP (see follow-ups).
- **Compromised bridge peer.** A node holding both networks (clearnet
  + onion) can correlate traffic. Mitigation: the PEX policy does not
  cross classes, so a compromised bridge sees traffic that already
  went through it; it creates no new address leaks.

Design
======

A. Address tagging
------------------

Add an explicit network-class field to `PeerAddr L4Proto`:

```
data NetworkClass
  = Clearnet
  | Onion
  -- reserved for the future: I2P, Yggdrasil, ...
  deriving (Eq, Ord, Show, Generic)
```

Parsing in `fromStringMay`:
- host ends with `.onion` (any char count, lower-case) becomes
  `Onion`
- otherwise becomes `Clearnet` (IPv4 / IPv6 / DNS name all go as
  Clearnet, because for the "reachable without Tor" PEX policy they
  are the same thing).

Serialisation (`Serialise PeerAddr`): add a new versioned CBOR tag.
Old peers read only Clearnet variants and ignore unknown tags (need
to check that serialise allows this; if not, version it via an
optional fallback as GroupKeySymm does, see PROTOCOL.md "Hand-rolled
Serialise").

B. Peer capability in the handshake
-----------------------------------

In `PeerHandshake` (ProtocolId 4) each side declares its *reachable
transport classes*:

```
data PeerCapabilities = PeerCapabilities
  { reachableVia :: Set NetworkClass
  -- ...
  }
```

Old peers (without the field) are assumed to be `{Clearnet}` by
default. New tor-only peers declare `{Onion}`, bridge nodes
`{Clearnet, Onion}`.

`reachableVia` is "how I can be reached", not "what I can do".
Outbound over SOCKS5 is something any peer can do; that is a detail
of its config, not its public characteristic.

C. PEX policy
-------------

When building the `PeerAnnounce` / `PeerExchange` payload for peer B,
the sender A filters its known-peers by the rule:

```
forwardAddress addr toPeer =
  any (`Set.member` reachableVia toPeer)
      (classesReachable addr)
```

that is, "an address goes to B only if B has at least one class by
which the address is actually reachable".

Concrete cases:

| A sends to B  | B clearnet | B onion | B bridge |
|---------------|------------|---------|----------|
| clearnet addr | yes        | no      | yes      |
| onion addr    | no         | yes     | yes      |

A's own address is filtered by the same rule, so a tor-only A does
not hand its `.onion` to a clearnet peer (which cannot reach it), and
hands nothing else (it has nothing else).

Degenerate case: a tor-only A talks to a clearnet-only B. A stays
silent about itself and about its onion siblings. Why do they talk at
all, then? Through a bridge: B knows the bridge, A knows the bridge,
each receives the other's traffic through the bridge as a relay. hbs2
already has relay semantics (refchans, mirror nodes); direct
peer-to-peer is not required.

D. Bridge nodes - what is allowed, what is not
----------------------------------------------

Allowed:
- Listen on clearnet TCP/UDP and an onion HiddenService at the same
  time.
- Keep addresses of both classes in their known-peers.
- Forward addresses according to the policy in section C.

Not allowed (at the policy level, not the code level):
- Substituting an onion address with one's own clearnet address "for
  convenience".
- Using onion siblings as a relay for outgoing clearnet traffic and
  vice versa without explicit consent (that is a refchan-level
  operator decision).

Leaks through traffic correlation (the same blocks showing up on both
transports at once) are out of scope for this PEP.

E. Outbound over SOCKS5
-----------------------

The basics already exist: `tcp.socks5 = "127.0.0.1:9050"` +
`Messaging.TCP` calls `connectSOCKS5`. What is needed:

1. The `PeerAddr` parser accepts `.onion` hostnames (see A). It
   currently chokes on non-IP.
2. `connectSOCKS5` is called with the hostname, not an IP, so that Tor
   does the resolve itself. Check that the current call
   `connectSOCKS5 ... (show i) ...` in [`Messaging/TCP.hs:347`] passes
   a string, not a pre-resolved IP, and skips the pre-resolve step for
   `Onion`.
3. If SOCKS5 is not configured, an attempt to connect to a `.onion`
   address should return a clear error, not a timeout.

F. Inbound - listen only on 127.0.0.1
-------------------------------------

No code change. Configuration:

```
listen-tcp "127.0.0.1:10351"
```

and alongside it `torrc`:

```
HiddenServiceDir /var/lib/tor/hbs2/
HiddenServicePort 10351 127.0.0.1:10351
```

For a tor-only deployment UDP is not used (Tor does not carry it).
hbs2-peer must work correctly with TCP transport alone; check that
block fetch / reflog / pex do not assume UDP.

G. Advertising one's own address
--------------------------------

Today a peer advertises what it listens on. For a tor-only deployment
that is `127.0.0.1`, useless to neighbours. An explicit declaration is
needed:

```
peer-public-address "tcp://abc...xyz.onion:10351"
```

In the absence of this setting the peer advertises nothing about
itself (it does not guess the "real" address via getsockname). This
requires reworking the PeerAnnounce logic; today the announce
apparently happens automatically.

Phased plan
===========

**Phase 1: outbound Tor (1-3 days). DONE.** Changes in
hbs2-core/hbs2-peer:
- (A) the `PeerAddr` parser accepts `.onion` hosts (as a name-carrying
  variant). The `NetworkClass` tag itself is deferred to Phase 3, where
  it is needed (for the PEX policy); outbound dialing does not require it.
- (E) SOCKS5 path with onion hosts, verified live against a real Tor
  daemon.
- A "tor-outbound" recipe with `tcp.socks5` (in `docs/multi-machine.md`,
  "Approach 3").

After this a peer operator can ship the config
`tcp.socks5 = "127.0.0.1:9050"` and have
`known-peer "tcp://xxx.onion:443"`; it all already works.

**Phase 2: inbound Tor + listen hardening. DONE (except G).**
- (F) onion-only / TCP-only operation: two config gates, `multicast off`
  and `bootstrap off`, let a peer run without LAN discovery or the
  hard-coded clearnet DNS bootstrap (both otherwise crash or leak on a
  Tor-only host).
- NixOS module: `services.hbs2-peer.enableTor = true` applies
  `services.tor.enable` + `client.enable` + a v3 HiddenService for
  hbs2-peer, binds to 127.0.0.1, and sets the onion-only knobs. See
  [`nix/nixos-module.nix`](nix/nixos-module.nix).
- [`docs/TOR_DEPLOYMENT.md`](docs/TOR_DEPLOYMENT.md): end-to-end recipe.
- (partial C) conservative anti-leak guard: name-carrying (`.onion`)
  addresses are now excluded from PEX entirely (`getAllPex2Peers`), so a
  node never gossips an onion address to anyone, including clearnet peers.
  An onion node reaching clearnet over Tor is fine; a clearnet node
  learning onion addresses is not. The selective version (onion -> onion
  sharing allowed, driven by the network-class policy) is Phase 3.
- (G) `peer-public-address` was deferred until the network-class policy
  existed; now DONE - see Phase 3 below.

**Phase 3: PEX policy + capability handshake. DONE (B + C).**
- (B) `PeerData` (the `PeerPong` payload) carries `reachableVia ::
  Set NetworkClass`, declared from the `network-class` config key
  (`clearnet` default | `onion` | `bridge`). Backward-compatible
  hand-rolled `Serialise`: the original two fields are written verbatim
  and the set appended, so old peers ignore the trailing bytes and new
  peers reading old data default to `{Clearnet}` (verified by round-trip).
- (C) `getAllPex2Peers` forwards an address to a recipient only if
  `classOf addr` is in the recipient's declared `reachableVia` (looked up
  from its `KnownPeer` session; unknown/old -> `{Clearnet}`). This
  replaces the Phase-2 blanket onion exclusion with the selective rule:
  onion -> onion is allowed, clearnet never receives onion.
- (A) No `PeerAddr` Serialise change was needed: the class is *derived*
  from the address (`classOf`, `.onion` -> `Onion`), not stored on it, and
  the Phase-1 name variant already serialises compatibly.
- `PROTOCOL.md` updated with the hand-rolled `PeerData` versioning.

**(G) peer-public-address. DONE.**
- A node advertises its own public address(es) via the `peer-public-address`
  config key (repeatable; e.g. a bridge sets a clearnet and an onion one).
- Disclosed through peer-meta, but class-gated: `mkPeerMeta` includes an
  address only if its class is in the *recipient's* `reachableVia`, so an
  onion address is handed only to onion-capable peers and never reaches a
  clearnet peer (verified: a `{Clearnet}` recipient gets nothing, an
  `{Onion}` recipient gets the onion). `peerMetaProto` now builds the meta
  per requester from its handshake-declared classes.
- The receiver (`fillPeerMeta`) dials the announced address directly. This
  is how an inbound onion peer - which otherwise appears only as the Tor
  exit `127.0.0.1` - becomes known by its real `.onion`, so it can be
  redialed and gossiped via PEX.
- Still open: broader isolation tests; and the RPC peer-introspection bug
  found while testing (see docs/notes/rpc-peer-locator-divergence.md).

**Phase 4: logging / debug audit (half a day).**
- Walk through `debug $ ... pretty pip` in hbs2-peer.
- Separate the "technical ID" (PeerNonce) from the "address". Log
  addresses only under `--debug`, not in the `--trace`-default.
- This is needed before any serious deployment, otherwise the operator
  leaks via logs.

Follow-ups (separate PEPs)
--------------------------

- **Padding / cover traffic.** Constant background noise so a passive
  observer does not see the "a new block crossed the link"
  correlation. Worth discussing the ROI; mobile peers resent the extra
  traffic.
- **Timing obfuscation.** Random delays in block-announce handling so
  an active attacker has a harder time correlating "peer A got a block
  at time T, peer B at T+delta, therefore A-B are linked".
- **Onion-only reflog / lwwref handling.** If a peer advertises a
  refchan reachable only over the onion network, how should that
  affect the mirror policy of clearnet peers? Possibly not at all, but
  check.
- **I2P.** If ever needed, carve out a separate PEP. Not fully clear
  whether SAM API or SOCKS-only via i2pd; needs separate research.

Open questions
==============

1. **The `serialise` package and forward-compat.** Can we extend
   `Serialise PeerAddr` so that old peers read new bytes as a
   Clearnet fallback? GroupKey 'Symm Serialise does this via
   `peekAvailable` + a version tag; the same pattern applies.
2. **Does `connectSOCKS5` work for an onion target today?** Need to
   read the source of `network-simple-tls` or the wrapper in use; some
   library versions do a local DNS resolve before handing the target
   to SOCKS, which for `.onion` yields NXDOMAIN.
3. **UDP-only features.** Multicast peer discovery, block-announce
   semantics - what assumes "we have UDP"? If anything relies on it,
   for a tor-only peer that is dead code and needs graceful
   degradation.
4. **PeerHandshake versioning.** There is apparently no capability
   negotiation today. Should it be introduced as part of this PEP, or
   as a separate, more general one, so that bootstrap-mode,
   mailbox-only-mode, etc. later go through the same mechanism?

Re-verification on change
=========================

If this PEP is accepted and implemented, do not forget to update:
- [`PROTOCOL.md`](PROTOCOL.md): a new entry on versioning the
  `PeerAddr` Serialise (Foundations / Hand-rolled section), the new
  PeerHandshake payload (PeerHandshake section), capability
  negotiation (a new section).
- [`ARCHITECTURE.md`](ARCHITECTURE.md): the Network transport section
  mentions Tor outbound and the onion-listen pattern.
- [`QUICKSTART.md`](QUICKSTART.md): add a "Where to go next" item about
  Tor.
- [`nix/nixos-module.nix`](nix/nixos-module.nix): new options
  `enableTor`, `peerPublicAddress`.
