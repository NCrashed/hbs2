# BUG: RPC peer introspection shows a different peer set than the live daemon

Status: open, not yet root-caused. Observability-only (does not affect protocol
behavior). Pre-existing; surfaced while testing PEP-05 Phase 3 (onion PEX).

## Summary

`hbs2-peer peers`, `hbs2-peer pexinfo` and `hbs2-peer do peer-info` (all served
over the RPC) report a peer set that diverges from the one the running daemon
actually uses in its protocol threads. On a Tor onion-only node this is glaring:
the RPC does not list the `.onion` peers the daemon is actively connected to,
and instead lists "dead" clearnet UDP peers that cannot have a live session
(UDP is disabled on such a node). An operator diagnosing an onion node with
`hbs2-peer peers` therefore sees a misleading, stale picture.

The actual protocol is unaffected: the PEX policy works (an onion node really
does learn and dial other onion nodes via PEX). Only the RPC introspection is
wrong.

## Impact

- `hbs2-peer peers` / `pexinfo` / `peer-info` are untrustworthy for diagnosing
  connectivity, especially on Tor/onion deployments.
- `pexinfo` in particular misrepresents what the node would gossip.
- No functional/security impact found: the real PEX handler operates on the
  correct (live) locator.

## Evidence (reproduced on the 3-peer onion harness, nix/tor-onion-test.nix)

Same process (`hbs2-onion-bob`), same instant, compared the journal stat lines
(emitted by `peerPingLoop`, which iterates `knownPeers pl`) against the RPC:

- Journal (`[notice] peer ... burst: ... seen ...`): clearnet TCP
  `185.158.248.142:3003`, `81.88.219.217:3003`; the inbound onion peer
  (`tcp://127.0.0.1:<rand>`, a Tor exit); and `tcp://<carol>.onion:9999`.
- RPC `peers`/`pexinfo`/`peer-info`: clearnet UDP `81.88.219.217:7351`,
  `185.158.248.142:7354`; clearnet TCP `185.158.248.142:3003`,
  `81.88.219.217:3003`. No onion, no inbound peer.

Key points:
- The split is consistent, not timing: 3/3 simultaneous samples had the onion
  peer in the journal and absent from the RPC.
- The RPC lists UDP peers (`:7351`/`:7354`) on a node where UDP is disabled
  (`listen "off"`) - they cannot correspond to live sessions, i.e. the RPC
  locator is stale/separate.
- `medianPeerRTT` for the same peer (`185.158.248.142:3003`) differed between
  journal (199 ms) and RPC (91 ms) - suggestive of separate PeerInfo state,
  though this one could also be rolling-median timing.

Conclusion: the RPC path and the protocol path observe **different peer-locator
(and probably PeerInfo) state** at runtime.

## What static analysis says (and why it is puzzling)

By the code there is exactly one locator and one env:
- `hbs2-peer/app/PeerMain.hs:873` - the only `newBrainyPeerLocator` (`pl`).
- `hbs2-peer/app/PeerMain.hs:963` - the only `newPeerEnv pl ...` (`penv`);
  `penv` is never rebound/shadowed.
- Main protocol responders run in `withPeerM penv` and include
  `peerExchangeProto` (PeerMain.hs:1259-1276).
- `peerPingLoop` runs as a `peerThread`, i.e. `withPeerM penv` (the journal
  stat report, `HBS2/app/PeerInfo.hs:184,206`).
- RPC context sets `rpcPeerEnv = penv` (PeerMain.hs:1353); handlers do
  `withPeerM (rpcPeerEnv co)` (e.g. `RPC2/Peers.hs`, `RPC2/PexInfo.hs`,
  `RPC2.hs:241`).
- `runPeerM` / `withPeerM` (`HBS2/Actors/Peer.hs:455,480`) use the passed env
  as-is; they do not copy it. `newPeerEnv` stores the passed `pl` verbatim.

So every path resolves `getPeerLocator` to `penv.envPeerLocator` (one TVar set),
and reading it should yield identical results. The observed divergence is
therefore a runtime fact not visible from the source - the next step has to be
instrumentation, not more reading.

## Suggested investigation

1. Tag env/locator identity. Add a unique id to `PeerEnv` (or print the
   `StableName`/address of the locator's TVar) and log it in both
   `peerPingLoop` (PeerInfo.hs:190, next to `debug "known peers"`) and an RPC
   handler (RPC2.hs:241 `peer-info`). Rebuild, deploy once, compare: confirm
   whether two distinct locator instances exist.
2. If two instances: find where the second is created/captured. Candidates to
   examine: the `hbs2-peer run` -> exec `start` wrapper (PeerMain.hs ~275);
   how the `async`/`runContT` threads in `runPeerM` capture the env; whether the
   RPC `runReaderT rpcctx` path somehow ends up with a different env than the
   protocol loop despite `rpcPeerEnv = penv`.
3. If one instance: the divergence is in the read path - inspect
   `knownPeers` / `knownPeersForPEX` / `getKnownPeers` and the `Sessions`
   cache (`_envSessions`) for a per-call snapshot or a second cache.

## Reproduction

`nix/tor-onion-test.nix` (3 onion peers: alice -> bob -> carol). Once bob holds
a live onion session to carol (visible in `journalctl -u hbs2-onion-bob` as
`peer tcp://<carol>.onion:9999 ... seen`), run `hbs2-peer pexinfo -r
127.0.0.1:13362` and observe carol is absent.

## Notes

- Not introduced by the Tor work: `RpcPeers`/`getKnownPeers`/`pexinfo` predate
  it. Onion peers merely made the divergence obvious (it was invisible while
  every peer was a clearnet `PeerL4`).
- Once fixed, `nix/tor-onion-test.nix` verification can switch from reading the
  journal back to the RPC.
