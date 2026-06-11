# RESOLVED (not a daemon bug): RPC peer introspection appeared to diverge from the live daemon

Status: resolved 2026-06-10. Root cause was a diagnostic / test-environment
artifact plus a dead CLI option, NOT a peer-locator split in the daemon. No
protocol or observability defect exists in hbs2-peer. Surfaced while testing
PEP-05 Phase 3 (onion PEX).

## What looked like the bug

On the 3-peer onion harness, `hbs2-peer peers` / `pexinfo` / `do peer-info`
(served over the RPC) reported a peer set that diverged from the one the running
daemon used in its protocol threads: the RPC listed "dead" clearnet UDP peers
and never listed the `.onion` peers the daemon was actively connected to, while
the journal stat lines (`peerPingLoop`) showed the correct live onion set. The
split was consistent across simultaneous samples, which ruled out timing.

## Root cause

The CLI was talking to a **different daemon** than the one whose journal we were
reading. Two independent facts combined:

1. **`-r/--rpc` was a dead option.** `withMyRPC` / `withRPCMessaging` in
   `hbs2-peer/app/CLI/Common.hs` resolved the RPC socket purely from
   `getRpcSocketName conf` and never read `_rpcOptAddr`. So `hbs2-peer peers -r
   127.0.0.1:13362` silently ignored the `-r` target.

2. **`getRpcSocketName` only honors `rpc unix "<path>"`.** It pattern-matches
   `ListVal (Key "rpc" [SymbolVal "unix", LitStrVal n])` and otherwise falls back
   to the default `/tmp/hbs2-rpc.socket` (`PeerConfig.hs:253`,
   `getRpcSocketNameM`). The harness used `rpc "127.0.0.1:<port>"` (an
   `addr:port` literal, not the `unix` form), so every test peer's socket name
   resolved to the default `/tmp/hbs2-rpc.socket` too.

On the dev box a production `services.hbs2-peer` instance runs with
`PrivateTmp=false`, so it owns the host `/tmp/hbs2-rpc.socket`. The test peers
ran with `PrivateTmp=true`, isolating their own `/tmp/hbs2-rpc.socket` inside
their namespaces (unreachable from the host). Net effect: every `hbs2-peer ...`
invocation from the host hit the **production peer** via `/tmp/hbs2-rpc.socket`,
regardless of `-r`. The production peer's known set (its configured known-peer
`tcp://81.88.219.217:3003`, plus its UDP peers) is exactly what the RPC showed.

The journal we compared against came from the test peer (`hbs2-onion-bob`),
which really was connected to carol over onion. Two different daemons, two
different peer sets - not one daemon with two locators.

### Confirmation

Debug instrumentation in the `peers` RPC handler wrote a file via a relative
path (CWD = the answering process's WorkingDirectory). It landed in
`/var/lib/hbs2-peer/` - the **production** peer's `storageDir`/WorkingDirectory -
not the test peer's `/var/lib/hbs2-onion-bob`. That pinned the answering process
as the production peer. (Instrumentation has since been reverted.)

## Fix

1. **CLI `-r` now works** (`hbs2-peer/app/CLI/Common.hs`): `withMyRPC` and
   `withRPCMessaging` use `fromMaybe (getRpcSocketName conf) (view rpcOptAddr o)`,
   so `-r/--rpc <path>` overrides the config and targets a specific peer's RPC
   unix socket. Help text changed from `addr:port` to "path to the peer RPC unix
   socket (overrides config)" (the daemon exposes a unix-socket RPC, not TCP).

2. **Harness gives each peer its own socket** (`nix/tor-onion-test.nix`): peers
   now set `rpc unix "<stateDir>/rpc.socket"` instead of `rpc "127.0.0.1:<port>"`,
   so the three never collide on `/tmp/hbs2-rpc.socket`. `PrivateTmp` is dropped
   (no longer needed, and its removal makes each peer's RPC reachable from the
   host). Query a specific peer with
   `hbs2-peer peers -r /var/lib/hbs2-onion-bob/rpc.socket`.

## Takeaways

- There is no peer-locator divergence in the daemon. The static analysis in the
  original ticket was correct: one env, one locator, every path resolves the same
  `getPeerLocator`. The "two states" were two processes.
- When diagnosing a multi-peer-on-one-host setup, always target the intended
  peer's socket explicitly with `-r <socket>`; otherwise the shared
  `/tmp/hbs2-rpc.socket` wins.
- The onion-harness verification can now use the RPC directly (per-peer `-r`),
  not just journald - but journald remains a valid cross-check.
