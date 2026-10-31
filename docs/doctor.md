# Diagnose a local peer

Run the doctor from the same user account and environment as the peer:

```text
hbs2-peer doctor
```

The command does not write configuration or stored data. It checks the local
peer and its network boundary in seven always-on steps:

- `config` — which configuration file was selected and whether it exists;
- `rpc` — which Unix socket was selected and whether the daemon answers;
- `version` — whether the CLI and the running daemon have the same version;
- `peers` — how many network peers the daemon currently sees;
- `bootstrap` — whether each configured bootstrap DNS name returns at least
  one usable peer address;
- `configured-peers` — whether every `known-peer` entry parses, answers a
  signed handshake, and exposes the identity that signed it;
- `peer-metadata` — the signed network classes and the latest advertised TCP,
  HTTP, public-address, and relay mailbox PoW values for connected peers.

The bootstrap check makes bounded DNS TXT queries. It uses the same
`bootstrap-dns` entries and built-in `bootstrap.hbs2.app` name as the peer,
and counts only records that parse as supported peer addresses. With
`bootstrap off`, it makes no bootstrap query and reports the check as skipped.

The configured-peer check sends a bounded diagnostic handshake probe to each
`known-peer`. A successful result names both the configured address and the
verified signing key. This can refresh the daemon's in-memory known-peer state,
but does not rewrite its config or persistent storage. With no `known-peer`
entries, the check is skipped.

The peer-metadata check reads a snapshot from the running daemon. Network
classes come from the signed handshake. The remaining fields come from the
latest peer-meta announcement and are treated as untrusted input: a malformed
port, public address, or PoW floor produces a warning naming the peer and key.
An omitted optional field is reported as not advertised; a metadata response
that has not arrived yet is a separate warning and never masquerades as an
empty policy.

To diagnose the repository path as well, name its public key:

```text
hbs2-peer doctor --repo 9gtFy65ap1Hk9Mc71pMjc32zFsKcNZLVPWbBAbnkE4dP
```

This adds six ordered checks: `repository-key`, `repository-lwwref`,
`repository-manifest`, `repository-reflog`, `repository-snapshot`, and
`repository-sigils`. The first failed boundary has an actionable error; later
boundaries are skipped instead of reporting consequences as separate causes.

The repository probe is read-only. It does not fetch an LWWRef, subscribe to a
RefLog, request missing blocks, or announce a sigil. A complete snapshot means
that every block reachable from the current RefLog head is already present on
the selected peer, so that peer is a complete local provider. The sigil check
then verifies that every declared mailbox has a declared sigil, its complete
bytes are present, its signature is valid, and it names that mailbox.

For the complete contribution path, run the product-level doctor from a clone:

```text
hbs2-hub doctor --repo <repository-key>
```

It runs the same D1-D3 peer and repository checks, then adds four ordered forge
checks: `forge-mailbox`, `forge-policy`, `forge-sender`, and
`forge-delivery`. `--author` and `--sender` can select another contributor
identity; otherwise the command uses the same repository and identity defaults
as `hbs2-hub issue new` and `hbs2-hub pr new`.

A healthy local peer prints one line per check and ends with `doctor: OK`.
Every failed check prints a `remedy:` line. A warning, such as having no
connected peers yet, keeps a successful exit status; a failure makes the
command exit non-zero.

## Start with the common case

Start the peer in one terminal:

```text
hbs2-peer run
```

Then diagnose it in another:

```text
hbs2-peer doctor
```

If the peer is managed by a service, leave the service running and invoke the
doctor as the user that can access its RPC socket.

## Select a different config or socket

The doctor uses the same default configuration as `hbs2-peer`. Override it
when diagnosing another instance:

```text
hbs2-peer doctor --config /path/to/peer-config-directory
hbs2-peer doctor --rpc /path/to/hbs2-rpc.socket
hbs2-peer doctor --rpc /path/to/hbs2-rpc.socket --repo <repository-key>
```

`--config` names the directory containing the file named `config`, not the
file itself. `--rpc` takes precedence over the socket selected by that
configuration. It is especially useful when a service and an interactive peer
use different sockets.

The command's complete option list is always available locally:

```text
hbs2-peer doctor --help
```

## Machine-readable report

For scripts and support reports, request the stable JSON document:

```text
hbs2-peer doctor --json
hbs2-hub doctor --repo <repository-key> --json
```

The document has `contract: 1`, `document: "doctor"`, an overall `ok` value,
and a `checks` array. Each check contains a stable name, status, message, and
optional remedy.

## Reading common results

- `configuration file not found` — run `hbs2-peer init`, or select the intended
  config with `--config`.
- `peer did not answer` — start or restart the daemon; if it uses a non-default
  socket, pass that path with `--rpc`.
- `CLI is ..., daemon is ...` — the installed tools and running process are
  different builds. Upgrade them together, restart the daemon, and run the
  doctor again.
- `no network peers are connected` — the local process works, but it currently
  has no network path. Check `hbs2-peer peers`, `known-peer`/`bootstrap-dns`
  configuration, and firewall reachability.
- `no usable peer addresses resolved` — DNS failed, returned no TXT records,
  or returned records that are not supported peer addresses. Check the named
  DNS record or add a reachable `known-peer` entry. If DNS discovery was
  intentionally disabled for this peer, set `bootstrap off` in its config.
- `no configured peer answered a signed handshake` — all configured addresses
  were syntactically valid, but none completed the peer handshake. Check the
  addresses, the remote services, and firewall reachability.
- `invalid known-peer entries` — fix or remove the named entries. Invalid
  addresses are not silently counted as unreachable peers.
- `peer metadata has not been observed` — the handshake completed before the
  asynchronous metadata exchange. Run doctor again; if it persists, inspect
  the peer logs and the remote peer's metadata protocol support.
- `invalid ...` under `peer-metadata` — the named peer advertised a present
  value that does not parse or lies outside its protocol range. Fix the remote
  peer configuration rather than treating it as an absent default.
- `no LWWRef value is available` — the selected daemon has not discovered the
  repository key. Use the printed `hbs2-peer lwwref fetch` command, wait for
  propagation, and run doctor again.
- `manifest ... is incomplete` — the LWWRef arrived but one of the blocks that
  names or contains its manifest has not. Fetch the first hash named by the
  check or keep a complete repository peer online.
- `RefLog ... has no current value` — the manifest is readable, but the
  repository history head has not propagated to this daemon yet.
- `block(s) ... are missing on this peer` — the current RefLog head is known,
  but this peer cannot serve a complete clone from it. Keep a complete replica
  online or fetch from one.
- a failure under `repository-sigils` — the manifest's collaboration address
  is incomplete, absent, malformed, or names a different mailbox. The owner
  must publish and announce the corrected sigil.
- `the repository declares no ... hub ingress` — the repository owner must
  publish a hub mailbox and its sigil in the manifest.
- `current mailbox policy is not observable` — the local peer can discover the
  mailbox but cannot currently read its policy. This remains a warning because
  it is an observation gap, not proof that the policy is absent.
- a failure under `forge-sender` — initialise or select a signing identity and
  reply mailbox with `hbs2-hub identity init`, then run the doctor again.
- `local-peer handoff ... this is not delivery` — this node recorded that it
  handed a letter to its local peer. Use `hbs2-hub updates` for later
  acknowledgements; no signed network receipt exists for the intermediate
  delivery states yet.
- `canon-published` — the latest locally recorded letter is now represented by
  an admitted, honoured, or origin event in the clone's canon. This is stronger
  evidence than a local handoff.

## Current scope

`hbs2-peer doctor` implements D1-D3: local runtime, network reachability and
metadata, plus repository discovery, local snapshot completeness, and required
sigils. `hbs2-hub doctor` consumes that stable report and implements D4 over
forge policy, local sender readiness, and the strongest delivery evidence this
node can prove. A complete local snapshot is useful provider evidence, but it
is not a signed remote availability receipt; provider advertisements and
key-only remote lookup remain part of the repository-leasing work. Likewise,
relay-stored and mailbox-accepted cannot be reported as facts until the network
has signed delivery receipts. See the
[product-readiness roadmap](product-roadmap.md).
