# Diagnose a local peer

Run the doctor from the same user account and environment as the peer:

```text
hbs2-peer doctor
```

The command does not write configuration or stored data. It checks the local
peer and its network boundary in six steps:

- `config` — which configuration file was selected and whether it exists;
- `rpc` — which Unix socket was selected and whether the daemon answers;
- `version` — whether the CLI and the running daemon have the same version;
- `peers` — how many network peers the daemon currently sees;
- `bootstrap` — whether each configured bootstrap DNS name returns at least
  one usable peer address;
- `configured-peers` — whether every `known-peer` entry parses, answers a
  signed handshake, and exposes the identity that signed it.

The bootstrap check makes bounded DNS TXT queries. It uses the same
`bootstrap-dns` entries and built-in `bootstrap.hbs2.app` name as the peer,
and counts only records that parse as supported peer addresses. With
`bootstrap off`, it makes no bootstrap query and reports the check as skipped.

The configured-peer check sends a bounded diagnostic handshake probe to each
`known-peer`. A successful result names both the configured address and the
verified signing key. This can refresh the daemon's in-memory known-peer state,
but does not rewrite its config or persistent storage. With no `known-peer`
entries, the check is skipped.

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

## Current scope

The current doctor implements the local-runtime slice plus bootstrap resolution,
configured-peer reachability, and verified peer identity from the network
slice. It does **not** yet accept `--repo` and does not inspect advertised
transports or peer metadata, or prove repository availability, sigil
propagation, or mailbox delivery. The remaining network, repository, and forge
checks are tracked as D2-D4 in the
[product-readiness roadmap](product-roadmap.md).
