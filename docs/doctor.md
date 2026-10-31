# Diagnose a local peer

Run the doctor from the same user account and environment as the peer:

```text
hbs2-peer doctor
```

The command is read-only. It checks the local peer boundary in four steps:

- `config` — which configuration file was selected and whether it exists;
- `rpc` — which Unix socket was selected and whether the daemon answers;
- `version` — whether the CLI and the running daemon have the same version;
- `peers` — how many network peers the daemon currently sees.

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

## Current scope

The current doctor implements the local-runtime slice only. It does **not**
yet accept `--repo` and does not prove repository availability, relay
reachability, sigil propagation, or mailbox delivery. Those network,
repository, and forge checks are tracked as D2-D4 in the
[product-readiness roadmap](product-roadmap.md).
