# Replicating a repository to a second machine

This walkthrough picks up where [`QUICKSTART.md`](../QUICKSTART.md)
leaves off. The quickstart runs a single peer and pushes a git
repository into it. Here you add a second machine running its own
peer, connect the two, and watch a push on machine A appear on
machine B without any manual copying.

Throughout, **machine A** is the one that already has a repository
pushed (quickstart step 5) and **machine B** is the new one.

## Before you start

On **both** machines, complete quickstart steps 1 to 4:

- `hbs2-peer init` to generate the config,
- register the keyring directory with `hbs2-keyman add-mask`,
- generate the peer identity and run `hbs2-keyman update`,
- start the peer with `hbs2-peer run`.

Each peer gets its own identity; do not copy `default.key` between
machines.

On **machine A**, make sure a repository is pushed and note its
public identifier (the base58 string after `hbs23://`, called
`<REPO_KEY>` below). You can list it any time from inside the working
tree:
```
git hbs2 remotes
```

Finally, the two peers must be able to reach each other over the
peer's UDP port, which is `7351` by default (confirm with
`hbs2-peer poke`). On a LAN that usually just works; across networks
open UDP `7351` on any firewall in between.

## How peers find each other

A fresh peer does not know about your other machine. You introduce
them in one of two ways. Pick one.

### Approach 1: known-peer (simplest for two machines)

Best when both machines have stable, reachable addresses (a LAN, a
VPN, or known public IPs).

On **machine B**, edit `~/.config/hbs2-peer/config` and add a line
pointing at machine A:
```
known-peer "<A-IP>:7351"
```
The address may also carry an explicit transport, for example
`udp://<A-IP>:7351` or `tcp://<A-IP>:10351`. Bare `host:port`
defaults to UDP.

Adding the reverse entry on machine A (`known-peer "<B-IP>:7351"`) is
optional but makes the link more robust: once either side learns the
other, peer exchange (PEX) shares the rest.

Restart the peers so they reread the config, then confirm they see
each other:
```
hbs2-peer peers
```
The other peer's key and address should be listed.

### Approach 2: bootstrap-dns with a TXT record

More flexible: you publish peer addresses in DNS, so you can list
several seed peers and change them later without editing every
machine's config.

Pick a domain you control and add a TXT record whose value is a peer
address:
```
seed.example.com.  IN  TXT  "udp://<A-IP>:7351"
```
You can add more than one TXT record on the same name to list several
peers.

Then on **machine B** (and any other peer), add to
`~/.config/hbs2-peer/config`:
```
bootstrap-dns "seed.example.com"
```
On startup the peer looks up the TXT records at that name and pings
the listed addresses. Restart the peer and check connectivity the
same way:
```
hbs2-peer peers
```

The compiled-in default is `bootstrap.hbs2.app`. The domain is
reserved but the bootstrap node behind it is not deployed yet, so do
not rely on it; use your own record or `known-peer` for now.

### Approach 3: reaching a peer over Tor (outbound)

When machine A is published as a Tor onion service, machine B can dial
it without either side revealing its IP, and without any port
forwarding on A. This covers the outbound direction (B reaching A's
`.onion`); hosting your own peer as an onion service is a separate
deployment step covered elsewhere.

On **machine B** you need a running Tor daemon exposing a local SOCKS5
port (the default is `127.0.0.1:9050`). Point hbs2-peer at it and add
the onion peer in `~/.config/hbs2-peer/config`:
```
tcp.socks5 "127.0.0.1:9050"
known-peer "tcp://<A-onion-address>.onion:<port>"
```
The `.onion` name is never resolved locally: it is handed to the Tor
proxy verbatim, and Tor resolves and routes it. The same SOCKS5 proxy
also covers any other `tcp://` peer you list, so you can mix onion and
clearnet TCP peers.

Restart the peer and confirm the link with `hbs2-peer peers`. If you
list a `.onion` peer but do not set `tcp.socks5`, the dial fails
immediately with a clear error instead of hanging on name resolution.

## Replicate the repository

With the peers connected, clone the repository on **machine B**:
```
git clone hbs23://<REPO_KEY> myrepo
cd myrepo
cat README.md
```
You should see the contents machine A pushed.

Cloning does more than fetch the current state: it tells machine B's
peer to subscribe to the repository's `lwwref` (the head pointer) and
`reflog` (the stream of git transactions). From now on machine B
keeps those references up to date on its own. Confirm the
subscriptions:
```
hbs2-peer poll list
```
You should see an `lwwref` entry and a `reflog` entry for the
repository.

## Verify automatic replication

On **machine A**, make a change and push it:
```
echo "second line" >> README.md
git commit -am "second commit"
git push <remote>
```
(use the remote name from `git hbs2 remotes`).

On **machine B**, the new transaction propagates on its own: directly
over the gossip protocol while both peers are connected, and through
the periodic poll as a catch-up if a peer was offline when the push
happened. You can watch the head move without re-cloning:
```
git hbs2 repo:refs <REPO_KEY>
```
The branch hash updates to match machine A's new commit. To bring
your working copy in line, use a normal pull:
```
git pull
cat README.md
```
You should now see `second line` on machine B.

If nothing arrives, work through:

- `hbs2-peer peers` on both sides: are they actually connected?
- `hbs2-peer poll list` on B: are the `lwwref` and `reflog`
  subscriptions present?
- firewall: is UDP `7351` open end to end?

Gossip is best effort, so the slowest the catch-up should ever be is
the reflog poll interval (about 11 minutes by default).

## A note on pushing from both machines

Cloning is read-only: machine B does not need the repository signing
key to receive machine A's pushes. To push *from* machine B you must
also give it the repository signing key, which lives on machine A at
`~/.hbs2-keyman/keys/<REPO_KEY>-lwwref.key`. Copy that file into
machine B's keyring directory (`~/.hbs2-keyman/keys/`) and run
`hbs2-keyman update`. Coordinating concurrent writes from two
machines is otherwise an ordinary git concern and is beyond this
walkthrough.

## Where to go next

- **Encrypted repositories.** Restrict read access to holders of
  specific keys. See [`encrypted-repos.md`](encrypted-repos.md).

- **Dedicated mirror or relay node.** Run a peer that replicates and
  redistributes a repository with no git checkout of its own. See
  [`MIRROR_SETUP.md`](MIRROR_SETUP.md) and
  [`VERIFY_MIRROR.md`](VERIFY_MIRROR.md).

- **Heads and history.** For how `lwwref` and `reflog` differ and why
  there are two of them, see
  [`LWWREF_VS_REFLOG.md`](LWWREF_VS_REFLOG.md).
