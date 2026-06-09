# Running an hbs2 peer over Tor

This covers two things:

1. **Outbound:** dialing other peers that are published as Tor onion
   services (`*.onion`), without revealing your IP and without any port
   forwarding.
2. **Inbound / hosting:** publishing your own peer as an onion service so
   others can reach you the same way.

A peer set up this way is **onion-only**: it talks to the hbs2 network
exclusively over Tor. That gives NAT traversal with no router config and
keeps the node's real address off the wire.

I2P is out of scope.

## How it works

- Outbound: hbs2-peer dials `tcp://<hostname>.onion:<port>` through a
  local Tor SOCKS5 proxy (`tcp.socks5`). The `.onion` name is handed to
  Tor unresolved; Tor builds the circuit. Any `tcp://` peer can be dialed
  this way, onion or clearnet.
- Inbound: the peer listens on a loopback TCP port, and a Tor
  HiddenService forwards an onion virtual port to it. Nothing is exposed
  on the network directly.

For an onion-only peer you also turn off two clearnet behaviors:

- `multicast off` stops LAN peer discovery. Otherwise the peer announces
  itself on the local network, which both leaks reachability and bypasses
  the onion transport.
- `bootstrap off` stops the DNS bootstrap loop (which always pings the
  built-in `bootstrap.hbs2.app` seed over UDP). An onion-only peer has no
  clearnet path, so this would only fail.

Peers find each other by explicit `known-peer` entries. Automatic
advertisement of your own onion address to peers is a later phase; for
now you share your `.onion` out of band and the other side adds it.

## NixOS (recommended)

The `services.hbs2-peer` module has an `enableTor` switch that wires Tor,
the HiddenService, the SOCKS proxy, loopback binding, and the
onion-only knobs for you.

```nix
services.hbs2-peer = {
  enable = true;
  keyFile = "/var/lib/hbs2-peer/peer.key";
  listenTcpPort = 10351;        # required: the onion service forwards here
  enableTor = true;
  # onionPort = 10351;          # virtual onion port, defaults to listenTcpPort
  knownPeers = [
    "tcp://someotherpeer.onion:10351"
  ];
};
```

This generates a config that binds everything to `127.0.0.1`, adds
`tcp.socks5 "127.0.0.1:9050"`, `multicast off`, `bootstrap off`, and
enables a local Tor daemon with a v3 HiddenService forwarding the onion
port to `127.0.0.1:10351`. No firewall ports are opened: the onion
service is the only way in.

After `nixos-rebuild switch`, read your onion address and share it:

```
sudo cat /var/lib/tor/onion/hbs2-peer/hostname
```

Give peers `tcp://<that-hostname>:10351` to add to their `knownPeers`.

## Manual setup (non-NixOS)

Run Tor with a SOCKS proxy (the default `127.0.0.1:9050`) and a
HiddenService. In `torrc`:

```
HiddenServiceDir /var/lib/tor/hbs2/
HiddenServicePort 10351 127.0.0.1:10351
```

In `~/.config/hbs2-peer/config`:

```
listen "127.0.0.1:7351"
listen-tcp "127.0.0.1:10351"
tcp.socks5 "127.0.0.1:9050"
multicast off
bootstrap off
key "/path/to/peer.key"
known-peer "tcp://someotherpeer.onion:10351"
```

Read your onion address from `/var/lib/tor/hbs2/hostname` and share it as
`tcp://<hostname>:10351`.

## Outbound-only

If you only want to *reach* onion peers (no hosting), you do not need a
HiddenService. A local Tor SOCKS proxy plus two config lines is enough:

```
tcp.socks5 "127.0.0.1:9050"
known-peer "tcp://someotherpeer.onion:10351"
```

See also the short recipe in
[`multi-machine.md`](multi-machine.md) ("Approach 3").

## Verifying

Confirm the link is up and going through Tor:

```
hbs2-peer peers
```

The onion peer should appear with its `tcp://<...>.onion` address and a
round-trip time of seconds (Tor circuit latency), not milliseconds.

## Notes and limits

- Bridge nodes (clearnet + onion at once) are possible but not covered by
  `enableTor`, which sets an onion-only posture. Configure such a node by
  hand.
- Traffic-correlation defenses (padding, timing obfuscation) are out of
  scope here.
- Onion addresses are never gossiped via PEX, so a clearnet peer cannot
  learn them from you. (An onion node reaching clearnet over Tor is fine;
  a clearnet node learning onion addresses is not.) Automatic
  onion-address advertisement and selective onion-to-onion PEX are a later
  phase; until then, wire peers with `known-peer`.
