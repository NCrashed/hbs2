# Self-contained NixOS module for an end-to-end Tor onion outbound test of
# the PEP-05 Phase 1 work (branch pep-05-tor-outbound).
#
# It stands up two throwaway hbs2-peer instances (alice, bob) on the same
# host, each published as its own v3 onion service, and wires each one's
# `known-peer` to the OTHER's `.onion` address. Both have
# `tcp.socks5 "127.0.0.1:9050"`, so the only way they reach each other over
# TCP is by dialing an onion name through Tor - which is exactly the code
# path under test (PeerAddr name variant -> connectTCP -> socksConnect with a
# domain name -> Tor resolves the .onion).
#
# This module is deliberately separate from nix/nixos-module.nix (the
# production peer) and does not touch any existing hbs2-peer instance.
#
# Usage in /etc/nixos/configuration.nix:
#
#     imports = [ /path/to/hbs2/nix/tor-onion-test.nix ];
#     services.hbs2OnionTest = {
#       enable  = true;
#       # MUST be hbs2-peer built from the pep-05-tor-outbound branch,
#       # otherwise the test exercises the old binary. E.g. from your flake:
#       #   package = inputs.hbs2.packages.${pkgs.system}.hbs2-peer;
#       package = <hbs2-peer-from-the-branch>;
#     };
#
# After `nixos-rebuild switch`:
#
#   # the two generated onion addresses are printed by the setup unit:
#   journalctl -u hbs2-onion-setup | grep onion:
#
#   # watch alice dial bob's .onion through Tor (the proof):
#   journalctl -fu hbs2-onion-alice | grep -i 'OPEN CLIENT CONNECTION'
#
#   # each peer's known peers; the other shows up with a tcp://<...>.onion addr:
#   hbs2-peer peers -r 127.0.0.1:13361   # alice
#   hbs2-peer peers -r 127.0.0.1:13362   # bob
#
# Note: both peers also run local UDP multicast discovery (it has no config
# gate yet), so they may ALSO form a direct udp://127.0.0.1 session. That is
# harmless and does not invalidate the test: the onion TCP session is a
# separate, additional connection. The unambiguous signal is the
# "OPEN CLIENT CONNECTION tcp://<...>.onion" log line and the peer appearing
# with its tcp onion address.

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.hbs2OnionTest;

  user = "hbs2onion";

  # onion virtual port both hidden services expose
  vport = 9999;

  # Distinct loopback addresses so that the single-host setup cannot take a
  # direct-TCP shortcut. An inbound onion connection arrives from the Tor
  # exit as 127.0.0.1, and peer-meta reconstruction would dial
  # 127.0.0.1:<listen-tcp>; putting each peer on its own 127.0.0.x means
  # that reconstructed address points nowhere, so the only working path
  # between the two is the onion service.
  peers = {
    alice = { host = "127.0.0.2"; tcp = 10361; udp = 17351; };
    bob   = { host = "127.0.0.3"; tcp = 10362; udp = 17352; };
    carol = { host = "127.0.0.4"; tcp = 10363; udp = 17353; };
  };

  # Topology for the onion-PEX test (PEP-05 C): alice knows only bob, carol
  # knows only bob, bob knows both. alice must therefore discover carol's
  # .onion via PEX from bob - which only happens because all three declare
  # network-class "onion" (a clearnet peer would never be handed the onion).

  stateDir = name: "/var/lib/hbs2-onion-${name}";
  xdgDir   = name: "${stateDir name}/xdg";
  onionHostname = name: "/var/lib/tor/onion/hbs2-${name}/hostname";

in {
  options.services.hbs2OnionTest = {
    enable = mkEnableOption "hbs2 Tor onion outbound end-to-end self-test (two local peers over Tor)";

    package = mkOption {
      type = types.package;
      description = ''
        hbs2-peer package to test. MUST be built from the
        pep-05-tor-outbound branch (the one that wires tcp.socks5 and
        adds .onion peer addresses); otherwise this tests the old binary.
      '';
    };
  };

  config = mkIf cfg.enable {

    users.users.${user} = {
      isSystemUser = true;
      group = user;
      home = "/var/lib/hbs2-onion";
      description = "hbs2 onion test peers";
    };
    users.groups.${user} = {};

    # Tor: a local SOCKS5 proxy (9050) for outbound + one onion service per peer.
    services.tor = {
      enable = true;
      client.enable = true; # SOCKSPort 127.0.0.1:9050
      relay.onionServices = {
        hbs2-alice = {
          version = 3;
          map = [ { port = vport; target = { addr = peers.alice.host; port = peers.alice.tcp; }; } ];
        };
        hbs2-bob = {
          version = 3;
          map = [ { port = vport; target = { addr = peers.bob.host; port = peers.bob.tcp; }; } ];
        };
        hbs2-carol = {
          version = 3;
          map = [ { port = vport; target = { addr = peers.carol.host; port = peers.carol.tcp; }; } ];
        };
      };
    };

    systemd.services =
      let
        # static part of the config; the cross-wired known-peer line is
        # appended at runtime once Tor has minted the other peer's onion name
        mkStaticConfig = name: p: pkgs.writeText "hbs2-onion-${name}.conf" ''
          # Onion-only posture: TCP-only (no UDP socket at all), no LAN
          # multicast discovery, no clearnet DNS bootstrap. The two peers can
          # ONLY reach each other through their onion known-peer, so the
          # session below is unambiguously over Tor. `listen "off"` also
          # avoids EINVAL when pinging any UDP peer left in brains.db (a send
          # from a non-routable socket otherwise respawns the peer via
          # peerThread -> GoAgainException).
          listen "off"
          # TCP on a per-peer loopback addr (see `peers` note): reachable
          # only through the onion service.
          listen-tcp "${p.host}:${toString p.tcp}"
          multicast off
          bootstrap off
          network-class "onion"
          # No HTTP API for the test (default port is 5005 for every peer,
          # so two instances collide on bind and respawn-loop).
          http-port "off"
          tcp.socks5 "127.0.0.1:9050"
          # Per-peer RPC unix socket inside the instance's own state dir, so
          # the three peers never collide on the default /tmp/hbs2-rpc.socket
          # and each is reachable via `hbs2-peer <cmd> -r ${stateDir name}/rpc.socket`.
          rpc unix "${stateDir name}/rpc.socket"
          key "${stateDir name}/default.key"
          brains "${stateDir name}/brains.db"
          storage "${stateDir name}/storage"
          accept-block-announce *
        '';

        mkRunService = name: p: {
          "hbs2-onion-${name}" = {
            description = "hbs2 onion test peer ${name}";
            after = [ "hbs2-onion-setup.service" "tor.service" "network.target" ];
            requires = [ "hbs2-onion-setup.service" ];
            wantedBy = [ "multi-user.target" ];
            serviceConfig = {
              Type = "simple";
              User = user;
              Group = user;
              # Pin HOME and all XDG roots inside the per-instance state dir
              # (owned by the peer user). hbs2-peer creates dirs under HOME /
              # XDG_DATA_HOME at startup; without this it tries to mkdir the
              # user's home (/var/lib/hbs2-onion, not writable) and fails.
              # config is discovered via XDG_CONFIG_HOME/hbs2-peer/config.
              Environment = [
                "HOME=${stateDir name}"
                "XDG_CONFIG_HOME=${xdgDir name}"
                "XDG_DATA_HOME=${stateDir name}/data"
                "XDG_CACHE_HOME=${stateDir name}/cache"
              ];
              ExecStart = "${cfg.package}/bin/hbs2-peer run";
              Restart = "always";
              RestartSec = "10s";
              WorkingDirectory = stateDir name;
              # Each peer has its own `rpc unix` socket in its state dir (see
              # config above), so there is no /tmp/hbs2-rpc.socket collision and
              # PrivateTmp is unnecessary. Dropping it also makes each peer's RPC
              # reachable from the host for `hbs2-peer ... -r <socket>`. No
              # ProtectSystem / RestrictAddressFamilies here: this is a
              # throwaway test peer, and the AF restriction broke the HTTP and
              # UDP/DNS workers ("Address family not supported by protocol").
              NoNewPrivileges = true;
            };
          };
        };
      in
        mkMerge [
          (mkRunService "alice" peers.alice)
          (mkRunService "bob"   peers.bob)
          (mkRunService "carol" peers.carol)
          {
            # One-shot: create identities, wait for Tor to publish both onion
            # hostnames, then write each peer's config cross-wired to the
            # other's onion address.
            hbs2-onion-setup = {
              description = "prepare hbs2 onion test peers (keys, configs, cross-wire onion addresses)";
              after = [ "tor.service" "network-online.target" ];
              wants = [ "tor.service" "network-online.target" ];
              wantedBy = [ "multi-user.target" ];
              path = [ cfg.package pkgs.coreutils ];
              serviceConfig = {
                Type = "oneshot";
                RemainAfterExit = true;
              };
              script = ''
                set -eu

                for name in alice bob carol; do
                  dir="/var/lib/hbs2-onion-$name"
                  mkdir -p "$dir/xdg/hbs2-peer" "$dir/storage"
                  # init creates $dir/default.key (and a config we ignore)
                  if [ ! -f "$dir/default.key" ]; then
                    hbs2-peer init "$dir"
                  fi
                  # start from a clean brains DB so no UDP peers learned in
                  # earlier (multicast-on) runs linger and muddy the test
                  rm -f "$dir/brains.db"
                done

                # wait until Tor has published all three onion hostnames
                for f in ${onionHostname "alice"} ${onionHostname "bob"} ${onionHostname "carol"}; do
                  for i in $(seq 1 60); do
                    [ -s "$f" ] && break
                    sleep 2
                  done
                done

                ALICE_ONION="$(cat ${onionHostname "alice"})"
                BOB_ONION="$(cat ${onionHostname "bob"})"
                CAROL_ONION="$(cat ${onionHostname "carol"})"

                A_CFG=/var/lib/hbs2-onion-alice/xdg/hbs2-peer/config
                B_CFG=/var/lib/hbs2-onion-bob/xdg/hbs2-peer/config
                C_CFG=/var/lib/hbs2-onion-carol/xdg/hbs2-peer/config

                # Topology: alice<->bob and bob<->carol bidirectional, but NOT
                # alice<->carol. Each pair both dials and receives the other, so
                # an inbound onion connection (seen as the Tor exit 127.0.0.1)
                # coexists with the dialed .onion entry for the same identity.
                # The peer-dedup fix keeps the routable .onion from being evicted
                # by that loopback address (peerDialable in HBS2.Net.Proto.Types);
                # before the fix a directional topology was needed to dodge the
                # eviction. alice does not know carol and must learn it via PEX
                # from bob, which proves onion -> onion PEX.
                # Each peer also advertises its own .onion via peer-public-address
                # (PEP-05 G), disclosed only to onion-capable peers so it never
                # leaks to clearnet.
                cp ${mkStaticConfig "alice" peers.alice} "$A_CFG"
                echo "known-peer \"tcp://$BOB_ONION:${toString vport}\"" >> "$A_CFG"
                echo "peer-public-address \"tcp://$ALICE_ONION:${toString vport}\"" >> "$A_CFG"

                cp ${mkStaticConfig "bob" peers.bob} "$B_CFG"
                echo "known-peer \"tcp://$ALICE_ONION:${toString vport}\"" >> "$B_CFG"
                echo "known-peer \"tcp://$CAROL_ONION:${toString vport}\"" >> "$B_CFG"
                echo "peer-public-address \"tcp://$BOB_ONION:${toString vport}\"" >> "$B_CFG"

                cp ${mkStaticConfig "carol" peers.carol} "$C_CFG"
                echo "known-peer \"tcp://$BOB_ONION:${toString vport}\"" >> "$C_CFG"
                echo "peer-public-address \"tcp://$CAROL_ONION:${toString vport}\"" >> "$C_CFG"

                chmod 0644 "$A_CFG" "$B_CFG" "$C_CFG"
                chown -R ${user}:${user} \
                  /var/lib/hbs2-onion-alice /var/lib/hbs2-onion-bob /var/lib/hbs2-onion-carol

                echo "alice onion: $ALICE_ONION:${toString vport}"
                echo "bob   onion: $BOB_ONION:${toString vport}"
                echo "carol onion: $CAROL_ONION:${toString vport}"
              '';
            };
          }
        ];
  };
}
