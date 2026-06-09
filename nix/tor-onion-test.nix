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

  peers = {
    alice = { tcp = 10361; udp = 17351; rpc = 13361; };
    bob   = { tcp = 10362; udp = 17352; rpc = 13362; };
  };

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
          map = [ { port = vport; target = { addr = "127.0.0.1"; port = peers.alice.tcp; }; } ];
        };
        hbs2-bob = {
          version = 3;
          map = [ { port = vport; target = { addr = "127.0.0.1"; port = peers.bob.tcp; }; } ];
        };
      };
    };

    systemd.services =
      let
        # static part of the config; the cross-wired known-peer line is
        # appended at runtime once Tor has minted the other peer's onion name
        mkStaticConfig = name: p: pkgs.writeText "hbs2-onion-${name}.conf" ''
          # Onion-only posture: loopback binds, no LAN multicast discovery,
          # no clearnet DNS bootstrap. With multicast off the two peers can
          # ONLY reach each other through their onion known-peer, so the
          # session below is unambiguously over Tor (no udp/loopback
          # shortcut). bootstrap off avoids the hard-coded bootstrap.hbs2.app
          # UDP ping (which from a loopback socket fails EINVAL and, via
          # peerThread -> GoAgainException, respawns the peer).
          listen "127.0.0.1:${toString p.udp}"
          # TCP stays on loopback: reachable only through the onion service.
          listen-tcp "127.0.0.1:${toString p.tcp}"
          multicast off
          bootstrap off
          # No HTTP API for the test (default port is 5005 for every peer,
          # so two instances collide on bind and respawn-loop).
          http-port "off"
          tcp.socks5 "127.0.0.1:9050"
          rpc "127.0.0.1:${toString p.rpc}"
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
              # each instance hardcodes /tmp/hbs2-rpc.socket; PrivateTmp keeps
              # the two from colliding (we query via the TCP rpc port). No
              # ProtectSystem / RestrictAddressFamilies here: this is a
              # throwaway test peer, and the AF restriction broke the HTTP and
              # UDP/DNS workers ("Address family not supported by protocol").
              PrivateTmp = true;
              NoNewPrivileges = true;
            };
          };
        };
      in
        mkMerge [
          (mkRunService "alice" peers.alice)
          (mkRunService "bob"   peers.bob)
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

                for name in alice bob; do
                  dir="/var/lib/hbs2-onion-$name"
                  mkdir -p "$dir/xdg/hbs2-peer" "$dir/storage"
                  # init creates $dir/default.key (and a config we ignore)
                  if [ ! -f "$dir/default.key" ]; then
                    hbs2-peer init "$dir"
                  fi
                done

                # wait until Tor has published both onion hostnames
                for f in ${onionHostname "alice"} ${onionHostname "bob"}; do
                  for i in $(seq 1 60); do
                    [ -s "$f" ] && break
                    sleep 2
                  done
                done

                ALICE_ONION="$(cat ${onionHostname "alice"})"
                BOB_ONION="$(cat ${onionHostname "bob"})"

                # alice dials bob's onion, and vice versa
                A_CFG=/var/lib/hbs2-onion-alice/xdg/hbs2-peer/config
                B_CFG=/var/lib/hbs2-onion-bob/xdg/hbs2-peer/config

                cp ${mkStaticConfig "alice" peers.alice} "$A_CFG"
                echo "known-peer \"tcp://$BOB_ONION:${toString vport}\"" >> "$A_CFG"

                cp ${mkStaticConfig "bob" peers.bob} "$B_CFG"
                echo "known-peer \"tcp://$ALICE_ONION:${toString vport}\"" >> "$B_CFG"

                chmod 0644 "$A_CFG" "$B_CFG"
                chown -R ${user}:${user} /var/lib/hbs2-onion-alice /var/lib/hbs2-onion-bob

                echo "alice onion: $ALICE_ONION:${toString vport}"
                echo "bob   onion: $BOB_ONION:${toString vport}"
              '';
            };
          }
        ];
  };
}
