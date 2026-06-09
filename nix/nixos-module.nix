self: { config, lib, pkgs, ... }:

with lib;

let
  inherit (pkgs.stdenv.hostPlatform) system;

  cfg = config.services.hbs2-peer;

  # PeerMain.hs derives the mailbox SQLite root as
  # `takeDirectory storageDir </> "hbs2-mailbox"` (a sibling of the
  # storage directory). The path is not exposed as a config knob,
  # so the module has to manage it explicitly: create it and let
  # the sandboxed unit write into it.
  mailboxDir = "${dirOf (toString cfg.storageDir)}/hbs2-mailbox";

  # With Tor enabled the peer binds only loopback (the onion HiddenService
  # is the only outward door) and runs onion-only: no LAN multicast
  # discovery, no clearnet DNS bootstrap.
  bindAddr = if cfg.enableTor then "127.0.0.1" else cfg.listenAddress;

  # Onion virtual port; defaults to the TCP listen port when unset.
  onionVPort =
    if cfg.onionPort != null then cfg.onionPort
    else if cfg.listenTcpPort != null then cfg.listenTcpPort
    else 0;

  configFile = pkgs.writeText "hbs2-peer.conf" ''
    listen "${bindAddr}:${toString cfg.listenPort}"
    ${optionalString (cfg.listenTcpPort != null) ''
    listen-tcp "${bindAddr}:${toString cfg.listenTcpPort}"
    ''}
    rpc "${cfg.rpcAddress}:${toString cfg.rpcPort}"
    http-port ${toString cfg.httpPort}
    key "${cfg.keyFile}"
    storage "${cfg.storageDir}"
    accept-block-announce *
    ${optionalString cfg.enableTor ''
    tcp.socks5 "127.0.0.1:${toString cfg.torSocksPort}"
    multicast off
    bootstrap off
    ''}
    ${optionalString (!cfg.enableTor)
        (concatMapStringsSep "\n" (dns: ''bootstrap-dns "${dns}"'') cfg.bootstrapDns)}
    ${concatMapStringsSep "\n" (peer: ''known-peer "${peer}"'') cfg.knownPeers}
    ${optionalString (cfg.extraConfig != "") cfg.extraConfig}
  '';

in {
  options.services.hbs2-peer = {
    enable = mkEnableOption "HBS2 peer daemon";

    package = mkOption {
      type = types.package;
      default = self.packages.${system}.hbs2-peer;
      defaultText = literalExpression "hbs2.packages.\${system}.hbs2-peer";
      description = "hbs2-peer package to use.";
    };

    keyFile = mkOption {
      type = types.path;
      description = ''
        Path to the hbs2-peer keyring file (contains both the private
        and public halves of the peer's long-term signing key).
        Generate with `hbs2-cli hbs2:keyring:new > peer.key` and copy
        to the server out of band. Keep readable only by `cfg.user`.
      '';
      example = "/var/lib/hbs2-peer/peer.key";
    };

    listenAddress = mkOption {
      type = types.str;
      default = "0.0.0.0";
      description = "Address hbs2-peer listens on (UDP and optional TCP).";
    };

    listenPort = mkOption {
      type = types.port;
      default = 7351;
      description = "UDP listen port for peer-to-peer traffic.";
    };

    listenTcpPort = mkOption {
      type = types.nullOr types.port;
      default = null;
      description = "Optional TCP listen port for peer-to-peer traffic.";
      example = 10351;
    };

    rpcAddress = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Address for the TCP RPC endpoint exposed by hbs2-peer.";
    };

    rpcPort = mkOption {
      type = types.port;
      default = 13331;
      description = "TCP RPC port.";
    };

    httpPort = mkOption {
      type = types.port;
      default = 5001;
      description = "HTTP API port for local applications.";
    };

    storageDir = mkOption {
      type = types.path;
      default = "/var/lib/hbs2-peer";
      description = "Directory for hbs2-peer storage data.";
    };

    bootstrapDns = mkOption {
      type = types.listOf types.str;
      default = [ "bootstrap.hbs2.app" ];
      description = ''
        DNS names whose TXT records list peer addresses, used for
        peer discovery. The hardcoded default in the daemon source is
        also `bootstrap.hbs2.app`; setting it here makes the intent
        explicit and lets you append additional bootstrap domains.
      '';
      example = [ "bootstrap.hbs2.app" "bootstrap.example.com" ];
    };

    knownPeers = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        Hard-coded peer addresses to dial directly, bypassing DNS
        bootstrap. Format: `addr:port` or `tcp://host:port`.
      '';
      example = [ "10.250.0.1:7354" "tcp://hbs2.example.com:3003" ];
    };

    extraConfig = mkOption {
      type = types.lines;
      default = "";
      description = "Extra lines appended to the hbs2-peer config file.";
      example = ''
        poll lwwref 1 "3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC"
        poll reflog 1 "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"
      '';
    };

    enableIpv6 = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to allow `AF_INET6` for the service. Leave off on
        hosts without IPv6 to avoid "Address family not supported"
        errors at runtime.
      '';
    };

    user = mkOption {
      type = types.str;
      default = "hbs2";
      description = "User account under which hbs2-peer runs.";
    };

    group = mkOption {
      type = types.str;
      default = "hbs2";
      description = ''
        Group under which hbs2-peer runs. Also the RPC access group:
        the daemon runs with `UMask=0007`, so the Unix RPC socket at
        `/tmp/hbs2-rpc.socket` is created mode `0770` with this group,
        and any user added to it can talk to the peer via `hbs2-cli`,
        `hbs2-keyman`, `git-remote-hbs23`, etc.

        To grant a user RPC access, in your system config:

            users.users.alice.extraGroups = [ "hbs2" ];

        This mirrors the standard `docker` group pattern.
      '';
    };

    openFirewall = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to open the configured ports in the system firewall.";
    };

    enableTor = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Run as a Tor onion peer. Enables a local Tor daemon (SOCKS proxy
        + a v3 HiddenService forwarding `onionPort` to the loopback TCP
        listener), points hbs2-peer at the SOCKS proxy, binds all
        sockets to 127.0.0.1, and switches the peer to onion-only mode
        (`multicast off`, `bootstrap off`). Requires `listenTcpPort`.

        The onion address is generated by Tor on first start; read it
        from `/var/lib/tor/onion/hbs2-peer/hostname` and share it as
        `tcp://<hostname>:${"\${toString onionPort}"}` so peers can add it
        via `knownPeers`. Outward firewall ports are not opened: the only
        reachable endpoint is the onion service.
      '';
    };

    torSocksPort = mkOption {
      type = types.port;
      default = 9050;
      description = ''
        Local Tor SOCKS5 port hbs2-peer dials through when enableTor is
        set. Must match Tor's actual SOCKS port; `services.tor.client.enable`
        (set by this module) listens on 9050 by default. If you change this,
        configure Tor's `services.tor.settings.SOCKSPort` to match.
      '';
    };

    onionPort = mkOption {
      type = types.nullOr types.port;
      default = null;
      description = ''
        Virtual port exposed by the onion HiddenService. Defaults to
        `listenTcpPort`. This is the port peers use in
        `tcp://<hostname>.onion:<onionPort>`.
      '';
    };
  };

  config = mkIf cfg.enable {
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.group;
      home = cfg.storageDir;
      createHome = true;
      description = "hbs2-peer daemon user";
    };

    users.groups.${cfg.group} = {};

    systemd.tmpfiles.rules = [
      "d '${cfg.storageDir}' 0750 ${cfg.user} ${cfg.group} - -"
      "d '${mailboxDir}' 0750 ${cfg.user} ${cfg.group} - -"
    ];

    environment.etc."hbs2-peer/config" = {
      source = configFile;
      mode = "0644";
    };

    systemd.services.hbs2-peer = {
      description = "hbs2-peer daemon";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      # Re-deploy with a changed config (poll lines, peers, ports, ...)
      # must restart the daemon. Without this, the deploy updates the
      # file on disk but the running process keeps its old in-memory
      # config until something else triggers a restart.
      restartTriggers = [ configFile ];

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.group;
        ExecStart = "${cfg.package}/bin/hbs2-peer run";
        Restart = "always";
        RestartSec = "10s";
        WorkingDirectory = cfg.storageDir;

        Environment = "XDG_CONFIG_HOME=/etc";

        # Socket is hardcoded at /tmp/hbs2-rpc.socket and the daemon
        # binds it without an explicit fchmod, so the perms come from
        # the process umask. 0007 yields `srwxrwx---`, which lets
        # members of `cfg.group` connect (write to the socket file is
        # required by Unix-domain `connect(2)`) while keeping everyone
        # else out. Trade-off: brains/mailbox SQLite files in storage
        # also become group-rw, which we accept because group members
        # are trusted operators by definition.
        UMask = "0007";

        NoNewPrivileges = true;
        PrivateTmp = false;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadWritePaths = [ cfg.storageDir mailboxDir "/tmp" ];

        RestrictAddressFamilies =
          [ "AF_INET" "AF_UNIX" ] ++ optional cfg.enableIpv6 "AF_INET6";

        AmbientCapabilities = [];
        CapabilityBoundingSet = [];
      };

      preStart = ''
        if [ ! -f "${cfg.keyFile}" ]; then
          echo "ERROR: hbs2-peer key file not found at ${cfg.keyFile}"
          exit 1
        fi
      '';
    };

    # Onion peers are reachable only via the HiddenService; the loopback
    # listeners must not be opened to the network.
    networking.firewall = mkIf (cfg.openFirewall && !cfg.enableTor) {
      allowedUDPPorts = [ cfg.listenPort ];
      allowedTCPPorts = optional (cfg.listenTcpPort != null) cfg.listenTcpPort;
    };

    assertions = optional cfg.enableTor {
      assertion = cfg.listenTcpPort != null;
      message = "services.hbs2-peer.enableTor requires listenTcpPort (the onion HiddenService forwards to it).";
    };

    services.tor = mkIf cfg.enableTor {
      enable = true;
      client.enable = true; # local SOCKS5 proxy on torSocksPort
      relay.onionServices."hbs2-peer" = {
        version = 3;
        map = [ { port = onionVPort; target = { addr = "127.0.0.1"; port = cfg.listenTcpPort; }; } ];
      };
    };
  };
}
