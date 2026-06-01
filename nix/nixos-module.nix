self: { config, lib, pkgs, ... }:

with lib;

let
  inherit (pkgs.stdenv.hostPlatform) system;

  cfg = config.services.hbs2-peer;

  configFile = pkgs.writeText "hbs2-peer.conf" ''
    listen "${cfg.listenAddress}:${toString cfg.listenPort}"
    ${optionalString (cfg.listenTcpPort != null) ''
    listen-tcp "${cfg.listenAddress}:${toString cfg.listenTcpPort}"
    ''}
    rpc "${cfg.rpcAddress}:${toString cfg.rpcPort}"
    http-port ${toString cfg.httpPort}
    key "${cfg.keyFile}"
    storage "${cfg.storageDir}"
    accept-block-announce *
    ${concatMapStringsSep "\n" (dns: ''bootstrap-dns "${dns}"'') cfg.bootstrapDns}
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
      description = "Group under which hbs2-peer runs.";
    };

    openFirewall = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to open the configured ports in the system firewall.";
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

        NoNewPrivileges = true;
        PrivateTmp = false;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadWritePaths = [ cfg.storageDir "/tmp" ];

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

    networking.firewall = mkIf cfg.openFirewall {
      allowedUDPPorts = [ cfg.listenPort ];
      allowedTCPPorts = optional (cfg.listenTcpPort != null) cfg.listenTcpPort;
    };
  };
}
