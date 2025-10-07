{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.hbs2-peer;

  # Import hbs2 from GitHub using flake
  hbs2Flake = builtins.getFlake "github:NCrashed/hbs2/dev-0.25.3";

  # Get hbs2-peer package from the flake
  hbs2-peer = hbs2Flake.packages.${pkgs.system}.hbs2-peer;

  # Generate configuration file
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
      default = hbs2-peer;
      description = "HBS2 peer package to use";
    };

    keyFile = mkOption {
      type = types.path;
      description = ''
        Path to the HBS2 peer key file.
        This file should contain the peer's private key.
        Keep this file secure and private!
      '';
      example = "/var/secrets/hbs2-peer.key";
    };

    listenAddress = mkOption {
      type = types.str;
      default = "0.0.0.0";
      description = "UDP listen address for peer connections";
    };

    listenPort = mkOption {
      type = types.port;
      default = 7354;
      description = "UDP listen port for peer connections";
    };

    listenTcpPort = mkOption {
      type = types.nullOr types.port;
      default = null;
      description = "TCP listen port for peer connections (optional)";
      example = 3003;
    };

    rpcAddress = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "RPC listen address";
    };

    rpcPort = mkOption {
      type = types.port;
      default = 13331;
      description = "RPC listen port";
    };

    httpPort = mkOption {
      type = types.port;
      default = 5001;
      description = "HTTP API port for local applications";
    };

    storageDir = mkOption {
      type = types.path;
      default = "/var/lib/hbs2-peer";
      description = "Directory for HBS2 storage data";
    };

    bootstrapDns = mkOption {
      type = types.listOf types.str;
      default = [ "bootstrap.hbs2.app" ];
      description = "DNS bootstrap domains for peer discovery";
      example = [ "bootstrap.hbs2.app" "bootstrap.example.com" ];
    };

    knownPeers = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Known peer addresses in format 'ip:port'";
      example = [ "10.250.0.1:7354" "192.168.1.100:7354" ];
    };

    extraConfig = mkOption {
      type = types.lines;
      default = "";
      description = "Extra configuration to append to hbs2-peer.conf";
      example = ''
        poll reflog 1 "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"
      '';
    };

    enableIpv6 = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Enable IPv6 support. If your system doesn't support IPv6,
        keep this disabled to avoid "Address family not supported" errors.
      '';
    };

    user = mkOption {
      type = types.str;
      default = "hbs2";
      description = "User account under which hbs2-peer runs";
    };

    group = mkOption {
      type = types.str;
      default = "hbs2";
      description = "Group under which hbs2-peer runs";
    };
  };

  config = mkIf cfg.enable {
    # Create user and group
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.group;
      home = cfg.storageDir;
      createHome = true;
      description = "HBS2 peer daemon user";
    };

    users.groups.${cfg.group} = {};

    # Ensure storage and mailbox directories exist with correct permissions
    systemd.tmpfiles.rules = [
      "d '${cfg.storageDir}' 0750 ${cfg.user} ${cfg.group} - -"
      "d '/var/lib/hbs2-mailbox' 0750 ${cfg.user} ${cfg.group} - -"
    ];

    # Create config directory in /etc
    environment.etc."hbs2-peer/config" = {
      source = configFile;
      mode = "0644";
    };

    # Systemd service
    systemd.services.hbs2-peer = {
      description = "HBS2 Peer Daemon";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.group;
        ExecStart = "${cfg.package}/bin/hbs2-peer run";
        Restart = "always";
        RestartSec = "10s";
        WorkingDirectory = cfg.storageDir;

        # Set XDG config directory to read config from /etc
        Environment = "XDG_CONFIG_HOME=/etc";

        # Security hardening
        NoNewPrivileges = true;
        PrivateTmp = false;  # hbs2-peer uses /tmp for Unix sockets (hbs2-rpc.socket)
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadWritePaths = [
          cfg.storageDir
          "/tmp"  # For Unix RPC socket
          "/var/lib/hbs2-mailbox"  # For mailbox state.db
        ];

        # Network - AF_UNIX for RPC socket, AF_INET for IPv4, optionally AF_INET6
        RestrictAddressFamilies = [ "AF_INET" "AF_UNIX" ] ++ optional cfg.enableIpv6 "AF_INET6";

        # Capabilities
        AmbientCapabilities = [];
        CapabilityBoundingSet = [];
      };

      preStart = ''
        # Verify key file exists
        if [ ! -f "${cfg.keyFile}" ]; then
          echo "ERROR: Key file not found at ${cfg.keyFile}"
          exit 1
        fi

        # Verify storage directory is writable
        if [ ! -w "${cfg.storageDir}" ]; then
          echo "ERROR: Storage directory ${cfg.storageDir} is not writable"
          exit 1
        fi
      '';
    };

    # Open firewall ports
    networking.firewall = {
      allowedUDPPorts = [ cfg.listenPort ];
      allowedTCPPorts = optional (cfg.listenTcpPort != null) cfg.listenTcpPort;
    };
  };
}
