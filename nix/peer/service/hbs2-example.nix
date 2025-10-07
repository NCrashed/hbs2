# Example NixOps configuration for HBS2 peer deployment
#
# Usage:
# 1. Import this module in your NixOps configuration
# 2. Create a peer key file (see instructions below)
# 3. Deploy with: nixops deploy or colmena

{ config, pkgs, ... }:

{
  imports = [ ./hbs2.nix ];

  services.hbs2-peer = {
    enable = true;

    # REQUIRED: Path to your peer's private key file
    # Generate a new key with:
    #   nix develop github:NCrashed/hbs2/dev-0.25.3 -c hbs2-cli hbs2:keyring:new > peer.key
    # Then deploy it securely to your server
    keyFile = "/var/secrets/hbs2-peer.key";

    # Network configuration
    listenAddress = "0.0.0.0";
    listenPort = 7354;          # Default UDP port
    listenTcpPort = 3003;       # Optional TCP port

    # Local API configuration
    rpcAddress = "127.0.0.1";
    rpcPort = 13331;
    httpPort = 5001;            # HTTP API for local applications

    # Storage location
    storageDir = "/var/lib/hbs2-peer";

    # Bootstrap configuration
    bootstrapDns = [
      "bootstrap.hbs2.app"
      # Add your own bootstrap domains here
    ];

    # Known peers (optional, but recommended for faster bootstrapping)
    knownPeers = [
      # "10.250.0.1:7354"
      # "your-peer-ip:7354"
    ];

    # Extra configuration (optional)
    extraConfig = ''
      # Mirror/relay repositories (automatically sync and distribute)
      # Get keys with: git hbs2 remotes && git hbs2 repo:manifest <remote-name>

      # Repository 1
      # poll lwwref 1 "3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC"
      # poll reflog 1 "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"

      # Repository 2
      # poll lwwref 1 "AnotherRepoLWWRefKey..."
      # poll reflog 1 "AnotherRepoRefLogKey..."
    '';
  };

  # Ensure the secrets directory exists
  system.activationScripts.hbs2-secrets = ''
    mkdir -p /var/secrets
    chmod 700 /var/secrets
  '';

  # Optional: Install hbs2 CLI tools system-wide
  environment.systemPackages = with pkgs; [
    # You can add hbs2 tools here if needed
    # config.services.hbs2-peer.package
  ];
}

# Instructions for setting up the peer key:
#
# 1. Generate a new peer key locally:
#    $ nix develop github:NCrashed/hbs2/dev-0.25.3 -c hbs2-cli hbs2:keyring:new > peer.key
#
# 2. View the public key:
#    $ nix develop github:NCrashed/hbs2/dev-0.25.3 -c hbs2-cli hbs2:keyring:show peer.key
#
# 3. Deploy the key securely to your server:
#    $ scp peer.key root@your-server:/var/secrets/hbs2-peer.key
#    $ ssh root@your-server 'chmod 600 /var/secrets/hbs2-peer.key'
#
# 4. Deploy your NixOps configuration:
#    $ nixops deploy
