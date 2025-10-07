# HBS2 Peer NixOS Deployment Guide

This guide explains how to deploy an HBS2 peer using the NixOS module.

## Quick Start

### 1. Generate Peer Key

First, generate a new peer key:

```bash
nix develop github:NCrashed/hbs2/dev-0.25.3 -c hbs2-cli hbs2:keyring:new > peer.key
```

View the public key:

```bash
nix develop github:NCrashed/hbs2/dev-0.25.3 -c hbs2-cli hbs2:keyring:show peer.key
```

Example output:
```
sign-key:  3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC
```

### 2. Deploy Key to Server

Securely copy the key to your server:

```bash
scp peer.key root@your-server:/var/secrets/hbs2-peer.key
ssh root@your-server 'chmod 600 /var/secrets/hbs2-peer.key'
```

### 3. Configure NixOS Module

Add to your NixOS or NixOps configuration (see the [hbs2-example](./hbs2-example.nix)):

```nix
{ config, pkgs, ... }:

{
  imports = [ ./service/hbs2.nix ];

  services.hbs2-peer = {
    enable = true;
    keyFile = "/var/secrets/hbs2-peer.key";
    httpPort = 5001;

    # Optional: add known peers for faster bootstrapping
    knownPeers = [
      "10.250.0.1:7354"
    ];
  };
}
```

### 4. Deploy

For NixOps:
```bash
nixops deploy
```

For NixOS:
```bash
nixos-rebuild switch
```

### 5. Verify Service

Check service status:
```bash
systemctl status hbs2-peer
```

View logs:
```bash
journalctl -u hbs2-peer -f
```

## Configuration Options

### Required Options

- `enable` - Enable the HBS2 peer service
- `keyFile` - Path to peer's private key file (must exist on the server)

### Network Options

- `listenAddress` - UDP listen address (default: `"0.0.0.0"`)
- `listenPort` - UDP listen port (default: `7354`)
- `listenTcpPort` - TCP listen port (default: `null`, optional)
- `rpcAddress` - RPC API address (default: `"127.0.0.1"`)
- `rpcPort` - RPC API port (default: `13331`)
- `httpPort` - HTTP API port (default: `5001`)

### Storage Options

- `storageDir` - Storage directory (default: `"/var/lib/hbs2-peer"`)

### Bootstrap Options

- `bootstrapDns` - DNS bootstrap domains (default: `["bootstrap.hbs2.app"]`)
- `knownPeers` - Known peer addresses (default: `[]`)

### Advanced Options

- `extraConfig` - Additional configuration lines
- `user` - Service user (default: `"hbs2"`)
- `group` - Service group (default: `"hbs2"`)
- `package` - HBS2 package to use (default: auto-built from GitHub)

## Example: Full Configuration

```nix
services.hbs2-peer = {
  enable = true;

  # Security
  keyFile = "/var/secrets/hbs2-peer.key";

  # Network
  listenAddress = "0.0.0.0";
  listenPort = 7354;
  listenTcpPort = 3003;
  httpPort = 5001;

  # Storage
  storageDir = "/var/lib/hbs2-peer";

  # Bootstrap
  bootstrapDns = [
    "bootstrap.hbs2.app"
    "bootstrap.example.com"
  ];

  knownPeers = [
    "10.250.0.1:7354"
    "192.168.1.100:7354"
  ];

  # Subscribe to repositories
  extraConfig = ''
    poll reflog 1 "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"
    poll lwwref 1 "3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC"
  '';
};
```

## Security Considerations

1. **Key File Protection**: The peer key file contains your peer's private key. Keep it secure:
   ```bash
   chmod 600 /var/secrets/hbs2-peer.key
   chown root:root /var/secrets/hbs2-peer.key
   ```

2. **Firewall**: The module automatically opens the configured UDP and TCP ports. Review your firewall rules.

3. **RPC Access**: By default, RPC API listen on localhost only. Do not expose it publicly without authentication.

4. **Storage Directory**: The storage directory is set to be readable/writable only by the `hbs2` user.
