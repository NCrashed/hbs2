# Setting Up HBS2 Mirror/Relay Node

This guide explains how to configure a remote HBS2 node to automatically mirror and distribute your repository.

## Overview

A **mirror node** (relay peer) continuously synchronizes and distributes your repository data without requiring a local git checkout. This is useful for:

- Creating distributed backups
- Improving availability and download speed
- Running public seed servers
- Geographic distribution

## Architecture

```
Your Local Node                Remote Mirror Node
    ↓                                ↓
LWWRef: 3XgC98...              Subscribes to:
    ↓                          - LWWRef: 3XgC98...
RefLog: BTThPdH...            - RefLog: BTThPdH...
    ↓                                ↓
Git Repository                 Auto-synced Data
(push updates)         →       (distributed to peers)
```

## Method 1: Using hbs2-peer Configuration (Recommended)

### Step 1: Get Your Repository Keys

On your local machine with the repository:

```bash
# Get repository remotes
git hbs2 remotes

# Output example:
# 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC phrase-demise
```

The first hash is your **LWWRef key** (repository identifier).

### Step 2: Get the RefLog Key

```bash
# Show repository manifest
git hbs2 repo:manifest phrase-demise

# Output example:
# (manifest
#   (hbs2-git 3)
#   (seed 12345)
#   (public)
#   (reflog "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP")
#   (gk "...")
# )
```

Note the **RefLog key** from the `reflog` field.

### Step 3: Configure Remote Mirror Node

Add to your remote node's `hbs2-peer` configuration:

```
; Subscribe to repository's LWWRef (main metadata)
poll lwwref 1 "3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC"

; Subscribe to repository's RefLog (git transactions)
poll reflog 1 "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"
```

#### For NixOS Module:

```nix
services.hbs2-peer = {
  enable = true;
  keyFile = "/var/secrets/hbs2-peer.key";
  httpPort = 5001;

  extraConfig = ''
    ; Mirror your repository
    poll lwwref 1 "3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC"
    poll reflog 1 "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"
  '';
};
```

### Step 4: Restart Remote Peer

```bash
# For systemd
sudo systemctl restart hbs2-peer

# Verify it's working
sudo journalctl -u hbs2-peer -f
```

The peer will automatically:
1. Subscribe to LWWRef updates
2. Subscribe to RefLog transactions
3. Download all repository data
4. Keep it synchronized with your pushes
5. Distribute it to other peers

---

## Method 2: Using git-hbs2 repo:relay-only (Alternative)

This method uses the `git-hbs2` tool to configure relay-only mode.

### On Remote Node:

```bash
# Install hbs2-git3
nix profile install github:NCrashed/hbs2/dev-0.25.3#hbs2-git3

# Subscribe to repository for relay-only (no git checkout)
git hbs2 repo:relay-only 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC
```

This command:
1. Subscribes peer to the repository's LWWRef
2. Waits for manifest to download
3. Automatically subscribes to the RefLog from manifest
4. Starts mirroring all data

---

## Verification

### Check Subscription Status

On the remote node:

```bash
# Check if peer is polling the references
journalctl -u hbs2-peer | grep -i "poll\|subscribe"

# Should see entries like:
# poll lwwref 3XgC98VY...
# poll reflog BTThPdH...
```

### Check Data Synchronization

```bash
# Install hbs2-git3 on remote node (if not already)
nix develop github:NCrashed/hbs2/dev-0.25.3

# Check repository manifest
git hbs2 repo:manifest 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC

# Check imported refs
git hbs2 repo:refs 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC

# Check object count
git hbs2 repo:index:count 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC
```

**For detailed verification instructions, see [VERIFY_MIRROR.md](VERIFY_MIRROR.md)**

### Test from Another Node

On a third machine:

```bash
# Add your mirror as a known peer
# (in hbs2-peer config)
known-peer "your-mirror-ip:7354"

# Clone from the network (should fetch from mirror)
git clone hbs23://3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC my-repo

# Check logs to verify data came from mirror
```

---

## Complete Example: Setting Up Public Mirror

### Scenario

- **Local repo**: `3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC`
- **RefLog**: `BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP`
- **Mirror server**: `mirror.example.com` (public IP: 1.2.3.4)

### Step 1: On Mirror Server

Create `/etc/nixos/hbs2-mirror.nix`:

```nix
{ config, pkgs, ... }:

{
  imports = [ ./path/to/hbs2.nix ];

  services.hbs2-peer = {
    enable = true;
    keyFile = "/var/secrets/hbs2-peer.key";

    # Listen on public interface
    listenAddress = "0.0.0.0";
    listenPort = 7354;
    listenTcpPort = 3003;

    # HTTP API for local access
    httpPort = 5001;

    # Storage for mirrored data
    storageDir = "/var/lib/hbs2-peer";

    # Bootstrap
    bootstrapDns = [ "bootstrap.hbs2.app" ];

    # Known peers (optional, for faster sync)
    knownPeers = [
      "your-local-node:7354"  # If reachable
    ];

    # Mirror configuration
    extraConfig = ''
      ; Mirror the repository
      poll lwwref 1 "3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC"
      poll reflog 1 "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"

      ; Add more repositories to mirror
      ; poll lwwref 1 "AnotherRepoKey..."
      ; poll reflog 1 "AnotherRefLogKey..."
    '';
  };

  # Open firewall for HBS2
  networking.firewall = {
    allowedUDPPorts = [ 7354 ];
    allowedTCPPorts = [ 3003 ];
  };
}
```

### Step 2: Deploy

```bash
# Deploy to mirror server
nixops deploy -d mirror

# Or for NixOS
sudo nixos-rebuild switch
```

### Step 3: Verify Mirror is Working

```bash
# On mirror server
sudo journalctl -u hbs2-peer -f

# Should see:
# - Bootstrap connections
# - Polling lwwref/reflog
# - Downloading blocks
# - Peer connections
```

### Step 4: Update DNS Bootstrap (Optional)

Add your mirror to DNS bootstrap:

```
bootstrap.example.com. IN TXT "udp://1.2.3.4:7354"
```

### Step 5: Announce Your Mirror

Update your project README:

```markdown
## HBS2 Repository

Repository ID: `3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC`

### Public Mirrors

- `mirror.example.com:7354` (US East)
- Add to your hbs2-peer config:
  ```
  known-peer "mirror.example.com:7354"
  ```
```

---

## Multiple Repositories on One Mirror

You can mirror multiple repositories on a single node:

```nix
services.hbs2-peer = {
  extraConfig = ''
    ; Repository 1
    poll lwwref 1 "3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC"
    poll reflog 1 "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"

    ; Repository 2
    poll lwwref 1 "7YHJu9PQWsVKBmWaDNLWQr2umxzzT5kq8829JhFk32Ls"
    poll reflog 1 "9MGnB6SqqsAGgCSr3k9RJgY4nTwiMRXrgZUmKPFndzn8"

    ; Repository 3 (encrypted)
    poll lwwref 1 "5BCaH95cWsVKBmWaDNLWQr2umxzzT5kqRRKNTm2J15Ls"
    poll reflog 1 "JAuk1UJzZfbDGKVazSQU5yYQ3NGfk4gVeZzBCduf5TgQ"
  '';
};
```

---

## Monitoring Mirror Health

### Check Storage Usage

```bash
# Check storage size
du -sh /var/lib/hbs2-peer

# Check number of blocks
find /var/lib/hbs2-peer -type f | wc -l
```

### Check Sync Status

```bash
# Check when last data was received
sudo journalctl -u hbs2-peer | grep -i "download\|block\|fetch" | tail -20

# Check peer connections
sudo journalctl -u hbs2-peer | grep -i "peer\|session" | tail -20
```

### Verify Repository is Current

```bash
# On mirror server, with hbs2-git3 installed
git hbs2 repo:head 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC

# Compare with local node
# (on your development machine)
git hbs2 repo:head phrase-demise

# Hashes should match
```

---

## Troubleshooting

### Mirror Not Syncing

**Problem:** Mirror doesn't receive updates

**Solutions:**

1. Check peer can reach your local node:
   ```bash
   # On mirror
   git hbs2 repo:wait 3XgC98VY46WhfBbkngM3vPLpFc5pai6FuEZ7PjTzkqzC
   ```

2. Add local node as known peer:
   ```
   known-peer "your-local-ip:7354"
   ```

3. Verify firewall allows UDP 7354

4. Check logs for errors:
   ```bash
   sudo journalctl -u hbs2-peer | grep -i error
   ```

### Slow Initial Sync

**Problem:** First sync takes very long

**Expected:** Initial sync of large repository (Linux kernel size) may take hours

**Speed up:**
- Add local node as known peer
- Increase network bandwidth
- Check no packet loss: `ping -c 100 mirror-ip`

### Mirror Using Too Much Storage

**Problem:** Storage growing indefinitely

**Solution:** HBS2 currently doesn't have automatic garbage collection. You need to:

1. Stop the peer
2. Clear storage
3. Let it re-sync (it will only download referenced blocks)

```bash
sudo systemctl stop hbs2-peer
sudo rm -rf /var/lib/hbs2-peer/*
sudo systemctl start hbs2-peer
```

---

## Security Considerations

### For Public Mirrors

1. **No Private Keys Needed**: Mirror nodes don't need repository signing keys
2. **Read-Only**: Cannot modify repository data
3. **Firewall**: Only UDP 7354 needs to be open
4. **Rate Limiting**: Consider iptables rate limiting for public mirrors

### For Private Repositories

Mirror encrypted repositories:

```nix
extraConfig = ''
  ; Encrypted repository
  poll lwwref 1 "EncryptedRepoKey..."
  poll reflog 1 "EncryptedRefLogKey..."
'';
```

**Note:** Mirror will store encrypted data but cannot decrypt it without group keys.

---

## Summary

**Quick Setup:**

1. Get your repo's LWWRef and RefLog keys
2. Add to remote node's config:
   ```
   poll lwwref 1 "YourLWWRefKey"
   poll reflog 1 "YourRefLogKey"
   ```
3. Restart hbs2-peer
4. Mirror automatically syncs and distributes

**Benefits:**
- ✅ Automatic synchronization
- ✅ No git checkout needed on mirror
- ✅ Increases data availability
- ✅ Improves network resilience
- ✅ Can mirror multiple repos on one node
