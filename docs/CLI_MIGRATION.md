# CLI Migration Guide: v0.24 → v0.25.3

## Breaking Change: `hbs2` Command Renamed

In **v0.25.3**, the old `hbs2` CLI has been renamed to **`hbs2-obsolete`** and replaced with specialized tools.

## Command Migration Table

| Old Command (v0.24) | New Command (v0.25.3) |
|---------------------|----------------------|
| `hbs2 keyring-new` | `hbs2-cli hbs2:keyring:new` |
| `hbs2 keyring-list` | `hbs2-cli hbs2:keyring:show` |
| `hbs2 lwwref:create` | `hbs2-cli hbs2:lwwref:create` |
| *(any hbs2 command)* | `hbs2-obsolete <command>` (deprecated) |

## Installing Tools

### For All Tools

```bash
nix profile install github:NCrashed/hbs2/dev-0.25.3
```

This installs:
- `hbs2-cli` - CLI operations
- `hbs2-git3` / `git-hbs2` - Git integration
- `hbs2-peer` - P2P daemon
- `hbs2-keyman` - Key management
- `hbs2-sync` - File sync
- `hbs2-obsolete` - Legacy CLI (deprecated)

### For Specific Packages

```bash
# Just git tools
nix profile install github:NCrashed/hbs2/dev-0.25.3#hbs2-git3

# Just peer
nix profile install github:NCrashed/hbs2/dev-0.25.3#hbs2-peer

# Just CLI
nix profile install github:NCrashed/hbs2/dev-0.25.3#hbs2-cli
```

## Common Tasks

### Generate Peer Key

**Old:**
```bash
hbs2 keyring-new > peer.key
```

**New (recommended):**
```bash
hbs2-cli hbs2:keyring:new > peer.key
```

**Alternative (deprecated):**
```bash
hbs2-obsolete keyring-new > peer.key
```

### View Key Information

**Old:**
```bash
hbs2 keyring-list peer.key
```

**New:**
```bash
hbs2-cli hbs2:keyring:show peer.key
```

### Create LWWRef

**Old:**
```bash
hbs2 lwwref:create
```

**New:**
```bash
hbs2-cli hbs2:lwwref:create
```

### Git Operations

Git commands remain the same:

```bash
# These work in both versions
git hbs2 init --new
git hbs2 remotes
git hbs2 reflog:export remote-name --ref refs/heads/master
```

## Development Workflow

### Old Workflow (v0.24)

```bash
# Generate key
hbs2 keyring-new > peer.key

# Create repo
git hbs2 export --public --new <key>

# Add remote
git remote add myremote hbs2://<key>
git push myremote
```

### New Workflow (v0.25.3)

```bash
# Generate key (if needed for peer)
hbs2-cli hbs2:keyring:new > peer.key

# Create repo
git hbs2 init --new

# Export is automatic, just push
git push phrase-demise master
```

## Why the Change?

The monolithic `hbs2` command was split into specialized tools for better organization:

- **`hbs2-cli`** - General HBS2 operations (keyring, lwwref, storage)
- **`hbs2-git3`** - Git-specific operations (replaces old `git hbs2 export`)
- **`hbs2-peer`** - P2P network daemon
- **`hbs2-keyman`** - Advanced key management
- **`hbs2-sync`** - File synchronization

The old `hbs2` command is kept as `hbs2-obsolete` for backward compatibility but is deprecated.

## Scripts Update Examples

### Before (v0.24)

```bash
#!/bin/bash
# generate-keys.sh

for i in {1..5}; do
    hbs2 keyring-new > "key-$i.key"
    echo "Generated key-$i.key"
done
```

### After (v0.25.3)

```bash
#!/bin/bash
# generate-keys.sh

for i in {1..5}; do
    hbs2-cli hbs2:keyring:new > "key-$i.key"
    echo "Generated key-$i.key"
done
```

## NixOS Configuration Update

### Before (v0.24)

```nix
environment.systemPackages = [
  (pkgs.callPackage /path/to/hbs2 {}).hbs2
];
```

### After (v0.25.3)

```nix
environment.systemPackages = [
  (pkgs.callPackage /path/to/hbs2 {}).hbs2-cli
  (pkgs.callPackage /path/to/hbs2 {}).hbs2-git3
  (pkgs.callPackage /path/to/hbs2 {}).hbs2-peer
];
```

## Finding the Right Tool

| Task | Use This Tool |
|------|---------------|
| Generate keyring | `hbs2-cli hbs2:keyring:new` |
| Create LWWRef | `hbs2-cli hbs2:lwwref:create` |
| Git operations | `git-hbs2` or `hbs2-git3` |
| Run P2P node | `hbs2-peer run` |
| Manage keys | `hbs2-keyman` |
| Sync files | `hbs2-sync` |
| Legacy operations | `hbs2-obsolete` (deprecated) |

## Help Commands

```bash
# List all hbs2-cli commands
hbs2-cli --help

# Git help
git-hbs2 --help

# Peer help
hbs2-peer --help

# Key manager help
hbs2-keyman --help
```

## Troubleshooting

### "hbs2: command not found"

**Cause:** You're using v0.25.3 but trying old commands.

**Solution:** Use `hbs2-cli` or `hbs2-obsolete` instead:

```bash
# Instead of:
hbs2 keyring-new

# Use:
hbs2-cli hbs2:keyring:new

# Or (deprecated):
hbs2-obsolete keyring-new
```

### "Package 'hbs2' not found in flake"

**Cause:** The package name is correct, but it builds `hbs2-obsolete` binary.

**Solution:** The package `hbs2` exists but produces `hbs2-obsolete` binary. Install individual tools:

```bash
nix profile install github:NCrashed/hbs2/dev-0.25.3#hbs2-cli
nix profile install github:NCrashed/hbs2/dev-0.25.3#hbs2-git3
```

## Quick Reference Card

```
┌─────────────────────────────────────────────────────────┐
│ HBS2 v0.25.3 Quick Reference                           │
├─────────────────────────────────────────────────────────┤
│ KEYRING                                                 │
│   hbs2-cli hbs2:keyring:new > key.key                  │
│   hbs2-cli hbs2:keyring:show key.key                   │
│                                                         │
│ GIT                                                     │
│   git hbs2 init --new                                  │
│   git hbs2 remotes                                     │
│   git hbs2 reflog:export <remote> --ref <ref>         │
│   git push <remote> <branch>                           │
│                                                         │
│ PEER                                                    │
│   hbs2-peer run                                        │
│   hbs2-peer poke                                       │
│                                                         │
│ VERIFICATION                                            │
│   git hbs2 repo:manifest <key>                         │
│   git hbs2 repo:refs <key>                             │
│   git hbs2 repo:index:count <key>                      │
└─────────────────────────────────────────────────────────┘
```
