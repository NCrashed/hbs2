# NixOS desktop setup

This guide installs an hbs2 peer on a NixOS desktop, keeps it running
as a user service, and clones a repository over `hbs23://`. Run
`hbs2-peer doctor` before cloning so network failures remain distinct
from encrypted-repository access failures.

## Install

### Pinned imperative install (non-flake system configuration)

This does not require converting `/etc/nixos/configuration.nix` to a
flake. Pin the revision so every machine runs the same protocol build:

```sh
HBS2_REV=ad2337b3
nix profile install "github:NCrashed/hbs2/${HBS2_REV}#default"
hbs2-peer --version
hbs2-git3 --version
command -v git-remote-hbs23
```

The first build can take several minutes because the flake currently
declares no project binary cache.

### Home Manager (declarative user peer)

Add the pinned hbs2 input and module to your Home Manager flake:

```nix
inputs.hbs2.url = "github:NCrashed/hbs2/ad2337b3";

# In the Home Manager module list:
imports = [ inputs.hbs2.homeManagerModules.default ];
services.hbs2.enable = true;
```

The module installs the complete default package and enables the
`hbs2-peer` user service.

### NixOS system service

For a machine-wide daemon, import `inputs.hbs2.nixosModules.default`,
put the client package on `PATH`, and provide a pre-generated private
key with mode `0600`:

```nix
{ inputs, pkgs, ... }:
{
  imports = [ inputs.hbs2.nixosModules.default ];
  environment.systemPackages = [ inputs.hbs2.packages.${pkgs.system}.default ];
  services.hbs2-peer = {
    enable = true;
    keyFile = /var/lib/hbs2-peer/peer.key;
    knownPeers = [ "server.local:7351" ];
  };
}
```

The rest of this guide uses the per-user layout.

## Initialise keys and storage

```sh
hbs2-peer init
chmod 600 ~/.config/hbs2-peer/default.key
hbs2-keyman update
hbs2-cli hbs2:keyring:show ~/.config/hbs2-peer/default.key
```

`hbs2-peer init` creates both the config and a unique peer keyring. Do
not overwrite it and do not copy one peer identity between machines.
The important default locations are:

| Data | Default location |
|---|---|
| peer config and identity | `~/.config/hbs2-peer/` |
| peer block storage | `~/.local/share/hbs2/ncq3/` |
| keyman-owned keyrings | `~/.hbs2-keyman/keys/` |
| keyman index | `~/.local/share/hbs2-keyman/state.db` |

`hbs2-keyman update` automatically discovers the active peer keyring
from the peer config. Use `hbs2-keyman add-mask` only for additional
keyring locations.

## Run it as a user service

Home Manager users already have the unit. For the imperative install,
create `~/.config/systemd/user/hbs2-peer.service`:

```ini
[Unit]
Description=HBS2 peer daemon
After=network.target

[Service]
Type=simple
ExecStart=%h/.nix-profile/bin/hbs2-peer run
Restart=on-failure
RestartSec=5s

[Install]
WantedBy=default.target
```

Then enable it and keep the user manager alive after logout:

```sh
systemctl --user daemon-reload
systemctl --user enable --now hbs2-peer
sudo loginctl enable-linger "$USER"
systemctl --user status hbs2-peer
journalctl --user -u hbs2-peer -f
```

After changing the peer config, use
`systemctl --user restart hbs2-peer`. Without linger, the peer stops at
logout and a later fetch resembles a network failure.

## Connect and diagnose before cloning

The default `bootstrap.hbs2.app` currently resolves usable public
peers. To pin a home server, add this to
`~/.config/hbs2-peer/config`:

```text
known-peer "server.local:7351"
```

Use a stable DNS name or DHCP reservation. A raw DHCP address can
change. Restart the service and verify the entire path:

```sh
systemctl --user restart hbs2-peer
hbs2-peer doctor
```

Doctor should confirm config, RPC and version agreement, connected
peers, bootstrap resolution, and the configured peer. Fix these before
attempting a clone.

## Authorise an encrypted clone

The preferred flow gives this machine its own member identity. On a
current member machine that also holds the repository signing key:

```sh
hbs2-git3 repo:member:add <REPO_KEY> enc:<DESKTOP_ENCRYPTION_PUBKEY>
```

Get `<DESKTOP_ENCRYPTION_PUBKEY>` from the desktop's `pub-key:` line
printed by `hbs2:keyring:show`. Do not use its `sign-key:` value.

On the desktop, fetch and verify the active group key before cloning:

```sh
hbs2-git3 repo:gk <REPO_KEY>
GK_HASH=<HASH_FROM_THE_GK_FORM>
hbs2-peer fetch "$GK_HASH"
hbs2-cli "(hbs2:groupkey:find-secret \"$GK_HASH\")"
hbs2-peer doctor --repo <REPO_KEY>
```

The final keyman command must print a group-key id. `can't load
groupkey` means the public group-key object is not local; fetch it.
`groupkey secret not found` means this keyman has no member secret;
add the machine as a member or restore and index the intended member
keyring.

Older hbs2 revisions could not grant a new machine access to existing
history. Copying an existing member secret was the workaround, but it
shares one identity across machines. `repo:member:add` is the supported
path now and publishes compatibility envelopes for existing history.

## Clone and verify content

```sh
git clone hbs23://<REPO_KEY> ~/repo-checkout
cd ~/repo-checkout
git rev-parse HEAD
git rev-parse 'HEAD^{tree}'
git ls-files -z | sort -z | xargs -0 sha256sum | sha256sum
git fsck
git fetch origin
```

Compare the commit, tree, and tracked-file checksum with the source
checkout. A successful exit by itself is not enough evidence for an
encrypted clone. `git fsck` may report dangling blobs retained from
the hbs2 transaction stream; dangling objects are not corruption.

These identities have different roles:

| Identity | Purpose | Needed to clone? |
|---|---|---|
| peer signing identity | identifies one network daemon | the daemon needs its own |
| member encryption identity | opens an encrypted group key | yes, for encrypted data |
| repository signing key | authorises repository head updates | no; required to push |
| hub author identity | signs issues and hub events | no |

A reader needs a member secret; a writer additionally needs the
repository signing key. Do not substitute one public key for another
just because their base58 encodings look alike.

## Back up and uninstall

For encrypted repositories, the member secret is the access credential.
Keys are random: there is no seed phrase, reset, or escrow. If all
member secrets are lost, surviving git objects remain permanently
unreadable. Keep an encrypted offline backup and preferably a second
independent member. Back up the repository signing key separately if
you need to keep publishing.

To remove an imperative user installation:

```sh
systemctl --user disable --now hbs2-peer
nix profile list
nix profile remove <NAME_FROM_PROFILE_LIST>
```

These commands intentionally leave keys and peer storage in place so
an uninstall cannot destroy access or replicated data.
