# Installation

hbs2 is a Haskell project. There are three supported ways to install
it; they produce the same binaries. Pick whichever fits your setup.

## Requirements

- Linux on x86_64 or aarch64. macOS and Windows-WSL have been used
  in the past but are not currently tested.
- About 4 GB of disk space for the dependency build.
- A network connection for the first build (deps come from Hackage).

## Option 1: Cabal + ghcup

The path with the broadest reach. Does not require Nix.

### System libraries

On Debian or Ubuntu:
```
sudo apt-get install -y \
    pkg-config libsodium-dev libssl-dev \
    zlib1g-dev libicu-dev libmagic-dev
```
On Fedora or RHEL:
```
sudo dnf install -y \
    pkgconf-pkg-config libsodium-devel openssl-devel \
    zlib-devel libicu-devel file-devel
```
On Arch:
```
sudo pacman -S --needed \
    pkgconf libsodium openssl zlib icu file
```

### Haskell toolchain

Install [ghcup](https://www.haskell.org/ghcup/) if it is not already
on your system:
```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
Then install and select the pinned versions:
```
ghcup install ghc 9.6.6
ghcup install cabal 3.12.1.0
ghcup set ghc 9.6.6
ghcup set cabal 3.12.1.0
```

### Build

```
git clone https://github.com/NCrashed/hbs2.git
cd hbs2
cabal update
cabal build all
```

The first build resolves and compiles several hundred dependencies
and takes a while (typically 30-60 minutes on a laptop). Subsequent
builds are incremental.

### Install binaries

To put the executables on your PATH:
```
cabal install \
    --installdir ~/.local/bin \
    --overwrite-policy=always \
    exe:hbs2-peer exe:hbs2-cli exe:hbs2-sync \
    exe:hbs2-keyman \
    exe:hbs2-git3 exe:git-remote-hbs23 \
    exe:ncq3
```

Make sure `~/.local/bin` is on your PATH.

The old monolithic `hbs2` binary has been split into specialised
tools: `hbs2-cli` for general operations, `hbs2-git3` for git
integration, `hbs2-peer` for the P2P daemon, `hbs2-keyman` for key
management, and `hbs2-sync` for directory sync. See
[`docs/CLI_MIGRATION.md`](docs/CLI_MIGRATION.md) for the mapping
from old commands to new ones.

### Optional: `git hbs2` dispatcher

Git's external-command dispatch looks for `git-hbs2` on PATH when you
type `git hbs2 ...`. The cabal install does not produce that wrapper
directly. For the cabal path, drop a one-line shim onto PATH so the
documented `git hbs2 ...` invocations work:
```
printf '#!/bin/sh\nexec hbs2-git3 "$@"\n' > ~/.local/bin/git-hbs2
chmod +x ~/.local/bin/git-hbs2
```
The Nix flake builds and installs a `git-hbs2` wrapper for you, so
this step is only needed for the cabal install path.

## Option 2: Nix flake

For users who already have Nix with flakes enabled.

To install the binaries into your user profile:
```
git clone https://github.com/NCrashed/hbs2.git
cd hbs2
nix profile install .
```

Without a binary cache the first install builds the entire dependency
graph and can take a couple of hours. There is no public binary cache
for this fork yet.

To open a development shell with GHC, cabal, and all build inputs:
```
git clone https://github.com/NCrashed/hbs2.git
cd hbs2
nix develop
```

## Option 3: Home Manager module

For NixOS or Home Manager users who want `hbs2-peer` running as a
user systemd service.

Add the flake as an input to your Home Manager configuration:
```
{
    inputs.hbs2.url = "github:NCrashed/hbs2";
}
```
Then enable the module:
```
{
    imports = [ hbs2.homeManagerModules.default ];
    services.hbs2.enable = true;
}
```
This installs the binaries and starts a user-level `hbs2-peer.service`.

## Verifying the install

Check that the main binaries are visible:
```
    hbs2-peer --help
    hbs2-cli --help
    hbs2-keyman --help
    git-hbs2 --help
```
If any of these fail with "command not found", verify that the
install directory is on your PATH.

## Next steps

See [`QUICKSTART.md`](QUICKSTART.md) for a short walkthrough from a
fresh install to a git repository pushed into hbs2 and cloned back
out.
