# Installation

hbs2 is a Haskell project. There are six supported ways to install
it; they produce the same binaries. Pick whichever fits your setup.

## Requirements

- Linux on x86_64 or aarch64, or macOS on Apple Silicon (Homebrew
  or Nix). Windows-WSL has been used in the past but is not
  currently tested.
- About 4 GB of disk space for the dependency build (only required
  for the source-build options below).
- A network connection for the first build (deps come from Hackage).

## Option 1: Prebuilt static binary (Linux x86_64)

The fastest path. No toolchain, no dependencies, no build. Pulls a
statically linked (musl) tarball from the GitHub release page.

```
TAG=0.25.3.0  # set to the release you want
curl -fL -o hbs2.tar.gz "https://github.com/NCrashed/hbs2/releases/download/${TAG}/hbs2-${TAG}-x86_64-linux-musl.tar.gz"
curl -fL -o hbs2.tar.gz.sha256 "https://github.com/NCrashed/hbs2/releases/download/${TAG}/hbs2-${TAG}-x86_64-linux-musl.tar.gz.sha256"
sha256sum -c hbs2.tar.gz.sha256
tar xzf hbs2.tar.gz
sudo cp hbs2-${TAG}-x86_64-linux-musl/bin/* /usr/local/bin/
```

Skip ahead to "Verifying the install" below.

The binaries depend on nothing at runtime; they will work on any
modern Linux distribution. Note that releases prior to 0.25.3.1 may
not have a binary tarball attached; for those use the source paths
below.

## Option 2: Homebrew (macOS, Apple Silicon)

No toolchain, no build: installs a prebuilt bundle from the GitHub
release page via the [NCrashed/homebrew-hbs2](https://github.com/NCrashed/homebrew-hbs2)
tap.

```
brew install ncrashed/hbs2/hbs2-peer
```

This puts the full binary set on PATH: `hbs2-peer`, `hbs2-cli`,
`hbs2-sync`, `hbs2-keyman`, `hbs2-git3`, `git-remote-hbs23`,
`git-hbs2` (so `git hbs2 ...` works), and `ncq3`.

To run the peer as a background service via launchd:

```
hbs2-peer init   # one-time setup, before the first start
brew services start hbs2-peer
```

Logs go to `$(brew --prefix)/var/log/hbs2-peer.log`; configuration
lives in `~/.config/hbs2-peer/`.

Notes:

- Apple Silicon only. On Intel Macs use the Nix flake (Option 5) or
  Docker (Option 3).
- `hbs2-sync`'s `mount` subcommand requires
  [macFUSE](https://osxfuse.github.io/); everything else works
  without it.

Skip ahead to "Verifying the install" below.

## Option 3: Docker image (running hbs2-peer as a service)

Targeted at server deployments. The image (~40 MB compressed) bundles
the full hbs2 binary set on top of musl-static binaries, following the
same convention as official postgres/redis/mysql images that ship
their admin CLI alongside the daemon. You get `hbs2-peer` (the
daemon) plus `hbs2-cli`, `hbs2-keyman`, `hbs2-git3`, `git-remote-hbs23`,
`git-hbs2`, `hbs2-sync`, and `ncq3` ready for `docker exec`.

Pull and run:

```
docker pull ghcr.io/ncrashed/hbs2-peer:latest
docker run --name hbs2-peer \
  -v hbs2-data:/data \
  -p 7351:7351/udp \
  -p 10351:10351 \
  -p 5000:5000 \
  ghcr.io/ncrashed/hbs2-peer:latest
```

The image uses `/data` for config (`/data/.config/hbs2-peer`), keys
(`/data/.hbs2-keyman/keys/`), and storage (`/data/.local/share/hbs2`)
via `HOME=/data`. On first start `hbs2-peer` creates a default
config; edit it via `docker exec` or by mounting your own directory
in place of the named volume.

Day-to-day management is via `docker exec`. The common operations
follow the same patterns as a native install but with a `docker exec
hbs2-peer ` prefix:

```
docker exec hbs2-peer hbs2-peer poke
docker exec hbs2-peer hbs2-peer poll add <REF> lwwref 31
docker exec hbs2-peer hbs2-cli "(hbs2:tree:metadata:get \"<HASH>\")"
docker exec hbs2-peer hbs2-keyman list
```

For initial peer setup (one-time):

```
docker exec hbs2-peer hbs2-peer init
docker exec hbs2-peer sh -c 'hbs2-cli hbs2:keyring:new > /data/.config/hbs2-peer/default.key'
docker exec hbs2-peer hbs2-keyman add-mask "/data/.config/hbs2-peer/*.key"
docker exec hbs2-peer hbs2-keyman update
docker restart hbs2-peer
```

Image tags follow the source release tags (`0.25.3.1`, etc.). Use a
pinned tag in production; `latest` follows the most recent release.

## Option 4: Cabal + ghcup

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

## Option 5: Nix flake

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

## Option 6: Home Manager module

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
