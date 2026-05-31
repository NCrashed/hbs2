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

The first build resolves and compiles around 200 dependencies and
takes a while (typically 30-60 minutes on a laptop). Subsequent
builds are incremental.

### Install binaries

To put the executables on your PATH:
```
cabal install \
    --installdir ~/.local/bin \
    --overwrite-policy=always \
    exe:hbs2 exe:hbs2-peer exe:hbs2-cli exe:hbs2-sync \
    exe:hbs2-keyman \
    exe:git-hbs2 exe:git-remote-hbs2 exe:hbs2-git-subscribe
```

Make sure `~/.local/bin` is on your PATH.

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
    imports = [ hbs2.homeManagerModules.${system}.default ];
    services.hbs2.enable = true;
}
```
This installs the binaries and starts a user-level `hbs2-peer.service`.

## Verifying the install

Check that the main binaries are visible:
```
    hbs2-peer version
    hbs2 --help
    hbs2-cli --help
```
If any of these fail with "command not found", verify that the
install directory is on your PATH.

## Next steps

See [`QUICKSTART.md`](QUICKSTART.md) for a short walkthrough from a
fresh install to a git repository pushed into hbs2 and cloned back
out.
