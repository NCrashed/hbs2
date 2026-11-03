# Nix binary cache operations

hbs2 publishes signed default-package closures for every system exposed
by the flake:

- `x86_64-linux`
- `aarch64-linux`
- `x86_64-darwin`
- `aarch64-darwin`

The public cache contract is:

```text
URL: https://hbs2.cachix.org
key: hbs2.cachix.org-1:IGiwrFd/DNMG/+Oqjor6wwBh7T6EWC9uGR1Y1pD4VaI=
```

The flake declares these as `extra-substituters` and
`extra-trusted-public-keys`. Nix asks before accepting flake-provided
configuration unless the user has explicitly enabled
`accept-flake-config`. On multi-user Nix, these are restricted settings:
an ordinary user cannot grant trust, so NixOS must declare the same URL
and key through `nix.settings`. The copy-paste configuration is in
[`INSTALL.md`](../INSTALL.md).

## Publishing and retention

`.github/workflows/nix-cache.yml` builds the default closure on all four
systems and uploads it with a self-managed signing key. A second job uses
a fresh runner and `--max-jobs 0`, so installation fails rather than
silently compiling if any required path is absent from binary caches. It
also executes `hbs2-peer --version`, `hbs2-git3 --version`, and verifies
the `git-remote-hbs23` helper.

Every release tag is pinned in Cachix with `--keep-forever`. A maintainer
can retain another documented revision with:

```sh
gh workflow run nix-cache.yml -f revision=<GIT_REVISION>
```

Normal `master` builds follow the cache account's standard retention
policy; release and explicitly requested revisions do not.

CI needs two repository secrets:

- `CACHIX_AUTH_TOKEN`: a cache-scoped write token;
- `CACHIX_SIGNING_KEY`: the private self-signing key.

A Cachix personal token is only needed to initialise a signing key. It
must not be stored in GitHub Actions.

## Credential and signing-key rotation

Rotate `CACHIX_AUTH_TOKEN` independently in Cachix and replace the GitHub
secret. No client configuration changes when only that token changes.

Cachix currently cannot replace or add a key after a self-signing public
key has been registered. If the signing key is lost or exposed:

1. create a new cache name, for example `hbs2-v2`;
2. generate its self-signing pair and back up the private key offline;
3. replace the two GitHub secrets;
4. change the URL, public key, and `CACHE_NAME` together in the flake,
   workflow, installer documentation, and smoke test;
5. publish and verify every supported system;
6. keep the old cache read-only until all documented pinned revisions
   have a replacement or have reached their announced support end.

Never commit the signing key, cache token, personal token, or `.env`.

## Source-build fallback

The cache is an optimisation, not a build input. On a machine where the
hbs2 cache is not configured system-wide, ignore this flake's cache
configuration and build hbs2 locally while retaining the normal
`cache.nixos.org` dependency cache:

```sh
nix profile add --no-accept-flake-config \
  "github:NCrashed/hbs2/<GIT_REVISION>#default"
```

This can take considerably longer because the hbs2 Haskell packages are
built on the local machine.
