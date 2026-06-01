# 0.25.3.0  2026-06-01

First release under new maintenance, continuing the work Dmitry Zuikov
(voidlizard) had in flight on the `dev-0.25.3` branch. Anton Gushcha
took over as maintainer in 2026 after Dmitry's passing in 2025; the
canonical repository moved to github.com/NCrashed/hbs2, with the
original archived at github.com/voidlizard/hbs2 (now
github.com/NCrashed/hbs2-legacy). See HISTORY.md for the full story.

The previous tag in the series is `0.24.1.1`. There is no `0.24.1.2`
release tag; that line in this file refers to a single in-flight
change that never shipped before the maintenance transition and is
included here for historical continuity.

## Project

  - **Scope narrowed.** Maintenance focuses on two flagship use cases:
    distributed git hosting and file synchronization. Components moved
    to archive-only status (kept in the legacy repository, not built
    in this release): `hbs2-qblf`, `hbs2-git-dashboard`, `fixme-new`,
    `hbs2-fixer`.

  - **Wire protocol stability commitment.** Every ProtocolId currently
    assigned in `hbs2-peer/lib/HBS2/Peer/Proto.hs` is frozen as of
    this release. Future wire-level features get new ProtocolIds, not
    payload changes to existing ones. See PROTOCOL.md for the full
    registry. Encrypted-overlay framing and `GroupKey 'Symm` hand-
    rolled Serialise remain unchanged from prior 0.24.x peers.

## Packages

  - `hbs2-storage-ncq` (new in this release line) becomes the primary
    on-disk storage backend. Provides `HBS2.Storage.NCQ3` (current,
    log-structured) and `HBS2.Storage.NCQ` (legacy, retained for
    migration). Migration from NCQv1 storage available via the `ncq3`
    executable and `scripts/ncq-migrate.ss`.

  - `hbs2-log-structured` (new) carries the cuckoo-hash and
    structured-data primitives NCQ3 builds on.

  - `hbs2-storage-simple` remains shipped; still maintained for tests
    and small deployments. New installs should use NCQ3.

  - `hbs2-git3` (new) replaces the legacy `hbs2-git` package as the
    git remote helper. URL scheme is `hbs23://` (was `hbs2://`);
    binaries: `hbs2-git3`, `git-remote-hbs23`. The bf6 wrapper
    `git-hbs2` exposes `git hbs2 ...` subcommands.

  - `hbs2-cli` (new in this release line) becomes the primary
    command-line surface. Replaces most subcommands of the legacy
    monolithic `hbs2` binary. See `docs/CLI_MIGRATION.md` for the
    full rename table (e.g., `hbs2 keyring-new` → `hbs2-cli
    hbs2:keyring:new`). The legacy `hbs2` package is not shipped in
    this release; the old binary is available upstream as
    `hbs2-obsolete` in the dev-0.25.3 source but excluded from the
    v1 fork.

  - `hbs2-keyman-direct-lib` exposed as a sub-package alongside
    `hbs2-keyman` for in-process clients.

## Build

  - **Reproducible toolchain.** Pinned GHC 9.6.6, Cabal 3.12.1.0,
    Hackage `index-state` 2026-05-31. `cabal.project.freeze` pins
    277 exact transitive dependency versions taken from the nix
    development shell. `allow-newer: all` removed.

  - **Explicit packages list** in `cabal.project` (no globs). Each
    project package and each vendored library in `miscellaneous/`
    is listed by path.

  - **Build target.** `nix develop --command cabal build all
    --enable-tests` succeeds from a clean cabal store.

## CI

  - GitHub Actions matrix builds on `ubuntu-latest` and
    `macos-latest`. Both compile every package and every test-suite
    (`cabal build all --enable-tests`).

  - Test execution is intentionally not enabled in CI yet; the
    legacy test suite has not been fully audited for the current API.

## Tests

  - `test:test-tcp` (`test/TestTCP.hs`) and `test:test-proto-service`
    (`test/PrototypeGenericService.hs`) are marked `buildable: False`
    because they target older API shapes that no longer exist.
    Replacements: `TestTCPNet` exercises the live TCP path; modern
    suites cover the rest (`test-udp`, `test-ncq`, `test-storage-
    service`, `test-walk-merkle-conditional`, `test-misc`, more).

## Documentation

  - `README.md`, `INSTALL.md`, `QUICKSTART.md`, `ARCHITECTURE.md`,
    `PROTOCOL.md`, `CONTRIBUTING.md`, `HISTORY.md`, `LICENSE` all
    written or rewritten for this release. `PROTOCOL.md` was
    re-verified against `dev-0.25.3` sources; `MailBoxProto`
    (ProtocolId 13001) added to the registry.

  - `docs/` directory carries voidlizard's technical notes
    unchanged (`CLI_MIGRATION.md`, `MIRROR_SETUP.md`,
    `VERIFY_MIRROR.md`, `LWWREF_VS_REFLOG.md`, `devlog.md`, plus
    `drafts/`, `notes/`, `papers/`, `proto/`, `refchan/`,
    `todo/`).

  - QUICKSTART updated to match `git hbs2 init --new` behavior
    actually shipped: repository signing key is auto-generated and
    stored at `~/.hbs2-keyman/keys/<pubkey>-lwwref.key`, git remote
    is auto-wired with a two-word slug, URL scheme is `hbs23://`.

## Infrastructure

  - **NixOS module.** `nixosModules.default` flake output for
    deploying `hbs2-peer` as a system service. Options cover all
    config keys, opens firewall by default, runs as a system user
    with capability dropping and `ProtectSystem=strict`.

  - **Home Manager module.** `homeManagerModules.default` flake
    output for user-level `hbs2-peer.service`.

  - **Bootstrap node.** `bootstrap.hbs2.app` is the hardcoded
    default in `hbs2-peer/app/Bootstrap.hs:60`. The DNS is under
    NCrashed's control and operational; the node is hosted on the
    aerospace deployment with TXT records announcing peer
    addresses.

  - **Self-hosted source mirror.** The project repository is
    mirrored via hbs2 itself at
    `hbs23://9gtFy65ap1Hk9Mc71pMjc32zFsKcNZLVPWbBAbnkE4dP`,
    served by the bootstrap node. Read-only for contributors;
    submissions still arrive via GitHub PRs.

## Known issues

  - `hbs2-cli` does not read piped stdin correctly. Workaround:
    pass payloads as quoted string literals.
  - `block:put` does not auto-broadcast. After a `put`, an
    explicit `hbs2-peer announce <hash>` is required for other
    peers to learn the block exists.
  - The flake's `packages.${system}.static` derivation evaluates
    but does not build cleanly because of a `pkgsStatic`
    cabal2nix interaction inherited from upstream. The dynamic
    `default` output is the supported install path.

# 0.24.1.2  2024-04-27
  - Bump scotty version (legacy, not shipped as a tagged release)
