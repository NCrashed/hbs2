# 0.25.4.0  2026-06-08

Feature and maintenance release. Adds an opt-in announce flag to the
CLI, de-vendors four bundled libraries onto Hackage, restores
annotated tag push, and ships new documentation.

## Features

  - **`block:put --announce`.** `hbs2-cli hbs2:peer:storage:block:put`
    now accepts an optional `--announce` flag that broadcasts a
    `BlockAnnounce` right after storing, so a "put on A, get on B" flow
    works without a separate `hbs2-peer announce`. Off by default so
    encrypted-refchan and group-key workflows that gate publication are
    not surprised; higher-level flows (git push, sync) announce
    internally and do not need it. Closes
    [#5](https://github.com/NCrashed/hbs2/issues/5).

## Fixes

  - **Annotated tag push to `hbs23://`.** `git push hbs2 <annotated-tag>`
    previously failed with a generic "failed to push some refs" because
    `r:push` handed the tag-object SHA straight to the commit-chain
    walker; the walker then tried to parse the tag body as a commit
    and threw `InvalidObjectFormat`. The remote helper now inspects
    the type of the pushed SHA via `git cat-file -t`. For a tag
    object it peels to the commit with `<sha>^{commit}` for the
    chain walk and passes the tag SHA as an extra object so the
    final segment includes the tag body itself. The export pipeline
    grows a third `[GitHash]` "extras" parameter that is serialised
    into the same source queue after the commit workers finish.
    `GitObjectType` gains a `Tag` constructor, the segment encoding
    learns an `'A'` short marker, and `gitPackTypeOf` maps `Tag` to
    `OBJ_TAG` so `git index-pack` accepts the resulting pack on the
    fetch side. Lightweight tag push is unchanged. Closes
    [#7](https://github.com/NCrashed/hbs2/issues/7).
  - **`r:list` emits HEAD as a symbolic ref** instead of a duplicate
    object line, so clients resolve the default branch correctly.
  - **NixOS module:** create and whitelist the `hbs2-mailbox` sibling
    directory, and grant the RPC group access to the peer socket.

## Dependencies

  - hbs2 no longer vendors `saltine`, `bytestring-mmap`, `db-pipe`, or
    `suckless-conf`. They are now consumed from Hackage
    (`saltine-0.2.2.0`, `bytestring-mmap-compat-0.2.3`,
    `db-pipe-0.1.0.1`, `suckless-conf-0.1.2.9`; the latter three are
    published and maintained under <https://github.com/NCrashed>).
    This is a step toward a plain `cabal install` story; there are no
    runtime behaviour changes.

## Docs

  - New walkthroughs: `docs/multi-machine.md` (replicating a
    repository to a second machine) and `docs/encrypted-repos.md`
    (group-key encrypted repositories, including key backup).
  - Design proposals published under `docs/drafts/`: PEP-05 (Tor
    transport), PEP-13 (post-quantum encryption), PEP-14 (encrypted
    keystore), PEP-15 (HD keys from a mnemonic), PEP-16 (barter
    storage).

## Compatibility

  - **Segment marker `'A'` (annotated tags) is new since 0.25.3.2.**
    Peers from 0.25.3.0..0.25.3.2 decoding a segment that contains an
    annotated tag will fall through the `FromStringMaybe (Short ...)`
    default branch and silently relabel the tag object as `Blob`.
    Concretely: on replication from a 0.25.4.0 peer to an older peer,
    annotated tags become unreachable on the older peer. The on-wire
    fix is to upgrade the receiving peer to 0.25.4.0. There is no
    mixed-cluster fallback.

# 0.25.3.2  2026-06-04

Patch release. Adds a Docker image as a third distribution path
alongside the static tarball.

## Release

  - **Docker image published on tag push.** The release workflow
    grows a second job (`docker-linux-x86_64`) that runs in parallel
    with the static tarball build. It produces a single OCI image
    bundling the full hbs2 binary surface (hbs2-peer, hbs2-cli,
    hbs2-keyman, hbs2-git3, git-remote-hbs23, git-hbs2, hbs2-sync,
    ncq3) on top of the musl-static binaries, and pushes it to
    `ghcr.io/${owner}/hbs2-peer:${TAG}` and `:latest`. Total size is
    ~40 MB compressed. This follows the postgres/redis convention of
    shipping the admin CLI alongside the daemon so that all common
    operator tasks work via `docker exec`. Image config: `HOME=/data`
    routes config (`~/.config/hbs2-peer`), keys
    (`~/.hbs2-keyman/keys/`), and storage (`~/.local/share/hbs2`)
    into a single `/data` volume; no Entrypoint, so `docker run image
    hbs2-cli ...` works as smoothly as the default `hbs2-peer run`.

## Build internals

  - New flake output `packages.x86_64-linux.docker` built via
    `dockerTools.buildImage`. A `stripPackageToBin` helper re-derives
    each shipped binary through a one-shot `cp -L` so the image's
    runtime closure carries only the actual binary content rather
    than the Haskell `lib/` outputs, which would otherwise drag in
    the GHC + GCC toolchain for every package (~3 GiB per package,
    432 MB compressed for the image without the trim).
  - `bf6-git-hbs2` is excluded from the image because it is a
    shebang script hardcoding a `/nix/store/...-suckless-conf`
    path, which would re-introduce the GHC toolchain into the
    closure. `git hbs2 ...` dispatching is provided instead by a
    symlink `/bin/git-hbs2 -> hbs2-git3`, mirroring the cabal-install
    fallback documented in INSTALL.md.

## Documentation

  - INSTALL.md: Docker becomes Option 2; gains a section with
    common `docker exec` operator commands (peer poke, poll add,
    metadata lookup, initial setup, etc.). Cabal, Nix flake, and
    Home Manager paths bumped by one.
  - CONTRIBUTING.md: release section describes the two parallel
    jobs and a local-build fallback for both the tarball and the
    image.

# 0.25.3.1  2026-06-04

Patch release. Resolves the two open code-level items on the 0.25.3.0
"Known issues" list (`hbs2-cli` stdin and `.#static` build), ports the
FAQ and cookbook material from voidlizard's original site into the
repository, and adds a release workflow that publishes a statically
linked Linux binary on every version tag.

## Fixes

  - **`hbs2-cli` reads piped stdin.** `recover` in
    `HBS2.CLI.Run.Internal` previously used a catch-and-retry pattern
    around the user's action. When the action read stdin via strict
    `Data.ByteString.getContents` (which drains and closes the handle
    on the first attempt) the retry tripped on a closed handle. The
    rewritten `recover` probes for the peer up front, populates the
    RPC env once, and runs the user's action exactly once. Closes
    [#4](https://github.com/NCrashed/hbs2/issues/4).

  - **`.#packages.x86_64-linux.static` builds end-to-end.** pkgsStatic
    ships GHC 9.4 with unix 2.7.3, which differs from the dynamic
    toolchain's GHC 9.6 / unix 2.8 in three `Posix.IO` APIs the
    storage layer uses: `openFd` argument list, `fdRead` return type,
    and `fdWrite` argument type. New module
    `HBS2.Storage.NCQ3.Internal.UnixCompat` carries CPP-shimmed
    `openFdCompat`, `fdReadBS`, and `fdWriteBS`; the storage call
    sites in `NCQ.hs`, `Fossil.hs`, and `Run.hs` route through it.
    A darwin-only fdWrite in `HBS2.Data.Log.Structured.NCQ` is
    inline-fixed similarly. Closes
    [#6](https://github.com/NCrashed/hbs2/issues/6).

## Release

  - **Static binary tarball published on tag push.** New workflow
    `.github/workflows/release.yml` builds
    `.#packages.x86_64-linux.static` with the GitHub Actions cache
    (`magic-nix-cache-action`), packages the result as
    `hbs2-${TAG}-x86_64-linux-musl.tar.gz` with a SHA256 sidecar,
    and uploads both to the corresponding GitHub Release. The
    workflow is also reachable via `workflow_dispatch` so the
    maintainer can re-run it for an existing tag. INSTALL.md
    promotes this to the primary install option for Linux x86_64
    users; CONTRIBUTING.md documents the release process and a
    local-build fallback.

## Documentation

  - **`docs/FAQ.md`** (new). What hbs2 is, crypto primitives,
    side-by-side comparisons with Syncthing, Radicle, and IPFS, the
    CBOR-not-JSON rationale, what the parens in `hbs2-cli`
    invocations mean. Material restructured from
    [`hbs2.krizanic.net`](https://hbs2.krizanic.net) (the restored
    mirror of voidlizard's original site).

  - **`docs/COOKBOOK.md`** (new). Working recipes for non-git tasks:
    file sharing between two peers, polling a remote `lwwref`,
    reaching content over the peer's HTTP gateway, deleting local
    blocks and trees, encrypting trees with a group key, storing
    small inline content via `block:put`. Every recipe was verified
    against the binaries shipped in this release.

  - **`hbs2-storage-ncq/README.md`** (new). On-disk layout (KPD
    records, N-way cuckoo index), runtime structure (sharded
    memtable, IO queues), defaults table, NCQv1 -> NCQ3 migration,
    reported performance figures (attributed to voidlizard's
    historical measurements, not re-measured for this release), and
    a code map.

  - **`ARCHITECTURE.md`**: short note on why `hbs2-git3` stores zstd
    segments rather than reusing git's native pack format.

  - **`README.md`**: links to the two new docs.

## Removed

  - "Known issues" listed under 0.25.3.0 for `hbs2-cli` stdin and
    `.#static` build are resolved by the fixes above.

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
