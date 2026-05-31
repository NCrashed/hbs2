# hbs2

A peer-to-peer content-addressable storage system with two primary
applications: **distributed git hosting** and **file synchronization**
across your devices.

## What it gives you

- **Encrypted distributed git** - host your own repositories across a
  small set of machines (your laptop, a VPS, a friend's server)
  without depending on GitHub, GitLab, or any other central service.
  Repositories can be end-to-end encrypted; only holders of the right
  keys can read them.

- **File synchronization** - keep a directory in sync between several
  of your devices, with optional encryption. Conceptually similar to
  Syncthing, built on the same content-addressable substrate as the
  git layer.

Both run on top of hbs2's core: a P2P CAS that handles peer discovery,
block distribution, signature verification, and group-key encryption.

## Status

Active. Single primary maintainer.

The wire protocol and storage format are stable and have been in
production use since 2023. The user-facing tooling - installation,
configuration, day-to-day ergonomics - is uneven and being actively
improved.

If you are looking for a polished product, this isn't it yet. If you
are looking for working infrastructure you can run yourself and
contribute to, read on.

## Why this exists

Centralized services for git hosting and file sync are convenient but
brittle. Accounts get suspended, services get blocked at the
jurisdiction level, companies fold, data gets lost. The standard
self-hosted alternatives solve part of the problem but each requires
either a permanent server with a public IP or careful manual setup
across devices.

hbs2 is built on a different premise: data is content-addressed,
distributed among peers, and identified by cryptographic keys rather
than DNS names. There is no central server; any peer can hold a copy.
This makes the data resilient to single-node failures, easy to share
selectively, and verifiable without trusting any particular host.

## Quickstart

A five-minute walkthrough - from zero to a working distributed git
repository - is in [`QUICKSTART.md`](QUICKSTART.md).

For installation options (Nix, Cabal, prebuilt binaries) see
[`INSTALL.md`](INSTALL.md).

## Documentation

- [`ARCHITECTURE.md`](ARCHITECTURE.md) - tour of the components and
  how they fit together.
- [`PROTOCOL.md`](PROTOCOL.md) - wire format specification.
- [`CONTRIBUTING.md`](CONTRIBUTING.md) - how to build, test, and
  submit changes.
- [`HISTORY.md`](HISTORY.md) - origin of the project.

Documentation is being written alongside the code rework; some of the
files above are still drafts or stubs. PRs against any of them are
welcome.

## History

hbs2 was created by Dmitry Zuikov (**voidlizard**) starting in 2023
and actively developed for two years until his death in 2025. This
repository continues his work with a narrowed scope. See
[`HISTORY.md`](HISTORY.md) for the full story.

## License

BSD 3-Clause. See [`LICENSE`](LICENSE).
