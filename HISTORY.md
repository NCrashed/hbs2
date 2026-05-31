# History

hbs2 was created by Dmitry Zuikov, known online as **voidlizard**.
He started the project in 2023 to solve a problem he kept hitting in
practice: there was no good way to host a private git repository
across a small set of devices without depending on a centralized
service. The standard distributed alternatives either required a
permanent server with a public IP, or fell apart under modest real-world
constraints: NAT, sporadic connectivity, file conflicts.

What began as "distributed git that actually works" grew into a
general-purpose peer-to-peer content-addressable storage system. On
top of the core, applications were built for file synchronization,
encrypted group communication, a self-hosted issue tracker, and a few
more experimental ideas. The project was actively developed for
roughly two years, used in production by a small team, and self-hosted
on its own protocol at `hbs2://BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP`.

Dmitry passed away in 2025. After his death the codebase stalled,
as his collaborators were not in a position to take over active
maintenance.

## This repository

This repository (github.com/NCrashed/hbs2) is a continuation of his
work, taken over by Anton Gushcha in 2026.

The continuation is deliberately narrower in scope than the original.
It focuses on the two use cases that were furthest along and most
useful in practice (**distributed git hosting** and **file
synchronization**) and archives the more experimental components:
the consensus protocol prototypes (`hbs2-qblf`), the web dashboard
(`hbs2-git-dashboard`), the standalone issue tracker (`fixme-new`),
and the storage repair tool (`hbs2-fixer`).

The wire protocol is preserved without breaking changes, so nodes
running the legacy hbs2 codebase can interoperate with this one.
On-disk storage is NCQ3, the format Dmitry shipped in late 2024;
NCQv1 compatibility is dropped.

### Base branch

The initial v1 work was built on the legacy `master` branch of the
upstream repository. In 2026 the fork was replanted on top of
`dev-0.25.3`, the branch Dmitry used for his last year of active
development. `dev-0.25.3` is several hundred commits ahead of `master`
and contains the storage rewrite (`hbs2-storage-ncq`), the
log-structured primitives (`hbs2-log-structured`), the new git remote
helper (`hbs2-git3`), and the split CLI (`hbs2-cli`). Subsequent
development continues from that base.

## Legacy archive

The original repository remains available read-only at
github.com/voidlizard/hbs2. Historical development happened both there
and on the self-hosted mirror referenced above. The protocol, the
architecture, the implementation decisions, and most of the code in
this repository are Dmitry's.
