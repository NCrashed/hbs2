# Contributing

Thanks for considering a contribution. This is a small project under
a single primary maintainer, so a short up-front conversation goes a
long way toward avoiding wasted work. For anything beyond a typo fix
or a small isolated patch, please open an issue first to align on
scope.

## Project scope

The two features under active maintenance are **distributed git
hosting** and **file synchronization**. The legacy experimental
components (consensus prototypes, web dashboard, standalone issue
tracker, repair tool) are not part of this fork; see
[`HISTORY.md`](HISTORY.md) for context.

Changes inside the two flagship areas are welcome. Changes outside
them are welcome but will be slower to land. Changes that
re-introduce the archived components will not be accepted without a
clear rationale.

## Setting up

Follow [`INSTALL.md`](INSTALL.md) to get a working build environment.
Either path (Nix or Cabal + ghcup) works; CI runs the Cabal path on
ubuntu-latest and macos-latest.

If you are using Nix:
```
nix develop
```
If you are using Cabal + ghcup, make sure your toolchain matches the
pinned versions (GHC 9.6.6, Cabal 3.12.1.0) so `cabal.project`'s
`with-compiler` directive is satisfied.

## Building and testing

```
cabal build all --enable-tests
```
This compiles every package in the project, including all
test-suites. Test code that does not compile is a CI failure.

`cabal test all` (running tests) is **not yet** wired into CI. The
legacy test suite has not been fully audited for the current API and
runtime environment; some suites are marked `buildable: False` in
`hbs2-tests/hbs2-tests.cabal` and need a rewrite. If you fix one,
remove the `buildable: False` line in the same patch.

If you add a new test, run it locally before submitting and make
sure it succeeds.

## Submitting changes

1. Fork the repository and create a feature branch off `master`.
2. Make changes in small, focused commits. Commit messages should
   have a one-line subject (under ~70 characters) and, where useful,
   a body explaining *why* the change is needed.
3. Open a pull request against `master`. CI runs automatically and
   must be green before merge.
4. Expect review comments. The maintainer may ask for revisions or
   for the scope to be narrowed.

For a small obvious change, opening a PR directly is fine. For
larger work, an issue first saves time on both sides.

## Adding or updating dependencies

This project pins exact versions of every Haskell dependency in
`cabal.project.freeze`. The freeze keeps builds reproducible across
machines and across time, and it is also what makes the CI cache
key meaningful.

To add a new direct dependency:

1. Add it to the relevant `.cabal` file's `build-depends`.
2. Pin its version (and any new transitive dependencies the solver
   pulls in) in `cabal.project.freeze`.
3. Run `cabal build all --enable-tests` locally and make sure it
   passes before opening a PR.

To bump an existing dependency:

1. Edit the version in `cabal.project.freeze`.
2. Bump the Hackage `index-state` in `cabal.project` if the new
   version was published after the current pin.
3. Build, test, and verify CI.

Bumping many dependencies at once is fragile because of the way
upstream version bounds interact. Smaller PRs are easier to land.

## Code style

There are no automated formatters enforced in CI. Match the style of
the surrounding code. `weeder.toml` and `.hlint.yaml` exist for
opt-in linting; they are not blocking.

Avoid adding new vendored forks under `miscellaneous/`. We are
working *down* from the existing set, not up. If you need an
upstream patch, prefer a real PR upstream over a fork.

## Questions

Open a GitHub issue. There is no separate chat channel yet.
