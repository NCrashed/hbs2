#!/usr/bin/env bash
#
# Package the dynamic aarch64-darwin build into a self-contained
# tarball with bundled dylibs (Mach-O equivalent of $ORIGIN-based
# RPATH bundling on Linux). Output goes to ./dist-darwin/.
#
# Run on an Apple Silicon Mac with Nix installed.
#
#   ./scripts/bundle-darwin.sh 0.25.3.2
#
# Produces:
#   dist-darwin/hbs2-<VERSION>-aarch64-apple-darwin.tar.gz
#   dist-darwin/hbs2-<VERSION>-aarch64-apple-darwin.tar.gz.sha256

set -euo pipefail

VERSION="${1:?usage: $0 <version>}"
ARCH="aarch64-apple-darwin"
NAME="hbs2-${VERSION}-${ARCH}"

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WORK="${REPO_ROOT}/dist-darwin"
BUNDLE="${WORK}/${NAME}"

# bf6-git-hbs2 is excluded for the same reason it is excluded from
# the docker image: it is a shebang script that pulls suckless-conf
# (and transitively GHC) into the closure. In the package output it
# appears under bin/ as `git-hbs2`, so both names are excluded; the
# git-hbs2 symlink below provides the `git hbs2 ...` dispatch instead.
EXCLUDE_BINS=(bf6-git-hbs2 git-hbs2)

is_excluded() {
  local n="$1"
  for x in "${EXCLUDE_BINS[@]}"; do [[ "$n" == "$x" ]] && return 0; done
  return 1
}

echo "==> Building .#packages.aarch64-darwin.default"
DARWIN_PKG="$(nix build --no-link --print-out-paths "${REPO_ROOT}#packages.aarch64-darwin.default")"
echo "    ${DARWIN_PKG}"

echo "==> Preparing ${BUNDLE}"
rm -rf "${WORK}"
mkdir -p "${BUNDLE}/bin" "${BUNDLE}/lib"

echo "==> Copying binaries (resolving symlinks)"
shopt -s nullglob
for f in "${DARWIN_PKG}/bin"/*; do
  name="$(basename "$f")"
  if is_excluded "$name"; then
    echo "    skip ${name}"
    continue
  fi
  cp -L "$f" "${BUNDLE}/bin/${name}"
  chmod +w "${BUNDLE}/bin/${name}"
done

# git-hbs2 -> hbs2-git3 (mirrors dockerImage convention so that
# `git hbs2 ...` works once bin/ is on PATH).
ln -s hbs2-git3 "${BUNDLE}/bin/git-hbs2"

echo "==> Bundling dylibs via dylibbundler"
# dylibbundler walks each binary's dylib closure, copies non-system
# dylibs into ${BUNDLE}/lib, and rewrites install names (both in the
# binaries and in the copied dylibs themselves) to
# @loader_path/../lib/<name>. System libs (/usr/lib, /System/...)
# are left referenced in place.
X_FLAGS=()
for f in "${BUNDLE}/bin"/*; do
  [[ -L "$f" ]] && continue
  X_FLAGS+=(-x "$f")
done

nix shell nixpkgs#macdylibbundler -c dylibbundler \
  -of -b -cd \
  -d "${BUNDLE}/lib" \
  -p '@loader_path/../lib/' \
  "${X_FLAGS[@]}"

echo "==> Re-signing modified binaries (ad-hoc)"
# install_name_tool invalidates code signatures. Apple Silicon refuses
# to launch arm64 binaries without a valid signature, even ad-hoc, so
# re-sign every binary and bundled dylib with `codesign -s -`.
while IFS= read -r f; do
  codesign --force --sign - "$f" >/dev/null 2>&1 || {
    echo "    WARN: failed to sign $f" >&2
  }
done < <(find "${BUNDLE}/bin" "${BUNDLE}/lib" -type f)

echo "==> Writing README and LICENSE"
cp "${REPO_ROOT}/LICENSE" "${BUNDLE}/LICENSE"
cat > "${BUNDLE}/README.txt" <<EOF
hbs2 ${VERSION} for ${ARCH} (Apple Silicon).

Dynamically linked against macOS system libraries (libSystem, etc.);
all non-system dependencies are bundled in lib/ and resolved via
@loader_path/../lib so bin/ and lib/ must remain siblings if you
relocate them.

To install manually: copy the contents to a prefix on PATH, e.g.
  sudo cp -R bin/* /usr/local/bin/
  sudo cp -R lib/* /usr/local/lib/
or unpack anywhere and add the bin/ directory to PATH.

The recommended install path is Homebrew:
  brew install ncrashed/hbs2/hbs2-peer

Notes:
  - hbs2-sync's "mount" subcommand requires macFUSE
    (https://osxfuse.github.io/). All other hbs2-sync subcommands
    work without it.
  - Source: https://github.com/NCrashed/hbs2
EOF

echo "==> Building tarball"
( cd "${WORK}" && tar -czf "${NAME}.tar.gz" "${NAME}" )
( cd "${WORK}" && shasum -a 256 "${NAME}.tar.gz" > "${NAME}.tar.gz.sha256" )

echo
echo "DONE"
ls -la "${WORK}/${NAME}.tar.gz" "${WORK}/${NAME}.tar.gz.sha256"
echo
echo "sha256:"
cat "${WORK}/${NAME}.tar.gz.sha256"
