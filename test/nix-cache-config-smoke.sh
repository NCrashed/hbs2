#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"

cache_url=https://hbs2.cachix.org
cache_key='hbs2.cachix.org-1:IGiwrFd/DNMG/+Oqjor6wwBh7T6EWC9uGR1Y1pD4VaI='
workflow=.github/workflows/nix-cache.yml

fail() {
  echo "nix cache config smoke: $*" >&2
  exit 1
}

cache_contract_files=(flake.nix INSTALL.md docs/nixos-desktop.md docs/nix-cache.md)

grep -Fq "$cache_url" "${cache_contract_files[@]}" \
  || fail "cache URL is not consistent across flake and install docs"
grep -Fq "$cache_key" "${cache_contract_files[@]}" \
  || fail "cache public key is not consistent across flake and install docs"

test -f "$workflow" || fail "missing $workflow"
grep -Fq 'cachix/cachix-action@v17' "$workflow" \
  || fail "Cachix action is not pinned to v17"
grep -Fq 'secrets.CACHIX_AUTH_TOKEN' "$workflow" \
  || fail "upload token is not wired"
grep -Fq 'secrets.CACHIX_SIGNING_KEY' "$workflow" \
  || fail "self-signing key is not wired"
grep -Fq 'CACHE_NAME: hbs2' "$workflow" \
  || fail "workflow cache name drifted"
grep -Fq -- '--max-jobs 0' "$workflow" \
  || fail "clean cache-hit verification can compile locally"

for binary in hbs2-peer hbs2-git3 git-remote-hbs23; do
  grep -Fq "$binary" "$workflow" \
    || fail "cache-hit workflow does not verify $binary"
done

grep -Fq '.env' .gitignore || fail ".env is not ignored"

echo "nix cache config smoke: OK"
