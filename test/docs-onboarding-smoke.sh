#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"

fail() {
  echo "docs onboarding smoke: $*" >&2
  exit 1
}

if grep -Eq 'init.*does not generate|bootstrap node.*not (yet )?deployed' \
    QUICKSTART.md docs/multi-machine.md; then
  fail "stale init/bootstrap guidance returned"
fi

required=(
  'hbs2-peer init'
  'hbs2-keyman update'
  'loginctl enable-linger'
  'hbs2-peer doctor'
  'repo:member:add'
  'hbs2:groupkey:find-secret'
  'git rev-parse HEAD'
  "HEAD^{tree}"
  'git fsck'
  'git-remote-hbs23'
)

for needle in "${required[@]}"; do
  grep -Fq "$needle" QUICKSTART.md docs/nixos-desktop.md docs/encrypted-repos.md \
    || fail "missing documented command or concept: $needle"
done

peer_bin=${HBS2_PEER:-}
git3_bin=${HBS2_GIT3:-}

if [[ -z "$peer_bin" ]] && command -v cabal >/dev/null; then
  peer_bin=$(cabal list-bin hbs2-peer:exe:hbs2-peer 2>/dev/null || true)
fi
if [[ -z "$git3_bin" ]] && command -v cabal >/dev/null; then
  git3_bin=$(cabal list-bin hbs2-git3:exe:hbs2-git3 2>/dev/null || true)
fi

if [[ -x "$peer_bin" && -x "$git3_bin" ]]; then
  HBS2_PEER="$peer_bin" HBS2_GIT3="$git3_bin" \
    bash hbs2-git3/test/e2e/cli-discovery.sh
else
  echo "docs onboarding smoke: binaries absent; static checks passed"
fi

echo "docs onboarding smoke: OK"
