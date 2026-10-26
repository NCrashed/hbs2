#!/usr/bin/env bash
# The bounded repository wait and `repo:init <key>`, end to end.
#
# One throwaway hbs2-peer on 127.0.0.1, multicast and bootstrap off, no known
# peers, in a temporary HOME: its config, storage, keyman and git config are
# all under one mktemp directory, so nothing here can reach a real peer, key or
# repository. The rpc socket is in that directory too, not /tmp/hbs2-rpc.socket.
#
# What it walks through is the way a repository gets lost and comes back:
#
#   1. a repository is made and pushed (with and without --new), and cloned;
#   2. the peer's storage is wiped, which is what the ncq3 rollback did to a
#      real one: the key is still in keyman, the lwwref has no value anywhere;
#   3. a push from the clone FAILS, within the limit, saying what happened and
#      what to run (it used to hang forever and print nothing);
#   4. `repo:init <remote>` publishes the repository again under the same key,
#      in the clone that still holds state about the lost one;
#   5. `git push --all` and `--tags` from there work, a fresh clone sees them,
#      and the original working copy, also with stale state, can push a branch;
#   6. `repo:init <key>` on a key that has a readable value changes nothing;
#   7. a key with no value and no private key in keyman is refused;
#   8. a key whose value points at a manifest that does not exist is refused,
#      and replaced with --force.
#
# Usage:  BIN=<dir with hbs2-peer hbs2-cli hbs2-keyman hbs2-git3 git-remote-hbs23> \
#           hbs2-git3/test/e2e/reinit.sh
#
# From the source tree, after `cabal build all`:
#   BIN=$(for b in hbs2-peer hbs2-cli hbs2-keyman hbs2-git3:exe:hbs2-git3 git-remote-hbs23; do
#          dirname "$(cabal list-bin "$b")"; done | paste -sd:) hbs2-git3/test/e2e/reinit.sh
#
# KEEP=1 keeps the temporary directory. HBS2_GIT_REPO_WAIT (default 8 here)
# is the limit under test.

set -euo pipefail

: "${BIN:?set BIN to a PATH fragment holding hbs2-peer, hbs2-cli, hbs2-keyman, hbs2-git3 and git-remote-hbs23}"

UDP_PORT=${UDP_PORT:-17351}
TCP_PORT=${TCP_PORT:-17352}
WAIT=${HBS2_GIT_REPO_WAIT:-8}

T=$(mktemp -d "${TMPDIR:-/tmp}/hbs2-git3-e2e.XXXXXX")

export PATH="$BIN:$PATH"
export HOME="$T/home"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export GIT_CONFIG_GLOBAL="$HOME/.gitconfig"
export GIT_CONFIG_NOSYSTEM=1
export LC_ALL=C
export HBS2_GIT_REPO_WAIT="$WAIT"
unset GIT_DIR GIT_WORK_TREE HBS2TRACE

mkdir -p "$HOME"

PEER_PID=
PASS=0

stop_peer() {
  if [ -n "$PEER_PID" ]; then
    kill "$PEER_PID" 2>/dev/null || true
    wait "$PEER_PID" 2>/dev/null || true
    PEER_PID=
  fi
}

cleanup() {
  stop_peer
  if [ "${KEEP:-}" = 1 ]; then echo "kept $T"; else rm -rf "$T"; fi
}
trap cleanup EXIT

fail() { echo "FAIL: $*" >&2; exit 1; }
ok()   { PASS=$((PASS+1)); echo "ok $PASS - $*"; }

git config --global user.name  e2e
git config --global user.email e2e@localhost
git config --global commit.gpgsign false
git config --global tag.gpgsign false
git config --global init.defaultBranch master

# The peer's own key comes from `hbs2-peer init`; everything it binds to is
# then set here, so the defaults (0.0.0.0, the shared rpc socket, port 5005)
# never apply.
hbs2-peer init >/dev/null
PEER_CONF="$XDG_CONFIG_HOME/hbs2-peer/config"
[ -f "$XDG_CONFIG_HOME/hbs2-peer/default.key" ] || fail "hbs2-peer init made no key"
cat > "$PEER_CONF" <<CONF
listen "127.0.0.1:$UDP_PORT"
listen-tcp "127.0.0.1:$TCP_PORT"
rpc unix "$T/rpc.socket"
http-port "off"
multicast off
bootstrap off
storage "$T/storage"
key "./default.key"
CONF

start_peer() {
  (cd "$XDG_CONFIG_HOME/hbs2-peer" && exec hbs2-peer run >>"$T/peer.log" 2>&1) &
  PEER_PID=$!
  for _ in $(seq 1 100); do
    if hbs2-peer poke >/dev/null 2>&1; then return 0; fi
    kill -0 "$PEER_PID" 2>/dev/null || { tail -30 "$T/peer.log" >&2; fail "peer exited"; }
    sleep 0.2
  done
  tail -30 "$T/peer.log" >&2
  fail "peer did not answer"
}

hbs2-keyman update >/dev/null 2>&1 || true

start_peer
hbs2-peer poke | grep -q "$T/rpc.socket" || fail "poke does not report the temporary socket"
ok "a temporary peer is up on $T/rpc.socket"

# 1. a repository, pushed and cloned ----------------------------------------

mkdir "$T/work"
cd "$T/work"
git init -q
echo one > f; git add f; git commit -qm one
git tag v1
hbs2-git3 repo:init --new > "$T/init.out" 2>&1 || { cat "$T/init.out"; fail "init --new"; }
REMOTE=$(git remote | head -1)
URL=$(git remote get-url "$REMOTE")
KEY=${URL#hbs23://}
[ -n "$KEY" ] && [ "$KEY" != "$URL" ] || fail "no hbs23 remote after init: $URL"

git push -q "$REMOTE" --all 2>"$T/push0.err" || { cat "$T/push0.err"; fail "first push"; }
git push -q "$REMOTE" --tags 2>>"$T/push0.err" || { cat "$T/push0.err"; fail "first tag push"; }
ok "init --new and the first push"

# Without --new: init waits for a value the fresh key cannot have, then
# publishes. The reflog then has its first entry too, so a push does not wait
# for one that would never come.
mkdir "$T/plain"
cd "$T/plain"
git init -q
echo p > p; git add p; git commit -qm plain
hbs2-git3 repo:init > "$T/init-plain.out" 2>&1 || { cat "$T/init-plain.out"; fail "init"; }
git push -q "$(git remote | head -1)" master 2>"$T/push-plain.err" || { cat "$T/push-plain.err"; fail "push after plain init"; }
ok "init without --new, then a push"
cd "$T/work"

git clone -q "$URL" "$T/clone1" 2>"$T/clone1.err" || { cat "$T/clone1.err"; fail "clone"; }
[ "$(git -C "$T/clone1" log -1 --format=%s)" = one ] || fail "clone has the wrong head"
ls "$T/clone1/.hbs2-git3" | grep -qv '^config$' || fail "clone keeps no state, the stale-state case would test nothing"
ok "a clone, with local state about the repository"

# 2. the storage goes --------------------------------------------------------

stop_peer
rm -rf "$T/storage"
start_peer
[ -z "$(hbs2-peer lwwref get "$KEY" 2>/dev/null)" ] || fail "lwwref still has a value after the wipe"
ok "storage wiped: the lwwref has no value, the key is still in keyman"

# 3. a push now fails, in time, and says why --------------------------------

cd "$T/clone1"
echo two >> f; git commit -qam two
t0=$(date +%s)
set +e
timeout $((WAIT * 4 + 30)) git push origin master >"$T/push1.out" 2>"$T/push1.err"
rc=$?
set -e
[ "$rc" != 0 ] || fail "push to a lost repository succeeded"
t1=$(date +%s)
[ "$rc" != 124 ] || fail "push to a lost repository still hangs"
grep -q "repository $KEY not found: its lwwref has no value on this peer or its neighbours" "$T/push1.err" \
  || { cat "$T/push1.err"; fail "push did not say the repository is not found"; }
grep -q "hbs2-git3 repo:init $KEY" "$T/push1.err" || { cat "$T/push1.err"; fail "push did not say how to recover"; }
ok "git push to a lost repository fails after $((t1 - t0))s (limit ${WAIT}s) and says what to run"

if timeout $((WAIT * 4 + 30)) hbs2-git3 repo:manifest "$KEY" >"$T/mf.out" 2>"$T/mf.err"; then
  fail "repo:manifest of a lost repository succeeded"
fi
grep -q "not found" "$T/mf.err" || { cat "$T/mf.err"; fail "repo:manifest did not say why"; }
ok "the CLI gives up the same way"

# 4. publish it again, under the same key, from the stale clone ------------

hbs2-git3 repo:init origin >"$T/reinit.out" 2>"$T/reinit.err" || { cat "$T/reinit.out" "$T/reinit.err"; fail "repo:init origin"; }
grep -q "has no value; publishing a new one" "$T/reinit.err" || { cat "$T/reinit.err"; fail "re-init did not say what it found"; }
grep -q "git remote origin already points at hbs23://$KEY" "$T/reinit.out" || { cat "$T/reinit.out"; fail "re-init added a second remote"; }
grep -q "published repository $KEY" "$T/reinit.out" || { cat "$T/reinit.out"; fail "re-init did not report"; }
[ "$(git remote | wc -l)" = 1 ] || fail "re-init added a remote"
[ -n "$(hbs2-peer lwwref get "$KEY")" ] || fail "re-init wrote no lwwref value"
ok "repo:init <remote> publishes under the old key and keeps the remote"

# 5. pushes from stale state work, and a fresh clone sees them ---------------

git push -q origin --all 2>"$T/push2.err" || { cat "$T/push2.err"; fail "push --all after re-init"; }
git push -q origin --tags 2>>"$T/push2.err" || { cat "$T/push2.err"; fail "push --tags after re-init"; }
ok "git push --all and --tags from the clone with stale state"

git clone -q "$URL" "$T/clone2" 2>"$T/clone2.err" || { cat "$T/clone2.err"; fail "clone after re-init"; }
[ "$(git -C "$T/clone2" log -1 --format=%s)" = two ] || fail "fresh clone has the wrong head"
git -C "$T/clone2" rev-parse -q --verify refs/tags/v1 >/dev/null || fail "fresh clone has no tag"
ok "a fresh clone gets the branch and the tag"

cd "$T/work"
git checkout -qb feature
echo three > g; git add g; git commit -qm three
git push -q "$REMOTE" feature 2>"$T/push3.err" || { cat "$T/push3.err"; fail "push from the original working copy"; }
git -C "$T/clone2" fetch -q origin 2>/dev/null || fail "fetch"
[ "$(git -C "$T/clone2" log -1 --format=%s origin/feature)" = three ] || fail "branch from the original copy not seen"
ok "the original working copy, with its own stale state, pushes a branch"

# 6. a readable repository is left alone -------------------------------------

before=$(hbs2-peer lwwref get "$KEY")
hbs2-git3 repo:init "$KEY" >"$T/again.out" 2>"$T/again.err" || { cat "$T/again.out" "$T/again.err"; fail "repo:init on a live key"; }
grep -q "repository $KEY is already published" "$T/again.out" || { cat "$T/again.out"; fail "did not say it is published"; }
grep -q "nothing was changed" "$T/again.out" || fail "did not say nothing changed"
[ "$(hbs2-peer lwwref get "$KEY")" = "$before" ] || fail "the lwwref changed"
hbs2-git3 repo:init --force "$URL" >"$T/again2.out" 2>"$T/again2.err" || fail "repo:init --force on a live key"
[ "$(hbs2-peer lwwref get "$KEY")" = "$before" ] || fail "--force replaced a readable repository"
ok "repo:init on a readable repository changes nothing, --force included"

# 7. no private key ----------------------------------------------------------

STRANGER=2v3ubvkrQaWzhBCZW14JDWUW1LGBMnkirWeJJvZxnNUV
if hbs2-git3 repo:init "$STRANGER" >"$T/stranger.out" 2>"$T/stranger.err"; then
  fail "repo:init without the private key succeeded"
fi
grep -q "its private key is not known to hbs2-keyman" "$T/stranger.err" || { cat "$T/stranger.err"; fail "no-key refusal unclear"; }
[ -z "$(hbs2-peer lwwref get "$STRANGER" 2>/dev/null)" ] || fail "something was written for a key we do not own"
ok "a key with no private key in keyman is refused"

# 8. a value whose manifest does not exist -----------------------------------

NOWHERE=8yqJyq5jxKmDdMwzGvQhx3srKuc1FqmxCYSZKz5yzXWJ
K3=$(hbs2-cli hbs2:lwwref:create | sed -n 's/.*pk "\([^"]*\)".*/\1/p')
[ -n "$K3" ] || fail "could not create a key"
hbs2-cli hbs2:lwwref:update "$K3" "$NOWHERE" >/dev/null
[ -n "$(hbs2-peer lwwref get "$K3")" ] || fail "no value for the broken key"
if hbs2-git3 repo:init "$K3" >"$T/broken.out" 2>"$T/broken.err"; then
  fail "repo:init over an unreadable value succeeded without --force"
fi
grep -q "its manifest could not be read" "$T/broken.err" || { cat "$T/broken.err"; fail "unreadable refusal unclear"; }
grep -q "repo:init --force $K3" "$T/broken.err" || fail "unreadable refusal does not name --force"
hbs2-git3 repo:init --force "$K3" >"$T/forced.out" 2>"$T/forced.err" || { cat "$T/forced.out" "$T/forced.err"; fail "--force"; }
hbs2-git3 repo:manifest "$K3" >"$T/forced.mf" 2>/dev/null || fail "no readable manifest after --force"
grep -q reflog "$T/forced.mf" || fail "manifest after --force has no reflog"
ok "an unreadable value is refused, and replaced with --force"

echo "all $PASS passed"
