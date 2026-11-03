#!/usr/bin/env bash
# Three isolated peers proving an encrypted repository can be cloned through a
# storage-only relay after the owner goes offline.
#
# A (owner) and B (new member) only know R.  Their declared network class keeps
# the loopback addresses used by this harness out of PEX, and the test also
# checks the live peer lists before A is stopped.  Every role has a separate
# HOME, RPC socket, peer key, keyman database and block store.
#
# Usage:
#   BIN=<PATH fragment containing hbs2-peer, hbs2-cli, hbs2-keyman,
#        hbs2-git3 and git-remote-hbs23> \
#     hbs2-git3/test/e2e/encrypted-relay-clone.sh

set -euo pipefail

: "${BIN:?set BIN to the built hbs2 command directories}"

T=$(mktemp -d "${TMPDIR:-/tmp}/hbs2-encrypted-relay-e2e.XXXXXX")
export PATH="$BIN:$PATH"
export LC_ALL=C
export GIT_CONFIG_NOSYSTEM=1
export HBS2_GIT_REPO_WAIT=${HBS2_GIT_REPO_WAIT:-12}
unset GIT_DIR GIT_WORK_TREE HBS2TRACE

PASS=0
fail() { echo "FAIL: $*" >&2; exit 1; }
ok() { PASS=$((PASS + 1)); echo "ok $PASS - $*"; }

role() {
  local name=$1
  shift
  local home="$T/$name/home"
  env HOME="$home" \
      XDG_CONFIG_HOME="$home/.config" \
      XDG_DATA_HOME="$home/.local/share" \
      XDG_STATE_HOME="$home/.local/state" \
      XDG_CACHE_HOME="$home/.cache" \
      GIT_CONFIG_GLOBAL="$home/.gitconfig" \
      "$@"
}

pid_file() { echo "$T/$1/peer.pid"; }

stop_peer() {
  local name=$1 file pid
  file=$(pid_file "$name")
  [ -f "$file" ] || return 0
  pid=$(cat "$file")
  kill "$pid" 2>/dev/null || true
  wait "$pid" 2>/dev/null || true
  rm -f "$file"
}

cleanup() {
  stop_peer A
  stop_peer R
  stop_peer B
  if [ "${KEEP:-}" = 1 ]; then echo "kept $T"; else rm -rf "$T"; fi
}
trap cleanup EXIT

write_config() {
  local name=$1 udp=$2 known=${3:-} class=${4:-clearnet}
  local conf="$T/$name/home/.config/hbs2-peer/config"
  {
    echo "listen \"127.0.0.1:$udp\""
    echo 'listen-tcp "off"'
    echo "rpc unix \"$T/$name/rpc.socket\""
    echo 'http-port "off"'
    echo 'multicast off'
    echo 'bootstrap off'
    echo "network-class \"$class\""
    echo 'accept-block-announce *'
    echo "storage \"$T/$name/storage\""
    echo 'key "./default.key"'
    if [ -n "$known" ]; then echo "known-peer \"$known\""; fi
  } > "$conf"
}

start_peer() {
  local name=$1 log file home
  log="$T/$name/peer.log"
  file=$(pid_file "$name")
  home="$T/$name/home"
  (cd "$home/.config/hbs2-peer" && exec env \
      HOME="$home" \
      XDG_CONFIG_HOME="$home/.config" \
      XDG_DATA_HOME="$home/.local/share" \
      XDG_STATE_HOME="$home/.local/state" \
      XDG_CACHE_HOME="$home/.cache" \
      GIT_CONFIG_GLOBAL="$home/.gitconfig" \
      hbs2-peer run >>"$log" 2>&1) &
  echo $! > "$file"
  for _ in $(seq 1 150); do
    if role "$name" hbs2-peer poke >/dev/null 2>&1; then return 0; fi
    kill -0 "$(cat "$file")" 2>/dev/null || {
      tail -40 "$log" >&2
      fail "$name peer exited"
    }
    sleep 0.2
  done
  tail -40 "$log" >&2
  fail "$name peer did not answer"
}

wait_for_peer_key() {
  local name=$1 wanted=$2
  for _ in $(seq 1 300); do
    role "$name" hbs2-peer peers 2>/dev/null | grep -q "$wanted" && return 0
    sleep 0.2
  done
  role "$name" hbs2-peer peers >&2 || true
  fail "$name did not connect to $wanted"
}

wait_complete_repo() {
  local name=$1 key=$2
  for _ in $(seq 1 120); do
    role "$name" hbs2-peer doctor --repo "$key" >/dev/null 2>&1 && return 0
    sleep 0.5
  done
  role "$name" hbs2-peer doctor --repo "$key" >&2 || true
  fail "$name did not obtain a complete repository $key"
}

assert_tree_complete() {
  local name=$1 root=$2 out
  out=$(role "$name" hbs2-cli "(hbs2:tree:missed \"$root\")")
  [ -z "$out" ] || [ "$out" = "()" ] || fail "$name is missing descendants of $root: $out"
}

lww_head() {
  local name=$1 key=$2
  role "$name" timeout 5 hbs2-peer lwwref get "$key" 2>/dev/null |
    sed -n 's/.*(value "\([^"]*\)").*/\1/p' |
    tail -1
}

for name in A R B; do
  mkdir -p "$T/$name/home"
  role "$name" hbs2-peer init >/dev/null
  # Keyman discovers the exact key path from the effective peer config; there
  # is no broad HOME scan and no separate add-mask onboarding step.
  role "$name" hbs2-keyman update >/dev/null
  role "$name" git config --global user.name "$name-e2e"
  role "$name" git config --global user.email "$name-e2e@localhost"
  role "$name" git config --global commit.gpgsign false
  role "$name" git config --global tag.gpgsign false
  role "$name" git config --global init.defaultBranch master
done

write_config R 18452 "" bridge
# Both endpoints declare onion reachability so R will not hand either endpoint's
# loopback clearnet address to the other through PEX.  The explicit R address is
# still dialled, and is the only configured path.
write_config A 18451 "127.0.0.1:18452" onion
write_config B 18453 "127.0.0.1:18452" onion

start_peer R
start_peer A
start_peer B

SIGN_A=$(role A hbs2-cli hbs2:keyring:show "$T/A/home/.config/hbs2-peer/default.key" | awk '/sign-key:/ {print $2; exit}')
SIGN_B=$(role B hbs2-cli hbs2:keyring:show "$T/B/home/.config/hbs2-peer/default.key" | awk '/sign-key:/ {print $2; exit}')
SIGN_R=$(role R hbs2-cli hbs2:keyring:show "$T/R/home/.config/hbs2-peer/default.key" | awk '/sign-key:/ {print $2; exit}')
ENC_A=$(role A hbs2-cli hbs2:keyring:show "$T/A/home/.config/hbs2-peer/default.key" | awk '/pub-key:/ {print $2; exit}')
ENC_B=$(role B hbs2-cli hbs2:keyring:show "$T/B/home/.config/hbs2-peer/default.key" | awk '/pub-key:/ {print $2; exit}')
[ -n "$SIGN_A" ] && [ -n "$SIGN_B" ] && [ -n "$SIGN_R" ] || fail "could not read peer signing keys"
[ -n "$ENC_A" ] && [ -n "$ENC_B" ] || fail "could not read member encryption keys"

wait_for_peer_key A "$SIGN_R"
wait_for_peer_key B "$SIGN_R"
if role A hbs2-peer peers | grep -q "$SIGN_B" || role B hbs2-peer peers | grep -q "$SIGN_A"; then
  fail "A and B established a forbidden direct connection"
fi
ok "A and B are connected only through R"

GK1=$(role A hbs2-cli "(hbs2:groupkey:store (hbs2:groupkey:create $ENC_A))")
[ -n "$GK1" ] || fail "owner group key was not created"
# Some peer builds keep announcing until interrupted; one successful interval
# is enough for this fixture, and must not pin the whole test indefinitely.
role A timeout 3 hbs2-peer announce "$GK1" >/dev/null 2>&1 || [ "$?" -eq 124 ]

mkdir -p "$T/A/work"
role A git -C "$T/A/work" init -q
echo before-membership > "$T/A/work/history.txt"
role A git -C "$T/A/work" add history.txt
role A git -C "$T/A/work" commit -qm before-membership
(cd "$T/A/work" && role A hbs2-git3 repo:init --new --encrypted "$GK1") >"$T/init.out" 2>"$T/init.err" || {
  cat "$T/init.out" "$T/init.err" >&2
  fail "encrypted repo:init"
}
REMOTE=$(role A git -C "$T/A/work" remote | head -1)
URL=$(role A git -C "$T/A/work" remote get-url "$REMOTE")
KEY=${URL#hbs23://}
[ -n "$KEY" ] && [ "$KEY" != "$URL" ] || fail "repo:init made no hbs23 remote"
role A git -C "$T/A/work" push -q "$REMOTE" master
ok "A published an owner-only encrypted commit"

role R timeout 90 hbs2-git3 repo:relay-only "$KEY" >"$T/relay0.out" 2>"$T/relay0.err" || {
  cat "$T/relay0.out" "$T/relay0.err" >&2
  fail "R could not subscribe to the repository"
}
wait_complete_repo R "$KEY"
ok "R obtained a complete encrypted replica"

set +e
role B timeout 60 git clone -q "$URL" "$T/B/before-clone" >"$T/preclone.out" 2>"$T/preclone.err"
pre_rc=$?
set -e
[ "$pre_rc" != 0 ] || fail "B cloned before it was a member"
[ "$pre_rc" != 124 ] || fail "pre-membership clone hung"
grep -Eqi 'group.?key.*secret|secret.*group.?key|GitRepoNoAccess' "$T/preclone.err" || {
  cat "$T/preclone.err" >&2
  fail "pre-membership clone did not identify the missing group-key secret"
}
! grep -q 'ImportBlocksUnavailable' "$T/preclone.err" || {
  cat "$T/preclone.err" >&2
  fail "pre-membership clone mistook access denial for missing blocks"
}
ok "B fails promptly with a missing group-key-secret diagnosis"

OLD_HEAD=$(lww_head R "$KEY")
OWNER_HEAD_BEFORE_DRY=$(lww_head A "$KEY")
role A hbs2-git3 repo:member:add --dry-run --json "$KEY" "enc:$ENC_B" >"$T/member-dry-run.out" || {
  cat "$T/member-dry-run.out" >&2
  fail "repo:member:add --dry-run"
}
grep -q '"status":"dry-run"' "$T/member-dry-run.out" || fail "dry run did not report its status"
[ "$(lww_head A "$KEY")" = "$OWNER_HEAD_BEFORE_DRY" ] || fail "dry run changed the repository head"

role A hbs2-git3 repo:member:add --json "$KEY" "enc:$ENC_B" >"$T/member-add.out" 2>"$T/member-add.err" || {
  cat "$T/member-add.out" "$T/member-add.err" >&2
  fail "repo:member:add"
}
grep -q '"status":"updated"' "$T/member-add.out" || fail "member add did not report an update"
GK2=$(sed -n 's/.*"groupKey":"\([^"]*\)".*/\1/p' "$T/member-add.out")
[ -n "$GK2" ] && [ "$GK2" != "$GK1" ] || fail "member add did not publish a new group key"

HEAD_AFTER_ADD=$(lww_head A "$KEY")
role A hbs2-git3 repo:member:add --json "$KEY" "enc:$ENC_B" >"$T/member-again.out" || fail "idempotent member add"
grep -q '"status":"unchanged"' "$T/member-again.out" || fail "repeated member add was not a no-op"
[ "$(lww_head A "$KEY")" = "$HEAD_AFTER_ADD" ] || fail "repeated member add changed the repository head"
ok "A added B with one high-level, dry-runnable, idempotent command"

echo after-membership >> "$T/A/work/history.txt"
role A git -C "$T/A/work" commit -qam after-membership
role A git -C "$T/A/work" push -q "$REMOTE" master

for _ in $(seq 1 120); do
  role R timeout 5 hbs2-peer lwwref fetch "$KEY" >/dev/null 2>&1 || true
  NEW_HEAD=$(lww_head R "$KEY" || true)
  [ -n "$NEW_HEAD" ] && [ "$NEW_HEAD" != "$OLD_HEAD" ] && break
  sleep 0.5
done
NEW_HEAD=${NEW_HEAD:-}
[ -n "$NEW_HEAD" ] && [ "$NEW_HEAD" != "$OLD_HEAD" ] || fail "R did not observe the membership update (old=$OLD_HEAD new=$NEW_HEAD)"
role R timeout 90 hbs2-git3 repo:relay-only "$KEY" >"$T/relay1.out" 2>"$T/relay1.err" || {
  cat "$T/relay1.out" "$T/relay1.err" >&2
  fail "R could not replicate the post-rotation pack trees"
}
wait_complete_repo R "$KEY"
assert_tree_complete R "$NEW_HEAD"
ok "R holds the complete post-rotation Merkle graph"

if role R hbs2-cli "(hbs2:groupkey:find-secret \"$GK2\")" >"$T/relay-secret.out" 2>"$T/relay-secret.err"; then
  fail "storage relay unexpectedly decrypted the group key"
fi
ok "R stores but cannot decrypt the repository"

stop_peer A
stop_peer B
rm -rf "$T/B/storage" "$T/B/before-clone" "$T/B/clone"
# Rebind B to a fresh endpoint so R cannot mistake the restarted process for
# the stale encrypted transport flow that used its original UDP tuple.
write_config B 18454 "127.0.0.1:18452" onion
start_peer B
wait_for_peer_key B "$SIGN_R"

role B timeout 90 git clone -q "$URL" "$T/B/clone" >"$T/clone.out" 2>"$T/clone.err" || {
  cat "$T/clone.out" "$T/clone.err" >&2
  fail "relay-only clone with A offline"
}

[ "$(role B git -C "$T/B/clone" rev-list --count HEAD)" = 2 ] || fail "clone does not contain both commits"
grep -q '^before-membership$' "$T/B/clone/history.txt" || fail "pre-membership content is absent"
grep -q '^after-membership$' "$T/B/clone/history.txt" || fail "post-membership content is absent"
role B git -C "$T/B/clone" fsck --full >/dev/null
assert_tree_complete B "$NEW_HEAD"
role B hbs2-cli "(hbs2:groupkey:find-secret \"$GK1\")" >/dev/null || fail "B cannot resolve the historical compatibility envelope"
ok "B cloned through R with A offline, read both eras, and passed git fsck"

echo "1..$PASS"
