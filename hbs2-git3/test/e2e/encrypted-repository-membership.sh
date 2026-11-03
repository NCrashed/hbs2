#!/usr/bin/env bash
# An encrypted repository distinguishes missing membership from missing blocks.
#
# Two isolated peers are connected directly over loopback.  A publishes a Git
# repository encrypted to A alone.  B first clones as a non-member and must
# fail promptly with the exact group-key hash, without exhausting the missing
# block retry loop.  A then rotates the repository group key to include B; a
# fresh clone on B must contain the history written before that rotation.
#
# Usage, after `cabal build all`:
#   BIN=$(for b in hbs2-peer hbs2-cli hbs2-keyman hbs2-git3:exe:hbs2-git3 git-remote-hbs23; do
#          dirname "$(cabal list-bin "$b")"
#        done | paste -sd:) \
#     hbs2-git3/test/e2e/encrypted-repository-membership.sh
#
# KEEP=1 preserves the temporary directory and peer logs on failure.

set -euo pipefail

: "${BIN:?set BIN to a PATH fragment holding hbs2-peer, hbs2-cli, hbs2-keyman, hbs2-git3 and git-remote-hbs23}"

PORT_BASE=${PORT_BASE:-37351}
A_UDP_PORT=${A_UDP_PORT:-$PORT_BASE}
B_UDP_PORT=${B_UDP_PORT:-$((PORT_BASE + 1))}
A_TCP_PORT=${A_TCP_PORT:-$((PORT_BASE + 10))}
B_TCP_PORT=${B_TCP_PORT:-$((PORT_BASE + 11))}
WAIT=${HBS2_GIT_REPO_WAIT:-8}
CLONE_TIMEOUT=${CLONE_TIMEOUT:-90}
PROMPT_LIMIT=${PROMPT_LIMIT:-30}
PORT_LOCK_WAIT=${PORT_LOCK_WAIT:-300}

command -v flock >/dev/null 2>&1 || {
  echo "flock is required to isolate the encrypted repository e2e ports" >&2
  exit 2
}

PORT_LOCK_FILE="/tmp/hbs2-git3-encrypted-e2e.$PORT_BASE.lock"
exec {PORT_LOCK_FD}>"$PORT_LOCK_FILE"
flock -w "$PORT_LOCK_WAIT" "$PORT_LOCK_FD" || {
  echo "timed out waiting for encrypted repository e2e ports at base $PORT_BASE" >&2
  exit 1
}

T=$(mktemp -d "${TMPDIR:-/tmp}/hbs2-git3-encrypted-e2e.XXXXXX")

export PATH="$BIN:$PATH"
unset GIT_DIR GIT_WORK_TREE HBS2TRACE

declare -A PEER_PID=()
PASS=0

node_env() {
  local node=$1
  shift
  local home="$T/$node/home"
  env \
    HOME="$home" \
    XDG_CONFIG_HOME="$home/.config" \
    XDG_DATA_HOME="$home/.local/share" \
    XDG_STATE_HOME="$home/.local/state" \
    XDG_CACHE_HOME="$home/.cache" \
    GIT_CONFIG_GLOBAL="$home/.gitconfig" \
    GIT_CONFIG_NOSYSTEM=1 \
    LC_ALL=C \
    HBS2_GIT_REPO_WAIT="$WAIT" \
    "$@"
}

node_exec() {
  local node=$1
  shift
  local home="$T/$node/home"
  exec env \
    HOME="$home" \
    XDG_CONFIG_HOME="$home/.config" \
    XDG_DATA_HOME="$home/.local/share" \
    XDG_STATE_HOME="$home/.local/state" \
    XDG_CACHE_HOME="$home/.cache" \
    GIT_CONFIG_GLOBAL="$home/.gitconfig" \
    GIT_CONFIG_NOSYSTEM=1 \
    LC_ALL=C \
    HBS2_GIT_REPO_WAIT="$WAIT" \
    "$@"
}

fail() {
  echo "FAIL: $*" >&2
  exit 1
}

ok() {
  PASS=$((PASS + 1))
  echo "ok $PASS - $*"
}

stop_peer() {
  local node=$1
  local pid=${PEER_PID[$node]:-}
  if [ -n "$pid" ]; then
    kill -- "-$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
    unset 'PEER_PID[$node]'
  fi
}

cleanup() {
  stop_peer A
  stop_peer B
  if [ "${KEEP:-}" = 1 ]; then
    echo "kept $T"
  else
    rm -rf "$T"
  fi
}
trap cleanup EXIT

write_peer_config() {
  local node=$1
  local udp_port=$2
  local tcp_port=$3
  local known_tcp_port=$4
  local home="$T/$node/home"
  local config_dir="$home/.config/hbs2-peer"

  mkdir -p "$home"
  node_env "$node" hbs2-peer init >/dev/null
  [ -f "$config_dir/default.key" ] || fail "node $node: hbs2-peer init made no key"

  {
    echo "listen \"127.0.0.1:$udp_port\""
    echo "listen-tcp \"127.0.0.1:$tcp_port\""
    echo "rpc unix \"$T/$node/rpc.socket\""
    echo 'http-port "off"'
    echo 'multicast off'
    echo 'bootstrap off'
    echo "storage \"$T/$node/storage\""
    echo 'key "./default.key"'
    echo "peer-public-address \"tcp://127.0.0.1:$tcp_port\""
    echo "known-peer \"tcp://127.0.0.1:$known_tcp_port\""
  } > "$config_dir/config"

  node_env "$node" hbs2-keyman update >/dev/null 2>&1 ||
    fail "node $node: hbs2-keyman update"
  node_env "$node" git config --global user.name e2e
  node_env "$node" git config --global user.email e2e@localhost
  node_env "$node" git config --global commit.gpgsign false
  node_env "$node" git config --global tag.gpgsign false
  node_env "$node" git config --global init.defaultBranch master
}

start_peer() {
  local node=$1
  local config_dir="$T/$node/home/.config/hbs2-peer"
  local poke=

  (
    cd "$config_dir"
    node_exec "$node" setsid hbs2-peer run +RTS -N1 -RTS >>"$T/$node/peer.log" 2>&1
  ) &
  PEER_PID[$node]=$!

  for _ in $(seq 1 100); do
    poke=$(node_env "$node" hbs2-peer poke 2>/dev/null || true)
    if grep -q "rpc: \"$T/$node/rpc.socket\"" <<<"$poke"; then
      sleep 2
      return 0
    fi
    if ! kill -0 "${PEER_PID[$node]}" 2>/dev/null; then
      tail -40 "$T/$node/peer.log" >&2 || true
      fail "node $node: peer exited"
    fi
    sleep 0.2
  done

  tail -40 "$T/$node/peer.log" >&2 || true
  fail "node $node: peer did not answer"
}

peer_key() {
  local node=$1
  sed -n 's/.*run peer \([1-9A-HJ-NP-Za-km-z]*\).*/\1/p' "$T/$node/peer.log" | head -1
}

wait_for_authenticated_peer() {
  local node=$1
  local expected_node=$2
  local expected_address=$3
  local node_address=
  local expected_key=
  local peers=
  local deadline=$((SECONDS + 30))

  case "$node" in
    A) node_address="127.0.0.1:$A_UDP_PORT" ;;
    B) node_address="127.0.0.1:$B_UDP_PORT" ;;
    *) fail "unknown peer node $node" ;;
  esac
  expected_key=$(peer_key "$expected_node")
  [ -n "$expected_key" ] || fail "node $expected_node did not report its peer key"

  while [ "$SECONDS" -lt "$deadline" ]; do
    peers=$(node_env "$node" hbs2-peer peers 2>/dev/null || true)
    if awk -v key="$expected_key" '$1 == key { found = 1 } END { exit !found }' <<<"$peers"; then
      return 0
    fi
    node_env "$node" timeout 2 hbs2-peer ping "$expected_address" >/dev/null 2>&1 || true
    node_env "$expected_node" timeout 2 hbs2-peer ping "$node_address" >/dev/null 2>&1 || true
    kill -0 "${PEER_PID[$node]}" 2>/dev/null || fail "node $node exited before authenticating $expected_node"
    sleep 0.2
  done

  printf '%s\n' "$peers" >&2
  tail -60 "$T/$node/peer.log" >&2 || true
  fail "node $node did not authenticate peer $expected_node"
}

encryption_key() {
  local node=$1
  local key_file=$2
  node_env "$node" hbs2-cli "hbs2:keyring:show $key_file" |
    sed -n 's/.*pub-key:[[:space:]]*//p' |
    head -1
}

write_peer_config A "$A_UDP_PORT" "$A_TCP_PORT" "$B_TCP_PORT"
write_peer_config B "$B_UDP_PORT" "$B_TCP_PORT" "$A_TCP_PORT"
start_peer A
start_peer B
wait_for_authenticated_peer A B "tcp://127.0.0.1:$B_TCP_PORT"
wait_for_authenticated_peer B A "tcp://127.0.0.1:$A_TCP_PORT"
ok "two isolated peers authenticated over loopback"

A_KEY_FILE=$(node_env A timeout 30 hbs2-cli '(hbs2:keyman:keys:add (hbs2:keyring:new 1))')
B_KEY_FILE=$(node_env B timeout 30 hbs2-cli '(hbs2:keyman:keys:add (hbs2:keyring:new 1))')
A_ENCRYPTION_KEY=$(encryption_key A "$A_KEY_FILE")
B_ENCRYPTION_KEY=$(encryption_key B "$B_KEY_FILE")
[ -n "$A_ENCRYPTION_KEY" ] || fail "node A has no encryption public key"
[ -n "$B_ENCRYPTION_KEY" ] || fail "node B has no encryption public key"

OWNER_GROUP_KEY=$(node_env A timeout 30 hbs2-cli \
  "(hbs2:groupkey:store (hbs2:groupkey:create $A_ENCRYPTION_KEY))")
[ -n "$OWNER_GROUP_KEY" ] || fail "could not create the owner-only group key"

mkdir "$T/work"
cd "$T/work"
node_env A git init -q
printf 'written before membership\n' > history.txt
node_env A git add history.txt
node_env A git commit -qm "history before membership"
node_env A timeout 60 hbs2-git3 repo:init --new --encrypted "$OWNER_GROUP_KEY"
REMOTE=$(node_env A git remote | head -1)
URL=$(node_env A git remote get-url "$REMOTE")
node_env A timeout 120 git push -q "$REMOTE" master
ok "A published owner-only encrypted history"

denied_at=$SECONDS
set +e
node_env B timeout "$CLONE_TIMEOUT" git clone "$URL" "$T/clone-denied" \
  >"$T/clone-denied.out" 2>"$T/clone-denied.err"
denied_rc=$?
set -e
denied_seconds=$((SECONDS - denied_at))

[ "$denied_rc" -ne 0 ] || fail "non-member clone succeeded"
[ "$denied_rc" -ne 124 ] || {
  cat "$T/clone-denied.err" >&2
  fail "non-member clone timed out instead of reporting missing membership"
}
[ "$denied_seconds" -lt "$PROMPT_LIMIT" ] || {
  cat "$T/clone-denied.err" >&2
  fail "non-member clone took ${denied_seconds}s; expected less than ${PROMPT_LIMIT}s"
}
grep -Fq "group-key secret not found for $OWNER_GROUP_KEY" "$T/clone-denied.err" || {
  cat "$T/clone-denied.err" >&2
  fail "non-member clone did not identify group key $OWNER_GROUP_KEY"
}
if grep -Fq 'ImportBlocksUnavailable' "$T/clone-denied.err"; then
  cat "$T/clone-denied.err" >&2
  fail "missing membership was reported as unavailable blocks"
fi
ok "B failed in ${denied_seconds}s with the exact missing group-key secret"

ROTATED_GROUP_KEY=$(node_env A timeout 30 hbs2-cli \
  "(hbs2:groupkey:store (hbs2:groupkey:update (hbs2:groupkey:load $OWNER_GROUP_KEY) (list (add . $B_ENCRYPTION_KEY))))")
[ -n "$ROTATED_GROUP_KEY" ] || fail "could not rotate the repository group key"
node_env A timeout 60 hbs2-git3 repo:gk:update "$REMOTE" "$ROTATED_GROUP_KEY"

node_env B timeout 120 git clone -q "$URL" "$T/clone-allowed" \
  2>"$T/clone-allowed.err" || {
    cat "$T/clone-allowed.err" >&2
    fail "member clone failed after group-key rotation"
  }

[ "$(node_env B git -C "$T/clone-allowed" log -1 --format=%s)" = "history before membership" ] ||
  fail "clone after rotation has the wrong history"
[ "$(cat "$T/clone-allowed/history.txt")" = "written before membership" ] ||
  fail "clone after rotation has the wrong payload"
ok "adding B through group-key rotation unlocks the complete pre-rotation history"
