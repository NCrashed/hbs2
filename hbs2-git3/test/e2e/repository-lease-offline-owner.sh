#!/usr/bin/env bash
# A complete repository remains cloneable from its lease provider after its
# owner goes offline. LEASE_DISCOVERY_MODE=direct covers the provider-local
# M1 contract; LEASE_DISCOVERY_MODE=relay covers repository-key-only M4
# discovery through a separate advert relay.
#
# Node A publishes a repository. Node B accepts a repository lease and must
# reach Active with a verifiable receipt. A is then stopped. Only after that
# do we start a clean node C and clone the repository. In direct mode C's sole
# known peer is B. In relay mode C's sole configured peer is R; B advertises
# its signed replica through R and C discovers B using only the repository key.
#
# Usage, after `cabal build all`:
#   BIN=$(for b in hbs2-peer hbs2-cli hbs2-keyman hbs2-git3:exe:hbs2-git3 git-remote-hbs23 hbs2-hub; do
#          dirname "$(cabal list-bin "$b")"
#        done | paste -sd:) \
#     hbs2-git3/test/e2e/repository-lease-three-node.sh
#
# For repository-key-only discovery use repository-discovery-four-node.sh.
#
# KEEP=1 preserves the temporary directory and peer logs on failure.

set -euo pipefail

: "${BIN:?set BIN to a PATH fragment holding hbs2-peer, hbs2-cli, hbs2-keyman, hbs2-git3 and git-remote-hbs23}"

PORT_BASE=${PORT_BASE:-27351}
A_UDP_PORT=${A_UDP_PORT:-$PORT_BASE}
B_UDP_PORT=${B_UDP_PORT:-$((PORT_BASE + 1))}
C_UDP_PORT=${C_UDP_PORT:-$((PORT_BASE + 2))}
R_UDP_PORT=${R_UDP_PORT:-$((PORT_BASE + 3))}
A_TCP_PORT=${A_TCP_PORT:-$((PORT_BASE + 10))}
B_TCP_PORT=${B_TCP_PORT:-$((PORT_BASE + 11))}
C_TCP_PORT=${C_TCP_PORT:-$((PORT_BASE + 12))}
R_TCP_PORT=${R_TCP_PORT:-$((PORT_BASE + 13))}
WAIT=${HBS2_GIT_REPO_WAIT:-12}
LEASE_WAIT=${LEASE_WAIT:-300}
CLONE_TIMEOUT=${CLONE_TIMEOUT:-120}
LEASE_DISCOVERY_MODE=${LEASE_DISCOVERY_MODE:-direct}
PORT_LOCK_WAIT=${PORT_LOCK_WAIT:-300}

case "$LEASE_DISCOVERY_MODE" in
  direct|relay) ;;
  *) echo "LEASE_DISCOVERY_MODE must be direct or relay" >&2; exit 2 ;;
esac

command -v flock >/dev/null 2>&1 || {
  echo "flock is required to isolate the repository lease e2e ports" >&2
  exit 2
}
# Keep the lock independent of a caller-specific TMPDIR (for example, two
# concurrent nix-shell instances) because the guarded loopback ports are global.
PORT_LOCK_FILE="/tmp/hbs2-git3-lease-e2e.$PORT_BASE.lock"
exec {PORT_LOCK_FD}>"$PORT_LOCK_FILE"
flock -w "$PORT_LOCK_WAIT" "$PORT_LOCK_FD" || {
  echo "timed out waiting for repository lease e2e ports at base $PORT_BASE" >&2
  exit 1
}

T=$(mktemp -d "${TMPDIR:-/tmp}/hbs2-git3-lease-e2e.XXXXXX")

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
    # `hbs2-peer run` supervises a `start` child.  Both live in the dedicated
    # session created below, so stop the whole group and never leak a listener
    # into the next e2e invocation.
    kill -- "-$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
    unset 'PEER_PID[$node]'
  fi
}

cleanup() {
  stop_peer A
  stop_peer B
  stop_peer C
  stop_peer R
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
  local public_tcp_port=$4
  shift 4
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
    if [ -n "$public_tcp_port" ]; then
      echo "peer-public-address \"tcp://127.0.0.1:$public_tcp_port\""
    fi
    for known_address in "$@"; do
      if [[ "$known_address" == *://* ]]; then
        echo "known-peer \"$known_address\""
      else
        echo "known-peer \"127.0.0.1:$known_address\""
      fi
    done
  } > "$config_dir/config"

  node_env "$node" hbs2-keyman update >/dev/null 2>&1 ||
    fail "node $node: hbs2-keyman update"
  node_env "$node" git config --global user.name e2e
  node_env "$node" git config --global user.email e2e@localhost
  node_env "$node" git config --global commit.gpgsign false
  node_env "$node" git config --global tag.gpgsign false
  node_env "$node" git config --global init.defaultBranch master
}

wait_for_authenticated_peer() {
  local node=$1
  local expected_node=$2
  local expected_address=$3
  local expected_port=${expected_address##*:}
  local ping_address=$expected_address
  local node_address=
  local expected_key=
  local deadline=$((SECONDS + 30))
  local peers=

  if [[ "$ping_address" != *://* ]]; then
    ping_address="127.0.0.1:$ping_address"
  fi
  case "$node" in
    A) node_address="127.0.0.1:$A_UDP_PORT" ;;
    B) node_address="127.0.0.1:$B_UDP_PORT" ;;
    C) node_address="127.0.0.1:$C_UDP_PORT" ;;
    R) node_address="127.0.0.1:$R_UDP_PORT" ;;
    *) fail "unknown peer node $node" ;;
  esac
  # Logger colour resets may directly follow the key, so extract only the
  # Base58 alphabet instead of treating every non-space byte as key material.
  expected_key=$(sed -n 's/.*run peer \([1-9A-HJ-NP-Za-km-z]*\).*/\1/p' "$T/$expected_node/peer.log" | head -1)
  [ -n "$expected_key" ] || fail "node $expected_node did not report its peer key"

  while [ "$SECONDS" -lt "$deadline" ]; do
    peers=$(node_env "$node" hbs2-peer peers 2>/dev/null || true)
    if awk -v key="$expected_key" '$1 == key { found = 1 } END { exit !found }' <<<"$peers"; then
      return 0
    fi
    # RPC readiness precedes the first successful handshake. A signed ping
    # makes its initiator visible to the receiver, so drive both directions:
    # the current node may know a TCP endpoint while its counterpart can still
    # reach the current node's local UDP listener.
    node_env "$node" timeout 2 hbs2-peer ping "$ping_address" >/dev/null 2>&1 || true
    node_env "$expected_node" timeout 2 hbs2-peer ping "$node_address" >/dev/null 2>&1 || true
    kill -0 "${PEER_PID[$node]}" 2>/dev/null || fail "node $node exited before authenticating peer $expected_node"
    sleep 0.2
  done

  printf '%s\n' "$peers" >&2
  printf 'expected peer key: %s\n' "$expected_key" >&2
  tail -60 "$T/$node/peer.log" >&2 || true
  fail "node $node did not authenticate peer $expected_node at :$expected_port"
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
      # The RPC socket appears before UDP protocol workers have reliably
      # entered their receive loops. Give those workers the same two-second
      # warm-up used by bootstrap before testing signed peer handshakes.
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

wait_for_lease_rpc() {
  local deadline=$((SECONDS + 30))
  while [ "$SECONDS" -lt "$deadline" ]; do
    if node_env B hbs2-git3 repo:lease:capacity >/dev/null 2>&1; then
      return 0
    fi
    kill -0 "${PEER_PID[B]}" 2>/dev/null || fail "node B exited before lease RPC became ready"
    sleep 0.2
  done
  tail -60 "$T/B/peer.log" >&2 || true
  fail "node B: repository lease RPC did not become ready"
}

wait_for_active_lease() {
  local repository=$1
  local deadline=$((SECONDS + LEASE_WAIT))
  local status=

  while [ "$SECONDS" -lt "$deadline" ]; do
    status=$(node_env B hbs2-git3 repo:lease:show "$repository" 2>/dev/null || true)
    if grep -q '(state active)' <<<"$status"; then
      printf '%s\n' "$status" > "$T/B/lease-active.out"
      return 0
    fi
    kill -0 "${PEER_PID[B]}" 2>/dev/null || {
      tail -60 "$T/B/peer.log" >&2 || true
      fail "node B exited while synchronising the lease"
    }
    sleep 1
  done

  printf '%s\n' "$status" >&2
  tail -80 "$T/B/peer.log" >&2 || true
  fail "node B did not reach an active lease within ${LEASE_WAIT}s"
}

assert_active_lease() {
  local repository=$1
  local deadline=$((SECONDS + 30))
  local status=

  while [ "$SECONDS" -lt "$deadline" ]; do
    status=$(node_env B hbs2-git3 repo:lease:show "$repository" 2>/dev/null || true)
    if grep -q '(state active)' <<<"$status"; then
      return 0
    fi
    kill -0 "${PEER_PID[B]}" 2>/dev/null || fail "provider B exited during clone"
    sleep 0.2
  done

  printf '%s\n' "$status" >&2
  fail "B lease is no longer active"
}

add_repository_lease() {
  local repository=$1
  local deadline=$((SECONDS + 30))

  # Publishing the branch and tag can briefly occupy the peer RPC worker.
  # Lease creation is idempotent by repository (a partial first call is
  # renewed), so retry the complete CLI transaction within a fixed bound.
  while [ "$SECONDS" -lt "$deadline" ]; do
    if node_env B hbs2-git3 repo:lease:add \
        --max-bytes 104857600 \
        --max-blocks 100000 \
        --max-update-lag 30 \
        "$repository" >"$T/B/lease-add.out" 2>"$T/B/lease-add.err"; then
      return 0
    fi
    kill -0 "${PEER_PID[B]}" 2>/dev/null || fail "node B exited while adding the lease"
    sleep 0.2
  done

  cat "$T/B/lease-add.out" "$T/B/lease-add.err" >&2
  fail "node B: repo:lease:add"
}

wait_for_remote_ref() {
  local node=$1
  local url=$2
  local ref=$3
  local deadline=$((SECONDS + 120))
  local refs=

  while [ "$SECONDS" -lt "$deadline" ]; do
    refs=$(node_env "$node" timeout 15 git ls-remote "$url" "$ref" \
      2>"$T/$node/remote-ref.err" || true)
    if grep -q "[[:space:]]$ref$" <<<"$refs"; then
      return 0
    fi
    sleep 1
  done

  printf '%s\n' "$refs" >&2
  cat "$T/$node/remote-ref.err" >&2 || true
  fail "node $node did not observe $ref before the lease was requested"
}

# Every config is written up front, but C remains entirely offline until A has
# stopped. In relay mode R is a separate cache/rendezvous node: B publishes to
# R, while C has no configured address for B.
if [ "$LEASE_DISCOVERY_MODE" = relay ]; then
  write_peer_config A "$A_UDP_PORT" "$A_TCP_PORT" "$A_TCP_PORT"
  write_peer_config B "$B_UDP_PORT" "$B_TCP_PORT" "$B_TCP_PORT" \
    "tcp://127.0.0.1:$A_TCP_PORT" "tcp://127.0.0.1:$R_TCP_PORT"
  write_peer_config R "$R_UDP_PORT" "$R_TCP_PORT" ""
  write_peer_config C "$C_UDP_PORT" "$C_TCP_PORT" "" "tcp://127.0.0.1:$R_TCP_PORT"
else
  write_peer_config A "$A_UDP_PORT" "$A_TCP_PORT" "$A_TCP_PORT"
  write_peer_config B "$B_UDP_PORT" "$B_TCP_PORT" "$B_TCP_PORT" \
    "tcp://127.0.0.1:$A_TCP_PORT"
  write_peer_config C "$C_UDP_PORT" "$C_TCP_PORT" "" \
    "tcp://127.0.0.1:$B_TCP_PORT"
fi

if [ "$LEASE_DISCOVERY_MODE" = relay ]; then
  # R and A listen first; B is the sole initiator for both long-lived links.
  # This avoids duplicate TCP flows while keeping R an advert-only rendezvous.
  start_peer R
  start_peer A
  start_peer B
  wait_for_authenticated_peer B R "tcp://127.0.0.1:$R_TCP_PORT"
  wait_for_authenticated_peer B A "tcp://127.0.0.1:$A_TCP_PORT"
  wait_for_authenticated_peer A B "tcp://127.0.0.1:$B_TCP_PORT"
else
  start_peer A
  start_peer B
  wait_for_authenticated_peer A B "tcp://127.0.0.1:$B_TCP_PORT"
  wait_for_authenticated_peer B A "tcp://127.0.0.1:$A_TCP_PORT"
fi
wait_for_lease_rpc
if [ "$LEASE_DISCOVERY_MODE" = relay ]; then
  ok "owner A, provider B, and advert relay R are running"
else
  ok "owner A and provider B are running"
fi

# Publish a repository with both a branch and a tag on A.
mkdir "$T/work"
cd "$T/work"
node_env A git init -q
echo one > payload.txt
node_env A git add payload.txt
node_env A git commit -qm one
node_env A git tag v1
node_env A hbs2-git3 repo:init --new >"$T/A/init.out" 2>"$T/A/init.err" || {
  cat "$T/A/init.out" "$T/A/init.err" >&2
  fail "node A: repo:init --new"
}
REMOTE=$(node_env A git remote | head -1)
URL=$(node_env A git remote get-url "$REMOTE")
KEY=${URL#hbs23://}
[ -n "$KEY" ] && [ "$KEY" != "$URL" ] || fail "node A: no hbs23 remote after init: $URL"

node_env A git push -q "$REMOTE" --all 2>"$T/A/push.err" || {
  cat "$T/A/push.err" >&2
  fail "node A: branch push"
}
# Mutable-root ordering has one-second timestamp granularity. Ensure the tag
# snapshot is causally newer instead of relying on hash tie-breaking when two
# supported remote-helper pushes finish within the same second.
branch_push_second=$(date +%s)
while [ "$(date +%s)" -le "$branch_push_second" ]; do
  sleep 0.1
done
node_env A git push -q "$REMOTE" --tags 2>>"$T/A/push.err" || {
  cat "$T/A/push.err" >&2
  fail "node A: tag push"
}
# The two supported remote-helper updates produce two repository snapshots.
# Do not let B lease and advertise the first one merely because it completed
# before the tag update reached B's peer.
wait_for_remote_ref B "$URL" refs/tags/v1
ok "A published $KEY with a branch and tag"

# A lease is not useful while it is merely Syncing. Wait for the complete
# inventory, durable pin, and provider-signed receipt on B.
add_repository_lease "$KEY"
wait_for_active_lease "$KEY"
node_env B hbs2-git3 repo:lease:receipt "$KEY" >"$T/B/receipt.out" 2>"$T/B/receipt.err" || {
  cat "$T/B/receipt.out" "$T/B/receipt.err" >&2
  fail "node B: active lease has no valid receipt"
}
grep -q '(replica-receipt-v1 ' "$T/B/receipt.out" || fail "node B: malformed receipt output"
ok "B holds a complete, pinned, receipted replica"

# Eliminate both direct retrieval and accidental prefetch: A is dead before
# C's peer process has ever run.
stop_peer A
if node_env A hbs2-peer poke >/dev/null 2>&1; then
  fail "node A still answers after shutdown"
fi
start_peer C
if [ "$LEASE_DISCOVERY_MODE" = relay ]; then
  wait_for_authenticated_peer C R "tcp://127.0.0.1:$R_TCP_PORT"
  if grep -q ":$B_UDP_PORT\|:$B_TCP_PORT" "$T/C/home/.config/hbs2-peer/config"; then
    fail "node C configuration leaks provider B's address"
  fi
  ok "A is offline and clean node C is configured with only relay R"
else
  ok "A is offline and clean node C knows only B"
fi

node_env C timeout "$CLONE_TIMEOUT" git clone -q "$URL" "$T/clone" 2>"$T/C/clone.err" || {
  cat "$T/C/clone.err" >&2
  tail -n 80 -- "$T/B/peer.log" "$T/C/peer.log" >&2 || true
  fail "node C could not clone from B while A was offline"
}

if [ "$LEASE_DISCOVERY_MODE" = relay ]; then
  grep -q 'repository provider' "$T/C/clone.err" || {
    cat "$T/C/clone.err" >&2
    fail "node C cloned without reporting a selected replica advert provider"
  }
fi

[ "$(node_env C git -C "$T/clone" log -1 --format=%s)" = one ] || fail "clone has the wrong head"
[ "$(cat "$T/clone/payload.txt")" = one ] || fail "clone has the wrong payload"
node_env C git -C "$T/clone" rev-parse -q --verify refs/tags/v1 >/dev/null || fail "clone has no v1 tag"
kill -0 "${PEER_PID[B]}" 2>/dev/null || fail "provider B exited during clone"
assert_active_lease "$KEY"
if [ "$LEASE_DISCOVERY_MODE" = relay ]; then
  ok "C discovered B through R and cloned the complete repository while A was offline"
else
  ok "C cloned the complete repository from B while A was offline"
fi

if [ "$LEASE_DISCOVERY_MODE" = relay ]; then
  # The forge-facing status contract must expose the same live discovery fact;
  # this is the seam consumed by hbs2-hub-browser.  It reports a lower bound,
  # because the bounded RPC selects one authenticated provider rather than
  # enumerating the whole network. Direct M1 intentionally has no advert: C
  # knows B from configuration and therefore has no discovery fact to expose.
  (
    cd "$T/clone"
    node_env C timeout 60 hbs2-hub status --repo "$KEY" --json
  ) >"$T/C/hub-status.json" 2>"$T/C/hub-status.err" || {
    cat "$T/C/hub-status.json" "$T/C/hub-status.err" >&2
    fail "node C: hbs2-hub status could not inspect replica discovery"
  }
  grep -Eq '"complete_replicas_minimum"[[:space:]]*:[[:space:]]*1' "$T/C/hub-status.json" || {
    cat "$T/C/hub-status.json" >&2
    fail "node C: hub status did not report the proved replica floor"
  }
  grep -Eq '"at_risk"[[:space:]]*:[[:space:]]*false' "$T/C/hub-status.json" || {
    cat "$T/C/hub-status.json" >&2
    fail "node C: hub status marked a reachable verified replica at risk"
  }
  ok "hub status exposes the reachable verified replica while A is offline"
fi

echo "PASS: $PASS checks"
