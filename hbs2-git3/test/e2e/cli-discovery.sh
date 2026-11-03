#!/usr/bin/env bash
set -euo pipefail

: "${HBS2_PEER:?set HBS2_PEER to the built hbs2-peer executable}"
: "${HBS2_GIT3:?set HBS2_GIT3 to the built hbs2-git3 executable}"

T=$(mktemp -d "${TMPDIR:-/tmp}/hbs2-cli-discovery.XXXXXX")
trap 'rm -rf "$T"' EXIT

export HOME="$T/home"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
cd "$T"

$HBS2_PEER --help >/dev/null
$HBS2_GIT3 --help >/dev/null

peer_version=$($HBS2_PEER --version)
git3_version=$($HBS2_GIT3 --version)
[ "$peer_version" = "$git3_version" ]
[[ "$peer_version" =~ ^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$ ]]

update_help=$($HBS2_GIT3 repo:gk:update --help)
grep -q 'repo:gk:update REPO GROUP_KEY_HASH' <<<"$update_help"
grep -q 'GroupKey object' <<<"$update_help"
grep -q 'not a member' <<<"$update_help"

extra_help=$($HBS2_GIT3 repo:gk:add:extra:keys --help)
grep -q 'GROUP_KEY_HASH' <<<"$extra_help"
grep -q 'not a member' <<<"$extra_help"

set +e
malformed=$($HBS2_GIT3 repo:gk:update only-a-repo 2>&1)
malformed_rc=$?
set -e
[ "$malformed_rc" -ne 0 ]
grep -q 'expected (repo:gk:update REPO GROUP_KEY_HASH)' <<<"$malformed"
! grep -q 'BadFormException' <<<"$malformed"

[ ! -e "$XDG_CONFIG_HOME/hbs2-peer" ]
[ ! -e "$HOME/.hbs2-git3" ]
[ ! -e "$T/.hbs2-git3" ]

echo "cli discovery: OK ($peer_version)"
