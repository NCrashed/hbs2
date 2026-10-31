#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
export LEASE_DISCOVERY_MODE=direct
exec "$SCRIPT_DIR/repository-lease-offline-owner.sh" "$@"
