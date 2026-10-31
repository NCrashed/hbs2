#!/usr/bin/env bash
# External M4 acceptance: A owns, B leases, R relays the signed advert, and a
# clean C discovers B by repository key after A has gone offline.

set -euo pipefail

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
export LEASE_DISCOVERY_MODE=relay
exec "$SCRIPT_DIR/repository-lease-offline-owner.sh" "$@"
