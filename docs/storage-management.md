# Storage inventory and cleanup

The peer can account its NCQ block store against application-owned retention
roots. Repository polls, active repository leases, and explicit pins are
reported with their logical, unique, shared, and missing byte counts.

```console
hbs2-peer storage inventory
hbs2-peer storage inventory --json
hbs2-peer storage inventory --explain <block-hash>
```

Inventory is fail-closed. Unknown reference types, incomplete roots, missing
blocks, or writes observed during the scan are shown as coverage problems. In
that state unattributed blocks remain visible, but none are called
reclaimable.

Applications outside the Git repository protocol must register their roots.
The pin walker understands ordinary Merkle, annotated, and sequential HBS2
objects; an opaque block is retained as a one-block root.

```console
hbs2-peer storage pin desktop-files <root-hash> --kind file-sync
hbs2-peer storage pins
hbs2-peer storage unpin desktop-files
```

Pins are durable in the peer's Brains state database. Reusing a name updates
the existing pin.

Cleanup is a dry run unless `--execute` is present:

```console
hbs2-peer storage cleanup
hbs2-peer storage cleanup --json
hbs2-peer storage cleanup --execute
```

Before execution the daemon collects the inventory again under a cleanup
lock. It refuses the operation if the physical store, roots, coverage, or
candidate set changed. Deletion writes idempotent NCQ tombstones. It is
semantic cleanup, not disk compaction: run the NCQ compaction operation
separately when physical disk space must be returned.

