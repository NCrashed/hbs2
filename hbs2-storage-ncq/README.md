# hbs2-storage-ncq

The current production storage backend for hbs2-peer. Append-only
log-structured CAS with a separate disk-paged index, built on the
primitives in `hbs2-log-structured`.

This README covers the on-disk layout, operational notes, and the
NCQv1 -> NCQ3 migration path. For where storage fits in the wider
system, see [`../ARCHITECTURE.md`](../ARCHITECTURE.md).

## Why NCQ3

The original storage layout used a git-style directory tree
(`root/XX/YYY..YY`) with one file per block. As block counts grew,
filesystem B-trees became the bottleneck for both writes and lookups,
and large directories degraded with use. NCQ3 replaces that with a
journal-plus-index design: blocks are appended to large segment files,
and a separate cuckoo-style hash table on disk maps hash -> (file,
offset).

The journal layout is deliberately uncompressed. Blocks are already
content-addressed and small enough that the sendfile() path beats
zstd-on-read for typical access patterns; compression is left to
application layers (`hbs2-git3` zstd-compresses its segments before
handing them to storage).

## On-disk layout

Two file families live under the storage root (default
`$HOME/.local/share/hbs2`):

- **Journals.** Append-only segment files containing the actual
  block data. Each record is `K | P | D`:
  - `K`: 32-byte block hash (BLAKE2b-256)
  - `P`: 4-byte prefix tag indicating record type. Currently used:
    `B` (block), `R` (reference), `T` (tombstone), `M` (metadata).
  - `D`: payload, length-prefixed.

  On the wire layer of the file: `(S D)*` where `S` is a big-endian
  32-bit size. No special library needed to parse a segment.

- **Indices.** Disk-paged hash tables in N-way pseudo-cuckoo layout
  (see `hbs2-log-structured/HBS2/Data/Log/Structured/NCQ.hs`). Each
  bucket holds a small fixed number of slots; each slot stores
  `KEY | FILE_ID | OFFSET | SIZE | PADDING`. Typical load factor is
  0.4 to 0.67 by design.

The state directory tracks which segments and indices are live;
segments not referenced by any state are eligible for garbage
collection via the merge/sweep process.

## Runtime structure

Several pieces work together:

- **Memtable.** Sharded in-memory buffer (typically 4 shards by
  hash). Writes land here first; flushes go to the active journal
  segment.
- **Write IO queue.** Sharded per-segment. Per-segment ordering
  prevents concurrent writes on the same file; the sharded design
  scales linearly with thread count better than a single queue with
  semaphores.
- **Fsync queue.** Single, separate from writes. Periodic durability
  syncs (every 16 MB of writes by default) are issued without
  blocking the write path.
- **Index cache.** Up to 64 indices and 64 data segments are kept
  mmap-cached for fast lookup; older ones are evicted.

Single-threaded access per file resource is enforced; this is the
simplest way to avoid concurrent-write bugs on segment files.

## Defaults

Set in `HBS2.Storage.NCQ3.Internal.ncqStorageOpen`:

| setting              | default        |
|----------------------|----------------|
| fsync interval       | 16 MB          |
| write queue depth    | 4096           |
| min log size         | 512 MB         |
| max log size         | ~10 GB         |
| cached indices       | 64             |
| cached data segments | 64             |
| sweep interval       | 30 s           |

Override by passing an update function to `ncqStorageOpen` when
opening storage from code. There is no runtime config file for these
yet; they are tuned at compile time for typical workloads.

## Migration from NCQv1

NCQv1 data is not upgraded in place. The `ncq3` executable in this
package is a small script runner that exposes NCQ3 storage primitives
(`ncq:open`, block enumeration, etc.) as bindings; the actual
end-to-end migration is scripted in
[`bf6/ncq-migrate.ss`](../bf6/ncq-migrate.ss).

The short version: open the old NCQv1 store directory and enumerate
blocks, open a fresh NCQ3 store, copy each block (the hash is
recomputed and verified on the way through), then point `hbs2-peer`
at the new directory. The NCQv1 read code is kept in this same
package (`HBS2.Storage.NCQ`) specifically for this flow; it is not
loaded at runtime by `hbs2-peer`.

## Reported performance

Performance figures originally published by voidlizard for the
reference implementation on his hardware:

- Write throughput with hash computation and indexing, 16 MB fsync:
  ~850 MB/s.
- Lookups: ~825K/sec with a single index segment and multiple
  reader threads.

For comparison he cited ~50 MB/s for SQLite on the same workload and
~400 MB/s for raw writes without hashing or indexing. These numbers
have not been re-measured against the v1 codebase; treat them as
historical design targets rather than current benchmarks. If you
need authoritative numbers, the `hbs2-storage-simple-benchmarks`
package (and its NCQ3 counterpart in `tests/`) provide the
measurement harness.

## Code map

- `HBS2.Storage.NCQ3` - public storage interface.
- `HBS2.Storage.NCQ3.Internal` - top-level state, open/close,
  background services.
- `.Internal.Memtable` - sharded in-memory write buffer.
- `.Internal.Index` - cuckoo-style on-disk hash table over the
  log-structured primitives.
- `.Internal.Files` - segment file layout and ID management.
- `.Internal.State` - atomic state snapshots with refcounting,
  used to make GC crash-safe.
- `.Internal.Fossil` / `.Sweep` / `.Fsync` - background services
  that merge segments, run garbage collection, and issue durability
  syncs.
- `HBS2.Storage.NCQ` - legacy NCQv1 read path retained for
  migration.
