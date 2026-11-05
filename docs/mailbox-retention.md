# Mailbox retention

Mailbox retention has three distinct stages:

1. **Logical expiry/deletion.** A signed message is no longer live once its TTL
   deadline passes, or after the mailbox owner authorizes deletion. It is then
   omitted at admission, listing, replication, and merge boundaries.
2. **Replicated tombstone/grace.** Owner deletion is carried by the authenticated
   `Deleted` entry. The first time a peer verifies deletion (or derives TTL
   expiry) it records the decision durably. The entry and data remain during the
   synchronization grace period so disconnected peers can converge. Re-seeing
   the same entry cannot restart this clock.
3. **Physical purge.** After grace, a canon-aware retention run compacts the
   mailbox tree and deletes message, group-key, and attachment blocks that are
   not shared by a live message and are not reachable from accepted public
   canon. The durable merge index remains, so an old peer cannot resurrect a
   compacted entry.

## Time semantics

`messageCreated` is a Unix timestamp in seconds and `messageTTL` is a duration
in seconds. The expiration deadline is `created + TTL`, using saturating
arithmetic. All peers allow a fixed 300-second clock disagreement: timestamps
farther in the future are refused, and expiry takes effect at
`created + TTL + 300`.

Clock skew is intentionally not locally configurable; different values would
make peers disagree about liveness. Physical purge grace is operational and is
configurable in the peer config:

```lisp
(hbs2:mailbox:purge-grace 604800) ; seconds; default seven days
```

Set it longer than the maximum time a mailbox replica is expected to remain
offline.

## Inspect and purge

From a clone containing the repository's accepted public canon:

```console
hbs2-hub inbox retention --repo REPO_KEY
hbs2-hub inbox retention --repo REPO_KEY --execute
```

The first command is a read-only plan. The second folds canon, derives the
protected attachment roots from `frParts`, and asks the peer to execute the
validated plan for that repository's mailbox only. Targeting matters when one
peer hosts several repositories: one repository's canon is never treated as a
complete protection set for another. Execution refuses if a canon-protected
tree or mailbox tree is incomplete. The report includes the skew and grace, tracked and eligible
records, protected roots, candidate/deleted block counts, and deletion failures.

The lower-level peer command is read-only and useful for diagnostics:

```console
hbs2-peer mailbox retention
```

It deliberately has no execution switch: the peer cannot determine which
repository canon is authoritative. The hub command is the only CLI execution
path.
