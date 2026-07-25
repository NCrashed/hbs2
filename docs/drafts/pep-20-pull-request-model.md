PEP-20: pull-request model

Status: draft, started 2026-07-25.
Author: NCrashed (Anton Gushcha)
Part of: PEP-17 (hbs2-hub umbrella).
Depends on: hbs2-git3 (reflog export/import, remote helper), PEP-18
            (letter, attachments), PEP-19 (canon, pulls ref, merge event).
Related: PEP-21 (triage / moderation), PEP-22 (hub CLI).

This sub-proposal specifies how a contributor proposes changes and how a
maintainer reviews and merges them: what a fork is, how the proposed diff is
transferred, the fetch/verify/merge flow, how status is recorded in canon,
and how the contributor is notified. It corrects an efficiency assumption the
umbrella and the earlier sub-proposals carried, so it opens with that.


The efficiency reality (read this first)
======================================

PEP-17 and the first drafts of PEP-18/PEP-19 assumed that because git objects
are content-addressed and shared in storage, fetching a contributor's fork is
cheap and "only the delta transfers". A close read of hbs2-git3 shows that is
true only for incremental pushes to the same reflog, and false for fetching a
different repo key (a real fork):

  - A fork is a distinct repo key, hence a distinct derived reflog (the
    reflog key is derived from the repo key plus a random per-repo `seed`).
  - Block storage is global and content-addressed across repos (one peer
    store), so dedup is real, but it is exact-block-hash dedup only.
  - Pushing to a fresh fork reflog re-exports the whole history: `export`
    seeds its "already have" set only from that reflog's own object index,
    which is empty for a new fork, so every object is re-emitted. The objects
    are streamed by concurrent workers and zstd-compressed with size-bounded
    cutoffs, so segment bytes are not canonical: the same commits produce
    different segment blocks with different hashes.
  - `r:fetch` in the remote helper is a no-op; import is whole-reflog
    (`importGitRefLog` rebuilds a packfile from every segment of the newest
    checkpoint). There is no fetch-one-ref and no delta/bundle/partial-object
    path.

Net: fetching a fork repo key re-downloads the fork's entire history, because
its segment blocks do not match the base's block hashes. The delta-cheapness
the design wants exists only when pushing or fetching the same reflog.

This drives the PR model: the cheap, stranger-friendly path is to ship the
diff as a delta artifact carried in the letter (its size is the delta, no
git3 change needed), not to fetch a foreign fork.


What a fork is
============

A fork is not a special object. A contributor clones the repo, commits to a
branch, and pushes to their own repo key (their own LWWRef and derived
reflog), because they cannot write to the owner's reflog. That gives them a
persistent, addressable `hbs23://<fork-key>` others can pull. The cost is the
one above: pulling that key re-imports the fork's whole reflog today. A fork
is therefore worth it when the contributor wants a durable public fork or
long-lived collaboration, not for a one-shot change, for which the delta
artifact is strictly cheaper.


Two submission paths
==================

Delta artifact (default). The contributor produces a git bundle of the
proposed range and ships it as a PEP-18 attachment (`bundle-part`):

```
git bundle create pr.bundle <base>..<source-ref>
```

The range uses `source-ref` (a ref name), not a raw `source-tip` sha: `git
bundle` refuses to build a bundle that records no ref, and a bare sha is not
a ref, so `<base>..<source-tip>` fails with "Refusing to create empty
bundle". The recorded ref's tip is what the maintainer checks against the
signed `source-tip` (see Verify). The bundle contains exactly the objects
reachable from `source-ref` but not from `base`, so its size is the delta. It
is carried as a Mailbox part (an encrypted merkle tree), transferring only
the delta over hbs2, with no dependence on fork-fetch dedup. A bundle
preserves exact commit shas, merge structure, and author identities, so the
merged history matches what the contributor signed for, and it keeps the
`source-tip == signed value` verification below meaningful. (A `git
format-patch`/`am` series is deliberately not used; it rewrites shas, which
would make that verification impossible. See Rejected alternatives.)

Fork pointer (for durable forks). The letter instead names the fork by repo
key and ref (`source`, `source-ref`, `source-tip`, `onto`, `base`), and the
maintainer fetches it over hbs2. Use this when the contributor wants a
persistent fork or expects long iteration; accept that fetching re-imports
the fork's full reflog until git3 gains cheaper cross-key transfer (see What
must be built). Both paths carry the same `source-tip`/`base` coordinates, so
verification is identical.


The PR letter and its canon event
===============================

A PR is a PEP-18 letter with `kind pr`, folding into a PEP-19 `open` event
whose `kind` is `pr`. The letter carries the coordinates and, on the default
path, the bundle:

```
(kind pr) (op open)
(target     <repo-lwwref-b58>)
(title      "...")
(source     hbs23://<fork-repo-key>)  ; present on the fork-pointer path
(source-ref refs/heads/feature)
(source-tip <git-sha1>)               ; the commit being proposed (inside the signed box)
(onto       refs/heads/master)
(base       <git-sha1>)               ; merge-base the branch forked from
(bundle-part <hashref>)                ; present on the delta-artifact path: the bundle
```

`source-tip` and `base` live inside the signed inner box (PEP-18), so the
proposed commit and fork point are authenticated to the contributor. On the
delta path `source` may be omitted; on the fork path `bundle-part` may be
omitted. The event is public canon once folded, so the diff becomes visible;
the bundle attachment is decryptable by any clone via the `part-secret` the
owner publishes at fold (PEP-19 Attachments in public canon).


Fetch and verify
==============

Order of operations. Verification runs on the private Tier B letter before
anything becomes public: the maintainer fetches and checks the bundle while
the submission is still in the mailbox. Only on acceptance does the maintainer
fold the `open` event, which assigns the issue `number` and makes the PR
public canon. Staging `refs/hbs2/pulls/<number>/head` needs that `number`, so
it necessarily follows the fold; public review then happens against the staged
tip. So the sequence is: verify privately, fold (assign number, publish),
stage, review.

The maintainer, who already holds the base history, obtains the proposed
commits and checks them against the signed claim:

  1. Get objects. The bundle bytes come from a stranger, so fetch with fsck
     enabled to reject malformed or malicious objects:
     - Delta path: fetch and decrypt `bundle-part`, then `git bundle verify
       pr.bundle` and
       ```
       git -c transfer.fsckObjects=true -c fetch.fsckObjects=true \
           fetch pr.bundle <source-ref>
       ```
       (fsck defaults off, so it must be set explicitly). This requires
       `base` present locally; if the contributor forked from a commit the
       maintainer does not have, they must ship a self-contained bundle (no
       `..base`), larger but complete.
     - Fork path: `git fetch hbs23://<fork-key> <source-ref>` (whole-reflog
       import, per the efficiency note).
  2. Verify. Confirm the fetched tip equals the signed `source-tip`, and that
     the signed `base` is an ancestor of `source-tip`. On the delta path the
     bundle construction (`base..source-ref`) already guarantees the ancestor
     relation; on the fork path nothing does, so check it explicitly
     (`git merge-base --is-ancestor <base> <source-tip>`). git's own object
     hashing binds the content to `source-tip`, so a tampered bundle cannot
     masquerade as the signed tip.
  3. Stage. Set the proposed tip under the PEP-19 ref so it is reviewable and
     survives the contributor going away:
     ```
     refs/hbs2/pulls/<number>/head -> <source-tip>
     ```
     Publishing this ref exports `source-tip` into the maintainer's own reflog,
     which is a same-reflog incremental push: the maintainer already has
     `base`, so only the delta objects are written. So staging is cheap even
     though fetching a foreign fork is not.


Review
====

Review is ordinary git against the staged tip: `git range-diff base
source-tip`, `git diff onto...source-tip`, checkout, build, test. Discussion
happens as PEP-19 `comment` events on the PR thread (from either side, via
letters or owner-native), so the review conversation is canon and travels
with the repo.


Merge
====

On acceptance the maintainer integrates and records the result:

  1. Integrate onto the target branch by whatever policy the repo uses
     (merge, rebase, squash, fast-forward) against `onto`.
  2. Push the updated `onto` branch to the owner's repo. This is a
     same-reflog incremental push, so only the new objects transfer.
  3. Emit a PEP-19 `merge` event on the PR thread, owner-signed, recording
     the outcome:
     ```
     (op merge) (merge-commit <sha1>) (merged-into refs/heads/master)
     ```
  4. Set status to merged with an owner-signed `set`:
     ```
     (op set) (set status merged)
     ```
     (`merge` records the git result; `set status merged` records the tracker
     state. A repo may treat merged as a terminal closed sub-state.)

The `pulls/<n>/head` ref may be retained for the record or dropped per the
retention policy. The proposed commits, once merged, live in the normal
reflog like any other history.


Rejection and revision
====================

Rejection is an owner-signed `set status closed` (optionally with a
`close` note as the event body). No git objects change. The `pulls/<n>/head`
ref may be dropped.

Revision is a `revise` letter on the same thread (PEP-18/PEP-19 op `revise`,
not `comment`): it carries updated coordinates (a new `bundle-part` and
`source-tip`, or an updated fork ref), referencing the PR thread by its
canonical id (PEP-18 threading). A `comment` alone would not do, because the
PEP-19 fold updates a thread's proposed coordinates only from `revise` events;
a `comment` just appends discussion and the deterministic fold would never see
the new tip. `revise` is author-of-record only, so no one but the PR's author
can redirect it (a canon key may also, e.g. to point at a mirror). The
maintainer re-fetches, re-verifies, and re-stages `pulls/<n>/head` to the new
tip. Because each `revise` is an independent signed event ordered by `seq`,
the review history is preserved and the current proposed tip is unambiguously
the latest surviving `revise` (or the `open` coordinates if none).


Contributor notification
======================

Status changes are sent back to the contributor's `reply-mailbox` (PEP-18),
when present: acceptance with the assigned number, merge result, or rejection
with the note. Notification is best-effort and never authoritative: the
contributor can also read the outcome from public canon, since all of it
(merge event, status) is in Tier A. A drive-by contributor who supplied no
`reply-mailbox` simply reads canon.


What exists today vs what must be built
====================================

Exists today:

  - Same-reflog incremental push is delta-cheap (the maintainer's staging and
    merge pushes), via the per-reflog object index and block-hash dedup.
  - Global content-addressed block storage shared across repo keys.
  - The remote helper and reflog import for fork-pointer fetches (whole
    reflog).
  - PEP-18 attachments (encrypted parts) to carry a bundle; PEP-19 `pulls`
    ref, `merge` event, and `part-secret` for public decryption.

Must be built:

  - The delta-artifact path end to end: `hub pr new` builds the bundle and
    attaches it; triage fetches, `git bundle verify`, unbundles, verifies
    against `source-tip`/`base`, stages `pulls/<n>/head`.
  - The merge/record step: emit the `merge` and `set status` events and push.
  - Contributor-side `hub pr` and maintainer-side triage (shared with PEP-22).

Deferred git3 work (to make the fork-pointer path delta-cheap, not required
for the default path):

  - Deterministic export so identical history yields identical segment blocks
    and cross-fork block dedup actually fires. This needs all three together,
    or the hashes still diverge: canonical object ordering, content-defined
    chunk boundaries, and per-frame (not streaming) compression so a chunk
    compresses to the same bytes regardless of what preceded it. Alternatives:
    an object-level (not segment-level) transfer/dedup path, and/or a
    fetch-one-ref path in the remote helper. Any of these is a git3-level
    change tracked outside this PEP.


Rejected alternatives
===================

Fork-fetch as the default. Rejected on the efficiency finding above: fetching
a foreign fork re-imports its whole reflog today, so it is the wrong default
for one-shot PRs. It remains available for durable forks.

Patch series as a diff format at all. `git format-patch`/`am` is simple but
`am` rewrites shas and flattens merges, so the applied commits differ from
the signed `source-tip` and the "fetched tip == source-tip" verification (the
heart of Fetch and verify) becomes impossible. Rejected entirely, not just as
a default: supporting it would add a second interop format forever and one
that cannot be verified against the signed claim. A single-commit change is
just as simple as a bundle (`hub pr new` builds it either way).

Shared contributor reflog. Letting contributors push to one shared reflog
would make transfer delta-cheap but requires giving strangers write access to
a shared key, contradicting the PEP-17 two-tier trust model. Rejected.

Requiring git3 changes before shipping PRs. The delta-artifact path needs no
git3 change, so PRs can ship now; the deterministic-segmentation work is a
later optimization for the fork path, not a blocker.


Open questions
============

- Bundle base availability: policy when `base` is not present at the
  maintainer (force self-contained bundle vs negotiate a common ancestor).
- Whether `set status merged` is a distinct status or a closed sub-state, and
  how the renderer displays merged vs closed.
- Large-PR ergonomics on the delta path: a bundle above the inline size still
  fits as a `bundle-part`, but very large ranges may argue for the fork path
  despite its cost; guidance to be settled with PEP-22.
- Whether to verify a fork-path fetch incrementally (checking `source-tip`
  before importing the whole reflog) to fail fast.
- Interaction with PEP-19 compaction: how long `pulls/<n>/head` refs and their
  objects are retained after merge or rejection.
