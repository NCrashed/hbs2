PEP-19: canonical in-repo metadata (Tier A)

Status: draft, started 2026-07-25.
Author: NCrashed (Anton Gushcha)
Part of: PEP-17 (hbs2-hub umbrella).
Depends on: hbs2-git3 reflog/LWWRef git model, suckless-conf S-expressions,
            SignedBox / sigil identities.
Related: PEP-18 (collaboration mailbox / letter), PEP-20 (pull-request
         model), PEP-21 (triage / moderation / delegation),
         PEP-22 (hub CLI and web rendering contract).

This sub-proposal specifies Tier A from PEP-17: the canonical, owner-blessed
project state (issues, pull-request metadata, comments, labels, status) that
lives inside the repository itself, so a clone carries the full tracker and
can be browsed offline with no extra infrastructure.

PEP-18 defines the ingress letter (Tier B, from anyone). PEP-19 defines what
an accepted submission becomes once the owner folds it into canon, how that
canon is stored, and how any clone recomputes an identical view from it. The
fold/triage bridge between the two is specified here because it is what
produces canon; the triage UX and moderation policy are PEP-21/PEP-22.


Design constraints (from PEP-17)
================================

1. Store everything in the repository. A plain fetch of the repo must carry
   the entire tracker, versioned like the code, browsable offline.

2. Deterministic materialization. Any clone must recompute the same view
   from the same canonical data. No node-local state may leak into the
   canonical result.

3. Owner-authoritative. Tier A contains only what the owner (or a delegated
   maintainer) has blessed. Authorship of the underlying content, however,
   must stay attributable to whoever actually wrote it, including strangers.

4. Cheap. The tracker must not bloat the repo. It rides the same
   content-addressed, deduplicated storage the git objects already use.


Where canon lives
=================

Canon is an orphan git commit chain published under the ref

```
refs/hbs2/meta
```

Its commits are ordinary git commits (parent-linked, forming their own
lineage with no connection to any code branch). Each commit's tree holds the
event files described below. The ref is pushed and fetched through the exact
mechanism the code branches already use.

Why an orphan commit chain, grounded in the current git3 model:

  - hbs2-git3 stores refs as free-text `R` sections in the segment log and
    advertises every non-null ref it finds (`importedRefs`, `r:list`). Ref
    names are not whitelisted, so `refs/hbs2/meta` is a first-class ref end
    to end with no code change.

  - `export` walks the commit graph from the pushed tip and emits the full
    tree closure. An orphan commit (unreachable from any branch) is fully
    supported today: `git push hbs23://<key> <sha>:refs/hbs2/meta` stores
    and later advertises it. (A ref pointing directly at a tree or blob is
    deliberately not used here, see Rejected alternatives.)

  - git objects are content-addressed and deduplicated at the block level,
    and git3 keeps an object index so an object already present in any prior
    segment is not re-emitted. A tracker made of small text files that
    change incrementally therefore costs almost nothing per update.

  - It is git-native: the tracker is versioned, diffable, and mergeable with
    ordinary git tooling, and a clone that fetches the ref has the whole
    history with no side channel.

The existing group-key journal (the second element of the LWWRef payload) is
the precedent for extra repo-level metadata, but it is meant for small fixed
data. Issue and PR history is large, append-mostly, and naturally
tree-shaped, so a commit chain is the right tool, not the LWWRef payload.

Fetching the meta ref. A bare `git clone` of an hbs23 remote pulls the code
branches; it does not create a local `refs/hbs2/meta` unless asked, because
git's default refspec covers only `refs/heads/*` and tags. The hub tooling
therefore fetches canon explicitly, with a forcing `+` refspec (canon can be
history-rewritten by compaction, see below):

```
git fetch <remote> '+refs/hbs2/meta:refs/hbs2/meta'
```

`hub clone` / `hub sync` wrap this. Making a plain `git clone` include the
tracker automatically is an implementation option (a fetch-refspec injected
by the remote helper) tracked in Open questions; the data model does not
depend on it.


The event model
==============

An issue or PR thread is not a mutable record. It is an event-sourced
entity: its current state is the deterministic fold of an append-only
sequence of signed events. This is the model proven in fixme-new (source at
`/home/user/dev/hbs2-legacy/fixme-new`, not in this repo), where a `Fixme`
is the monoidal fold of `(key, attr, value, weight)` assignments and state,
labels, and assignee are all just attributes. PEP-19 generalizes it to
threads with comments and to the two-tier trust split.

There is no fixed open/close/label schema baked into storage. Structured
fields (status, labels, assignee, title, milestone, number) are attributes
set by `set` events under last-writer-wins by a monotonic weight; free-form
discussion is carried by `comment` events that accumulate in order. New
field kinds are added by convention without a storage migration.

Every mutation is one event:

  - open      start a thread (issue or pr), carries kind, title, body
  - comment   append discussion to a thread
  - revise    PR-only: update the thread's proposed coordinates (new tip),
              author-of-record only (see Pull-request canon and PEP-20)
  - set       assign a structured attribute (status, labels, assignee, ...)
  - close     shorthand for (set status closed), may carry a closing note
  - reopen    shorthand for (set status open), may carry a note
  - merge     PR-only: record the merge result (see Pull-request canon)
  - redact    owner-only: mark a prior event's rendered content hidden
              (a display-level action, see Redaction)
  - delegate  owner-only: authorize a maintainer sign key as a canon key
  - revoke    owner-only: withdraw a delegated maintainer key
              (delegate/revoke are defined by PEP-21; the fold processes them
              in seq order to know who may sign each later event)


Signing and canonical encoding
=============================

The two-tier trust model of PEP-17 is realized directly in the event
structure. Every event, without exception, carries two nested,
independently verifiable signed boxes. There is no unsigned event: the
storage invariant is "no author box, no event".

Author box, who said it. A `SignedBox` over exactly what the author actually
said: op and its author-supplied fields (for open: kind, title, body; for a
reply: the thread reference, reply-to, body or requested labels; the
author's declared timestamp). Signed by the author's signing key. For a
stranger's issue this is the stranger's key. For an owner-native event
(including every owner-authored `set`) the owner is simply both author and
canon, so the owner signs this box too. This is the fix for the otherwise
unsigned owner action: the assignment itself is inside a signed box, not
floating as bare canon fields.

Canon box, who blessed it and where it sits. A `SignedBox` over the fields
the owner assigns at fold time, which the author could not have known: the
event-id (the author box's content hash), the canonical order weight `seq`,
the human issue `number`, the folded-at time, the `origin` letter hash, and,
for events with encrypted attachments, the message group secret (see
Attachments in public canon). Signed by an authorized canon key.

Threading needs no canon-box normalization: a thread id and a reply-to are
canonical event-ids, and an event-id is the hash of an author box, which the
sender constructs, so the sender computes them directly (see Thread identity).

The two boxes are independent so the event-id is stable before `seq`/`number`
exist, and so an event extracted from the tree in isolation is verifiable:
the author box proves authorship, the canon box proves admission to canon.
The reflog signature already makes the whole tree unforgeable in transit and
storage, but it only proves the owner published the tree; it says nothing
about who authored a given comment, does not survive extraction of a single
event, and does not carry across a compaction rewrite. The per-event boxes
supply attributability, extract-one verifiability, and tamper-evidence
within canon; that is why both layers exist.

Canonical encoding. suckless-conf S-expressions have no canonical
serialization (whitespace, clause order, string escaping, and which clauses
belong to the signed part are all unpinned), so signatures are never taken
over rendered text. The signed unit is the binary `SignedBox` (the CBOR
`Serialise` representation), exactly as the Mailbox protocol signs the
serialized message rather than any printed form. The event-id is the hbs2
content hash of the serialized author box bytes. The S-expression in the
event file is a readable projection of the decoded boxes; it is regenerated,
never trusted over the boxes, and if a projected clause disagrees with the
box the box wins.

Author box payload type. The author box wraps one shared content record used
by both tiers (recommended): a Tier B `HubLetter` (PEP-18) is the subset a
non-owner may author (ops `open`/`comment` and the request ops), and an
owner-native event (`set`/`merge`/`redact`) is the same record with owner-only
ops. Fold and renderers then handle a single type. The alternative, an author
box carrying a tagged union of two record types, is equivalent but makes both
sides dispatch on the tag; either way the exact record layout is pinned once,
jointly with PEP-18.


Event schema (S-expression)
===========================

One event per file. The two authoritative artifacts are the base58-encoded
signed boxes; the remaining clauses are the readable projection.

```
(hub-event 1)
(op       open)                     ; open|comment|revise|set|close|reopen|merge|redact|delegate|revoke

;; --- authoritative signed boxes (verification operates on these) ---
(author-box <base58 of serialised SignedBox>)   ; signed by the author
(canon-box  <base58 of serialised SignedBox>)    ; signed by a canon key

;; --- readable projection of the AUTHOR box (regenerated, not trusted) ---
(author    <sign-pubkey-b58>)
(author-ts <word64>)               ; Unix epoch seconds, UTC; = a folded letter's `created`
(kind      issue)                  ; on open: issue | pr
(title     "...")                  ; on open
(thread    <thread-id>)            ; replies only, absent on open; the canonical
                                   ;   thread id (an event-id), sender-computable
(reply-to  <event-id>)             ; canonical id of the event replied to, replies only
;; body: inline text in the file body, or (body-part <hashref>) for large/binary
(set       status closed)          ; on set/close/reopen: the assignment
(redacts   <event-id>)             ; on redact: the target event
(delegate  <sign-pubkey-b58>)      ; on delegate: the maintainer key authorized
(revoke    <sign-pubkey-b58>)      ; on revoke: the maintainer key withdrawn

;; --- readable projection of the CANON box (regenerated, not trusted) ---
(seq       <word64>)               ; globally monotonic order weight
(number    <int>)                  ; on open only: human #N, owner-assigned
(origin    <message-hash>)         ; Tier B letter this was folded from (absent if owner-native)
(folded-ts <word64>)               ; Unix epoch seconds, UTC, owner's clock at fold
(canon-by  <sign-pubkey-b58>)      ; owner or delegated maintainer
(part-secret <group-secret-b58>)   ; only on events referencing encrypted parts:
                                   ;   the message group secret, so canon readers decrypt them
```

An owner-blessed status change is therefore a fully signed event: the owner
signs an author box carrying `(op set) (set status closed)` and a canon box
carrying its `seq`/`folded-ts`/event-id. Nothing about the assignment is
unsigned.

`event-id` is the content hash of the author box, so it is stable and
independent of the canon fields the owner adds later. On `open` the author
box carries no `thread` clause (the event is the thread root); its
`thread-id` is the opening `event-id` itself, which anyone can recompute by
hashing the author box. This removes the circularity of a thread id that
would otherwise have to be inside the content whose hash defines it. Replies
carry the canonical `thread` (and optional `reply-to`) directly in their
author box; these are the ids the fold uses, with no canon-box normalization.

A `close`/`reopen` note, if present, is the event body, rendered as a comment
attached to the status change. A separate discussion comment is its own
`comment` event.


Thread identity
=============

Thread and reply identifiers are canonical event-ids, and an event-id is the
hash of an author box. The sender constructs the author box, so the sender
computes the canonical id at send time, with no owner handshake. This makes
identity uniform across the two tiers and removes the id-mapping the first
draft carried:

  - The opening event carries no `thread` (its id is the hash of its own
    author box, which cannot contain that hash). Its `thread-id` is that
    event-id. The opening letter's Mailbox message hash is recorded as
    `origin` for provenance, nothing more.

  - A reply (a `comment`/`set`/`close`/... event, or the letter that folds
    into one) carries the canonical `thread` and optional `reply-to`
    directly. The thread's author computed these when sending the opening
    letter; a third party reads them from public canon. The fold uses them
    as-is; there is no letter-to-canon translation.

A reply may reference an event whose letter has not yet been folded: its
event-id is still computable, so the reference resolves once both are in
canon and dangles harmlessly otherwise. Acknowledgement (the owner replying
to a letter's back-channel) is only needed for the human `number` and status,
never for threading.


On-disk tree layout
==================

The tree under a `refs/hbs2/meta` commit:

```
/
  version                      ; "hub-meta 1"
  threads/
    <thread-id>/
      00000000000000000042-<event-id>   ; name = seq zero-padded to 20 digits + "-" + event-id
      00000000000000000097-<event-id>
      ...
  repo/
    00000000000000000005-<event-id>     ; repo-scope events: delegate/revoke,
    ...                                 ;   and any public triage bans (PEP-21)
  index/
    number.sexp                ; (number <n> <thread-id>) lines, owner-assigned map
```

Events come in two scopes. Thread-scope events (`open`/`comment`/`revise`/
`set`/`close`/`reopen`/`merge`/`redact`) live under `threads/<thread-id>/`.
Repo-scope events that belong to no thread (`delegate`/`revoke`, and a public
triage ban if PEP-21 records one) live under `repo/`. `readEventLog` reads
both, and the fold merges them into one `seq`-ordered stream (repo-scope
`delegate`/`revoke` must be seen in `seq` order alongside thread events so the
maintainer set is correct at each event, see the fold).

`seq` is a `word64`, so filenames zero-pad it to 20 digits; a lexical
listing is then already in canonical order, but the fold does not rely on
filesystem order (it sorts explicitly, see below). Because every file is
immutable and content-stable, an incremental update adds a few files and
reuses every existing object, which is where the git3 dedup pays off.
`index/number.sexp` is a convenience map from human `#N` to `thread-id`; it
is regenerable from the surviving `open` events (which compaction never
drops, see Retention) and is never trusted over them.

There is no materialized issue view stored in the tree. Canon is the event
log; the view is recomputed (see Read contract). This keeps canon minimal
and its determinism trivial to guarantee.


Deterministic materialization
============================

The fold is a pure, total function of the event set. Ordering:

  - Primary key: `seq`, a globally monotonic integer the owner assigns at
    fold time. Because it is assigned by a single authority in the order
    events are admitted, it is a total order over all canon events, and the
    commit-chain packaging is irrelevant to the result.

  - Tie-break: `event-id` (content hash), lexical. This guarantees a total
    order even in the degenerate case of a duplicated `seq`, so the fold is
    deterministic regardless of enumeration order or how commits batched
    the events.

Admission. An event enters the fold only if all of the following hold; any
failure drops the event and is surfaced as a warning, never silently
altering state:

  1. The author box signature verifies.
  2. The canon box signature verifies and references this event's id.
  3. `canon-by` is an authorized canon key: the repo owner key (the signing
     key of the repo's LWWRef), or a maintainer key delegated and not yet
     revoked as of this event's `seq`. Delegation is by owner-signed
     `delegate`/`revoke` canon events (PEP-21); with no delegation, the owner
     key is the sole canon key. Any other `canon-by` is rejected.
  4. For owner-authored ops (`set`, `merge`, `redact`), the author box signer
     is also an authorized canon key. A stranger cannot author these; their
     letter equivalents are only requests (see Folding). (`number` is not an
     op: it is a canon-box field on the `open` event, so it is owner-signed
     by construction.)
  5. For `delegate`/`revoke`, both the author box signer and `canon-by` must
     equal the LWWRef owner key exactly, not merely an authorized canon key.
     This is the root-of-trust rule: without it a delegate could sign its own
     `delegate` and expand the maintainer set, escalating privilege. Rule 3
     alone (any authorized canon key) does not close this, so `delegate`/
     `revoke` are gated on the owner key specifically.

Deduplicate by event-id. Canon events are keyed by event-id (the author box
hash). The same author box can arrive more than once: a Tier B letter can be
rewrapped in a fresh transport envelope and resent by anyone holding it
(PEP-18 Replay), and a fold can be recomputed. The fold collapses all copies
of an event-id to one canon event, so a rewrapped or re-folded letter never
produces duplicates. If two copies somehow carry different canon boxes, the
lowest `seq` wins (the first admission is authoritative).

Rule 3 (`canon-by` authorized as of the event's `seq`) depends on the
maintainer set at that point, so admission is not a pure pre-filter: it is
interleaved with the seq-ordered pass. The fold carries a `maintainers` set,
seeded with the owner key and updated by `delegate`/`revoke` as they are
encountered, and checks each event's canon key against `maintainers` as it
goes (`e.thread` is the event's `thread` clause, or its own event-id on
`open`):

```
state = {}                                  ; thread-id -> ThreadState
byid  = {}                                  ; event-id -> the materialized event (for redact)
seen  = {}                                  ; event-id set, for dedup
maintainers = { owner_key }                 ; authorized canon keys, evolves by seq
for e in sort(events, by=(seq, event_id)):
    if e.id in seen: continue               ; dedup by event-id
    if not admit(e, maintainers): continue  ; rules 1-4, using maintainers as-of now
    if e.op in {comment,revise,set,close,reopen,merge} and e.thread not in state:
        warn("dangling thread"); continue   ; reply to a non-admitted open -> drop
    seen.add(e.id); byid[e.id] = e
    case e.op of
      open   -> state[e.id] = new(kind, title, number, author, opened_at)
      set    -> state[e.thread].attrs[k] = v        ; LWW, later seq overwrites
      close  -> state[e.thread].attrs.status = closed ; body -> attached note
      reopen -> state[e.thread].attrs.status = open   ; body -> attached note
      comment-> state[e.thread].comments.append(e)    ; kept in seq order
      revise -> if e.author == state[e.thread].author or e.author in maintainers:
                  state[e.thread].pr.coords = coords(e)  ; new proposed tip; else drop
      merge  -> state[e.thread].pr = merged(e)
      redact -> if e.redacts in byid: byid[e.redacts].rendered = hidden ; content stays in tree
      delegate -> maintainers.add(e.key)              ; admit() enforced rule 5:
      revoke   -> maintainers.remove(e.key)           ;   owner-key-signed only
```

Because `delegate`/`revoke` are applied in the same `seq` order that governs
admission, an event is judged against exactly the maintainer set in force when
it was signed; a later `revoke` never retroactively invalidates events the key
signed while authorized. This is why compaction must never drop a
`delegate`/`revoke` event (see Retention).

Two safety rules the pass encodes. A reply-class event whose `thread` is not
an admitted `open` is dropped with a warning (a dangling reference, which
`hub verify` reports, PEP-22), so a comment on a rejected or never-folded open
cannot create a phantom thread. And `redact` locates its target through the
`byid` index of already-materialized events; a `redact` naming an unknown
event-id is a no-op. Since the pass is `seq`-ordered and a `redact` targets a
prior event, the target is always present when a valid `redact` runs.

`revise` is author-authored but restricted to the author of record: the fold
applies it only when its author box signer equals the opening event's author
(or is a canon key), so a third party cannot redirect someone else's PR to
their own tip. Because updates are ordered by `seq`, the latest surviving
`revise` (or the `open` coordinates if none) is the current proposed tip,
unambiguously.

`apply` is order-independent per attribute (LWW keyed by the monotonic seq)
and append-only for comments (seq gives their order), so re-folding always
yields the same state.


Issue and PR numbering
====================

Human-facing numbers (#1, #2, ...) are assigned by canon at fold time as the
owner-signed `number` field in the opening event's canon box, mirrored in
`index/number.sexp`. This resolves numbering in a decentralized system
cleanly: numbers are monotonic and deterministic per repo because a single
authority mints them, while the coordination-free `thread-id` remains the
stable machine handle. A fork that has not been folded by the owner has
thread-ids but no numbers, which is correct: numbers are an owner-namespace.

Because `number` lives on the `open` event and compaction never drops `open`
events (see Retention), the number map stays fully derivable from canon.


Pull-request canon
================

A PR thread is an issue thread whose opening event has `kind pr` and carries
the source coordinates from the PEP-18 letter. A later `revise` event (from
the author of record) supersedes them with a new proposed tip, so the current
coordinates are the latest surviving `open`/`revise` by `seq`:

```
(source     hbs23://<fork-repo-key>)
(source-ref refs/heads/feature)
(source-tip <git-sha1>)
(onto       refs/heads/master)
(base       <git-sha1>)
```

Proposed objects. Before merge, the owner keeps the proposed tip fetchable
and reviewable offline under

```
refs/hbs2/pulls/<number>/head  ->  <source-tip>
```

This is an ordinary orphan-style ref. Publishing it is cheap: the owner
already holds `base`, so exporting `source-tip` into their own reflog is a
same-reflog incremental push that writes only the delta objects (unlike
fetching a foreign fork, which re-imports its whole reflog today, see PEP-20).
The PR tip then stays browsable even if the contributor's node goes away. On
merge, the owner updates the real code branch normally and emits a `merge`
event recording the resulting merge commit:

```
(op merge) (merge-commit <sha1>) (merged-into refs/heads/master)
```

On reject/close, the `pulls/<n>/head` ref may be dropped or retained for the
record per the retention policy. The proposed commits, once merged, live in
the normal reflog like any other history.


Attachments in public canon
=========================

An event's body or diff may live in an attachment: a `(body-part <hashref>)`
or PR `(bundle-part <hashref>)` clause inside the author box, pointing at an
encrypted merkle tree (PEP-18 ships attachments as Mailbox parts). Those
trees are encrypted with the letter's per-message group secret, wrapped only
for the maintainers, and the hashref is inside the signed author box that
canon publishes verbatim. A public clone would therefore see an event
referencing an attachment it cannot decrypt, and re-encrypting the part is
not possible: a new ciphertext has a new hash and would break the signed
reference.

So when a folded event references encrypted parts, the owner publishes the
message group secret in the owner-signed canon box (`part-secret`). This
reveals nothing beyond what the fold already makes public, because that
secret encrypted only this letter's own content, which is being published.
A canon reader fetches the referenced tree over hbs2 and decrypts it with
`part-secret`. Note the attachment blocks are hbs2 storage objects fetched
over the same transport as the git data, not git blobs in the meta tree;
making attachments git-native blobs (a self-contained clone) is a later
option, not required here. Owner-native events with no attachments carry no
`part-secret`.


Folding Tier B letters into Tier A
================================

This is the bridge that produces canon. When the owner accepts a PEP-18
letter, the hub maps it to one or more events, preserving authorship. A
PEP-18 letter carries a nested inner `SignedBox` over its plaintext payload
(distinct from the Mailbox transport signature, which covers the encrypted
envelope); that inner box is what the owner extracts on decrypt and stores as
the event's author box, verbatim, so the sender's signature is carried into
canon and stays publicly verifiable without the mailbox. The owner adds the
canon box. Mapping:

```
letter (op open),  kind issue/pr  ->  open event
                                       author box = the letter's inner SignedBox
                                       canon box  = owner assigns seq, number,
                                       origin = letter hash, thread-id = event-id
letter (op comment)               ->  comment event (author = sender)
letter (op revise), kind pr       ->  revise event (author-of-record only)
letter (op close|reopen|label)    ->  a REQUEST, not canon by itself
```

Identifiers need no reconciliation (see Thread identity): a letter already
carries canonical event-ids, since the sender computes them from the author
box it constructs. The bridge only records the opening letter's message hash
as `origin` for provenance. It also publishes the message group secret into
the canon box when the event references encrypted parts (see Attachments in
public canon).

Permission model. Three classes of op, enforced by which key signs:

  - Author-authored: `open`, `comment`. Anyone who can send a letter may
    author these; the author box carries their signature, and the owner's
    canon box only admits and orders it. A stranger's comment enters canon
    as genuinely theirs.

  - Author-of-record: `revise`. Author-authored, but the fold applies it only
    when its author equals the thread's `open` author (or a canon key), so a
    third party cannot redirect someone else's PR to a tip they control.

  - Owner-authored: `set` (status, labels, assignee, milestone), `merge`,
    `redact`, `delegate`/`revoke`, plus the `number` field the owner assigns
    on `open`. Admission requires the author box signer to be an authorized
    canon key (rule 4 above); `delegate`/`revoke` specifically must be signed
    by the LWWRef owner key, the root of trust, not merely a delegate. A
    stranger's `close` letter is a request the owner may honor by authoring an
    owner-signed `close`, or ignore. The delegation model is PEP-21.

Because canon only ever contains owner-blessed events, Tier B spam, drafts,
and harassment never reach a clone unless the owner promotes them, which is
exactly the PEP-17 privacy-by-construction property.


Multi-maintainer ordering
=======================

Single-writer is the default and needs no consensus: the owner assigns `seq`
in the order they fold, the `refs/hbs2/meta` chain is linear, and there is
no conflict. This covers the common case.

For more than one maintainer, PEP-17's optional maintainer-consensus RefChan
becomes the ordering oracle, reusing exactly the fixme-new pattern: delegated
maintainers sign their canon events and propose them to the RefChan, and the
RefChan `AcceptTran` timestamps supply a total order (as fixme-new derived
weights from accept time). A single publisher, the holder of the repo's reflog
key, consumes that RefChan-ordered log, assigns `seq`/`number` from the
consensus order, and snapshots it into the `refs/hbs2/meta` commit chain. So
the two-tier idea recurses: the RefChan is the multi-writer ordering log,
`refs/hbs2/meta` is the published, self-contained snapshot every clone reads.

This is where signing and publishing must not be conflated (PEP-21). A
delegated maintainer can sign canon events, but only the reflog-key holder can
push `refs/hbs2/meta`; the RefChan is the channel that carries a delegate's
signed events to that publisher. Because there is exactly one publisher,
`seq`/`number` uniqueness is automatic. PEP-21 specifies the signing-versus-
publishing capability split and the routing. A repo names the consensus
channel with the optional manifest clause from PEP-17:

```
(refchan <refchan-key-b58>)
```


Read contract for renderers
=========================

The hub layer is a library plus CLI; the web UI (PEP-22) is a pure view over
what they expose. The contract:

  - `readEventLog  :: CommitChain -> [Event]`   parse the meta tree
  - `materialize   :: [Event] -> Map ThreadId ThreadState`  the fold above
  - `ThreadState` fields: number, kind, title, status, labels, assignee,
    author, created_at, updated_at, comments[], and for PRs the source/onto
    coordinates and merge result.

The CLI maintains a local materialized cache in SQLite, ported from
fixme-new (`/home/user/dev/hbs2-legacy/fixme-new/lib/Fixme/State.hs`): its
single `object(o, w, k, v)` table with last-write-wins by weight, plus a
`comments` table and a `scanned`-style table recording the last applied
`seq` so re-fold is incremental. The port is nearly verbatim; the monofold,
the LWW-by-weight upsert, and the scanned-dedup table all carry over. The
cache is node-local and never canonical: it is always reproducible from the
event log, so it is gitignored and rebuildable. A renderer reads either the
library `materialize` directly or the SQLite cache; it never writes canon.


Redaction, retention, compaction
==============================

Redaction is display-level, and its limits must be stated plainly. A
`redact` event marks a target event's rendered content hidden; the fold sets
its `rendered` flag and the renderer suppresses the body. The target file
stays in the tree, so its content remains in every clone that already
fetched it. Redaction therefore does not remove a leaked secret: an
accidental secret committed to canon is present in every replica and must be
rotated, not redacted. Redaction suits removing abusive or off-topic content
from the rendered view while keeping the thread coherent. Because the target
event is untouched, its author box signature keeps verifying; there is no
"signature over hidden content" problem, since nothing is actually removed.

True erasure is bounded by the storage layer, not granted by ref control.
Erasing content means rewriting the `refs/hbs2/meta` lineage to a version
that never contained the file. But hbs2 storage is content-addressed and
append-only at the segment level: force-updating the ref does not delete the
objects from the owner's own storage (absent a separate GC pass) and cannot
delete them from other nodes that already fetched the segments. Honestly
scoped, erasure removes content only from future fetches of the new history
line, not from existing replicas. This reinforces the rotation-not-erasure
rule for secrets.

Compaction bounds log growth without sacrificing verifiability. The log
grows mostly from `set` churn (repeated status/label changes), and only the
winning (highest-seq) `set` per (thread, attribute) is load-bearing for
state. Compaction rewrites `refs/hbs2/meta` to a new lineage that drops only
superseded, body-less `set`-class events (a plain `set`, or a `close`/`reopen`
carrying no note), and must retain, untouched:

  - every `open` event (carries `number`, thread root, and authorship, and
    keeps `index/number.sexp` derivable);
  - every `comment` event (irreplaceable authored discussion);
  - every `merge` and `redact` event;
  - every `delegate` and `revoke` event, always: admission of every historical
    event depends on the maintainer set as of its `seq`, reconstructed from
    these; dropping one would change which past events the fold admits and
    break determinism (they are not `set`-class);
  - every `set`-class event carrying a non-empty body, including a
    `close`/`reopen` with a note, since that note is authored discussion
    even when the status change it made was later superseded;
  - the single winning `set` event per (thread, attribute).

This preserves offline authorship verifiability for all discussion and
thread roots while removing the bulk (the overwritten `set` events), so the
size win is nearly the full churn. No materialized snapshot is ever written
into canon as a substitute for events; canon stays events-only, and any
snapshot is the rebuildable cache. Because compaction rewrites the ref
lineage, `hub sync` must follow it with the forcing `+refs/hbs2/meta:...`
refspec shown earlier. Compaction cadence and the "superseded" predicate are
policy, deferred to PEP-21.


What exists today vs what must be built
====================================

Exists today (from the git3 model):

  - Arbitrary ref names end to end; orphan-commit push/fetch/advertise for
    `refs/hbs2/meta` and `refs/hbs2/pulls/*` with no code change.
  - Content-addressed dedup across refs, so an incremental tracker update is
    nearly free.
  - The event-sourced attribute model, monoidal fold, and SQLite
    materialization, proven in fixme-new
    (`/home/user/dev/hbs2-legacy/fixme-new`) and portable nearly verbatim.
  - S-expression parsing (suckless-conf) for the readable projection, and
    SignedBox (signing the binary Serialise form, as Mailbox does) for the
    two-layer signing and canonical encoding.
  - The manifest is trivially extensible with a `(mailbox ...)` /
    `(refchan ...)` clause (a two-line reader/emitter change) per PEP-17.

Must be built:

  - The hub-meta library: event box encode/decode, the two-layer
    sign/verify, the deterministic fold, and the SQLite cache with
    incremental re-fold.
  - The fold/triage bridge that maps accepted PEP-18 letters to canon events
    (shared with PEP-20 for PRs, PEP-22 for the CLI). Identifiers need no
    mapping: letters already carry canonical event-ids (see Thread identity).
  - `hub clone`/`hub sync` wrapping the explicit forcing `refs/hbs2/meta`
    fetch refspec (and, optionally, a remote-helper change to auto-include it
    in a plain clone).
  - PR proposed-object handling under `refs/hbs2/pulls/<n>/head` (fetch,
    review, merge, status), specified in full by PEP-20.


Rejected alternatives
===================

A new reflog section type for meta events. The reflog already tags sections
B/T/C/A/R; an `M` (meta) section could carry events directly. Rejected: it
is an export/import code change, it is not git-native (no `git log` over the
tracker, no diff, no plain-clone), and it forgoes the orphan-commit path
that already works. The commit chain gives versioning and tooling for free.

A ref pointing directly at a tree (refs/hbs2/meta -> tree). Slightly less
overhead than a commit chain, but the remote helper only special-cases
commit tips (and tags peeled to a commit, plus deletes) in
`GitRemoteHelper.hs`; a tree or blob tip is not handled and falls into
`export`, which feeds it to the commit-graph walk and crashes with
`InvalidObjectFormat` in `gitReadCommitTree`. Supporting it is a code change,
and it loses the built-in history/versioning a commit chain provides. Not
worth it.

Materialized view stored in the tree as canon. Committing the folded state
(not just events) would let a renderer skip the fold, but it makes canon
redundant with itself, risks divergence between the stored view and the
events, and complicates the determinism guarantee. Rejected: canon is events
only, and compaction never substitutes a snapshot for events; the
materialized view is always the rebuildable cache.

Storing only author pubkey + origin hash, no embedded signature. Smaller,
but authorship would then be verifiable only while the Tier B mailbox
message survives, breaking offline verifiability from a clone. Rejected in
favor of the embedded author box, which is also why compaction is defined to
retain `open`/`comment`/`merge`/`redact` boxes rather than collapse them
into an unsigned snapshot.


Open questions
============

- Auto-fetching the meta ref on a plain `git clone` (remote-helper injected
  refspec) vs requiring `hub sync`. The model works either way; decide the
  ergonomics.
- Delegated-maintainer keys: resolved by PEP-21 (owner-signed
  `delegate`/`revoke` canon events, seq-ordered validity); a clone learns the
  set from canon. Scoped roles (triage-only vs merge-capable) remain open.
- Compaction cadence and the exact "superseded" predicate.
- Whether `redact` should also be expressible by a contributor over their own
  content (self-deletion request) vs owner-only.
- Interaction with PEP-13 (PQ) / group-key rotation for any encrypted parts
  referenced by `(body-part <hashref>)` in canon.
