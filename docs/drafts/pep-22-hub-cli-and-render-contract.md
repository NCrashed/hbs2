PEP-22: hub CLI and web rendering contract

Status: draft, started 2026-07-25.
Author: NCrashed (Anton Gushcha)
Part of: PEP-17 (hbs2-hub umbrella).
Depends on: PEP-18 (letter), PEP-19 (canon, fold, read contract),
            PEP-20 (pull-request model), PEP-21 (triage/moderation),
            hbs2-git3, hbs2-cli, fixme-new (archived) as starting code.
Related: all of PEP-18..21.

This is the last sub-proposal. It fixes the command surface a human or script
drives the forge with, and the stable data contract a web UI renders, so the
web layer stays a pure view and independent renderers interoperate. It adds no
new protocol: every command is a driver over the library the other
sub-proposals define.


Principle: CLI-first, web as a pure view
======================================

The hub is a library plus a CLI. The library is the union of the pieces the
other PEPs specify: the PEP-18 letter (compose/verify), the PEP-19 canon
(event boxes, the deterministic fold, the SQLite materialization cache), the
PEP-20 PR flow, and the PEP-21 moderation controls. The CLI is a thin driver
over that library. A minimalist Gitea-style web UI is optional and is a pure
view: it renders the contract below and never folds, decrypts, verifies, or
writes canon itself. Everything the web can show, the CLI can show; the web
adds presentation, not capability.


Three layers and how data flows
=============================

```
compose (Tier B)          read (any clone)          maintain / moderate (Tier A)
  hub issue new             hub issue|pr list/show     hub inbox / accept / merge
  hub pr new/revise    -->  hub log / verify      <--  hub issue close/label/assign
        |                        ^                          hub policy / block / ban
        v                        |                          hub maintainer / compact
   PEP-18 letter            render contract                     |
   -> Mailbox               (from the fold)                     v
                                 ^                          PEP-19 canon events
   library: letter + canon + fold + cache + moderation      -> refs/hbs2/meta
```

Writes are one-way into the tiers: compose commands emit PEP-18 letters (Tier
B), maintain commands emit owner-signed PEP-19 events (Tier A). Reads all go
through the fold to the materialized cache; the render contract is the
serialized projection of that. The web view is downstream of the contract and
writes nothing.


CLI surface
=========

The surface is `hub <noun> <verb>`, conventional and scriptable. It is also
exposable as hbs2-cli `bindMatch` S-expression entries reusing fixme-new's
command grammar (see Reuse). Commands group by the capability they need.

Read (any clone; needs only the repo and its canon):

```
hub clone   hbs23://<repo-key>      ; clone code + fetch refs/hbs2/meta
hub sync                            ; fetch code + '+refs/hbs2/meta:...' (PEP-19)
hub issue list [query]             ; folded issues; query DSL below
hub issue show <n|thread-id>       ; thread with comments, status, labels
hub pr    list [query]
hub pr    show  <n|thread-id>      ; PR thread + coordinates + diff
hub pr    checkout <n>            ; fetch pulls/<n>/head to a local branch for review
hub log   [<n>]                    ; timeline of surviving events (subject to compaction)
hub verify                         ; re-run the fold's checks; report dropped events
```

Contribute (Tier B letters, PEP-18; needs the target mailbox + a sigil):

```
hub issue new  --target <repo> --title ... [--label ...] [< body]
hub issue comment <thread-id> [< body]
hub issue close|reopen|label <thread-id> ...   ; a REQUEST (owner decides, PEP-19)
hub pr new     --target <repo> --onto master --from <ref>   ; builds a bundle (PEP-20)
hub pr revise  <thread-id> --from <ref>                     ; author-of-record (PEP-20)
hub pr comment <thread-id> [< body]
hub updates                        ; read own reply-mailbox; correlate acks with sent threads
hub identity set-reply-mailbox <key> --sigil <hashref>      ; optional back-channel
```

`hub pr new`/`revise` default to the delta-artifact path: they build a git
bundle `base..<ref>` and attach it (PEP-20), computing `base`/`source-tip`
from the local repo and signing them into the letter. `--fork <fork-key>` uses
the fork-pointer path instead. `hub updates` is how a contributor reads the
back-channel: the owner's acknowledgements and status updates arrive as
messages in the contributor's own `reply-mailbox` (PEP-18/20), and this verb
reads that mailbox and correlates each ack to the thread it sent.

Maintain (Tier A, owner or delegated maintainer; PEP-19/20):

```
hub inbox                          ; decrypted triage queue (Tier B), verified
hub inbox show <msg>               ; one submission, inner author + payload + attachments
hub inbox accept <msg>             ; fold into canon (assigns number), then delete (PEP-21)
hub inbox reject <msg> [< note]    ; delete the letter + optional courtesy note; NO canon event
hub issue close|reopen|label|assign <n> ...   ; owner-signed set events on a folded thread
hub pr   merge <n> [--strategy ...]           ; verify, integrate, merge event (PEP-20)
```

Two distinctions matter here. `hub inbox reject` acts on an unfolded Tier B
submission: there is no canon thread to close (the fold never ran), so it is
just a `DeleteMessages` plus an optional courtesy note to the sender's
`reply-mailbox`, emitting no canon event. Closing an already-folded thread is
the separate `hub issue close <n>`, an owner-signed `set status closed` on an
existing canon thread. And `hub issue close|reopen|label` appears in both the
contribute and maintain groups: the resolution rule is capability-based, if
the caller holds an authorized canon key the verb emits an owner-signed event,
otherwise it emits a request letter. A maintainer who wants to only ask (not
act) passes `--request` to force the letter form.

Moderate (PEP-21; owner or delegated where noted):

```
hub policy show
hub policy set (sender allow|deny all|<key>) | (pow D) | (rate <key> <s>) | (quota <key> <n>)
hub block <envelope-key>           ; peer-layer deny (bounds storage; evadable)
hub ban   <inner-author-key>       ; triage-layer deny (authoritative for canon)
hub delete <msg-predicate>         ; DeleteMessages (retention, PEP-21)
hub maintainer add|remove <key>    ; delegate/revoke, OWNER key only (PEP-21)
hub compact                        ; canon compaction (PEP-19/21), forces refs/hbs2/meta
```

Query DSL and templates are inherited from fixme-new: the query language
`~attr:value` (like), `&&`/`||`/`!`, compiled over the folded view, and
fixme-new's own S-expression template system (`SimpleTemplate`, rendered to
styled terminal output). So `hub issue list` is a report query with a
templated format, not a fixed one.


The stable render contract
========================

This is the deliverable that keeps the web layer a pure view: a versioned,
read-only, serialized projection of the fold that any renderer consumes
without touching hbs2, crypto, or the event log. It is the PEP-19 `ThreadState`
made into a documented wire schema, so a web UI, a static export, and a
third-party renderer all agree.

Properties:

  - Derived purely from canon via the fold, so it is reproducible on any clone
    and never a separate source of truth.
  - Versioned (`contract N`) and field-documented, so the web layer does not
    break on internal changes and independent renderers interoperate.
  - Read-only. It carries no keys and no capability to write; composing and
    moderating are CLI/library actions, never contract fields.
  - Carries provenance identity, so a renderer can attribute without doing
    crypto: each item names its author key and the canon key that blessed it.
    It carries no `verified` boolean, because everything in the contract has
    already passed the PEP-19 admission check (a bad signature or an
    unauthorized canon key is dropped by the fold, never materialized). So
    presence in the contract is exactly "verified"; there is nothing to flag.
    Auditing what was dropped is `hub verify`'s job, not the contract's.

Shape (JSON projection of a thread):

```
{
  "contract": 1,
  "kind": "pr",                       // issue | pr
  "number": 42,
  "thread_id": "<event-id>",
  "title": "...",
  "status": "open",                   // open | closed | merged | ...
  "labels": ["bug","ui"],
  "assignees": ["<sign-key>"],       // multi-valued, like labels
  "author": "<sign-key>",             // opening event's author
  "canon_by": "<sign-key>",           // canon key that blessed the opening event
  "created_at": 1737763200,           // author-ts of the opening event
  "updated_at": 1737849600,
  "redacted": false,
  "comments": [
    { "event_id": "...", "author": "<sign-key>", "canon_by": "<sign-key>",
      "ts": 1737766800, "body": "...", "redacted": false }
  ],
  "pr": {                             // present when kind == pr
    "onto": "refs/heads/master",
    "base": "<sha1>", "tip": "<sha1>",      // latest surviving open/revise (PEP-20)
    "merge_commit": "<sha1|null>",
    "diff": { "format": "unified",
              "availability": "available",  // available | reconstructable | unavailable
              "truncated": false, "text": "..." }
  }
}
```

A renderer must tolerate a `reply-to` that names no event it can see, or one
that belongs to another thread. Nothing validates it, deliberately: the
reference may point at a letter the owner chose not to fold, and rejecting
such a comment at the bridge would strand a legitimate submission for good.
Render it flat rather than failing or inventing a parent, and leave the
reporting to `hub verify`, which lists dangling and cross-thread references
along with the dropped events.

Provenance is per item, not per thread: `canon_by` sits on the opening event
and on each comment, because under delegation (PEP-21) different events on one
thread may be blessed by different maintainer keys, so a single thread-level
`canon_by` would be lossy.

The PR `diff` is precomputed by the library from `base..tip` so a static
renderer needs no git. Its `availability` is three-state, because the objects
may or may not be present: `available` (objects in the repo, diff inlined,
possibly `truncated` with a link to the staged `refs/hbs2/pulls/<n>/head`),
`reconstructable` (objects gone, e.g. a rejected PR whose `pulls` ref was
dropped, but recoverable from the `bundle-part` attachment via its
`part-secret`, at a cost, so the renderer offers to rebuild rather than
inlining), or `unavailable` (neither present nor reconstructable). An index
document lists threads (number, kind, title, status, updated_at) for list
views. An activity document exposes the surviving event stream in `seq` order
for a timeline (subject to compaction, PEP-19/21). Both index and activity
schemas are to be pinned alongside this one (Open questions).


Web renderer: static export or local serve
=========================================

Two consumption modes, both pure views over the contract:

  - Static export: `hub render` writes a self-contained static site (the
    contract plus HTML/CSS, no server, no hbs2) from a clone, so the tracker
    is browsable offline, satisfying PEP-17's offline requirement. This is the
    minimalist Gitea-style UI as a build artifact.
  - Local serve: `hub serve` runs a read-only local server that regenerates
    the contract from the fold on change, for a live view. It still writes
    nothing to canon.

An interactive web that wants to offer "comment" or "merge" buttons does not
write canon directly: it shells out to the CLI/library (compose a letter, or
an owner action), which is the only writer. The rendering contract stays
read-only; interactivity is a separate, explicitly-authenticated path, not a
contract field. This preserves "web is a pure view": the view cannot forge
canon because it has no signing capability.


Provenance and verification in the view
=====================================

Because canon is signed end to end (PEP-19), the renderer can attribute
without computing anything. The fold admits only events whose author box and
canon box verify and whose canon key was authorized at the event's `seq`;
anything failing is dropped, never materialized. So the contract's `author`
and `canon_by` keys are sufficient: the UI marks a comment "signed by
<author>" and a status change "blessed by <maintainer>" from the keys alone,
and the fact that the item is present already means it verified. There is no
"unverified" state to render inside a thread.

Auditing what did not make it is a separate tool: `hub verify` re-runs the
fold's checks over the raw event log and reports events that were dropped and
why (bad signature, unauthorized canon key, dangling reference). That belongs
in an operator view, not the per-thread render contract, which stays a clean
projection of admitted state. Redacted items render as withheld, not removed
(PEP-19). All of this is a pure function of canon, so every clone shows the
same thing.

Issue numbers are the other canon-box field taken on trust. The fold applies
`number` as the owner signed it and checks neither uniqueness nor
monotonicity, which is harmless while a single owner mints them but not once
delegation is in real use: two maintainers folding concurrently can mint the
same number, and nothing in the fold notices. `hub verify` reports duplicate
and non-monotonic numbers, naming the canon keys involved. Making the fold
reject them would be worse, since a clone would then silently show fewer
threads than canon contains.

Where these come from, concretely: the fold collects them as it goes and hands
back a list alongside the threads, in `seq` order. It is the one pass that sees
the whole log in order, so anything `hub verify` recomputed from the raw events
would be a second, divergent implementation of the same walk. The list covers
duplicate `seq`, duplicate and backwards `number`, backwards `folded-ts`, two
events folded from one letter, an event naming an encrypted part with no
`part-secret`, and an attribute value that is not in canonical form for its
name.

`hub verify` also flags what the fold accepts but cannot police. The
canon-box `folded-ts` is load-bearing (the render contract's times come from
it, precisely because the author's declared timestamp is attacker-chosen),
yet it is asserted by whoever signs the canon box. A delegated maintainer,
who is by construction trusted less than the owner (PEP-21 rule 5), can set
`folded-ts` to an absurd value and pin a thread to the top of every
recency-sorted view, and no later event can undo it. The fold does not reject
this, because `folded-ts` carries no ordering authority (`seq` does) and a
strict rule would break legitimate clock skew. Instead `hub verify` reports
`folded-ts` that is non-monotonic with respect to `seq`, or implausibly far
from its neighbours, naming the canon key that signed it. Detection at audit
time, not silent acceptance and not a hard fold rule.

The same applies to an event that names an encrypted `body-part` or
`bundle-part` and carries no `part-secret`. The fold admits it, because the
reference is inside a signed author box and there is nothing wrong with the
event; what is missing is the key, and no later event can supply it (PEP-19
"Attachments in public canon"). A folder written to this spec refuses to mint
such an event, so this only appears in canon somebody else published, which is
exactly what `hub verify` is for. It reports the event and the thread, so a
reader knows the attachment will never open rather than retrying forever.


Reuse of fixme-new
================

fixme-new (archived at `/home/user/dev/hbs2-legacy/fixme-new`) already
implements most of this shape over a channel and is the starting code:

  - The `bindMatch` S-expression command grammar (create/edit/modify/delete/
    list/show/report, `Run.hs`) maps onto the `hub` verbs.
  - The report query DSL (`~like`, `&&`/`||`/`!` compiled to SQL over a folded
    JSON blob, `State.hs`) is the `hub issue list [query]` engine.
  - The `SimpleTemplate` S-expression template system (`Types.hs`, rendered to
    styled terminal output) is the list/show formatting. (fixme-new has no
    mustache/microstache; a mustache-style web templating layer, if wanted,
    is new work, not a port.)
  - The SQLite `object(o,w,k,v)` materialization with last-write-wins is the
    PEP-19 cache (generalized with comments and a last-applied-`seq` marker).

What changes from fixme-new: the channel is Mailbox ingress plus reflog canon
(not a RefChan), events carry the PEP-19 two-layer signing, and the PR verbs,
the render contract, and its web export are new.


What exists today vs what must be built
====================================

Exists today:

  - hbs2-cli's `bindMatch` S-expression command machinery and the Mailbox
    message create/read commands the compose/triage verbs build on.
  - fixme-new's query DSL, templates, and SQLite materialization to port.
  - The git tooling `hub pr` shells out to (bundle, fetch, merge, index-pack).

Must be built:

  - The `hub` CLI itself: the verbs above, over the PEP-18..21 library.
  - The render contract serializer (versioned JSON from the fold) and its
    schema document.
  - `hub render` (static export) and optionally `hub serve` (read-only local),
    plus a minimal HTML/CSS renderer of the contract.
  - `hub verify` as the audit tool that re-runs the fold's checks and reports
    dropped events (the contract itself carries only provenance keys, no
    verified flags, since only admitted events materialize), plus the
    `folded-ts` and issue-number checks described above.


Rejected alternatives
===================

Web writes canon directly. Letting the web layer sign and push canon would
require it to hold keys and implement the fold and crypto, ending "pure view"
and enlarging the trusted surface. Rejected: writes go through the
CLI/library; the contract is read-only.

A live server as the only web mode. Requiring `hub serve` would break the
offline, no-infrastructure requirement. Rejected: static export is the
baseline, live serve is optional.

Rendering straight from the event log in the web layer. Letting the web fold
and verify itself duplicates the library in JavaScript and risks divergent
materialization across renderers. Rejected: the fold is the library's job and
its output is the single documented contract.

A bespoke CLI grammar unrelated to hbs2-cli/fixme-new. Rejected: reusing the
`bindMatch` grammar and fixme-new's query/template engine is less code and
keeps the forge scriptable in the same idiom as the rest of hbs2.


Open questions
============

- Contract transport: static JSON files vs a documented local HTTP endpoint
  for `hub serve`, and whether both share one schema version line.
- Pinning the index and activity document schemas (only the thread document is
  sketched here) before implementation, under the same `contract` version.
- Diff size policy in the contract: inline threshold, pagination, and when to
  force the renderer to the staged ref instead. Related: whether `hub render`
  (static, no hbs2) materializes referenced attachments into the export or
  drops them to keep it self-contained and small.
- How the interactive-web write path authenticates to the CLI/library
  (local socket, capability token) without becoming a second writer.
- Whether `hub render` output should itself be publishable over hbs2 (a
  content-addressed static site) so the web view is distributed like the repo.
- Identity ergonomics for compose commands (sigil/petname resolution) shared
  with the cross-cutting identity concern deferred by PEP-17/PEP-19.
