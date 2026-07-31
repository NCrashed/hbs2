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
(event boxes and the deterministic fold), the PEP-20 PR flow, and the PEP-21
moderation controls. It is pure: no storage, no network, no clock. Whether
reads are served by re-folding or from a cache is the CLI's decision and is
discussed under Reuse. The CLI is a thin driver over that library. A
minimalist Gitea-style web UI is optional and is a pure view: it renders the
contract below and never folds, decrypts, verifies, or writes canon itself.
Everything the web can show, the CLI can show; the web adds presentation, not
capability.


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
   library: letter + canon + fold + moderation              -> refs/hbs2/meta
```

Writes are one-way into the tiers: compose commands emit PEP-18 letters (Tier
B), maintain commands emit owner-signed PEP-19 events (Tier A). Reads all go
through the fold, and the render contract is the serialized projection of its
result. The web view is downstream of the contract and writes nothing.


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
hub verify <repo-key>              ; re-run the fold's checks; report dropped events
```

`hub verify` takes the repository key rather than reading it from canon, and
this is the one place a verb needs an argument that looks like it should be
derivable. It is not derivable: the owner key is the root of the trust chain
(rule 3 of the fold), so a tree that named its own owner would be a tree that
could rename it, and the whole audit would then be an audit against whatever the
tree claimed.

It reads canon out of the git ref directly and talks to no peer, so it runs in a
hook, in CI, and in a clone whose peer is down. It opens no network connection
either, and that takes saying because it is not free: `ls-tree -l` must know the
size of every entry, so in a blobless or partial clone it drives a lazy fetch per
missing blob, through the AUDITED repository's remote urls, `core.sshCommand` and
`credential.helper`. The reader sets `GIT_NO_LAZY_FETCH=1`, so a missing object is
reported as missing instead of fetched, and every bound below is a bound on what
reaches the disk rather than a remark made after it got there.

That it runs in a hook is also the reason its exit code is a contract rather than
"non-zero on trouble": a hook has to tell "the audit ran and found things" from
"the audit could not run", and one code for both told a hook that an unfetched ref
and a tree full of forged events were the same event.

| code | meaning                                              |
|------|------------------------------------------------------|
| 0    | the audit ran and found nothing                       |
| 1    | usage: a bad argument, an unknown verb                 |
| 2    | the audit ran and found drops, anomalies, unreadable files, no `version`, or a file with no version clause |
| 3    | `refs/hbs2/meta` is not here, or is here and broken    |
| 4    | not a git repository, or git could not be run          |
| 5    | the ref is here and does not resolve to a commit       |
| 6    | canon is stamped `(hub-meta N)` newer than this build  |
| 7    | the `version` file is here and does not read           |
| 8    | canon is past the reader's byte bound                  |
| 9    | the tree will not list (a pruned object, a permission) |
| 10   | canon is past the reader's file-count bound            |
| 11   | the tree listing is past the reader's byte bound       |
| 12   | the reader could not run git at all: a local failure   |
| 13   | the `version` file is here and THIS CLONE cannot read it |
| 141  | a closed pipe: 128 plus SIGPIPE, e.g. piping into `head`  |

The last two lines of a clean run are normative, because a hook parses them:

```
maintainers <n> redacted <n>
admitted <n> dropped <n> anomalies <n> unreadable <n> unversioned <n> tree-version ok|missing
```

Every number the exit code counts is on those lines. Two of them are about what
the fold DECIDED rather than what it refused: a tree holding one valid version
file and nothing else otherwise prints all zeroes and exits 0, which reads
exactly like a healthy tracker.

3 to 13 is "could not run", so a hook that only cares about the distinction tests
for that range. 141 is neither: it is what a shell reports for a program a pipe
killed, and it used to be 1, which this table gives to usage errors. It is also
the one code that prints nothing: by the time it happens there is nowhere left to
print. Every other one prints what to do about it on stderr. The numbers
are a contract: a hook branches on them, so they may be added to and not
reassigned.

Two of these are worth reading twice. 3 covers both an absent ref and a ref whose
loose file is corrupt, because git offers no way to tell those apart: `show-ref
--verify --quiet` exits 1 for both and both messages say "not a valid ref". A
fetch is the remedy either way, which is why they can share a code and the advice
names both. And 12 is the one refusal that is not about canon at all: no process
slots, no file descriptors, git gone from `PATH` mid-audit. It used to be reported
as an unreadable file, which exits 2, telling a hook that a local resource limit
was a finding about somebody's repository.

A missing `version` file exits 2 rather than 3-and-up: PEP-19 requires the file,
so its absence is a finding, but the tree still folds, under the OLDEST rules this
build knows. Not this build's newest, which would make deleting one unsigned file
a way to choose which rules canon is folded under.

13 against 7 is the same distinction one level down: 7 says the file is wrong, 13
says this clone cannot read it, which is the ordinary state of a `--filter=blob:none`
clone and not a complaint about anybody's canon.

The report is read by people and by scripts, so two things about its shape are
fixed. A path is printed QUOTED, with a quote inside it escaped, because a path
may contain ": " and the line is `unreadable <path>: <reason>`: unquoted, the
author of a tree chose what the reason field said. And a tool's own words are
printed as an indented block with each line marked `|`, never as a field value,
because this program prints its own advice at the same indent underneath.

Three things the report says that are worth naming, because each was once
silently dropped:

  - A file under `threads/` or `repo/` that is not an event file is reported by
    path, not skipped. Something somebody put in canon is a finding.
  - A path the layout does not have at all is reported the same way. The two
    exceptions are `version` and `index/number.sexp`, which have their own
    readers; the number index is not otherwise read, since the fold assigns
    numbers itself and a file cannot be allowed to change what the fold decided.
  - A blob whose object this clone does not have is reported as that, and not as
    a submodule. It is the one of these that fetching fixes.
  - A path the tree lists TWICE is named. git fsck calls it duplicateEntries; on
    `version` it is a refusal, because taking the first would let the order of
    entries in somebody else's tree pick the rules canon is folded under.

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
hub inbox [--mailbox <key>]        ; decrypted triage queue (Tier B), verified
hub inbox show <msg>               ; one submission, inner author + payload + attachments
hub inbox accept <msg>             ; fold into canon (assigns number), then delete (PEP-21)
hub inbox reject <msg> [< note]    ; delete the letter + optional courtesy note; NO canon event
hub issue close|reopen|label|assign <n> ...   ; owner-signed set events on a folded thread
hub pr   merge <n> [--strategy ...]           ; verify, integrate, merge event (PEP-20)
```

`hub inbox` takes no mailbox key in the ordinary case: it reads the repository
you are standing in, resolves the ingress mailbox from its manifest (PEP-18
`mailboxByTier`), and that same manifest is where the PEP-21 deny-list comes
from. `--mailbox <key>` names one directly and skips both, which makes it the
form that is available before a manifest reader exists and the form that has no
deny-list to apply. It is therefore not a shorthand: a queue read that way shows
letters from banned authors, and anything built on it must not treat "it was in
the queue" as "it may be folded".

Either way the mailbox has to be one the local peer holds. A peer only asks the
network about mailboxes in its own database, so a key it does not have reads as
an empty inbox on this run and on every future one; a reader that cannot tell
those two apart is reporting silence as an answer. The distinction is available
(`mailboxGetStatus` answers `Nothing` for a mailbox the peer does not know) and
has to be used.

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
  "created_at": 1737763200000,        // folded-ts of the opening event: the
                                      //   trusted clock, and what sorting uses.
                                      //   Unix epoch MILLISECONDS, here and in
                                      //   every other time in this document
  "declared_at": 1737763100000,       // author-ts: advisory and attacker-chosen,
                                      //   shown but never ordered by
  "updated_at": 1737849600000,        // folded-ts of the latest event on the
                                      //   thread, including a redaction of one
                                      //   of its events: moderating a thread
                                      //   changes it
  "redacted": false,
  "body": "...",                      // the opening event's own text, which is
                                      //   not a comment and is not in the list
                                      //   below; null when it had none
  "body_part": "<hashref|null>",      // large or binary body, fetched over hbs2
  "part_secret": "<b58|null>",        //   and decrypted with this
  "labels_requested": ["bug"],        // what the AUTHOR asked for on open, never
                                      //   applied: showing these as labels would
                                      //   let a stranger label their own issue
  "comments": [
    { "event_id": "...", "author": "<sign-key>", "canon_by": "<sign-key>",
      "ts": 1737766800000,              // folded-ts: trusted, and what orders
                                        //   the list. The name is historical;
                                        //   it is the same clock as created_at
      "declared_at": 1737766700000,     // author-ts: advisory, shown, never
                                        //   ordered by, exactly as on a thread
      "body": "...", "redacted": false,
      "body_part": "<hashref|null>", "part_secret": "<b58|null>" }
  ],
  "pr": {                             // present when kind == pr
    "onto": "refs/heads/master",
    "base": "<sha1>", "tip": "<sha1>",      // latest surviving open/revise (PEP-20)
    "source": "hbs23://<fork-key>|null",    // the fork to pull from, on that path
    "source_ref": "refs/heads/topic",       // the branch inside it
    "bundle_part": "<hashref|null>",        // or the bundle, on the delta path
    "part_secret": "<b58|null>",            //   and the key that opens it
    "merge_commit": "<sha1|null>",
    "coords_author": "<sign-key>",          // who supplied THESE coordinates: a
    "coords_canon_by": "<sign-key>",        //   maintainer may revise a PR, so
                                            //   this is not always `author`
    "diff": { "format": "unified",
              "availability": "available",  // available | reconstructable | unavailable
              "truncated": false, "text": "..." }
  }
}
```

A renderer sanitizes control characters before it prints. Every text field here
comes from whoever sent the letter, the canon file escapes only what its own
grammar needs (a backslash, a quote, and the three whitespace characters), and
everything else travels verbatim, as it must: canon carries what was signed. So
a title with an ESC in it is a title with an ESC in it, and printing that to a
terminal hands a stranger the terminal. Strip or escape C0 on the way out, in
every renderer, including the plain-text one.

A triage loop checks its own key against the maintainer set ONCE before a pass,
not per letter. `UnauthorizedForRepo` is a retry, because in a repo with several
maintainers another folder can do what this one cannot; for a single folder
whose delegation has been withdrawn it is the same answer for every letter
forever, and a loop that only reads the per-letter refusal will spin over the
whole mailbox on every pass and fold nothing.

A renderer must tolerate a `reply-to` that names no event it can see, or one
that belongs to another thread. Nothing validates it, deliberately: the
reference may point at a letter the owner chose not to fold, and rejecting
such a comment at the bridge would strand a legitimate submission for good.
Render it flat rather than failing or inventing a parent, and leave the
reporting to `hub verify`, which lists dangling and cross-thread references
along with the dropped events.

That last part is not implemented and needs a fold change, not a CLI one: the
fold has no anomaly for a `reply-to` that dangles or crosses a thread, so `hub
verify` cannot list what nothing records. Adding one is an admission-rule change
under PEP-19 and therefore a `hub-meta` bump once anything has published canon;
until then it is free, which is an argument for doing it before the first
release rather than after.

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
  - fixme-new's SQLite `object(o,w,k,v)` materialization is NOT ported, and
    this entry is here to say so rather than to plan a port.

    Two reasons, and the second is the one that matters. The first: hbs2-hub
    has no persistence today and no dependency that could give it one. The
    library is letter, canon, fold and manifest, all pure functions returning
    `Either`, which is what lets the whole admission surface be property-tested
    without a filesystem. Adding a store is a decision about the CLI, not about
    the library, and it is not made here.

    The second: fixme-new advances a last-applied-`seq` marker and applies what
    is past it, and PEP-19 forbids that shape whatever the store is. The fold is
    one pass over the fully sorted set, so a file with a lower `seq` arriving
    later inserts into the middle and can admit an event an earlier pass dropped
    as dangling. A marker would leave that event dropped for good, in one clone
    and not in another, with nothing reporting anything. Canon minted by the
    triage bridge never has that shape, since the bridge will not mint a reply
    before its thread is in canon; canon written by anything else may, and
    `hub verify` has to reach a verdict on that canon too.

    What replaces it depends on a thing not yet decided: whether reads run in a
    long-lived process or a one-shot one. In a daemon the answer needs no store
    at all, since holding the `FoldResult` and dropping it when the canon tree
    hash changes costs nothing and cannot go stale across a restart that takes
    the cache with it. In a one-shot CLI something has to survive the process,
    and then the rule is that it caches the OUTPUT of a fold and never a
    position inside one. Two keys are safe and either is enough alone: the whole
    result against the canon tree hash together with the repository key and the
    `hub-meta` version, which are the fold's only other inputs; or per file
    against that file's own content hash, which is the one that stays useful
    when the tree grows. Measured on this code, parsing the files is 72% of a
    cold fold, signature verification 19%, and the ordered pass these rules
    describe 2.4%, so the per-file key is where the time actually is. If a
    persistent store is wanted, `db-pipe` is what the rest of this project uses.

What changes from fixme-new: the channel is Mailbox ingress plus reflog canon
(not a RefChan), events carry the PEP-19 two-layer signing, and the PR verbs,
the render contract, and its web export are new.


What exists today vs what must be built
====================================

Exists today:

  - hbs2-cli's `bindMatch` S-expression command machinery and the Mailbox
    message create/read commands the compose/triage verbs build on.
  - fixme-new's query DSL and templates to port. Its SQLite materialization is
    not portable here and is not counted: see Reuse.
  - The PEP-18..21 library itself, which is pure and has no store, so nothing
    below inherits one by default.
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

    The reading half of this exists: canon comes out of the git ref, the fold
    runs over it, and drops, anomalies and unreadable files are reported with a
    non-zero exit. What is not built is the part that needs canon to have been
    WRITTEN by something, which is the accept path, so the verb has so far only
    been run against trees assembled by hand. Reading canon is also the piece
    both remaining branches stand on: the accept path needs the view it mints
    against, and the render contract is a projection of the same fold.


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
