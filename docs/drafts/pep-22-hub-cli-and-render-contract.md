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
  hub pr new/revise    -->  hub log / verify      <--  hub issue close/label/redact
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
| 2    | the audit ran and found drops, anomalies, unreadable files, misnamed files, no `version`, or a file with no version clause |
| 3    | `refs/hbs2/meta` is not here, or is here and broken    |
| 4    | not a git repository                                   |
| 5    | the ref is here and does not resolve to a commit       |
| 6    | canon is stamped `(hub-meta N)` newer than this build  |
| 7    | the `version` file is here and gave no usable version   |
| 8    | canon is past the reader's byte bound                  |
| 9    | the tree will not list, whether git said so or this reader did |
| 10   | canon is past the reader's file-count bound            |
| 11   | the tree listing is past the reader's byte bound       |
| 12   | no answer out of git: it would not start, or it is gone |
| 13   | the `version` file is here and THIS CLONE cannot read it |
| 14   | reading canon, or one object in it, cost more than the reader will spend |
| 15   | git ran and did not answer                              |
| 16   | the tree listed and its files went out of step with git |
| 17   | the peer does not hold this mailbox (`hub inbox`)       |
| 18   | the peer is running and stopped answering (`hub inbox`, `hub issue new`) |
| 19   | no signing key here for the author (`hub issue new`)     |
| 20   | the peer answered and would not store the message (`hub issue new`) |
| 21   | no signing key here for the canon identity (`hub inbox accept`, `hub issue close`) |
| 22   | the letter named is not one this node can read                |
| 23   | the bridge would not bless it: triage refused, canon untouched (also an owner verb signed by a key the repository does not authorize) |
| 24   | canon could not be written                                    |
| 25   | the event rendered to a file this build could not read back    |
| 26   | canon holds no such thread (`hub issue show`, `hub pr show`, `hub issue close`) |
| 27   | git would not build the bundle or would not answer (`hub pr new`) |
| 28   | the objects a pull request proposes are not usable             |
| 29   | canon holds no such pull request (`hub pr merge`)               |
| 30   | the merge was not recorded, and canon is unchanged             |
| 31   | the delegation was not written, and canon is unchanged         |
| 32   | already folded into canon (`hub inbox reject`)                  |
| 33   | nothing was deleted (`hub inbox reject`)                        |
| 34   | the mailbox has no policy, or it will not read                 |
| 35   | the policy was not changed (`hub block`, `hub unblock`)          |
| 36   | the deny-list will not read (`hub ban`)                         |
| 37   | the proof-of-work a mailbox charges could not be solved in time |
| 141  | a closed pipe: 128 plus SIGPIPE, e.g. piping into `head`  |

3 to 16 are `hub verify`'s own; 17 and 18 belong to `hub inbox` and are added
above rather than folded into an existing number, because the numbers are a
contract and reassigning one is worse than spending one. Both used to be 1,
which this table gives to usage errors, so a hook could not tell "you mistyped
the key" from "run `hbs2-peer mailbox create`" from "the peer is wedged" -- three
situations with three different remedies and one exit code between them. `hub
inbox` also exits 2, with the sense the row above gives it: the queue was read
and part of the mailbox tree was not, so the list is incomplete in both
directions.

19 and 20 are the same correction on the write side, where it matters more: every
failure of `hub issue new` used to exit 1, so "you mistyped a flag" and "your
letter is in nobody's mailbox" were one number, and the caller had been told
nothing that would make them keep watching for it. 18 is REUSED rather than
duplicated, because a peer that stopped answering is the same situation and the
same remedy whichever verb met it; widening a row is not reassigning one. An
oversized field stays at 1: it is a bad argument, which is what 1 is for.

The last two lines of a clean run are normative, because a hook parses them:

```
maintainers <n> redacted <n>
admitted <n> dropped <n> anomalies <n> unreadable <n> unversioned <n> misnamed <n> tree-version ok|missing
```

Every number the exit code counts is on those lines. Two of them are about what
the fold DECIDED rather than what it refused: a tree holding one valid version
file and nothing else otherwise prints all zeroes and exits 0, which reads
exactly like a healthy tracker.

3 to 16 is "could not run", so a hook that only cares about the distinction tests
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
names both. And 12 is the one refusal that is not about canon at all: git not on
PATH, no process slots, no file descriptors, or the git this reader did start
gone before it answered. It used to be reported as an unreadable file, which
exits 2, telling a hook that a local resource limit was a finding about
somebody's repository.

12, 15 and 16 are three states of one tool and are worth keeping apart, because
only one of them is worth retrying. 12 is no answer out of git at all: it would not
start (not on PATH, no process slots, no file descriptors), or the one this
reader did start is gone before it answered, which is what an OOM killer inside a
hook's cgroup looks like from this end. 15 is git running and saying nothing, which a
retry buys another wait of: reachable with a FIFO at `.git/refs/hbs2/meta`, where
`rev-parse` answers and `show-ref` blocks on open, and which used to exit 12 with
an invitation to try again. 16 is git running and saying something this build
cannot follow, which is a version disagreement to report rather than anything
about the repository; it used to exit 9, "the tree will not list", printed about
a listing that had been read whole and parsed.

A LISTING that stalls stays at 9 and does not become 15, and the line is drawn
there on purpose. 15 says a retry is pointless and the thing to look for is a
FIFO or a dead mount, which is true of a ref lookup that cannot finish and false
of `ls-tree`: `ls-tree -r` walks the tree AS A TREE, so a commit whose subtrees
all point at one subtree costs 64^12 traversals of 116 KB of objects, and there
the answer really is compaction, which is what 9's advice says. A stalled ref
lookup can only be the machine; a stalled listing can be the tree.

14 is a bound on the WALK and on one object inside it, which the three listing
bounds cannot give: a tree of
45000 paths whose entries share one subtree is five objects and 172 KB on disk,
and its listing is 3.6 MB against a 102 MB bound and 45001 records against
200000. Every listing bound passes, and the walk that follows is bounded by none
of them. It matters because this verb is meant for a hook, where an expensive
walk is a push blocked by a repository that fits in an email.

Blobs are read through one `cat-file --batch` for the whole walk. The same tree
took 82 seconds when it was one process per path, and about six minutes at the
file bound; it is seconds now. The budget stays, because a fast path and a bound
answer different questions: the fast path decides what a walk usually costs, the
bound decides what it may cost at worst, on a tree chosen by whoever is being
audited.

Batch reading is not free of its own hazards, and the reader carries the cost of
that. A `cat-file --batch` reply is NOT self-delimiting: git writes the size from
the object's header and then the whole body, and a loose object can be
self-consistent and lie -- a header of `blob 10` over two megabytes hashes to its
own name, so `cat-file -s` answers 10 and `ls-tree -l` prints 10. Reading the
announced number leaves the rest of the body in the pipe as the answer to the next
object, which is one path's content and verdict reported under another's name.
Checking the echoed object id and the newline after the body does not close that:
both are bytes the lying body can contain. What closes it is that a reply is not
over until the stream is quiet, so the reader checks that nothing is pending
before it accepts a blob. Delivering such an object needs access to the object
store -- `upload-pack` will not pack one -- so this is not a remote attack; it is
the reason the reader does not trust a size it was told.

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
printed as an indented block with each line marked `|`, because this program
prints its own advice at the same indent underneath, and an unmarked block puts a
stranger's text exactly where a line telling the reader what to run goes.

That rule governs the REFUSALS, which are one message with advice under it. It
cannot govern the per-file findings, and saying "never as a field value" claimed
it did: a finding is one line of a list, the report promises one `Doc` per line,
and a tree may have a thousand of them, so a block there puts a paragraph in the
middle of a list a thousand times over. What git said about one unreadable file
is therefore a field, escaped onto one line like every other stranger's text.
Nothing follows it at that indent, which is what the marker exists to prevent.

Five things the report says that are worth naming, because each was once
silently dropped:

  - A file under `threads/` or `repo/` that is not an event file is reported by
    path, not skipped. Something somebody put in canon is a finding.
  - A path the layout does not have at all is reported the same way. The two
    exceptions are `version`, which has a reader of its own, and
    `index/number.sexp`, which has none: nothing opens it, because the fold
    assigns numbers itself and a file cannot be allowed to change what the fold
    decided. It is passed over ONLY WHEN IT IS A BLOB, though. A gitlink at that
    path is reported like any other, since a skip by path alone made the one file
    in the tree nothing reads into the one place a submodule could sit unnamed.
  - A blob whose object this clone does not have is reported as that, and not as
    a submodule. It is the one of these that fetching fixes.
  - A path the tree lists TWICE is named. git fsck calls it duplicateEntries; on
    `version` two entries that DIFFER are a refusal, because taking the first
    would let the order of entries in somebody else's tree pick the rules canon
    is folded under. Two identical entries collapse and the file is read: it is
    one file listed twice, the tree is still reported as malformed, and refusing
    would be refusing over a question that has one answer.
  - An event file whose name is not the one PEP-19 gives it is named, and the
    event in it is still folded. Two things are checked here: that the name is
    twenty digits, a hyphen and an event-id, and that the event-id is the one the
    file holds.

    THREE are not, and the reason is cost rather than capability. `seq` and the
    scope (a `delegate` under `threads/` rather than `repo/`) are ordinary fields
    of the canon box, and the thread directory a file sits in is an ordinary
    prefix of its path; `unboxChecked` is exported and is already called outside
    the fold. What stops the reader doing it is that opening a canon box verifies
    a signature, and the fold opens every one of them a moment later: doing it
    here means verifying canon twice, in a verb whose whole point is running
    inside a pre-receive hook. The place that has already paid for it is the
    fold, and the fold is not shown a path. Giving it one is the change, and it
    is not made here.

    An earlier version of this paragraph said the canon box is one "only the fold
    opens", which is not true of this tree and was the wrong reason for the right
    decision.

    Reported and not dropped, deliberately: no signature covers a path, so a
    reader that refused a misnamed file would show less than canon holds and
    disagree with every other clone over a byte nobody signed.

Contribute (Tier B letters, PEP-18; needs the target mailbox + a sigil):

```
hub issue new  --target <repo> --title ... [--label ...] [--body <text>|-]
hub issue|pr comment --thread <thread-id> [--reply-to <event-id>] --body <text>|-
                                               ; one verb under two names: the op
                                               ;   carries no kind, and no --target
                                               ;   because the thread names the repo
hub issue close|reopen|label <thread-id> ...   ; NOT BUILT: the request letter (PEP-18
                                               ;   carries the ops; no verb composes one)
hub pr new     --target <repo> --onto master --from <ref>   ; builds a bundle (PEP-20)
hub pr revise  --thread <thread-id> --onto <ref> --from <ref>
                                               ; author-of-record (PEP-20): new
                                               ;   coordinates, no title and no body
hub updates                        ; NOT BUILT: needs the reply-mailbox back-channel
hub identity set-reply-mailbox <key> --sigil <hashref>      ; NOT BUILT, same reason
```

Every value is behind a flag and nothing is positional (except `hub issue new`'s
inherited positional form), because a repo key, an author key, a sigil and a
thread-id are all thirty-two bytes of base58: a swap is a correctly signed
letter claiming the wrong author, or a reply in a stranger's thread, and a
signed box cannot be taken back. A comment with no body is refused rather than
sent: the body is the whole content of the op, so an empty one spends a seq to
say nothing.

`hub pr new`/`revise` default to the delta-artifact path: they build a git
bundle `base..<ref>` and attach it (PEP-20), computing `base`/`source-tip`
from the local repo and signing them into the letter. They are one code path
with two contents, which is what keeps a revision proposing the same shape of
thing an open did. `--fork <fork-key>` uses the fork-pointer path instead, and
is not built.

A revision is refused unless the author of record signs it, and the bridge is
deliberately stricter than the fold here: the fold also admits a maintainer's
revision, but a maintainer revising through the LETTER path would be acting as
somebody else. That refusal happens on the maintainer's machine, and with the
ack path unbuilt there is nothing to tell the sender, so the verb says it up
front instead.

`hub updates` is how a contributor reads the
back-channel: the owner's acknowledgements and status updates arrive as
messages in the contributor's own `reply-mailbox` (PEP-18/20), and this verb
reads that mailbox and correlates each ack to the thread it sent.

Maintain (Tier A, owner or delegated maintainer; PEP-19/20):

```
hub inbox [--mailbox <key>] [--repo <key>]   ; decrypted triage queue (Tier B), verified;
                                   ;   --repo applies that repository deny-list
hub inbox show --mailbox <key> --message <hash> [--repo <key>]
                                   ; one submission whole: inner author, title, body,
                                   ;   attachments measured, and what triage makes of
                                   ;   it. --repo also asks canon whether it is folded
hub inbox accept <msg> [--keep]    ; fold into canon (assigns number), then drop the
                                   ;   letter from the mailbox (PEP-21 fold-then-
                                   ;   delete). --keep leaves it. The MAILBOX key
                                   ;   signs a drop, so a delegate folding with --as
                                   ;   cannot make one: the fold stands, the letter
                                   ;   stays, and the report says which happened
hub inbox reject <msg>             ; refuse it here; NO canon event, and no courtesy
                                   ;   note either -- the ack path is unbuilt (PEP-18)
hub issue close|reopen --repo <key> --number <n> [--note <text>] [--as <key>]
                                   ; owner-signed status event on a folded thread; the
                                   ;   status follows from the op, so no separate set
hub issue label --repo <key> --number <n> --label <l>... | --clear
                                   ; owner-signed set; REPLACES the labels, and the
                                   ;   empty set has to be said (--clear), not implied
hub redact --repo <key> --event <event-id>
                                   ; display-level hide (PEP-19): canon keeps the bytes
hub pr   merge <n> [--strategy ...]           ; verify, integrate, merge event (PEP-20)
```

`assign` was listed here and is not built: it would be an owner-signed set of an
`assignee` attribute, which the fold already carries as an ordinary attribute,
and nothing but the verb is missing.

Every value is behind a flag because a repository key, a delegate key and an
event-id are all thirty-two bytes of base58, so position cannot tell them apart
and a swap would be signed. `--as <key>` names a delegate (PEP-21) and defaults
to the repository key; `--number` is what `hub issue list` prints, resolved to a
thread through the same fold every reader runs, so a number canon does not hold
is a refusal here rather than an event minted against a thread that does not
exist.

`hub inbox` takes no mailbox key in the ordinary case: it reads the repository
you are standing in and resolves the ingress mailbox from its manifest (PEP-18
`mailboxByTier`). `--mailbox <key>` names one directly and skips that, which
makes it the form available before a manifest reader exists.

THE DENY-LIST IS NOT IN THE MANIFEST, and this section used to say it was,
which contradicts PEP-21 and the implementation both. It is hub state, local to
the node doing the triage: a file under the XDG data directory keyed by
repository, written by `hub ban` and read by `hub inbox accept`. That is
deliberate rather than pending -- a triage ban is one maintainer's decision
about what THEY will fold, not a fact about the repository that every clone
should inherit -- and PEP-21 says so under "Two enforcement layers".

What is true of `--mailbox`, and worth keeping: a queue read before a manifest
reader exists is a queue read without whatever the manifest would have said, so
nothing built on it may treat "it was in the queue" as "it may be folded". The
accept path applies the deny-list itself, which is where that guarantee
actually lives.

THE QUEUE APPLIES IT TOO, given `hub inbox --repo <key>`. The list is keyed by
repository and the queue reads a mailbox, so without a repository there is no
list to apply -- but a banned author's letters then sit in the queue looking
like work, and a maintainer reads a list they were told was filtered. So the
repository is a flag on the queue as well, and when it is absent `hub inbox`
says in its footer that no deny-list was applied, rather than showing a filtered
queue and an unfiltered one that look alike. Accept still applies the list
itself: the queue is a view, and a view is not an authorization.

Either way the mailbox has to be one the local peer holds. A peer only asks the
network about mailboxes in its own database, so a key it does not have reads as
an empty inbox on this run and on every future one; a reader that cannot tell
those two apart is reporting silence as an answer. The distinction is available
(`mailboxGetStatus` answers `Nothing` for a mailbox the peer does not know) and
has to be used.

A DROP IS NOT A DELETE, and both verbs that make one say so. `DeleteMessages`
writes a tombstone: the queue stops showing the letter, and the blocks stay on
disk, because nothing in this project walks a mailbox and frees bytes. What
protects a folded letter's attachments is therefore not the tombstone but
PEP-21's pin -- a purge, when somebody writes one, must skip any tree a folded
canon event references. Accept dropping the letter it folded is the first caller
that obligation has.

Two distinctions matter here. `hub inbox reject` acts on an unfolded Tier B
submission: there is no canon thread to close (the fold never ran), so it emits
no canon event. It was specified as a `DeleteMessages` plus an optional courtesy
note to the sender's `reply-mailbox`; the delete is built and the note is not
(the ack path is unbuilt), so what it does today is drop the letter and say
nothing to its author. It refuses a letter canon already holds, and not because
the tombstone would differ -- accept writes the same one -- but because
rejecting says the letter was not taken and canon says it was. Closing an
already-folded thread is
the separate `hub issue close --number <n>`, an owner-signed close event on an
existing canon thread.

WHICH FORM A VERB TAKES IS THE VERB, NOT THE CALLER. This section used to
specify capability-based dispatch: one `hub issue close` that emitted an
owner-signed event if the caller held an authorized canon key and a request
letter otherwise, with `--request` to force the letter. That is not what is
built, and it should not be: a verb whose meaning depends on which keys happen
to be in the local keyman is a verb nobody can read from a script, and the two
outcomes -- an event in append-only canon, and a letter in somebody else's
mailbox -- are too far apart to pick by inference. So `hub issue close|reopen|
label` are owner-signed only: no signing key for the identity is exit 21, and a
key the repository does not authorize is exit 23, in both cases with canon
untouched. The letter form is still what PEP-18 carries (`close`, `reopen` and
`set` are letter ops the bridge honours through `hub inbox accept`), but this
build has no verb that composes one; a contributor asks in a comment.

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
Render it flat rather than failing or inventing a parent. Reporting it belongs
with the dropped events in `hub verify`, and does not happen yet:

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

    This is built, and this paragraph used to say half of it was: canon comes
    out of the git ref, the fold runs over it, and drops, anomalies and
    unreadable files are reported with a non-zero exit. It said the verb had
    only ever run against trees assembled by hand, which stopped being true when
    the accept path landed: the suite now folds letters through accept and
    audits what it wrote. What remains of this bullet is the render contract,
    which is a projection of the same fold.


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
