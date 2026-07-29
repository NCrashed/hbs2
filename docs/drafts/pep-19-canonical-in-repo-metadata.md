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
labels, and assignees are all just attributes. PEP-19 generalizes it to
threads with comments and to the two-tier trust split.

There is no fixed open/close/label schema baked into storage. Structured
fields (status, labels, assignees, title, milestone, number) are attributes
set by `set` events under last-writer-wins by a monotonic weight, with
multi-valued ones (`labels`, `assignees`, and the plural is the name: an
attribute that can hold a set is spelled as one everywhere, so nothing has to
remember which spelling normalizes) encoded as a comma-separated list in
sorted order with no spaces, so that the same set of labels always produces the
same bytes and therefore the same event-id. Which names are multi-valued is
part of that rule, so the list belongs with the encoder rather than in each
writer. Attribute names are lowercase, for the same reason and enforced the
same way: `Labels` would sail past set canonicalization as though it were a
scalar and then sit in the attribute map as a second, near-invisible
attribute. Note where the obligation falls: a writer normalizes before
signing, and the fold does NOT normalize on the way in. It cannot, since the
value is inside a signed author box, and it must not drop the event either, so
an unnormalized value is admitted and reported as an anomaly for `hub verify`.
Free-form
discussion is carried by `comment` events that accumulate in order. New
field kinds are added by convention without a storage migration.

Every mutation is one event:

  - open      start a thread (issue or pr), carries kind, title, body
  - comment   append discussion to a thread
  - revise    PR-only: update the thread's proposed coordinates (new tip),
              author-of-record only (see Pull-request canon and PEP-20)
  - set       assign a structured attribute (status, labels, assignees, ...)
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
the human issue `number`, the folded-at time, the `origin` letter hash, the
`honours` request box, and, for events with encrypted attachments, the message
group secret (see Attachments in public canon). Signed by an authorized canon
key.

Honouring a request re-authors it under the owner's clock, which means two
different requests honoured on one tick can produce identical bytes, and so one
event-id: the declared time is overwritten and the signature is deterministic. A
close, a reopen and a close again on one tick do the same. A loop that reads its
clock once per batch is the ordinary implementation and not a bug to report, so
the honour path walks the stamp forward a millisecond at a time until the bytes
are new, up to a small bound. The alternative was refusing, which meant either
deleting a letter whose request was never carried out or stopping the loop over
a millisecond. What it will not do is walk past the `folded-ts` ceiling: at the
top of the range there is no millisecond to move to, and minting one past it
would produce an event the fold drops.

Threading needs no canon-box normalization: a thread id and a reply-to are
canonical event-ids, and an event-id is the hash of an author box, which the
sender constructs, so the sender computes them directly (see Thread identity).

What is signed, exactly. A signature says "this key signed these bytes", never
"this key signed an event", so each of the two payloads is written with the
domain it is signed for in front of it: the CBOR that gets signed and hashed is
`[domain, payload]`, with `domain` the `word32` `0x48423241` ("HB2A") for an
author box and `0x48423243` ("HB2C") for a canon box. A verifier that finds any
other domain refuses the box. Without this, a signature made over some other
record with a coincidentally identical encoding is a valid signature here: a
sum-constructor tag is an ordinary small CBOR integer, so anything of the shape
[small int, hash, int] signed by the owner for any purpose was a signed
redaction of any event. The domains are assigned once and never reused or
renumbered, because they are inside the signed bytes and therefore inside every
event-id.

Everything reachable from either payload is frozen for the same reason, and is
listed here rather than left to an implementation to read out of one. Two that
disagree about any line of it produce canon they cannot verify for each other,
and there is no repair afterwards, because an event-id hashes the bytes.

The author payload is a sum, encoded with its constructor's position as the tag,
in this order: `open`, `comment`, `revise`, `set`, `close`, `reopen`, `merge`,
`redact`, `delegate`, `revoke`. New ops are appended and none is ever reordered
or removed. The fields of each are in the order the schema below lists them, and
`redact`, `delegate` and `revoke` begin with their target repository like an
`open` does. A `redact` needs it for a reason the others do not: every remaining
op names a thread, a thread is an `open`, and an `open` names its repository, so
they are bound transitively. A `redact` names an event-id and nothing else, and
would otherwise be the one op a maintainer of two repositories could sign in one
and have take effect in the other.

PR coordinates are a product, in this order: `source`, `source-ref`,
`source-tip`, `onto`, `base`, `bundle-part`, with the first and last optional.

The canon payload is frozen too, and it is the one it is easiest to forget,
since no event-id covers it: every canon signature ever made does. Its fields
are `target`, `event-id`, `seq`, `number`, `origin`, `honours`, `folded-ts`,
`part-secret`, with `number`, `origin`, `honours` and `part-secret` optional. The
`part-secret` is raw key bytes, not a serialised key type, for the reason given
under Attachments.

`honours` is the author box of the request this event carries out, when it
carries one out, and it is the identity a rewrap cannot change. `origin` names
the message, and re-encrypting the identical request to the same mailbox
produces a new message with a new hash; the box inside is signed and is the same
in every envelope. Without `honours` in canon, a restart forgets which requests
have already been honoured in the only way that survives a rewrap, and anyone
holding the old ciphertext could have the same close applied again on every
resend. See "Replay, rewrap, and deduplication".

The `target` there is the repository the BLESSING is for, and it is first
because it is the first thing a reader has to ask. The author box saying which
repo it was written for is not enough, because what canon counts is the
blessing: one person delegated in two repositories is an ordinary arrangement,
and any file they sign in one of them is byte for byte valid in the other, both
signatures verifying and the canon box referencing its own author box. Without
this field the stamp is therefore spent before the fold gets as far as noticing
that the CONTENT belongs elsewhere, which hands one repository's high-water mark
to another, and in a hostile version hands over `maxBound`, after which nothing
can be minted in the victim again, the `revoke` that would answer it included.

The kind of an `open` is a two-constructor sum in the order `issue`, `pr`.

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

Domain separation. Every signed payload carries the domain it was signed for
as the first field inside the signed bytes. Ed25519 signs bytes and says
nothing about what they mean, so a signature is bound to a record type only by
the accident that no other record has the same encoding, and here the same key
signs several: an author box, a canon box, a git3 LWWRef on every push, a
sigil. That accident does not hold up. A sum constructor tag is an ordinary
small CBOR integer, so `serialise (7, <hashref>, 5)` is byte for byte
`serialise (redact <hashref> at 5)`: any record of shape [small int, hash,
int] that the owner signs for any purpose was a signed redaction of any event
in the repo. Nothing has that shape today (the LWWRef escapes only because its
third field is optional, and so an array rather than an int), but that is an
accident of four unrelated types across two packages, and one field type
changing anywhere would open it silently.

The domain is a large constant, assigned once per payload type and never
reused or renumbered. It is inside the signed bytes and therefore inside every
event-id, which is why it has to exist before any canon does: an event-id
hashes the whole box, so this cannot be added afterwards without invalidating
every signature ever made. A reader that meets a correctly signed payload
carrying another domain reports it apart from both a forgery and a newer
schema: the signature is real and the shape is right, but these bytes were
signed as a different kind of record.

The guarantee is one-directional, and this is worth stating because it will
otherwise be read as more than it is. Canon payloads are protected from
signatures made elsewhere. A canon signature lifted INTO another hbs2 protocol
is not protected by anything here, because the LWWRef, the sigil, the mailbox
envelope and the refchan all sign untagged payloads: they are older than this
rule and have live users, so tagging them is a format break for each of them.
No collision exists today in that direction either, and for the same reason as
before: it is arithmetic about CBOR shapes, not a property anyone arranged.
Closing it means the domain wrapper moving into the shared signing helper,
which is a change to every one of those formats and belongs to whoever
schedules that break. Until then the rule reads: a hub payload cannot be
forged from a foreign signature, and a foreign payload may yet be forgeable
from a hub signature.

The signature scheme must be deterministic. The event-id hashes the whole
author box, signature included, so a randomized scheme would give the same
content a different id on every signing, and two properties would go with
it: a sender could no longer compute the thread-id before delivery (PEP-18),
and dedup by event-id would stop recognizing a resent letter. Ed25519 is
deterministic, which is why this works today; any replacement (PEP-13) must
be too, or the id has to move to hashing the signed payload alone.

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

One event per file. The two authoritative artifacts are the base64-encoded
signed boxes; the remaining clauses are the readable projection.

```
;; --- the version of THIS FILE, reported by a reader and obeyed by nobody ---
(hub-event 1)

;; --- authoritative signed boxes (verification operates on these) ---
(author-box <base64 of serialised SignedBox>)   ; signed by the author
(canon-box  <base64 of serialised SignedBox>)    ; signed by a canon key

;; --- readable projection of the AUTHOR box (regenerated, not trusted) ---
(author    <sign-pubkey-b58>)
(kind      pr)                     ; on open: issue | pr
(op        open)                   ; open|comment|revise|set|close|reopen|merge|redact|delegate|revoke
(target    <sign-pubkey-b58>)      ; on open: the repo the author box was signed for
(title     "a title")              ; on open
(created   <word64>)               ; the author's declared time, Unix epoch MILLISECONDS
(labels    "bug" "ui")             ; on open: what the author ASKS for, never applied
(body      "...")                  ; inline text, or (body-part <hashref>) for large/binary
(thread    <thread-id>)            ; replies only, absent on open; the canonical
                                   ;   thread id (an event-id), sender-computable
(reply-to  <event-id>)             ; canonical id of the event replied to, replies only
(source     "hbs23://<fork-key>")  ; on open/revise of a pr: the fork-pointer path
(source-ref "refs/heads/...")      ;   and the four coordinates that always travel
(source-tip "<sha1>")
(onto       "refs/heads/master")
(base       "<sha1>")
(bundle-part <hashref>)            ;   or the bundle, on the delta path
(set       "status" "closed")      ; on set: the assignment, name AND value quoted
(merge-commit "<sha1>")            ; on merge
(merged-into  "refs/heads/master")
(redacts   <event-id>)             ; on redact: the target event
(delegate  <sign-pubkey-b58>)      ; on delegate: the maintainer key authorized
(revoke    <sign-pubkey-b58>)      ; on revoke: the maintainer key withdrawn
                                   ;   both carry (target) too: a delegation is
                                   ;   for ONE repository

;; --- readable projection of the CANON box (regenerated, not trusted) ---
(seq       <word64>)               ; globally monotonic order weight
(number    <int>)                  ; on open only: human #N, owner-assigned
(origin    <message-hash>)         ; the Tier B letter this event came from, if any.
                                   ;   Present on a folded letter, and on an
                                   ;   owner-authored event that honours a
                                   ;   request (the letter is its provenance).
                                   ;   Absent only when nothing prompted it.
(honours   <event-id>)             ; on an event that carries out a request: the
                                   ;   author box the requester signed. Survives
                                   ;   a rewrap, which (origin) does not.
(folded-ts <word64>)               ; Unix epoch milliseconds, UTC, owner's clock at fold
                                   ;   refused above 4102444800000 (2100-01-01Z):
                                   ;   the next stamp is clamped to be no lower
(canon-by  <sign-pubkey-b58>)      ; owner or delegated maintainer
(canon-target <sign-pubkey-b58>)   ; the repo this BLESSING is for; named apart
                                   ;   from the author box's own (target)
(part-secret <group-secret-b58>)   ; only on events referencing encrypted parts:
                                   ;   the PARTS group secret, NOT the one over
                                   ;   messageData: publishing that one would publish
                                   ;   the sender's reply address (PEP-18)
```

Every string in the projection is quoted, including an attribute NAME, which is
the one that looks like a symbol and is not: a name arrives from whoever sent
the letter, the fold admits one that is not a vocabulary word (it reports it and
nothing more), and a name of `a)(canon-box ...` written bare puts a clause of
the sender's choosing into a file that has two valid signatures in it. The op,
the kind, keys, hashes and the two boxes are the only bare symbols, and every
one of those is generated here from a fixed vocabulary, a base58 alphabet or a
base64 one.

The boxes are base64 while every key and hash in the file is base58, and the
mismatch is deliberate. Base58 earns its keep on short things people read aloud
and copy by hand; a box is kilobytes, and base58 is a base conversion over a big
integer, so decoding it is quadratic in the length: 12 ms at 4 KiB, 213 ms at
16 KiB, two and a half seconds at 64 KiB, paid by every fold in every clone for
every event. Base64 is linear and a third smaller, and nobody reads a box.

Five rules make those bytes a format rather than a habit, and a reader and a
writer that disagree about any of them cannot exchange canon.

One clause per line, and no line wrapping. Several forms on one line read back
as one nested form in this s-expression dialect, so the line breaks are part of
the grammar; and a printer that wraps at a page width would make the bytes of an
event depend on somebody's idea of a terminal, in a tree that is
content-addressed.

Strings are escaped so that they read back: a backslash, a double quote, and
the three whitespace characters are written `\\`, `\"`, `\n`, `\r`, `\t`. This
is not cosmetic. A title comes from whoever sent the letter, and one unescaped
quote does not spoil a display line, it ends the string early and everything
after it is re-read as whatever it happens to look like, in a file that has two
valid signatures in it and can never be rewritten.

Every other control character is written `\xNN` followed by the empty escape
`\&`. That one is not about parsing, it is about the terminal: a canon file is
read with `git show` and `cat` far more often than with a parser, and a raw ESC
in a body is a terminal escape sequence, so a title could reposition the cursor
and rewrite what a maintainer sees while they decide whether to sign it. The
trailing `\&` is load-bearing: a numeric escape swallows any hex digit that
follows it, so ESC followed by `5` would otherwise read back as one character.

A reader un-escapes those and no others, which is the half of the rule a writer
cannot state alone: escaping is only a round trip if both sides agree on the
alphabet. Anything else after a backslash is malformed rather than interesting,
and a reader is free to refuse the file.

A writer bounds how many escape sequences it emits, and truncates the projection
when it would exceed the bound. Un-escaping costs a parser time superlinear in
how many escapes a file holds: on a file shaped like a real event it is 0.06 s
at 512 escapes, 0.19 s at 1024, 0.76 s at 2048, 3.0 s at 4096, and at the file
size bound, where every other byte of the body is a backslash, eighty seconds.
That last file is HONEST: a 32 KiB body is inside every limit PEP-18 sets, and a
body of newlines is what a code block is. So the bound cannot be a refusal on
the reading side alone; the writer has to stay under it, and it can, because the
projection is never read back and the boxes above it carry the content in full.
A truncated projection says so, in a marker that carries no escape of its own.
The bound is 1024 escapes per file, which is where the measured cost crosses two
tenths of a second and which a fold pays once per event.

Every hash-shaped field an event carries has to be a hash: a thread, a
`reply-to`, a `body-part`, a `bundle-part`, a redact target. A hash reference is
a byte string on the wire and nothing about the type says thirty-two of them, so
this is not implied by the size limits below, which measure text. Most of these
fields are compared against something that exists and a wrong-length one merely
fails to match, but `reply-to` is deliberately compared against nothing (see the
render contract), so it was the one field that could carry fifty kilobytes past
every gate, into a signed author box, and into a file no reader would accept
again, with the bytes inside the signature and inside the event-id where nothing
can reach them. It is one rule over all of them rather than a list of
exceptions, and the box-size budget below assumes it: four kilobytes of slack
covers a handful of hashes only while a hash is a hash.

The same rule covers the repository an op names (`target`, on open, redact,
delegate and revoke), because a key is a byte string on the wire too. Those four
are the ops whose target the fold compares against the owner, so a wrong-length
one merely fails to match there, which is a drop and not a refusal, and by then
the file is already written.

The bridge refuses to mint an event whose text is over a limit, and canon does
not care what those limits are: they are triage policy, so two hubs may draw
them differently without disagreeing about canon, and a letter over one is kept
rather than deleted. They exist because this is the last gate before something
is in every clone forever. The fields that have one, with the numbers this
implementation uses: an inline body 32 KiB, a title 512 bytes, an attribute
value 4 KiB, an attribute name 128 bytes, a git ref or sha or fork locator 512
bytes, one label 128 bytes and at most 32 of them, and one encrypted part 64
MiB. Every one is measured in BYTES of UTF-8, since what they bound is what a
relay carries and what a clone keeps, and a limit in characters is a limit an
attacker chooses the units of.

A reader refuses a file before parsing it, on three bounds: its size in UTF-8
bytes, the number of FORMS it opens, and the size of each encoded box. Reading
is the first thing anyone does with a tree and the last thing they can refuse to
do, so the cost of a file has to be bounded by something cheaper than parsing
it: an s-expression parse is superlinear in the number of forms, so one file of
a few hundred kilobytes, written by anyone who can write to a clone, otherwise
stops every fold and every `hub verify` everywhere, before a single signature is
checked.

Forms, not parentheses, and forms means every way of opening one. A body is a
string literal and code inside one has brackets, so counting every `(` in the
file was a bound on the contributor rather than on the attacker: a hundred and
fifteen of them anywhere in an issue made the file unreadable for good. And
counting only `(` was no bound at all either, because the parser makes a list
per bracket pair, a list per quote, quasiquote or unquote, and a list per
non-empty LINE. Each spelling of the attack was cheaper than the last: square
brackets cost four minutes a file, and bare atoms one to a line cost over a
minute for eighty kilobytes, growing sixfold per doubling, with no punctuation
at all.

The count therefore covers `([{`, the three quoting marks, and a newline; it
skips what is inside a string literal; and it tracks comments, because a quote
inside a comment would otherwise put the counter into a string it is not in and
stop it counting anything after that. Counting lines as well as brackets
double-counts an honest file, which a bound can afford: a real event is a dozen
or so lines against a limit of a hundred and twenty-eight.

The third bound is the escape count, and it is the one the other two do not
imply: see "Strings are escaped so that they read back" above for the
measurements and for why the writer, not only the reader, has to respect it.

And the size bounds are DERIVED from the limits the bridge mints under, not
chosen beside them. A reader bound picked on its own is a reader that refuses what its
own writer produced, which is what happened here: with a token capped at 4 KiB
the bridge would mint the 32 KiB inline body this document promises, write the
file, and answer "too large" on reading it back. The derivation is: a box is the
sum of the field limits above plus framing; a token is that base64-encoded; a
file is two tokens plus the projection, which repeats the same text and can
double under escaping. With the numbers above that gives a box of 48 768 bytes,
a token of 65 024, a file of 235 776, and forms are capped at 128 because the
schema has a couple of dozen. A second implementation should derive them the
same way rather than copy these.

The projection is never read back. A reader takes the two box clauses and the
version, and regenerates everything else from what it unboxed. Believing
`(seq 5)` over the canon box would mean believing an unsigned line of text
written by whoever last touched the file.

`(hub-event N)` is reported and never obeyed. It vetoes neither the tree nor the
file. Not the tree, because the clause is unsigned text and one line saying
`(hub-event 4294967295)` would otherwise make a repository unreadable for every
clone, permanently and for free. And not the file either: the two boxes are
self-describing CBOR, so a version this reader does not know does not make them
unreadable, and refusing over it would throw away a `seq` the reader could see
perfectly well. The same holds when the clause is missing or is not a number at
all: the file is read and the version is reported as unknown. A veto there is a
way to make a signed event go missing by editing one unsigned line of the file
it sits in, and missing is harder to notice than wrong. What a newer schema can really do is put content inside a box
that an older build cannot decode, and the fold answers that itself, by dropping
the event and keeping its stamp. The tree's own `(hub-meta N)` is the one that
governs everything, because it names the admission rules that produced the
canon, and folding under rules a reader does not know is how a clone quietly
disagrees with every other clone.

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

A sender may write a reply before the letter it answers has been folded: the
event-id is computable either way, so nothing stops them. Folding is the
part that is ordered. The fold is a single ascending pass over `seq`, so a
reply must carry a higher `seq` than the event it answers, and an event
minted for a thread that is not in canon yet can never be admitted: it is
dropped as dangling, and re-folding does not repair it, because the ordering
is already fixed by the numbers on the two events.

So the rule is on the folding side: a maintainer folds the opening letter
before the replies to it, and the triage bridge refuses a reply whose thread
it does not yet see rather than minting an event that is doomed. A reply
arriving out of order is not lost, it simply waits in the mailbox until its
thread exists. Acknowledgement (the owner replying to a letter's
back-channel) is only needed for the human `number` and status, never for
threading.


On-disk tree layout
==================

The tree under a `refs/hbs2/meta` commit:

```
/
  version                      ; exactly "(hub-meta 1)\n", a clause like every
                               ;   other one in this tree
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

Its reader has bounds of its own, and they are not the event file's: the index
has an entry per thread, so an event's clause bound would cap a repository at
128 issues, and a reader hands the parser one line at a time rather than the
whole file, so the cost stays linear. A writer stops at the same bounds instead
of writing past them, since a file this build refuses to read back is worse than
a shorter one: a truncated index is a prefix of the full one and is still
correct for every number it holds, and refusing an index is not refusing the
repository.

There is no materialized issue view stored in the tree. Canon is the event
log; the view is recomputed (see Read contract). This keeps canon minimal
and its determinism trivial to guarantee.


Deterministic materialization
============================

The fold is consensus, and its rules are versioned. Determinism is not only
"the same build folds the same log the same way": two clones running
different builds must agree, or the same canon means different things to
different readers. So any change to the admission rules, to the set of drop
reasons, or to how state is derived from an admitted event is a format
change, and MUST bump the `version` file in the meta tree (`hub-meta N`).
A reader that meets a higher version reports it rather than folding, since
its answer would differ from the publisher's.

Adding an op is the same kind of change and follows the same rule. The
version file exists for this; the obligation is what was missing.

Two things this does not yet say, both deliberate and both due before a
release rather than after.

The first is what a v2 build does with v1 canon. Refusing to fold a HIGHER
version is specified and implemented; the other direction is not. A v2 build
meeting canon written under v1 rules has three options (fold it under v1 rules
and render a v1 view, refuse it, or migrate by rewriting), and which one is
right depends on what the rule change was. Until there is a second version
there is nothing to decide between, but the decision has to be written down
with the bump, not discovered afterwards, and per-version rules imply keeping
the v1 fold rather than editing it in place.

The second is that `hub-meta` is 1 while the rules are still moving. Changes
that would normally require a bump (a new drop reason, a change to the signed
encoding, a new admission rule) are being made at 1 on the grounds that no
canon exists yet, which is true and has an expiry: the number is free only
until something writes a tree somebody else might read. The first release that
ships a writer bumps it, and every change after that pays the full price.

Ordering:

  - Primary key: `seq`, a globally monotonic integer the owner assigns at
    fold time. Because it is assigned by a single authority in the order
    events are admitted, it is a total order over all canon events, and the
    commit-chain packaging is irrelevant to the result.

  - Tie-break: `event-id` (content hash), lexical. This guarantees a total
    order even in the degenerate case of a duplicated `seq`.

  - Last tie-break: the content hash of the canon box, lexical. Two events
    that agree on `seq` and `event-id` are the same author box blessed twice,
    which is one event: the first in the order is admitted and the second is
    dropped as a duplicate. Which one is first therefore decides whose `seq`,
    `number` and folded-at the thread gets, and the first two keys cannot see
    any of that, since all of it lives in the canon box. Without this third
    key the result would depend on the order the events were enumerated, and
    two folders reading one tree could disagree about the next `number` to
    mint.

    A canon written as specified cannot contain such a pair, because the file
    name is `%020d(seq)-<event-id>` and both copies are the same path. The
    rule is here because the fold is defined over a set of events, not over a
    tree that is assumed well formed, and `hub verify` (PEP-22) has to reach
    a verdict on canon somebody else wrote.

With all three keys the fold is deterministic regardless of enumeration order
or how commits batched the events.

A note on the version, since this document says in several places that changing
any of this bumps `hub-meta` and the number has stayed at 1 through every change
so far. Both are true and they are not in tension: the rule is about canon that
EXISTS. Nothing has published a tree, so there is no clone to disagree with and
nothing to migrate, and the number will be 1 for the first one written. The
first published tree is what closes the window; after it, every line here costs
a bump and a fork in the fold.

The set of refusals is consensus too, and finer than a reader might expect on
purpose. "Kind and payload disagree" and "the stamp is unusable" are each four
different things, and which one it was cannot be recovered from the event
afterwards: a reader looking at a dropped pull request cannot tell whether the
coordinates were missing, unreachable, or on a thread that is not a PR at all,
and an operator whose counter has stalled needs to know WHICH counter. So they
are separate reasons, and adding or merging one is a `hub-meta` bump like any
other admission change.

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
  4. For owner-authored ops (`set`, `close`, `reopen`, `merge`, `redact`), the
     author box signer
     is also an authorized canon key. A stranger cannot author these; their
     letter equivalents are only requests (see Folding). (`number` is not an
     op: it is a canon-box field on the `open` event, so it is owner-signed
     by construction.)
  4a. Payload rules that are admission rules, and so are consensus like the
     rest of this list. An `open`, a `delegate`, a `revoke` and a `redact` name
     THIS repository in their author box, and EVERY event names it in its canon
     box:
     otherwise an event authored or blessed for another repo could be lifted out
     of that repo's canon and replayed here with valid signatures. For an `open` that means
     somebody else's words appearing here; for a `delegate` it means an owner
     who has two repositories signing, in one of them, a maintainer set for
     both. A `pr` open carries
     coordinates and an `issue` open does not. A PR's coordinates, on an `open`
     and on every `revise`, name at least one of a fork to pull from or a
     bundle to fetch. A `revise` and a `merge` are PR-only, so both are dropped
     on an issue thread. An event that breaks any of these is dropped rather
     than admitted, because canon would otherwise hold a proposal nobody can
     obtain, or a thread whose kind and payload disagree.
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
      merge  -> if state[e.thread].kind /= pr: drop  ; a pr-only op on an issue
                state[e.thread].pr = merged(e)
                state[e.thread].attrs.status = merged ; the merge IS the status
      redact -> if e.redacts in byid: byid[e.redacts].rendered = hidden ; content stays
                state[thread_of(e.redacts)].updated = e.folded_ts       ;   in tree
      delegate -> maintainers.add(e.key)              ; admit() enforced rule 5:
      revoke   -> if e.key /= owner_key:              ;   owner-key-signed only
                    maintainers.remove(e.key)         ; revoking the owner is a no-op
```

Three details in there are easy to miss and are all consensus. A `merge`
sets `status` itself rather than waiting for a separate owner-signed `set`:
the alternative leaves canon claiming a merged PR is open until a second event
arrives, and the render contract reads `status`. Revoking the OWNER key is a
no-op, because the owner key is the root of trust and cannot be delegated away,
so it cannot be withdrawn either; the event is admitted and changes nothing. And
a `redact` moves the `updated` clock of the thread it hides an event in, because
moderating a thread changes it: without that a thread whose comment was just
withdrawn sorts as untouched since before the redaction, and `updated` is what
orders every list a renderer shows.

Because `delegate`/`revoke` are applied in the same `seq` order that governs
admission, an event is judged against the maintainer set in force AT ITS `seq`,
and a later `revoke` never retroactively invalidates events the key blessed
before it. This is why compaction must never drop a `delegate`/`revoke` event
(see Retention).

Read that literally, because the difference is load-bearing. The fold has no
idea when anything was signed: it knows the `seq` the signer chose. A revoked
maintainer who picks a `seq` below their own revocation therefore passes rule 3
and can bless a `redact`, an `open` with a number, or a comment on an old
thread, for as long as they hold the key. What stops that today is not the fold
but publication: only the holder of the reflog key can write `refs/hbs2/meta`,
so a withdrawn maintainer has nothing to put their file into (see the two-tier
model above). Anything that widens publication, a shared RefChan in particular,
loses that guarantee and needs a real rule: the obvious one is that an event's
`seq` must be above everything already in canon, which is an admission rule and
so a `hub-meta` bump, and which a partially-fetched clone cannot evaluate. It
is not in this version. PEP-21 states the same limit in the same words.

A revocation also cannot be undone by replaying the `delegate` that preceded
it. Moving the old event later in the log means writing it again, and its
event-id is the hash of its author box, which the `seq` is not part of: the
replayed event has the id it always had, the fold has already seen that id,
and it is dropped as a duplicate. Reviving the key needs a fresh `delegate`
signed by the owner, which is the intended path. Note what this property rests
on: keeping `seq` out of the author box (see Event schema). Were the ordering
folded into the signed content, the same delegation at a new position would be
a new event, and revocation would only hold as long as nobody kept a copy.

Two safety rules the pass encodes. A reply-class event whose `thread` is not
an admitted `open` is dropped with a warning (a dangling reference, which
`hub verify` reports, PEP-22), so a comment on a rejected or never-folded open
cannot create a phantom thread. And `redact` locates its target through the
`byid` index of already-materialized events; a `redact` naming an event that
is not in it is DROPPED, with its own reason, rather than admitted as a no-op.
Dropping is the better of the two because it is visible: `hub verify` names
it, where a silent no-op leaves a redaction that reported success and did
nothing. The pass is `seq`-ordered, so the target is present whenever the
`redact` was minted after it, which is the ordering the bridge enforces: it
refuses to mint a `redact` for a target not yet in canon.

`delegate` and `revoke` naming a key that changes nothing (revoking a key that
was never delegated, or a mistyped key) are admitted as written and spend a
`seq`, reporting success while doing nothing. This is the same shape as the
unknown `redact` above, which is dropped precisely so that it is visible, and
it should be resolved the same way when the rules are next opened; it is left
as it is for now because the fold's maintainer set is derived, not stored, and
checking membership at admission time is a rule change rather than a fix.

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
secret those parts were encrypted with in the owner-signed canon box
(`part-secret`). That is the PARTS secret, which PEP-18 requires to be separate
from the one over `messageData`, and the separation is what makes publishing it
safe: it opens the attachments the fold is publishing anyway and nothing else.
Publishing the message's own secret instead would also open the letter's
transport envelope, and with it the back-channel clauses that PEP-18 keeps out
of the signed letter precisely so a contributor's personal mailbox address does
not reach every clone. That leak would also be retroactive, since the
ciphertext is held by every peer that ever relayed the mailbox.

The two secrets are the same type and the same length, so nothing about the
bytes says which one a caller is holding, and handing over the wrong one is the
unrecoverable case above. The API therefore keeps them apart by type: the canon
field takes a distinct part-secret type, built where a part tree is actually
decrypted, and the fold bridge additionally refuses to mint when the secret
offered for the parts equals the secret over the message payload. That check is
the only mechanical defence available, the two values being otherwise
indistinguishable.

The secret is supplied per part, inside the evidence that says the part was
opened, and never as a value of its own beside the parts. This is the difference
between a claim about something and a claim about nothing. A free-floating
secret can be well-formed, usable and not the message secret while opening none
of the parts it is published next to, and every check above would pass while
canon acquired a permanent reference to bytes nobody can read. A bridge that
does no IO cannot verify a decryption, so what it can do instead is refuse a
secret that is not attached to an act of opening a named part: the only way to
say "this part is here" is to say how big it is and what opened it, and neither
is a thing a caller has without having looked. What remains unprovable is
whether the named secret really was the one used, and nothing in a pure function
can close that; what is closed is the gap between "a secret was supplied" and
"a secret was supplied for this part".

One event carries one `part-secret` while an `open` can reference two trees, a
body-part and a PR bundle. That is sound only because both come from the same
Mailbox message and therefore from the same parts key. Since the evidence is now
per part, the two can disagree, and an event whose parts were opened with two
different secrets is refused: there is no field that can carry both, on this hub
or any other, so the letter is unfoldable rather than merely refused here.

Neither the type nor the checks are the end of it, and the residue is worth
writing down. Comparing against the message secret catches one secret handed
over twice; it cannot catch them handed over crossed, and on an owner-native
event there is no message secret to compare against at all, which is not the
impossible case it looks like, since an owner-authored `open` or `comment`
carries a body-part like any other. The structural fix is to stop taking a
secret from the caller at all and derive it from the group key embedded in the
part tree itself: that is the one value which by construction cannot be the
message secret, because it is the key the tree was encrypted under, and it is
also the one that cannot be a secret for some other part. That is a change to
the layer that fetches trees, so it is recorded here as the intended shape
rather than done: until then the per-part evidence is the best available and the
type is what keeps the question visible.

For that check to run, the caller has to hand over the message secret with the
parts, so the letter-side evidence always carries it and there is no value on
that path meaning "I have nothing to say about attachments". A reader of a
letter holds the message secret by definition, having read the letter with it,
and the distinction matters because the alternative reading is a wedge: a letter
naming a part its message does not carry is the sender's doing and must not stop
the loop, while a caller that never wired the parts through is a bug that must.
Only an owner-native event, which has no message behind it, can say the second
thing.

The field is raw key bytes (`Saltine.encode` of the group secret), not a
serialised key type: canon is forever, and the CBOR shape of a byte string is
pinned by CBOR itself, while the shape of a key type is pinned by whatever the
crypto library derives for it. A writer that put something else there produces
canon whose attachments never open, and by the paragraph below there is no
repair, so a size check on the way in is worth the line it costs.
A canon reader fetches the referenced tree over hbs2 and decrypts it with
`part-secret`. Note the attachment blocks are hbs2 storage objects fetched
over the same transport as the git data, not git blobs in the meta tree;
making attachments git-native blobs (a self-contained clone) is a later
option, not required here. Owner-native events with no attachments carry no
`part-secret`.

This is a refusal, not a best effort. A folder that cannot supply the secret
must decline to mint rather than write the event without it, and the letter
stays in the mailbox to be retried. The reason it cannot be repaired later is
the same one that rules out re-encrypting: the reference is inside a signed
author box, so a follow-up event cannot amend it, and once the Mailbox message
has been deleted (PEP-21 retention) the secret does not exist anywhere. The
cost of getting this wrong is therefore permanent and silent: canon looks
complete, the fold admits the event, and every clone sees an attachment that
will never open.


Folding Tier B letters into Tier A
================================

This is the bridge that produces canon. When the owner accepts a PEP-18
letter, the hub maps it to exactly one event, preserving authorship. One
letter, at most one event: the letter's message hash is recorded as the
event's `origin`, and a folder skips a letter whose hash already appears as
an origin in canon (that is how a triage loop re-reading a mailbox after a
restart avoids folding the same letter twice). A letter that produced two
events would make that check ambiguous, since only one of the two could
carry the origin. Honouring a request (`close`, `reopen`, `label`) is the
same rule and one more: the owner-authored event that results carries the
requesting letter's hash as its `origin` AND the requester's own author box as
its `honours`, and a second honour is refused on either. Both are needed because
they catch different resends. The message hash catches a triage loop re-reading
the mailbox after a restart. The box catches a rewrap: re-encrypting the
identical request produces a new message with a new hash, so the origin does not
see it, and honouring re-authors the content under the owner's clock, so the
event-id does not either. Only the second is an attack, and without it anyone
who kept the ciphertext could have the same close applied again on every resend,
forever. A
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
letter (op close|reopen|set)      ->  a REQUEST, not canon by itself
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

  - Owner-authored: `set` (status, labels, assignees, milestone), `merge`,
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
signed events to that publisher. PEP-21 specifies the signing-versus-
publishing capability split and the routing.

Who stamps `seq`, precisely. `seq` and `number` live inside the canon box, so
whoever signs that box chooses them, and a delegated maintainer signs canon
boxes. Uniqueness is therefore a property of the deployment, not of the
format: it holds while one publisher does the stamping, and two maintainers
stamping concurrently can pick the same values. The fold stays deterministic
regardless, because the total order is `(seq, event-id)` and the tie-break
settles it, but the result is not what either maintainer intended, so
`hub verify` reports duplicate and non-monotonic `seq`/`number` (PEP-22).

Folded-ts is clamped, not trusted. The publisher stamps `folded-ts` from its
own clock, and a clock corrected backwards would write canon that every
verifier flags forever, with no later event able to fix a signed timestamp. The
stamp is therefore the greater of the folder's clock and the `folded-ts`
already in canon: monotonic by construction, and taken from canon rather than
from node-local state, exactly like `seq` and `number`. Clamped rather than
refused, because refusing would let one maintainer with a fast clock stop every
other folder until real time caught up, which is the cursor failure below in
another costume.

The cursor can be poisoned by `seq` as well as by `number`, and the `seq`
variant is worse. A stamp of `maxBound - 1` on any event leaves the next
`seq` at the top of the range, and from then on EVERY op is refused, not only
`open`: the folder cannot mint a comment, a close, a redaction, or the
`revoke` that would withdraw the delegate who did it.

Who can do that is bounded, and in three steps rather than two, because the
middle one is where the difficulty lives. See the stamp rule below for the
statement; the consequence is this. A currently authorized maintainer can strand
the cursor, which is the standing exposure of delegating to anyone: one hostile
or broken maintainer, who can already do worse, and whom `hub verify` names. A
key the log never authorized can do nothing at all, which matters because the
fold is a public function over whatever files a tree holds and `hub verify` runs
it on canon somebody else wrote. And a key whose delegation has been WITHDRAWN
can only move the cursor a bounded step past where it already is, which is what
keeps `revoke` a remedy rather than a formality: without that bound the owner's
only answer to a mutinous delegate is the re-stamping compaction described
below, and with it the answer is the revocation itself.

A known bound on the same weakness. The fold refuses a `seq` or `number` of
`maxBound`, since the next mint is the maximum plus one and would wrap. It does
not refuse a stamp that is merely absurd: a maintainer who signs
`number = maxBound - 1` leaves the counter one mint from the end, and from then
on every `open` is refused because there is no number left to assign. Closing
that needs a rule about what a stamp may be RELATIVE to the ones before it, and
a relative rule needs the log that came before, which a clone that has fetched
part of canon does not have; two nodes would then disagree about which events
count. Until someone needs it, the exposure is one hostile or broken
maintainer, who can already do worse, and `hub verify` names them.

`folded-ts` is bounded differently, because the damage is shaped differently.
The counters wrap only at the very top, and each mint moves them by one; this
one is carried forward by `max`, so a stamp ANYWHERE in the range pins every
later event to it, on every node, and every time the render contract shows is
derived from that field. What bounds it is therefore a ceiling rather than the
top of the range: `folded-ts` above 2100-01-01T00:00:00Z (`4102444800000`) is
refused. That is an admission rule, so it is part of `hub-meta 1` and cannot be
changed afterwards; it is expressible as one because it is absolute, needing no
knowledge of the events before it, which is exactly what the counters' rule
would need. The triage bridge refuses to stamp the same value from the other
side, since the clamp means the value it is about to write is the greater of
its own clock and canon's, and minting past the ceiling would produce an event
the fold drops. Inside the range the pin remains possible, and the recovery is
the re-stamping compaction below.

An event that was not admitted still spends its POSITION. This is not an
optimization to skip. Admission is not final: canon arrives a file at a time,
and a reply dropped as dangling is admitted the moment the opening event it
names turns up. And even where admission is final, the file occupies that
`seq`, so minting into it leaves canon holding two events there for good, with
every last-writer-wins attribute between them settled by a hash rather than by
time. The rule is therefore about the canon box, not about the event: the `seq` of a
stamp signed by an authorized key is spent whether or not the event was applied,
including one whose author content this build cannot decode at all, which is
precisely the older build reading a newer schema's `open`.

The `seq` and nothing else. The other two stamped values are spent only on
ADMISSION, and for the same reason in two costumes: neither is a position in the
log, so an event that was refused took neither.

The human `number` is a label on a thread that exists. An `open` aimed at
another repository, blessed by a maintainer of this one, must not burn one: it
never showed anyone a number, and at the top of the range it would strand the
counter and abort every later triage run. The two failure modes are not equal. A
number handed out twice is a reported anomaly on a field that is displayed and
nothing more; a number that cannot be handed out at all stops the repository.
The same rule keeps two builds agreeing: an event whose content one of them
cannot read is not admitted by either, so neither spends its number. What a
reader does record from an event it could not read is that the number was SEEN,
which costs nothing and is what makes the duplicate visible when the other build
hands it out again.

The `folded-ts` is the floor the next mint is clamped to. A refused file stamped
at the ceiling would pin every later stamp in the repository to the year 2100,
which is precisely the harm the ceiling exists to bound, so a refused file does
not raise the floor. What the floor is taken over is the highest admitted stamp
and not the last one: assigning rather than maximizing let an admitted event
with a lower stamp lower the floor, and a folder that rebuilt from canon after a
restart then disagreed with the one that had been running.

Which leaves the anomaly. "The clock went backwards" is a different question
from "how high has the clock been", and it is asked of the previous ADMITTED
event, not of the floor. Sharing one number for both made a refused file raise
the bar, so a strictly increasing log of admitted events reported itself as
going backwards.

Before any of that, the stamp has to be for THIS repository: the canon box names
it (see the frozen fields above), and a blessing signed for another repo spends
nothing here however authorized its signer is, because a maintainer of two
repositories is authorized in both. The same rule is what keeps a number
remembered from an unreadable file from being remembered at all when the file is
not one this log counts: a number recorded off a stranger's file becomes a
duplicate reported against the next honest `open`, with the honest maintainer's
key beside it.

Who may spend one is then three-valued. A key authorized at that point in the
log spends its stamp outright. A key the log never authorized spends nothing, and
that exclusion is the load-bearing one: counting a stranger would let anyone who
can write one file into one clone strand the cursor at the top of its range,
with the owner unable to mint even the `revoke` that would answer it. A key
whose delegation has been WITHDRAWN spends a stamp only if its `seq` is within a
small window of the cursor.

That middle case is not a compromise, it is the shape of the problem. The honest
case it exists for is a delegate who minted from a view built before the
revocation, whose event the publisher then wrote: that leaves a refused file at
a `seq` that is nonetheless taken, handing it out again puts two events there,
and no ill intent is needed. Such a `seq` came from a cursor, so it is always
within a step or two of one. Counting a withdrawn key WITHOUT that bound is what
makes `revoke` stop being a remedy: the same key can then stamp `maxBound - 1`
and pin `folded-ts` at the ceiling, and the owner has no answer but compaction.
With the bound, a run of such files can creep the cursor by the window each
time and no further. This implementation uses sixteen.

What stays restricted to admitted events, besides the number, is the `origin`
set: an event that was not applied folded no letter.

Compaction must retain what the fold's own checks read. Two of them look at
canon rather than at the event in hand, and compaction is what can take that
canon away.

The first is the origin set. One letter folds to at most one event, and the
check that enforces it asks whether canon already holds an event with this
letter's hash as its `origin`. That set is built from admitted events, so
compacting away an honoured `close` removes the only record that its letter
was honoured, and the same letter honoured again after a restart produces a
second event. The second is `redact`: a retained redaction whose target has
been compacted away names an event that is no longer there, and the fold drops
it, silently un-redacting the content the redaction existed to withhold.

The retention predicate therefore keeps, in addition to what Retention already
lists: every event that is the target of a retained `redact`, and every event
whose `origin` a still-reachable letter could repeat. The first is exact. The
second is not, since a letter's hash is not derivable from canon, so in
practice it means retaining the `origin` field of compacted events even when
their content is dropped, as a set the fold can still consult.

A purge of attachment trees needs one more precondition, and it is not about
retention rules but about what the running build can see. The set of parts canon
references is collected from the events the fold could READ, and an event from a
newer schema is exactly the one whose references are unavailable: its author
content does not decode here, so its `body-part` and its bundle are invisible.
Deleting trees on that basis would delete what a newer build can see canon
pointing at. A canon-aware purge is therefore only safe on a fold that reported
no undecodable event; meeting one means upgrade first, and the drop report names
them.

Recovery, since the bound is reachable in practice. A delegated maintainer who
stamps `number = maxBound - 1` leaves every later `open` refused, and
compaction cannot help by dropping the event, because an `open` is exactly what
compaction must retain. What does work is that compaction rewrites canon under
fresh canon boxes: the owner re-stamps the retained events with a fresh `seq`
and `number` sequence, keeping every author box verbatim so that no event-id,
and therefore no thread identity, changes. That is a history rewrite of
`refs/hbs2/meta`, which the fetch refspec already forces, and it is the same
operation compaction performs for size. `hub verify` reports a stamp far above
its predecessor, so the situation is noticed before the counter runs out rather
than after.

Making this structural rather than conventional would mean splitting the
event in two: a delegate signs only the author box and proposes it, and the
publisher, holding the reflog key, is the sole signer of canon boxes. That is
the cleaner model and costs a type, but it also makes every delegate's action
a round trip through the publisher, which is only worth it for a repo that
actually runs several maintainers. Left as an option; until then the rule is
one publisher per repo, checked by audit rather than enforced by the types.

Note the related case: canon can hold two files at one `seq` when one of them
is refused, since the stamp rules above spend a `seq` only for a canon box an
authorized key signed. Determinism is unaffected for the same reason as above,
and the bridge avoids minting doomed events in the first place.

What the audit says about it is worth being exact about, because the two halves
are reported differently and on purpose. The duplicate-`seq` anomaly is about
two ADMITTED events, which is a publisher having minted twice; a refused file
sharing a `seq` with an admitted one is not an anomaly of anybody's event, and
attributing it to the admitted one would let a stranger who dropped a file into
a clone put a warning on somebody else's work. It appears in the drop report
instead, which carries the `seq` and the key that signed it, so an operator sees
both entries at that `seq` and can tell which is which.

A repo names the consensus channel with the optional manifest clause from
PEP-17:

```
(refchan <refchan-key-b58>)
```


Read contract for renderers
=========================

The hub layer is a library plus CLI; the web UI (PEP-22) is a pure view over
what they expose. The contract:

  - `readEventLog  :: CommitChain -> [Event]`   parse the meta tree
  - `materialize   :: [Event] -> Map ThreadId ThreadState`  the fold above
  - `ThreadState` fields: number, kind, title, status, labels, assignees,
    author, created_at, updated_at, comments[], and for PRs the source/onto
    coordinates and merge result.

The CLI maintains a local materialized cache in SQLite, ported from
fixme-new (`/home/user/dev/hbs2-legacy/fixme-new/lib/Fixme/State.hs`): its
single `object(o, w, k, v)` table with last-write-wins by weight, plus a
`comments` table. The port is nearly verbatim; the monofold and the
LWW-by-weight upsert carry over. The cache is node-local and never canonical:
it is always reproducible from the event log, so it is gitignored and
rebuildable.

What does NOT carry over is the `scanned`-style table recording the last applied
`seq` as a resume point. That works for a log that only ever grows at the end,
and canon does not: files arrive in whatever order a fetch produces them, and an
`open` with a lower `seq` can turn up after the reply that names it has already
been refused as dangling. Resuming from "the highest seq applied" would leave
that reply refused forever, in a cache nobody would think to distrust.

The rule an incremental cache has to follow is therefore: a batch of new files
may be applied on top of the cache only if every one of them has a `seq` above
every `seq` already applied. Otherwise the cache is dropped and rebuilt from the
whole log, which is cheap because the fold is cheap and the tree is local. Any
cleverer scheme has to answer what a late arrival does to admission, and
admission is not a function of one event. A renderer reads either the
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

Attachments are the sharpest case. A redacted event keeps its `part-secret`
in canon, so the encrypted tree it points at stays decryptable by everyone
who has cloned the repo. Hiding the body in the renderer does nothing to the
attachment, which is the same rotate-do-not-redact rule stated for inline
secrets, only more so.

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
