PEP-17: hbs2-hub (umbrella over PEP-18..22)

Status: draft, discussion started 2026-06-08; all sub-proposals
        (PEP-18..22) drafted 2026-07-25.
Author: NCrashed (Anton Gushcha)
Depends on: current reflog/LWWRef git model (hbs2-git3), RefChan,
            Mailbox protocol, GroupKeySymm, Sigil identities.
Related: PEP-13 (PQ encryption), PEP-14 (encrypted keystore),
         PEP-16 (barter storage).

This is the umbrella vision. The concrete work is split into the
sub-proposals PEP-18..22 defined below. Nothing here requires a new
network protocol: the system is assembled from primitives that already
exist in hbs2 (reflog, RefChan, Mailbox, group-key encryption, sigils).


Goal
====

Make hbs2 a decentralized replacement for a GitHub/Gitea-style forge.
Each hbs2 node can host repositories, accept issues and pull requests
from anyone (including developers it has never heard of), and expose a
local view of all of that. The forge layer ("hbs2-hub") must be usable
purely from the CLI on top of hbs2; a minimalist Gitea-style web UI is
a thin renderer bolted on later, never a requirement.

Two hard requirements shape the design:

1. Store everything that can live in the repository inside the
   repository itself, so that a plain clone carries the full project
   state (issues, accepted pull requests, labels, discussion) and can
   be browsed offline with no extra infrastructure.

2. The things that cannot live in the repository, namely submissions
   from developers who have no write access (issues and pull requests
   from strangers), need a separate ingress path with its own trust
   model.


Trust model: two tiers
======================

The repository's canonical branch is single-writer: only the owner
(holder of the repo signing key, and the reflog key derived from it)
can push. Issues and pull requests are multi-author and append-mostly.
That mismatch is the whole problem, and it is resolved by separating
the write path for non-owners from the published canonical state.

Tier A, canonical, owner-authoritative.
  Everything the owner has blessed lives as files in the git tree and
  travels through the existing reflog mechanism: issues, pull-request
  metadata, comments, labels, status. This is the "store everything in
  the repo" requirement. A clone sees the full tracker offline. This is
  the model of git-bug / Fossil / radicle-patches / the old fixme.

Tier B, ingress, from anyone.
  A stranger cannot put a file in someone else's repo (no reflog key).
  They need a channel to submit. The right primitive is the Mailbox
  protocol, not a RefChan: see "Why Mailbox, not RefChan notifiers".

The owner's node reads Tier B, triages, and folds accepted submissions
into Tier A. The public sees only Tier A. Spam, drafts, and harassment
in Tier B never become world-visible unless the owner promotes them.


Why Mailbox, not RefChan notifiers
==================================

RefChan looked like the obvious channel for third-party submissions,
but its access control is a closed allowlist. A RefChan head block is
accepted only when signed by the key equal to the RefChan id itself
(the owner key); `authors`, `notifiers`, and `readers` are sets inside
that head block, and `peers` is a weighted map (a HashMap carrying a
weight per peer, RefChan/Types.hs). There is no wildcard and no
self-service:
to let someone post, the owner must edit the head S-expression, add a
`(notifier "<pubkey>")` clause, bump the version, re-sign with the
RefChan key, and re-publish. That is "the maintainer provisions an
account", which is exactly wrong for "a stranger files a bug".

The Mailbox protocol is built for the opposite case. Anyone may
`SendMessage` to a mailbox addressed by a public key, with no prior
membership. Acceptance is gated by a Policy that the mailbox owner
publishes (signed, versioned). The current BasicPolicy is an
allow/deny list with a default action; setting `(sender allow all)`
plus targeted `(sender deny <key>)` yields an open inbox with banning.
Proof-of-work gating is anticipated in the protocol but not yet
implemented (see PEP-21).

Conclusion:
  - Mailbox is the ingress for the open world (Tier B).
  - RefChan stays as an optional multi-maintainer consensus log for the
    canonical state when a repo has more than one maintainer.
  - reflog + in-repo files are the published canonical state (Tier A)
    for the single-maintainer case, or a snapshot otherwise.


Forks are just repo keys
========================

A pull request is, in GitHub terms, "an issue plus where to pull a
branch from". In hbs2 a fork is not a special object: the contributor
clones, commits to a branch, and pushes to their own repo key (their
own LWWRef and derived reflog), because they cannot write to the
owner's reflog. A pull request letter then names the fork's repo key and
the ref to pull, or it carries the diff inline.

A correction the umbrella owes to the reader: block storage is
content-addressed and shared, but a fork is a separate reflog whose
segments are re-exported and re-compressed non-deterministically, so
their block hashes do not match the base's and fetching a fork repo key
re-imports its whole history today, not just the delta. The cheap,
stranger-friendly path is therefore to ship the diff as a delta artifact
(a git bundle `base..tip`) carried inline as an attachment, whose size is
the delta and which needs no git3 change. PEP-20 works this out and keeps
the fork-pointer path for durable forks, with the deterministic-export
git3 work noted as what would make that path cheap too.


The issue/PR letter (overview; full spec in PEP-18)
===================================================

Envelope. A submission is a Mailbox `Message`, i.e. a
`SignedBox (MessageContent s)` signed by the sender's signing key. The
mailbox address is the repository's collaboration mailbox key, declared
in the repo manifest (see "Manifest wiring"). Sender and recipient are
addressed by Sigil (a published identity binding a signing key to an
encryption key).

Encryption. The message body and attachments are encrypted with a
per-message symmetric group key, wrapped for each recipient's
encryption key. The format mandates a non-empty recipient set, so a
submission is readable only by the maintainer(s) it is addressed to.
This is what makes Tier B private by construction; Tier A is where
content becomes public.

Body. The decrypted payload is shown below as an S-expression, in the
same style as manifests and policies. PEP-18 pins the authoritative form:
a typed record signed as binary (a nested inner SignedBox over the
plaintext, distinct from the Mailbox transport signature), with the
S-expression as its readable projection. That inner box is what PEP-19
reuses as the canon author box. Two kinds:

Issue letter:
```
(hub-msg 1)
(kind issue)
(op open)                     ; open | comment | revise | set | close | reopen
                              ;   PEP-18 is authoritative: the op that applies a
                              ;   label is `set`, and it is owner-signed
(target <repo-lwwref-b58>)
(thread <thread-id>)          ; absent on open; on reply, the canonical thread
                              ;   id (sender-computable, see PEP-18/PEP-19)
(title "...")
(labels "bug" "ui")           ; strings, not symbols: a label may hold a space
(reply-to <event-id>)         ; threading
;; body in messageData, or as a part
;; reply-mailbox/reply-sigil ride the transport envelope, not this signed body
```

The notification back-channel closes the loop but must not be published
into canon. The sender's signing key (from the SignedBox) identifies the
author but is not a mailbox address, so the owner needs an explicit
mailbox key plus a sigil (for the encryption key) to reply privately.
Because the signed letter body is folded into canon verbatim, those
fields do NOT belong in it, or a contributor's personal mailbox would be
public in every clone forever. PEP-18 therefore carries `reply-mailbox` +
`reply-sigil` in the transport envelope (the encrypted `messageData`
alongside the signed body, not inside it), and makes them optional: a
drive-by contributor who hosts no mailbox omits them and simply forgoes
notifications. Threading never depends on this, since the canonical thread
id is sender-computable; acknowledgement only conveys the issue number and
status.

Pull-request letter (issue plus where to pull):
```
(hub-msg 1)
(kind pr)
(op open)
(target <repo-lwwref-b58>)
(thread <thread-id>)                  ; as in the issue letter
(title "...")
(source     hbs23://<fork-repo-key>)  ; contributor's fork = own repo key
(source-ref refs/heads/feature)
(source-tip <git-sha1>)               ; commit being proposed
(onto       refs/heads/master)
(base       <git-sha1>)               ; merge-base the branch forked from
```

The diff may be shipped two ways, both using `messageParts`:
  1. a delta artifact (git bundle `base..tip`) as a message `part`, whose
     size is the delta (the default; see PEP-20);
  2. a pointer to the fork repo key, fetched over hbs2, for durable forks
     (fetching re-imports the fork's whole reflog today, so not delta-cheap
     yet; see PEP-20).

Threading. Each later comment or status change is another message to
the same mailbox carrying the same `(thread id)` and a `(reply-to
hash)`. A thread is an append-only DAG of signed messages, like email.
Authorship is the sender's key.

The reserved `messageSchema :: Maybe HashRef` flag is the intended hook
to declare the letter schema by hash once schema support lands; until
then the `(kind ...)` clause in the payload carries it.


Canonical in-repo state (overview; full spec in PEP-19)
=======================================================

Accepted submissions are folded into Tier A as an event-sourced log
materialized into files under an orphan git commit chain at
`refs/hbs2/meta`, one append-only event stream per issue/PR thread. The
fold is deterministic so that any clone recomputes the same materialized
view. The full specification (on-disk layout, two-layer signing,
deterministic fold ordering by owner-assigned `seq`, multi-maintainer
ordering via the optional RefChan, the read contract for renderers,
redaction/compaction) is PEP-19, now drafted at
docs/drafts/pep-19-canonical-in-repo-metadata.md.


Lifecycle / triage
==================

1. Sender builds the letter (`message:create:multipart`), it is signed,
   and sent via `SendMessage` to the repo mailbox.
2. The maintainer's peer applies the mailbox Policy (accept/deny, future
   PoW). Accepted messages land in the per-recipient mailbox merkle
   tree, retrievable via `CheckMailbox` / `MailboxStatus`.
3. The `hub` CLI walks the tree, decrypts (the maintainer holds the
   recipient key), parses the payload, and presents a triage queue.
4. Triage:
   - issue: `hub inbox accept` folds the thread into Tier A and pushes;
     `hub inbox block <key>` adds a deny to the policy.
   - pr: unbundle the delta artifact (or fetch the fork), verify the tip
     against the signed coordinates, review, merge, push canon; record
     status in Tier A and send an ack message to the contributor's own
     mailbox as a notification (PEP-20).
5. Retention: `DeleteMessages` (a signed predicate) and per-message TTL
   prune accepted or spam letters from the mailbox tree.

Notifications are symmetric and serverless: the letter carries a
`reply-mailbox` naming the contributor's own mailbox, and status updates
(acknowledgement with the assigned thread id and number, merge/close results)
are messages sent back to it. NOT a clause of the signed body, which the
correction above says and this paragraph used to contradict: it rides the
transport envelope, so that a contributor's personal mailbox key does not end
up in every clone forever.


Manifest wiring
===============

The repo manifest gains a clause naming the collaboration mailbox, the
same way fixme-new declared its refchan:
```
(mailbox <mailbox-key-b58> hub)
```
The second atom `hub` is a role tag: it marks this mailbox as the one
serving the forge ingress, distinguishing it from any other mailbox a
repo might later declare for a different purpose. A peer wiring the forge
selects the mailbox tagged `hub`. PEP-18 adds a companion
`(mailbox-sigil <mailbox-key-b58> <hashref>)` clause carrying that
mailbox's sigil, so a fresh clone has the encryption key to submit
without a live lookup, and an optional trust-tier tag so a repo can run
more than one inbox (a low-friction known-contributor tier and a
PoW-gated open tier, sized by PEP-21).
Optionally a second clause names a maintainer-consensus refchan for the
multi-maintainer case:
```
(refchan <refchan-key-b58>)
```
A peer that wants to host or mirror the forge subscribes to the mailbox
(and refchan) found in the manifest.


CLI surface (overview; full spec in PEP-22)
===========================================

The hub layer is a library plus a CLI; the web UI only renders what the
CLI/library expose. Indicative commands:
```
hub issue new|list|show|comment|close
hub pr   new --from hbs23://.../branch --onto master
hub pr   list|show|merge
hub inbox            ; triage queue (decrypted Tier B)
hub inbox accept ID  ; fold into canonical Tier A
hub inbox block KEY  ; add deny to mailbox policy
```
fixme-new (archived) already implemented issues over a channel and is
the natural starting point; pull requests are the increment.


Sub-proposals
=============

All five are now drafted (2026-07-25) under docs/drafts/. This umbrella
gives the overview; each linked draft is authoritative where they differ.

PEP-18: collaboration mailbox and letter schema.
  (docs/drafts/pep-18-collaboration-mailbox-letter.md) The issue/PR letter
  format: envelope over a Mailbox Message, the durable inner SignedBox that
  becomes the canon author box, the typed payload for issue and pr kinds,
  threading, attachments, the manifest `(mailbox ...)` / `(mailbox-sigil ...)`
  clauses. Pins the schema so independent implementations interoperate.

PEP-19: canonical in-repo metadata.
  (docs/drafts/pep-19-canonical-in-repo-metadata.md) Canon as an orphan
  commit chain under `refs/hbs2/meta`, the two-layer signing, the
  deterministic seq-ordered fold, numbering, redaction/compaction, and the
  read contract for renderers.

PEP-20: pull-request model.
  (docs/drafts/pep-20-pull-request-model.md) Forks as repo keys, the
  delta-artifact (git bundle) default vs the fork-pointer path with its
  honest efficiency finding, fetch/verify/merge flow, the `revise` op,
  status recording, contributor notification.

PEP-21: triage and moderation.
  (docs/drafts/pep-21-triage-moderation.md) The two enforcement layers
  (peer envelope vs triage content), proof-of-work, rate/quota, envelope
  vs inner-author bans, trust tiers, retention/GC, delegated canon keys,
  and the compaction policy.

PEP-22: hub CLI and web rendering contract.
  (docs/drafts/pep-22-hub-cli-and-render-contract.md) The `hub` command
  surface and the versioned read-only render contract a minimalist
  Gitea-style web UI renders, so the web layer stays a pure view.


Open questions
=============

- Public-readability of the inbox. Chosen: private-to-maintainer by
  construction, public only after fold. Confirm this is acceptable
  (no public "draft PR" visible before triage).
- Activating `messageSchema` (HashRef) vs carrying `(kind ...)` in the
  payload. Start with payload; migrate when schema support lands.
- Anti-spam without PoW today: BasicPolicy allow/deny plus banning;
  PoW deferred to PEP-21.
- Bounded mailbox growth (storage DoS). `(sender allow all)` means the
  maintainer's peer accepts any sender's messages into the mailbox merkle
  tree, so unfiltered spam grows the peer's on-disk state, not just the
  triage queue. TTL and DeleteMessages exist, but until PoW or rate
  limiting (PEP-21) they are reactive. This is a disk-consumption risk
  distinct from queue noise and must be sized when opening an inbox.
- Multi-maintainer canon: when to introduce the RefChan consensus log
  vs staying single-writer with a fold.
- Identity ergonomics: sigil distribution and petnames are out of scope
  here; cross-reference a future identity PEP.
- Fork retention: the owner fetching many forks accumulates branches;
  define a prune policy.
