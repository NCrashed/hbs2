PEP-17: hbs2-hub (umbrella over PEP-18..22)

Status: draft, discussion started 2026-06-08.
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
(the owner key); `authors`, `notifiers`, `readers`, and `peers` are all
sets inside that head block. There is no wildcard and no self-service:
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
owner's reflog. Because git objects are content-addressed and shared
blocks already live in storage, a fork barely duplicates data: only the
delta transfers when the owner fetches it. A pull request letter then
only needs to name the fork's repo key and the ref to pull, or it can
carry the patch inline.


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

Body. The decrypted payload is an S-expression, in the same style as
manifests and policies. Two kinds:

Issue letter:
```
(hub-msg 1)
(kind issue)
(op open)                  ; open | comment | close | reopen | label
(target <repo-lwwref-b58>)
(thread <thread-id>)       ; fresh id on open, existing id on reply
(title "...")
(labels bug ui)
(reply-to <message-hash>)  ; threading
;; body in messageData, or as a part
```

Pull-request letter (issue plus where to pull):
```
(hub-msg 1)
(kind pr)
(op open)
(target <repo-lwwref-b58>)
(thread <thread-id>)
(title "...")
(source     hbs23://<fork-repo-key>)  ; contributor's fork = own repo key
(source-ref refs/heads/feature)
(source-tip <git-sha1>)               ; commit being proposed
(onto       refs/heads/master)
(base       <git-sha1>)               ; merge-base the branch forked from
```

The diff may be shipped two ways, both already supported by
`hbs2:mailbox:message:create:multipart`:
  1. inline patch as a message `part` (self-contained, small PRs);
  2. a pointer to the fork repo key, fetched over hbs2 (large or
     long-lived PRs, cheap due to content-addressed dedup).

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
materialized into files, e.g. under an orphan ref `refs/hbs2/meta`,
one append-only event stream per issue/PR thread. The fold must be
deterministic so that any clone recomputes the same materialized view.
Open questions: exact on-disk layout, conflict resolution when multiple
maintainers fold concurrently (this is where an optional RefChan
consensus log helps), and how a web renderer reads it.


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
   - pr: fetch the fork (or apply the inline patch), review, merge, push
     canon; record status in Tier A and send a reply message to the
     contributor's own mailbox as a notification.
5. Retention: `DeleteMessages` (a signed predicate) and per-message TTL
   prune accepted or spam letters from the mailbox tree.

Notifications are symmetric and serverless: the contributor publishes
their own mailbox sigil, and status updates are messages sent back to
it.


Manifest wiring
===============

The repo manifest gains a clause naming the collaboration mailbox, the
same way fixme-new declared its refchan:
```
(mailbox <mailbox-key-b58> hub)
```
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

PEP-18: collaboration mailbox and message schema.
  The issue/PR letter format: envelope, encryption to maintainer,
  payload S-expression for issue and pr kinds, threading, attachments,
  the manifest `(mailbox ...)` clause. Pins the schema so independent
  implementations interoperate.

PEP-19: canonical in-repo metadata.
  How accepted issues/PRs/comments/labels are stored as files in the
  git tree, the event-sourced fold, deterministic materialization, and
  the read contract for renderers.

PEP-20: pull-request model.
  Forks as repo keys, inline-patch vs fork-pointer submission, fetch and
  merge flow, status recording, contributor notification.

PEP-21: triage and moderation.
  Mailbox Policy beyond BasicPolicy: proof-of-work anti-spam, trust
  tiers (multiple inboxes), deny lists, rate limiting, retention/GC of
  the mailbox tree.

PEP-22: hub CLI and web rendering contract.
  The CLI command surface and the stable data contract a minimalist
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
- Multi-maintainer canon: when to introduce the RefChan consensus log
  vs staying single-writer with a fold.
- Identity ergonomics: sigil distribution and petnames are out of scope
  here; cross-reference a future identity PEP.
- Fork retention: the owner fetching many forks accumulates branches;
  define a prune policy.
