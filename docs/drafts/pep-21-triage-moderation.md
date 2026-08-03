PEP-21: triage, moderation, and retention

Status: draft, started 2026-07-25.
Author: NCrashed (Anton Gushcha)
Part of: PEP-17 (hbs2-hub umbrella).
Depends on: Mailbox Policy (hbs2-peer Proto/Mailbox/Policy), SignedBox,
            PEP-18 (letter/envelope), PEP-19 (canon, admission, compaction).
Related: PEP-20 (pull-request model), PEP-22 (hub CLI).

This sub-proposal covers everything between "a letter arrives" and "it is
canon or gone": spam defence, banning, trust tiers, retention and garbage
collection of the mailbox, delegation of canon-signing authority to
co-maintainers, and the canon compaction policy. It absorbs several items the
other sub-proposals explicitly deferred here: inner-author ban enforcement
(PEP-18), delegated maintainer keys (PEP-19 admission), and the compaction
cadence and "superseded" predicate (PEP-19).


The two enforcement layers
========================

Everything in this PEP follows from one fact about the Mailbox
implementation: the hosting peer never decrypts messages. Policy is evaluated
in `mailboxInQ` over the envelope only, so a peer sees the sender's sign key,
the relaying peer key, the recipient sign keys, the flags (timestamp, TTL),
the group key, the part hashrefs, and the encrypted body, but not the inner
author, the content, or even whether the letter is an issue or a PR. That
splits moderation into two layers with different powers and different jobs:

  - Peer layer (envelope, pre-decryption). Runs on every peer that hosts the
    mailbox, via the Mailbox `Policy` (`policyAcceptPeer`,
    `policyAcceptSender`, `policyAcceptMessage`). It decides what gets stored
    in the mailbox merkle tree, so it is the only layer that can bound
    on-disk growth. It can act only on envelope-visible data.

  - Triage layer (content, post-decryption). Runs on the maintainer's node in
    the hub tooling. It decrypts, reads the inner author and payload, and
    decides what gets folded into canon. It is authoritative for what becomes
    public but cannot stop a peer from storing a message.

Most defences need both: the peer layer to bound storage, the triage layer to
bound what pollutes canon. Where a control belongs is decided by which data
it needs.


Peer-layer policy today, and its reach
====================================

The only concrete policy is `BasicPolicy`: two default actions (peer, sender)
plus allow/deny override maps keyed by sign keys, deny-by-default, declared as
`(peer|sender allow|deny all|<key>)` S-expressions, stored as a merkle tree
and pointed at by an owner-signed, monotonically versioned `SetPolicyPayload`.
An open forge inbox is `(sender allow all)` with targeted `(sender deny
<key>)` bans (PEP-18).

Its reach and limits, as the code stands:

  - `policyAcceptSender`/`policyAcceptPeer` work as described.
  - `policyAcceptMessage` exists but currently ignores message content and
    reuses the sender decision, so there is no message-level gate yet. It is
    the hook every message-level control below plugs into.
  - The policy S-expr parser silently ignores unrecognized clauses, so the
    new policy clauses this PEP proposes (`pow`, `rate`, `quota`) are
    backward-compatible: an old peer ignores them and simply enforces less.
    This applies only to the policy clauses; the PoW witness itself is a
    wire-format change (see Proof-of-work).
  - A message is gossiped before the policy drop decision (a known
    `maybe-dont-gossip-message-if-dropped-by-policy` gap). So policy bounds
    what a peer stores, not what transits the gossip network; storage is the
    DoS surface that matters, and this PEP targets it, but the gossip
    amplification is noted as a separate limitation.


Proof-of-work: bounding distinct-message creation
===============================================

The open-inbox storage-DoS risk (PEP-17/PEP-18 "bounded mailbox growth") is
that `(sender allow all)` lets anyone grow the tree. The peer-layer answer is
proof-of-work, checkable on the envelope before accept. It is not implemented
today (only a code comment and the design paper anticipate it), so this pins
the shape.

  - Witness placement. The PoW nonce is a dedicated unsigned field in the
    `SendMessage` wire message, beside `messageContent`, not inside
    `MessageFlags`. `MessageFlags` lives inside `MessageContent`, which is
    signed and whose serialization defines `messageHash`; a nonce there would
    be self-referential (every grind attempt would change `MessageContent`,
    force an ed25519 re-sign, and shift the very `messageHash` the work binds
    to). A field outside the signature is solved after signing, over the final
    `messageHash`, and needs no authenticity of its own: it is
    self-verifying. The witness is checked only at accept and is not stored in
    the mailbox tree (it gates submission over gossip, not tree replication
    between a mailbox's own hosts). Note this is a wire-format change, not a
    policy-only one: a new field on `SendMessage` an old peer cannot parse, so
    it needs a new message constructor or a protocol-version bump (unlike the
    policy clauses below, which old peers safely ignore). Until peers upgrade,
    a mixed network simply has some peers that do not enforce PoW.
  - Binding. The work targets `H(mailboxKey || messageHash || nonce)` having
    at least D leading zero bits, where `messageHash` is the hash the peer
    already computes for dedup. Binding to `mailboxKey` prevents reusing one
    solution across mailboxes; binding to `messageHash` ties it to this exact
    message.
  - What it bounds. Each distinct message costs fresh work, so PoW bounds the
    rate of distinct messages a spammer can create. It does not stop
    re-sending one solved message, but that is a replay, and the peer's dedup
    (a `hasBlock` on the `RoutedEntry` hash) skips both gossip and accept for
    an already-seen message, so a replay adds nothing to storage and does not
    even re-amplify over gossip. PoW plus dedup together bound growth.
  - Difficulty in policy. D is declared in the signed policy (`(pow D)`),
    versioned like the rest, so the owner tunes it. There is no per-tier PoW
    clause: a tier is already its own mailbox with its own policy (Trust
    tiers), so each tier just carries its own `(pow D)` (zero or absent on a
    low-friction tier). Adaptive difficulty (raise D under load, the paper's
    `set-pow-factor`) is an option layered on top.
  - Freshness. The peer may additionally require `messageCreated` within a
    recent window, rejecting stale PoW. Be honest about its reach:
    `messageCreated` is declared by the sender, so the window only prevents
    unbounded accumulation of old solutions; a flood precomputed for a future
    timestamp inside the window is not stopped. The real bound is the work per
    message, not the window.

Must be built: the dedicated PoW witness field on `SendMessage`, a
`policyAcceptMessage` that verifies it against the policy's D, and the `hub`
client-side solver.


Rate limiting and quotas
======================

PoW throttles anonymous strangers; rate limits throttle a known key that is
allowed but noisy. Both are peer-layer, stateful, and plug into
`policyAcceptMessage`:

  - Rate: a minimum interval per sender key (the paper's cooldown), e.g.
    `(rate <key> <seconds>)` or a default `(rate all <seconds>)`.
  - Quota: a cap on stored messages or bytes per sender before new ones are
    refused, e.g. `(quota <key> <n>)`.

These require per-peer state (last-seen, counts) that is not part of the
signed policy, only the thresholds are. They bound a single key's footprint;
PoW bounds key-churn. A quota counts stored messages or bytes, so it composes
nicely with fold-then-delete (below): once a letter is accepted and folded,
deleting it from the mailbox frees the sender's quota automatically, so a
productive contributor is not starved by their own accepted history. Must be
built: the state and the `policyAcceptMessage` checks.


Deny-lists: envelope key vs inner author
======================================

Banning has the same two-layer split, and the difference matters because of
PEP-18 rewrap-replay (the envelope signer and inner author can differ):

  - Peer-layer ban (`sender deny <envelope-key>`). Stops that envelope key
    from storing new messages, so it bounds storage. It is evadable: a banned
    author can rewrap their inner box under a fresh envelope key and resend.
    Combined with PoW, evasion still costs work per rekey.

  - Triage-layer ban (inner author). The maintainer decrypts, reads the inner
    author's sign key, and refuses to fold letters from a deny-listed author,
    regardless of which envelope carried them. This is the authoritative ban
    for what enters canon, and the only one that can target the real author.

So a full ban is both: deny the envelope key to bound storage, and deny the
inner author to keep them out of canon. The triage deny-list is hub state
(not the Mailbox policy), and may itself be recorded as owner-signed canon so
it travels and is auditable.


Trust tiers and multiple inboxes
==============================

PEP-17's "trust tiers" are a deployment pattern over the primitives, not a new
mechanism. A repo declares more than one collaboration mailbox, each a
distinct mailbox key with its own policy. This needs a small PEP-18 manifest
addition (a tier tag on the mailbox clause and a mailbox-qualified sigil
clause, since with two inboxes an unqualified sigil is ambiguous), specified
there. Each tier is then just a mailbox with its own policy:

  - A low-friction inbox for allow-listed known contributors: `(sender allow
    <key>)...` with no PoW.
  - An open inbox for strangers: `(sender allow all)` with a non-trivial
    `(pow D)` and rate limits.

Promoting a contributor is moving their key to the low-friction tier's
allow-list (a policy version bump). This keeps the common good-actor path
cheap while pricing the anonymous path, without any protocol change beyond the
policy clauses above.


Refusals a triage loop must budget
==================================

The bridge answers each letter with one of five dispositions, and two of them
are the loop's problem rather than the letter's.

  - RETRY means the repo is not ready yet: fold something else and come back.
    Two of its causes are chosen by the SENDER, though, not by the repo: a
    letter naming an attachment nobody will serve, and one whose parts are
    encrypted for a group the maintainers are not in. Both re-verify a
    signature on every pass and neither ever resolves, so a loop needs a retry
    budget per message and must move a message that exhausts it to the parked
    set. Without one, a handful of letters costs a signature check per letter
    per pass forever.

  - PARK means nothing about this repo will change the answer: a newer schema,
    a body over the local size limit, a payload this build cannot decode. The
    letter is kept, because an upgrade or a configuration change can make it
    foldable, but it must not be re-examined every pass. The set has no
    durable marker in canon by construction (canon holds what was folded, not
    what was not), so the loop owns it, and it is the set an attacker can grow
    most cheaply. Cap it, and prefer capping by sender.

  - ABORT means the caller is wired wrong: stop, do not touch the letter.
    DISCARD and DECIDE are the ordinary outcomes.

A letter is deleted from the mailbox only after a RE-FOLD of the tree the event
was written to shows it admitted. Accepting is a pure function of the view it
was handed, and a view is a cache: a delegation withdrawn between the read and
the write, or a write that failed, leaves an event the fold does not admit and a
letter nobody will ever look at again. Fold, write, re-fold, and only then
delete; the origin the accept hands back is the hash to delete by, and the fold
reports the same origin once the event is really in canon. Deleting on the
strength of a Right is how a contributor's submission disappears without anyone
seeing an error.

Publishing a triage ban into canon, which this document leaves as an open
question, is deferred past `hub-meta 1`. It would need a new author-content
constructor and an admission rule saying who may sign one and what it does,
which is a consensus change: the deny-list stays loop state, and the earliest a
public ban can appear is `hub-meta 2`. Recorded here so that nothing plans
around a clause that does not exist.

Which of the five a refusal gets depends on WHERE it was raised, not on what it
says. Honouring a request runs every check twice, once over what the letter
asked for and once over what the maintainer is actually signing, and the same
words mean opposite things on the two sides: a thread mismatch, an oversized
note, an unknown thread or an attribute name that is not canonical, raised
against content triage composed, says nothing whatever about the letter. Those
are ABORT, and a loop that folds then deletes must not delete a good submission
over a bug in a maintainer's own tooling. Everything the owner-native path
refuses is composed in the same sense, since there is no letter there at all.

The mirror of that rule is that a refusal about a LETTER must never stop the
loop, because anyone can send a letter. Naming an attachment the message does
not carry, offering a secret that turns out to be the message's own, an
attribute the sender did not canonicalize: all of these are things a stranger
chooses, so the answer is to be rid of the letter or to park it, never to halt.
Getting this backwards in either direction is how one letter takes a hub down,
or how a hub deletes the submissions it was built to collect.

The deny-list reaches an undecodable letter through its envelope key, which is
the only key available when the payload cannot be parsed. That is deliberately
the one place an envelope ban is used at all, and it should not be mistaken for
a bound on the parked set: an envelope key costs nothing to generate, so what
this stops is a repeat sender who keeps using one, not a sender who rotates.
The cap above is what bounds the set; this only saves the loop from re-checking
the same key's garbage. It also means one refusal, AuthorDenied, now has two
subjects, and an operator whose list holds inner authors alone will never see
it fire on the second: the two lists are not the same list.

Retention and garbage collection
==============================

Retention today is reactive and reclaims nothing: `DeleteMessages` writes a
`Deleted` tombstone (verified against the mailbox key, and against the message
the tombstone names) but only the single-hash `MessageHashEq` predicate is
honored, message and attachment blocks are never purged (explicit TODOs), and
the per-message TTL field is never enforced. This PEP defines the retention
model to fill those gaps.

The second half of that verification is what makes fold-then-delete safe to run
at all. It makes every delete box the owner issues public, and until issue #15 a
merge checked only who had signed one, never what it authorised, so one of them
could be reused as a proof against any other message in the same mailbox. A
retention policy that mints proofs routinely needs proofs that are bound to
their target; otherwise the busier the hub, the more keys to its own front door
it hands out.

  - Fold-then-delete, and it must be canon-aware. Once a letter is folded, the
    mailbox copy of the message envelope is redundant (canon holds the durable
    author box), so acceptance triggers a `DeleteMessages` for that message.
    But an accepted event's `body-part`/`bundle-part` hashrefs live in the
    published author box forever, and PEP-18/PEP-19 promise a clone can fetch
    and decrypt those trees with the published `part-secret`. So the referenced
    part trees must NOT be purged. Deletion removes only the message envelope
    and any parts the accepted event does not reference. Purge (below) then
    applies only to blocks of unfolded or rejected letters. Without this, an
    accepted issue's screenshot or a merged PR's bundle would become
    `unavailable` (PEP-22) after GC.

    DECIDED 2026-08-04, and the two halves are separate. First: the protected
    set is DERIVED FROM CANON, not stored. There is no pin table and there
    should not be one. A purge must read canon anyway, since only canon says
    which letters were folded; the fold already yields the set as `frParts`;
    and a table maintained beside canon is a second owner of one fact, whose
    drift would be discovered as deleted bytes. The predicate is therefore
    "reachable from canon", which is also the only correct shape: parts are
    content-addressed, so one tree can be referenced by two letters and
    "belongs to this message" is not a question with an answer.

    Second: a hub MAY NOT delete an attachment canon references, for as long as
    it references it. The alternative reading is defensible on the wire -- what
    PEP-18 publishes is the key, not a promise that any one peer stores the
    bytes -- and it is rejected here: a clone that can see a reference and its
    key and find the bytes nowhere has a forge with broken attachments, and the
    hub that published the reference is the one peer certain to have had them.
    The cost is accepted: disk grows monotonically with what is folded, and
    retention bites only on the unfolded and the rejected.

    NOTE ON WHAT EXISTS. Neither half is urgent, because the thing they guard
    against does not exist: `delBlock` is in the storage class and nothing
    walks a mailbox to call it, so `DeleteMessages` today writes a tombstone
    and frees nothing. What is decided above is the obligation a purge inherits
    the day somebody writes one.
  - Compound predicates. Extend the honored predicate set (the And/Or
    structure already exists but is rejected) so an owner can prune in bulk:
    all messages from a banned envelope key, or all older than a timestamp.
    Bulk purge obeys the same pin: canon-referenced part trees survive it.
  - TTL enforcement. Honor `messageTTL`: a peer drops (tombstones, then
    purges) a message past its TTL without an explicit delete, so spam that
    was never triaged still ages out. TTL expiry is convergent without any
    coordination, because the deadline (`messageCreated + messageTTL`) is
    computable by every peer from the plaintext envelope alone. A peer that
    syncs an already-expired message drops it by the same rule, so a message
    cannot be resurrected by re-propagation. The one requirement is that the
    grace period before physical purge exceed the sync horizon, so a peer
    does not purge a message another peer is still handing out.
  - Actual purge. Beyond tombstoning, reclaim the message block and its
    attachment trees once tombstoned and past that grace period (the
    `actually-purge-messages-and-attachments` TODO), except any tree pinned by
    a folded canon event. This is what makes the storage bound real rather than
    monotonic without breaking canon references.

Must be built: compound-predicate handling, TTL expiry, the purge/GC pass, and
the canon-referenced-part pin set. Fold-then-delete is hub behaviour on top of
existing `DeleteMessages`.


Delegated canon-signing keys
==========================

PEP-19 admits a canon event only if `canon-by` is an authorized canon key,
and left the sole key as the repo owner (the LWWRef signer) "until PEP-21
defines delegation". Here it is.

Delegation is itself owner-signed canon, so a clone learns the maintainer set
from the same data it verifies, and validity is ordered:

  - The owner emits `delegate`/`revoke` events (owner-only ops, canon-signed
    by the LWWRef owner key) naming a maintainer sign key.
  - A canon event signed by key K is admitted iff K is the owner key, or K
    was delegated and not yet revoked AS OF THAT EVENT'S `seq`. Events a
    maintainer blessed at a `seq` inside their window stay valid after the
    revocation, which is what makes revoking safe to do.
  - Read as of that event's `seq`, and not "signed while authorized", because
    the fold cannot tell when anything was signed: it reads the `seq` the
    signer chose. A revoked maintainer who picks a `seq` below their own
    revocation therefore still satisfies the rule, and can bless a `redact`,
    an `open`, or a comment for as long as they hold the key. What stops that
    is publication and not the fold: only the reflog key can write
    `refs/hbs2/meta`, so a withdrawn maintainer has nothing to put the file
    into. A scheme that widens publication (a shared RefChan) loses the
    guarantee and needs an admission rule about `seq` relative to canon, which
    is a `hub-meta` bump and which a partially fetched clone cannot evaluate.
    PEP-19 records the same limit; do not read either as more than it says.
  - The owner key is the root of trust and cannot be delegated away; revoke
    applies only to delegated keys, and a `revoke` naming the owner key is
    admitted and does nothing.

This makes the maintainer set event-sourced and auditable, consistent with
the rest of canon, and supersedes the PEP-19 placeholder.


Signing versus publishing
=======================

Delegating a canon key grants the right to sign, not the right to publish, and
those are different capabilities. Publishing canon means pushing
`refs/hbs2/meta` (and staging `refs/hbs2/pulls/*`, and pushing merges to code
branches, and rewriting on compaction), and every such push goes into the
repo's reflog, which requires the reflog signing key, derived from the private
LWWRef key. A delegate holding only a canon key can sign an event but cannot
put it anywhere. The capability hierarchy is strict:

```
LWWRef sk   -> everything (repo identity, can derive reflog key)
reflog sk   -> push the whole repo (all refs, including code branches)
canon key   -> sign canon events only (no push)
```

So a repo with more than one maintainer must choose how signed events reach a
reflog-key holder. Three models:

  - Shared reflog key. The owner shares the reflog secret with a co-maintainer.
    Simple, but it is a full-push grant (the co-maintainer can rewrite code
    branches too), so it fits a co-owner, not a triage-only delegate.
  - Sign-and-route (recommended). The delegate signs canon events with their
    delegated key and routes them to a single publisher (the owner, or a
    dedicated publisher daemon holding the reflog key) over the
    maintainer-consensus RefChan (PEP-19). The publisher orders them, assigns
    `seq`/`number`, and pushes. This is the only model that gives a genuine
    triage-only role: the delegate can bless content but never push.
  - Sign-only, owner publishes. The degenerate case of the above with the
    owner as the sole publisher and no RefChan, for a single trusted delegate.

The recommended model is what PEP-19's multi-maintainer section assumes: the
"designated maintainer" who snapshots the RefChan into `refs/hbs2/meta` is
whoever holds the reflog key, and there is exactly one of them, which is why
`seq`/`number` uniqueness is automatic. Delegation is who may sign; the reflog
key is who may publish; the RefChan is how a signer's events reach a publisher.
Scoped roles (a delegate who may triage but not merge) are an open question,
but "sign but not publish" is already the baseline a delegated canon key
gives.


Canon compaction policy
=====================

PEP-19 defines the compaction mechanism (drop only superseded body-less
`set`-class events, keep every `open`/`comment`/`merge`/`redact` and each
winning `set`) and deferred the policy here.

  - The "superseded" predicate. A `set` event for `(thread, attribute)` is
    superseded iff a higher-`seq` `set` (or `close`/`reopen` for the status
    attribute) exists for the same `(thread, attribute)`, the older event
    carries no body, AND no retained `redact` names it. Only such events are
    droppable; everything else is retained (PEP-19).

    The redact clause is not optional. Compaction keeps every `redact` but
    would otherwise drop the `set` one of them hides, leaving the `redact`
    pointing at nothing: the fold then treats it as an unknown target and
    drops it, so the highest admitted `seq` can fall and the bridge reuses a
    `seq` already spent. Reuse is tolerated (PEP-19) and the audit reports
    it, but compaction should not manufacture it.
  - Delegation events are never droppable. `delegate`/`revoke` must survive
    compaction untouched (they are not `set`-class). Admission of every
    historical event depends on the maintainer set as of its `seq`, which is
    reconstructed from the `delegate`/`revoke` events with lower `seq`;
    dropping one would change which past events the fold admits, breaking
    determinism. This retain rule belongs in both this section and the PEP-19
    retain list.
  - Cadence. Compaction is an owner (or delegated-maintainer) operation run on
    a schedule or a size trigger, never automatic mid-fold, so the canon ref
    is rewritten deliberately. `hub sync` follows the rewrite with the forcing
    `+refs/hbs2/meta` refspec (PEP-19).
  - What is preserved and what is not. Any clone that folds the compacted log
    computes the identical materialized state, because compaction only drops
    values the fold would have overwritten. What is lost, by design, is the
    timeline of those overwritten values (the history of status/label
    changes), which is observable in an activity or audit view. Compaction
    trades that timeline for size; a repo that wants full activity history
    simply compacts less or not at all.


What exists today vs what must be built
====================================

Exists today:

  - `BasicPolicy` (peer/sender allow-deny, deny default), signed versioned
    policy pointer, and the `IsAcceptPolicy` hooks including the unused
    `policyAcceptMessage`.
  - `DeleteMessages` with proof verification and the single-hash predicate;
    the And/Or predicate structure (rejected at runtime today).
  - The mailbox merkle tree and the fold/decrypt path triage builds on.

Must be built:

  - Peer layer: the envelope PoW field and its `policyAcceptMessage` check,
    rate/quota state and checks, the `(pow|rate|quota ...)` policy clauses,
    and gossip-after-policy (closing the gossip-before-policy gap) scoped
    correctly: policy gates storage and gossip only for mailboxes the peer
    hosts. A peer that does not host the mailbox has no policy for it (the
    default is deny-all) and must keep relaying as before, or transit nodes
    would stop forwarding submissions and delivery to the hosting peer via
    intermediate hops would break.
  - Retention: compound delete predicates, TTL expiry, and the purge/GC pass.
  - Triage layer: the inner-author deny-list, fold-then-delete, and recording
    triage bans as canon.
  - Canon: `delegate`/`revoke` events and the seq-ordered canon-key check in
    the fold (superseding PEP-19's placeholder), and the compaction runner.


Rejected alternatives
===================

Content-based peer policy. Letting the hosting peer filter on the letter's
kind or body would need the peer to decrypt, breaking the privacy-by-
construction that makes Tier B private to the maintainer. Rejected: the peer
acts on the envelope only; content filtering is the triage layer's job.

Banning solely by envelope key. Simple, but rewrap-replay lets a banned
author return under a new envelope key, so an envelope-key ban alone cannot
keep someone out of canon. Rejected as sufficient: it bounds storage, and the
inner-author triage ban is what actually excludes the author.

Delegation via the repo manifest instead of canon. The manifest (LWWRef) is
owner-signed and could list maintainer keys, but it has no `seq` ordering, so
it cannot express "valid as of when the event was signed"; a revoked
maintainer's past events would be hard to keep valid. Rejected in favour of
seq-ordered `delegate`/`revoke` canon events.

PoW everywhere. Charging PoW on every inbox, including known contributors,
adds friction for good actors. Rejected in favour of trust tiers: PoW on the
open inbox, allow-list (no PoW) on the known-contributor inbox.


Open questions
============

- Exact PoW function and difficulty encoding (leading-zero-bits vs a target),
  and whether difficulty adapts automatically or only by policy version.
- Where the freshness window for PoW is enforced and how much clock skew to
  tolerate.
- Whether the triage inner-author deny-list should be public canon (auditable,
  but names the banned) or private hub state.
- Rate/quota state durability across peer restarts, and whether it is
  per-peer or coordinated among a mailbox's hosts.
- Interaction of fold-then-delete with mirrors that want to retain the raw
  Tier B letters for independent audit.
- Whether delegation should support scoped maintainers (e.g. triage-only vs
  merge-capable) or stay a single canon-signing capability.
