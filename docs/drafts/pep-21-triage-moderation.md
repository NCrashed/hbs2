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
  - A policy clause this build does not know REFUSES THE WHOLE FILE, and the
    caller falls back to `defaultBasicPolicy`, which is deny-all. This was
    written here the other way round, as silent-ignore, and that was true of
    every released peer up to 0.25.5.0; it stopped being true in the tree
    afterwards. So the new clauses this PEP proposes (`pow`, `rate`, `quota`)
    are backward-compatible only towards a RELEASED peer, which ignores them
    and enforces less. A peer built from a revision that has the strict parser
    and not the clause reads such a policy as no policy at all and denies
    everything -- an unreleased window, but the one an operator upgrading a
    hub piecemeal is standing in.
    This applies only to the policy clauses; the PoW witness itself is a
    wire-format change of a much harder kind, and an old peer does not
    ignore it but drops the whole message (see Proof-of-work).
  - A message is gossiped before the policy drop decision (a known
    `maybe-dont-gossip-message-if-dropped-by-policy` gap). So policy bounds
    what a peer stores, not what transits the gossip network; storage is the
    DoS surface that matters, and this PEP targets it, but the gossip
    amplification is noted as a separate limitation.


Proof-of-work: bounding distinct-message creation
===============================================

The open-inbox storage-DoS risk (PEP-17/PEP-18 "bounded mailbox growth") is
that `(sender allow all)` lets anyone grow the tree. The peer-layer answer is
proof-of-work, checkable on the envelope before accept.

BUILT, in `HBS2.Peer.Proto.Mailbox.PoW` and the two check sites below. What
follows is the shape and the reasoning behind it; where the code settled
somewhere other than where this section first pointed, the paragraph says so
rather than being quietly rewritten, because the reason for the move is worth
as much as the destination.

What the wire allows. Three facts about the mailbox protocol decide every
choice below, and all three are read off the code rather than assumed:

  - `MailBoxProto` is a derived generic `Serialise` sum
    (`Peer/Proto.hs:160`, protocol id 13001), so constructors are tagged by
    position and records are encoded by arity. Appending a constructor leaves
    every existing encoding untouched. Adding a field to an existing record
    changes that record's arity, and an old peer then fails to decode every
    message carrying that record, not only the new ones. For `MessageFlags`,
    `MessageContent` or `Message` that means all mailbox traffic, so no
    variant of this PEP adds a field to any of them.
  - Relaying re-encodes. `gossip mess` is `request pip msg`
    (`app/PeerTypes.hs:380`), and `request` sends `AnyMessage proto (encode
    msg)` (`Actors/Peer.hs:289`): the value that was decoded is serialised
    again. Nothing a relaying peer cannot parse survives a hop. In particular
    the trailing-bytes tolerance of `deserialiseOrFail`, which the peer's own
    test suite records (`test/TestSuite.hs:91`), buys nothing here: an
    appendix reaches the first peer and dies there.
  - A failed decode is silent. `maybe (pure ()) ... (decoder msg)`
    (`Actors/Peer.hs:518`) drops the message with no log, no counter and no
    reply to the sender.

Together these say there is no ignore-and-forward path in this protocol. A
witness that must reach the hosting peer is a fork of the relay path, and the
only choice left is how wide the break is.

  - Witness placement. A new constructor, appended last to
    `MailBoxProtoMessage`:

    ```haskell
    SendMessageStamped (Message s) MessageStamp
    ```

    Appended last because tags are positional; inserting it anywhere else
    renumbers `DeleteMessages` and breaks deployed peers on messages that
    have nothing to do with PoW.

    Outside `Message`, not inside it, and this is what fixes the identity the
    work binds to. The peer stores `putBlock sto (serialise m)` with
    `m :: Message s` (`MailboxProtoWorker.hs:925`), so keeping the witness
    out of `Message` keeps the stored blob and its hash identical whether the
    message arrived stamped or plain. The witness is checked at accept and is
    not stored in the tree: it gates submission over gossip, not tree
    replication between a mailbox's own hosts.

    The stamp carries the nonce and the mailbox key it was solved for. NOT
    the difficulty it claims, which this said first and the code does not do:
    the verifier counts the zero bits itself and compares them against what it
    wants, so a claimed difficulty is a field nothing reads and a sender can
    lie in. The key has to be in there: a message names
    several recipients, the work is bound to one of them, and a verifier
    cannot tell which without being told. It also lets the pre-gossip check
    below run on a peer that does not host the mailbox and has no policy for
    it. The stamp is unsigned and needs no authenticity, because it is
    self-verifying against the message it names, and a stamp naming a key
    that is not among `messageRecipients` is simply not a stamp for this
    message.
  - Binding. The work targets

    ```
    H(serialise (mailboxKey, hashObject (serialise (msg :: Message s)), nonce))
    ```

    A CBOR TRIPLE and not a concatenation, which is what this said until the
    code was written: three byte strings run together have no field
    boundaries, so a mailbox key ending in the bytes a message hash begins
    with is a second reading of the same preimage. The encoding is the one
    thing a second implementation cannot guess, so it is the encoding that is
    written down here.

    having at least D leading zero bits. Binding to `mailboxKey` prevents
    reusing one solution across mailboxes. Binding to the hash of `Message`
    ties it to this exact message, and to precisely the bytes that will
    occupy disk if it is accepted, so the work is paid for the thing that
    costs storage.

    NOT the hash the peer computes for dedup. That one is
    `hashObject (serialise mess)` over the whole `MailBoxProto` value
    (`Proto/Mailbox.hs:218`), which now contains the stamp, so binding to it
    would be circular: every grind attempt would change the hash the work
    binds to. The `Message` hash is fixed before the grind starts, which is
    also what keeps the grind hash-only: the message is signed once, before
    solving, and the solver never touches ed25519.
  - Dedup of a restamp. The `RoutedEntry` marker for a stamped message is
    `H(serialise (mailboxKey, bits, msg))`, not the hash of the whole
    `MailBoxProto` value, so re-sending one message under a fresh nonce of the
    same strength is recognized as the same message. Without that rule a
    spammer buys a second flood for each fresh solution; with it, a restamp is
    `seen` and goes nowhere. Storage dedup already collapses the duplicate
    either way; this is about gossip amplification.

    THE WORK IS IN THE MARKER, and leaving it out was a way to stop somebody
    else's mail. With only `(mailboxKey, msg)`, anybody who had seen a message
    could mint `MessageStamp1 mbox 0` -- free, and forwarded wherever the peer's
    floor is zero, which is the default -- and race it ahead of the honest copy.
    Both carry the same marker, so the honest D-bit copy is `seen` and dies at
    its sender's first hop, while the attacker's copy is refused for want of
    work at the host: the letter arrives nowhere, the sender paid D bits for it,
    and by this document's own admission there is no rejection signal to notice
    with. With the bits in it, suppressing a copy worth D bits costs D bits.
    What an attacker can still buy is a second flood per DIFFICULTY rather than
    a suppression, and each additional one doubles in price, which is a bounded
    and priced amplification instead of an unbounded denial.

    Three hashes, not one. This section claimed the marker and the binding
    above were the same value, and they are deliberately not: the binding is
    the triple with the nonce in it, the marker is the pair without it (a
    marker that moved with the nonce would gate nothing), and the block the
    message becomes is `hashObject (serialise m)` over the message alone. The
    mailbox key is in the marker so that a stamped message cannot have its
    stamp stripped and be re-sent plain to suppress the honest copy, which a
    marker shared with the plain path would allow.
  - Where it is checked, which is two places, because the flood and the disk
    are different surfaces. The protocol handler gossips before any policy is
    consulted (`Proto/Mailbox.hs:255`, with `policyAcceptMessage` running
    later in the queue drain at `MailboxProtoWorker.hs:915`), and at gossip
    time the handler does not yet know which mailbox the message is for, so
    it cannot read `(pow D)`. Hence:

      - a peer-global minimum difficulty, `(hbs2:mailbox:pow-min D)` in the
        peer's own config, checked in the `SendMessageStamped` branch before
        `gossip`. Zero by default, so a peer that has not been told otherwise
        behaves as today. What it bounds is narrower than "what this peer
        amplifies", and the difference is worth writing down: a PLAIN
        `SendMessage` is still forwarded before any policy is consulted, as it
        always was, so the floor prices the stamped path and not flooding in
        general. The stamped path needs a price of its own because it carries
        a second dedup identity: one message stamped for each of its N
        recipients is N markers and N floods, and at difficulty zero that is
        free.

        A WEAK STAMP MEANS "I WILL NOT FORWARD THIS", and nothing else. The
        first implementation dropped the message instead, which made the floor
        a setting that silently loses mail: a peer with a floor of 16 relaying
        for a mailbox that charges 12 black-holed a letter both ends had paid
        for correctly, and applied to a mailbox the same peer HOSTS it turned
        "I will not amplify" into "I will not store". The floor cannot be
        calibrated against `(pow D)` in any case, since the relay has not read
        the mailbox's policy and usually cannot. So a stamp below the floor,
        or one naming a mailbox the message is not addressed to, stops the
        gossip and lets the message through to the queue, where the mailbox's
        own policy decides with the real D in hand.
      - the per-mailbox `(pow D)` from the signed policy, checked in the queue
        drain. It bounds what this peer stores.

    The policy DECLARES the difficulty and does not check it: `policyPoW`
    answers a number and the caller does the verifying. This section first
    said the opposite -- that `policyAcceptMessage` would gain a parameter
    carrying the stamp -- and the code went the other way, because verifying a
    stamp needs the mailbox key and the message bytes and neither is the
    policy's business. A policy says what it wants; the peer holding the
    message finds out whether it got it. Rate limits and quotas land the same
    way for the same reason: the state they count lives in the peer, not in a
    file the mailbox owner signs.

  - What is NOT charged, which is the half this section originally missed. A
    stamp is not stored in the mailbox tree, so a peer replicating another
    host's tree has none to offer for the messages in it. Charging `(pow D)`
    there would mean a mailbox that charges anything cannot replicate between
    its own hosts -- it would refuse everything its co-hosts hand it. So the
    accept path takes a `MessageOrigin`: `Submitted` (over gossip, pays) or
    `Replicated` (out of a tree, does not).

    WHAT BOUNDS THE REPLICATION PATH IS NOT `policyAcceptPeer`, though this
    section said it was, on the grounds that "the message admitted work-free
    is one a trusted co-host already holds". It is not, and the reason is
    structural: one clause is answering two different questions. `(peer ...)`
    gates the RELAYING neighbour, so an open inbox has to say `(peer allow
    all)` or a stranger's letter never reaches it over gossip at all; and the
    same clause then lets any handshaked peer announce a `MailboxStatus`
    naming a tree it invented, whose every message is admitted with the work
    check skipped. On the one configuration `(pow D)` exists for, it bounded
    nothing.

    The second question is therefore asked separately, and locally:
    `(hbs2:mailbox:replicate-from <peer-key>)` in the hosting peer's own
    config, repeatable, naming the peers it replicates a charging mailbox
    with. A mailbox with `(pow D)` takes a `Replicated` message only from
    those; a mailbox that charges nothing is unaffected and behaves exactly as
    before. Absent means nobody, so the default fails closed: a charging
    mailbox that has named no co-hosts simply does not take replication, which
    is a single-host inbox and the ordinary case.

    Local and not in the signed policy on purpose. Replication is disk the
    HOSTING peer spends, and which co-hosts it will fill that disk for is not
    a question a stranger's policy should be able to answer for it. The cost
    is that the answer does not travel with the mailbox: every host of a
    charging mailbox names its co-hosts itself.
  - What it bounds. Each distinct message costs fresh work, so PoW bounds the
    rate of distinct messages a spammer can create. Replay of one solved
    message is bounded by dedup, not by work: the `RoutedEntry` marker stops a
    replay being GOSSIPED again, the merged marker in the queue drain stops it
    being stored twice, and an in-flight set on the input queue stops N copies
    of one message taking N slots in it.

    That last one is a bound on the QUEUE and not on the work, and it is there
    because accept was deliberately moved out from under the gossip marker: a
    message refused by a policy that had not been written yet was otherwise lost
    for good, since nothing stored it and every later copy was suppressed as
    seen. The cost of that freedom was a slot per copy, in a queue 8000 deep
    drained every ten seconds, so replaying one packet at link rate filled it
    and the honest submissions behind it were dropped -- permanently, since only
    the replication path retries. The set holds what is in flight and nothing
    else; the drain clears it as it takes the batch, so a message refused this
    round is queued again on the next copy.

    A flood of DISTINCT messages below a mailbox's `(pow D)` used to be sized
    rather than priced: each took a slot until the drain read the policy ten
    seconds later and refused it, so the work was free to skip and the honest
    submissions behind it were dropped. The check now runs at the door as well,
    against a per-mailbox cache of D that the drain fills as it reads each
    policy.

    THE CACHE IS NEVER AN AUTHORITY and fails open in every direction: a mailbox
    this peer has read no policy for pays, a mailbox charging zero pays, a
    message naming no recipient pays, and one paid recipient out of several buys
    the slot. The drain still decides each recipient separately with the signed
    policy in hand. A message let through costs a slot and is then refused
    properly; one refused at the door on stale information would be a message
    lost to a policy that had changed, which is why `mailboxSetPolicy` forgets
    the entry rather than updating it.

    AND ONE SENDER MAY HOLD ONLY A SHARE of the queue -- an eighth -- whether or
    not any work is asked for. That is the half proof-of-work cannot reach:
    most mailboxes charge nothing, and for them the flood is free by design, so
    what is bounded there is starvation rather than cost. It bounds ONE peer; a
    flood from eight is a different attack, answered at the peer layer by who
    may talk to this node at all.

    Rate limiting proper -- counting per sender over a window -- is still the
    answer this document defers, and it is now a refinement rather than the
    only thing standing between an open inbox and a free flood.
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

The PoW deployment cost, accepted deliberately
============================================

A stamped letter travels only over a path of upgraded peers. An old peer that
receives `SendMessageStamped` cannot decode it, and by the third fact above
it does nothing at all: no gossip, no store, no reply. It is a black hole, not
a peer that merely enforces less.

This is accepted rather than worked around. The alternatives were weighed and
none of them are better:

  - A trailing unsigned witness after the encoded value would be ignored by
    old peers, which sounds like exactly what is wanted, but relaying
    re-encodes, so it survives one hop and reaches no hub that is not the
    sender's direct neighbour.
  - A field on `MessageFlags`, `MessageContent` or `Message` breaks decoding
    of all mailbox traffic for old peers, which is strictly worse than
    breaking only the stamped part of it.
  - Reusing the reserved `messageSchema :: Maybe HashRef` slot inside the
    flags is the one shape that changes no wire format at all: old peers
    relay it untouched, and the work can bind the content hash so the grind
    stays hash-only. It was rejected because it spends a reserved extension
    point on a nonce, puts the witness in the tree forever, and makes the
    stamp part of what the author signs. If the deployment gap turns out to
    hurt more than expected, this is the fallback to reconsider, and it is
    recorded here for that reason rather than as a live option.

What the gap actually costs is small, because PoW is not for everyone. The
known-contributor tier carries no `(pow D)` and keeps using plain
`SendMessage`, so contributor traffic is unaffected. The open tier is where
strangers submit, and a stranger whose submission does not arrive is the
failure mode PoW exists to produce, only for the wrong reason. The mitigation
is on the client: `hub` reads the target mailbox's policy before sending, and
solves what that policy charges.

WITH A REACH THIS SECTION FIRST OVERSTATED. The policy comes from the local
peer's status for that mailbox, and a peer has that only for mailboxes it
HOLDS. Writing to somebody else's hub is exactly the case where it holds
none, and there the client reads no charge and sends plain. It is right in
the only sense available -- it has no evidence of a charge -- and wrong in
the sense that matters, because the hub may drop the letter. Sending to a
mailbox that charges therefore wants the peer to hold it first, the same
`mailbox create` and fetch a contributor already does to read the replies.

Since one stamp is work for one mailbox, a letter addressed to several
mailboxes that charge is sent once per stamp. The bytes are identical -- the
stamp rides beside the message -- so what multiplies is gossip and not
storage, and the second copy to reach a mailbox that already took the first
is dropped as merged.

There is no rejection signal at all, which is the one genuinely unpleasant
part: a stamp that is too weak, a stamp that is stale, and an old peer
somewhere in the path are indistinguishable to the sender. All three look
like nothing happening.


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
the canon-referenced-part pin set.

Fold-then-delete is DONE, as the hub behaviour this predicted: `hub inbox
accept` drops the letter it folded (`--keep` opts out), and `hub inbox reject`
drops one it did not. Both go through the same signed single-message delete, so
the tombstone is identical and only the meaning differs.

That makes the pin above load-bearing rather than anticipated, and it is the one
part of this section a later writer must not skip: the letters a purge finds
tombstoned now include every letter canon took, so a purge that reclaims their
attachment trees without consulting canon breaks exactly the references rule A2
protects. Until such a purge exists nothing is reclaimed and nothing is at risk.


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

    That identity is not only a promise, it is the CHECK a clone makes: `hub
    sync` does not force the canon ref (PEP-22), so a compaction reaches a
    clone as a divergence, and folding both lineages is how it tells one from
    a fork. The predicate above is written to make that check pass -- which is
    also why an event this build cannot resolve, or one the fold dropped, is
    retained rather than tidied away: neither is a value the fold overwrote.

  - What is built: all three. The predicate is `HBS2.Hub.Compact`, the runner
    is `hub compact` (with `--dry-run`, which shows the plan from the same code
    path that writes it), and the sync-side check is `hub sync --repo`.


What exists today vs what must be built
====================================

Exists today:

  - `BasicPolicy` (peer/sender allow-deny, deny default), signed versioned
    policy pointer, and the `IsAcceptPolicy` hooks including the unused
    `policyAcceptMessage`.
  - Proof-of-work end to end: `SendMessageStamped` and `MessageStamp`, the
    `(pow D)` policy clause, `(hbs2:mailbox:pow-min D)` on the peer, both
    check sites, the restamp dedup rule, the `Replicated` exemption, and the
    solver in `hub`.
  - `DeleteMessages` with proof verification and the single-hash predicate;
    the And/Or predicate structure (rejected at runtime today).
  - The mailbox merkle tree and the fold/decrypt path triage builds on.

Must be built:

  - Peer layer: rate/quota state and checks, the `(rate|quota ...)` policy
    clauses, and gossip-after-policy (closing the gossip-before-policy gap) scoped
    correctly: policy gates storage and gossip only for mailboxes the peer
    hosts. A peer that does not host the mailbox has no policy for it (the
    default is deny-all) and must keep relaying as before, or transit nodes
    would stop forwarding submissions and delivery to the hosting peer via
    intermediate hops would break.
  - Retention: compound delete predicates, TTL expiry, and the purge/GC pass.
  - Triage layer: fold-then-delete, and recording triage bans as canon. The
    inner-author deny-list is BUILT (`hub ban`, `hub unban`, `hub ban list`,
    applied by `hub inbox accept` before anything is minted); it lives in this
    node's own state rather than in the manifest, for the reason the two
    enforcement layers above give.
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

A PoW witness that old peers keep relaying. Two shapes would have avoided the
deployment gap: a witness trailing the encoded value, and a nonce in the
reserved `messageSchema` slot inside the signed flags. The first does not
work at all, because relaying re-encodes and the appendix dies at the first
hop. The second does work, and was still rejected: it spends a reserved
extension point, writes the witness into the tree forever, and folds the
stamp into what the author signs. Rejected in favour of a new constructor and
an honest break, with the reasoning kept in Proof-of-work so it can be
revisited if the gap hurts.


Open questions
============

- Whether difficulty adapts automatically or only by policy version. The
  rest of this question is settled: D leading zero bits, over the peer's own
  `HbSync` hash of `(mailboxKey, messageHash, nonce)`, where `messageHash`
  names exactly the bytes the peer stores.
- Freshness is NOT implemented. `messageCreated` is not read on the accept
  path at all, so a solution keeps forever and nothing charges for its age.
  Left out deliberately rather than forgotten: the window is bounded by what
  a sender declares about its own clock, so it prevents accumulation of old
  solutions and nothing else, and the real bound is the work per message.
  Still open: where it is enforced and how much skew to tolerate.
- Whether a sender should get any signal for a refused submission. Today a
  weak stamp, a stale stamp and an old peer in the path are all silence. The
  client reads the mailbox policy beforehand where it can, which is only for
  mailboxes its own peer holds.
- Whether the triage inner-author deny-list should be public canon (auditable,
  but names the banned) or private hub state.
- Rate/quota state durability across peer restarts, and whether it is
  per-peer or coordinated among a mailbox's hosts.
- Interaction of fold-then-delete with mirrors that want to retain the raw
  Tier B letters for independent audit.
- Whether delegation should support scoped maintainers (e.g. triage-only vs
  merge-capable) or stay a single canon-signing capability.
