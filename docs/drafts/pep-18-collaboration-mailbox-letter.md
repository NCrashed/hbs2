PEP-18: collaboration mailbox and letter schema (Tier B)

Status: draft, started 2026-07-25.
Author: NCrashed (Anton Gushcha)
Part of: PEP-17 (hbs2-hub umbrella).
Depends on: Mailbox protocol (hbs2-peer Proto/Mailbox), SignedBox,
            GroupKeySymm, Sigil identities, hbs2-git3 repo manifest.
Related: PEP-19 (canonical in-repo metadata), PEP-20 (pull-request model),
         PEP-21 (triage / moderation / PoW), PEP-22 (hub CLI).

This sub-proposal pins Tier B from PEP-17: the ingress letter a contributor
sends to submit an issue or a pull request to a repo whose owner they may
never have met. It fixes the envelope (how the letter rides the Mailbox
protocol), the durable authorship proof, the payload schema for `issue` and
`pr` kinds, threading and identity, attachments and how they survive folding
into public canon, addressing/discovery, the acceptance policy for an open
inbox, and retention. Pinning it lets independent implementations
interoperate and gives PEP-19 the exact object it folds into canon.

PEP-19 already depends on things defined here: the durable author box it
reuses verbatim, the group secret it needs to publish attachments, and the
thread identity model.


What rides on what
=================

A letter is a Mailbox `Message`. The Mailbox protocol already gives us:
anyone may send to a key with no prior membership, acceptance is gated by a
signed versioned Policy, and the body is encrypted to the recipients. From
the code:

```
Message s        = MessageBasic { messageContent :: SignedBox (MessageContent s) s }
MessageContent s =
  { messageFlags      :: MessageFlags          -- created ts, ttl, compression, schema
  , messageRecipients :: Set (PubKey 'Sign s)  -- addressed by SIGN key, >=1 enforced
  , messageGK0        :: Either HashRef (GroupKey 'Symm s)  -- per-message group key
  , messageParts      :: Set HashRef           -- attachments = encrypted merkle trees
  , messageData       :: SmallEncryptedBlock ByteString     -- encrypted body
  }
```

The letter maps onto this as:

  - `messageRecipients` = the repo collaboration mailbox key (a sign key,
    see Addressing). At least one recipient is mandatory; the Mailbox create
    path throws `RecipientsNotSet` otherwise, which is exactly the PEP-17
    "non-empty recipient set" that makes Tier B private by construction.
  - `messageGK0` = a fresh per-message symmetric group key, with the group
    secret sealed to each maintainer's encryption key (from the mailbox
    sigil). Only the maintainer(s) can decrypt.
  - `messageData` = the encrypted letter plaintext (below).
  - `messageParts` = optional attachments (inline patch, files) as encrypted
    merkle trees; the same group key encrypts them.
  - `messageFlags.messageCreated` = send time; `messageSchema` is reserved
    (see Schema versioning).

Delivery is `RpcMailboxSend` / the `SendMessage` wire message, which the peer
gossips; the maintainer's hosting peer accepts it into the mailbox merkle
tree subject to Policy.


Two signatures, and what goes public
==================================

The Mailbox message is signed by whoever wrapped the envelope, but that
signature covers the serialised `MessageContent`, which contains the
encrypted body, the recipient set, and the group key. It is a transport
signature: it authenticates the envelope, but it is taken over the
ciphertext, so on its own it says nothing about the plaintext the sender
authored and it binds the Tier B recipient set. That is the wrong thing to
carry into public canon.

So a letter carries a second, nested signature over the plaintext it will
eventually publish, and the plaintext of `messageData` is a tagged union: a
`Letter` (the durable signed content plus a transport-only back-channel), or
an `Ack` (a courtesy notification, below, with no inner box). This is a
type-level decision an implementer makes on day one:

```
data MessageData = Letter (SignedBox HubLetter) ReplyChannel   -- issue/pr/comment/revise
                 | Ack    AckRecord                            -- owner -> contributor
inner        = makeSignedBox senderSignPk senderSignSk (serialise letter)  -- SignedBox HubLetter s
replyChannel = ReplyChannel { reply-mailbox, reply-sigil }                  -- transport only, optional
plaintext(messageData) = serialise (Letter inner replyChannel)   -- the Letter case
```

  - The inner `SignedBox HubLetter` authenticates exactly the public content
    the sender authored. It is directly verifiable against the sender's sign
    key with no group secret and no recipient set, so it survives into public
    canon and can be checked offline from a clone. This inner box is the
    PEP-19 canon author box, reused verbatim; the PEP-19 event-id is the hbs2
    content hash of its serialised bytes.
  - The `ReplyChannel` (back-channel, below) is transport metadata for the
    owner, not authored public content. It sits outside the inner box so it
    is never folded into canon; its authenticity comes from the outer
    transport signature over the envelope.

On accept the owner decrypts `messageData`, recovers `(inner, replyChannel)`,
stores `inner` unchanged as the event's author box, uses `replyChannel` to
notify, and discards `replyChannel` from canon. The double signing is
intentional: transport integrity is separate from durable, publicly
verifiable authorship, and the back-channel is separate from both.


HubLetter: typed record, S-expression projection
===============================================

Signatures are never taken over rendered S-expression text (suckless-conf has
no canonical serialization). Following the same rule PEP-19 and the Mailbox
`SignedBox` use, the authoritative letter is a typed record with a
`Serialise` (CBOR) instance, and the inner box is signed over its serialised
bytes. The S-expression below is the human-readable projection of that
record. This refines the PEP-17 overview wording "the payload is an
S-expression": the payload is a typed record signed as binary; the
S-expression is its projection.

The author-box payload is one shared record type across the two tiers
(recommended): a Tier B letter is the subset a non-owner may author, and a
PEP-19 owner-native event (`set`, `merge`, `redact`, `delegate`, `revoke`) is the same record with
owner-only ops. So fold and renderers handle a single type rather than a sum
of two. `HubLetter` is the name for the Tier-B-authored subset.

Where the version lives. `(hub-msg N)` is a field of the payload envelope,
NOT of the signed content, and the projection below shows it for readability
only. The reason is the event-id: it is the hash of the author box, so
anything inside that box is frozen for good and a version bump there would
rewrite every existing id. The envelope bytes are never hashed, so the
version is cheap to carry and to bump there, and it lets an old reader answer
"newer schema" instead of "malformed". Canon does not need it in the box
either: PEP-19 versions canon at the event-file and tree level.

`HubLetter` fields (projection):

```
(hub-msg  1)                       ; schema version, carried by the envelope
(kind     issue)                   ; issue | pr
(op       open)                    ; open | comment | revise | close | reopen | label
(target   <repo-lwwref-b58>)       ; on open only: which repository, which is
                                   ;   what blocks cross-repo replay. A reply
                                   ;   needs none: it names a thread, and a
                                   ;   thread-id is globally unique, so a
                                   ;   reply aimed elsewhere finds no thread
                                   ;   here and is dropped as dangling.
(created  <word64>)                ; sender clock, Unix epoch seconds UTC
                                   ;   folds to the PEP-19 event author-ts (same field)

;; threading / identity (see Threading below)
(thread   <thread-id>)             ; absent on open; on reply, the canonical thread id
(reply-to <event-id>)             ; the specific event being replied to (optional)

;; content
(title    "...")                   ; on open
(labels   bug ui)                  ; requested labels (advisory; owner decides)
;; body: inline text in the record's body field, or (body-part <hashref>)
```

Pull-request letters add the source coordinates (kind = pr):

```
(source     hbs23://<fork-repo-key>)  ; contributor's fork = their own repo key
(source-ref refs/heads/feature)
(source-tip <git-sha1>)               ; commit being proposed
(onto       refs/heads/master)
(base       <git-sha1>)               ; merge-base the branch forked from
(bundle-part <hashref>)                ; delta artifact (git bundle base..tip) as an
                                      ;   attachment; the default PR path, see PEP-20
```

Body size. `messageData` is a single `SmallEncryptedBlock`: one secretbox
over the whole payload, no chunking, and it rides in a gossiped message.
Inline body text must therefore stay small; above a soft limit (on the order
of tens of KiB) the body must move to a `body-part` attachment (a chunked
encrypted tree) and the inline body left empty.

Interop rule. A reader ignores clauses it does not know, so adding a field to
an existing op stays compatible. An unknown `op` is a different matter and
cannot be ignored: the letter content is one CBOR sum, so an unrecognized
constructor fails the decode of the whole record and there is nothing left to
skip. Adding an op is therefore a schema change and MUST bump `(hub-msg N)`,
which the envelope carries outside the signed content precisely so an older
reader can report "newer schema" instead of failing blind.

A reader that meets a correctly signed letter it cannot decode must not treat
it as a forgery: verification and decoding are separate steps, and the two
outcomes are reported apart, or an honest newer sender looks like an
attacker.

Acknowledgement letter (kind = ack). The back-channel needs a pinned format or
it is not interoperable. When the owner accepts, closes, or merges, they send
an `ack` letter to the contributor's `reply-mailbox`; `hub updates` (PEP-22)
reads these and correlates them to sent threads:

```
(hub-msg 1)
(kind   ack)
(target <repo-lwwref-b58>)
(thread <thread-id>)                  ; the canonical thread this acknowledges
(number <int>)                        ; the assigned issue/PR number
(status <open|closed|merged|...>)     ; the new canonical status
(merge-commit <git-sha1>)             ; on a merged PR (optional)
```

An `ack` is a courtesy notification, not canon: it is a Mailbox message signed
by the owner (or a delegated maintainer), and the recipient trusts it by
checking the sender key against the repo's maintainer set. It carries no inner
box and is never folded; the authoritative status always lives in canon, and
`ack` only saves the contributor a poll. It has no `op` because it asserts
nothing the contributor authored.


The back-channel (transport, not canon)
=====================================

Notifications need somewhere to go, but a contributor's personal mailbox must
not be published into every clone forever. The back-channel therefore lives
in the transport-only `ReplyChannel` outside the inner box, discarded at
fold:

```
(reply-mailbox <sign-key-b58>)     ; sender's own mailbox key for notifications
(reply-sigil   <hashref>)          ; sender's sigil, so the owner can encrypt back
```

`reply-mailbox` is optional. A drive-by contributor who hosts no mailbox of
their own simply omits it; only notifications are lost. Nothing else breaks,
because the contributor can compute the canonical thread id themselves (see
Threading) and can read status from public canon. When present, `reply-sigil`
is required alongside it: the sender's sign key is recoverable from the inner
box signature, but a sign key is not an encryption key, so the owner needs
the sigil to reply privately.

Because `ReplyChannel` is authenticated only by the outer transport signature,
a rewrapper (see Replay) can substitute their own back-channel. Resolved in
favour of strictness: triage honours the channel only when the envelope
signer equals the inner author, and otherwise treats the letter as having
none. Honouring it unconditionally would let a rewrapper redirect a
contributor's notifications to themselves, which is worse than the
contributor simply not getting notified.

The cost is that a store-and-forward relay (an open question below) cannot
carry the back-channel on someone's behalf: relayed letters arrive with no
usable reply address, so their authors read status from public canon
instead. If relaying is adopted, this rule is what has to be revisited, for
example by having the original sender's envelope travel intact inside the
relayed one.

Both halves are mandatory together. A mailbox key with no sigil leaves the
owner unable to encrypt anything back, so the pair is either fully present
or absent; there is no half-specified state.


Threading and identity
====================

The canonical thread id is computable by the sender, which removes any
out-of-band handshake for threading. In PEP-19 the thread id is the opening
event-id, which is the hbs2 content hash of the opening inner box. The sender
constructs that inner box, so the sender computes the canonical thread id at
send time, before any acknowledgement. Consequences:

  - Opening letter (`op open`). Carries no `thread` clause: the opening event
    is the thread root, and its id is the hash of its own inner box (it
    cannot contain that hash). The letter's Mailbox message hash is what the
    owner records as PEP-19 `origin`, for provenance.
  - Reply letter (`op comment|revise|close|reopen|label`). Carries `thread` = the
    canonical thread id and, optionally, `reply-to` = a specific canonical
    event-id. The author of the thread computed these when they sent the
    opening letter; a third party reads them from public canon. Both are
    canonical ids, so the fold uses them directly with no letter-to-canon id
    mapping.

Acknowledgement is still useful, but only for what the sender cannot compute:
the human issue `number` and status. On accept the owner may send a reply to
`reply-mailbox` carrying the assigned number; threading does not depend on it.
A reply may reference an event whose letter has not yet been folded (its
event-id is still computable); the reference resolves once both are in canon
and dangles harmlessly otherwise.


Attachments, and how they survive folding into public canon
=========================================================

Attachments (an inline patch, screenshots, logs) are Mailbox parts: each is
an encrypted merkle tree stored in hbs2 storage, referenced by `HashRef` in
`messageParts` and named from the payload (`body-part`, `bundle-part`). The
same per-message group secret encrypts them, and `readMessage` decrypts only
the body, so a triage tool downloads and decrypts a part separately.

This creates a problem PEP-17 and the first draft glossed over. The
`body-part`/`bundle-part` hashrefs live inside the signed inner box, which
PEP-19 publishes into public canon verbatim. But the trees they point at are
encrypted with the group secret wrapped only for the maintainers. A public
clone would therefore see an event referencing an attachment it cannot
decrypt. Re-encrypting and re-publishing the part is not an option: a new
ciphertext has a new hash and would break the signed reference.

The fix: at fold, the owner publishes the message's group secret alongside
the event (PEP-19 carries it in the owner-signed canon box). This reveals
nothing that is not already being made public, because that same secret
encrypted only this letter's own content, which the fold is publishing
anyway. A canon reader then fetches the referenced encrypted tree over hbs2
and decrypts it with the published secret. For this to keep working, the fold
also pins the referenced part trees so retention does not garbage-collect
them when it deletes the accepted letter from the mailbox (PEP-21 makes purge
canon-aware); only parts of unfolded or rejected letters are reclaimed. Note the attachment blocks are
hbs2 storage objects fetched over the same transport as the git data, not git
blobs inside the meta tree; making attachments git-native blobs is possible
but is a later option, not required here.


Addressing and discovery
======================

A mailbox is addressed by a sign key (`MailboxKey s = PubKey 'Sign s`), and
the per-mailbox message tree is keyed by that sign key. To encrypt to the
maintainer a sender needs the matching encryption key, which lives in the
mailbox's sigil (`Sigil` binds a sign key to one `Encrypt` key, signed). The
repo manifest therefore carries the mailbox key and its sigil:

```
(mailbox <mailbox-sign-key-b58> hub [<tier>])       ; a collaboration mailbox
(mailbox-sigil <mailbox-sign-key-b58> <hashref>)    ; sigil for THAT mailbox
```

The `mailbox-sigil` clause makes a fresh clone able to submit without a live
directory lookup: there is no resolve-sigil-by-sign-key service today
(`loadSigil` takes a hash), so the sigil hash must be discoverable, and the
manifest is where it belongs. The sigil clause names the mailbox key it
belongs to, so it stays unambiguous when a repo declares more than one
mailbox. This refines PEP-17's "Manifest wiring", which named only the
`(mailbox ...)` clause.

Trust tiers (optional). A repo may declare more than one mailbox clause, each
with a distinct key and an optional `<tier>` tag (for example a low-friction
`known` inbox and an open `public` inbox), each governed by its own policy
(PEP-21 sizes the tiers: allow-list without PoW for `known`, PoW plus rate
limits for `public`). A contributor the maintainer has allow-listed selects
their tier by name. A submitter naming no tier resolves in this order: the
untiered mailbox, then one tagged `public`, then the first hub mailbox
declared. The fallback matters because a repo may declare only `known` and
`public` and no untiered inbox, which would otherwise leave a default
submitter with nothing to address. With a single mailbox the tag is omitted
and there is nothing to choose.

Multi-maintainer note. Within one mailbox, if several maintainers must read
it, the group secret is sealed to each of their encryption keys. A
`SigilData` binds exactly one encryption key, so this is expressed by multiple
`(mailbox-sigil <mailbox-sign-key-b58> <hashref>)` clauses for the same
mailbox key (one sigil per maintainer), not by one sigil carrying several
keys; routing still uses the single mailbox sign key. Key sharing across
maintainers is an identity concern deferred to PEP-21.


Ops and permission
================

The `op` catalogue matches PEP-17 and folds onto PEP-19 events per its
permission model:

  - `open`, `comment` are author-authored. Anyone who can send a letter may
    author them; on accept they become canon as genuinely the sender's, the
    inner box carrying the sender's signature.
  - `revise` (PR only) updates the proposed tip. It is author-authored but
    the fold applies it only from the thread's author of record (PEP-19), so
    no one else can redirect a PR to a tip they control.
  - `close`, `reopen`, `label` are requests, not canon by themselves. A
    stranger's `close` letter asks the owner to close; it becomes canon only
    if the owner issues an owner-signed `set`/`close` event (PEP-19 rule 4).
    A contributor closing or relabelling their own submission is still only a
    request the owner may honor.

So a letter can express any op, but Tier B never mutates canon directly; the
owner's fold is the only writer of Tier A.


Replay, rewrap, and deduplication
===============================

The envelope signer and the inner-box signer can differ: anyone holding a
decrypted letter can rewrap the same inner box in a fresh envelope and resend
it. This is not forgery (the inner authorship is intact) but it has
consequences that must be handled:

  - Deduplicate by event-id. The fold must key canon events by event-id (the
    inner box hash), so the same inner box arriving in several envelopes
    yields one canon event, never duplicates. This rule is stated in PEP-19.
  - Ban evasion via relay. Mailbox Policy bans by the envelope (outer) sign
    key, so a banned author's inner box can be relayed under a different
    envelope key. The hosting peer cannot see through this (it does not
    decrypt). Enforcing a ban on the real author therefore belongs to the
    triage layer, which decrypts, reads the inner author, and can drop or
    refuse to fold letters whose inner author is deny-listed (PEP-21).
  - Cross-repo replay is already blocked: `target` is inside the inner
    signature, so an inner box authored for one repo cannot be folded into
    another.


Sending, hosting, delivery
========================

  - Host. The maintainer's peer hosts the mailbox: `RpcMailboxCreate
    (<mailbox-sign-key>, MailboxHub)`. Only mailboxes present in the peer's DB
    are accepted and stored.
  - Send. The contributor builds the `Message` (multipart create) and calls
    `RpcMailboxSend`; the peer injects `SendMessage` and gossips it. Delivery
    depends on the sender's peer being able to gossip to the maintainer's
    peer (ordinary hbs2 connectivity, including over Tor per PEP-05).
  - Sync. A hosting peer periodically broadcasts `CheckMailbox` and pulls
    signed `MailboxStatus` (the tree root plus the signed policy pointer),
    then downloads and merges new entries. There is no per-remote-mailbox
    poller registration analogous to reflog/refchan; hosting plus gossip is
    the mechanism.
  - Triage read. The maintainer walks the per-mailbox merkle tree
    (`RpcMailboxGet` root, then the `Exists`/`Deleted` entries), reads each
    message (`readMessage` decrypts the body with the maintainer's encryption
    key via keyman), recovers `(inner, replyChannel)`, verifies the inner
    box, checks the inner author against any deny-list, and parses the
    payload. PEP-22 specifies the triage CLI.


Acceptance policy for an open inbox
=================================

The forge inbox is open with banning, expressed in `BasicPolicy`:

```
(sender allow all)          ; open inbox: accept submissions from anyone
(sender deny <sign-key>)    ; targeted ban (by envelope key; see Replay)
(peer   allow all)          ; accept relaying peers (tighten if desired)
```

The policy text is stored as a merkle tree; the owner publishes a signed,
monotonically versioned pointer to it (`SetPolicyPayload { mailboxKey,
policyVersion :: Word32, policyRef }` wrapped in a `SignedBox` signed by the
mailbox owner). The peer accepts a policy only if its version strictly
exceeds the stored one. Default when no policy is set is deny-all, so an open
inbox is an explicit choice.

Anti-spam limits today. `policyAcceptMessage` currently ignores message
content and reuses the sender decision, and proof-of-work is not implemented
(only a code comment and the design paper anticipate it). So the peer-level
levers are allow/deny by envelope sign key and the default action; deny-list
enforcement on the real (inner) author is a triage-layer job (see Replay).
PoW, rate limiting, and trust tiers are PEP-21.

Bounded mailbox growth. `(sender allow all)` means the maintainer's peer
accepts any sender's messages into the mailbox merkle tree, so unfiltered
spam grows on-disk state, not just the triage queue. Until PoW or rate
limiting lands this is a disk-consumption risk to size when opening an inbox,
carried as a PEP-17 open question.


Retention
========

Accepted or spam letters are pruned with `DeleteMessages`: an owner-signed
`SignedBox (DeleteMessagesPayload)` carrying a predicate. Today only the
single-hash predicate (`MessageHashEq`) is honored; compound And/Or
predicates are defined but rejected as unsupported. The merge worker verifies
the delete proof (the signed box must be for this mailbox) before writing a
`Deleted` tombstone.

Limits to state honestly: deletion is tombstone-only. Message blocks and
attachment trees are not garbage-collected (explicit TODOs), and the
per-message TTL field exists in `MessageFlags` but is never enforced. So
retention is reactive and does not reclaim storage yet. Real purge, TTL
enforcement, and GC are follow-ups (PEP-21).


Schema versioning and the messageSchema hook
==========================================

Two version surfaces exist. Today the payload carries `(hub-msg N)`, and that
is the authoritative schema marker readers switch on. The Mailbox
`MessageFlags.messageSchema :: Maybe HashRef` field is reserved and currently
never read or written by any code; it is the intended long-term hook to
declare the letter schema by content hash (a pointer to a schema descriptor
tree) so a reader can fetch the exact grammar. Plan: carry `(hub-msg N)` in
the payload now; populate `messageSchema` with the hub-letter schema hash once
schema support lands, keeping the payload marker for compatibility.


What exists today vs what must be built
====================================

Exists today:

  - The whole Mailbox transport: create/send/gossip/store/read, per-mailbox
    merkle log, signed versioned `BasicPolicy`, signed single-hash deletion,
    multipart create with encrypted part trees, `SignedBox` sign/verify over
    binary Serialise, `Sigil` binding and `GroupKeySymm` encryption to
    recipients, and recovery of the group secret via keyman.
  - The manifest is trivially extensible with the `(mailbox ...)` /
    `(mailbox-sigil ...)` clauses (a two-line reader/emitter change).

Must be built:

  - The shared author-box content record (`HubLetter` as its Tier-B subset)
    with `Serialise` + S-expr projection, the inner-`SignedBox`
    construction/verification, and the `(inner, replyChannel)` framing of
    `messageData`.
  - Manifest readers/emitters for `(mailbox <key> hub [<tier>])` and one or
    more mailbox-qualified `(mailbox-sigil <key> <hashref>)`.
  - The acknowledgement path: on accept, optionally send a number/status
    reply to `reply-mailbox` (resolving `reply-sigil` to encrypt).
  - Publishing the message group secret into the canon box at fold, and the
    reader path that fetches and decrypts attachments with it (shared with
    PEP-19).
  - The contributor-side compose (`hub issue new`, `hub pr new`) and the
    triage read that recovers/verifies the inner box and enforces inner-author
    bans (PEP-22).

Deferred to other PEPs: PoW / rate limiting / trust tiers and real
retention/GC (PEP-21); the fetch/merge of a PR fork (PEP-20); the fold into
canon (PEP-19).


Rejected alternatives
===================

RefChan for ingress. Rejected in PEP-17: RefChan access control is a closed
owner-curated allowlist (a head block accepted only when signed by the key
equal to the RefChan id), so a stranger cannot post without the owner
provisioning them. Mailbox is built for the open case. This PEP does not
reopen that decision.

Reusing the Mailbox transport SignedBox as the canon author box. Simpler (no
nested box), but the transport signature is over the ciphertext and binds the
recipient set; carrying it into public canon would either leak Tier B
metadata or require the group secret to relate it to any plaintext. Rejected
in favor of the inner `SignedBox HubLetter` over plaintext, which is publicly
and offline verifiable.

Putting the back-channel inside the signed letter. Simpler, but the inner box
is published into canon verbatim, so the contributor's personal mailbox key
would be public in every clone forever and unremovable (redaction is
display-level). Rejected: the back-channel is transport-only, outside the
inner box.

Signing the S-expression text. No canonical serialization exists for
suckless-conf, so text signatures would not be reproducible across
implementations. Rejected: the signed unit is the binary Serialise of the
typed record, and the S-expr is a projection, consistent with PEP-19.

Making the fork pointer the default PR path. Intuitive (name the fork, fetch
it), but fetching a foreign fork repo key re-imports its whole reflog today
(PEP-20 efficiency finding), so it is not delta-cheap. The default is instead
a delta artifact (git bundle `base..tip`) shipped inline as `bundle-part`,
whose size is the delta; the fork pointer stays available for durable forks.
Both carry the same `source-tip`/`base`, so verification is identical
(PEP-20).


Open questions
============

- The exact shared content record shape and CBOR layout to pin for interop
  (field order, optional-field encoding, the op union across Tier B and
  owner-native), to be fixed alongside the PEP-19 event record.
- Whether to inline the sender's sigil bytes in `ReplyChannel` versus
  referencing it by `reply-sigil` hash (inlining removes a fetch but grows
  the envelope). Now confined to the transport part, not canon.
- Delivery assurance: a submission depends on gossip reaching the hosting
  peer; whether the hub should offer a store-and-forward relay for offline
  maintainers (touches MailboxRelay type) is left open. Note the back-channel
  rule above already constrains the answer: a relay rewraps, so relayed
  letters lose their reply address unless the original envelope is preserved
  inside the relayed one.
- Making attachments git-native blobs in the meta tree (self-contained clone)
  versus the current fetch-over-hbs2-plus-published-secret path.
- Populating `messageSchema` and defining the schema-descriptor tree format
  once schema support lands.
