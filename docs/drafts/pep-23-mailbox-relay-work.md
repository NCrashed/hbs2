PEP-23: a work field on every floodable mailbox packet

Status: draft, started 2026-08-23.
Author: NCrashed (Anton Gushcha)
Extends: PEP-21 (triage, moderation, retention: the stamp and the peer floor).
Related: PEP-17 (hbs2-hub umbrella), PEP-18 (letter), PEP-22 (`hub drop`).
Supersedes: sections 1, 2 and 5 of `docs/drafts/hub-open-wire-questions.md`.

PEP-21 introduced two numbers: `(pow D)`, what a MAILBOX charges a sender,
declared in its signed policy; and `(hbs2:mailbox:pow-min D)`, what a PEER is
willing to amplify, set by its operator and in nobody's policy. The first one
works. The second one does not, and this proposal is about why and what to do.

It is one wire change (a work field on a delete), one implementation change
that needs no wire change (a delete that names a set), and two changes that
are local to a peer and its client (publishing the floor, and solving for it).


The one fact this follows from
============================

`pow-min` is a switch, not a dial.

A peer decides whether to carry a packet on with `forwardable`
(`Proto/Mailbox/PoW.hs`), which is `named && bits >= floor`. Three branches in
`mailboxProto` call it, and two of them call it with a literal zero:

```haskell
-- the SendMessage branch, and the DeleteMessages branch
let carry = forwardable floorD True 0
```

They pass zero because there is nowhere on the wire for those two packets to
carry a number. `DeleteMessages` has no stamp field at all, and an unstamped
`SendMessage` is unstamped by definition. So for both of them the comparison
is `0 >= floorD`, and the consequences are:

  - Setting `pow-min` to 1 stops this peer forwarding ALL unstamped mail and
    ALL deletes, permanently and silently. That is not a threshold, it is a
    different mode of operation.
  - Therefore the only floor an operator can safely set is 0.
  - And at 0 the `DeleteMessages` branch is what section 1 of the open-wire
    questions describes: an unbounded broadcast primitive costing one
    signature, because the mailbox key on a delete is RECOVERED from the
    signature and can be a keypair minted for the occasion.

So the two open items are not two problems. They are the same problem seen
from the two ends: the floor cannot be satisfied (item 1) and cannot be
observed (item 2), because for two of the three floodable packets there is no
number to satisfy or observe.


What the code does today
======================

Verified against the tree at 2026-08-23. Anchors are file plus name, not line.

  - `Proto/Mailbox/PoW.hs`, `forwardable`: one rule, three call sites, two of
    them passing a literal zero, as above. Gates CARRYING, never TAKING.

  - `Proto/Mailbox.hs`, `mailboxProto`: the `DeleteMessages` branch verifies
    the signature (`unboxSignedBox0`), then decides `carry`, then gossips, and
    only then calls `mailboxAcceptDelete`. The gossip cannot move under the
    ownership check: a transit peer holds no mailbox and is obliged to
    forward.

  - `MailboxProtoWorker.hs`: `mailboxAcceptDelete` and `mailboxSendDelete`
    both require `getMailboxType_`, and `mailboxInQ` requires it before
    storing a message. So only a peer that HOSTS a mailbox ever acts on a
    delete or keeps a letter; everybody else relays and forgets.

  - `MailboxProtoWorker.hs`, `mailboxFetchQ` and `mailboxCheckQ`: `CheckMailbox`
    is gossiped and is NOT re-gossiped by the handler, and `MailboxStatus` is
    a `response`, sent back to the asker. Mailbox synchronisation between
    co-hosts is therefore strictly one hop.

  - `PeerTypes.hs`, `authorized`: `mailboxProto` runs only for a peer that has
    completed a handshake (`KnownPeerKey`), so the source address of a mailbox
    packet is not blindly spoofable.

  - `Proto/Mailbox/Merge.hs`: `SimplePredicateExpr` already has `And`, `Or`,
    `Op` and `End` ON THE WIRE. `admitDeleted` honours only
    `Op (MessageHashEq h)` and answers `MergeUnsupportedPred` to everything
    else, deliberately, so that an older reader never guesses what a newer one
    meant by a delete.

  - `hbs2-hub/ingress/HBS2/Hub/CLI/Drop.hs`, `dropMessage`: one message per
    delete, one packet, one signature.

  - `PeerTypes.hs` `mkPeerMeta` and `Proto/PeerMeta.hs`: `PeerMeta` is
    `[(Text, ByteString)]`, exchanged on protocol id 9 with
    `requestPeriodLim = ReqLimPerMessage 0.25`, answered only to authenticated
    peers, cached per neighbour in `_peerMeta`. It already carries
    `http-port`, `listen-tcp` and `public-address`. Unknown keys are ignored.

  - `HBS2/Defaults.hs`: `defMaxDatagram` is 4096 bytes, which is the size a
    gossiped packet has to fit. The stream transports allow 16 MiB
    (`defMaxFrame`), so UDP is the binding constraint.

One property of the encoding matters enough to record, because the whole
compatibility argument rests on it. `serialise` (0.2.6.1) writes
`listLen (n+1)` followed by a constructor tag word for EVERY constructor,
including the sole constructor of a one-constructor type. Checked directly:

```
data Inner    = A Int | B Int | C Int
data OuterOne = V1 Inner
data OuterTwo = W1 Inner | W2 Inner

serialise (V1 (A 7))  ==  8200820007
serialise (W1 (A 7))  ==  8200820007
```

So appending a constructor never disturbs the ones before it, in either the
inner or the outer type: old bytes decode unchanged under the new type, and
new bytes fail on an old peer with `Bad constructor number`, after which
`runProto` drops them silently (as it does for an unknown protocol id).


Why the two cheaper fixes are not available
=========================================

Both were considered and both are wrong, and the reason is the same fact
about how far things travel.

**"Relay a delete only if you host the mailbox."** This is what an earlier
review asked for and what the open-questions doc already rejects. A delete is
needed by exactly the hosts of its mailbox, but a host can be many hops away,
because `SendMessage` floods multi-hop and a host three hops from the owner
holds letters that flooded to it. Cutting the relay at the first non-host
leaves those letters undeletable forever.

**"Relay a delete only if you have heard of the mailbox."** Attractive,
because a freshly minted key names a mailbox nobody has heard of, and it would
cost the attacker their whole amplification. It fails on the fact above:
`CheckMailbox` and `MailboxStatus` travel one hop, so a transit peer between
the owner and a distant host has never heard of the mailbox and never will.
The knowledge does not spread as far as the letters do.

What remains is what PEP-21 already named: bound the amplification by work.
The rest of this proposal is about making that possible and making it cheap
for the party that is not attacking anybody.


Step A. A work field on a delete
==============================

The wire
--------

One constructor, appended last, for the same reason `SendMessageStamped` is
appended last:

```haskell
data MailBoxProtoMessage s e =
    SendMessage         (Message s)                                 -- 0
  | CheckMailbox        (Maybe Word64) (MailboxKey s)               -- 1
  | MailboxStatus       (SignedBox (MailBoxStatusPayload s) s)      -- 2
  | DeleteMessages      (SignedBox (DeleteMessagesPayload s) s)     -- 3
  | SendMessageStamped  (Message s) (MessageStamp s)                -- 4
  | DeleteMessagesStamped                                           -- 5, NEW
      (SignedBox (DeleteMessagesPayload s) s)
      (MessageStamp s)
```

The stamp rides BESIDE the signed box and not inside it, for the same two
reasons it rides beside a message: the owner must be able to re-grind without
re-signing, and the bytes stored as the delete proof block must be identical
whether the delete arrived stamped or plain, since `admitDeleted` reads that
block back and compares the signer against the mailbox key.

`MessageStamp` is reused as the type. It is already `(mailbox key, nonce)` and
nothing about it is message-specific.

The preimage
------------

`stampPreimage mbox h nonce = serialise (mbox, h, nonce)` stays exactly as it
is. Only what is passed as `h` changes:

  - for a message, `h = messageKey msg`, the hash of the bytes the peer stores;
  - for a delete, `h = HashRef (hashObject (serialise box))`, the hash of the
    signed box, which is also the hash of the proof block a `Deleted` entry
    points at. One identity again names the work, the marker and the block.

That answers the third question section 5 of the open-wire questions left
open: the same preimage FUNCTION, a different hash inside it. Work solved for
a message cannot be replayed as work for a delete, because the two hashes are
digests of differently-typed CBOR values and matching them is a Blake2b
collision.

`solveStamp` and `stampBits` compute `h` internally today, so they split:

```haskell
solveStampOver :: PoWDifficulty -> MailboxKey s -> HashRef -> MessageStamp s
stampBitsOver  :: HashRef -> MessageStamp s -> Int

deleteKey        :: SignedBox (DeleteMessagesPayload s) s -> HashRef
solveDeleteStamp :: PoWDifficulty -> MailboxKey s
                 -> SignedBox (DeleteMessagesPayload s) s -> MessageStamp s
```

with `solveStamp` and `stampBits` becoming one-line wrappers, so the existing
tests in `hbs2-peer/test/MailboxPoW.hs` keep testing what they test.

The `stampNames` analogue is `msMailbox st == mbox`, where `mbox` is the key
recovered from the signature. The relay has already paid for that recovery on
this branch before it decides anything, so the check is free. Call it
`stampSignedFor`. Its job is the same as `stampNames`: work solved for
somebody else's mailbox is work for somebody else's delivery.

The dedup marker
----------------

This is the part that is easy to get wrong, and `stampMarker` already explains
why. The plain branch marks a delete with `hashObject (serialise mess)` over
the whole `MailBoxProto` value. If the stamped branch did the same, the stamp
would be in the marker and a restamp under a fresh nonce would look like a new
packet and buy a second flood per solution. So the stamped delete gets its
own marker, built like `stampMarker`, with the WORK in it and the nonce out:

```haskell
deleteMarker st box =
  HashRef (hashObject (serialise (msMailbox st, bits, box)))
  where bits = stampBitsOver (deleteKey box) st
```

The bits are in it for the reason `stampMarker` gives at length: without them,
anyone who saw a stamped delete could mint a zero-bit restamp, have it
accepted for gossip wherever the floor is zero, and race it ahead of the
honest copy, which would then be suppressed as `seen` everywhere it had not
yet reached. With the bits in, suppressing a copy worth D bits costs D bits.

Plain and stamped markers differ, which is also deliberate. An attacker who
strips the stamp and re-sends a delete plain does not suppress the honest
stamped copy. Stripping gains them nothing anyway: acceptance was never gated
by the floor, so the delete still takes effect at every host it reaches.

What it costs whom
------------------

The owner pays, and only for RELAYING. `mailboxAcceptDelete` is not gated by
the floor, and `mailboxSendDelete` runs `inner`, so a delete in the owner's
own mailbox on the owner's own peer is unaffected at any difficulty. What the
work buys is the hop past a relay that charges.

It is still the honest party paying, which is worth saying plainly: the
signature on a delete proves the signer owns A mailbox key, just not one
anybody hosts, so there is nothing cheaper to charge. Step B is what makes
that acceptable in practice.


Step B. A delete that names a set
===============================

`hub drop` on a triage queue of 500 spam letters is 500 packets, 500
signatures and, after step A, 500 grinds. That is the only bulk delete this
system actually has, and it is entirely avoidable: a delete may already name
a set on the wire.

Why `Or` and not a new predicate
--------------------------------

A flat `MessageHashIn [HashRef]` would be a new `SimplePredicate`
constructor. Appending is decode-safe in general, but not here: this type
lives INSIDE the signed box, so an old peer fails in `unboxSignedBox0` and
drops the delete entirely, with no verdict and no diagnostic.

`Or` is already on the wire and already decodes on every deployed peer. An
old reader walks it, does not match `PlainMessageDelete`, and answers
`MergeUnsupportedPred` -- the exact case that verdict was written for -- while
still relaying the packet onward. So `Or` degrades the way the design intended
and a new constructor does not. Use `Or`.

The rules
---------

  - A delete payload denotes the SET of hashes named by `Op (MessageHashEq h)`
    anywhere in its expression tree.
  - `End` terminates a spine and contributes no hash. `Op Nop` contributes
    none either and stays refused as before.
  - `And` remains unimplemented. It has no meaning over a set of hashes, and
    a reader must not invent one.
  - `admitDeleted` accepts when the entry's target is a MEMBER of that set.
    The property it exists to protect is unchanged: a proof still has to name
    the message the entry deletes, so `MergeWrongTarget` still fires for a
    public delete box stapled to somebody else's letter.
  - `mailboxAcceptDelete` writes one `Deleted` entry per named hash, all
    pointing at the same proof block, and enqueues one merge per entry.
    `maxMergeQueue` is 4096, so a full batch is comfortably inside it.

The bound
---------

A gossiped packet must fit `defMaxDatagram`, 4096 bytes. Each target costs
about 40 bytes in the tree, and the signed box costs about 110, so:

  - the reader refuses a payload naming more than `maxDeleteTargets = 64`,
    with its own verdict (`MergeTooManyTargets`) rather than folding into
    `MergeUnsupportedPred`, because this module's rule is that a refusal names
    which of the reasons it is;
  - the builder in `hub drop` batches targets until either 64 targets or an
    encoded box over 3072 bytes, then signs, grinds and sends that batch.

64 targets is one signature and one grind per 64 letters, which is the whole
point of this step.


Step C. The floor is published, not asked for
===========================================

A sender solves for what the mailbox charges, because that is in the mailbox's
signed policy and is the only difficulty they can read. A relay's floor is in
nobody's policy, and where such a relay is on the only path, "will not
forward" is "does not arrive" with nothing said to anybody.

The floor becomes a `PeerMeta` key:

```
mailbox-pow-min   the value of (hbs2:mailbox:pow-min D), decimal, omitted at 0
```

written in `mkPeerMeta` beside `http-port` and read back out of `_peerMeta`.
This is not a wire change at all: `PeerMeta` is an association list, unknown
keys are already ignored, the protocol is already rate limited
(`ReqLimPerMessage 0.25`) and already answers only authenticated peers.

One local fix is needed with it. `fillPeerMeta` stops sending `GetPeerMeta` to
a peer once `_peerHttpApiAddress` resolves to a `Right`, so meta is fetched at
most three times per peer and then never refreshed. It should re-request on
the probe period regardless of the HTTP address state, or a floor an operator
changes today is invisible to neighbours until they reconnect.

Why not a refusal message
-------------------------

Section 2 of the open-wire questions proposed a "not forwarded, floor is N
bits" message on the mailbox protocol. It should not be built, and the reason
is not the amplification concern the section raises.

A refusal travels ONE HOP BACK. The refusing relay knows `thatPeer`, which is
the neighbour that handed it the packet, and gossip keeps no reverse path, so
there is nothing to address a refusal to except that neighbour. The sender
learns their letter was refused only in the case where the refusing peer is
their own peer's direct neighbour.

That case is worth something -- if every neighbour of the origin refuses, the
letter never left the machine -- but it is exactly the case a published floor
covers, in advance, without a reply, without a rate limiter, and without a new
message type. A reply is an amplification primitive that has to be bounded and
bound to an address that actually sent the packet, and buying that machinery
for a one-hop signal that a `PeerMeta` key already provides is a bad trade.

`mailboxNotForwarded` stays as it is. The operator who set a floor is the only
party who can weigh what it costs somebody else, and a counter in the periodic
report is the shape that does not become its own denial of service when a
flood arrives.


Step D. The client solves for what it can see
===========================================

`stampsFor` (`hbs2-hub/ingress/HBS2/Hub/CLI/Compose.hs`) solves exactly
`policyPoW` today, so a letter to a mailbox that charges nothing is sent
unstamped and carries zero bits past every relay. With step C the sending
peer knows its neighbours' floors, so the difficulty to solve becomes:

```
max (policy D of the target mailbox) (max floor over this peer's neighbours)
```

At the default this is `max 0 0`, a zero-bit stamp, which `solveStamp` returns
at nonce 0. It costs nothing and changes nothing until somebody sets a floor.

The peer exposes the neighbour maximum over the mailbox RPC so the client does
not have to learn about peer meta. `powBudget` and `solveWithin` already bound
what a grind is allowed to cost, and `PoWTooHard` already says so.


Not in this proposal
==================

**Making the stamp non-optional on `SendMessage`.** Section 5 of the
open-wire questions pairs this with step A. On inspection it buys close to
nothing. A stamp at difficulty zero is solved by nonce 0 and is free, so
mandating the field does not price anything; and at a non-zero floor the
unstamped path is ALREADY closed completely, since it pays a literal zero. The
only gain is one branch instead of two, and the cost is a decode failure on
every peer that has not upgraded, for all plain mail rather than for a subset
of it. Worth doing eventually as a cleanup, not worth doing as a defence.

**A new protocol id for the mailbox protocol.** Not needed. Appending a
constructor keeps 13001 readable by every deployed peer for every packet shape
that exists today.


Compatibility, and the PROTOCOL.md freeze
=======================================

What breaks, precisely, and nothing else: a peer that has not upgraded cannot
decode `DeleteMessagesStamped`, so it neither relays nor accepts it. A stamped
delete travels only over a path of upgraded peers. This is the same cost
`SendMessageStamped` already accepted, recorded in PEP-21 under "The PoW
deployment cost, accepted deliberately", and it applies only when somebody
actually stamps a delete, which at a floor of 0 nobody needs to.

Step B is softer: an old peer decodes a set-valued delete, relays it, and
refuses to merge it with `MergeUnsupportedPred`. Steps C and D change no
format at all.

`PROTOCOL.md` says the wire is frozen as of 0.25.3.0, that future wire-level
features receive new protocol ids, and that existing ids do not get new
payload versions. `SendMessageStamped` already bent that rule, additively and
without saying so. This proposal takes the position that the rule should be
amended rather than quietly broken a second time: `PROTOCOL.md` gains an
explicit exception for 13001 stating that its payload sum may gain
constructors at the end and may not gain anything else, and that a peer
treats an unknown constructor as a packet it drops.

The alternative -- move the mailbox protocol to 13002 and declare 13001
experimental -- is cleaner about the freeze and worse about everything else,
since it partitions on the whole protocol rather than on one packet shape. It
is recorded here so the choice is visible; the exception is what this proposal
assumes.


Order of work, and the one flag day
=================================

Steps B, C and D break nothing and are useful on their own, so they go first:

  1. B, the set-valued delete: reader (`admitDeleted`, `mailboxAcceptDelete`,
     `maxDeleteTargets`, `MergeTooManyTargets`) and builder (`hub drop`
     batching). Testable entirely in `hbs2-peer/test/MailboxMerge.hs`, which
     already has the fixtures.
  2. C, the published floor, plus the `fillPeerMeta` refresh fix.
  3. D, the client difficulty, in `stampsFor`, plus the RPC that carries the
     neighbour maximum.
  4. A, the wire change: the constructor, the preimage split, the marker, and
     the `forwardable` call on the new branch.

Then the part that actually wants the current moment. After A, a non-zero
`pow-min` finally means something other than "stop relaying", and it becomes
possible to argue about what its DEFAULT should be. Changing that default is a
flag day: every sender on the network has to grind before a stranger's letter
crosses a stock peer. The network today is the author's node and two
volunteers, so the flag day costs three upgrades. It will not be that cheap
again, and if the default is going to move it should move now.

This proposal does not pick the number. It says that picking it is a separate,
deliberate decision that A makes available and that nothing before A does.


What this does not fix
====================

  - **Distant floors stay invisible.** Step C tells a sender what its own
    neighbours charge. A relay four hops away with a higher floor still drops
    the packet in silence. In a flood network with no end-to-end feedback that
    is not fixable, and the honest mitigation is the one already in place: the
    operator who sets a floor is the party told what it costs, through
    `mailboxNotForwarded`.

  - **A delete still costs the owner.** Step A charges the one party whose
    identity is provable, because the signature proves ownership of a key and
    not of a hosted mailbox. Step B reduces the cost by up to 64x for the bulk
    case and does nothing for a single delete.

  - **At floor 0 nothing is bounded.** All four steps make a floor usable;
    none of them raise it. Until the default moves, or an operator sets one, a
    delete is still one signature for a network-wide broadcast.

  - **Co-host synchronisation is still one hop.** Two hosts of the same
    mailbox that are not neighbours do not sync with each other, and this
    proposal does not change that. It is what makes multi-hop delete relaying
    load-bearing, and it deserves its own item.
