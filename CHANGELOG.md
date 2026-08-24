# Unreleased

## Added

  - **`hbs2-hub issue open`: the owner can file a bug on their own project.**
    Every thread in canon began as a letter, so opening one meant composing a
    Tier B letter to your own mailbox, waiting for the peer to settle it, and
    folding it -- a mailbox, a sigil, five verbs and a round trip to write down
    something you already know. The bridge has always minted an owner-native
    open and stamped its number; nothing called it.

    Issues only, and not for want of symmetry: a pull request is coordinates and
    a bundle, so an owner-native `pr open` would have to build one. An owner with
    the branch in hand has git, so what is missing there is a different verb.

  - **`hbs2-hub inbox honour`: a request can be granted.** A close, reopen or
    set from a stranger cannot become canon as theirs -- PEP-19 makes those ops
    owner-authored -- so `inbox accept` refuses one, and refusing was the only
    answer this tool had: a request could be turned down or ignored and never
    granted. `honourRequest` and `honourWith` have been in the library since the
    bridge was written, called from tests and from nothing else.

    Verbatim, and only what carries no words: a closing note would become a
    comment authored by the signer, and an attribute is the requester's choice
    of both name and value. Those come back as `NeedsReview`, and the owner
    writes their own.

  - **`hbs2-hub sent` and `hbs2-hub status`.** The sent log has a header
    explaining why seeing what you sent matters; its only reader was `hub
    updates`, as a filter -- the log was kept so an acknowledgement could be
    matched against it, and there was no way to look at the thing being matched.
    `hub sent` prints it, and says out loud that an entry is the peer taking a
    message, not a mailbox accepting it.

    `hub status` answers six questions that were each computable and none of
    which was askable: the repository, whether this machine can sign for it,
    what canon here holds, whether that canon has reached the remote, which
    mailbox the repository declares and how many letters are in it. It pushes
    nothing -- the publication check is a read-only probe split out of `hub
    publish` -- and every absence is a line rather than a silence, because a
    missing mailbox line reads as "nothing waiting".

  - **`hbs2-hub inbox --json`.** The fourth document under the contract counter,
    and the first that is NOT derived from canon: a thread contract is a
    function of the fold, so two clones produce the same bytes, while a queue is
    one mailbox read at one moment by a node holding particular keys, filtered
    by a deny-list that is local and unsigned. Two maintainers will not agree
    and neither is wrong. The counts travel with it, so a consumer cannot read
    `letters` and silently work on a prefix of a mailbox.

  - **`hbs2-hub inbox reject` tells the sender.** `sendAck` had one caller, so a
    refusal and a letter nobody had looked at were the same silence on the
    contributor's side, forever: nothing on their machine changes, and canon
    they can fetch says nothing about a letter that never entered it.

    That acknowledgement cannot be checked against anything, and the help says
    so: an accept's can be, by folding canon and finding the event, while this
    one is a claim about something canon does not hold. `--silent` skips it,
    which is the case rejecting is most often for -- an ack confirms the address
    is live and that somebody read it.

  - **`hbs2-hub issue|pr show --json`: the PEP-22 render contract.** There was
    no machine-readable output at all, so a web layer had nothing to read and
    any other consumer would have had to parse the columns meant for a person.
    This is the thing PEP-22 calls the deliverable that keeps a web layer a
    pure view: one thread as a versioned JSON object, derived purely from
    canon, so two clones of a repository produce the same bytes.

    It carries no `verified` flag and will not grow one. Everything in it has
    already passed the PEP-19 admission check -- a bad signature or an
    unauthorized canon key is dropped by the fold and never materialized -- so
    presence in the document IS verification, and auditing what was dropped is
    `hub verify`'s job.

    A REDACTED ITEM CARRIES NO TEXT. It keeps `redacted: true` and its
    identity, so a renderer can say something was withdrawn, and the title,
    body and attachment are null rather than shipped with a flag beside them: a
    body shipped that way has been published to every renderer that forgets to
    read the flag, which is the exact failure a redaction exists to prevent.

    For a pull request the diff is precomputed from `base..tip`, so a static
    renderer needs no git, and its availability is three-state:
    `reconstructable` is the objects being gone while the bundle attachment
    that would rebuild them is still named by canon, so a renderer can offer to
    rebuild rather than showing nothing.

    Not implemented, and deliberately: the index and activity documents. PEP-22
    leaves both schemas open, so pinning them now would be inventing a contract
    rather than implementing one. Neither is `hub render` (static export) nor
    `hub serve`.

  - **`hbs2-hub whoami`, and a refusal that stops the pair it checks from
    being sent.** A letter needs an author key, a sender sigil and a mailbox of
    your own; three tools make those three things, no verb produced any of
    them, and no help text named a tool that did. `whoami` lists the signing
    keys this machine holds and names the tool for each of the other two. It
    creates nothing: making a key, a sigil or a mailbox belongs to
    `hbs2-keyman`, `hbs2-cli` and `hbs2-peer`, and a fourth tool that also made
    them would be a fourth place for them to disagree about where they live.

    The author key and the sender sigil are separate values that have to agree,
    and the way they disagree was invisible. A sigil naming somebody else's key
    produces a letter that is signed correctly and folds correctly, and the hub
    then declines to acknowledge it, because it will not seal a maintainer's
    reply to a key the sender does not hold. Every step correct, and none of
    them anywhere near the person who could fix it, whose `issue new` had
    exited 0.

    `hbs2-hub whoami --author <key> --sender <hash>` answers that before it
    matters, and the composing verbs now refuse the pair outright: `issue new`,
    `pr new`, `pr revise` and the comment verbs all check it before anything is
    signed or sent, and exit `47` naming both values. It is the same rule the
    hub applies before acknowledging, asked on the machine that can act on it.

  - **`hbs2-hub`: a decentralized forge.** Issues, pull requests,
    comments, labels and merge records live inside the git repository they
    are about, as a signed append-only log under `refs/hbs2/meta` that
    every clone folds offline. Contributors reach a project through an
    encrypted mailbox rather than an account: they compose a letter, seal
    it to the project's ingress mailbox, and a maintainer folds it into
    the log or does not. A stranger's signature never writes the record by
    itself.

    Forty-one verbs, all of them documented by `hbs2-hub help <verb>`. The
    read side (`issue list|show`, `pr list|show`, `log`, `verify`) needs
    neither a peer nor a key, because the record is a git ref. The write
    side is triage (`inbox`, `inbox show|accept|reject`), the owner ops
    (`issue close|reopen|label|assign`, `redact`, `pr merge`, `maintainer
    add|remove`), moderation (`ban`, `block`, `policy`) and housekeeping
    (`compact`). Contributors get `issue new`, `pr new`, `pr revise`,
    `issue|pr comment` and `updates`.

    A pull request ships as a git bundle of the delta, so proposing a
    change to a large repository does not transfer the repository. The
    proposed tip and the fork point are signed and checked against the
    objects that actually arrive; `pr checkout` puts the result on a
    branch, and `pr merge` refuses to record a merge whose commit does not
    contain the tip the contributor signed for.

    Nothing publishes by itself: every verb that writes writes a git ref
    in the repository you are standing in, and `hbs2-hub publish` sends
    it. That is deliberate -- a delegate may bless events into the record
    and cannot push them.

    `docs/hbs2-hub.md` is the guide, and it ends with what is not built
    yet. The largest of those is that there is no web interface.

  - **`hbs2-git3 repo:mailbox:set|sigil|drop|list`: a repository can say
    where it takes contributions.** PEP-18 gives the manifest a
    `(mailbox <key> hub)` clause and a `(mailbox-sigil <key> <hash>)` beside
    it, so a contributor who has the repository can discover where to send a
    letter and which key to seal it to. `hbs2-hub` has always read them --
    `inbox`, `inbox accept` and every composing verb fall back to `--repo`
    when a mailbox or a recipient sigil is not named -- and nothing wrote
    them: this package had no verb that edited a manifest at all. So the
    discovery half of PEP-18 was a reader with nothing to read, and the
    mailbox key had to be published on a web page.

    A mailbox is a SIGN key and sealing to it needs the matching ENCRYPTION
    key, which lives in a sigil, and nothing resolves one from the other; so
    both clauses matter and both are written here. Each publishes as it goes,
    since the manifest is signed into the repository's LWWRef.

    The clause is spelled in this package and read in the other one, which is
    the thing to be careful about. It is checked before anything is stored:
    the manifest is rendered to the exact bytes that will be saved, parsed
    back, and the clause looked for with the same pattern the forge matches.
    A drift on either side is a refusal, or a failing test -- the literal
    text is pinned from both packages.

## Changed

  - **PEP-23: `hbs2:mailbox:pow-min` becomes a price instead of a switch.** The
    floor decides whether a peer carries a mailbox packet onward. A plain
    message and a delete had no field to carry work in, so they paid a literal
    zero, and any non-zero floor stopped that peer relaying all plain mail and
    all deletes outright -- silently, permanently, and with nothing a sender
    could do about it. So the only safe floor was zero, and at zero a delete was
    an unbounded broadcast primitive costing one signature. Four changes, of
    which one touches the wire.

    *A delete names a set of messages.* `Or` had been in the delete predicate on
    the wire since the mailbox protocol was written and no reader honoured it,
    so every tombstone cost its own signature and its own gossiped packet --
    and a reject drops a letter and every rewrapped copy of it. A payload now
    denotes the set its `MessageHashEq` leaves name, up to `maxDeleteTargets`
    (64), which is what fits a datagram and is asserted by a test. `And`, `Nop`
    and a predicate naming nothing stay refused, separately from naming too
    many: a reader that took the leaves it recognised and ignored the rest would
    act on half a sentence. Issue #15 is unaffected -- a proof must still name
    the message the entry deletes. No wire format changed; an older peer relays
    such a delete and declines to merge it.

    *A peer publishes the floor it enforces,* as `mailbox-pow-min` in its peer
    meta, which costs no wire change: an unknown key in that list is already
    ignored. Omitted at zero, told to every neighbour regardless of class -- a
    floor is a price, not a location. The mailbox worker's report gains
    `powFloorNeighbourMin` beside `powNotForwarded`: one says what this peer's
    floor cost somebody else, the other what somebody else's costs this peer.

    *A sender pays for the road out, not only for storage.* What a mailbox
    charges decides whether a letter is stored; what a peer charges decides
    whether it is carried. Each charging recipient is solved at
    `max (policy D) floor`, and a letter whose recipients all charge nothing
    gets one stamp at the floor rather than none -- one for the road and not one
    per recipient, since a relay asks only that the packet carry work for some
    recipient of it. The number comes from the new `RpcMailboxPoWFloor`, which
    answers the peer's own floor together with its neighbours'.
    `hbs2-peer mailbox send` and `delete:message` pay it too.

    *A delete can carry proof of work.* `DeleteMessagesStamped` is appended
    after `SendMessageStamped` -- the one wire change. The work binds to the
    hash of the signed box, which is also the hash of the proof block a
    `Deleted` entry names, so one identity covers the work, the marker and the
    block; a letter's stamp cannot be spent on a delete. The stamped delete gets
    its own dedup marker with the nonce out and the work in. An older peer
    cannot decode the constructor, so a stamped delete travels only over
    upgraded peers -- the cost `SendMessageStamped` already accepted, and one
    that applies only when somebody stamps. `RpcMailboxDeleteMessages` takes the
    stamp, so `MailboxAPIProto` is bumped. `PROTOCOL.md` now states the
    append-only exception for protocol 13001 outright rather than leaving the
    freeze contradicted by the source.

    **The default floor stays zero,** so all of this is inert until an operator
    sets one. Moving that default is a separate decision and is not taken here.

  - **A neighbour cannot set the price of your outgoing mail.** Five defects
    found reviewing the above, all around one fact: part of the floor a client
    is handed comes out of a neighbour's peer-meta, and a neighbour is anybody
    who finished a handshake.

    The floor was read with `readMay` at `Word8`, whose derived `Read` goes
    through `Integer` and then `fromInteger` -- so `"-1"` parsed as **255**, the
    largest floor there is. `peerMetaNat` reads an `Integer` and refuses what
    does not fit; the same trap sat one key over on `listen-tcp`, where `"-1"`
    was a request to probe port 65535.

    The peer took the MAXIMUM over its neighbours and called it "what decides
    whether the packet travels at all". It is not: each neighbour decides
    separately, so one willing neighbour is enough and the number is the
    MINIMUM. Under the maximum, one peer publishing a large number priced
    everything this node sent.

    Neither client survived a floor it could not reach -- both ground and then
    threw -- so an unpayable floor stopped outgoing mail rather than merely not
    being carried. `maxPayableFloor` (20 bits, about a second) caps what a
    client will solve for a RELAY floor, never for what a mailbox charges in its
    own signed policy; above it both send unstamped and say why. The two clients
    also waited 300 seconds and 60 with different messages, and now share
    `powBudget`.

    And, unrelated to proof-of-work: `mailbox delete:message` signed with
    whatever key keyman returned without checking it was the one asked for.
    keyman answers with the credentials of the FILE holding a key, so a mailbox
    key kept as a secondary in its keyring produced a delete against a mailbox
    nobody has, reported as success. The hub has had this check since it was
    written.

  - **`hbs2-hub --help`: the list says what each verb is for, and `hub issue`
    answers.** Forty-one names in one alphabetical column said which words exist
    and not which of them a contributor wants -- `issue new` and `pr new` are
    the whole path in, and they sat between `inbox reject` and `log` with
    nothing to tell them apart. The list is grouped by noun and carries the same
    one-line description `help <verb>` prints, so the two cannot disagree, and a
    verb added to a module appears under its noun by itself.

    `hbs2-hub issue` -- what somebody types before they know the second word --
    answered "unknown verb: issue" and stopped, with the eight verbs it was
    asking for sitting in the dictionary under exactly that prefix. It now
    prints them. And `hbs2-hub help issue` answers in the spelling a command
    line accepts rather than in `hub:issue:close`.

  - **`hbs2-hub`: a number reads back in the form the listing prints.** `hub
    issue list` prints `#7` and `hub issue show <key> #7` was a usage error, so
    the one value on the row a reader was meant to reuse was the one thing
    nothing accepted. It parses wherever a number does, and is still a number:
    `#`, `#-1` and `#x` are still refused.

  - **`hbs2-hub`: an empty listing says which kind of empty it is.** An empty
    project and a mistyped filter printed the same nothing. `--status` and
    `--label` are matched literally against attribute values, which are a
    stranger's bytes and extensible by design, so there is no vocabulary to
    check a value against -- what can be said is what canon holds, and that is
    what an empty listing now names, on stderr, exiting 0.

    In the same family: `hub log <key> <n>` on a number canon does not hold
    exited 0 with no output, which is what a thread with no events looks like;
    it now exits `26` like `issue show`. `maintainer list` in a clone with no
    canon answered with the owner key and nothing else -- a true sentence about
    an empty fold and a false one about the repository -- and now refuses like
    every other read verb. The remedy for missing canon names `hbs2-hub sync`,
    which fetches all three refs, before the raw refspec that fetches one.

  - **`hbs2-hub`: a refusal says what it could not use.** Every argument reader
    here answers yes-or-no, so the failure branch of every verb had one thing to
    say -- the synopsis -- and said it to `hbs2-hub issue list NOTAKEY` and to
    `hbs2-hub issue list` alike. Every identifier in this tool is forty-four
    characters of base58 pasted from somewhere else, which makes "that is not a
    key, and here it is" the highest-value sentence the tool can print.

    It says only what it can know without the verb's own flag list: a key-shaped
    flag whose value is not thirty-two bytes of base58, a flag with nothing
    after it, a flag given twice, and the words no flag claimed. Not "unknown
    flag", which needs the list that lives inside the reader that just failed.
    The exit code is still 1, which is what PEP-22 gives a usage error.

  - **`hbs2-hub inbox`: the queue fits on a screen and carries the subject.**
    A row printed four full base58 values -- about a hundred and eighty
    characters before any of the words -- and the subject was not on it at all:
    it existed only inside `hub inbox show --message <hash>`, one letter at a
    time, so deciding what to look at first meant opening every letter in turn.
    Rows now show the front eight characters of each identifier and the title of
    an open; `--long` prints the whole values, which is what a script pipes.

    The title goes last, because it is the one field of unbounded width and the
    only one a stranger writes as prose: everything a maintainer compares
    between rows stays in the same columns whatever the title does.

  - **`hbs2-hub`: Haskell stops leaking into refusals.** A letter that would not
    open was reported by its constructor, so `NotFetched` and `NotForUs` -- two
    states with opposite remedies -- read alike; `hub log` printed `HubIssue`,
    epoch milliseconds, and a derived `PRCoords` record as the most important
    line of a pull request. Each of those has a renderer written for a person,
    and now goes through it.

    Two escaped through the RTS at exit 1, the code the contract reserves for a
    mistyped flag. A sigil this node has not fetched is a `CreateMessageError`
    that nothing caught, on a contributor's first command; and every keyman call
    on a machine that has never run `hbs2-keyman` threw a raw SQLite error --
    including `hub whoami`, which is the first command in the manual, in the
    fresh-install case it is written for. Both are caught, both say what to do,
    and they exit `52` and `51`.

  - **`hbs2-hub`: advice on stderr can no longer overtake the report on stdout.**
    The streams are buffered differently the moment either is not a terminal, so
    in `hub inbox accept > log 2>&1` -- every CI log of a canon write -- the note
    about publishing came out above the event and the commit it was about. On a
    terminal it looked right, which is why it stayed. The flush moved from
    `refuse` into `saying`, where it covers everything written to stderr and not
    only refusals, and the three verbs that wrote to stderr by hand go through
    it.

  - **`hbs2-hub`: the verbs that send a letter report the same way.** `issue
    new` printed an s-expression with no trailing newline, so the shell prompt
    landed on it, in a vocabulary nothing else in the tool used; its three
    siblings printed lines. All four print lines now, and all four end by saying
    what happens next: the peer has the letter, nothing is in canon until a
    maintainer folds it, and `hub updates` is what comes back.

  - **`hbs2-hub`: `--dry-run` on every verb that writes canon.** `inbox
    accept`, `redact`, `pr merge`, `maintainer add|remove` and the owner verbs
    (`close`, `reopen`, `label`, `assign`) each mint an owner-signed event from
    arguments that are thirty-two bytes of base58, into a log that is
    append-only: a wrong event is answered by another event and never
    withdrawn. `--dry-run` signs nothing and writes nothing, and prints the
    event those arguments produce and the file it would go into.

    It stops at the commit and not before it, which is what makes it a
    rehearsal rather than a restatement of the arguments: canon is read, the
    event is minted, the bridge is asked, the write is planned, and every
    refusal along the way is a refusal in the dry run too. Nothing downstream
    of the commit happens either, so `inbox accept --dry-run` sends no
    acknowledgement and leaves the letter in the mailbox.

    No prompt, no `--yes`, no `--force`: a prompt in a verb that runs from a
    git hook is a hang, and a confirmation people type through is not a check.

  - **`hbs2-hub inbox reject`: `--repo` is now required.** It was optional, and
    the one check this verb makes is against canon: rejecting says the letter
    was not taken, so a letter canon already holds must not be rejected. With
    the flag optional, that check ran only when the caller happened to pass a
    flag they were told they could leave out, and `hub inbox accept` -- which
    decides about the same letter -- has always required it. A break for
    anybody scripting it, made before a release rather than after.

  - **`hbs2-hub maintainer add|remove`: an event that would change nothing is
    refused.** Delegating to a key that is already a maintainer, revoking one
    that is not, and revoking the owner (whom PEP-19 rule 5 keeps in the set
    whatever the log says) each mint a well-formed owner-signed event that the
    fold admits, spends a seq, and leaves the maintainer set exactly as it was.
    The verb then printed "maintainers are now:" and a list, which reads like
    it did the thing -- while the usual way to reach one of these three states
    is a key typed wrong. The set is in the fold the verb already read.

  - **`hbs2-hub`: three advisory lines moved off stdout.** `hub ban list` said
    "nobody is banned here" into the stream a script reads keys from, so a
    caller piping it into a loop got one iteration over those four words.
    `hub ban`/`hub unban` said "nothing to change" and `hub compact` said
    "nothing to compact" the same way, and `hub policy` likewise. All four are
    advice about the command rather than its result, so they go to stderr with
    the rest of it. The exit codes are unchanged and are still what a hook
    branches on.

  - **`hbs2-hub`: a refused write says what the bridge said, in every verb.**
    Four verbs mint an event onto canon and three of them printed the derived
    `Show` of the refusal, so a contributor whose pull request arrived with
    nothing to fetch was told `BadContent CoordsUnreachable`. Only
    `hub inbox accept` printed the sentence the bridge writes for a person --
    and the sentence is the whole reason `TriageError` has a hand-written
    renderer. `hub maintainer add|remove`, `hub pr merge` and the owner verbs
    (`issue close|reopen|label|assign`, `redact`) now print it too.

    The exit codes are unchanged. `hub pr merge` and the maintainer verbs each
    decided in their own words that every way of stopping means one thing to
    whoever ran them -- nothing was published, the repository is as it was --
    and that decision is now spelled once rather than being three identical
    numbers in a row.

  - **`hbs2-hub`: a truncated number index is mentioned by every verb that
    writes one.** Each canon write regenerates `index/number.sexp` from the
    whole fold, so any of them can overflow it, and only `hub inbox accept`
    said so. The note now comes from the shared write, and it goes to stderr:
    it is advice about a convenience map that PEP-19 says is regenerable and
    never trusted, not part of what the verb produced.

  - **`hbs2-hub`: a refused submission says which of five things was wrong with
    it.** Triage printed "kind and payload disagree" for all five, one of which
    is not a disagreement at all: a pull request that arrived with neither a
    bundle attached nor a fork to pull from. The kind and the payload agree
    there and the change is simply missing -- and it is the ONE of the five a
    sender can fix, so it was the one the maintainer most needed to be able to
    quote back. The other four are "a pull request with no coordinates", "an
    issue carrying pull request coordinates", "a pull-request-only op on an
    issue" and a revise whose new coordinates fetch nothing.

    The words are the fold's own, not a second set written for triage. Refusing
    exactly what the fold would drop is this module's whole promise, so an
    operator comparing a triage refusal against a `hub verify` report now reads
    one vocabulary rather than two.

  - **`hbs2-hub`: the list of verbs that need a running peer is now the list of
    verbs that need one.** It used to be the other way round: the 26 verbs that
    need NOTHING but the local repository were named, against the 15 that talk
    to `hbs2-peer`. That put the hand-maintained list on the side that keeps
    growing, and on the side whose mistake is silent -- a verb absent from it,
    or misspelled in it, quietly paid a `hbs2-peer poke` with no timeout:
    1.55 s against a live peer, 6.0 s against a stub, and against a WEDGED peer
    it hung. Which is the peer an operator has when they reach for exactly
    these verbs.

    Named the other way round, both mistakes become loud. A new peer-free verb
    needs no edit at all. A new peer-ful verb left off the list now fails on
    its first run with a message that names the verb and says it is a bug in
    the build rather than in the reader's setup -- because "can't locate
    hbs2-peer rpc", which is what it used to print, is advice for a problem
    they do not have, about a daemon that may be running perfectly well. And a
    name in the list that is not a verb at all is caught at startup, before
    anything dispatches, since a name the dictionary does not hold is a name
    nothing else would ever have examined.

    No verb changes which side it is on.

  - **`hbs2-hub`: what a verb does when a repository has no canon yet is one
    rule, written down.** Eleven verbs read canon and each spelled its own
    answer to a missing `refs/hbs2/meta`. Two answers were in the tree, both
    defensible, neither stated: `hub issue close` refused with "canon is
    unreadable" while `hub inbox accept` in the same repository started from an
    empty fold, and nothing said which was intended.

    The rule is now an argument to a shared reader: a verb that needs canon to
    HOLD something refuses, a verb that ASKS canon a question answers no.

    Three behaviours change with it.

    `hub maintainer add|remove` can now write the first event canon holds.
    Naming a co-maintainer before anybody has filed an issue is an ordinary
    order of work, and it was refused only because `accept` and this verb had
    each written the answer separately.

    `hub compact` with no canon exits 42, its own "there was nothing to do",
    rather than 3, "canon is unreadable". No canon and canon with nothing
    superseded in it are one event for a scheduled run: neither is a failure
    and neither writes anything, so a hook branching on 42 was missing half the
    cases it was written for.

    `hub updates` counts the owner as a maintainer of a canon that does not
    exist, because the owner is one by definition rather than by any event. It
    used to answer an empty set, which refused even an ack signed by the owner
    of a repository whose canon this node had never fetched -- and the owner key
    is the authority such an ack is checked against.

    Two things go with it. Every refusal now prints the REMEDY: ten of the
    eleven printed a bare reason and only the listing verb said that a plain
    clone does not fetch `refs/hbs2/meta` and gave the line that does. And
    `hub updates` no longer branches on `codeOf e == 3` -- control flow keyed on
    a number out of the presentation table, whose own comment says the numbers
    are a contract about stability and not a set of constructors.

## Changed

  - **`hbs2-hub`: every integer in the render contract survives its own stated
    reader.** PEP-22 names a web layer as the consumer and the contract promises
    that what is in it is what canon holds; `JSON.parse` returns a double, and
    two fields were bounded by nothing. `18446744073709551614` comes back as
    `18446744073709552000`, so for those two the promise was false.

    `number` is now capped at 2^53 - 1 by the fold. That is an ADMISSION RULE,
    so it is consensus and it is set now because now is the only time it can be:
    like `maxFoldedTs` it cannot be raised or lowered once a repository has
    canon written under it, and nothing has been published yet. Nothing honest
    is affected -- the cursor starts at 1 and counts, so the range above the cap
    is reachable only by a canon box somebody wrote by hand. The bridge's
    minting gate moved with it, since the module's promise is that it never
    mints what the fold drops. The alternative was emitting the field as a
    string, which would cost every renderer a conversion forever to carry values
    no canon has.

    `declared_at` is the author's own unverifiable claim and canon is right to
    admit it, so the projection clamps it at the ceiling canon admits --
    the same clamp the terminal renderer has applied all along. The two agreeing
    is the point. The other three timestamps are folded-ts, which the fold
    already caps three orders below what a double holds.

    Also there: the contract carries `"document": "thread"`. One version counter
    for three document types, two of which PEP-22 leaves open, would mean two
    different shapes both calling themselves `contract 1` with nothing to branch
    on. A field a consumer handles from the start costs nothing; one added later
    is a field every renderer must treat as optional forever.

  - **`hbs2-hub`: the version a canon commit declares is a function of what it
    holds.** `renderMeta` took no argument and wrote `hubMetaVersion`, the build
    constant, into every commit. So the first accept by a newer build rewrote
    `version` for a canon holding no new event, and every clone on the older
    build then refused the WHOLE tree -- `CanonTooNewHere`, exit 6 -- rather than
    the one event it could not read. Not a degraded view of an otherwise usable
    tracker: the five hundred issues it did understand, gone, with no warning on
    the writing side and no way back, since canon is append-only.

    Version 2 exists because of `PartRef`, and that is a per-EVENT cost: a
    version 1 reader would admit an event this build drops (`PartNotProven`) and
    compute a different event-id for the same letter, so an event that names a
    part cannot be read by one -- and an event that names none can, which is
    most of them and all of them in a tracker nobody has attached anything to.
    `metaVersionFor` derives that from `eventPartRefs` rather than from the
    constructor, because it is the same question and there should not be two
    answers to it.

    A commit now declares the highest version any event it retains needs, and
    never below what the tree already said: a version is a floor a reader has to
    meet, so lowering it would tell a version 1 build it may fold a tree holding
    version 2 events. That case is reachable through compaction, whose retained
    set may no longer hold the event that raised it. An event this build cannot
    decode -- which compaction retains on purpose -- contributes nothing and is
    covered by the declared floor, the version under which it arrived.

    And the `version` file carries a second clause, `(hub-min M)`: the lowest
    reader that still produces a SOUND view. That is a different question from
    what the tree was written under, and it is the one that should gate a
    reader. The 1-to-2 step is `PartRef`, which reaches only events that NAME a
    part: a version 1 reader meeting one cannot decode it, ghosts it -- spending
    its seq, so the numbering does not shift -- and folds everything else
    correctly. The fold's ghost path is built for exactly that and, gated on the
    rules version, could never run: PEP-19 requires a bump for a new op and the
    bump locked the reader out of the tree.

    `hub-min` is a claim about what each bump CHANGED, not something derivable
    from the events: a future version that changes an admission rule, an
    ordering, or how state is derived from an event an older reader can decode
    has to raise it to itself, because below it that reader would not be behind,
    it would be wrong. A tree that omits the clause says its rules version,
    which is what every tree written before the clause existed means, so nothing
    about an older tree changes.

    A second CLAUSE and not a second atom, and that is the only extension this
    file has: the reader tolerates a clause it does not know and refuses one of
    the wrong arity, so `(hub-meta 3 2)` would make the tree unreadable to every
    build that exists. Free to add now, impossible to retrofit into readers that
    have shipped.

    Still open and unchanged: the fold does not select rules by version, so a
    tree declaring 2 is folded by this build's rules and by nothing else. The
    machinery to put a second rule set anywhere now exists (`frMeta` carries what
    the tree declared, rather than the caller collapsing it); the second rule set
    does not.

  - **`hbs2-hub`, `hbs2-git3`: a manifest clause survives a field this build
    does not know.** Both readers listed the clause arity literally, so
    `(mailbox K hub public v2)` matched neither branch and the clause did not
    lose a field -- it DISAPPEARED. `hubMailboxes` answered empty, `mailboxFor`
    said the repository declares no ingress, and a contributor was told it is
    not a forge, while the owner saw the clause in their own manifest with no
    way to learn that half the network did not. The hub's module header claimed
    the tolerant convention and git3 already wrote the tolerant predicate in one
    place and the strict one in another, which is the same drift from both
    sides. Extension by a new field now costs the field; extension by a new
    clause was always free and still is.

  - **`hbs2-hub`: what a letter reads and what it writes are two questions.**
    One constant answered both and was compared in five places, so a v2 build
    had to edit five sites to keep reading the v1 letters already sitting in
    mailboxes -- and the site that gets missed costs nothing loudly:
    `letterThreadId` answers `Nothing` for a good letter, `hub updates`
    correlates against nothing, exit code zero. `hubMsgWrite` and
    `hubMsgReadable` are the same number today and change no behaviour; what
    they change is that v2 edits one line.

## Security

  - **`hbs2-hub`: an object name is the case git writes it in.** `isHexDigit`
    admits `A-F`, which is what git accepts when a person types an id; nothing
    that reaches canon is typed by a person. On the delta path an upper-case tip
    was compared to the signed one as text and reported as "the objects are not
    the ones the contributor put their name to" -- true of the bytes and false
    about what happened. On the fork path nothing fetches at all, so it reached
    canon, and `hub pr checkout` compares the same way: that number could never
    have been checked out, in any clone, ever. Two predicates said this rule and
    had drifted apart in exactly this; there is one now.

  - **`hbs2-hub inbox accept`: an acknowledgement carries the maintainer's
    note.** The ack was assembled by hand beside a function written, documented
    and tested to assemble it, and the hand-built one was wrong in three ways:
    the status came from the fold as it was BEFORE the accept, and the merge
    commit and the note were hardwired absent -- so a closing note, already
    public in canon and the one thing that saves a contributor from going to
    read canon to find out why, reached nobody.

  - **`hbs2-core`: a stream frame is no longer as long as the far end says.**
    Both stream transports read a four-byte length and then that many bytes, as
    given: `recv sock n` allocates a buffer of n, so a peer declaring 4 GiB had
    the process try to hold 4 GiB before a byte of it arrived. On TCP that sits
    behind a four-byte cookie handshake and nothing else; on the UNIX socket it
    is anybody who can open the path.

    Frames now go through one reader with a ceiling (`defMaxFrame`, 16 MiB --
    two orders above anything this codebase sends, since a block is 256 KiB and
    travels in chunks), and the read is chunked, so even a permitted length is
    allocated as it arrives. A frame above the ceiling drops the connection
    rather than being skipped: the length is how a reader finds the next frame,
    so skipping one would be guessing where the stream resumes.

  - **`hbs2-hub`: a pull request coordinate has to be a git name.** The five
    coordinates and a merge's two fields were bounded by SIZE and by nothing
    else, and a coordinate is not a quantity of bytes: it is a ref name or an
    object name. What that permitted was a signed letter whose `base` was
    `--output=sub/` -- a git option, well under any bound, admissible canon in
    every clone forever, waiting for any reader that put it on a command line.
    One such reader existed and was fixed on its own side; this is the rule that
    keeps the next one from mattering.

    An admission rule, so it lands before a release: narrowing what canon may
    hold is a break once anything has been published under the old one. The
    shape checks moved out of the git module into the pure library in the
    process, because the gate that decides what a signed letter may carry could
    not reach a check that lived beside the git calls -- which is how one field
    came to have two shapes.

  - **`hbs2-peer`: a letter to two charging mailboxes reaches both.** A stamp
    pays for one mailbox (PEP-21), so a letter to two of them is two copies of
    one message carrying two stamps -- which is exactly what `hbs2-hub` sends.
    The queue's dedup remembered only whether a copy of that message was in
    flight and whether it paid, and the copies hash alike (a stamp is not part
    of what the sender signs), so the second was dropped as a repeat and only
    whichever stamp arrived first was ever delivered. No attacker involved.

    The dedup is keyed on the recipients a copy pays for now, and a copy is
    admitted when it covers one no queued copy has. The bound is the shape it
    was: at worst one slot per recipient per message per batch, over a recipient
    list the message format bounds.

    In the same place: an unknown recipient no longer counts as payment. The
    admission check fails open by design -- it runs on a cache and the drain is
    the authority -- but it failed open per recipient, so padding the list with
    a key nobody has heard of bought the slot the proof-of-work is supposed to
    buy. The fail-open now applies to the message as a whole: when this peer
    holds a policy for none of the mailboxes named it has no opinion, and among
    the ones it does hold, an unknown key is neither a payment nor a refusal.

  - **`hbs2-hub`: a pull request says what it weighs.** A bundle is a pack, so
    what a contributor sends is compressed at whatever ratio the content
    allows -- 512 MiB of zeros bundles to about 522 KiB, near 1000:1 -- and the
    fetch is cheap because git keeps the pack as it arrived. `hub pr checkout`
    is where it becomes files, and no number appeared anywhere between the
    mailbox and a reviewer's working tree. The attachment bound this hub
    accepts, at that ratio, buys tens of gigabytes.

    `hub inbox accept` now prints what the proposal adds, and `hub pr checkout`
    refuses above half a gigabyte with `--anyway` to proceed. The two differ
    because the moments differ: the accept has already taken the objects, and
    the checkout is where they are written out.

    The number is what git will write, not what git shipped -- the packed size
    is precisely what hides this. And it is measured over `base..tip`, so a
    large project's own history does not count towards the bound and the same
    figure means the same thing in a small repository and in a large one.

  - **`hbs2-hub`: `FETCH_HEAD` is no longer used as a private channel.** It is a
    file in the real git directory that every fetch in the repository rewrites,
    and nothing in this package takes a lock -- so between writing it and
    reading it sat a concurrent `hub inbox accept`, a second `hub sync`, or the
    operator's own `git fetch` in another terminal. `acceptBundle` compared
    somebody else's ref against the letter's signed tip; `hub sync` was sharper
    still, since on a clone with no canon yet it did an unconditional
    `update-ref refs/hbs2/meta` on whatever the file held.

    Neither needed a lock, because each had a better source already in hand.
    `acceptBundle` reads the tip out of the bundle header, which is a file in a
    temporary directory of that call's own -- and reads it before the fetch, so
    a bundle whose tip is not the signed one is refused before a single object
    is written. `hub sync` takes the tip from the `ls-remote` probe it already
    ran and was discarding after a test for emptiness.

    That gave the delta path one new rule: a bundle records exactly one ref.
    PEP-20's delta is `base..source-ref` and git records the one ref that names;
    picking among several by matching the letter's short name against git's
    fully-qualified one would be this build guessing at git's refspec rules on a
    value a stranger chose.

  - **`hbs2-hub inbox accept`: a refused pull request no longer leaves its pack
    behind.** The bundle is fetched into a quarantine so that a proposal the
    maintainer turns down writes nothing into their object store -- unreachable
    objects outlive `git gc` by its two-week grace, so every refusal was disk a
    stranger chose, for a fortnight, in somebody else's repository. One check
    was outside it: that the base the letter signed really is an ancestor of the
    tip it signed ran in the caller, after `acceptBundle` had returned, which is
    after the quarantine was released. A range that is not a range was refused
    with its pack already written.

    The check moved inside, between the tip comparison and the second fetch, and
    the base is an argument now for the reason the signed tip already was: a
    check the caller performs is a check the caller can omit, and the caller
    that omits it stages objects no fork point explains.

  - **`hbs2-hub`: a path git will not index can no longer become canon.** The
    reader took any path component that was not `.` or `..`, so
    `threads/.git/<event>`, `threads/a//b` and `repo/.GIT` folded as events.
    git's index refuses those, and every canon write reads the parent tree into
    an index first -- so `read-tree` fails on the whole parent, and one such
    file anywhere in canon makes the repository unwritable by every verb at
    once, with no verb here able to remove it. The only barrier was `fsck` on
    the four fetches: a whole class of writes resting on one config line.

    Refused by shape now, and by a denylist rather than an allowlist. The
    obvious rule -- accept only the base58 and the digits this layout writes --
    also refuses a multibyte component, which git indexes and which this package
    folds on purpose; a reader stricter than git stops folding canon another
    reader takes, which is a divergence between clones and buys nothing, since
    the wedge is only about paths git refuses. What is refused is four shapes
    that are each refused by git and each impossible in a name this build
    writes: the empty component, anything beginning with a dot, a tilde, and a
    backslash.

    A tree that already holds one now says what that means -- the repository is
    unwritable until canon is rewritten, `hub verify` names the file, and
    `hub compact` writes a lineage without it -- rather than git's `error:
    invalid path` under a sentence about a command nobody ran.

  - **`hbs2-hub`: a redaction hides what it says it hides.** The render
    contract's own header states the rule -- a redacted item carries no text,
    because a body shipped beside a boolean has been published to every renderer
    that forgets to read the boolean. Four ways the code did not keep it.

    **A redacted pull request published the secret the same document had just
    withheld.** `threadContract` hides `part_secret`; `prContract` took no
    redaction argument at all, and for a PR's opening event the two are the same
    bytes -- the fold fills `tsPartSecret` and `psPartSecret` from
    `ccPartSecret` of one canon box. So a document carried
    `"part_secret": null` and, two lines later, `"pr": {"part_secret": "<the 32
    bytes>"}`. The coordinates went out with it: five stranger-chosen strings of
    up to 512 bytes each. The hashes stay -- a bundle part is not text somebody
    wrote, and a renderer offering to rebuild a withdrawn proposal has to say
    there was one.

    **`labels_requested` survived redaction in both renderers**: up to 32 labels
    of 128 bytes, author-chosen, on the one event a redact of an open is usually
    aimed at, printed beside a null title and a null body.

    **The two renderers disagreed about what a redaction covers.** The terminal
    printed a redacted comment's `body-part` hash from outside the branch that
    withholds its body, and told the reader "secret published" for a redacted
    thread -- which is where to go and read it. The contract hid both.

    **And a redact of anything but an open or a comment hid nothing while
    reporting success.** `frRedacted` is consulted in exactly one place, which
    sets the flag on a thread and on its comments, so a redact naming a
    `revise`, a `merge`, a `set`, a note-less `close`, a `delegate`, a `revoke`
    or another `redact` was admitted, spent a seq, moved the thread's `updated`,
    was counted by `hub verify`, was retained forever by compaction -- and
    changed no rendering anywhere, while the verb printed an event id, a seq and
    a commit. A maintainer moderating an abusive revision was told it worked.

    That one is now refused at the bridge, before anything is signed, rather
    than made to work in the projection: hiding what a `revise` contributed
    needs per-source provenance the fold does not keep, since a thread carries
    the latest coordinates and not which event supplied them. `redactable` is
    total over the ops with no wildcard, so an op whose content a reader starts
    showing has to be added there in the same change. `frAdmitted` and
    `CanonView`'s copy of it now carry that answer alongside the scope, in ONE
    record rather than a second map -- the two have diverged five times in this
    module's history and every one was a field the cache updated by its own
    rule.

  - **`hbs2-hub`: three verbs wrote the wrong thing into append-only canon.**

    **The assignee attribute was spelled two ways and nobody agreed.**
    `hub issue|pr assign` wrote `assignee`, singular and scalar, and the
    terminal reader read it back, so those two agreed with each other and with
    nothing else. `multiValued` lists the plural, so `normalizeAttr` never
    touched the value and `hub verify` raised no `UnnormalizedAttr`; and the
    PEP-22 render contract reads the plural, so every thread the shipped verb
    ever assigned came out of `--json` as `"assignees": []` while
    `hub issue show` printed an assignee. PEP-19 settles the spelling and says
    why: an attribute that can hold a set is spelled as one everywhere, so
    nothing has to remember which spelling normalizes. Canon is append-only,
    which is why this had to move before anybody published an assignment. The
    value now goes through `encodeLabels` like every other set-valued
    attribute, and `--to` has room to become repeatable, which the singular
    name did not.

    The spelling is now a constant the writer and both readers take from one
    place (`attrLabels`, `attrAssignees`, next to `multiValued`), so the drift
    is not a thing a test catches but a thing that cannot be written. The
    vocabulary was string literals in eight files, which is why each side could
    be self-consistent, have a test, and be wrong.

    **`--clear` silently beat `--label`.** One reader served four verbs and its
    known-flag set was the union of all four, so a flag the verb in hand does
    not use was not refused but dropped: `issue close --to <key>` was accepted
    and the assignment discarded. Worse, `label` admitted `--label` and
    `--clear` together and resolved the pair in favour of the clear, so
    `issue label --label bug --clear` published an owner-signed event REMOVING
    every label when the operator had asked to add one. The sibling verb refuses
    the same shape two functions away, and this verb's own help spends a
    paragraph saying that `--clear` is spelled out so as not to publish a
    mistake. Each verb now declares its own flags, and the pair is refused.

    **A duplicate number was resolved by hash order.** `DupNumber` is an anomaly
    the fold reports and does not drop, so canon can legitimately hold two
    threads numbered alike -- two maintainers minting from one view is the case
    PEP-19 leaves open. All four resolvers took the head of an unordered
    traversal. For a reader that is an arbitrary answer; for a writer it is
    worse, since `hub issue close --number 42` minted an owner-signed close
    against whichever thread the HAMT yielded first and said nothing about the
    other. One resolver now, `oneNumbered`, which refuses and names both
    thread-ids (new code 49): choosing between two threads to sign against is
    not a decision a tool makes for a maintainer.

    Related, and the same root: `numberIndexOf` sorted on the number alone, and
    `sortOn` is stable, so a tie fell back on `HashMap` order -- two clones
    folding one canon wrote different `index/number.sexp` bytes and so different
    tree and commit ids for it. Nothing materialized differently, since the
    index is a hint no signature covers, but two honest rewrites of one canon
    should not look like different objects.

  - **`hbs2-hub compact` refuses a canon holding a file it cannot read.** The
    rule this verb implements is about EVENTS, and `stEvents` is only the files
    that read, parsed and became one. Everything else -- a blob a shallow or
    partial clone does not have, a file over the reader's bound, a path the
    layout does not have, a duplicate -- is in `stBad`, which the verb never
    looked at; and the writer commits the plan and nothing else, with no
    `read-tree`, so nothing outside the plan survives. A compaction over such a
    tree therefore DELETED those files, reported only the events it had dropped,
    and exited 0.

    Two ways in, neither exotic. A partial clone classifies event blobs as
    absent and `hub verify` there exits 2 without refusing, so compacting
    publishes a canon those events are gone from. And a hostile upstream puts
    one file this reader will not take into its own canon, so that a maintainer
    who compacts launders the finding out of their lineage while it stays in
    everybody else's.

    Refused rather than normalised, and the whole tree rather than the file:
    what a compaction may drop is written down, and a file it cannot read is not
    on that list. New code 48, and the refusal names the files and points at
    `hub verify`, through the same `pathDoc` that verb uses.

  - **`hbs2-hub sync` stops when fetching the code moved canon.** The first
    thing the verb does is fetch the branches, by whatever refspec the remote is
    configured with -- which it does not choose. A remote configured
    `+refs/hbs2/*:refs/hbs2/*`, which is what somebody does after reading our own
    advice to fetch canon that way, has canon inside that refspec. So the
    careful never-force logic underneath ran against a ref the fetch had already
    forced, and reported `CanonSame` about a rollback; with `fetch.prune` and no
    canon on the remote the ref was DELETED and the verb said "the remote has
    none, so nothing here folds yet" having just removed the only copy.

    Canon is now read before that fetch and checked after it. A fast-forward is
    left alone, because on a mirror that fetch is how canon legitimately arrives
    and refusing it would make the verb useless there; anything else is a
    refusal naming both commits and the `git update-ref` that puts it back. The
    objects are never lost, only the ref.

  - **`hbs2-peer`: a delete is no longer relayed at any floor.**
    `DeleteMessages` recovers the mailbox key FROM the signature, so "signed by
    the mailbox key" is satisfied by any freshly generated keypair, and the
    check that this peer hosts that mailbox is downstream of the relay. One
    attacker-side signature therefore bought a network-wide broadcast at fan-out
    per hop, reaching peers that host no mailbox at all. The TODO above the code
    named this and named the remedy.

    The relay stays ahead of the ownership check, and deliberately: a transit
    peer holds no mailbox and must forward, or delivery through intermediate
    hops breaks (PEP-21). What was missing is the gate its two sibling branches
    have -- the peer's own PoW floor, which answers "how much work am I willing
    to amplify". A delete carries no stamp, so it carries zero bits: at the
    default floor of 0 nothing changes for anybody, and any floor an operator
    set now means what they said. The relay memory is consulted only when
    forwarding is intended, for the reason the sibling records: a branch that
    does not forward would otherwise eat a message's first appearance and
    suppress the honest copy everywhere it had not yet reached.

    What this does NOT close, plainly: at floor 0 the amplification is still
    there, and bounding it needs a field for a stamp that the wire does not
    have. That is the same open question as an unstamped `SendMessage` and it is
    answered by a protocol version, not here.

  - **`hbs2-hub`: reading a stranger's pull request no longer hands their text
    to git as an option.** `hub issue|pr show <repo> <n> --json` builds the
    diff of a proposal, and it built the rev range by concatenating two
    coordinates out of canon: `prBase <> ".." <> prSourceTip`, one argv word, no
    shape check and no `--`. Both halves are a contributor's, bounded by size
    and by nothing else, and a fork-path proposal reaches canon with nothing
    verified about it at all -- the accept says so out loud.

    So `--output=sub/` and `/victim.txt` meet through the range's own `..` to
    make `--output=sub/../victim.txt`, and `git diff` truncates whatever that
    resolves to, exits 0 and prints nothing. The path is fully attacker-chosen,
    absolute paths included, and the contract then reports the diff as
    `available` with empty text, so the document lies about it as well.
    Reproduced on git 2.46.

    `validSha` and `validRefName` exist for this and are applied to every other
    coordinate this package hands git; this was the one call that skipped them.
    The coordinates now go as two separate words with a `--` after them, and
    coordinates that are not object names are not asked for at all -- the diff
    is reported unavailable, which is what it is. The decision is
    `Read.diffArgv`, exported and tested, because this module's own rule is that
    what decides something does not live in a `where` clause.

    Also there: `maxDiffBytes` was applied with `Text.length` against a constant
    named for bytes, so a diff of multibyte text reached four times the stated
    bound in the JSON a web layer embeds. It is measured with `utf8Length` and
    cut with `takeBytes`, which cuts between code points.

  - **`hbs2-hub`: a pull request must carry the objects it proposes.**
    `acceptBundle` checked that the fetched tip is the signed one and called
    that proof that the objects are the contributor's, because git's hashing
    binds content to a commit id. It is not proof. The quarantine keeps the
    repository's own object store as an ALTERNATE, so `FETCH_HEAD^{commit}`
    resolves through it, and a bundle with an empty pack -- a v2 header naming
    any commit the maintainer already holds, 106 bytes in total -- passes
    `git bundle verify` ("records a complete history"), fetches with exit 0, and
    produces exactly the signed tip having transferred nothing.

    What that buys is attribution: any tip already in canon, including another
    contributor's accepted `source-tip` or a recorded `merge-commit`, can be
    re-proposed as the attacker's own, and every clone's canon says so. And
    publication: `hub publish` force-pushes `refs/hbs2/pulls/*`, and pushing a
    ref sends everything reachable from it, so a maintainer who merged locally
    and recorded `hub pr merge --commit <sha>` before pushing the branch
    publishes that history on their next publish.

    The question is now asked of the quarantine ALONE, with no alternate: is the
    signed tip an object this fetch wrote. An honest bundle always answers yes,
    since git packs `base..ref` and refuses to build an empty bundle at all. The
    refusal is `BundleNoObjects` and says which commit and why it matters. Tested
    against real git with a hand-built bundle, because `git bundle create` will
    not make this one -- which is the point.

  - **`hbs2-hub`: every verb that signs now checks that the key it was given is
    the key it asked for.** `loadCredentials` resolves a key to the FILE that
    holds it and answers with that file's credentials, whose sign key is the
    file's PRIMARY one. For a key that is a secondary in its keyring, the secret
    that comes back belongs to a different identity -- and nine call sites used
    it and then declared the key they had asked about.

    What that produced is the worst shape a tool can have: `hub maintainer add`
    printed an event id, a seq, a commit and the new maintainer set, and exited
    0, while `hub verify` on the same repository answered "author signature does
    not verify, admitted 0 dropped 1". The event is in canon, it is refused by
    every clone including the one that wrote it, and nothing between the two
    commands said so. `hub inbox accept`, `hub issue close|reopen|label|assign`,
    `hub redact`, `hub pr merge`, `hub issue|pr new`, `hub pr revise`, the
    comment verbs, `hub policy` and the mailbox delete all signed the same way.

    Nothing downstream could catch it. The bridge is handed a secret key and
    told whose it is, so the mismatch is invisible by the time any check runs;
    it has to be caught on the CLI side of that boundary, and now is, in one
    place. `hub whoami --author` was answering the same question with the same
    call, so it said "this machine can sign as it" about a key it could not sign
    as -- the one command written to tell you otherwise.

    Two functions in `HBS2.Hub.CLI.Common`: `signerFor`, which refuses
    credentials whose sign key is not the key asked about, and `signingPair`,
    which takes both halves of a signing pair out of one record so a
    `TriageCtx` cannot be built from two identities. The refusal names the key
    and says no keyring here holds it as its own signing key, which is a
    different sentence from having no key at all.

  - **`hbs2-peer`: reading a mailbox policy no longer costs a merkle read and
    two parses per inbound packet.** A `CheckMailbox` is forty bytes with any
    key in the field, sent by any handshaken peer; answering one meant a
    `getBlock`, a signature check, a merkle-tree read, a text parse and a clause
    parse, with no cache. `MailboxStatus` paid the same on arrival, and so did
    every (message, recipient) pair on the ingest path. None of the three is
    rate-limited -- the mailbox verbs run under `NoLimit` -- and all of them run
    on the deferred pool the other protocols share, so what a stranger bought
    with one packet came out of everybody's budget.

    It was filed in-tree as a performance note. That was the wrong
    classification: unmetered work a stranger can ask for as often as they like
    is a denial-of-service surface whatever the constant factor is.

    The parse is now kept per mailbox, keyed by the hash it was parsed from.
    Content addressing does the invalidating: the same hash is the same bytes,
    a rewritten policy does not match and is re-read, and there is no
    invalidation call to forget. One entry per hosted mailbox that has a policy,
    replaced in place rather than accumulated, so nothing a stranger sends makes
    the map grow. The hash lookup itself stays on every request -- one indexed
    local select, and its result is what tells "the owner said nothing" from
    "the owner said something this build could not read".

  - **`hbs2-peer`: the clock window made the mailbox status check decide
    nothing, and it is gone.** A status counted as fresh if it echoed a nonce
    this peer issued OR its timestamp was within ten seconds of the reader's
    clock. The timestamp is written by whoever sends the status, so the second
    half was satisfied by any peer with a working clock -- which made the first
    half unreachable. `StatusUnasked` was never produced, `useTree` was true for
    everybody, and the status/tree split shipped in `9c3822af` never applied
    once.

    What that was worth to an attacker: a forged status makes the reader fetch
    and MERGE a tree the announcer built. The `Replicated` branch of the drain
    checks the peer policy and the sender policy, and checks no work at all
    under `(pow 0)`, which is the default. Against the open-inbox recipe PEP-18
    gives (`(sender allow all)`, `(peer allow all)`) that is unbounded free
    writing into somebody else's mailbox, around the queue and around the work
    an honest sender pays for.

    Freshness is now the nonce and nothing else.

    **THIS BREAKS SYNC WITH EVERY RELEASED PEER, in one direction.** The echo
    landed after 0.25.5.0, so no released version answers with one, and a peer
    on this build will not take a tree from one that does not. The other
    direction still works: an older requester puts its own clock in the nonce
    field, this build echoes that back, and the old peer's own window check
    passes -- so an un-upgraded host still pulls from an upgraded one. Policy
    propagates either way, since an unsolicited status carries it. What stops
    is upgraded-pulls-from-old, and the accept path says so at `warn` rather
    than `debug`, naming the peer.

    Taken deliberately rather than deferred: there is no compatibility promise
    across 0.25.x, and the network this is reviving has no co-hosting to
    preserve.

## Fixed

  - **The deny-list and the sent log were read whole, and reading them is
    quadratic.** The S-expression parser is superlinear in the number of forms
    handed to it at once: the deny-list measured 42 ms at 1024 bans, 1.97 s at
    8192 and 8.86 s at 16384. It is read on every `hbs2-hub inbox accept`, and
    the sent log grows by one record per letter sent with nothing trimming it.
    Both are read a line at a time now, which is what they are written as, and
    the cost is linear: 0.41 s for the 16384 bans that took 8.86 s.

    Not bounded, which would have been the other way to fix it: both files are
    the operator's own and both grow by the tool working normally, so a ceiling
    on the file would eventually refuse an accept over a list nobody did
    anything wrong with. What is bounded is the line.

  - **A rewrite could drop a delegation and the revoke that undid it, and still
    look like the same canon.** The check that decides whether somebody else's
    rewritten canon is a compaction or a fork compared the maintainer set as of
    the END of the log, where a `delegate` and its `revoke` cancel out. So a
    lineage with the pair removed matched on every field, while what was erased
    is the record that a key was ever authorized -- which is what says whether
    the events it signed were admissible when they were signed. The two
    unconditionally-retained ops are compared directly now.

    The two counters the check omitted are compared as well: the highest number
    and the last folded-ts, on the argument already written for the highest seq.
    A publisher mints the next value from each, and a rewrite that lowers one
    hands the next publisher a value that has already been spent.

  - **git could answer with as much as it liked.** The runner kept stdout with
    no ceiling at all, under a sentence that is true of exactly one caller:
    stdout of `git bundle create -` IS the bundle, so a bound there truncates an
    artifact. Every other caller is asked about bytes somebody else published.
    `git bundle verify` on a header-only bundle of 20000 refs -- a small text
    file, attached to a letter -- answers with 1.1 MB at exit zero, and every
    one of those bytes was then decoded, escaped, split into a line apiece and
    rendered before anything was printed. `git diff` for the JSON contract was
    bounded by nothing but its timeout, and then truncated to 256 KiB after the
    whole of it had been held.

    The ceiling is a parameter now, and the reader says whether it cut: a prefix
    of a message needs no announcement, a prefix of an ANSWER is a wrong answer
    that looks like a right one, so over the bound the call fails instead of
    returning half a listing. `bundle create` passes the size a letter may
    carry, which also closes the other half of this: a bundle too large to send
    is refused as it is produced rather than built whole and held.

  - **A repository's manifest and a mailbox's policy were fetched whole before
    anything asked how big they were.** Both carried a byte bound and both
    bounds were arguments to the PARSER: by the time the parser saw the file,
    the tree had been concatenated into memory and copied strict. So the bound
    said what would be parsed and nothing said what would be fetched, against a
    tree whose size is chosen by whoever published it. A manifest is read by
    `issue new`, `pr new`, `comment` and `whoami --repo` from a key the caller
    typed; a policy is read once per recipient on every send.

    Both are weighed first now: one walk, a byte budget and a block budget, leaf
    sizes taken from the block index and no leaf content read at all -- a tree
    naming a hundred gigabytes is refused having moved none of them. The block
    budget is the half a byte budget misses, since a leaf naming ten thousand
    empty blocks weighs nothing and costs ten thousand lookups.

    The manifest's LOG was read whole to take its first entry, and now stops
    after that entry. It also no longer throws: the ref can be fetched while
    the log under it is not, which is what a peer that has just seen a
    repository looks like, and `hbs2-hub issue new --repo <key>` against one
    died with a raw `MerkleHashNotFound` rather than saying which block it was
    waiting for.

  - **The canon writer checked that the planned file landed, not that the
    inherited one survived.** `update-index --index-info` adds an entry with
    `OK_TO_REPLACE`, so writing `threads/<id>/1.event` into an index that holds
    a FILE at `threads/<id>` removes that file: exit zero, nothing on stderr,
    and the planned path lands -- so the check that was there was satisfied and
    the commit was published with a file of canon silently gone. Reproduced on
    git 2.46 before it was fixed.

    One more `ls-tree -r`, on the parent, and its own refusal beside the
    existing one: a planned file that did not land and a file canon already had
    are different facts. Only where there is a parent to keep, so `hub compact`
    is untouched -- it commits an orphan root from an empty index and drops
    files on purpose. The writer's own haddock had been claiming this direction
    came free from `read-tree`, which is what made the half-check look whole.

  - **`hbs2-hub log <repo> <n>` walked every thread once per line.** The
    resolution from a number to the threads carrying it sat inside the
    comprehension's guard, so it ran per LOG ENTRY, over a canon bounded at
    200000 files in each direction and published by somebody else. It is
    resolved once now, and by the same function every other resolver in the
    package uses: the renderer takes the threads rather than the number, which
    makes the quadratic version unwritable instead of merely absent. The
    refusal for a number canon does not hold used to ask the same question a
    second way, a few lines above, and now shares the one answer. `hub log <n>`
    had no test; nothing asked what the filter answered, so nothing noticed
    what answering it cost.

  - **`hbs2-hub inbox`: the mailbox walk has a ceiling.** Entering each node
    once bounded the SHAPE of a stranger's tree; nothing bounded its SIZE. A
    mailbox is public, how many entries its tree lists is chosen by whoever
    writes to it, and that walk runs in full on `inbox`, on `inbox show` and on
    `inbox accept`, every time -- one call per node and one per entry, at the
    tree's count. Bounding the letters OPENED bounded the keyman lookups and the
    secretbox opens and left the walk itself.

    One budget over blocks, spent by both halves, because charging entries alone
    leaves a million empty leaves free and charging nodes alone leaves one leaf
    naming a million entries free. It is charged when a leaf is entered, so a
    list longer than the budget is refused before its entries are fetched.

    A walk that stops is a WRONG answer and not a short one, the way a hole in
    the tree is: the part not read may hold `Exists` entries, which hides
    letters, or `Deleted` entries, which brings back letters that were settled.
    So it goes through the gate holes already go through -- `hub inbox` exits 2
    and says so, `hub inbox accept` refuses until `--incomplete` -- with its own
    words, because no fetch fixes this one and telling somebody a block could
    not be read sends them looking for a peer that is still downloading.

    What keeps a mailbox under the line is retention, and PEP-21 defers it:
    nothing here prunes a mailbox and triage GROWS one, since rejecting a letter
    writes a tombstone beside the entry it settles. The bound does not fix that.
    It makes a mailbox past the line loud instead of slow.

  - **`hbs2-hub --codes`: the exit codes are a table the tool prints.** PEP-22
    calls them a contract and the manual repeats it; they were defined in
    fifteen modules, four of them documented nowhere, and the next number was
    chosen by grepping for the last one -- which is how two would eventually be
    chosen at once. There is one list now, and it is not copied into the manual:
    a table in two places disagrees with itself, so this one is generated from
    the constants the verbs actually exit with and the manual points at it. The
    constants stay where they are used, since each carries a paragraph about why
    that refusal is worth its own number, and the list is an enum the two
    functions over it are total on -- an entry without a number or a sentence
    does not compile. Six tests, the first of which is that no two share a
    number.

  - **`hbs2-hub issue new`: the positional form is gone.** It took five values,
    four of them base58 blobs, and two PAIRS of those are interchangeable at the
    pattern level -- the repository key and the author key are both signing
    keys, the two sigils are both hashes. So a swap within either pair sent a
    correctly signed, delivered letter authored by the repository key, with no
    error and a zero exit, and an authorship claim lives inside a signed box
    where nothing afterwards can take it back. The verb's own documentation
    said as much and kept the form because the tests drove it, which is a reason
    to keep a form and not a reason it is safe. Every value is behind a flag
    now; a name cannot be swapped silently. Before a release rather than after,
    because afterwards removing it is a break.

  - **`hbs2-hub`: `--key` meant four different kinds of key.** An inner author
    at `ban`, an envelope key at `block`, a canon signer at `maintainer add`, a
    person at `assign --to` -- all thirty-two bytes of base58, so every swap
    between them was well-typed and silent, and each of the four help pages
    spent a paragraph explaining which layer its own verb was about. The verbs
    keep their names, which say what they do; the flag says which layer, which
    is the half that was missing: `--author-key`, `--envelope-key`,
    `--maintainer-key`, `--assignee`. A reader who has the flag right cannot
    have the layer wrong.

    The old spellings still work and are no longer printed, on the same terms as
    `--target`: one release for the scripts that have them. And the same change
    puts `ban` and `unban` on the shared repository flags, so `--target` works
    on the verb and not only on `ban list`, which is where it worked before.

  - **`hbs2-hub`: the six read verbs refused the flag every writing verb
    takes.** `issue list`, `pr list`, `issue show`, `pr show`, `log` and
    `verify` took the repository positionally and only positionally, so
    `--repo` -- which every verb that writes takes, and which a user therefore
    learns from the other half of the tool -- was a usage error on the half that
    reads. All six take it now, through one reader so they cannot drift apart,
    and `show` grew `--number`/`--thread` to go with it. The positional form
    still works: accepting the flag is additive today, and removing the
    positional form after a release would not be, so whichever way this settles
    the flag had to exist first.

  - **`hbs2-hub rm <file>` deleted the file.** The dictionary inherited about
    154 verbs nothing here documents -- `rm`, `mv`, `cp`, `cd`, `setenv` and the
    whole `run:proc:*` family -- so a typo landed in them: `hbs2-hub rm
    victim.txt` removed it and exited 0, and `help rm` answered for it while
    `--help`, which lists the forge verbs, did not.

    The surface is stated positively now: the dictionary is filtered to the
    `hub:` verbs plus `help`, `--help`, `--version` and `--run`. Not by dropping
    a module -- those primitives live next to the evaluator, not in the
    file-operations module their names suggest, so the obvious removal takes
    something else and leaves `rm` bound -- and not by a blocklist, which the
    upstream set would outgrow. `--run` evaluates in the same dictionary, so a
    script gets the forge verbs in sequence and nothing that touches the
    filesystem or spawns a process; anything that has to move a file has a
    shell, and `hbs2-cli` still ships the full set.

  - **`hbs2-hub compact`: it erased the only record a tree keeps of two rule
    sets.** `(hub-event N)` is per file and has no consumer yet; every retained
    file goes back through the renderer on a compaction, and the version came
    from this build's constant -- so a file that said 1 came back saying 2, and
    a reader could no longer tell an old event from a new one, in a tree that no
    longer held the evidence. The one verb that rewrites files was the one that
    destroyed what it had to preserve. The declared version survives now, keyed
    by the path the file is written back under, which is the path it was read
    at. A clause nothing reads yet is exactly the one that must survive until
    something does: it cannot be recovered afterwards.

  - **`hbs2-hub`: two frozen things nothing was watching.** The part-proof
    preimage is `(part, secret, author)` in that order, and the order is as
    frozen as the domain tag beside it, which was carefully labelled while this
    was not: swapping two fields compiles, passes every distinctness property
    the suite asserts, and turns every event that names an attachment into a
    `PartNotProven` drop -- canon that still looks well formed and is quietly
    shorter, with no signature to have failed. And domain separation was an
    argument rather than a check: `Domained` protects the two records this
    package signs, and nothing stopped a record in another package -- a git3
    LWWRef, a sigil -- from acquiring the shape of one, which would make a
    signature made for that record a signed event. Both are pinned by tests now,
    the second with a positive control, since a negative that cannot pass is a
    test that asserts nothing.

  - **`hbs2-hub`: canon bytes depended on the compiler's Unicode tables.** What
    is escaped in a projection is decided by `generalCategory`, which is GHC's
    table and not a constant, so a code point that moves into `Cf` in a later
    Unicode revision is escaped by one build and not another -- the same event
    rendering to different bytes, and canon's bytes are a commit id. Two clones
    would hold canons that say the same thing and compare as diverged. The
    predicate is pinned by a checksum over every code point: that does not make
    the classification portable, and nothing short of an explicit table would,
    but a compiler whose tables differ now fails the build instead of forking
    canon in silence.

  - **`hbs2-peer`: three unbounded stores behind the mailbox worker, and a
    batch that took the whole worker down with it.**

    The merge queue took every `Deleted` entry of a downloaded tree with no
    validation, and put an entry back whenever its proof block was absent, for
    as long as it stayed absent. So a tree a stranger uploaded -- about ten
    megabytes for a hundred thousand entries naming proofs that do not exist --
    bought a hundred thousand permanent queue entries, and behind each of them a
    `wip` entry the downloader sweeps only on completion and a row in the brains
    database that survives a restart, plus a storage read per entry per poll.
    Bounded per mailbox now, in `enqueueMerge`, which is the one function all
    three call sites go through. Nothing is lost over the cap: an entry that
    does not fit is still in the tree it came from, and the next status walks
    that tree again.

    An accepted message's attachments went to the downloader one by one with no
    cap, on a different path from the one `maxMailboxDownloads` guards, so a
    single message could commit an unbounded number of downloads. The reader's
    own bound fires much later and cannot help: hbs2-hub walks at most sixteen
    parts, at triage, by which time the peer has paid for all of them.

    And the drain took a whole batch out of the queue before processing it, with
    no handler: any throw -- a busy sqlite, a storage error -- unwound out of
    the worker, which was then torn down and restarted, and the rest of the
    batch went with it, already out of the queue and in nobody's. Only messages
    a co-host will send again ever came back. Confined to the message that
    raised it, with `tryAny` rather than `try` so that cancelling the worker
    still stops it.

  - **`hbs2-peer`: a relay's proof-of-work floor was invisible to everyone.** A
    sender solves for what the MAILBOX charges, which is in its signed policy; a
    peer's `pow-min` is that peer's own number and is in nobody's policy, so a
    relay set to 16 does not carry a message that honestly paid the 12 its
    destination asked for. The only trace was a debug line. It is counted into
    the periodic report now -- a warning per message would fire hardest exactly
    when a flood is arriving, which is what the floor is for -- and the option's
    own documentation says what a non-zero value costs somebody else. The sender
    still cannot observe the floor it failed; that needs a refusal on the wire.

  - **`hbs2-hub`: one event could put a chosen letter beyond folding, forever
    and in silence.** An event's `origin` says "I was folded from the message
    with this hash", and nothing bound the claim to a message that exists -- the
    fold records it for any admitted event and cannot do better, having no
    mailbox. So an authorized key could mint one ordinary event carrying the
    hash of a letter it wanted kept out, and that letter was refused as already
    folded from then on: the origin set never shrinks, so revoking the key did
    not free it, and `DupOrigin` fires only on a SECOND event with that origin,
    so `hub verify` reported nothing.

    The other end of the claim was already in canon. Folding a letter produces
    an event whose id is the hash of the author's inner box, and honouring its
    request records that same box id, so the gate now fires only when canon also
    holds one of the two. A claim that fits neither is ignored rather than
    reported: the gate exists so a triage loop re-reading a mailbox after a
    restart does not fold one letter twice, and in that case canon does hold the
    letter's own event. Where it does not, the letter folds and canon ends up
    with two events claiming one origin -- which is what `DupOrigin` is for, so
    the attack turns itself into a reported anomaly instead of a silent block.
    `inbox reject` asks the same question, or the two verbs would disagree about
    one letter.

  - **`hbs2-hub inbox`: the queue could be pushed off its own first page, and
    a ban did not take a letter out of it.** Two ways the only screen a
    maintainer triages from was a stranger's to fill.

    The page is the first N of the live set in hash order, and a mailbox is
    public, so which N those are is chosen by whoever writes to it: grinding
    message hashes below the honest ones is a few thousand signatures and
    displaces every real letter, permanently, since nothing raised the bound
    and the only remedy was rejecting junk one hash at a time. `--after` walks
    past it and the truncation note prints the value to continue with; `--limit`
    lowers how many are opened and is clamped, so it cannot raise the cost past
    what one read will take. By hash and not by offset, because the set grows
    underneath and an offset would skip a page.

    And `igAllowed` has always said the queue applies the deny-list "because
    triage is a queue a human reads and a banned author's letter should not be
    in it". It was in it, as one more unreadable line. Such letters are left out
    and COUNTED -- dropping them silently would leave a queue that says nothing
    about how much of itself a stranger is producing, and no sign at all to a
    maintainer who banned a key by mistake. The slot is not given back and
    cannot be: the ban is on the inner author, so the letter is decrypted before
    anybody knows whose it is.

  - **`hbs2-hub inbox accept`: it folded over a mailbox tree it had not read
    whole.** `hub inbox` has said for a long time that a chunk which would not
    read makes its list wrong in BOTH directions -- a missing chunk of `Exists`
    entries hides letters, one of `Deleted` entries brings back letters that
    were already settled -- and exits 2 for it. The two verbs that ask a
    MEMBERSHIP question over the same walk read only the live set, so the
    reasoning stopped at the verb that lists and never reached the one that
    publishes to every clone forever.

    `accept` refuses now, rather than warning: the act is irreversible, and a
    warning before an irreversible act that proceeds anyway is a warning nobody
    reads twice. `--incomplete` is the way past it, and it exists because a
    mailbox tree is a stranger's -- without it one block nobody can serve would
    make a mailbox permanently unacceptable-from, which is a denial anybody
    could arrange. `inbox show` writes nothing, so it says so instead of
    refusing, and both verbs qualify their "is not in mailbox" refusal, which
    is the direction in which it can be a lie.

  - **`hbs2-hub`: a repository could send every contribution to the wrong
    mailbox, silently.** The manifest carries two clauses written by hand and
    only one of them decides anything: `resolveKeys` on the peer takes the
    recipient's sign key out of the SIGIL's own signed box, so
    `(mailbox-sigil K H)` where `H` names some other key delivered every letter
    to that other key's mailbox. The owner read `K`, saw an empty queue, and had
    no reason to doubt it; the contributor was told `queued` and exited 0; no
    layer between them was wrong about anything. `sigilNames` is exactly the
    predicate for this and was applied on the reply side only.

    The sigil is checked against the mailbox it was published for, and the
    refusal names both keys, because the fix is to change one of the two and the
    owner has to see which. One block read, and the sigil is about to be read
    anyway to seal the message. A sigil named by hand with `--recipient` is not
    checked -- that is the documented way to mean a mailbox other than the
    declared one -- and one this node cannot read proceeds rather than accuses.

  - **`hbs2-hub`: the reply channel claimed a privacy it does not have.** Its
    comment said a contributor's mailbox key must not end up in every clone,
    which is why it sits outside the inner box. It does end up there: the
    channel key has to equal the inner author (or a hub could be made to send
    maintainer-signed acks to somebody who asked for none), and the author key
    is what canon publishes verbatim, forever. Corrected in the code and in the
    manual, which now has a section saying it plainly: a reader of canon can
    NAME your mailbox and watch its tree, and cannot put anything in it --
    sealing needs the encryption key, which is in your sigil, which canon does
    not carry. Nothing about identity changes either way, since the author key
    already correlates a person across everything they have written in.

  - **`hbs2-hub inbox`: one letter under many envelopes cost one decision
    each.** Re-signing a captured message under a fresh key needs no key and no
    plaintext: a signed box keeps its payload as opaque bytes and the message
    carries no sender field, so anyone who saw the ciphertext go past can put
    their own signature on it. PEP-18 reads as though this is available to a
    recipient; it is available to a bystander.

    Canon was never at risk -- an event is named by the hash of the author's
    inner box, so a second copy is refused as already folded -- but everything a
    PERSON does was one-shot: a queue line, a decision and a tombstone per copy,
    for a letter read once.

    The queue groups copies onto one line with a count, and `inbox reject` drops
    the group. The grouping key is the hash of the ciphertext, which a re-wrap
    cannot change (producing a different valid ciphertext of one plaintext needs
    the plaintext) and which this node computes before any keyman lookup, so a
    letter now costs one decrypt however many envelopes it arrives under. A copy
    made distinct by padding the ciphertext no longer opens, so it is a line
    rather than a decision.

    WHAT THIS DOES NOT FIX, now said in `hub ban`'s own help and in the manual:
    every copy carries the same inner author, so the only ban that stops a flood
    names the person whose letter was captured. `hub block` names the envelope
    key and a re-wrapper mints a fresh one per copy. An open inbox has no
    re-wrap resistance of its own; what prices the flood is `hub policy pow`,
    which charges nothing until it is set.

  - **`hbs2-hub inbox accept`: it decrypted every attachment a message carried,
    and published a key to all of them.** Two halves of one gap between what a
    message carries and what its letter names, and nothing compared the two
    sets.

    The walk took the message's whole part set while the gate that reads its
    result only ever looks up what the content references, so a letter naming
    nothing could still have this node fetch, measure and decrypt sixteen
    attachments of 64 MiB, all of it live at once, for an event that would point
    at none of them. It reads the inner box first now -- a signature check over
    bytes already in memory -- and walks only what the letter names.

    And the secret the owner signs into canon is ONE KEY FOR THE WHOLE MESSAGE.
    PEP-18 argues that publishing it is safe because it opens what the event
    points at, which is only true when the two sets agree; a letter naming one
    part in a message carrying sixteen had the owner publish the key to all
    sixteen, in public append-only canon, forever. A message carrying an
    attachment its letter never names is refused, which is what makes the
    argument true. The refusal is decided from what the message DECLARES, so it
    needs nothing opened.

  - **`hbs2-hub`: a banned sender could park a letter in the queue forever.**
    `openLetterAs` has a branch that applies the deny-list to the envelope key
    when a letter's schema is one this build cannot read -- the one case where
    there is no inner author to ask about -- and it was dead code: the version
    is checked a layer earlier, so such a letter never reached the function that
    would ban its sender. The tests reached it by building the value by hand, so
    they were green over a path production does not take. Since an
    unreadable-version letter is a wait rather than a discard (rightly: a newer
    build folds it), every pass opened it again. The check now runs where the
    version is actually decided.

  - **`hbs2-hub compact`: every compaction commit was dated 1970.** The stamp
    is deliberately taken from canon rather than from a clock, so that two
    maintainers compacting one canon produce one commit -- but the field taken
    was `frMaxSeq`, a position, where `cnWhen` is documented as epoch
    milliseconds and the writer divides it by 1000. So a canon holding forty
    events was published at the epoch while every other canon writer passed a
    real clock, and a canon whose highest seq passed 2^63-1 could not be
    compacted at all, git refusing that date. `frLastFolded` is the same
    determinism and is the value the field asks for; it is bounded by
    `maxFoldedTs`, which is an admission rule, so no admitted event can carry a
    date git will not take.

  - **`hbs2-hub pr merge`: the ordinary way of naming a commit was refused.**
    `--commit` went to a check written for the shape CANON holds -- 40 or 64
    hex -- so the seven characters `git log --oneline` prints came back as the
    refusal written for a forged letter ("a signed letter says who wrote a
    name, not what it is"), at an exit code documented as belonging to
    `hub pr new`. An abbreviation is resolved through git now, and the whole
    object name is what canon records: an abbreviation is unambiguous only in
    the repository it was read in and only while that repository stays this
    size, and a merge event is permanent. Still hex only, not a branch or tag
    name, for the same reason.

  - **`hbs2-hub`: one canon file could end a repository, and `hub-meta` is 3.**
    A key whose delegation was current spent its stamp outright, so a single
    canon box at `seq = maxBound - 1` put the cursor at the top of its range,
    and every entry point in the bridge is gated on that cursor -- including the
    `revoke` the owner would answer it with. PEP-19 promised a recovery
    (compaction re-stamping the retained events) that was never implemented, and
    promised that `hub verify` would notice beforehand, which it did not.

    A window existed and applied only to a key whose delegation had ALREADY been
    withdrawn: it bounded the case where the owner has noticed and left the one
    where they have not. It applies to every authorized key now. A bound that
    only takes effect after the harm is visible is not a bound.

    THE TWO COUNTERS TAKE DIFFERENT WINDOWS, because their honest gaps differ.
    Numbers are handed out one per `open` and compaction never drops an `open`,
    so the surviving numbers are dense and the window is 16; a number outside it
    is REFUSED, since a number is a label a reader is shown. Sequence numbers
    are not dense -- compaction drops superseded events out of the tree, so the
    gap between two survivors is however many went between them -- so that
    window is 2^24 and a seq outside it is ADMITTED and simply does not move the
    mark. Refusing it would let a legitimately compacted canon become
    unfoldable, which is a repository bricked by its own maintenance; leaving it
    admitted costs at most a later duplicate seq, which is reported and ordered
    deterministically. A generous window is still a bound: reaching the top of a
    2^64 range through a 2^24 one takes 2^40 files, at which point flooding
    canon is the cheaper attack.

    `hub verify` reports the stamp that did not move the mark
    (`SeqTooFarAhead`), which is the only trace such an event leaves and is what
    PEP-19 asked for. A tree that lost files and a hard compaction produce it
    too, and the report says so.

    This is a consensus change and it raises `hub-min` with it, which no bump
    has needed before: `frMaxSeq` IS the cursor, so two maintainers on either
    side of the change would mint from different positions and fork rather than
    one of them lagging. Free today because nothing has been published, and
    impossible later, which is the whole reason it is being done now.

  - **`hbs2-hub updates`: an ack was checked against the wrong repository's
    maintainers.** The `RepoRef` an ack names was passed into the predicate and
    then discarded, so membership was asked of the set belonging to the
    repository the READER named. A maintainer of that repository could sign an
    ack about a different one and have it printed as a maintainer's word about
    the reader's own submission, with a status and a note of their choosing; a
    thread-id is the hash of an author box in public canon, so naming one costs
    nothing. The line now prints the repository the ack is about, which is what
    made an honest cross-repository ack indistinguishable by eye from a forged
    one.

  - **`hbs2-hub publish`: the scenario it is shaped around answered with a raw
    git error.** It learned the remote's canon from `ls-remote` and asked
    `merge-base --is-ancestor` about it -- but a clone that has never fetched
    does not HOLD that object, which is what "the remote is ahead" means, so
    the default path produced "fatal: Not a valid commit name" at exit 46 and
    the sentence written for the case appeared only after fetching by hand. Not
    having the object is the strongest form of holding canon this clone does
    not contain, so it is now the same refusal, with the same remedy.

  - **`hbs2-hub publish`: it force-pushed staged proposals after refusing to
    publish canon.** The canon push is a fast-forward check and the pulls push
    is a force, and the second sat outside the branch that decides the first --
    so a run that printed "NOT published, nothing was written" and exited 45
    had just replaced the remote's `refs/hbs2/pulls/*` in the same breath.
    Those refs are numbered out of canon, and the canon they were numbered out
    of is the one this clone does not have. They are held back now, and the
    report says so rather than leaving a reader to infer it from the canon
    line.

  - **`hbs2-hub`: the deny-list was rewritten in place.** A torn write leaves a
    file that is SHORTER and still parses -- every line is one key -- so an
    interrupted `hub ban` silently unbans whatever came after the break. That
    is the one failure the design otherwise excludes: `loadBans` refuses a file
    it cannot read entirely, precisely so that a list somebody shortened is a
    refusal rather than a shorter list. Written beside it and renamed over it
    now, in the same directory so the rename is atomic.

  - **`hbs2-hub`: two files that are not UTF-8 threw past their own contract.**
    `loadBans` and `loadSent` answer `Either Text`, and read through
    `Data.Text.IO.readFile`, which throws `UnicodeException` -- so a corrupt
    deny-list aborted an accept with a raw exception instead of the refusal
    that has an exit code. Both catch it and report it as what it is.

  - **`hbs2-hub`: `hub issue new` lost its exit code on an oversized field.**
    It left through `throwIO (userError ...)`, which exits 1 via the RTS, where
    the same check in `issue comment` and `pr new` goes through `refuse` --
    the defect `refuse`'s own haddock is a paragraph about.

  - **`hbs2-hub`: two positional numbers bypassed the guard written for them.**
    `hub log <repo> <n>` and `hub issue show <repo> <n>` did an unguarded
    `fromIntegral` on the `Integer` a literal carries, while `flagWord` exists
    because `--number 18446744073709551617` once wrapped to 1 and the verb
    answered about a thread nobody named. Display-only here, and the rule has
    one source of truth or it has none.

## Changed

  - **`hbs2-hub`: the shared CLI plumbing is no longer part of the `hub inbox`
    verb.** `refuse`, `saying`, the peer wiring and the exit codes lived in
    `HBS2.Hub.CLI.Inbox`, and twelve other verb modules imported them from
    there -- so `hub compact` depended on the queue verb in order to know how
    to refuse, and `HBS2.Hub.Deny` carried a note about routing around the
    near-cycle that caused. They are in `HBS2.Hub.CLI.Common` now, which is a
    module and not a verb. No behaviour changes; what changes is that the exit
    codes, which PEP-22 makes a contract, are in one file.

  - **`hbs2-hub`: `pr` is a whole noun.** `close`, `reopen`, `label` and
    `assign` were bound under `issue` only, so closing a pull request was
    spelled `hub issue close --number <pr number>`. That WORKS -- the number
    index does not filter on kind -- and nobody guesses it. All four are bound
    under both nouns now, which is what `pr comment` had been doing since it
    was written, for the same reason: a thread is an issue or a pull request
    and these ops care about neither. `AClose` carries no kind.

  - **`hbs2-hub`: `issue|pr comment` takes `--number`.** Every other thread
    verb resolves a number against the local fold; this one required
    `--thread` and 44 characters of base58 that no listing prints -- `hub issue
    list` shows `#3` and `hub issue show` had to be read to find the id. It now
    takes either, and refuses both at once, a number with no `--repo` to
    resolve it against, and a comment naming no thread at all. The letter is
    unchanged: it still carries a thread-id and no repository, which is what
    PEP-18 says a comment names.

## Fixed

  - **`hbs2-hub`: a noun was not a help topic.** The top-level help prints
    verbs as `issue new` and `pr list`; the search behind `hub help <word>`
    matched the raw dictionary keys, every one of which begins `hub:`. So the
    spelling the help printed could never match the search the help
    advertised, and `hbs2-hub help issue` -- the first thing anybody types --
    answered that no such entry exists. It now tries this tool's own spelling
    first and what was typed second: `help issue`, `help pr` and `help
    maintainer` list their families, and `help print` still finds the builtin.
    Ordering matters and is why it is written down: with the other order,
    `help pr` answers with `print`, `println` and `proc:pipe`.

  - **`hbs2-hub`: `hub help unban`, `unblock`, `issue reopen` and `maintainer
    remove` described their siblings.** Each pair is built from one helper with
    one `desc`, so the help for the second verb opened with a paragraph about
    what the first one does and never said what the second does. Each now
    leads with its own sentence.

  - **`hbs2-hub`: `hub help block` denied that `hub ban` exists.** It ended
    "keeping an author out of canon is the triage layer, which this build does
    not have" -- while `hub ban` is in the same binary and in the same listing,
    and `hub policy pow`'s help fifty lines away points at it. The wrong one
    was on the verb a maintainer reaches for first, so somebody being spammed
    would block the envelope key, watch the spammer rewrap, and conclude the
    tool could not stop it.

  - **`hbs2-hub`: `--flag=value` on the last three verbs that refused it.**
    `ban list`, `maintainer list` and `policy show` matched their arguments by
    hand, in one fixed order. They go through `flagsOf` now, like every other
    verb and like the spelling `hub issue new`'s usage advertises to everybody.

  - **`hbs2-hub`: `--version`, and two exit codes that were not what they said.**
    There was no way to ask which build was answering, though the exit codes
    are a documented contract that may be added to: `--version` and `version`
    were both "unknown verb". `codeNothingToCompact` (42) was defined,
    exported, documented as the thing a scheduled compaction tells from a
    refusal, and returned by no path -- the verb printed and exited zero.
    And `hub sync` exited 27 on a git failure, which is documented as
    belonging to `hub pr new`: a number naming a command nobody ran. It has
    its own now, shared with `hub publish`, since those two are the ends of one
    wire.

  - **`hbs2-hub`: an unknown verb named the word that was right.** `hub issue
    nwe` reported `unknown verb: issue`, pointing at the half that is a verb
    and saying nothing about the half that is not. It prints the whole line.

  - **`hbs2-hub`: several synopses showed optional flags as required and left
    others out entirely.** The SYNOPSIS line is rendered from each verb's
    `args` list, which carries no optionality marker and had been filled
    inconsistently. `hub help sync` never mentioned `--repo`, which is the flag
    that turns a divergence into a resolved compaction, while `hub help
    compact` tells you to run exactly that. `hub help compact` itself omitted
    `--dry-run` -- the flag its own prose says to run first. `inbox accept`
    omitted `--as` and `--keep`; `issue close` showed `--note` as required;
    `comment` never mentioned how to name a thread. Optionals are bracketed
    now and the missing flags are there.

    Not fixed: these are still hand-written per verb rather than derived from
    each reader's own flag list, so the next one added can drift the same way.

  - **`hbs2-hub`: `hub issue list` took a flag as a filter value.** It was the
    last argument reader in the package not going through `flagsOf`, and it had
    the hole the shared reader exists to close: its check looked at
    even-indexed words only, so `hub issue list K --status --label` passed it
    (index 0 is a known flag, the length is even) and the pairing then bound
    `--label` as the value of `--status`. A listing filtered on a status nobody
    typed, exit zero, which is the worst shape a filter can fail in: the caller
    reads the output as filtered. This is the same case `c40ed994` removed from
    six other readers.

    It goes through `flagsOf` now, which also brings `--flag=value` -- the
    spelling `hub issue new`'s usage has been advertising to everybody and
    which this verb refused -- and refuses a stray word and an unknown flag
    instead of answering with an unfiltered listing.

  - **`hbs2-hub`: nine of the ten ops were rendered by no test.**
    `contentDoc` dispatches on every constructor of `AuthorContent` and one
    test passed `AOpen`. The other nine were unreached on the verb that prints
    a stranger's UNFOLDED letter -- the surface that produced `054d4dd2`, and
    the thing a maintainer reads while deciding whether to sign it into canon
    forever. One escape in one field of one op rewrites the lines above it in
    that terminal, and which arm carries it is the attacker's choice: they
    write the letter.

    A table drives all eleven shapes with `\ESC[2K` in every text field. No
    defect was found -- every arm was already correct -- and the point is that
    nothing said so, and nothing would have said otherwise. Two more assert
    what escaping is FOR: the bytes survive (a maintainer has to be able to
    read what they are deciding about) and a newline cannot forge a field of
    the report.

  - **`hbs2-hub`: a test in `ReadSpec` that could not fail.** It asserted that
    a forged row was absent from `unlines (drop 1 (lines out))` after the line
    above it had established there was exactly one line -- so the predicate ran
    against the empty string and was constantly true. The row count was the
    only real assertion, and a renderer that CLIPPED a title at its newline
    would have passed it while silently throwing away what a stranger wrote.
    It now asserts that the newline is escaped rather than dropped.

  - **`hbs2-hub`: the step that chooses the attachment secret canon publishes
    forever was run by no test.** `igOpenPart` turns a fetched encrypted tree
    into the `PartSecret` the owner puts into canon, where it stays in every
    clone that ever fetches the thread. It had four sites in the repository:
    the declaration, the production wiring, one caller inside the accept verb,
    and a stub in the test suite that answers `Left`. No test overrode the
    stub, so every `PartOpened` in the whole suite was a fixture literal and
    the arm that picks the published secret was reached by nothing.
    `measurePart` was tested at one end and `requireParts` at the other; what
    joins them was not.

    The loop is now `partEvidence`, a named exported function rather than
    fifteen lines inside a verb, and five tests drive it with an
    `igOpenPart` that really returns bytes and a key. What they pin: the secret
    arrives in the evidence unchanged, a secret of the wrong length is refused
    rather than published, a part this node cannot open is locked, an
    oversized part is NOT opened at all (opening is the spend the size gate
    exists to prevent), and a part still arriving is told from one that will
    not be taken.

    That last one also closes a gap in the existing fixture, whose two
    `igSize` branches returned the same value, so the "not all of it is here"
    answer was never produced -- the case the accept turns into a retry rather
    than a refusal.

## Security

  - **`hbs2-hub`: `hub compact` could publish canon with an event silently
    gone.** `git update-index --index-info` answers a path it will not take
    with `Ignoring path ...` on stderr and EXIT ZERO, so `write-tree`,
    `commit-tree` and `update-ref` all succeed on a tree with the file missing
    and the verb reports the commit it made and the way back. Reproduced on git
    2.46 with `threads/../x`: the entry is dropped and `write-tree` returns the
    empty tree.

    The path does not have to be one this build chose. A compaction writes back
    the names the tree already had, deliberately -- deriving them instead would
    quietly rename a misnamed file, which `hub verify` reports and a compaction
    is not entitled to repair -- so one file somebody else put under such a
    name is enough.

    Two independent halves now. The reader will not fold a path whose
    components are not names, so `threads/../x` is not an event here rather
    than a malformed one. And the writer lists the tree it built and refuses to
    commit unless every planned file is in it, which does not depend on knowing
    which paths are bad: what was planned either landed or nothing is
    published.

  - **`hbs2-hub`: `hub sync` was the one fetch with fsck off.** `acceptBundle`
    sets `transfer.fsckObjects` and `fetch.fsckObjects` and says why -- the
    objects are a stranger's and git's own history has malformed-object
    vulnerabilities in it. `syncFrom` makes three fetches and set neither flag,
    and its objects are the ones `hub pr checkout` puts into a reviewer's
    working tree. A hostile upstream publishes both the canon coordinates and
    the pull ref, so the mismatch check `prCheckout` does passes and only git's
    checkout-time defences are left. All three fetches go through one helper
    that sets both flags now.

  - **`hbs2-hub`: a fork-path pull request entered canon with nothing checked
    and nothing said.** PEP-20's second path names a fork to fetch over hbs2
    instead of carrying a bundle, and this build cannot fetch one (git3's
    remote helper has no fetch-one-ref). So `bundleOf` answered `Nothing`, the
    whole verify block was skipped, `stagePull` was skipped, and the accept
    printed exactly the lines it prints for a verified proposal. Canon then
    carried, in every clone, a signed claim that the tip exists and the base is
    its ancestor -- which nobody had established. The maintainer found out at
    `hub pr checkout`, which reported nothing staged.

    Not refused, since the path is in the spec and the fetch is what is
    missing; said. The accept now names the source, the tip and the base, says
    that none of them was verified and that nothing is staged, and tells the
    maintainer to fetch the fork before merging anything.

  - **`hbs2-peer`: a proof-of-work floor was bypassed by not carrying a
    stamp.** `mailboxPoWFloor` -- what an operator sets with
    `hbs2:mailbox:pow-min` to say how much work this peer is willing to
    amplify -- was consulted only in the `SendMessageStamped` branch. A plain
    `SendMessage` was relayed unconditionally, which is every message a peer
    built before PEP-21 sends and every message an attacker chooses to send.

    Relaying is `broadCastMessage` to every known peer, and the mailbox
    protocol is registered independently of the mailbox worker, so a peer that
    hosts no mailbox at all amplifies too. One throwaway keypair, a signed
    `MessageContent` with an empty recipient set, and a byte varied per packet
    was a network-wide broadcast storm that the one setting meant to bound it
    did not touch.

    An unstamped message now carries zero bits of work, which is what it
    carries. A floor of zero -- the default -- forwards it exactly as before,
    so nothing changes for anybody who has not asked for otherwise; any
    non-zero floor now means what it says. Acceptance is unaffected, as in the
    stamped branch: a message that will not be forwarded still reaches the
    queue, where the mailbox's own signed policy decides it.

  - **`hbs2-peer`: an unsolicited status could queue unbounded downloads,
    including rows that survive a restart.** The download queue is keyed by the
    `(policy version, tree hash)` pair, so the bound is one entry per ROOT
    somebody names, and a root is a value the sender invents. The comment
    beside it claimed the queue was bounded by the number of mailboxes this
    peer holds; the check it describes bounds which KEY may be named, not how
    many trees may be named for it, and nothing rate-limits the requests.

    Each queued entry costs a poll every ten seconds with a `findMissedBlocks`
    behind it, an entry in the block downloader's working set that its sweeper
    removes only once the download completes -- so a root whose blocks never
    arrive is never swept -- and a row in the brains database that is deleted
    only when the block turns up. That last one outlives the process.

    The queue is now capped at `maxMailboxDownloads`, and the download is asked
    for after the slot is taken rather than before, so a refusal does not pay
    for the fetch it refuses. Dropping an announcement is cheap to be wrong
    about: a peer that really holds the tree re-announces it on the next
    ten-minute check.

    NOT fixed here: the brains rows and the downloader's working set are still
    only cleared by a completed download. What changed is that the mailbox path
    can no longer feed them without bound.

  - **`hbs2-peer`: a paid letter could be suppressed by re-sending it without
    its stamp.** The in-flight dedup at the mailbox queue's door was keyed on
    the hash of the `Message`, and a proof-of-work stamp is deliberately not
    part of the message it pays for -- it cannot be, since the sender signs the
    message and then grinds. So a stamped copy and a stripped one hash alike,
    and the queued tuple kept whichever arrived first.

    Anybody who had seen a paid letter on gossip could therefore re-send it
    stripped: the plain copy lands first in each ten-second drain window, the
    honest stamped one is deduped away as a repeat, and the drain refuses the
    queued copy for want of work. The sender paid 2^D hashes and got no
    rejection to look at. The door's own work check does not stop it -- it
    fails open on a cold cache, which every peer has after a restart and after
    any `mailbox set-policy`, and it is satisfied by ANY recipient, so a letter
    naming one charging mailbox and one free one is admitted unpaid.

    The dedup now records whether the queued copy pays, and a copy that pays is
    admitted once beside one that does not. That is one extra slot per message
    per batch; after it the map says paid and every further copy is free again,
    which is what the dedup exists for. The drain handles the pair in either
    order, since whichever is taken second finds the merge recorded and skips.

  - **`hbs2-peer`: the mailbox queue could wedge shut permanently.**
    `inMessageQueueInNum` is what the door gates on, and it was decremented one
    message at a time inside the processing loop, outside any bracket, after
    the whole batch had already been flushed out of the queue. Any exception in
    that loop -- a busy SQLite, a storage error -- lost the decrements for the
    rest of the batch. The worker restarts and the TVar does not: it is created
    once, so the inflation accumulated across every failure, and once it reached
    `inQueueDepth` the door answered `QueueFull` for everything, forever, with
    an empty queue and nothing left to decrement. One `warn` line was the whole
    symptom.

    The depth is now set to zero in the same transaction as the flush, which is
    not an optimisation but the value: the queue is empty at that point.
    Writing the truth rather than adjusting towards it also repairs a counter
    that has already drifted, which an increment can never do.

## Added

  - **`hbs2-hub publish`, and the line every canon-writing verb was missing.**
    `refs/hbs2/meta` is an ordinary git ref in the repository the operator is
    standing in. Every verb that writes canon -- accept, merge, close, label,
    assign, redact, maintainer add, compact -- wrote it locally, printed the
    commit, and exited zero, and nothing in the tool ever said the word push.
    So a maintainer could triage a queue, fold six letters, close two issues
    and record a merge, have every command succeed, and have the contributor's
    `hub updates` still show nothing a week later, with `hub verify` and
    `hub log` locally confirming, correctly, that everything was fine.

    `hub publish` is the other half of `hub sync` and carries the same two
    things: canon, and the staged proposals under `refs/hbs2/pulls/*` that
    `hub pr checkout` reads. Without the second, a reviewer's clone has nothing
    to check out.

    CANON IS NOT FORCED, mirroring the fetch side. A remote holding canon this
    clone does not contain is what a second maintainer's folds look like, and a
    forced push would take them out of the ref every reader follows while their
    events sat unreferenced in the object store. The push is refused, nothing
    is written, and the report names the remedy (`hub sync --repo <key>`, which
    folds both lineages and takes a rewrite when it is one). Exit code 45.

    THE PROPOSAL REFS ARE FORCED, and the asymmetry is deliberate: a revised
    pull request moves its head, which is a rewrite by construction, and
    `hub sync` already fetches them with a plus. They are derived from canon
    rather than authored, and PEP-21's A1 has one publisher, so there is no
    second author to race with.

    It signs nothing and checks no key: what gates publishing is who may push.
    A delegate can bless events into canon and cannot publish them, and this
    verb is where that stops being an abstraction.

    Every canon-writing verb now ends with a note on stderr saying the commit
    has not left the machine and naming the verb that sends it.

## Changed

  - **`hbs2-hub`: the repository key is spelled `--repo` everywhere.** It was
    one value under two names, one noun apart: `hub pr new --target K` beside
    `hub pr merge --repo K`. Both are thirty-two bytes of base58, so neither
    reader could tell it had been handed its sibling's spelling -- what it
    produced was a usage message about a flag the caller had just supplied.
    `--target` also collided with a second meaning: `target` is a field inside
    the signed author box (PEP-19), which is a different thing from the flag
    that used to fill it.

    `--target` STILL WORKS on every verb and is printed by none. It is a hidden
    alias for one release, so a script written against the old spelling keeps
    running; the comment on `repoFlags` is the reminder to take it out.

    Giving both spellings at once is refused rather than resolved, even when
    they agree: two spellings of one value on one line is a line somebody
    edited half way, and picking one is the guess `flagOnce` exists to refuse.

    Both readers are one function now (`flagRepo`, `flagRepoMaybe`), used by
    every verb that takes a repository, rather than a `flagOnce kvs "--repo"`
    per verb.

  - **`hbs2-hub`: `hub issue new`'s usage no longer denies reading the
    manifest it reads.** The text said `--recipient` could not be resolved from
    `--target` because "this verb does not read" the repository's manifest,
    while the verb has read it -- that is what makes `--recipient` optional and
    what saves a contributor from finding a 44-character sigil hash before they
    can file anything. The paragraph now says which flag is optional and what
    happens when both are given.

## Security

  - **`hbs2-hub`: three places where one stranger chose how much work this node
    did.** Each is the same shape: a bound existed for the size of one thing
    and none for how many of them there were, or for how much a tool asked
    about somebody's bytes was allowed to say back.

    **Attachments per letter.** `maxPartBytes` (64 MiB) and `maxPartBlocks`
    (32768 reads) are per part; the count is the sender's. `hub inbox accept`
    measured and opened every part a message named, so a letter naming a
    hundred was a hundred times whatever one part cost. The cheap direction is
    measurement rather than size: a tree that spends its whole walk budget is
    about a kilobyte to send, so a few thousand named parts bought a hundred
    million storage reads for a few megabytes of upload. There is a
    `maxMessageParts` now, checked before anything is read. `hub inbox show`
    walks the same list and stops at the same number, reporting what it did not
    walk rather than dropping it silently.

    **What git is allowed to say.** `gitRun` drained both pipes with no
    keep-bound. The comment beside it justifies that for stdout, which for
    `bundle create -` IS the bundle; it was applied to stderr as well, where
    the bytes are a message about a stranger's file. `git bundle verify` echoes
    one `error: <sha>` line per missing prerequisite, and a bundle header needs
    no pack behind it -- it is a text file somebody uploads as an attachment.
    Measured on this build: 4000 fabricated prerequisites, a 172 KB
    attachment, 196051 characters of stderr, all of it then decoded, escaped,
    split into one `Doc` per line and rendered to a `String`. At the
    attachment bound that is tens of megabytes. stderr is now kept to 64 KiB,
    which is what the reader half of the same module has always used.

    **S-expressions fetched from elsewhere.** A repository manifest and a
    mailbox policy were handed straight to `parseTop` with no byte bound, no
    clause bound and no escape bound, while canon files -- the same threat,
    from the same kind of source -- go through a reader that checks all three
    in one pass first. That reader's own note measures `parseTop` at 36
    seconds for 128 KB of bare atoms, because it is superlinear in the number
    of TOP-LEVEL items and in nothing else. The manifest is read for any repo
    key a user types; the policy is read per recipient on every send. Both go
    through the canon reader now, with limits of their own, since a manifest is
    not an event.

  - **`hbs2-hub`: a compaction could delete a value canon was keeping, on the
    word of an event the fold had refused.** `compactionOf` decided which
    `set` events were superseded by asking `resolve`, which checks two
    signatures and an id binding and NOTHING ELSE: not the target, not the
    maintainer set. So an event the fold refuses still counted as the winner
    for its attribute, and the owner's real `set` underneath it was dropped
    from canon while the event that displaced it was never admitted anywhere.
    After the rewrite the attribute reverts to whatever an earlier event said,
    or disappears.

    Reaching it needs no attacker. A delegate mints from a view built before
    their revocation, the publisher writes the result, and the fold refuses it
    as `UnauthorizedCanon` -- a case the fold documents as honest and
    ordinary. The hostile version is worse only in aim: anyone who can put one
    file into `refs/hbs2/meta` chooses which admitted events the owner's next
    compaction deletes, by stamping a self-signed `set` at a high `seq`.

    The rule now takes the `FoldResult` over the same canon. An event the fold
    refused may neither be dropped nor supersede one it admitted -- the second
    half is the one that lost data, and the module header had claimed the
    first while implementing neither.

    Two more retentions, both from PEP-19 "Compaction" and both invisible to a
    reader. Any event carrying an `origin` or an `honours` is kept: one letter
    folds to at most one event, and the check enforcing it asks canon for the
    letter's hash, so compacting an honoured `close` away let the same letter,
    still sitting in a mailbox, be honoured a second time. And `equivalentTo`
    -- the check a clone makes before taking a rewrite over a fork -- now
    compares the origin and honoured sets too, so a lineage that quietly lost
    them is a fork rather than a compaction.

    `hub compact` also stopped matching kept events with `elem` over a list.
    The comment on `sortCanon` measured that same shape at 20 s for 40000
    files against a bound of 200000.

  - **`hbs2-peer`: a stranger could suppress a chosen letter, permanently and
    silently.** "This entry is already merged into this mailbox" was the
    PRESENCE OF A BLOCK at `hashObject (serialise (MergedEntry mailbox
    entry))`. Every input to that hash is public: the mailbox key is
    announced, and the entry hash is derived from a message anybody sees on
    gossip. The block store it lived in is content addressed and fetches on
    demand, which makes it writable by whoever wants something in it.

    So: compute the marker hash for a message you want gone, send the host a
    `MailboxStatus` for that mailbox naming the marker hash as the tree root,
    answer the resulting request with the seventy-five bytes that hash to it,
    and `putBlock` keeps them because the hash matches. From that moment the
    message is refused at the door on every delivery, at `debug` level, with
    no rejection reaching the sender. Replication does not repair it: the
    walk over an honest co-host's tree consults the same marker and skips the
    same entry. One packet and one served block, per message, chosen by hash.

    The mailbox had to be one this peer hosts, which is public, and the
    attacker had to pass `policyAcceptPeer`, which by default and under the
    documented open-inbox recipe allows everybody.

    Merge state now lives in the mailbox's own SQLite, in a `merged` table
    keyed by mailbox and entry. The general rule, since it outlives this one
    marker: the presence of a block in a GLOBAL content-addressed store cannot
    carry a LOCAL decision, because any path that fetches blocks on demand is
    a path by which a stranger chooses what that store holds. State meaning
    "we did this" belongs where only we write.

    Upgrading costs work and not correctness. Existing markers are blocks and
    this reads rows, so every entry already in a mailbox is processed once
    more: one signature check apiece and one tree rebuild per mailbox, after
    which the rows exist and the early exit is back. It cannot lose an entry,
    because the merge unions the new set with what the tree already holds. The
    orphaned marker blocks stay in the store as garbage, which is the
    pre-existing `$class: leak` and not made worse.

    `mergedMarker` and `MergedEntry` are gone from
    `HBS2.Peer.Proto.Mailbox.Entry`. Nothing outside the peer used them, and
    the two derivations that remain there (`existsEntryHash`,
    `deletedEntryHash`) are unchanged and still golden-tested: those are entry
    names in a replicated tree, which is what a content-addressed hash is
    for. `RoutedEntry`, the gossip dedup marker, had the same shape; the
    entry below is that one.

  - **`hbs2-peer`: the same trick stopped a chosen letter from being
    forwarded.** "This peer has already relayed this" was the presence of a
    block at `hashObject (serialise (RoutedEntry h))`, where `h` is the hash of
    a message anybody reads off the wire. Same store, same on-demand fetch,
    same plantable answer: put the marker on the peers between a sender and a
    hub and the letter is never forwarded, with nothing said at any log level.
    Weaker than the merge marker, which suppressed storage outright, and it
    reaches the same end when the target has no other route.

    Relay memory now lives in the process, in
    `HBS2.Peer.Proto.Mailbox.Relayed`, behind a new `mailboxRelayOnce` on the
    protocol adapter. It is a test-and-set in one transaction rather than a
    lookup and a later write, which also closes the window in which two
    handlers meeting one message both forwarded it.

    It is BOUNDED BY COUNT, in two generations of 65536: entries go into the
    young one, both are consulted, and a full young one ages into the old.
    That is the second thing wrong with the block, which was never collected
    and grew by one per distinct message forever (the `$class: leak` that
    stood beside it). What the bound trades is that suppression is no longer
    permanent: after a generation of distinct messages an old one presented
    again is forwarded again. That buys a flooder nothing, since re-sending a
    message they already hold costs exactly what sending a new one costs.

    `RoutedEntry` is gone from `HBS2.Peer.Proto.Mailbox.Entry`. The wire
    format is untouched: it was never on the wire, only in the store.

  - **`hbs2-hub`: a letter could have the hub mail a stranger, signed by the
    repository.** PEP-18 honours a reply channel only when it names the inner
    author's own mailbox and the envelope signer is that same key. A channel is
    a key AND A SIGIL HASH, and only the key was checked: `openLetterAs` is
    pure and reading a sigil needs a storage, so the sigil rode through
    untouched. `sendAck` then discarded the key entirely and handed the sigil
    to the message layer, where `resolveKeys` takes BOTH the recipient's sign
    key and its encryption key out of the sigil's own signed box.

    So the field that was checked decided nothing and the field that decided
    everything was not checked. A letter naming its own author and a victim's
    published sigil passed, and every `hub inbox accept` of one sealed an
    `AckRecord` to the victim's encryption key and delivered it to the victim's
    mailbox, signed by the repository's canon key. One accepted letter, one
    unsolicited message, from an address the victim cannot easily ban because
    it is the repo's. That is the reflector PEP-18 describes and the comments
    above both functions claimed was closed.

    The sigil is now loaded and refused unless it names the key that asked, as
    a new `AckWrongSigil`. Refusing is not a failure of the accept: by the time
    an ack is attempted the event is in canon, so this is a notification that
    did not happen. A sigil that cannot be READ is deliberately not a refusal
    -- the block may simply not have arrived, and the send reports that on its
    own; only a sigil that resolves to another key is turned away.

    Both halves of the decision are exported functions with tests, which is the
    other half of the fix: the rule had nowhere to be asked, and the test that
    existed passed one constant sigil into every case.

  - **`hbs2-core`: two versions of an encrypted file went out under one
    keystream.** An encrypted tree derives its content key from the group
    secret and a nonce that is a hash of the payload's first megabyte, and
    every block's nonce was that value with the block's position mixed in.
    Two payloads sharing that megabyte therefore agreed on the key AND on
    the whole nonce sequence, so past the point where they diverged each
    pair of blocks was a two-time pad: XOR the ciphertexts and the
    plaintexts' XOR falls out, with the Poly1305 one-time key repeating
    alongside. No key material was needed, only the ciphertext blocks any
    peer replicates.

    Appending to a file does exactly this, and the group secret it needs
    is long lived by design: `hbs2-sync` carries one secret for the life
    of a synced directory, including across changes to the reader list,
    and `hbs2-git3` keeps one in the repository manifest. Editing a file
    over a megabyte long past its first megabyte, then syncing it, was
    enough.

    Per-block nonces now come from the block, keyed by a value derived
    from the group secret and never published, and travel with the
    ciphertext. A block that changed cannot collide with what it replaced;
    a block that did not still encrypts to the same bytes, so an append
    still costs only the blocks it touched. The metadata block, which has
    a single nonce and no room for that derivation, takes a nonce hashed
    from the metadata rather than from the payload.

    This is the format's Method2, which has been in the reader since the
    format was introduced, so trees written by a fixed peer are read by
    every released version. Method1 trees keep decrypting. What no longer
    happens is writing one: new blocks are framed as a nonce and a box, so
    a tree written now does not deduplicate against a copy of itself
    written before, and re-encrypting an existing repository or directory
    stores it once more.

    The prefix rule itself is unchanged and still deliberate. What it
    leaks is unchanged too: identical payloads under one group key still
    encrypt identically, which is what makes the deduplication work and
    what tells a holder of the ciphertext that two payloads are the same.

  - **`hbs2-core`: any peer could take over the encrypted flow to any
    other peer, by naming its nonce first.** `ByPass` names a flow by a
    32-bit key built from two 16-bit nonces, one per end. Half of that
    value is the remote side's free choice and rides in the clear, in the
    `HEY` constructor OUTSIDE the signed box. The shared key was installed
    under that flow key on a first-writer-wins rule, so a flow key was
    being used as a name for a peer when it is only a name for a flow.

    Whoever announced a nonce colliding with an honest peer's got their
    own key installed where that peer's belonged. The honest peer's `HEY`
    then found the slot taken and installed nothing, while its nonce WAS
    recorded, so the victim went on encrypting to it, under the stranger's
    key. Traffic meant for the honest peer became readable by the stranger
    and unreadable by its recipient. `hbs2-peer` builds the layer with
    `byPassDef`, whose `byPassKeyAllowed` accepts every key, so a valid
    `HEY` for this cost an attacker one signature and a nonce it read off
    the wire. `cleanupByPassMessaging` kept the shadowed entry rather than
    repairing it. Sixteen bits is also small enough that two HONEST peers
    collide by birthday at a few hundred, breaking the flow between them
    with nobody attacking anything.

    Two things are separated now. What we encrypt to a peer is keyed by
    the peer, in `peerFlow`, and comes only from the `HEY` that peer
    signed: a stranger's announcement can no longer speak for somebody
    else's address. What may decrypt a given flow key is a bounded set of
    candidates rather than one key, and the Poly1305 tag picks among them,
    so peers sharing a flow key stay readable instead of taking turns.
    `cleanupByPassMessaging` rebuilds that set from the peers still known,
    which repairs a squatted flow key instead of carrying it forward.

    The WIRE FORMAT IS UNCHANGED. Both the `HEY` handshake and the packet
    header are byte for byte what they were, and no protocol version moves,
    so a fixed peer and an unfixed one still talk. What changed is only
    which key each side reaches for.

    Not fixed here, and worth knowing: a `HEY` is still trusted about the
    address it arrives from, so an attacker who spoofs a peer's source
    address still redirects our traffic for it. That was true before this
    change and remains true after. Closing it needs pinning an address to
    a signing key, which is a policy decision about how peers rotate
    credentials, not a repair to this layer.

  - **`hbs2-core`: a public key was whatever length its sender said it
    was.** The five saltine types this project puts on the wire are
    newtypes over a `ByteString` and had DERIVED `Serialise` instances.
    The generic encoding is that `ByteString` and the generic decoding
    takes one of any length; saltine's own decoder is the length check for
    the scheme, and it was being walked straight past. Measured before the
    change: a 35-byte public key decoded, and `Crypto.decode` on the same
    bytes answered `Nothing`.

    libsodium's verify takes no length argument and reads its thirty-two
    or sixty-four bytes out of whatever buffer it is handed. So appending
    one byte to your own public key gives a key that verifies your
    signatures exactly as the real one does and is a DIFFERENT VALUE,
    equal to nothing on anybody's list, and every deny list, maintainer
    list and mailbox-key comparison in this project is a lookup on that
    value. That is the hub envelope hole from the previous commit, at its
    root rather than at one call site. A key SHORTER than the scheme is
    the quieter direction: the read runs off the end of the buffer.

    The instances are hand-written now. The ENCODING IS UNCHANGED, byte
    for byte, and had to be: these bytes are inside signatures, inside
    block hashes and inside every ref key derived from a public key, so a
    different spelling would invalidate the network and everything stored
    in it. All five encode as a two-element array, the constructor tag and
    the raw bytes, exactly as before. The decoder now asks saltine, which
    is the only thing that knows the size each scheme wants.

    Consequences worth knowing. A key of the wrong length can no longer be
    constructed at all: saltine does not export the constructor either, so
    the `wellFormed`/`validHubKey` guards in `hbs2-hub` and the
    `(not a key: N bytes)` rendering are now unreachable. They stay as
    defence in depth, their comments no longer claim a hole that is
    closed, and the three tests that used to build such a key through the
    generic encoding now pin the refusal instead, since their fixtures
    cannot exist. `hbs2-peer`'s `testVersionedKeys` asserted the opposite
    of this fix -- it established that a key plus `"AAA"` decoded as an
    ordinary `RefLogRequest`, an experiment in versioning keys by
    appending bytes -- and now asserts the refusal, in both the bare and
    the wrapped form, plus the short-key direction.

  - **`hbs2-core`: a kilobyte of well-formed blocks could stop any peer
    that walked a root somebody else chose.** A merkle node lists child
    hashes and nothing stops two of them being equal, so the block graph
    is a DAG and the number of paths through it is not the number of
    blocks in it. `walkMerkle'` follows every edge, so a chain in which
    every node names its one child twice costs 2^depth for depth+1
    blocks. Measured with the blocks in a map, so the cost is the walk
    and nothing else:

    ```
     depth   blocks         visits    seconds
         4        5             31      0.000
        16       17         131071      0.127
        20       21        2097151      2.084
        22       23        8388607      8.958
    ```

    Twenty-three blocks is about a kilobyte, every hash in it resolves,
    there is no cycle, and a peer serves it the ordinary way. Depth 40 is
    forty-one blocks and does not finish. `mailboxAcceptStatus` hands an
    announced root to `findMissedBlocks`, which walks it and additionally
    accumulates the result with `S.toList_`, so this was reachable from
    any peer that had completed a handshake.

    A visited set is NOT the fix for `walkMerkle'`, and the sibling
    `deepScan` keeping one is not the precedent it looks like. Skipping a
    repeated node is only sound when the caller is asking about a set:
    `readFromMerkle` concatenates leaf payloads in traversal order, and
    identical runs of content are identical blocks and hash the same, so
    a file of ten megabytes of zeroes genuinely does have one leaf block
    named many times. Deduplicating there returns a shorter file, with no
    error. So `walkMerkle'` still follows every edge, and new
    `walkMerkleUnique'`/`walkMerkleUnique` enter each node once, for the
    callers whose question is which blocks a graph mentions:
    `findMissedBlocks`, the mailbox worker's walk of a downloaded status
    tree, and `hbs2-hub`'s ingress read of a mailbox. On the same inputs:

    ```
     depth   blocks         visits    seconds
        20       21             21      0.000
        22       23             23      0.000
        40       41             41      0.000
        64       65             65      0.000
    ```

    The visited set also bounds what those callers can accumulate, to the
    number of distinct blocks rather than the number of paths to them.

    `hbs2-core`'s test suite has a group for this, with the bomb at depth
    40 and a case pinning that the plain walk still repeats a leaf a tree
    names twice, since that is the property `readFromMerkle` stands on
    and the one a future dedup would quietly break.

    Not fixed here: `walkMerkle'` remains unbounded for callers that read
    content, which is correct but means a hostile tree still costs them
    paths rather than blocks, and `findMissedBlocks2`'s own recursion
    into nested merkle roots does not share a visited set across calls.
    Both are polynomial rather than exponential now.

  - **`hbs2-hub`: an argument beginning with `(` or `[` was executed.**
    `argvAtom` parsed such a word as a form and handed it to the
    evaluator, described in its own haddock as "the script escape hatch
    and the one place a user is asking to be lexed". It was neither. It
    applied to the VALUE OF A FLAG as much as to the verb position, and
    the evaluator evaluates a verb's arguments before the verb's pattern
    match runs, so

    ```
    hbs2-hub inbox '(run:proc:quiet "sh" "-c" "touch /tmp/proof")'
    ```

    created the file and then printed the inbox usage. The dictionary
    this tool shares with `hbs2-cli` also holds `rm`, `mv`, `cp`,
    `call:proc`, `setenv` and `cd`. PEP-22 specifies a renderer that
    "shells out to the CLI" with comment text a stranger wrote on the
    web, which is the version of this where nobody typed anything.

    It also contradicted the rule the same module states two paragraphs
    higher: `--title '(pwd)'` would have signed what the form evaluated
    to, in a tool whose claim is that it signs what was typed, and canon
    is append-only. The daily-use half was `--title '[bug] segfault'`
    exiting with `NameNotBound (Id "bug")`, an internal constructor
    naming a word nobody typed, for a title spelling a great many people
    use.

    The branch is gone and every argument is now the string it looks
    like. Nothing became unreachable: a form as the first word never got
    this far anyway (`verbOf` excludes a leading bracket, so it exits as
    an unknown verb), and a script still arrives on stdin or through
    `--run <file>`. The property test that covers this function now
    generates brackets, which it did not while they meant "lex me".

  - **`hbs2-hub`: a canon file could cost `hub verify` a minute of CPU
    while reporting one clause.** `scanText` counted the characters that
    open a form -- brackets, the quote family, a newline -- and a bare
    atom is none of them. `parseTop` accumulates top-level items with a
    left-nested append, so its cost is quadratic in how many items the
    top level has, and a `version` file of atoms separated by spaces and
    holding not one newline passed every bound. Measured end to end on
    this build, `hub verify` against such a repository:

    ```
    honest file:      45 ms
     32 KB:         2225 ms
     64 KB:         8899 ms
    128 KB:        36581 ms
    ```

    Four times the cost for twice the input, and 128 KB is well inside
    `maxEventBytes`; the walk clock is only checked between blobs, so two
    such files run to twice that before `CanonTooSlow` fires. Anybody
    with write access to a tree could stop every fold and every verify in
    every clone, which is the attack `maxClauses` exists to stop, in a
    spelling the bound did not count.

    What is counted is now an ITEM AT THE TOP LEVEL: brackets and the
    quote family and newlines as before, plus a bare atom or a bare
    string literal where the depth is zero. Depth is tracked because the
    cost is in the top level and nowhere else -- the same 65536 atoms
    moved inside one enclosing form parse in 317 ms -- so nesting is not
    penalised. Nothing a writer emits counts differently: `renderEvent`
    puts one bracketed clause on each line, so a real file has no bare
    atom at the top level and its count is unchanged. The same file now
    takes 45 ms and exits 7.

  - **`hbs2-hub`: an envelope signer was not checked for being a key, so
    a deny list was bypassed by appending one byte.** The ingress
    recovered the envelope signer with `unboxSignedBox0`, which does not
    check the recovered key's length; `unboxChecked`, in this package,
    does, and says why in a comment this path did not follow. The
    `Serialise` instances for a signing key and a signature are generic
    over a newtype, so they take any length and walk past the length
    check the crypto library does in its own decoder, and libsodium then
    reads its thirty-two bytes and ignores the rest.

    So a sender who appended junk to their own public key produced an
    envelope that verified under a key equal to nothing anybody has on a
    list, and `openLetterAs`'s one envelope-level check -- documented
    there as the only thing keeping a stranger out of the parked set --
    never fired. Each padding also changed the message hash, so one
    letter became N distinct entries, N stored blocks on every relaying
    peer and N queue lines, each printing as `(not a key: N bytes)`:
    nothing for a maintainer to copy into a block list either. A key that
    is not a key is now no identity at all, and the letter is refused as
    `BadEnvelopeSig`.

  - **`hbs2-peer`: a network delete created storage refs for mailboxes
    the peer does not host.** `mailboxAcceptDelete` never consulted the
    `mailbox` table, unlike `mailboxSendDelete` four screens below it,
    which refuses an unknown mailbox before it does anything. The key
    that signs a delete proof IS the mailbox key, so any peer that had
    finished a handshake could generate a throwaway keypair, sign a
    payload naming its own key, and send `DeleteMessages`: the receiver
    wrote the box block, wrote a `Deleted` entry block, enqueued a merge
    and then called `updateRef` on a `MailboxRefKey` it had never heard
    of. Repeat with a fresh keypair for as long as you like, and the
    message is gossiped onward first, so the whole network does it.

    The check `mailboxSendDelete` already makes is now made here too.
    Policy is still not consulted on this path: the default `BasicPolicy`
    is deny-all, and applying it here would stop an owner deleting in
    their own mailbox until a policy is set explicitly. The `TODO` above
    the function stays for that.

  - **`hbs2-peer`: a mailbox delete proof was not bound to the message it
    deleted (#15).** When merging a mailbox tree served by another peer,
    the worker checked that the entry's proof was a delete box signed by
    the mailbox key, and never checked that the box authorised deleting
    the message the entry named. The two hashes that have to agree were
    both discarded at the match site: the entry's target in a `_`
    pattern, the payload's predicate by never being read on that path.

    Every delete box an owner issues is public, gossiped and stored as a
    block, and PEP-21 fold-then-delete makes issuing them routine. So one
    of them worked as a proof for anything else in the same mailbox: any
    peer that had finished a handshake could staple it to an entry naming
    somebody else's message, serve a tree holding that entry, and have
    the message leave the mailbox with no missing block, no unsettled
    state and no diagnostic. For `hbs2-hub` that is a contributor's issue
    silently never reaching the maintainer.

    The decision is now a pure function, `admitDeleted` in
    `HBS2.Peer.Proto.Mailbox.Merge`, with a verdict per reason and a
    refusal in the log; it was buried inside a `runMaybeT` inside a
    stream inside a poll, which is much of why it went so long looking
    like a check it was not. Peers are now stricter, so the divergence
    from an unpatched peer is in the safe direction.

    **This does not clean up.** The merge carries entries already in a
    tree forward without re-checking them, so a peer that accepted a
    forgery keeps and re-serves it. The fix stops the forgery spreading
    and stops it being accepted again; a mailbox suspected of having been
    poisoned has to be recreated.

  - **`hbs2-peer`: the mailbox download path clobbered its own merge
    queue.** `HM.insert` inside the loop over a downloaded tree's entries
    replaced the whole pending set on every iteration, so a tree carrying
    several `Deleted` entries contributed at most one of them. The loss
    was permanent rather than racy: the surrounding code counts fetch
    failures only, a clobbered entry is not one, so the download was
    dropped from the queue as complete and never retried. In practice a
    synced mailbox kept messages its owner had deleted, which for a
    reader that folds and then deletes means already-processed letters
    reappearing in the queue. Both call sites go through `enqueueMerge`
    now.

  - **`hbs2-peer`: a mailbox status was judged by a clock, and any peer
    that finished a handshake could ask for one.** `CheckMailbox` has
    carried a nonce field since it was written; the responder threw it
    away and stamped the reply with ITS OWN clock, and the requester
    compared that against its own inside a ten-second window. That is not
    evidence the status answers anything: it says somebody's clock is
    near ours, so a status captured from an earlier exchange replays for
    as long as the window lasts. The responder now echoes the
    requester's nonce, which turns the same two fields into a
    challenge-response WITH NO CHANGE TO THE WIRE FORMAT. Nonces come
    from libsodium's `randombytes_buf` rather than from a clock or from
    `System.Random` -- splitmix's `mix64` is invertible, and two observed
    outputs give the whole sequence -- are bound to the mailbox they were
    issued about, live 60 seconds, and are not consumed by whoever
    answers first, since a gossiped request is meant to be answered by
    several peers. The TTL is short because a nonce we still accept is a
    window in which a genuine status captured off the wire replays, and
    the rule it replaces allowed ten seconds.

    Be clear about what the echo does and does not buy. A `CheckMailbox`
    is gossiped, so every connected peer is handed the nonce and any of
    them can echo it. The rule answers "is this a reply to something I
    asked recently", not "was this peer entitled to reply" -- that is
    what policy is for, below. Binding to the mailbox stops a nonce shown
    for one mailbox being spent on a status for another.

    The old clock window is kept as an accept-either fallback, and while
    it is there the gain is correctness rather than strength: anybody can
    put a plausible timestamp in a reply. Two things need it. A responder
    that has not upgraded does not echo at all; and `mailboxSetPolicy`
    gossips an unsolicited `MailboxStatus`, so that a new policy
    propagates without waiting for the next poll, which has no nonce at
    any version -- and that broadcast is not only the owner's, since
    `policyDownloadQ` reaches it on every peer that finishes downloading
    a policy. Dropping the fallback is a separate change with that
    broadcast as its precondition. The rule is now one function,
    `statusIsFresh`, because both halves together were asserted nowhere,
    which is the state the previous rule lived in for years with broken
    arithmetic.

    Answering is now gated on `policyAcceptPeer`, and so is ACCEPTING
    somebody else's status. The first is what the module's own `NOTE:
    CheckMailbox-auth` describes in its closing lines and needs no new
    message, since a peer is already authorized and known to the
    protocol. The second is what `NOTE: possible-poisoning-attack` asked
    for in as many words -- restrict in policy which peers may serve
    status, ignore the rest -- and no nonce could substitute for it, for
    the reason above. Before this, a mailbox key was enough for any
    handshaked peer to read that mailbox's merkle root, the sync
    fingerprint, and watch it move as letters arrived; and any handshaked
    peer could announce a root of its choosing and have it downloaded.
    What the gate does NOT hide is that we hold the mailbox at all:
    `mailboxCheckQ` broadcasts a `CheckMailbox` for every mailbox we host
    to every known peer every ten minutes, denied peers included.

    Applying the policy naively would have been a regression and not a
    fix: `mailboxGetPolicy` substitutes `defaultBasicPolicy`, which is
    deny/deny, so every mailbox that has never had a policy written would
    have stopped syncing, which is the trap the message-accept path
    already hit. A new `mailboxGetPolicyMay` answers "did the owner ever
    write one", and a mailbox with no policy answers everybody, as
    before. Presence is the row in the `policy` table, not whether its
    content can be read, so a policy whose block is lost or whose clauses
    this build cannot parse yields deny/deny: the owner said something we
    did not catch, which is not the owner saying nothing. A policy tree
    that is still DOWNLOADING is not that case and answers everybody --
    the row is written by `mailboxSetPolicy`, which `policyDownloadQ`
    reaches only once the tree is complete.

    **Upgrade note.** `parseBasicPolicy` defaults the peer action to
    deny, so a deployed policy that says nothing about peers -- `(sender
    allow all)` with no `(peer allow all)`, which is how the shorter
    recipes in PEP-21 are written -- denies every peer. Until now that
    cost such a mailbox only inbound messages, which `mailboxInQ` was
    already dropping under the same default; now it also stops the
    mailbox syncing. `mailboxSetPolicy` warns when it stores a policy
    that admits no peer at all. PEP-18's full recipe includes the clause.

    Mailbox metadata is still not encrypted, and the position of record
    is now written beside that `NOTE` rather than left as an open
    question: status is public in principle because the message bodies
    are encrypted, and an owner who wants otherwise writes a policy.

    Cost, since the gate reads a policy on a path a remote peer can
    drive: the check runs AFTER the cheap "do we host this" lookup, so a
    `CheckMailbox` naming a mailbox we do not hold costs what it always
    did, and `mailboxGetPolicyMay` reads the row once instead of twice,
    which also halves the policy queries on the message-ingest path.
    Caching the parsed policy per (mailbox, policy hash) would remove the
    rest and is marked in-tree, not done here.

    The nonce store lives in `HBS2.Peer.Proto.Mailbox.Nonce`; the store
    itself is a `TVar`, and both decisions over it are pure functions, so
    `hbs2-peer`'s `mailbox status` groups can ask them questions: binding
    to the mailbox, the TTL boundary, a clock jumping backwards, pruning,
    that the first answer does not consume the nonce, that a nonce is
    drawn rather than read off the clock, and that the accept-either rule
    is a disjunction. The decisions this took, and what was deliberately
    not done, are in `docs/drafts/checkmailbox-auth-context.md`.

  - `cabal test` in CI covers `hbs2-peer` as well as `hbs2-hub`, since
    the above is only a regression test if something runs it.

  - **`hbs2-peer`: one anonymous HTTP POST appended a forged transaction
    to any reflog the peer polls.** `post "/reflog"` handed the request
    body to the reflog worker and relayed it to every known peer before
    looking at a signature. The worker's subscriber enqueued the
    transaction twice: once through `reflogUpdate`, which verifies, and
    once unconditionally on the line below it. The consumer that drains
    that queue writes what it finds into the reflog and calls `updateRef`
    without verifying anything itself. `polled` is true for every reflog
    named in `poll`, that is, for every repository the peer hosts, so the
    second enqueue was the whole hole.

    Readers do not catch it either. `hbs2-git3`'s `readTxMay`
    deserialises a transaction without checking its signature, and
    `Import.hs` imports the complete checkpoint of highest rank, so a
    forged `SequentialRef` carrying a large rank became the repository
    state that clients pulled. The tree it pointed at could be staged
    beforehand through the other unauthenticated endpoint, `put "/"`,
    which stored 4 MiB per request into block storage and handed back the
    hash, an anonymous file host and an unbounded disk besides.

    Both write endpoints are gone and the HTTP API is read only. Posting
    a reflog transaction goes through `RpcRefLogPost` on the RPC socket,
    which is what `hbs2-cli`'s `hbs2:reflog:tx:post` already used, and
    storing a block goes the same way. The unconditional enqueue is gone
    too, so nothing reaches the reflog that `reflogUpdate` has not
    verified, whatever emitted the event. `hbs2-tests`' `create-raw-tx`,
    the only in-tree caller of the removed route, is deleted.

  - **`hbs2-peer`: the HTTP API listened on every interface by default.**
    A config with no `http-port` clause fell through to port 5005, and
    scotty's warp settings bind `0.0.0.0`, so a peer served its storage
    to the network without anyone having chosen that. Nothing behind the
    port asks who is calling: whoever reaches it reads any ref, tree or
    block the peer holds.

    It now binds `127.0.0.1` unless a new `http-listen` key says
    otherwise, in warp's host syntax (an address, a host name, or `*`,
    `*4`, `*6`). `http-port` still means the port and `http-port "off"`
    still switches the API off entirely; the peer logs the address it
    settled on at startup. **This changes behaviour on upgrade:** a peer
    that is reached over HTTP from another machine needs `http-listen
    "0.0.0.0"` written into its config. The NixOS module grew an
    `httpAddress` option with the same loopback default.

    A loopback binding is no longer announced in peer-meta, because a
    port a neighbour cannot open is nothing but an invitation to probe
    it.

## Features

  - **`hbs2-hub`: canon can be written, and one letter can be accepted
    into it.** Everything around this existed already and had nowhere to
    put its result: the fold decided what canon means, the bridge minted
    an event from a stranger's letter, the file format rendered one, and
    the reader folded a tree back. Nothing committed a tree. The vertical
    slice PEP-17 is for, a stranger files an issue and a maintainer takes
    it, was open at exactly that step.

    `planCanon` turns accepted events into the files PEP-19's layout puts
    them in: the `version` file, one immutable file per event under
    `threads/<id>/` or `repo/`, and a regenerated `index/number.sexp`.
    Every file is READ BACK with this build's own reader before it is
    accepted, because being able to write what cannot be read is the
    failure that costs most here, and the reader has three bounds a size
    check would not cover. A number the index cannot hold is reported
    rather than dropped in silence.

    `CanonSink` is the mirror of `CanonSource`, and `withGitSink` is its
    git half: `hash-object`, `read-tree`, `update-index --index-info`,
    `write-tree`, `commit-tree`, `update-ref`. Paths only ever travel on
    stdin, never argv, for the reason the reader already documents at
    length. The index it builds is its own, named by `GIT_INDEX_FILE`, so
    accepting an issue does not stage the operator's working tree. The
    ref move is a compare-and-swap against the canon that was folded, so
    two accepts racing leave one refusal instead of one silent loss, and
    the commit's dates come from the caller's stamp, which makes the
    commit id a function of canon rather than of when the writer ran.

    `hub inbox accept --mailbox K --repo R --message H` is the verb. It
    re-reads canon from git, requires the letter to actually be in that
    mailbox rather than merely present in local storage, asks the bridge,
    and commits. Every value is behind a flag, including the message,
    because a sign key and a hash are both thirty-two bytes of base58: a
    positional message parsed `--repo <hash> <key>` happily with the two
    swapped, and the first thing to notice was the bridge.

    Two things PEP-22 puts in this verb are deliberately absent, and the
    verb says so on success rather than leaving them to be discovered:
    the letter is not deleted (fold-then-delete is PEP-21 retention) and
    no acknowledgement is sent (the record type exists, the sending does
    not). Neither loses anything, since re-accepting the same letter is
    refused as `AlreadyInCanon`. No deny-list is applied either: PEP-21
    policy lives in the repo manifest and there is no manifest reader.

  - **`hbs2-hub`: the tracker can be read.** Accepting an issue into
    canon and then having no verb that shows it was a strange place to
    stop: `hub verify` prints an audit, which is counts and problems,
    and never a title or a status. `hub issue list`, `hub issue show`,
    `hub pr list`, `hub pr show` and `hub log` are the read side, and
    like `hub verify` they need no peer and no key, because canon is a
    git ref and the fold over it is all they print.

    A title, a label and a body arrive in a letter anybody may send, so
    every one of them is printed through `safeText`: a title carrying a
    newline would otherwise forge a second row of the listing, and one
    carrying an erase-line sequence would rewrite the row above it. Both
    are tested by trying it. Order is imposed rather than taken from a
    container: `frThreads` iterates in hash order, which is stable for a
    build and is not a promise, so threads sort by number and events by
    seq.

    `--status` and `--label` narrow a listing. They are exact matches
    and not the query DSL PEP-22 inherits from fixme-new, which compiles
    to SQL over a materialized cache this build does not have; the help
    says so rather than leaving the difference to be discovered. A label
    is one an owner applied: what an author asked for on open is shown
    separately and never counted, since applying one is an owner-signed
    event (PEP-19) and merging the two would let a stranger label their
    own issue.

  - **`hbs2-hub`: the git side of a pull request.** PEP-20's delta path
    as far as git is concerned: build a bundle of `base..source-ref` and
    report the tip it recorded, take a stranger's bundle in, answer the
    ancestor question, and stage a proposed tip under
    `refs/hbs2/pulls/<n>/head`. What is still missing above it is the
    letter that carries the bundle, the triage that runs this, and the
    merge event.

    A ref name and a sha arrive inside a contributor's signed box, which
    says who wrote them and nothing about what they are, and both reach
    a git command line. git reads a leading dash as an option, so a
    `source-ref` of `--upload-pack=...` would be a signed letter that
    runs a program. Every value is checked against a shape first, and
    the shape is narrower than what git would accept. `fsck` is turned
    on explicitly for the fetch, twice, because git leaves it off and
    these are a stranger's objects.

    The signed tip is an argument to the fetch rather than something the
    caller compares afterwards. It is the step the whole delta path
    rests on, git's own hashing binds the content to it, and a check the
    caller performs is a check the caller can omit; the one who omits it
    stages unsigned objects under a number. Staging is a
    compare-and-swap for the same reason canon's ref move is: a pull ref
    that moved means somebody else used that number.

    An empty range gets its own answer instead of git's prose. It is the
    ordinary mistake (nothing committed yet, or the base named as the
    branch itself), not a failure.

    One git runner now serves the canon writer and this, in
    `Repo/Git.hs` beside `gitIn`. It decides only the two answers that
    are not about the command, so each caller reads a non-zero exit in
    its own vocabulary; for one of them an exit of 1 is not a failure at
    all. The audit reader deliberately keeps its own, which has bounds
    this does not.

  - **`hbs2-peer`: a sender can name its own attachment in the payload it
    signs.** `createMessage` built a message's parts itself, from
    producers, and returned when they were already inside a signed box.
    A part is named by the hash of its encrypted tree, so a sender that
    wanted to REFERENCE one learned the name after the signature was
    over bytes that did not mention it. PEP-18's letters need exactly
    that (`body-part`, `bundle-part`), and it was not expressible.

    It is now two functions: `createAttachments` stores the parts and
    says what they are called, `createMessageWith` builds the message
    around parts that already exist. `createMessage` keeps its signature
    and is the two of them in a row, so every existing caller is
    unaffected and the wire format is untouched.

    The separation the split had to preserve is that the parts carry a
    group key of their own, never the message's. PEP-18 turns on it:
    folding an attachment into public canon publishes the key to it, and
    the message key also opens the back-channel that a contributor's
    personal mailbox address rides in.

  - **`hbs2-hub`: `hub pr new` proposes a change as a bundle.** The
    contributor's end of PEP-20's delta path: build a git bundle of
    `base..--from` in this repository, ship it as an encrypted
    attachment, and sign coordinates that name it. The bundle's size is
    the delta, so proposing a change to a large repository does not
    transfer the repository.

    The order is the whole difficulty, and it is why `sendLetter` split
    the way `createMessage` did: a part is named by the hash of its
    encrypted tree and PEP-18 puts that hash inside the signed box, so
    the bundle exists before the box that names it. `attachToLetter`
    stores the parts, `sendLetterWith` seals a letter around parts that
    already exist, and `sendLetter` is the no-attachment case.

    `--base` defaults to the merge-base of `--onto` and `--from`, which
    is what a contributor almost always means; naming it is for the case
    where git's answer is wrong, such as a rebase onto something the
    maintainer does not have. The tip comes from the bundle rather than
    from a second lookup: it is the tip of the ref the bundle recorded,
    which is what the maintainer's fetch will produce, and asking git
    again would answer a different question.

    Not done yet, and needed before a maintainer can act on one of
    these: the read side has to fetch and decrypt the attachment, and
    triage has to run the verification and stage the tip.

  - **`hbs2-hub`: an accepted letter's attachment reaches public canon.**
    `hub inbox accept` handed the bridge `noMessageParts`, so a folded
    event referencing an attachment carried no part-secret and a public
    clone could see the reference and not the bytes (PEP-18
    "Attachments in public canon"). It now measures each part the
    message carries, opens the ones it can, and hands over real
    evidence, so the owner publishes the secret the fold is supposed to.

    The size comes from walking the part's tree rather than from a
    declaration. PEP-18's gate refuses an oversized attachment before
    anything is paid for it, and its note assumed a size in the tree's
    root block; nothing in this project writes one, so the alternative
    would have been a number the sender chose. The walk reads the tree's
    nodes and asks each leaf's size without fetching it, which also
    answers whether the part is fully downloaded, and that is the
    difference between a wait and a refusal. An oversized part is
    reported at its size and never opened, since decrypting one the gate
    is about to refuse is the spend the gate exists to prevent.

  - **`hbs2-hub`: accepting a pull request verifies it and stages the
    tip.** `hub inbox accept` now does the two steps PEP-20 puts either
    side of the fold. It takes the bundle in with fsck on, checks that
    what arrives is the commit the contributor signed for, and checks
    that the signed base really is an ancestor of it; then, after the
    canon commit, it stages the tip under `refs/hbs2/pulls/<n>/head`.

    The order is the one the dependencies force and the one the spec
    gives. Verification sits between the mint and the commit, because
    minting writes nothing and the commit is the only step that
    publishes, so a bundle that does not match leaves canon untouched
    and no seq spent. Staging follows the commit, because the pull ref
    needs the number and the number is not public until canon holds it.
    A staging failure is reported and does not undo the fold: the
    coordinates are in canon and the objects are already here, so it is
    a step to repeat rather than something lost.

    An issue and a fork-pointer pull request both have nothing here to
    verify, and they mean it for different reasons, so the decision is
    its own exported function rather than a branch inside the verb.

  - **`hbs2-hub`: `hub pr merge` records a merge, and refuses to record
    one that did not happen.** It records rather than performs:
    PEP-20 leaves the integration to whatever policy the repository uses
    (merge, rebase, squash, fast-forward), so the maintainer does that
    with git and then tells canon.

    What it checks is the sentence it is about to publish. A merge event
    says a proposal was integrated, and if the commit it names does not
    contain the tip the contributor signed for, that is false in every
    clone forever. git answers the question exactly, so it is asked
    before anything is written and nothing is written when the answer is
    no.

    The merge event sets the status to merged by itself, which is what
    PEP-19 specifies: no second `set` is written, because canon would
    otherwise claim a merged pull request was open until it arrived.

    That completes PEP-20's delta path: propose, verify, stage, merge.

  - **`hbs2-hub`: `hub maintainer add|remove|list`.** The verbs over
    PEP-21 delegation, which the fold and the bridge already decided:
    who may bless a canon event is a function of the log, and adding or
    withdrawing a key is an ordinary owner-signed event.

    Only the repository's own key may write one, so there is no `--as`.
    PEP-19 rule 5 requires both signatures on a delegation to be the
    owner's exactly, because a delegate that could delegate could grow
    the maintainer set; offering a flag that can only be wrong would
    mint an event the fold then drops with the seq already spent.

    The help says the thing a person adding a maintainer will assume
    otherwise: signing is not publishing. A delegate may sign events
    every clone will admit and cannot push `refs/hbs2/meta` to put them
    there, which needs the reflog key. How their events reach canon is a
    deployment question, and PEP-21 answers it three ways.

    `list` marks the owner, because the owner is in the set by
    definition rather than by any event, and a reader comparing the list
    against the log would otherwise find one entry with nothing behind
    it.

  - **`hbs2-hub`: `hub inbox reject`, and the rule it is the first to
    obey.** A letter can be dropped from the triage queue without being
    folded. It writes a tombstone, which is what "delete" means here:
    the queue stops showing the letter and no disk is freed, because
    nothing in this build walks a mailbox and deletes blocks.

    The mailbox key signs it. The peer takes the signer of the payload
    as the mailbox being deleted from, so a repo key or a delegate's
    canon key produces a delete against a mailbox nobody has.

    `--repo` is optional and its absence is a real difference rather
    than a default: without a canon to consult, the check that refuses a
    letter already folded cannot run, and the verb says which of the two
    happened instead of implying it looked.

    That check is PEP-21 retention's canon-awareness, now settled in the
    drafts: the protected set is derived from canon and not recorded
    anywhere, and a hub may not drop an attachment canon references.
    Neither half is urgent, because what they guard against does not
    exist yet -- `delBlock` is in the storage class and nothing walks a
    mailbox to call it -- but they are the obligation a purge inherits.

  - **`hbs2-hub`: `hub policy show`, `hub block` and `hub unblock`.**
    PEP-21's peer layer over the existing BasicPolicy: read a mailbox's
    accept policy, add a sender-deny clause, take one out. The help says
    which of the two layers this is, because the difference decides
    whether the operator is finished. A deny here matches the ENVELOPE
    key before anything is decrypted, so it bounds what the peer stores
    and relays; anyone holding a decrypted letter can re-send it under a
    fresh envelope, and keeping an author out of canon is the triage
    layer, which this build does not have.

    The file it writes is sorted. `getAsSyntax` renders a policy from
    two hash maps, so its clause order is hash order, and this text is
    hashed and versioned: without an imposed order, reading a policy and
    writing it back unchanged would produce different bytes. For the
    same reason a block that changes nothing writes nothing, since a
    version bump republishes the file to every peer holding the mailbox.

    Unblocking removes the clause rather than setting it to allow: an
    allow beside a default of allow says nothing and never goes away,
    and against a default of deny it would grant something the operator
    did not ask this verb for.

  - **`hbs2-hub`: `hub ban`, and the first predicate that is not `const
    True`.** PEP-21's triage layer, which denies an INNER author: the
    key inside the signed box, which a rewrapper cannot change, and
    therefore the only deny that is authoritative for canon. `hub inbox
    accept` now applies it, so a banned author's letter is refused
    before anything is minted.

    Local state and unsigned, which is PEP-21's own decision rather than
    an omission: publishing a ban into canon needs a new author-content
    op and an admission rule, and the spec defers that to `hub-meta 2`.
    So the list does not travel, two hubs serving one repository may
    disagree, and the verb says so. It lives outside the working tree
    for the same reason: a file under the repository looks like
    something that travels and is eventually committed by accident.

    A file it cannot read stops an accept rather than reading as empty.
    A deny-list that silently returns nothing when it is damaged is one
    an attacker shortens by writing something odd into it, and the
    accept it silently permits is the thing the list existed to stop.

  - **`hbs2-peer`, `hbs2-hub`: a mailbox can charge proof-of-work for a
    letter.** PEP-21's peer layer for the open inbox, which is the one
    tier where `(sender allow all)` means anybody may grow the tree.
    `(pow D)` in a mailbox's signed policy asks for D leading zero bits
    over `(mailbox key, message hash, nonce)`, and `hub` reads that
    policy before sending and solves what it finds. Absent or zero is
    every mailbox that exists today, unchanged and needing no stamp.

    The work binds to the hash of the message AS STORED and to one
    mailbox key. To the stored bytes, so it is paid for exactly what will
    occupy disk, and because that hash is fixed before the search starts
    the grind is hashing and nothing else -- the letter is signed once,
    before the stamp exists. To one mailbox, so a solution cannot be
    moved to another inbox; a letter to two mailboxes that both charge is
    solved and sent twice, with the same bytes each time.

    Checked in two places, because the flood and the disk are different
    surfaces. `(hbs2:mailbox:pow-min D)` in the peer's config is a floor
    checked before the message is forwarded, where the peer does not yet
    know which mailbox it is for; the mailbox's own `(pow D)` is checked
    where the message is stored, which is the only place its policy has
    been read. Replication between a mailbox's own hosts pays nothing: a
    stamp is not kept in the tree, so a co-host has none to offer, and
    what bounds that path is `(peer allow|deny)` as before.

    THE COST IS A WIRE BREAK, taken deliberately. A stamped letter
    travels as a new protocol constructor, relaying re-encodes what it
    decoded, and a peer that cannot parse it forwards nothing -- so a
    stamped letter only crosses upgraded peers, and an old one in the
    path is silence. Nothing else changes: an unstamped letter is the
    message it always was, and the constructor is appended last so every
    existing encoding is untouched. A sender gets no rejection signal
    either, and reading the policy first only works for a mailbox the
    local peer holds. Both are written up in PEP-21 rather than papered
    over.

    `MailboxAPIProto` is bumped again, and this time the REQUEST changed:
    `RpcMailboxSend` carries the stamp beside the message. An old client
    calling a new peer would send bytes it cannot decode, and a new
    client calling an old peer would have its letter sent without the
    work the mailbox asked for. The id makes both refuse to connect.

## Fixed

  - **`hbs2-peer`: a neighbour with no HTTP API was re-probed for its
    metadata forever.** `fillPeerMeta` abandoned its whole `runMaybeT`
    when a neighbour's meta carried no `http-port`, or when the peer's
    address carries a name no port can be swapped into (an `.onion`),
    leaving `_peerHttpApiAddress` at `Left`, the value that means "not
    asked yet". Every probe period it therefore re-requested meta it
    already held and subscribed another `PeerMetaEventKey` handler, which
    the 600 second sweeper dropped and the next round put back. Having no
    HTTP API is now recorded as the answer it is. This cost little while
    every peer announced a port; with loopback the default, it would have
    been every neighbour of every peer.

  - **`hbs2-peer`: the mailbox database was published before its tables
    existed.** `mailboxStateEvolve` wrote the `DBPipeEnv` into `mailboxDB`
    and created the schema afterwards, so between the two a `CheckMailbox`
    from any peer reached `getMailboxType_` and got `no such table:
    mailbox`. Nothing contains that: the proto handler runs through
    `deferred`, which is `addJob` onto `_envDeferred`, and the pipeline
    runner is started with `asyncLinked`, so the exception leaves the
    pipeline for the linked thread rather than staying inside the
    request. The window reopened on every restart of the worker, with the
    network fully live -- which is exactly when a request arrives. The
    schema is created first now. Containing OTHER SQLite errors on that
    path is a separate change and is not in this one.

  - **`hbs2-peer`: a policy could name a mailbox it was not signed for.**
    `mailboxSetPolicy` never compared `sppMailboxKey` against the signer,
    and the two halves of the code disagreed about which one identifies
    the policy: the row is written under the SIGNER and `policyDownloadQ`
    reads the current version under `sppMailboxKey`. A policy naming
    somebody else's mailbox was version-compared against theirs and stored
    under its own, so it could step over a legitimate update by version
    number. It could never be written under another key -- the write is
    always keyed by the signer -- but two answers to "whose policy is
    this" is one too many. The payload must now name its signer.

    `RpcMailboxSetPolicy` also took a mailbox key and ignored it entirely,
    so `mailbox set-policy KEY <version> <file>` set the policy of
    whatever the BOX named and reported success for `KEY`. The handler
    checks the two agree.

  - **`hbs2-peer`: deleting a mailbox left its policy behind.** There is
    no foreign key between the tables and `mailboxDelete` removed only the
    `mailbox` row, so recreating the same key resurrected the old policy
    -- including for a mailbox deleted precisely to reset an open
    `(sender allow all)`. Both rows go in one transaction now.

  - **`hbs2-peer`: a delete whose proof had not arrived was dropped, not
    retried.** `HBS2.Peer.Proto.Mailbox.Merge`'s own header says the fetch
    case is deliberately left to the worker "so that a proof still in
    flight is retried rather than refused". The worker refused it: the
    whole pending set leaves the merge queue in one transaction before any
    of it is examined, an entry with no proof block aborts its `runMaybeT`
    on the way to `admitDeleted`, and nothing put it back. Recovery
    happened only by accident, when a later status announcement re-walked
    a tree that still held the entry. It is re-enqueued now, which is what
    the header always claimed.

  - `hbs2-peer` CLI honesty, four small ones. `mailbox create --sigil` and
    `--sigil-file` are documented in `--help` and were live
    `error "not implemented"` -- a bare sentence under a GHC call stack,
    which reads as a crash; they refuse by name now and the help says
    NOT IMPLEMENTED YET. `mailbox` with no arguments parsed to `[]`, ran
    nothing, printed nothing and exited 0; it prints the help. `mailbox
    get` and `mailbox status` printed an EMPTY LINE for a mailbox the peer
    does not hold, since `pretty Nothing` is `mempty`, which reads as "it
    is there and empty"; `get` now tells "no such mailbox" from "held, no
    ref merged yet" by asking for the status, and both name the remedy.
    `mailbox create` for an existing mailbox with a DIFFERENT type
    answered success and changed nothing (`on conflict do nothing`); it is
    a refusal naming both types, while a repeat with the same type stays
    idempotent.

  - `mailbox create ... relay` warns that relay semantics are not
    implemented. `MailboxType` is stored and printed and no branch in the
    peer reads it, so a `relay` mailbox in this build is the same
    unbounded accumulator a `hub` one is, and `messageTTL` and
    `messageCreated` are signed, stored and read by nothing. Better said
    at create time than discovered from the disk.

  - **`hbs2-hub`: `hub inbox` said "not fetched yet" about a block it
    holds.** A message block that is present and does not decode as a
    message shared `NotFetched` with one that has not arrived: a wait, for
    something that will never change, retried forever with a zero exit.
    `readEntries`, fifteen lines above the site that got this wrong, draws
    exactly that distinction for tree blocks. It is `NotAMessage` now, and
    it names nobody, since the envelope is inside the bytes that did not
    decode.

  - **`hbs2-hub`: a keyman that cannot be consulted looked like a mailbox
    with nothing for you in it.** `ReadNoGroupKeyAccess` is what an index
    that was never updated, a key file the process cannot read and
    credentials that do not parse all come back as, and it becomes
    `NotForUs` -- printed once per letter, with a zero exit, reading as
    "none of this is mine" and indistinguishable from it. Distinguishing
    them needs a keyman API that says which it was; what this reader can
    do honestly, and now does, is name the other possibility when EVERY
    letter in the queue says it, and point at `hbs2-keyman list`.

  - **`hbs2-hub`: `hub inbox` had no bound on how much it would read.** A
    mailbox is public, so the number of letters in it is chosen by whoever
    writes to it, and this opened every one and held every `LetterView` --
    each carrying a body of up to `maxInlineBody` -- resident before
    printing a line: one RPC per tree node, one per entry, one per
    message, a keyman lookup and a secretbox open each. The canon reader
    next door carries three bounds and a refusal for each; this had none.
    `maxInboxLetters` is 1000, chosen as a guess about people rather than
    about mailboxes, and what is left out is counted in `irOmitted`, said
    in a note and exits 2 -- a list missing letters is wrong, not short.
    The order is applied before the cut, so which thousand you get does
    not reshuffle between runs.

  - **`hbs2-hub`: `hub help <unknown>` reported the miss on stdout and
    exited 0**, so `hub help "$v" || die` learned nothing and the
    diagnostic landed in the stream a caller was capturing as output. It
    goes through the same `refuse` as every other failure: stderr, exit 1.

  - **`hbs2-hub`: `hub issue new` stripped leading whitespace from the
    body.** `Text.strip` was used where the comment beside it described
    removing a trailing newline, so a body whose first line is an indented
    code block or a quoted diff was signed as different bytes from the
    file that was piped in -- and the body is inside the author box, and
    therefore inside the event-id, so it could not be corrected
    afterwards. Trailing only now.

  - `hub issue new`'s usage says that `--target` and `--recipient` are not
    cross-checked and why: the sigil decides which mailbox the letter
    lands in, `--target` says which repository it is about, and resolving
    one from the other needs that repository's manifest, which this verb
    deliberately does not read. A letter naming one repo and sent to
    another's hub is signed, delivered, and dropped at fold time as
    `WrongTarget` with no reply path. Saying so is not a fix; it is what
    can be said until the manifest reader exists.

  - **`hbs2-core`: `findMissedBlocks` started a fresh walk for every
    nested merkle root it met.** The walk over one tree is bounded by
    `walkMerkleUnique`, which enters a node once; the function's OWN
    recursion was not. A leaf payload may name a nested merkle root, and
    each one began again with an empty visited set, so N leaf entries all
    naming the same nested root cost N complete walks of it, multiplying
    at every level of nesting. Polynomial rather than exponential, but the
    exponent is the nesting depth and the root is one somebody else chose.
    The visited set is now shared across the whole recursion, so the cost
    is the number of distinct blocks reachable from the root, once -- and
    that also bounds what `findMissedBlocks` can accumulate, since it
    collects the stream with `S.toList_`. Deduplicating is sound for the
    same reason it is sound in `walkMerkleUnique`: the answer is a set.

  - **`hbs2-peer`: a replayed delete rebuilt the whole mailbox tree,
    every two seconds.** Every delete box an owner issues is public,
    gossiped and stored as a block, and PEP-21 fold-then-delete makes
    issuing them routine. `mailboxAcceptDelete` is called outside the
    `seen` gate, and must be -- it is the only path by which an unfinished
    merge completes -- so each re-receipt of the same box wrote a block,
    queued a merge, and had `mailboxMergeQ` re-read the entire mailbox log
    and rebuild the merkle from scratch on its next poll. Sending one
    captured delete box on a loop was therefore an O(N) rebuild on a loop.

    The accept path now derives the entry hash without touching storage
    and asks whether it is already merged: one lookup. The merge loop
    rebuilds only when the entry set actually changed, and writes its
    merged markers in both branches, after `updateRef` in both, since a
    marker that outlived a crash before the ref update would claim an
    entry the tree does not hold.

  - **`hbs2-peer`: the mailbox tree root depended on hash-table iteration
    order.** `toPTree` chunks a list in its own order, and the list came
    straight from `HS.toList`, so the root was a function of the
    traversal and not of the entry set. That root is the fingerprint peers
    compare to decide whether they are in sync, so two peers holding
    identical entries but producing different orders would download from
    each other indefinitely. HAMT iteration for a non-colliding key set is
    stable in practice, which is why this had not been seen, but it is a
    property of the container and not a promise. The list is sorted now,
    which is also what makes "the set did not change" imply "the root will
    not change" for the skip above. Existing mailboxes get one reshuffle.

  - **`hbs2-peer`: a mailbox with no policy lost every letter that
    arrived, permanently.** Three things had to be true at once and all
    three were. `mailboxGetPolicy` falls back to `defaultBasicPolicy`,
    which is `Deny Deny`, so a mailbox that has not had a policy set
    accepts nothing. The `RoutedEntry` dedup marker was written by the
    protocol handler immediately after gossip, ten seconds BEFORE
    `mailboxInQ` evaluated that policy. And a message the policy refused
    was `mzero`d out of existence: never stored, the queue already
    flushed. So every later gossip of the same message was suppressed as
    `seen`, and creating a mailbox and not setting a policy in the same
    breath was silent, permanent loss -- unrecoverable even after the
    policy was fixed.

    The marker gates GOSSIP now, which is what it is for, and nothing
    else. Accepting is a write to a bounded queue; the expensive part (a
    signature check per recipient, a policy read and parse with no cache)
    is in `mailboxInQ`, which gained a cheap early-out for a message
    already merged into that mailbox. So a re-gossiped message costs one
    lookup when it is already in, and gets another chance when it is not.
    The default stays deny: failing closed is right, and it was the other
    two that made it a data-loss bug.

  - **`hbs2-peer`: a full input queue was counted as a successful
    download.** `mailboxAcceptMessage` answered `()`, dropped on a full
    `TBQueue` and incremented a counter nothing read. The merge path
    called it from inside the walk over a downloaded tree, did not treat
    a drop as a failure, and so removed the tree from the download queue
    as complete, with its own `FIXME: what-if-message-queue-full?` on the
    line above. A burst larger than the 8000-slot queue was permanent loss
    with no diagnostic and no retry. It answers whether it took the
    message now, the drop is logged and published on the probe, and the
    download stays in the queue so the next poll walks the tree again.

  - **`hbs2-peer`: the merge path skipped the peer policy entirely.** The
    walk over a downloaded status tree called `mailboxAcceptMessage` with
    `mzero` for the peer, and `mailboxInQ` reads `Nothing` as "no peer to
    check" -- which is right for a message this node injected itself and
    wrong here. A peer denied by `(peer deny <key>)` only had to serve its
    messages inside a status tree instead of sending them, and every one
    of them was admitted with the peer check skipped. The announcing peer
    is carried on the download and passed through.

  - **`hbs2-peer`: a status for a mailbox the peer does not host queued a
    download that never expired.** `mailboxAcceptStatus` did no existence
    check, so any handshaked peer could announce a status for a key it
    invented and have us store its policy block and ask the network for a
    root it chose. Entries leave `inMailboxDownloadQ` only when the tree
    downloads completely and without a single failure, so a root whose
    blocks never arrive stayed forever and was re-polled every two
    seconds: N bogus statuses, N permanent entries, N permanent download
    requests. The mailbox must be one we host now, which bounds the queue
    by the number we hold, and both download queues expire an entry after
    an hour using the timestamps they were already recording and never
    reading. Giving up is not losing: `mailboxCheckQ` re-asks for the
    statuses of our own mailboxes on its own cycle.

  - **`hbs2-peer`: four mailbox RPC methods could not express failure, so
    the CLI reported success for everything.** `RpcMailboxCreate`,
    `RpcMailboxDelete` and `RpcMailboxSend` had `Output = ()` and their
    handlers `void`ed the result; `RpcMailboxList` collapsed errors with
    `fromRight mempty`. The service functions behind all four DO return
    `Either MailboxServiceError`, so the information existed and was
    thrown away at the boundary: `mailbox create` printed `()` and exited
    0 whether the row was written or SQLite threw, `mailbox list` printed
    an empty list for "no mailboxes" and for "database not ready" alike,
    and `mailbox send` reported success for a message whose signature does
    not verify. All four carry the `Either` now and the CLI runs it
    through the same `orThrowPassIO` the other verbs already used.

    `MailboxAPIProto` is bumped. The request side did not change, so a
    peer would still have acted on an old client's call and only the reply
    would have failed to decode, leaving the client reporting failure for
    something that happened; a new id makes the mismatch refuse to connect
    instead. Client and daemon ship in one release, so this only matters
    to a mixed install.

    Note what this does NOT establish for `hub issue new`:
    `mailboxSendMessage` still answers `Right ()` unconditionally, since
    it fires the protocol inside `deferred` and returns. The verb still
    says `queued` rather than `sent`, and the comment there now says which
    half changed.

  - **`hbs2-peer`: a misspelled policy clause was a silent no-op, and the
    file still read as valid.** `parseBasicPolicy` ended its clause loop
    in a `_ -> pure ()` catch-all and returned `Just` regardless, so
    `(peer alow all)` was dropped without a word while `loadPolicyContent`
    reported a policy it had successfully read, and the mailbox ran on
    whatever its default action said. The `orThrowUser "invalid policy"`
    guards in `hbs2-cli` were unreachable for the same reason. An
    unrecognised clause now fails the whole file, which is the safe
    direction: callers fall back to `defaultBasicPolicy`, which is
    deny/deny. Clauses are flattened first, so a policy written on one
    line is still read -- layout is not part of the format, and without
    that the new strictness would have refused a working file.

  - **`hbs2-peer`: `mailbox set-policy` truncated the version to
    `Word32`.** The CLI takes an `Integer` and the payload field is a
    `Word32`, with `fromIntegral` between them: `set-policy KEY -1 f`
    stored 4294967295. `mailboxSetPolicy` accepts only a strictly greater
    version and there is no lowering path, so a typo bricked policy
    updates for that mailbox permanently, silently, and reported success.
    The range is checked before anything is signed.

  - **`hbs2-hub`: `hub issue new` takes `--label`, which PEP-22 specifies
    and the code refused.** The flag was not in `knownFlags`, so the guard
    that makes a typo a refusal also refused this, and the requested
    labels were hard-coded to `[]` -- so `labels_requested` in the render
    contract could never be populated by the tool that populates it, and a
    person following the spec got a usage message that does not mention
    labels. It is repeatable, and it is a REQUEST: PEP-19 makes applying a
    label the owner's to sign, which is why the contract renders these
    separately from `labels`.

  - **`hbs2-hub`: a flag in the value position was taken as the value.**
    `hub issue new ... --title --draft` parsed cleanly and signed the
    string `--draft` as the title, into the author box and therefore into
    the event-id, which is the hash of that box: canon is append-only, so
    it cannot be corrected. A `--`-prefixed token is a missing value now.
    `--flag=value` is accepted as well, which it was nowhere before
    (`--title=t` paired the whole word with the NEXT one and printed a
    usage message that did not say why), and is the spelling for a value
    that legitimately begins with a dash.

  - **`hbs2-hub`: every `hub issue new` failure exited 1**, the code
    PEP-22 reserves for usage errors, so "you mistyped a flag" and "your
    letter is in nobody's mailbox" were one number and the caller had been
    told nothing that would make them keep watching for it. It is 18 for a
    peer that stopped answering (the same row `hub inbox` uses, widened
    rather than duplicated), 19 for no signing key here for the author, 20
    for a peer that answered and would not take the message. An oversized
    field stays at 1, which is what 1 is for. PEP-22's table is updated;
    the codes are a contract and may be added to, not reassigned.

  - **`hbs2-peer`: two peers whose clocks differed in one direction
    never synchronised a mailbox.** The freshness check on a peer's
    mailbox status was `abs (now - nonce) < 10`, which reads as a
    ten-second window either side. Both operands are `Word64`: on an
    unsigned type `abs` is the identity and the subtraction wraps, so a
    responder whose clock was a single second AHEAD produced a difference
    of about 2^64 and had its status discarded. The window was ten
    seconds in one direction only, and the discard goes through `exit ()`
    and logs nothing, so the failure had no symptom beyond a mailbox that
    did not sync.

    The difference is now `clockSkew`, a named function that subtracts
    the smaller from the larger and is symmetric by construction, so the
    call site cannot get it wrong by writing the arguments the other way
    round. A refused status now says so, with the skew and who sent it.

    `clockSkew` is pure and exported, and `hbs2-peer`'s test suite has a
    `MailboxStatus` group covering both directions, both sides of the
    boundary, symmetry and the ends of the range. That suite previously
    held one module, so the arithmetic that was wrong here was arithmetic
    nothing could ask a question of.

## Fixed in the release artifacts

Three things that are user-visible on an install rather than in a source
tree:

  - The linux musl tarball shipped `bin/git-hbs2` as a one-line shebang
    script naming a `/nix/store` path that does not exist on the machine
    unpacking it, so `git hbs2 ...` and `git-hbs2 --help` (which
    INSTALL.md offers as an install check) failed with "no such file or
    directory". Wrong in every artifact up to and including 0.25.5.0. It
    is a symlink to `hbs2-git3` now, as it already was in the docker
    image and the macOS bundle.
  - The docker image carried `/bin/hub` and `/bin/git-hbs2` as symlinks
    into a store path that is not in the image, so both were dangling:
    `docker exec ... hub` could never have worked. `git-hbs2` was wrong
    in every artifact; `/bin/hub` only from the commit that added the
    alias, since `hbs2-hub` did not exist in 0.25.5.0. Nothing ran the
    image before pushing it; the release job does now.
  - The docker image grew by about 123 MB, because it now carries
    `gitMinimal`: `hbs2-hub` reads a repository's issue tracker by
    running `git`, and a forge CLI in an image with no git can announce
    a verb and not run it.

## Packages

  - **`hbs2-hub` (new).** The core of the decentralized forge specified
    by PEP-17: issues and pull requests carried by the repository
    itself, with ingress from anyone through the Mailbox protocol.
      - **Layout.** Three stanzas, and the split is the design rather
        than a convenience. The library is pure: no storage, no
        network, no clock, which is what lets the whole admission
        surface be property-tested without a peer. A `hub-ingress`
        sublibrary holds everything that decrypts or talks to
        hbs2-peer. The `hbs2-hub` binary is a driver over both. The
        PEP-22 surface is spelled `hub`, which is reached through a
        symlink rather than by claiming a name in `PATH` that a widely
        installed GitHub tool already uses.
      - **`hbs2-hub inbox <mailbox-key>`.** Read-only: waits for the
        peer's copy of a mailbox to settle, opens every message this
        node holds a key for, and reports what each letter asks for and
        the event-id it would fold to. Nothing is folded, minted or
        deleted. A mailbox the peer does not hold locally is reported
        as such rather than as an empty inbox, because a peer never
        asks the network about one it does not have.

        An empty answer is not reported as an empty mailbox either.
        The peer writes a mailbox ref only when a merge lands, so "no
        ref" and "not downloaded yet" are one observation from here;
        the reader waits the rounds out and then says which of the two
        it cannot rule out. Exit 2 when part of the mailbox tree could
        not be read, since a hole makes the list wrong in both
        directions; 17 when the peer does not hold the mailbox, with
        the `hbs2-peer mailbox create` line to fix it; 18 when the peer
        is running and stopped answering. All three used to be 1, which
        PEP-22 gives to usage errors.
      - **`hbs2-hub issue new`.** Composes a Tier B letter, signs the
        author box, seals it to the recipient sigils and hands it to
        the peer. Prints the message hash and the event-id, which the
        sender can compute before any maintainer has looked.

        Takes `--target`, `--sender`, `--recipient`, `--author` and
        `--title` in any order, which is the spelling PEP-22 specifies;
        the five positional arguments still work. The named form is
        worth preferring because the positional one is four base58
        blobs in a row and two pairs of them are interchangeable: the
        repo key and the author key are both sign keys, the two sigils
        are both hashes, and swapping either pair produced a correctly
        signed, delivered letter claiming the wrong author. An unknown
        or repeated flag is refused rather than ignored.

        The answer is `(queued ...)` and not `(sent ...)`. The peer's
        send RPC answers `()` and its handler discards the protocol's
        own result, so what a zero exit establishes is that the peer
        took the message -- not that a mailbox accepted it and not that
        it was delivered.
      - **Arguments are text, not script tokens.** Every word of `argv`
        used to be handed to the suckless script lexer and whatever came
        back was kept. That is right for a script and wrong for an
        argument the shell has already tokenised: `;` is the comment
        character, so `--title 'fix; see later'` bound the title `fix`
        and signed it, and a title with a space in it -- which is to say
        a title -- lexed as a function call and could not be passed at
        all. A word is now taken verbatim unless it starts with `(` or
        `[` (the script escape hatch) or spells a number or boolean that
        renders back to exactly the characters typed, which is what
        keeps an integer argument an integer while leaving `007` alone.
        A quoted word is no longer unquoted either: the lexer ran
        `readLitChar` over the inside, so `'"C:\temp"'` arrived as
        `C:<TAB>emp`. The quoting workaround existed only because a
        multi-word title had no other spelling, and it has one now.
      - **`hbs2-hub verify <repo-key>`.** Reads canon out of
        `refs/hbs2/meta`, re-runs the fold over it, and reports every
        event the rules did not admit, every anomaly in the ones they
        did, every file it could not read, every file with no readable
        version clause, every file whose name is not the one PEP-19
        gives it, and a missing `version` file, which PEP-19 requires.
        Exit 2 when it found any of those, so it works in a
        hook; 3 to 16 when the audit could not run, one code per reason
        and each with advice on stderr; 141 for a closed pipe. The codes
        are a table in PEP-22 and are a contract: they may be added to,
        not reassigned.

        Read-only, peerless and OFFLINE. It reaches nothing but the
        local repository and does not probe for a peer, so a wedged
        daemon cannot hang a hook, and it opens no network connection:
        `ls-tree -l` would otherwise drive a lazy fetch per missing blob
        in a partial clone, through the audited repository's own remote
        urls, `core.sshCommand` and `credential.helper`. The reader
        sets `GIT_NO_LAZY_FETCH`, `GIT_NO_REPLACE_OBJECTS` (so canon
        cannot be read out of a tree `refs/replace` substituted) and
        three variables that disable password prompting, and it REMOVES
        each of them from the inherited environment before setting it,
        since `getenv` returns the first of two bindings.

        Bounded in every direction: the listing's bytes as they arrive,
        the file count before any entry is built, the event bytes and
        each event's size from the listing, how much of a tool's
        complaint is kept, how long a call may take, and how long its
        teardown may take (close the pipes, SIGTERM, SIGKILL, give up).
        The walk that follows the listing has bounds of its own, in
        seconds and in bytes handed back, because the listing of an
        expensive tree is small: 45000 paths sharing one subtree is five
        objects and 172 KB on disk, and every listing bound passes it.
        And the report itself is bounded, at a thousand lines a section
        with the count of what was not printed, since a path that is not
        where canon puts things never becomes a blob and so no read
        bound ever sees it.

        Blobs are read through one `cat-file --batch` for the whole
        walk rather than a process per path. That reply is not
        self-delimiting and its announced size does not delimit it:
        git writes the size out of the object's header and then the
        whole body, and a loose object can be self-consistent and lie,
        so a header of `blob 10` over two megabytes hashes to its own
        name and `ls-tree -l` prints 10. Reading the announced number
        would leave the rest in the pipe as the answer to the next
        object, which is one path's content and verdict reported under
        another's name. The reader checks the echoed object id, the
        type, the newline after the body, and that nothing is pending
        before it accepts a blob; the last of those is the one that
        holds, since the first two are bytes a lying body can contain.
        It also refuses a size larger than it will hold, or one too
        wide for the number it is read into.

        Every refusal says WHO SAID IT. A tool's complaint is an
        indented block with each line marked `|`; this program's own
        sentences are not, because the advice under a refusal is printed
        at the same indent and an unmarked block put a stranger's text
        exactly where a line telling the reader what to run goes. The
        first refusal a new user meets is `git` not being on PATH, and
        it used to print the runtime's exec error inside that block, as
        though git had said it.

        A hash-shaped field that is not the length of a hash is printed
        by its size rather than by its base58. A `HashRef` takes any
        length on the wire and only the author box's are checked, so a
        canon box can carry tens of kilobytes in `origin`; base58 is
        quadratic, and the report can print a thousand such lines.

        Everything a stranger chose that reaches a terminal is escaped
        injectively: `\u{...}` for a character, `\x{...}` for a raw
        byte, so two paths differing in one invalid byte print as two
        lines. What is escaped is chosen by Unicode category, not by a
        list: the control characters, the format and separator classes,
        surrogates, private use, non-ASCII spaces, and the
        default-ignorables that no category covers. A path is quoted,
        because the report prints it before a colon and a reason and a
        path may contain ": ". Text out is UTF-8 whatever the locale
        says, and so is argv; an argument that is not valid UTF-8 is
        refused before anything is signed, and stdin is strict, so a
        letter body in another encoding stops the program rather than
        being signed full of replacement characters.

        A path listed twice is named; if the two entries differ, neither
        is read, because choosing between them would let the order of
        entries in somebody else's tree decide which signed event the
        fold sees. Identical entries collapse and the file is read: it
        is one file listed twice, and refusing would be refusing over a
        question that has one answer. On `version` the same rule holds,
        and there a difference is a refusal of the whole audit rather
        than of one file.

        An event file whose name is not the one PEP-19 gives it is
        named, and folded anyway: no signature covers a path, so
        dropping it would show less than canon holds and disagree with
        every other clone. Two things are checked, the shape and the
        event-id. Three are not -- `seq`, the scope, and the thread
        directory -- because checking them means opening the canon box,
        which verifies a signature the fold is about to verify again.

        The repository key is an argument: the owner key is the root of
        the trust chain, so canon that named its own owner would be
        canon that could rename it.

        Canon is fetched explicitly, since git's default refspec covers
        only heads and tags:

        ```
        git fetch <remote> '+refs/hbs2/meta:refs/hbs2/meta'
        ```

        Needs `git` on PATH, as `hbs2-git3` and `git-remote-hbs23` do.
      - **Canon (PEP-19).** One shared signed content record for both
        trust tiers, wrapped in two independent boxes: an author box
        (who said it, kept verbatim from the contributor's letter) and a
        canon box (which maintainer blessed it, and where it sits in the
        order). An event-id is the hash of the author box, so a sender
        computes the canonical thread-id before delivery. Threads are
        the deterministic fold of the event log ordered by
        `(seq, event-id)`, with every rejected event reported and a
        reason attached.
      - **Letters (PEP-18).** The Tier B payload: a versioned envelope
        whose version is checked before the body is decoded, an inner
        signature over the plaintext that survives into canon, a
        transport-only reply channel honoured only from its own author's
        envelope, and the acknowledgement record. A correctly signed
        letter this build cannot decode is reported as a newer schema,
        not as a forgery.
      - **Triage bridge.** The only path from a letter to canon, written
        as a gate: it refuses anything the fold would drop, so a
        maintainer's decision is never spent on an event that cannot be
        admitted. Property-tested against the fold over random triage
        sequences.
      - **Manifest clauses.** `(mailbox <key> hub [<tier>])` and
        `(mailbox-sigil <mailbox-key> <hashref>)`, so a fresh clone can
        find where to submit and how to encrypt to it with no live
        lookup.

## Fixes

  - **Signed payloads carry their domain (`hbs2-hub`).** Ed25519 signs
    bytes, so a signature was bound to a record type only by the accident
    that no other record encoded the same way, and the owner key signs
    four of them across two packages. A sum constructor tag is an
    ordinary small CBOR integer, so anything of shape [small int, hash,
    int] signed by the owner for any purpose was byte for byte a signed
    redaction of any event. Every signed payload now carries a domain
    constant as its first field. This had to land before any canon
    exists, since an event-id hashes the whole box.

  - **Mailbox attachments get their own group secret.** `createMessage`
    encrypted the message parts with the same secret as `messageData`.
    That is fine in isolation, but it makes the forge unbuildable:
    folding an attachment into public canon means publishing the key to
    it (PEP-19), and if that key also opens `messageData` it publishes
    the sender's private reply address along with it, to every clone,
    forever. Retroactively, too: the ciphertext sits with every peer that
    ever hosted or relayed the mailbox, so a copy kept today reads the
    address out of a message folded years later. Parts now get a separate
    secret over the same recipients, and since each part tree embeds its
    own group key, a reader given a part hash and that secret needs
    nothing from the message it arrived in. No wire-format change, and no
    existing caller sends parts.

## Documentation

  - **PEP-17 forge specification.** An umbrella draft plus five
    sub-proposals under `docs/drafts/`: the letter format (PEP-18),
    canonical in-repo metadata (PEP-19), the pull-request model
    (PEP-20), triage and moderation (PEP-21), and the CLI and render
    contract (PEP-22).

# 0.25.5.0  2026-06-11

Adds Tor onion-service support (PEP-05): an hbs2 peer can run as a v3
hidden service, dial other peers' `.onion` addresses over SOCKS5, and
keep onion and clearnet address classes isolated so a clearnet peer
never learns onion addresses. Plus follow-up robustness and
observability fixes.

## Features

  - **Tor onion-service support (PEP-05).** A peer can operate
    onion-only or as a bridge:
      - Outbound dialing of `.onion` peers through a SOCKS5 proxy
        (`tcp.socks5 "127.0.0.1:9050"` plus
        `known-peer "tcp://<id>.onion:<port>"`); the hidden-service
        name is handed to the proxy unresolved, so no name leaks to a
        local resolver. DNS host names under `tcp://` now work too.
      - TCP-only operation with local-discovery and bootstrap gated off
        (`multicast off`, `bootstrap off`, `listen "off"`).
      - **Network-class PEX policy.** A peer declares how it is
        reachable in the handshake
        (`network-class "clearnet" | "onion" | "bridge"`, default
        `clearnet`); PEX forwards an address only to peers reachable on
        that class, so a clearnet peer never learns onion addresses
        while onion peers still discover each other. The `PeerData`
        handshake payload gains a backward-compatible `reachableVia`
        field, so old peers (defaulting to clearnet) do not partition.
      - **`peer-public-address`.** A peer advertises its own public
        address(es) to neighbours reachable on that class, so a peer
        reached over Tor (which otherwise sees only the Tor exit) learns
        its real `.onion` and can redial and gossip it.
      - **NixOS `services.hbs2-peer.enableTor`.** Wires `services.tor`,
        a v3 hidden service, the SOCKS proxy, and the onion-only config
        in one option.

## Fixes

  - **Onion peers stay known by their `.onion`, not `127.0.0.1`.** An
    inbound onion connection arrives from the local Tor exit as a
    loopback address. Two issues left a peer stuck under that useless
    address: the peer-dedup preferred a lower-RTT loopback over the
    routable `.onion`, and the TCP cookie dedup dropped the symmetric
    outbound `.onion` dial. The dedup now ranks by routability, and an
    inbound connection is re-keyed onto the `.onion` the peer
    advertises, so every peer in a meshed onion deployment knows its
    neighbours by `.onion`.
  - **Worker-thread supervision.** A failing worker thread is now
    restarted in place instead of throwing `GoAgainException` and
    respawning the whole peer, which previously turned a single bad
    bootstrap or PEX address into a crash loop.
  - **`hbs2-peer -r/--rpc` now selects the RPC socket.** The flag was
    parsed but ignored, so every CLI call fell back to the default
    `/tmp/hbs2-rpc.socket`; it now overrides the socket path, letting
    you target a specific peer (e.g. several peers on one host).
  - **`.onion` addresses redacted in default logs.** Default-level log
    lines render a peer's `.onion` as a short one-way fingerprint
    (`<onion:NNNN>`), so an operator's journald output does not leak
    where their peers are. Verbose `-d`/`-t` logs still print full
    addresses and should be treated as sensitive.

## Docs

  - New `docs/TOR_DEPLOYMENT.md` end-to-end recipe (NixOS, manual, and
    outbound-only); `docs/multi-machine.md` gains a Tor-outbound
    pointer; the PEP-05 draft is updated to reflect the implemented
    design; `PROTOCOL.md` documents the hand-rolled `PeerData`
    versioning.

# 0.25.4.0  2026-06-08

Feature and maintenance release. Adds an opt-in announce flag to the
CLI, de-vendors four bundled libraries onto Hackage, restores
annotated tag push, and ships new documentation.

## Features

  - **`block:put --announce`.** `hbs2-cli hbs2:peer:storage:block:put`
    now accepts an optional `--announce` flag that broadcasts a
    `BlockAnnounce` right after storing, so a "put on A, get on B" flow
    works without a separate `hbs2-peer announce`. Off by default so
    encrypted-refchan and group-key workflows that gate publication are
    not surprised; higher-level flows (git push, sync) announce
    internally and do not need it. Closes
    [#5](https://github.com/NCrashed/hbs2/issues/5).

## Fixes

  - **Annotated tag push to `hbs23://`.** `git push hbs2 <annotated-tag>`
    previously failed with a generic "failed to push some refs" because
    `r:push` handed the tag-object SHA straight to the commit-chain
    walker; the walker then tried to parse the tag body as a commit
    and threw `InvalidObjectFormat`. The remote helper now inspects
    the type of the pushed SHA via `git cat-file -t`. For a tag
    object it peels to the commit with `<sha>^{commit}` for the
    chain walk and passes the tag SHA as an extra object so the
    final segment includes the tag body itself. The export pipeline
    grows a third `[GitHash]` "extras" parameter that is serialised
    into the same source queue after the commit workers finish.
    `GitObjectType` gains a `Tag` constructor, the segment encoding
    learns an `'A'` short marker, and `gitPackTypeOf` maps `Tag` to
    `OBJ_TAG` so `git index-pack` accepts the resulting pack on the
    fetch side. Lightweight tag push is unchanged. Closes
    [#7](https://github.com/NCrashed/hbs2/issues/7).
  - **`r:list` emits HEAD as a symbolic ref** instead of a duplicate
    object line, so clients resolve the default branch correctly.
  - **NixOS module:** create and whitelist the `hbs2-mailbox` sibling
    directory, and grant the RPC group access to the peer socket.

## Dependencies

  - hbs2 no longer vendors `saltine`, `bytestring-mmap`, `db-pipe`, or
    `suckless-conf`. They are now consumed from Hackage
    (`saltine-0.2.2.0`, `bytestring-mmap-compat-0.2.3`,
    `db-pipe-0.1.0.1`, `suckless-conf-0.1.2.9`; the latter three are
    published and maintained under <https://github.com/NCrashed>).
    This is a step toward a plain `cabal install` story; there are no
    runtime behaviour changes.

## Docs

  - New walkthroughs: `docs/multi-machine.md` (replicating a
    repository to a second machine) and `docs/encrypted-repos.md`
    (group-key encrypted repositories, including key backup).
  - Design proposals published under `docs/drafts/`: PEP-05 (Tor
    transport), PEP-13 (post-quantum encryption), PEP-14 (encrypted
    keystore), PEP-15 (HD keys from a mnemonic), PEP-16 (barter
    storage).

## Compatibility

  - **Segment marker `'A'` (annotated tags) is new since 0.25.3.2.**
    Peers from 0.25.3.0..0.25.3.2 decoding a segment that contains an
    annotated tag will fall through the `FromStringMaybe (Short ...)`
    default branch and silently relabel the tag object as `Blob`.
    Concretely: on replication from a 0.25.4.0 peer to an older peer,
    annotated tags become unreachable on the older peer. The on-wire
    fix is to upgrade the receiving peer to 0.25.4.0. There is no
    mixed-cluster fallback.

# 0.25.3.2  2026-06-04

Patch release. Adds a Docker image as a third distribution path
alongside the static tarball.

## Release

  - **Docker image published on tag push.** The release workflow
    grows a second job (`docker-linux-x86_64`) that runs in parallel
    with the static tarball build. It produces a single OCI image
    bundling the full hbs2 binary surface (hbs2-peer, hbs2-cli,
    hbs2-keyman, hbs2-git3, git-remote-hbs23, git-hbs2, hbs2-sync,
    ncq3) on top of the musl-static binaries, and pushes it to
    `ghcr.io/${owner}/hbs2-peer:${TAG}` and `:latest`. Total size is
    ~40 MB compressed. This follows the postgres/redis convention of
    shipping the admin CLI alongside the daemon so that all common
    operator tasks work via `docker exec`. Image config: `HOME=/data`
    routes config (`~/.config/hbs2-peer`), keys
    (`~/.hbs2-keyman/keys/`), and storage (`~/.local/share/hbs2`)
    into a single `/data` volume; no Entrypoint, so `docker run image
    hbs2-cli ...` works as smoothly as the default `hbs2-peer run`.

## Build internals

  - New flake output `packages.x86_64-linux.docker` built via
    `dockerTools.buildImage`. A `stripPackageToBin` helper re-derives
    each shipped binary through a one-shot `cp -L` so the image's
    runtime closure carries only the actual binary content rather
    than the Haskell `lib/` outputs, which would otherwise drag in
    the GHC + GCC toolchain for every package (~3 GiB per package,
    432 MB compressed for the image without the trim).
  - `bf6-git-hbs2` is excluded from the image because it is a
    shebang script hardcoding a `/nix/store/...-suckless-conf`
    path, which would re-introduce the GHC toolchain into the
    closure. `git hbs2 ...` dispatching is provided instead by a
    symlink `/bin/git-hbs2 -> hbs2-git3`, mirroring the cabal-install
    fallback documented in INSTALL.md.

## Documentation

  - INSTALL.md: Docker becomes Option 2; gains a section with
    common `docker exec` operator commands (peer poke, poll add,
    metadata lookup, initial setup, etc.). Cabal, Nix flake, and
    Home Manager paths bumped by one.
  - CONTRIBUTING.md: release section describes the two parallel
    jobs and a local-build fallback for both the tarball and the
    image.

# 0.25.3.1  2026-06-04

Patch release. Resolves the two open code-level items on the 0.25.3.0
"Known issues" list (`hbs2-cli` stdin and `.#static` build), ports the
FAQ and cookbook material from voidlizard's original site into the
repository, and adds a release workflow that publishes a statically
linked Linux binary on every version tag.

## Fixes

  - **`hbs2-cli` reads piped stdin.** `recover` in
    `HBS2.CLI.Run.Internal` previously used a catch-and-retry pattern
    around the user's action. When the action read stdin via strict
    `Data.ByteString.getContents` (which drains and closes the handle
    on the first attempt) the retry tripped on a closed handle. The
    rewritten `recover` probes for the peer up front, populates the
    RPC env once, and runs the user's action exactly once. Closes
    [#4](https://github.com/NCrashed/hbs2/issues/4).

  - **`.#packages.x86_64-linux.static` builds end-to-end.** pkgsStatic
    ships GHC 9.4 with unix 2.7.3, which differs from the dynamic
    toolchain's GHC 9.6 / unix 2.8 in three `Posix.IO` APIs the
    storage layer uses: `openFd` argument list, `fdRead` return type,
    and `fdWrite` argument type. New module
    `HBS2.Storage.NCQ3.Internal.UnixCompat` carries CPP-shimmed
    `openFdCompat`, `fdReadBS`, and `fdWriteBS`; the storage call
    sites in `NCQ.hs`, `Fossil.hs`, and `Run.hs` route through it.
    A darwin-only fdWrite in `HBS2.Data.Log.Structured.NCQ` is
    inline-fixed similarly. Closes
    [#6](https://github.com/NCrashed/hbs2/issues/6).

## Release

  - **Static binary tarball published on tag push.** New workflow
    `.github/workflows/release.yml` builds
    `.#packages.x86_64-linux.static` with the GitHub Actions cache
    (`magic-nix-cache-action`), packages the result as
    `hbs2-${TAG}-x86_64-linux-musl.tar.gz` with a SHA256 sidecar,
    and uploads both to the corresponding GitHub Release. The
    workflow is also reachable via `workflow_dispatch` so the
    maintainer can re-run it for an existing tag. INSTALL.md
    promotes this to the primary install option for Linux x86_64
    users; CONTRIBUTING.md documents the release process and a
    local-build fallback.

## Documentation

  - **`docs/FAQ.md`** (new). What hbs2 is, crypto primitives,
    side-by-side comparisons with Syncthing, Radicle, and IPFS, the
    CBOR-not-JSON rationale, what the parens in `hbs2-cli`
    invocations mean. Material restructured from
    [`hbs2.krizanic.net`](https://hbs2.krizanic.net) (the restored
    mirror of voidlizard's original site).

  - **`docs/COOKBOOK.md`** (new). Working recipes for non-git tasks:
    file sharing between two peers, polling a remote `lwwref`,
    reaching content over the peer's HTTP gateway, deleting local
    blocks and trees, encrypting trees with a group key, storing
    small inline content via `block:put`. Every recipe was verified
    against the binaries shipped in this release.

  - **`hbs2-storage-ncq/README.md`** (new). On-disk layout (KPD
    records, N-way cuckoo index), runtime structure (sharded
    memtable, IO queues), defaults table, NCQv1 -> NCQ3 migration,
    reported performance figures (attributed to voidlizard's
    historical measurements, not re-measured for this release), and
    a code map.

  - **`ARCHITECTURE.md`**: short note on why `hbs2-git3` stores zstd
    segments rather than reusing git's native pack format.

  - **`README.md`**: links to the two new docs.

## Removed

  - "Known issues" listed under 0.25.3.0 for `hbs2-cli` stdin and
    `.#static` build are resolved by the fixes above.

# 0.25.3.0  2026-06-01

First release under new maintenance, continuing the work Dmitry Zuikov
(voidlizard) had in flight on the `dev-0.25.3` branch. Anton Gushcha
took over as maintainer in 2026 after Dmitry's passing in 2025; the
canonical repository moved to github.com/NCrashed/hbs2, with the
original archived at github.com/voidlizard/hbs2 (now
github.com/NCrashed/hbs2-legacy). See HISTORY.md for the full story.

The previous tag in the series is `0.24.1.1`. There is no `0.24.1.2`
release tag; that line in this file refers to a single in-flight
change that never shipped before the maintenance transition and is
included here for historical continuity.

## Project

  - **Scope narrowed.** Maintenance focuses on two flagship use cases:
    distributed git hosting and file synchronization. Components moved
    to archive-only status (kept in the legacy repository, not built
    in this release): `hbs2-qblf`, `hbs2-git-dashboard`, `fixme-new`,
    `hbs2-fixer`.

  - **Wire protocol stability commitment.** Every ProtocolId currently
    assigned in `hbs2-peer/lib/HBS2/Peer/Proto.hs` is frozen as of
    this release. Future wire-level features get new ProtocolIds, not
    payload changes to existing ones. See PROTOCOL.md for the full
    registry. Encrypted-overlay framing and `GroupKey 'Symm` hand-
    rolled Serialise remain unchanged from prior 0.24.x peers.

## Packages

  - `hbs2-storage-ncq` (new in this release line) becomes the primary
    on-disk storage backend. Provides `HBS2.Storage.NCQ3` (current,
    log-structured) and `HBS2.Storage.NCQ` (legacy, retained for
    migration). Migration from NCQv1 storage available via the `ncq3`
    executable and `scripts/ncq-migrate.ss`.

  - `hbs2-log-structured` (new) carries the cuckoo-hash and
    structured-data primitives NCQ3 builds on.

  - `hbs2-storage-simple` remains shipped; still maintained for tests
    and small deployments. New installs should use NCQ3.

  - `hbs2-git3` (new) replaces the legacy `hbs2-git` package as the
    git remote helper. URL scheme is `hbs23://` (was `hbs2://`);
    binaries: `hbs2-git3`, `git-remote-hbs23`. The bf6 wrapper
    `git-hbs2` exposes `git hbs2 ...` subcommands.

  - `hbs2-cli` (new in this release line) becomes the primary
    command-line surface. Replaces most subcommands of the legacy
    monolithic `hbs2` binary. See `docs/CLI_MIGRATION.md` for the
    full rename table (e.g., `hbs2 keyring-new` becomes `hbs2-cli
    hbs2:keyring:new`). The legacy `hbs2` package is not shipped in
    this release; the old binary is available upstream as
    `hbs2-obsolete` in the dev-0.25.3 source but excluded from the
    v1 fork.

  - `hbs2-keyman-direct-lib` exposed as a sub-package alongside
    `hbs2-keyman` for in-process clients.

## Build

  - **Reproducible toolchain.** Pinned GHC 9.6.6, Cabal 3.12.1.0,
    Hackage `index-state` 2026-05-31. `cabal.project.freeze` pins
    277 exact transitive dependency versions taken from the nix
    development shell. `allow-newer: all` removed.

  - **Explicit packages list** in `cabal.project` (no globs). Each
    project package and each vendored library in `miscellaneous/`
    is listed by path.

  - **Build target.** `nix develop --command cabal build all
    --enable-tests` succeeds from a clean cabal store.

## CI

  - GitHub Actions matrix builds on `ubuntu-latest` and
    `macos-latest`. Both compile every package and every test-suite
    (`cabal build all --enable-tests`).

  - Test execution is intentionally not enabled in CI yet; the
    legacy test suite has not been fully audited for the current API.

## Tests

  - `test:test-tcp` (`test/TestTCP.hs`) and `test:test-proto-service`
    (`test/PrototypeGenericService.hs`) are marked `buildable: False`
    because they target older API shapes that no longer exist.
    Replacements: `TestTCPNet` exercises the live TCP path; modern
    suites cover the rest (`test-udp`, `test-ncq`, `test-storage-
    service`, `test-walk-merkle-conditional`, `test-misc`, more).

## Documentation

  - `README.md`, `INSTALL.md`, `QUICKSTART.md`, `ARCHITECTURE.md`,
    `PROTOCOL.md`, `CONTRIBUTING.md`, `HISTORY.md`, `LICENSE` all
    written or rewritten for this release. `PROTOCOL.md` was
    re-verified against `dev-0.25.3` sources; `MailBoxProto`
    (ProtocolId 13001) added to the registry.

  - `docs/` directory carries voidlizard's technical notes
    unchanged (`CLI_MIGRATION.md`, `MIRROR_SETUP.md`,
    `VERIFY_MIRROR.md`, `LWWREF_VS_REFLOG.md`, `devlog.md`, plus
    `drafts/`, `notes/`, `papers/`, `proto/`, `refchan/`,
    `todo/`).

  - QUICKSTART updated to match `git hbs2 init --new` behavior
    actually shipped: repository signing key is auto-generated and
    stored at `~/.hbs2-keyman/keys/<pubkey>-lwwref.key`, git remote
    is auto-wired with a two-word slug, URL scheme is `hbs23://`.

## Infrastructure

  - **NixOS module.** `nixosModules.default` flake output for
    deploying `hbs2-peer` as a system service. Options cover all
    config keys, opens firewall by default, runs as a system user
    with capability dropping and `ProtectSystem=strict`.

  - **Home Manager module.** `homeManagerModules.default` flake
    output for user-level `hbs2-peer.service`.

  - **Bootstrap node.** `bootstrap.hbs2.app` is the hardcoded
    default in `hbs2-peer/app/Bootstrap.hs:60`. The DNS is under
    NCrashed's control and operational; the node is hosted on the
    aerospace deployment with TXT records announcing peer
    addresses.

  - **Self-hosted source mirror.** The project repository is
    mirrored via hbs2 itself at
    `hbs23://9gtFy65ap1Hk9Mc71pMjc32zFsKcNZLVPWbBAbnkE4dP`,
    served by the bootstrap node. Read-only for contributors;
    submissions still arrive via GitHub PRs.

## Known issues

  - `hbs2-cli` does not read piped stdin correctly. Workaround:
    pass payloads as quoted string literals.
  - `block:put` does not auto-broadcast. After a `put`, an
    explicit `hbs2-peer announce <hash>` is required for other
    peers to learn the block exists.
  - The flake's `packages.${system}.static` derivation evaluates
    but does not build cleanly because of a `pkgsStatic`
    cabal2nix interaction inherited from upstream. The dynamic
    `default` output is the supported install path.

# 0.24.1.2  2024-04-27
  - Bump scotty version (legacy, not shipped as a tagged release)
