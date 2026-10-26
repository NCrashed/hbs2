# hbs2-hub: issues and pull requests, in the repository

`hbs2-hub` is a decentralized forge. Issues, pull requests, comments,
labels and merge records live inside the git repository they are about,
as a signed append-only log; contributors reach a project through an
encrypted mailbox rather than through an account on a server.

There is no web interface yet. Everything below is the command line.

For the short version, the commands for each common job without the
reasoning, see [hub-quickstart.md](hub-quickstart.md).

## Status

Working and new. It ships for the first time in this release and has not
been through a season of real use. Parts of the design are not built
yet; read [What is missing](#what-is-missing) before you plan a
migration onto it.

The design is written up in `docs/drafts/pep-17` through `pep-22`.
Those are working notes and will be removed as their reasoning moves
into the code; this file is the part meant for people using the tool.

## The idea in one page

Two tiers.

**Canon** is the record. It is an orphan commit chain under
`refs/hbs2/meta` in the repository itself, one file per event: an issue
opened, a comment added, a label applied, a pull request merged. Every
event carries two signatures, the author's and the blessing of a key the
repository's owner has authorized. Reading the state of an issue means
folding that log, which every clone can do offline. Nothing is a
database and there is no server to ask.

**Letters** are how somebody who is not a maintainer gets an event
considered. A contributor composes a letter, seals it to the project's
ingress mailbox, and the mailbox delivers it over the hbs2 network. A
maintainer lists the queue, reads a letter, and folds it into canon or
does not. Until that happens the letter is a request and nothing more:
a stranger's signature never writes canon by itself.

So the two questions a forge has to answer have separate answers here.
*What is true* is canon, which is signed and in the repository. *Who may
say something* is the mailbox policy, which is the peer layer and bounds
disk rather than truth.

## What you need

- `hbs2-peer` running, with a repository already published through
  `hbs2-git3`. [`QUICKSTART.md`](../QUICKSTART.md) gets you that far.
- The **repository key**: the base58 key `hbs2-git3` gave the repository.
  Almost every verb takes it as `--repo`.
- For anything that sends or receives a letter, a **sigil**: a small
  published record naming an encryption key. `hbs2-cli
  hbs2:sigil:create:from-keyring <keyring-file>` makes one.

If you are not sure you have those, ask:

```
hbs2-hub whoami
```

It lists the signing keys this machine holds and names the tool that
makes each of the other two. It creates nothing.

Before your first letter, check that your author key and your sender
sigil are actually the same identity:

```
hbs2-hub whoami --author <your-sign-key> --sender <your-sigil-hash>
```

They are separate values and they have to agree. A sigil naming somebody
else's key produces a letter that is signed correctly, folds correctly,
and can never be answered: a hub will not seal a maintainer's reply to a
key you do not hold. The composing verbs refuse that pair outright, but
it is cheaper to find out before you have written the issue.

Read verbs need none of this beyond the repository key. They fold a git
ref and talk to no peer:

```
hbs2-hub issue list <repo-key>
hbs2-hub pr list <repo-key>
hbs2-hub issue show <repo-key> 7
hbs2-hub log <repo-key>
hbs2-hub verify <repo-key>
```

A plain clone does not bring canon with it. Fetch it first:

```
git fetch origin '+refs/hbs2/meta:refs/hbs2/meta'
```

or `hbs2-hub sync --repo <repo-key>`, which fetches the branches, canon
and the staged proposals in one go.

## Opening a project to contributors

You need a mailbox on your peer, a policy on it, and the repository
saying where it is.

**1. Create the mailbox.** Pick or generate a sign keypair; its public
key is the mailbox's address.

```
hbs2-peer mailbox create --key <mailbox-sign-key> hub
```

**2. Say who may write to it.** A mailbox with no policy is not an open
one: the peer refuses messages until you say otherwise. This is the only
verb that may create a policy, which is why it takes both defaults:

```
hbs2-hub policy default --mailbox <mailbox-key> --sender allow --peer allow
```

`--sender` is whose letters it takes; `--peer` is which peers it will
sync with at all. `(peer deny all)` stops the mailbox syncing entirely,
which is almost never what anyone wants.

**3. Charge for the privilege, if it is public.** An open inbox that
costs nothing to write to will be found. A proof-of-work floor makes
each message cost the sender measurable time:

```
hbs2-hub policy pow --mailbox <mailbox-key> --bits 20
hbs2-hub policy show --mailbox <mailbox-key>
```

**4. Declare it in the repository**, so a contributor who has the
repository does not have to be told anything else:

```
hbs2-git3 repo:mailbox:set   <repo-key> --key <mailbox-key>
hbs2-git3 repo:mailbox:sigil <repo-key> --key <mailbox-key> --sigil <hash>
```

The first writes `(mailbox <key> hub)` into the repository manifest,
which is what `hbs2-hub` reads whenever it is given `--repo` and no
mailbox. The second publishes a sigil for it: a mailbox is addressed by
a SIGN key, and sealing a letter to it needs the matching encryption
key, which lives in a sigil, and nothing resolves one from the other.

Both publish as they go: the manifest is signed and posted to your peer,
so there is no separate step. `hbs2-git3 repo:mailbox:list <repo-key>`
shows what a contributor will find, and `repo:mailbox:drop` takes it
back.

From here on a contributor needs the repository key and nothing else:
every verb that composes a letter falls back to what the manifest
declares, and so does `hbs2-hub inbox --repo <repo-key>` on your side.

## Filing an issue

```
hbs2-hub issue new \
  --repo   <repo-key> \
  --sender <your-sigil> \
  --author <your-sign-key> \
  --title  "the tests hang on aarch64" \
  --body -
```

There is no `--recipient` here because the repository declares one. Give
`--recipient <sigil-hash>` when it does not, or when you mean a mailbox
other than the one it publishes.

`--body -` reads the body from stdin. Nothing is read from stdin
otherwise, deliberately: git hands a hook `<old> <new> <ref-name>` there,
and a body goes inside the signature and the event-id, where canon
cannot repair it.

It prints the message hash, which is the letter's identity everywhere
else, and the event-id the thread will have once folded.

## Proposing a change

```
hbs2-hub pr new \
  --repo   <repo-key> \
  --sender <your-sigil> \
  --author <your-sign-key> \
  --title  "fix the aarch64 hang" \
  --onto   master \
  --from   my-branch
```

This builds a git bundle of the range in your own clone and ships it as
an encrypted attachment. What travels is the delta, so proposing a
change to a large repository does not transfer the repository.

The proposed tip and the fork point are signed, and the maintainer
checks the objects that arrive against them. To change a proposal after
review, `hbs2-hub pr revise --thread <thread-id> --onto ... --from ...`
sends a new range for the same thread.

## Reading the answer

An acknowledgement comes back to your own mailbox:

```
hbs2-hub updates --mailbox <your-mailbox-key> --repo <repo-key>
```

`--repo` is not optional: an ack is trusted only when a maintainer of
the repository it names signed it, and the maintainer set comes from
that repository's canon in the clone you are standing in.

An ack is a courtesy, not authority. Canon is the record; if the two
ever disagree, canon wins.

### Your mailbox is not private

The letter you sent names the mailbox to reply to, and that name is your
author key: the two have to be the same, or a hub could be made to send
maintainer-signed acks to somebody who asked for none of them.

Your author key is in canon, verbatim, in every clone, forever. That is
what authorship means and it is not something to fix. It follows that
anyone who reads the repository can name your mailbox and ask their own
peer to fetch it, and so watch how much traffic it carries and when.

What they cannot do is put anything in it. Sealing a message needs your
encryption key, which lives in your sigil, and canon does not carry the
sigil.

So: contributing tells the world that this key wrote this issue, and
where to reach that key. It does not tell them how. If you want a
repository not to be linkable to your other work, the thing to vary is
the author key, not the mailbox.

## Triage

```
hbs2-hub inbox --mailbox <mailbox-key> --repo <repo-key>
```

Read-only. It waits for your peer's copy of the mailbox to settle, opens
every message you hold a key for, and reports what each asks for.
Nothing is folded, minted or deleted. Expect it to take a few seconds.

A mailbox nothing has been sent to yet answers in one round (about 2.5
s) with an empty list: your peer has merged nothing into it and has
nothing for it in flight, and says so. While a copy is still downloading
from another peer, or a letter is between arrival and the mailbox, the
wait runs longer, and an empty answer at its end says on stderr that it
could not tell empty from still arriving. A peer older than this hub
cannot say whether it is busy, so with one every empty mailbox gets the
long wait and that note. `hbs2-peer mailbox pending <mailbox-key>` shows
what the peer has in flight.

One row per letter: the message, the envelope key, the author, what the
letter asks for, and the subject when it opens something. The
identifiers are shown by their first eight characters, which is enough
to tell two rows apart and not enough to pass to anything: every verb
that takes one takes the whole value, and `--long` prints them.

To watch the queue instead of asking it again every minute:

```
hbs2-hub inbox --repo <repo-key> --follow
```

It prints the queue as above and keeps running. Every five seconds it
asks your peer for the mailbox root, which is one local call, and once a
minute it asks the peer to fetch the mailbox from the network. Only when
the root moves does it read the queue again, and then it prints the rows
it has not printed yet. A row comes again when what it says changed: a
letter seen before its block arrived opens on a later read, or another
copy of it turns up. A letter rejected or accepted elsewhere leaves the
queue and prints nothing.

Each line is flushed as it is written, so a program reading the output
line by line sees a letter when it is printed. With `--json` each read
is one `queue` document on one line, holding only the new letters. It is
as read-only as `inbox`. Notes on stderr come when they start to apply,
not on every read, and there is no exit code `2` for a short page: the
next read may be whole. SIGINT and SIGTERM stop it with `0`. A peer that
stops answering (`18`) or no longer holds the mailbox (`17`) stops it
too, so a supervisor that restarts it sees why.

One letter in full, body included:

```
hbs2-hub inbox show --mailbox <mailbox-key> --message <message-hash>
```

A program reading letters in a loop adds `--json` and gets the body, the
attachments and a verdict as one object; see "For a program rather than
a person" below.

Folding it into canon is the step that turns a request into the record,
and it cannot be taken back: an event in canon is in every clone that
ever fetches it.

```
hbs2-hub inbox accept --mailbox <mailbox-key> --repo <repo-key> \
                      --message <message-hash>
```

The repository key signs it unless you name another with `--as` or a
default; a delegate should read
[Signing as a delegate](#signing-as-a-delegate) first.

A letter that ASKS for something -- close this, reopen that, set a
label -- cannot be folded as the sender's own event: those are
owner-authored, so canon would refuse it. Agreeing means authoring the
same thing yourself, which is one verb:

```
hbs2-hub inbox honour --repo <repo-key> --message <message-hash>
```

The event is yours, the time is now, and the letter is kept as the
origin, so a reader can see what was asked and who asked. It grants only
what carries no words: a closing note would become a comment in your
name, and an attribute is the requester's choice of both name and value.
Those come back as `NeedsReview`, and you write your own with
`hbs2-hub issue close` and friends.

For a pull request this also verifies the bundle before anything is
published, and stages the proposed tip at `refs/hbs2/pulls/<n>/head`
afterwards. Reviewing it is then local:

```
hbs2-hub pr checkout --repo <repo-key> --number 7 --branch review/7
```

Merging is your own git, with whatever policy the project uses. Canon
records what happened, and refuses to record a claim that is not true:

```
git merge --no-ff review/7 && git push origin master
hbs2-hub pr merge --repo <repo-key> --number 7 \
                  --commit <merge-sha> --into refs/heads/master
```

The owner verbs write canon directly, without a letter:

```
hbs2-hub issue open   --repo <repo-key> --title "the tests hang on aarch64"
hbs2-hub issue close  --repo <repo-key> --number 7 --note "fixed in 1.2"
hbs2-hub issue label  --repo <repo-key> --number 7 --label bug --label aarch64
hbs2-hub issue assign --repo <repo-key> --number 7 --to <key>
hbs2-hub issue reply  --repo <repo-key> --number 7 --body "can you try 1.2?"
hbs2-hub redact       --repo <repo-key> --event <event-id>
```

`issue reply` (and `pr reply`, the same verb) is how a maintainer
answers in a thread. `issue comment` is the contributor's path: it sends
a letter to the repository's mailbox, and a maintainer using it would
then have to find that letter in `inbox` and accept it. `reply` writes
the comment onto canon at once, signed by the repository key or by a
delegate from `hbs2-hub maintainer list`, chosen by `--as` or its default
(see [Signing as a delegate](#signing-as-a-delegate)). Any other key is
refused before anything is signed. The thread is named by `--number` or
by `--thread <thread-id>`, `--body -` reads stdin, and `--reply-to
<event-id>` says which event it answers. Readers show it like any other
comment. It has no origin, since no letter asked for it.

`redact` is display-level and PEP-19 says so: the bytes stay in every
clone, readers stop showing the body. It answers a mistake or abuse, not
a secret. A secret that reached canon has been published.

Every verb on this page that writes canon takes `--dry-run`. It signs
nothing and writes nothing: it prints the event your arguments would
mint and the file it would go into, after running every check the real
run makes. Worth the second command when the arguments are keys, since
a repo key, a maintainer key, an author key and a message hash are all
thirty-two bytes of base58, and canon is append-only.

Turning a letter down is the other half of triage:

```
hbs2-hub inbox reject --repo <repo-key> --mailbox <mailbox-key> \
                      --message <message-hash>
```

`--repo` is required, and the reason is the sentence the verb prints:
rejecting says the letter was not taken, so it asks canon first and
refuses when canon already holds it.

The sender is told, when their letter asked for an answer and named a
mailbox of their own. That acknowledgement cannot be checked against
anything -- an accept's can be, by folding canon and finding the event,
and this one is a claim about something canon does not hold. `--silent`
skips it, which is what rejecting is most often for: an ack confirms the
address is live and that somebody read it.

## Where you stand

```
hbs2-hub status --repo <repo-key> [--remote <name>]
```

Six facts, none of which needs a decision from you: the repository,
which key a canon write would sign with and whether this machine can
(see [Signing as a delegate](#signing-as-a-delegate)), what canon here holds, whether
that canon has reached each remote, which mailbox the repository
declares and how many letters are in it. It pushes nothing; the
publication check asks the remote and does not write to it.

Without `--remote` every remote of the clone is asked, one
`published <remote>:` line each, so canon sent with
`hbs2-hub publish --remote practice-forum` shows up on the
`practice-forum` line even when `origin` has none. With `--remote` only
that remote is asked. The check is an `ls-remote`, which reads a
remote's fetch URL: a remote that fetches from one place and also pushes
to another is reported for the place it fetches from.

The publication line has four answers, because they are four different
next steps: the remote would not answer, the remote holds none, the two
agree, and the remote holds canon this clone does not have. It will not say "ahead"
or "behind" -- telling those apart means folding both, which is what
`hbs2-hub sync` does.

And what you have sent:

```
hbs2-hub sent
```

One line per letter this node handed to its peer. That is what the log
records: not that a mailbox took them, and not that anybody read them.
`hbs2-hub updates` is what came back.

## Publishing

**Nothing above pushes.** Every verb that writes canon writes a git ref
in the repository you are standing in, and that is all. Send it with:

```
hbs2-hub publish
```

which pushes `refs/hbs2/meta` and `refs/hbs2/pulls/*`. Without the
second, `hbs2-hub pr checkout` in somebody else's clone has nothing to
check out.

Canon is never force-pushed. If the remote holds canon your clone does
not have, because a second maintainer folded something, nothing is
written and it says so; `hbs2-hub sync --repo <repo-key>` folds both and
takes the rewrite when it is one.

This is also the line between two capabilities. A delegate can bless
events into canon and cannot push them: signing is not publishing, and
who may push is git's question, not this tool's.

## More than one maintainer

```
hbs2-hub maintainer add  --repo <repo-key> --key <their-key>
hbs2-hub maintainer list --repo <repo-key>
```

Only the repository's own key may write a delegation, and there is no
`--as`. A delegate that could delegate could grow the maintainer set,
which is exactly the escalation the rule closes.

Revoking does not invalidate what a key blessed before: admission is
judged as of each event's own position in the log.

Neither verb writes an event that would change nothing. Delegating to a
key that is already a maintainer, revoking one that is not, and revoking
the owner are all refused: each mints a well-formed event the fold
admits, and the maintainer set afterwards is the one you started with,
so the usual reason to run one of these is a key typed wrong.

### Signing as a delegate

Every verb that writes canon (`inbox accept`, `inbox honour`,
`inbox reject` for its acknowledgement, `issue open|close|reopen|label|assign`,
the same for `pr`, `pr merge`, `redact`) takes `--as <key>` to name the
key that signs. Without it the key is chosen in this order:

1. `--as <key>` on the command line;
2. the environment variable `HBS2_HUB_AS`;
3. `git config hbs2.hub.as` in the repository you are standing in, as git
   resolves it (the clone's own config over the global one);
4. the repository key.

An empty value counts as unset. A value that is not a key is refused,
not skipped: skipping it would sign with the repository key, which is
the mistake the setting is there to prevent. An explicit `--as` does not
read the other two, so a broken default never stands in its way.

A delegate whose machine also holds the repository key wants step 3,
set once per clone:

```
hbs2-hub maintainer use --repo <repo-key> --maintainer-key <your-key>
```

It checks that the key is the owner or a maintainer in canon here and
that this machine can sign as it, then writes
`git config --local hbs2.hub.as <your-key>`. `--clear` removes it. When
a default picks the signer, the verb says so on stderr, and the report
of a write ends with a `signed by` line naming the key and where it came
from. `hbs2-hub status --repo <repo-key>` and
`hbs2-hub whoami --repo <repo-key>` show which key would sign before you
write anything, and `status` warns when that key is not a maintainer in
canon here.

An acknowledgement is sealed from the key that signed the event, so that
key's keyring needs an encryption key as well as a signing key. When it
has none, `inbox accept` folds the letter, says
`no acknowledgement: the signer <key> has no encryption key`, and names
the ways to sign as a key that has one. The event is in canon either
way.

## Keeping the noise out

Two deny-lists, at two layers, and they are not interchangeable.

```
hbs2-hub ban   --repo <repo-key>    --key <author-key>
hbs2-hub block --mailbox <mbox-key> --key <envelope-key>
```

`ban` is the inner author key, which is inside the signed box, so it
survives somebody re-wrapping the letter under a fresh envelope. It
bounds what this node folds into canon. It is local, unsigned, and does
not travel to other nodes.

`block` is the envelope key, at the peer layer. It bounds what this peer
stores and relays, and it is evadable by exactly the re-wrap `ban`
survives. A full stop is both: one bounds the disk, the other bounds
canon.

### What a re-wrap costs, and who pays

Re-signing a captured message under a fresh key needs no key and no
plaintext. The payload of a signed box is opaque bytes and the box
carries no sender field, so anyone who saw the ciphertext go past can
put their own signature on it. They cannot read it, cannot change a
word of it, and cannot make it say anything else.

What that does not reach is canon. An event is named by the hash of the
author's inner box, so a second copy of one letter is refused as already
folded, and a request carried out twice is caught the same way.

What it does reach is your attention. The queue groups copies of one
letter onto a single line, and `hbs2-hub inbox reject` drops the group,
so a flood costs one decision rather than one per copy.

The part that has no clean answer: every copy carries the same inner
author, so the only `ban` that stops them all names the person whose
letter was captured. They may have sent it once and done nothing wrong.
`block` names the envelope key, and a re-wrapper mints a fresh one per
copy, so it cannot keep up.

An open inbox therefore has no re-wrap resistance of its own. What
prices the flood at the door is proof-of-work:

```
hbs2-hub policy pow --mailbox <mbox-key> --bits <n>
```

It charges nothing until you set it. Work is per message and nothing
carries over, so it is a price and not a ban: a determined sender pays
and continues.

## Housekeeping

A `set` event that a later one overwrote is dead weight. Compaction
writes a new lineage without them:

```
hbs2-hub compact --repo <repo-key> --dry-run
hbs2-hub compact --repo <repo-key>
```

Run the dry run first. Canon is what every clone folds, and this is the
one verb that takes something out of it. What is lost is the timeline of
overwritten values, who set which label when. Every open, comment,
merge, redact and delegation is kept, along with the winning value of
each attribute.

Every clone sees a divergence afterwards, because the lineage changed.
`hbs2-hub sync --repo <repo-key>` folds both and takes the rewrite when
the two materialize identically, which a compaction does by
construction.

## Checking that canon is what it claims

```
hbs2-hub verify <repo-key>
```

Re-runs the admission rules over the whole log and reports every event
they did not admit and every anomaly in the ones they did. It needs no
peer and no key, and it exits non-zero when it finds something, which
makes it usable from a hook.

The repository key is an argument rather than read from the tree,
because canon that named its own owner would be canon that could rename
it.

## For a program rather than a person

```
hbs2-hub issue show <repo-key> 7 --json
hbs2-hub pr show    <repo-key> 7 --json
```

This is PEP-22's render contract: one thread as a versioned JSON object
that a web layer, a static export or a third-party renderer reads without
touching hbs2, crypto or the event log. It is derived purely from canon,
so two clones of one repository produce the same bytes.

Check the `contract` field before anything else. Fields are added without
a bump, so ignore what you do not know; the number moves when something
changes meaning or goes away.

There is no `verified` flag and there will not be one. Everything in the
contract has already passed the admission check: a bad signature or an
unauthorized key is dropped by the fold and never materialized, so being
in the document IS being verified. Auditing what was dropped is
`hbs2-hub verify`.

A redacted item carries no text. It keeps its `redacted: true` and its
identity, so a renderer can say that something was withdrawn, and the
title, the body and the attachment are `null` in the document rather
than present with a flag beside them.

For a pull request the diff is precomputed from `base..tip`, so a static
renderer needs no git. `diff.availability` is `available`, or
`reconstructable` when the objects are gone but the bundle attachment
that would rebuild them is still named by canon, or `unavailable`.

The read verbs a triage loop uses take `--json` too, and print one
document each under the same counter, told apart by `document`:

```
hbs2-hub inbox show --mailbox <mailbox-key> --message <hash> [--repo <key>] --json
hbs2-hub inbox      --mailbox <mailbox-key> [--repo <key>] --json
hbs2-hub log        <repo-key> [<number>] --json
hbs2-hub maintainer list --repo <repo-key> --json
hbs2-hub status     --repo <repo-key> --json
hbs2-hub sent       [--repo <repo-key>] --json
hbs2-hub updates    --mailbox <your-mailbox> --repo <repo-key> --json
```

| verb | `document` | from |
|---|---|---|
| `issue show`, `pr show` | `thread` | canon |
| `log` | `log` | canon |
| `maintainer list` | `maintainers` | canon |
| `inbox` | `queue` | a mailbox read |
| `inbox show` | `letter` | a mailbox read |
| `updates` | `updates` | a mailbox read |
| `status` | `status` | canon, the remotes, the peer |
| `sent` | `sent` | this node's own log |

Only the canon ones are the same bytes in every clone. The others are
one node's view at one moment, with its keys and its deny-list, and two
maintainers may get different answers without either being wrong. Notes
still go to stderr, so stdout is one document; the exit codes are the
same as without `--json`. Every key is always present, `null` when it
does not apply. Times are milliseconds since the epoch. The
mailbox-side documents spell an event and a thread `event` and
`thread`, as the queue does; the canon-side ones say `event_id` and
`thread_id`, as the thread document does.

`status --json` answers the signing question the way the text report
does: `signing` is whether this machine can sign as the key in `signer`,
and `signer` names that key (the one a canon write without `--as` would
use), its `source` (`env` for `HBS2_HUB_AS`, `git_config` for
`hbs2.hub.as`, or `repo_key`), and whether it is a `maintainer` in canon
here. A default that does not read leaves `key` null and says why in
`error`; every write would refuse it.

`inbox show --json` is the one to decide on. It carries the whole body,
untruncated and unescaped (JSON escapes control characters itself), the
attachments measured, and a verdict:

```json
{
  "contract": 1,
  "document": "letter",
  "message": "5MTAgKgs...",
  "envelope": "Gy3S18nJ...",
  "event": "32of39gf...",
  "repo": "7VM1rtxr...",
  "unreadable": null,
  "triage": {
    "verdict": "already_folded",
    "reason": { "code": "already_in_canon", "detail": "..." },
    "disposition": "folds",
    "deny_list_applied": true,
    "canon_consulted": true
  },
  "folded": true,
  "author": "Gy3S18nJ...",
  "op": "comment",
  "declared_at": 1793004741230,
  "subject": { "thread": "7QcNZqwS...", "number": 1, "in_canon": true,
               "target": null, "kind": null, "title": null },
  "body": "the whole body",
  "body_part": null,
  "labels_requested": [],
  "reply_to": null,
  "pr": null,
  "attr": null, "value": null, "merge_commit": null, "merged_into": null,
  "redacts": null, "key": null,
  "attachments": [],
  "attachments_unmeasured": 0
}
```

`triage.verdict` is one of:

- `would_fold`: nothing this read can see stops an accept. `reason` is
  null.
- `pending`: an attachment is not here yet (`part_not_fetched`). Ask
  again later.
- `already_folded`: canon holds this letter (`already_in_canon`).
- `refused`: an accept would refuse it. `reason.code` is
  `request_only` (use `inbox honour`), `owner_native`, `author_denied`,
  `wrong_repo`, `unknown_thread` or `too_many_parts`.
- `unreadable`: the letter did not open. The code says why:
  `not_fetched` is a wait, the rest (`not_for_us`, `bad_envelope_sig`,
  `bad_inner_sig`, `unsupported_version` and so on) are not.

The verdict is a preview. `inbox accept` runs the bridge's full check,
which needs the signing key and asks more than a read can: whether the
signer may bless, whether a revise comes from the thread's author,
whether the fold would drop the payload. `wrong_repo`, `unknown_thread`
and `already_folded` are only given when `--repo` was named, which is
also what `deny_list_applied` and `canon_consulted` say.

What is NOT there: an index document for list views and an activity
document for a timeline. PEP-22 leaves both schemas open, so pinning them
here would be inventing a contract rather than implementing one.

## For an AI agent: the MCP server

```
hbs2-hub mcp --repo <repo-key>
```

is a [Model Context Protocol](https://modelcontextprotocol.io) server:
JSON-RPC 2.0 on stdin and stdout, one message per line. An agent such as
Claude Code starts it itself and gets the forge as typed tools with JSON
schemas, so it does not have to compose command lines. You do not run it
by hand.

Registering it with Claude Code, from the clone the agent will work in:

```
claude mcp add hbs2-hub -- hbs2-hub mcp --repo <repo-key>
```

`--scope project` writes it into the project's `.mcp.json` instead, for
everyone who works in that repository, and `-e HBS2_HUB_REPO=<repo-key>`
passes a default through the environment rather than the command line.
A maintainer who triages adds the mailbox and, for a delegate, the canon
key; a contributor who comments adds the author key and sender sigil:

```
claude mcp add hbs2-hub -- hbs2-hub mcp --repo <repo-key> \
    --mailbox <mailbox-key> --as <canon-key>
claude mcp add hbs2-hub -- hbs2-hub mcp --repo <repo-key> \
    --author <your-sign-key> --sender <your-sigil>
claude mcp add hbs2-hub-ro -- hbs2-hub mcp --repo <repo-key> --read-only
```

Each of these is a default that a tool call may override, so a call names
what differs and not the repository it has named every time. Each also
reads from the environment (`HBS2_HUB_REPO`, `HBS2_HUB_MAILBOX`,
`HBS2_HUB_AUTHOR`, `HBS2_HUB_SENDER`, `HBS2_HUB_REMOTE`, `HBS2_HUB_DIR`);
a flag wins over the environment. A value that does not read as a key is
refused at startup rather than ignored.

The canon key is the exception. The server passes `--as` to a verb only
when the call names one or the server was started with `--as`; otherwise
the verb picks the signer as it does on a command line, from
`HBS2_HUB_AS`, then `git config hbs2.hub.as` in `--dir`, then the
repository key (see [Signing as a delegate](#signing-as-a-delegate)). A
delegate can run `hbs2-hub maintainer use` once in the clone instead of
passing `--as` to the server, and the agent and the command line then
sign with the same key. The `status` tool says which key that is
(`signer.key`, `signer.source`).

The verbs run in `--dir`, by default the directory the client started
the server in, which for Claude Code is the project. That has to be a
clone with canon in it: see the `sync` tool.

The tools:

| Tool | Verb | |
|---|---|---|
| `status` | `status --json` | where this clone stands, and which key would sign |
| `whoami` | `whoami` | which keys this machine has (text) |
| `issue_list`, `pr_list` | `issue list`, `pr list` | filters: `status`, `label` (text) |
| `issue_show`, `pr_show` | `issue show --json`, `pr show --json` | the render contract, as structured content |
| `log` | `log --json` | provenance per event, no text |
| `verify` | `verify` | a finding is a result, not a tool error |
| `inbox_list` | `inbox --json` | the queue document; `after`, `limit` |
| `inbox_show` | `inbox show --json` | one letter, body included, with a triage verdict |
| `inbox_wait` | `inbox --follow --json` | blocks until a letter arrives |
| `inbox_accept` | `inbox accept` | writes canon |
| `inbox_honour` | `inbox honour` | writes canon |
| `inbox_reject` | `inbox reject` | writes the mailbox |
| `issue_open`, `issue_close`, `issue_label` | `issue open`, `close`, `label` | write canon; `kind: pr` for a pull request |
| `issue_reply` | `issue reply` | writes a maintainer's comment onto canon; `kind: pr` for a pull request |
| `issue_comment` | `issue comment` | sends a letter |
| `publish` | `publish` | pushes canon to a remote |
| `sync` | `sync` | fetches into this clone |

Each tool runs one verb of the same `hbs2-hub` binary, as a child
process, and returns what it printed, its exit code and what the code
means (the table `--codes` prints). The tool layer decides nothing the
verb does not: the checks a call relies on are the verb's own, and so is
the refusal of an argument the tool does not take.

`status`, `log`, `issue_show`, `pr_show`, `inbox_list` and `inbox_show`
run their verb with `--json` and return the document (see [For a program
rather than a person](#for-a-program-rather-than-a-person)) as structured
content as well as text. `whoami`, `issue_list` and `pr_list` return
text: those verbs print no document yet.

**Writes.** Every tool that writes takes `dry_run`, and its description
says that canon is public and permanent once published. `inbox_accept`,
`inbox_honour`, `issue_open`, `issue_close`, `issue_label` and
`issue_reply` pass it to the verb's own `--dry-run`, which runs every
check and prints the event without signing anything. `inbox_reject`,
`issue_comment`, `publish` and `sync` have no dry run of their own; for
them `dry_run` shows the command that would run and runs a read-only
look instead (`inbox show`, `whoami`, `status`). `--read-only` offers
none of the writing tools at all.

**Waiting for letters.** `inbox_wait` runs the follower described under
[Triage](#triage) and returns at the first read that holds a letter the
caller has not seen, or at `timeout_seconds` (300 by default, at most an
hour). Pass `known`, the message hashes you have already seen: a letter
not among them is returned at once, so nothing that arrived between two
calls is missed. Each answer carries `seen` for the next call. A client
that sends a progress token gets a progress notification every fifteen
seconds while it waits. It is a tool and not a server notification
because a client shows a tool's result to the model and, today, does not
show it a notification; and because the follower already knows when the
mailbox moved.

A read can be cancelled, and the verb under it is stopped; a write runs
to its end, since half an accept is worse than either outcome. Every
verb is stopped after `--timeout` seconds (600 by default). When the
client goes away, reads in flight are stopped and writes are left to
finish.

## Scripting

Exit codes are a contract: they may be added to and are never
reassigned. `0` is success and `1` is a usage error; every refusal has
its own number, so a hook can tell "the bridge would not bless this
letter" from "git would not write the commit" without reading the
message. `hbs2-hub compact` exits `42` for "there was nothing to
compact", which is not a failure and is the ordinary state of a young
forge.

```
hbs2-hub --codes
```

prints the whole table with a line each. It is not a copy of one kept
here: it is generated from the constants the verbs actually exit with, so
the tool and its documentation cannot come to disagree about a number a
script branches on. `3` to `16` belong to `hub verify` and say why canon
could not be read; `hbs2-hub help verify` documents those.

`hbs2-hub --help` lists the verbs and `hbs2-hub help <verb>` documents
one.

`hbs2-hub --version` prints the build, so a script can ask which
contract it is talking to.

Refusals and advice go to stderr; what a verb produced goes to stdout.
Not everywhere yet: a few verbs print "nothing to change" and "nobody is
banned here" on stdout, into the stream a script is reading keys from.
Branch on the exit code, not on the absence of output.

## What is missing

Worth knowing before you commit to this.

- **No web interface.** The per-thread render contract exists (see
  below) and nothing consumes it yet: there is no `hub render` static
  export, no `hub serve`, and no index or activity document. PEP-22
  leaves the schemas for those last two open, so they are not written
  yet rather than written wrong.

- **Queries are exact matches.** `--status` and `--label` filter, and
  that is all. There is no query language and no cache; every read folds
  canon in memory.

- **A rejected letter still takes disk, and a mailbox has a ceiling.**
  `hbs2-hub inbox reject` writes a tombstone so the queue stops showing
  it: triage makes a mailbox bigger, not smaller, and nothing prunes one.
  Reclaiming the space is not implemented. The reader stops walking at
  200000 blocks and says so, rather than paying whatever a stranger's
  tree costs: `hbs2-hub inbox` exits 2 and `hbs2-hub inbox accept`
  refuses until `--incomplete`. No fetch changes that, and until a purge
  exists nothing lowers a mailbox back under the line.
