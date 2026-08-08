# hbs2-hub: issues and pull requests, in the repository

`hbs2-hub` is a decentralized forge. Issues, pull requests, comments,
labels and merge records live inside the git repository they are about,
as a signed append-only log; contributors reach a project through an
encrypted mailbox rather than through an account on a server.

There is no web interface yet. Everything below is the command line.

## Status

Working and new. It ships for the first time in this release and has not
been through a season of real use. Parts of the design are not built
yet, including one that affects how contributors find a project at all:
read [What is missing](#what-is-missing) before you plan a migration
onto it.

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

You need a mailbox on your peer, a policy on it, and a way for
contributors to learn both.

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

**4. Publish a sigil for the mailbox** so contributors can seal letters
to it, and tell people the repository key and that sigil.

At this point a contributor with those two values can file an issue.

## Filing an issue

```
hbs2-hub issue new \
  --repo      <repo-key> \
  --sender    <your-sigil> \
  --recipient <hub-sigil> \
  --author    <your-sign-key> \
  --title     "the tests hang on aarch64" \
  --body -
```

`--body -` reads the body from stdin. Nothing is read from stdin
otherwise, deliberately: git hands a hook `<old> <new> <ref-name>` there,
and a body goes inside the signature and the event-id, where canon
cannot repair it.

It prints the message hash, which is the letter's identity everywhere
else, and the event-id the thread will have once folded.

## Proposing a change

```
hbs2-hub pr new \
  --repo      <repo-key> \
  --sender    <your-sigil> \
  --recipient <hub-sigil> \
  --author    <your-sign-key> \
  --title     "fix the aarch64 hang" \
  --onto      master \
  --from      my-branch
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

## Triage

```
hbs2-hub inbox --mailbox <mailbox-key> --repo <repo-key>
```

Read-only. It waits for your peer's copy of the mailbox to settle, opens
every message you hold a key for, and reports what each asks for.
Nothing is folded, minted or deleted. Expect it to take a few seconds:
an empty answer is only believable after the wait.

One letter in full, body included:

```
hbs2-hub inbox show --mailbox <mailbox-key> --message <message-hash>
```

Folding it into canon is the step that turns a request into the record,
and it cannot be taken back: an event in canon is in every clone that
ever fetches it.

```
hbs2-hub inbox accept --mailbox <mailbox-key> --repo <repo-key> \
                      --message <message-hash>
```

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
hbs2-hub issue close  --repo <repo-key> --number 7 --note "fixed in 1.2"
hbs2-hub issue label  --repo <repo-key> --number 7 --label bug --label aarch64
hbs2-hub issue assign --repo <repo-key> --number 7 --to <key>
hbs2-hub redact       --repo <repo-key> --event <event-id>
```

`redact` is display-level and PEP-19 says so: the bytes stay in every
clone, readers stop showing the body. It answers a mistake or abuse, not
a secret. A secret that reached canon has been published.

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

## Scripting

Exit codes are a contract: they may be added to and are never
reassigned. `0` is success and `1` is a usage error; every refusal has
its own number, so a hook can tell "the bridge would not bless this
letter" from "git would not write the commit" without reading the
message. `hbs2-hub compact` exits `42` for "there was nothing to
compact", which is not a failure and is the ordinary state of a young
forge. `hbs2-hub --help` lists the verbs and `hbs2-hub help <verb>`
documents one.

`hbs2-hub --version` prints the build, so a script can ask which
contract it is talking to.

Refusals and advice go to stderr; what a verb produced goes to stdout.
Not everywhere yet: a few verbs print "nothing to change" and "nobody is
banned here" on stdout, into the stream a script is reading keys from.
Branch on the exit code, not on the absence of output.

## What is missing

Worth knowing before you commit to this.

- **A repository cannot yet declare its ingress mailbox.** PEP-18 gives
  the manifest a `(mailbox <key> hub)` clause so a contributor who knows
  the repository can discover where to send a letter. `hbs2-hub` reads
  that clause: `inbox`, `inbox accept` and the composing verbs all fall
  back to `--repo` when the mailbox or the recipient sigil is not named.
  Nothing writes it. `hbs2-git3` has no verb that edits a manifest
  beyond group keys, so the clause can only get there by hand. Until
  that is fixed, publish the mailbox key and its sigil the way you would
  publish anything else, in the README or on a page.

- **No web interface.** The render contract PEP-22 describes, and with
  it any machine-readable output beyond exit codes, is not implemented.
  Everything prints for a person to read.

- **No `whoami`.** A contributor has no verb that tells them which of
  their keys is which, or which sigil they are sending under.

- **Queries are exact matches.** `--status` and `--label` filter, and
  that is all. There is no query language and no cache; every read folds
  canon in memory.

- **A rejected letter still takes disk.** `hbs2-hub inbox reject` writes
  a tombstone so the queue stops showing it. Reclaiming the space is not
  implemented.
