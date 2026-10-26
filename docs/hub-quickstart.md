# hbs2-hub quickstart

`hbs2-hub` adds issues and pull requests to a git repository published
over hbs2. There is no server and no account. The issues and pull
requests are stored in the repository, and contributors write to the
project through an encrypted mailbox.

This page is the short version: what to type for the common jobs. The
[full guide](hbs2-hub.md) explains each step and the rules behind it.
To watch the whole thing run on your machine in a minute and a half,
see the [demo repository](https://github.com/NCrashed/hbs2-demo).

## Five words

- **Repo key.** The project's address, `hbs23://<repo-key>`, made by
  `hbs2-git3 repo:init`. Holding its private key is owning the project.
- **Mailbox.** Where letters to the project arrive, on the owner's
  peer. Its public encryption key is published as a **sigil**. The
  repository names both, so a contributor needs only the repo key.
- **Letter.** An issue, comment or pull request, signed by its author
  and encrypted to the mailbox. A letter is a request, never a change.
- **Canon.** The project's record: a signed, append-only log in the
  repository under `refs/hbs2/meta`. Only the owner, or a maintainer
  the owner named, adds to it.
- **Publish.** Canon is a git ref. `hbs2-hub publish` pushes it;
  `hbs2-hub sync` fetches it.

## What you need

- A running `hbs2-peer` ([QUICKSTART.md](../QUICKSTART.md)).
- `hbs2-hub`, `hbs2-git3` and `git-remote-hbs23` on your `PATH`.

## Once, for everybody: an identity

```
hbs2-hub identity init
```

This makes a signing key (in `hbs2-keyman`), the sigil that lets a
maintainer answer you, and a mailbox on your peer where those answers
land. It remembers the key and the sigil in `git config --global`
(`hbs2.hub.author`, `hbs2.hub.sender`), so you never type them. Run it
again any time: it reuses what is there.

## Inside a clone, most flags fill themselves in

In a clone of an hbs2 project, `hbs2-hub` reads what it needs from the
clone:

- `--repo` from the `hbs23://` remote (`origin`, or the only one);
- `--author` and `--sender` from your identity;
- for `pr new`, `--onto` is the branch the remote's HEAD names and
  `--from` is the branch you are on.

It says what it filled in on stderr (`hbs2-hub: using --repo ...`). A
flag you type always wins. To override a default for a session, set
`HBS2_HUB_REPO`, `HBS2_HUB_AUTHOR` or `HBS2_HUB_SENDER`; for one clone,
`git config hbs2.hub.repo <key>`.

## Contributing to a project

```
git clone hbs23://<repo-key> project
cd project
```

Report a bug:

```
echo "What happened, and how to reproduce it." |
  hbs2-hub issue new --title "Short summary" --body -
```

Send a fix, from a branch:

```
git checkout -b my-fix
# edit, commit
hbs2-hub pr new --title "Fix the thing"
```

Only your new commits travel, as a git bundle inside the letter. Your
peer does a little proof-of-work before sending ("solving 18 bits of
work"), well under a second on a recent laptop; it is what keeps
mailboxes from being flooded.

See what happened:

```
hbs2-hub updates           # the owner's replies, in your mailbox
hbs2-hub sync              # fetch the project's record
hbs2-hub issue list
hbs2-hub pr list
hbs2-hub issue show <number>
```

Until the owner accepts your letter, it is not in the lists. That is
the design, not a delay: nothing a stranger sends becomes part of the
project by itself.

## Running a project

### Once: open it to contributions

In your clone of a repository you pushed with `hbs2-git3`, after
`hbs2-hub identity init`:

```
hbs2-hub project open
hbs2-hub publish
```

`project open` makes the project's mailbox and lets anyone write to it,
stores its sigil, and declares both in the repository, so a contributor
needs only the repo key. It also makes your identity a maintainer and
this clone's default signer, because the repository key cannot encrypt
and replies are sealed by the key that signs. `--pow 20` makes each
letter cost about a second of work, for a project that gets spam. Run
it again any time: what is already in place is kept.

Then give people the repo key.

### Go through what arrived

```
hbs2-hub inbox                      # one line per letter
hbs2-hub inbox --long               # with full ids
hbs2-hub inbox --follow             # keep running, print new letters
hbs2-hub inbox show --message <id>  # one letter, whole
```

Take a letter into the record, or turn it down:

```
hbs2-hub inbox accept --message <id>
hbs2-hub inbox reject --message <id>
```

Accepting cannot be undone: once published, it is in every clone. The
contributor gets a reply in their mailbox either way. Every verb that
writes the record takes `--dry-run` to show what it would write first.

A letter that asks you to close, reopen or label something is granted
with `hbs2-hub inbox honour --message <id>`.

### Review and merge a pull request

```
hbs2-hub pr checkout --number <n> --branch review
git diff master review
git checkout master
git merge --no-ff review
git push origin master
hbs2-hub pr merge --number <n> --commit <merge-commit> --into refs/heads/master
hbs2-hub issue close --number <issue> --note "Fixed by #<n>."
```

The merge is ordinary git, in whatever style you like. `pr merge` only
records it, and refuses if the commit does not contain the proposal.

### Publish

```
hbs2-hub publish
```

Nothing you did above is visible to anyone until this runs. If it
refuses because the remote has canon you do not (another maintainer
published first), run `hbs2-hub sync` and publish again.

### Write to the record directly

```
hbs2-hub issue open   --title "Release checklist"
hbs2-hub issue label  --number <n> --label bug
hbs2-hub issue assign --number <n> --assignee <key>
hbs2-hub issue reply  --number <n> --body "Thanks, looking."
hbs2-hub issue close  --number <n> --note "Done."
```

### Share the work

The person you want is usually somebody who has already sent issues or
pull requests. Add them by the number of one of their threads:

```
hbs2-hub maintainer add --author-of <number>
hbs2-hub publish
```

(`--maintainer-key <key>` works too.) On their side, once they have
synced:

```
hbs2-hub maintainer use
```

which makes their identity the key their clone signs the record with.
A maintainer can accept letters and write to the record. Publishing it
still needs push access to the repository, which `maintainer add` does
not give.

### Keep spam out

```
hbs2-hub project open --pow 20      # each letter costs about a second of work
hbs2-hub ban   --author-key <author-key>
hbs2-hub block --mailbox <mailbox-key> --envelope-key <envelope-key>
```

`ban` stops letters by an author from entering your record. `block`
stops your peer from storing and passing on a sender's letters. The
proof-of-work price is per letter.

## When something goes wrong

| You see | It means | Do |
| --- | --- | --- |
| `fetching sigil ... from the network`, then `this node does not hold the sigil` | your peer could not get the project's sigil from its neighbours | check `hbs2-peer peers`; retry later |
| a verb asks for `--repo` | you are not in a clone with an `hbs23://` remote, or it has several and none is `origin` | `cd` into the clone, or pass `--repo` |
| a verb asks for `--author` or `--sender` | no identity on this machine | `hbs2-hub identity init` |
| `nothing waiting` in `inbox`, but a letter was sent | it has not arrived yet | wait, or use `inbox --follow` |
| the issue you sent is not in `issue list` | the owner has not accepted it, or not published | `hbs2-hub updates`; `hbs2-hub sync` again later |
| `no acknowledgement: the signer ... has no encryption key` | the project signs with the repository key | owner: `hbs2-hub project open` once |
| `publish` says the remote holds canon you do not | someone else published first | `hbs2-hub sync`, then `publish` |
| `repository ... not found` on clone | your peer cannot reach a peer that has it | check `hbs2-peer peers`; see [multi-machine.md](multi-machine.md) |

`hbs2-hub verify` rechecks every signature in the record and needs no
network. `hbs2-hub --codes` lists the exit codes for scripts.
`hbs2-hub help <verb>` documents any command.
