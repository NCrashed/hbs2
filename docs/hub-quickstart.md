# hbs2-hub quickstart

`hbs2-hub` adds issues and pull requests to a git repository published
over hbs2. There is no server and no account. The issues and pull
requests are stored in the repository, and contributors write to the
project through an encrypted mailbox.

This page is the short version: what to type for the common jobs. The
[full guide](hbs2-hub.md) explains each step and the rules behind it.
To watch the whole thing run on your machine in a minute and a half,
see the [demo repository](https://github.com/NCrashed/hbs2-hub-demo).

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

## Contributing to a project

### Once: an identity

```
mkdir -p ~/.hbs2-keyman/keys
hbs2-cli hbs2:keyring:new > ~/.hbs2-keyman/keys/me.key
hbs2-keyman update
hbs2-cli hbs2:keyring:show ~/.hbs2-keyman/keys/me.key    # sign-key: <my-key>
hbs2-cli hbs2:sigil:create:from-keyring ~/.hbs2-keyman/keys/me.key > me.sigil
hbs2-cli hbs2:sigil:store:file me.sigil                  # prints <my-sigil>
hbs2-peer mailbox create --key <my-key> hub
```

`<my-key>` signs your letters and appears in the project's record as
the author. `<my-sigil>` lets the owner answer you. The mailbox is
where answers arrive.

### Once per project: clone it and check

```
git clone hbs23://<repo-key> project
cd project
hbs2-hub whoami --author <my-key> --sender <my-sigil> --repo <repo-key>
hbs2-peer fetch <recipient-sigil>
```

`whoami` checks that your key and sigil belong together, and prints
the project's mailbox and the sigil to seal letters to. The `fetch`
brings that sigil to your peer; without it the first letter fails with
"this node does not hold the sigil".

### Report a bug

```
echo "What happened, and how to reproduce it." |
  hbs2-hub issue new --repo <repo-key> --author <my-key> --sender <my-sigil> \
                     --title "Short summary" --body -
```

Your peer does a little proof-of-work first ("solving 18 bits of
work"), well under a second on a recent laptop. It is what keeps
mailboxes from being flooded.

### Send a fix

```
git checkout -b my-fix
# edit, commit
hbs2-hub pr new --repo <repo-key> --author <my-key> --sender <my-sigil> \
                --title "Fix the thing" --onto master --from my-fix
```

Only your new commits travel, as a git bundle inside the letter.

### See what happened

```
hbs2-hub sync --repo <repo-key>
hbs2-hub issue list <repo-key>
hbs2-hub pr list <repo-key>
hbs2-hub issue show <repo-key> <number>
```

Until the owner accepts your letter, it is not in the list. That is
the design, not a delay: nothing a stranger sends becomes part of the
project by itself.

## Running a project

### Once: open it to contributions

Starting from a repository you pushed with `hbs2-git3`:

```
mkdir -p ~/.hbs2-keyman/keys
hbs2-cli hbs2:keyring:new > ~/.hbs2-keyman/keys/mailbox.key
hbs2-keyman update
hbs2-cli hbs2:keyring:show ~/.hbs2-keyman/keys/mailbox.key   # sign-key: <mailbox-key>
hbs2-peer mailbox create --key <mailbox-key> hub
hbs2-hub policy default --mailbox <mailbox-key> --sender allow --peer allow
hbs2-cli hbs2:sigil:create:from-keyring ~/.hbs2-keyman/keys/mailbox.key > mailbox.sigil
hbs2-cli hbs2:sigil:store:file mailbox.sigil                 # prints <mailbox-sigil>
hbs2-git3 repo:mailbox:set   <repo-key> --key <mailbox-key>
hbs2-git3 repo:mailbox:sigil <repo-key> --key <mailbox-key> --sigil <mailbox-sigil>
hbs2-hub status --repo <repo-key>
```

Then give people the repo key. That is all they need.

### Go through what arrived

```
hbs2-hub inbox --repo <repo-key>               # one line per letter
hbs2-hub inbox --repo <repo-key> --long        # with full ids
hbs2-hub inbox --repo <repo-key> --follow      # keep running, print new letters
hbs2-hub inbox show --mailbox <mailbox-key> --repo <repo-key> --message <id>
```

Take a letter into the record, or turn it down:

```
hbs2-hub inbox accept --repo <repo-key> --message <id>
hbs2-hub inbox reject --repo <repo-key> --mailbox <mailbox-key> --message <id>
```

Accepting cannot be undone: once published, it is in every clone.
Every verb that writes the record takes `--dry-run` to show what it
would write first.

A letter that asks you to close, reopen or label something is granted
with `hbs2-hub inbox honour --repo <repo-key> --message <id>`.

### Review and merge a pull request

```
hbs2-hub pr checkout --repo <repo-key> --number <n> --branch review
git diff master review
git checkout master
git merge --no-ff review
git push origin master
hbs2-hub pr merge --repo <repo-key> --number <n> --commit <merge-commit> --into refs/heads/master
hbs2-hub issue close --repo <repo-key> --number <issue> --note "Fixed by #<n>."
```

The merge is ordinary git, in whatever style you like. `pr merge` only
records it, and refuses if the commit does not contain the proposal.

### Publish

```
hbs2-hub publish
```

Nothing you did above is visible to anyone until this runs. If it
refuses because the remote has canon you do not (another maintainer
published first), run `hbs2-hub sync --repo <repo-key>` and publish
again.

### Write to the record directly

```
hbs2-hub issue open   --repo <repo-key> --title "Release checklist"
hbs2-hub issue label  --repo <repo-key> --number <n> --label bug
hbs2-hub issue assign --repo <repo-key> --number <n> --assignee <key>
hbs2-hub issue close  --repo <repo-key> --number <n> --note "Done."
```

### Share the work

```
hbs2-hub maintainer add  --repo <repo-key> --maintainer-key <their-key>
hbs2-hub maintainer list --repo <repo-key>
```

A maintainer can accept letters and write to the record. Publishing
needs push access to the repository.

### Keep spam out

```
hbs2-hub policy pow --mailbox <mailbox-key> --bits 20   # each letter costs ~1 s of work
hbs2-hub ban   --repo <repo-key>       --author-key <author-key>
hbs2-hub block --mailbox <mailbox-key> --envelope-key <envelope-key>
```

`ban` stops letters by an author from entering your record. `block`
stops your peer from storing and passing on a sender's letters. The
proof-of-work price is per letter.

## When something goes wrong

| You see | It means | Do |
| --- | --- | --- |
| `this node does not hold the sigil <hash>` | your peer has not fetched the project's sigil | `hbs2-peer fetch <hash>`, then retry |
| `nothing waiting` in `inbox`, but a letter was sent | it has not arrived yet | wait, or use `inbox --follow` |
| the issue you sent is not in `issue list` | the owner has not accepted it, or not published | ask the owner; `hbs2-hub sync` again later |
| `no acknowledgement: no encryption key for <repo-key>` | the repository key cannot seal replies | nothing to fix for the record; contributors read it with `sync` |
| `publish` says the remote holds canon you do not | someone else published first | `hbs2-hub sync --repo <repo-key>`, then `publish` |
| `repository ... not found` on clone | your peer cannot reach a peer that has it | check `hbs2-peer peers`; see [multi-machine.md](multi-machine.md) |

`hbs2-hub verify <repo-key>` rechecks every signature in the record and
needs no network. `hbs2-hub --codes` lists the exit codes for scripts.
`hbs2-hub help <verb>` documents any command.
