PEP-24: hbs2-hub in the browser

Status: draft, started 2026-10-26.
Author: NCrashed (Anton Gushcha)
Part of: PEP-17 (hbs2-hub umbrella).
Depends on: PEP-19 (canon, fold), PEP-22 (CLI and render contract),
            the `hub mcp` tool layer (hbs2-hub/ingress/HBS2/Hub/Mcp.hs).
Related: PEP-18 (letter), PEP-20 (pull requests), PEP-21 (triage).

A browser extension for Firefox and Chrome that shows a repository's hbs2-hub
issues and pull requests on its GitHub pages, and lets a person file, propose,
triage and accept from there. It is the first human frontend of the forge and
it is deliberately built so the second one, for projects that are not on
GitHub at all, is the same application over a different transport.

It adds no protocol to hbs2. It adds one verb to hbs2-hub (`native-host`), one
document to the render contract (`index`), and one file convention for
repositories (`.hbs2/hub`).


Motivation
==========

The projects using the forge today are hybrids. openwar3 pushes its code to
GitHub and to `hbs23://7VM1...` from the same `origin`, and its issues and pull
requests, many of them written by agents through `hub mcp`, live in canon on
the hbs2 side. The people who read the code read it on GitHub. They see none of
the forge unless they install hbs2, clone, and run `hbs2-hub issue list`.

An extension closes that gap without asking anybody to leave GitHub, and
without GitHub's cooperation: GitHub stays where the code is browsed, canon
stays where the record is kept, and the page shows both. Nothing is mirrored
into GitHub Issues, so there is still exactly one source of truth.


Goals and non-goals
===================

Goals:

  - Read, on a GitHub repository page: the hbs2 issue and pull request lists,
    and any thread in full, rendered from the PEP-22 contract.
  - Write, from the same page, through the user's own hbs2-hub: file an issue,
    comment, propose a change, and, for a maintainer, triage the inbox, reply,
    close, label, record a merge and publish.
  - Firefox and Chrome from one code base (WebExtensions, Manifest V3).
  - The same UI, later, as a standalone frontend for projects with no GitHub
    repository (`hub serve`), and a static export for reading without hbs2
    (`hub render`, PEP-22).

Non-goals:

  - No keys in the browser and no cryptography in JavaScript. Signing, sealing,
    proof-of-work and verification stay in hbs2-hub, where they are tested.
  - No GitHub API calls that write, and no GitHub token. The extension reads
    public GitHub pages and one public file; it never acts on GitHub as the
    user.
  - Not a GitHub replacement and not a mirror: hbs2 threads are not copied into
    GitHub Issues or Pull Requests.
  - Not a peer. The extension does not speak the hbs2 wire protocol.


Architecture
============

Three parts, and only the last one holds anything that matters:

```
  github.com page            extension                     this machine
  +---------------+   DOM    +----------------------+  stdio  +------------------+
  | content script|<-------->| background (service  |<------->| hbs2-hub         |
  | (isolated     |  ports   |  worker), extension  | native  |   native-host    |
  |  world)       |          |  pages (thread view, |  msg    |   -> hub verbs   |
  +---------------+          |  forms, inbox)       |         |   -> hbs2-peer   |
                             +----------------------+         +------------------+
```

  - Content script: finds the repository, reads `.hbs2/hub`, adds an "hbs2"
    tab to the repository header, the hbs2 rows to GitHub's issue and pull
    request panel, and draws a thread in place. Everything it draws is text
    nodes. It never sends a write request.
  - Background service worker: owns the one native messaging connection, routes
    requests, enforces which origin may ask for what (see Threat model).
  - Extension pages: the forms and the maintainer inbox, which write, run in
    the extension's origin. Reading views (the rows and the thread) are also
    drawn inside GitHub's page, from text nodes only, see "Canon text".
  - `hbs2-hub native-host`: a long-running child the browser starts. It is the
    `hub mcp` tool layer behind a different framing: each request names a
    tool and its arguments, and the host runs the verb as a child process,
    exactly as `hub mcp` does, and returns its JSON document or its refusal.


Finding the repository: `.hbs2/hub`
==================================

A repository that has an hbs2 side says so in a file at its root on the
default branch:

```
# hbs2-hub pointer: where this repository's issues and pull requests live
hbs23://7VM1rtxrTmpXYVidNFHu5ZbNqRKMhKoRAV8vJebGaRN9
```

Rules:

  - UTF-8 text. Lines starting with `#` and blank lines are ignored. The first
    other line is the repository URL, `hbs23://<base58 key>`, and nothing else
    is read in this version. Later versions may add `name value` lines; a
    reader ignores lines it does not know.
  - The content script fetches it from
    `https://raw.githubusercontent.com/<owner>/<repo>/HEAD/.hbs2/hub`, which is
    public and needs no token. A 404 means the repository has no hbs2 side and
    the extension shows nothing.
  - The user can also map a GitHub repository to a key by hand in the
    extension's settings, for a repository whose owner has not added the file.
    A hand mapping wins over the file.

`hbs2-git3 repo:init` should offer to write the file, and `hub project open`
should check that it is there and matches.

The pointer is a claim made by whoever can push to the GitHub repository, and
it is treated as one: see "A pointer that lies" below.


The native messaging protocol
=============================

Framing is the browsers' own: each message is a 32-bit length in native byte
order followed by that many bytes of UTF-8 JSON, in both directions. A message
from the host to the browser may not exceed 1 MB in either browser; the other
direction allows far more but nothing here needs it.

Request, from the background worker:

```
{ "id": 17, "op": "call", "tool": "issue_list",
  "args": { "repo": "7VM1..." } }
```

Responses, from the host:

```
{ "id": 17, "ok": true, "result": { "contract": 1, "document": "index", ... } }
{ "id": 18, "ok": false,
  "error": { "code": 52, "meaning": "sigil-not-held", "message": "..." } }
{ "id": 19, "progress": "solving 18 bits of work" }
```

  - `op` is one of `hello`, `tools`, `call`, `cancel`.
    - `hello` returns the host's version, the `hub-meta` it reads, and the
      contract versions it emits, so an old host and a new extension can tell
      each other apart instead of failing on a field.
    - `tools` returns the tool table: name, title, argument schema, and
      whether it reads or writes and whether it reaches the network. It is the
      `hub mcp` table, so the two cannot drift.
    - `call` runs one tool. `cancel` kills the child running request `id`.
  - Tool names, argument names and result documents are the `hub mcp` ones.
    A tool the MCP layer does not have yet is added there first and appears
    here with no host change.
  - Errors carry the CLI's exit code and its name from `hbs2-hub --codes`,
    so the UI can branch on meaning, plus the verb's own stderr as text for
    the person.
  - Long operations (sync, proof-of-work, `inbox_wait`) send `progress`
    messages carrying the verb's stderr lines, then one final response.
  - Documents over the 1 MB limit are paged by the tool (an index has a
    cursor, a diff is truncated and says so, PEP-22 `truncated`). The host
    never splits one JSON document across messages.

Installing the host means writing one manifest per browser, naming the binary
and the extension allowed to start it:

  - Chrome: `~/.config/google-chrome/NativeMessagingHosts/app.hbs2.hub.json`
    (and the Chromium path), with `"allowed_origins": ["chrome-extension://<id>/"]`.
  - Firefox: `~/.mozilla/native-messaging-hosts/app.hbs2.hub.json`, with
    `"allowed_extensions": ["<id>"]`.

`hbs2-hub native-host --install` writes both for the binary it is, with the
published extension ids built in, and `--uninstall` removes them. The NixOS
and Home Manager modules can write the same files declaratively.


Where the host reads and writes
===============================

Every read verb folds a git ref in a clone, so the host needs one.

  - A CACHE CLONE per repository key, created on first use under
    `$XDG_CACHE_HOME/hbs2-hub/repos/<key>` with `git clone hbs23://<key>` and
    kept current with `hub sync`. Enough for reading, and for a contributor's
    letters, which write nothing locally.
  - A WORKING CLONE, named in the extension settings or in
    `git config --global hbs2-hub.<key>.clone <path>` (a subsection, since a
    base58 key may start with a digit), for anything that writes
    canon or needs the user's branches: accept, reply, close, merge, publish,
    and a pull request from a local branch. Writes never go to the cache
    clone, because canon written there would be published from a clone the
    user does not look at.

A pull request needs commits. Two sources, both producing the same `pr new`:

  - a branch in the working clone, as today;
  - a branch of a GitHub fork, for somebody who works on GitHub: the host
    fetches `https://github.com/<fork>.git <branch>` into the cache clone and
    proposes from there, with the base computed against the upstream default
    branch. Only public forks, since the host holds no GitHub credentials.


The `index` document
====================

PEP-22 left the list-view schema open. It is pinned here, because a list is
the first thing this frontend shows:

```
{
  "contract": 1,
  "document": "index",
  "repo": "<repo-key>",
  "canon": "<commit of refs/hbs2/meta it was folded from>",
  "kind": "issue",                     // issue | pr | all
  "threads": [
    { "number": 7, "kind": "issue", "thread_id": "<event-id>",
      "title": "...", "status": "open", "labels": ["bug"],
      "author": "<sign-key>", "comments": 3,
      "created_at": 1737763200000, "updated_at": 1737849600000,
      "redacted": false }
  ],
  "cursor": null                       // or a value to pass back for the next page
}
```

Same rules as the thread document: derived from canon only, times are folded
times in milliseconds, a redacted thread has `title: null`, unknown fields are
ignored by readers, the number moves only when a field changes meaning or goes
away. Emitted by `hub issue list --json` and `hub pr list --json`, which do not
exist yet, and by the `issue_list` / `pr_list` tools.


What the UI does
================

On github.com/<owner>/<repo>, when a pointer or a mapping exists:

  - an "hbs2" tab in the repository header, with open issue and PR counts;
  - on `/issues` and `/pulls`, a section listing hbs2 threads (from `index`),
    each line opening the thread view;
  - the thread itself, drawn in place on the issues or pulls page at
    `#hbs2/<kind>/<number>`, under GitHub's own header and laid out like a
    GitHub issue: title and number, state, the conversation as comment boxes
    with a generated face per key, labels and participants down the side,
    and for a PR its coordinates and precomputed diff (PEP-22). Every item is
    attributed by `author` and `canon_by` key. The same view is the
    extension's own thread page, and later the standalone frontend's;
  - "New issue on hbs2" and "New pull request on hbs2" buttons in the list
    panel, a comment box at the end of a thread, and for a maintainer a merge
    box on an open pull request. Each opens the extension's form in a window of
    its own (`compose.html`); a draft typed on the GitHub page is carried there
    and shown again;
  - for a maintainer (the identity is in `maintainer list`), an inbox page
    with accept, reject and honour per letter, reply and close on threads,
    and an "unpublished" badge with a Publish button.

The form works in three steps: the form; a review, which runs the tool with
`dry_run` and shows its output and what signing does; and the write, after
which canon is published when the tool wrote it. Whether the person maintains
the repository comes from `status --local` in the working clone (no remote is
asked): a maintainer's issue and comment go straight into canon (`issue_open`,
`issue_reply`) and are published, anybody else's are letters (`issue_new`,
`issue_comment`). A pull request is always a letter (`pr_new`), since its
commits travel as the letter's bundle. Merge is `hub pr land` (`pr_land`):
a merge commit computed without touching the working tree, pushed to the hbs23
remote and to `origin` when that is another remote, recorded and published.

On a GitHub pull request or commit page of the same repository, "Record merge"
offers `hub pr merge` with that commit filled in, for a maintainer who merged
an hbs2 pull request through GitHub. The verb still checks that the commit
contains the proposal.

Injection into GitHub's DOM is kept to anchors and list lines, attached to the
most stable landmarks the page has and failing quietly: when GitHub changes its
markup, the toolbar popup still opens every view. The extension never depends
on GitHub's own JavaScript.


Threat model
============

The host is a signing oracle for the user's identity and, for a maintainer, for
canon. Everything below is about who can make it sign.

  - OTHER WEBSITES AND EXTENSIONS. Only the extension ids in the host
    manifest can start the host; the browser enforces it. No web page can
    reach native messaging at all.
  - THE GITHUB PAGE ITSELF. Content scripts run in an isolated world, but they
    read a page an attacker can influence (a repository, an issue title, a
    README). So the content script's port is READ-ONLY: the background worker
    refuses any `call` of a writing tool that arrives from a content-script
    port. Writes come only from extension pages, and only after the user
    confirms a summary of exactly what will be signed (tool, repository key,
    author or canon key, title and body), rendered by the extension, not by the
    page. Where a verb has `--dry-run`, the summary is the dry run's output.
  - CANON TEXT. Titles, bodies and comments are a stranger's bytes (PEP-22).
    They are inserted as text nodes, never as HTML, on GitHub's page as in
    the extension's. Markdown is read by the extension's own small reader
    into a tree and built with DOM calls: raw HTML in a body stays text, and
    a link keeps only an http(s) target. Nothing that reads is a way to
    write: the content script's port stays read-only wherever it draws. Control characters are escaped as PEP-22
    requires of every renderer.
  - A POINTER THAT LIES. `.hbs2/hub` can name any key. It cannot forge a
    thread, since canon is verified against the key it names (the repository
    key is the root of trust), but it can point readers at somebody else's
    tracker. So the extension pins the key per GitHub repository on first use,
    shows it, and warns, without following, when the pointer changes. A hand
    mapping in settings overrides the file.
  - LOCAL PROGRAMS. The host runs as the user with the user's keys, as the CLI
    does. It adds no privilege a local program did not already have.
  - A READ-ONLY SWITCH in the extension settings starts the host with the MCP
    layer's read-only tool set, for a machine that should only look.


The standalone frontend
=======================

The extension's pages talk to one interface: `call(tool, args)` returning a
document or a refusal, plus `progress`. Two transports implement it:

  - native messaging, in the extension;
  - HTTP on loopback, from `hub serve`, for projects that are not on GitHub: the
    same pages served as a local web app, with a per-session token in the URL so
    another local user's browser tab cannot drive it, and the same
    read-only/write split.

`hub render` (PEP-22) writes the read-only pages and their documents as a
static site, for publishing a tracker anywhere without hbs2.

So the extension is not a detour. Its pages, its renderer and its forms are the
frontend; GitHub is one place they can be mounted.


Code layout
===========

  - A new repository, `NCrashed/hbs2-hub-browser`: the extension (TypeScript,
    Manifest V3, one build for Firefox and Chrome), its pages, and the HTTP
    transport for `hub serve`.
  - In hbs2: `hub native-host` (framing, `hello`, `tools`, `call`, `cancel`,
    `--install`), the `index` document and `--json` on the list verbs, and the
    MCP tools still missing for this UI: `issue_new`, `pr_new`, `pr_merge`,
    `pr_checkout`, `maintainer_list`, `maintainer_add`, `identity_init` and
    `project_open` (`issue_new`, `pr_new` and `pr_land` are in). (`issue_list`, `pr_list`, `issue_show`, `pr_show`,
    `inbox_*`, `issue_reply`, `issue_comment`, `publish` and `sync` exist.)


Phases
======

  1. Read. `index` document and list `--json`; `native-host` with read tools;
     the extension shows lists and threads on GitHub. Tried on openwar3.
  2. Contribute. `issue_new`, `issue_comment` and `pr_new` from a working clone
     (done), and from a public GitHub fork branch, with confirmation and progress.
  3. Maintain. Reply and merge (`pr_land`) with publish are done; inbox, accept,
     reject, honour, close, label and record merge remain.
  4. Standalone. `hub serve` over the same pages; `hub render` static export.


Rejected alternatives
=====================

  - Talking to hbs2-peer's HTTP API from the extension. It is loopback-only and
    read-only by design, serves blocks and refs, not folded canon, and cannot
    sign. The fold would have to be reimplemented in JavaScript.
  - hbs2 in the browser (WebAssembly or a JavaScript port). Keys in the
    browser, a second implementation of the fold and the crypto to keep in
    step, and a peer that cannot hold storage or listen.
  - Mirroring into GitHub Issues through the GitHub API. Two sources of truth,
    a GitHub token with write access in the extension, and a hard dependency on
    the service the forge exists to not depend on.
  - A bookmarklet or user script. No native messaging, so no way to reach the
    user's keys without a local web server, which is the `hub serve` path.


Open questions
==============

  - Whether the `index` document should carry the last comment's author and
    time for list views, or whether `updated_at` is enough.
  - Paging: a cursor per tool, or a generic `offset/limit` the host applies.
  - The thread view on GitHub: an extension page in a new tab, or the browser's
    side panel (Chrome `sidePanel`, Firefox `sidebar_action`), which differ.
  - Whether `.hbs2/hub` should also be readable from the hbs2 repository
    manifest the other way round (a `(github ...)` clause), so a reader of the
    hbs2 side can find the GitHub side.
  - Store distribution: AMO signing for Firefox is required even for
    self-distribution; the Chrome Web Store needs a developer account. Until
    then, an unpacked build and a signed `.xpi`.
