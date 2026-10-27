PEP-25: hbs2-hub on the web, without GitHub
============================================

Status: draft
Builds on: PEP-22 (the render contract), PEP-24 (the browser extension)


Why
===

PEP-24 puts hbs2-hub on a project's GitHub pages. A project that leaves
GitHub has no such pages: nothing shows its code, and its issues and
pull requests are reachable from the command line only. This is the
frontend for that project, and it is the same frontend: the extension's
pages, served by `hbs2-hub serve` instead of mounted on GitHub, plus a
view of the code that GitHub used to provide.


What
====

`hbs2-hub serve` is an HTTP server with two modes.

  - LOCAL (the default): on 127.0.0.1, for the person at the machine. It
    reads and writes, as the extension does, through the same tool table
    (`hub mcp`, PEP-24's native host), signing with the user's own keys.
  - PUBLIC (`--public`): on an address the operator chooses, read-only,
    for anyone. The project's page on the web: its code, README, issues
    and pull requests. No tool that writes is offered, and no key is
    reachable from it.

The pages are the extension's, built for the web (`dist/web` in
hbs2-hub-browser) with an HTTP transport in place of native messaging.
They add a repository view: README, file tree, a file, the history, a
commit, the branches and tags.


The pages
=========

One codebase in hbs2-hub-browser. The extension and the web build share
every view (lists, threads, the forms, the inbox, the maintainers); the
web build adds:

  - a home page: the repositories this server shows;
  - a repository header with Code, Issues, Pull requests (and for a
    maintainer in local mode, Inbox), as GitHub's;
  - Code: the README rendered, the tree at a ref and path, a file, the
    log, a commit with its diff, the branches and tags.

The web build keeps the extension's pages as they are: one HTML page each,
named by query parameters (`pages/repo.html?key=...`, `pages/thread.html?
key=...&kind=pr&number=2`), with a home page at the root. The only
differences are the transport and a top bar; the server serves files and
the API, nothing else.

`hbs2-hub serve` finds the pages in a directory: `--web <dir>`, else
`share/hbs2-hub/web` beside the binary (where the nix package and the
release tarballs put them). The Haskell build does not need node.


The API
=======

  - `GET /api/hello`: version, mode (local or public), the repositories
    (public), the identity (local).
  - `GET /api/tools`: the tool table, as the native host's `tools`.
  - `POST /api/call` with `{tool, args}`: the result, as the native host's
    `call`, streamed as newline-delimited JSON: progress lines, then one
    result line.

The code view's read tools, `code_refs`, `code_tree`, `code_blob`,
`code_log` and `code_commit`, run the verbs `hub code refs|tree|blob|log|show`
against the clone a call runs in, with the same bounds every git read in
hbs2-hub has: a time limit, a cap on what is read (512 KiB of a file or a
diff, cut there and marked truncated), and git's environment hardened. A ref
is a branch, a tag, a sha or HEAD and never an expression; a path is plain
names, none of them `.` or `..`; both reach git after `--end-of-options`.

A repository is named on its pages by its manifest: `(name "...")` and
`(description "...")`, which `hbs2-git3 repo:describe` writes and
publishes as it does the mailbox clauses. Without them it is its key.


Threat model
============

LOCAL. The server is a signing oracle for the user, exactly as the native
host is, and reachable from any page in any browser on the machine, which
the native host is not. So:

  - it binds 127.0.0.1 only, and refuses a request whose Host is not
    `127.0.0.1:<port>` or `localhost:<port>` (DNS rebinding);
  - every `/api` request carries a per-run token, printed with the URL
    the server opens; a request without it is refused before anything
    runs. A token in the URL is moved into session storage and out of the
    address on the first load;
  - it sends no CORS headers, so no other origin can read an answer, and
    refuses a cross-origin POST by its Origin header;
  - writes still go through the review: the compose window, the inbox and
    the maintainers page show the dry run first, as in the extension.

PUBLIC. A stranger's requests run verbs on the operator's machine. So:

  - an allowlist of tools, not every read: the issue and pull request
    lists and threads, the log, verify, the maintainers and the code view.
    The inbox is a read and is not public: it opens letters sealed to the
    operator's mailbox. Neither are status and whoami, which describe the
    operator's keys;
  - an allowlist of repositories (`--repo <key>`, repeated): a request for
    any other key is refused. Without it a stranger could make the server
    clone and fetch any repository on the network;
  - no working clone: reads run in the server's own cache clones, synced
    on a timer, never per request;
  - bounded work: a fixed number of verbs at once, a queue that refuses
    past its depth, answers cached for a short time, and every verb under
    its existing time and size bounds;
  - nothing it serves is the operator's: no identity, no mailbox, no
    status of the operator's clone.

Every title, body, path and file is a stranger's bytes, as PEP-22 says:
text nodes and the extension's own Markdown reader, never HTML. A file is
served to the page as data, never as a document of its own, so an `.html`
in a repository cannot run in the server's origin.


Phases
======

  1. `hub serve` local: the API, the token, the pages for the web with the
     HTTP transport, the home page and the forge views. Done, with the
     public mode's tool allowlist and repository allowlist, which phase 3
     builds on.
  2. The code view: the `hub code` verbs, their tools, the pages. Done,
     with `hbs2-git3 repo:describe`, which writes `(name ...)` and
     `(description ...)` into the manifest for the header to show.
  3. `--public`: the allowlist, the read-only table, the bounds.
  4. Packaging: the web directory in the nix package and the tarballs.

`hub render` (a static site, PEP-24) stays an open question: a public
`hub serve` covers the same ground with one implementation, and a static
export can come later from the same pages.
