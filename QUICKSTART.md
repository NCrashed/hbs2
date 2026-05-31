# Quickstart

This walkthrough takes you from a fresh install to a git repository
that has been pushed into hbs2 and cloned back out. It runs entirely
on a single machine in two terminals and needs no external peers.

If you have not installed hbs2, see [`INSTALL.md`](INSTALL.md) first.

If you came from the legacy `hbs2` monolithic CLI, the old commands
are gone. The `hbs2-cli` and `hbs2-git3` tools replace them. See
[`docs/CLI_MIGRATION.md`](docs/CLI_MIGRATION.md) for the full mapping.

## 1. Generate a peer config

Each `hbs2-peer` instance reads its config from a directory. The
default location is `~/.config/hbs2-peer`. Generate a default config
there:
```
hbs2-peer init
```
This writes `~/.config/hbs2-peer/config` with sensible defaults: UDP
on 7351, TCP on 10351, key path `./default.key`. Open the file if you
want to see it; for this walkthrough no edits are needed.

`hbs2-peer init` does not generate the key file itself; the next step
does that.

## 2. Register the keyring directory

hbs2 ships with a small key manager (`hbs2-keyman`) that knows where
your local private keys live. Tools that need to sign something (the
peer, `git hbs2`, sync, and so on) ask the key manager rather than
reading files directly. Point it at the config directory so any key
you put there gets picked up:
```
hbs2-keyman add-mask "$HOME/.config/hbs2-peer/*.key"
```
You only need to do this once per machine.

## 3. Generate a peer identity

The peer needs a long-term keypair to identify itself on the network.
Create it at the path the config expects, then have the key manager
rescan:
```
hbs2-cli hbs2:keyring:new > ~/.config/hbs2-peer/default.key
hbs2-keyman update
```
This file contains both the private and public halves of the keypair.
Keep it private.

For multi-machine setups you will later add either `known-peer`
entries pinning specific peers, or a `bootstrap-dns` line pointing
at a domain whose TXT record lists peer addresses. The hardcoded
default is `bootstrap.hbs2.app`; the domain is reserved but the
bootstrap node behind it is not yet deployed, so for now pin
specific peers with `known-peer` until that node is up.

## 4. Start the peer

In terminal 1:
```
hbs2-peer run
```
The peer starts up, opens its listening sockets, and waits. Leave it
running.

## 5. Create a repository key

A hbs2 git repository is identified by a public signing key. Create
a keypair for it in the same directory the key manager watches, then
rescan. In terminal 2:
```
hbs2-cli hbs2:keyring:new > ~/.config/hbs2-peer/myrepo.key
hbs2-keyman update
hbs2-cli hbs2:keyring:show ~/.config/hbs2-peer/myrepo.key
```
The output has a line like:
```
sign-key: eq5ZFnB9HQTMTeYasYC3pSZLedcP7Zp2eDkJNdehVVk
```
That is the public identifier of the repository. Copy it; the rest
of this walkthrough refers to it as `<REPO_KEY>`.

You can confirm the key manager sees the secret half with:
```
hbs2-keyman list
```

## 6. Push a git repository

Pick an existing git repository or create a small one:
```
mkdir myrepo && cd myrepo
git init
echo hello > README.md
git add README.md
git commit -m "Initial commit"
```
Initialize the repository for hbs2. The `--new` flag tells `git hbs2`
to set up local state for a new repository and bind it to a signing
key from the key manager:
```
git hbs2 init --new
```
Confirm the local hbs2 remote setup:
```
git hbs2 remotes
```
This lists the configured remotes and the signing key bound to each.
Then add a git remote pointing at the repository key and push:
```
git remote add hbs2 hbs2://<REPO_KEY>
git push hbs2
```
The push tells your local `hbs2-peer` to publish the blocks into its
storage.

## 7. Clone it back

To verify, clone the same repository into a separate directory:
```
cd /tmp
git clone hbs2://<REPO_KEY> myrepo-clone
cd myrepo-clone
cat README.md
```
You should see `hello`. The clone talks to your running `hbs2-peer`
over its local socket and reads the blocks back out.

## Where to go next

- **Replicate to a second machine.** Repeat steps 1-4 on another
  machine. Configure both peers to find each other via `known-peer`
  in the config, or by setting up a DNS TXT record and pointing
  `bootstrap-dns` at it. The two peers will then replicate the
  repository. See [`docs/MIRROR_SETUP.md`](docs/MIRROR_SETUP.md) for
  a multi-machine mirror setup.

- **Encrypted repositories.** Use group keys to make a repository
  readable only by holders of specific keys. `git hbs2 init --new
  --encrypted <group-key-hash>` initialises a repository with
  symmetric group encryption.

- **Architecture.** See [`ARCHITECTURE.md`](ARCHITECTURE.md) for a
  tour of the components involved in what you just did.
