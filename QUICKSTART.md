# Quickstart

This walkthrough takes you from a fresh install to a git repository
that has been pushed into hbs2 and cloned back out. It runs entirely
on a single machine in two terminals and needs no external peers.

If you have not installed hbs2, see [`INSTALL.md`](INSTALL.md) first.

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
peer, `git hbs2 export`, sync, and so on) ask the key manager rather
than reading files directly. Point it at the config directory so any
key you put there gets picked up:
```
hbs2-keyman add-mask "$HOME/.config/hbs2-peer/*.key"
```
You only need to do this once per machine.

## 3. Generate a peer identity

The peer needs a long-term keypair to identify itself on the network.
Create it at the path the config expects, then have the key manager
rescan:
```
hbs2 keyring-new > ~/.config/hbs2-peer/default.key
hbs2-keyman update
```
This file contains both the private and public halves of the keypair.
Keep it private.

For multi-machine setups you will later add either `known-peer`
entries pinning specific peers, or a `bootstrap-dns` line pointing
at a domain whose TXT record lists peer addresses. The legacy
`bootstrap.hbs2.net` is no longer served; bring your own bootstrap
when you scale beyond one machine.

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
hbs2 keyring-new > ~/.config/hbs2-peer/myrepo.key
hbs2-keyman update
hbs2 keyring-list ~/.config/hbs2-peer/myrepo.key
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
Export the repository to hbs2 using the key from step 5. This
creates a reflog for it:
```
git hbs2 export --public --new <REPO_KEY>
```
Add an hbs2 remote and push:
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
  repository.

- **Encrypted repositories.** Use group keys to make a repository
  readable only by holders of specific keys. (Dedicated walkthrough
  pending.)

- **Architecture.** See [`ARCHITECTURE.md`](ARCHITECTURE.md) for a
  tour of the components involved in what you just did.
