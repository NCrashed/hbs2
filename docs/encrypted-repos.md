# Encrypted repositories

By default an hbs2 git repository is public: anyone who learns its
`<REPO_KEY>` can clone it. An encrypted repository instead restricts
read access to the holders of specific keys, using a **group key**.
This builds on [`QUICKSTART.md`](../QUICKSTART.md) and pairs naturally
with [`multi-machine.md`](multi-machine.md).

## How it works

A group key is a list of recipient public encryption keys. When you
initialise a repository with a group key, the git objects (the data
segments) are encrypted with a symmetric key that is sealed
separately to each recipient. The repository manifest carries a
`(gk "<hash>")` pointer to the stored group key, so the key travels
with the repository.

Two consequences follow:

- The data segments are unreadable without a member key, but the
  metadata envelope (object framing, the manifest) is not encrypted.
  This is deliberate: an untrusted peer can store and forward the
  repository without being able to read it, which is what makes
  mirroring an encrypted repo safe.

- Reading is transparent. If your local key manager holds a recipient
  secret key that matches the group, `git clone` and `git pull`
  decrypt automatically. There is no separate "unlock" step.

The encryption keypair is distinct from the peer's signing identity.
Conveniently, the peer keyring you generated in the quickstart
(`~/.config/hbs2-peer/default.key`) already contains one encryption
keypair, and `hbs2-keyman` already scans it via the mask you added in
quickstart step 2. So each machine can use its own peer keyring as its
group membership identity with no extra setup.

## Step 1: collect each member's public encryption key

On every machine that should be able to read the repository, print
the keyring and copy the `pub-key:` value (that is the public
encryption key; `sign-key:` is the signing key and is not what you
want here):
```
hbs2-cli hbs2:keyring:show ~/.config/hbs2-peer/default.key
```
Example output:
```
sign-key:  HwPB9bR3GWf67QfwicLCiNpfhpwMxkyYEJ3eGj7Bifmr
pub-key:   4rRpqUtt5fLzKR85znxQ4K4inr21vQMiN1ghZxGgdDAf
```
Collect one `pub-key:` per member. Below they are `<ENC_A>` and
`<ENC_B>`.

## Step 2: create the group key

On the machine that will own the repository (say machine A), create
the group key from the member list and store it in your peer. The
nested form below creates the key and stores it in one go, printing
the group key hash:
```
hbs2-cli "(hbs2:groupkey:store (hbs2:groupkey:create <ENC_A> <ENC_B>))"
```
The output is a single base58 hash, called `<GK_HASH>` below. Storing
it in your peer means it will replicate alongside the repository.

Check the membership at any time:
```
hbs2-cli "(hbs2:groupkey:dump \"<GK_HASH>\")"
```
This lists the `group-key-id` and one `member` line per recipient.

## Step 3: initialise an encrypted repository

This is the ordinary quickstart push with one extra flag,
`--encrypted`, carrying the group key hash:
```
mkdir secret && cd secret
git init
echo "classified" > README.md
git add README.md
git commit -m "initial"
git hbs2 init --new --encrypted <GK_HASH>
```
As with a public repo, `git hbs2 init` prints the new `<REPO_KEY>`
and wires up a remote. Push it:
```
git push <remote>
```
(use the remote name from `git hbs2 remotes`).

## Step 4: read it on another member's machine

On machine B, whose `pub-key:` you included in the group and whose
secret key `hbs2-keyman` already knows, clone as usual:
```
git clone hbs23://<REPO_KEY> secret
cat secret/README.md
```
Decryption happens transparently. A peer that is *not* a group member
can still replicate the encrypted blocks (and so can act as a mirror),
but cannot read the contents.

If a clone comes back empty or fails to decrypt, check that this
machine actually holds a member secret:
```
hbs2-cli "(hbs2:groupkey:find-secret \"<GK_HASH>\")"
```
It prints a key id when `hbs2-keyman` holds a matching member secret,
and reports `groupkey secret not found` otherwise. The usual cause of
the latter is that the keyring holding the member key is not in
keyman's scan path; add it with `hbs2-keyman add-mask` and run
`hbs2-keyman update`.

## Adding or removing members

You can derive a new group key from an existing one. You must be a
current member (your secret must be in `hbs2-keyman`), because
changing the membership requires decrypting the current key first. The
form below loads the current key, applies the changes, and stores the
result, printing a new hash:
```
hbs2-cli "(hbs2:groupkey:store (hbs2:groupkey:update (hbs2:groupkey:load <GK_HASH>) (list (add . <ENC_C>) (remove . <ENC_A>))))"
```
Each entry is `(add . <key>)` or `(remove . <key>)`; include as many
as you need.

Two caveats:

- Re-pointing an existing repository at a new group key is not yet a
  single command. In practice, use the new group key when you
  initialise a fresh repository; an existing repository keeps the
  group key it was created with.

- The scheme has no perfect forward secrecy. A member who later leaves
  can still decrypt anything that was ever encrypted to a group key
  they held. If that matters, rotate the key and re-encrypt the
  content rather than relying on removal alone.

## Back up your keys

This is the part that bites people. With an encrypted repository your
member secret key *is* your access, and there is no recovery path:

- Keys are random. A keyring is freshly generated random key material;
  there is no seed phrase or password from which to reproduce it.
  Regenerating `default.key` gives you a brand new encryption keypair
  with a *different* public key, which is not a member of any existing
  group key.

- There is no escrow and no reset. The group key seals the symmetric
  key to each member's public key; without a surviving member's secret
  key nothing can decrypt it. If every member loses their key, the
  encrypted blocks remain on the network forever but are permanently
  unreadable, including by you.

So treat the keyring that holds your member secret as irreplaceable.
That is the file you read in step 1, normally
`~/.config/hbs2-peer/default.key` (or a dedicated keyring under
`~/.hbs2-keyman/keys/` if you made one). Back it up the way you back
up an SSH private key:

- keep an offline copy (it is unencrypted private key material, so
  store it encrypted at rest, for example on an encrypted volume or in
  a password manager),
- never commit it to a repository,
- restore by copying the file back into the keyring directory and
  running `hbs2-keyman update`.

For anything you care about, do not rely on a single key. Add a second
member at group-key creation time so one lost key is not fatal: a
co-maintainer's key, or a second backup keyring that you control and
store separately. Plan this up front, because adding a member later
(see rotation above) itself requires a surviving member secret.

The group key itself is not secret and replicates with the repository,
so it does not need backing up; it is only useful to someone who also
holds a member secret. The repository *signing* key
(`~/.hbs2-keyman/keys/<REPO_KEY>-lwwref.key`) is a separate concern
with a different failure mode: losing it means you can no longer push
updates, but members can still read what is already there. Back it up
too if you intend to keep publishing. See
[`multi-machine.md`](multi-machine.md) for where the signing key lives
and how to move it between machines.

## Where to go next

- **Second machine basics.** For connecting two peers and verifying
  replication, see [`multi-machine.md`](multi-machine.md).

- **Mirroring encrypted repos.** A mirror stores the encrypted blocks
  without holding any member key. See [`MIRROR_SETUP.md`](MIRROR_SETUP.md).

- **Encrypting plain files.** The same group-key mechanism applies to
  individual trees; see the "Encrypt a tree with a group key" recipe
  in [`COOKBOOK.md`](COOKBOOK.md).
