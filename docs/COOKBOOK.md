# Cookbook

Short, working recipes for common tasks beyond the git path covered in
[`QUICKSTART.md`](../QUICKSTART.md). Each recipe assumes you have a
running `hbs2-peer` and the `hbs2-cli`/`hbs2-peer` binaries on your
path. See [`INSTALL.md`](../INSTALL.md) for setup.

All recipes were verified against the v1 binaries shipped from this
repository.

## Share a file between two hosts

**Host A** stores the file as a Merkle tree with a small piece of
metadata, then prints the resulting tree hash:

```
hbs2-cli "(hbs2:tree:metadata:file [kw note \"some description\"] \"./yehatrmx.mp3\")"
```

The output is a single base58-encoded hash. Pass that hash to
**Host B** out of band (chat, email, etc.).

**Host B** asks its peer to pull the tree from the network, waits for
the download to finish, then writes the bytes to a file:

```
hbs2-peer fetch -p <HASH>
hbs2-cli "(hbs2:tree:read:stdout \"<HASH>\")" > yehatrmx.mp3
```

`-p` shows download progress and blocks until the tree is fully
fetched. Both hosts must have peers that can reach each other,
directly or through PEX.

To inspect the stored metadata (MIME type, original filename, the
`note` you set):

```
hbs2-cli "(hbs2:tree:metadata:get \"<HASH>\")"
```

## Subscribe to someone else's reference

You can follow another peer's published `lwwref` and have your local
peer keep a copy of whatever they push to it. Useful for mirroring a
git repository, a static site, or any other content that updates over
time.

```
hbs2-peer poll add <REF> lwwref 31
```

The third argument is the polling interval in minutes. Polling matters
because gossip-style updates are best-effort: if your peer was offline
when the publisher pushed, polling will catch the update on the next
tick. The example uses 31 minutes; pick a value that matches how
quickly you need to see updates.

To list current subscriptions:

```
hbs2-peer poll list
```

## Reach content over HTTP

The peer exposes a small HTTP gateway. Find its port from `poke`:

```
hbs2-peer poke | grep http-port
```

A polled `lwwref` is then reachable at:

```
http://localhost:<port>/ref/<REF>
```

This is the same mechanism used to host static sites and to inspect
trees from a browser. Trees stored with `hbs2:tree:metadata:file`
serve with the correct `Content-Type` and `Content-Disposition`
because those are taken from the metadata.

## Delete content from local storage

Single block:

```
hbs2-cli "(hbs2:peer:storage:block:del \"<BLOCK_HASH>\")"
```

Entire Merkle tree (walks the tree and removes every block):

```
hbs2-cli "(hbs2:tree:delete \"<TREE_HASH>\")"
```

These delete from your local peer's storage only. Other peers that
have already replicated the content still hold their copies. If the
tree is referenced by a polled `lwwref`, your peer will fetch it again
on the next poll, so unsubscribe first if you want the deletion to
stick.

## Encrypt a tree with a group key

A group key is a list of public keys whose holders can read the tree.
Create one with `hbs2-cli`'s keyring tooling (see the keyring entries
in `--help` output); the result is a hash you can reuse across many
trees.

Store an encrypted tree by passing `gk <GROUP_KEY_HASH>` in the
keyword list:

```
hbs2-cli "(hbs2:tree:metadata:file [kw note \"secret\" gk <GROUP_KEY_HASH>] \"./file.mp3\")"
```

The segments are encrypted under the group key; the metadata
envelope (MIME type, filename, the `note` field) is not. This is the
property that lets untrusted peers store and forward the tree without
being able to read it.

To check who is in a given group key:

```
hbs2-cli "(hbs2:groupkey:dump \"<GROUP_KEY_HASH>\")"
```

The group-key scheme has no perfect forward secrecy: a key holder who
later leaves the group can still decrypt every tree that was ever
encrypted to a group key they had access to. Rotate group keys and
re-encrypt content when members change if this matters to you.

## Storing small content directly (no metadata tree)

For data that fits in a single block (262144 bytes), you can skip the
Merkle layer:

```
echo "some content" | hbs2-cli hbs2:peer:storage:block:put
```

The output is the block hash.

By default `block:put` only writes to your local peer's storage; other
peers do not learn the block exists until something emits a
`BlockAnnounce`. Pass `--announce` to broadcast one right after
storing, so a "put on A, get on B" flow works without a separate
`hbs2-peer announce`:

```
hbs2-cli "(hbs2:peer:storage:block:put --announce \"some content\")"
```

It is off by default so encrypted-refchan and group-key workflows that
gate publication are not surprised. Higher-level flows (git push, sync)
do not need it: they emit reflog transactions that announce internally.

Read it back with:

```
hbs2-cli "(hbs2:peer:storage:block:get \"<HASH>\")"
```

This is the right tool for short signed messages, references, or
ad-hoc testing. For real files, use `hbs2:tree:metadata:file` so the
data automatically splits into blocks and carries MIME information.
