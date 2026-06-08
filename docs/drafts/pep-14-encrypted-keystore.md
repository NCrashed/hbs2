# PEP-14: encrypted-keystore-and-keyman-agent

Status: draft
Author: NCrashed
Depends on: current hbs2-keyman implementation, GroupKeySymm (secretbox)
Related: PEP-15 (HD keys from a mnemonic), PEP-13 (PQC encryption)

## Motivation

Today all private key material sits on disk in the clear. A keyring
(`PeerCredentials`) is serialised to base58-CBOR and written to a
`.key` file with no encryption at all
(`hbs2-core/lib/HBS2/Net/Auth/Credentials.hs`, printed through
`AsCredFile`). hbs2-keyman is not a daemon: it scans the directory
`~/.hbs2-keyman/keys` with the mask `**/*.key`, records a
pubkey-to-path mapping in the SQLite `state.db`, and reads the
secrets from the file on every request
(`hbs2-keyman-direct-lib/HBS2/KeyMan/Keys/Direct.hs`,
`runKeymanClientRO`).

This means:

  - Any process running as the user (an infostealer, a trojaned
    dependency, a compromised CI agent) walks off with every key in
    one `cat`.
  - Backing up the key directory (rsync, cloud sync, a disk
    snapshot) carries the secrets out in plaintext.
  - Leaking a single keyring file compromises both the signing key
    and every encryption key inside it.

Neither GPG nor SSH works this way: private keys are encrypted with
a passphrase by default, and the decrypted form lives only in the
memory of an agent (`ssh-agent`, `gpg-agent`). hbs2 should get to the
same place.

The problem compounds with the other PEPs. PEP-13 introduces large
PQC keys (ML-KEM secret ~2.4 KB, ML-DSA ~4 KB), and PEP-15
introduces a master seed from which everything is derived, that is, a
single extremely high value secret. Neither can be allowed to live on
disk in the clear.

## Threat model

What this protects:

  - **At-rest theft.** An infostealer or malware running as the user,
    a stolen disk or laptop, a leaked backup. Without the passphrase
    the file is useless.
  - **Accidental leakage through sync.** An encrypted keyring is
    safer to move around and back up.

What this does NOT protect (and must be stated plainly in the docs,
just like ssh-agent):

  - **A compromised process while the agent is unlocked.** As long as
    the agent is unlocked, an attacker running as the user can ask it
    to perform an operation (or scrape the key from process memory).
    The agent shrinks the window and removes secrets from disk, but
    it does not make the machine trusted.
  - **A keylogger on passphrase entry.** Out of scope; mitigated by
    hardware tokens (a separate PEP).

## Goals

  - Keep all private material on disk only in encrypted form.
  - Hold decrypted keys in the memory of a long-lived agent, unlocked
    once per session.
  - Do not break existing repositories, keys, or user workflows;
    migrate smoothly.
  - Reuse the primitives already in the codebase (libsodium
    secretbox, HKDF) instead of introducing a new crypto stack just
    for storage.

## Non-goals

  - Hardware tokens / Secret Service / macOS Keychain (separate PEPs;
    this design leaves an extension point for them).
  - Changing the key formats themselves (that is PEP-13/PEP-15).
  - Protecting against a compromised host with the agent unlocked.

## Design

### Encrypted keyring format

Introduce a versioned encrypted container that wraps the existing
`PeerCredentials` serialisation without touching the old format:

```
data EncryptedKeystore = EncryptedKeystore
  { ekVersion :: Word8          -- format version
  , ekKdf     :: KdfParams      -- Argon2id: salt, opslimit, memlimit
  , ekNonce   :: ByteString     -- secretbox nonce
  , ekCipher  :: ByteString     -- secretbox(serialised keyring)
  }
```

  - KDF: Argon2id (libsodium `crypto_pwhash`) derives a 32-byte key
    from the passphrase and salt. The cost parameters are stored in
    the container so they can be raised over time.
  - Symmetric layer: XSalsa20-Poly1305 (libsodium secretbox), already
    used in `GroupKeySymm` (`Crypto.Saltine.Core.SecretBox`); no new
    primitive needed.
  - The file is recognised by a magic signature/header comment (as
    today's `.key` already has), so the parser can tell the plaintext
    and encrypted variants apart.

Dependency note: secretbox is present in saltine; Argon2id
(`crypto_pwhash`) may not be exposed in the current saltine version,
so it will require extending the saltine fork or a thin libsodium
FFI. Confirm during implementation (see also the seeded-keygen gap in
PEP-15).

### keyman as an agent

Today every keyman call opens a fresh DB connection and reads files
from disk (`newKeymanClientEnv`). Turn keyman into a long-lived
agent:

  - On start the agent reads the encrypted containers; once unlocked
    (passphrase entry) it keeps the decrypted secrets only in memory
    (in locked pages where possible, `sodium_mlock`).
  - Tools (hbs2-peer, git hbs2, find-secret) talk to the agent over
    the same RPC/unix-socket boundary that already exists
    (`runKeymanClient*` becomes a client of the agent rather than a
    reader of files).
  - The socket is mode 0600, owned by the user; the trust model rests
    on that (as with ssh-agent).
  - Locking policy: idle TTL, explicit `hbs2-keyman lock`, lock on
    logout/sleep (platform dependent).

`state.db` stays as a public index (pubkey, type, path/reference,
weight). Crucially, the public-key index must not require unlocking,
otherwise recipient resolution and key selection break. The secret is
needed only at the moment of an operation.

### Unlocking in headless/CI

  - Passphrase from a managed environment secret or from a file with
    strict permissions (an explicitly flagged mode, with a warning).
  - A separate "service keystore" holding a narrow set of keys for
    daemons (for example the hbs2-peer identity), so the whole user
    keyring is not left unlocked.
  - This intersects with the NixOS module (`keyFile`): the module
    should be able to supply an encrypted keystore plus a passphrase
    from systemd-credentials.

## Compatibility and migration

  - Old plaintext `.key` files keep being read indefinitely
    (otherwise we break everyone).
  - A command `hbs2-keyman encrypt` encrypts an existing keyring with
    a passphrase and switches keyman to the encrypted variant;
    `decrypt` for the reverse export.
  - Mixed mode during the transition: some keys plaintext, some
    encrypted.
  - A `require-encrypted` policy in the keyman config: when enabled,
    keyman refuses to index plaintext `.key` files and warns about
    them (a path toward "encrypted by default" in a future major).

## Open questions

  - One shared container per keyring, or per-key containers? (Per-key
    is nicer for differing policies/TTLs, but more files.)
  - Whether to keep the PEP-15 master seed in the same container or in
    a separate one with a stricter unlock policy.
  - Whether to add a protocol-level "sign/decrypt without releasing
    the key" for all operations (like ssh-agent), or whether
    "hand the secret to a trusted local client" is enough. The former
    is stricter; the latter is easier to reach on the current code.
  - Memory locking (`mlock`) and behaviour in containers with limits.
  - An extension point for hardware tokens and OS keychains as
    alternative agent backends.

## Rollout plan

  - Phase 0: the `EncryptedKeystore` format, KDF/secretbox, round-trip
    tests; `encrypt`/`decrypt` commands.
  - Phase 1: the keyman agent with unlock and in-memory secrets;
    `runKeymanClient*` over the agent socket; public index without
    unlocking.
  - Phase 2: policies (`require-encrypted`, TTL, lock), headless/CI
    unlock, NixOS module integration.
  - Phase 3: extension points for tokens/OS keychains.
