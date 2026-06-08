# PEP-15: hd-mnemonic-keys

Status: draft
Author: NCrashed
Depends on: existing Crypto/Bip39.hs and derivedKey (HKDF), keyman
Related: PEP-14 (encrypted storage), PEP-13 (PQC encryption)

## Motivation

Keys in hbs2 are generated randomly every time
(`Sign.newKeypair`, `Encrypt.newKeypair` in
`hbs2-core/lib/HBS2/Net/Auth/Credentials.hs`). The consequence: every
new key must be backed up individually right after it is created, or
it is lost forever. For encrypted repositories this hurts the most:
losing a recipient secret means permanently losing access to
everything that was encrypted to it (see the backup section in
`docs/encrypted-repos.md`), with no recovery.

The problem grows worse:

  - PEP-13 adds large PQC keys; "back up every key" scales badly.
  - The number of identities and keys grows over time (peer,
    repository signing, per-member encryption keys).

Bitcoin solved exactly this: a BIP-39 mnemonic as a single master
seed, and BIP-32/SLIP-0010 as deterministic derivation of child keys.
Write down 24 words once on paper or metal, and the whole key tree is
recoverable from them. This PEP proposes the same model for hbs2.

## Already in the code (this is not greenfield)

  - `hbs2-git3/lib/Crypto/Bip39.hs`: `toMnemonic`, `fromMnemonic`,
    `mnemonicToSeed` (PBKDF2-SHA512, 2048 iterations). BIP-39 is
    already implemented, just not yet used for key generation.
  - `hbs2-core/lib/HBS2/Net/Auth/Schema.hs`: `derivedKey` derives a
    deterministic Ed25519 signing key from a `Word64` seed and a
    parent secret via HKDF-SHA-512. This already runs in production
    for repository reflog keys (see `getRepoRefLogCredentials` in
    `hbs2-git3/.../State.hs`, the manifest `seed` field).

So both the mnemonic and HKDF-based Ed25519 derivation already exist
and are battle-tested. What is missing: a single master root (the
mnemonic as the source), derivation for encryption (X25519) and PQC
keys, a path scheme, and keyman integration.

## Goals

  - A single master seed (a BIP-39 mnemonic) from which all of the
    user's keys are deterministically derived.
  - Back up once (the phrase), with no need to back up each key.
  - Support deriving both classical (Ed25519, X25519) and hybrid PQC
    keys (PEP-13) from the same phrase.
  - Additive: existing random keys keep working.

## Non-goals

  - Watch-only / non-hardened derivation of public keys without the
    secret. We do not need it; use hardened-only (as SLIP-0010 does
    for Ed25519, which also removes the BIP-32 edge cases specific to
    Edwards curves).
  - Byte-for-byte compatibility with BIP-32/SLIP-0010 for
    cross-wallet portability. What matters is determinism within
    hbs2, not import into third-party wallets. Use our own HKDF domain
    with explicit domain separation.

## Design

### Root

```
entropy (128/256 bits, CSPRNG)
  -> BIP-39 mnemonic (12/24 words)         [Crypto.Bip39.toMnemonic]
  -> seed = PBKDF2-SHA512(mnemonic, "...") [Crypto.Bip39.mnemonicToSeed]
  -> master = HKDF-extract(seed)
```

An optional passphrase (the BIP-39 "25th word") is supported by
`mnemonicToSeed` and provides plausible deniability / a hidden vault.

### Derivation tree (hardened-only)

A path analogous to BIP-44, adapted to our key types:

```
m / purpose' / account' / key-type' / index'
```

  - `purpose'` fixed for hbs2.
  - `account'` separates independent identities (personal, work).
  - `key-type'`: sign | encrypt | reflog | pq-kem | pq-sign.
  - `index'` numbers keys within a type.

Each step: `child = HKDF-SHA-512(parent, info = label || index)` with
domain separation in `info` (this, incidentally, answers PEP-13's
open question about domain separation). Reuse the same HKDF already in
`derivedKey`.

### Generating a key from a derived seed

From a leaf 32-byte secret, produce a concrete keypair
deterministically:

  - Ed25519 (sign): `crypto_sign_seed_keypair(seed)`.
  - X25519 (encrypt): `crypto_box_seed_keypair(seed)` (an X25519
    secret is essentially a clamp of 32 bytes; the public part is
    scalar multiplication against the base point).
  - PQC (PEP-13): ML-KEM and ML-DSA per FIPS 203/204 have
    deterministic key generation from a seed; liboqs provides
    derandomized keypair variants. So ONE mnemonic recovers both
    classical and hybrid keys. Pin the exact liboqs signature during
    implementation.

This is critical: it couples PEP-15 and PEP-13. For a hybrid key to
be recoverable from the phrase, both of its halves (X25519 and ML-KEM)
are derived from adjacent tree nodes under the same `account'`.

### Dependency: seeded keygen

saltine 0.2.2.0 does not expose `crypto_sign_seed_keypair` /
`crypto_box_seed_keypair` (confirmed). One of the following is needed:

  - extend our saltine fork and publish it (in the spirit of the
    devendoring plan), or
  - a thin in-house libsodium FFI covering just seeded keygen.

A fallback without a seeded API: derive Ed25519 via the existing
`derivedKey`/HKDF and convert to X25519
(`crypto_sign_ed25519_sk_to_curve25519`). But direct seeded keygen is
cleaner; the FFI is preferred.

### keyman integration

  - keyman stores (encrypted, per PEP-14) the master seed/mnemonic
    instead of a pile of `.key` files, and derives keys on demand.
  - `state.db` holds a public index: pubkey -> derivation path
    (plus type, weight). Recipient resolution and secret selection
    work as before; for a private operation keyman derives the key
    from the seed on the fly (after the agent is unlocked, per
    PEP-14).
  - Commands: `hbs2-keyman seed:new` (generate a mnemonic),
    `seed:import` (enter an existing one), `derive` (show the
    pubkey/path for a type + index), `seed:show` (print the phrase
    behind a confirmation, for backup).

## Compatibility and migration

  - Existing random keys remain explicit keyring entries and keep
    working indefinitely; HD is additive.
  - New identities are recommended to be created from the seed.
  - Importing an old random key into the HD tree is impossible (it is
    not derivable from the phrase); it simply keeps living as an
    explicit entry, which is fine for the transition period.

## Caveats

  - The master seed is a single point of compromise: knowing the
    phrase means knowing every derived key. So the seed must be stored
    encrypted (PEP-14) and backed up offline (paper/metal), as in
    Bitcoin.
  - A derived key cannot be "revoked by forgetting": it can be
    reproduced from the seed at any time. Rotation means moving to a
    new `index'`/`account'` and re-encrypting content (this echoes the
    lack of forward secrecy in group keys and the rotation discussion
    in PEP-13).
  - Hardened-only means no derivation of public keys without the
    secret. For hbs2 this is acceptable (no watch-only scenario is
    needed).

## Open questions

  - Finalise the path scheme (`purpose'`, the set of `key-type'`).
  - Whether to fold the existing reflog derivation (`derivedKey` from
    a `Word64` seed) under this same root, for a single source of
    truth, or to keep it as a separate branch.
  - One shared master seed per user, or one seed per account.
  - Default mnemonic length (24 words = 256 bits of entropy is
    recommended for long-lived identities).

## Rollout plan

  - Phase 0: seeded keygen (FFI/saltine fork), round-trip tests for
    Ed25519/X25519 from a seed.
  - Phase 1: the root from a mnemonic, the HKDF tree with domain
    separation, the `seed:*` and `derive` commands, the public path
    index in state.db.
  - Phase 2: integration with the keyman agent (PEP-14): the master
    seed encrypted, keys derived on the fly.
  - Phase 3: derivation of hybrid PQC keys (PEP-13) from the same
    mnemonic.
