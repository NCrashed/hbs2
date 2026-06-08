# PEP-13: post-quantum-encryption (hybrid PQC encryption scheme)

Status: draft
Author: NCrashed
Depends on: PEP-02 (ACB), current GroupKeySymm implementation
Related: PEP-15 (HD keys from a mnemonic), PEP-14 (encrypted storage)

## Motivation

The confidentiality of an encrypted repository rests entirely on
elliptic-curve asymmetric crypto (X25519 sealed box when wrapping the
group secret for each recipient; Ed25519 when signing refs and
identities). Both are broken by Shor's algorithm on a
cryptographically relevant quantum computer (CRQC).

The symmetric layer (XSalsa20-Poly1305 on a 256-bit key, HKDF-SHA-512,
Merkle hashes) is quantum-sufficient: Grover's algorithm gives only a
quadratic speedup, turning 256 bits into ~128 bits of strength, which
is out of reach. The symmetric layer does not need to change.

The main threat is not "someday it will be broken" but
harvest-now-decrypt-later. By its nature hbs2:

  - replicates encrypted blocks widely and permanently
    (content-addressed storage, blocks spread across peers);
  - stores the GroupKey with the wrapped secret and the recipients'
    public keys right next to them.

This means a private repository that goes onto the network today can
be archived by an adversary and decrypted on the day a CRQC arrives.
The threat to confidentiality is in effect from the moment of first
publication, not from the moment a quantum computer appears. Given the
NIST timelines (deprecation of ECC recommended by 2030, disallowed by
2035), this is worth tackling early.

## Goals

  - Protect repository confidentiality against
    harvest-now-decrypt-later.
  - Do not break existing encrypted repositories and old keys.
  - Retain classical strength in case a classical break is found in
    the new PQC scheme (hence a hybrid, not a replacement).

## Non-goals

  - Replacing symmetric encryption (it is quantum-sufficient).
  - An urgent signature migration. Forging a signature requires
    breaking a key while it is still in use; back-dating archival
    signatures is pointless. Signatures migrate in a separate phase.
  - Support for arbitrary PQC algorithms. We pin the NIST standards.

## Choice of primitives

  - KEM (wrapping the group secret): X25519 + ML-KEM-768 (FIPS 203,
    formerly Kyber), hybrid. The shared secrets are combined in a KDF;
    confidentiality holds as long as at least one of the two is
    intact.
  - Signatures (phase 2): Ed25519 + ML-DSA (FIPS 204, Dilithium). For
    long-lived root identities, optionally SLH-DSA (FIPS 205,
    SPHINCS+) as a more conservative variant (hash-based only).
  - Symmetric: unchanged (XSalsa20-Poly1305, HKDF-SHA-512).

## Design

### New crypto scheme at the type level

The code already has scheme indirection (`'HBS2Basic` in
`PubKey 'Encrypt`, `PubKey 'Sign`, etc.). Introduce a parallel scheme
without touching the old one:

```
data CryptoScheme = HBS2Basic | HBS2Hybrid

type instance PubKey  'Encrypt 'HBS2Hybrid = HybridEncPK
type instance PrivKey 'Encrypt 'HBS2Hybrid = HybridEncSK

data HybridEncPK = HybridEncPK
  { hePkX25519 :: Encrypt.PublicKey   -- 32 bytes, as before
  , hePkMlKem  :: MlKemPublicKey       -- ~1184 bytes
  }
```

In phase 1 signatures stay `HBS2Basic`/Ed25519. The hybrid touches
encryption only.

### Hybrid wrapping of the group secret

Today the secret is wrapped like this:

```
Recipients s = HashMap (PubKey 'Encrypt s) (EncryptedBox GroupSecret)
```

Extend EncryptedBox with a double-wrapped variant. The idea: obtain a
shared secret independently from two KEMs, combine them through a KDF,
and encrypt the group secret itself with the resulting key. That way
confidentiality holds as long as at least one KEM is intact.

```
data HybridWrap = HybridWrap
  { hwX25519Box :: ByteString    -- sealed box over X25519, as before
  , hwMlKemCt   :: ByteString    -- ML-KEM ciphertext (~1088 bytes)
  , hwSecretBox :: ByteString    -- secretbox(group-secret) under kek
  , hwNonce     :: ByteString
  }
```

Key-encryption-key derivation:

```
ss_x   = X25519(eph_sk, recipient_pk)            -- classical DH
ss_pq  = ML-KEM.decap(ct, recipient_mlkem_sk)    -- PQ encapsulation
kek    = HKDF-SHA-512( ss_x || ss_pq )           -- both secrets together
gsec   = secretbox_open(kek, nonce, hwSecretBox)
```

On encryption: generate an ephemeral X25519 (as in the sealed box) and
ML-KEM.encap to the recipient's public key, combine both shared
secrets in the same HKDF, and encrypt the group secret with the
resulting kek.

### New MTreeEncryption variant

`MTreeEncryption` is already versioned (`EncryptGroupNaClSymm1/2`).
Add a variant for the hybrid so the reader knows how to decrypt the
secret wrapper (the bulk block layer itself does not change):

```
data MTreeEncryption
  = NullEncryption
  | CryptAccessKeyNaClAsymm (Hash HbSync)
  | EncryptGroupNaClSymm1 (Hash HbSync) ByteString
  | EncryptGroupNaClSymm2 EncryptGroupNaClSymmOpts (Hash HbSync) ByteString
  | EncryptGroupHybridSymm (Hash HbSync) ByteString   -- new
```

Bulk block encryption (XSalsa20-Poly1305 + index nonces) stays the
same: only the way the group secret is delivered to the recipient
changes.

### GroupKey versioning

`GroupKeySymmFancy` already carries `groupKeyIdScheme` and
`groupKeyTimestamp`. Use `groupKeyIdScheme` to mark the hybrid
wrapper, and bump the serialisation format version while keeping
backward-compatible reading of old keys.

## Compatibility and migration

  - Old repositories (`EncryptGroupNaClSymm1/2`) are read as before,
    indefinitely.
  - New private repositories are created as hybrid when every
    recipient has an ML-KEM key; otherwise fall back to the old scheme
    with a warning.
  - A recipient with a hybrid key can be added to both an old and a
    new GroupKey (the X25519 part is compatible).
  - Re-encrypting existing repositories to the hybrid does not undo
    harvest-now-decrypt-later for blocks that already leaked, but it
    protects everything published after the migration. This must be
    stated plainly in the docs.

## Library and dependencies

saltine/libsodium do not contain PQC. Options:

  - An FFI binding to liboqs (which has ML-KEM, ML-DSA, SLH-DSA).
  - botan (has ML-KEM/ML-DSA), but pulls in a large dependency.

Preferred: a thin in-house FFI binding to liboqs covering only the
needed algorithms, in the spirit of the devendoring policy (fork
under NCrashed, publish to Hackage where possible). See the
devendoring plan.

## Sizes (rough)

| Object              | Classical (X25519) | PQC                  |
|---------------------|--------------------|----------------------|
| Public enc key      | 32 bytes           | ML-KEM-768 ~1184     |
| KEM ciphertext      | 32 (eph pk)        | ML-KEM-768 ~1088     |
| Signature           | Ed25519 64         | ML-DSA ~2420         |
| Signature (SLH-DSA) | -                  | tens of KB           |

The growth per recipient secret wrapper is tolerable, but the
recipient HashMap in a GroupKey grows linearly with the number of
readers. On refs (frequent signatures) the signature growth is more
noticeable, which is why the signature phase is separate.

## Rollout plan

  - Phase 0: FFI binding to liboqs (ML-KEM), round-trip tests.
  - Phase 1: the `HBS2Hybrid` scheme for encryption,
    `EncryptGroupHybridSymm`, hybrid wrapping of the group secret,
    generation of hybrid keys in the keyring, reading of old formats.
  - Phase 2 (later): hybrid signatures (Ed25519 + ML-DSA), optionally
    SLH-DSA for root identities.

## Open questions

  - ML-KEM level: 768 (default) or 1024 for long-lived repositories?
  - Where to store the ML-KEM key relative to the existing keyring: a
    new KeyringEntry variant or an extension of the current one?
  - Is a separate group-key rotation mechanism needed after migration,
    to force re-encryption to the hybrid?
  - Should domain separation be baked into the HKDF from the start (a
    scheme label in `info`) to keep classical and hybrid derivations
    apart?
