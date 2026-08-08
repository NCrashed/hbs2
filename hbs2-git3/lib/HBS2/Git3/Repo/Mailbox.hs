-- | The manifest clauses that say where a repository takes contributions
-- (PEP-18 "Addressing and discovery").
--
-- > (mailbox <mailbox-sign-key-b58> hub [<tier>])
-- > (mailbox-sigil <mailbox-sign-key-b58> <hashref>)
--
-- WHY THIS IS HERE AND NOT IN THE FORGE. It is the forge that invented these
-- clauses and the forge that reads them ("HBS2.Hub.Manifest"), but the manifest
-- is THIS package's file: its shape is set by @repo:init@ and the only function
-- that rewrites it is 'updateRepoHead', next door. A writer in the hub package
-- would mean the hub depending on the git remote helper, which is a dependency
-- that would drag a peer, a storage backend and a database into a library whose
-- point is that it folds a log with none of them.
--
-- So there are two spellings of a three-token clause, in two packages, and that
-- is the thing to be careful about. The care taken is 'writesBackAsMailbox'
-- below: the manifest is rendered to the exact bytes 'updateRepoHead' would
-- store, parsed back, and the clause looked for with the SAME pattern the hub's
-- reader matches. A drift in either spelling is a refusal before anything is
-- written, rather than a repository that says nothing about where to write to
-- it and no way to tell why.
--
-- WHAT WAS THERE BEFORE: nothing. The clauses were read by five verbs of
-- @hbs2-hub@ and emitted by a function called only by its own test, and this
-- package had no verb that edited a manifest at all. A maintainer could not
-- declare an ingress mailbox, so a contributor could not discover one, so the
-- whole discovery half of PEP-18 was a read side with no write side.
module HBS2.Git3.Repo.Mailbox
  ( hubRole
  , mailboxClause
  , mailboxSigilClause
  , setMailbox
  , dropMailbox
  , addMailboxSigil
  , dropMailboxSigil
  , declaredMailboxes
  , declaredSigils
  , writesBackAsMailbox
  , writesBackAsSigil
  , renderManifest
  ) where

import HBS2.Git3.Prelude

import Data.Config.Suckless.Script

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Char (isAlpha,isAlphaNum)
import Prettyprinter (vcat)

-- | The role tag that marks the forge ingress. PEP-18 leaves room for others;
-- this package writes only this one.
hubRole :: Text
hubRole = "hub"

-- | Emit @(mailbox <key> hub [<tier>])@, or say why it cannot be written.
--
-- THE EMPTY STRING IS NOT A VALUE THIS FORMAT HAS, which is the one way this
-- can fail. A string literal is a run of characters, so an empty one produces
-- no token at all and the clause loses a field: @(mailbox KEY hub "")@ reads
-- back as a mailbox with no tier, which is a DIFFERENT mailbox -- the untiered
-- one is the inbox a stranger is told to use. There is no spelling to escape
-- into, so this refuses and names the field.
mailboxClause :: GitRepoKey -> Maybe Text -> Either Text (Syntax C)
mailboxClause k tier
  | any Text.null tier = Left "the tier is empty, and this format has no empty string"
  | otherwise =
      Right $ mkForm "mailbox" $
        [ mkSym (show (pretty (AsBase58 k))), atom hubRole ]
          <> maybe [] (\t -> [atom t]) tier

-- | Emit @(mailbox-sigil <mailbox-key> <hashref>)@.
--
-- A mailbox is addressed by a SIGN key and sealing a letter to it needs the
-- matching ENCRYPTION key, which lives in a sigil. Nothing resolves one from
-- the other, so the hash goes in the manifest and a fresh clone can compose
-- with no live lookup.
mailboxSigilClause :: GitRepoKey -> HashRef -> Syntax C
mailboxSigilClause k h =
  mkForm "mailbox-sigil" [ mkSym (show (pretty (AsBase58 k)))
                         , mkSym (show (pretty h)) ]

-- | A symbol where one reads back as itself, a string otherwise.
--
-- The role and the tier are written bare, matching PEP-18's examples, and the
-- leading-letter rule is what keeps that honest: a value starting with a digit
-- would lex back as a number and one with a space in it as two atoms. The
-- string branch escapes, because a tier reaches this from wherever the owner
-- copied it and an unescaped quote would close the string early and leave the
-- rest of the manifest reading as whatever it happened to look like.
atom :: Text -> Syntax C
atom t = case Text.uncons t of
  Just (c,rest) | isAlpha c, Text.all plain rest -> mkSym (Text.unpack t)
  _                                              -> mkStr t
  where
    plain ch = isAlphaNum ch || ch `elem` ("-_.:/" :: String)

-- | The manifest as 'updateRepoHead' will store it.
--
-- Exported so the round-trip guards below check the BYTES and not the value:
-- what the hub eventually parses is this text, read out of a tree, and an
-- escaping bug lives exactly in the gap between the two.
renderManifest :: [Syntax C] -> String
renderManifest = show . vcat . fmap pretty

-- | Replace this mailbox's clause, or add it.
--
-- Keyed on the mailbox key, so re-running with a different tier edits rather
-- than accumulating: two @(mailbox K hub ...)@ clauses for one key would make
-- 'mailboxByTier' answer from whichever came first, which is not an answer.
--
-- Order is preserved and the clause is appended when it is new, so a manifest
-- a person has read stays recognizable.
setMailbox :: GitRepoKey -> Maybe Text -> [Syntax C] -> Either Text [Syntax C]
setMailbox k tier mf = do
  c <- mailboxClause k tier
  let without = [ x | x <- mf, not (mailboxFor k x) ]
  pure $ if any (mailboxFor k) mf
           then [ if mailboxFor k x then c else x | x <- mf ]
           else without <> [c]

-- | Take this mailbox out, and its sigils with it.
--
-- Both, because a sigil clause names the mailbox it belongs to and one left
-- behind describes an inbox the repository no longer declares. Nobody would
-- read it, and it would still be a published encryption key with no stated
-- purpose.
dropMailbox :: GitRepoKey -> [Syntax C] -> [Syntax C]
dropMailbox k mf = [ x | x <- mf, not (mailboxFor k x), not (sigilFor k x) ]

-- | Add a sigil for this mailbox, unless the same pair is already declared.
--
-- Added and not replaced: one mailbox read by several maintainers gets several
-- sigils, since a sigil binds exactly one encryption key.
addMailboxSigil :: GitRepoKey -> HashRef -> [Syntax C] -> [Syntax C]
addMailboxSigil k h mf
  | any same mf = mf
  | otherwise   = mf <> [mailboxSigilClause k h]
  where
    same x = sigilFor k x && renderManifest [x] == renderManifest [mailboxSigilClause k h]

-- | Take one sigil out, leaving the mailbox and any others.
dropMailboxSigil :: GitRepoKey -> HashRef -> [Syntax C] -> [Syntax C]
dropMailboxSigil k h mf = [ x | x <- mf, not (gone x) ]
  where
    gone x = sigilFor k x && renderManifest [x] == renderManifest [mailboxSigilClause k h]

-- | Is this clause a mailbox declaration for this key?
--
-- THE SAME PATTERN THE HUB'S READER MATCHES, and deliberately no stricter: it
-- accepts any role, so replacing a clause replaces the one that is there rather
-- than leaving a stale @(mailbox K relay)@ beside a new @(mailbox K hub)@.
mailboxFor :: GitRepoKey -> Syntax C -> Bool
mailboxFor k = \case
  ListVal (SymbolVal "mailbox" : SignPubKeyLike k' : StringLike _ : _) -> k' == k
  _ -> False

sigilFor :: GitRepoKey -> Syntax C -> Bool
sigilFor k = \case
  ListVal [SymbolVal "mailbox-sigil", SignPubKeyLike k', HashLike _] -> k' == k
  _ -> False

-- | Every mailbox this manifest declares, as (key, role, tier).
declaredMailboxes :: [Syntax C] -> [(GitRepoKey, Text, Maybe Text)]
declaredMailboxes mf = do
  x <- mf
  case x of
    ListVal [SymbolVal "mailbox", SignPubKeyLike k, StringLike role] ->
      [(k, Text.pack role, Nothing)]
    ListVal [SymbolVal "mailbox", SignPubKeyLike k, StringLike role, StringLike tier] ->
      [(k, Text.pack role, Just (Text.pack tier))]
    _ -> []

-- | Every sigil this manifest declares, as (mailbox key, sigil hash).
declaredSigils :: [Syntax C] -> [(GitRepoKey, HashRef)]
declaredSigils mf = do
  x <- mf
  case x of
    ListVal [SymbolVal "mailbox-sigil", SignPubKeyLike k, HashLike h] -> [(k, h)]
    _ -> []

-- | Does the manifest, once written and read back, declare this mailbox?
--
-- THE GUARD THIS MODULE IS FOR. The clause is spelled here and read in another
-- package, and nothing links the two but agreement; what CAN be checked, and is
-- checked before anything is stored, is that the text this package will write
-- parses into the shape that package looks for. Rendering and re-parsing is not
-- ceremony: what the hub reads is bytes out of a tree, so an escaping fault
-- lives precisely in the step this repeats.
writesBackAsMailbox :: GitRepoKey -> Maybe Text -> [Syntax C] -> Bool
writesBackAsMailbox k tier mf =
  case parseTop (renderManifest mf) of
    Left _ -> False
    Right back ->
      [(k, hubRole, tier)] == [ m | m@(k',_,_) <- declaredMailboxes back, k' == k ]

-- | And the same for a sigil.
writesBackAsSigil :: GitRepoKey -> HashRef -> [Syntax C] -> Bool
writesBackAsSigil k h mf =
  case parseTop (renderManifest mf) of
    Left _      -> False
    Right back  -> isJust (lookup' (declaredSigils back))
  where
    lookup' xs = case [ () | (k',h') <- xs, k' == k, h' == h ] of
                   (_:_) -> Just ()
                   []    -> Nothing
