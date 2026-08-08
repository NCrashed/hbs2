-- | Repo-manifest wiring for the forge (PEP-18 "Addressing and discovery").
--
-- The git3 repo manifest is a list of suckless S-expressions. The hub adds
-- two clauses to it:
--
-- > (mailbox <mailbox-sign-key-b58> hub [<tier>])
-- > (mailbox-sigil <mailbox-sign-key-b58> <hashref>)
--
-- A mailbox is addressed by a sign key, but encrypting to its maintainer
-- needs the matching encryption key, which lives in a sigil. There is no
-- resolve-sigil-by-sign-key service, so the sigil hash has to be
-- discoverable, and the manifest is where it belongs: a fresh clone can then
-- submit with no live lookup.
--
-- The sigil clause names the mailbox it belongs to, so it stays unambiguous
-- when a repo declares more than one inbox (trust tiers, PEP-21), and a
-- single mailbox read by several maintainers simply gets several sigil
-- clauses (a SigilData binds exactly one encryption key).
--
-- Reading follows the git3 convention: pattern-match the clause shape and
-- ignore anything unrecognized.
module HBS2.Hub.Manifest
  ( HubMailbox(..)
  , MailboxSigil(..)
  , hubRole
  , publicTier
  , mailboxes
  , hubMailboxes
  , mailboxByTier
  , sigils
  , sigilsFor
  , mailboxClause
  , mailboxSigilClause
  ) where

import HBS2.Hub.Types (HubKey)
import HBS2.Hub.Letter (sexpStr)

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..),pattern HashLike)
import HBS2.Net.Auth.Credentials (pattern SignPubKeyLike)
import HBS2.Prelude.Plated (Pretty(..))

import Data.Config.Suckless.Syntax

import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Char (isAlpha,isAlphaNum)
import Data.Text qualified as Text

-- | A collaboration mailbox declared by the repo.
data HubMailbox = HubMailbox
  { mbKey  :: HubKey
  , mbRole :: Text          -- ^ role tag; the forge ingress uses "hub"
  , mbTier :: Maybe Text    -- ^ optional trust tier (PEP-21); absent = the only inbox
  }
  deriving stock (Eq,Show)

-- | A sigil bound to the mailbox it describes.
data MailboxSigil = MailboxSigil
  { msMailbox :: HubKey
  , msSigil   :: HashRef
  }
  deriving stock (Eq,Show)

-- | The role tag marking the forge ingress mailbox.
hubRole :: Text
hubRole = "hub"

-- | Every @(mailbox ...)@ clause, whatever its role.
-- The key is matched with 'SignPubKeyLike', which accepts a symbol or a
-- string literal, so the role and tier accept both too ('StringLike'); a
-- stricter match here would silently ignore @(mailbox "KEY" "hub")@.
-- A TRAILING @rest@, not two exact arities. This module's header claims the
-- git3 convention of ignoring what it does not recognize, which was true of
-- unknown CLAUSES and false of unknown FIELDS: both branches listed the arity
-- literally, so @(mailbox K hub public v2)@ matched neither and the clause did
-- not lose a field -- it DISAPPEARED. 'hubMailboxes' would answer empty,
-- 'mailboxFor' would say the repository declares no ingress, and a contributor
-- would be told it is not a forge, while the owner sees the clause in their own
-- manifest and has no way to learn that half the network does not.
--
-- git3 already writes the tolerant predicate in one place ('mailboxFor', with
-- @: _@) and the strict one in another, which is the same drift from the other
-- side.
mailboxes :: [Syntax c] -> [HubMailbox]
mailboxes syn = concatMap one syn
  where
    one = \case
      ListVal (SymbolVal "mailbox" : SignPubKeyLike k : StringLike role : rest) ->
        [HubMailbox k (Text.pack role) (tierOf rest)]
      _ -> []

    tierOf (StringLike t : _) = Just (Text.pack t)
    tierOf _                  = Nothing

-- | The forge ingress mailboxes: those tagged 'hubRole'.
hubMailboxes :: [Syntax c] -> [HubMailbox]
hubMailboxes = filter ((== hubRole) . mbRole) . mailboxes

-- | The tier a repo offers to strangers when the submitter names none.
publicTier :: Text
publicTier = "public"

-- | Pick an ingress mailbox by tier.
--
-- A named tier must match exactly. 'Nothing' means "the default inbox a
-- stranger should use", and falls back in order: the untiered mailbox, then
-- one tagged 'publicTier', then the first hub mailbox declared. Without the
-- fallback a repo that declares only @known@ and @public@ would leave a
-- default submitter with nothing to address.
mailboxByTier :: Maybe Text -> [Syntax c] -> Maybe HubMailbox
mailboxByTier (Just tier) syn = firstOf ((== Just tier) . mbTier) (hubMailboxes syn)
mailboxByTier Nothing syn =
  firstOf (isNothing . mbTier) mbs
    `orElse` firstOf ((== Just publicTier) . mbTier) mbs
    `orElse` firstOf (const True) mbs
  where
    mbs = hubMailboxes syn
    orElse a b = maybe b Just a

firstOf :: (a -> Bool) -> [a] -> Maybe a
firstOf p xs = case filter p xs of
  (x:_) -> Just x
  []    -> Nothing

-- | Every @(mailbox-sigil ...)@ clause.
sigils :: [Syntax c] -> [MailboxSigil]
sigils syn = concatMap one syn
  where
    one = \case
      -- Trailing @rest@ for the reason 'mailboxes' has one: a field added later
      -- must cost this clause a field and not the whole clause.
      ListVal (SymbolVal "mailbox-sigil" : SignPubKeyLike k : HashLike h : _) ->
        [MailboxSigil k h]
      _ -> []

-- | The sigils published for one mailbox. More than one means more than one
-- maintainer reads that inbox; a sender seals the group secret to all of them.
sigilsFor :: HubKey -> [Syntax c] -> [HashRef]
sigilsFor k = map msSigil . filter ((== k) . msMailbox) . sigils

-- | Emit @(mailbox <key> <role> [<tier>])@, or nothing when it would not read
-- back.
--
-- THE EMPTY STRING IS NOT A VALUE THIS FORMAT HAS. A string literal is built
-- out of a run of characters, so an empty one produces no token at all and the
-- clause loses a field: @(mailbox KEY hub "")@ parses as a mailbox with NO
-- tier, and @(mailbox KEY "")@ as a clause with no role, which is not a mailbox
-- clause and vanishes. Verified against the pinned parser.
--
-- That is a hole in the round trip this module's contract is, and it is not one
-- a writer can escape its way out of: there is no spelling to emit. So the
-- writer refuses instead, and says which field it was. An empty tier is anyway
-- a tag that tags nothing, and 'mailboxByTier' already has a value for "no
-- tier": absent.
mailboxClause :: HubMailbox -> Either Text (Syntax C)
mailboxClause (HubMailbox k role tier)
  | Text.null role = Left "the role is empty, and this format has no empty string"
  | any Text.null tier = Left "the tier is empty, and this format has no empty string"
  | otherwise =
      Right $ mkForm "mailbox" $
        [ mkSym (show (pretty (AsBase58 k))), atom role ]
        <> maybe [] (\t -> [atom t]) tier

-- | A symbol where one reads back as itself, a string otherwise.
--
-- The parser takes either ('stringLike' accepts a 'SymbolVal'), so this is
-- about the emitted form matching PEP-18's examples, which write the role and
-- tier bare. The leading-letter rule is what keeps the round trip honest: a
-- value starting with a digit would lex back as a number, and one containing
-- a space would lex back as two atoms.
--
-- The string branch escapes, and for the same reason every other projection in
-- this project does: 'mkStr' hands the printer a literal it wraps in quotes and
-- nothing more, so a role with a quote in it would close the string early and
-- the rest of the manifest would read back as whatever it happened to look
-- like. The manifest is the owner's own file, but a role reaches it from
-- wherever the owner copied it, and this is the last place that can be true.
atom :: Text -> Syntax C
atom t = case Text.uncons t of
  Just (c,rest) | isAlpha c, Text.all plain rest -> mkSym (Text.unpack t)
  _                                              -> sexpStr t
  where
    plain ch = isAlphaNum ch || ch `elem` ("-_.:/" :: String)

-- | Emit @(mailbox-sigil <mailbox-key> <hashref>)@.
mailboxSigilClause :: MailboxSigil -> Syntax C
mailboxSigilClause (MailboxSigil k h) =
  mkForm "mailbox-sigil"
    [ mkSym (show (pretty (AsBase58 k))), mkSym (show (pretty h)) ]
