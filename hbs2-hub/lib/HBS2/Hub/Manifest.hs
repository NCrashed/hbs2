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

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (HashRef(..),pattern HashLike)
import HBS2.Net.Auth.Credentials (pattern SignPubKeyLike)
import HBS2.Prelude.Plated (Pretty(..))

import Data.Config.Suckless.Syntax

import Data.Maybe (isNothing)
import Data.Text (Text)
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
mailboxes :: [Syntax c] -> [HubMailbox]
mailboxes syn = concatMap one syn
  where
    one = \case
      ListVal [SymbolVal "mailbox", SignPubKeyLike k, StringLike role] ->
        [HubMailbox k (Text.pack role) Nothing]
      ListVal [SymbolVal "mailbox", SignPubKeyLike k, StringLike role, StringLike tier] ->
        [HubMailbox k (Text.pack role) (Just (Text.pack tier))]
      _ -> []

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
      ListVal [SymbolVal "mailbox-sigil", SignPubKeyLike k, HashLike h] ->
        [MailboxSigil k h]
      _ -> []

-- | The sigils published for one mailbox. More than one means more than one
-- maintainer reads that inbox; a sender seals the group secret to all of them.
sigilsFor :: HubKey -> [Syntax c] -> [HashRef]
sigilsFor k = map msSigil . filter ((== k) . msMailbox) . sigils

-- | Emit @(mailbox <key> <role> [<tier>])@.
--
-- The role and tier are printed as strings, matching what the parser
-- accepts: a symbol would not survive a tier containing a space.
mailboxClause :: HubMailbox -> Syntax C
mailboxClause (HubMailbox k role tier) =
  mkForm "mailbox" $
    [ mkSym (show (pretty (AsBase58 k))), mkStr (Text.unpack role) ]
    <> maybe [] (\t -> [mkStr (Text.unpack t)]) tier

-- | Emit @(mailbox-sigil <mailbox-key> <hashref>)@.
mailboxSigilClause :: MailboxSigil -> Syntax C
mailboxSigilClause (MailboxSigil k h) =
  mkForm "mailbox-sigil"
    [ mkSym (show (pretty (AsBase58 k))), mkSym (show (pretty h)) ]
