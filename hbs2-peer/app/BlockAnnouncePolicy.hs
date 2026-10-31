module BlockAnnouncePolicy where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.Net.Proto.Types

import PeerConfig

import Control.Monad.Reader
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text

data PeerAcceptAnnounceKey

data AcceptAnnounce = AcceptAnnounceAll
                    | AcceptAnnounceFrom (Set (PubKey 'Sign (Encryption L4Proto)))

instance Pretty AcceptAnnounce where
  pretty = \case
    AcceptAnnounceAll     -> parens ("accept-announce" <+> "*")

    -- FIXME: better-pretty-for-AcceptAnnounceFrom
    AcceptAnnounceFrom xs -> parens ("accept-announce" <+> pretty (fmap AsBase58 (Set.toList xs)))

instance HasCfgKey PeerAcceptAnnounceKey AcceptAnnounce where
  key = "accept-block-announce"

instance (Monad m, HasConf m) => HasCfgValue PeerAcceptAnnounceKey AcceptAnnounce m where
  cfgValue = do
    syn <- getConf
    pure $ fromMaybe (AcceptAnnounceFrom (lst syn)) (fromAll syn)
    where
      fromAll syn = headMay [ AcceptAnnounceAll | ListVal (Key s [SymbolVal "*"]) <- syn, s == kk ]
      lst syn = Set.fromList $
                catMaybes [ fromStringMay @(PubKey 'Sign (Encryption L4Proto)) (Text.unpack e)
                          | ListVal (Key s [LitStrVal e]) <- syn, s == kk
                          ]
      kk = key @PeerAcceptAnnounceKey @AcceptAnnounce

announcePolicy :: PeerConfig -> AcceptAnnounce
announcePolicy (PeerConfig syn) = runReader (cfgValue @PeerAcceptAnnounceKey) syn

-- | Apply the configured policy to a sender.  A wildcard deliberately accepts
-- an unknown sender: requiring a 'KnownPeerKey' here made
-- @accept-block-announce *@ behave exactly like "all known peers", which kept
-- public relays from fetching blocks announced by repository contributors.
-- Known senders still carry their ban result, and an explicit allow-list still
-- requires an authenticated key.
announceAllowed
  :: AcceptAnnounce
  -> Maybe (Bool, PubKey 'Sign (Encryption L4Proto))
  -> Bool
announceAllowed AcceptAnnounceAll Nothing                = True
announceAllowed AcceptAnnounceAll (Just (banned, _))     = not banned
announceAllowed (AcceptAnnounceFrom _) Nothing           = False
announceAllowed (AcceptAnnounceFrom allowed) (Just (banned, sender)) =
  not banned && sender `Set.member` allowed
