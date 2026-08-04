-- | The triage deny-list, as a store (PEP-21 "Deny-lists").
--
-- Split from the verbs in "HBS2.Hub.CLI.Ban" because three callers need the
-- list and one of them cannot import that module: `hub inbox` builds its queue
-- in "HBS2.Hub.CLI.Inbox", which Ban already imports for 'refuse'. A cycle is
-- the shape "who may be folded" being answered next to "how a refusal prints".
--
-- LOCAL STATE, and that is PEP-21's decision rather than an omission: a public
-- ban needs an author-content constructor and an admission rule saying who may
-- sign it, which is a consensus change. So this list does not travel, is not
-- signed, and two hubs serving one repository can disagree about it.
--
-- OUTSIDE THE WORKING TREE for that reason. A file under the repository would
-- look like something that travels, and would eventually be committed by
-- somebody's @git add -A@, which is the one thing a list this build cannot
-- publish must not do by accident.
module HBS2.Hub.Deny
  ( banPath
  , renderBans
  , parseBans
  , allowedBy
  , loadBans
  , codeNoBanList
  ) where

import HBS2.Hub.Types (HubKey,safeText)

import HBS2.CLI.Prelude

import HBS2.Base58 (AsBase58(..))

import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (sort)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (doesFileExist,getXdgDirectory,XdgDirectory(..))
import System.FilePath ((</>))

-- | The list could not be read, and this build will not guess at it.
--
-- A deny-list that reads as empty when it is unreadable is a deny-list that
-- silently stops working, which is the failure this whole layer exists to
-- prevent.
codeNoBanList :: Int

codeNoBanList = 36
-- | Where this node keeps its deny-list for one repository.
--
-- Under XDG data, keyed by the repository, because the list is this NODE's
-- decision about that repository: two hubs serving one repo may differ, and
-- one hub serving two repos must not confuse them.
banPath :: MonadIO m => HubKey -> m FilePath
banPath repo = do
  dir <- liftIO (getXdgDirectory XdgData "hbs2-hub")
  pure (dir </> "banned" </> show (pretty (AsBase58 repo)))

-- | The file's bytes, sorted so that banning two keys in either order gives
-- one file. Nothing hashes this today; it is a file a person reads and a
-- diff should show one line changing.
renderBans :: HashSet HubKey -> Text
renderBans ks =
  Text.unlines (sort [ Text.pack (show (pretty (mkList @C [ mkSym "ban"
                                                          , mkSym (show (pretty (AsBase58 k)))])))
                     | k <- HS.toList ks ])

-- | And back. An unreadable line is an error rather than a skip: a deny-list
-- that quietly drops what it cannot read is one an attacker shortens by
-- writing something odd into it.
parseBans :: Text -> Either Text (HashSet HubKey)
parseBans txt = do
  syn <- either (const (Left "the file is not a list of clauses")) Right
           (parseTop (Text.unpack txt))
  fmap HS.fromList (traverse one [ s | s <- syn ])
  where
    one = \case
      ListVal [SymbolVal "ban", SignPubKeyLike k] -> Right k
      other -> Left ("not a ban clause: " <> safeText (Text.pack (show (pretty other))))

-- | The predicate the triage loop wants: may this author be folded?
--
-- Takes the set rather than a path so the decision is a function, and so the
-- one place that reads a file is not also the one that decides.
allowedBy :: HashSet HubKey -> HubKey -> Bool
allowedBy bans = not . (`HS.member` bans)

-- | This node's deny-list for a repository, or why it could not be read.
--
-- Exported so the accept path can apply it, which is the whole point of the
-- list: a verb that only wrote it and never consulted it would be a moderation
-- feature that moderates nothing.
--
-- A missing file is an empty list, which is the only sane reading of "nobody
-- has banned anybody here". An unreadable one is NOT: a deny-list that reads
-- as empty when it is damaged stops working silently, which is the failure
-- this layer exists to prevent.
loadBans :: MonadIO m => HubKey -> m (Either Text (HashSet HubKey))
loadBans repo = do
  p <- banPath repo
  here <- liftIO (doesFileExist p)
  if not here then pure (Right HS.empty)
    else liftIO (Text.readFile p) <&> parseBans
