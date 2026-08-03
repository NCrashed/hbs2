-- | @hub ban@, @hub unban@ and @hub ban list@ (PEP-21 "Deny-lists").
--
-- The TRIAGE layer, which is the other of PEP-21's two and the one that
-- decides what enters canon. It denies an INNER AUTHOR: the key inside the
-- signed box, which is the real author and the one thing a rewrapper cannot
-- change. The peer layer next door ("HBS2.Hub.CLI.Policy") denies an envelope
-- key, bounds what this peer stores, and is evadable by re-sending under a
-- fresh envelope. A full ban is both.
--
-- LOCAL STATE, and that is PEP-21's decision rather than an omission here:
-- "the deny-list stays loop state, and the earliest a public ban can appear is
-- hub-meta 2". Publishing one into canon needs a new author-content
-- constructor and an admission rule saying who may sign it and what it does,
-- which is a consensus change. So this list does not travel, is not signed,
-- and two hubs serving one repository can disagree about it.
--
-- OUTSIDE THE WORKING TREE for that reason. A file under the repository would
-- look like something that travels, and would eventually be committed by
-- somebody's @git add -A@, which is the one thing a list this build cannot
-- publish must not do by accident.
module HBS2.Hub.CLI.Ban
  ( banEntries
  , banUsage
  , banArgs
  , BanArgs(..)
  , banPath
  , renderBans
  , parseBans
  , allowedBy
  , loadBans
  , codeNoBanList
  ) where

import HBS2.Hub.Types (HubKey,safeText)
import HBS2.Hub.CLI.Inbox (refuse)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))

import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (sort)
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (createDirectoryIfMissing,doesFileExist,getXdgDirectory
                        ,XdgDirectory(..))
import System.Exit (die,exitSuccess)
import System.FilePath ((</>),takeDirectory)

-- | The list could not be read, and this build will not guess at it.
--
-- A deny-list that reads as empty when it is unreadable is a deny-list that
-- silently stops working, which is the failure this whole layer exists to
-- prevent.
codeNoBanList :: Int
codeNoBanList = 36

data BanArgs = BanArgs
  { baRepo :: HubKey
  , baKey  :: Maybe HubKey
  }
  deriving stock (Eq,Show)

banUsage :: Doc ()
banUsage =
  "usage: hbs2-hub ban|unban --repo <key> --key <author-key>"
    <> line <> "       hbs2-hub ban list --repo <key>"

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

banEntries :: forall c m . ( IsContext c
                           , MonadUnliftIO m
                           , Exception (BadFormException c)
                           ) => MakeDictM c m ()
banEntries = do

  banVerb "hub:ban" True "refuse to fold letters from an author"
  banVerb "hub:unban" False "fold letters from an author again"

  brief "list the authors this node will not fold letters from"
    $ args [arg "string" "--repo repo-key"]
    $ desc ( "The TRIAGE layer: these are inner-author keys, the identity"
             <> line <> "inside the signed box, which a rewrapper cannot change."
             <> line
             <> line <> "Local to this node and unsigned. PEP-21 defers publishing"
             <> line <> "a ban into canon to hub-meta 2, because it needs a new"
             <> line <> "author-content op and an admission rule, so two hubs"
             <> line <> "serving one repository may disagree about this list." )
    $ entry $ bindMatch "hub:ban:list" $ nil_ \case
        [ StringLike "--repo", SignPubKeyLike repo ] -> lift do
          bans <- readBans repo
          liftIO $ if HS.null bans
            then print ("nobody is banned here" :: Doc ())
            else mapM_ (print . pretty . AsBase58) (sortKeys bans)
        _ -> liftIO (die (show banUsage))

  where

    sortKeys = List.sortOn (show . pretty . AsBase58) . HS.toList

    banVerb name ban what =
      brief what
        $ args [arg "string" "--repo repo-key", arg "string" "--key author-key"]
        $ desc ( "The INNER AUTHOR key, not the envelope. It is the identity"
                 <> line <> "inside the signed box, so it survives a rewrap, which"
                 <> line <> "is what makes this the authoritative deny for canon."
                 <> line
                 <> line <> "It bounds what this node FOLDS, not what it stores:"
                 <> line <> "a banned author's letters still arrive and still take"
                 <> line <> "disk. Bounding storage is 'hub block', on the envelope"
                 <> line <> "key. A full ban is both."
                 <> line
                 <> line <> "Local and unsigned, and it does not travel: PEP-21"
                 <> line <> "defers a published ban to hub-meta 2. Past events stay"
                 <> line <> "in canon; this refuses future ones." )
        $ entry $ bindMatch name $ nil_ \case
            (banArgs -> Just ba) | Just who <- baKey ba -> lift (setBan ban ba who)
            _ -> liftIO (die (show banUsage))

    readBans repo =
      loadBans repo >>= either
        (\e -> liftIO (refuse (show ("the deny-list will not read:" <+> pretty e)) codeNoBanList))
        pure

    setBan ban ba who = do
      bans <- readBans (baRepo ba)
      let bans' = (if ban then HS.insert else HS.delete) who bans

      when (bans' == bans) $ liftIO do
        print ("nothing to change" :: Doc ())
        exitSuccess

      p <- banPath (baRepo ba)
      liftIO do
        createDirectoryIfMissing True (takeDirectory p)
        Text.writeFile p (renderBans bans')
        print $ vcat
          [ (if ban then "banned" else "unbanned") <+> pretty (AsBase58 who)
          , pretty (HS.size bans') <+> "author(s) denied, in" <+> pretty p
          , "this is local and unsigned; it does not travel"
          ]

-- Both behind flags, and both are keys of one type.
banArgs :: forall c . [Syntax c] -> Maybe BanArgs
banArgs syn = do
  repo <- flagged "--repo"
  pure (BanArgs repo (flagged "--key"))
  where
    flagged n = case [ v | (StringLike n', SignPubKeyLike v) <- zip syn (drop 1 syn)
                         , n' == n ] of
                  [v] -> Just v
                  _   -> Nothing

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
