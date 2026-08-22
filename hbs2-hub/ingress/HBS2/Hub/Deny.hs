-- | The triage deny-list, as a store (PEP-21 "Deny-lists").
--
-- Split from the verbs in "HBS2.Hub.CLI.Ban" because three callers need the
-- list and none of them should have to import a verb to get it.
--
-- It used to be a near-cycle as well: `hub inbox` builds its queue in
-- "HBS2.Hub.CLI.Inbox", and Ban imported that same module for 'refuse', so
-- "who may be folded" was answered next door to "how a refusal prints". That
-- half is gone -- 'refuse' and the exit codes live in "HBS2.Hub.CLI.Common"
-- now, which is a module and not a verb -- and the split here stands on its
-- own reason.
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
  , saveBans
  , denyingFor
  , denyUnreadable
  , codeNoBanList
  ) where

import HBS2.Hub.Types (HubKey,safeText)
import HBS2.Hub.Canon (clausesWith)

import HBS2.CLI.Prelude

import HBS2.Base58 (AsBase58(..))

import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (sort)
import Data.Text qualified as Text
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.IO qualified as Text
import System.Directory (doesFileExist,createDirectoryIfMissing,getXdgDirectory
                        ,renameFile,XdgDirectory(..))
import System.FilePath ((</>),takeDirectory)

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
--
-- A LINE AT A TIME, through the bounded reader the rest of this package uses.
-- 'parseTop' is superlinear in the number of top-level forms -- measured on
-- this file at 42 ms for 1024 bans, 136 ms for 2048 and 1.97 s for 8192 -- and
-- 'loadBans' runs on every accept, so a hub that bans steadily was paying a
-- quadratic for it. A ban IS a line ('renderBans' writes them with
-- 'Text.unlines' and a base58 key holds no newline), so parsing one line at a
-- time makes the cost linear in the number of bans.
--
-- LINEAR AND NOT BOUNDED, which is the difference between this file and a
-- manifest. A manifest is a stranger's and a bound on it is a refusal to be
-- imposed on; this list is the operator's own and grows by them using the verb
-- that writes it, so a wall would eventually stop an accept over a file nobody
-- did anything wrong with. What is bounded is the LINE, which nothing legitimate
-- makes long.
--
-- Several clauses on one line still read: 'clausesWith' answers with every form
-- the line opens, which is what keeps a hand-edited file working.
parseBans :: Text -> Either Text (HashSet HubKey)
parseBans txt =
  fmap HS.fromList . traverse one . concat =<< traverse line (Text.lines txt)
  where
    line l = either (\e -> Left (Text.pack (show (pretty e)) <> ": " <> safeText l)) Right
               (clausesWith maxBanLineBytes maxBanLineClauses l)

    one = \case
      ListVal [SymbolVal "ban", SignPubKeyLike k] -> Right k
      other -> Left ("not a ban clause: " <> safeText (Text.pack (show (pretty other))))

-- | What one line of the list may weigh, and how many clauses it may open.
--
-- A ban is @(ban \<44 base58 characters\>)@, which is 52 bytes. The bound is the
-- number index's, for the same reason and out of the same measurements: nothing
-- that writes this file writes a long line, and the cost being defended against
-- is the parser's, not the disk's.
maxBanLineBytes, maxBanLineClauses :: Int
maxBanLineBytes   = 256
maxBanLineClauses = 4

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
    -- try, because Data.Text.IO.readFile throws UnicodeException on a file
    -- that is not UTF-8, which walks straight past this function's Either and
    -- out of an accept as a raw exception. A deny-list that will not decode is
    -- a deny-list this node cannot read, which is the answer this already has.
    else liftIO (try @_ @UnicodeException (Text.readFile p)) <&> \case
           Left e -> Left ("the deny-list is not UTF-8: " <> Text.pack (show e))
           Right t -> parseBans t

-- | And back to disk, atomically.
--
-- BESIDE THE READER, because the two halves are one argument. A torn in-place
-- write leaves a file that is SHORTER and still parses -- every line is one
-- key, so half a file is a valid deny-list with keys missing -- and that is the
-- one failure this design is otherwise built to exclude: 'loadBans' refuses a
-- file it cannot read ENTIRELY, precisely so that a list an attacker shortened
-- is a refusal rather than a shorter list. An interrupted write does the
-- shortening for them.
--
-- rename is atomic within a filesystem, and the temporary is made in the same
-- directory so that it is the same one.
--
-- Answers with the path, which is what the verb prints.
saveBans :: MonadIO m => HubKey -> HashSet HubKey -> m FilePath
saveBans repo bans = do
  p <- banPath repo
  liftIO do
    createDirectoryIfMissing True (takeDirectory p)
    let tmp = p <> ".new"
    Text.writeFile tmp (renderBans bans)
    renameFile tmp p
  pure p

-- | The predicate the triage path applies, for the repository it was given.
--
-- THE WHOLE WIRE, in one place: which repository, to a file, to a set, to the
-- question the loop asks about an author. Three verbs built it by hand out of
-- 'loadBans' and 'allowedBy' -- @hub inbox@, @hub inbox show@ and @hub inbox
-- accept@ -- and the tests proved that the PREDICATE refuses a banned author
-- without ever proving the predicate is built from the file. That is the whole
-- of PEP-21 banning hanging on one unasserted wire, in three copies.
--
-- 'Nothing' is "no repository was named", which is the form that reads a
-- mailbox by key: there is no list to apply and no ban is a claim about anyone.
denyingFor :: MonadIO m => Maybe HubKey -> m (Either Text (HubKey -> Bool))
denyingFor = \case
  Nothing   -> pure (Right (const True))
  Just repo -> fmap allowedBy <$> loadBans repo

-- | What a verb says about a deny-list it could not read.
--
-- One sentence, because three verbs wrote it out and two of them escaped the
-- reason while the third printed it raw -- and the reason quotes a line of a
-- file, which is the sort of thing that carries an escape sequence.
denyUnreadable :: Text -> Doc ann
denyUnreadable e = "the deny-list will not read:" <+> pretty (safeText e)
