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
-- "the deny-list stays loop state until some hub-meta bump carries one".
-- Publishing one into canon needs a new author-content constructor and an
-- admission rule saying who may sign it and what it does, which is a consensus
-- change. So this list does not travel, is not signed, and two hubs serving one
-- repository can disagree about it. The spec used to name the next version and
-- two of those have shipped for other reasons since, which is why it names none.
--
-- OUTSIDE THE WORKING TREE for that reason. A file under the repository would
-- look like something that travels, and would eventually be committed by
-- somebody's @git add -A@, which is the one thing a list this build cannot
-- publish must not do by accident.
module HBS2.Hub.CLI.Ban
  ( banEntries
  , banUsage
  , banListArgs
  , banArgs
  , BanArgs(..)
  ) where

import HBS2.Hub.Types (HubKey,RepoRef)
import HBS2.Hub.Deny (loadBans,renderBans,banPath,codeNoBanList)
import HBS2.Hub.CLI.Argv (flagsOf,flagOnce,flagMaybe,repoFlags,flagRepo)
import HBS2.Hub.CLI.Common (refuse,saying)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))

import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.Text.IO qualified as Text
import System.Directory (createDirectoryIfMissing,renameFile)
import System.Exit (die,exitSuccess)
import System.FilePath (takeDirectory)

data BanArgs = BanArgs
  { baRepo :: HubKey
  , baKey  :: Maybe HubKey
  }
  deriving stock (Eq,Show)

banUsage :: Doc ()
banUsage =
  "usage: hbs2-hub ban|unban --repo <key> --key <author-key>"
    <> line <> "       hbs2-hub ban list --repo <key>"

-- The list itself lives in "HBS2.Hub.Deny": three callers need it and one of
-- them, the queue in HBS2.Hub.CLI.Inbox, is a module this one imports.
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
             <> line <> "a ban into canon to some later hub-meta: it needs a new"
             <> line <> "author-content op and an admission rule, so two hubs"
             <> line <> "serving one repository may disagree about this list." )
    $ entry $ bindMatch "hub:ban:list" $ nil_ \case
        (banListArgs -> Just repo) -> lift do
          bans <- readBans repo
          liftIO $ if HS.null bans
            -- On stderr, and it is the difference between a list and a
            -- sentence. STDOUT here is a stream of keys a script reads; a
            -- prose line in it is a key that parses as nothing, and a caller
            -- that pipes this into a loop gets one iteration over the words
            -- "nobody is banned here".
            then saying ("nobody is banned here" <> line)
            else mapM_ (print . pretty . AsBase58) (sortKeys bans)
        _ -> liftIO (die (show banUsage))

  where

    sortKeys = List.sortOn (show . pretty . AsBase58) . HS.toList

    banVerb name ban what =
      brief what
        $ args [arg "string" "--repo repo-key", arg "string" "--key author-key"]
        -- The sibling's own sentence first. Both verbs shared one description
        -- verbatim, so `hub help unban` opened with a paragraph about what a
        -- ban is and never said what unbanning does.
        $ desc ( (if ban then "Adds a key to this repository's deny-list."
                         else "Takes a key off this repository's deny-list.")
                 <> line
                 <> line <> "The INNER AUTHOR key, not the envelope. It is the identity"
                 <> line <> "inside the signed box, so it survives a rewrap, which"
                 <> line <> "is what makes this the authoritative deny for canon."
                 <> line
                 <> line <> "It bounds what this node FOLDS, not what it stores:"
                 <> line <> "a banned author's letters still arrive and still take"
                 <> line <> "disk. Bounding storage is 'hub block', on the envelope"
                 <> line <> "key. A full ban is both."
                 <> line
                 <> line <> "READ THIS BEFORE BANNING SOMEBODY FOR A FLOOD OF ONE"
                 <> line <> "LETTER. Re-signing a captured message under a fresh key"
                 <> line <> "needs no key and no plaintext: anyone who saw the"
                 <> line <> "ciphertext can do it. Every such copy carries the same"
                 <> line <> "inner author, so the only ban that stops them all is a"
                 <> line <> "ban on the person whose letter was captured -- who may"
                 <> line <> "have sent it once and done nothing else."
                 <> line
                 <> line <> "The queue groups copies onto one line and 'hub inbox"
                 <> line <> "reject' drops the group, so a flood costs one decision"
                 <> line <> "rather than one per copy. What prices it at the door is"
                 <> line <> "'hub policy pow', which charges nothing until set."
                 <> line
                 <> line <> "Local and unsigned, and it does not travel: PEP-21"
                 <> line <> "defers a published ban to a later hub-meta. Past events stay"
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
        -- Advice, so stderr, like every other line in this package that is
        -- about the command rather than its result.
        saying ("nothing to change" <> line)
        exitSuccess

      p <- banPath (baRepo ba)
      liftIO do
        createDirectoryIfMissing True (takeDirectory p)
        -- WRITTEN BESIDE IT AND RENAMED OVER IT, never in place.
        --
        -- A torn in-place write leaves a file that is SHORTER and still
        -- parses -- every line is one key, so half a file is a valid
        -- deny-list with keys missing. That is the one failure this design is
        -- otherwise built to exclude: 'loadBans' refuses a file it cannot
        -- read entirely, precisely so that a list an attacker shortened is a
        -- refusal rather than a shorter list. An interrupted write did the
        -- shortening for them.
        --
        -- rename is atomic within a filesystem, and the temporary is made in
        -- the same directory so that it is the same one.
        let tmp = p <> ".new"
        Text.writeFile tmp (renderBans bans')
        renameFile tmp p
        print $ vcat
          [ (if ban then "banned" else "unbanned") <+> pretty (AsBase58 who)
          , pretty (HS.size bans') <+> "author(s) denied, in" <+> pretty p
          , "this is local and unsigned; it does not travel"
          ]

-- Both behind flags, and both are keys of one type.
banArgs :: forall c . IsContext c => [Syntax c] -> Maybe BanArgs
banArgs syn = do
  kvs  <- flagsOf ["--repo","--key"] syn
  repo <- flagOnce kvs "--repo" >>= asKey
  k    <- flagMaybe kvs "--key" asKey
  pure (BanArgs repo k)
  where
    asKey = \case { SignPubKeyLike v -> Just v ; _ -> Nothing }

-- | @--repo <key>@, and nothing else.
--
-- Its own reader rather than an inline pattern, for the reason every other verb
-- here has one: a hand-matched @[StringLike "--repo", SignPubKeyLike k]@ takes
-- the two words in that order and refuses everything else, including
-- @--repo=<key>@ -- which 'issueUsage' tells every user is accepted and which
-- five verbs, this among them, did not take.
banListArgs :: forall c . IsContext c => [Syntax c] -> Maybe RepoRef
banListArgs syn = flagsOf repoFlags syn >>= flagRepo asKey
  where
    asKey = \case { SignPubKeyLike v -> Just v ; _ -> Nothing }
