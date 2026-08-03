-- | @hub inbox accept@: one letter into canon (PEP-22 "Maintain").
--
-- The verb that closes the loop a stranger's letter opens. Everything it
-- decides is decided elsewhere: "HBS2.Hub.Ingress" reads the letter,
-- "HBS2.Hub.Bridge" says whether it may be blessed and mints the event,
-- "HBS2.Hub.Repo" turns that into files, and "HBS2.Hub.Repo.GitWrite" commits
-- them. What is here is the order, the arguments and the refusals.
--
-- IRREVERSIBLE, and the only verb in this build that is: an event minted into
-- canon is in every clone that ever fetches it. That is why the read verb next
-- door is read-only by construction and why this one re-reads canon rather
-- than trusting anything it was handed.
--
-- TWO THINGS PEP-22 PUTS IN THIS VERB ARE NOT HERE, and both are named on
-- stdout after a successful accept rather than left for somebody to discover.
-- The letter is NOT deleted from the mailbox: fold-then-delete is PEP-21
-- retention, which needs a @DeleteMessages@ path this build does not have.
-- No acknowledgement is sent to the contributor's reply channel: the record
-- type exists ("HBS2.Hub.Letter"), the sending does not. Neither omission
-- loses anything -- re-accepting the same letter is refused by the bridge as
-- @AlreadyInCanon@, and a contributor without an ack reads status from public
-- canon, which PEP-18 already says is the fallback.
module HBS2.Hub.CLI.Accept
  ( acceptEntries
  , acceptUsage
  , acceptArgs
  , codeNoCanonKey
  , codeLetterUnreadable
  , codeTriageRefused
  , codeCanonUnwritable
  , codeCanonUnplannable
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Hub.Letter (EnvelopeSigner(..),maxPartBytes)
import HBS2.Hub.Bridge
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.Repo.GitWrite (withGitSink)
import HBS2.Hub.Ingress
import HBS2.Hub.CLI.Inbox (overRpc, refuse, codeMailboxUnknown, codePeerSilent, PeerSilent)
import HBS2.Hub.CLI.Verify (codeOf)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Peer.RPC.Client
import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,loadCredentials)
import HBS2.Net.Auth.Credentials (_peerSignSk)
import HBS2.Storage

import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import System.Exit (die)

-- | And what a machine with no key for the canon identity exits with.
codeNoCanonKey :: Int
codeNoCanonKey = 21

-- | The letter named is not one this node can read.
codeLetterUnreadable :: Int
codeLetterUnreadable = 22

-- | The bridge would not bless it. Not a failure of this program: a letter
-- that asks for something a stranger's signature cannot carry is the ordinary
-- outcome of triage.
codeTriageRefused :: Int
codeTriageRefused = 23

-- | Canon could not be written.
codeCanonUnwritable :: Int
codeCanonUnwritable = 24

-- | The event rendered to a file this build could not read back, so it was not
-- written. Its own code because, unlike the one above, it is this build's bug
-- and not the repository's state.
codeCanonUnplannable :: Int
codeCanonUnplannable = 25

acceptUsage :: Doc ()
acceptUsage =
  "usage: hbs2-hub hub inbox accept --mailbox <key> --repo <key> --message <hash> [--as <key>]"

-- | @hub inbox accept@.
acceptEntries :: forall c m . ( IsContext c
                              , MonadUnliftIO m
                              , HasStorage m
                              , HasClientAPI MailboxAPI UNIX m
                              , Exception (BadFormException c)
                              ) => MakeDictM c m ()
acceptEntries = do

  brief "fold one letter from the ingress mailbox into canon"
    $ args [ arg "string" "--mailbox mailbox-key"
           , arg "string" "--repo repo-key"
           , arg "string" "--message message-hash" ]
    $ desc ( "Writes. Everything else in this tool reads."
             <> line
             <> line <> "Reads canon out of refs/hbs2/meta in this repository,"
             <> line <> "opens the named letter, asks the bridge whether it may be"
             <> line <> "blessed, and commits the event onto canon. The commit is a"
             <> line <> "compare-and-swap against the canon it folded, so two"
             <> line <> "accepts racing leave one refusal rather than one silent"
             <> line <> "loss."
             <> line
             <> line <> "--as names the canon key to sign with, for a delegate"
             <> line <> "(PEP-21). It defaults to the repo key, which is the owner"
             <> line <> "and the root of trust."
             <> line
             <> line <> "The letter is NOT deleted afterwards and no acknowledgement"
             <> line <> "is sent: both are PEP-21 retention and PEP-18 back-channel"
             <> line <> "work this build does not have. Accepting the same letter"
             <> line <> "twice is refused, so leaving it in the mailbox is safe."
             <> line
             <> line <> "No deny-list is applied. PEP-21 policy lives in the repo"
             <> line <> "manifest and there is no manifest reader yet, so a banned"
             <> line <> "author's letter is accepted like any other. Read the queue"
             <> line <> "with 'hub inbox' and decide before running this." )
    $ entry $ bindMatch "hub:inbox:accept" $ nil_ \case
        (acceptArgs -> Just a) -> lift (accept a)
        _ -> liftIO (die (show acceptUsage))

  where

    accept (mbox, repo, msg, asKey) = do
      let canonKey = fromMaybe repo asKey

      creds <- runKeymanClientRO (loadCredentials canonKey)
                 >>= maybe (liftIO (refuse (show ("no signing key here for"
                                                   <+> pretty (AsBase58 canonKey)))
                                           codeNoCanonKey))
                           pure

      -- Canon first, and from git rather than from anything remembered: the
      -- commit read here is the value the publish compares against, so reading
      -- it late is what makes the compare-and-swap mean anything.
      --
      -- A repository with no canon ref is the FIRST accept, not a failure. Every
      -- other way of not reading is, and they keep the codes `hub verify`
      -- already assigns them, so a script branches on one table.
      (parent, fr) <- withGitCanon (\cs -> readCanon cs repo) >>= \case
        Right st -> pure (Just (stCommit st), stFold st)
        Left NoCanonRef{} -> pure (Nothing, foldEvents repo [])
        Left e -> liftIO (refuse (show (pretty e)) (codeOf e))

      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX
      let ig = overRpc sto api

      -- THE LETTER MUST BE IN THIS MAILBOX. Reading it by hash alone would
      -- work, and would accept any message-shaped block the peer has ever
      -- downloaded for any reason: a hash is not a claim about where the bytes
      -- came from. The bridge would still check the target and the signature,
      -- so the damage is bounded, but "the operator triaged this queue and
      -- picked this line out of it" is the thing the verb is supposed to mean,
      -- and it is cheap to make true.
      --
      -- One inbox read per accept, which is the same read `hub inbox` just did.
      inbox <- readInbox ig mbox
                 `catch` (\(e :: MailboxUnknown) -> liftIO (refuse (show e) codeMailboxUnknown))
                 `catch` (\(e :: PeerSilent)     -> liftIO (refuse (show e) codePeerSilent))

      unless (msg `elem` fmap lvMessage (irLetters inbox)) $
        liftIO $ refuse (show ( pretty msg <+> "is not in mailbox"
                                  <+> pretty (AsBase58 mbox)
                                  <> (if irSettled inbox
                                        then mempty
                                        else ", which had not settled when it was read") ))
                        codeLetterUnreadable

      raw <- rawMessage ig msg
               `catch` (\(e :: MailboxUnknown) -> liftIO (refuse (show e) codeMailboxUnknown))
               `catch` (\(e :: PeerSilent)     -> liftIO (refuse (show e) codePeerSilent))
             >>= either (\e -> liftIO (refuse (show ("cannot read" <+> pretty msg
                                                      <> ":" <+> viaShow e))
                                              codeLetterUnreadable))
                        pure

      -- Milliseconds, the unit every stamp in this package is in (PEP-19). In
      -- seconds two owner ops in one tick collapse to one event-id.
      now <- liftIO getPOSIXTime <&> floor . (* 1000)

      let ctx = TriageCtx (canonKey, _peerSignSk creds) (const True) repo

      -- What is known about each attachment, gathered before the bridge is
      -- asked. Measured first and opened second, and only when the size is one
      -- this hub would carry: opening means decrypting, and decrypting a part
      -- the gate is about to refuse for its size is exactly the spend the gate
      -- exists to prevent (PEP-18). An oversized part is reported at its size
      -- and the refusal comes back as PartTooLarge.
      evidence <- for (lrParts raw) $ \h -> do
        facts <- measurePart ig h
        (,) h <$> case facts of
          -- Not measurable is not fetched: the size is unknown, and zero is
          -- the only honest stand-in for a number nothing has said yet.
          Left _ -> pure (PartPending 0)
          Right f
            | not (pfHere f)             -> pure (PartPending (pfSize f))
            | pfSize f > maxPartBytes    -> pure (PartLocked (pfSize f))
            | otherwise -> igOpenPart ig h >>= \case
                Left _ -> pure (PartLocked (pfSize f))
                Right (_, sec) -> case mkPartSecret sec of
                  Just s  -> pure (PartOpened (pfSize f) s)
                  -- A secret of the wrong length is not a key, so this node
                  -- cannot open the part however the read went.
                  Nothing -> pure (PartLocked (pfSize f))

      acc <- either (\e -> liftIO (refuse (show ("refused:" <+> viaShow e))
                                          codeTriageRefused))
                    pure
               (acceptLetter ctx (EnvelopeSigner (lrEnvelope raw)) (viewOf fr)
                             now msg (attachments (lrSecret raw) evidence)
                             (lrData raw))

      -- The index is regenerated from the fold, plus the number this accept
      -- just minted. Derived rather than re-folded because the derivation is
      -- exactly what the fold would do: an open adds one thread with one
      -- number and changes no other, and nothing else mints a number at all.
      let minted = [ (n, t) | Just n <- [acNumber acc], ThreadScope t <- [acScope acc] ]
          numbers = sortOn fst (numberIndexOf fr <> minted)

      plan <- either (\e -> liftIO (refuse (show (pretty e)) codeCanonUnplannable)) pure
                (planCanon [(eventPath acc, acEvent acc)] numbers)

      commit <- withGitSink (\sk -> skCommit sk (CanonWrite parent (cwFiles plan)
                                                            (message acc) now))
                  >>= either (\e -> liftIO (refuse (show (pretty e)) codeCanonUnwritable))
                             pure

      liftIO $ print $ vcat
        [ "accepted" <+> pretty (eventId (acEvent acc))
        , "seq" <+> pretty (acSeq acc)
        , maybe mempty (\n -> "number" <+> pretty n) (acNumber acc)
        , "commit" <+> pretty commit
        , "left in the mailbox:" <+> pretty msg
            <+> "(no delete, no acknowledgement; see --help)"
        ]

      for_ (omittedNote plan) (liftIO . print)

    -- A commit message a human reading `git log refs/hbs2/meta` can act on.
    -- Not a place to put anything authoritative: everything that is signed is
    -- in the files.
    message acc = "hub: " <> tshow (acNumber acc) <> " " <> tshow (eventId (acEvent acc))

    omittedNote plan
      | cwIndexOmitted plan <= 0 = Nothing
      | otherwise = Just ( "note:" <+> pretty (cwIndexOmitted plan)
                             <+> "number(s) did not fit index/number.sexp;"
                             <+> "it is a convenience map and is regenerable" )

    tshow :: Pretty a => a -> Text
    tshow = fromString . show . pretty

-- The flags in any order, and the message hash positionally.
--
-- Named flags for the two keys rather than positions, for the reason the
-- compose verb gives about its own: a repo key and a mailbox key are the same
-- type, so a swap is a well-typed accept against the wrong repository.
acceptArgs :: forall c . [Syntax c] -> Maybe (HubKey, RepoRef, HashRef, Maybe HubKey)
acceptArgs syn = do
  mbox <- flagged "--mailbox" asKey
  repo <- flagged "--repo" asKey
  h    <- flagged "--message" asHash
  pure (mbox, repo, h, flagged "--as" asKey)
  where
    -- EVERY value is behind a flag, including the message, and nothing is
    -- positional. Not a style choice: a sign key and a hash are both thirty-two
    -- bytes of base58, so 'SignPubKeyLike' matches a hash and 'HashLike'
    -- matches a key, and there is no reading of a bare word that can tell them
    -- apart. A positional message therefore made `--repo <hash> <key>` parse
    -- happily with the two swapped, and the first thing that noticed was the
    -- bridge, refusing the letter for a repository the caller never named.
    --
    -- What is left is a caller typing the wrong value after the right flag,
    -- which no parser can catch and a human reading the line can.
    flagged :: forall v . String -> (Syntax c -> Maybe v) -> Maybe v
    flagged n f = case [ v | (StringLike n', f -> Just v) <- zip syn (drop 1 syn)
                           , n' == n ] of
                    [v] -> Just v
                    -- A repeated flag is refused rather than resolved. Neither
                    -- rule is safe when the value decides what gets published:
                    -- first-wins and last-wins are both a guess about which of
                    -- two repositories the caller meant.
                    _   -> Nothing

    asKey = \case
      SignPubKeyLike k -> Just k
      _                -> Nothing

    asHash = \case
      HashLike h -> Just h
      _          -> Nothing
