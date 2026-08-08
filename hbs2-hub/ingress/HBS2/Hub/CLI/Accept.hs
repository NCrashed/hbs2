-- | @hub inbox accept@: one letter into canon (PEP-22 "Maintain").
--
-- The verb that closes the loop a stranger's letter opens. Everything it
-- decides is decided elsewhere: "HBS2.Hub.Ingress" reads the letter,
-- "HBS2.Hub.Bridge" says whether it may be blessed and mints the event,
-- "HBS2.Hub.Repo" turns that into files, and "HBS2.Hub.Repo.GitWrite" commits
-- them. What is here is the order, the arguments and the refusals.
--
-- IRREVERSIBLE: an event minted into canon is in every clone that ever fetches
-- it. That is why the read verb next door is read-only by construction and why
-- this one re-reads canon rather than trusting anything it was handed.
--
-- It said "and the only verb in this build that is", which was true when it was
-- written and stopped being true six verbs later. What is still particular to
-- this one is WHOSE bytes it publishes: the owner verbs commit what the owner
-- typed, and this one commits what a stranger sent.
--
-- FOR A PULL REQUEST it also does the two steps PEP-20 puts either side of the
-- fold. The bundle is verified BEFORE anything is published: minting writes
-- nothing, so the check sits between the mint and the commit and a refusal
-- leaves canon untouched. The proposed tip is staged AFTER, because
-- @refs\/hbs2\/pulls\/<n>\/head@ needs the number and the number is not public
-- until canon holds it. So the order is verify, fold, stage, which is the
-- order the spec gives and the only one the dependencies allow.
--
-- THE LETTER IS DROPPED FROM THE MAILBOX LAST, which is PEP-21's fold-then-
-- delete ("hub behaviour on top of existing @DeleteMessages@"). Last, because
-- everything above it can refuse and this cannot be taken back; and soft,
-- because by the time it runs the accept has already happened. A drop that
-- fails leaves a folded letter in the queue and says so, and that is a queue
-- entry rather than a lost fold: re-accepting is refused by the bridge as
-- @AlreadyInCanon@, so nothing about canon depends on it.
--
-- WHAT PROTECTS THE ATTACHMENTS is the fold and not the tombstone. Rule A2 says
-- a hub may not delete an attachment canon references, and dropping the letter
-- does not: the entry is a tombstone, the blocks stay, and PEP-21 puts the
-- obligation on the purge nobody has written yet -- it must skip any tree
-- pinned by a folded canon event. This verb is the reason that obligation now
-- has a caller. What a purge must never do is take the parts of a letter
-- BECAUSE it was dropped, when the reason it was dropped is that canon took it.
--
-- AND THE CONTRIBUTOR IS TOLD, when the letter asked and this node can (see
-- "HBS2.Hub.CLI.Ack"). Between the commit and the drop, because it reports the
-- fold and must not depend on the letter still being in the mailbox. Like the
-- drop, it is soft: an ack that does not go out is a notification that did not
-- happen, and the report says which. Canon is the authority either way, which
-- is what PEP-18 means by calling this a courtesy.
module HBS2.Hub.CLI.Accept
  ( acceptEntries
  , acceptUsage
  , acceptArgs
  , AcceptArgs(..)
  , codeNoCanonKey
  , codeLetterUnreadable
  , codeTriageRefused
  , codeCanonUnwritable
  , codeCanonUnplannable
  , codeBundleUnusable
  , bundleOf
  , forkOf
  , partEvidence
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Hub.Letter (EnvelopeSigner(..),maxPartBytes,maxMessageParts
                       ,AckRecord(..),ReplyChannel(..)
                       ,openLetterAs)
import HBS2.Hub.Bridge
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.Repo.GitBundle (acceptBundle,isAncestor,stagePull,pullTip,pullRef)
import HBS2.Hub.Ingress
import HBS2.Hub.CLI.Publish (notPublishedYet)
import HBS2.Hub.CLI.Common (overRpc, refuse, saying, manifestCode
                           ,codeMailboxUnknown, codePeerSilent
                           ,withCanon, OnMissing(..)
                           ,blessed, committing, WriteStop(..)
                           ,signerFor, signingPair)
import HBS2.Hub.CLI.Ack (sendAck,AckTrouble(..))
import HBS2.Hub.CLI.Compose (Outbound(..))
import HBS2.Hub.CLI.Drop (dropMessage)
import HBS2.Hub.CLI.Argv (flagsAndSwitches,flagOnce,flagMaybe,flagSwitch,repoFlags,flagRepo,flagRepoMaybe)
import HBS2.Hub.Deny (loadBans,allowedBy,codeNoBanList)
import HBS2.Hub.Repo.Manifest (mailboxFor)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Peer.RPC.Client
import HBS2.Storage

import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.List (sortOn)
import Data.Maybe (fromMaybe,listToMaybe)
import System.Exit (die,exitWith,ExitCode(..))
import System.IO.Error (isResourceVanishedError)

-- | And what a machine with no key for the canon identity exits with.
codeNoCanonKey :: Int
codeNoCanonKey = 21

-- | The letter named is not one this node can read.
codeLetterUnreadable :: Int
codeLetterUnreadable = 22

-- | The letter names more attachments than this hub will walk.
--
-- Its own code and not 'codeTriageRefused', because nothing about the letter's
-- CONTENT was judged: this fires before the parts are measured, which is the
-- point of it. A hub that raises 'maxMessageParts' folds the same letter
-- afterwards.
codePartsTooMany :: Int
codePartsTooMany = 44

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

-- | The letter proposes a change whose objects are not usable: the bundle will
-- not fetch, or it fetches something other than what the contributor signed
-- for, or the signed base is not an ancestor of the signed tip.
--
-- Its own code because it is the contributor's doing and nothing on this side
-- fixes it. Canon is untouched when it fires: the check sits between the mint,
-- which writes nothing, and the commit, which is the only step that publishes.
codeBundleUnusable :: Int
codeBundleUnusable = 28

-- | The bundle a letter proposes, and the claims to check it against.
--
-- 'Nothing' for an issue, which proposes no objects, and 'Nothing' for a pull
-- request on the fork-pointer path, which names a fork to fetch over hbs2
-- rather than carrying an attachment (PEP-20). Both mean "there is nothing to
-- verify here", and they mean it for different reasons; what must not happen
-- is the third answer, a PR with a bundle that goes unchecked because this
-- said no.
--
-- Top level and exported for the reason every other decision in this package
-- is: it decides whether an accept verifies anything at all, and inside a
-- @where@ clause nothing could ask it.
bundleOf :: AuthorContent -> Maybe (HashRef, Text, Text, Text)
bundleOf = \case
  AOpen _ _ _ _ _ _ (Just c) _ -> fromCoords c
  ARevise _ c _                -> fromCoords c
  _                            -> Nothing
  where
    -- The hash alone: what the bundle path needs is the part to open, and the
    -- proof beside it is the bridge's business (requireParts) and the fold's.
    fromCoords c = do
      part <- prBundle c
      pure (ptPart part, prSourceRef c, prSourceTip c, prBase c)

-- | The FORK-PATH coordinates a letter proposes, when that is what it proposes.
--
-- The other half of 'bundleOf', and the reason it exists is that the two
-- Nothings it returns are not the same nothing. An issue proposes no objects;
-- a fork-path pull request proposes objects this build cannot fetch.
--
-- PEP-20 leaves fetching a fork to git3's remote helper, which has no
-- fetch-one-ref path, so nothing here verifies that the tip exists, that the
-- base is its ancestor, or that the fork key is a repository at all -- and
-- nothing stages it, because there are no objects to stage. All of which is
-- fine and none of which was SAID: the accept printed the same lines as for an
-- issue, and canon then carried a signed claim in every clone that nobody had
-- checked. The maintainer found out at @hub pr checkout@, which reported
-- nothing staged.
forkOf :: AuthorContent -> Maybe (Text, Text, Text)
forkOf = \case
  AOpen _ _ _ _ _ _ (Just c) _ -> fromCoords c
  ARevise _ c _                -> fromCoords c
  _                            -> Nothing
  where
    fromCoords c = do
      src <- prSource c
      -- A letter carrying both is a delta letter that also names a fork, and
      -- the bundle is what gets verified. Only the bundle-less one is this.
      case prBundle c of
        Just{}  -> Nothing
        Nothing -> pure (src, prSourceTip c, prBase c)

-- | What is known about each attachment a message carries, gathered before the
-- bridge is asked.
--
-- MEASURED FIRST AND OPENED SECOND, and only when the size is one this hub
-- would carry: opening means decrypting, and decrypting a part the gate is
-- about to refuse for its size is exactly the spend the gate exists to prevent
-- (PEP-18). An oversized part is reported at its size and comes back from the
-- bridge as @PartTooLarge@.
--
-- The BYTES are kept beside the evidence as well as the secret, because a pull
-- request's bundle is verified further down and decrypting it twice for want of
-- holding it once is the same spend the size gate is careful about.
--
-- TOP LEVEL AND EXPORTED, which is the point of it existing as a function.
-- 'igOpenPart' is what turns a fetched encrypted tree into the 'PartSecret'
-- the owner publishes into canon FOREVER, and while this loop was inside the
-- verb nothing could reach it: the stub in the test suite answers @Left@, so
-- every 'PartOpened' in the whole suite was a fixture literal and the arm that
-- chooses the published secret was run by nothing at all.
partEvidence :: MonadUnliftIO m
             => Ingress m
             -> [HashRef]
             -> m [(HashRef, (PartEvidence, Maybe LBS.ByteString))]
partEvidence ig hs = for hs $ \h -> do
  facts <- measurePart ig h
  (,) h <$> case facts of
    -- Not measurable is not fetched: the size is unknown, and zero is the only
    -- honest stand-in for a number nothing has said yet.
    Left _ -> pure (PartPending 0, Nothing)
    Right f
      | not (pfHere f)          -> pure (PartPending (pfSize f), Nothing)
      | pfSize f > maxPartBytes -> pure (PartLocked (pfSize f), Nothing)
      | otherwise -> igOpenPart ig h >>= \case
          Left _ -> pure (PartLocked (pfSize f), Nothing)
          Right (bs, sec) -> case mkPartSecret sec of
            Just s  -> pure (PartOpened (pfSize f) s, Just bs)
            -- A secret of the wrong length is not a key, so this node cannot
            -- open the part however the read went.
            Nothing -> pure (PartLocked (pfSize f), Nothing)

-- | What one accept was asked to fold.
--
-- A record and not a tuple since @--keep@ made it five values, two of which are
-- the same key type: a positional reading of that is a well-typed accept
-- against the wrong repository.
data AcceptArgs = AcceptArgs
  { aaMailbox :: Maybe HubKey   -- ^ absent: read it from the repository manifest
  , aaRepo    :: RepoRef
  , aaMessage :: HashRef
  , aaAs      :: Maybe HubKey   -- ^ a delegate's canon key (PEP-21)
    -- | Leave the letter in the mailbox after folding it.
    --
    -- For the operator who wants the queue to be the record of what arrived
    -- rather than of what is outstanding. It is not the default, because a
    -- queue that keeps everything it folded is a queue nobody can triage from
    -- after a week.
  , aaKeep    :: Bool
  }
  deriving stock (Eq,Show)

acceptUsage :: Doc ()
acceptUsage =
  "usage: hbs2-hub inbox accept --repo <key> --message <hash> [--mailbox <key>] [--as <key>] [--keep]"

-- | @hub inbox accept@.
acceptEntries :: forall c m . ( IsContext c
                              , MonadUnliftIO m
                              , HasStorage m
                              , HasClientAPI MailboxAPI UNIX m
                              , HasClientAPI LWWRefAPI UNIX m
                              , Exception (BadFormException c)
                              ) => MakeDictM c m ()
acceptEntries = do

  brief "fold one letter from the ingress mailbox into canon"
    $ args [ arg "string" "--mailbox mailbox-key"
           , arg "string" "--repo repo-key"
           , arg "string" "--message message-hash"
           , arg "string" "[--as canon-key]"
           , arg "string" "[--keep]" ]
    -- It was "Writes. Everything else in this tool reads", and that was true
    -- when accept was the only writer. Six more verbs write canon now, and a
    -- sentence a reader can check against `hub --help` is a sentence that has
    -- to stay true.
    $ desc ( "The verb that turns a stranger's request into the record."
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
             <> line <> "The letter is dropped from the mailbox afterwards, which"
             <> line <> "writes a tombstone and frees no disk (see hub inbox"
             <> line <> "reject). --keep skips it. The MAILBOX key signs a drop,"
             <> line <> "so a delegate signing canon with --as cannot make one:"
             <> line <> "the fold still stands and the letter stays, and this says"
             <> line <> "which happened rather than exiting non-zero for a"
             <> line <> "cleanup step after the work is published."
             <> line
             <> line <> "An acknowledgement goes to the contributor when their"
             <> line <> "letter asked for one and named their own mailbox (PEP-18"
             <> line <> "honours no other kind). It is signed by the canon key,"
             <> line <> "which is what makes it checkable, and it is a courtesy:"
             <> line <> "canon is the authority, so an ack that does not go out"
             <> line <> "costs a notification and nothing else."
             <> line
             <> line <> "This node's triage deny-list IS applied (see hub ban):"
             <> line <> "a letter whose INNER author is denied here is refused"
             <> line <> "before anything is minted. That list is local and"
             <> line <> "unsigned; PEP-21 defers a published ban to a later hub-meta." )
    $ entry $ bindMatch "hub:inbox:accept" $ nil_ \case
        (acceptArgs -> Just a) -> lift (accept a)
        _ -> liftIO (die (show acceptUsage))

  where

    accept a = do
      let repo = aaRepo a
          msg  = aaMessage a
          canonKey = fromMaybe repo (aaAs a)

      -- The mailbox named, or the one this repository declares (PEP-18). The
      -- repository is required by this verb either way, so the lookup always
      -- has something to ask about, and naming the mailbox skips it.
      mbox <- mailboxFor (aaMailbox a) repo
                >>= either (\e -> liftIO (refuse (show (pretty e)) (manifestCode e)))
                           pure

      creds <- signerFor canonKey
                 >>= maybe (liftIO (refuse (show ( "cannot sign as"
                                                    <+> pretty (AsBase58 canonKey)
                                                    <> line
                                                    <> "  no keyring here holds it as its own"
                                                    <+> "signing key" ))
                                           codeNoCanonKey))
                           pure

      -- Canon first, and from git rather than from anything remembered: the
      -- commit read here is the value the publish compares against, so reading
      -- it late is what makes the compare-and-swap mean anything.
      --
      -- A repository with no canon ref is the FIRST accept, not a failure. Every
      -- other way of not reading is, and they keep the codes `hub verify`
      -- already assigns them, so a script branches on one table.
      (parent, fr) <- withCanon TreatAsEmpty repo withGitCanon

      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX

      -- This node's triage deny-list (PEP-21). Local and unsigned, because a
      -- published ban needs a consensus change the spec defers to a later
      -- hub-meta,
      -- so what it does is refuse to FOLD a banned author rather than claim
      -- anything about them in canon.
      --
      -- Read before anything is opened, and a damaged list stops the accept:
      -- carrying on with an empty one would fold exactly the letters somebody
      -- took the trouble to ban.
      bans <- loadBans repo
                >>= either (\e -> liftIO (refuse (show ("the deny-list will not read:"
                                                          <+> pretty e))
                                                 codeNoBanList))
                           pure

      let allowed = allowedBy bans
          ig = overRpc sto api

      -- THE LETTER MUST BE IN THIS MAILBOX. Reading it by hash alone would
      -- work, and would accept any message-shaped block the peer has ever
      -- downloaded for any reason: a hash is not a claim about where the bytes
      -- came from. The bridge would still check the target and the signature,
      -- so the damage is bounded, but "the operator triaged this queue and
      -- picked this line out of it" is the thing the verb is supposed to mean,
      -- and it is cheap to make true.
      --
      -- The mailbox's live set and NOT the queue: this asks a membership
      -- question, and asking it through 'readInbox' decrypted up to
      -- 'maxInboxLetters' letters -- a keyman lookup, a secretbox open and a
      -- parse each -- to answer whether one hash is in a set the walk already
      -- had.
      inbox <- mailboxLive ig mbox
                 `catch` (\(e :: MailboxUnknown) -> liftIO (refuse (show e) codeMailboxUnknown))
                 `catch` (\(e :: PeerSilent)     -> liftIO (refuse (show e) codePeerSilent))

      -- Against everything the mailbox HOLDS, not against the page of it the
      -- queue opens. `readInbox` sorts the live set by hash and opens the first
      -- thousand, so asking the opened prefix made membership a function of how
      -- many letters a stranger had sent: a thousand messages ground to hashes
      -- below the honest one (a few thousand signatures, minutes of CPU) sorted
      -- every real letter past the cut, and this verb then answered "is not in
      -- mailbox" about a letter that is, with no flag to raise the bound. That
      -- one was permanent for the price of an afternoon; the drop below now
      -- clears the junk, which shortens the queue and does not fix the reasoning.
      unless (HS.member msg (mlLive inbox)) $
        liftIO $ refuse (show ( pretty msg <+> "is not in mailbox"
                                  <+> pretty (AsBase58 mbox)
                                  <> (if mlSettled inbox
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

      let ctx = TriageCtx (signingPair creds) allowed repo

      -- What is known about each attachment, gathered before the bridge is
      -- asked. Measured first and opened second, and only when the size is one
      -- this hub would carry: opening means decrypting, and decrypting a part
      -- the gate is about to refuse for its size is exactly the spend the gate
      -- exists to prevent (PEP-18). An oversized part is reported at its size
      -- and the refusal comes back as PartTooLarge.
      -- HOW MANY, before what any of them costs. The two bounds below are per
      -- part -- 'maxPartBytes' resident while a part is open, 'maxPartBlocks'
      -- reads while it is measured -- and the count is the sender's to pick, so
      -- without this the loop is unbounded work from one letter. The cheap
      -- direction is measurement, not size: a tree that spends its whole walk
      -- budget is about a kilobyte to send, so a few thousand named parts is
      -- a hundred million storage reads for a few megabytes of upload.
      --
      -- Before the walk and not inside it, because a count is known without
      -- touching the storage at all.
      when (length (lrParts raw) > maxMessageParts) $
        liftIO $ refuse (show ( pretty msg <+> "names" <+> pretty (length (lrParts raw))
                                  <+> "attachments, and this hub walks at most"
                                  <+> pretty maxMessageParts
                                <> line
                                <> "  nothing was read: the letter is left where it is." ))
                        codePartsTooMany

      opened <- partEvidence ig (lrParts raw)

      let evidence = [ (h, ev) | (h, (ev, _)) <- opened ]

      acc <- blessed codeTriageRefused
               (acceptLetter ctx (EnvelopeSigner (lrEnvelope raw)) (viewOf fr)
                             now msg (attachments (lrSecret raw) evidence)
                             (lrData raw))

      -- THE BUNDLE IS CHECKED BEFORE ANYTHING IS PUBLISHED, which is PEP-20's
      -- order ("verify privately, fold, stage"): minting is pure and writes
      -- nothing, so this sits between the mint and the commit and a refusal
      -- here leaves canon untouched and no seq spent.
      --
      -- What it establishes is the one thing the delta path rests on: the
      -- objects that arrive are the ones the contributor signed for. git's own
      -- hashing binds the content to the tip, so a bundle that produces the
      -- signed tip is theirs and one that does not is somebody else's.
      for_ (bundleOf (acContent acc)) $ \(part, ref, tip, base) -> do
        bytes <- case List.lookup part opened of
          Just (_, Just bs) -> pure (LBS.toStrict bs)
          -- The bridge admitted the letter, so the part opened; this is the
          -- letter naming a part its own message does not carry, which the
          -- gate answers as PartNotInMessage. Belt and braces, and it says
          -- which part rather than crashing on a lookup.
          _ -> liftIO $ refuse (show ("the letter names a bundle this message does"
                                        <+> "not carry:" <+> pretty part))
                               codeBundleUnusable

        _ <- acceptBundle Nothing bytes ref tip
               >>= either (\e -> liftIO (refuse (show (pretty e)) codeBundleUnusable)) pure

        -- The ancestor relation is guaranteed by how the bundle was built
        -- (base..source-ref), and it is checked anyway: the construction is
        -- the CONTRIBUTOR's, and this is the maintainer deciding whether to
        -- publish a number for it.
        anc <- isAncestor Nothing base tip
                 >>= either (\e -> liftIO (refuse (show (pretty e)) codeBundleUnusable)) pure
        unless anc $
          liftIO $ refuse (show ("the signed base is not an ancestor of the signed"
                                   <+> "tip; the range is not what it says it is"))
                          codeBundleUnusable

      -- The index is regenerated from the fold, plus the number this accept
      -- just minted. Derived rather than re-folded because the derivation is
      -- exactly what the fold would do: an open adds one thread with one
      -- number and changes no other, and nothing else mints a number at all.
      let minted = [ (n, t) | Just n <- [acNumber acc], ThreadScope t <- [acScope acc] ]
          numbers = sortOn fst (numberIndexOf fr <> minted)

      commit <- committing (WriteStop codeCanonUnplannable codeCanonUnwritable) parent
                  (frMeta fr) [(eventPath acc, acEvent acc)] numbers (message acc) now

      -- AFTER the commit, because staging needs the number and the number is
      -- not public until canon holds it (PEP-20). A failure here leaves a
      -- folded PR with no staged tip, which is a repeatable step rather than
      -- a lost one: the coordinates are in canon and the bundle's objects are
      -- already in this repository.
      -- The number to stage under, and it is NOT 'acNumber': a number is minted
      -- by an open and by nothing else, so a revise carried one through here as
      -- Nothing and fell out of the case below. The bundle was fetched, fsck'd
      -- and checked against the signed tip, canon was told the proposed tip had
      -- moved, and refs/hbs2/pulls/<n>/head went on pointing at the tip the
      -- maintainer had already asked the contributor to change. PEP-20 says
      -- plainly that a revise re-stages. The thread's number comes out of the
      -- fold this accept read, which is where every other reader gets it.
      let numbered t = [ n | (n, t') <- numberIndexOf fr, t' == t ]
          stageAt = case (acNumber acc, acScope acc) of
            (Just n, _)            -> Just n
            (Nothing, ThreadScope t) -> listToMaybe (numbered t)
            _                      -> Nothing

      staged <- case (stageAt, bundleOf (acContent acc)) of
        (Just n, Just (_, _, tip, _)) -> do
          let notStaged e = do
                liftIO $ hPutDoc stderr
                  ( "hbs2-hub: the event is in canon and the tip is not staged:"
                      <+> pretty e <> line
                      -- NOT "re-run": the same letter mints the same author box
                      -- id, so a second accept is refused as AlreadyInCanon and
                      -- never reaches this line. What is left is the one command
                      -- this would have run, and nothing else is pending.
                      <> "  nothing else is pending. To stage it by hand:" <> line
                      <> "    git update-ref " <> pretty (pullRef n)
                      <+> pretty tip <> line )
                pure Nothing
          -- The old side of the swap read from the ref itself rather than from
          -- canon: canon says what was proposed, and this is asking what is
          -- staged, which are different questions whenever a previous stage
          -- failed.
          pullTip Nothing n >>= \case
            Left e -> notStaged e
            Right old ->
              stagePull Nothing n tip old >>= \case
                Right () -> pure (Just (pullRef n))
                Left e   -> notStaged e
        _ -> pure Nothing

      -- THE CONTRIBUTOR IS TOLD, when they asked to be and this node can.
      --
      -- Before the drop and after the commit: what it reports is the fold, so
      -- it needs the fold to have happened, and it must not depend on the
      -- letter still being in the mailbox.
      --
      -- The channel comes from a second read of the same payload, vetted the
      -- same way the queue vets it: 'openLetterAs' honours a reply channel only
      -- when it names the inner author's own mailbox and the envelope signer is
      -- that key, so a rewrapper cannot redirect it and a sender cannot name a
      -- stranger's. Read here rather than carried through the bridge because it
      -- is transport, not content: it is deliberately outside the signed box so
      -- that it never reaches canon.
      let reply = case openLetterAs allowed (EnvelopeSigner (lrEnvelope raw))
                                    (lrData raw) of
                    Right (_, _, _, rc) -> rc
                    Left _              -> NoReply

          thread = case acScope acc of { ThreadScope t -> Just t ; _ -> Nothing }

          -- What canon says the thread's status is now. An open is open by
          -- definition; anything else inherits what the thread already had,
          -- which is the value this event did not change.
          statusNow t = case acContent acc of
            AOpen{} -> "open"
            _ -> maybe "open" (fromMaybe "open" . HM.lookup "status" . tsAttrs)
                       (HM.lookup t (frThreads fr))

      acked <- case thread of
        Nothing -> pure (Left AckNotAsked)
        Just t -> do
          let ackRec = AckRecord { akTarget = repo
                                 , akThread = t
                                   -- The number a person will use, which for a
                                   -- comment is the thread's and not this
                                   -- event's: only an open mints one.
                                 , akNumber = stageAt
                                 , akStatus = statusNow t
                                 , akMergeCommit = Nothing
                                 , akNote = Nothing
                                 }
          sendAck (Outbound sto api rpcTimeout) canonKey creds reply ackRec

      -- LAST, and only now: everything above can refuse, and a letter dropped
      -- before a refusal would be a letter neither folded nor in the queue.
      --
      -- A failure here is reported and does not change the exit code, for the
      -- same reason a failed stage does not: the accept happened. A caller that
      -- read non-zero as "not folded" would be wrong about the one verb whose
      -- work cannot be undone, and re-running it hits AlreadyInCanon.
      kept <- if aaKeep a
                then pure (Just "--keep was given")
                else dropMessage mbox msg >>= \case
                       Right () -> pure Nothing
                       Left e -> do
                         liftIO $ saying
                           ( "hbs2-hub: the event is in canon and the letter was not"
                               <+> "dropped:" <+> pretty e <> line
                               <> "  nothing else is pending. To drop it by hand:" <> line
                               <> "    hbs2-peer mailbox delete:message"
                               <+> pretty (AsBase58 mbox) <+> pretty msg <> line )
                         pure (Just "see stderr")

      -- The same guard the three read verbs carry, and this is the verb that
      -- needed it most. Everything above has happened: the event is minted, the
      -- bundle is verified, the commit has landed and the ref is staged. A
      -- closed pipe here (`hub inbox accept ... | head -1`) let the exception
      -- out through main, which exits 1, the code PEP-22 gives to a bad
      -- argument. A wrapper branching on that concluded the accept had failed,
      -- about the one verb whose work cannot be undone.
      liftIO $ handleJust
        (\e -> if isResourceVanishedError e then Just () else Nothing)
        (\_ -> exitWith (ExitFailure 141))
        do
          print $ vcat
            [ "accepted" <+> pretty (eventId (acEvent acc))
            , "seq" <+> pretty (acSeq acc)
            , maybe mempty (\n -> "number" <+> pretty n) (acNumber acc)
            , "commit" <+> pretty commit
            , maybe mempty ("staged" <+>) (fmap pretty staged)
            , maybe ("dropped from the mailbox:" <+> pretty msg)
                    (\why -> "left in the mailbox:" <+> pretty msg
                               <+> parens (pretty (why :: Text)))
                    kept
            , either (\e -> "no acknowledgement:" <+> pretty e)
                     (\h -> "acknowledged" <+> hashDoc h)
                     acked
            ]

      -- AND WHAT WAS NOT CHECKED, when nothing was.
      --
      -- A fork-path proposal passes through every step above without one of
      -- them looking at it: 'bundleOf' answers Nothing, so the verify block
      -- does not run, and there are no objects to stage. The event is in canon
      -- and every clone reads coordinates nobody confirmed. Said here rather
      -- than refused, because PEP-20 has the path and this build simply cannot
      -- fetch it (git3's helper has no fetch-one-ref); what is wrong is a
      -- report that looked exactly like a verified one.
      for_ (forkOf (acContent acc)) $ \(src, tip, base) -> liftIO $ saying
        ( "this is a FORK-PATH proposal and NOTHING ABOUT IT WAS VERIFIED."
            <> line <> "  source" <+> pretty (safeText src)
            <> line <> "  tip   " <+> pretty (safeText tip)
            <> line <> "  base  " <+> pretty (safeText base)
            <> line <> "  the tip was not fetched, the base was not shown to be its"
            <+> "ancestor," <> line <> "  and nothing is staged, so `hub pr checkout`"
            <+> "has nothing to check out." <> line
            <> "  Fetch the fork yourself before you merge anything."
            <> line )

      liftIO (saying (notPublishedYet <> line))

    -- A commit message a human reading `git log refs/hbs2/meta` can act on.
    -- Not a place to put anything authoritative: everything that is signed is
    -- in the files.
    message acc = "hub: " <> tshow (acNumber acc) <> " " <> tshow (eventId (acEvent acc))

    tshow :: Pretty a => a -> Text
    tshow = fromString . show . pretty

-- The flags in any order, and the message hash positionally.
--
-- Named flags for the two keys rather than positions, for the reason the
-- compose verb gives about its own: a repo key and a mailbox key are the same
-- type, so a swap is a well-typed accept against the wrong repository.
acceptArgs :: forall c . IsContext c => [Syntax c] -> Maybe AcceptArgs
acceptArgs syn = do
  kvs  <- flagsAndSwitches (repoFlags <> ["--mailbox","--message","--as"]) ["--keep"] syn
  -- Optional, unlike the repository: without it the manifest is read. A
  -- caller who names it is not asking to be corrected, so it wins and costs
  -- no lookup.
  mbox <- flagMaybe kvs "--mailbox" asKey
  repo <- flagRepo asKey kvs
  h    <- flagOnce kvs "--message" >>= asHash
  -- 'flagMaybe' and not a lookup that swallows a bad value: --as names the key
  -- this event is BLESSED with, and answering "not given" for "given, and not a
  -- key" fell back to the repo key and signed canon under a key the caller did
  -- not name.
  as   <- flagMaybe kvs "--as" asKey
  -- A switch, so `--keep --repo K` cannot bind the repository as its value.
  keep <- flagSwitch kvs "--keep"
  pure (AcceptArgs mbox repo h as keep)
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
    --
    -- The pairing itself, the repeated-flag refusal and the unknown-flag
    -- refusal are 'flagsOf' in HBS2.Hub.CLI.Argv, shared with every other verb
    -- that writes.
    asKey = \case
      SignPubKeyLike k -> Just k
      _                -> Nothing

    asHash = \case
      HashLike h -> Just h
      _          -> Nothing
