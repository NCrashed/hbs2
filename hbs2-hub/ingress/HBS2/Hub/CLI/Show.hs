-- 'contentDoc' below dispatches on every constructor of 'AuthorContent' with no
-- wildcard, on content an attacker composed. The library modules set this for
-- the same reason: a constructor added without a case here is a crash in front
-- of a maintainer rather than a build error.
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | @hub inbox show@: one submission, whole (PEP-22 "Maintain").
--
-- The queue next door prints one line per letter and no body, which is right
-- for a queue and wrong as the only view there is: it left @hub inbox accept@
-- as the first verb that reads a stranger's title and body, and it reads them
-- by publishing them into append-only canon. This is the verb that shows what
-- accept would fold, before it folds it.
--
-- READ-ONLY, like 'HBS2.Hub.CLI.Inbox' and unlike everything it sits between.
-- Nothing here writes canon, mints, or deletes.
--
-- EVERYTHING PRINTED IS A STRANGER'S, and it is printed at a terminal. So every
-- hash and key goes through 'hashDoc'/'keyDoc' (base58 is quadratic and none of
-- these fields is bounded by anything but a parse), every text goes through
-- 'safeText' (a title can hold an escape sequence), and the body is printed as a
-- quoted block, one line per line, so that a body cannot forge a line of this
-- report.
module HBS2.Hub.CLI.Show
  ( showEntries
  , showUsage
    -- * The parts that decide something
  , Shown(..)
  , showDoc
  , showNotes
  , ShowArgs(..)
  , showArgs
  , maxBodyLines
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold (frOrigins)
import HBS2.Hub.Letter (Disposition(..),maxMessageParts)
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.Ingress
import HBS2.Hub.Deny (loadBans,allowedBy,codeNoBanList)
import HBS2.Hub.CLI.Inbox (overRpc,refuse,saying,utcOf
                          ,codeMailboxUnknown,codePeerSilent)
import HBS2.Hub.CLI.Accept (codeLetterUnreadable)
import HBS2.Hub.CLI.Argv (flagsOf,flagOnce,flagMaybe)
import HBS2.Hub.CLI.Verify (codeOf)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.Maybe (isJust)
import Data.Text qualified as Text
import System.Exit (die,exitWith,ExitCode(..))
import System.IO.Error (isResourceVanishedError)

-- | The most lines of a body this prints.
--
-- A body is bounded at 'HBS2.Hub.Letter.maxInlineBody' by the reader that
-- decodes it, so the total is bounded already; what is not is how many LINES
-- those bytes are, and 32 KiB of newlines is 32 000 of them. Truncation is
-- counted and said, never silent: this is the view somebody reads before
-- signing.
maxBodyLines :: Int
maxBodyLines = 200

-- | What was gathered about one letter.
--
-- A record between the reading and the rendering, so that the rendering -- the
-- part that decides what a maintainer sees before they publish something -- is
-- a pure function a test can drive without a peer, a mailbox or a key.
data Shown = Shown
  { swView  :: LetterView
    -- | Every part the MESSAGE carries, measured, in the order it carries them.
    -- Measured and not opened: showing a letter must not cost what folding it
    -- would (PEP-18's size gate), and a part's size and presence are what
    -- decide whether an accept can proceed at all.
  , swParts :: [(HashRef, Either PartTrouble PartFacts)]
    -- | How many parts were named and NOT measured, because the letter names
    -- more than 'maxMessageParts'.
    --
    -- Counted and said rather than silently dropped, for the reason
    -- 'irTruncated' is: a truncated list is a WRONG list, not a shorter one,
    -- and this one is truncated by a number a stranger chose. Truncating at all
    -- is what stops a letter naming a few thousand trees from costing this
    -- read verb a walk apiece -- and @hub inbox show@ is exactly what somebody
    -- runs before deciding whether to accept.
  , swPartsUnmeasured :: Int
    -- | Whether canon already holds this letter, or 'Nothing' when no
    -- repository was named and canon was therefore not consulted.
  , swFolded :: Maybe Bool
  }
  deriving stock (Eq,Show)

showUsage :: Doc ()
showUsage =
  "usage: hbs2-hub inbox show --mailbox <key> --message <hash> [--repo <key>]"

showEntries :: forall c m . ( IsContext c
                            , MonadUnliftIO m
                            , HasStorage m
                            , HasClientAPI MailboxAPI UNIX m
                            , Exception (BadFormException c)
                            ) => MakeDictM c m ()
showEntries = do

  brief "show one letter from the ingress mailbox, whole"
    $ args [ arg "string" "--mailbox mailbox-key"
           , arg "string" "--message message-hash" ]
    $ desc ( "Read-only. Opens one letter and prints what it asks for,"
             <> line <> "including the title and the body the queue does not show."
             <> line
             <> line <> "The letter must be IN this mailbox. Reading it by hash"
             <> line <> "alone would show any message-shaped block the peer has"
             <> line <> "ever downloaded, and a hash is not a claim about where"
             <> line <> "the bytes came from."
             <> line
             <> line <> "Attachments are measured, not opened: a size and whether"
             <> line <> "the peer has it yet, which is what decides whether an"
             <> line <> "accept can proceed. Opening them is folding's cost."
             <> line
             <> line <> "--repo names whose deny-list to apply (hub ban) and whose"
             <> line <> "canon to ask whether this letter was already folded."
             <> line <> "Without it neither question is answered, and the report"
             <> line <> "says so rather than leaving \"would fold\" to be read as"
             <> line <> "permission." )
    $ entry $ bindMatch "hub:inbox:show" $ nil_ \case
        (showArgs -> Just sa) -> lift (showIt sa)
        _ -> liftIO (die (show showUsage))

  where

    showIt sa = do
      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX

      -- The deny-list, when the caller says which repository's. Applied through
      -- the ingress like the queue applies it, so that a denied author reads as
      -- denied HERE rather than as a letter that would fold.
      allowed <- case shRepo sa of
        Nothing -> pure (const True)
        Just repo -> loadBans repo >>= \case
          Right bans -> pure (allowedBy bans)
          Left e -> liftIO (refuse (show ("the deny-list will not read:"
                                            <+> pretty (safeText e)))
                                   codeNoBanList)

      let ig = (overRpc sto api) { igAllowed = allowed }
          mbox = shMailbox sa
          msg = shMessage sa

      -- The live set and not the queue: this is a membership question, and
      -- 'readInbox' would decrypt a page of the mailbox to answer it.
      live <- mailboxLive ig mbox
                `catch` (\(e :: MailboxUnknown) -> liftIO (refuse (show e) codeMailboxUnknown))
                `catch` (\(e :: PeerSilent)     -> liftIO (refuse (show e) codePeerSilent))

      unless (HS.member msg (mlLive live)) $
        liftIO $ refuse (show ( pretty msg <+> "is not in mailbox"
                                  <+> pretty (AsBase58 mbox)
                                  <> (if mlSettled live
                                        then mempty
                                        else ", which had not settled when it was read") ))
                        codeLetterUnreadable

      lv <- openMessage ig msg
              `catch` (\(e :: PeerSilent) -> liftIO (refuse (show e) codePeerSilent))

      -- A second read of the same message, for the parts alone: 'openMessage'
      -- answers what the letter ASKS FOR and drops what it CARRIES, which is
      -- the message's business and not the letter's. A letter that will not
      -- open has no parts to report, which is what the 'Left' is.
      raw <- rawMessage ig msg
               `catch` (\(e :: PeerSilent) -> liftIO (refuse (show e) codePeerSilent))

      -- Bounded, and the bound is the accept's: a letter this verb measures in
      -- full and the accept then refuses for its count would be a preview of a
      -- thing that cannot happen.
      let named = either (const []) lrParts raw
          (walked, unmeasured) = splitAt maxMessageParts named

      parts <- for walked $ \h -> (,) h <$> measurePart ig h

      folded <- for (shRepo sa) $ \repo ->
        withGitCanon (\cs -> readCanon cs repo) >>= \case
          -- No canon ref is not a failure: it is a repository whose first
          -- accept has not happened, and the honest answer is "not folded".
          Left NoCanonRef{} -> pure False
          Left e -> liftIO (refuse (show (pretty e)) (codeOf e))
          Right st -> pure (HS.member msg (frOrigins (stFold st)))

      let sw = Shown lv parts (length unmeasured) folded

      liftIO $ handleJust
        (\e -> if isResourceVanishedError e then Just () else Nothing)
        (\_ -> exitWith (ExitFailure 141))
        (mapM_ print (showDoc sw))

      liftIO $ for_ (showNotes (isJust (shRepo sa)) sw) (saying . (<> line))

-- | The letter itself, for stdout.
showDoc :: Shown -> [Doc ann]
showDoc sw =
     [ field "message" (hashDoc (lvMessage lv))
     , field "envelope" (maybe "-" keyDoc (lvEnvelope lv))
     ]
  <> letterDoc
  <> partsDoc (swParts sw)
  <> unmeasuredDoc
  <> canonDoc

  where
    lv = swView sw

    -- On stdout with the parts and not in the notes, because it is a fact
    -- about the list above rather than about the run: a reader counting the
    -- lines has to see that the count is short and by how much. Says the
    -- accept's answer too, since that is the next thing they will try.
    unmeasuredDoc
      | swPartsUnmeasured sw <= 0 = []
      | otherwise =
          [ field "parts-unmeasured"
                  ( pretty (swPartsUnmeasured sw)
                      <+> "more named and not walked; this letter names more than"
                      <+> pretty maxMessageParts <> "," <+> "so an accept refuses it" ) ]

    letterDoc = case lvLetter lv of
      -- Not a failure to stop on and not an empty report either: what could be
      -- established is above (which message, and whose envelope when the
      -- signature said so), and this is the rest.
      Left e -> [ field "unreadable" (pretty e) ]
      Right (author, content, disp) ->
           [ field "author" (keyDoc author)
           , field "event" (maybe "?" hashDoc (lvEventId lv))
             -- The author's own clock: advisory, unverifiable, and shown
             -- because a maintainer reads it, never sorted on (PEP-19).
           , field "at" (pretty (utcOf (authorTs content)))
           , field "op" (opDoc content)
           , field "triage" (dispDoc disp)
           ]
        <> contentDoc content

    canonDoc = case swFolded sw of
      Nothing -> []
      Just True -> [ field "canon" ("already folded; accept refuses it as"
                                      <+> "AlreadyInCanon") ]
      Just False -> [ field "canon" "not folded" ]

-- | What is true ABOUT the report above, for stderr.
--
-- A list rather than an action, like 'HBS2.Hub.CLI.Inbox.inboxNotes': a test can
-- ask what this run would say without capturing a handle.
showNotes :: Bool -> Shown -> [Doc ann]
showNotes listed sw
  | listed = []
  | otherwise = denyNote <> canonNote
  where
    -- Only when there is something for it to be about, like the queue's: the
    -- line it qualifies is "triage: would fold", and against a letter nothing
    -- would fold anyway it is a warning about a sentence that is not on screen.
    denyNote
      | folds =
          [ "hub: no deny-list was applied (it is kept per repository), so"
              <+> "\"triage: would fold\" means the admission rules would take"
              <+> "this letter, not that its author is allowed to send. Name the"
              <+> "repository with --repo to apply it." ]
      | otherwise = []

    -- This one is not conditional: a letter already in canon looks exactly like
    -- one that is not from here, and accepting it again is the mistake the
    -- answer would have prevented.
    canonNote =
      [ "hub: canon was not consulted, so nothing above says whether this letter"
          <+> "has already been folded. Name the repository with --repo." ]

    folds = case lvLetter (swView sw) of
      Right (_, _, FoldsToCanon) -> True
      _                          -> False

-- One label column wide enough for the longest of them, so the values line up
-- without prettyprinter's fill (which would align on the rendered width of a
-- key nobody bounds).
field :: Text -> Doc ann -> Doc ann
field k v = pretty (Text.justifyLeft 9 ' ' k) <+> v

-- Every text a stranger wrote, escaped. Named once, so that a field added
-- later has to go out of its way to print raw bytes.
text :: Text -> Doc ann
text = pretty . safeText

opDoc :: AuthorContent -> Doc ann
opDoc = \case
  AOpen{} -> "open"; AComment{} -> "comment"; ARevise{} -> "revise"
  ASet{} -> "set"; AClose{} -> "close"; AReopen{} -> "reopen"
  AMerge{} -> "merge"; ARedact{} -> "redact"
  ADelegate{} -> "delegate"; ARevoke{} -> "revoke"

dispDoc :: Disposition -> Doc ann
dispDoc = \case
  FoldsToCanon -> "would fold"
  RequestOnly  -> "a request: only the owner can act on it (hub issue close/label)"
  OwnerNative  -> "owner-only: not acceptable from a letter at all"

contentDoc :: AuthorContent -> [Doc ann]
contentDoc = \case
  AOpen repo kind title labels body part coords _ ->
       [ field "target" (keyDoc repo)
       , field "kind" (kindDoc kind)
       , field "title" (text title)
       ]
    <> labelsDoc labels
    <> coordsDoc coords
    -- Named "body-part" and not "part", because the parts the MESSAGE carries
    -- are listed separately: this is the letter saying which of them its body
    -- is, and a letter naming one its message does not carry is a thing the
    -- bridge answers rather than a thing this should hide.
    <> partRefDoc "body-part" part
    <> bodyDoc body

  AComment thread replyTo body part _ ->
       [ field "thread" (hashDoc thread) ]
    <> maybe [] (\e -> [ field "reply-to" (hashDoc e) ]) replyTo
    <> partRefDoc "body-part" part
    <> bodyDoc body

  ARevise thread coords _ ->
    field "thread" (hashDoc thread) : coordsDoc (Just coords)

  ASet thread attr value _ ->
    [ field "thread" (hashDoc thread)
    , field "attr" (text attr)
    , field "value" (text value)
    ]

  AClose thread note _ -> field "thread" (hashDoc thread) : bodyDoc note
  AReopen thread note _ -> field "thread" (hashDoc thread) : bodyDoc note

  AMerge thread commit into _ ->
    [ field "thread" (hashDoc thread)
    , field "merge" (text commit)
    , field "into" (text into)
    ]

  ARedact repo e _ ->
    [ field "target" (keyDoc repo), field "redacts" (hashDoc e) ]

  ADelegate repo k _ ->
    [ field "target" (keyDoc repo), field "key" (keyDoc k) ]

  ARevoke repo k _ ->
    [ field "target" (keyDoc repo), field "key" (keyDoc k) ]

  where
    kindDoc = \case { HubIssue -> "issue" ; HubPR -> "pr" }

    -- Said to be a request every time they are shown. A letter's labels are
    -- what the author ASKS for: the fold does not apply them, because a
    -- stranger must not label their own submission (PEP-18), and a report that
    -- printed them beside the title would be showing a thread property that
    -- does not exist.
    labelsDoc [] = []
    labelsDoc ls =
      [ field "labels" (text (Text.intercalate ", " ls)
                          <+> "(requested; only an owner-signed set applies them)") ]

    coordsDoc Nothing = []
    coordsDoc (Just c) =
         [ field "onto" (text (prOnto c))
         , field "from" (text (prSourceRef c))
         , field "tip" (text (prSourceTip c))
         , field "base" (text (prBase c))
         ]
      <> maybe [] (\s -> [ field "source" (text s) ]) (prSource c)
      <> partRefDoc "bundle" (prBundle c)

    -- The part and not its proof: the proof is what the bridge checks
    -- ('provesPart') and what the fold drops a letter for, and printing a
    -- second hash beside every attachment would say nothing a maintainer can
    -- act on. Whether the proof held is the disposition, above.
    partRefDoc _ Nothing = []
    partRefDoc name (Just p) = [ field name (hashDoc (ptPart p)) ]

-- | A body as a quoted block, one line per line.
--
-- QUOTED, so that a body cannot forge a line of this report: every line of it
-- starts with a marker no field line has. Escaped per line rather than whole,
-- because 'safeText' escapes a newline (it is a control character, and in a
-- title that is exactly right) and a body's newlines are its layout.
bodyDoc :: Maybe Text -> [Doc ann]
bodyDoc Nothing = []
bodyDoc (Just b)
  | Text.null b = [ field "body" "(empty)" ]
  | otherwise = field "body" mempty : shown <> more
  where
    ls = Text.lines b
    shown = [ "  |" <+> text l | l <- List.take maxBodyLines ls ]
    more
      | length ls <= maxBodyLines = []
      | otherwise = [ "  |" <+> "..." <+> pretty (length ls - maxBodyLines)
                        <+> "more line(s) not shown" ]

partsDoc :: [(HashRef, Either PartTrouble PartFacts)] -> [Doc ann]
partsDoc ps = [ field "part" (hashDoc h <+> facts f) | (h, f) <- ps ]
  where
    facts = \case
      Left t -> pretty t
      Right f -> pretty (pfSize f) <+> "bytes"
                   <+> (if pfHere f then "here" else "not fetched yet")

-- | @--mailbox <key> --message <hash> [--repo <key>]@.
--
-- The same three the reject path takes, and every one of them behind a flag for
-- the same reason: a mailbox key, a repo key and a message hash are all
-- thirty-two bytes of base58.
data ShowArgs = ShowArgs
  { shMailbox :: HubKey
  , shMessage :: HashRef
  , shRepo    :: Maybe RepoRef
  }
  deriving stock (Eq,Show)

showArgs :: forall c . IsContext c => [Syntax c] -> Maybe ShowArgs
showArgs syn = do
  kvs  <- flagsOf ["--mailbox","--message","--repo"] syn
  mbox <- flagOnce kvs "--mailbox" >>= asKey
  h    <- flagOnce kvs "--message" >>= asHash
  repo <- flagMaybe kvs "--repo" asKey
  pure (ShowArgs mbox h repo)
  where
    asKey  = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    asHash = \case { HashLike x -> Just x ; _ -> Nothing }
