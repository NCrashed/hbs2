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
  , letterContract
  , letterOrder
  , Verdict(..)
  , verdictOf
  , openErrorCode
  , ShowArgs(..)
  , showArgs
  , maxBodyLines
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold (frOrigins,frThreads,ThreadState(..))
import HBS2.Hub.Render (contractVersion,renderDocument,opName,declaredAt,b58,key58,kindOf)
import HBS2.Hub.Letter (Disposition(..),LetterError(..),maxMessageParts)
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.Ingress
import HBS2.Hub.Deny (denyingFor,denyUnreadable,codeNoBanList)
import HBS2.Hub.Bridge (TriageError(..))
import HBS2.Hub.CLI.Common (overRpc,refuse,saying,utcOf
                           ,codeMailboxUnknown,codePeerSilent
                           ,withCanon,OnMissing(..)
                           ,mailboxDoubt,doubtDoc,notInMailboxDoc)
import HBS2.Hub.CLI.Accept (codeLetterUnreadable)
import HBS2.Hub.CLI.Inbox (dispositionName)
import HBS2.Hub.CLI.Argv (badArgs,flagsAndSwitches,flagSwitch,flagOnce,repoFlags,flagRepoMaybe)
import HBS2.Hub.CLI.Verify (codeOf)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import Data.Aeson (Value(..),object,(.=))
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Control.Monad (join)
import Data.List qualified as List
import Data.Maybe (isJust)
import Data.Word (Word64)
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
    -- 'irOmitted' is: a truncated list is a WRONG list, not a shorter one,
    -- and this one is truncated by a number a stranger chose. Truncating at all
    -- is what stops a letter naming a few thousand trees from costing this
    -- read verb a walk apiece -- and @hub inbox show@ is exactly what somebody
    -- runs before deciding whether to accept.
  , swPartsUnmeasured :: Int
    -- | Whether canon already holds this letter, or 'Nothing' when no
    -- repository was named and canon was therefore not consulted.
  , swFolded :: Maybe Bool
    -- | The repository named with @--repo@: whose deny-list was applied and
    -- whose canon was asked. 'Nothing' means neither question was answered.
  , swRepo :: Maybe RepoRef
    -- | Every thread canon holds, with its number when it has one. Empty when
    -- canon was not consulted, which 'swRepo' tells apart from a canon that
    -- holds no threads.
    --
    -- The whole map and not the one entry this letter names, so that the
    -- question "which thread, and is it there" is answered by the pure half
    -- where a test can ask it.
  , swCanonThreads :: HashMap ThreadId (Maybe Word64)
  }
  deriving stock (Eq,Show)

showUsage :: Doc ()
showUsage =
  "usage: hbs2-hub inbox show --mailbox <key> --message <hash> [--repo <key>] [--json]"

showEntries :: forall c m . ( IsContext c
                            , MonadUnliftIO m
                            , HasStorage m
                            , HasClientAPI MailboxAPI UNIX m
                            , Exception (BadFormException c)
                            ) => MakeDictM c m ()
showEntries = do

  brief "show one letter from the ingress mailbox, whole"
    $ args [ arg "string" "--mailbox mailbox-key"
           , arg "string" "--message message-hash"
           , arg "string" "[--repo repo-key]", arg "string" "[--json]" ]
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
             <> line <> "permission."
             <> line
             <> line <> "--json prints one `letter` document instead (contract 1):"
             <> line <> "the identity, the whole body unescaped and untruncated,"
             <> line <> "the attachments, and triage.verdict, one of would_fold,"
             <> line <> "pending, already_folded, refused or unreadable, with"
             <> line <> "triage.reason {code, detail} for all but the first. The"
             <> line <> "verdict is a preview: accept runs checks a read cannot."
             <> line <> "Notes still go to stderr." )
    $ entry $ bindMatch "hub:inbox:show" $ nil_ \case
        (showArgs -> Just sa) -> lift (showIt sa)
        other -> liftIO (badArgs showUsage other)

  where

    showIt sa = do
      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX

      -- The deny-list, when the caller says which repository's. Applied through
      -- the ingress like the queue applies it, so that a denied author reads as
      -- denied HERE rather than as a letter that would fold.
      allowed <- denyingFor (shRepo sa)
                   >>= either (\e -> liftIO (refuse (show (denyUnreadable e)) codeNoBanList))
                              pure

      let ig = (overRpc sto api) { igAllowed = allowed }
          mbox = shMailbox sa
          msg = shMessage sa

      -- The live set and not the queue: this is a membership question, and
      -- 'readInbox' would decrypt a page of the mailbox to answer it.
      live <- mailboxLive ig mbox
                `catch` (\(e :: MailboxUnknown) -> liftIO (refuse (show e) codeMailboxUnknown))
                `catch` (\(e :: PeerSilent)     -> liftIO (refuse (show e) codePeerSilent))

      -- A HOLE IN THE TREE MAKES THIS ANSWER WRONG IN BOTH DIRECTIONS, and this
      -- verb read only the live set. Said and not refused, unlike the accept
      -- next door: showing a letter writes nothing, so the useful thing is for
      -- the reader to know how much to believe the answer they got. It goes to
      -- stderr, so a --json consumer's stdout is still one document.
      liftIO $ for_ (mailboxDoubt live) $ \d ->
        saying (doubtDoc mbox msg d <> line)

      -- The refusal repeats the caveat, because this is the direction in which
      -- it may be a lie: the entry naming this message can be in the part of
      -- the tree that did not read.
      unless (HS.member msg (mlLive live)) $
        liftIO $ refuse (show (notInMailboxDoc mbox msg live)) codeLetterUnreadable

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

      canon <- for (shRepo sa) $ \repo ->
        -- TreatAsEmpty: this ASKS canon a question ("is this letter already
        -- folded"), and a repository whose first accept has not happened
        -- answers no.
        withCanon TreatAsEmpty repo withGitCanon <&> snd

      let sw = Shown { swView = lv
                     , swParts = parts
                     , swPartsUnmeasured = length unmeasured
                     , swFolded = HS.member msg . frOrigins <$> canon
                     , swRepo = shRepo sa
                     , swCanonThreads = maybe mempty (fmap tsNumber . frThreads) canon
                     }

      -- The whole document or nothing, like the queue's: a consumer handed
      -- half of one by a closed pipe would parse a letter with no verdict.
      liftIO $ handleJust
        (\e -> if isResourceVanishedError e then Just () else Nothing)
        (\_ -> exitWith (ExitFailure 141))
        (if shJson sa
           then LBS.putStr (renderDocument letterOrder (letterContract sw))
           else mapM_ print (showDoc sw))

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
      -- The SENTENCE the accept prints, not the constructor it prints it for:
      -- this line exists to tell a maintainer what the next command will do,
      -- and 'TriageError' has a hand-written Pretty for exactly that moment.
      Just True -> [ field "canon" ("already folded; accept refuses it:"
                                      <+> pretty AlreadyInCanon) ]
      Just False -> [ field "canon" "not folded" ]

-- | What an accept of this letter would come to, as far as this read can tell.
data Verdict =
    WouldFold                -- ^ nothing this read can see stops an accept
  | Pending Text             -- ^ an attachment is not here yet: come back later
  | AlreadyFolded            -- ^ canon holds it; an accept refuses it
  | Refused Text Text        -- ^ an accept refuses it: a code, and why in words
  | Unreadable Text Text     -- ^ the letter did not open: a code, and why
  deriving stock (Eq,Show)

-- | The verdict, from what was gathered.
--
-- A PREVIEW, and the document says so: the accept runs the bridge, which asks
-- more than this read can (whether the signer may bless, whether a revise is
-- by the thread's author, whether the fold would drop the payload). What is
-- here is what a read verb can establish without a key: whether the letter
-- opened, what it asks for, whether canon has it, whether it is aimed at the
-- repository that was named and at a thread that repository holds, and
-- whether its attachments are here.
--
-- In the order the accept meets them, so that the reason given is the one an
-- accept would print first.
verdictOf :: Shown -> Verdict
verdictOf sw = case lvLetter (swView sw) of
  Left e -> case e of
    BadLetterHere AuthorDenied ->
      Refused "author_denied" "the author is on this repository's deny-list (hub ban)"
    _ -> Unreadable (openErrorCode e) (Text.pack (show (pretty e)))
  Right (_, content, disp)
    | swFolded sw == Just True -> AlreadyFolded
    | RequestOnly <- disp ->
        Refused "request_only"
                "a request: only the owner can act on it (hub inbox honour)"
    | OwnerNative <- disp ->
        Refused "owner_native" "owner-only: not acceptable from a letter at all"
    | Just repo <- swRepo sw, Just t <- targetOf content, t /= repo ->
        Refused "wrong_repo" "the letter was written for another repository"
    | Just _ <- swRepo sw, Just th <- authorThread content
    , not (HM.member th (swCanonThreads sw)) ->
        Refused "unknown_thread" (Text.pack (show (pretty UnknownThread)))
    | swPartsUnmeasured sw > 0 ->
        Refused "too_many_parts"
                ("the letter names more than " <> Text.pack (show maxMessageParts)
                   <> " attachments")
    | any (not . here . snd) (swParts sw) ->
        Pending "an attachment has not been fetched yet"
    | otherwise -> WouldFold
  where
    here = either (const False) pfHere

-- | The repository an op names, for the four that name one.
targetOf :: AuthorContent -> Maybe RepoRef
targetOf = \case
  AOpen r _ _ _ _ _ _ _ -> Just r
  ARedact r _ _ -> Just r
  ADelegate r _ _ -> Just r
  ARevoke r _ _ -> Just r
  AComment{} -> Nothing; ARevise{} -> Nothing; ASet{} -> Nothing
  AClose{} -> Nothing; AReopen{} -> Nothing; AMerge{} -> Nothing

-- | Why a letter did not open, as a word a program branches on.
--
-- The constructor names, spelled the way JSON keys are. A code and not the
-- sentence, because the sentences are for people and get reworded.
openErrorCode :: OpenError -> Text
openErrorCode = \case
  NotFetched      -> "not_fetched"
  NotAMessage     -> "not_a_message"
  NotForUs        -> "not_for_us"
  GroupKeyByRef   -> "group_key_by_ref"
  Undecipherable  -> "undecipherable"
  BadEnvelopeSig  -> "bad_envelope_sig"
  BadLetterHere e -> case e of
    MalformedPayload       -> "malformed_payload"
    UnsupportedVersion _   -> "unsupported_version"
    BadInnerSig            -> "bad_inner_sig"
    UndecodableContent _ _ -> "undecodable_content"
    NotALetter             -> "not_a_letter"
    NotAnAck               -> "not_an_ack"
    AuthorDenied           -> "author_denied"
    UntrustedAck           -> "untrusted_ack"
    UnrelatedAck           -> "unrelated_ack"

partTroubleCode :: PartTrouble -> Text
partTroubleCode = \case
  PartNotFetchedYet   -> "not_fetched"
  PartNotATree _      -> "not_a_tree"
  PartUnreadable _    -> "unreadable"
  PartTooManyBlocks _ -> "too_many_blocks"

-- | The letter as a document (@hub inbox show --json@).
--
-- A DOCUMENT UNDER THE SAME COUNTER as the queue, and like the queue not
-- derived from canon: it is one node's reading of one message at one moment,
-- with that node's keys and deny-list. Two maintainers may disagree about it
-- and neither is wrong.
--
-- EVERY KEY IS ALWAYS PRESENT, null when it does not apply, so that a consumer
-- tells "this op has no body" from "this renderer forgot the body" by reading
-- and not by inference. The op-specific ones are flat rather than nested per
-- op, and named the way the thread document names the same values.
--
-- THE BODY IS WHOLE. The terminal report stops at 'maxBodyLines' because a
-- screen is a screen; here the bound is the reader's, 'maxInlineBody', which
-- already holds. Nothing is escaped either: JSON carries control characters as
-- @\\u001b@, and a program deciding whether to accept has to see exactly what
-- it would be accepting.
letterContract :: Shown -> Value
letterContract sw = object $
  [ "contract"   .= contractVersion
  , "document"   .= ("letter" :: Text)
  , "message"    .= b58 (lvMessage lv)
  , "envelope"   .= fmap key58 (lvEnvelope lv)
  , "event"      .= fmap b58 (lvEventId lv)
  , "repo"       .= fmap key58 (swRepo sw)
  , "unreadable" .= case lvLetter lv of
      Left e  -> object [ "code" .= openErrorCode e
                        , "detail" .= (Text.pack (show (pretty e)) :: Text) ]
      Right _ -> Null
  , "triage"     .= triage
  , "folded"     .= swFolded sw
  , "attachments" .= fmap part (swParts sw)
  , "attachments_unmeasured" .= swPartsUnmeasured sw
  ]
  <> letterFields
  where
    lv = swView sw

    triage = object
      [ "verdict" .= verdictName v
      , "reason"  .= reason v
      , "disposition" .= either (const Null) (\(_,_,d) -> String (dispositionName d))
                                (lvLetter lv)
      , "deny_list_applied" .= isJust (swRepo sw)
      , "canon_consulted"   .= isJust (swRepo sw)
      ]
      where v = verdictOf sw

    verdictName = \case
      WouldFold -> "would_fold" :: Text
      Pending{} -> "pending"
      AlreadyFolded -> "already_folded"
      Refused{} -> "refused"
      Unreadable{} -> "unreadable"

    reason = \case
      WouldFold -> Null
      Pending d -> why "part_not_fetched" d
      AlreadyFolded -> why "already_in_canon" (Text.pack (show (pretty AlreadyInCanon)))
      Refused c d -> why c d
      Unreadable c d -> why c d
      where why c d = object [ "code" .= c, "detail" .= d ]

    part (h, f) = object $ ("part" .= b58 h) : case f of
      Left t  -> [ "size" .= Null, "here" .= False
                 , "trouble" .= object [ "code" .= partTroubleCode t
                                       , "detail" .= (Text.pack (show (pretty t)) :: Text) ] ]
      Right x -> [ "size" .= pfSize x, "here" .= pfHere x, "trouble" .= Null ]

    -- What the letter says, every key present whatever the op.
    letterFields = case lvLetter lv of
      Left _ -> fields Nothing
      Right (author, content, _) -> fields (Just (author, content))

    fields mc =
      [ "author"      .= fmap (key58 . fst) mc
      , "op"          .= fmap (opName . snd) mc
      , "declared_at" .= fmap (declaredAt . authorTs . snd) mc
      , "subject"     .= maybe Null (subject . snd) mc
      , "body"        .= (mc >>= bodyOf . snd)
      , "body_part"   .= (mc >>= fmap (b58 . ptPart) . bodyPartOf . snd)
      , "labels_requested" .= maybe [] (labelsOf . snd) mc
      , "reply_to"    .= (mc >>= replyOf . snd)
      , "pr"          .= maybe Null (coordsOf . snd) mc
      , "attr"        .= (mc >>= \case { (_, ASet _ a _ _) -> Just a ; _ -> Nothing })
      , "value"       .= (mc >>= \case { (_, ASet _ _ x _) -> Just x ; _ -> Nothing })
      , "merge_commit" .= (mc >>= \case { (_, AMerge _ c _ _) -> Just c ; _ -> Nothing })
      , "merged_into" .= (mc >>= \case { (_, AMerge _ _ i _) -> Just i ; _ -> Nothing })
      , "redacts"     .= (mc >>= \case { (_, ARedact _ e _) -> Just (b58 e) ; _ -> Nothing })
      , "key"         .= (mc >>= \case
                            (_, ADelegate _ k _) -> Just (key58 k)
                            (_, ARevoke _ k _)   -> Just (key58 k)
                            _                    -> Nothing)
      ]

    -- WHAT IT IS ABOUT. The thread an open would start is its own event-id,
    -- so an open has a thread too; its number is known only once canon holds
    -- it, and only when canon was asked.
    subject c = object
      [ "thread" .= fmap b58 thread
      , "number" .= (thread >>= \t -> join (HM.lookup t (swCanonThreads sw)))
      , "in_canon" .= case (swRepo sw, thread) of
          (Just _, Just t) -> Just (HM.member t (swCanonThreads sw))
          _                -> Nothing
      , "target" .= fmap key58 (targetOf c)
      , "kind"   .= case c of { AOpen _ k _ _ _ _ _ _ -> Just (kindOf k) ; _ -> Nothing }
      , "title"  .= case c of { AOpen _ _ t _ _ _ _ _ -> Just t ; _ -> Nothing }
      ]
      where
        thread = case c of
          AOpen{} -> lvEventId lv
          _       -> authorThread c

    bodyOf = \case
      AOpen _ _ _ _ b _ _ _ -> b
      AComment _ _ b _ _ -> b
      AClose _ n _ -> n
      AReopen _ n _ -> n
      _ -> Nothing

    bodyPartOf = \case
      AOpen _ _ _ _ _ p _ _ -> p
      AComment _ _ _ p _ -> p
      _ -> Nothing

    labelsOf = \case
      AOpen _ _ _ ls _ _ _ _ -> ls
      _ -> []

    replyOf = \case
      AComment _ r _ _ _ -> fmap b58 r
      _ -> Nothing

    coordsOf = \case
      AOpen _ _ _ _ _ _ (Just co) _ -> coords co
      ARevise _ co _ -> coords co
      _ -> Null

    coords co = object
      [ "onto"        .= prOnto co
      , "base"        .= prBase co
      , "tip"         .= prSourceTip co
      , "source"      .= prSource co
      , "source_ref"  .= prSourceRef co
      , "bundle_part" .= fmap (b58 . ptPart) (prBundle co)
      ]

-- | The letter document's key order: identity first, then the decision, then
-- what the letter says, then the parts.
letterOrder :: [Text]
letterOrder =
  [ "message", "envelope", "event", "repo", "unreadable"
  , "triage", "verdict", "reason", "code", "detail", "disposition"
  , "deny_list_applied", "canon_consulted", "folded"
  , "author", "op", "declared_at", "subject"
  , "thread", "number", "in_canon", "target", "kind", "title"
  , "body", "body_part", "labels_requested", "reply_to"
  , "pr", "onto", "base", "tip", "source", "source_ref", "bundle_part"
  , "attr", "value", "merge_commit", "merged_into", "redacts", "key"
  , "attachments", "part", "size", "here", "trouble"
  , "attachments_unmeasured"
  ]

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
    -- | The letter as a document rather than as a report. See 'letterContract'.
  , shJson    :: Bool
  }
  deriving stock (Eq,Show)

showArgs :: forall c . IsContext c => [Syntax c] -> Maybe ShowArgs
showArgs syn = do
  kvs  <- flagsAndSwitches (repoFlags <> ["--mailbox","--message"]) ["--json"] syn
  mbox <- flagOnce kvs "--mailbox" >>= asKey
  h    <- flagOnce kvs "--message" >>= asHash
  repo <- flagRepoMaybe asKey kvs
  asJson <- flagSwitch kvs "--json"
  pure (ShowArgs mbox h repo asJson)
  where
    asKey  = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    asHash = \case { HashLike x -> Just x ; _ -> Nothing }
