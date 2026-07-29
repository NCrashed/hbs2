-- The taxonomy in 'outcome' is a total function over 'TriageError' and the
-- only mechanical check of that is the incomplete-patterns warning, which
-- would otherwise be a pattern-match failure inside a triage loop rather than
-- a build error. Here it is a build error.
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | The triage bridge: Tier B letter to Tier A canon event (PEP-19
-- "Folding Tier B letters into Tier A").
--
-- This is the only place where a letter becomes canon, and it is a gate, not
-- a stamp. It refuses before minting, for two different reasons:
--
--   * Authority. A bridge that blessed whatever arrived would let anyone
--     author an owner-native op (a @redact@ hiding any event) and hand it to
--     the owner to sign. The deny-list is applied to the INNER author, since
--     an envelope-key ban is evaded by rewrapping (PEP-18); the one exception
--     is a letter whose schema this build cannot parse, where there is no
--     inner author to name (see 'tcAllowed').
--
--   * Order. The fold is a single ascending pass over @seq@, so a reply must
--     carry a higher @seq@ than the event it answers. Minting a reply whose
--     thread is not in canon yet produces an event that can never be
--     admitted: the fold drops it as dangling, and re-folding changes
--     nothing, so the submission is lost for good. The bridge therefore
--     requires the thread to exist already, which means a maintainer folds
--     an opening letter before the replies to it.
--
-- The bridge never re-signs the author box of an accepted letter. It takes
-- the inner box out verbatim and wraps an owner-signed canon box around it,
-- so the event-id the sender computed is the event-id canon ends up with,
-- and the authorship proof survives.
--
-- Everything here is pure: minting is a function of the current canon plus
-- the letter, so two folders given the same inputs mint the same events.
--
-- The property the module owes its caller is that it never mints an event
-- the fold will drop, and that holds by enumeration over 'DropReason':
--
--   * @BadAuthorSig@, @BadCanonSig@, @UndecodableAuthor@,
--     @UndecodableCanon@, @IdMismatch@: impossible for boxes minted here.
--   * @WrongTarget@, @BadKind@, @BadThread@, @UnknownRedact@, @DupId@,
--     @BadRevise@, @UnauthorizedDelegate@: refused by the gate, on both the
--     letter path and the owner-native one.
--   * @UnauthorizedCanon@: refused by checking this folder's own key against
--     the maintainer set canon reports, which is why 'CanonView' carries it.
--   * @BadStamp@: a number is minted only for an open; the cursor is refused
--     once it reaches the top of its range ('CursorExhausted'); and the
--     @folded-ts@ about to be stamped is refused once it passes the ceiling
--     canon admits ('StampOutOfRange'). Those are the three stamped fields,
--     and every gate is in 'requireCanon' or 'requireStamp', which all four
--     entry points go through.
--
-- Anything that weakens one of those checks weakens the property, so the
-- view must come from the fold ('viewOf') rather than be reconstructed.
-- The accumulated view must also stay equal to the rebuilt one: it is a
-- cache of what the fold would say, and a divergence is a bug even where
-- nothing currently reads the field that diverged.
module HBS2.Hub.Bridge
  ( CanonCursor(..)
  , TriageCtx(..)
  , ThreadFacts(..)
  , Outcome(..)
  , Pending(..)
  , LetterParts
  , OwnerParts
  , PartEvidence(..)
  , noMessageParts
  , attachments
  , attachmentsNoKey
  , noOwnAttachments
  , ownAttachments
  , outcome
  , EventScope(..)
    -- The constructor is deliberately not exported: a view is a cache of what
    -- the fold would say, and the module's promise (never mint what the fold
    -- drops) is only as good as that. Build one with 'viewOf' or 'emptyView'.
    -- The fields are readable, and updatable, which is an escape hatch a test
    -- or a tool can still use knowingly rather than by reaching for the
    -- nearest constructor.
  , CanonView( cvCursor, cvThreads, cvEvents, cvMaintainers, cvOrigins
             , cvLastFolded, cvRepo )
  , Accepted(..)
  , ackFor
  , TriageError(..)
  , viewOf
  , emptyView
  , initialCursor
  , cursorFrom
  , authorizedCanon
  , acceptLetter
  , honourRequest
  , honourWith
  , ownerEvent
  , eventPath
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold
import HBS2.Hub.Letter

import HBS2.Data.Types.Refs (HashRef)
import HBS2.Data.Types.SignedBox (SignedBox)
import HBS2.Net.Auth.Credentials

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Control.Monad (when)
import Data.Maybe (fromMaybe,isJust,isNothing)
import Data.Text (Text)
import Data.Word (Word64)

-- | What the next canon box gets stamped with.
--
-- Both counters come from canon itself ('viewOf'), not from node-local
-- state, so a folder that rebuilds from the repo mints the same values a
-- previous one would have. PEP-19 leaves uniqueness to there being a single
-- publisher; @hub verify@ reports it if that ever stops being true.
data CanonCursor = CanonCursor
  { ccrNextSeq    :: Word64
  , ccrNextNumber :: Word64
  }
  deriving stock (Eq,Show)

-- | What stays fixed while triaging a batch: who is folding, for which
-- repo, and which authors are allowed. Separated from the per-letter
-- arguments because those three are the same for every letter in a run,
-- and a positional list long enough to hide a swap is how the wrong key
-- ends up in the wrong slot.
data TriageCtx = TriageCtx
  { tcCanon   :: (HubKey, PrivKey 'Sign HubScheme)  -- ^ the folding key
    -- | May a letter from this key be folded (PEP-21)? Asked about the INNER
    -- author, which is the identity a ban is about, except on a letter this
    -- build cannot parse: there is no inner author to name there, so it is
    -- asked about the envelope signer instead ('openLetterAs'). A list holding
    -- only inner authors therefore never matches a sender who rotates envelope
    -- keys, which is the one case where an envelope ban does anything.
  , tcAllowed :: HubKey -> Bool
  , tcRepo    :: RepoRef
  }

-- | What the bridge remembers about a thread already in canon.
data ThreadFacts = ThreadFacts
  { tfKind   :: HubKind
  , tfAuthor :: HubKey       -- ^ author of record: only they may revise
  , tfNumber :: Maybe Word64 -- ^ the human number, for acknowledging a reply
    -- | The thread's status as of the last accept.
    --
    -- Here because an acknowledgement carries it (PEP-18) and the layer that
    -- sends one has an 'Accepted' and a view, not a fold: without this it
    -- would have to re-fold canon after every letter, or write a second
    -- implementation of the last-writer-wins rule and have it drift. The fold
    -- keeps the same value in @tsAttrs@, and the two are checked against each
    -- other on every accept the property test makes.
  , tfStatus :: Text
  }
  deriving stock (Eq,Show)

-- | Where an event lives in the canon tree (PEP-19 layout).
data EventScope =
    ThreadScope ThreadId  -- ^ under @threads/<thread-id>/@
  | RepoScope             -- ^ under @repo/@: delegate and revoke
  deriving stock (Eq,Show)

-- | As much of canon as the bridge needs to decide before minting: what to
-- stamp, which threads a reply may name, and which events already exist.
data CanonView = CanonView
  { cvCursor  :: CanonCursor
    -- | What the bridge needs to know about each thread: the kind (revise is
    -- PR-only), the author of record (only they may revise), and the number
    -- (an ack for a reply reports the thread's number, which the sender
    -- cannot compute and 'acNumber' only carries on an open).
  , cvThreads :: HashMap ThreadId ThreadFacts
    -- | Every admitted event and the thread it belongs to, taken straight
    -- from the fold. Reconstructing it from the materialized threads would
    -- silently omit every event that leaves no visible trace (a @set@, a
    -- @merge@, a note-less @close@), so a redact of one would be refused and
    -- a resent revise would not be caught as a duplicate. That divergence
    -- only appears after a restart, when the view is rebuilt from canon.
  , cvEvents  :: HashMap EventId (Maybe ThreadId)
    -- | The canon keys still authorized at the end of the log. A folder must
    -- check its OWN key against this: a maintainer whose delegation has been
    -- revoked can still sign, but the fold will not admit what they sign, so
    -- minting would consume a triaged letter and produce nothing.
  , cvMaintainers :: HashSet HubKey
    -- | Letters already folded, so a triage loop re-reading the mailbox
    -- after a restart does not honour the same request twice.
  , cvOrigins :: HashSet HashRef
    -- | The @folded-ts@ of the last event in canon, so the next stamp cannot
    -- go below it. Like the cursor, it comes from canon rather than from the
    -- folder's own state: an honest maintainer whose clock is corrected
    -- backwards would otherwise mint canon that @hub verify@ complains about
    -- forever, and there is no later event that can fix a signed timestamp.
  , cvLastFolded :: Word64
    -- | The repo this view describes. Kept here rather than passed
    -- alongside, because a view of another repo is otherwise
    -- indistinguishable: RepoRef and HubKey are the same type.
  , cvRepo :: RepoRef
  }
  deriving stock (Eq,Show)

-- | The view of an empty repo.
--
-- Seeded with the owner exactly as the fold seeds its own maintainer set, so
-- that a view carried forward across accepts stays equal to one rebuilt with
-- 'viewOf' from the same canon. Any divergence between the two is a bug: the
-- accumulated view is a cache of what the fold would say.
emptyView :: RepoRef -> CanonView
emptyView repo =
  CanonView initialCursor HM.empty HM.empty (HS.singleton repo) HS.empty 0 repo

-- | May this key bless events for this repo right now?
--
-- The repo comes from the view, which knows it: taking it separately would
-- be a second source of truth that could disagree.
authorizedCanon :: CanonView -> HubKey -> Bool
authorizedCanon view k = k == cvRepo view || HS.member k (cvMaintainers view)

initialCursor :: CanonCursor
initialCursor = CanonCursor 1 1

-- | Derive the view from folded canon.
--
-- The repo comes from the fold itself, so there is one place to get it
-- wrong rather than two.
viewOf :: FoldResult -> CanonView
viewOf fr = CanonView
  { cvCursor  = cursorFrom fr
  , cvThreads = fmap facts (frThreads fr)
  , cvEvents  = frAdmitted fr
  , cvMaintainers = frMaintainers fr
  , cvOrigins = frOrigins fr
  , cvLastFolded = frLastFolded fr
  , cvRepo = frOwner fr
  }
  where
    -- The status comes out of the same attribute map the render contract reads
    -- (PEP-22), so the cached copy cannot mean anything else. An open always
    -- has one; the default is for canon written by something that did not.
    facts t = ThreadFacts (tsKind t) (tsAuthor t) (tsNumber t)
                (fromMaybe "" (HM.lookup "status" (tsAttrs t)))

-- | The next seq and number to mint, given folded canon.
cursorFrom :: FoldResult -> CanonCursor
cursorFrom fr = CanonCursor (frMaxSeq fr + 1) (frMaxNumber fr + 1)

data TriageError =
    -- | The letter could not be opened at all.
    BadLetter LetterError
    -- | The letter asks for something a stranger's signature cannot carry
    -- into canon: a request the owner must re-author ('honourRequest'), or
    -- an owner-native op that should never have arrived as a letter.
  | NotAcceptable Disposition
    -- | A request the owner may honour: carries what was asked, so the
    -- caller does not reopen the letter to find out.
  | Requested Pending
    -- | The letter was authored for a different repository.
  | WrongRepo
    -- | A reply naming a thread that is not in canon yet, or an owner event
    -- referring to one. Fold the opening letter first: minting now would
    -- produce an event the fold can never admit.
  | UnknownThread
    -- | This exact author box is already in canon. What is left for this to
    -- catch once the origin is checked first: a resend under a NEW Mailbox
    -- message, which is what rewrapping produces (PEP-18) and what an
    -- owner-native op re-minted from identical content produces too.
  | AlreadyInCanon
    -- | A revise from someone other than the thread's author of record, or
    -- on a thread that is not a PR.
  | NotAuthorOfRecord
    -- | Kind and payload disagree: a PR without coordinates, an issue
    -- carrying them, or a PR-only op on an issue thread. The fold would drop
    -- it, so it is refused before minting.
  | BadContent
    -- | This key may not bless events for this repo: either it was never a
    -- maintainer, its delegation has been revoked, or it is not the owner
    -- key and the op is delegate/revoke (PEP-19 rule 5).
  | UnauthorizedForRepo
    -- | A redact naming an event that is not in canon. Distinct from
    -- 'UnknownThread': the target is an event, and the fold reports this
    -- case separately too.
  | UnknownTarget
    -- | The cursor has reached the top of its range, so the next stamp would
    -- be one the fold refuses ('BadStamp'). Unreachable in practice, but it
    -- is the one case that would otherwise leave the module's promise
    -- incomplete.
  | CursorExhausted
    -- | The @folded-ts@ this mint would stamp is above the ceiling canon
    -- admits ('maxFoldedTs'): either this folder's clock reads a century out,
    -- or canon already holds a stamp that high and the clamp cannot go under
    -- it. The same case as 'CursorExhausted' for the third stamped field, and
    -- an operator's to fix either way: no later event can lower a signed
    -- timestamp, so what fixes it is compaction.
  | StampOutOfRange
    -- | The content the owner signed names a different thread than the
    -- request being honoured. Editing what was asked for is fine; moving it
    -- elsewhere is not honouring it.
  | ThreadMismatch
    -- | This Mailbox message has already been folded into canon. Distinct
    -- from 'AlreadyInCanon', which is about the event: honouring a request
    -- re-authors it under the owner's clock, so the same letter honoured
    -- twice yields two different event-ids and only the origin ties them
    -- back to one message. Both paths check it, so this also covers a letter
    -- simply presented twice, which is the ordinary case after a restart.
  | AlreadyHonoured
    -- | The view describes a different repository than the context.
  | ViewRepoMismatch
    -- | The content references an encrypted part and the caller supplied no
    -- group secret to unlock it. The letter is fine; the caller has to pass
    -- the secret its Mailbox message came with, so this is a retry.
    --
    -- Minting anyway would put a signed reference to bytes nobody can read
    -- into canon, permanently: the part cannot be re-encrypted (a new hash
    -- would break the author signature) and once the message is deleted from
    -- the mailbox the secret is gone (PEP-19 "Attachments in public canon").
  | MissingPartSecret
    -- | The content names a part the message does not carry. Not repairable:
    -- a later message cannot add a part to an author box already signed.
  | PartNotInMessage HashRef
    -- | The caller passed the no-attachments value for content that names an
    -- attachment, so there is no evidence to judge against. A caller bug: the
    -- builders that carry evidence are the ones with a message behind them.
  | NoAttachmentsSupplied
    -- | The message carries the part but this node has not fetched the tree
    -- yet. Nothing is wrong; come back when it has.
  | PartNotFetched HashRef
    -- | The part is over what this hub will carry ('maxPartBytes'). Local
    -- policy like 'BodyTooLarge' and parked for the same reason: another hub
    -- may take it, and the reference would be permanent here.
  | PartTooLarge HashRef
    -- | The secret offered for the parts cannot be a key: it is the wrong
    -- length. Minting it would put a reference to bytes nobody can read into
    -- canon, permanently.
  | BadPartSecret
    -- | The secret offered for the parts is the secret over the message
    -- payload. Publishing it would publish the letter's transport envelope,
    -- and with it the sender's private reply address, to every clone forever
    -- (PEP-18). A caller bug, and the one this type exists to catch.
  | MessageSecretOffered
    -- | A text field over the limit triage will carry inline
    -- ('maxInlineBody'): the field name, so the sender can be told what to
    -- move to an attachment.
  | BodyTooLarge Text
    -- | A hash-shaped field that is not a hash ('validHashRef'), named. Not a
    -- threshold like the one above and not local policy either: a 'HashRef'
    -- takes any length on the wire, so this was the one field that could carry
    -- fifty kilobytes past every size gate, into a signed author box, and into
    -- a canon file no reader would ever accept again.
  | MalformedRef Text
    -- | Only the repo owner key may delegate or revoke (PEP-19 rule 5), and
    -- this folding key is a delegate. Distinct from 'UnauthorizedForRepo':
    -- that one another folder can satisfy, this one nobody but the owner can,
    -- so a delegate's tooling asking for it is a bug in the caller.
  | OwnerKeyRequired
    -- | A refusal about what the CALLER composed rather than about the letter.
    --
    -- Every check on the honour path runs twice over, once on what the letter
    -- asked for and once on what the owner is actually signing, and the same
    -- refusal means opposite things on the two sides. A thread mismatch, an
    -- oversized note, an unknown thread or a name that is not canonical, raised
    -- against content triage wrote, says nothing whatever about the letter, and
    -- a loop that folds then deletes would delete a good submission over a bug
    -- in a maintainer's own tooling. Everything the owner-native path refuses
    -- is composed too, since there is no letter there at all.
    --
    -- Wrapping rather than a constructor per site: the site is the fact, and
    -- the inner error still says which check it was.
  | Composed TriageError
    -- | The request carries text the owner would be signing as their own
    -- words. 'honourRequest' will not do that; use 'honourWith' with content
    -- the maintainer actually wrote.
  | NeedsReview
    -- | A multi-valued attribute whose value is not in canonical form
    -- ('normalizeAttr'). The same set of labels in another order is other
    -- bytes and so another event-id, which is exactly what the canonical form
    -- exists to prevent. The bridge will not normalize it silently: what the
    -- owner signs must be what the caller meant to sign.
  | UnnormalizedValue Text
  deriving stock (Eq,Show)

-- | What a triage loop should do with a refusal.
--
-- Treating every 'Left' the same way is wrong in both directions: retrying
-- spam forever, or discarding a letter that only arrived out of order. PEP-18
-- is explicit that an early reply is not rejected, it waits in the mailbox
-- until its thread is folded.
--
-- Five outcomes rather than three, because three collapsed cases that need
-- different handling. 'Retry' and 'Park' both mean "keep the letter", but a
-- loop that treats them alike re-verifies the signature of every piece of
-- undecodable garbage on every pass, which an attacker produces by signing
-- random bytes. 'Abort' means "keep the letter AND stop": a caller that
-- silently retried its own wiring bug would spin, and one that discarded would
-- delete valid letters (PEP-21 folds then deletes).
data Outcome =
    Retry     -- ^ nothing is wrong with the letter; the repo is not ready yet
    -- | Nothing is wrong with the letter, and nothing that happens to this
    -- repo will change the answer: it needs a newer build, or a different
    -- setting, or a resend. Keep it, but stop looking at it every pass.
    --
    -- The set has no durable marker yet, so a loop has to remember it out of
    -- band, and 'BodyTooLarge' is the first way a stranger can grow it.
  | Park
  | Decide    -- ^ a valid request the maintainer must act on ('honourWith')
  | Discard   -- ^ the letter can never become canon here
    -- | The caller is wired wrong. Not about the letter at all: leave it
    -- alone, stop the loop, and fix the caller.
  | Abort
  deriving stock (Eq,Show)

-- | A request the maintainer has to rule on, already opened.
--
-- Carried out of the refusal so a triage UI can show what is being asked
-- and by whom without opening the letter, and verifying its signature,
-- a third time.
data Pending = Pending
  { pdAuthor  :: HubKey
  , pdContent :: AuthorContent
  , pdReply   :: ReplyChannel
  }
  deriving stock (Eq)

-- Deliberately partial, for the same reason 'Accepted' is: this sits inside
-- 'TriageError', which callers log, and a derived instance would print the
-- body and the reply channel that PEP-18 keeps out of public canon.
instance Show Pending where
  show p = "Pending {author = " <> show (pdAuthor p)
        <> ", op = " <> opName (pdContent p)
        <> ", thread = " <> show (authorThread (pdContent p))
        <> ", reply = " <> haveReply (pdReply p)
        <> "}"
    where
      haveReply NoReply = "none"
      haveReply ReplyTo{} = "present"
      opName = \case
        AOpen{} -> "open"; AComment{} -> "comment"; ARevise{} -> "revise"
        ASet{} -> "set"; AClose{} -> "close"; AReopen{} -> "reopen"
        AMerge{} -> "merge"; ARedact{} -> "redact"
        ADelegate{} -> "delegate"; ARevoke{} -> "revoke"

outcome :: TriageError -> Outcome
outcome = \case
  -- Order, not validity: fold the thread's opening letter and come back.
  UnknownThread   -> Retry
  UnknownTarget   -> Retry
  -- Our own key, not the letter's: another folder may be able to.
  UnauthorizedForRepo -> Retry
  -- The letter is from a newer schema than this build speaks. Discarding it
  -- would strand a perfectly good submission that an upgrade could fold, and
  -- letters already sitting in mailboxes are exactly the ones this happens
  -- to (see 'hubMsgVersion').
  BadLetter (UnsupportedVersion _)              -> Park
  BadLetter (UndecodableContent _ Undecodable)  -> Park
  -- Trailing bytes are not what an honest newer sender produces, so this
  -- one is tampering rather than a version gap.
  BadLetter (UndecodableContent _ TrailingData) -> Discard
  -- Nor is a payload signed for another domain. This is Discard rather than
  -- Park for a reason that is not the neighbours' reason: domains are never
  -- renumbered, so a newer sender's letter carries the domain this build
  -- already knows and fails on its payload instead ('Undecodable'). Anything
  -- that reaches here was signed as something else, and no upgrade changes
  -- that.
  BadLetter (UndecodableContent _ WrongDomain)  -> Discard
  -- Both are normal triage outcomes, not failures: a maintainer has to look.
  Requested _ -> Decide
  NeedsReview -> Decide
  -- A letter carrying an op only the owner may author: the letter is at fault
  -- and no later pass changes that. Reached from both 'acceptLetter' and the
  -- check on what the letter asked for, and it means the same thing in both.
  NotAcceptable OwnerNative -> Discard
  -- Whereas this one only ever comes from the honour paths, since
  -- 'acceptLetter' folds a FoldsToCanon letter rather than refusing it: a
  -- valid opening letter was handed to the wrong entry point, and a loop that
  -- folds then deletes would delete it. (`NotAcceptable RequestOnly` is
  -- unreachable through `acceptLetter`, which raises `Requested` first.)
  NotAcceptable _   -> Abort
  -- Not about the letter at all: whatever the check was, it ran against
  -- something the caller wrote. Stop and fix the caller, and leave the letter
  -- exactly where it is.
  Composed _        -> Abort
  BadLetter _       -> Discard
  WrongRepo         -> Discard
  AlreadyInCanon    -> Discard
  NotAuthorOfRecord -> Discard
  BadContent        -> Discard
  ThreadMismatch    -> Discard
  AlreadyHonoured   -> Discard
  -- A dead reference in a letter anyone can send. Discard, because the caller
  -- has already supplied its evidence about the message by this point and
  -- there is nothing for it to fix; stopping the loop instead would let one
  -- stranger wedge triage with a single letter.
  PartNotInMessage _   -> Discard
  -- The sender chose an attribute name or value that is not in canonical form,
  -- so no owner can honour it as sent and no later pass changes that. Raised
  -- against what triage composes it is 'Composed' instead, which is a stop.
  UnnormalizedValue _  -> Discard
  -- These two are the caller's own doing.
  NoAttachmentsSupplied -> Abort
  BadPartSecret        -> Abort
  -- Two causes that look identical from here: a sender who encrypted their own
  -- parts with the message key, and a caller that wrapped one secret twice.
  -- Discarding would have the second delete every letter carrying an
  -- attachment, and with it the only copy of the part secret; aborting would
  -- hand the loop to the first. Park is the one answer that costs neither.
  MessageSecretOffered -> Park
  -- Local policy, not consensus: another hub may carry a body or a part this
  -- one will not ('maxInlineBody', 'maxPartBytes'). Deleting a letter over a
  -- threshold this build picked would be irreversible for a decision that is
  -- not canon's.
  BodyTooLarge _     -> Park
  PartTooLarge _     -> Park
  -- Not a threshold: nobody can fold this letter, here or anywhere, and no
  -- later pass changes what the sender signed.
  MalformedRef _     -> Discard
  -- The letter is intact and the caller can supply what is missing.
  MissingPartSecret  -> Retry
  PartNotFetched _   -> Retry
  -- Wiring, not letters. Retrying a view built for another repo would spin on
  -- a bug, and discarding would throw away a valid letter; a cursor at the end
  -- of its range needs an operator, not another pass.
  ViewRepoMismatch    -> Abort
  OwnerKeyRequired    -> Abort
  CursorExhausted   -> Abort
  StampOutOfRange   -> Abort

-- Mark a refusal as being about what the caller composed rather than about the
-- letter it was answering. Idempotent by construction: nothing wraps twice,
-- because the checks under it never look at a letter.
composed :: Either TriageError a -> Either TriageError a
composed = either (Left . Composed) Right

-- Refuse an author box canon already holds: the fold would drop the second
-- copy as a duplicate.
seenAlready :: CanonView -> EventId -> Either TriageError ()
seenAlready view eid
  | HM.member eid (cvEvents view) = Left AlreadyInCanon
  | otherwise                     = Right ()

-- Refuse a letter whose message has already been folded. 'seenAlready' does
-- not cover this: honouring a request re-authors it under the owner's clock,
-- so the same letter honoured twice yields two different event-ids, and only
-- the origin ties them back to one message.
honouredAlready :: CanonView -> HashRef -> Either TriageError ()
honouredAlready view origin
  | HS.member origin (cvOrigins view) = Left AlreadyHonoured
  | otherwise                         = Right ()

-- | What the caller can vouch for about an event's encrypted attachments.
--
-- Three separate facts, because they fail differently. A part the message
-- never carried is a dead reference and always will be; a part not fetched yet
-- is a wait; a missing or malformed secret is a key the caller has to go and
-- get. All three are permanent if minted, because the reference lives inside a
-- signed author box (PEP-19 "Attachments in public canon"), which is why the
-- bridge takes the evidence rather than a bare secret.
data PartsOf = PartsOf
  { poSecret  :: Maybe PartSecret  -- ^ group secret for the part trees
    -- | The secret over the Mailbox message's own payload, when there is a
    -- message. Never published, and carried only to be refused: PEP-18 gives
    -- the parts a secret of their own, and if the two arrive equal then the
    -- caller is handing over the key to the envelope, whose back-channel
    -- clauses must never reach canon. Nothing about the bytes says which is
    -- which, so this is the only check that can catch it.
  , poMessage :: Maybe MessageSecret
    -- | The parts the caller vouches for, and what it knows about each. On the
    -- letter path these are the message's @messageParts@; on an owner-native
    -- event there is no message, and the caller is vouching for what it just
    -- created.
  , poCarried :: HashMap HashRef PartEvidence
  }
  deriving stock (Eq)

-- | What the caller knows about ONE part.
--
-- Per part rather than per message, because a message is fetched a tree at a
-- time and the answer for one attachment is not the answer for the next. The
-- shape also refuses to let a caller claim more than it knows: there is no way
-- to say "fetched" without saying how big, which is the number the gate needs
-- and the one a caller cannot have without having looked.
data PartEvidence =
    PartHere Word64      -- ^ present in local storage, this many bytes
    -- | Carried by the message and not fetched yet, of this declared size.
    --
    -- The size is not optional here either, and that is what makes the limit
    -- worth anything: a tree declares its size in its root block, which is one
    -- fetch, so a caller can know it before pulling the payload. Without it the
    -- gate could only fire after the download, and a stranger naming a hundred
    -- gigabytes would have the hub spend the hundred gigabytes and only then
    -- park the letter. What is bounded has to be what triage spends, not only
    -- what canon keeps.
  | PartPending Word64
  deriving stock (Eq,Show)

-- Deliberately partial, like 'Show' on 'PartSecret' and for the same reason:
-- 'poMessage' is the more dangerous of the two secrets and it is plain bytes,
-- so a derived instance would put it in a log the first time anyone traced a
-- refusal.
instance Show PartsOf where
  show p = "PartsOf {secret = " <> maybe "none" (const "present") (poSecret p)
        <> ", message-secret = " <> maybe "none" (const "present") (poMessage p)
        <> ", carried = " <> show (HM.size (poCarried p))
        <> ", fetched = " <> show (length [ () | PartHere _ <- HM.elems (poCarried p) ])
        <> "}"

-- | A letter's attachments.
--
-- Opaque, and the two paths have separate types, because the message secret
-- cannot be optional here and a constructor in reach makes it optional. The
-- letter builders all demand it; the one that does not is 'OwnerParts', where
-- there is no message and therefore nothing to confuse the parts secret with.
newtype LetterParts = LetterParts PartsOf
  deriving newtype (Eq,Show)

-- | An owner-native event's attachments.
newtype OwnerParts = OwnerParts PartsOf
  deriving newtype (Eq,Show)

-- | A message that carries no attachments.
--
-- It takes the message secret like every other letter builder, and that is the
-- whole of the point. A reader of a letter always holds one, because it read
-- the letter with it, so an argument-less value here would be a way of saying
-- "I have no evidence" that reads as "there is nothing to check". Those are
-- different answers: a letter naming a part its message does not carry is the
-- sender's doing and must not stop the loop, while a caller that never wired
-- the parts through is a bug that must. Only 'noOwnAttachments' can say the
-- second thing now, and on that path there is no message to be wrong about.
noMessageParts :: MessageSecret -> LetterParts
noMessageParts msg = LetterParts (PartsOf Nothing (Just msg) HM.empty)

-- | What the message carries, and what the caller knows about each part.
--
-- One builder rather than one per state of the download, because a message is
-- fetched a tree at a time: a letter with two attachments, one of them here,
-- was not expressible before and the convenient builder answered "fetched" for
-- everything it was given, which is the answer the gate exists to check.
attachments
  :: PartSecret                   -- ^ the secret the PARTS are encrypted with
  -> MessageSecret                -- ^ the secret @messageData@ was encrypted with, to refuse
  -> [(HashRef, PartEvidence)]    -- ^ the message's parts
  -> LetterParts
attachments s msg hs = LetterParts (PartsOf (Just s) (Just msg) (HM.fromList hs))

-- | The same, but the caller has no key for them.
attachmentsNoKey :: MessageSecret -> [(HashRef, PartEvidence)] -> LetterParts
attachmentsNoKey msg hs = LetterParts (PartsOf Nothing (Just msg) (HM.fromList hs))

-- | An owner-native event with no attachments.
noOwnAttachments :: OwnerParts
noOwnAttachments = OwnerParts (PartsOf Nothing Nothing HM.empty)

-- | Attachments the owner created itself.
--
-- There is no message secret to compare against here, so the check that the
-- parts secret is not the message secret cannot run: this path takes the
-- caller's word. It is not the impossible-to-get-wrong case it may look like,
-- since an owner-native @open@ or @comment@ can carry a body-part like any
-- other; PEP-19 records deriving the secret from the part tree's own group key
-- as the fix that would make the question unaskable.
ownAttachments :: PartSecret -> [(HashRef, PartEvidence)] -> OwnerParts
ownAttachments s hs = OwnerParts (PartsOf (Just s) Nothing (HM.fromList hs))

-- Refuse to publish a reference to encrypted bytes with no way to read them,
-- and refuse a reference to bytes that are not there at all.
requireParts :: PartsOf -> AuthorContent -> Either TriageError ()
requireParts po content
  -- Told nothing at all about attachments, for content that names one. That
  -- is the caller reaching for the empty value rather than wiring the parts
  -- through, and it is the only shape in which a caller bug and a hostile
  -- letter are indistinguishable, so it is the only one that stops the loop.
  | referencesPart content, toldNothing = Left NoAttachmentsSupplied
  | otherwise = do
      mapM_ present (eventParts content)
      if referencesPart content then secretOK else Right ()
  where
    -- Told nothing means exactly that: no secret, no parts AND no message
    -- behind them. A message that carried no attachments is not the same
    -- thing, and reading it as one let any stranger wedge the loop by naming
    -- a part in a letter whose message has none. Only reachable from
    -- 'noOwnAttachments' now: every letter builder demands the message
    -- secret, which is exactly the evidence this asks for.
    toldNothing = isNothing (poSecret po)
               && HM.null (poCarried po)
               && isNothing (poMessage po)

    -- Past that, the caller has supplied evidence about the message, so a part
    -- the evidence does not list is the letter's doing and no later pass
    -- changes it. Anyone can send such a letter; treating it as a caller bug
    -- would let one stranger stop triage for good.
    -- Size first, and for both states: the answer to "too big" does not change
    -- once it is downloaded, so refusing it before is strictly better and is
    -- the only order in which the limit bounds anything but canon.
    present h = case HM.lookup h (poCarried po) of
      Nothing                              -> Left (PartNotInMessage h)
      Just (PartPending n) | n > maxPartBytes -> Left (PartTooLarge h)
                           | otherwise        -> Left (PartNotFetched h)
      Just (PartHere n)    | n > maxPartBytes -> Left (PartTooLarge h)
                           | otherwise        -> Right ()

    secretOK = case poSecret po of
      Nothing -> Left MissingPartSecret
      Just sec
        | maybe False (sameSecret sec) (poMessage po) -> Left MessageSecretOffered
        -- Checked here as well as in 'mkPartSecret', because a secret can
        -- reach this point without passing through it: 'Serialise' decodes one
        -- unchecked so that canon somebody else wrote can be read, and
        -- compaction re-stamps events by reading the old canon boxes.
        | not (usablePartSecret sec)                 -> Left BadPartSecret
        | otherwise                                  -> Right ()

-- Refuse a field triage will not carry inline. A local policy, not an
-- admission rule (see 'maxInlineBody'), enforced here because the bridge is
-- the last gate before something is in every clone forever.
requireSize :: AuthorContent -> Either TriageError ()
requireSize content = maybe (Right ()) (Left . BodyTooLarge) (oversizedField content)

-- Refuse a hash-shaped field that is not a hash. Beside the size gate rather
-- than inside it because it is not a threshold: the field is malformed, no hub
-- would take it, and a letter carrying one can never be folded anywhere.
requireRefs :: AuthorContent -> Either TriageError ()
requireRefs content = maybe (Right ()) (Left . MalformedRef) (malformedRef content)

-- Refuse to sign a multi-valued attribute the caller did not canonicalize.
-- The fold cannot fix one (the value is inside a signed author box) and must
-- not drop it either, so it would be admitted and reported forever.
requireNormalized :: AuthorContent -> Either TriageError ()
requireNormalized = \case
  ASet _ k v _ | not (normalizedAttr k v) -> Left (UnnormalizedValue k)
  _                                       -> Right ()

-- Would signing this verbatim put someone else's words in the owner's mouth?
-- A note materializes as a comment authored by whoever signed the event, and
-- an attribute is the requester's choice of both name and value.
verbatimUnsafe :: AuthorContent -> Bool
verbatimUnsafe = \case
  AClose _ note _  -> isJust note
  AReopen _ note _ -> isJust note
  ASet{}           -> True
  _                -> False

-- Refuse to mint a number the cursor cannot produce. Only an open mints
-- one, so only an open is checked.
requireStamp :: CanonView -> AuthorContent -> Either TriageError ()
requireStamp view content
  | isJust (mintedNumber content (cvCursor view))
  , ccrNextNumber (cvCursor view) == maxBound = Left CursorExhausted
  | otherwise = Right ()

-- Refuse to mint under a key canon will not accept as a blesser, or with a
-- seq or a folded-ts the fold would reject.
requireCanon :: RepoRef -> CanonView -> Word64 -> HubKey -> Either TriageError ()
requireCanon repo view folded k
  | cvRepo view /= repo                    = Left ViewRepoMismatch
  | not (authorizedCanon view k)           = Left UnauthorizedForRepo
  | ccrNextSeq cur == maxBound             = Left CursorExhausted
  -- The mirror of the fold's ceiling on the same field. Without it the one
  -- stamped value with no gate on this side was the one the caller supplies
  -- directly, and a folder whose clock read past the ceiling would mint an
  -- event the fold drops as 'BadStamp', and then keep minting: the accepted
  -- view carries the stamp forward, so every later letter is clamped up to it
  -- and burns too, and a loop that folds then deletes deletes each of them.
  | stampFor folded view > maxFoldedTs     = Left StampOutOfRange
  | otherwise                              = Right ()
  where
    cur = cvCursor view

-- The folded-ts a mint against this view would stamp.
--
-- Clamped, not refused. The stamp is the folder's own, so a clock behind canon
-- is the folder's problem and not the letter's; refusing would let one
-- maintainer with a fast clock stop every other folder until real time caught
-- up, which is the cursor failure again. Monotonic is what the readers need,
-- and this is the cheapest way to guarantee it.
--
-- One function, because 'requireCanon' has to judge the same value 'accepted'
-- goes on to stamp: two copies of @max@ would be two chances to gate one
-- expression and stamp another.
stampFor :: Word64 -> CanonView -> Word64
stampFor folded view = max folded (cvLastFolded view)

-- | What accepting a letter produced.
data Accepted = Accepted
  { acEvent  :: Event
    -- | Canon plus this event, for the next accept.
    --
    -- Carry it forward ONLY once the event is durably written. The view is a
    -- cache of canon, so advancing it for an event that never reaches the
    -- tree makes it disagree with a rebuild: a burnt @seq@ is harmless, but
    -- a burnt @number@ leaves a gap in the issue numbering that no later
    -- fold can explain. On a failed write, keep the view that was passed in.
  , acView   :: CanonView
    -- | Where the event belongs in the tree. A redact goes with the thread
    -- of the event it hides, not under an id of its own, and delegate and
    -- revoke belong to no thread at all.
  , acScope  :: EventScope
  , acNumber :: Maybe Word64     -- ^ minted here, on an open
    -- | The seq stamped on this event. The writer names the file by it, and
    -- deriving it back out of the cursor is both awkward and easy to get
    -- wrong by one.
  , acSeq    :: Word64
    -- | Who authored the box, and what it says. Both are known here, and
    -- recovering them downstream would mean verifying the signature again.
  , acAuthor  :: HubKey
  , acContent :: AuthorContent
    -- | The sender's back-channel, already vetted: present only when the
    -- envelope signer was the inner author. Returned so the caller can
    -- notify without opening the letter a second time.
  , acReply  :: ReplyChannel
    -- | The Mailbox message this was folded from, when there was one.
    --
    -- Handed back rather than left for the caller to remember alongside,
    -- because what the caller does next is delete that message, and the
    -- predicate it deletes by takes this exact hash. A parallel list zipped
    -- against the results is one off-by-one away from deleting a letter
    -- nobody has read.
  , acOrigin :: Maybe HashRef
  }

-- Deliberately partial: the reply channel is transport-only and PEP-18
-- keeps it out of public canon, so it must not reach a log through a
-- derived Show either, and the body can be large.
instance Show Accepted where
  show a = "Accepted {event = " <> show (eventId (acEvent a))
        <> ", scope = " <> show (acScope a)
        <> ", seq = " <> show (acSeq a)
        <> ", number = " <> show (acNumber a)
        <> ", author = " <> show (acAuthor a)
        -- Not a secret, and the field whose haddock says one off-by-one
        -- deletes an unread letter: the one thing worth having in the log when
        -- tracing exactly that confusion.
        <> ", origin = " <> show (acOrigin a)
        <> "}"

-- | The acknowledgement for what was just accepted (PEP-18), or 'Nothing' for
-- an event that belongs to no thread and so acknowledges nothing.
--
-- Assembled here because everything it needs is here and nowhere else together:
-- the number and the status belong to the THREAD, not to the event, and a
-- caller holding an 'Accepted' would otherwise have to re-fold canon to learn
-- them, or keep its own copy of the last-writer-wins rule. Whether to send it,
-- and where, is the caller's ('acReply').
ackFor :: Accepted -> Maybe AckRecord
ackFor a = case acScope a of
  RepoScope       -> Nothing
  ThreadScope thr -> do
    tf <- HM.lookup thr (cvThreads (acView a))
    pure AckRecord
      { akTarget = cvRepo (acView a)
      , akThread = thr
      , akNumber = tfNumber tf
      , akStatus = tfStatus tf
      , akMergeCommit = case acContent a of
          AMerge _ mc _ _ -> Just mc
          _               -> Nothing
        -- The words the maintainer wrote, when this event carried any. They are
        -- already public in canon; what this saves the contributor is having to
        -- go and read canon to find out why.
      , akNote = case acContent a of
          AClose _ note _  -> note
          AReopen _ note _ -> note
          _                -> Nothing
      }

-- | Accept a letter into canon.
--
-- Only 'FoldsToCanon' ops (open, comment, revise) may be blessed as they
-- stand, because only those carry an authorship the owner is merely
-- admitting rather than asserting. A @close@ from a stranger is a request
-- (see 'honourRequest'); a @redact@ is owner-native and is refused outright.
--
-- The part secret is supplied by the caller, which is the layer that
-- decrypted the Mailbox message and therefore holds it. It is published only
-- when the letter actually references an attachment, so canon does not carry
-- secrets for messages with nothing encrypted in them.
acceptLetter
  :: TriageCtx
  -> EnvelopeSigner                     -- ^ who signed the Mailbox envelope
  -> CanonView
  -> Word64                             -- ^ folded-at: owner clock, epoch MILLISECONDS
  -> HashRef                            -- ^ origin: hash of the Mailbox message
  -> LetterParts                        -- ^ the message's attachments, if any
  -> MessageData
  -> Either TriageError Accepted
acceptLetter ctx envelopeSigner view folded origin (LetterParts parts) md = do
  let canonKp@(canonPk,_) = tcCanon ctx
      allowed = tcAllowed ctx
      repo = tcRepo ctx
  -- Our own authority first: a revoked maintainer can still sign, but the
  -- fold will not admit it, so minting would burn a triaged letter.
  requireCanon repo view folded canonPk

  (box, author, content, reply) <-
    either (Left . BadLetter) Right (openLetterAs allowed envelopeSigner md)

  -- Before anything that asks a human: a triage loop re-reading the mailbox
  -- after a restart must not raise a decision on a letter whose request was
  -- already honoured, only to have 'honourRequest' refuse it afterwards.
  honouredAlready view origin

  -- Before the request prompt as well: an oversized closing note is refused
  -- whoever wrote it, and raising it as a decision first would put a letter in
  -- front of a maintainer that no answer can fold.
  requireSize content
  requireRefs content

  -- And the same for an attribute nobody can sign as sent. Without this the
  -- letter reaches a maintainer as a request, the obvious thing to do with a
  -- request is to hand what it carries to 'honourWith', and that refuses it as
  -- the caller's doing and stops the loop: one letter naming an attribute
  -- "Labels" would be all it takes. What the owner may still do is sign a
  -- normalized version of their own, which is 'honourWith' with content the
  -- caller ran through 'normalizeAttr'.
  requireNormalized content

  -- A request needs its thread to exist before it is worth a maintainer's
  -- attention: otherwise a close naming an unfolded thread would raise a
  -- decision that 'honourRequest' then refuses, while a comment naming the
  -- same thread correctly waits in the mailbox.
  case classify content of
    FoldsToCanon -> Right ()
    RequestOnly
      | maybe True (`HM.member` cvThreads view) (authorThread content)
                  -> Left (Requested (Pending author content reply))
      | otherwise -> Left UnknownThread
    other         -> Left (NotAcceptable other)

  requireStamp view content
  seenAlready view (authorBoxId box)

  thread <- case content of
    AOpen target kind _ _ _ _ coords _
      | target /= repo                    -> Left WrongRepo
      | kind == HubPR && isNothing coords -> Left BadContent
      | kind == HubIssue && isJust coords -> Left BadContent
      | maybe False (not . reachableCoords) coords -> Left BadContent
      | otherwise                         -> Right (authorBoxId box)

    AComment thr _ _ _ _ -> known thr

    ARevise thr coords _ ->
      case HM.lookup thr (cvThreads view) of
        Nothing -> Left UnknownThread
        Just tf
          -- Kind and payload first: those are about the shape of the
          -- request, not about who signed it.
          | tfKind tf /= HubPR           -> Left BadContent
          | not (reachableCoords coords) -> Left BadContent
          | tfAuthor tf == author        -> Right thr
          -- Stricter than the fold on purpose: the fold also lets a
          -- maintainer revise, but a maintainer doing that through the
          -- letter path would be acting as someone else.
          | otherwise                    -> Left NotAuthorOfRecord

    -- classify has already restricted this to the three ops above.
    _ -> Left BadContent

  -- Last, because this one is about what the caller supplied rather than about
  -- the letter: a shape error should be reported as such rather than as a
  -- missing key.
  requireParts parts content

  Right (accepted canonKp repo view folded (Just origin) (poSecret parts) content box author
           (ThreadScope thread) reply)
  where
    known thr
      | HM.member thr (cvThreads view) = Right thr
      | otherwise                      = Left UnknownThread

-- | Honour a request VERBATIM, re-authoring exactly what the letter asked
-- for under the canon key.
--
-- A stranger's @close@/@reopen@/@set@ letter cannot become canon as theirs:
-- the fold would drop it, because those ops are owner-authored. What the owner
-- can do is agree, which means authoring the same content themselves. The
-- declared timestamp becomes the owner's, since the owner is the one declaring
-- it now; the letter's provenance is kept as the origin.
--
-- Only what the request carries no words of. A closing note would become a
-- comment authored by the signer, and an attribute is the requester's choice
-- of both name and value, so those are refused with 'NeedsReview' and belong
-- on 'honourWith'. A note-less close or reopen is the whole of what "agreed
-- as sent" can mean.
honourRequest
  :: TriageCtx
  -> EnvelopeSigner                     -- ^ who signed the Mailbox envelope
  -> CanonView
  -> Word64                             -- ^ folded-at and declared time: epoch MILLISECONDS
  -> HashRef                            -- ^ origin: the requesting message
  -> MessageData
  -> Either TriageError Accepted
honourRequest ctx envelopeSigner view folded origin md = do
  (_box, _author, content, reply) <-
    either (Left . BadLetter) Right (openLetterAs (tcAllowed ctx) envelopeSigner md)
  honourOpened Verbatim (tcCanon ctx) (tcRepo ctx) view folded origin content content reply

-- | Honour a request, but sign content the maintainer has looked at and
-- possibly edited.
--
-- 'honourRequest' signs the requester's content verbatim, so it refuses
-- anything the requester phrased: a note, or an attribute name and value of
-- their choosing. This is where those go. Triage decides what it is agreeing
-- to and signs that, so the content is passed explicitly; the letter is still
-- required, for its provenance and to check that what it asked for was a
-- request in the first place.
honourWith
  :: TriageCtx
  -> EnvelopeSigner                     -- ^ who signed the Mailbox envelope
  -> CanonView
  -> Word64                             -- ^ folded-at and declared time: epoch MILLISECONDS
  -> HashRef                            -- ^ origin: the requesting message
  -> AuthorContent                      -- ^ what the owner actually signs
  -> MessageData                        -- ^ the request being honoured
  -> Either TriageError Accepted
honourWith ctx envelopeSigner view folded origin content0 md = do
  (_box, _author, asked, reply) <-
    either (Left . BadLetter) Right (openLetterAs (tcAllowed ctx) envelopeSigner md)
  honourOpened Reviewed (tcCanon ctx) (tcRepo ctx) view folded origin asked content0 reply

-- Which of the two honour paths this is.
--
-- A flag rather than a comparison of the two contents: on the verbatim path
-- they are equal by construction, and a maintainer who happens to agree word
-- for word must not be treated as though they had not read it.
data Honour = Verbatim | Reviewed
  deriving stock (Eq)

-- The shared body, taking the letter already opened: 'honourRequest' would
-- otherwise verify the same signature twice.
honourOpened
  :: Honour
  -> (HubKey, PrivKey 'Sign HubScheme)
  -> RepoRef
  -> CanonView
  -> Word64
  -> HashRef
  -> AuthorContent    -- ^ what the letter asked for
  -> AuthorContent    -- ^ what the owner signs, possibly edited
  -> ReplyChannel
  -> Either TriageError Accepted
honourOpened path canonKp@(pk,sk) repo view folded origin asked content0 reply = do
  requireCanon repo view folded pk

  -- A re-authored request gets a fresh id from the clock, so only the
  -- origin can tell that this very letter was honoured before. Without it a
  -- triage loop that restarts and re-reads the mailbox closes the thread
  -- twice and duplicates the note.
  honouredAlready view origin

  -- After those two, deliberately. Both are reasons this letter must not
  -- reach a human at all: a view wired to another repo is a bug to stop on,
  -- and a letter already folded would have the maintainer write a reply that
  -- is then refused. Asking for review first would hide either behind a
  -- normal-looking request.
  when (path == Verbatim && verbatimUnsafe content0) (Left NeedsReview)

  -- What the letter asked for is judged bare, because a refusal here is about
  -- the letter.
  case classify asked of
    RequestOnly -> Right ()
    other       -> Left (NotAcceptable other)

  -- Everything from here down judges content the CALLER composed, so every
  -- refusal is wrapped: the same words mean opposite things depending on which
  -- side they were raised against, and a loop that folds then deletes would
  -- otherwise delete a good submission over a bug in a maintainer's tooling.
  composed $ do
    -- The owner signs this, so the same size gate applies as on any other
    -- owner-authored event: 'ownerEvent' checks it, and there is no reason for
    -- the two owner paths to disagree.
    requireSize content0
    requireRefs content0
    requireNormalized content0

    -- What the owner signs must still be a request-shaped op on a thread that
    -- exists, whether or not it is exactly what was asked for.
    case classify content0 of
      RequestOnly -> Right ()
      other       -> Left (NotAcceptable other)

    -- Editing what a request asked for is the point of this function; moving
    -- it to another thread is not, and would leave the event carrying an
    -- origin that points at a letter about something else.
    if authorThread content0 == authorThread asked
      then Right ()
      else Left ThreadMismatch

  -- NOT composed, and the check just above is why: the two contents name the
  -- same thread by now, so a thread canon does not hold is the LETTER's thread,
  -- and that is the ordinary out-of-order case. Fold the opening letter and
  -- come back; deleting this one would lose a good submission.
  thread <- case authorThread content0 of
    Just thr | HM.member thr (cvThreads view) -> Right thr
             | otherwise                      -> Left UnknownThread
    Nothing -> Left (Composed BadContent)

  -- Re-author: a new box signed by the owner, carrying the owner's clock.
  -- Honouring the same request twice at the same clock produces the same
  -- bytes and so the same id, which the fold would drop as a duplicate.
  -- No requireStamp here: a RequestOnly op never mints a number, which
  -- 'classify' above has already established.
  let content = withAuthorTs folded content0
      box = signAuthor pk sk content
  -- Composed, because this one is about the CLOCK the caller passed. A close, a
  -- reopen and a close again on one tick re-author to the same bytes and so to
  -- the same id, and answering that by discarding would delete the letter, leave
  -- the thread open, and tell triage it had closed it. The caller has to vary
  -- something; the letter has done nothing wrong.
  composed (seenAlready view (authorBoxId box))

  -- No requireParts: 'classify' has established that this is a close, reopen
  -- or set, none of which can reference a part.
  Right (accepted canonKp repo view folded (Just origin) Nothing content box pk
           (ThreadScope thread) reply)

-- | An owner-native event: the owner is both author and canon (PEP-19).
-- Used for status changes, merges, redactions and delegation, none of which
-- ever arrive as a letter.
--
-- Ordering is checked here too. A redact whose target is not in canon yet is
-- refused rather than minted, because the fold treats such a redact as a
-- silent no-op, and a thread op on an unknown thread would be dropped.
-- Every refusal on this path is 'Composed', without exception: there is no
-- letter here, so nothing this function judges is anything a sender did. A
-- triage loop that folds then deletes must not delete somebody's submission
-- because the maintainer's own tooling composed a set with unsorted labels.
ownerEvent
  :: TriageCtx
  -> CanonView
  -> Word64                             -- ^ folded-at: owner clock, epoch MILLISECONDS
  -> OwnerParts                         -- ^ the event's attachments, if any
  -> AuthorContent
  -> Either TriageError Accepted
ownerEvent ctx view folded parts content =
  composed (ownerEvent' ctx view folded parts content)

ownerEvent'
  :: TriageCtx
  -> CanonView
  -> Word64
  -> OwnerParts
  -> AuthorContent
  -> Either TriageError Accepted
ownerEvent' ctx view folded (OwnerParts parts) content = do
  let kp@(pk,sk) = tcCanon ctx
      repo = tcRepo ctx
  requireCanon repo view folded pk

  let box = signAuthor pk sk content
      eid = authorBoxId box

  requireStamp view content
  seenAlready view eid

  -- Before the scope is resolved, so that a malformed hash is reported as one
  -- rather than as the unknown target it necessarily looks like.
  requireSize content
  requireRefs content
  requireNormalized content

  scope <- case content of
    -- A redact belongs with the thread of the event it hides, so it lands
    -- beside it in the tree rather than under an id of its own.
    ARedact target _ -> case HM.lookup target (cvEvents view) of
      Nothing         -> Left UnknownTarget
      Just (Just thr) -> Right (ThreadScope thr)
      Just Nothing    -> Right RepoScope

    -- Only the owner key may delegate (PEP-19 rule 5): a delegate signing
    -- one would be dropped, so refuse before minting.
    ADelegate target _ _ | target /= repo -> Left WrongRepo
                         | pk /= repo     -> Left OwnerKeyRequired
                         | otherwise      -> Right RepoScope
    ARevoke target _ _   | target /= repo -> Left WrongRepo
                         | pk /= repo     -> Left OwnerKeyRequired
                         | otherwise      -> Right RepoScope

    AOpen target kind _ _ _ _ coords _
      | target /= repo                    -> Left WrongRepo
      | kind == HubPR && isNothing coords -> Left BadContent
      | kind == HubIssue && isJust coords -> Left BadContent
      | maybe False (not . reachableCoords) coords -> Left BadContent
      | otherwise                         -> Right (ThreadScope eid)

    -- The remaining ops are thread ops; the fold checks kind for the PR-only
    -- ones, so the bridge checks it too rather than minting a doomed event.
    _ -> case authorThread content of
           Nothing -> Left BadContent
           Just thr -> case HM.lookup thr (cvThreads view) of
             Nothing -> Left UnknownThread
             Just tf
               | prOnly content && tfKind tf /= HubPR -> Left BadContent
               -- The fold checks the coordinates of a revise as well, and it
               -- lets a maintainer author one, so this path is reachable:
               -- without the check a maintainer mints an event the fold then
               -- drops, burning a seq and losing the action silently.
               | not (coordsOK content)               -> Left BadContent
               | otherwise                            -> Right (ThreadScope thr)

  requireParts parts content

  Right (accepted kp repo view folded Nothing (poSecret parts) content box pk scope NoReply)
  where
    coordsOK = \case
      ARevise _ c _ -> reachableCoords c
      _             -> True

    prOnly = \case
      ARevise{} -> True
      AMerge{}  -> True
      _         -> False

-- | Where the writer must put this event in the canon tree (PEP-19 layout).
--
-- The name carries the @seq@ zero-padded so that a lexical listing of a
-- directory is the fold's own order, and the event-id so that two events at
-- one @seq@ are two files. It is also why a well-formed canon cannot hold two
-- blessings of one author box at one @seq@: they are the same path (see the
-- sort key in "HBS2.Hub.Fold").
eventPath :: Accepted -> FilePath
eventPath a = dir <> "/" <> eventFileName (acSeq a) (eventId (acEvent a))
  where
    dir = case acScope a of
            ThreadScope t -> threadDir t
            RepoScope     -> repoDir

-- The number this content mints, if any: only a thread has one.
mintedNumber :: AuthorContent -> CanonCursor -> Maybe Word64
mintedNumber AOpen{} cur = Just (ccrNextNumber cur)
mintedNumber _ _ = Nothing

-- Stamp a canon box onto an author box, advance the view, and report
-- everything the caller and the writer downstream will need.
--
-- The author is passed in rather than recovered from the box: both callers
-- already know it (one verified the letter, the other just signed it), and
-- unboxing again would be a fourth signature check per letter with a silent
-- fallback on failure, in the one place that decides who may revise a thread.
accepted
  :: (HubKey, PrivKey 'Sign HubScheme)
  -> RepoRef                           -- ^ needed to mirror the fold's owner rule
  -> CanonView
  -> Word64
  -> Maybe HashRef
  -> Maybe PartSecret
  -> AuthorContent
  -> SignedBox AuthorContent HubScheme
  -> HubKey                            -- ^ the author box's signer
  -> EventScope
  -> ReplyChannel
  -> Accepted
accepted (pk,sk) repo view folded origin secret content box authorOf scope reply =
  Accepted { acEvent = Event box canonBox
           , acView = view'
           , acScope = scope
           , acNumber = mintedNumber content cur
           , acSeq = ccrNextSeq cur
           , acAuthor = authorOf
           , acContent = content
           , acReply = reply
           , acOrigin = origin
           }
  where
    cur = cvCursor view
    eid = authorBoxId box

    -- Bounded above by 'requireCanon', which every entry point runs first.
    stamped = stampFor folded view

    opensThread = case content of
      AOpen{} -> True
      _       -> False

    -- Only publish a secret when there is something encrypted to unlock.
    secret' | referencesPart content = secret
            | otherwise              = Nothing

    canonBox = signCanon pk sk CanonContent
      { ccTarget     = repo
      , ccEventId    = eid
      , ccSeq        = ccrNextSeq cur
      , ccNumber     = mintedNumber content cur
      , ccOrigin     = origin
      , ccFoldedTs   = stamped
      , ccPartSecret = secret'
      }

    view' = view
      { cvCursor = CanonCursor
          { ccrNextSeq    = ccrNextSeq cur + 1
          , ccrNextNumber = if opensThread then ccrNextNumber cur + 1 else ccrNextNumber cur
          }
        -- The status is maintained here exactly as the fold maintains the
        -- attribute it mirrors, and by the same four ops. A divergence would be
        -- a bug even if nothing read the field, since the accumulated view is a
        -- cache of the rebuilt one.
      , cvThreads = case content of
          AOpen _ kind _ _ _ _ _ _ ->
            HM.insert eid (ThreadFacts kind authorOf (mintedNumber content cur) "open")
              (cvThreads view)
          ASet thr "status" v _ -> setStatus thr v
          AClose thr _ _        -> setStatus thr "closed"
          AReopen thr _ _       -> setStatus thr "open"
          AMerge thr _ _ _      -> setStatus thr "merged"
          _                     -> cvThreads view
      , cvEvents = HM.insert eid (scopeThread scope) (cvEvents view)
        -- Delegation takes effect for the next accept, exactly as it will
        -- when the view is later rebuilt from canon. Revoking the owner is a
        -- no-op here for the same reason it is one in the fold: the owner is
        -- the root of trust and cannot be delegated away, so it cannot be
        -- withdrawn either. Deleting it here would leave the accumulated
        -- view disagreeing with the rebuilt one about a legitimate event.
      , cvMaintainers = case content of
          ADelegate _ k _           -> HS.insert k (cvMaintainers view)
          ARevoke _ k _ | k /= repo -> HS.delete k (cvMaintainers view)
          _                       -> cvMaintainers view
      , cvOrigins = maybe (cvOrigins view) (`HS.insert` cvOrigins view) origin
      , cvLastFolded = stamped
      }

    setStatus thr v = HM.adjust (\tf -> tf { tfStatus = v }) thr (cvThreads view)

    scopeThread = \case
      ThreadScope thr -> Just thr
      RepoScope       -> Nothing
