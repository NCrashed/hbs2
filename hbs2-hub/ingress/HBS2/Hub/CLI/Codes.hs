{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
-- | Every code this tool exits with, in one place (PEP-22 "Scripting").
--
-- WHY THIS MODULE EXISTS. "HBS2.Hub.CLI.Common" has said from the beginning
-- that "the exit codes are a contract: a hook branches on them, so they may be
-- added to and never reassigned. Keeping them in one file is the point -- four
-- separate tables in four verbs is four places to forget when one is added."
-- They were then defined in fifteen modules, four of them were absent from the
-- documented table, and the next number was chosen by grepping for the last
-- one.
--
-- THE CONSTANTS STAY WHERE THEY ARE USED, and this imports them rather than
-- replacing them: each one carries a paragraph about why that refusal is worth
-- telling apart from its neighbours, and that paragraph belongs beside the verb
-- it is about. What was missing is not a home for the numbers, it is a list
-- that cannot be out of date -- so 'codeFor' is total over 'Refusal' and this
-- module compiles only when every member has both a number and a sentence.
--
-- What it still cannot catch is somebody writing @codeFoo = 51@ in a verb and
-- never coming here. Nothing short of moving every definition would, and moving
-- them would separate each number from the reasoning that justifies it. This is
-- the one obvious place to look, and 'hbs2-hub --codes' is what makes it read
-- by somebody other than its author.
module HBS2.Hub.CLI.Codes
  ( Refusal(..)
  , refusals
  , codeFor
  , meaning
  , codesDoc
  ) where

import HBS2.Hub.Deny (codeNoBanList)
import HBS2.Hub.Sent (codeNoSentLog)
import HBS2.Hub.Repo.Manifest (codeNoManifest)
import HBS2.Hub.CLI.Accept (codeNoCanonKey,codeLetterUnreadable,codeTriageRefused,codePartsTooMany
                          ,codeCanonUnwritable,codeCanonUnplannable,codeBundleUnusable)
import HBS2.Hub.CLI.Common (codeMailboxUnknown,codePeerSilent,codeMailboxIncomplete
                           ,codeNoKeyman)
import HBS2.Hub.CLI.Compact (codeNothingToCompact,codeNotThisCanon,codeCanonUnreadableHere)
import HBS2.Hub.CLI.Compose (codeNoKey,codeNotStored,codeNoWork,codeWrongSigil,codeUnsendable)
import HBS2.Hub.CLI.Maintainer (codeNotDelegated)
import HBS2.Hub.CLI.Policy (codeNoPolicy,codeNotSet)
import HBS2.Hub.CLI.Pr (codeBundleFailed,codeNoSuchPr,codeNotMerged,codeNotStaged,codeTooBig)
import HBS2.Hub.CLI.Publish (codeNotPublished,codePublishFailed)
import HBS2.Hub.CLI.Read (codeNoSuchThread,codeAmbiguousNumber)
import HBS2.Hub.CLI.Reject (codeAlreadyFolded,codeNotRejected)
import HBS2.Hub.CLI.Sync (codeDiverged)

import HBS2.Prelude.Plated (Doc,pretty,(<+>),line,vcat,fill)

import Data.List (sortOn)
import Data.Text (Text)

-- | One refusal, named.
--
-- The ENUM is the list: 'refusals' is @[minBound .. maxBound]@, so a member
-- added here appears in the table without anybody remembering to add it, and
-- the two functions below will not compile until it has a number and a
-- sentence. That is the whole mechanism.
--
-- @hub verify@'s own 3..16 are NOT here: they come out of 'codeOf' over
-- 'CanonUnreadable', which is a different shape -- one code per constructor of
-- a type that already exists. They are in the table below all the same, so that
-- what the tool prints is every code and not most of them.
data Refusal =
    MailboxUnknown
  | PeerSilent
  | NoKey
  | NotStored
  | NoCanonKey
  | LetterUnreadable
  | TriageRefused
  | CanonUnwritable
  | CanonUnplannable
  | NoSuchThread
  | BundleFailed
  | BundleUnusable
  | NoSuchPr
  | NotMerged
  | NotDelegated
  | AlreadyFolded
  | NotRejected
  | NoPolicy
  | NotSet
  | NoBanList
  | NoWork
  | NoSentLog
  | NotStaged
  | Diverged
  | NoManifest
  | NothingToCompact
  | NotThisCanon
  | PartsTooMany
  | NotPublished
  | PublishFailed
  | WrongSigil
  | CanonUnreadableHere
  | AmbiguousNumber
  | MailboxIncomplete
  | NoKeyman
  | Unsendable
  | TooBig
  deriving stock (Eq,Ord,Show,Enum,Bounded)

-- | Every one of them, by construction.
refusals :: [Refusal]
refusals = [minBound .. maxBound]

-- | The number, taken from the constant the verb actually exits with.
--
-- Referenced rather than repeated, so this cannot drift from what the tool
-- does: a renumbering that only touched the constant would move this too, and
-- one that only touched this would not compile.
codeFor :: Refusal -> Int
codeFor = \case
  MailboxUnknown      -> codeMailboxUnknown
  PeerSilent          -> codePeerSilent
  NoKey               -> codeNoKey
  NotStored           -> codeNotStored
  NoCanonKey          -> codeNoCanonKey
  LetterUnreadable    -> codeLetterUnreadable
  TriageRefused       -> codeTriageRefused
  CanonUnwritable     -> codeCanonUnwritable
  CanonUnplannable    -> codeCanonUnplannable
  NoSuchThread        -> codeNoSuchThread
  BundleFailed        -> codeBundleFailed
  BundleUnusable      -> codeBundleUnusable
  NoSuchPr            -> codeNoSuchPr
  NotMerged           -> codeNotMerged
  NotDelegated        -> codeNotDelegated
  AlreadyFolded       -> codeAlreadyFolded
  NotRejected         -> codeNotRejected
  NoPolicy            -> codeNoPolicy
  NotSet              -> codeNotSet
  NoBanList           -> codeNoBanList
  NoWork              -> codeNoWork
  NoSentLog           -> codeNoSentLog
  NotStaged           -> codeNotStaged
  Diverged            -> codeDiverged
  NoManifest          -> codeNoManifest
  NothingToCompact    -> codeNothingToCompact
  NotThisCanon        -> codeNotThisCanon
  PartsTooMany        -> codePartsTooMany
  NotPublished        -> codeNotPublished
  PublishFailed       -> codePublishFailed
  WrongSigil          -> codeWrongSigil
  CanonUnreadableHere -> codeCanonUnreadableHere
  AmbiguousNumber     -> codeAmbiguousNumber
  MailboxIncomplete   -> codeMailboxIncomplete
  NoKeyman            -> codeNoKeyman
  Unsendable          -> codeUnsendable
  TooBig              -> codeTooBig

-- | And what it means, in the one line a script author needs.
--
-- Not the constant's haddock, which is a paragraph about why this refusal is
-- worth its own number: that argument belongs beside the verb. This is what the
-- number MEANS, which is the half a caller branching on it is missing.
meaning :: Refusal -> Text
meaning = \case
  MailboxUnknown      -> "this peer does not hold that mailbox"
  PeerSilent          -> "the peer is running and did not answer"
  NoKey               -> "no signing key for the identity named"
  NotStored            -> "the peer would not store the message"
  NoCanonKey          -> "no signing key for the canon identity"
  LetterUnreadable    -> "the letter named is not one this node can read"
  TriageRefused       -> "the bridge would not bless it (an ordinary outcome)"
  CanonUnwritable     -> "canon could not be written"
  CanonUnplannable    -> "the event did not read back as itself; nothing written"
  NoSuchThread        -> "canon holds no thread with that number"
  BundleFailed        -> "git would not build the bundle, or would not answer"
  BundleUnusable      -> "the bundle is not what the contributor signed for"
  NoSuchPr            -> "that number is not a pull request"
  NotMerged           -> "the commit named does not contain the proposed tip"
  NotDelegated        -> "that key is not a maintainer of this repository"
  AlreadyFolded       -> "canon already holds this letter, so it was not rejected"
  NotRejected         -> "nothing was deleted"
  NoPolicy            -> "that mailbox has no policy to read or amend"
  NotSet              -> "the policy was not written"
  NoBanList           -> "the deny-list will not read"
  NoWork              -> "the proof-of-work this mailbox charges was not solved"
  NoSentLog           -> "the log of what this node sent will not read"
  NotStaged           -> "nothing is staged for that pull request"
  Diverged            -> "the two canons are forks, not one rewritten lineage"
  NoManifest          -> "this node cannot say what that repository declares"
  NothingToCompact    -> "there was nothing superseded to drop (not a failure)"
  NotThisCanon        -> "the key named does not own the canon here"
  PartsTooMany        -> "the letter names more attachments than this hub walks"
  NotPublished        -> "the remote holds canon this clone does not contain"
  PublishFailed       -> "git would not push, or would not answer"
  WrongSigil          -> "the sender sigil does not name the author key"
  CanonUnreadableHere -> "canon holds a file this reader cannot carry forward"
  AmbiguousNumber     -> "canon holds more than one thread with that number"
  MailboxIncomplete   -> "the mailbox tree did not read whole; nothing written"
  NoKeyman            -> "this machine has no key database (run hbs2-keyman)"
  Unsendable          -> "the letter was not composed; nothing was sent"
  TooBig              -> "the proposal writes more than a checkout will take"

-- | The table, for @hbs2-hub --codes@.
--
-- PRINTED BY THE TOOL rather than copied into the manual, because a table in
-- two places is a table that disagrees with itself: this one is generated from
-- the constants the verbs exit with, so it is right by construction, and the
-- manual points at it.
--
-- Sorted by number, which is the order somebody reading a script's exit status
-- wants. The verify range is listed first and separately, because those are one
-- verb's and the rest are the tool's.
codesDoc :: [Doc ann]
codesDoc =
  [ "hbs2-hub exit codes. A contract (PEP-22): added to, never reassigned."
  , ""
  , "  0   success"
  , "  1   a usage error: a bad argument, an unknown verb"
  , "  2   hub verify: the audit ran and found something"
  , ""
  , "  3..16 belong to `hub verify` and say why canon could not be read;"
  , "  `hbs2-hub help verify` documents them."
  , ""
  ]
  <> [ "  " <> fill 4 (pretty (codeFor r)) <> pretty (meaning r)
     | r <- sortOn codeFor refusals ]
