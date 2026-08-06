{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
-- | Which events a compaction may drop (PEP-19 "Compaction", PEP-21 "Canon
-- compaction policy").
--
-- Canon grows mostly from @set@ churn: a status flipped twice, labels edited
-- five times. Only the winning value is load-bearing, and PEP-19 lets a rewrite
-- drop the overwritten ones -- so long as everything a reader or the fold might
-- still need survives. This module is that rule and nothing else: pure, total,
-- and separate from the verb that will rewrite the ref, because what may be
-- dropped from append-only canon is a decision worth reading on its own.
--
-- IT IS ASKED ABOUT THE FOLD, not only about the events. The rule takes a
-- 'FoldResult' over the same canon, and that is not a convenience: an event the
-- fold refused must neither be dropped nor be allowed to displace one it
-- admitted. Both halves were wrong when this took a bare list. 'resolve' checks
-- two signatures and an id binding and nothing else -- not the target, not the
-- maintainer set -- so a self-signed @set@ at a high @seq@ that the fold refuses
-- as @UnauthorizedCanon@ counted as the winner for its attribute, and the
-- owner's real @set@ underneath it was dropped from canon while the event that
-- displaced it was never admitted anywhere. That case is ORDINARY, not hostile:
-- a delegate minting from a view built before their revocation produces exactly
-- it, which "HBS2.Hub.Fold" documents as honest.
--
-- WHAT IS DROPPABLE, exactly: an ADMITTED @set@-class event (a plain @set@, or
-- a @close@ or @reopen@ carrying no note) for a @(thread, attribute)@ that a
-- higher-@seq@ admitted event of the same class has already overwritten, that
-- no @redact@ names, and that folded no letter.
--
-- Everything else is retained, and the list is not a summary of the rule -- it
-- is the rule, because each item is retained for its own reason:
--
--   * every @open@: it carries the number, the thread root and the authorship,
--     and @index/number.sexp@ is derived from it;
--   * every @comment@: authored discussion is irreplaceable;
--   * every @merge@ and @redact@;
--   * every @delegate@ and @revoke@, ALWAYS. Admission of every historical
--     event depends on the maintainer set as of that event's @seq@, which is
--     reconstructed from these. Dropping one changes which past events the fold
--     admits, which breaks the property compaction exists to preserve;
--   * every @set@-class event carrying a body: a note on a @close@ is authored
--     discussion even when the status it set was later overwritten;
--   * every event that FOLDED A LETTER, meaning one carrying an @origin@ or an
--     @honours@. One letter folds to at most one event, and the check that
--     enforces it asks whether canon already holds an event with that letter's
--     hash as its origin. Compacting an honoured @close@ away removes the only
--     record that its letter was carried out, so the same letter honoured again
--     after a restart mints a second event. PEP-19 allows keeping just the
--     @origin@ field of an otherwise dropped event; keeping the event is
--     strictly stronger and costs no format. It also costs little in practice,
--     since the churn compaction exists for is owner-native and carries no
--     origin at all;
--   * the winning @set@ per @(thread, attribute)@.
--
-- AND EVERYTHING THIS MODULE DOES NOT UNDERSTAND. An event whose boxes will not
-- resolve is retained, and so is one the fold dropped: a compaction that
-- quietly removed them would edit what @hub verify@ reports about this
-- repository, which is a different act from bounding its size and not one to
-- perform by accident.
module HBS2.Hub.Compact
  ( Compaction(..)
  , compactionOf
  , droppable
  , equivalentTo
  , Attr(..)
  , attrOf
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Fold (Resolved(..),resolve,FoldResult(..),DropReason)

import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (foldl')
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Word (Word64)

-- | What one compaction would do, as two lists rather than one predicate.
--
-- Both halves, because the caller writes the first and reports the second, and
-- because a plan whose input order is preserved is one a person can diff
-- against the tree they are about to replace.
data Compaction = Compaction
  { cpKeep :: [Event]   -- ^ in the order they were given
  , cpDrop :: [Event]   -- ^ likewise
  }
  deriving stock (Eq,Show)

-- | What a @set@-class event sets.
--
-- A named type rather than a 'Text', because @close@ and @reopen@ set the
-- status attribute without naming it, and the rule that they supersede a plain
-- @set status@ (and are superseded by one) is exactly the equality this type
-- makes true.
data Attr = Attr ThreadId Text
  deriving stock (Eq,Show)

instance Hashable Attr where
  hashWithSalt s (Attr t a) = hashWithSalt s (t, a)

-- | The @(thread, attribute)@ a @set@-class event writes, and 'Nothing' for
-- everything else.
--
-- Also the place the body test lives, since the two questions have one answer:
-- a @close@ with a note is not a @set@-class event for this purpose at all --
-- it is authored discussion that happens to change a status.
attrOf :: AuthorContent -> Maybe Attr
attrOf = \case
  ASet thr a _ _              -> Just (Attr thr a)
  AClose thr Nothing _        -> Just (Attr thr "status")
  AReopen thr Nothing _       -> Just (Attr thr "status")
  -- A note is a body. PEP-19 retains these explicitly.
  AClose{}                    -> Nothing
  AReopen{}                   -> Nothing
  AOpen{}                     -> Nothing
  AComment{}                  -> Nothing
  ARevise{}                   -> Nothing
  AMerge{}                    -> Nothing
  ARedact{}                   -> Nothing
  ADelegate{}                 -> Nothing
  ARevoke{}                   -> Nothing

-- | The attribute a @set@-class event writes, INCLUDING the ones with bodies.
--
-- 'attrOf' answers 'Nothing' for those because they are not droppable; the
-- supersession map needs them anyway, since a note-carrying @close@ is retained
-- AND still overwrites the plain @set@ under it.
--
-- Spelled out over every constructor rather than closed with a wildcard, and
-- the module's @-Werror=incomplete-patterns@ is what keeps it that way: this is
-- the second half of one rule whose halves must disagree in exactly one place,
-- and under a wildcard a new @set@-class op silently gets the wrong one.
attrWritten :: AuthorContent -> Maybe Attr
attrWritten = \case
  ASet thr a _ _  -> Just (Attr thr a)
  AClose thr _ _  -> Just (Attr thr "status")
  AReopen thr _ _ -> Just (Attr thr "status")
  AOpen{}         -> Nothing
  AComment{}      -> Nothing
  ARevise{}       -> Nothing
  AMerge{}        -> Nothing
  ARedact{}       -> Nothing
  ADelegate{}     -> Nothing
  ARevoke{}       -> Nothing

-- | The plan for one canon.
--
-- Takes the fold over this canon and the events as canon holds them, in any
-- order, and answers in that same order. Nothing here reads a clock or a
-- repository: two maintainers running a compaction over the same canon produce
-- the same two lists, which is what lets a clone check one rather than trust
-- it.
compactionOf :: FoldResult -> [Event] -> Compaction
compactionOf fr evs = Compaction { cpKeep = reverse keep, cpDrop = reverse drop' }
  where
    -- ONE resolve per event, and it is two Ed25519 verifies. The previous
    -- shape called it three times -- once for the winners, once for the
    -- redactions, once inside the predicate -- over a list bounded by
    -- 'maxCanonFiles'.
    resolved = [ (e, resolve e) | e <- evs ]

    -- foldl' and a reverse, not foldr. The step pattern-matches its
    -- accumulator, so a foldr is strict in it and builds the whole spine on
    -- the stack before returning anything; "HBS2.Hub.Fold" records the same
    -- fix for the same reason.
    (keep, drop') = foldl' split ([], []) resolved

    split (ks, ds) (e, r)
      | droppable admitted winners redacted r = (ks, e : ds)
      | otherwise                             = (e : ks, ds)

    admitted = HM.keysSet (frAdmitted fr)

    -- The highest seq per attribute, over every ADMITTED set-class event
    -- including the ones carrying bodies. Admitted, because an event the fold
    -- refused sets nothing, so letting it supersede would drop a value in
    -- favour of one no reader will ever see.
    winners :: HashMap Attr Word64
    winners = HM.fromListWith max
                [ (a, rSeq r)
                | (_, Right r) <- resolved
                , HS.member (rId r) admitted
                , Just a <- [attrWritten (rContent r)]
                ]

    -- Every event any redact names, admitted or not. Every redact is retained,
    -- and a redact the fold refused still says what somebody meant to withhold,
    -- so the conservative side is the right one here.
    redacted :: HashSet EventId
    redacted = HS.fromList [ e' | (_, Right r) <- resolved
                                , ARedact _ e' _ <- [rContent r] ]

-- | Whether two canons say the same thing.
--
-- The property compaction claims, written as a check rather than left as a
-- promise, because a clone needs it: @hub sync@ does not force the canon ref,
-- so a rewrite upstream arrives as a divergence and is indistinguishable from a
-- fork until somebody folds both. This is what tells them apart.
--
-- WHAT IS COMPARED, and each for its own reason:
--
--   * the threads, which are the materialized state: every attribute, every
--     comment, every PR coordinate. This is what a reader sees;
--   * the maintainer set, which a reader does NOT see and which decides what
--     canon will admit next. Two canons can agree on every thread while one of
--     them quietly dropped a @delegate@ nobody had used yet, and taking that
--     one hands the repository a maintainer set its owner did not write;
--   * the redacted set, since a rewrite that dropped a @redact@ would leave
--     every clone showing a body somebody withdrew;
--   * the origins and the honoured set, which no reader sees either and which
--     are what stop one letter folding twice. A rewrite that dropped them looks
--     identical in every thread and lets every letter still in a mailbox be
--     folded a second time. An honest compaction keeps them, because the rule
--     above retains any event carrying one;
--   * the highest seq, which the bridge mints against. A compaction cannot
--     lower it -- what it drops is superseded, so something higher survives --
--     and a rewrite that did lower it would hand the bridge a number already
--     spent.
--
-- WHAT IS NOT: the log. That is the timeline of overwritten values, and losing
-- it is exactly what a compaction trades for size (PEP-21). Comparing it would
-- make every compaction look like a fork.
--
-- NOR the drops. An event the fold refused is retained by 'droppable', so a
-- HONEST compaction preserves them; but this predicate is asked about canon a
-- stranger published, and reporting a difference in what somebody else's tree
-- got wrong is not the question being asked -- whether the state is the same
-- is.
equivalentTo :: FoldResult -> FoldResult -> Bool
equivalentTo a b =
     frThreads a == frThreads b
  && frMaintainers a == frMaintainers b
  && frRedacted a == frRedacted b
  && frOrigins a == frOrigins b
  && frHonoured a == frHonoured b
  && frMaxSeq a == frMaxSeq b

-- | Whether this one event may go, given what the fold admitted, what
-- supersedes and what is redacted.
--
-- Exported and taking the resolved form, so a test can drive the decision one
-- event at a time without re-resolving it -- and so that the four conditions
-- can be read as four conditions.
droppable :: HashSet EventId          -- ^ admitted, from @frAdmitted@
          -> HashMap Attr Word64      -- ^ highest admitted seq per attribute
          -> HashSet EventId          -- ^ every event a redact names
          -> Either DropReason Resolved
          -> Bool
droppable admitted winners redacted = \case
  -- An event this build cannot resolve is not this build's to remove.
  Left _ -> False
  Right r
    -- Nor is one the fold refused. It is what @hub verify@ reports, and taking
    -- it away edits the audit rather than bounding the size.
    | not (HS.member (rId r) admitted) -> False
    -- Nor one that folded a letter: its origin is the only record that the
    -- letter was carried out, and without it the same letter folds again.
    | isJust (ccOrigin (rCanon r)) -> False
    | isJust (ccHonours (rCanon r)) -> False
    | otherwise -> case attrOf (rContent r) of
        Nothing -> False
        Just a ->
             -- Superseded, STRICTLY: two events at one seq do not supersede
             -- each other, and canon can hold a pair (the fold reports it as an
             -- anomaly and orders them by canon-box hash). Dropping either
             -- would be choosing between them on a tie this rule has no opinion
             -- about.
             maybe False (> rSeq r) (HM.lookup a winners)
             -- And nothing hides it. A redact that outlived its target names an
             -- event the fold no longer sees, which lowers the highest admitted
             -- seq and hands the bridge a number it has already spent. PEP-21:
             -- reuse is tolerated and reported, and compaction should not
             -- manufacture it.
          && not (HS.member (rId r) redacted)
