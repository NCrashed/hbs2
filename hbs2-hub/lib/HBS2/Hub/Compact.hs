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
-- WHAT IS DROPPABLE, exactly: a @set@-class event (a plain @set@, or a @close@
-- or @reopen@ carrying no note) for a @(thread, attribute)@ that a
-- higher-@seq@ event of the same class has already overwritten, and that no
-- @redact@ names.
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
import HBS2.Hub.Fold (Resolved(..),resolve,FoldResult(..))

import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
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

-- | The plan for one canon.
--
-- Takes the events as canon holds them, in any order, and answers in that same
-- order. Nothing here reads a clock or a repository: two maintainers running a
-- compaction over the same canon produce the same two lists, which is what lets
-- a clone check one rather than trust it.
compactionOf :: [Event] -> Compaction
compactionOf evs = Compaction { cpKeep = keep, cpDrop = drop' }
  where
    (keep, drop') = foldr split ([], []) evs

    split e (ks, ds)
      | droppable winners redacted e = (ks, e : ds)
      | otherwise                    = (e : ks, ds)

    -- The highest seq per attribute, over EVERY set-class event including the
    -- ones carrying bodies: a note-carrying close is retained and still
    -- supersedes, so the plain set under it is droppable.
    winners :: HashMap Attr Word64
    winners = HM.fromListWith max
                [ (a, rSeq r)
                | Right r <- fmap resolve evs
                , Just a <- [attrOfResolved r]
                ]

    -- Every event any redact names. Every redact is retained, so "named by a
    -- retained redact" and "named by a redact" are the same set.
    redacted :: HashSet EventId
    redacted = HS.fromList [ e' | Right r <- fmap resolve evs
                                , ARedact _ e' _ <- [rContent r] ]

    -- The attribute a set-class event writes, INCLUDING the ones with bodies:
    -- 'attrOf' answers Nothing for those because they are not droppable, and
    -- the supersession map needs them anyway.
    attrOfResolved r = case rContent r of
      ASet thr a _ _  -> Just (Attr thr a)
      AClose thr _ _  -> Just (Attr thr "status")
      AReopen thr _ _ -> Just (Attr thr "status")
      _               -> Nothing

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
  && frMaxSeq a == frMaxSeq b

-- | Whether this one event may go, given what supersedes and what is redacted.
--
-- Exported so a test can drive the decision one event at a time, and so the
-- three conditions can be read as three conditions.
droppable :: HashMap Attr Word64 -> HashSet EventId -> Event -> Bool
droppable winners redacted e = case resolve e of
  -- An event this build cannot resolve is not this build's to remove.
  Left _ -> False
  Right r -> case attrOf (rContent r) of
    Nothing -> False
    Just a ->
         -- Superseded, STRICTLY: two events at one seq do not supersede each
         -- other, and canon can hold a pair (the fold reports it as an anomaly
         -- and orders them by canon-box hash). Dropping either would be
         -- choosing between them on a tie this rule has no opinion about.
         maybe False (> rSeq r) (HM.lookup a winners)
         -- And nothing hides it. A redact that outlived its target names an
         -- event the fold no longer sees, which lowers the highest admitted
         -- seq and hands the bridge a number it has already spent. PEP-21:
         -- reuse is tolerated and reported, and compaction should not
         -- manufacture it.
      && not (HS.member (rId r) redacted)
