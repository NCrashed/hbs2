-- codeOf and the advice below dispatch on every constructor of CanonUnreadable
-- with no wildcard: a new one must say what it costs a hook and what to do about
-- it, and a missing case here is a pattern-match failure at the moment somebody
-- is trying to find out why their audit will not run.
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | @hub verify@: re-run the fold's checks over canon and report (PEP-22).
--
-- The audit tool, and the one verb whose whole output is what a fold REFUSED.
-- Everything it prints comes from 'FoldResult': the drops, which are events
-- canon holds and the rules do not admit, and the anomalies, which are events
-- the rules admit and nobody should have written. The difference is the point of
-- having both, and PEP-19 keeps them apart for a reason worth repeating here: an
-- anomaly is fold-legal, so refusing it would make a clone show less than canon
-- holds, which is worse than showing it and saying so.
module HBS2.Hub.CLI.Verify
  ( verifyEntries
    -- * The report, for testing
    --
    -- $ These are the whole output of the verb, and the reason they are exported
    -- is that nothing else could reach them: a Doc built inside an IO action that
    -- ends in exitWith cannot be asserted on.
  , reportDoc
  , reportCode
  , refusalDoc
  , codeOf
    -- | A canon path on its way to a terminal. Exported because @hub compact@
    -- refuses on the same list this reports, and printing a stranger's path by
    -- two rules is how one of them comes to print it raw.
  , pathDoc


    -- | The IO half: prints the report and exits. Exported for the one thing the
    -- pure halves cannot show, which is that a closed pipe exits 141 rather than
    -- 1 and that the exit code matches what reportCode computed.
  , report
  , refused
  , usage
  ) where

import HBS2.Hub.Types (pathText,HubKey)
import HBS2.Hub.CLI.Argv (repoAndFlags)
import HBS2.Hash (hashObject,HbSync)
import HBS2.Base58 (AsBase58(..))
import HBS2.Hub.Fold
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import System.Exit (exitWith,ExitCode(..),die)
import System.IO.Error (isResourceVanishedError)

verifyEntries :: forall c m . ( IsContext c
                              , MonadUnliftIO m
                              , Exception (BadFormException c)
                              ) => MakeDictM c m ()
verifyEntries = do

  brief "re-run the fold's checks over this repository's canon and report"
    $ args [arg "string" "repo-key"]
    $ desc ( "Read-only, and needs no peer: canon is a git ref in this"
             <> line <> "repository. Reports every event the rules did not admit"
             <> line <> "and every anomaly in the ones they did."
             <> line
             <> line <> "Fetch canon first; a plain clone does not bring it, since"
             <> line <> "git's default refspec covers only heads and tags:"
             <> line <> "  git fetch <remote> '+refs/hbs2/meta:refs/hbs2/meta'"
             <> line
             <> line <> "The repository key is an argument because the tree cannot"
             <> line <> "be trusted to say whose it is: the owner key is the root of"
             <> line <> "the trust chain, so canon that named its own owner would be"
             <> line <> "canon that could rename it." )
    $ entry $ bindMatch "hub:verify" $ nil_ \case
        -- Positionally or behind --repo. See 'repoAndFlags': the flag is what
        -- every verb that writes takes, so a reader who learned it there met a
        -- usage error on the six verbs that only read.
        (repoAndFlags asKey [] -> Just (repo, _)) -> lift do
          withGitCanon $ \cs ->
            readCanon cs repo >>= liftIO . either refused report

        -- Its own message, not a BadFormException: PEP-22 says every refusal
        -- prints what to do about it, and the commonest attempt of all --
        -- `hub verify --help` -- landed on "hbs2-hub: BadFormException
        -- (hub:verify --help)", which names an internal type and a spelling the
        -- caller did not type.
        _ -> liftIO (die (show (usage :: Doc ())))

asKey :: forall c . IsContext c => Syntax c -> Maybe HubKey
asKey = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }

-- What this verb takes, in the words somebody typing it would use.
usage :: Doc ann
usage = "usage: hub verify <repo-key>" <> line
          <> "   or: hub verify --repo <repo-key>" <> line
          <> "  the repository's key in base58, which is what `hbs2-git3` calls"
          <> " the repo key." <> line
          <> "  It is an argument because the owner key is the root of the trust"
          <> " chain: canon" <> line
          <> "  that named its own owner would be canon that could rename it."
          <> line <> "  `hub help verify` says more."

-- There is nothing to audit, and which nothing it is decides both the advice and
-- the exit code. One code for all of them told a hook that an unfetched ref, a
-- repository that is not one, a tree with a pruned object and canon from the
-- future were the same event.
refused :: CanonUnreadable -> IO ()
refused u = do
  hPutDoc stderr (refusalDoc u <> line)
  exitWith (ExitFailure (codeOf u))

-- Is this a problem with the CLONE rather than with the tree? Both mean "not
-- readable here" and neither means the publisher did anything wrong.
notHere :: FileProblem -> Bool
notHere = \case
  FileObjectMissing   -> True
  FileUnreadable _    -> True
  -- About THIS READER, says its own haddock: the listing record could not be
  -- parsed. Exit 7 told whoever published canon to rewrite a file that may be
  -- perfectly good, over a format this build does not know how to read.
  FileListingUnparsed -> True
  -- Total, with no wildcard, for the reason the module header gives: a new
  -- FileProblem falling into a default here would exit 7 and tell whoever
  -- published canon to rewrite a file that is fine. FileListingUnparsed already
  -- did, and it is documented as being about this reader, not about the tree.
  FileMalformed _     -> False
  FileTooLarge _      -> False
  FileNotABlob        -> False
  FileIsADirectory    -> False
  FileUnexpected      -> False
  FileDuplicated      -> False
  -- The file read here perfectly well; the number in it is not a version. That
  -- is the publisher's, not the clone's.
  FileBadVersion _    -> False


-- | What a refusal says: the reason, and what to do about it.
refusalDoc :: CanonUnreadable -> Doc ann
refusalDoc u = "hub:" <+> pretty u <> advice u
  where
    -- Total, and the wildcard is gone with the Werror above holding it that way.
    -- Two of these used to fall into it and print a bare complaint: a repository
    -- that is not one and an unreadable version file, which are the two a reader
    -- is least likely to work out unaided.
    advice = \case
      NoCanonRef _ -> line <> "  Fetch it, which a plain clone does not:" <> line
                      <> "    git fetch <remote> '+" <> pretty metaRef
                      <> ":" <> pretty metaRef <> "'" <> line
                      -- Three causes, and git gives this reader no way to tell
                      -- them apart (see the note in HBS2.Hub.Repo.Git), so all
                      -- three are named. The fetch above is the remedy for two of
                      -- them, which is why it comes first.
                      <> "  Or the ref is here and broken, or nothing has"
                      <> " published canon for this repository yet."
      -- NOT "check that git is installed", which this said until git-not-installed
      -- stopped arriving here: git ran to produce this and exited non-zero, so
      -- advertising a cause the constructor now rules out was the fixed bug left
      -- standing in the other half of the split.
      NoRepository _ -> line <> "  Run this inside a git repository. If you are in"
                          <> " one, git refused it:" <> line
                          <> "  read the message above -- an ownership complaint"
                          <> " is fixed with safe.directory."
      -- No mention of tags: this reader asks for @^{commit}@, which PEELS an
      -- annotated tag, so a ref pointing at one is accepted and audited. The
      -- advice said otherwise and was checked: exit 0, canon read.
      RefUnresolved _ -> line <> "  The ref is here and what it names is not."
                           <> " Fetch again, or unshallow." <> line
                           <> "  A ref pointing at a tree or a blob is not canon;"
                           <> " one pointing at a tag is" <> line
                           <> "  read as the commit the tag names."
      -- No single cause, so no single remedy: the message above is git's own, and
      -- claiming "a pruned object" over a permission error or a listing past the
      -- bound sent people to fetch what was already there.
      -- Split on WHO said it: git's complaints have several causes and only some
      -- are fixed by fetching, while this reader's own refusals (a bound reached,
      -- a format it cannot parse) are not fixed by fetching at all, and were
      -- getting "a pruned object or a partial clone".
      TreeUnreadable (ToolSaid _) ->
        line <> "  Read the message above: a pruned object or a partial clone is"
             <> " fixed by" <> line
             <> "  fetching, a permission or ownership complaint is not."
      TreeUnreadable (ReaderSays _) ->
        line <> "  This reader gave up or could not parse what it was given."
             <> " Fetching will not help;" <> line
             <> "  compaction (PEP-19) will, and so will reporting the git version"
             <> " if the format" <> line <> "  has moved."
      CanonTooNewHere _ -> line <> "  Upgrade; this build would fold it under"
                             <> " rules it does not implement."
      -- The one problem whose file cannot be named in the report, because it is
      -- not an event and the report is a report about events.
      --
      -- Two different remedies, and the wrong one used to be printed under a
      -- message that contradicted it: in a blobless clone the line above reads
      -- "the object is not in this clone; fetch, or unshallow" and the advice
      -- under it told the reader to go and ask whoever published canon to rewrite
      -- the file. That is the ordinary state after clone --filter=blob:none.
      -- Three remedies, and each of the three used to be printed under a message
      -- that contradicted it.
      VersionUnreadable FileListingUnparsed ->
        line <> "  This build could not read the LISTING RECORD for"
             <+> pathDoc versionPath <> ", not the file." <> line
             <> "  Neither fetching nor git fsck is the answer: upgrade, or report"
             <> " the git version." 
      VersionUnreadable p
        | notHere p -> line <> "  " <> pathDoc versionPath
                         <> " is listed and this clone did not read it. Fetching"
                         <> " may be the answer" <> line
                         <> "  (a partial or shallow clone); so may git fsck, if"
                         <> " the object is here and broken." <> line
                         <> "  Nothing is known to be wrong with what was"
                         <> " published."
        | otherwise -> line <> "  " <> pathDoc versionPath
                         <> " governs the admission rules, so this reader"
                         <> " will not guess" <> line
                         <> "  at them. Whoever published canon has to"
                         <> " rewrite that file."
      CanonTooBig _  -> line <> "  Compaction is the answer to a canon this"
                          <> " large (PEP-19), not a bigger reader."
      CanonTooMany _ -> line <> "  Compaction is the answer to a canon this"
                          <> " large (PEP-19), not a bigger reader."
      CanonListingTooBig _ -> line <> "  Compaction is the answer to a canon this"
                                <> " large (PEP-19), not a bigger reader."
      -- Three sources now, not two, and they do not share a remedy: the byte
      -- budget fires on a large tree, the walk's clock on a small tree that is
      -- expensive to walk, and the per-object deadline on ONE object arriving too
      -- slowly, which compaction does not fix and a sick disk explains. The line
      -- above says which, and this names the two answers rather than one.
      CanonTooSlow _ -> line <> "  Reading this tree cost more than this reader"
                          <> " will spend on one; the line" <> line
                          <> "  above says what ran out. For a tree, compaction"
                          <> " (PEP-19) is the answer," <> line
                          <> "  and so is not running this in a pre-receive hook"
                          <> " on somebody else's" <> line
                          <> "  tree. For a single object, look at the storage it"
                          <> " is coming off."
      -- The one refusal that is not about canon. Said so, because the others all
      -- are, and a reader who has seen the other ten will read this as the
      -- eleventh thing wrong with somebody else's tree.
      -- BOTH SHAPES, and naming only one of them is a mistake this line has made
      -- twice: first "git gone from PATH mid-audit", which missed git never
      -- having been there, and then the exec-only list, which missed a git that
      -- ran and was killed. The message above says which happened.
      ReaderFailed _ -> line <> "  This is local: git is not on PATH, nothing is"
                          <> " left to start it with," <> line
                          <> "  or the git this reader did start is gone. Nothing"
                          <> " was learned about" <> line
                          <> "  canon, one way or the other."
      -- Deliberately NOT the advice above, which this used to get. That one says
      -- a retry is worth trying; here git is sitting there, and a retry buys
      -- another minute and another stuck child. Reached by a FIFO at
      -- .git/refs/hbs2/meta, where rev-parse answers and show-ref blocks on open.
      ToolStalled _ -> line <> "  git is running and not answering, so retrying"
                          <> " buys another wait." <> line
                          <> "  Look for a FIFO or a dead mount in the object"
                          <> " store, or a filesystem" <> line
                          <> "  that is not responding."
      -- The listing arrived and parsed; the walk over its files did not. So no
      -- fetching, and no compaction either, which is what this got when it was
      -- reported as a tree that will not list.
      BlobsOutOfStep _ -> line <> "  The tree listed and its files did not read."
                            <> " This is this build and git" <> line
                            <> "  disagreeing about the cat-file protocol:"
                            <> " report the git version." <> line
                            <> "  Neither fetching nor compaction is the answer."

-- | What a refusal exits with.
--
-- Distinct codes, and chosen so a hook can say "audit could not run" without
-- enumerating them: 3 and up is a refusal, 2 is a completed audit that found
-- something, 0 a clean one. 1 is left to the argument and usage failures every
-- verb shares.
--
-- One code per constructor, with no pair sharing one. The two that did were the
-- two worth telling apart from a script: "the ref is here and broken" against
-- "the tree will not list", and "too big" against "too many", which differ in
-- which bound there is to argue with.
codeOf :: CanonUnreadable -> Int
codeOf = \case
  -- Split from 7 for the reason the advice is: the file being unreadable HERE is
  -- not the file being wrong, and 7 says canon is wrong.
  VersionUnreadable p | notHere p -> 13
  NoCanonRef{}         -> 3
  NoRepository{}       -> 4
  RefUnresolved{}      -> 5
  CanonTooNewHere{}    -> 6
  VersionUnreadable{}  -> 7
  CanonTooBig{}        -> 8
  TreeUnreadable{}     -> 9
  CanonTooMany{}       -> 10
  CanonListingTooBig{} -> 11
  CanonTooSlow{}       -> 14
  -- Appended, not slotted in: the numbers are a contract. 15 is git present and
  -- silent, which used to be 12 ("local, and worth retrying") or 4 ("not a git
  -- repository"); 16 is git present and talking nonsense, which used to be 9
  -- ("the tree will not list") about a listing that had already been read whole.
  ToolStalled{}        -> 15
  BlobsOutOfStep{}     -> 16
  -- Not a number about canon at all: a script that retries on it is retrying
  -- something local, which is the only one of these worth retrying. (It is no
  -- longer the highest, 13 having been added after it; the numbers are a
  -- contract, so they are appended to and not renumbered.)
  ReaderFailed{}       -> 12

report :: CanonState -> IO ()
report st = do
  -- A closed pipe is not a usage error, and 1 is what usage errors exit with:
  -- `hub verify | head -1` on a report past the buffer gave 1, which PEP-22
  -- assigns to a bad argument. 141 is 128 plus SIGPIPE, what a shell reports for
  -- a program a pipe killed.
  --
  -- Scoped to the printing of the report and nothing else. Wrapped around the
  -- whole dispatcher it also caught a usage error whose stderr had been closed,
  -- turning a documented 1 into a silent 141.
  handleJust (\e -> if isResourceVanishedError e then Just () else Nothing)
             (\_ -> exitWith (ExitFailure 141))
             (for_ (reportDoc st) print)
  -- Non-zero when there is anything to act on, so this is usable in a hook.
  case reportCode st of
    0 -> pure ()
    n -> exitWith (ExitFailure n)

-- | The audit, one 'Doc' per line.
--
-- Pure, and split out from the printing for one reason: this is the output the
-- whole verb exists to produce, and nothing exercised it. Every line here is a
-- line about a stranger's bytes.
reportDoc :: CanonState -> [Doc ann]
reportDoc st =
  -- The owner key is on the header line, because an audit is an audit AGAINST a
  -- key and the report did not say which. The likeliest mistake anybody makes
  -- with this verb is pasting a fork's key instead of upstream's, and its result
  -- is "admitted 0 dropped 57", which reads exactly like mass forgery.
  [ "canon" <+> pretty (stCommit st)
      <+> parens (maybe "no version file" (("hub-meta" <+>) . pretty) (stVersion st))
      <+> "owner" <+> pretty (AsBase58 (frOwner (stFold st))) ]

  -- A line of its own, and a finding, because PEP-19 requires the file. As a
  -- parenthesis on the header it was the only thing in this report that a reader
  -- had to notice unprompted, and the audit exited zero over it. The fold used the
  -- oldest rules, which is the safe guess and still a guess.
  <> [ "no version file" <+> pathDoc versionPath <> ":"
         <+> "PEP-19 requires it; folded under hub-meta"
         <+> pretty assumedMetaVersion
     | stVersion st == Nothing ]

  -- The unreadable files first: a file nothing could parse never became an
  -- event, so no drop or anomaly below can mention it, and a reader who stopped
  -- at the fold's own report would not learn it existed.
  --
  -- Through pathDoc, because a path is a stranger's bytes: the tree is read with
  -- -z precisely because a path may contain anything but NUL, and a file named
  -- with a newline and a plausible summary line forged the last line of this
  -- report. Every other stranger's text in this project already goes through it.
  <> capped "unreadable"
       [ "unreadable" <+> pathDoc p <> ":" <+> pretty e | (p, e) <- stBad st ]

  -- Files whose own version clause did not read. Reported and never obeyed
  -- (PEP-19), and reported at all because a writer that appends to this tree is
  -- about to rewrite those files with its own version.
  -- "no READABLE file version": the clause may be absent, unreadable or listed
  -- twice, and the parser answers Nothing to all three, so the line said the one
  -- of them that is not always true.
  -- "no single readable": absent, malformed and listed twice all arrive here as
  -- Nothing, and the line used to name only the first of the three. A maintainer
  -- who opened the file, saw a perfectly good clause and stopped believing the
  -- audit was reading a file with two of them.
  <> capped "unversioned"
       [ "no single readable file version" <+> pathDoc p | p <- noFileVersion st ]

  -- Reported and still folded, for the reason 'NameProblem' gives: the name is
  -- normative (PEP-19) and no signature covers it, so a reader that dropped a
  -- misnamed file would show less than canon holds and disagree with every other
  -- clone over a byte nobody signed.
  <> capped "misnamed"
       [ "misnamed" <+> pathDoc p <> ":" <+> pretty np | (p, np) <- stMisnamed st ]

  <> capped "dropped" (fmap pretty dropped)
  <> capped "anomaly" (fmap pretty anomalies)

  -- Counts, not a partition, and said so: an event with two anomalies adds one
  -- to admitted and two to anomalies.
  -- EVERY finding, because a hook reads this line and the exit code, and the two
  -- disagreed: three "no readable file version" lines above, all zeroes here, and
  -- exit 2. A summary that omits two of the five things reportCode counts is a
  -- summary of a different audit.
  -- What the fold DECIDED, not only what it refused. A tree with one valid
  -- version file and nothing else printed "admitted 0 dropped 0 anomalies 0" and
  -- exited zero, which is indistinguishable from a healthy tracker: the two
  -- numbers that tell them apart are how many keys may write and how many events
  -- are withheld.
  <> [ "maintainers" <+> pretty (HS.size (frMaintainers fr))
         <+> "redacted" <+> pretty (HS.size (frRedacted fr)) ]

  <> [ "admitted" <+> pretty (length (frLog fr))
         <+> "dropped" <+> pretty (length dropped)
         <+> "anomalies" <+> pretty (length anomalies)
         <+> "unreadable" <+> pretty (length (stBad st))
         <+> "unversioned" <+> pretty (length (noFileVersion st))
         <+> "misnamed" <+> pretty (length (stMisnamed st))
         <+> "tree-version" <+> maybe "missing" (const "ok") (stVersion st) ]
  where
    fr = stFold st
    dropped = frDropped fr
    anomalies = frAnomalies fr

    -- A BOUND ON THE REPORT, which had none of any kind.
    --
    -- Every list above is one line per path, and every bound this reader has is
    -- on what it READS. A path that is not where canon puts things never reaches
    -- a blob, so the read budget never sees it, and it is printed all the same:
    -- measured, 4555 bytes of git objects produced 369 600 298 bytes of stdout
    -- and 661 MB resident, exiting 2. Eighty thousand times the input, out of a
    -- repository that fits in an email, into the terminal of whoever ran the
    -- hook.
    --
    -- The COUNTS are not capped, because they are one line each and they are what
    -- the exit code is computed from. Cutting the lines and keeping the totals is
    -- what makes the cut safe to make: nothing a script reads changes.
    --
    -- What is left is 1000 lines a section, and 'pathDoc' spends up to 1800
    -- characters on one path, so the worst case is still a few megabytes rather
    -- than a few hundred bytes. That is a terminal being filled, not a machine
    -- being taken down, and shrinking pathDoc's own budget is the fix for it.
    capped what xs
      | n <= maxReportLines = xs
      | otherwise = List.take maxReportLines xs
          <> [ "and" <+> pretty (n - maxReportLines) <+> "more" <+> what
                 <+> "lines, not printed; the counts below are of all of them" ]
      where n = length xs

    maxReportLines = 1000

-- | What a completed audit exits with: 2 if it found anything, 0 otherwise.
--
-- All three, and not only the drops: an anomaly is admitted canon that should not
-- exist, which is exactly what an audit is for, and an unreadable file is a file
-- somebody has to look at.
--
-- And a missing version file, which PEP-19 requires. It cannot be a refusal,
-- because the tree still folds under the oldest rules and refusing would show
-- less than canon holds; it cannot be nothing either, because what governs the
-- admission rules is absent and unsigned.
reportCode :: CanonState -> Int
reportCode st
  | clean = 0
  | otherwise = 2
  where
    fr = stFold st
    -- Every line the report prints as a finding, and nothing else. The two that
    -- were printed and not counted are the missing tree version and the files
    -- with no version clause of their own: stdout listed them, 0 said 0, and
    -- PEP-22 defines 0 as an audit that found nothing.
    clean = List.null (frDropped fr) && List.null (frAnomalies fr)
              && List.null (stBad st)
              && List.null (noFileVersion st)
              && List.null (stMisnamed st)
              && stVersion st /= Nothing

-- Files whose own version clause did not read.
noFileVersion :: CanonState -> [ByteString]
noFileVersion st = [ p | (p, Nothing) <- stFileVersions st ]

-- A tree path on a terminal: escaped for display, and injectively, so two paths
-- that differ read differently. A path is a stranger's bytes, and one with a
-- newline in it forged the summary line of this report before this was here. See
-- 'pathText'.
--
-- TRUNCATED, because a git tree entry may hold a path of any length (measured: a
-- single 1 048 638-byte record) and pathText spends up to six characters on a
-- byte, so one entry could put six megabytes on somebody's terminal and a full
-- listing six hundred. The head is what identifies a file; the tail is what an
-- author chose to make it unreadable with.
pathDoc :: ByteString -> Doc ann
pathDoc p
  | BS.length p <= keep = pretty (pathText p)
  -- Truncated AND still injective, which the first attempt at this was not: it
  -- cut the escaped text at 300 characters and appended the byte length, so two
  -- paths of equal length sharing a 300-character prefix printed as one line --
  -- exactly the collision this whole notation exists to prevent, reintroduced by
  -- the fix for the size of one line. It also cut off the closing quote, counted
  -- escaped characters as if they were bytes, and could cut inside an escape.
  --
  -- The hash is what keeps the property: the prefix is cut on RAW BYTES (so no
  -- escape is split), the length and the digest of the whole path follow, and two
  -- paths that differ differ in at least one of the three.
  | otherwise = pretty (pathText (BS.take keep p))
                  <+> parens ( "truncated;" <+> pretty (BS.length p) <+> "bytes,"
                                 <+> pretty (Text.take 16 (digest p)) )
  where
    keep = 300
    digest = Text.pack . show . pretty . hashObject @HbSync
