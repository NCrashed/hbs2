-- | @hub pr new@ (PEP-20 delta path, PEP-22 "Contribute").
--
-- The contributor's end of a pull request. It bundles the range, attaches the
-- bundle, and signs coordinates that name it, in that order, because the order
-- is the whole difficulty: a part is named by the hash of its encrypted tree
-- and PEP-18 puts that hash inside the signed box, so the bundle exists before
-- the box exists.
--
-- WHAT IS SIGNED and what is not. @source-tip@ and @base@ go inside the inner
-- box, so the proposed commit and the fork point are authenticated to the
-- contributor and a maintainer can check the objects that arrive against them.
-- The bundle itself is not signed and does not need to be: git's own hashing
-- binds its contents to @source-tip@, so a tampered bundle cannot produce the
-- signed tip.
--
-- The tip comes from the BUNDLE, not from a second lookup. It is the tip of
-- the ref the bundle actually recorded, which is what the maintainer's fetch
-- will produce; asking git a second time would be asking a different question
-- (what the ref points at now) and the two can differ by one commit made
-- between the calls.
module HBS2.Hub.CLI.Pr
  ( prEntries
  , prNewUsage
  , prNewArgs
  , PrNew(..)
  , prReviseUsage
  , prReviseArgs
  , PrRevise(..)
  , codeBundleFailed
  , codeNoSuchPr
  , codeNotMerged
  , prMergeUsage
  , prMergeArgs
  , PrMerge(..)
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Ingress (rpcTimeout)
import HBS2.Hub.Bridge
import HBS2.Hub.Fold
import HBS2.Hub.Repo
import HBS2.Hub.Repo.Git (withGitCanon)
import HBS2.Hub.Repo.GitWrite (withGitSink)
import HBS2.Hub.Repo.GitBundle
import HBS2.Hub.CLI.Argv (flagsOf,flagOnce,flagMaybe,flagText,flagWord)
import HBS2.Hub.CLI.Verify (codeOf)
import HBS2.Hub.CLI.Inbox (PeerSilent(..),refuse,codePeerSilent)
import HBS2.Hub.CLI.Compose (Outbound(..),attachToLetter,sendLetterWith,codeNoKey,readBody,letterBody
                            ,NotStored(..),codeNotStored,PoWTooHard(..),codeNoWork)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Net.Auth.Credentials
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,loadCredentials)

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Data.Text qualified as Text
import System.Exit (die)

-- | git would not build or would not answer.
--
-- Its own code because nothing about the letter is wrong: the range is empty,
-- the ref does not exist, this is not a repository. All of them are fixed in
-- the working tree and none by retyping the command.
codeBundleFailed :: Int
codeBundleFailed = 27

-- | What @hub pr new@ was asked to propose.
data PrNew = PrNew
  { pnRepo   :: RepoRef
  , pnSender :: HashRef       -- ^ the contributor's sigil
  , pnRcpt   :: HashRef       -- ^ the hub's sigil
  , pnAuthor :: HubKey        -- ^ the key that signs the inner box
  , pnTitle  :: Text
  , pnOnto   :: Text          -- ^ the branch being proposed into
  , pnFrom   :: Text          -- ^ the branch being proposed
  , pnBase   :: Maybe Text    -- ^ override for the fork point
  , pnBody   :: Maybe Text    -- ^ --body, where "-" means stdin
  }
  deriving stock (Eq,Show)

prNewUsage :: Doc ()
prNewUsage =
  "usage: hbs2-hub pr new --target <repo-key> --sender <sigil> --recipient <sigil>"
    <> line <> "       --author <key> --title <text> --onto <ref> --from <ref> [--base <sha>]"

prEntries :: forall c m . ( IsContext c
                          , MonadUnliftIO m
                          , HasStorage m
                          , HasClientAPI MailboxAPI UNIX m
                          , Exception (BadFormException c)
                          ) => MakeDictM c m ()
prEntries = do

  brief "propose a change: bundle a range and send it as a Tier B letter"
    $ args [ arg "string" "--target repo-key", arg "string" "--sender sender-sigil"
           , arg "string" "--recipient recipient-sigil", arg "string" "--author author-key"
           , arg "string" "--title title", arg "string" "--onto ref"
           , arg "string" "--from ref" ]
    $ desc ( "Builds a git bundle of base..--from in THIS repository and ships"
             <> line <> "it as an encrypted attachment (PEP-20's delta path). The"
             <> line <> "bundle's size is the delta, so proposing a change to a"
             <> line <> "large repository does not transfer the repository."
             <> line
             <> line <> "--base defaults to the merge-base of --onto and --from,"
             <> line <> "which is what a contributor almost always means. Name it"
             <> line <> "only when git's answer is not: a rebase onto something"
             <> line <> "the maintainer does not have yet is the case."
             <> line
             <> line <> "The proposed commit and the fork point are signed, so the"
             <> line <> "maintainer checks the objects that arrive against them."
             <> line <> "The bundle itself is not signed and does not need to be:"
             <> line <> "git's hashing binds its contents to the tip."
             <> line
             <> line <> "The body comes from --body, and --body - reads it from"
             <> line <> "stdin. Nothing is read from stdin otherwise: git hands a"
             <> line <> "hook <old> <new> <ref-name> there, and a body goes inside"
             <> line <> "the signature and the event-id, where canon cannot fix it."
             <> line
             <> line <> "Prints the message hash and the thread-id, which the"
             <> line <> "sender can compute before any maintainer has looked." )
    $ entry $ bindMatch "hub:pr:new" \case
        (prNewArgs -> Just pn) -> lift (prNew pn)
          >> pure nil
        _ -> liftIO (die (show prNewUsage))

  brief "propose new coordinates for a pull request you opened"
    $ args [ arg "string" "--sender sender-sigil"
           , arg "string" "--recipient recipient-sigil"
           , arg "string" "--author author-key", arg "string" "--thread thread-id"
           , arg "string" "--onto ref", arg "string" "--from ref" ]
    $ desc ( "Builds a fresh bundle and signs coordinates that replace the"
             <> line <> "ones canon holds for this thread (PEP-20). The thread"
             <> line <> "keeps its number, its title and its comments; what"
             <> line <> "changes is what is being proposed."
             <> line
             <> line <> "THE AUTHOR OF RECORD signs it: the key that opened the"
             <> line <> "thread. A revision signed by anybody else is refused at"
             <> line <> "triage, and the refusal happens on the maintainer's"
             <> line <> "machine with no way to tell you, since the ack path is"
             <> line <> "unbuilt (PEP-18). A maintainer wanting to change a"
             <> line <> "proposal asks for it in a comment."
             <> line
             <> line <> "No title and no body: a revision carries coordinates and"
             <> line <> "nothing else. Say what changed in a comment."
             <> line
             <> line <> "--base defaults to the merge-base of --onto and --from,"
             <> line <> "which is what a contributor almost always means. After a"
             <> line <> "rebase that is the answer that has moved, so it is worth"
             <> line <> "reading the base this prints." )
    $ entry $ bindMatch "hub:pr:revise" $ nil_ \case
        (prReviseArgs -> Just pr) -> lift (prRevise pr)
        _ -> liftIO (die (show prReviseUsage))

  brief "record that a pull request was merged"
    $ args [ arg "string" "--repo repo-key", arg "string" "--number n"
           , arg "string" "--commit sha", arg "string" "--into ref" ]
    $ desc ( "RECORDS a merge; it does not perform one. PEP-20 leaves the"
             <> line <> "integration to whatever policy the repository uses --"
             <> line <> "merge, rebase, squash, fast-forward -- so do that with"
             <> line <> "git, push the branch, and then tell canon what happened."
             <> line
             <> line <> "What it checks is the claim it is about to publish: that"
             <> line <> "--commit really contains the tip the contributor signed"
             <> line <> "for. Canon is append-only, so a merge event naming a"
             <> line <> "commit that does not carry the proposal would be a false"
             <> line <> "statement in every clone forever."
             <> line
             <> line <> "The merge event sets the status to merged by itself"
             <> line <> "(PEP-19). No second 'set' is written and none should be:"
             <> line <> "canon would claim a merged pull request was open until it"
             <> line <> "arrived." )
    $ entry $ bindMatch "hub:pr:merge" $ nil_ \case
        (prMergeArgs -> Just pm) -> lift (prMerge pm)
        _ -> liftIO (die (show prMergeUsage))

  where

    prMerge pm = do
      creds <- runKeymanClientRO (loadCredentials (pmAs pm))
                 >>= maybe (liftIO (refuse (show ("no signing key here for"
                                                   <+> pretty (AsBase58 (pmAs pm))))
                                           codeNoKey))
                           pure

      (parent, fr) <- withGitCanon (\cs -> readCanon cs (pmRepo pm)) >>= \case
        Right st -> pure (Just (stCommit st), stFold st)
        Left e -> liftIO (refuse (show (pretty e)) (codeOf e))

      -- The thread by its number, which is what a person has in front of them.
      -- Canon is the only place that maps one to the other, and it is the map
      -- the fold rebuilt rather than the convenience index in the tree.
      t <- case [ x | x <- HM.elems (frThreads fr), tsNumber x == Just (pmNumber pm) ] of
             (x:_) -> pure x
             []    -> liftIO $ refuse (show ("canon holds no thread numbered"
                                               <+> pretty (pmNumber pm)))
                                      codeNoSuchPr

      pr <- case (tsKind t, tsPR t) of
              (HubPR, Just pr) -> pure pr
              _ -> liftIO $ refuse (show ("#" <> pretty (pmNumber pm)
                                            <+> "is not a pull request"))
                                   codeNoSuchPr

      -- THE CHECK THIS VERB EXISTS FOR. A merge event says a proposal was
      -- integrated; if the commit it names does not contain the tip the
      -- contributor signed for, that sentence is false, and canon is
      -- append-only. git answers it exactly.
      let tip = prSourceTip (psCoords pr)
      anc <- isAncestor Nothing tip (pmCommit pm)
               >>= either (\e -> liftIO (refuse (show (pretty e)) codeBundleFailed)) pure
      unless anc $
        liftIO $ refuse (show ( "the commit named does not contain the proposed tip"
                                  <> line <> "  proposed" <+> pretty tip
                                  <> line <> "  merge   " <+> pretty (pmCommit pm)
                                  <> line <> "  nothing was written" ))
                        codeNotMerged

      now <- liftIO getPOSIXTime <&> floor . (* 1000)

      let ctx = TriageCtx (pmAs pm, _peerSignSk creds) (const True) (pmRepo pm)
          content = AMerge (tsId t) (pmCommit pm) (pmInto pm) now

      acc <- either (\e -> liftIO (refuse (show ("refused:" <+> viaShow e)) codeNotMerged))
                    pure
               (ownerEvent ctx (viewOf fr) now noOwnAttachments content)

      plan <- either (\e -> liftIO (refuse (show (pretty e)) codeNotMerged)) pure
                (planCanon [(eventPath acc, acEvent acc)] (numberIndexOf fr))

      commit <- withGitSink (\sk -> skCommit sk (CanonWrite parent (cwFiles plan)
                                                   ("hub: merged #" <> tshow (pmNumber pm))
                                                   now))
                  >>= either (\e -> liftIO (refuse (show (pretty e)) codeNotMerged)) pure

      liftIO $ print $ vcat
        [ "merged #" <> pretty (pmNumber pm) <+> "into" <+> pretty (pmInto pm)
        , "event" <+> pretty (eventId (acEvent acc))
        , "commit" <+> pretty commit
        , "status is now merged; PEP-19 has the merge event set it, so no"
            <+> "second event was written"
        ]

    tshow :: Word64 -> Text
    tshow = fromString . show

    prNew pn = do

      body <- liftIO (readBody (fmap Text.unpack (pnBody pn)))

      creds <- signingKey (pnAuthor pn)

      (b, base, part) <- propose (pnAuthor pn) (pnSender pn) (pnRcpt pn)
                                 (pnOnto pn) (pnFrom pn) (pnBase pn)

      now <- liftIO getPOSIXTime <&> floor . (* 1000)

      -- TWO: coordinates that name it, signed.
      --
      -- prSource is Nothing: this is the delta path, and PEP-20 says a letter
      -- on it may omit the fork pointer. What makes the change fetchable is
      -- the bundle, which the fold checks for (reachableCoords).
      let coords = PRCoords Nothing (pnFrom pn) (bnTip b) (pnOnto pn) base (Just part)
          content = AOpen (pnRepo pn) HubPR (pnTitle pn) [] (letterBody body)
                          Nothing (Just coords) now

      box <- sealed (pnAuthor pn) creds content

      h <- send (pnAuthor pn) (pnSender pn) (pnRcpt pn) part box

      liftIO $ print $ vcat
        [ "queued" <+> pretty h
        , "thread" <+> pretty (authorBoxId box)
        , "tip" <+> pretty (bnTip b) <+> "base" <+> pretty base
        , "bundle" <+> hashDoc (ptPart part)
            <+> parens (pretty (BS.length (bnBytes b)) <+> "bytes before encryption")
        ]

    -- | @hub pr revise@: the same two steps, against a thread that exists.
    --
    -- No title and no body, because 'ARevise' carries neither: PEP-20 makes a
    -- revision a change of COORDINATES and nothing else, so what a contributor
    -- wants to say about it is a comment, which is its own op and its own verb.
    prRevise pr = do
      creds <- signingKey (pvAuthor pr)

      (b, base, part) <- propose (pvAuthor pr) (pvSender pr) (pvRcpt pr)
                                 (pvOnto pr) (pvFrom pr) (pvBase pr)

      now <- liftIO getPOSIXTime <&> floor . (* 1000)

      let coords = PRCoords Nothing (pvFrom pr) (bnTip b) (pvOnto pr) base (Just part)
          content = ARevise (pvThread pr) coords now

      box <- sealed (pvAuthor pr) creds content

      h <- send (pvAuthor pr) (pvSender pr) (pvRcpt pr) part box

      liftIO $ print $ vcat
        [ "queued" <+> pretty h
        , "event" <+> pretty (authorBoxId box)
        , "on" <+> pretty (pvThread pr)
        , "tip" <+> pretty (bnTip b) <+> "base" <+> pretty base
        , "bundle" <+> hashDoc (ptPart part)
            <+> parens (pretty (BS.length (bnBytes b)) <+> "bytes before encryption")
        -- Said here because the refusal happens on the maintainer's machine,
        -- hours later, with no path back to the sender (PEP-18: the ack is
        -- unbuilt). The bridge is stricter than the fold about this on purpose.
        , "the author of record signs a revision: a letter signed by anyone else"
            <+> "is refused at triage"
        ]

    -- The two steps that make a proposal, shared by both verbs above. The order
    -- is the whole difficulty: a part is named by the hash of its encrypted
    -- tree and PEP-18 puts that hash inside the signed box, so the bundle has
    -- to exist before the box does.
    propose author sender rcpt onto from mbase = do
      -- The fork point first, because the bundle is a range from it and
      -- because it is what the maintainer will be told to check against.
      base <- case mbase of
                Just b  -> pure b
                Nothing -> gitOr =<< mergeBase Nothing onto from

      b <- gitOr =<< bundleRange Nothing base from

      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX
      let ob = Outbound sto api rpcTimeout

      -- ONE: the attachment, so it has a name and a proof that it is ours.
      parts <- attachToLetter ob author sender [rcpt]
                 [ ( [ ("file-name", "pr.bundle")
                     , ("mime-type", "application/x-git-bundle") ]
                   , pure (LBS.fromStrict (bnBytes b)) ) ]

      part <- case parts of
                [p] -> pure p
                _   -> liftIO (refuse "the attachment was not stored" codeBundleFailed)

      pure (b, base, part)

    signingKey k =
      runKeymanClientRO (loadCredentials k)
        >>= maybe (liftIO (refuse (show ("no signing key here for"
                                          <+> pretty (AsBase58 k)))
                                  codeNoKey))
                  pure

    sealed author creds content = do
      -- The same bound the fold will apply, before anything is signed: an
      -- oversized field inside a signed box is a letter no hub will fold, and
      -- the signature cannot be redone over less.
      for_ (oversizedField content) $ \f ->
        liftIO $ refuse (show ("over the size limit for a letter:" <+> pretty f)) 1
      pure (signAuthor author (_peerSignSk creds) content)

    send author sender rcpt part box = do
      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX
      let ob = Outbound sto api rpcTimeout
      -- Every way sending can fail, and two of these were leaking out through
      -- the RTS as exit 1 -- the code PEP-22 gives to a mistyped flag. A hook
      -- that cannot tell "your arguments are wrong" from "the letter is in no
      -- mailbox" retries the wrong one of them.
      sendLetterWith ob sender [rcpt] [ptPart part] box (ReplyTo author sender)
        `catch` (\(e :: PeerSilent) -> liftIO (refuse (show e) codePeerSilent))
        `catch` (\(e :: NotStored)  -> liftIO (refuse (show e) codeNotStored))
        `catch` (\(e :: PoWTooHard) -> liftIO (refuse (show e) codeNoWork))

    gitOr = either (\e -> liftIO (refuse (show (pretty e)) codeBundleFailed)) pure

-- Every value behind a flag, for the reason `hub inbox accept` has none
-- positional: a repo key, a sigil hash and an author key are all thirty-two
-- bytes of base58, so nothing here can be told apart by position.
prNewArgs :: forall c . IsContext c => [Syntax c] -> Maybe PrNew
prNewArgs syn = do
  kvs    <- flagsOf knownFlags syn
  repo   <- flagOnce kvs "--target"    >>= asKey
  sender <- flagOnce kvs "--sender"    >>= asHash
  rcpt   <- flagOnce kvs "--recipient" >>= asHash
  author <- flagOnce kvs "--author"    >>= asKey
  title  <- flagOnce kvs "--title"     >>= asText
  onto   <- flagOnce kvs "--onto"      >>= asText
  from   <- flagOnce kvs "--from"      >>= asText
  base   <- flagMaybe kvs "--base" asText
  -- Named, not taken off stdin: see 'readBody'. A hook that opens a pull
  -- request is exactly the caller that has git's own bytes on its stdin.
  body   <- flagMaybe kvs "--body" asText
  pure (PrNew repo sender rcpt author title onto from base body)
  where
    -- The list is the point: this reader used to pair each word with the next
    -- and ask no more, so `--title --onto refs/heads/master` bound the title
    -- `--onto` and signed it into the event-id, and `--dry-run` was dropped on
    -- the floor. Both guards live in 'flagsOf' now.
    knownFlags = [ "--target","--sender","--recipient","--author"
                 , "--title","--onto","--from","--base","--body" ]

    asKey  = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    asHash = \case { HashLike h -> Just h ; _ -> Nothing }
    -- Through 'flagText', so a branch or a title that spells a number is one:
    -- `--from 2026` is a branch name, and this refused it as a usage error.
    asText = fmap Text.pack . flagText

-- | What @hub pr revise@ was asked to propose instead.
--
-- 'PrNew' minus the repository and the title, plus the thread: the repository
-- is not named because the thread names it (the fold makes that binding), and
-- the title is not named because a revision does not change one.
data PrRevise = PrRevise
  { pvSender :: HashRef
  , pvRcpt   :: HashRef
  , pvAuthor :: HubKey       -- ^ must be the thread's author of record
  , pvThread :: ThreadId
  , pvOnto   :: Text
  , pvFrom   :: Text
  , pvBase   :: Maybe Text
  }
  deriving stock (Eq,Show)

prReviseUsage :: Doc ()
prReviseUsage =
  "usage: hbs2-hub pr revise --sender <sigil> --recipient <sigil> --author <key>"
    <> line <> "       --thread <thread-id> --onto <ref> --from <ref> [--base <sha>]"

prReviseArgs :: forall c . IsContext c => [Syntax c] -> Maybe PrRevise
prReviseArgs syn = do
  kvs    <- flagsOf [ "--sender","--recipient","--author","--thread"
                    , "--onto","--from","--base" ] syn
  sender <- flagOnce kvs "--sender"    >>= asHash
  rcpt   <- flagOnce kvs "--recipient" >>= asHash
  author <- flagOnce kvs "--author"    >>= asKey
  thread <- flagOnce kvs "--thread"    >>= asHash
  onto   <- flagOnce kvs "--onto"      >>= asText
  from   <- flagOnce kvs "--from"      >>= asText
  base   <- flagMaybe kvs "--base" asText
  pure (PrRevise sender rcpt author thread onto from base)
  where
    asKey  = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    asHash = \case { HashLike h -> Just h ; _ -> Nothing }
    asText = fmap Text.pack . flagText

-- | What @hub pr merge@ was asked to record.
data PrMerge = PrMerge
  { pmRepo   :: RepoRef
  , pmNumber :: Word64
  , pmCommit :: Text        -- ^ the merge commit, in this repository
  , pmInto   :: Text        -- ^ the branch it landed on
  , pmAs     :: HubKey      -- ^ the canon key; defaults to the repo key
  }
  deriving stock (Eq,Show)

prMergeUsage :: Doc ()
prMergeUsage =
  "usage: hbs2-hub pr merge --repo <key> --number <n> --commit <sha> --into <ref> [--as <key>]"

-- | Canon holds no such pull request, or the number names something else.
codeNoSuchPr :: Int
codeNoSuchPr = 29

-- | The merge was not recorded, and canon is unchanged.
--
-- One code for every way of stopping, because they all mean the same thing to
-- whoever runs this: nothing was published and the repository is as it was.
codeNotMerged :: Int
codeNotMerged = 30

prMergeArgs :: forall c . IsContext c => [Syntax c] -> Maybe PrMerge
prMergeArgs syn = do
  kvs  <- flagsOf ["--repo","--number","--commit","--into","--as"] syn
  repo <- flagOnce kvs "--repo"   >>= asKey
  -- A number, non-negative AND small enough to be the one that was typed:
  -- 'flagWord' owns both ends. `--number 18446744073709551617` used to wrap to
  -- 1 and record a merge against a pull request nobody named.
  n    <- flagOnce kvs "--number" >>= flagWord
  sha  <- flagOnce kvs "--commit" >>= asText
  into <- flagOnce kvs "--into"   >>= asText
  as   <- flagMaybe kvs "--as" asKey
  pure (PrMerge repo n sha into (fromMaybe repo as))
  where
    asKey  = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    -- Through 'flagText': an abbreviated sha can be all digits, and one in
    -- twenty-seven of the seven-character ones is.
    asText = fmap Text.pack . flagText
