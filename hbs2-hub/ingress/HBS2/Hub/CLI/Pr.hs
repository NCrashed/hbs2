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
import HBS2.Hub.CLI.Verify (codeOf)
import HBS2.Hub.CLI.Inbox (PeerSilent(..),refuse,codePeerSilent)
import HBS2.Hub.CLI.Compose (Outbound(..),attachToLetter,sendLetterWith,codeNoKey)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.Refs (pattern HashLike)
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
import Data.Time.Clock.POSIX (getPOSIXTime)
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
             <> line <> "The body is read from stdin when stdin is a pipe or a file."
             <> line
             <> line <> "Prints the message hash and the thread-id, which the"
             <> line <> "sender can compute before any maintainer has looked." )
    $ entry $ bindMatch "hub:pr:new" \case
        (prNewArgs -> Just pn) -> lift (prNew pn)
          >> pure nil
        _ -> liftIO (die (show prNewUsage))

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

      body <- liftIO $ hIsTerminalDevice stdin >>= \case
                True  -> pure ""
                False -> getContents

      creds <- runKeymanClientRO (loadCredentials (pnAuthor pn))
                 >>= maybe (liftIO (refuse (show ("no signing key here for"
                                                   <+> pretty (AsBase58 (pnAuthor pn))))
                                           codeNoKey))
                           pure

      -- The fork point first, because the bundle is a range from it and
      -- because it is what the maintainer will be told to check against.
      base <- case pnBase pn of
                Just b  -> pure b
                Nothing -> gitOr =<< mergeBase Nothing (pnOnto pn) (pnFrom pn)

      b <- gitOr =<< bundleRange Nothing base (pnFrom pn)

      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX
      let ob = Outbound sto api rpcTimeout

      -- ONE: the attachment, so it has a name.
      parts <- attachToLetter ob (pnSender pn) [pnRcpt pn]
                 [ ( [ ("file-name", "pr.bundle")
                     , ("mime-type", "application/x-git-bundle") ]
                   , pure (LBS.fromStrict (bnBytes b)) ) ]

      part <- case parts of
                [p] -> pure p
                _   -> liftIO (refuse "the attachment was not stored" codeBundleFailed)

      now <- liftIO getPOSIXTime <&> floor . (* 1000)

      -- TWO: coordinates that name it, signed.
      --
      -- prSource is Nothing: this is the delta path, and PEP-20 says a letter
      -- on it may omit the fork pointer. What makes the change fetchable is
      -- the bundle, which the fold checks for (reachableCoords).
      let coords = PRCoords Nothing (pnFrom pn) (bnTip b) (pnOnto pn) base (Just part)
          content = AOpen (pnRepo pn) HubPR (pnTitle pn) [] (bodyOf body)
                          Nothing (Just coords) now

      for_ (oversizedField content) $ \f ->
        liftIO $ refuse (show ("over the size limit for a letter:" <+> pretty f)) 1

      let box = signAuthor (pnAuthor pn) (_peerSignSk creds) content

      h <- sendLetterWith ob (pnSender pn) [pnRcpt pn] [part] box noReplyChannel
             `catch` (\(e :: PeerSilent) -> liftIO (refuse (show e) codePeerSilent))

      liftIO $ print $ vcat
        [ "queued" <+> pretty h
        , "thread" <+> pretty (authorBoxId box)
        , "tip" <+> pretty (bnTip b) <+> "base" <+> pretty base
        , "bundle" <+> pretty part
            <+> parens (pretty (BS.length (bnBytes b)) <+> "bytes before encryption")
        ]

    gitOr = either (\e -> liftIO (refuse (show (pretty e)) codeBundleFailed)) pure

    -- A trailing newline from the shell is not part of the body, and an empty
    -- body is absent rather than a zero-length one.
    bodyOf s = case Text.dropWhileEnd (== '\n') (Text.pack s) of
                 t | Text.null t -> Nothing
                   | otherwise   -> Just t

-- Every value behind a flag, for the reason `hub inbox accept` has none
-- positional: a repo key, a sigil hash and an author key are all thirty-two
-- bytes of base58, so nothing here can be told apart by position.
prNewArgs :: forall c . [Syntax c] -> Maybe PrNew
prNewArgs syn = do
  repo   <- flagged "--target" asKey
  sender <- flagged "--sender" asHash
  rcpt   <- flagged "--recipient" asHash
  author <- flagged "--author" asKey
  title  <- flagged "--title" asText
  onto   <- flagged "--onto" asText
  from   <- flagged "--from" asText
  pure (PrNew repo sender rcpt author title onto from (flagged "--base" asText))
  where
    flagged :: forall v . String -> (Syntax c -> Maybe v) -> Maybe v
    flagged n f = case [ v | (StringLike n', f -> Just v) <- zip syn (drop 1 syn)
                           , n' == n ] of
                    [v] -> Just v
                    _   -> Nothing

    asKey  = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    asHash = \case { HashLike h -> Just h ; _ -> Nothing }
    asText = \case { StringLike s -> Just (Text.pack s) ; _ -> Nothing }

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

prMergeArgs :: forall c . [Syntax c] -> Maybe PrMerge
prMergeArgs syn = do
  repo <- flagged "--repo" asKey
  n    <- flagged "--number" asNum
  sha  <- flagged "--commit" asText
  into <- flagged "--into" asText
  pure (PrMerge repo n sha into (fromMaybe repo (flagged "--as" asKey)))
  where
    flagged :: forall v . String -> (Syntax c -> Maybe v) -> Maybe v
    flagged k f = case [ v | (StringLike k', f -> Just v) <- zip syn (drop 1 syn)
                           , k' == k ] of
                    [v] -> Just v
                    _   -> Nothing

    asKey  = \case { SignPubKeyLike k -> Just k ; _ -> Nothing }
    asText = \case { StringLike s -> Just (Text.pack s) ; _ -> Nothing }
    -- A number, and a non-negative one. An issue number is a Word64 in canon,
    -- and a negative literal would wrap into one nobody minted.
    asNum  = \case
      LitIntVal i | i >= 0 -> Just (fromIntegral i :: Word64)
      _ -> Nothing
