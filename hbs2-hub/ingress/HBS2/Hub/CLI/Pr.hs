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
  ) where

import HBS2.Hub.Types
import HBS2.Hub.Letter
import HBS2.Hub.Ingress (rpcTimeout)
import HBS2.Hub.Repo.GitBundle
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

  where

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
