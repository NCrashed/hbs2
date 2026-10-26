{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git3.Repo.Init
  ( initRepo
  , newRepoOpt
  , encryptedNewOpt
  , forceOpt
  , relayOnlyRepo
  , ExistingRef(..)
  , ReinitDecision(..)
  , decideReinit
  , RepoInitError(..)
  ) where

import HBS2.Git3.Prelude
import HBS2.Git3.State
import HBS2.Git3.Repo.Types
import HBS2.Git3.Repo.Tools
import HBS2.Git.Local.CLI (gitRunCommand)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import HBS2.Git3.Export

import HBS2.System.Dir

import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.GroupKeySymm

import HBS2.KeyMan.Keys.Direct

import Data.Config.Suckless.Script
import Data.Config.Suckless.Almost.RPC

import Data.Maybe
import Data.Word
import Data.Text qualified as Text
import Lens.Micro.Platform
import System.IO qualified as IO

import System.Random hiding (next)


{- HLINT ignore "Functor law"-}

data CInit =
    CheckRepoKeyExist
  | CreateRepoKey
  | CheckRepoKeyStart GitRepoKey
  | CheckRepoKeyWait TimeSpec (Timeout 'Seconds) GitRepoKey
  | CheckRepoDefBlock GitRepoKey (LWWRef 'HBS2Basic)
  | ReinitRepoKey GitRepoKey
  | CreateRepoDefBlock GitRepoKey

newRepoOpt :: Syntax C
newRepoOpt = mkSym "--new"

encryptedNewOpt :: Syntax C
encryptedNewOpt = mkSym "--encrypted"

forceOpt :: Syntax C
forceOpt = mkSym "--force"

-- | What a repository key already has, found by waiting for it.
data ExistingRef =
    RefEmpty                -- ^ no value on this peer or its neighbours
  | RefReadable HashRef     -- ^ a value whose manifest can be read
  | RefUnreadable HashRef   -- ^ a value whose manifest cannot
  deriving stock (Eq,Show)

data ReinitDecision =
    PublishFresh
  | KeepExisting HashRef
  | RefuseUnreadable HashRef
  | ReplaceUnreadable HashRef
  deriving stock (Eq,Show)

-- | What @repo:init <key>@ does with what the key already has.
--
-- A REPOSITORY THAT CAN BE READ IS NEVER REPLACED, --force or not. Its
-- manifest names the reflog all of its history is in, and a new manifest would
-- name a new, empty one: every clone would see a repository with no branches,
-- and the old history would stay in the network with nothing pointing at it.
-- Nothing a user can mean by "init" needs that.
--
-- One whose manifest cannot be read is replaced only when asked: "cannot be
-- read here" may mean "has not arrived yet", and replacing it then wins over
-- the real one, because an lwwref keeps the newest value.
decideReinit :: Bool -> ExistingRef -> ReinitDecision
decideReinit force = \case
  RefEmpty                    -> PublishFresh
  RefReadable h               -> KeepExisting h
  RefUnreadable h | force     -> ReplaceUnreadable h
                  | otherwise -> RefuseUnreadable h

-- | A refusal, worded for the person who ran the command.
newtype RepoInitError = RepoInitError String
  deriving stock (Typeable)

instance Show RepoInitError where
  show (RepoInitError s) = s

instance Exception RepoInitError


relayOnlyRepo :: forall m . HBS2GitPerks m => [Syntax C] -> Git3 m ()
relayOnlyRepo syn = connectedDo do
  case syn of
    [ SignPubKeyLike repo ] -> do
      setGitRepoKey repo
      waitRepo (Just 10) =<< getGitRepoKeyThrow

    e -> throwIO (BadFormException (mkList e))


-- | @repo:init [--new] [--encrypted gk] [--force] [repo]@
--
-- Without @repo@ a new key is created and a repository published under it,
-- as before. With one (an hbs23:// URL, a key, or the name of a git remote)
-- the repository is published under THAT key, if it has nothing yet: the way
-- back for a repository whose lwwref was lost, keeping the URL every clone
-- already has. See 'decideReinit' for what happens when it is not empty.
initRepo :: forall m . HBS2GitPerks m => [Syntax C] -> Git3 m ()
initRepo syn = do

  let (opts, argz) = splitOpts [ ("--new",0)
                               , ("--encrypted",1)
                               , ("--force",0)
                               ] syn

  let new = or [ True | ListVal [SymbolVal "--new"] <- opts ]
  let force = or [ True | ListVal [SymbolVal "--force"] <- opts ]
  let gkh = lastMay [ gk | ListVal [SymbolVal "--encrypted", HashLike gk] <- opts ]

  -- Before `git init`: a remote name is looked up in this repository's config.
  explicit <- case argz of
                [] -> pure Nothing
                xs -> Just <$> resolveRepoKeyThrow xs

  when (force && isNothing explicit) do
    throwIO $ RepoInitError "--force needs the key of the repository to replace"

  callProc "git" ["init"] []

  root <- getConfigRootFile

  touch root

  sto  <- getStorage
  lwwAPI    <- getClientAPI @LWWRefAPI @UNIX
  peerAPI   <- getClientAPI @PeerAPI @UNIX

  debug $ "initRepo" <+> pretty opts <+> pretty syn

  flip fix (maybe CheckRepoKeyExist ReinitRepoKey explicit) $ \next -> \case
    CheckRepoKeyExist -> do
      debug "initRepo:CheckRepoKey"
      mbk <- getGitRepoKey
      next $ maybe CreateRepoKey CheckRepoKeyStart mbk

    CreateRepoKey -> do

      debug $ "initRepo:CreateRepoKey" <+> pretty root

      answ <- callProc "hbs2-cli" [] [mkSym "hbs2:lwwref:create"]

      pk <- [ x | ListVal [SymbolVal "pk", SignPubKeyLike x] <- answ ]
              & lastMay
              & orThrowUser "can't create new lwwref"

      -- liftIO $ appendFile root (show $ pretty $ mkForm "repo:ref" [mkSym @C (show $ pretty (AsBase58 pk))])

      setGitRepoKey pk

      next $ CheckRepoKeyStart pk

    CheckRepoKeyStart pk -> do

      debug $ "initRepo:CheckRepoKeyStart" <+> pretty new <+> pretty opts <+> pretty (AsBase58 pk)

      callRpcWaitMay @RpcLWWRefFetch (TimeoutSec 1) lwwAPI (LWWRefKey pk)
        >>= orThrowUser "rpc timeout"

      now <- getTimeCoarse

      let waity = if new then ceiling 0.5e9 else ceiling 30e9

      let till = TimeoutTS (now + fromNanoSecs waity )

      next $ CheckRepoKeyWait (coerce till) 1.0 pk

    CheckRepoKeyWait till w pk -> do

      debug $ "initRepo:CheckRepoKeyWait" <+> pretty (AsBase58 pk)

      rv <- getRepoRefMaybe

      now <- getTimeCoarse

      if now > till then do
        next $ CreateRepoDefBlock pk
      else do
        maybe1 rv (pause w >> next (CheckRepoKeyWait till (1.10 * w) pk))
                  (next . CheckRepoDefBlock pk)

    CheckRepoDefBlock pk LWWRef{..} -> do
      debug $ "init:CheckRepoDefBlock" <+> pretty (AsBase58 pk) <+> pretty lwwValue

      repo <- getRepoManifest

      reflog <- getRefLog repo & orThrow GitRepoManifestMalformed

      callRpcWaitMay @RpcPollAdd (TimeoutSec 1) peerAPI (reflog, "reflog", 17)
         >>= orThrowUser "rpc timeout"

      -- FIXME: remove-this
      liftIO $ print $ pretty $ mkForm "manifest" (coerce repo)

    ReinitRepoKey pk -> do

      setGitRepoKey pk

      limit <- getRepoWaitTimeout

      callRpcWaitMay @RpcPollAdd (TimeoutSec 2) peerAPI (pk, "lwwref", 31)
         >>= orThrow (RepoPeerNotAnswering 2)

      -- The whole limit, --new or not. Deciding "empty" early is the one
      -- mistake here that cannot be undone: the value published next is newer
      -- than any real one still on its way, and an lwwref keeps the newest.
      say $ "looking for repository" <+> pretty (AsBase58 pk)
              <+> "on this peer and its neighbours, up to" <+> pretty (round @_ @Integer (realToFrac @_ @Double limit)) <> "s"

      existing <- withAsync (fetchLoop pk) $ const do
        v <- pollUntil limit (join <$> callRpcWaitMay @RpcLWWRefGet (TimeoutSec 1) lwwAPI (LWWRefKey pk))
        case v of
          Nothing -> pure RefEmpty
          Just LWWRef{..} -> do
            mf <- pollUntil limit (either (const Nothing) Just <$> try @_ @SomeException getRepoManifest)
            pure $ maybe (RefUnreadable lwwValue) (const (RefReadable lwwValue)) mf

      case decideReinit force existing of
        KeepExisting h -> do
          mf <- getRepoManifest
          let reflog = getRefLog mf
          for_ reflog $ \r ->
            callRpcWaitMay @RpcPollAdd (TimeoutSec 1) peerAPI (r, "reflog", 17)
              >>= orThrowUser "rpc timeout"
          liftIO $ print $ "repository" <+> pretty (AsBase58 pk) <+> "is already published"
                    <+> parens ("lwwref value" <+> pretty h
                                 <> maybe mempty (\r -> "," <+> "reflog" <+> pretty (AsBase58 r)) reflog)
                    <> "; nothing was changed"

        RefuseUnreadable h -> throwIO $ RepoInitError $ show $
             "repository" <+> pretty (AsBase58 pk) <+> "has an lwwref value" <+> pretty h
          <> ", but its manifest could not be read from this peer or its neighbours."
          <+> "Nothing was changed. If its blocks are lost for good, run"
          <+> "`hbs2-git3 repo:init --force" <+> pretty (AsBase58 pk) <> "`"
          <+> "to publish a new, empty repository over it."

        ReplaceUnreadable h -> do
          ownerOrRefuse pk
          say $ "replacing the unreadable value" <+> pretty h <+> "of" <+> pretty (AsBase58 pk)
          next (CreateRepoDefBlock pk)

        PublishFresh -> do
          ownerOrRefuse pk
          say $ "repository" <+> pretty (AsBase58 pk) <+> "has no value; publishing a new one"
          next (CreateRepoDefBlock pk)

    CreateRepoDefBlock pk  -> do

      debug $ "init:CreateRepoDefBlock" <+> pretty (AsBase58 pk)

      seed <- randomIO @Word64

      creds <- runKeymanClientRO (loadCredentials pk) >>= orThrow (noKey pk)

      let sk = view peerSignSk creds
      (rpk,rsk) <- derivedKey @'HBS2Basic @'Sign seed sk

      callRpcWaitMay @RpcPollAdd (TimeoutSec 1) peerAPI (rpk, "reflog", 17)
         >>= orThrowUser "rpc timeout"

      gkRefs <- case gkh of
               Nothing -> pure []
               Just h  -> do
                 _ <- loadGroupKeyMaybe @'HBS2Basic sto h >>= orThrow (GroupKeyNotFound 1)
                 pure [h]

      let manifest = [
             mkForm @C "hbs2-git" [mkInt 3]
           , mkForm "seed" [mkInt seed]
           , mkForm "public" []
           , mkForm "reflog" [mkSym (show $ pretty (AsBase58 rpk))]
           ] <> map (\h -> mkForm "gk" [mkSym (show $ pretty (AsBase58 h))]) gkRefs

      -- Over a lost value there is nothing to carry forward, and an
      -- unreadable one cannot be read for its group keys anyway.
      if isJust explicit
        then replaceRepoHead pk manifest gkRefs
        else updateRepoHead pk manifest gkRefs

      -- A remote that already points here is the one the user pushes to; a
      -- second one under a made-up name would only be noise.
      known <- listRemotes <&> lookup pk . fmap (\(a,b) -> (b,a))

      remoteName <- case known of
        Just r -> do
          liftIO $ print $ "git remote" <+> pretty r <+> "already points at" <+> pretty (remoteRepoURL pk)
          pure (show (pretty r))

        Nothing -> do
          -- `origin` when the clone has none, which is the case for a new
          -- repository and is what every git tutorial and script assumes. A
          -- made-up name only when origin is taken by something else.
          -- Every remote, not only the hbs23 ones listRemotes returns: an origin
          -- that points at GitHub is still taken.
          taken <- gitRunCommand "git remote"
                     <&> either (const []) (fmap LBS8.unpack . LBS8.lines)
          remoteName <- if "origin" `notElem` taken
                          then pure "origin"
                          else newRemoteName pk <&> show . pretty
          let remoteVal  = Text.unpack $ remoteRepoURL pk
          _ <- callProc "git" ["remote", "add", remoteName, remoteVal] mempty
          liftIO $ print $ pretty "added git remote" <+> pretty remoteName <+> pretty remoteVal
          pure remoteName

      updateRepoKey pk

      -- Always, and not only for --new: a reflog nobody has written to has no
      -- value, and a push waits for the reflog to have one. Without this a
      -- repository made by `init` could not be pushed to until it had been
      -- pushed to.
      postNullTx

      when (isJust explicit) do
        liftIO $ print $ vcat
          [ "published repository" <+> pretty (AsBase58 pk) <+> "with reflog" <+> pretty (AsBase58 rpk)
          , "now push everything to it:"
          , "  git push" <+> pretty remoteName <+> "--all"
          , "  git push" <+> pretty remoteName <+> "--tags"
          ]

  where
    noKey pk = RepoInitError $ show $
         "cannot publish repository" <+> pretty (AsBase58 pk) <> ":"
     <+> "its private key is not known to hbs2-keyman."
     <+> "Only the owner of the key can publish under it; put the key file"
     <+> "where hbs2-keyman looks (see `hbs2-keyman config`)"
     <+> "and run `hbs2-keyman update`."

    -- Before saying anything is about to be published.
    ownerOrRefuse pk = do
      creds <- runKeymanClientRO (loadCredentials pk)
      when (isNothing creds) $ throwIO (noKey pk)

    say :: forall a . Doc a -> Git3 m ()
    say d = liftIO $ IO.hPutStrLn IO.stderr (show d)

    fetchLoop pk = do
      lwwAPI <- getClientAPI @LWWRefAPI @UNIX
      flip fix 1 \again i -> do
        void (callRpcWaitMay @RpcLWWRefFetch (TimeoutSec 1) lwwAPI (LWWRefKey pk))
        pause @'Seconds i
        again (min 10 (i * 2))
