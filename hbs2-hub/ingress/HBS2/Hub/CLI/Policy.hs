-- | @hub policy show@, @hub block@ and @hub unblock@ (PEP-21, PEP-22
-- "Moderate").
--
-- The PEER layer of PEP-21's two, and the help says which one it is because
-- the difference decides whether the operator is done. A deny here is matched
-- against the ENVELOPE key, before anything is decrypted, so what it bounds is
-- what this peer STORES and relays. It does not bound what enters canon: a
-- rewrapper re-sends somebody else's inner box under a fresh envelope, and the
-- peer sees a key it has never denied. Banning an author for canon is the
-- triage layer, and this build does not have it yet.
--
-- ORDER IS IMPOSED on the file this writes. 'getAsSyntax' renders a policy
-- from two HashMaps, so its clause order is hash order: rendering one policy
-- twice gives two texts and two tree hashes. That is survivable for the peer,
-- which compares versions and not hashes, and it is not survivable for a verb
-- that reads a policy, changes one clause and writes it back -- two runs
-- reaching the same policy have to produce the same file, or every no-op
-- bumps a version and republishes.
--
-- THE MAILBOX KEY SIGNS IT, as with a delete: the peer takes the payload's
-- signer as the mailbox whose policy this is.
module HBS2.Hub.CLI.Policy
  ( policyEntries
  , policyUsage
  , policyArgs
  , PolicyArgs(..)
  , denying
  , policyText
  , codeNoPolicy
  , codeNotSet
  ) where

import HBS2.Hub.Types (HubKey,HubScheme,safeText)
import HBS2.Hub.Ingress (rpcTimeout)
import HBS2.Hub.CLI.Inbox (refuse,codePeerSilent,PeerSilent(..),bounded)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.SignedBox (makeSignedBox,unboxSignedBox0)
import HBS2.Net.Auth.Credentials (_peerSignSk)
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Policy.Basic
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage
import HBS2.Storage.Operations.Class (readFromMerkle,writeAsMerkle)
import HBS2.Storage.Operations.ByteString (pattern SimpleKey)

import HBS2.KeyMan.Keys.Direct (runKeymanClientRO,loadCredentials)

import Data.Config.Suckless
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.List (sort)
import Data.Text qualified as Text
import Control.Monad.Except (runExceptT)
import Data.Coerce (coerce)
import HBS2.Storage.Operations.ByteString ()
import System.Exit (die,exitSuccess)

-- | The mailbox has no policy to read, or it will not read.
codeNoPolicy :: Int
codeNoPolicy = 34

-- | Nothing was changed.
codeNotSet :: Int
codeNotSet = 35

data PolicyArgs = PolicyArgs
  { paMailbox :: HubKey
  , paKey     :: Maybe HubKey   -- ^ whom to block or unblock
  }
  deriving stock (Eq,Show)

policyUsage :: Doc ()
policyUsage =
  "usage: hbs2-hub policy show --mailbox <key>"
    <> line <> "       hbs2-hub block|unblock --mailbox <key> --key <envelope-key>"

-- | The policy with one sender denied, or allowed again.
--
-- Pure, and exported, because it is the whole of what these two verbs decide.
-- A block is not "add a clause": a key already denied must produce the same
-- policy, or every re-run bumps a version and republishes a file nobody
-- changed.
denying :: Bool -> HubKey -> BasicPolicy HBS2Basic -> BasicPolicy HBS2Basic
denying deny who p
  | deny      = p { bpSenders = HM.insert who Deny (bpSenders p) }
  -- Removed rather than set to Allow: the default is what an unlisted sender
  -- gets, and writing Allow beside a default of Allow is a clause that says
  -- nothing and never goes away. An operator who wants a sender allowed
  -- against a deny-all default is asking for something else, and this verb
  -- does not offer it rather than doing it by accident.
  | otherwise = p { bpSenders = HM.delete who (bpSenders p) }

-- | The bytes a policy is stored as.
--
-- Sorted, for the reason the module header gives: 'getAsSyntax' hands back
-- hash order and this text is hashed.
policyText :: BasicPolicy HBS2Basic -> Text
policyText p = Text.unlines (sort [ Text.pack (show (pretty c))
                                  | c <- getAsSyntax @C p ])

policyEntries :: forall c m . ( IsContext c
                              , MonadUnliftIO m
                              , HasStorage m
                              , HasClientAPI MailboxAPI UNIX m
                              , Exception (BadFormException c)
                              ) => MakeDictM c m ()
policyEntries = do

  brief "print the accept policy of a hub's ingress mailbox"
    $ args [arg "string" "--mailbox mailbox-key"]
    $ desc ( "The PEER layer (PEP-21): these clauses are matched before"
             <> line <> "anything is decrypted, against the envelope key, so what"
             <> line <> "they bound is what this peer stores and relays."
             <> line
             <> line <> "A mailbox with no policy is deny-all by default, which"
             <> line <> "is reported as such rather than as an empty policy." )
    $ entry $ bindMatch "hub:policy:show" $ nil_ \case
        [ StringLike "--mailbox", SignPubKeyLike mbox ] -> lift do
          (v, p) <- currentPolicy mbox
          liftIO $ print $ vcat
            [ "version" <+> pretty v
            , pretty (safeText (policyText p))
            ]
        _ -> liftIO (die (show policyUsage))

  denyVerb "hub:block" True "refuse a sender at the peer layer"
  denyVerb "hub:unblock" False "stop refusing a sender at the peer layer"

  where

    denyVerb name deny what =
      brief what
        $ args [arg "string" "--mailbox mailbox-key", arg "string" "--key envelope-key"]
        $ desc ( "Rewrites the mailbox's policy with one clause added or"
                 <> line <> "removed, under the next version. Re-running it when"
                 <> line <> "nothing would change writes nothing: a version bump"
                 <> line <> "republishes the file to every peer holding the mailbox."
                 <> line
                 <> line <> "THIS IS THE ENVELOPE KEY, not the author. It bounds"
                 <> line <> "what this peer stores, and it is evadable: anyone"
                 <> line <> "holding a decrypted letter can re-send it under a"
                 <> line <> "fresh envelope, and the peer sees a key nobody denied."
                 <> line <> "Keeping an author out of canon is the triage layer,"
                 <> line <> "which this build does not have; until it does, this is"
                 <> line <> "a storage bound and not a ban." )
        $ entry $ bindMatch name $ nil_ \case
            (policyArgs -> Just pa) | Just who <- paKey pa -> lift (setDeny deny pa who)
            _ -> liftIO (die (show policyUsage))

    -- The policy as it stands, and the version it stands at.
    currentPolicy mbox = do
      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX

      st <- callRpcWaitMay @RpcMailboxGetStatus rpcTimeout api mbox
              >>= maybe (liftIO (refuse (show (PeerSilent "the mailbox service"))
                                        codePeerSilent))
                        pure
              >>= either (\e -> liftIO (refuse (show ("the peer refused:" <+> viaShow e))
                                               codeNoPolicy))
                         pure
              >>= maybe (liftIO (refuse (show ("this peer does not hold mailbox"
                                                 <+> pretty (AsBase58 mbox)))
                                        codeNoPolicy))
                        pure

      case mbsMailboxPolicy st >>= unboxSignedBox0 of
        -- No policy is not an empty policy: an unset mailbox is deny-all, so
        -- reporting nothing would read as "everything is allowed".
        Nothing -> pure (0 :: PolicyVersion, defaultBasicPolicy @HBS2Basic)
        Just (_, spp) -> do
          lbs <- bounded rpcTimeout "the policy file"
                   (liftIO (runExceptT (readFromMerkle sto (SimpleKey (coerce (sppPolicyRef spp))))))
                   >>= either (\e -> liftIO (refuse (show ("the policy will not read:"
                                                             <+> viaShow e))
                                                    codeNoPolicy))
                              pure
          p <- parseBasicPolicy @HBS2Basic (either (const mempty) id
                                              (parseTop (LBS.unpack lbs)))
                 >>= maybe (liftIO (refuse "the policy file will not parse" codeNoPolicy))
                           pure
          pure (sppPolicyVersion spp, p)

    setDeny deny pa who = do
      (v, p) <- currentPolicy (paMailbox pa)

      let p' = denying deny who p

      when (p' == p) $ liftIO do
        print ("nothing to change; the policy was not rewritten" :: Doc ())
        exitSuccess

      creds <- runKeymanClientRO (loadCredentials (paMailbox pa))
                 >>= maybe (liftIO (refuse (show ( "no signing key here for"
                                                     <+> pretty (AsBase58 (paMailbox pa))
                                                     <> line
                                                     <> "  the mailbox's own key signs its"
                                                     <+> "policy" ))
                                           codeNotSet))
                           pure

      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX

      href <- liftIO (writeAsMerkle sto (LBS.pack (Text.unpack (policyText p'))))

      let payload = SetPolicyPayload (paMailbox pa) (succ v) (HashRef href)
          box = makeSignedBox @HubScheme (paMailbox pa) (_peerSignSk creds) payload

      callRpcWaitMay @RpcMailboxSetPolicy rpcTimeout api (paMailbox pa, box)
        >>= maybe (liftIO (refuse (show (PeerSilent "the mailbox policy")) codePeerSilent))
                  pure
        >>= either (\e -> liftIO (refuse (show ("the peer refused the policy:"
                                                  <+> viaShow e))
                                         codeNotSet))
                   pure

      liftIO $ print $ vcat
        [ (if deny then "blocked" else "unblocked") <+> pretty (AsBase58 who)
        , "policy version" <+> pretty (succ v)
        , pretty (safeText (policyText p'))
        ]

-- Both values behind flags, and both are keys of one type.
policyArgs :: forall c . [Syntax c] -> Maybe PolicyArgs
policyArgs syn = do
  mbox <- flagged "--mailbox"
  pure (PolicyArgs mbox (flagged "--key"))
  where
    flagged n = case [ v | (StringLike n', SignPubKeyLike v) <- zip syn (drop 1 syn)
                         , n' == n ] of
                  [v] -> Just v
                  _   -> Nothing
