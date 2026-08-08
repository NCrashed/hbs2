-- | @hub policy show@, @hub block@ and @hub unblock@ (PEP-21, PEP-22
-- "Moderate").
--
-- The PEER layer of PEP-21's two, and the help says which one it is because
-- the difference decides whether the operator is done. A deny here is matched
-- against the ENVELOPE key, before anything is decrypted, so what it bounds is
-- what this peer STORES and relays. It does not bound what enters canon: a
-- rewrapper re-sends somebody else's inner box under a fresh envelope, and the
-- peer sees a key it has never denied. Banning an author for canon is the
-- triage layer, and it is @hub ban@ ("HBS2.Hub.CLI.Ban"): it denies the key
-- INSIDE the signed box, which a rewrap cannot change. A full stop is both.
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
  , policyShowArgs
  , PolicyArgs(..)
  , denying
  , withPoW
  , withDefaults
  , powUsage
  , powArgs
  , defaultUsage
  , defaultArgs
  , policyText
  , readPolicy
  , readPolicyWith
  , PolicyGone(..)
  , policyGoneCode
  , codeNoPolicy
  , codeNotSet
  ) where

import HBS2.Hub.Types (HubKey,HubScheme,safeText)
import HBS2.Hub.Canon (clausesWith)
import HBS2.Hub.CLI.Argv (flagsOf,flagOnce,flagMaybe,flagText,flagWord)
import HBS2.Hub.Ingress (rpcTimeout,bounded,PeerSilent(..))
import HBS2.Hub.CLI.Common (refuse,saying,codePeerSilent,signerFor,signingPair)

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Types.SignedBox (makeSignedBox,unboxSignedBox0)
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Policy.Basic
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage
import HBS2.Storage.Operations.Class (readFromMerkle,writeAsMerkle)
import HBS2.Storage.Operations.ByteString (pattern SimpleKey)


import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.List (sort)
import Data.Maybe (fromMaybe,isJust)
import Data.Word (Word8)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Control.Monad.Except (runExceptT,throwError)
import Data.Coerce (coerce)
import HBS2.Storage.Operations.ByteString ()
import System.Exit (die,exitSuccess)

-- | What a policy file may weigh, and how many clauses it may have.
--
-- A policy is a deny-list, so unlike a manifest it has a reason to be long, and
-- unlike an event file it has no writer on this side to derive a bound from.
-- These are chosen against the cost instead: 'parseTop' is superlinear in the
-- number of TOP-LEVEL items, which for a policy is one per rule, so the bound
-- that matters is the clause count and not the size. Eight thousand rules is
-- far past any moderation list somebody maintains by hand, and well under where
-- the measurements in "HBS2.Hub.Canon" turn into seconds.
--
-- Refusing one reads as 'PolicyUnparsed', which is deny/deny: a policy this
-- node cannot read is one the owner wrote and this node must not guess at.
maxPolicyBytes, maxPolicyClauses :: Int
maxPolicyBytes   = 4 * 1024 * 1024
maxPolicyClauses = 8192

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

-- | The policy with a proof-of-work floor, or with none.
--
-- Pure and exported for the reason 'denying' is: it is the whole of what the
-- verb decides, and a re-run that changes nothing must produce the same policy
-- rather than a version bump that republishes a file to every peer.
--
-- Zero IS the absent value here rather than a separate case (@bpPoW@ is
-- documented as "zero when unset"), so clearing the floor and setting it to
-- zero are one operation. That is not a coincidence to paper over: a floor of
-- zero bits is work every message already does.
withPoW :: PoWDifficulty -> BasicPolicy HBS2Basic -> BasicPolicy HBS2Basic
withPoW d p = p { bpPoW = d }

-- | The policy with its default actions replaced, where they were named.
--
-- The two defaults are what an unlisted key gets, and they answer different
-- questions: the SENDER default decides whose letters this mailbox takes, and
-- the PEER default decides which peers it will sync with at all. Naming one
-- must not silently restate the other, which is why both are 'Maybe' and an
-- absent one is left alone.
withDefaults :: Maybe BasicPolicyAction     -- ^ sender
             -> Maybe BasicPolicyAction     -- ^ peer
             -> BasicPolicy HBS2Basic
             -> BasicPolicy HBS2Basic
withDefaults sender peer p =
  p { bpDefaultSenderAction = fromMaybe (bpDefaultSenderAction p) sender
    , bpDefaultPeerAction = fromMaybe (bpDefaultPeerAction p) peer
    }

-- | The bytes a policy is stored as.
--
-- Sorted, for the reason the module header gives: 'getAsSyntax' hands back
-- hash order and this text is hashed.
policyText :: BasicPolicy HBS2Basic -> Text
policyText p = Text.unlines (sort [ Text.pack (show (pretty c))
                                  | c <- getAsSyntax @C p ])

-- | Why this peer could not say what a mailbox's policy is.
--
-- Told apart rather than collapsed into Nothing, because the send path treats
-- them differently from the policy verbs: not holding a mailbox is the ORDINARY
-- case for a letter addressed elsewhere, while a policy that will not parse is
-- a peer holding something broken and worth saying so about.
data PolicyGone =
    PolicyPeerSilent          -- ^ nothing came back in the time allowed
  | PolicyPeerRefused String  -- ^ the peer answered, with an error
  | PolicyNotHere HubKey      -- ^ this peer does not hold that mailbox
  | PolicyUnreadable String   -- ^ the policy file would not read
  | PolicyUnparsed            -- ^ ...or would not parse

instance Pretty PolicyGone where
  pretty = \case
    PolicyPeerSilent     -> pretty (show (PeerSilent "the mailbox service"))
    PolicyPeerRefused e  -> "the peer refused:" <+> pretty e
    PolicyNotHere mbox   -> "this peer does not hold mailbox" <+> pretty (AsBase58 mbox)
    PolicyUnreadable e   -> "the policy will not read:" <+> pretty e
    PolicyUnparsed       -> "the policy file will not parse"

-- | And what a verb that has nowhere else to go should exit with.
policyGoneCode :: PolicyGone -> Int
policyGoneCode = \case
  PolicyPeerSilent -> codePeerSilent
  _                -> codeNoPolicy

-- | The policy this peer holds for a mailbox, or why it does not.
--
-- Answers rather than exits, which is the whole reason it is separate from the
-- verbs above: the send path asks this about a mailbox it is writing TO, and a
-- peer that does not hold it is the normal state of affairs there.
--
-- A mailbox held with no policy set answers @Nothing@ and not a failure, and
-- not the deny/deny fallback either: see 'readPolicyWith' for what telling those
-- two apart is worth.
readPolicy :: forall m . ( MonadUnliftIO m
                         , HasStorage m
                         , HasClientAPI MailboxAPI UNIX m
                         )
           => HubKey -> m (Either PolicyGone (Maybe (PolicyVersion, BasicPolicy HBS2Basic)))
readPolicy mbox = do
  sto <- getStorage
  api <- getClientAPI @MailboxAPI @UNIX
  readPolicyWith sto api mbox

-- | The same, for a caller that carries its peer around rather than reaching
-- for it: 'HBS2.Hub.CLI.Compose.Outbound' holds both of these already, and its
-- send path has no @HasStorage@ to reach through.
readPolicyWith :: forall m . MonadUnliftIO m
               => AnyStorage
               -> ServiceCaller MailboxAPI UNIX
               -> HubKey
               -> m (Either PolicyGone (Maybe (PolicyVersion, BasicPolicy HBS2Basic)))
readPolicyWith sto api mbox = runExceptT do

  st <- lift (callRpcWaitMay @RpcMailboxGetStatus rpcTimeout api mbox)
          >>= maybe (throwError PolicyPeerSilent) pure
          >>= either (throwError . PolicyPeerRefused . show . viaShow) pure
          >>= maybe (throwError (PolicyNotHere mbox)) pure

  case mbsMailboxPolicy st >>= unboxSignedBox0 of
    -- NOT SET is its own answer, and it used to be @(0, defaultBasicPolicy)@.
    -- That reads as deny/deny, which is what the peer falls back to when it
    -- ADMITS a message, and it is the opposite of what the peer does with the
    -- same state everywhere else: a mailbox with no policy row answers every
    -- peer's CheckMailbox and accepts a status from any of them
    -- (mailboxGetPolicyMay, deliberately, or a mailbox would never sync before
    -- its owner got round to writing one).
    --
    -- Two states told apart by nothing was not a reporting problem. `hub block`
    -- read this value, added one clause and published it, so blocking one
    -- spammer on a mailbox nobody had written a policy for MATERIALISED
    -- deny/deny: from that moment the peer answered no CheckMailbox and took no
    -- status, and the mailbox stopped syncing. `hub unblock` does not undo it
    -- (the row still exists) and this CLI has no verb that writes `peer allow
    -- all` back.
    Nothing -> pure Nothing
    Just (_, spp) -> do
      lbs <- lift (bounded rpcTimeout "the policy file"
                     (liftIO (runExceptT (readFromMerkle sto (SimpleKey (coerce (sppPolicyRef spp)))))))
               >>= either (throwError . PolicyUnreadable . show . viaShow) pure
      -- BOUNDED, through the reader canon files go through. A policy file is
      -- fetched from a merkle tree somebody else published and was going
      -- straight into 'parseTop' with no bound of any kind, on a path that runs
      -- per recipient on every send. 'parseTop' is superlinear in the number of
      -- top-level items (see 'scanText' in "HBS2.Hub.Canon"), so the file that
      -- costs the most is not the biggest one.
      --
      -- Unreadable and unparseable stay one answer, deliberately: both mean
      -- nothing usable was read, and the caller's decision is the same.
      cs <- either (const (throwError PolicyUnparsed)) pure
              (clausesWith maxPolicyBytes maxPolicyClauses
                 (Text.decodeUtf8Lenient (LBS.toStrict lbs)))
      p <- lift (parseBasicPolicy @HBS2Basic cs)
             >>= maybe (throwError PolicyUnparsed) pure
      pure (Just (sppPolicyVersion spp, p))

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
        (policyShowArgs -> Just mbox) -> lift do
          currentPolicy mbox >>= \case
            -- Said as the state it is, rather than as the deny/deny this used
            -- to print. Both halves matter to an operator deciding what to do
            -- next, and they point in opposite directions.
            Nothing -> liftIO $ print $ vcat
              [ "no policy set"
              , "  messages are denied (the peer falls back to deny/deny to admit one)"
              , "  and every peer is answered (a mailbox with no policy row syncs with all)"
              , "  write one with: hub policy default --mailbox <key>"
                  <+> "--sender allow --peer allow"
              ]
            Just (v, p) -> liftIO $ print $ vcat
              [ "version" <+> pretty v
              , pretty (safeText (policyText p))
              ]
        _ -> liftIO (die (show policyUsage))

  denyVerb "hub:block" True "refuse a sender at the peer layer"
  denyVerb "hub:unblock" False "stop refusing a sender at the peer layer"

  brief "charge every sender proof-of-work, or stop charging it"
    $ args [ arg "string" "--mailbox mailbox-key", arg "string" "--bits n" ]
    $ desc ( "Sets the (pow D) clause: a mailbox that charges D bits refuses"
             <> line <> "a message whose stamp does not carry them (PEP-21). The"
             <> line <> "sender grinds it once per mailbox, before sending; every"
             <> line <> "peer checks it before storing anything."
             <> line
             <> line <> "--bits 0 removes the charge, and it is the same thing as"
             <> line <> "removing the clause: zero bits is work every message has"
             <> line <> "already done."
             <> line
             <> line <> "WHAT IT BOUNDS AND WHAT IT DOES NOT. It prices a message,"
             <> line <> "so a flood of them costs the sender time. It is not a ban"
             <> line <> "and not a rate limit: work is per message and nothing"
             <> line <> "carries over, so a determined sender pays and continues."
             <> line <> "Keeping one author out of canon is hub ban."
             <> line
             <> line <> "Refuses a mailbox with no policy, like hub block: a policy"
             <> line <> "invented here would be deny/deny plus your clause, which"
             <> line <> "stops the mailbox syncing with every peer at once. Write"
             <> line <> "the defaults first with hub policy default." )
    $ entry $ bindMatch "hub:policy:pow" $ nil_ \case
        (powArgs -> Just (mbox, bits)) -> lift (setPoW mbox bits)
        _ -> liftIO (die (show powUsage))

  brief "set what an unlisted sender or peer gets"
    $ args [ arg "string" "--mailbox mailbox-key"
           , arg "string" "--sender allow|deny", arg "string" "--peer allow|deny" ]
    $ desc ( "The two defaults, which are the shape of the mailbox: --sender"
             <> line <> "says whose letters it takes, --peer says which peers it"
             <> line <> "will sync with at all. A public forge is (sender allow"
             <> line <> "all) with a proof-of-work floor beside it; a private one"
             <> line <> "is (sender deny all) plus named keys."
             <> line
             <> line <> "This is the verb that MAY create a policy, and the only"
             <> line <> "one: it is the only one whose arguments say what both"
             <> line <> "defaults are. Creating one therefore needs both, and"
             <> line <> "editing an existing one needs only the half you mean."
             <> line
             <> line <> "A mailbox with no policy is not the same as an empty"
             <> line <> "one: the peer denies messages and answers every peer, so"
             <> line <> "writing (peer deny all) here is how a mailbox stops"
             <> line <> "syncing, which is rarely what anybody wants." )
    $ entry $ bindMatch "hub:policy:default" $ nil_ \case
        (defaultArgs -> Just (mbox, sender, peer))
          | isJust sender || isJust peer -> lift (setDefaults mbox sender peer)
        _ -> liftIO (die (show defaultUsage))

  where

    denyVerb name deny what =
      brief what
        $ args [arg "string" "--mailbox mailbox-key", arg "string" "--key envelope-key"]
        -- Each sibling's own sentence first: one shared description meant
        -- `hub help unblock` never said what unblocking does.
        $ desc ( (if deny then "Denies an envelope key at this mailbox."
                          else "Removes a deny clause, restoring the default.")
                 <> line
                 <> line <> "Rewrites the mailbox's policy with one clause added or"
                 <> line <> "removed, under the next version. Re-running it when"
                 <> line <> "nothing would change writes nothing: a version bump"
                 <> line <> "republishes the file to every peer holding the mailbox."
                 <> line
                 <> line <> "THIS IS THE ENVELOPE KEY, not the author. It bounds"
                 <> line <> "what this peer stores, and it is evadable: anyone"
                 <> line <> "holding a decrypted letter can re-send it under a"
                 <> line <> "fresh envelope, and the peer sees a key nobody denied."
                 <> line <> "Keeping an author out of canon is `hub ban`, which"
                 <> line <> "denies the INNER key and therefore survives a rewrap."
                 <> line <> "A full stop is both: this bounds the disk, that one"
                 <> line <> "bounds canon." )
        $ entry $ bindMatch name $ nil_ \case
            (policyArgs -> Just pa) | Just who <- paKey pa -> lift (setDeny deny pa who)
            _ -> liftIO (die (show policyUsage))

    -- The policy as it stands, and the version it stands at. These verbs exist
    -- to talk about one mailbox's policy, so every way of not having one is
    -- fatal here; 'readPolicy' is where the same reading is done for a caller
    -- that has somewhere else to go.
    currentPolicy mbox =
      readPolicy mbox >>= either (\e -> liftIO (refuse (show (pretty e)) (policyGoneCode e)))
                                 pure

    setDeny deny pa who = do
      -- REFUSED rather than created. A policy this verb invented would be the
      -- deny/deny fallback plus one clause, and publishing that turns a mailbox
      -- that syncs with everybody into one that syncs with nobody, in a verb
      -- whose whole job is to refuse ONE key. There is no way back from here
      -- either: `hub unblock` removes the clause and leaves the row, and nothing
      -- in this CLI writes `peer allow all` except `hub policy default`.
      (v, p) <- existing (paMailbox pa) "blocking one key"
      publish (paMailbox pa) (succ v) (denying deny who p)
        ((if deny then "blocked" else "unblocked") <+> pretty (AsBase58 who))

    setPoW mbox bits = do
      (v, p) <- existing mbox "charging work"
      publish mbox (succ v) (withPoW bits p)
        (if bits == 0 then "no proof-of-work is charged"
                      else "charging" <+> pretty bits <+> "bits of work")

    -- The one verb that may write a policy where there was none, because it is
    -- the only one whose arguments say what BOTH defaults are. Everything else
    -- edits one clause and would have to invent the rest.
    setDefaults mbox sender peer =
      currentPolicy mbox >>= \case
        Just (v, p) -> publish mbox (succ v) (withDefaults sender peer p) "policy changed"
        Nothing -> case (sender, peer) of
          (Just s, Just q) ->
            -- Version 1, which is what a mailbox with no policy row expects: the
            -- peer admits a strictly greater version and there is nothing to be
            -- greater than yet.
            publish mbox 1 (withDefaults (Just s) (Just q) (defaultBasicPolicy @HBS2Basic))
                    "policy written"
          _ -> liftIO $ refuse (show ( "this mailbox has no policy, so both defaults"
                                         <+> "have to be named" <> line
                                         <> "  --sender says whose letters it takes,"
                                         <+> "--peer says which peers it syncs with"
                                         <> line
                                         <> "  naming one and inheriting the other means"
                                         <+> "inheriting deny, which for --peer is a"
                                         <> line
                                         <> "  mailbox that stops syncing at all" ))
                               codeNoPolicy

    -- The policy as it stands, or a refusal that says what to do instead. Every
    -- verb but the one above needs one to exist.
    existing mbox what =
      currentPolicy mbox
        >>= maybe (liftIO (refuse (show ( "this mailbox has no policy, and" <+> what
                                            <+> "is not a reason to write one"
                                            <> line
                                            <> "  a policy written from here would be"
                                            <+> "deny/deny plus your clause, which stops"
                                            <> line
                                            <> "  the mailbox syncing with every peer"
                                            <+> "at once"
                                            <> line
                                            <> "  write the defaults you mean first:"
                                            <+> "hub policy default --mailbox"
                                            <+> pretty (AsBase58 mbox)
                                            <+> "--sender allow --peer allow" ))
                                  codeNoPolicy))
                  pure

    -- Sign, store, publish, report. One copy, because a policy is a file every
    -- peer holding the mailbox will fetch, and three verbs writing it three
    -- ways is three answers to what a version bump costs.
    publish mbox v p' said = do
      -- A re-run that changes nothing writes nothing. A version bump is not
      -- free: it republishes the file to every peer holding the mailbox.
      unchanged <- currentPolicy mbox <&> maybe False ((== p') . snd)
      when unchanged $ liftIO do
        saying ("nothing to change; the policy was not rewritten" <> line)
        exitSuccess

      creds <- signerFor mbox
                 >>= maybe (liftIO (refuse (show ( "cannot sign as"
                                                     <+> pretty (AsBase58 mbox)
                                                     <> line
                                                     <> "  the mailbox's own key signs its"
                                                     <+> "policy" ))
                                           codeNotSet))
                           pure

      sto <- getStorage
      api <- getClientAPI @MailboxAPI @UNIX

      href <- liftIO (writeAsMerkle sto (LBS.pack (Text.unpack (policyText p'))))

      let payload = SetPolicyPayload mbox v (HashRef href)
          box = uncurry (makeSignedBox @HubScheme) (signingPair creds) payload

      callRpcWaitMay @RpcMailboxSetPolicy rpcTimeout api (mbox, box)
        >>= maybe (liftIO (refuse (show (PeerSilent "the mailbox policy")) codePeerSilent))
                  pure
        >>= either (\e -> liftIO (refuse (show ("the peer refused the policy:"
                                                  <+> viaShow e))
                                         codeNotSet))
                   pure

      liftIO $ print $ vcat
        [ said
        , "policy version" <+> pretty v
        , pretty (safeText (policyText p'))
        ]

-- Both values behind flags, and both are keys of one type.
policyArgs :: forall c . IsContext c => [Syntax c] -> Maybe PolicyArgs
policyArgs syn = do
  kvs  <- flagsOf ["--mailbox","--key"] syn
  mbox <- flagOnce kvs "--mailbox" >>= asKey
  k    <- flagMaybe kvs "--key" asKey
  pure (PolicyArgs mbox k)
  where
    asKey = \case { SignPubKeyLike v -> Just v ; _ -> Nothing }

powUsage :: Doc ()
powUsage = "usage: hbs2-hub policy pow --mailbox <key> --bits <0..255>"

-- | @--mailbox <key> --bits <n>@.
--
-- The difficulty is a 'Word8' on the wire, and a value outside it is refused
-- rather than truncated: @--bits 256@ silently becoming 0 is a mailbox an
-- operator believes is charging the most work possible and which charges none.
powArgs :: forall c . IsContext c => [Syntax c] -> Maybe (HubKey, PoWDifficulty)
powArgs syn = do
  kvs  <- flagsOf ["--mailbox","--bits"] syn
  mbox <- flagOnce kvs "--mailbox" >>= asKey
  bits <- flagOnce kvs "--bits" >>= flagWord >>= inRange
  pure (mbox, bits)
  where
    asKey = \case { SignPubKeyLike v -> Just v ; _ -> Nothing }
    inRange n | n <= fromIntegral (maxBound :: Word8) = Just (fromIntegral n)
              | otherwise = Nothing

defaultUsage :: Doc ()
defaultUsage =
  "usage: hbs2-hub policy default --mailbox <key> [--sender allow|deny] [--peer allow|deny]"
    <> line <> "  both are required when the mailbox has no policy yet"

-- | @--mailbox <key> [--sender allow|deny] [--peer allow|deny]@.
--
-- Each default is optional HERE and constrained by the verb: absent means "do
-- not change this half", which only means something when there is a policy to
-- inherit from. The reader cannot tell, so the verb decides.
defaultArgs :: forall c . IsContext c
            => [Syntax c]
            -> Maybe (HubKey, Maybe BasicPolicyAction, Maybe BasicPolicyAction)
defaultArgs syn = do
  kvs    <- flagsOf ["--mailbox","--sender","--peer"] syn
  mbox   <- flagOnce kvs "--mailbox" >>= asKey
  sender <- flagMaybe kvs "--sender" asAction
  peer   <- flagMaybe kvs "--peer" asAction
  pure (mbox, sender, peer)
  where
    asKey = \case { SignPubKeyLike v -> Just v ; _ -> Nothing }
    -- The two words the policy file itself uses, and nothing else: "yes",
    -- "true" and "open" are all things somebody would type and all things a
    -- reader would have to guess the sense of.
    asAction v = flagText v >>= \case
      "allow" -> Just Allow
      "deny"  -> Just Deny
      _       -> Nothing

-- | @--mailbox <key>@, and nothing else. See 'HBS2.Hub.CLI.Ban.banListArgs'.
policyShowArgs :: forall c . IsContext c => [Syntax c] -> Maybe HubKey
policyShowArgs syn = do
  kvs <- flagsOf ["--mailbox"] syn
  flagOnce kvs "--mailbox" >>= \case { SignPubKeyLike v -> Just v ; _ -> Nothing }
