-- | Reading a repository's manifest (PEP-18 "Addressing and discovery").
--
-- "HBS2.Hub.Manifest" says what the clauses MEAN and is pure; this is the half
-- that fetches the bytes, and it is the piece every verb in this tool has been
-- working around. Six of them take a mailbox key or a sigil hash as an argument
-- and say in their help that the form which reads it from the manifest "needs a
-- manifest reader, which does not exist yet".
--
-- WHAT A MANIFEST IS, as far as this is concerned: the repository key names an
-- LWWRef, the ref's value is a log whose first entry is a tree, and the tree is
-- text that parses as S-expressions. That is hbs2-git3's layout, and it is read
-- here rather than imported because the whole of it is four calls and the
-- alternative is a package dependency from the forge onto the git remote
-- helper.
--
-- NOTHING HERE IS SIGNED CONTENT. The LWWRef is signed by the repository key --
-- that is what makes it the repository's manifest and not somebody's guess --
-- and everything inside it is whatever the owner wrote. A mailbox key found
-- here is a claim by the repository owner, which is exactly the claim a
-- contributor wants: they are asking where to send a letter ABOUT that
-- repository.
module HBS2.Hub.Repo.Manifest
  ( readManifest
  , ManifestGone(..)
  , codeNoManifest
  , mailboxFor
  , sigilFor
  , sigilTrouble
  , mailboxOf
  , sigilOf
  ) where

import HBS2.Hub.Types (HubKey,HubScheme,RepoRef,safeText)
import HBS2.Hub.Canon (clausesWith)
import HBS2.Hub.Letter (sigilOwner)
import HBS2.Hub.Manifest
import HBS2.Hub.Ingress (rpcTimeout,bounded,PeerSilent(..))

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.Merkle (getTreeContents)

import HBS2.Base58 (AsBase58(..))
import HBS2.Data.Detect (readLogThrow)
import HBS2.Net.Auth.Credentials.Sigil (loadSigil,Sigil)
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Peer.Proto.LWWRef
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Storage

import Control.Monad.Except (runExceptT)
import Data.Text.Encoding qualified as Text
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (listToMaybe)

-- | Why this node cannot say what a repository declares.
--
-- Told apart rather than collapsed, because two of them are a wait and the
-- rest are not: a peer that has never fetched the ref looks exactly like a
-- repository nobody has published, and telling a caller to try again is only
-- honest for one of those.
data ManifestGone =
    ManifestPeerSilent            -- ^ the peer is up and did not answer
  | ManifestNoRef RepoRef         -- ^ this peer holds no LWWRef for that key
  | ManifestEmpty                 -- ^ the ref is here and names nothing readable
  | ManifestUnreadable Text       -- ^ the tree would not read
  | ManifestUnparsed              -- ^ it read and is not a list of clauses
    -- | It read, it parsed, and it declares no ingress mailbox. A repository
    -- that is not a forge, or one whose owner has not published the clause
    -- yet: PEP-18 makes the mailbox optional, so this is a state and not a
    -- fault.
  | ManifestNoMailbox RepoRef
    -- | It declares a mailbox and no sigil for it. A letter cannot be sealed to
    -- a sign key, so this is a mailbox nobody outside can write to: the owner
    -- published half the pair.
  | ManifestNoSigil RepoRef HubKey
    -- | It declares a mailbox and a sigil, and the sigil is somebody else's.
    --
    -- THE SIGIL DECIDES WHERE THE LETTER GOES, not the @(mailbox K hub)@
    -- clause: @resolveKeys@ on the peer takes the recipient's SIGN key out of
    -- the sigil's own signed box. So an owner who writes their personal sigil
    -- into the clause gets every contribution delivered to their personal
    -- mailbox, while @hub inbox --repo K@ reads K and shows nothing, and the
    -- contributor is told @queued@ and exits 0. Three parties, all behaving
    -- correctly, and no error anywhere.
    --
    -- Carries what was declared and what the sigil actually names, because the
    -- fix is to change one of the two and the owner has to be able to see which.
  | ManifestSigilMismatch RepoRef HubKey HashRef HubKey
  deriving stock (Eq,Show)

instance Pretty ManifestGone where
  pretty = \case
    ManifestPeerSilent -> pretty (show (PeerSilent "the repository reference"))
    ManifestNoRef k ->
      "this peer holds no manifest for" <+> pretty (AsBase58 k) <> line
        <> "  it may not have fetched it yet, or the key may name no repository."
        <> line
        <> "  `hbs2-peer lwwref fetch` asks for one; naming the mailbox or the"
        <+> "sigil directly skips this lookup."
    ManifestEmpty ->
      "the repository reference is here and points at nothing this can read"
    ManifestUnreadable e -> "the manifest will not read:" <+> pretty (safeText e)
    ManifestUnparsed -> "the manifest is not a list of clauses"
    ManifestNoMailbox k ->
      pretty (AsBase58 k) <+> "declares no ingress mailbox" <> line
        <> "  it may not be a forge, or its owner may not have published the"
        <+> "clause. Naming --mailbox skips the lookup."
    ManifestNoSigil k mbox ->
      pretty (AsBase58 k) <+> "declares mailbox" <+> pretty (AsBase58 mbox)
        <+> "and no sigil for it" <> line
        <> "  a letter is sealed to an encryption key and a mailbox key is not"
        <+> "one, so nothing can be sent there until the owner publishes the"
        <> line <> "  sigil clause. Naming --recipient skips the lookup."
    ManifestSigilMismatch k mbox href names ->
      pretty (AsBase58 k) <+> "publishes a sigil for a mailbox that is not the"
        <+> "one it declares" <> line
        <> "  declared mailbox " <+> pretty (AsBase58 mbox) <> line
        <> "  sigil           " <+> pretty href <> line
        <> "  which names     " <+> pretty (AsBase58 names) <> line
        <> "  The SIGIL decides where a letter goes, so a letter sent here would"
        <+> "arrive at the" <> line
        <> "  second key while the owner reads the first and sees nothing."
        <+> "Nothing was sent." <> line
        <> "  The owner fixes it with `hbs2-git3 repo:mailbox:sigil`; naming"
        <+> "--recipient by hand" <> line <> "  overrides the clause if you"
        <+> "know which mailbox you mean."

-- | And what a verb that has nowhere else to go should exit with.
--
-- One code for every way of not having a manifest except silence, which keeps
-- its own: the remedy for the rest is the same, name the value by hand.
codeNoManifest :: Int
codeNoManifest = 41

-- | What a repository declares, or why this node cannot say.
--
-- Answers rather than exits, like 'HBS2.Hub.CLI.Policy.readPolicy': a verb that
-- was given the value explicitly should not pay for this at all, and one that
-- was not has its own words for the refusal.
readManifest :: forall m . ( MonadUnliftIO m
                           , HasStorage m
                           , HasClientAPI LWWRefAPI UNIX m
                           )
             => RepoRef -> m (Either ManifestGone [Syntax C])
readManifest repo = do
  sto <- getStorage
  api <- getClientAPI @LWWRefAPI @UNIX

  callRpcWaitMay @RpcLWWRefGet rpcTimeout api (LWWRefKey repo) >>= \case
    Nothing -> pure (Left ManifestPeerSilent)
    Just Nothing -> pure (Left (ManifestNoRef repo))
    Just (Just lww) -> do
      -- BOUNDED, like every other storage read in this package: the storage
      -- client's getBlock has no timeout of its own, so a peer that answers the
      -- ref service and then stalls would hang the verb rather than fail it.
      entries <- bounded rpcTimeout "the repository manifest"
                   (readLogThrow (getBlock sto) (lwwValue lww))

      case listToMaybe entries of
        Nothing -> pure (Left ManifestEmpty)
        Just mfref -> do
          r <- bounded rpcTimeout "the manifest tree"
                 (runExceptT (getTreeContents sto mfref))
          case r of
            Left e -> pure (Left (ManifestUnreadable (tshow e)))
            -- BOUNDED, through the same reader canon files go through. A
            -- manifest is fetched from wherever the repository key points, so
            -- it is a stranger's S-expression exactly as an event file is, and
            -- this handed it straight to 'parseTop': no byte bound, no clause
            -- bound, and 'parseTop' is superlinear in the number of top-level
            -- items (see 'scanText' -- 128 KB of bare atoms measured at 36 s).
            -- Every verb that resolves a repo key by hand reaches this.
            Right lbs ->
              pure $ either (const (Left ManifestUnparsed)) Right
                       (clausesWith maxManifestBytes maxManifestClauses
                          (Text.decodeUtf8Lenient (LBS.toStrict lbs)))
  where
    tshow :: Show e => e -> Text
    tshow = fromString . show

-- | What a manifest may weigh, and how many clauses it may have.
--
-- Not the event file's numbers, because a manifest is not an event: it is a
-- short declaration -- a few mailboxes, a sigil apiece, whatever else the repo
-- format puts there -- and a repository with a thousand of anything in its
-- manifest is not a shape this project has. Generous against that and still far
-- under where 'parseTop' starts costing seconds.
--
-- Refusing one is not refusing the repository, the same way refusing an index
-- is not: the caller answers 'ManifestUnparsed' and a verb given the mailbox by
-- hand never reads it at all.
maxManifestBytes, maxManifestClauses :: Int
maxManifestBytes   = 256 * 1024
maxManifestClauses = 1024

-- | The mailbox a verb should read: the one it was given, or the one the
-- repository declares.
--
-- The rule every verb in the inbox family follows, in one place so that they
-- follow the same one. A key given by hand wins and costs nothing -- no RPC is
-- made -- because a caller who names a mailbox is not asking to be corrected;
-- the manifest is consulted only when there is nothing else to go on.
mailboxFor :: forall m . ( MonadUnliftIO m
                         , HasStorage m
                         , HasClientAPI LWWRefAPI UNIX m
                         )
           => Maybe HubKey -> RepoRef -> m (Either ManifestGone HubKey)
mailboxFor (Just k) _ = pure (Right k)
mailboxFor Nothing repo =
  readManifest repo <&> (>>= maybe (Left (ManifestNoMailbox repo)) Right . mailboxOf)

-- | The sigil a letter should be sealed to: the one it was given, or the one
-- the repository publishes for its ingress mailbox.
--
-- The compose side of 'mailboxFor', and it needs both clauses: the mailbox
-- clause says WHERE a letter goes and the sigil clause carries the encryption
-- key it must be sealed with (PEP-18 puts both in the manifest for exactly this
-- reason -- a sign key is not an encryption key and no service resolves one to
-- the other).
--
-- A repository that declares a mailbox and no sigil for it is reported as
-- 'ManifestNoSigil' rather than as having no mailbox: the two are different
-- mistakes by the owner, and only one of them is "this is not a forge".
--
-- AND THE SIGIL IS CHECKED AGAINST THE MAILBOX IT IS PUBLISHED FOR, which
-- nothing did. The two clauses are written by hand and the sigil is the one
-- that decides anything: @resolveKeys@ on the peer takes the recipient's sign
-- key out of the sigil's own box, so @(mailbox-sigil K H)@ where H names some
-- other key delivers every contribution to that other key's mailbox. The owner
-- reads K and sees an empty queue, the contributor is told @queued@ and exits
-- 0, and no layer between them is wrong about anything. 'sigilNames' is exactly
-- this predicate and was applied on the reply side only.
--
-- One block read, and the sigil is about to be read anyway to seal the message.
--
-- A SIGIL GIVEN BY HAND IS NOT CHECKED, on the rule the whole family follows: a
-- caller who names one is not asking to be corrected, and @--recipient@ is the
-- documented way to mean a mailbox other than the declared one.
--
-- A sigil this node cannot read PROCEEDS, matching 'checkReplyChannel' on the
-- other side: missing bytes are not evidence of a mismatch, and the send fails
-- on its own if they are really gone.
sigilFor :: forall m . ( MonadUnliftIO m
                       , HasStorage m
                       , HasClientAPI LWWRefAPI UNIX m
                       )
         => Maybe HashRef -> RepoRef -> m (Either ManifestGone HashRef)
sigilFor (Just h) _ = pure (Right h)
sigilFor Nothing repo = do
  declared <- readManifest repo <&> \case
    Left e -> Left e
    Right mf -> case mailboxOf mf of
      Nothing -> Left (ManifestNoMailbox repo)
      Just mbox -> maybe (Left (ManifestNoSigil repo mbox))
                         (Right . (,) mbox)
                         (sigilOf mbox mf)

  case declared of
    Left e -> pure (Left e)
    Right (mbox, href) -> do
      sto <- getStorage
      sigilTrouble repo mbox href <$> loadSigil @HubScheme sto href

-- | Whether a sigil the manifest published for a mailbox is that mailbox's.
--
-- The decision on its own, so it can be asserted without a peer, an LWWRef and
-- a storage: what this says is which of two clauses an owner has to change, and
-- that should be readable without the three calls that fetch them.
--
-- 'Nothing' is a sigil this node could not read, and it PROCEEDS: absent bytes
-- are not evidence of a mismatch, and the send fails on its own if they are
-- really gone. The same answer 'HBS2.Hub.CLI.Compose.checkReplyChannel' gives
-- on the other side of the same question.
sigilTrouble :: RepoRef -> HubKey -> HashRef -> Maybe (Sigil HubScheme)
             -> Either ManifestGone HashRef
sigilTrouble repo mbox href msi = case sigilOwner =<< msi of
  Just names | names /= mbox -> Left (ManifestSigilMismatch repo mbox href names)
  _                          -> Right href

-- | The ingress mailbox a repository declares, if it declares one.
--
-- The PUBLIC tier, which is what a stranger's letter is for and what @hub
-- inbox@ reads. A repository declaring several inboxes (PEP-21 trust tiers)
-- has to be asked for the others by key, because picking one for the caller
-- would be picking whose letters they are about to fold.
mailboxOf :: [Syntax C] -> Maybe HubKey
mailboxOf mf = mbKey <$> mailboxByTier Nothing mf

-- | The sigil a repository publishes for one of its mailboxes.
--
-- The FIRST, and a repository may publish several: a sigil binds one
-- encryption key, so a mailbox read by three maintainers has three. Any of
-- them seals to a reader of that mailbox, which is what a letter needs; the
-- clauses are in the order the owner wrote them, so this is their preference
-- and not this reader's.
sigilOf :: HubKey -> [Syntax C] -> Maybe HashRef
sigilOf mbox mf = listToMaybe (sigilsFor mbox mf)
