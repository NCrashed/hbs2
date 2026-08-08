{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language MultiWayIf #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language PatternSynonyms #-}
module MailboxProtoWorker ( mailboxProtoWorker
                          , createMailboxProtoWorker
                          , mailboxProtoWorkerSetProbe
                          , MailboxProtoWorker
                          , IsMailboxProtoAdapter
                          , MailboxProtoException(..)
                          , hbs2MailboxDirOpt
                          ) where

import HBS2.Prelude.Plated
import MailboxConfig
import MailboxMerged
import MailboxQueue
import HBS2.OrDie
import HBS2.Actors.Peer
import HBS2.Data.Types.Refs
import HBS2.Data.Detect
import HBS2.Net.Proto
import HBS2.Base58
import HBS2.Storage
import HBS2.Storage.Operations.Missed
import HBS2.Storage.Operations.ByteString
import HBS2.Merkle
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.SignedBox
import HBS2.Net.Proto.Types
import HBS2.Peer.Proto
import HBS2.Peer.Proto.Mailbox
import HBS2.Peer.Proto.Mailbox.Entry
import HBS2.Peer.Proto.Mailbox.Merge
import HBS2.Peer.Proto.Mailbox.Nonce
import HBS2.Peer.Proto.Mailbox.Relayed
import HBS2.Peer.Proto.Mailbox.PolicyCache
import HBS2.Peer.Proto.Mailbox.Policy
import HBS2.Peer.Proto.Mailbox.Policy.Basic
import HBS2.Peer.Proto.Mailbox.PoW
import HBS2.Net.Messaging.Unix
import HBS2.Net.Auth.Credentials

import HBS2.Polling
import HBS2.System.Dir
import HBS2.Misc.PrettyStuff

import Brains
import PeerConfig
import PeerTypes

import DBPipe.SQLite as Q

import Data.Config.Suckless.Script

import Control.Concurrent.STM qualified as STM
-- import Control.Concurrent.STM.TBQueue
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Except (throwError)
import Data.Coerce
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Set qualified as Set
import Data.Either
import Data.List qualified as L
import Data.Maybe
import Data.Word
import Data.Hashable
import Codec.Serialise
import Lens.Micro.Platform
import Text.InterpolatedString.Perl6 (qc)
import Streaming.Prelude qualified as S
import UnliftIO

-- | How many mailbox trees this peer will be trying to fetch at once.
--
-- A bound on what an ANNOUNCEMENT may cost, which is a different question from
-- which mailbox may be announced: a status names a tree root, a root is a value
-- the sender invents, and each one queued costs a poll every ten seconds, an
-- entry in the downloader's working set, and a row in the brains database that
-- outlives a restart.
--
-- The number is a judgement. An honest peer has one live root per mailbox it
-- hosts, plus whatever is in flight while co-hosts converge, so this is far
-- above any real steady state and small enough that the ten-second sweep over
-- it stays a rounding error.
maxMailboxDownloads :: Int
maxMailboxDownloads = 256

newtype PolicyHash = PolicyHash HashRef
                     deriving newtype (Eq,Ord,Show,Hashable,Pretty)

instance FromField PolicyHash where
  fromField s = PolicyHash . fromString <$> fromField @String s

instance ToField PolicyHash where
  toField f = toField (show $ pretty f)

data MailboxProtoException =
    MailboxProtoWorkerTerminatedException
  | MailboxProtoCantAccessMailboxes FilePath
  | MailboxProtoMailboxDirNotSet
  deriving stock (Show,Typeable)

instance Exception MailboxProtoException

hbs2MailboxDirOpt :: String
hbs2MailboxDirOpt = "hbs2:mailbox:dir"

-- The mailbox options this worker reads live in "MailboxConfig", where a test
-- can ask what a config means without building a worker.

{- HLINT ignore "Functor law" -}

data PolicyDownload s =
  PolicyDownload
  { policyDownloadWhen :: Word64
  , policyDownloadWhat :: SetPolicyPayload s
  , policyDownloadBox  :: HashRef
  }
  deriving stock (Generic)

instance ForMailbox s => Serialise (PolicyDownload s)

deriving instance ForMailbox s => Eq (PolicyDownload s)

instance ForMailbox s => Hashable (PolicyDownload s) where
  hashWithSalt s p = hashWithSalt s (serialise p)

data MailboxDownload s =
  MailboxDownload
  { mailboxRef         :: MailboxRefKey s
  , mailboxStatusRef   :: HashRef
  , mailboxDownWhen    :: Word64
  , mailboxDownPolicy  :: Maybe PolicyVersion
  , mailboxDownDone    :: Bool
    -- | Who announced this tree. Carried so that the messages found inside it
    -- are admitted under the same peer policy as the ones that arrive by
    -- 'SendMessage': the merge path passed 'mzero' here, which 'mailboxInQ'
    -- reads as "no peer to check".
  , mailboxDownPeer    :: PubKey 'Sign s
  }
  deriving stock (Generic)

deriving stock instance ForMailbox s => Eq (MailboxDownload s)

instance ForMailbox s => Hashable (MailboxDownload s)

data MailboxProtoWorker (s :: CryptoScheme) e =
  MailboxProtoWorker
  { mpwPeerEnv            :: PeerEnv e
  , mpwStorage            :: AnyStorage
  , mpwCredentials        :: PeerCredentials s
  , mpwFetchQ             :: TVar (HashSet (MailboxRefKey s))
    -- | Nonces we have put in our own outgoing 'CheckMailbox' requests. A
    -- 'MailboxStatus' is an answer to one of ours or it is somebody talking
    -- unprompted; see "HBS2.Peer.Proto.Mailbox.Nonce".
  , mpwCheckNonces        :: CheckNonces (MailboxRefKey s)
    -- | What this peer has already put back on the wire, so that gossip does
    -- not circulate a message for as long as the graph has cycles. In memory
    -- and bounded by count; it used to be a block whose address a stranger
    -- could compute, see "HBS2.Peer.Proto.Mailbox.Relayed".
  , mpwRelayed            :: Relayed
    -- | The parsed policy for each hosted mailbox, keyed by the hash it was
    -- parsed from, so a policy change invalidates it by not matching. Reading
    -- one is a getBlock, a signature check, a merkle read and two parses, and
    -- it sat on two paths a stranger drives at whatever rate they like; see
    -- "HBS2.Peer.Proto.Mailbox.PolicyCache".
  , mpwPolicyCache        :: PolicyCache (MailboxRefKey s) (BasicPolicy s)
    -- | Least proof-of-work this peer will forward, from
    -- 'hbs2MailboxPoWMinOpt'. A TVar because it is read from the config after
    -- the worker exists, the way the database and the probe are.
  , mpwPoWFloor           :: TVar PoWDifficulty
    -- | Peers this one takes replication from for a charging mailbox, from
    -- 'hbs2MailboxReplicateFromOpt'. Read from the config like the floor above.
  , mpwReplicateFrom      :: TVar (HashSet (PubKey 'Sign s))
    -- | What is in the queue right now, by message hash. Not a dedup of
    -- everything ever seen: see 'mailboxAcceptMessage' for why it is exactly
    -- as long-lived as one batch.
    -- The value is whether the QUEUED copy pays its way. A stamp is not part of
    -- the message it pays for, so two copies of one letter hash alike and only
    -- this tells them apart; see 'mailboxAcceptMessage'.
  , inMessageQueueSeen    :: TVar (HashMap HashRef Bool)
    -- | How much of the queue each sender holds right now.
    --
    -- Cleared with the batch, like the set above, and for the same reason: it
    -- is a fact about what is IN the queue, so it has to stop being true when
    -- the queue empties. What it bounds is starvation -- one peer taking every
    -- slot -- which is the half proof-of-work cannot reach, since most
    -- mailboxes charge nothing.
  , inMessageQueuePeers   :: TVar (HashMap (PubKey 'Sign s) Int)
    -- | What this peer believes each mailbox charges, from the last time the
    -- drain read its policy.
    --
    -- A CACHE and never an authority: the drain re-reads the signed policy for
    -- every message it takes, and this only decides whether the message gets
    -- as far as the drain. Missing means "let it through", so a cold peer
    -- behaves exactly as it did before this existed.
  , mpwPoWKnown           :: TVar (HashMap (PubKey 'Sign s) PoWDifficulty)
  , inMessageQueue        :: TBQueue ( Maybe (PubKey 'Sign s)
                                     , MessageOrigin s
                                     , Message s
                                     , MessageContent s )
  , inMessageMergeQueue   :: TVar (HashMap (MailboxRefKey s) (HashSet HashRef))
  , inPolicyDownloadQ     :: TVar (HashMap HashRef (PolicyDownload s))
  , inMailboxDownloadQ    :: TVar (HashMap HashRef (MailboxDownload s))
  , inMessageQueueInNum   :: TVar Int
  , inMessageQueueOutNum  :: TVar Int
  , inMessageQueueDropped :: TVar Int
  , inMessageDeclined     :: TVar Int
  , mailboxDB             :: TVar (Maybe DBPipeEnv)
  , probe                 :: TVar AnyProbe
  }

okay :: Monad m => good -> m (Either bad good)
okay good = pure (Right good)

-- | Сколько держать незавершённую загрузку, прежде чем сдаться.
--
-- Оба поля времени -- mailboxDownWhen и policyDownloadWhen -- писались и не
-- читались никем, а запись уходила из очереди только по полностью успешной
-- загрузке. Значит корень, блоки которого не придут никогда, оставался там
-- навсегда и переспрашивался каждые две секунды.
--
-- Сдаться не значит потерять: mailboxCheckQ переспрашивает статусы своих ящиков
-- по кругу, поэтому живая загрузка вернётся в очередь сама. Час -- это с большим
-- запасом на большое дерево и всё ещё конечно.
mailboxDownTTL, policyDownTTL :: Word64
mailboxDownTTL = 3600
policyDownTTL  = 3600

-- 'PlainMessageDelete' moved to HBS2.Peer.Proto.Mailbox.Merge, beside the
-- decision that has to read the same predicate. It lived here, in an executable
-- module, which is part of why the merge path never consulted it: nothing that
-- could be tested could see it either.

instance IsAcceptPolicy HBS2Basic () where
  policyAcceptPeer _ _ = pure True
  policyAcceptMessage _ _ _ = pure True
  policyAcceptSender _ _ = pure True

instance (s ~ HBS2Basic, e ~ L4Proto, s ~ Encryption e) => IsMailboxProtoAdapter s (MailboxProtoWorker s e) where

  mailboxGetCredentials = pure . mpwCredentials

  mailboxGetStorage = pure . mpwStorage

  mailboxGetPolicy me mbox = do
    let def = AnyPolicy (defaultBasicPolicy @s)
    fromMaybe def <$> mailboxGetPolicyMay @s me mbox

  -- Только записанная владельцем policy, без подстановки умолчания.
  --
  -- Различие проходит по СТРОКЕ в таблице, а не по читаемости содержимого: если
  -- строка есть, а блок потерян или клауза от более новой сборки не
  -- разбирается, loadPolicyContentAt отдаёт deny/deny, и это правильная сторона
  -- -- владелец что-то сказал. Nothing означает ровно одно: не говорил ничего.
  --
  -- Пока дерево policy ЕЩЁ КАЧАЕТСЯ, строки нет вовсе: её пишет
  -- mailboxSetPolicy, а policyDownloadQ зовёт его уже после того, как
  -- findMissedBlocks вернул пусто. Так что незакончённая загрузка -- это
  -- Nothing, а не deny/deny.
  --
  -- Один select, а не два: хеш, по которому проверяется наличие, он же и
  -- читается. Эта функция стоит под mailboxGetPolicy, то есть на пути приёма
  -- каждого сообщения для каждого получателя.
  --
  -- И под ней же -- ответ на CheckMailbox и приём чужого MailboxStatus, то есть
  -- два пути, темп которых задаёт чужой пир. Select остаётся на каждом запросе:
  -- он индексный и локальный, а его результат -- и признак «владелец что-то
  -- сказал», и штамп годности кэша. Всё, что за ним (getBlock, проверка
  -- подписи, обход merkle-дерева, два разбора), считается один раз на
  -- (ящик, хеш). Подробности и границы -- в PolicyCache.
  mailboxGetPolicyMay MailboxProtoWorker{..} mbox = runMaybeT do
    dbe <- readTVarIO mailboxDB >>= toMPlus
    ha  <- policyHashFor dbe mbox >>= toMPlus
    AnyPolicy <$> policyAt mpwPolicyCache mbox ha (loadPolicyContentAt mpwStorage mbox ha)

  mailboxCheckNonce MailboxProtoWorker{..} mbox nonce =
    acceptCheckNonce mpwCheckNonces mbox nonce

  mailboxRelayOnce MailboxProtoWorker{..} = relayOnce mpwRelayed

  mailboxPoWFloor MailboxProtoWorker{..} = readTVarIO mpwPoWFloor

  mailboxReplicateFrom MailboxProtoWorker{..} = readTVarIO mpwReplicateFrom

  mailboxAcceptMessage MailboxProtoWorker{..} peer origin m c = do
    -- ONE SLOT PER DISTINCT MESSAGE, however many copies arrive.
    --
    -- Accept was deliberately taken out from under the gossip marker (see
    -- takeMessage in HBS2.Peer.Proto.Mailbox): a message refused by a policy
    -- that had not been written yet was otherwise lost for good, since nothing
    -- stored it and every later copy was suppressed as `seen`. The cost of that
    -- was a queue slot per COPY, and the queue is 8000 deep and drained every
    -- ten seconds, so replaying one packet at link rate filled it and the
    -- honest Submitted messages behind it were dropped -- permanently, since
    -- only the replication path retries.
    --
    -- The set is what makes a replay free again without putting the marker back
    -- in front of accept. It holds what is IN FLIGHT and nothing else: the
    -- drain clears it as it takes the batch, so a message refused this round is
    -- queued again on the next copy, which is the property the marker was
    -- removed to get.
    let h = HashRef (hashObject (serialise m))

    -- WHAT THE WORK PAYS FOR IS THE SLOT, and this is where it is charged.
    --
    -- The drain checks the signed policy ten seconds later and is the
    -- authority; before this, that was the ONLY check, so a flood of distinct
    -- unstamped messages to a mailbox charging (pow D) took a slot apiece and
    -- the honest submissions behind them were dropped. Free to send, which is
    -- the one thing proof-of-work exists to stop.
    --
    -- Against a CACHE, and it fails open in every direction (see 'paidFor'): a
    -- mailbox this peer has not read a policy for pays, so a cold peer behaves
    -- exactly as it did. Replication carries no stamp and is not charged here
    -- either, for the reason the drain does not charge it: a co-host has none
    -- to offer.
    known <- readTVarIO mpwPoWKnown
    let paid = case origin of
          Replicated -> True
          Submitted stamp ->
            paidFor (`HM.lookup` known)
                    (\d r -> maybe False (stampOk d r m) stamp)
                    (Set.toList (messageRecipients c))

    verdict <- atomically do
      inflight <- readTVar inMessageQueueSeen
      -- WHAT IS IN FLIGHT IS A MESSAGE AND WHETHER THE QUEUED COPY PAYS, and
      -- the second half is why this is a map rather than a set.
      --
      -- The stamp is deliberately outside 'Message' (it must not be inside what
      -- the sender signs), so a stamped copy and a plain one of the same letter
      -- hash alike -- and the tuple in the queue keeps whichever arrived first.
      -- An attacker who has seen a paid letter on gossip could therefore
      -- re-send it stripped, land the plain copy first in every drain window,
      -- have the honest stamped copy deduped away as a repeat, and watch the
      -- drain refuse the queued one ten seconds later for want of work. The
      -- sender paid 2^D hashes and got no rejection to look at.
      --
      -- So a copy that PAYS is admitted once even when a copy that does not is
      -- already queued. It costs one extra slot, once per message per batch:
      -- after the upgrade the map says paid and every further copy is free
      -- again. The drain handles the pair in either order -- whichever is taken
      -- second finds the merge already recorded and skips.
      if not (takesASlot (HM.lookup h inflight) paid)
        -- Queued already, by a copy no worse than this one.
        then pure (Right True)
        else do
          queued <- readTVar inMessageQueueInNum
          ps <- readTVar inMessageQueuePeers
          -- Nothing for this node's own submission over its own RPC, which is not
          -- somebody else's traffic and is not rationed against it.
          let held = fmap (\p -> fromMaybe 0 (HM.lookup p ps)) peer
          case admitTo queued held paid of
            Left why -> do
              modifyTVar inMessageQueueDropped succ
              pure (Left why)
            Right () -> do
              writeTBQueue inMessageQueue (peer, origin, m, c)
              modifyTVar   inMessageQueueInNum succ
              writeTVar    inMessageQueueSeen (HM.insert h paid inflight)
              for_ peer $ \p -> modifyTVar inMessageQueuePeers (HM.insertWith (+) p 1)
              pure (Right True)

    -- Said out loud, and said WHICH: the counter is published on the probe, but
    -- a drop is a message that will not arrive, and the three reasons send an
    -- operator to three different places -- this peer is behind, one neighbour
    -- is flooding, or somebody is flooding without paying.
    case verdict of
      Right _ -> pure ()
      Left why -> warn $ red "mailbox: message dropped at the queue"
                          <+> viaShow why <+> pretty h
                          <+> parens (pretty (fmap AsBase58 peer))

    pure (either (const False) id verdict)

  mailboxAcceptDelete MailboxProtoWorker{..} mbox dmp box = do
    debug $ red "<<>> mailbox: mailboxAcceptDelete" <+> pretty mbox

    let sto = mpwStorage
    -- TODO: add-policy-reference

    flip runContT pure do

      -- Ящик должен быть наш, и это та же проверка, которую делает
      -- mailboxSendDelete прямо ниже. Здесь её не было вообще: ни таблицы
      -- mailbox, ни policy -- только TODO выше.
      --
      -- Что это стоило. Ключ, которым подписан proof, и есть ключ ящика (см.
      -- admitDeleted), поэтому любой пир после хендшейка мог сгенерировать
      -- одноразовую пару, подписать DeleteMessagesPayload на самого себя и
      -- прислать DeleteMessages: мы писали блок бокса, писали блок Deleted,
      -- ставили мерж в очередь и в mailboxMergeQ звали updateRef на
      -- MailboxRefKey, о котором не слышали никогда. Повторить с новой парой --
      -- и так сколько угодно раз, причём сообщение ещё и уходит дальше по сети
      -- (gossip выше по стеку, в mailboxProto). То есть незнакомец заводил у нас
      -- ref'ы и занимал диск в темпе, который выбирал сам.
      --
      -- Policy тут по-прежнему не спрашивается: дефолт BasicPolicy это
      -- Deny/Deny, и включить её на этом пути значит запретить владельцу
      -- удалять в своём же ящике, пока policy не выставлена явно. Это отдельный
      -- разговор, TODO выше про него и остаётся.
      mdbe <- readTVarIO mailboxDB

      dbe <- ContT $ maybe1 mdbe dbNotReady

      t <- getMailboxType_ dbe mbox

      void $ ContT $ maybe1 t notOurs

      -- Предикат разбирается до всякой записи, потому что из него и из бокса
      -- выводится хеш записи, а из него -- маркер «уже влито».
      let what' = case dmp of
                   PlainMessageDelete x -> Just x
                   _ -> Nothing

      what <- ContT $ maybe1 what' unsupportedPredicate

      -- Повторно пришедший delete теперь стоит один поиск.
      --
      -- Каждый delete-box, который выпускает владелец, публичен, рассылается по
      -- сети и лежит блоком, а fold-then-delete из PEP-21 делает их выпуск
      -- рутиной. Раньше каждое повторное получение одного и того же бокса писало
      -- блок, ставило запись в очередь слияния, и mailboxMergeQ на следующем
      -- опросе перечитывал ВЕСЬ лог ящика и пересобирал дерево целиком: повтор
      -- одного публичного сообщения раз в две секунды -- это O(N) перестройка
      -- раз в две секунды.
      --
      -- Вызов стоит вне гейта `unless seen` и должен там стоять: это
      -- единственный путь, по которому доезжает незавершённый мерж. Поэтому
      -- дешёвый выход нужен здесь. Оба хеша считаются без хранилища; putBlock
      -- ниже вернёт ровно boxH, содержимое адресуется своим хешем.
      let boxH   = HashRef (hashObject (serialise box))
          entry  = deletedEntry boxH what
          entryH = deletedEntryHash boxH what

      merged <- isMerged dbe mbox entryH

      void $ ContT $ maybe1 (guard (not merged) :: Maybe ()) (alreadyMerged entryH)

      h' <- putBlock sto (serialise box)

      void $ ContT $ maybe1 h' storageFail

      deh' <- enqueueBlock sto (serialise entry)
               <&> fmap HashRef

      deh <- ContT $ maybe1 deh' storageFail

      -- insertWith for the reason the download path needs it: two deletes issued
      -- for one mailbox between two merge polls collided here and the first was
      -- dropped.
      atomically $ modifyTVar inMessageMergeQueue (enqueueMerge mbox deh)

    where
      storageFail = err $ red "mailbox (storage:critical)" <+> "block writing failure"
      unsupportedPredicate = err $ red "mailbox (unsuported-predicate)"
      dbNotReady = err $ red "mailbox (delete)" <+> "database not ready"
      notOurs = debug $ red "mailbox (delete)"
                  <+> "not ours, ignored:" <+> pretty mbox
      alreadyMerged e = debug $ "mailbox (delete): already merged, skip"
                          <+> pretty mbox <+> pretty e

instance ( s ~ Encryption e, e ~ L4Proto
         ) => IsMailboxService s (MailboxProtoWorker s e) where
  mailboxCreate MailboxProtoWorker{..} t p = do
    debug $ "mailboxWorker.mailboxCreate" <+> pretty (AsBase58 p) <+> pretty t

    flip runContT pure $ callCC \exit -> do

      mdbe <- readTVarIO mailboxDB

      dbe <- ContT $ maybe1 mdbe (pure $ Left (MailboxCreateFailed "database not ready"))

      -- Существующий ящик ДРУГОГО типа -- это отказ, а не тишина.
      --
      -- `on conflict do nothing` ниже оставляет старую строку как есть, и раньше
      -- этот путь всё равно отвечал Right (): `create --key K relay` для ящика,
      -- заведённого как hub, сообщал об успехе и не менял ничего. Тип решает,
      -- как ящик себя ведёт, так что это ровно тот ответ, за который потом
      -- никто не сможет зацепиться. Повтор с ТЕМ ЖЕ типом остаётся успехом:
      -- create идемпотентен, и на это опираются скрипты.
      existing <- getMailboxType_ dbe (MailboxRefKey @s p)

      case existing of
        Just t0 | t0 /= t ->
          exit $ Left (MailboxCreateFailed
                        (show $ "mailbox" <+> pretty (AsBase58 p) <+> "already exists as"
                                  <+> pretty t0 <> ", not" <+> pretty t))
        _ -> none

      r <- liftIO $ try @_ @SomeException $ withDB dbe do
             insert [qc|
             insert into mailbox (recipient,type)
             values (?,?)
             on conflict (recipient) do nothing
                       |] (show $ pretty $ AsBase58 p, show $ pretty t)

      case r of
        Right{} -> pure $ Right ()
        Left{}  -> pure $ Left (MailboxCreateFailed "database operation")

  mailboxSetPolicy me@MailboxProtoWorker{..} sbox = do
    -- check policy version
    -- check policy has peers
    -- write policy block
    -- update reference to policy block
    --
    -- test: write policy, check mailboxGetStatus

    debug $ red "mailboxSetPolicy"

    runExceptT do

      -- check policy signature
      (who, spp) <- unboxSignedBox0 sbox
                      & orThrowError (MailboxAuthError "invalid signature")

      -- Ящик, названный ВНУТРИ полиси, должен быть тем, кто её подписал.
      --
      -- Этой проверки не было, и два пути расходились: строка пишется под
      -- ключом ПОДПИСАНТА (ниже), а policyDownloadQ читает текущую версию под
      -- sppMailboxKey. Полиси, называющая внутри чужой ящик, сравнивалась по
      -- версии с чужим и записывалась под свой -- то есть могла молча
      -- перепрыгнуть законное обновление чужого ящика по номеру версии.
      -- Записать чужую полиси это не давало (ключ записи всегда подписант), но
      -- два ответа на вопрос «чья это полиси» -- на один больше, чем нужно.
      unless (sppMailboxKey spp == who) do
        throwError (MailboxAuthError "policy names a mailbox it is not signed for")

      -- The door's cache of what this mailbox charges is now stale, and stale
      -- HIGH is the direction that costs: a policy that lowered D would have
      -- the queue refusing messages the mailbox has just started accepting.
      -- Forgotten rather than updated, since forgetting fails open and the
      -- drain fills it again from the policy it reads.
      atomically $ modifyTVar mpwPoWKnown (HM.delete who)

      dbe <- readTVarIO mailboxDB
                >>= orThrowError (MailboxSetPolicyFailed "database not ready")

      loaded <- loadPolicyPayloadFor dbe mpwStorage (MailboxRefKey @s who)
                  <&> fmap ( unboxSignedBox0 @(SetPolicyPayload s) @s .  snd )
                  <&> join

      what <- case loaded of
        Nothing -> do
          err $ red "mailboxSetPolicy FUCKED"
          putBlock mpwStorage (serialise sbox)
            >>= orThrowError (MailboxSetPolicyFailed "storage error")
            <&> HashRef

        Just (k, spp0) | sppPolicyVersion spp > sppPolicyVersion spp0 || k /= who -> do
          putBlock mpwStorage (serialise sbox)
            >>= orThrowError (MailboxSetPolicyFailed "storage error")
            <&> HashRef

        _ -> do
         throwError (MailboxSetPolicyFailed "too old")

      liftIO $ withDB dbe $ Q.transactional do
        insert [qc| insert into policy (mailbox,hash) values(?,?)
                    on conflict (mailbox) do update set hash = excluded.hash
                  |] (MailboxRefKey @s who, PolicyHash what)

      -- Политика, которая молчит про пиров, запрещает их всех.
      --
      -- parseBasicPolicy ставит bpDefaultPeerAction = Deny, поэтому
      -- `(sender allow all)` без единой клаузы про peer -- это deny всем пирам.
      -- Раньше это стоило приёма сообщений от пиров; теперь -- ещё и ответа на
      -- CheckMailbox, и приёма чужих статусов, то есть ящик перестаёт
      -- синхронизироваться вовсе. Сокращённые рецепты в PEP-21 записаны именно
      -- так; полный, с `(peer allow all)`, есть в PEP-18.
      --
      -- Предупреждение тут, а не в самих проверках: сюда приходят только при
      -- смене политики, а проверки -- на каждый запрос.
      pol <- loadPolicyContentAt mpwStorage (MailboxRefKey @s who) what

      when (bpDefaultPeerAction pol == Deny && HM.null (bpPeers pol)) do
        warn $ red "mailbox: policy denies every peer for" <+> pretty (AsBase58 who)
                 <> line <> indent 2 ( "no (peer allow ...) clause, so this mailbox"
                                       <+> "will neither answer nor accept a status" )

      void $ runMaybeT do
        msp <- mailboxGetStatus me (MailboxRefKey @s who)
                     >>= toMPlus
                     >>= toMPlus

        creds <- mailboxGetCredentials @s me
        let box = makeSignedBox @s (view peerSignPk creds) (view peerSignSk creds) msp

        liftIO $ withPeerM mpwPeerEnv do
          gossip (MailBoxProtoV1 @s @e (MailboxStatus box))

      pure what

  mailboxDelete MailboxProtoWorker{..} mbox = do

    flip runContT pure do

      mdbe <- readTVarIO mailboxDB

      dbe <- ContT $ maybe1 mdbe (pure $ Left (MailboxOperationError "database not ready"))

      debug $ red "delete fucking mailbox" <+> pretty (MailboxRefKey @s mbox)

      -- TODO: actually-purge-messages-and-attachments

      -- Обе строки, и в одной транзакции.
      --
      -- Полиси не удалялась, а внешнего ключа между таблицами нет, поэтому она
      -- переживала ящик: пересоздание того же ключа воскрешало старую. Для
      -- ящика, который удалили ИМЕННО ЧТОБЫ сбросить открытую
      -- `(sender allow all)`, это возвращало его открытым.
      liftIO $ withDB dbe $ Q.transactional do
        insert [qc| delete from mailbox where recipient = ? |] (Only (MailboxRefKey @s mbox))
        insert [qc| delete from policy where mailbox = ? |] (Only (MailboxRefKey @s mbox))

      delRef mpwStorage (MailboxRefKey @s mbox)

      pure $ Right ()

  mailboxSendDelete w@MailboxProtoWorker{..} box = do
    debug $ red "mailboxSendDelete"

    flip runContT pure do

      -- 1. unpack-and-check
      let r = unboxSignedBox0 box

      (k, _) <- ContT $ maybe1 r authFailed

      mdbe <- readTVarIO mailboxDB

      dbe <- ContT $ maybe1 mdbe dbNotReady

      t <- getMailboxType_ dbe (MailboxRefKey @s k)

      void $ ContT $ maybe1 t (noMailbox k)

      -- 2. what?
      -- gossip and shit

      liftIO $ withPeerM mpwPeerEnv do
        me <- ownPeer @e
        runResponseM me $ do
          mailboxProto @e True w (MailBoxProtoV1 (DeleteMessages box))

      okay ()

    where
      dbNotReady = pure $ Left (MailboxOperationError "database not ready")
      authFailed = pure $ Left (MailboxAuthError "inconsistent signature")
      noMailbox k = pure $
        Left (MailboxOperationError (show $ "no mailox" <+> pretty (AsBase58 k)))


  mailboxSendMessage w@MailboxProtoWorker{..} stamp mess = do
    -- we do not check message signature here
    -- because it will be checked in the protocol handler anyway
    liftIO $ withPeerM mpwPeerEnv do
      me <- ownPeer @e
      runResponseM me $ do
        mailboxProto @e True w (MailBoxProtoV1 (maybe (SendMessage mess)
                                                      (SendMessageStamped mess)
                                                      stamp))

    pure $ Right ()

  mailboxListBasic  MailboxProtoWorker{..} = do

    flip runContT pure do

      mdbe <- readTVarIO mailboxDB

      dbe <- ContT $ maybe1 mdbe (pure $ Left (MailboxCreateFailed "database not ready"))

      debug $ red "mailboxListBasic"

      r <- listMailboxes dbe

      pure $ Right r

  mailboxAcceptStatus me@MailboxProtoWorker{..} ref who origin s2@MailBoxStatusPayload{..} = do

    flip runContT pure $ callCC \stop -> do

      s0 <- runMaybeT do
        MailBoxStatusPayload{..} <- mailboxGetStatus me ref
                                      >>= toMPlus
                                      >>= toMPlus
        toMPlus mbsMailboxHash

      now <- liftIO $ getPOSIXTime <&> round

      mdbe <- readTVarIO mailboxDB

      dbe <- ContT $ maybe1 mdbe (pure $ Left (MailboxCreateFailed "database not ready"))

      -- Ящик должен быть нашим, и этой проверки здесь не было вовсе.
      --
      -- Без неё любой пир после хендшейка объявлял статус для ключа, который сам
      -- же и выдумал, а мы на каждую пару (версия, хеш) клали запись в
      -- inMailboxDownloadQ и просили скачать выбранный им корень. Запись уходит
      -- оттуда только когда дерево скачано целиком и без единой неудачи, так что
      -- корень, блоки которого не придут никогда, оставался там навсегда: N
      -- фальшивых статусов -- N вечных записей и N вечных запросов на загрузку,
      -- переспрашиваемых каждые две секунды. Плюс сама policy пишется блоком до
      -- всякой проверки.
      --
      -- Теперь чужой ключ назвать нельзя. СКОЛЬКО деревьев можно назвать для
      -- своего -- отдельный вопрос, и на него отвечает maxMailboxDownloads
      -- ниже: ключ очереди это пара (версия, хеш), то есть запись на каждый
      -- объявленный корень, а корень объявляющий выдумывает.
      t <- getMailboxType_ dbe ref

      when (isNothing t) do
        debug $ "mailbox: status for a mailbox we do not host, ignored" <+> pretty ref

      void $ ContT $ maybe1 t (okay ())

      p0 <- loadPolicyPayloadFor dbe mpwStorage ref
              <&> fmap (sppPolicyVersion . snd) . ((unboxSignedBox0 . snd) =<<)
              <&> fromMaybe 0

      let bogusPolicyMessage  =
              err $ red "!!! arrived invalid policy signature for"
                  <+> pretty ref
                  <+> "from"
                  <+> pretty (AsBase58 who)

      -- ТОЛЬКО ПО ОТВЕТУ НА НАШ ВОПРОС.
      --
      -- Дерево -- это хеш, который кто-то объявил, и принять его значит скачать
      -- и слить всё, что под ним. Policy ниже подписана владельцем и проверяет
      -- себя сама, поэтому она идёт и из непрошеного статуса; дерево -- нет.
      let downloadStatus v = do
            maybe1 mbsMailboxHash (okay ()) $ \h -> do
              when (s0 /= Just h && useTree (statusUse origin)) do
                -- one download per version per hash
                let downKey = HashRef $ hashObject (serialise (v,h))

                -- AND NO MORE THAN 'maxMailboxDownloads' OF THEM.
                --
                -- The key is the (version, hash) pair, so the bound is one
                -- entry per ROOT somebody names, and a root is a value they
                -- invent. The note above says the queue is bounded by the
                -- number of mailboxes we hold, and the mailbox check it
                -- describes does bound WHICH KEY may be named -- not how many
                -- trees may be named for it.
                --
                -- What each entry costs while it sits there: a poll every ten
                -- seconds, each a findMissedBlocks; an entry in the
                -- downloader's `wip`, which its sweeper removes only once the
                -- download completes, so a root whose blocks never arrive is
                -- never swept; and a row in the brains database, deleted only
                -- when the block turns up -- that one SURVIVES RESTARTS.
                -- Nothing rate-limits the requests, so this was a line-rate
                -- input to three unbounded stores at once.
                --
                -- Over the cap the announcement is dropped rather than
                -- queued, and dropping it is cheap to be wrong about: a peer
                -- that really holds a tree re-announces it on the next
                -- 'mailboxCheckQ' round.
                --
                -- The download is asked for AFTER the slot is taken, not
                -- before, or the refusal costs the same fetch it exists to
                -- refuse.
                taken <- atomically do
                  q <- readTVar inMailboxDownloadQ
                  if HM.member downKey q then pure False
                    else if HM.size q >= maxMailboxDownloads then pure False
                    else do
                      writeTVar inMailboxDownloadQ
                        (HM.insert downKey (MailboxDownload ref h now v False who) q)
                      pure True

                if taken then startDownloadStuff me h else
                  debug $ red "mailbox: download queue full, status ignored"
                            <+> pretty ref <+> pretty h
              okay ()

      case mbsMailboxPolicy of
        Nothing -> downloadStatus Nothing

        Just newPolicy -> do

          -- TODO: handle-invalid-policy-error
          --   not "okay" actually

          (rcptKey, pNew) <- ContT $ maybe1 (unboxSignedBox0 newPolicy)
                                            (bogusPolicyMessage >> okay ())

          when (coerce rcptKey /= ref) $ lift bogusPolicyMessage >> stop (Right ())

          when (sppPolicyVersion pNew > p0) do
            startDownloadStuff me (sppPolicyRef pNew)

            mph <- putBlock mpwStorage (serialise newPolicy)

            for_ mph $ \ph -> do
              let insActually = HM.insert (sppPolicyRef pNew) (PolicyDownload now pNew (HashRef ph))
              atomically $ modifyTVar inPolicyDownloadQ insActually

          let v = Just $ max p0 (sppPolicyVersion pNew)

          downloadStatus v

  mailboxGetStatus MailboxProtoWorker{..} ref = do
    -- TODO: support-policy-ASAP

    now <- liftIO $ getPOSIXTime <&> round

    flip runContT pure do

      mdbe <- readTVarIO mailboxDB

      dbe <- ContT $ maybe1 mdbe (pure $ Left (MailboxCreateFailed "database not ready"))

      t' <- getMailboxType_ dbe ref

      t <- ContT $ maybe1 t' (pure $ Right Nothing)

      v <- getRef mpwStorage ref <&> fmap HashRef

      spp <- loadPolicyPayloadFor dbe mpwStorage ref
               <&> fmap snd

      pure $ Right $ Just $ MailBoxStatusPayload @s now (coerce ref) t v spp

  mailboxFetch MailboxProtoWorker{..} ref = do
    debug $ red "mailboxFetch" <+> pretty ref
    atomically (modifyTVar mpwFetchQ (HS.insert ref))
    okay ()

startDownloadStuff :: forall s e m . (ForMailbox s, s ~ Encryption e, MyPeer e, MonadIO m)
              => MailboxProtoWorker s e
              -> HashRef
              -> m ()

startDownloadStuff MailboxProtoWorker{..} href = do
  liftIO $ withPeerM mpwPeerEnv
    $ do
      debug $ "startDownloadStuff" <+> pretty href
      addDownload @e Nothing (coerce href)

listMailboxes :: forall s m . (ForMailbox s, MonadIO m)
              => DBPipeEnv
              -> m [(MailboxRefKey s, MailboxType)]
listMailboxes dbe = do
  withDB dbe do
   select_ [qc|select recipient,type from mailbox|]

loadPolicyPayloadFor :: forall s m . (ForMailbox s, MonadIO m)
              => DBPipeEnv
              -> AnyStorage
              -> MailboxRefKey s
              -> m (Maybe (HashRef, SignedBox (SetPolicyPayload s) s))
loadPolicyPayloadFor dbe sto who = do
  phash <- policyHashFor dbe who

  runMaybeT do
     ha <- toMPlus phash
     what <- getBlock sto (coerce ha)
                >>= toMPlus
                <&> deserialiseOrFail
                >>= toMPlus
     pure (ha, what)

-- | The block a mailbox's policy row points at, if it has one.
--
-- The row on its own, without reading what it names. That is the distinction
-- 'mailboxGetPolicyMay' turns on: a policy whose content cannot be read is still
-- a policy the owner wrote, and only the absence of the ROW means nothing was
-- ever said.
policyHashFor :: forall s m . (ForMailbox s, MonadIO m)
              => DBPipeEnv
              -> MailboxRefKey s
              -> m (Maybe HashRef)
policyHashFor dbe who = withDB dbe do
  select @(Only PolicyHash) [qc|select hash from policy where mailbox = ? limit 1|] (Only who)
    <&> fmap (coerce @_ @HashRef . fromOnly)
    <&> headMay


loadPolicyPayloadUnboxed :: forall s m . (ForMailbox s, MonadIO m)
              => DBPipeEnv
              -> AnyStorage
              -> MailboxRefKey s
              -> m (Maybe (SetPolicyPayload s))
loadPolicyPayloadUnboxed dbe sto mbox = do
  loadPolicyPayloadFor dbe sto mbox
   <&> fmap snd
   <&> fmap unboxSignedBox0
   <&> join
   <&> fmap snd

-- | Read and parse the policy a known block names.
--
-- Takes the hash rather than looking it up, because every caller has it
-- already: 'mailboxGetPolicyMay' needs the row anyway, to tell "no policy" from
-- "a policy we cannot read", and it sits on the message ingest path, once per
-- message per recipient. The version that did its own lookup meant two queries
-- for one answer on that path.
--
-- Every failure below lands on 'defaultBasicPolicy', which is deny/deny. That is
-- deliberate and it is the closed direction: an unreadable block, a merkle tree
-- that will not read, or a clause this build does not understand all mean the
-- owner said something we did not catch.
loadPolicyContentAt :: forall s m . (s ~ HBS2Basic, ForMailbox s, MonadIO m)
                    => AnyStorage
                    -> MailboxRefKey s
                    -> HashRef
                    -> m (BasicPolicy s)
loadPolicyContentAt sto mbox ha = do
  let def = defaultBasicPolicy @s
  fromMaybe def <$> runMaybeT do
    box <- getBlock sto (coerce ha)
             >>= toMPlus
             <&> deserialiseOrFail @(SignedBox (SetPolicyPayload s) s)
             >>= toMPlus

    SetPolicyPayload{..} <- unboxSignedBox0 box & toMPlus <&> snd

    lbs' <- runExceptT (readFromMerkle sto (SimpleKey (coerce sppPolicyRef)))

    when (isLeft lbs') do
      warn $ yellow "can't read policy for" <+> pretty mbox

    syn' <- toMPlus lbs'
              <&> LBS8.unpack
              <&> parseTop

    when (isLeft syn') do
      warn $ yellow "can't parse policy for" <+> pretty mbox

    syn <- toMPlus syn'

    liftIO (parseBasicPolicy  syn) >>= toMPlus

getMailboxType_ :: (ForMailbox s, MonadIO m) => DBPipeEnv -> MailboxRefKey s -> m (Maybe MailboxType)
getMailboxType_ d r = do
  let sql = [qc|select type from mailbox where recipient = ? limit 1|]
  withDB d do
   select @(Only String) sql (Only r)
     <&> fmap (fromStringMay @MailboxType  . fromOnly)
     <&> headMay . catMaybes

mailboxProtoWorkerSetProbe :: forall s e m . ( MonadIO m
                                           , s ~ Encryption e
                                           , ForMailbox s
                                           )
                           => MailboxProtoWorker s e
                           -> AnyProbe
                           -> m ()
mailboxProtoWorkerSetProbe MailboxProtoWorker{..} p
  = atomically  $ writeTVar probe p


createMailboxProtoWorker :: forall s e m . ( MonadIO m
                                           , s ~ Encryption e
                                           , ForMailbox s
                                           )
                         => PeerCredentials s
                         -> PeerEnv e
                         -> AnyStorage
                         -> m (MailboxProtoWorker s e)
createMailboxProtoWorker pc pe sto = do
  MailboxProtoWorker pe sto pc
    <$> newTVarIO mempty            -- mpwFetchQ
    <*> newCheckNonces
    <*> newRelayed
    <*> newPolicyCache
    <*> newTVarIO 0                 -- mpwPoWFloor
    <*> newTVarIO mempty            -- mpwReplicateFrom
    <*> newTVarIO mempty            -- inMessageQueueSeen
    <*> newTVarIO mempty            -- inMessageQueuePeers
    <*> newTVarIO mempty            -- mpwPoWKnown
    -- The depth is 'inQueueDepth' and not a literal: the per-peer share is a
    -- fraction of it, and a bound written against a number somewhere else is
    -- a bound that drifts.
    <*> newTBQueueIO (fromIntegral inQueueDepth)
    <*> newTVarIO mempty            -- inMessageMergeQueue
    <*> newTVarIO mempty            -- inPolicyDownloadQ
    <*> newTVarIO mempty            -- inMailboxDownloadQ
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO Nothing
    <*> newTVarIO (AnyProbe ())

mailboxProtoWorker :: forall e s m . ( MonadIO m
                                     , MonadUnliftIO m
                                     , MyPeer e
                                     , HasStorage m
                                     , Sessions e (KnownPeer e) m
                                     , HasGossip e (MailBoxProto s e) m
                                     , Signatures s
                                     , s ~ Encryption e
                                     , IsRefPubKey s
                                     , ForMailbox s
                                     , m ~ PeerM e IO
                                     , e ~ L4Proto
                                     )
             => m [Syntax C]
             -> MailboxProtoWorker s e
             -> m ()

mailboxProtoWorker readConf me@MailboxProtoWorker{..} = do

  pause @'Seconds 1

  flip runContT pure do

    dbe <- lift $ mailboxStateEvolve readConf me

    -- Порог берётся один раз, на старте, как и всё остальное из конфига здесь.
    lift do
      floorD <- readConf <&> poWFloorFrom
      atomically $ writeTVar mpwPoWFloor floorD
      when (floorD > 0) do
        debug $ "mailbox: will not forward messages under"
                  <+> pretty floorD <+> "bits of work"

      hosts <- readConf <&> replicateFromIn @s
      atomically $ writeTVar mpwReplicateFrom hosts
      -- Said out loud on both sides, because the silent half is the one that
      -- surprises: a mailbox that charges work and names no co-hosts simply
      -- stops taking replication, which looks like a mailbox that will not sync.
      if HS.null hosts
        then debug "mailbox: no replication peers configured; a mailbox with (pow D) will take none"
        else debug $ "mailbox: replicating a charging mailbox with"
                       <+> pretty (HS.size hosts) <+> "peer(s)"

    dpipe <- ContT $ withAsync (runPipe dbe)

    inq <- ContT $ withAsync (mailboxInQ dbe)

    mergeQ <- ContT $ withAsync (mailboxMergeQ dbe)

    mCheckQ <- ContT $ withAsync (mailboxCheckQ dbe)

    mFetchQ <- ContT $ withAsync (mailboxFetchQ dbe)

    pDownQ <- ContT $ withAsync (policyDownloadQ dbe)

    sDownQ <- ContT $ withAsync (stateDownloadQ dbe)

    bs <- ContT $ withAsync do

      forever do
        pause @'Seconds 10

        pro <- readTVarIO probe

        values <- atomically do
          mpwFetchQSize <- readTVar mpwFetchQ <&> HS.size
          inMessageMergeQueueSize <- readTVar inMessageMergeQueue <&> HM.size
          inPolicyDownloadQSize <- readTVar inPolicyDownloadQ <&> HM.size
          inMailboxDownloadQSize <- readTVar inMailboxDownloadQ <&> HM.size
          -- Дропы тоже. Счётчик существовал и не читался ничем: четыре размера
          -- очередей отчитывались, а единственное число, которое означает
          -- потерянное сообщение, -- нет.
          dropped <- readTVar inMessageQueueDropped
          pure $ [ ("mpwFetchQ", fromIntegral mpwFetchQSize)
                 , ("inMessageMergeQueue", fromIntegral inMessageMergeQueueSize)
                 , ("inPolicyDownloadQ", fromIntegral inPolicyDownloadQSize)
                 , ("inMailboxDownloadQ", fromIntegral inMailboxDownloadQSize)
                 , ("inMessageQueueDropped", fromIntegral dropped)
                 ]
        acceptReport pro values
        debug $ "I'm" <+> yellow "mailboxProtoWorker"

    void $ waitAnyCancel [bs,dpipe,inq,mergeQ,pDownQ,sDownQ,mCheckQ,mFetchQ]

    `catch` \( e :: MailboxProtoException ) -> do
      err $ red "mailbox protocol worker terminated" <+> viaShow e

    `finally` do
      warn $ yellow "mailbox protocol worker exited"

  where

    mailboxInQ dbe = do
      let sto = mpwStorage
      forever do
        pause @'Seconds 10
        mess <- atomically do
          -- Cleared WITH the batch, in one transaction: a message that arrives
          -- between the flush and the clear would otherwise be forgotten by the
          -- set and still be in nobody's queue.
          xs <- STM.flushTBQueue inMessageQueue
          writeTVar inMessageQueueSeen mempty
          -- With the batch, like the set: both are facts about what is IN the
          -- queue, and a share held against an empty queue is a peer rationed
          -- for something it no longer occupies.
          writeTVar inMessageQueuePeers mempty
          -- AND THE DEPTH, here rather than one message at a time below.
          --
          -- The queue is empty after the flush -- this is one transaction, so
          -- nothing ran between them -- and the counter says how full it is, so
          -- zero is not an optimisation but the value. It used to be decremented
          -- per message inside the loop, outside any bracket, while `admitTo`
          -- gates on it: one exception anywhere in the processing (a busy
          -- SQLite, a storage error) lost the decrements for the whole rest of
          -- the batch. The worker restarts and this TVar does not -- it is
          -- created once, in `createMailboxProtoWorker` -- so the inflation
          -- accumulated, and at `inQueueDepth` the door answered QueueFull
          -- forever, with an empty queue and nothing left to decrement.
          --
          -- Writing the truth rather than adjusting towards it also repairs a
          -- counter that has already drifted, which is the property an
          -- increment can never have.
          writeTVar inMessageQueueInNum 0
          pure xs
        for_ mess $ \(peer, origin, m, s) -> do

          -- TODO: process-with-policy

          for_ (messageRecipients s) $ \rcpt -> void $ runMaybeT do

            let theMailbox  = MailboxRefKey @s rcpt

            mbox <- getMailboxType_ @s dbe theMailbox
                       >>= toMPlus

            -- Уже в этом ящике -- дальше делать нечего.
            --
            -- Дешёвый ранний выход, и он то, что делает повторный приём
            -- доступным по цене: отметка о пересылке больше не гейтит приём
            -- (см. ветку SendMessage в HBS2.Peer.Proto.Mailbox), поэтому одно и
            -- то же сообщение приходит сюда при каждой повторной рассылке. Дорогое
            -- ниже -- проверка подписи на каждого получателя и чтение с разбором
            -- policy без всякого кэша -- платится теперь только за сообщение,
            -- которого в ящике ещё нет.
            --
            -- Хеш записи -- функция одного лишь сообщения, поэтому его можно
            -- посчитать, ничего не сохраняя. Сообщение, отвергнутое политикой,
            -- не сохранялось, значит и отметки о нём нет: оно пройдёт дальше и
            -- получит ещё одну попытку, когда у ящика появится политика. Ровно
            -- то, ради чего приём и был отвязан от маркера.
            --
            -- Спрашивается СТРОКА В БАЗЕ ящика, а не наличие блока. Раньше это
            -- было наличие блока по хешу, который считается из публичных
            -- величин, то есть чужой мог его подсадить и заткнуть выбранное
            -- письмо навсегда. Почему так делать нельзя вообще -- в заголовке
            -- "MailboxMerged".
            let ha0 = HashRef (hashObject (serialise m))

            merged <- isMerged dbe theMailbox (existsEntryHash ha0)

            when merged do
              debug $ "mailbox: already merged, skip" <+> pretty theMailbox <+> pretty ha0
              mzero

            -- FIXME: excess-sign-check
            (sender, _) <- unboxSignedBox0 (messageContent m) & toMPlus

            po <- mailboxGetPolicy @s me theMailbox

            acceptPeer <- maybe1 peer (pure True) $ \p ->
                             policyAcceptPeer @s po p

            unless acceptPeer do
              warn $ red "message dropped by peer policy"
                      <+> pretty mbox <+> pretty (fmap AsBase58 peer)
              mzero

            accept <- policyAcceptMessage @s po sender s

            unless accept do
              warn $ red "message dropped by policy for" <+> pretty theMailbox
              mzero

            -- Доказательство работы, и это тот слой, который ограничивает ДИСК.
            --
            -- Пир в обработчике протокола проверил только свой собственный
            -- порог -- сколько работы он готов пересылать; сколько её требует
            -- ЭТОТ ящик, знает только его подписанная policy, а она читается
            -- здесь. Ноль -- это ящик, который ничего не требует, то есть всё,
            -- что было до PEP-21, и тогда штамп просто не нужен.
            --
            -- Проверяется против rcpt, а не против ящика из штампа: работа,
            -- решённая для другого ящика, к этому отношения не имеет.
            --
            -- И только для того, что пришло госсипом: у сообщения, вынутого из
            -- чужого дерева, штампа нет и быть не может (см. 'MessageOrigin').
            powD <- policyPoW @s po

            -- Remembered for the door. The check there runs on this and cannot
            -- read a policy itself; writing it here costs nothing, since the
            -- policy has just been read and parsed anyway.
            atomically $ modifyTVar mpwPoWKnown (HM.insert rcpt powD)

            case origin of
              -- РЕПЛИКАЦИЯ ПЛАТИТ ДОВЕРИЕМ, раз не может заплатить работой.
              --
              -- Штампа в дереве нет, значит со-хосту его взять неоткуда, и
              -- требовать работу тут -- это ящик с (pow D), который перестал
              -- реплицироваться. Раньше отсюда следовало «не проверять вовсе», а
              -- границей назначался policyAcceptPeer. Границей он быть не может:
              -- та же клауза отвечает на вопрос «кто ретранслирует», и открытый
              -- inbox обязан сказать в ней allow all, иначе письмо незнакомца до
              -- него не дойдёт. После чего любой пир объявляет статус с деревом,
              -- которое сам и придумал, и всё его содержимое ложится на диск с
              -- пропущенной проверкой работы -- ровно на той конфигурации, ради
              -- которой (pow D) и вводился.
              --
              -- Так что вопрос задан отдельно и локально: чей диск, тот и решает,
              -- с кем реплицируется. Пусто по умолчанию, то есть платящий ящик не
              -- берёт репликацию, пока оператор не назовёт со-хостов; ящик,
              -- который ничего не просит, живёт как жил.
              Replicated | powD > 0 -> do
                hosts <- mailboxReplicateFrom @s me
                let ok = maybe False (`HS.member` hosts) peer
                unless ok do
                  warn $ red "message dropped: replication from a peer this one does not replicate with"
                          <+> pretty theMailbox
                          <+> parens (pretty (fmap AsBase58 peer))
                  mzero
              Replicated      -> pure ()
              Submitted stamp -> when (powD > 0) do
                let ok = maybe False (stampOk powD rcpt m) stamp
                unless ok do
                  warn $ red "message dropped for want of proof-of-work"
                          <+> pretty theMailbox
                          <+> parens ("wanted" <+> pretty powD <+> "bits, got"
                                        <+> pretty (maybe 0 (stampBits m) stamp))
                  mzero

            -- TODO: ASAP-block-accounting
            ha' <- putBlock sto (serialise m) <&> fmap HashRef

            ha <- case ha' of
                    Just x -> pure x
                    Nothing -> do
                      err $ red "storage error, can't store message"
                      mzero

            debug $ yellow "mailbox: message stored" <+> pretty theMailbox <+> pretty ha

            -- TODO: add-policy-reference
            h' <- enqueueBlock sto (serialise (existsEntry ha))

            for_ h' $ \h -> do
              atomically do
                modifyTVar inMessageMergeQueue  (HM.insertWith (<>) theMailbox (HS.singleton (HashRef h)))

            -- TODO: check-attachment-policy-for-mailbox

            -- TODO: ASAP-block-accounting-for-attachment
            for_ (messageParts s) (startDownloadStuff me)
            either (startDownloadStuff me) dontHandle (messageGK0 s)


    mailboxMergeQ dbe = do
      let sto = mpwStorage
      -- FIXME: poll-timeout-hardcode?
      let mboxes = readTVarIO inMessageMergeQueue
                    <&> fmap (,2) . HM.keys . HM.filter ( not . HS.null )

      polling (Polling 2 5) mboxes $ \r -> void $ runMaybeT do
        debug $ yellow "mailbox: merge-poll" <+> pretty r

        -- NOTE: reliability
        --   в случае отказа сторейджа все эти сообщения будут потеряны
        --   однако, ввиду дублирования -- они рано или поздно будут
        --   восстановлены с других реплик, если таковые имеются.
        --
        --   Кроме того, мы можем писать WAL.
        --
        newTx <- atomically do
                   n <- readTVar inMessageMergeQueue
                            <&>  fromMaybe mempty . HM.lookup r
                   modifyTVar inMessageMergeQueue  (HM.delete r)
                   pure n

        wipTx <- newTVarIO HS.empty

        newTxProvenL <- S.toList_ $
          for_ newTx $ \th -> void $ runMaybeT do

            tx <- getBlock sto (coerce th)
                    >>= toMPlus

            case deserialiseOrFail tx of

              Left{} -> do
                -- here, but lame
                err $ red "mailbox (invalid block)"
                markMerged dbe r th

              -- maybe to something more sophisticated
              Right (Exists{}) -> lift $ S.yield th

              -- The entry's TARGET is bound now. It used to be matched as a `_`
              -- here, and the payload's own predicate was never read on this
              -- path at all, so the two hashes that have to agree were the two
              -- values thrown away: the check established that the proof was a
              -- delete box signed for this mailbox, and not that it authorised
              -- deleting this message. Every delete box the owner ever issued is
              -- public, so one of them worked as a proof for anything else in
              -- the same mailbox. Issue #15.
              Right (Deleted (ProofOfDelete{..}) what) -> case deleteMessage of

                -- Names no proof at all. There is nothing to fetch and nothing
                -- that will change, so this is a refusal rather than an entry
                -- retried on every poll for as long as the tree holds it.
                Nothing ->
                  warn $ red "mailbox: refusing a delete entry" <+> pretty th
                           <+> "for" <+> pretty r <> ":"
                           <+> "it names no proof at all"

                Just h -> do
                  mbox <- getBlock sto (coerce h)

                  -- The one outcome here that is NOT a judgement: the proof may
                  -- simply not have arrived yet. Left outside 'admitDeleted' on
                  -- purpose, so that a proof still in flight is retried on a
                  -- later poll instead of being refused as unsigned.
                  when (isNothing mbox) do
                    startDownloadStuff me h
                    warn $ red "<<~~~>>" <+> "Proof not found!" <+> pretty h
                    -- И обратно в очередь. Заголовок HBS2.Peer.Proto.Mailbox.Merge
                    -- прямо говорит, что этот случай оставлен воркеру, "so that a
                    -- proof still in flight is retried rather than refused" -- а
                    -- воркер его отбрасывал: весь набор уходит из очереди одной
                    -- транзакцией ДО разбора, запись без доказательства обрывает
                    -- свой runMaybeT на toMPlus ниже, и вернуть её было некому.
                    -- Восстанавливалось это только случайно, когда более поздний
                    -- статус приводил к повторному обходу дерева, всё ещё
                    -- содержащего запись.
                    --
                    -- Растёт это не бесконечно: очередь -- множество по хешу
                    -- записи, а записи приходят только из ящиков, которые мы сами
                    -- держим (см. проверки в mailboxAcceptDelete и
                    -- mailboxAcceptStatus).
                    atomically $ modifyTVar inMessageMergeQueue (enqueueMerge r th)

                  bs <- toMPlus mbox

                  case admitDeleted r what bs of
                    MergeAccept -> do
                      debug $ red "<<***>> mailbox:" <+> "PROVEN message deleting" <+> pretty h
                      lift $ S.yield th

                    -- Said out loud. A failing guard inside runMaybeT dropped
                    -- the entry in silence, so a poisoning attempt was not
                    -- merely ineffective, it was invisible.
                    bad ->
                      warn $ red "mailbox: refusing a delete entry" <+> pretty th
                               <+> "for" <+> pretty r <> ":" <+> pretty bad

        let newTxProven = HS.fromList newTxProvenL

        v <- getRef sto r <&> fmap HashRef
        txs <- maybe1 v (pure mempty) (readLog (liftIO . getBlock sto) )

        let existing = HS.fromList txs
            fresh    = newTxProven `HS.difference` existing

        -- Перестройка только когда набор записей действительно изменился.
        --
        -- Ниже читается весь лог ящика и makeMerkle собирает дерево заново, то
        -- есть цена опроса линейна по размеру ящика. Когда всё пришедшее уже в
        -- дереве -- а повторно присланная запись выглядит именно так -- работа
        -- целиком лишняя, и updateRef вдобавок переписывает ref тем же
        -- значением.
        if HS.null fresh then
          debug $ yellow "mailbox unchanged" <+> pretty r

        else do
          -- Отсортировано, и это не косметика. toPTree нарезает список по его
          -- порядку, так что корень был функцией порядка обхода HashSet, а не
          -- самого набора записей. А корень -- это отпечаток, по которому пиры
          -- решают, синхронны они или нет (сравнение s0 в mailboxAcceptStatus):
          -- два пира с одинаковым набором, но разным порядком, качали бы друг у
          -- друга бесконечно. Порядок HAMT для набора без коллизий на практике
          -- устойчив, но это свойство реализации контейнера, а не обещание --
          -- и без него «набор не изменился» не означает «корень не изменится».
          let mergedTx = L.sort (HS.toList (HS.union existing newTxProven))

          -- FIXME: size-hardcode-again
          let pt = toPTree (MaxSize 6000) (MaxNum 1024) mergedTx
          nref <- makeMerkle 0 pt $ \(_,_,bss) -> void $ liftIO $ putBlock sto bss

          updateRef sto r nref
          debug $ yellow "mailbox updated" <+> pretty r <+> pretty nref

        -- Отметки пишутся ПОСЛЕ обновления ref и в обеих ветках. После -- потому
        -- что отметка, пережившая падение до updateRef, объявила бы влитым то,
        -- чего в дереве нет. В обеих -- потому что в ветке без изменений записи
        -- в дереве уже лежат, и отметка там правду и говорит; без неё запись
        -- возвращалась бы сюда на каждом опросе.
        for_ newTxProven $ \t -> markMerged dbe r t

    policyDownloadQ dbe = do

      -- FIXME: too-often-checks-affect-performance
      --   $class: performance
      let policies = readTVarIO inPolicyDownloadQ
                        <&> HM.toList
                        <&> fmap (,1)

      polling (Polling 10 10) policies $ \(pk,PolicyDownload{..}) -> do
        now <- liftIO $ getPOSIXTime <&> round

        expired <- pure (clockSkew now policyDownloadWhen > policyDownTTL)

        when expired do
          warn $ red "mailbox: giving up on a policy download" <+> pretty pk
                   <+> parens ("after" <+> pretty policyDownTTL <+> "s")
          atomically $ modifyTVar inPolicyDownloadQ (HM.delete pk)

        done <- if expired then pure False else findMissedBlocks mpwStorage pk <&> L.null

        when done $ flip runContT pure do

          let mbox = MailboxRefKey (sppMailboxKey policyDownloadWhat)

          current <- loadPolicyPayloadUnboxed @s dbe mpwStorage mbox
                       <&> fmap sppPolicyVersion
                       <&> fromMaybe 0

          let downloaded = sppPolicyVersion policyDownloadWhat

          mlbs <- getBlock mpwStorage (coerce policyDownloadBox)

          lbs  <- ContT $ maybe1 mlbs (err $ red "storage fail: missed block" <+> pretty pk)

          let msp = deserialiseOrFail @(SignedBox (SetPolicyPayload s) s) lbs
                     & either (const Nothing) Just

          spb <- ContT $ maybe1 msp (err $ red "storage fail: corrupted block" <+> pretty pk)

          when (downloaded > current) do
            void $ mailboxSetPolicy  me spb

          atomically $ modifyTVar inPolicyDownloadQ (HM.delete pk)

    stateDownloadQ dbe = do

      let mail = readTVarIO inMailboxDownloadQ
                        <&> HM.toList
                        <&> fmap (,10)

      polling (Polling 2 2) mail $ \(pk, down@MailboxDownload{..}) -> do
        now <- liftIO $ getPOSIXTime <&> round

        expired <- pure (clockSkew now mailboxDownWhen > mailboxDownTTL)

        when expired do
          warn $ red "mailbox: giving up on a state download" <+> pretty pk
                   <+> parens ("after" <+> pretty mailboxDownTTL <+> "s")
          atomically $ modifyTVar inMailboxDownloadQ (HM.delete pk)

        done <- if expired then pure False
                           else findMissedBlocks mpwStorage mailboxStatusRef <&> L.null

        fails <- newTVarIO 0

        when (done && not mailboxDownDone) do
          atomically $ modifyTVar inMailboxDownloadQ (HM.insert pk (down { mailboxDownDone = True }))
          debug $ "mailbox state downloaded" <+> pretty pk

        when done do
          debug $ "mailbox/debug: drop state" <+> pretty pk <+> pretty mailboxStatusRef

          -- FIXME: assume-huge-mailboxes

          -- walkMerkleUnique, because mailboxStatusRef is a root ANOTHER PEER
          -- chose. A plain walk follows every edge, so a chain of nodes each
          -- naming its one child twice costs 2^depth for depth+1 blocks: about a
          -- kilobyte of well-formed blocks, served the ordinary way, is nine
          -- seconds at depth 22 and does not finish at depth 40. The question
          -- here is which entries the tree mentions, which is a set, so entering
          -- a node once is the right answer as well as the affordable one; the
          -- work below is idempotent per entry anyway.
          walkMerkleUnique @[HashRef] (coerce mailboxStatusRef) (getBlock mpwStorage) $ \case
            Left what -> do
              err $ red "mailbox: missed block for tree" <+> pretty mailboxStatusRef <+> pretty what
              atomically $ modifyTVar fails succ

            Right hs  ->  do
              for_ hs $ \h -> void $ runMaybeT do
                debug $ red ">>>" <+> "MERGE MAILBOX ENTRY" <+> pretty h

                -- Строка в базе, а не блок в хранилище. Здесь это стоило
                -- дороже всего: подсаженный блок затыкал и репликацию с
                -- честного со-хоста, то есть последний путь, по которому
                -- заткнутое сообщение могло доехать. См. "MailboxMerged".
                already <- isMerged dbe mailboxRef h

                when already do
                  debug $ red "!!!" <+> "skip already merged tx" <+> pretty h
                  mzero

                entry' <- getBlock mpwStorage (coerce h)

                when (isNothing entry') do
                  startDownloadStuff me h
                  atomically $ modifyTVar fails succ
                  mzero

                entry <- toMPlus entry'
                           <&> deserialiseOrFail @MailboxEntry
                           >>= toMPlus

                case entry of
                  Deleted{} -> do
                    -- insertWith, not insert. This runs INSIDE the loop over the
                    -- tree's entries, and a plain insert replaces the whole set
                    -- on every iteration -- so a tree carrying N Deleted entries
                    -- contributed at most one of them, and which one depended on
                    -- how the two-second merge poll interleaved with this walk.
                    -- The loss was then permanent: 'fails' counts fetch failures
                    -- only, clobbered entries are not failures, so failNum == 0
                    -- and the download was dropped from the queue below.
                    atomically $ modifyTVar inMessageMergeQueue (enqueueMerge mailboxRef h)
                    -- write-already-merged

                  Exists _ w -> do
                    debug $ red ">>>" <+> blue "TX: Exists" <+> pretty w
                    msg' <- getBlock mpwStorage (coerce w)

                    case msg' of
                      Nothing -> do
                        debug $ red  "START DOWNLOAD" <+> pretty w
                        startDownloadStuff me w
                        atomically $ modifyTVar fails succ
                        mzero

                      Just msg -> do
                        let mess = deserialiseOrFail @(Message s) msg

                        case mess of
                          Left{} -> do
                            warn $ "malformed message" <+> pretty w
                            markMerged dbe mailboxRef h

                          Right normal -> do
                            let checked = unboxSignedBox0 (messageContent normal)

                            case checked of
                              Nothing -> do
                                warn $ "invalid signature for message" <+> pretty w
                                markMerged dbe mailboxRef h

                              Just (_, content) -> do
                                -- A full queue is a FAILURE of this download,
                                -- and it used not to be counted as one: the
                                -- entry was walked, the message was dropped on
                                -- the floor, `fails` stayed at zero, and the
                                -- tree was removed from the queue below as
                                -- complete. So a burst larger than the queue was
                                -- permanent loss with a counter to show for it.
                                -- Left in the queue, the next poll walks the
                                -- tree again and the message gets another go.
                                --
                                -- The announcing peer is passed on, rather than
                                -- mzero. `mailboxInQ` reads Nothing as "no peer
                                -- to check", which is right for a message this
                                -- node injected itself and wrong here: a peer
                                -- denied by `(peer deny <key>)` only had to
                                -- serve its messages inside a status tree
                                -- instead of sending them, and every one of them
                                -- was admitted with the peer check skipped.
                                --
                                -- 'Replicated', и это не «штампа нет», а «не
                                -- за что платить»: штамп в дереве не лежит,
                                -- значит у со-хоста его и нет. Ящик с (pow D)
                                -- иначе отверг бы всё, что ему передают
                                -- собственные хосты, то есть перестал бы
                                -- реплицироваться. Границей тут служит
                                -- policyAcceptPeer -- см. 'MessageOrigin'.
                                took <- mailboxAcceptMessage me (Just mailboxDownPeer) Replicated normal content
                                unless took do
                                  atomically $ modifyTVar fails succ

          failNum <- readTVarIO fails

          when (failNum == 0) do
            debug $ "mailbox state process succeed" <+> pretty mailboxStatusRef
            atomically $ modifyTVar inMailboxDownloadQ (HM.delete pk)

    mailboxFetchQ dbe = forever do
      toFetch <- atomically $ do
        q <- readTVar mpwFetchQ
        when (HS.null q) STM.retry
        writeTVar mpwFetchQ mempty
        pure q

      for_ toFetch $ \r -> do
        t <- getMailboxType_ dbe r
        maybe1 t none $ \_ -> do
          debug $ yellow "mailbox: SEND FETCH REQUEST FOR" <+> pretty r
          -- Нонс, а не отметка времени: то, что вернётся эхом, и есть проверка
          -- свежести ответа. См. HBS2.Peer.Proto.Mailbox.Nonce.
          nonce <- issueCheckNonce mpwCheckNonces r
          gossip (MailBoxProtoV1 @s @e (CheckMailbox  (Just nonce) (coerce r)))

    mailboxCheckQ dbe = do

      -- FIXME: mailbox-check-period
      --   ten minutes, hardcoded. The comment here said sixty seconds "for
      --   debug purposes" long after the number below stopped being sixty;
      --   checkNonceTTL is chosen against this value, so it is worth it saying
      --   what it is.
      let mboxes = liftIO (listMailboxes @s dbe <&> fmap (set _2 600) )

      polling (Polling 10 10) mboxes $ \r -> do
        debug $ yellow "mailbox: SEND FETCH REQUEST FOR" <+> pretty r
        nonce <- issueCheckNonce mpwCheckNonces r
        gossip (MailBoxProtoV1 @s @e (CheckMailbox  (Just nonce) (coerce r)))

mailboxStateEvolve :: forall e s m . ( MonadIO m
                                     , MonadUnliftIO m
                                     , HasStorage m
                                     , s ~ Encryption e
                                     )
                   => m [Syntax C]
                   -> MailboxProtoWorker s e -> m DBPipeEnv

mailboxStateEvolve readConf MailboxProtoWorker{..}  = do

  conf <- readConf

  debug $ red "mailboxStateEvolve" <> line <> pretty conf

  mailboxDir <- lastMay [ dir
                        | ListVal [StringLike o, StringLike dir] <- conf
                        , o == hbs2MailboxDirOpt
                        ]
                  & orThrow MailboxProtoMailboxDirNotSet

  r <- try @_ @SomeException (mkdir mailboxDir)

  either (const $ throwIO (MailboxProtoCantAccessMailboxes mailboxDir)) dontHandle r

  dbe <- newDBPipeEnv dbPipeOptsDef (mailboxDir </> "state.db")

  -- Таблицы создаются ДО того, как база становится видимой остальным.
  --
  -- Было наоборот, и между двумя строчками существовало окно, в котором
  -- mailboxDB уже отдаёт Just, а таблиц ещё нет. Любой CheckMailbox от любого
  -- пира в этот момент доходит до getMailboxType_ и получает
  -- "no such table: mailbox". Ловить это некому: обработчик протокола идёт через
  -- deferred, то есть addJob в _envDeferred, а runPipeline _envDeferred запущен
  -- через asyncLinked (HBS2.Actors.Peer), так что исключение уходит из
  -- пайплайна в связанный поток, а не остаётся внутри запроса.
  --
  -- Окно открывалось заново при каждом перезапуске воркера peerThread, когда
  -- сеть уже полностью жива, то есть ровно тогда, когда запрос и придёт.
  withDB dbe $ Q.transactional do
    ddl [qc|create table if not exists
             mailbox ( recipient text not null
                     , type      text not null
                     , primary key (recipient)
                     )
           |]

    ddl [qc|create table if not exists
             policy ( mailbox   text not null
                    , hash      text not null
                    , primary key (mailbox)
                    )
           |]

    mergedDDL

  atomically $ writeTVar mailboxDB (Just dbe)

  pure dbe


-- The 'MailboxRefKey' field instances live in "MailboxMerged", which is the
-- other module keying rows by one. See the note there.

instance FromField MailboxType where
  fromField w = fromField @String w <&> fromString @MailboxType


