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
                          , hbs2MailboxPoWMinOpt
                          , poWFloorFrom
                          ) where

import HBS2.Prelude.Plated
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

-- | @(hbs2:mailbox:pow-min D)@: the least proof-of-work this peer forwards.
--
-- A peer-wide floor, and it has to be peer-wide rather than per-mailbox: it is
-- consulted before gossip, where the peer does not yet know which mailbox a
-- message is for and usually hosts none of them. What a mailbox charges is
-- @(pow D)@ in its own signed policy, and that one bounds storage.
--
-- Absent means zero, which forwards a stamped message on the same terms as a
-- plain one.
hbs2MailboxPoWMinOpt :: String
hbs2MailboxPoWMinOpt = "hbs2:mailbox:pow-min"

-- | The floor as the config states it, or zero if it does not.
--
-- A pure function over the parsed config so it can be tested without a peer.
-- The last clause wins, which is how the other options here read; a value that
-- does not fit is ignored rather than clamped, because a clamped 4096 would
-- silently become a floor nobody asked for.
poWFloorFrom :: [Syntax C] -> PoWDifficulty
poWFloorFrom conf =
  fromMaybe 0 $ lastMay [ fromIntegral d
                        | ListVal [StringLike o, LitIntVal d] <- conf
                        , o == hbs2MailboxPoWMinOpt
                        , d >= 0 && d <= 255
                        ]

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
    -- | Least proof-of-work this peer will forward, from
    -- 'hbs2MailboxPoWMinOpt'. A TVar because it is read from the config after
    -- the worker exists, the way the database and the probe are.
  , mpwPoWFloor           :: TVar PoWDifficulty
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
  mailboxGetPolicyMay MailboxProtoWorker{..} mbox = runMaybeT do
    dbe <- readTVarIO mailboxDB >>= toMPlus
    ha  <- policyHashFor dbe mbox >>= toMPlus
    AnyPolicy <$> loadPolicyContentAt mpwStorage mbox ha

  mailboxCheckNonce MailboxProtoWorker{..} mbox nonce =
    acceptCheckNonce mpwCheckNonces mbox nonce

  mailboxPoWFloor MailboxProtoWorker{..} = readTVarIO mpwPoWFloor

  mailboxAcceptMessage MailboxProtoWorker{..} peer origin m c = do
    took <- atomically do
      full <- isFullTBQueue inMessageQueue
      if full then do
        modifyTVar inMessageQueueDropped succ
        pure False
      else do
        writeTBQueue inMessageQueue (peer, origin, m, c)
        modifyTVar   inMessageQueueInNum succ
        pure True

    -- Said out loud. The counter above is published on the probe now, but a
    -- drop is a message that will not arrive, and a peer's log is where somebody
    -- looks when one did not.
    unless took do
      warn $ red "mailbox: input queue full, message dropped"
               <+> pretty (HashRef (hashObject (serialise m)))

    pure took

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

      merged <- hasBlock sto (mergedMarker mbox entryH) <&> isJust

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

  mailboxAcceptStatus me@MailboxProtoWorker{..} ref who s2@MailBoxStatusPayload{..} = do

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
      -- Теперь очередь ограничена сверху числом ящиков, которые мы держим.
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

      let downloadStatus v = do
            maybe1 mbsMailboxHash (okay ()) $ \h -> do
              when (s0 /= Just h) do
                startDownloadStuff me h
                -- one download per version per hash
                let downKey = HashRef $ hashObject (serialise (v,h))
                atomically $ modifyTVar inMailboxDownloadQ
                  (HM.insert downKey (MailboxDownload ref h now v False who))
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
  -- FIXME: queue-size-hardcode
  --   $class: hardcode
  MailboxProtoWorker pe sto pc
    <$> newTVarIO mempty
    <*> newCheckNonces
    <*> newTVarIO 0
    <*> newTBQueueIO 8000
    <*> newTVarIO mempty
    <*> newTVarIO mempty
    <*> newTVarIO mempty
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

    dpipe <- ContT $ withAsync (runPipe dbe)

    inq <- ContT $ withAsync (mailboxInQ dbe)

    mergeQ <- ContT $ withAsync mailboxMergeQ

    mCheckQ <- ContT $ withAsync (mailboxCheckQ dbe)

    mFetchQ <- ContT $ withAsync (mailboxFetchQ dbe)

    pDownQ <- ContT $ withAsync (policyDownloadQ dbe)

    sDownQ <- ContT $ withAsync stateDownloadQ

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
        mess <- atomically $ STM.flushTBQueue inMessageQueue
        for_ mess $ \(peer, origin, m, s) -> do
          atomically $ modifyTVar inMessageQueueInNum pred

          -- TODO: process-with-policy

          for_ (messageRecipients s) $ \rcpt -> void $ runMaybeT do

            let theMailbox  = MailboxRefKey @s rcpt

            mbox <- getMailboxType_ @s dbe theMailbox
                       >>= toMPlus

            -- Уже в этом ящике -- дальше делать нечего.
            --
            -- Дешёвый ранний выход, и он то, что делает повторный приём
            -- доступным по цене: маркер RoutedEntry больше не гейтит приём (см.
            -- ветку SendMessage в HBS2.Peer.Proto.Mailbox), поэтому одно и то же
            -- сообщение приходит сюда при каждой повторной рассылке. Дорогое
            -- ниже -- проверка подписи на каждого получателя и чтение с разбором
            -- policy без всякого кэша -- платится теперь только за сообщение,
            -- которого в ящике ещё нет.
            --
            -- Хеш записи -- функция одного лишь сообщения, поэтому его можно
            -- посчитать, ничего не сохраняя. Сообщение, отвергнутое политикой,
            -- не сохранялось, значит и маркера у него нет: оно пройдёт дальше и
            -- получит ещё одну попытку, когда у ящика появится политика. Ровно
            -- то, ради чего приём и был отвязан от маркера.
            let ha0     = HashRef (hashObject (serialise m))
                mergedH = mergedMarker theMailbox (existsEntryHash ha0)

            merged <- hasBlock sto mergedH <&> isJust

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

            case origin of
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


    mailboxMergeQ = do
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
                void $ putBlock sto (serialise (MergedEntry r th))

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

        -- Маркеры пишутся ПОСЛЕ обновления ref и в обеих ветках. После -- потому
        -- что маркер, переживший падение до updateRef, объявил бы влитым то,
        -- чего в дереве нет. В обеих -- потому что в ветке без изменений записи
        -- в дереве уже лежат, и маркер там правду и говорит; без него запись
        -- возвращалась бы сюда на каждом опросе.
        for_ newTxProven $ \t -> do
          -- FIXME: use-bloom-filter-or-something
          --  $class: leak
          putBlock sto (serialise (MergedEntry r t))

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

    stateDownloadQ = do

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

                -- FIXME: invent-better-filter
                --  $class: leak
                let mergedEntry = serialise (MergedEntry mailboxRef h)
                let mergedH = mergedMarker mailboxRef h

                already <- getBlock mpwStorage mergedH

                when (isJust already) do
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
                            void $ putBlock mpwStorage mergedEntry

                          Right normal -> do
                            let checked = unboxSignedBox0 (messageContent normal)

                            case checked of
                              Nothing -> do
                                warn $ "invalid signature for message" <+> pretty w
                                void $ putBlock mpwStorage mergedEntry

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

  atomically $ writeTVar mailboxDB (Just dbe)

  pure dbe


instance ForMailbox s => ToField (MailboxRefKey s) where
  toField (MailboxRefKey a) = toField (show $ pretty (AsBase58 a))

instance ForMailbox s => FromField (MailboxRefKey s) where
  fromField w = fromField @String w <&> fromString @(MailboxRefKey s)

instance FromField MailboxType where
  fromField w = fromField @String w <&> fromString @MailboxType


