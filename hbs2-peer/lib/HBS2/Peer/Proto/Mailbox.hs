{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.Proto.Mailbox
  ( module HBS2.Peer.Proto.Mailbox
  , module HBS2.Peer.Proto.Mailbox.Message
  , module HBS2.Peer.Proto.Mailbox.Types
  , module HBS2.Peer.Proto.Mailbox.Ref
  ) where

import HBS2.Prelude.Plated

import HBS2.Hash
import HBS2.Base58
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Storage
import HBS2.Actors.Peer.Types
import HBS2.Data.Types.Peer
import HBS2.Net.Auth.Credentials


import HBS2.Net.Proto.Sessions
import HBS2.Peer.Proto.Peer
import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Peer.Proto.Mailbox.Message
import HBS2.Peer.Proto.Mailbox.Entry
import HBS2.Peer.Proto.Mailbox.Nonce
import HBS2.Peer.Proto.Mailbox.Policy
import HBS2.Peer.Proto.Mailbox.PoW
import HBS2.Peer.Proto.Mailbox.Ref

import HBS2.Misc.PrettyStuff
import HBS2.System.Logger.Simple

import Codec.Serialise()
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.HashSet (HashSet)
import Data.Maybe
import Data.Word
import Lens.Micro.Platform


class ForMailbox s => IsMailboxProtoAdapter s a where

  mailboxGetCredentials :: forall m . MonadIO m => a -> m (PeerCredentials s)

  mailboxGetStorage     :: forall m . MonadIO m => a -> m AnyStorage

  mailboxGetPolicy      :: forall m . MonadIO m => a -> MailboxRefKey s -> m (AnyPolicy s)

  -- | The mailbox's policy if one was ever set for it, and 'Nothing' if none
  -- was.
  --
  -- 'mailboxGetPolicy' cannot answer this: it falls back to
  -- @defaultBasicPolicy@, which is deny/deny, so "the owner denied this peer"
  -- and "nobody has written a policy yet" arrive as the same value. That is the
  -- right fallback where a message is being admitted and the wrong one for
  -- deciding whether to ANSWER a 'CheckMailbox' -- applied there it would stop
  -- every mailbox that has no policy from ever syncing.
  mailboxGetPolicyMay   :: forall m . MonadIO m => a -> MailboxRefKey s -> m (Maybe (AnyPolicy s))

  -- | Is this nonce one we issued for this mailbox, recently?
  --
  -- The requester's half of the 'CheckMailbox' challenge-response. See
  -- "HBS2.Peer.Proto.Mailbox.Nonce".
  mailboxCheckNonce     :: forall m . MonadIO m
                        => a
                        -> MailboxRefKey s
                        -> Word64 -- ^ nonce as it came back in the status
                        -> m Bool

  -- | The least proof-of-work this peer will amplify, in leading zero bits.
  --
  -- Not the mailbox's @(pow D)@, and it cannot be: this is consulted before
  -- gossip, where the peer does not yet know which mailbox the message is for
  -- and may host none of them. It bounds what this peer FORWARDS. What it
  -- stores is bounded by the mailbox's own policy, later, in the queue.
  --
  -- Zero by default, which is a peer that forwards a stamped message on the
  -- same terms as a plain one.
  mailboxPoWFloor       :: forall m . MonadIO m => a -> m PoWDifficulty
  mailboxPoWFloor _ = pure 0

  -- | Peers this one takes replication from, for a mailbox that charges work.
  --
  -- A message pulled out of somebody's status tree arrives as 'Replicated' and
  -- carries no stamp: the tree holds messages, not the work that bought them.
  -- So a charging mailbox either refuses replication or trusts the peer that
  -- announced it, and this is where that trust is named. Empty by default,
  -- which is a peer that takes none.
  --
  -- Not in the mailbox's policy, and that is the point: it is a decision about
  -- disk this peer spends, and @(peer allow|deny)@ cannot make it, being the
  -- clause an open inbox has to leave open for gossip to reach it at all.
  mailboxReplicateFrom  :: forall m . MonadIO m => a -> m (HashSet (PubKey 'Sign s))
  mailboxReplicateFrom _ = pure mempty

  mailboxAcceptMessage  :: forall m . (ForMailbox s, MonadIO m)
                        => a
                        -> Maybe (PubKey 'Sign s) -- ^ peer
                        -> MessageOrigin s        -- ^ which path it arrived on
                        -> Message s
                        -> MessageContent s
                        -- ^ True when the message was taken. It answered @()@,
                        -- and the only implementation drops on a full queue and
                        -- increments a counter nothing reads, so a burst larger
                        -- than the queue was lost with no diagnostic and no
                        -- retry -- and the download path counted the drop as a
                        -- success and removed the tree from its queue as
                        -- complete.
                        -> m Bool

  mailboxAcceptDelete   :: forall m . (ForMailbox s, MonadIO m)
                        => a
                        -> MailboxRefKey s
                        -> DeleteMessagesPayload s
                        -> SignedBox (DeleteMessagesPayload s) s -- ^ we need this for proof
                        -> m ()

class ForMailbox s => IsMailboxService s a where

  mailboxCreate :: forall m . MonadIO m
                => a
                -> MailboxType
                -> Recipient s
                -> m (Either MailboxServiceError ())

  mailboxSetPolicy :: forall m . MonadIO m
                   => a
                   -> SignedBox (SetPolicyPayload s) s
                   -> m (Either MailboxServiceError HashRef)

  mailboxDelete :: forall m . MonadIO m
                => a
                -> Recipient s
                -> m (Either MailboxServiceError ())

  mailboxSendMessage :: forall m . MonadIO m
                     => a
                     -> Maybe (MessageStamp s) -- ^ proof of work, when the mailbox charges for one
                     -> Message s
                     -> m (Either MailboxServiceError ())


  mailboxSendDelete :: forall m . MonadIO m
                    => a
                    -> SignedBox (DeleteMessagesPayload s) s
                    -> m (Either MailboxServiceError ())

  mailboxListBasic :: forall m . MonadIO m
                   => a
                   -> m (Either MailboxServiceError [(MailboxRefKey s, MailboxType)])

  mailboxGetStatus :: forall m . MonadIO m
                   => a
                   -> MailboxRefKey s
                   -> m (Either MailboxServiceError (Maybe (MailBoxStatusPayload s)))

  mailboxAcceptStatus :: forall m . MonadIO m
                      => a
                      -> MailboxRefKey s
                      -> PubKey 'Sign s -- ^ peer's key
                      -- | What this status entitles the peer to act on. A
                      -- status nobody asked for still carries a policy that
                      -- signs for itself; the tree it names does not.
                      -> StatusOrigin
                      -> MailBoxStatusPayload s
                      -> m (Either MailboxServiceError ())

  mailboxFetch :: forall m . MonadIO m
               => a
               -> MailboxRefKey s
               -> m (Either MailboxServiceError ())

data AnyMailboxService s =
  forall a  . (IsMailboxService s a) => AnyMailboxService { mailboxService :: a }

data AnyMailboxAdapter s =
  forall a . (IsMailboxProtoAdapter s a) => AnyMailboxAdapter { mailboxAdapter :: a }

instance ForMailbox s => IsMailboxService s (AnyMailboxService s) where
  mailboxCreate (AnyMailboxService a) = mailboxCreate @s a
  mailboxSetPolicy (AnyMailboxService a) = mailboxSetPolicy @s a
  mailboxDelete (AnyMailboxService a) = mailboxDelete @s a
  mailboxSendMessage (AnyMailboxService a) = mailboxSendMessage @s a
  mailboxSendDelete (AnyMailboxService a) = mailboxSendDelete @s a
  mailboxListBasic (AnyMailboxService a) = mailboxListBasic @s a
  mailboxGetStatus (AnyMailboxService a) = mailboxGetStatus @s a
  mailboxAcceptStatus (AnyMailboxService a) = mailboxAcceptStatus @s a
  mailboxFetch (AnyMailboxService a) = mailboxFetch @s a

instance ForMailbox s => IsMailboxProtoAdapter s (AnyMailboxAdapter s) where
  mailboxGetCredentials (AnyMailboxAdapter a) = mailboxGetCredentials @s a
  mailboxGetPolicy (AnyMailboxAdapter a) = mailboxGetPolicy @s a
  mailboxGetPolicyMay (AnyMailboxAdapter a) = mailboxGetPolicyMay @s a
  mailboxCheckNonce (AnyMailboxAdapter a) = mailboxCheckNonce @s a
  mailboxGetStorage (AnyMailboxAdapter a) = mailboxGetStorage @s a
  mailboxPoWFloor (AnyMailboxAdapter a) = mailboxPoWFloor @s a
  mailboxReplicateFrom (AnyMailboxAdapter a) = mailboxReplicateFrom @s a
  mailboxAcceptMessage (AnyMailboxAdapter a) = mailboxAcceptMessage @s a
  mailboxAcceptDelete (AnyMailboxAdapter a) = mailboxAcceptDelete @s a


mailboxProto :: forall e s m p a . ( MonadIO m
                                   , Response e p m
                                   , HasDeferred p e m
                                   , HasGossip e p m
                                   , IsMailboxProtoAdapter s a
                                   , IsMailboxService s a
                                   , Sessions e (KnownPeer e) m
                                   , p ~ MailBoxProto s e
                                   , s ~ Encryption e
                                   , ForMailbox s
                                   )
             => Bool -- ^ inner, i.e from own peer
             -> a
             -> MailBoxProto (Encryption e) e
             ->  m ()

mailboxProto inner adapter mess = deferred @p do
  -- common stuff

  sto  <- mailboxGetStorage @s adapter
  pc <- mailboxGetCredentials @s adapter

  now  <- liftIO $ getPOSIXTime <&> round
  that <- thatPeer @p
  se'  <- find (KnownPeerKey that) id

  flip runContT pure $ callCC \exit -> do

    pip <- if inner then do
              pure $ view peerSignPk pc
            else do
              se <- ContT $ maybe1 se' none
              pure $ view peerSignKey se

    -- Приём сообщения, со штампом или без. Одно тело на две ветки: они
    -- расходятся только в том, чем сообщение опознаётся для дедупа и что перед
    -- этим проверяется, а всё дальнейшее -- рассылка, маркер, очередь -- у них
    -- общее и обязано остаться общим.
    let takeMessageWith relay origin msg content h = do

          let routed = serialise (RoutedEntry h)
          let routedHash = hashObject routed

          seen <- hasBlock sto routedHash <&> isJust

          -- Маркер гейтит ТОЛЬКО пересылку, и это исправление.
          --
          -- Раньше он гейтил и приём тоже, а приём -- это очередь, за которой
          -- через десять секунд идёт policy. Порядок был такой: пишем маркер,
          -- потом решаем. Сообщение, отвергнутое политикой, нигде не сохранялось,
          -- очередь уже была слита, а любая повторная рассылка того же сообщения
          -- гасилась как `seen`. То есть отказ был окончательным.
          --
          -- А политика по умолчанию -- `defaultBasicPolicy`, то есть Deny/Deny, и
          -- она берётся всякий раз, когда для ящика не выставлено ничего. Значит:
          -- создать ящик, не успеть выставить политику -- и каждое письмо,
          -- пришедшее в этот промежуток, потеряно навсегда, даже после того, как
          -- политику поправят.
          --
          -- Приём стоит одну запись в ограниченную очередь; дорогое (проверка
          -- подписи на получателя, чтение и разбор policy) живёт в mailboxInQ, и
          -- там теперь есть дешёвый ранний выход для сообщения, которое в этом
          -- ящике уже лежит. Так что повтор обходится дёшево, а отказ перестал
          -- быть вечным.
          -- ПЕРЕСЫЛКА -- ОТДЕЛЬНОЕ РЕШЕНИЕ ОТ ПРИЁМА, и раньше их было одно.
          --
          -- Порог этого пира (mailboxPoWFloor) отвечает на вопрос «сколько
          -- работы я готов усиливать», и только на него: сколько требует ЯЩИК,
          -- знает лишь его подписанная policy, которой у реле нет. Проверка
          -- стояла через `exit ()`, то есть роняла сообщение целиком, и это
          -- значит, что оператор, поставивший себе порог 16, тихо терял письма
          -- к ящику, который просит 12 -- отправитель честно заплатил, обе
          -- стороны настроены верно, письмо не доходит и никто не узнаёт.
          -- Хуже того, порог применялся и к ящикам, которые этот же пир хостит:
          -- «не буду пересылать» превращалось в «не положу к себе».
          --
          -- Теперь слабый штамп означает ровно «дальше не понесу»: письмо
          -- доходит до очереди, и решает его судьбу policy ящика, у которой
          -- есть настоящее D.
          unless (seen || not relay) $ lift do
            gossip mess

            -- TODO: maybe-dont-gossip-message-if-dropped-by-policy
            --   сейчас policy проверяется для почтового ящика,
            --   а тут мы еще не знаем, какой почтовый ящик и есть
            --   ли он вообще. надо бы не рассылать, если пира
            --   не поддерживаем.
            --
            --   с другой стороны -- мы не поддерживаем, а другие,
            --   может, поддерживают.
            --
            -- ЧАСТИЧНО ЗАКРЫТО для сообщений со штампом: у них есть, что
            -- проверить до рассылки, не зная ящика, и это mailboxPoWFloor.

            -- TODO: expire-block-and-collect-garbage
            --   $class: leak
            void $ putBlock sto routed

          lift do
            let whoever = if inner then Nothing else Just pip
            void $ mailboxAcceptMessage adapter whoever origin msg content

    -- Подпись дешевле диска, поэтому она первой в обеих ветках.
    let unboxMessage msg = ContT $ maybe1 (unboxSignedBox0 @(MessageContent s) (messageContent msg)) none

    case mailBoxProtoPayload mess of
      SendMessage msg -> do

        debug $ red "AAAAAA!" <+> pretty now

        -- ок, сообщение нормальное, шлём госсип, пишем, что обработали
        -- TODO: increment-malformed-messages-statistics
        --   $workflow: backlog
        (_, content) <- unboxMessage msg

        takeMessageWith True (Submitted Nothing) msg content
          (hashObject @HbSync (serialise mess) & HashRef)

      -- То же сообщение, но с доказательством работы (PEP-21).
      --
      -- Проверка СТОИТ ДО РАССЫЛКИ И ДО МАРКЕРА, и оба порядка существенны.
      -- До рассылки -- потому что иначе штамп ограничивает только диск, а
      -- флуд уже ушёл ко всем известным пирам. До маркера -- потому что
      -- маркер, записанный для сообщения с негодным штампом, погасил бы
      -- годную копию того же сообщения везде, куда она ещё не дошла.
      SendMessageStamped msg stamp -> do

        (_, content) <- unboxMessage msg

        floorD <- lift $ mailboxPoWFloor @s adapter

        let bits = stampBits msg stamp

        -- Ящик, названный в штампе, должен быть среди получателей: работа,
        -- решённая для чужого ящика, -- это работа за чужую доставку. И это
        -- тоже вопрос про УСИЛЕНИЕ, а не про приём: негодный штамп -- это
        -- отсутствие штампа, а сообщение без штампа этот пир принимает и кладёт
        -- в очередь, где его судьбу решает policy ящика.
        let named = stampNames content stamp

        unless named do
          debug $ red "mailbox: stamp names a mailbox this message is not addressed to"
                    <+> pretty (AsBase58 (msMailbox stamp))

        unless (bits >= fromIntegral floorD) do
          debug $ red "mailbox: stamp too weak to forward"
                    <+> pretty bits <> ", wanted" <+> pretty floorD

        takeMessageWith (named && bits >= fromIntegral floorD)
                        (Submitted (Just stamp)) msg content (stampMarker stamp msg)

      -- NOTE: CheckMailbox-auth
      --   поскольку пир не владеет приватными ключами,
      --   то и подписать это сообщение он не может.
      --
      --   В таком случае, и в фоновом режиме нельзя будет
      --   синхронизировать ящики.
      --
      --   Поскольку все сообщения зашифрованы (но не их метаданные!)
      --   статус мейлобокса является открытой в принципе информацией.
      --
      --   Теперь у нас два пути:
      --    1. Отдавать только авторизованными пирам (которые имеют майлобоксы)
      --       для этого сделаем сообщение CheckMailboxAuth{}
      --
      --    2. Шифровать дерево с метаданными, так как нам в принципе
      --       может быть известен публичный ключ шифрования автора,
      --       но это сопряжено со сложностями с обновлением ключей.
      --
      --    С другой стороны, если нас не очень беспокоит возможное раскрытие
      --    метаданных --- то тот, кто скачает мейлобокс для анализа --- будет
      --    участвовать в раздаче.
      --
      --    С другой стороны, может он и хочет участвовать в раздаче, что бы каким-то
      --    образом ей вредить или устраивать слежку.
      --
      --    С этим всем можно бороться поведением и policy:
      --
      --    например:
      --      - не отдавать сообщения неизвестным пирам
      --      - требовать авторизацию (CheckMailboxAuth не нужен т.к. пир авторизован
      --        и так и известен в протоколе)
      --
      -- РЕШЕНО (2026-08-02): взят путь из последнего абзаца выше -- того, где
      -- сказано, что CheckMailboxAuth не нужен, потому что пир и так
      -- авторизован и известен протоколу. Ответ гейтится policy пира, нового
      -- сообщения не заводится. Нумерованный пункт 1, в отличие от этого,
      -- предлагал завести CheckMailboxAuth{}; он НЕ взят.
      --
      -- Метаданные НЕ шифруются, и позиция такая: статус ящика -- публичная в
      -- принципе информация, раз тела писем зашифрованы; владелец, которого это
      -- не устраивает, пишет policy. Подробности и разбор совместимости --
      -- docs/drafts/checkmailbox-auth-context.md.

      CheckMailbox nonce k ->  do

        debug $ red "mailbox:" <+> "CheckMailbox"

        creds <- mailboxGetCredentials @s adapter

        void $ runMaybeT do

          let mbox = MailboxRefKey @s k

          -- Статус СНАЧАЛА, policy потом, и порядок тут не косметический.
          --
          -- mailboxGetStatus начинается с getMailboxType_, то есть с одного
          -- индексного select, и на чужой ящик выходит сразу. А чтение policy --
          -- это второй select, getBlock, проверка подписи, обход merkle-дерева,
          -- разбор текста, и всё это без кэша. Гейт, стоящий первым, продавал бы
          -- любому хендшейкнутому пиру всю эту работу за пакет в сорок байт с
          -- любым ключом в поле, причём на общем пуле deferred, где живут и
          -- остальные протоколы.
          s <- mailboxGetStatus adapter mbox
                 >>= toMPlus
                 >>= toMPlus

          -- Кому отвечаем. Раньше -- любому, кто прошёл хендшейк.
          --
          -- Что это закрывает: корень дерева ящика (отпечаток синхронизации) и
          -- то, как он движется по мере прихода писем. Чего НЕ закрывает: сам
          -- факт, что мы держим этот ящик -- его mailboxCheckQ каждые десять
          -- минут сам рассылает госсипом всем известным пирам, включая тех,
          -- кому policy говорит deny.
          --
          -- Ящик БЕЗ policy отвечает всем, как и раньше. Наивное
          -- `mailboxGetPolicy` тут было бы регрессией, а не защитой: оно падает на
          -- defaultBasicPolicy, то есть deny/deny, и остановило бы синхронизацию
          -- каждому ящику, которому политику не выставляли -- это ровно та
          -- ловушка, в которую уже попадал путь приёма сообщений.
          --
          -- А если policy записана, но её содержимое не читается или не
          -- разбирается (блок потерян, клауза от более новой сборки),
          -- mailboxGetPolicyMay вернёт Just deny/deny, и мы не ответим. Владелец
          -- что-то сказал, мы не расслышали -- это не повод считать, что он не
          -- говорил ничего. Заметьте: пока дерево policy ЕЩЁ КАЧАЕТСЯ, строки в
          -- таблице нет вовсе (её пишет mailboxSetPolicy, а policyDownloadQ
          -- зовёт его уже после того, как всё скачано), так что это не тот
          -- случай, и ящик всё это время отвечает всем.
          --
          -- TODO: cache-parsed-policy
          --   $class: performance
          --   чтение и разбор policy повторяются на каждый входящий CheckMailbox
          --   и на каждое (сообщение, получатель) в mailboxInQ. Кэш по
          --   (ящик, хеш policy) убрал бы и то и другое.
          unless inner do
            po <- mailboxGetPolicyMay @s adapter mbox
            allowed <- maybe (pure True) (\p -> policyAcceptPeer @s p pip) po
            unless allowed do
              debug $ red "mailbox:" <+> "CheckMailbox declined by peer policy"
                        <+> pretty mbox <+> pretty (AsBase58 pip)
              mzero

          -- Эхо нонса запрашивающего вместо собственных часов, и это весь фикс
          -- на стороне отвечающего: формат сообщения тот же, поле то же.
          --
          -- Nothing -- это старый пир, который нонса не шлёт вовсе; ему остаётся
          -- отметка часами, как было.
          let s' = maybe s (\n -> s { mbsMailboxPayloadNonce = n }) nonce

          let box = makeSignedBox @s (view peerSignPk creds) (view peerSignSk creds) s'

          lift $ lift $ response @_ @p (MailBoxProtoV1 (MailboxStatus box))

      MailboxStatus box -> do

        debug $ red "mailbox:" <+> "MailboxStatus"

        let r = unboxSignedBox0 @(MailBoxStatusPayload s) box

        PeerData{..} <- ContT $ maybe1 se' none

        (who, content@MailBoxStatusPayload{..}) <- ContT $ maybe1 r none

        unless ( who == _peerSignKey ) $ exit ()

        -- Нонс, который мы сами и выдали, а не сравнение двух чужих друг другу
        -- часов. Это то, о чём говорил FIXME, стоявший тут годами: поле в
        -- CheckMailbox было с самого начала, отвечающий его выбрасывал и ставил
        -- свои часы, и «свежесть» означала лишь, что чьи-то часы близки к нашим.
        -- Записанный статус прошлого обмена переигрывался, пока окно не
        -- закроется.
        --
        -- Теперь ответ считается свежим, если эхом вернулся нонс, который мы
        -- отправляли ИМЕННО про этот ящик. Часов в этой проверке нет.
        fresh <- mailboxCheckNonce @s adapter (MailboxRefKey mbsMailboxKey) mbsMailboxPayloadNonce

        -- Переходный запасной путь, и он временный.
        --
        -- Отвечающий, который эту сборку ещё не поставил, штампует статус своими
        -- часами и никакого нонса не эхает. Обратное направление работает само:
        -- старый запрашивающий кладёт в нонс своё время, новый отвечающий эхает
        -- его назад, и старая проверка окна проходит. Ломается только это, и без
        -- запасного пути ящик со старым пиром не синхронизировался бы вовсе.
        --
        -- Сюда же попадает НЕЗАПРОШЕННЫЙ статус: mailboxSetPolicy рассылает его
        -- госсипом, чтобы новая policy разошлась, и никакого нонса мы под него не
        -- выдавали. Значит убрать этот путь -- не просто снять пять строк: сперва
        -- нужно решить, как живёт та рассылка.
        --
        -- Пока путь жив, выигрыш только в корректности (см. clockSkew), не в
        -- стойкости: подставить правдоподобное время может кто угодно.
        --
        -- Само правило -- statusIsFresh, отдельная функция: обе его половины
        -- вместе больше не проверяются нигде, кроме как здесь, а это ровно то
        -- состояние, в котором прошлое правило и прожило годы со сломанной
        -- арифметикой.
        let skew = clockSkew now mbsMailboxPayloadNonce

        -- НЕ ВЫХОД, и это разделение двух половин статуса.
        --
        -- Раньше непрошеный статус отбрасывался целиком, и из-за этого рассылка
        -- новой policy зависела от окна часов: без него она не доходила. Но
        -- policy внутри статуса подписана ключом ящика, называет сам себя и
        -- принимается только со строго большей версией -- свежесть ей не даёт
        -- ничего. Дерево -- даёт: это хеш, который кто-то объявил, и принять его
        -- значит скачать и слить всё, что под ним лежит (см. заметку о
        -- poisoning ниже).
        --
        -- Поэтому теперь непрошеный статус несёт policy и не двигает дерево, а
        -- окно осталось ровно одному потребителю: ответу пира, который эту
        -- сборку ещё не поставил. Когда она разойдётся, отсюда уходит
        -- statusIsFresh и вместе с ним само окно.
        let origin | statusIsFresh fresh now mbsMailboxPayloadNonce = StatusAnswered
                   | otherwise = StatusUnasked

        when (origin == StatusUnasked) do
          debug $ red "mailbox:" <+> "status not an answer of ours, tree ignored, clock skew"
                    <+> pretty skew <+> "s from" <+> pretty (AsBase58 who)

        -- NOTE: possible-poisoning-attack
        --  левый пир генерирует merkle tree сообщений и посылает его.
        --  чего он может добиться: добавить "валидных" сообщений, которых не было
        --  в ящике изначально. (зашифрованных, подписанных).
        --
        --  можно рассылать спам, ведь каждое спам-сообщение
        --  будет валидно.
        --  мы не можем подписывать что-либо подписью владельца ящика,
        --  ведь мы не владеем его ключом.
        --
        --  как бороться:  в policy ограничивать число пиров, которые
        --  могут отдавать статус и игнорировать статусы от прочих пиров.
        --
        --  другой вариант -- каким-то образом публикуется подтверждение
        --  от автора, что пир X владеет почтовым ящиком R.
        --
        --  собственно, это и есть policy.
        --
        --  а вот policy мы как раз можем публиковать с подписью автора,
        --  он участвует в процессе обновления policy.
        --
        -- СДЕЛАНО (2026-08-02): проверка ниже -- это первый из названных выше
        -- способов. Чей статус мы ПРИНИМАЕМ, и это вторая половина вопроса, чьи
        -- вопросы мы готовы слышать.
        --
        -- Одного нонса тут мало и не могло хватить: CheckMailbox уходит
        -- госсипом, значит нонс есть у каждого связанного пира, и «свежесть» для
        -- них всех истинна. Нонс отвечает на вопрос «это ответ на мой вопрос», а
        -- не на вопрос «вправе ли этот пир отвечать».
        --
        -- Умолчание то же, что и на стороне ответа: policy нет -- принимаем от
        -- всех, как раньше. Проверка стоит после проверки свежести, потому что
        -- чтение policy дороже.
        po <- mailboxGetPolicyMay @s adapter (MailboxRefKey mbsMailboxKey)
        trusted <- maybe (pure True) (\p -> policyAcceptPeer @s p who) po

        unless trusted do
          debug $ red "mailbox:" <+> "status declined by peer policy"
                    <+> pretty (AsBase58 mbsMailboxKey) <+> "from" <+> pretty (AsBase58 who)
          exit ()

        void $ mailboxAcceptStatus adapter (MailboxRefKey mbsMailboxKey) who origin content

      DeleteMessages box -> do

        -- TODO: possible-ddos
        --   посылаем левые сообщения, заставляем считать
        --   подписи
        --
        --   Решения:  ограничивать поток сообщения от пиров
        --
        --   Возможно, вообще принимать только сообщения от пиров,
        --   которые содержатся в U {Policy(Mailbox_i)}
        --
        --   Возможно: PoW

        let r = unboxSignedBox0 box

        (mbox, spp) <- ContT $ maybe1 r none

        let h = hashObject @HbSync (serialise mess) & HashRef

        let routed = serialise (RoutedEntry h)
        let routedHash = hashObject routed

        seen <- hasBlock sto routedHash <&> isJust

        unless seen $ lift do
          gossip mess
          -- TODO: expire-block-and-collect-garbage
          --   $class: leak
          void $ putBlock sto routed

        mailboxAcceptDelete adapter (MailboxRefKey mbox) spp box

        none

