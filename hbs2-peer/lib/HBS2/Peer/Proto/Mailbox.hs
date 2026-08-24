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

  -- | Should this peer put this message back on the wire now?
  --
  -- Test and set: 'True' the first time a hash is offered and 'False' ever
  -- after, for as long as this peer remembers it. Gossip hands the same message
  -- to every neighbour, so without this a message circulates for as long as the
  -- graph has cycles.
  --
  -- It answers a question about THIS PROCESS, which is why it is a method here
  -- rather than a lookup in the block store, where it used to live and where a
  -- stranger could plant the answer. See
  -- "HBS2.Peer.Proto.Mailbox.Relayed".
  mailboxRelayOnce      :: forall m . MonadIO m => a -> HashRef -> m Bool

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

  -- | Note that a message was not forwarded because it did not meet the floor.
  --
  -- COUNTED RATHER THAN LOGGED, and that is the point of it existing. A sender
  -- solves for what the MAILBOX charges, which is in the mailbox's signed
  -- policy; the floor above is a peer's own number and is in nobody's policy.
  -- So a relay set to 16 does not carry a message that honestly paid the 12 its
  -- destination asked for, and the sender does not learn why: there is no reply
  -- on this path. The only trace was a 'debug' line, which an operator running
  -- at the ordinary level never sees.
  --
  -- SINCE PEP-23 the floor is at least KNOWABLE one hop out: a peer
  -- publishes it in its meta and 'mailboxRelayFloor' answers what a client must
  -- pay to leave this machine. A relay further away is still invisible, and
  -- this counter is still the only thing that says the refusal happened.
  --
  -- A warning per message would be worse than nothing: the floor earns its keep
  -- exactly when a flood is arriving, which is when a line per message is its
  -- own denial of service. A counter in the periodic report says the same
  -- thing in one number, and only a peer that SET a floor can ever raise it.
  --
  -- Does nothing by default, so an adapter that keeps no statistics is unchanged.
  mailboxNotForwarded   :: forall m . MonadIO m => a -> m ()
  mailboxNotForwarded _ = pure ()

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
                    -> Maybe (MessageStamp s) -- ^ proof of work, when a relay on the way charges for one
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

  -- | The least work a message must carry to leave this machine (PEP-23).
  --
  -- The maximum of this peer's own 'mailboxPoWFloor' and the largest floor its
  -- neighbours have published. BOTH HALVES ARE NEEDED and the first is the one
  -- that surprises: a message submitted over the RPC goes through the same
  -- forwarding rule as one arriving from the network, so a peer with a floor of
  -- its own will not gossip a letter composed on the very machine it runs on.
  --
  -- It exists on the SERVICE and not on the adapter because it is the answer a
  -- client asks for before it grinds, and a client talks to the service. The
  -- adapter's 'mailboxPoWFloor' is the one half of it that gates a packet.
  --
  -- Zero by default, which is a peer that answers "nothing is required" and is
  -- what every peer said before this existed.
  mailboxRelayFloor :: forall m . MonadIO m => a -> m PoWDifficulty
  mailboxRelayFloor _ = pure 0

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
  mailboxRelayFloor (AnyMailboxService a) = mailboxRelayFloor @s a

instance ForMailbox s => IsMailboxProtoAdapter s (AnyMailboxAdapter s) where
  mailboxGetCredentials (AnyMailboxAdapter a) = mailboxGetCredentials @s a
  mailboxGetPolicy (AnyMailboxAdapter a) = mailboxGetPolicy @s a
  mailboxGetPolicyMay (AnyMailboxAdapter a) = mailboxGetPolicyMay @s a
  mailboxCheckNonce (AnyMailboxAdapter a) = mailboxCheckNonce @s a
  mailboxRelayOnce (AnyMailboxAdapter a) = mailboxRelayOnce @s a
  mailboxGetStorage (AnyMailboxAdapter a) = mailboxGetStorage @s a
  mailboxPoWFloor (AnyMailboxAdapter a) = mailboxPoWFloor @s a
  mailboxNotForwarded (AnyMailboxAdapter a) = mailboxNotForwarded @s a
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

          -- Отметка гейтит ТОЛЬКО пересылку, и это исправление.
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
          -- Спрашивается ПАМЯТЬ ПИРА, а не наличие блока, и спрашивается только
          -- когда пересылать вообще собираемся -- иначе ветка, которая не
          -- пересылает, съедала бы первое появление и гасила следующую копию.
          --
          -- Блок тут был вдвойне неправ. Его адрес считается из хеша сообщения,
          -- то есть из величины, которую видно на проводе, а хранилище блоков
          -- качает по запросу -- значит чужой мог подсадить отметку на пиров
          -- между отправителем и хабом, и выбранное письмо переставало
          -- пересылаться, молча. И он никогда не удалялся, что и был соседний
          -- $class: leak. См. заголовок "HBS2.Peer.Proto.Mailbox.Relayed".
          when relay do
            fresh <- lift $ mailboxRelayOnce @s adapter h

            when fresh $ lift do
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
              -- ЧАСТИЧНО ЗАКРЫТО порогом mailboxPoWFloor, и теперь он
              -- спрашивается на ОБЕИХ ветках. См. ветку SendMessage ниже:
              -- сообщение без штампа несёт ноль бит работы, а не «столько,
              -- сколько нужно».

          lift do
            let whoever = if inner then Nothing else Just pip
            void $ mailboxAcceptMessage adapter whoever origin msg content

    -- ОДНО ТЕЛО НА ОБЕ ФОРМЫ DELETE, как и у сообщения выше: расходятся они
    -- только тем, сколько бит несёт пакет и чем он опознаётся для дедупа, а всё
    -- дальнейшее -- проверка подписи, рассылка, маркер, приём -- у них общее и
    -- обязано остаться общим.
    --
    -- Порядок тот же, что и у сообщения со штампом, и обе его половины
    -- существенны. Проверка ДО рассылки -- иначе штамп ограничивал бы только
    -- диск, а флуд уже ушёл ко всем известным пирам. ДО маркера -- иначе маркер,
    -- записанный для пакета с негодным штампом, погасил бы годную копию того же
    -- delete везде, куда она ещё не дошла.
    let takeDeleteWith stamp box = do

          let r = unboxSignedBox0 box

          (mbox, spp) <- ContT $ maybe1 r none

          floorD <- lift $ mailboxPoWFloor @s adapter

          -- Сколько бит несёт пакет и чем он опознаётся -- единственное, чем
          -- различаются две формы.
          --
          -- `named` для delete -- это «штамп называет тот ящик, который подписал
          -- этот бокс» (stampNamesDelete): работа, решённая за чужой ящик, -- это
          -- работа за чужое удаление. Ключ восстановлен из подписи выше, так что
          -- проверка бесплатна.
          --
          -- У пакета без штампа `named` истинно, а бит ноль: платить не за кого,
          -- и порог 0 пропускает его как и раньше.
          --
          -- Одним проходом (stampWitness): биты и маркер считаются из одного
          -- ключа бокса, а не двумя независимыми вызовами, каждый из которых
          -- сериализует бокс заново. Это путь, на котором чужой пакет тратит наш
          -- CPU, и лишний обход тут не бесплатен.
          let (named, bits, marker) = case stamp of
                Nothing ->
                  (True, 0, hashObject @HbSync (serialise mess) & HashRef)
                Just st ->
                  let (b, m) = stampWitness @s (deleteKey @s box) st
                  in (stampNamesDelete mbox st, b, m)

          let carry = forwardable floorD named bits

          -- ДВЕ ПРИЧИНЫ -- ДВЕ СТРОКИ, как в ветке SendMessageStamped. Одна
          -- строка «too little work, wanted N» печаталась и тогда, когда бит
          -- хватало, а штамп называл чужой ящик: оператор читал про работу и шёл
          -- искать не то. Наблюдаемость тут держится на логах целиком.
          unless named do
            debug $ red "mailbox: a delete's stamp names another mailbox"
                      <+> pretty (AsBase58 mbox)

          unless (bits >= fromIntegral floorD) do
            debug $ red "mailbox: a delete carries too little work to forward"
                      <+> pretty bits <> ", wanted" <+> pretty floorD

          -- И СЧЁТЧИК, которого здесь не было. powNotForwarded в периодическом
          -- отчёте -- единственное, чем оператору показывают, во что его порог
          -- обошёлся чужой доставке, и для delete это обещание не выполнялось:
          -- обе ветки сообщения его дёргают, эта -- нет.
          unless carry do
            lift $ mailboxNotForwarded @s adapter

          -- Память пира, не блок: тот же разбор, что и у сообщения выше. И
          -- спрашивается ТОЛЬКО когда пересылать собираемся -- иначе ветка,
          -- которая не пересылает, съела бы первое появление и погасила
          -- следующую копию везде, куда та ещё не дошла.
          when carry do
            fresh <- lift $ mailboxRelayOnce @s adapter marker

            when fresh $ lift do
              gossip mess

          mailboxAcceptDelete adapter (MailboxRefKey mbox) spp box

          none

    -- Подпись дешевле диска, поэтому она первой в обеих ветках.
    let unboxMessage msg = ContT $ maybe1 (unboxSignedBox0 @(MessageContent s) (messageContent msg)) none

    case mailBoxProtoPayload mess of
      SendMessage msg -> do

        debug $ red "AAAAAA!" <+> pretty now

        -- ок, сообщение нормальное, шлём госсип, пишем, что обработали
        -- TODO: increment-malformed-messages-statistics
        --   $workflow: backlog
        (_, content) <- unboxMessage msg

        -- ПОРОГ СПРАШИВАЕТСЯ И ЗДЕСЬ, а раньше не спрашивался.
        --
        -- Он стоял только в ветке со штампом, то есть оператор, поставивший
        -- себе hbs2:mailbox:pow-min, обходился ровно тем, что штамп не
        -- прикладывают: сообщение без штампа ретранслировалось безусловно и
        -- всеми. А ретрансляция -- это broadcastMessage ко ВСЕМ известным
        -- пирам, и mailboxProto зарегистрирован независимо от воркера, так
        -- что усиливает и тот пир, который не держит ни одного ящика.
        --
        -- Сообщение без штампа несёт НОЛЬ бит работы. Это не «неизвестно
        -- сколько» и не «сколько попросят»: порог 0 (умолчание) такое
        -- сообщение пропускает и ничего не меняет ни для кого, а любой
        -- ненулевой порог означает ровно то, что оператор им сказал.
        --
        -- Приём это не гейтит, как и в ветке со штампом: слабый штамп значит
        -- «дальше не понесу», письмо доходит до очереди, и судьбу его решает
        -- policy ящика, у которой есть настоящее D.
        floorD <- lift $ mailboxPoWFloor @s adapter

        -- Through 'forwardable' like the two branches below it: an unstamped
        -- message pays zero bits, which is what "floor 0 lets it through" says.
        let carry = forwardable floorD True 0

        unless carry do
          lift $ mailboxNotForwarded @s adapter
          debug $ red "mailbox: unstamped message carries no work to forward"
                    <+> parens ("floor is" <+> pretty floorD <+> "bits")

        takeMessageWith carry (Submitted Nothing) msg content
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

        -- Одним проходом, как и в delete-ветке: биты и маркер выводятся из
        -- одного ключа сообщения, а не двумя вызовами, каждый из которых
        -- сериализует его заново.
        let (bits, marker) = stampWitness @s (messageKey @s msg) stamp

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
          lift $ mailboxNotForwarded @s adapter
          debug $ red "mailbox: stamp too weak to forward"
                    <+> pretty bits <> ", wanted" <+> pretty floorD

        takeMessageWith (forwardable floorD named bits)
                        (Submitted (Just stamp)) msg content marker

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
          -- СДЕЛАНО: кэш по (ящик, хеш policy), см.
          -- "HBS2.Peer.Proto.Mailbox.PolicyCache". Стояло как $class:
          -- performance, и это была неверная классификация: getBlock, проверка
          -- подписи, обход merkle-дерева и два разбора повторялись на каждый
          -- входящий CheckMailbox -- пакет в сорок байт, который шлёт кто
          -- угодно, под NoLimit и на общем пуле deferred. Неучтённая работа,
          -- которую чужой пир может просить сколько хочет, -- это отказ в
          -- обслуживании, а не константа.
          --
          -- Select по хешу остаётся здесь на каждом запросе, и это не остаток
          -- проблемы: он один и индексный, а его результат -- одновременно
          -- признак «владелец что-то сказал» и штамп годности кэша.
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

        -- ОКНА ЧАСОВ БОЛЬШЕ НЕТ, и решает только нонс.
        --
        -- Здесь стоял запасной путь: статус считался свежим ещё и тогда, когда
        -- его отметка времени лежала в десяти секундах от наших часов. Он был
        -- написан ради отвечающего, который эту сборку не поставил и нонса не
        -- эхает, и ради рассылки policy, под которую нонса нет ни у какой
        -- версии. Вторую половину снял разделённый статус: policy подписана
        -- ключом ящика, называет сам себя и принимается только со строго
        -- большей версией, так что непрошеный статус несёт её и без свежести.
        --
        -- А первая половина отменяла всю проверку. Отметку времени пишет
        -- отправитель, поэтому @clockSkew now nonce < 10@ выполнялось у любого,
        -- у кого идут часы: StatusUnasked был недостижим, useTree -- истинно
        -- для всех, и разделение статуса не применялось ни разу. Стоило это
        -- вот чего: подделанный статус заставляет узел скачать и слить дерево,
        -- которое собрал объявивший, а ветка Replicated в mailboxInQ при
        -- @(pow 0)@ -- умолчании -- не проверяет работу вовсе. Для открытого
        -- инбокса это бесплатная запись в чужой ящик мимо очереди.
        --
        -- ЧТО ЭТО ЛОМАЕТ, прямо: узел с этой сборкой не возьмёт дерево у пира,
        -- который нонс не эхает, то есть у любой выпущенной версии -- эхо
        -- появилось после 0.25.5.0. Policy при этом продолжает ходить, так что
        -- расходится именно синхронизация ящиков, и молчать об этом нельзя:
        -- сообщение ниже идёт warn, а не debug.
        let origin | fresh = StatusAnswered
                   | otherwise = StatusUnasked

        when (origin == StatusUnasked) do
          warn $ red "mailbox:" <+> "status is not an answer to anything we asked;"
                   <+> "its tree was ignored and this mailbox will not sync from"
                   <+> pretty (AsBase58 who) <> line
                   <> "  a peer older than the nonce echo cannot answer one; upgrade it."

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

        -- ЧТО ЗДЕСЬ ДОКАЗЫВАЕТ ПОДПИСЬ, и что она НЕ доказывает.
        --
        -- `mbox` не приходит с провода: он ВОССТАНАВЛИВАЕТСЯ из подписи, то
        -- есть подписант объявляется владельцем ящика. Значит «валидная подпись
        -- ключом ящика» выполняется для любой свежесгенерированной пары ключей,
        -- и стоит она отправителю одну подпись. Проверка «а держим ли мы такой
        -- ящик» живёт внутри mailboxAcceptDelete и относится к ДИСКУ.
        --
        -- Отсюда следовало, что эта ветка была неограниченным примитивом
        -- широковещания: один пакет -- рассылка всем известным пирам, на каждом
        -- хопе, включая пиров, не держащих ни одного ящика. Ровно про это
        -- прежний TODO possible-ddos, и он называл верное лекарство (PoW).
        --
        -- ПОЧЕМУ РАССЫЛКА ВСЁ ЖЕ ИДЁТ ДО ПРОВЕРКИ ВЛАДЕЛЬЦА, а не после.
        -- Соседняя ветка устроена так же и по той же причине: транзитный пир не
        -- держит ящика и обязан пересылать, иначе доставка через промежуточные
        -- хопы ломается (PEP-21). Перенос gossip под mailboxAcceptDelete закрыл
        -- бы усиление ценой самой доставки, а это не размен, а поломка.
        --
        -- Гейтом служит то же, чем гейтится сообщение без штампа: ПОРОГ
        -- ЭТОГО ПИРА. Delete без штампа несёт ноль бит работы, и это не
        -- «неизвестно сколько»: порог 0 (умолчание) пропускает его как и
        -- раньше, а любой ненулевой порог означает ровно то, что оператор им
        -- сказал. Приём это не гейтит, как и в обеих ветках сообщения: слабый
        -- штамп значит «дальше не понесу».
        --
        -- Что этим НЕ закрыто, честно: при пороге 0 усиление остаётся. Поле под
        -- штамп теперь есть (PEP-23, ветка ниже), но ограничивает оно
        -- только тогда, когда порог кто-то поставил; сам дефолт этот шаг не
        -- трогает и не должен.
        takeDeleteWith Nothing box

      -- Тот же delete, но с доказательством работы (PEP-23).
      DeleteMessagesStamped box stamp -> takeDeleteWith (Just stamp) box

