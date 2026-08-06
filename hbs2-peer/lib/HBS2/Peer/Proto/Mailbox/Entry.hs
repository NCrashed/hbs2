{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}

module HBS2.Peer.Proto.Mailbox.Entry where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Peer.Proto.Mailbox.Ref

import Control.Applicative
import Data.Word
import Codec.Serialise
import Data.Hashable
import Data.Maybe

data ProofOfDelete =
  ProofOfDelete
  { deleteMessage :: Maybe HashRef -- ^ different things?
  }
  deriving stock (Generic,Eq,Ord,Show)

data ProofOfExist =
  ProofOfExist
  { existedPolicy  :: Maybe HashRef
  }
  deriving stock (Generic,Eq,Ord,Show)

instance Monoid ProofOfDelete where
  mempty = ProofOfDelete mzero

instance Semigroup ProofOfDelete where
  (<>) (ProofOfDelete a1) (ProofOfDelete a2) = ProofOfDelete (a1 <|> a2)

instance Monoid ProofOfExist where
  mempty = ProofOfExist mzero

instance Semigroup ProofOfExist where
  (<>) (ProofOfExist a1) (ProofOfExist a2) = ProofOfExist (a1 <|> a2)

data MailboxEntry =
    Exists ProofOfExist  HashRef
  | Deleted ProofOfDelete HashRef  -- ^ proof-of-message-to-validate
  deriving stock (Eq,Ord,Show,Generic)

instance Hashable MailboxEntry where
  hashWithSalt salt = \case
    Exists p r -> hashWithSalt salt (0x177c1a3ad45b678e :: Word64, serialise (p,r))
    Deleted p r -> hashWithSalt salt (0xac3196b4809ea027 :: Word64, serialise (p,r))

data RoutedEntry = RoutedEntry HashRef
                   deriving stock (Eq,Ord,Show,Generic)


instance Serialise MailboxEntry
instance Serialise RoutedEntry
instance Serialise ProofOfDelete
instance Serialise ProofOfExist


-- | Запись, которой ящик говорит «это сообщение у меня есть».
--
-- Функция ОДНОГО сообщения: доказательство пустое, поэтому хеш записи можно
-- посчитать, ничего не сохраняя. На этом стоит дешёвый ранний выход в
-- mailboxInQ, который позволяет принимать одно и то же сообщение повторно, не
-- переплачивая за него проверкой подписи и разбором policy.
existsEntry :: HashRef -> MailboxEntry
existsEntry = Exists (ProofOfExist mzero)

-- | Хеш этой записи.
existsEntryHash :: HashRef -> HashRef
existsEntryHash = HashRef . hashObject . serialise . existsEntry

-- | Запись, которой ящик говорит «это сообщение удалено».
--
-- Первый аргумент -- блок с доказательством (подписанный владельцем delete-box),
-- второй -- сообщение, которое он разрешает удалить. Оба хеша считаются без
-- обращения к хранилищу, поэтому повторно пришедший delete можно опознать одним
-- поиском, не заводя ничего в очередь слияния.
deletedEntry :: HashRef -> HashRef -> MailboxEntry
deletedEntry proof = Deleted (ProofOfDelete (Just proof))

-- | Хеш этой записи.
deletedEntryHash :: HashRef -> HashRef -> HashRef
deletedEntryHash proof = HashRef . hashObject . serialise . deletedEntry proof

-- ЗДЕСЬ БЫЛ mergedMarker, и его здесь больше нет. «Эта запись уже влита в этот
-- ящик» было наличием блока по хешу, который считался из mailbox key и хеша
-- записи -- то есть из величин, публичных обеих. Хранилище блоков адресуется
-- содержимым и качает по запросу, так что посчитать этот хеш и подсадить блок
-- мог кто угодно, а дальше выбранное письмо не принималось никогда и молча.
-- Отметка переехала в базу самого ящика; причины целиком -- в заголовке
-- "MailboxMerged" (hbs2-peer/app).


