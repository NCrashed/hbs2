{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.Proto.RefChan.RefChanNotify where


import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Net.Proto
import HBS2.Net.Auth.Credentials
import HBS2.Events
import HBS2.Peer.Proto.Peer
import HBS2.Peer.Proto.RefChan.Adapter
import HBS2.Net.Proto.Sessions
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Actors.Peer.Types
import HBS2.Storage

import HBS2.Peer.Proto.RefChan.Types

import HBS2.System.Logger.Simple

import Control.Monad.Trans.Maybe
import Data.Maybe

refChanNotifyProto :: forall e s m proto . ( MonadIO m
                                           , Request e proto m
                                           , Response e proto m
                                           , HasRefChanId e proto
                                           , HasDeferred proto e m
                                           , HasGossip e proto m
                                           , IsPeerAddr e m
                                           , Pretty (Peer e)
                                           , Sessions e (RefChanHeadBlock e) m
                                           , Sessions e (KnownPeer e) m
                                           , EventEmitter e proto m
                                           , HasStorage m
                                           , Signatures s
                                           , IsRefPubKey s
                                           , ForRefChans e
                                           , proto ~ RefChanNotify e
                                           , s ~ Encryption e
                                           )
                  => Bool
                  -> RefChanAdapter e m
                  -> RefChanNotify e
                  -> m ()

refChanNotifyProto self adapter msg@(ActionRequest rchan a) = do
  debug $ "RefChanNotify ACTION REQUEST"
  pure ()

refChanNotifyProto self adapter msg@(Notify rchan box) = do
  -- аутентифицируем
  -- проверяем ACL
  -- пересылаем всем

  sto <- getStorage

  peer <- thatPeer @proto

  let h0 = hashObject @HbSync (serialise msg)

  auth <- find (KnownPeerKey peer) id <&> isJust

  void $ runMaybeT do

    guard =<< lift (refChanSubscribed adapter rchan)

    guard (self || auth)

    debug $ "&&& refChanNotifyProto" <+> pretty self

    deferred @proto do

      -- ЭТОТ ПИР, А НЕ ХРАНИЛИЩЕ. Здесь стояло `hasBlock sto h0`, где h0 --
      -- хеш пакета, видного на проводе: чужой считает его до отправки, кладёт
      -- блок, и гейт отвечает «уже видели» про пакет, который никто не
      -- обрабатывал. Выбранный notify замолкает на всех пирах, куда чужой
      -- дотянулся, и никто об этом не узнаёт. Плюс блок не удалялся никогда.
      -- См. "HBS2.Peer.Proto.Relayed".
      guard =<< lift (refChanRelayOnce adapter (HashRef h0))

      (authorKey, bs) <- MaybeT $ pure $ unboxSignedBox0 box

      let refchanKey = RefChanHeadKey @s rchan
      headBlock <- MaybeT $ getActualRefChanHead @e refchanKey

      guard $ checkACL ACLNotify headBlock Nothing authorKey

      -- FIXME: garbage-collection-required
      liftIO $ putBlock sto (serialise msg)

      -- теперь пересылаем по госсипу
      lift $ gossip msg

      -- FIXME: remove-debug
      let h1 = hashObject @HbSync (serialise box)
      debug $ "^^^ refChanNotifyProto" <+> pretty peer <+> pretty h0 <+> pretty h1

      -- тут надо заслать во внешнее приложение,
      -- равно как и в остальных refchan-протоколах

      unless self do
        debug $ "^^^ CALL refChanNotifyRely" <+> pretty h0
        lift $ refChanNotifyRely adapter rchan msg

      lift $ emit @e (RefChanNotifyEventKey rchan) (RefChanNotifyEvent (HashRef h0) msg)

