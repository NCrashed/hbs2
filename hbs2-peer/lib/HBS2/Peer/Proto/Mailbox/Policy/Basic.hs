{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2.Peer.Proto.Mailbox.Policy.Basic
  ( module HBS2.Peer.Proto.Mailbox.Policy
  , BasicPolicyAction(..)
  , getAsSyntax
  , parseBasicPolicy
  , defaultBasicPolicy
  , BasicPolicy(..)
  ) where

import HBS2.Prelude.Plated

import HBS2.Base58
import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Peer.Proto.Mailbox.Policy
import HBS2.Net.Auth.Credentials

import HBS2.System.Dir

import Data.Config.Suckless.Script

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe

data BasicPolicyAction =
  Allow | Deny
  deriving (Eq,Ord,Show,Generic)

data BasicPolicy s =
  BasicPolicy
  { bpDefaultPeerAction    :: BasicPolicyAction
  , bpDefaultSenderAction  :: BasicPolicyAction
  , bpPeers                :: HashMap (PubKey 'Sign s) BasicPolicyAction
  , bpSenders              :: HashMap (Sender s) BasicPolicyAction
  }
  deriving stock (Generic,Typeable)

deriving stock instance ForMailbox s => Eq (BasicPolicy s)

instance ForMailbox s => Pretty (BasicPolicy s) where
  pretty w = pretty (getAsSyntax @C w)

instance ForMailbox s => IsAcceptPolicy s (BasicPolicy s) where

  policyAcceptPeer BasicPolicy{..} p = do
    pure $ Allow == fromMaybe bpDefaultPeerAction (HM.lookup p bpPeers)

  policyAcceptSender BasicPolicy{..} p = do
    pure $ Allow == fromMaybe bpDefaultSenderAction (HM.lookup p bpSenders)

  policyAcceptMessage BasicPolicy{..} s m = do
    pure $ Allow == fromMaybe bpDefaultSenderAction (HM.lookup s bpSenders)

getAsSyntax :: forall c s . (ForMailbox s, IsContext c)
            => BasicPolicy s -> [Syntax c]
getAsSyntax BasicPolicy{..} =
  [ defPeerAction
  , defSenderAction
  ] <> peerActions <> senderActions
  where
    defPeerAction   = mkList [mkSym "peer", action bpDefaultPeerAction, mkSym "all"]
    defSenderAction = mkList [mkSym "sender", action bpDefaultSenderAction, mkSym "all"]

    peerActions = [ mkList [mkSym "peer", action a, mkSym (show $ pretty (AsBase58 who))]
                  | (who, a) <- HM.toList bpPeers ]

    senderActions = [ mkList [mkSym "sender", action a, mkSym (show $ pretty (AsBase58 who))]
                    | (who, a) <- HM.toList bpSenders ]


    action = \case
      Allow -> mkSym "allow"
      Deny  -> mkSym "deny"

defaultBasicPolicy :: forall s . (ForMailbox s) => BasicPolicy s
defaultBasicPolicy = BasicPolicy Deny Deny mempty mempty

parseBasicPolicy :: forall s c m . (IsContext c, s ~ HBS2Basic, ForMailbox s, MonadUnliftIO m)
                 => [Syntax c]
                 -> m (Maybe (BasicPolicy s))

parseBasicPolicy syn = do

  tpAction <- newTVarIO Deny
  tsAction <- newTVarIO Deny
  tpeers   <- newTVarIO mempty
  tsenders <- newTVarIO mempty

  -- Клаузу, которую эта сборка не понимает, теперь нельзя пропустить молча.
  -- Раньше в catch-all стояло `pure ()`, и функция всё равно возвращала Just:
  -- `(peer alow all)` -- одна буква -- отбрасывалось без единого слова, а
  -- loadPolicyContent докладывал об успешно прочитанной политике, и ящик
  -- оставался настолько открытым, насколько говорит его действие по умолчанию.
  -- По той же причине проверки `orThrowUser "invalid policy"` в hbs2-cli были
  -- недостижимы.
  --
  -- Отказ целиком, а не по клаузе, и это безопасное направление: вызывающая
  -- сторона падает обратно на defaultBasicPolicy, то есть deny/deny.
  tbad <- newTVarIO (0 :: Int)

  -- Раскладка не часть формата. parseTop делает список на строку, поэтому
  -- несколько клауз на одной строке приходят одной формой, содержащей их; файл
  -- мог быть переформатирован чем угодно. Без этого ужесточение выше отвергало
  -- бы рабочую политику, записанную в одну строку.
  let flat s = case s of
        ListVal (ListVal{} : _) -> [ x | x@ListVal{} <- unList s ]
        _                       -> [s]
      unList = \case
        ListVal xs -> xs
        _          -> []

  for_ (concatMap flat syn) $ \case
    ListVal [SymbolVal "peer", SymbolVal "allow", SymbolVal "all"]  -> do
      atomically $ writeTVar tpAction Allow

    ListVal [SymbolVal "peer", SymbolVal "deny", SymbolVal "all"]  -> do
      atomically $ writeTVar tpAction Deny

    ListVal [SymbolVal "peer", SymbolVal "allow", SignPubKeyLike who]  -> do
      atomically $ modifyTVar tpeers (HM.insert who Allow)

    ListVal [SymbolVal "peer", SymbolVal "deny", SignPubKeyLike who]  -> do
      atomically $ modifyTVar tpeers (HM.insert who Deny)

    ListVal [SymbolVal "sender", SymbolVal "allow", SymbolVal "all"]  -> do
      atomically $ writeTVar tsAction Allow

    ListVal [SymbolVal "sender", SymbolVal "deny", SymbolVal "all"]  -> do
      atomically $ writeTVar tsAction Deny

    ListVal [SymbolVal "sender", SymbolVal "allow", SignPubKeyLike who]  -> do
      atomically $ modifyTVar tsenders (HM.insert who Allow)

    ListVal [SymbolVal "sender", SymbolVal "deny", SignPubKeyLike who]  -> do
      atomically $ modifyTVar tsenders (HM.insert who Deny)

    _ -> atomically $ modifyTVar tbad succ

  a <- readTVarIO tpAction
  b <- readTVarIO tsAction
  c <- readTVarIO tpeers
  d <- readTVarIO tsenders

  bad <- readTVarIO tbad

  pure $ if bad > 0 then Nothing else Just (BasicPolicy @s a b c d)


