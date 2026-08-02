{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Who a flow key belongs to, and who gets to say so.
--
-- ByPass names an encrypted flow by a 32-bit key built from two 16-bit nonces,
-- one from each end. Half of that value is therefore the remote side's free
-- choice, and a HEY carries it in the clear, outside the signed box. A flow key
-- is a name for a flow. It was being used as a name for a peer.
--
-- Concretely: the shared key used to be installed under the flow key on a
-- first-writer-wins rule, so anyone who announced a nonce colliding with an
-- honest peer's got their own key installed where that peer's belonged. The
-- honest peer's HEY then found the slot taken and installed nothing, while its
-- nonce was still recorded, so outbound traffic for it went out encrypted to
-- the stranger. Sixteen bits is also small enough that two honest peers collide
-- by birthday at a few hundred, with the same result and no attacker.
--
-- These two cases pin the split that fixes it: what we encrypt to a peer is
-- keyed by the peer, and what may decrypt a flow key is a set of candidates.
module TestByPassFlow (testByPassFlowNotStolen, testByPassFlowSharedIsReadable) where

import HBS2.Data.Types.SignedBox
import HBS2.Net.Auth.Schema ()
import HBS2.Net.Messaging
import HBS2.Net.Messaging.Encrypted.ByPass
import HBS2.Net.Messaging.Encrypted.RandomPrefix
import HBS2.Net.Messaging.Fake

import Codec.Serialise
import Control.Concurrent.STM (readTVarIO)
import Control.Monad (foldM, forM_)
import Crypto.Saltine.Class qualified as SA
import Crypto.Saltine.Core.Box qualified as PKE
import Crypto.Saltine.Core.Sign qualified as Sign
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word
import Network.ByteOrder qualified as N
import Prettyprinter
import System.Random (randomIO)

import Test.Tasty.HUnit

-- A proto with nothing in it but an address, so the fake bus can tell the
-- parties apart.
data Flow

instance HasPeer Flow where
  newtype instance Peer Flow = FlowPeer Int
                               deriving newtype (Hashable)
                               deriving stock (Eq, Ord, Show)

type instance Encryption Flow = 'HBS2Basic

instance Pretty (Peer Flow) where
  pretty (FlowPeer n) = parens ("peer" <+> pretty n)

victimAddr, honestAddr, strangerAddr :: Peer Flow
victimAddr   = FlowPeer 0
honestAddr   = FlowPeer 1
strangerAddr = FlowPeer 2

-- | A signing identity plus the encryption keypair a HEY announces, which is
-- what 'newByPassMessaging' keeps to itself. Held here so a test can be a peer
-- that names its own nonce.
data Impostor =
  Impostor
  { impSignPk :: PubKey 'Sign 'HBS2Basic
  , impSignSk :: PrivKey 'Sign 'HBS2Basic
  , impEncPk  :: PubKey 'Encrypt 'HBS2Basic
  , impEncSk  :: PrivKey 'Encrypt 'HBS2Basic
  }

newImpostor :: IO Impostor
newImpostor = do
  Sign.Keypair ssk spk <- Sign.newKeypair
  PKE.Keypair esk epk  <- PKE.newKeypair
  pure $ Impostor spk ssk epk esk

-- | The HEY an impostor would send. This is 'sendHey' with the nonce as an
-- argument instead of a field: everything else about it is honest, and it is
-- signed by a key the default 'ByPassOpts' accepts, as it accepts every key.
impostorHey :: Impostor -> NonceA -> IO ByteString
impostorHey imp nonce = do
  pref <- randomPrefix (PrefixMethod1 4 11 heySeed) <&> toLazyByteString
  ts <- getPOSIXTime <&> round
  let box = makeSignedBox @'HBS2Basic (impSignPk imp) (impSignSk imp)
                                      (HEYBox @Flow ts (impEncPk imp))
  pure $ pref <> serialise (HEY @Flow nonce box)

-- | A packet an impostor would send on a flow it shares with the victim: the
-- header ByPass reads, then a box under the key both ends derive.
impostorPacket :: Impostor
               -> PubKey 'Encrypt 'HBS2Basic
               -> FlowKey
               -> ByteString
               -> IO ByteString
impostorPacket imp victimPke fk payload = do
  wnonce <- randomIO @Word32
  bnonce <- boxNonceFrom wnonce
  let ck = PKE.beforeNM (impEncSk imp) victimPke
  let ebs = PKE.boxAfterNM ck bnonce (LBS.toStrict payload)
  pure $ toLazyByteString (word32BE fk <> word32BE wnonce <> byteString ebs)

-- ByPass derives the box nonce from the 32-bit counter in the packet header by
-- zero padding it; the test has to spell the same rule to speak the same
-- packets. The width is asked of saltine rather than written down, for the same
-- reason ByPass asks it.
boxNonceFrom :: Word32 -> IO PKE.Nonce
boxNonceFrom a = do
  n <- PKE.newNonce <&> BS.length . SA.encode
  pure $ fromJust $ SA.decode $ BS.take n (N.bytestring32 a <> BS.replicate n 0)

-- | Pump a party's inbox. Each 'receive' takes at most one message off the fake
-- bus, and a HEY comes back as nothing at all, so this asks more times than
-- there can be messages and keeps whatever came out.
drain :: Messaging bus Flow ByteString => bus -> Peer Flow -> IO [ByteString]
drain bus me = foldM step [] [1 :: Int .. 64]
  where
    step acc _ = do
      got <- receive bus (To me)
      pure (acc <> [m | (_, m) <- got])

-- | An honest peer's flow cannot be taken over by a stranger who names its
-- nonce first.
--
-- The stranger gets in ahead of the honest peer with a HEY that is valid in
-- every way except that the nonce it names is somebody else's. Under the old
-- rule that was enough, and the victim's traffic for the honest peer went out
-- under the stranger's key: readable by the stranger, and by nobody else.
testByPassFlowNotStolen :: IO ()
testByPassFlowNotStolen = do
  bus <- newFakeP2P @Flow @ByteString False

  Sign.Keypair vsk vpk <- Sign.newKeypair
  Sign.Keypair hsk hpk <- Sign.newKeypair

  victim <- newByPassMessaging @Flow byPassDef bus victimAddr vpk vsk
  honest <- newByPassMessaging @Flow byPassDef bus honestAddr hpk hsk

  stranger <- newImpostor

  -- The nonce travels in the clear in every HEY the honest peer sends, so
  -- learning it costs an attacker nothing. Read it off the instance for brevity.
  hey <- impostorHey stranger (nonceA honest)
  sendTo bus (To victimAddr) (From strangerAddr) hey
  _ <- drain victim victimAddr

  -- Now the honest peer turns up and says the same nonce, truthfully. It has no
  -- key for the victim yet, so the knock goes in the clear and drags a HEY
  -- along behind it; a couple of rounds settle the handshake.
  sendTo honest (To victimAddr) (From honestAddr) ("knock" :: ByteString)
  forM_ [1 :: Int .. 3] $ \_ -> do
    _ <- drain victim victimAddr
    _ <- drain honest honestAddr
    pure ()

  -- The victim answers. The only question is who can read the answer.
  let secret = "for the honest peer only" :: ByteString
  sendTo victim (To honestAddr) (From victimAddr) secret

  -- It has to be encrypted, or the test would pass on plaintext.
  stat <- getStat victim
  assertBool "the victim encrypted rather than bypassed" (statEncrypted stat > 0)

  arrived <- drain honest honestAddr
  assertEqual "the honest peer reads what was sent to it" [secret] arrived

  -- The stranger keeps its own flow; what it lost is the ability to speak for
  -- somebody else's.
  flows <- readTVarIO (peerFlow victim)
  assertEqual "each peer holds its own flow" 2 (HM.size flows)

-- | Two peers may share a flow key without either becoming unreadable.
--
-- This is the honest version of the same collision: sixteen bits of nonce is
-- small enough that at a few hundred peers a pair of them land on one flow key
-- by accident. Both must stay readable, which means a flow key resolves to a set
-- of candidates and the Poly1305 tag picks among them, rather than to the single
-- key of whoever handshook first.
testByPassFlowSharedIsReadable :: IO ()
testByPassFlowSharedIsReadable = do
  bus <- newFakeP2P @Flow @ByteString False

  Sign.Keypair vsk vpk <- Sign.newKeypair
  victim <- newByPassMessaging @Flow byPassDef bus victimAddr vpk vsk

  firstPeer  <- newImpostor
  secondPeer <- newImpostor

  -- Both announce the same nonce, from different addresses and identities.
  let shared = 4242 :: NonceA
  let parties = [(honestAddr, firstPeer), (strangerAddr, secondPeer)]

  forM_ parties $ \(addr, who) -> do
    hey <- impostorHey who shared
    sendTo bus (To victimAddr) (From addr) hey
    _ <- drain victim victimAddr
    pure ()

  let fk = makeKey (nonceA victim) shared

  let payloads = ["from the first", "from the second"] :: [ByteString]

  forM_ (zip payloads parties) $ \(payload, (addr, who)) -> do
    pkt <- impostorPacket who (pke victim) fk payload
    sendTo bus (To victimAddr) (From addr) pkt
    got <- drain victim victimAddr
    assertEqual "a peer sharing a flow key is still readable" [payload] got
