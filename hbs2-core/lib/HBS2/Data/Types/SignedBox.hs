{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module HBS2.Data.Types.SignedBox where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Types
import HBS2.Net.Auth.Credentials

import Codec.Serialise
import Data.Hashable
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Control.Monad.Trans.Maybe
import Control.Monad.Identity

-- | WIRE FORMAT, FROZEN BY hbs2-hub EVENT IDS.
--
-- This is an ordinary derived-'Serialise' record and it does not look
-- load-bearing, so the note has to be here rather than in the package that
-- depends on it. A hub event-id is @hashObject (serialise (SignedBox pk bs
-- sig))@ -- the framing of THIS type, not only the payload inside it -- and
-- those ids are what threads name their replies by, what redactions name their
-- targets by, and what canon boxes bind to. Changing the encoding (a field, a
-- record, an algorithm tag, strict to lazy) rewrites every event-id in every
-- repository in the world, and nothing in either package's tests would notice:
-- hbs2-hub's golden fixture pins the bytes INSIDE the box.
--
-- Anything that has to change here needs a new type beside it, not an edit.
data SignedBox p s =
  SignedBox (PubKey 'Sign s) ByteString (Signature s)
  deriving stock (Generic)

deriving stock instance
  ( Eq (PubKey 'Sign s)
  , Eq (Signature s)
  ) => Eq (SignedBox p s)

instance ( Eq (PubKey 'Sign s)
         , Eq (Signature s)
         , Serialise (SignedBox p s)
         ) =>  Hashable (SignedBox p s) where
  hashWithSalt salt box = hashWithSalt salt (serialise box)


type ForSignedBox s = ( Serialise ( PubKey 'Sign s)
                      , FromStringMaybe (PubKey 'Sign s)
                      , Serialise (Signature s)
                      , Signatures s
                      , Eq (Signature s)
                      , Hashable (PubKey 'Sign s)
                      )

instance ForSignedBox s => Serialise (SignedBox p s)

makeSignedBox :: forall s p . (Serialise p, ForSignedBox s, Signatures s)
              => PubKey 'Sign s
              -> PrivKey 'Sign s
              -> p
              -> SignedBox p s

makeSignedBox pk sk msg = SignedBox @p @s pk bs sign
  where
    bs = LBS.toStrict (serialise msg)
    sign = makeSign @s sk bs


unboxSignedBox0 :: forall p s . (Serialise p, ForSignedBox s, Signatures s)
               => SignedBox p s
               -> Maybe (PubKey 'Sign s, p)

unboxSignedBox0 (SignedBox pk bs sign) = runIdentity $ runMaybeT do
  guard $ verifySign @s pk sign bs
  p <- MaybeT $ pure $ deserialiseOrFail @p (LBS.fromStrict bs) & either (const Nothing) Just
  pure (pk, p)

unboxSignedBox :: forall p s . (Serialise p, ForSignedBox s, Signatures s)
               => LBS.ByteString
               -> Maybe (PubKey 'Sign s, p)

unboxSignedBox bs = runIdentity $ runMaybeT do

  box <- MaybeT $ pure $ deserialiseOrFail @(SignedBox p s) bs
                          & either (pure Nothing) Just

  MaybeT $ pure $ unboxSignedBox0 box

