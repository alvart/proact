{-
  @license MIT
  Pairing.purs
-}

module Proact.Functor.Pairing
  ( class Pairing
  , class PairingM
  , pair
  , pairM
  , sym
  , zap
  )
where

import Control.Comonad.Env (EnvT(..))
import Control.Comonad.Traced (TracedT(..))
import Control.Comonad.Store (StoreT(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (StateT(..))
import Control.Monad.Writer (WriterT(..))
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..))
import Prelude
import Proact.Comonad.Trans.Cofree (CofreeT(..), CofreeF(..))
import Proact.Monad.Trans.Free (FreeT(..), FreeF(..))

-- | Represents all pairings between functors f and g.
-- | This asserts that any sums in f can annihilate any products in g, and vice
-- | versa.
class Pairing f g | f -> g, g -> f
  where
  -- | Pairs two functors given a pairing function.
  pair :: forall a b c . (a -> b -> c) -> f a -> g b -> c

-- | Represents a pairing with side effects.
class PairingM f g m | f -> g, g -> f
  where
  -- | Pairs two functors given a pairing function with side effects.
  pairM :: forall a b c . (a -> b -> m c) -> f a -> g b -> m c

-- (Identity, Identity) :: Pairing
instance identityIdentityPairing :: Pairing Identity Identity
  where
  pair f (Identity a) (Identity b) = f a b

-- (EnvT, ReaderT) :: Pairing
instance envTReaderTPairing :: (Pairing f g) => Pairing (EnvT e f) (ReaderT e g)
  where
  pair f (EnvT (Tuple e fb)) (ReaderT reader) = pair f fb (reader e)

-- (TracedT, WriterT) :: Pairing
instance tracedTWriterTPairing
  :: (Pairing f g) => Pairing (TracedT t f) (WriterT t g)
  where
  pair f (TracedT ft) (WriterT writer) =
    pair (\f1 (Tuple a w) -> f (f1 w) a) ft writer

-- (StoreT, StateT) :: Pairing
instance storeTStateTPairing
  :: (Pairing f g) => Pairing (StoreT s f) (StateT s g)
  where
  pair f (StoreT (Tuple fs s)) (StateT state) =
    pair (\f1 (Tuple a s1) -> f (f1 s1) a) fs (state s)

-- (CofreeT, FreeT) :: Pairing
instance cofreeTFreeTPairing
  :: (Pairing f g, Pairing w m) => Pairing (CofreeT f w) (FreeT g m)
  where
  pair f (CofreeT cofreeT) (FreeT freeT) = pair pairFree cofreeT freeT
    where
    pairFree (CofreeF a _) (Pure b) = f a b
    pairFree (CofreeF _ fa) (Free gb) = pair (pair f) (fa unit) (gb unit)

-- | All pairings are symmetric.
sym :: forall f g a b c . Pairing f g => (a -> b -> c) -> g a -> f b -> c
sym f ga fb = pair (flip f) fb ga

-- | Applies the pair of a function and an argument.
zap :: forall f g a b . Pairing f g => f (a -> b) -> g a -> b
zap = pair ($)
