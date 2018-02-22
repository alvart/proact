{-
  @license MIT
  Cofree.purs
-}

module Proact.Comonad.Trans.Cofree
  ( module Proact.Comonad.Class.ComonadCofree
  , module Proact.Trans.Class.Hoist
  , module Proact.Trans.Class.Interpret
  , CofreeF(..)
  , CofreeT(..)
  , coiterT
  , runCofreeT
  )
where

import Control.Comonad (class Comonad, class Extend, extend, extract)
import Control.Comonad.Trans.Class (class ComonadTrans)
import Data.Bifunctor (class Bifunctor, bimap, rmap)
import Data.Newtype (class Newtype, unwrap)
import Proact.Comonad.Class.ComonadCofree (class ComonadCofree)
import Proact.Trans.Class.Hoist (class HoistT, hoist)
import Proact.Trans.Class.Interpret (class InterpretT, interpret)
import Prelude

data CofreeF f a b = CofreeF a (Unit -> (f b))

newtype CofreeT f w a = CofreeT (w (CofreeF f a (CofreeT f w a)))

-- CofreeF :: Functor, Bifunctor
derive instance functorCofreeF :: (Functor f) => Functor (CofreeF f a)

instance bifunctorCofreeF :: (Functor f) => Bifunctor (CofreeF f)
  where
  bimap f g (CofreeF a fb) = CofreeF (f a) (map g <$> fb)

-- CofreeT
--   :: Newtype
--    , Functor
--    , Extend
--    , Comonad
--    , ComonadTrans
--    , ComonadCofree
--    , HoistT
--    , InterpretT
derive instance newtypeCofreeT :: Newtype (CofreeT f w a) _

instance functorCofreeT :: (Functor f, Functor w) => Functor (CofreeT f w)
  where
  map f (CofreeT w) = CofreeT $ map (bimap f (map f)) w

instance extendCofreeT :: (Functor f, Comonad w) => Extend (CofreeT f w)
  where
  extend f = CofreeT <<< extend extendStep <<< runCofreeT
    where
    extendStep w =
      CofreeF (f (CofreeT w)) (const $ snd $ rmap (extend f) $ extract w)

instance comonadCofreeT :: (Functor f, Comonad w) => Comonad (CofreeT f w)
  where
  extract (CofreeT w) = fst (extract w)

instance comonadTransCofreeT :: ComonadTrans (CofreeT f)
  where
  lower = map fst <<< runCofreeT

instance comonadCofreeCofreeT
  :: (Functor f, Comonad w) => ComonadCofree f (CofreeT f w)
  where
  lowerCofree (CofreeT w) = map extract $ snd $ extract w

  peel = snd <<< extract <<< runCofreeT

instance hoistTCofreeT :: (Functor f) => HoistT (CofreeT f)
  where
  hoist f = CofreeT <<< map (rmap (hoist f)) <<< f <<< runCofreeT

instance interpretTCofreeT :: InterpretT (CofreeT)
  where
  interpret f = CofreeT <<< map interpretF <<< runCofreeT
    where
    interpretF (CofreeF a fb) =
      CofreeF a \_ -> map (interpret f) $ f $ fb unit

-- | Unfolds a `CofreeT` Comonad Transformer from a Coalgebra and an initial
-- | Comonad.
coiterT
  :: forall f w a
   . Functor f => Comonad w => (w a -> f (w a)) -> w a -> CofreeT f w a
coiterT f =
  CofreeT <<< extend (\w -> CofreeF (extract w) (\_ -> map (coiterT f) (f w)))

-- | Deconstructs a `CofreeT` Comonad Transformer.
runCofreeT :: forall f w a . CofreeT f w a -> w (CofreeF f a (CofreeT f w a))
runCofreeT = unwrap

-- Returns the first item of the `CofreeF` tuple.
fst :: forall f a b . CofreeF f a b -> a
fst (CofreeF a _) = a

-- Returns the second item of the `CofreeF` tuple.
snd :: forall f a b . CofreeF f a b -> f b
snd (CofreeF _ fb) = fb unit
