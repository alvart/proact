{-
  @license MIT
  Free.purs
-}

module Proact.Monad.Trans.Free
  ( module Proact.Monad.Class.MonadFree
  , module Proact.Trans.Class.Hoist
  , module Proact.Trans.Class.Interpret
  , FreeF(..)
  , FreeT(..)
  , iterT
  , runFreeT
  )
where

import Control.Apply (lift2)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Prelude
import Proact.Monad.Class.MonadFree (class MonadFree)
import Proact.Trans.Class.Hoist (class HoistT, hoist)
import Proact.Trans.Class.Interpret (class InterpretT, interpret)

-- | Represents the base Functor for a Free Monad.
data FreeF f a b = Pure a | Free (Unit -> f b)

-- | Represents the Free Monad Transformer for a Functor `f` and a Monad `m`.
newtype FreeT f m a = FreeT (m (FreeF f a (FreeT f m a)))

-- FreeF :: Functor, Bifunctor
derive instance functorFreeF :: (Functor f) => Functor (FreeF f a)

instance bifunctorFreeT :: (Functor f) => Bifunctor (FreeF f)
  where
  bimap f _ (Pure a) = Pure (f a)
  bimap _ g (Free fb) = Free (map g <$> fb)

-- FreeT
--   :: Newtype
--    , Semigroup
--    , Monoid
--    , Functor
--    , Apply
--    , Applicative
--    , Bind
--    , Monad
--    , MonadTrans
--    , MonadFree
--    , HoistT
--    , InterpretT
derive instance newtypeFreeT :: Newtype (FreeT f m a) _

instance functorFreeT :: (Functor f, Functor m) => Functor (FreeT f m)
  where
  map f (FreeT m) = FreeT (bimap f (map f) <$> m)

instance semigroupFreeT
  :: (Functor f, Monad m, Semigroup a) => Semigroup (FreeT f m a)
  where
  append = lift2 append

instance monoidFreeT :: (Functor f, Monad m, Monoid a) => Monoid (FreeT f m a)
  where
  mempty = FreeT $ pure $ Pure mempty

instance applyFreeT :: (Functor f, Monad m) => Apply (FreeT f m)
  where
  apply = ap

instance applicativeFreeT :: (Functor f, Monad m) => Applicative (FreeT f m)
  where
  pure = FreeT <<< pure <<< Pure

instance bindFreeT :: (Functor f, Monad m) => Bind (FreeT f m)
  where
  bind (FreeT m) f = FreeT $ bind m bindStep
    where
    bindStep (Pure a) = runFreeT (f a)
    bindStep (Free step) = pure $ Free $ map (_ >>= f) <$> step

instance monadFreeT :: (Functor f, Monad m) => Monad (FreeT f m)

instance monadTransFreeT :: (Functor f) => MonadTrans (FreeT f)
  where
  lift = FreeT <<< map Pure

instance monadFreeFreeT :: (Functor f, Monad m) => MonadFree f (FreeT f m)
  where
  layer = FreeT <<< pure <<< Free <<< const

  liftFree = FreeT <<< pure <<< Free <<< const <<< map pure

instance hoistTFreeT :: (Functor f) => HoistT (FreeT f)
  where
  hoist f = FreeT <<< map (map (hoist f)) <<< f <<< runFreeT

instance interpretTFreeT :: InterpretT (FreeT)
  where
  interpret f = FreeT <<< map interpretF <<< runFreeT
    where
    interpretF (Pure a) = Pure a
    interpretF (Free fb) = Free \_ -> map (interpret f) $ f $ fb unit

-- | Folds a `FreeT` Monad Transformer into its underlying Monad using an
-- | Algebra.
iterT
  :: forall f m a
   . Functor f => Monad m => (f (m a) -> m a) -> FreeT f m a -> m a
iterT f (FreeT m) =
  do
  free <- m
  let step = map (iterT f) free
  case step
    of
    Pure a -> pure a
    Free next -> f $ next unit

-- | Deconstructs a `FreeT` Monad Transformer.
runFreeT :: forall f m a . FreeT f m a -> m (FreeF f a (FreeT f m a))
runFreeT = unwrap
