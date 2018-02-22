{-
  @license MIT
  MonadFree.purs
-}

module Proact.Monad.Class.MonadFree
  ( class MonadFree
  , layer
  , liftFree
  )
where

import Prelude

-- | Represents all Monads that do no work during the normalization step beyond
-- | simply grafting two monadic values together.
class (Functor f, Monad m) <= MonadFree f m | m -> f
  where
-- | Adds a layer to a Free Monad.
  layer :: forall a . f (m a) -> m a

-- | Lifts the Free container to a Free Monad.
  liftFree :: forall a . f a -> m a
