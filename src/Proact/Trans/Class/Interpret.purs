{-
  @license MIT
  Interpret.purs
-}

module Proact.Trans.Class.Interpret
  ( class InterpretT
  , interpret
  )
where

import Prelude

-- | Represents all transformers' transformers that support a natural
-- | transformation.
class InterpretT t
  where
  -- | Given a homomorphism from `f` to `g` this yields a homomorphism from
  -- | `t f m` to `t g m`.
  interpret
    :: forall f g
     . Functor f
    => Functor g
    => (f ~> g) -> (forall m . Functor m => t f m ~> t g m)
