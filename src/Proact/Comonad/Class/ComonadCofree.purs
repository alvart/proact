{-
  @license MIT
  ComonadCofree.purs
-}

module Proact.Comonad.Class.ComonadCofree
  ( class ComonadCofree
  , lowerCofree
  , peel
  )
where

import Control.Comonad (class Comonad)
import Prelude

-- | Represents all Comonads that do no work during the extension step beyond
-- | simply extracting two comonadic values together.
class (Functor f, Comonad w) <= ComonadCofree f w | w -> f
  where
  -- | Lowers the Cofree Comonad into its Cofree container.
  lowerCofree :: forall a . w a -> f a

  -- | Peels a layer off a Cofree Comonad.
  peel :: forall a . w a -> f (w a)
