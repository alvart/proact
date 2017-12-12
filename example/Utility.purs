{-
  @license MIT
  Utility.purs
-}

module Utility
where

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Lens (Getter, (^.))
import Data.Newtype (class Newtype, unwrap)
import Prelude

-- | An alias for arrow left composition.
o :: forall a b c d . Semigroupoid a => a c d -> a b c -> a b d
o = (<<<)

-- | Unwraps the return value of a function.
unwrap2 :: forall a b c . Newtype b c => (a -> b) -> (a -> c)
unwrap2 ab a = unwrap $ ab a

-- | Unwraps the return value of a function that receives two arguments.
unwrap3 :: forall a b c d . Newtype c d => (a -> b -> c) -> (a -> b -> d)
unwrap3 abc a b = unwrap $ abc a b

-- | An implementation of `use` from the Data.Lens library working in the
-- | `MonadAsk` context.
use' :: forall s t a b m . MonadAsk s m => Getter s t a b -> m a
use' p = asks (_ ^. p)

infixr 9 o as ..
