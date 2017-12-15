{-
  @license MIT
  Utility.purs
-}

module Utility
where

import Control.Monad.Eff (Eff)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Lens (Getter, (^.))
import Prelude
import Proact (ReactContext)

-- | An type synonym for React event handlers.
type ReactHandler fx event = event -> Eff (ReactContext fx) Unit

-- | An alias for arrow left composition.
o :: forall a b c d . Semigroupoid a => a c d -> a b c -> a b d
o = (<<<)

-- | An implementation of `use` from the Data.Lens library working in the
-- | `MonadAsk` context.
use' :: forall s t a b m . MonadAsk s m => Getter s t a b -> m a
use' p = asks (_ ^. p)

infixr 9 o as ..
