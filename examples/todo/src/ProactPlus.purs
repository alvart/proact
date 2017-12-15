{-
  @license MIT
  ProactPlus.purs
-}

module ProactPlus
where

import Control.Monad.Eff (Eff)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Lens (Getter, Iso, (^.))
import Prelude
import Proact (ReactContext)

-- | An type synonym for React event handlers.
type ReactHandler fx event = event -> Eff (ReactContext fx) Unit

-- | An alias for arrow left composition.
o :: forall a b c d . Semigroupoid a => a c d -> a b c -> a b d
o = (<<<)

infixr 9 o as ..

-- | An alias for the endofunctor identity.
_this :: forall _in out . Iso _in out _in out
_this = id

-- | An implementation of `use` from the Data.Lens library working in the
-- | `MonadAsk` context.
use' :: forall s t a b m . MonadAsk s m => Getter s t a b -> m a
use' p = asks (_ ^. p)
