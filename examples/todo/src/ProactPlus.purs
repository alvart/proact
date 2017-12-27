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
import Proact (Dispatcher, EventHandler, EventFx)

-- | An type synonym for React event handlers.
type ReactHandler fx event = event -> Eff (EventFx fx) Unit

-- | A type synonym for an event dispatcher that is accessible from a Component.
-- | An event object may be later provided to the handler to trigger an event
-- | action.
type EventDispatcher fx state =
  forall event
   . (event -> EventHandler fx state Unit)
  -> (event -> Eff (EventFx fx) Unit)

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

-- | Transforms a Proact dispatcher into an event dispatcher.
withEvent :: forall fx state . Dispatcher fx state -> EventDispatcher fx state
withEvent dispatcher eventDispatcher = dispatcher <<< eventDispatcher
