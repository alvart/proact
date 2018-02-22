{-
  @license MIT
  React.purs
-}

module Proact.React
  ( module ProactExports
  , ComponentT
  , Dispatcher
  , IndexedComponentT
  , ReactEff
  , dispatch
  , dispatcher
  , spec
  )
where

import Control.Comonad (class Comonad)
import Control.Comonad.Store (StoreT(..), pos, seek)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.IOSync (IOSync, runIOSync)
import Data.Array (head, singleton)
import Data.Lens (Lens')
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Prelude
import Proact
  ( ComponentT
  , EventHandlerT
  , VaultT(..)
  , dispatch
  , dispatcher
  , focusVault
  , render
  ) as P
import Proact (EventHandlerT(..), focus, focus', iFocus) as ProactExports
import Proact.Functor.Pairing (class Pairing, class PairingM)
import Proact.Comonad.Trans.Cofree (coiterT)
import React
  ( ReactSpec
  , ReadOnly
  , ReadWrite
  , ReactElement
  , ReactProps
  , ReactRefs
  , ReactState
  , ReactThis
  , readState
  , spec
  , spec'
  , writeState
  ) as React
import React.DOM (div') as React
import Unsafe.Coerce (unsafeCoerce)

-- | A type synonym for a React Component.
type ComponentT s f w g m = P.ComponentT s s IOSync f w g m

-- | A type synonym for a dispatcher that is accessible from a React Component
-- | and that executes the actions of an event handler provided to it.
type Dispatcher s f m a = P.EventHandlerT s f m a -> ReactEff a

-- | A type synonym for an Indexed React Component.
type IndexedComponentT i s f w g m = P.ComponentT (Tuple i s) s IOSync f w g m

-- | A type synonym for effects coming from React elements.
type ReactEff =
  Eff
    ( props :: React.ReactProps
    , refs :: React.ReactRefs React.ReadOnly
    , state :: React.ReactState React.ReadWrite
    )

-- | A type synonym for a `StoreT` containing the application's state.
type ReactStoreT s w a = StoreT (Maybe s) w (IOSync a)

-- A type synonym for a React state repository.
type VaultT s f w = P.VaultT s IOSync f w

-- | Dispatches React actions detached from the context of a Component. The
-- | state handled by the actions is seen through a given lens.
dispatch
  :: forall s1 s2 f w g m
   . Functor f
  => Functor g
  => Comonad w
  => Monad m
  => Pairing w m
  => PairingM f g IOSync
  => (ReactStoreT s1 w Unit -> f (ReactStoreT s1 w Unit))
  -> ReactStoreT s1 w Unit
  -> React.ReactThis { } s1
  -> Lens' s1 s2
  -> Dispatcher s2 g m Unit
dispatch iterator start this _lens eventHandler =
  do
  let dispatchVault = P.focusVault _lens $ vault iterator start writeState
  unsafeCoerceEff $ runIOSync $ P.dispatch dispatchVault eventHandler
  where
  writeState f s1 =
    f s1 *> maybe (pure unit) (void <<< liftEff <<< React.writeState this) s1

-- | Provides a dispatcher in the context of a React Component.
dispatcher
  :: forall s t f w g m
   . Functor f
  => Functor g
  => Comonad w
  => Pairing w m
  => PairingM f g IOSync
  => Monad m
  => P.ComponentT s t IOSync f w g m (Dispatcher t g m Unit)
dispatcher = map (unsafeCoerceEff <<< runIOSync) <$> P.dispatcher

-- | Creates a `ReactSpec` from a Coalgebra and a React Component.
spec
  :: forall s e f w g m
   . Functor f
  => Functor g
  => Comonad w
  => Monad m
  => Pairing w m
  => PairingM f g IOSync
  => (ReactStoreT s w Unit -> f (ReactStoreT s w Unit))
  -> ReactStoreT s w Unit
  -> ComponentT s f w g m React.ReactElement
  -> React.ReactSpec { } s React.ReactElement e
spec iterator start component =
  case pos start
    of
    Just state -> React.spec state render
    Nothing ->
      React.spec' (const $ pure $ unsafeCoerce unit)
        $ const
        $ pure
        $ React.div' []
  where
  render this =
    do
    state <- unsafeCoerceEff $ React.readState this

    let start' = seek (Just state) start
    let renderVault = vault iterator start' id
    let dispatchVault = vault iterator start' writeState

    map (maybe (React.div' [ ]) id <<< head)
      $ unsafeCoerceEff
      $ runIOSync
      $ P.render renderVault dispatchVault
      $ map singleton component
    where
    writeState f s1 =
      f s1 *> maybe (pure unit) (void <<< liftEff <<< React.writeState this) s1

-- Builds a state repository.
vault
  :: forall s f w
   . Functor f
  => Comonad w
  => (ReactStoreT s w Unit -> f (ReactStoreT s w Unit))
  -> ReactStoreT s w Unit
  -> ((Maybe s -> IOSync Unit) -> Maybe s -> IOSync Unit)
  -> VaultT s f w Unit
vault iterator (StoreT (Tuple w start)) _peek =
  P.VaultT $ coiterT iterator $ StoreT $ Tuple (map _peek w) start
