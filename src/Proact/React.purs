{-
  @license MIT
  React.purs
-}

module Proact.React
  ( module ProactExports
  , ComponentT
  , IndexedComponentT
  , dispatch
  , dispatch'
  , render
  )
where

import Control.Comonad (class Comonad)
import Control.Comonad.Store (StoreT(..), pos, seek)
import Data.Array (head, singleton)
import Data.Lens (Lens')
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Prelude
import Proact
  (ComponentT, EventHandlerT, VaultT(..), dispatch, focusVault, render) as P
import Proact
  (EventHandlerT(..), dispatcher, focus, focus', iFocus) as ProactExports
import Proact.Functor.Pairing (class Pairing, class PairingM)
import Proact.Comonad.Trans.Cofree (coiterT)
import React (ReactElement, ReactThis, writeState) as React
import React.DOM (div') as React

-- | A type synonym for a React Component.
type ComponentT s f w g m = P.ComponentT s s Effect f w g m

-- | A type synonym for an Indexed React Component.
type IndexedComponentT i s f w g m = P.ComponentT (Tuple i s) s Effect f w g m

-- | A type synonym for a `StoreT` containing the application's state.
type ReactStoreT s w a = StoreT (Maybe s) w (Effect a)

-- A type synonym for a React state repository.
type VaultT s f w = P.VaultT s Effect f w

-- | Dispatches React actions detached from the context of a Component. The
-- | state handled by the actions is seen through a given lens.
dispatch
  :: forall s1 s2 f w g m
   . Functor f
  => Functor g
  => Comonad w
  => Monad m
  => Pairing w m
  => PairingM f g Effect
  => Lens' { | s1 } { | s2 }
  -> (ReactStoreT { | s1 } w Unit -> f (ReactStoreT { | s1 } w Unit))
  -> ReactStoreT { | s1 } w Unit
  -> React.ReactThis { } { | s1 }
  -> P.EventHandlerT { | s2 } g m Unit
  -> Effect Unit
dispatch _lens = dispatch'' (P.focusVault _lens)

-- | Dispatches React actions detached from the context of a Component.
dispatch'
  :: forall s f w g m
   . Functor f
  => Functor g
  => Comonad w
  => Monad m
  => Pairing w m
  => PairingM f g Effect
  => (ReactStoreT { | s } w Unit -> f (ReactStoreT { | s } w Unit))
  -> ReactStoreT { | s } w Unit
  -> React.ReactThis { } { | s }
  -> P.EventHandlerT { | s } g m Unit
  -> Effect Unit
dispatch' = dispatch'' identity

-- | Renders a `ReactElement` from a React Context, a Coalgebra and a Proact
-- | Component.
render
  :: forall s f w g m
   . Functor f
  => Functor g
  => Comonad w
  => Monad m
  => Pairing w m
  => PairingM f g Effect
  => (ReactStoreT { | s } w Unit -> f (ReactStoreT { | s } w Unit))
  -> ReactStoreT { | s } w Unit
  -> ComponentT { | s } f w g m React.ReactElement
  -> React.ReactThis { } { | s }
  -> Effect React.ReactElement
render iterator start component this =
  case pos start
    of
    Just state ->
      do
      let start' = seek (Just state) start
      let dispatchVault = vault iterator start' $ writeState this
      let renderVault = vault iterator start' identity

      map (maybe (React.div' [ ]) identity <<< head)
        $ P.render renderVault dispatchVault
        $ map singleton component
    Nothing -> pure $ React.div' []

-- Dispatches React actions detached from the context of a Component. The state
-- handled by the React context is converted to a state type supported by the
-- vault via a transformation function.
dispatch''
  :: forall s1 s2 f w g m
   . Functor f
  => Functor g
  => Comonad w
  => Monad m
  => Pairing w m
  => PairingM f g Effect
  => (P.VaultT { | s1 } Effect f w Unit -> P.VaultT { | s2 } Effect f w Unit)
  -> (ReactStoreT { | s1 } w Unit -> f (ReactStoreT { | s1 } w Unit))
  -> ReactStoreT { | s1 } w Unit
  -> React.ReactThis { } { | s1 }
  -> P.EventHandlerT { | s2 } g m Unit
  -> Effect Unit
dispatch'' vaultStateConverter iterator start this = P.dispatch dispatchVault
  where
  dispatchVault = vaultStateConverter $ vault iterator start $ writeState this

-- Builds a state repository.
vault
  :: forall s f w
   . Functor f
  => Comonad w
  => (ReactStoreT s w Unit -> f (ReactStoreT s w Unit))
  -> ReactStoreT s w Unit
  -> ((Maybe s -> Effect Unit) -> Maybe s -> Effect Unit)
  -> VaultT s f w Unit
vault iterator (StoreT (Tuple w start)) _peek =
  P.VaultT $ coiterT iterator $ StoreT $ Tuple (map _peek w) start

-- Extends the action of writing a state by also writing it into a React
-- context.
writeState
  :: forall s
   . React.ReactThis { } { | s }
  -> (Maybe { | s } -> Effect Unit)
  -> Maybe { | s }
  -> Effect Unit
writeState this f s =
  f s *> maybe (pure unit) (void <<< React.writeState this) s
