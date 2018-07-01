{-
  @license MIT
  Proact.purs
-}

-- | Proact is the core library for a family of web frameworks built from
-- | composable components that share a singular state which is composed and
-- | decomposed through Profunctor lenses. This is unlike other functional web
-- | architectures that compose state through user-provided messages that become
-- | boilerplate code in the long run.
-- | Proact also separates the pure elements of a program from its side effects
-- | by using Free commands which are later paired with Cofree actions to create
-- | executable functions.  This strategy was chiefly derived from the
-- | "Free for DSLs, cofree for interpreters" series by Dave Laing.
-- |
-- | A web framework using Facebook's React library is provided in this package.

module Proact
  ( ComponentT(..)
  , EventHandlerT(..)
  , VaultT(..)
  , cohoist
  , cointerpret
  , dispatch
  , dispatcher
  , focus
  , focus'
  , focusVault
  , iFocus
  , render
  )
where

import Control.Apply (lift2)
import Control.Comonad (class Comonad, (<<=), extract)
import Control.Comonad.Store (StoreT(..), pos, seek)
import Control.Comonad.Trans.Class (lower)
import Control.Monad.Reader
  (class MonadAsk, ReaderT(..), ask, runReaderT, withReaderT)
import Control.Monad.State (class MonadState, StateT, runStateT, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Bifunctor (rmap)
import Data.Lens
  ( IndexedTraversal'
  , Lens'
  , Traversal'
  , Forget(..)
  , Indexed(..)
  , element
  , preview
  , set
  , view
  )
import Data.Lens.Index (class Index, ix)
import Data.Lens.Indexed (positions)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Tuple (Tuple(..), snd)
import Prelude
import Proact.Comonad.Class.ComonadCofree (peel)
import Proact.Comonad.Trans.Cofree (CofreeT(..), runCofreeT)
import Proact.Functor.Pairing (class Pairing, class PairingM, pair, pairM)
import Proact.Monad.Class.MonadFree (layer, liftFree)
import Proact.Monad.Trans.Free (class MonadFree, FreeF(..), FreeT, runFreeT)
import Proact.Trans.Class.Hoist (class HoistT, hoist)
import Proact.Trans.Class.Interpret (class InterpretT, interpret)

-- | A monadic representation of a component that provides access to its
-- | underlying state through the `MonadAsk` interface.
newtype ComponentT s t n f w g m a =
  ComponentT (ReaderT (VaultT t n f w Unit) (FreeT g (ReaderT s m)) a)

-- | A monadic representation of an event handler that manipulates the
-- | component's state through the `MonadState` interface.
newtype EventHandlerT s f m a = EventHandlerT (FreeT f (StateT s m) a)

-- | Represents a state repository built with a Comonad Transformer.
newtype VaultT s m f w a = VaultT (CofreeT f (StoreT (Maybe s) w) (m a))

-- ComponentT
--   :: Newtype
--    , Functor
--    , Apply
--    , Applicative
--    , Bind
--    , Monad
--    , MonadAsk
--    , MonadTrans
--    , MonadFree
--    , HoistT
--    , InterpretT
derive instance newtypeComponentT :: Newtype (ComponentT s t n f w g m a) _

instance functorComponentT
  :: (Functor g, Functor m) => Functor (ComponentT s t n f w g m)
  where
  map f (ComponentT fa) = ComponentT $ map f fa

instance applyComponentT
  :: (Functor g, Monad m) => Apply (ComponentT s t n f w g m)
  where
  apply = ap

instance applicativeComponentT
  :: (Functor g, Monad m) => Applicative (ComponentT s t n f w g m)
  where
  pure = ComponentT <<< pure

instance bindComponentT
  :: (Functor g, Monad m) => Bind (ComponentT s t n f w g m)
  where
  bind (ComponentT ma) f = ComponentT $ map f ma >>= unwrap

instance monadComponentT
  :: (Functor g, Monad m) => Monad (ComponentT s t n f w g m)

instance monadAskComponentT
  :: (Functor g, Monad m) => MonadAsk s (ComponentT s t n f w g m)
  where
  ask = ComponentT $ lift $ lift ask

instance monadTransComponentT
  :: (Functor g) => MonadTrans (ComponentT s t n f w g)
  where
  lift = ComponentT <<< lift <<< lift <<< lift

instance monadFreeComponentT
  :: (Functor g, Monad m) => MonadFree g (ComponentT s t n f w g m)
  where
  layer gma =
    ComponentT
      do
      vault <- ask
      lift $ layer $ flip runReaderT vault <<< unwrap <$> gma

  liftFree = ComponentT <<< lift <<< liftFree

instance hoistTComponentT :: (Functor g) => HoistT (ComponentT s t n f w g)
  where
  hoist f (ComponentT component) =
    ComponentT
      $ hoist (\freeT -> hoist (\readerT -> hoist f readerT) freeT) component

instance interpretTComponentT :: InterpretT (ComponentT s t n f w)
  where
  interpret f (ComponentT component) =
    ComponentT $ hoist (\freeT -> interpret f freeT) component

-- EventHandlerT
--   :: Newtype
--    , Functor
--    , Apply
--    , Applicative
--    , Bind
--    , Monad
--    , MonadAsk
--    , MonadState
--    , MonadTrans
--    , MonadFree
--    , HoistT
--    , InterpretT
derive instance newtypeEventHandlerT :: Newtype (EventHandlerT s f m a) _

instance functorEventHandlerT
  :: (Functor f, Functor m) => Functor (EventHandlerT s f m)
  where
  map f (EventHandlerT fa) = EventHandlerT $ map f fa

instance applyEventHandlerT
  :: (Functor f, Monad m) => Apply (EventHandlerT s f m)
  where
  apply = ap

instance applicativeEventHandlerT
  :: (Functor f, Monad m) => Applicative (EventHandlerT s f m)
  where
  pure = EventHandlerT <<< pure

instance bindEventHandlerT
  :: (Functor f, Monad m) => Bind (EventHandlerT s f m)
  where
  bind (EventHandlerT ma) f = EventHandlerT $ map f ma >>= unwrap

instance monadEventHandlerT
  :: (Functor f, Monad m) => Monad (EventHandlerT s f m)

instance monadStateEventHandlerT
  :: (Functor f, Monad m) => MonadState s (EventHandlerT s f m)
  where
  state = EventHandlerT <<< lift <<< state

instance monadTransEventHandlerT
  :: (Functor f, Monad m) => MonadTrans (EventHandlerT s f)
  where
  lift = EventHandlerT <<< lift <<< lift

instance monadFreeEventHandlerT
  :: (Functor f, Monad m) => MonadFree f (EventHandlerT s f m)
  where
  layer = EventHandlerT <<< layer <<< map unwrap

  liftFree = EventHandlerT <<< liftFree

instance hoistTEventHandlerT :: (Functor f) => HoistT (EventHandlerT s f)
  where
  hoist f (EventHandlerT eventHandler) =
    EventHandlerT $ hoist (\stateT -> hoist f stateT) eventHandler

instance interpretTEventHandlerT :: InterpretT (EventHandlerT s)
  where
  interpret f (EventHandlerT eventHandler) =
    EventHandlerT $ interpret f eventHandler

-- VaultT :: Newtype, HoistT, InterpretT
derive instance newtypeVaultT :: Newtype (VaultT s m f w a) _

instance hoistTVaultT :: (Functor f) => HoistT (VaultT s m f)
  where
  hoist f (VaultT vault) =
    VaultT $ hoist (\storeT -> hoist f storeT) vault

instance interpretTVaultT :: InterpretT (VaultT s m)
  where
  interpret f (VaultT vault) = VaultT $ interpret f vault

-- | Applies a hoist transformation to the state repository of a `ComponentT`.
cohoist
  :: forall s t n f w v g m a
   . Functor f
  => Functor w
  => Functor v
  => (w ~> v) -> ComponentT s t n f v g m a -> ComponentT s t n f w g m a
cohoist f (ComponentT component) =
  ComponentT $ withReaderT (\vault -> hoist f vault) component

-- | Applies an interpret transformation to the state repository of a
-- | `ComponentT`.
cointerpret
  :: forall s t n f h w g m a
   . Functor f
  => Functor h
  => Functor w
  => (f ~> h) -> ComponentT s t n h w g m a -> ComponentT s t n f w g m a
cointerpret f (ComponentT component) =
  ComponentT $ withReaderT (\vault -> interpret f vault) component

-- | Pairs a `CofreeT` vault with a `FreeT` event handler to trigger an event
-- | action.
dispatch
  :: forall s n f w g m
   . Functor f
  => Functor g
  => Comonad w
  => Monad m
  => Pairing w m
  => PairingM f g n
  => Monad n
  => VaultT s n f w Unit
  -> EventHandlerT s g m Unit
  -> n Unit
dispatch vault eventHandler = dispatch' (unwrap vault) (unwrap eventHandler)
  where
  dispatch' cofreeT freeT =
    do
    let stateT = runFreeT freeT
    let storeT = runCofreeT cofreeT
    maybe (pure unit) (dispatch'' storeT stateT) (pos storeT)

  dispatch'' storeT stateT _state =
    do
    let cofreeT = CofreeT $ seek (Just state') storeT
    extract cofreeT
    case step
      of
      Pure _ -> pure unit
      Free next -> pairM dispatch' (peel cofreeT) (next unit)
    where
    Tuple step state' =
      pair (flip const) (lower storeT) (runStateT stateT _state)

-- | Provides an action dispatcher in the context of a Proact `ComponentT`.
dispatcher
  :: forall s t n f w g m
   . Functor f
  => Functor g
  => Comonad w
  => Monad m
  => Pairing w m
  => PairingM f g n
  => Monad n
  => ComponentT s t n f w g m (EventHandlerT t g m Unit -> n Unit)
dispatcher = ComponentT $ map dispatch ask

-- | Changes a `ComponentT`'s state type through the lens of a `Traversal`.
-- | For a less restrictive albeit less general version, consider `focus'`.
focus
  :: forall s1 s2 n f w g m a
   . Functor f
  => Functor g
  => Comonad w
  => Monad m
  => Monoid a
  => Traversal' s1 s2
  -> ComponentT s2 s2 n f w g m a
  -> ComponentT s1 s1 n f w g m a
focus _traversal (ComponentT component) =
  ComponentT $ ReaderT $ focusFree <<< runReaderT focusEnvironment
  where
  focusFree freeT = lift ask >>= hoist lift <<< unwrap focusForget
    where
    focusForget =
      positions _traversal $ Indexed $ Forget $ lowerFreeReader freeT

  iFocusVault i = over VaultT focusCofree
    where
    focusCofree cofreeT =
      CofreeT $ StoreT $ Tuple (_peek <<< extract <<= w1) _pos
      where
      StoreT (Tuple w1 ms1) = runCofreeT cofreeT

      _peek f1 ms2 =
        rmap focusCofree $ f1 $ lift2 (set (element i _traversal)) ms2 ms1

      _pos = ms1 >>= preview (element i _traversal)

  focusEnvironment =
    do
    Tuple i _ <- lift $ lift ask
    withReaderT (iFocusVault i) $ hoist (hoist (withReaderT snd)) component

-- | Changes a `ComponentT`'s state type through a `Lens`.
-- | For a more general albeit more restrictive version, consider `focus`.
focus'
  :: forall s1 s2 n f w g m a
   . Functor f
  => Functor g
  => Comonad w
  => Monad m
  => Lens' s1 s2 -> ComponentT s2 s2 n f w g m a -> ComponentT s1 s1 n f w g m a
focus' _lens (ComponentT component) =
  ComponentT $ ReaderT $ focusFree <<< runReaderT focusEnvironment
  where
  focusFree freeT = lift ask >>= hoist lift <<< unwrap focusForget
    where
    focusForget = _lens $ Forget $ lowerFreeReader freeT

  focusEnvironment = withReaderT (focusVault _lens) component

-- | Changes the type of the state repository through a `Lens`.
focusVault
  :: forall s1 s2 f w m a
   . Functor f
  => Comonad w
  => Lens' s1 s2 -> VaultT s1 m f w a -> VaultT s2 m f w a
focusVault _lens = over VaultT focusCofree
  where
  focusCofree cofreeT =
    CofreeT $ StoreT $ Tuple (_peek <<< extract <<= w1) _pos
    where
    StoreT (Tuple w1 ms1) = runCofreeT cofreeT

    _peek f1 ms2 = rmap focusCofree $ f1 $ lift2 (set _lens) ms2 ms1

    _pos = map (view _lens) ms1

-- | Changes a component's state type through the lens of an indexed traversal.
iFocus
  :: forall s1 s2 i n f w g m a
   . Functor f
  => Functor g
  => Comonad w
  => Monad m
  => Monoid a
  => Index s1 i s2
  => IndexedTraversal' i s1 s2
  -> ComponentT (Tuple i s2) s2 n f w g m a
  -> ComponentT s1 s1 n f w g m a
iFocus _iTraversal (ComponentT component) =
  ComponentT $ ReaderT $ focusFree <<< runReaderT focusEnvironment
  where
  focusFree freeT = lift ask >>= hoist lift <<< unwrap focusForget
    where
    focusForget = _iTraversal $ Indexed $ Forget $ lowerFreeReader freeT

  iFocusVault i = over VaultT focusCofree
    where
    focusCofree cofreeT =
      CofreeT $ StoreT $ Tuple (_peek <<< extract <<= w1) _pos
      where
      StoreT (Tuple w1 ms1) = runCofreeT cofreeT

      _peek f1 ms2 = rmap focusCofree $ f1 $ lift2 (set (ix i)) ms2 ms1

      _pos = ms1 >>= preview (ix i)

  focusEnvironment =
    do
    Tuple i _ <- lift $ lift ask
    withReaderT (iFocusVault i) component

-- | Renders a `ComponentT` in a monadic context.
render
  :: forall s n f w g m a
   . Functor f
  => Functor g
  => Comonad w
  => Monad m
  => Monad n
  => PairingM f g n
  => Pairing w m
  => Monoid a
  => VaultT s n f w Unit
  -> VaultT s n f w Unit
  -> ComponentT s s n f w g m a
  -> n a
render renderVault dispatchVault component =
  render' (unwrap renderVault) $ runReaderT (unwrap component) dispatchVault
  where
  render' cofreeT freeT =
    do
    let stateT = runFreeT freeT
    let storeT = runCofreeT cofreeT
    maybe (pure mempty) (render'' storeT stateT) (pos storeT)

  render'' storeT stateT _state =
    do
    let cofreeT = CofreeT storeT
    extract cofreeT
    case step
      of
      Pure a -> pure a
      Free next -> pairM render' (peel cofreeT) (next unit)
    where
    step = pair (flip const) (lower storeT) (runReaderT stateT _state)

-- Lowers a `ReaderT` monad transformer.
lowerFreeReader
  :: forall s f m a
   . Functor f => Functor m => FreeT f (ReaderT s m) a -> s -> FreeT f m a
lowerFreeReader freeT s2 = hoist (\reader -> runReaderT reader s2) freeT
