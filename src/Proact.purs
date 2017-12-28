{-
  @license MIT
  Proact.purs
-}

-- | Proact is a web framework that provides a model-view-dispatcher abstraction
-- | on top of `purescript-react`. It exposes a clean monadic API to construct
-- | and compose components and to define event handlers with minimal code
-- | overhead. Unlike other dispatch architectures that use messages to
-- | communicate components, Proact dispatches event actions themselves which
-- | means there is less boilerplate code and more rapid development.

module Proact
( class Proactive
, Component
, EventHandler
, IndexedComponent
, This(ReactThis)
, Dispatcher
, EventFx
, dispatcher
, dispatcher'
, focus
, focus'
, focusThis
, iFocus
, readState
, spec
)
where

import Control.Apply (lift2)
import Control.Coroutine (Consumer, Producer, Await(..), Emit(..), await, emit)
import Control.Monad.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free.Trans (resume)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader
  (class MonadAsk, ReaderT, ask, mapReaderT, runReaderT, withReaderT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (class MonadState)
import Control.Monad.Trans.Class (lift)
import Data.Const (Const(..))
import Data.Either (Either(..), either)
import Data.Lens
  (class Wander, IndexedTraversal', Lens', Traversal', element, preview, set)
import Data.Lens.Index (class Index, ix)
import Data.Lens.Indexed (positions)
import Data.Lens.Internal.Indexed (Indexed(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude
import React
  ( ReactElement
  , ReactProps
  , ReactRefs
  , ReactSpec
  , ReactState
  , ReactThis
  , ReadOnly
  , ReadWrite
  , readState
  , spec
  , writeStateWithCallback
  )
  as React

-- | The `Proactive` type class represents any Proact component that provides
-- | access to its underlying state via the `readState` function and to a
-- | dispatcher able to execute the actions of an event handler via the
-- | `dispatcher` function.
-- |
-- | An implementation is given for `Component` and for `IndexedComponent`.
-- |
-- | Laws:
-- |
-- | - readState *> readState = readState
-- | - dispatcher *> dispatcher = dispatcher
class (Monad m) <= Proactive fx state m | m -> fx, m -> state
  where
  readState :: m state
  dispatcher :: m (Dispatcher fx state)

-- | A monadic representation of a React component's GUI providing access to the
-- | underlying state through the `MonadAsk` interface.
newtype Component fx state a =
  Component (ReaderT state (ReaderT (This fx { } state) (Eff fx)) a)

-- | A monadic representation of an event handler that manipulates the
-- | component's state through the `MonadState` interface.
newtype EventHandler fx state a = EventHandler (Engine (Aff fx) a state state)

-- | A monadic representation of a React component's GUI that is an element of
-- | an indexed data structure.
-- | It provides access to the component's underlying state and its index within
-- | the collection through the `MonadAsk` interface.
newtype IndexedComponent fx index state a =
  IndexedComponent
    (ReaderT (Tuple index state) (ReaderT (This fx { } state) (Eff fx)) a)

-- | A composable representation of the underlying React `this` object.
data This fx props state =
  This (state -> Aff fx Unit) (Eff fx (Maybe state))
  | ReactThis (React.ReactThis props state)

-- A component representation of the Profunctor, Strong, Choice and Wander type
-- classes.
newtype ProComponent m a _in out = ProComponent (ReaderT _in m a)

-- Represents a Monoid under application.
newtype Sequence f a = Sequence (f a)

-- | A type synonym for a dispatcher that is accessible from a Component and
-- | that executes the actions of an event handler provided to it.
type Dispatcher fx state = EventHandler fx state Unit -> Eff (EventFx fx) Unit

-- | A type synonym for the effects associated to events of React elements.
type EventFx fx =
  ( props :: React.ReactProps
  , refs :: React.ReactRefs React.ReadOnly
  , state :: React.ReactState React.ReadWrite
  | fx
  )

-- A type synonym for the producer/consumer stack that is the engine behind all
-- components.
type Engine m a _in out = Producer out (Consumer _in m) a

-- Sequence :: Newtype, Semigroup, Monoid
derive instance newtypeSequence :: Newtype (Sequence f a) _

instance semigroupSequence :: (Apply f, Semigroup a) => Semigroup (Sequence f a)
  where
  append (Sequence fa1) (Sequence fa2) = Sequence $ lift2 append fa1 fa2

instance monoidSequence :: (Applicative f, Monoid a) => Monoid (Sequence f a)
  where
  mempty = Sequence $ pure mempty

-- ProComponent :: Newtype, Profunctor, Strong, Choice, Wander
derive instance newtypeProComponent :: Newtype (ProComponent m a _in out) _

instance profunctorProComponent :: Profunctor (ProComponent m a)
  where
  dimap f _ (ProComponent reader) = ProComponent $ withReaderT f reader

instance strongProComponent :: Strong (ProComponent m a)
  where
  first (ProComponent reader) = ProComponent $ withReaderT fst reader

  second (ProComponent reader) = ProComponent $ withReaderT snd reader

instance choiceProComponent :: (Monad m, Monoid a) => Choice (ProComponent m a)
  where
  left (ProComponent reader) =
    ProComponent
      do
      _in <- ask
      either (lift <<< runReaderT reader) (const (pure mempty)) _in

  right (ProComponent reader) =
    ProComponent
      do
      _in <- ask
      either (const (pure mempty)) (lift <<< runReaderT reader) _in

instance wanderProComponent :: (Monad m, Monoid a) => Wander (ProComponent m a)
  where
  wander traversal (ProComponent reader) =
    ProComponent
      do
      in2 <- ask
      lift
        $ unwrap
        $ unwrap
        $ traversal (Const <<< Sequence <<< runReaderT reader) in2

-- EventHandler
--  :: Functor
--   , Apply
--   , Applicative
--   , Bind
--   , Monad
--   , MonadAsk
--   , MonadAff
--   , MonadEff
--   , MonadState
instance functorEventHandler :: Functor (EventHandler fx state)
  where
  map f (EventHandler fa) = EventHandler $ map f fa

instance applyEventHandler :: Apply (EventHandler fx state)
  where
  apply = ap

instance applicativeEventHandler :: Applicative (EventHandler fx state)
  where
  pure = EventHandler <<< pure

instance bindEventHandler :: Bind (EventHandler fx state)
  where
  bind (EventHandler ma) f = EventHandler $ map f ma >>= unwrap'
    where
    unwrap' (EventHandler engine) = engine

instance monadEventHandler :: Monad (EventHandler fx state)

instance monadAffEventHandler :: MonadAff fx (EventHandler fx state)
  where
  liftAff = EventHandler <<< lift <<< lift

instance monadEffEventHandler :: MonadEff fx (EventHandler fx state)
  where
  liftEff = EventHandler <<< lift <<< lift <<< liftEff

instance monadStateEventHandler :: MonadState state (EventHandler fx state)
  where
  state f =
    EventHandler
      do
      _state <- lift await
      let Tuple a state' = f _state
      emit state'
      pure a

-- Component
--  :: Functor
--   , Apply
--   , Applicative
--   , Bind
--   , Monad
--   , MonadAsk
--   , MonadEff
--   , Proactive
instance functorComponent :: Functor (Component fx state)
  where
  map f (Component fa) = Component $ map f fa

instance applyComponent :: Apply (Component fx state)
  where
  apply = ap

instance applicativeComponent :: Applicative (Component fx state)
  where
  pure = Component <<< pure

instance bindComponent :: Bind (Component fx state)
  where
  bind (Component ma) f = Component $ map f ma >>= unwrap'
    where
    unwrap' (Component reader) = reader

instance monadComponent :: Monad (Component fx state)

instance monadAskComponent :: MonadAsk state (Component fx state)
  where
  ask = Component ask

instance monadEffComponent :: MonadEff fx (Component fx state)
  where
  liftEff = Component <<< lift <<< lift

instance proactiveComponent :: Proactive fx state (Component fx state)
  where
  readState = ask
  dispatcher = Component $ map dispatcher' $ lift ask

-- IndexedComponent
--  :: Functor
--   , Apply
--   , Applicative
--   , Bind
--   , Monad
--   , MonadAsk
--   , MonadEff
--   , Proactive
instance functorIndexedComponent :: Functor (IndexedComponent fx index state)
  where
  map f (IndexedComponent fa) = IndexedComponent $ map f fa

instance applyIndexedComponent :: Apply (IndexedComponent fx index state)
  where
  apply = ap

instance applicativeIndexedComponent
  :: Applicative (IndexedComponent fx index state)
  where
  pure = IndexedComponent <<< pure

instance bindIndexedComponent :: Bind (IndexedComponent fx index state)
  where
  bind (IndexedComponent ma) f = IndexedComponent $ map f ma >>= unwrap'
    where
    unwrap' (IndexedComponent reader) = reader

instance monadIndexedComponent :: Monad (IndexedComponent fx index state)

instance monadAskIndexedComponent
  :: MonadAsk (Tuple index state) (IndexedComponent fx index state)
  where
  ask = IndexedComponent ask

instance monadEffIndexedComponent
  :: MonadEff fx (IndexedComponent fx index state)
  where
  liftEff = IndexedComponent <<< lift <<< lift

instance proactiveIndexedComponent
  :: Proactive fx state (IndexedComponent fx index state)
  where
  readState = map snd ask
  dispatcher = IndexedComponent $ map dispatcher' $ lift ask

-- | Retrieves a dispatcher from the context of any React component. Once the
-- | dispatcher receives an event handler it will execute its asynchronous code.
dispatcher'
  :: forall fx props state . This fx props state -> Dispatcher fx state
dispatcher' this (EventHandler engine) =
  unsafeCoerceEff $ launchAff_ $ tailRecM stepProducer engine
  where
  stepConsumer consumer =
    do
    step <- resume consumer
    case step
      of
      Left a -> pure $ Done a
      Right (Await awaitNext) ->
        do
        state <- liftEff $ read this
        pure $ maybe (Done $ Left unit) (Loop <<< awaitNext) state

  stepProducer producer =
    do
    step <- tailRecM stepConsumer $ resume producer
    case step
      of
      Left a -> pure $ Done a
      Right (Emit state emitNext) ->
        do
        push this state
        pure $ Loop emitNext

-- | Changes a component's state type through the lens of a traversal.
-- | For a less restrictive albeit less general version, consider `focus'`.
focus
  :: forall fx state1 state2 render
   . Monoid render
  => Traversal' state1 state2
  -> Component fx state2 render
  -> Component fx state1 render
focus _traversal (Component reader) =
  Component $ unwrap $ positions _traversal profunctor
  where
  focusThisWithIndex index this = This _push _read
    where
    _push state2 =
      (void <<< runMaybeT)
        do
        state1 <- MaybeT $ liftEff $ read this
        lift $ push this $ set (element index _traversal) state2 state1

    _read =
      runMaybeT
        do
        state1 <- MaybeT $ read this
        MaybeT $ pure $ preview (element index _traversal) state1

  profunctor =
    (Indexed <<< ProComponent)
      do
      Tuple index state2 <- ask
      lift $ withReaderT (focusThisWithIndex index) $ runReaderT reader state2

-- | Changes a component's state type through a lens.
-- | For a more general albeit more restrictive version, consider `focus`.
focus'
  :: forall fx state1 state2 render
   . Lens' state1 state2
  -> Component fx state2 render
  -> Component fx state1 render
focus' _lens (Component reader) =
  Component
    $ unwrap
    $ _lens
    $ ProComponent
    $ mapReaderT (withReaderT (focusThis _lens)) reader

-- | Changes the state type of the underlying React `this` object through a
-- | lens.
focusThis
  :: forall fx props state1 state2
   . Lens' state1 state2 -> This fx props state1 -> This fx props state2
focusThis _lens this = This _push _read
  where
  _push state2 =
    (void <<< runMaybeT)
      do
      state1 <- MaybeT $ liftEff $ read this
      lift $ push this $ set _lens state2 state1

  _read =
    runMaybeT
      do
      state1 <- MaybeT $ read this
      MaybeT $ pure $ preview _lens state1

-- | Changes a component's state type through the lens of an indexed traversal.
iFocus
  :: forall fx index state1 state2 render
   . Monoid render
  => Index state1 index state2
  => IndexedTraversal' index state1 state2
  -> IndexedComponent fx index state2 render
  -> Component fx state1 render
iFocus _iTraversal (IndexedComponent reader) =
  Component $ unwrap $ _iTraversal profunctor
  where
  focusThisWithIndex index this = This _push _read
    where
    _push state2 =
      (void <<< runMaybeT)
        do
        state1 <- MaybeT $ liftEff $ read this
        lift $ push this $ set (ix index) state2 state1

    _read =
      runMaybeT
        do
        state1 <- MaybeT $ read this
        MaybeT $ pure $ preview (ix index) state1

  profunctor =
    (Indexed <<< ProComponent)
      do
      indexedState <- ask
      let Tuple index _ = indexedState
      lift
        $ withReaderT (focusThisWithIndex index)
        $ runReaderT reader indexedState

-- | Creates a `ReactSpec` from a Proact Component.
spec
  :: forall fx state
   . Component fx state React.ReactElement
  -> state
  -> React.ReactSpec { } state fx
spec (Component reader) iState = React.spec iState render
  where
  render this =
    do
    state <- unsafeCoerceEff $ React.readState this
    unsafeCoerceEff $ reader `runReaderT` state `runReaderT` ReactThis this

-- An abstraction of the `React.writeState` function exposing an asynchronous
-- facade.
push :: forall fx props state . This fx props state -> state -> Aff fx Unit
push (This _push _) state = _push state
push (ReactThis this) state = makeAff _push
  where
  _push callback =
    do
    void
      $ unsafeCoerceEff
      $ React.writeStateWithCallback this state
      $ unsafeCoerceEff
      $ callback
      $ Right unit
    pure nonCanceler

-- An abstraction of the `React.readState` function exposing a synchronous
-- facade.
read :: forall fx props state . This fx props state -> Eff fx (Maybe state)
read (This _ _read) = _read
read (ReactThis this) = map Just $ unsafeCoerceEff $ React.readState this
