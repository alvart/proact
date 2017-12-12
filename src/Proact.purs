{-
  @license MIT
  Proact.purs
-}

-- | Proact is a web framework inspired by Thermite that provides a
-- | model-view-dispatcher abstraction on top of `purescript-react`. It exposes
-- | a clean monadic API to construct and compose components and to define event
-- | handlers with minimal code overhead. Unlike other dispatch architectures
-- | that use messages to communicate components, Proact dispatches event
-- | actions themselves which means there is less boilerplate code and more
-- | rapid development.
-- |
-- | The components work on an underlying asynchronous layer which allows for
-- | cleaner asynchronous code but has the side effect that the rendering of
-- | the component will be delayed. Because of this, the user is encouraged to
-- | provide a "splash screen" that will show while the component loads the
-- | actual GUI.

module Proact
( Action
, Component
, EventHandler
, EventHandle
, EventDispatcher
, ReactContext
, eventDispatcher
, focus
, focus'
, noAction
, spec
)
where

import Control.Apply (lift2)
import Control.Coroutine (Consumer, Producer, Await(..), Emit(..), await, emit)
import Control.Monad.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free.Trans
  (bimapFreeT, freeT, hoistFreeT, interpret, resume)
import Control.Monad.Reader
  (class MonadAsk, ReaderT, ask, runReaderT, withReaderT)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Rec.Loops (whileM_)
import Control.Monad.State
  (class MonadState, evalStateT, execStateT, get, gets, modify, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (runWriterT, tell)
import Data.Array.Partial (head, tail)
import Data.Bifunctor (lmap, rmap) as B
import Data.Either (Either(..), fromLeft, isRight)
import Data.Foldable (any, fold)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor, lmap, rmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Lens (class Wander, Lens', Traversal', element, set, view)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)
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

-- | An monoidal representation of applicative actions appended in sequence.
newtype Action fx = Action (Eff (ReactContext fx) Unit)

-- | A monadic representation of a component's GUI that provides access to its
-- | state through the `MonadAsk` interface.
newtype Component fx state a =
  Component (Engine (ReaderT (This fx state) (Eff fx)) a state state)

-- | A monadic representation of an event handler that manipulates the
-- | component's state through the `MonadState` interface and to the event data
-- | through the `MonadAsk` interface.
newtype EventHandler fx state event a =
  EventHandler (Engine (ReaderT event (Aff fx)) a state state)

-- A component representation of the Profunctor, Strong, Choice and Wander type
-- classes.
newtype ProComponent m a _in out = ProComponent (Int -> Engine m a _in out)

-- A composable representation of the underlying react `this` object.
data This fx state =
  This (state -> Aff fx Unit) (Eff fx state)
  | ReactThis (React.ReactThis {} state)

-- | A type synonym for an event handler that is accessible from a Component.
-- | An event object may be later provided to the handler to trigger an event
-- | action.
type EventDispatcher fx state event =
  Component fx state (EventHandler fx state event Unit -> EventHandle fx event)

-- | A type synonym for the target of an event handler that listens for a
-- | specific event and then triggers an action.
type EventHandle fx event = event -> Action fx

-- | A type synonym for effects associated to React components.
type ReactContext fx =
  ( props :: React.ReactProps
  , refs :: React.ReactRefs React.ReadOnly
  , state :: React.ReactState React.ReadWrite
  | fx
  )

-- A type synonym for the consumer co-routine at the first layer of a
-- producer/consumer stack.
type AwaitBlock m a _in out =
  Consumer _in m (Either a (Emit out (Producer out (Consumer _in m) a)))

-- A type synonym for the consumer co-routine at the first layer of a
-- producer/consumer stack that returns whether a Choice profunctor should yield
-- the mismatched input type or not.
type AwaitChoiceBlock m a _in out = AwaitBlock m (Either out a) _in out

-- A type synonym for the producer/consumer stack that is the engine behind all
-- components.
type Engine m a _in out = Producer out (Consumer _in m) a

-- Action :: NewType, SemiGroup, Monoid
derive instance newtypeAction :: Newtype (Action fx) _

instance semigroupAction :: Semigroup (Action fx)
  where
  append (Action a1) (Action a2) = Action $ lift2 append a1 a2

instance monoidAction :: Monoid (Action fx)
  where
  mempty = Action $ pure mempty

-- ProComponent :: NewType, Profunctor, Strong, Choice, Wander
derive instance newtypeProComponent :: Newtype (ProComponent m a _in out) _

instance profunctorProComponent :: (Monad m) => Profunctor (ProComponent m a)
  where
  dimap f g (ProComponent indexEngine) =
    ProComponent
      \index -> bimapFreeT (B.lmap g) (interpret (lmap f)) $ indexEngine index

instance strengthenProComponent
  :: (Monad m, MonadRec m) => Strong (ProComponent m a)
  where
  first component = strengthen (flip Tuple <<< snd) $ lmap fst component

  second component = strengthen (Tuple <<< fst) $ lmap snd component

instance choiceProComponent
  :: (Monad m, MonadRec m, Monoid a) => Choice (ProComponent m a)
  where
  left (ProComponent indexEngine) =
    choose selectLeft $ rmap Left $ ProComponent $ map (map Right) indexEngine
    where
    selectLeft (Await awaitNext) (Left in1) = awaitNext in1
    selectLeft _ (Right a) = pure $ Left $ Left (Right a)

  right (ProComponent indexEngine) =
    choose selectRight
      $ rmap Right
      $ ProComponent
      $ map (map Right) indexEngine
    where
    selectRight (Await awaitNext) (Right in1) = awaitNext in1
    selectRight _ (Left a) = pure $ Left $ Left (Left a)

instance wanderProComponent
  :: (Monad m, MonadRec m, Monoid a) => Wander (ProComponent m a)
  where
  wander traversal (ProComponent indexEngine) =
    ProComponent \_ ->
      do
      -- Request input (a collection of states) and use it to gather the last
      -- emitted value of every child component until its co-routine either
      -- completes or blocks awaiting for more input.
      in2 <- lift await

      Tuple out2 pendingBlocks <-
        lift
          $ lift
          $ runWriterT
          $ flip evalStateT 0
          $ traversal initAwaitBlock in2

      -- Yield the collection of states assembled in the last step.
      emit out2

      -- If any component is still blocked awaiting for input, request another
      -- collection of states and repeat the initialization process that clears
      -- the await block.
      let anyBlockPending = get >>= pure <<< any isRight
      resultList <-
        unsafePartial
          $ map (map (fst <<< fromLeft))
          $ flip execStateT pendingBlocks
          $ whileM_ anyBlockPending clearAwaitBlockList

      pure $ fold resultList
    where
    clearAwaitBlock in1 =
      unsafePartial
        do
        awaitBlock <- gets head
        modify tail
        lift
          case awaitBlock
            of
            Left (Tuple a out1) -> tell [Left $ Tuple a out1] *> pure out1
            Right (Await awaitNext) -> completeAwait $ awaitNext in1

    clearAwaitBlockList =
      do
      pendingBlocks <- get
      in2 <- lift $ lift await

      -- For every component, if it immediately blocks awaiting for input then
      -- use the data from the original request (the collection of states) to
      -- resume the co-routine, otherwise return the last yielded value before
      -- the co-routine completed or blocked.
      -- It is assumed that each component will perform at least one yield
      -- operation before completing or blocking.
      Tuple out2 pendingBlocks' <-
        lift
          $ lift
          $ lift
          $ runWriterT
          $ flip evalStateT pendingBlocks
          $ traversal clearAwaitBlock in2

      lift $ emit out2
      put pendingBlocks'

    completeAwait awaitBlock =
      unsafePartial
        do
        Left (Right (Emit out1 emitNext)) <- lift $ resume awaitBlock
        completeEmit out1 emitNext

    completeEmit out1 producer =
      unsafePartial
        do
        step <- lift $ resume $ resume producer
        case step
          of
          Left (Left a) -> tell [Left $ Tuple a out1] *> pure out1
          Right awaitBlock -> tell [Right awaitBlock] *> pure out1

    initAwaitBlock in1 =
      unsafePartial
        do
        index <- get
        put $ index + 1

        -- Make a copy of the initial component for every input state, then if
        -- it immediately blocks awaiting for more input use the data from the
        -- original request to resume the co-routine, otherwise return the last
        -- yielded value before the co-routine completed or blocked.
        -- It is assumed that each component will perform at least one yield
        -- operation before completing or blocking.
        step <- lift $ lift $ resume $ resume $ indexEngine index
        lift
          case step
            of
            Left (Right (Emit out1 emitNext)) -> completeEmit out1 emitNext
            Right (Await awaitNext) -> completeAwait $ awaitNext in1

-- EventHandler
--  :: NewType
--   , Functor
--   , Apply
--   , Applicative
--   , Bind
--   , Monad
--   , MonadAsk
--   , MonadEff
--   , MonadState
derive instance newtypeEventHandler :: Newtype (EventHandler fx state event a) _

instance functorEventHandler :: Functor (EventHandler fx state event)
  where
  map f (EventHandler fa) = EventHandler $ map f fa

instance applyEventHandler :: Apply (EventHandler fx state event)
  where
  apply = ap

instance applicativeEventHandler :: Applicative (EventHandler fx state event)
  where
  pure a = EventHandler $ pure a

instance bindEventHandler :: Bind (EventHandler fx state event)
  where
  bind (EventHandler ma) f =
    EventHandler
      do
      EventHandler mb <- map f ma
      mb

instance monadEventHandler :: Monad (EventHandler fx state event)

instance monadAskEventHandler :: MonadAsk event (EventHandler fx state event)
  where
  ask = EventHandler $ lift $ lift ask

instance monadEffEventHandler :: MonadEff fx (EventHandler fx state event)
  where
  liftEff = EventHandler <<< lift <<< lift <<< lift <<< liftEff

instance monadStateEventHandler
  :: MonadState state (EventHandler fx state event)
  where
  state f =
    EventHandler
      do
      _state <- lift await
      let Tuple a state' = f _state
      emit state'
      pure a

-- Component
--  :: NewType
--   , Functor
--   , Apply
--   , Applicative
--   , Bind
--   , Monad
--   , MonadAsk
--   , MonadEff
derive instance newtypeComponent :: Newtype (Component fx state a) _

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
  bind (Component ma) f =
    Component
      do
      Component mb <- map f ma
      mb

instance monadComponent :: Monad (Component fx state)

instance monadAskComponent :: MonadAsk state (Component fx state)
  where
  ask =
    Component
      do
      state <- lift await
      emit state
      pure state

instance monadEffComponent :: MonadEff fx (Component fx state)
  where
  liftEff = Component <<< lift <<< lift <<< lift <<< liftEff

-- | Retrieves an event handler from the current UI context. Once this handler
-- | receives an event component and an event it will trigger the actions
-- | contained in the monadic side-effects (asynchronous).
eventDispatcher :: forall fx state event . EventDispatcher fx state event
eventDispatcher =
  Component
    do
    this <- lift $ lift ask
    pure $ dispatcher this
  where
  dispatcher this (EventHandler engine) event =
    Action $ unsafeCoerceEff $ launchAff_ dispatch
    where
    dispatch = tailRecM stepProducer engine

    stepConsumer consumer =
      do
      step <- resume consumer `runReaderT` event
      case step
        of
        Left a -> pure $ Done a
        Right (Await awaitNext) ->
          do
          state <- liftEff $ read this
          pure $ Loop $ awaitNext state

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
  => Monoid state2
  => Traversal' state1 state2
  -> Component fx state2 render
  -> Component fx state1 render
focus _traversal (Component engine) =
  Component $ (unwrap $ _traversal profunctor) 0
  where
  focusThis index this = This _push _read
    where
    _push state2 =
      do
      state1 <- liftEff $ read this
      push this $ set (element index _traversal) state2 state1

    _read =
      do
      state1 <- read this
      pure $ view (element index _traversal) state1

  profunctor =
    ProComponent \index ->
      hoistEngine (withReaderT \this -> focusThis index this) engine

-- | Changes a component's state type through a lens.
-- | For a more general albeit more restrictive version, consider `focus`.
focus'
  :: forall fx state1 state2 render
   . Lens' state1 state2
  -> Component fx state2 render
  -> Component fx state1 render
focus' _lens (Component engine) = Component $ (unwrap $ _lens profunctor) 0
  where
  focusThis this = This _push _read
    where
    _push state2 =
      do
      state1 <- liftEff $ read this
      push this $ set _lens state2 state1

    _read =
      do
      state1 <- read this
      pure $ view _lens state1

  profunctor = ProComponent \index -> hoistEngine (withReaderT focusThis) engine

-- | Creates a default action with no side-effects.
noAction :: forall fx . Action fx
noAction = Action $ pure mempty

-- | Creates a `ReactSpec` from a Proact Component.
spec
  :: forall fx state
   . Component fx state React.ReactElement
  -> state
  -> React.ReactSpec {} state fx
spec (Component engine) iState = React.spec iState render
  where
  render _this = unsafeCoerceEff $ tailRecM stepProducer engine
    where
    stepConsumer consumer =
      do
      step <- resume consumer `runReaderT` this
      case step
        of
        Left a -> pure $ Done a
        Right (Await awaitNext) ->
          do
          state <- read this
          pure $ Loop $ awaitNext state

    stepProducer producer =
      do
      step <- tailRecM stepConsumer $ resume producer
      case step
        of
        Left a -> pure $ Done a
        Right (Emit _ emitNext) ->
          -- Components only emit unchanged states so it's safe to ignore them
          -- in this context.
          pure $ Loop emitNext

    this = ReactThis _this

-- Changes the contravariant type of a producer/consumer stack by giving the
-- caller a choice on how to deal with the new input type. The last execution
-- step signals the mismatched value that should be yielded before returning
-- a `mempty` result.
-- It is assumed that before emitting, the co-routine will at least once block
-- for input. This is to make sure that when choosing, there'll be an
-- opportunity to fail in case the input mismatched.
choose
  :: forall m a in1 in2 out
   . Monoid a
  => Monad m
  => MonadRec m
  =>
     (  Await in1 (AwaitChoiceBlock m a in1 out)
     -> in2
     -> AwaitChoiceBlock m a in1 out
     )
  -> ProComponent m (Either out a) in1 out
  -> ProComponent m a in2 out
choose select (ProComponent indexEngine) =
  ProComponent \index -> stepOutProducer $ stepInProducer $ indexEngine index
  where
  stepConsumer consumer =
    freeT \_ ->
      do
      step <- resume consumer
      case step
        of
        Left emitBlock -> pure $ Left $ B.rmap (map stepInProducer) emitBlock
        Right awaitBlock ->
          pure $ Right $ Await $ stepConsumer <<< select awaitBlock

  stepOutProducer producer =
    freeT \_ ->
      do
      step <- resume producer
      case step
        of
        Left tail ->
          case tail
            of
            Left out -> pure $ Right $ Emit out $ pure mempty
            Right a -> pure $ Left a
        Right (Emit out emitNext) ->
          pure $ Right $ Emit out $ stepOutProducer emitNext

  stepInProducer producer = freeT $ const $ stepConsumer $ resume producer

-- Changes the underlying monadic context of a component's engine.
hoistEngine
  :: forall m n a _in out
   . Functor n => (m ~> n) -> Engine m a _in out -> Engine n a _in out
hoistEngine hoistFunc engine = hoistFreeT (hoistFreeT hoistFunc) engine

-- An abstraction of the `React.writeState` function exposing an asynchronous
-- facade.
push :: forall fx state . This fx state -> state -> Aff fx Unit
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

-- An abstraction of the `React.readState` function exposing an asynchronous
-- facade.
read :: forall fx state . This fx state -> Eff fx state
read (This _ _read) = _read
read (ReactThis this) = unsafeCoerceEff $ React.readState this

-- Changes the covariant type of a profunctor component by merging it with the
-- most recent global state.
-- It is assumed that before emitting, the co-routine will at least once block
-- for input. This is so that the unfocused segment of the state can be emitted.
strengthen
  :: forall m b _in out1 out2
   . Monad m
  => MonadRec m
  => (_in -> out1 -> out2)
  -> ProComponent m b _in out1
  -> ProComponent m b _in out2
strengthen emit' (ProComponent indexEngine) =
  ProComponent \index -> stepProducer $ indexEngine index
  where
  completeAwait awaitBlock _in =
    freeT \_ -> unsafePartial
      do
      step <- resume awaitBlock
      case step
        of
        Left (Left b) -> pure $ Left $ Left b
        Left (Right (Emit out1 emitNext)) ->
          pure $ Left $ Right $ Emit (emit' _in out1) $ stepProducer emitNext

  stepProducer producer =
    freeT \_ -> freeT \_ -> unsafePartial
      do
      step <- resume $ resume producer
      case step
        of
        Left (Left b) -> pure $ Left $ Left b
        Right (Await awaitNext) ->
          pure $ Right $ Await \_in -> completeAwait (awaitNext _in) _in
