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
( ComponentT
, This
, EventComponent
, EventHandler
, ProComponent
, UIComponent
, choose
, spec
, eventHandler
, focus
, get
, gets
, modify
, put
)
where

import Control.Coroutine (Consumer, Producer, Await(..), Emit(..), await, emit)
import Control.Monad.Aff (Aff, launchAff_, makeAff, nonCanceler, runAff_)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrowException)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free.Trans
  (bimapFreeT, freeT, hoistFreeT, interpret, resume)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, withReaderT)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either(..), either)
import Data.Bifunctor (lmap, rmap) as B
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype) as N
import Data.Profunctor (class Profunctor, lmap, rmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Lens (Lens', Prism', set, view)
import Data.Tuple (Tuple(..), fst, snd)
import DOM.Node.Document (createElement) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM
import Partial.Unsafe (unsafePartial)
import Prelude
import React
  ( EventHandlerContext
  , ReactElement
  , ReactSpec
  , ReactThis
  , createClass
  , createFactory
  , readState
  , spec
  , writeState
  , writeStateWithCallback
  )
  as React
import React.DOM (text) as React
import ReactDOM (render) as React

-- | A type synonym for an event handler component. The user may request an
-- | event object by `ask`ing for one (the component instantiates the `MonadAsk`
-- | class. Also, in a similar way to a UI component, the user is able to `get`,
-- | `set` or `modify` the component state asynchronously.
type EventComponent fx state event = ComponentT state (ReaderT event (Aff fx))

-- | A type synonym for an event handler that is accessible from a UI component.
-- | An event object may be provided later to trigger an event action.
type EventHandler fx state event =
  EventComponent fx state event Unit
  -> event
  -> React.EventHandlerContext fx {} state Unit

-- | A component representation of the Profunctor, Strong and Choice classes.
newtype ProComponent m a in_ out =
  ProComponent (Producer out (Consumer in_ m) a)

instance profunctorProComponent :: (Monad m) => Profunctor (ProComponent m a)
  where
  dimap f g (ProComponent producer) =
    ProComponent $ bimapFreeT (B.lmap g) (interpret (lmap f)) producer

instance strongProComponent :: (Monad m) => Strong (ProComponent m a)
  where
  first component = strong snd (flip Tuple) $ lmap fst component

  second component = strong fst Tuple $ lmap snd component

instance choiceProComponent
  :: (Monad m, MonadRec m, Monoid a) => Choice (ProComponent m a)
  where
  left (ProComponent component) =
    choice selectLeft $ rmap Left $ ProComponent $ map Right component
    where
    selectLeft (Await awaitNext) (Left in1) = awaitNext in1
    selectLeft _ (Right a) = pure $ Left $ Left (Right a)

  right (ProComponent component) =
    choice selectLeft $ rmap Right $ ProComponent $ map Right component
    where
    selectLeft (Await awaitNext) (Right in1) = awaitNext in1
    selectLeft _ (Left a) = pure $ Left $ Left (Left a)

-- | A monadic representation of the component exposing a cleaner interface to
-- | the user.
newtype ComponentT state m a = ComponentT (ProComponent m a state state)

derive instance newtypeComponentT :: N.Newtype (ComponentT state m a) _

instance functorComponentT :: (Functor m) => Functor (ComponentT state m)
  where
  map f (ComponentT (ProComponent fa)) = ComponentT $ ProComponent $ map f fa

instance applyComponentT :: (Monad m) => Apply (ComponentT state m)
  where
  apply = ap

instance applicativeComponentT :: (Monad m) => Applicative (ComponentT state m)
  where
  pure a = ComponentT $ ProComponent $ pure a

instance bindComponentT :: (Monad m) => Bind (ComponentT state m)
  where
  bind (ComponentT (ProComponent ma)) f =
    (ComponentT <<< ProComponent)
      do
      ComponentT (ProComponent mb) <- map f ma
      mb

instance monadComponentT :: (Monad m) => Monad (ComponentT state m)

instance monadTransComponentT :: MonadTrans (ComponentT state)
  where
  lift = ComponentT <<< ProComponent <<< lift <<< lift

instance monadAskComponentT
  :: (MonadAsk a m) => MonadAsk a (ComponentT state m)
  where
  ask = lift ask

instance monadEffComponentT
  :: (MonadEff fx m) => MonadEff fx (ComponentT state m)
  where
  liftEff = lift <<< liftEff

-- | A composable representation of the underlying react `this` object. This
-- | object should not be handled by the user directly.
data This fx state =
  This (state -> Aff fx Unit) (Aff fx state)
  | ReactThis (React.ReactThis {} state)

-- | A type synonym for a UI component providing the user the ability to `get`,
-- | `set` or `modify` the component state asynchronously.
type UIComponent fx state render =
  ComponentT state (ReaderT (This fx state) (Aff fx)) render

-- | Using a prism and assuming a part of the state is of sum type, selects the
-- | component that matches the actual type of the state or returns a `mempty`
-- | render.
-- | Whenever a delay event tries to read from or write to its component state,
-- | if it's no longer selected it will instead perform the operation on a
-- | `mempty` state.
choose
  :: forall fx state1 state2 render
   . Monoid render
  => Monoid state2
  => Prism' state1 state2
  -> UIComponent fx state2 render
  -> UIComponent fx state1 render
choose prism_ component = ComponentT $ prism_ profunctor
  where
  focusThis this = This push_ read_
    where
    push_ state1 =
      do
      state2 <- read this
      push this $ set prism_ state1 state2

    read_ = view prism_ <$> read this

  ComponentT profunctor =
    hoistComponentT (withReaderT focusThis) component

-- | Retrieves an event handler from the current UI context. Once this handler
-- | receives an event component and an event it will trigger the action defined
-- | in the event component logic.
eventHandler
  :: forall fx state event
   . UIComponent fx state (EventHandler fx state event)
eventHandler = ask >>= pure <<< handler
  where
  handler this component event =
    unsafeCoerceEff $ launchAff_ $ dispatch this component event

-- | Using a lens to focus on a part of the state, changes the state type of a
-- | child component so that it may be later composed and merged with its
-- | parent.
focus
  :: forall fx state1 state2 render
   . Lens' state1 state2
  -> UIComponent fx state2 render
  -> UIComponent fx state1 render
focus lens_ component = ComponentT $ lens_ profunctor
  where
  focusThis this = This push_ read_
    where
    push_ state1 =
      do
      state2 <- read this
      push this $ set lens_ state1 state2

    read_ = view lens_ <$> read this

  ComponentT profunctor =
    hoistComponentT (withReaderT focusThis) component

-- Note:
--  Ideally, these functions should be part of an instance of the `MonadState`
--  class, however, because Purescript doesn't allow for default implementations
--  in class declarations, only the `state` function may be redefined for an
--  instance of `MonadState` which is not efficient for this implementation.
--  Therefore, I'm providing these abstractions separately within this API.
-- | Gets the state of the component.
get :: forall m state . (Monad m) => ComponentT state m state
get = ComponentT $ ProComponent $ lift await

-- | Get the state of the component but apply a transformation function first.
gets :: forall m state a . (Monad m) => (state -> a) -> ComponentT state m a
gets f = map f get

-- | Modifies the state of the component given a transformation function.
modify
  :: forall m state . (Monad m) => (state -> state) -> ComponentT state m Unit
modify f = gets f >>= put

-- | Sets the state of the component.
put :: forall m state . (Monad m) => state -> ComponentT state m Unit
put state = ComponentT $ ProComponent $ emit state

-- | Creates a `ReactSpec` from a UI component starting from an initial state of
-- | the component and rendering a default "splash screen" provided by the
-- | caller to be shown while the actual GUI is being rendered asynchronously.
spec
  :: forall fx state
   . UIComponent fx state React.ReactElement
  -> React.ReactElement
  -> state
  -> React.ReactSpec {} React.ReactElement fx
spec component splashScreen state =
  (React.spec splashScreen React.readState)
    { componentDidMount = asyncRender state }
  where
  asyncRender state_ this =
    do
    -- Create a dummy HTML node just to trigger the effect of rendering.
    noscript_ <- unsafeCoerceEff noscript

    -- Render a dummy React component to trigger a run of the UI component.
    -- Every time the state of the underlying React component changes, the
    -- parent (this) needs to be updated indirectly.
    unsafeCoerceEff
      $ void
      $ flip React.render noscript_
      $ flip React.createFactory {}
      $ React.createClass
      $
        (React.spec state_ $ const $ pure $ React.text "")
          { componentDidMount = dispatchRender this
          , componentDidUpdate =
              \stThis _ _-> unsafeCoerceEff $ stepRender this stThis
          }

  noscript =
    do
    document <- DOM.window >>= DOM.document
    DOM.createElement "noscript" $ DOM.htmlDocumentToDocument document

  dispatchRender uiThis this =
    do
    let this_ = ReactThis this
    -- Run the UI component and register a callback so that when the new GUI
    -- to be rendered is ready, the parent React component can be updated.
    unsafeCoerceEff
      $ runAff_ (either unsafeThrowException (syncRender uiThis))
      $ dispatch this_ component this_

  -- The GUI to be rendered is the state itself of the parent component which
  -- is updated indirectly by its child. The virtual DOM will make sure to only
  -- re-render the graphical elements that changed.
  stepRender uiThis stThis =
    do
    state_ <- React.readState stThis
    asyncRender state_ uiThis

  -- Indirectly render the parent component by re-writing its state with the new
  -- GUI.
  syncRender uiThis = void <<< unsafeCoerceEff <<< React.writeState uiThis

-- A type synonym for the inner consumer co-routine of a producer/consumer
-- stack.
type InnerConsumer m a in_ out =
  Consumer
    in_
    m
    (
      Either
        (Either out a)
        (Emit out (Producer out (Consumer in_ m) (Either out a)))
    )

-- Changes the contravariant type of a producer/consumer stack by giving the
-- caller a choice on how to deal with the new input type. The last execution
-- step signals the mismatched value that should be yielded before returning
-- a `mempty` result.
choice
  :: forall m a in1 in2 out
   . Monoid a
  => Monad m
  => MonadRec m
  =>
     (  Await in1 (InnerConsumer m a in1 out)
     -> in2
     -> InnerConsumer m a in1 out
     )
  -> ProComponent m (Either out a) in1 out
  -> ProComponent m a in2 out
choice select (ProComponent component) =
  ProComponent $ stepOutProducer $ stepInProducer component
  where
  stepConsumer consumer =
    (freeT <<< const)
      do
      step <- resume consumer
      case step
        of
        Left producerStep ->
          pure $ Left $ B.rmap (map stepInProducer) producerStep
        Right awaitNext ->
          pure $ Right $ Await $ stepConsumer <<< select awaitNext

  stepOutProducer producer =
    (freeT <<< const)
      do
      step <- resume producer
      case step
        of
        Left tail ->
          case tail
            of
            -- NOTE:
            --  Following the example of Function as an instance of a Choice
            --  Profunctor, I should yield the mismatched value before closing
            --  the co-routine. However, doing so triggers an infinite loop (I
            --  don't know why!) and not doing so seems to work fine so for now
            --  I won't yield the mismatched value.
            Left _ -> pure $ Left mempty
            Right a -> pure $ Left a
        Right (Emit out next) -> pure $ Right $ Emit out $ stepOutProducer next

  stepInProducer producer = freeT $ const $ stepConsumer $ resume producer

-- Runs a component and asynchronously returns its monadic value.
dispatch
  :: forall fx state arg a
   . This fx state
  -> ComponentT state (ReaderT arg (Aff fx)) a
  -> arg
  -> Aff fx a
dispatch this (ComponentT (ProComponent component)) arg =
  tailRecM stepProducer component
  where
  stepConsumer consumer =
    do
    step <- resume consumer `runReaderT` arg
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
      Right (Emit state next) ->
        do
        push this state
        pure $ Loop next

-- Changes the underlying monadic context of a component.
hoistComponentT
  :: forall state m n a
   . Functor n
  => (m ~> n) -> ComponentT state m a -> ComponentT state n a
hoistComponentT hoistFunc (ComponentT (ProComponent consumer)) =
  ComponentT $ ProComponent $ hoistFreeT (hoistFreeT hoistFunc) consumer

-- An abstraction of the `React.writeState` function exposing an asynchronous
-- facade.
push :: forall fx state . This fx state -> state -> Aff fx Unit
push (This push_ _) state = push_ state
push (ReactThis this) state = makeAff push_
  where
  push_ callback =
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
read :: forall fx state . This fx state -> Aff fx state
read (This _ read_) = read_
read (ReactThis this) = liftEff $ unsafeCoerceEff $ React.readState this

-- Changes the covariant type of a profunctor component by merging it with the
-- freshest global state.
strong
  :: forall m a b in_ out1 out2
   . Monad m
  => (in_ -> a)
  -> (a -> out1 -> out2)
  -> ProComponent m b in_ out1
  -> ProComponent m b in_ out2
strong unwrap wrap (ProComponent component) =
  ProComponent $ stepProducer component
  where
  await_ :: Producer out2 (Consumer in_ m) a
  await_ = map unwrap $ lift await

  stepProducer producer =
    (freeT <<< const <<< unsafePartial)
      do
      step <- resume producer
      case step
        of
        Left b -> pure $ Left b
        Right (Emit out1 next) ->
          do
          Left a <- resume await_
          pure $ Right $ Emit (wrap a out1) $ stepProducer next
