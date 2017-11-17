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
, Dispatcher
, EventComponent
, EventHandler
, ProComponent
, UIComponent
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
import Control.Monad.Free.Trans (bimapFreeT, hoistFreeT, interpret, resume)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, withReaderT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either(..), either)
import Data.Bifunctor (lmap) as B
import Data.Profunctor (class Profunctor, dimap, lmap)
import Data.Profunctor.Strong (class Strong)
import Data.Lens (Lens', view, set)
import Data.Tuple (Tuple(..), fst, snd)
import DOM.Node.Document (createElement) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM
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
import Unsafe.Coerce (unsafeCoerce)

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

-- | A component representation of the profunctor and strong classes.
newtype ProComponent m a in_ out =
  ProComponent (Consumer in_ (Producer out m) a)

instance profunctorProComponent :: (Monad m) => Profunctor (ProComponent m a)
  where
  dimap f g (ProComponent consumer) =
    ProComponent $ bimapFreeT (lmap f) (interpret (B.lmap g)) consumer

-- Force an await before resuming the component process to retrieve the external
-- context, then for every emit relay this data which is assumed to not have
-- changed during the co-routine execution.

-- TODO:
--  At the moment this assumption is incorrect and a dispatching queue needs
--  to be implemented.
instance strongProComponent :: (Monad m) => Strong (ProComponent m r)
  where
  first component =
    ProComponent
      do
      Tuple _ a <- await
      let ProComponent consumer = dimap fst (flip Tuple a) component
      consumer

  second component =
    ProComponent
      do
      Tuple a _ <- await
      let ProComponent consumer = dimap snd (Tuple a) component
      consumer

-- | A monadic representation of the component exposing a cleaner interface to
-- | the user.
newtype ComponentT state m a = ComponentT (ProComponent m a state state)

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
data Dispatcher fx state =
  Dispatcher (state -> Aff fx Unit) (Aff fx state)
  | ReactDispatcher (React.ReactThis {} state)

-- | A type synonym for a UI component providing the user the ability to `get`,
-- | `set` or `modify` the component state asynchronously.
type UIComponent fx state render =
  ComponentT state (ReaderT (Dispatcher fx state) (Aff fx)) render

-- | Retrieves an event handler from the current UI context. Once this handler
-- | receives an event component and an event it will trigger the action defined
-- | in the event component logic.
eventHandler
  :: forall fx state event
   . UIComponent fx state (EventHandler fx state event)
eventHandler = ask >>= pure <<< handler
  where
  handler dispatcher component event =
    -- TODO:
    --  Events fired simultaneously may revert the partial state of other
    --  components. The best approach to avoid this is to create a dispatcher
    --  of events that executes them synchronously (just like Flux does).
    unsafeCoerceEff $ launchAff_ $ dispatch dispatcher component event

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
  focusDispatcher dispatcher = Dispatcher push_ read_
    where
    push_ state2 =
      do
      state1 <- read dispatcher
      push dispatcher $ set lens_ state2 state1

    read_ = view lens_ <$> read dispatcher

  ComponentT profunctor =
    hoistComponentT (withReaderT focusDispatcher) component

-- Note:
--  Ideally, these functions should be part of an instance of the `MonadState`
--  class, however, because Purescript doesn't allow for default implementations
--  in class declarations, only the `state` function may be redefined for an
--  instance of `MonadState` which is not efficient for this implementation.
--  Therefore, I'm providing these abstractions separately within this API.
-- | Gets the state of the component.
get :: forall m state . (Monad m) => ComponentT state m state
get = ComponentT $ ProComponent await

-- | Get the state of the component but apply a transformation function first.
gets :: forall m state a . (Monad m) => (state -> a) -> ComponentT state m a
gets f = map f get

-- | Modifies the state of the component given a transformation function.
modify
  :: forall m state . (Monad m) => (state -> state) -> ComponentT state m Unit
modify f = gets f >>= put

-- | Sets the state of the component.
put :: forall m state . (Monad m) => state -> ComponentT state m Unit
put state = ComponentT $ ProComponent $ lift $ emit state

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
    let dispatcher = ReactDispatcher this
    -- Run the UI component and register a callback so that when the new GUI
    -- to be rendered is ready, the parent React component can be updated.
    unsafeCoerceEff
      $ runAff_ (either unsafeThrowException (syncRender uiThis))
      $ dispatch dispatcher component dispatcher

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

-- Runs a component and asynchronously return its monadic value.
dispatch
  :: forall fx state arg a
   . Dispatcher fx state
  -> ComponentT state (ReaderT arg (Aff fx)) a
  -> arg
  -> Aff fx a
dispatch dispatcher (ComponentT (ProComponent iConsumer)) arg =
  tailRecM consumerStep iConsumer
  where
  consumerStep consumer =
    do
    step <- tailRecM producerStep $ resume consumer
    case step
      of
      Left a -> pure $ Done a
      Right (Await awaitNext) ->
        do
        state <- read dispatcher
        pure $ Loop $ awaitNext state

  producerStep producer =
    do
    step <- resume producer `runReaderT` arg
    case step
      of
      Left a -> pure $ Done a
      Right (Emit state next) ->
        do
        push dispatcher state
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
push :: forall fx state . Dispatcher fx state -> state -> Aff fx Unit
push (Dispatcher push_ _) state = push_ state
push (ReactDispatcher this) state = makeAff push_
  where
  push_ callback =
    do
    void
      $ unsafeCoerceEff
      $ React.writeStateWithCallback this (unsafeCoerce state)
      $ unsafeCoerceEff
      $ callback
      $ Right unit
    pure nonCanceler

-- An abstraction of the `React.readState` function exposing an asynchronous
-- facade.
read :: forall fx state . Dispatcher fx state -> Aff fx state
read (Dispatcher _ read_) = read_
read (ReactDispatcher this) = liftEff $ unsafeCoerceEff $ React.readState this
