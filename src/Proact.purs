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
, ComponentT
, EventComponent
, EventHandler
, EventHandlerContext
, ProComponent
, This
, UIComponent
, eventHandler
, focus
, focus'
, spec
)
where

import Control.Coroutine (Consumer, Producer, Await(..), Emit(..), await, emit)
import Control.Monad.Aff (Aff, launchAff_, makeAff, nonCanceler, runAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrowException)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free.Trans
  (bimapFreeT, freeT, hoistFreeT, interpret, resume)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, withReaderT)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Rec.Loops (whileM_)
import Control.Monad.State
  (class MonadState, evalStateT, execStateT, get, gets, modify, put)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (runWriterT, tell)
import Data.Array.Partial (head, tail)
import Data.Bifunctor (lmap, rmap) as B
import Data.Either (Either(..), either, fromLeft, isRight)
import Data.Foldable (any, fold)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor, lmap, rmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Lens (class Wander, Lens', Traversal', set, view)
import Data.Tuple (Tuple(..), fst, snd)
import DOM.Node.Document (createElement) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM
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

-- | An monoidal representation of applicative actions appended with (*>).
newtype Action f = Action (f Unit)

derive instance newtypeAction :: Newtype (Action f) _

instance semigroupAction :: (Apply f) => Semigroup (Action f)
  where
  append (Action a1) (Action a2) = Action $ a1 *> a2

instance monoidAction :: (Applicative f) => Monoid (Action f)
  where
  mempty = Action $ pure unit

-- | A component representation of the Profunctor, Strong and Choice classes.
newtype ProComponent m a _in out =
  ProComponent (Producer out (Consumer _in m) a)

instance profunctorProComponent :: (Monad m) => Profunctor (ProComponent m a)
  where
  dimap f g (ProComponent producer) =
    ProComponent $ bimapFreeT (B.lmap g) (interpret (lmap f)) producer

instance strongProComponent
  :: (Monad m, MonadRec m) => Strong (ProComponent m a)
  where
  first component = strong (flip Tuple <<< snd) $ lmap fst component

  second component = strong (Tuple <<< fst) $ lmap snd component

instance choiceProComponent
  :: (Monad m, MonadRec m, Monoid a) => Choice (ProComponent m a)
  where
  left (ProComponent iProducer) =
    choice selectLeft $ rmap Left $ ProComponent $ map Right iProducer
    where
    selectLeft (Await awaitNext) (Left in1) = awaitNext in1
    selectLeft _ (Right a) = pure $ Left $ Left (Right a)

  right (ProComponent iProducer) =
    choice selectLeft $ rmap Right $ ProComponent $ map Right iProducer
    where
    selectLeft (Await awaitNext) (Right in1) = awaitNext in1
    selectLeft _ (Left a) = pure $ Left $ Left (Left a)

instance wanderProComponent
  :: (Monad m, MonadRec m, Monoid a) => Wander (ProComponent m a)
  where
  wander traversal (ProComponent iProducer) =
    ProComponent
      do
      -- Request input (a collection of states) and use it to gather the last
      -- emitted value of every child component until its co-routine either
      -- completes or blocks awaiting for more input.
      in2 <- lift await

      Tuple out2 pendingBlocks <-
        lift $ lift $ runWriterT $ traversal initAwaitBlock in2

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
      -- progress the co-routine, otherwise return the last yielded value before
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
        Left (Right (Emit out1 next)) <- lift $ resume awaitBlock
        completeEmit out1 next

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
        -- Make a copy of the initial component for every input state, then if
        -- it immediately blocks awaiting for more input use the data from the
        -- the original request to progress the co-routine, otherwise return the
        -- last yielded value before the co-routine completed or blocked.
        -- It is assumed that each component will perform at least one yield
        -- operation before completing or blocking.
        step <- lift $ resume $ resume iProducer
        case step
          of
          Left (Right (Emit out1 next)) -> completeEmit out1 next
          Right (Await awaitNext) -> completeAwait $ awaitNext in1

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

instance monadStateComponentT
  :: (Monad m) => MonadState state (ComponentT state m)
  where
  state f =
    (ComponentT <<< ProComponent)
      do
      _state <- lift await
      let Tuple a state' = f _state
      emit state'
      pure a

-- A type synonym for the state to be stored in the react component that besides
-- actual stateful data includes control information to handle rendering.
type ReactState state = { state :: state, refreshUI :: Boolean }

-- | A composable representation of the underlying react `this` object. This
-- | object should not be handled by the user directly.
data This fx state =
  This (state -> Aff fx Unit) (Aff fx state)
  | ReactThis Boolean (React.ReactThis {} (ReactState state))

-- | A type synonym for an event handler component. The user may request an
-- | event object by `ask`ing for one (the component instantiates the `MonadAsk`
-- | class. Also, in a similar way to a UI component, the user is able to `get`,
-- | `set` or `modify` the component state asynchronously.
type EventComponent fx state event = ComponentT state (ReaderT event (Aff fx))

-- | A type synonym for effects associated to React components.
type EventHandlerContext fx =
  Eff
    ( props :: React.ReactProps
    , refs :: React.ReactRefs React.ReadOnly
    , state :: React.ReactState React.ReadWrite
    | fx
    )

-- | A type synonym for an event handler that is accessible from a UI component.
-- | An event object may be provided later to trigger an event action.
type EventHandler fx state event =
  EventComponent fx state event Unit -> event -> Action (EventHandlerContext fx)

-- | A type synonym for a UI component providing the user the ability to `get`,
-- | `set` or `modify` the component state asynchronously.
type UIComponent fx state render =
  ComponentT state (ReaderT (This fx state) (Aff fx)) render

-- | Retrieves an event handler from the current UI context. Once this handler
-- | receives an event component and an event it will trigger the actions
-- | contained in the monadic side-effects (asynchronous).
eventHandler
  :: forall fx state event
   . UIComponent fx state (EventHandler fx state event)
eventHandler = ask >>= pure <<< handler
  where
  handler this component event =
    Action $ unsafeCoerceEff $ launchAff_ $ dispatch this component event

-- | Changes a component's state type through the lens of a traversal.
-- | For a less restrictive albeit less general version, consider `focus'`.
focus
  :: forall fx state1 state2 render
   . Monoid state2
  => Monoid render
  => Traversal' state1 state2
  -> UIComponent fx state2 render
  -> UIComponent fx state1 render
focus traversal_ component = ComponentT $ traversal_ profunctor
  where
  focusThis this = This push_ read_
    where
    push_ state2 =
      do
      state1 <- read this
      push this $ set traversal_ state2 state1

    read_ = view traversal_ <$> read this

  ComponentT profunctor = hoistComponentT (withReaderT focusThis) component

-- | Changes a component's state type through a lens.
-- | For a more general albeit more restrictive version, consider `focus`.
focus'
  :: forall fx state1 state2 render
   . Lens' state1 state2
  -> UIComponent fx state2 render
  -> UIComponent fx state1 render
focus' _iso component = ComponentT $ _iso profunctor
  where
  focusThis this = This _push _read
    where
    _push state2 =
      do
      state1 <- read this
      push this $ set _iso state2 state1

    _read = view _iso <$> read this

  ComponentT profunctor = hoistComponentT (withReaderT focusThis) component

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
  asyncRender _state this =
    do
    -- Create a dummy HTML node just to trigger the effect of rendering.
    _noscript <- unsafeCoerceEff noscript

    let reactState = { state : _state, refreshUI : false }

    -- Render a dummy React component to trigger a run of the UI component.
    -- Every time the state of the underlying React component changes, the
    -- parent (this) needs to be updated indirectly.
    unsafeCoerceEff
      $ void
      $ flip React.render _noscript
      $ flip React.createFactory {}
      $ React.createClass
      $
        (React.spec reactState $ const $ pure $ React.text "")
          { componentDidMount = dispatchRender this
          , componentDidUpdate =
              \stateThis _ _-> unsafeCoerceEff $ stepRender this stateThis
          }

  noscript =
    do
    document <- DOM.window >>= DOM.document
    DOM.createElement "noscript" $ DOM.htmlDocumentToDocument document

  dispatchRender uiThis this =
    do
    let renderThis = ReactThis false this
    let dispatcherThis = ReactThis true this

    -- Run the UI component and register a callback so that when the new GUI
    -- to be rendered is ready, the parent React component can be updated.
    unsafeCoerceEff
      $ runAff_ (either unsafeThrowException (syncRender uiThis))
      $ dispatch renderThis component dispatcherThis

  -- The GUI to be rendered is the state itself of the parent component which
  -- is updated indirectly by its child. The virtual DOM will make sure to only
  -- re-render the graphical elements that changed.
  stepRender uiThis stateThis =
    do
    reactState <- React.readState stateThis
    when (reactState.refreshUI) $ asyncRender reactState.state uiThis

  -- Indirectly render the parent component by re-writing its state with the new
  -- GUI.
  syncRender uiThis = void <<< unsafeCoerceEff <<< React.writeState uiThis

-- A type synonym for the consumer co-routine at the first layer of a
-- producer/consumer stack.
type AwaitBlock m a _in out =
  Consumer _in m (Either a (Emit out (Producer out (Consumer _in m) a)))

-- A type synonym for the consumer co-routine at the first layer of a
-- producer/consumer stack that returns whether a Choice profunctor should yield
-- the mismatched input type or not.
type AwaitChoiceBlock m a _in out = AwaitBlock m (Either out a) _in out

-- Changes the contravariant type of a producer/consumer stack by giving the
-- caller a choice on how to deal with the new input type. The last execution
-- step signals the mismatched value that should be yielded before returning
-- a `mempty` result.
-- It is assumed that before emitting, the co-routine will at least once block
-- for input. This is to make sure that the choice operation will have the
-- opportunity to fail in case the input was a mismatch.
choice
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
choice select (ProComponent iProducer) =
  ProComponent $ stepOutProducer $ stepInProducer iProducer
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
        Right (Emit out next) -> pure $ Right $ Emit out $ stepOutProducer next

  stepInProducer producer = freeT $ const $ stepConsumer $ resume producer

-- Runs a component and asynchronously returns its monadic value.
dispatch
  :: forall fx state arg a
   . This fx state
  -> ComponentT state (ReaderT arg (Aff fx)) a
  -> arg
  -> Aff fx a
dispatch this (ComponentT (ProComponent iProducer)) arg =
  tailRecM stepProducer iProducer
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
push (This _push _) state = _push state
push (ReactThis refreshUI this) state = makeAff _push
  where
  _push callback =
    do
    let _state = { state : state, refreshUI : refreshUI }
    void
      $ unsafeCoerceEff
      $ React.writeStateWithCallback this _state
      $ unsafeCoerceEff
      $ callback
      $ Right unit
    pure nonCanceler

-- An abstraction of the `React.readState` function exposing an asynchronous
-- facade.
read :: forall fx state . This fx state -> Aff fx state
read (This _ _read) = _read
read (ReactThis _ this) =
  map (_.state) $ liftEff $ unsafeCoerceEff $ React.readState this

-- Changes the covariant type of a profunctor component by merging it with the
-- most recent global state.
-- It is assumed that before emitting, the co-routine will at least once block
-- for input. This is so that the unfocused segment of the state can be emitted.
strong
  :: forall m b _in out1 out2
   . Monad m
  => MonadRec m
  => (_in -> out1 -> out2)
  -> ProComponent m b _in out1
  -> ProComponent m b _in out2
strong emit' (ProComponent iProducer) = ProComponent $ stepProducer iProducer
  where
  completeAwait awaitBlock _in =
    freeT \_ -> unsafePartial
      do
      step <- resume awaitBlock
      case step
        of
        Left (Left b) -> pure $ Left $ Left b
        Left (Right (Emit out1 next)) ->
          pure $ Left $ Right $ Emit (emit' _in out1) $ stepProducer next

  stepProducer producer =
    freeT \_ -> freeT \_ -> unsafePartial
      do
      step <- resume $ resume producer
      case step
        of
        Left (Left b) -> pure $ Left $ Left b
        Right (Await awaitNext) ->
          pure $ Right $ Await \_in -> completeAwait (awaitNext _in) _in
