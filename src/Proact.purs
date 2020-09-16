{-
  @license MIT
  Proact.purs
-}

-- | Proact is a set of utilities that leverage interactions with React through
-- | Free programs written with extensible effects. These interactions give
-- | access to a global singular state that is composable via Profunctor lenses.

module Proact
  ( Component
  , DISPATCHER
  , DispatcherF(..)
  , EventHandler
  , IndexedComponent
  , PComponent
  , _dispatcher
  , dispatch
  , dispatch_
  , dispatcher
  , dispatcher_
  , focus
  , focus'
  , focusHandler
  , iFocus
  , proact
  , translate
  )
where

import Control.Apply (lift2)
import Data.Either (Either(..))
import Data.Lens
  ( IndexedTraversal'
  , Traversal'
  , Forget(..)
  , Indexed(..)
  , Lens'
  , element
  , preview
  , set
  , view
  )
import Data.Lens.Index (class Index, ix)
import Data.Lens.Indexed (positions)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Fiber, launchAff)
import Prelude
import Proact.Versionable (class Versionable, reconcile)
import React (ReactThis, getState, modifyState)
import Run
  ( AFF
  , EFFECT
  , FProxy
  , Run
  , SProxy(..)
  , interpret
  , lift
  , liftEffect
  , on
  , peel
  , runBaseAff'
  , runBaseEffect
  , send
  )
import Run.Reader (READER, Reader(..), _reader, runReader)
import Run.State (STATE, State(..), _state)
import Unsafe.Coerce (unsafeCoerce)

-- | A type synonym for a Proact Component.
type Component s e1 e2 =
  Run (reader :: READER s, dispatcher :: DISPATCHER s e2 | e1)

-- | A type synonym for the effects that require an Event Dispatcher.
type DISPATCHER s e = FProxy (DispatcherF s e)

-- | Represents the Functor container for external interactions requiring an
-- | Event Dispatcher.
data DispatcherF s e a =
  Dispatcher ((EventHandler s e Unit -> Effect (Fiber Unit)) -> a)

-- | A type synonym for Free programs that manipulate their component state
-- | through the `State` effect.
type EventHandler s e = Run (state :: STATE s | e)

-- | A type synonym for an Indexed Proact Component.
type IndexedComponent i s e1 e2 =
  Run (reader :: READER (Tuple i s), dispatcher :: DISPATCHER s e2 | e1)

-- | A type synonym for Free programs with global access to its component state
-- | and to an event dispatcher.
type PComponent s t e1 e2 =
  Run (reader :: READER s, dispatcher :: DISPATCHER t e2 | e1)

-- Represents applicatives as monoids and semigroups.
newtype Ap m a = Ap (m a)

-- A type synonym for a function that focuses Event Handlers.
type HandlerFocus s1 s2 e =
  forall a . Monoid a => EventHandler s2 e a -> EventHandler s1 e a

-- Ap :: Newtype, Functor, Apply, Applicative, Semigroup, Monoid

derive instance newtypeAp :: Newtype (Ap m a) _
derive newtype instance functorAp :: (Functor m) => Functor (Ap m)
derive newtype instance applyAp :: (Apply m) => Apply (Ap m)
derive newtype instance applicativeAp :: (Applicative m) => Applicative (Ap m)

instance semigroupFreeT :: (Applicative m, Semigroup a) => Semigroup (Ap m a)
  where
  append = lift2 append

instance monoidFreeT :: (Apply m, Applicative m, Monoid a) => Monoid (Ap m a)
  where
  mempty = pure mempty

-- DispatcherF :: Functor

derive instance functorDispatcherF :: Functor (DispatcherF s e)

-- | Gets the proxy symbol of the Dispatcher effect.
_dispatcher :: SProxy "dispatcher"
_dispatcher = SProxy

-- | Runs the actions of an Event Handler in a React context. For events that
-- | update the state concurrently, a function to reconcile its different
-- | versions is applied.
dispatch
  :: forall s
   . Versionable s
  => ReactThis { } { __state :: s }
  -> EventHandler s (aff :: AFF, effect :: EFFECT) Unit
  -> Effect (Fiber Unit)
dispatch this eventHandler =
  do
  s <- getState this
  eventHandler # runState s.__state # void # runBaseAff' # launchAff
  where
  runState s r =
    case peel r
    of
      Left r' ->
        case on _state Left Right $ unsafeCoerce r'
        of
          Left f -> handleState f
          Right r'' -> send r'' >>= runState s
      Right a -> pure $ Tuple s a
    where
    handleState (State f next) =
      do
      let s' = f s
      liftEffect $ modifyState this $ flip reconcile' s'
      runState s' $ next s'
      where
      reconcile' s1 s2 = { __state : reconcile s1.__state s2 }

-- | Runs the actions of an Event Handler and discards the fiber. For events
-- | that update the state concurrently, a function to reconcile its different
-- | versions is applied.
dispatch_
  :: forall s
   . Versionable s
  => ReactThis { } { __state :: s }
  -> EventHandler s (aff :: AFF, effect :: EFFECT) Unit
  -> Effect Unit
dispatch_ this = void <<< dispatch this

-- | Provides an event dispatcher in the context of a dispatchable program that
-- | returns its fiber.
dispatcher
  :: forall s e1 e2
   . Run
      (dispatcher :: DISPATCHER s e2 | e1)
      (EventHandler s e2 Unit -> Effect (Fiber Unit))
dispatcher = lift _dispatcher $ Dispatcher identity

-- | Provides an event dispatcher in the context of a dispatchable program that
-- | discards its fiber.
dispatcher_
  :: forall s e1 e2
   . Run
      (dispatcher :: DISPATCHER s e2 | e1)
      (EventHandler s e2 Unit -> Effect Unit)
dispatcher_ = map (map void) dispatcher

-- | Changes a `Component`'s state type through the lens of a `Traversal`.
-- | For a less restrictive albeit less general version, consider `focus'`.
focus
  :: forall s1 s2 e1 e2 a
   . Monoid a
  => Traversal' s1 s2 -> Component s2 e1 e2 a -> Component s1 e1 e2 a
focus _traversal component =
  join
    <<< lift _reader
    <<< Reader
    <<< map unwrap
    <<< unwrap
    <<< positions _traversal
    <<< Indexed
    <<< Forget
    <<< map Ap $ \s ->
      do
      let index = fst s
      let _get = preview $ element index _traversal
      let _set = set $ element index _traversal
      component
        # _focusDispatcher (focusState _get _set)
        # withReader snd
        # runReader s
        # unsafeCoerce

-- | Changes a `Component`'s state type through the focus of a `Lens`.
-- | For a more general albeit more restrictive version, consider `focus`.
focus'
  :: forall s1 s2 e1 e2
   . Lens' s1 s2 -> (Component s2 e1 e2 ~> Component s1 e1 e2)
focus' _lens component =
  join
    <<< lift _reader
    <<< Reader
    <<< unwrap
    <<< _lens
    <<< Forget $ \s ->
      component
        # _focusDispatcher (focusHandler _lens)
        # runReader s
        # unsafeCoerce

-- | Changes an Event Handler's state type through the focus of a `Lens`.
focusHandler
  :: forall s1 s2 e . Lens' s1 s2 -> (EventHandler s2 e ~> EventHandler s1 e)
focusHandler _lens r =
  case peel r
  of
    Left r' ->
      case on _state Left Right $ unsafeCoerce r'
      of
        Left f -> handleState f
        Right r'' -> send r'' >>= focusHandler _lens
    Right a -> pure a
  where
  _get = view _lens

  _set = set _lens

  handleState (State f2 next) =
    join <<< lift _state <<< State f1 $ \s1 ->
      focusHandler _lens $ next $ _get s1
    where
    f1 s1 = flip _set s1 $ f2 $ _get s1

-- | Changes a `Component`'s state type through the lens of an indexed
-- | traversal.
iFocus
  :: forall s1 s2 i e1 e2 a
   . Monoid a
  => Index s1 i s2
  => IndexedTraversal' i s1 s2
  -> IndexedComponent i s2 e1 e2 a
  -> Component s1 e1 e2 a
iFocus _iTraversal component =
  join
    <<< lift _reader
    <<< Reader
    <<< map unwrap
    <<< unwrap
    <<< _iTraversal
    <<< Indexed
    <<< Forget
    <<< map Ap $ \s ->
      do
      let index = fst s
      let _get = preview $ ix index :: s1 -> Maybe s2
      let _set = set $ ix index
      component
        # _focusDispatcher (focusState _get _set)
        # runReader s
        # unsafeCoerce

-- | Extracts the value inside a Proact component. For events that update the
-- | state concurrently, a function to reconcile its different versions is
-- | applied.
proact
  :: forall s
   . Versionable s
  => ReactThis { } { __state :: s }
  -> Component s (effect :: EFFECT) (aff :: AFF, effect :: EFFECT)
  ~> Effect
proact this component =
  do
  s <- getState this
  component
    # runDispatcher this
    # runReader s.__state
    # runBaseEffect

-- | Translates the extensible effects of the Proact dispatcher.
translate
  :: forall s t e e1 e2
   . (Run e1 Unit -> Run e2 Unit) -> PComponent s t e e1 ~> PComponent s t e e2
translate translator component =
  component
    # unsafeCoerce
    # translateProact
    # unsafeCoerce
  where
  translateProact r =
    case peel r
    of
      Left r' ->
        case on _dispatcher Left Right r'
        of
          Left f -> handleDispatcher f
          Right r'' -> send r'' >>= translateProact
      Right a -> pure a
    where
    handleDispatcher (Dispatcher next) =
      join <<< lift _dispatcher <<< Dispatcher $ \d2 ->
        translateProact
          $ next
          $ d2 <<< unsafeCoerce <<< translator <<< unsafeCoerce

-- Changes the type of a Dispatcher through getter and setter functions.
_focusDispatcher
  :: forall s1 s2 e1 e2 a
   . HandlerFocus s1 s2 e2
  -> Run (dispatcher :: DISPATCHER s2 e2 | e1) a
  -> Run (dispatcher :: DISPATCHER s1 e2 | e1) a
_focusDispatcher handlerFocus r =
  case peel r
  of
    Left r' ->
      case on _dispatcher Left Right $ unsafeCoerce r'
      of
        Left f -> handleDispatcher f
        Right r'' -> send r'' >>= _focusDispatcher handlerFocus
    Right a -> pure a
  where
  handleDispatcher (Dispatcher next) =
    join <<< lift _dispatcher <<< Dispatcher $ \d1 ->
      _focusDispatcher handlerFocus $ next (d1 <<< handlerFocus)

-- Changes the type of the state through getter and setter functions.
focusState
  :: forall s1 s2 e a
   . Monoid a
  => (s1 -> Maybe s2)
  -> (s2 -> s1 -> s1)
  -> Run (state :: STATE s2 | e) a
  -> Run (state :: STATE s1 | e) a
focusState _get _set r =
  case peel r
  of
    Left r' ->
      case on _state Left Right $ unsafeCoerce r'
      of
        Left f -> handleState f
        Right r'' -> send r'' >>= focusState _get _set
    Right a -> pure a
  where
  handleState (State f2 next) =
    join <<< lift _state <<< State f1 $ \s1 ->
      maybe (pure mempty) identity
        $ map (focusState _get _set <<< next)
        $ _get s1
    where
    f1 s1 = maybe s1 identity $ map (flip _set s1 <<< f2) $ _get s1

-- Runs a program by providing to it an Event dispatcher.
runDispatcher
  :: forall s e
   . Versionable s
  => ReactThis { } { __state :: s }
  -> Run (dispatcher :: DISPATCHER s (aff :: AFF, effect :: EFFECT) | e)
  ~> Run e
runDispatcher this = interpret (on _dispatcher handleDispatcher send)
  where
  handleDispatcher
    :: DispatcherF s (aff :: AFF, effect :: EFFECT) ~> Run e
  handleDispatcher (Dispatcher next) = pure $ next (dispatch this)

-- Changes the type of the context in a Reader effect.
withReader
  :: forall r s1 s2
   . (s1 -> s2)
  -> (Run (reader :: READER s2 | r) ~> Run (reader :: READER s1 | r))
withReader _f r =
  case peel r
  of
    Left r' ->
      case on _reader Left Right $ unsafeCoerce r'
      of
        Left f -> handleReader f
        Right r'' -> send r'' >>= withReader _f
    Right a -> pure a
  where
  handleReader (Reader next) =
    join <<< lift _reader <<< Reader $ withReader _f <<< next <<< _f
