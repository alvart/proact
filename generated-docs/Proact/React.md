## Module Proact.React

#### `ComponentT`

``` purescript
type ComponentT s f w g m = ComponentT s s Effect f w g m
```

A type synonym for a React Component.

#### `IndexedComponentT`

``` purescript
type IndexedComponentT i s f w g m = ComponentT (Tuple i s) s Effect f w g m
```

A type synonym for an Indexed React Component.

#### `ReactStoreT`

``` purescript
type ReactStoreT s w a = StoreT (Maybe s) w (Effect a)
```

A type synonym for a `StoreT` containing the application's state.

#### `dispatch`

``` purescript
dispatch :: forall s1 s2 f w g m. Functor f => Functor g => Comonad w => Monad m => Pairing w m => PairingM f g Effect => Lens' {  | s1 } {  | s2 } -> (ReactStoreT {  | s1 } w Unit -> f (ReactStoreT {  | s1 } w Unit)) -> ReactStoreT {  | s1 } w Unit -> ReactThis {  } {  | s1 } -> EventHandlerT {  | s2 } g m Unit -> Effect Unit
```

Dispatches React actions detached from the context of a Component. The
state handled by the actions is seen through a given lens.

#### `dispatch'`

``` purescript
dispatch' :: forall s f w g m. Functor f => Functor g => Comonad w => Monad m => Pairing w m => PairingM f g Effect => (ReactStoreT {  | s } w Unit -> f (ReactStoreT {  | s } w Unit)) -> ReactStoreT {  | s } w Unit -> ReactThis {  } {  | s } -> EventHandlerT {  | s } g m Unit -> Effect Unit
```

Dispatches React actions detached from the context of a Component.

#### `render`

``` purescript
render :: forall s f w g m. Functor f => Functor g => Comonad w => Monad m => Pairing w m => PairingM f g Effect => (ReactStoreT {  | s } w Unit -> f (ReactStoreT {  | s } w Unit)) -> ReactStoreT {  | s } w Unit -> ReactThis {  } {  | s } -> ComponentT {  | s } f w g m ReactElement -> Effect ReactElement
```

Renders a `ReactElement` from a React Context, a Coalgebra and a Proact
Component.


### Re-exported from Proact:

#### `EventHandlerT`

``` purescript
newtype EventHandlerT s f m a
  = EventHandlerT (FreeT f (StateT s m) a)
```

A monadic representation of an event handler that manipulates the
component's state through the `MonadState` interface.

##### Instances
``` purescript
Newtype (EventHandlerT s f m a) _
(Functor f, Functor m) => Functor (EventHandlerT s f m)
(Functor f, Monad m) => Apply (EventHandlerT s f m)
(Functor f, Monad m) => Applicative (EventHandlerT s f m)
(Functor f, Monad m) => Bind (EventHandlerT s f m)
(Functor f, Monad m) => Monad (EventHandlerT s f m)
(Functor f, Monad m) => MonadState s (EventHandlerT s f m)
(Functor f, Monad m) => MonadTrans (EventHandlerT s f)
(Functor f, Monad m) => MonadFree f (EventHandlerT s f m)
(Functor f) => HoistT (EventHandlerT s f)
InterpretT (EventHandlerT s)
```

#### `iFocus`

``` purescript
iFocus :: forall s1 s2 i n f w g m a. Functor f => Functor g => Comonad w => Monad m => Monoid a => Index s1 i s2 => IndexedTraversal' i s1 s2 -> ComponentT (Tuple i s2) s2 n f w g m a -> ComponentT s1 s1 n f w g m a
```

Changes a component's state type through the lens of an indexed traversal.

#### `focus'`

``` purescript
focus' :: forall s1 s2 n f w g m a. Functor f => Functor g => Comonad w => Monad m => Lens' s1 s2 -> ComponentT s2 s2 n f w g m a -> ComponentT s1 s1 n f w g m a
```

Changes a `ComponentT`'s state type through a `Lens`.
For a more general albeit more restrictive version, consider `focus`.

#### `focus`

``` purescript
focus :: forall s1 s2 n f w g m a. Functor f => Functor g => Comonad w => Monad m => Monoid a => Traversal' s1 s2 -> ComponentT s2 s2 n f w g m a -> ComponentT s1 s1 n f w g m a
```

Changes a `ComponentT`'s state type through the lens of a `Traversal`.
For a less restrictive albeit less general version, consider `focus'`.

#### `dispatcher`

``` purescript
dispatcher :: forall s t n f w g m. Functor f => Functor g => Comonad w => Monad m => Pairing w m => PairingM f g n => Monad n => ComponentT s t n f w g m (EventHandlerT t g m Unit -> n Unit)
```

Provides an action dispatcher in the context of a Proact `ComponentT`.

