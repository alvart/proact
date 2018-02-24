## Module Proact.React

#### `ComponentT`

``` purescript
type ComponentT s f w g m = ComponentT s s IOSync f w g m
```

A type synonym for a React Component.

#### `Dispatcher`

``` purescript
type Dispatcher s f m a = EventHandlerT s f m a -> ReactEff a
```

A type synonym for a dispatcher that is accessible from a React Component
and that executes the actions of an event handler provided to it.

#### `IndexedComponentT`

``` purescript
type IndexedComponentT i s f w g m = ComponentT (Tuple i s) s IOSync f w g m
```

A type synonym for an Indexed React Component.

#### `ReactEff`

``` purescript
type ReactEff = Eff (props :: ReactProps, refs :: ReactRefs ReadOnly, state :: ReactState ReadWrite)
```

A type synonym for effects coming from React elements.

#### `dispatch`

``` purescript
dispatch :: forall s1 s2 f w g m. Functor f => Functor g => Comonad w => Monad m => Pairing w m => PairingM f g IOSync => Lens' s1 s2 -> (ReactStoreT s1 w Unit -> f (ReactStoreT s1 w Unit)) -> ReactStoreT s1 w Unit -> ReactThis {  } s1 -> Dispatcher s2 g m Unit
```

Dispatches React actions detached from the context of a Component. The
state handled by the actions is seen through a given lens.

#### `dispatcher`

``` purescript
dispatcher :: forall s t f w g m. Functor f => Functor g => Comonad w => Pairing w m => PairingM f g IOSync => Monad m => ComponentT s t IOSync f w g m (Dispatcher t g m Unit)
```

Provides a dispatcher in the context of a React Component.

#### `spec`

``` purescript
spec :: forall s e f w g m. Functor f => Functor g => Comonad w => Monad m => Pairing w m => PairingM f g IOSync => (ReactStoreT s w Unit -> f (ReactStoreT s w Unit)) -> ReactStoreT s w Unit -> ComponentT s f w g m ReactElement -> ReactSpec {  } s ReactElement e
```

Creates a `ReactSpec` from a Coalgebra and a React Component.


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

