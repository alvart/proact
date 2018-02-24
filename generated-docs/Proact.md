## Module Proact

Proact is the core library for a family of web frameworks built from
composable components that share a singular state which is composed and
decomposed through Profunctor lenses. This is unlike other functional web
architectures that compose state through user-provided messages that become
boilerplate code in the long run.
Proact also separates the pure elements of a program from its side effects
by using Free commands which are later paired with Cofree actions to create
executable functions.  This strategy was chiefly derived from the
"Free for DSLs, cofree for interpreters" series by Dave Laing.

A web framework using Facebook's React library is provided in this package.

#### `ComponentT`

``` purescript
newtype ComponentT s t n f w g m a
  = ComponentT (ReaderT (VaultT t n f w Unit) (FreeT g (ReaderT s m)) a)
```

A monadic representation of a component that provides access to its
underlying state through the `MonadAsk` interface.

##### Instances
``` purescript
Newtype (ComponentT s t n f w g m a) _
(Functor g, Functor m) => Functor (ComponentT s t n f w g m)
(Functor g, Monad m) => Apply (ComponentT s t n f w g m)
(Functor g, Monad m) => Applicative (ComponentT s t n f w g m)
(Functor g, Monad m) => Bind (ComponentT s t n f w g m)
(Functor g, Monad m) => Monad (ComponentT s t n f w g m)
(Functor g, Monad m) => MonadAsk s (ComponentT s t n f w g m)
(Functor g) => MonadTrans (ComponentT s t n f w g)
(Functor g, Monad m) => MonadFree g (ComponentT s t n f w g m)
(Functor g) => HoistT (ComponentT s t n f w g)
InterpretT (ComponentT s t n f w)
```

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

#### `VaultT`

``` purescript
newtype VaultT s m f w a
  = VaultT (CofreeT f (StoreT (Maybe s) w) (m a))
```

Represents a state repository built with a Comonad Transformer.

##### Instances
``` purescript
Newtype (VaultT s m f w a) _
(Functor f) => HoistT (VaultT s m f)
InterpretT (VaultT s m)
```

#### `cohoist`

``` purescript
cohoist :: forall s t n f w v g m a. Functor f => Functor w => Functor v => (w ~> v) -> ComponentT s t n f v g m a -> ComponentT s t n f w g m a
```

Applies a hoist transformation to the state repository of a `ComponentT`.

#### `cointerpret`

``` purescript
cointerpret :: forall s t n f h w g m a. Functor f => Functor h => Functor w => (f ~> h) -> ComponentT s t n h w g m a -> ComponentT s t n f w g m a
```

Applies an interpret transformation to the state repository of a
`ComponentT`.

#### `dispatch`

``` purescript
dispatch :: forall s n f w g m. Functor f => Functor g => Comonad w => Monad m => Pairing w m => PairingM f g n => Monad n => VaultT s n f w Unit -> EventHandlerT s g m Unit -> n Unit
```

Pairs a `CofreeT` vault with a `FreeT` event handler to trigger an event
action.

#### `dispatcher`

``` purescript
dispatcher :: forall s t n f w g m. Functor f => Functor g => Comonad w => Monad m => Pairing w m => PairingM f g n => Monad n => ComponentT s t n f w g m (EventHandlerT t g m Unit -> n Unit)
```

Provides an action dispatcher in the context of a Proact `ComponentT`.

#### `focus`

``` purescript
focus :: forall s1 s2 n f w g m a. Functor f => Functor g => Comonad w => Monad m => Monoid a => Traversal' s1 s2 -> ComponentT s2 s2 n f w g m a -> ComponentT s1 s1 n f w g m a
```

Changes a `ComponentT`'s state type through the lens of a `Traversal`.
For a less restrictive albeit less general version, consider `focus'`.

#### `focus'`

``` purescript
focus' :: forall s1 s2 n f w g m a. Functor f => Functor g => Comonad w => Monad m => Lens' s1 s2 -> ComponentT s2 s2 n f w g m a -> ComponentT s1 s1 n f w g m a
```

Changes a `ComponentT`'s state type through a `Lens`.
For a more general albeit more restrictive version, consider `focus`.

#### `focusVault`

``` purescript
focusVault :: forall s1 s2 f w m a. Functor f => Comonad w => Lens' s1 s2 -> VaultT s1 m f w a -> VaultT s2 m f w a
```

Changes the type of the state repository through a `Lens`.

#### `iFocus`

``` purescript
iFocus :: forall s1 s2 i n f w g m a. Functor f => Functor g => Comonad w => Monad m => Monoid a => Index s1 i s2 => IndexedTraversal' i s1 s2 -> ComponentT (Tuple i s2) s2 n f w g m a -> ComponentT s1 s1 n f w g m a
```

Changes a component's state type through the lens of an indexed traversal.

#### `render`

``` purescript
render :: forall s n f w g m a. Functor f => Functor g => Comonad w => Monad m => Monad n => PairingM f g n => Pairing w m => Monoid a => VaultT s n f w Unit -> VaultT s n f w Unit -> ComponentT s s n f w g m a -> n a
```

Renders a `ComponentT` in a monadic context.


