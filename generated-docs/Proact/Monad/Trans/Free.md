## Module Proact.Monad.Trans.Free

#### `FreeF`

``` purescript
data FreeF f a b
  = Pure a
  | Free (Unit -> f b)
```

Represents the base Functor for a Free Monad.

##### Instances
``` purescript
(Functor f) => Functor (FreeF f a)
(Functor f) => Bifunctor (FreeF f)
```

#### `FreeT`

``` purescript
newtype FreeT f m a
  = FreeT (m (FreeF f a (FreeT f m a)))
```

Represents the Free Monad Transformer for a Functor `f` and a Monad `m`.

##### Instances
``` purescript
Newtype (FreeT f m a) _
(Functor f, Functor m) => Functor (FreeT f m)
(Functor f, Monad m, Semigroup a) => Semigroup (FreeT f m a)
(Functor f, Monad m, Monoid a) => Monoid (FreeT f m a)
(Functor f, Monad m) => Apply (FreeT f m)
(Functor f, Monad m) => Applicative (FreeT f m)
(Functor f, Monad m) => Bind (FreeT f m)
(Functor f, Monad m) => Monad (FreeT f m)
(Functor f) => MonadTrans (FreeT f)
(Functor f, Monad m) => MonadFree f (FreeT f m)
(Functor f) => HoistT (FreeT f)
InterpretT FreeT
```

#### `iterT`

``` purescript
iterT :: forall f m a. Functor f => Monad m => (f (m a) -> m a) -> FreeT f m a -> m a
```

Folds a `FreeT` Monad Transformer into its underlying Monad using an
Algebra.

#### `runFreeT`

``` purescript
runFreeT :: forall f m a. FreeT f m a -> m (FreeF f a (FreeT f m a))
```

Deconstructs a `FreeT` Monad Transformer.


### Re-exported from Proact.Monad.Class.MonadFree:

#### `MonadFree`

``` purescript
class (Functor f, Monad m) <= MonadFree f m | m -> f
```

Represents all Monads that do no work during the normalization step beyond
simply grafting two monadic values together.

### Re-exported from Proact.Trans.Class.Hoist:

#### `HoistT`

``` purescript
class HoistT t  where
  hoist :: forall f g. Functor f => Functor g => (f ~> g) -> ((t f) ~> (t g))
```

Represents all transformers that support a natural transformation.

##### Instances
``` purescript
HoistT (ReaderT r)
HoistT (WriterT w)
HoistT (StateT s)
HoistT (EnvT e)
HoistT (TracedT t)
HoistT (StoreT s)
```

### Re-exported from Proact.Trans.Class.Interpret:

#### `InterpretT`

``` purescript
class InterpretT t  where
  interpret :: forall f g. Functor f => Functor g => (f ~> g) -> (forall m. Functor m => (t f m) ~> (t g m))
```

Represents all transformers' transformers that support a natural
transformation.

