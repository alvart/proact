## Module Proact.Comonad.Trans.Cofree

#### `CofreeF`

``` purescript
data CofreeF f a b
  = CofreeF a (Unit -> (f b))
```

##### Instances
``` purescript
(Functor f) => Functor (CofreeF f a)
(Functor f) => Bifunctor (CofreeF f)
```

#### `CofreeT`

``` purescript
newtype CofreeT f w a
  = CofreeT (w (CofreeF f a (CofreeT f w a)))
```

##### Instances
``` purescript
Newtype (CofreeT f w a) _
(Functor f, Functor w) => Functor (CofreeT f w)
(Functor f, Comonad w) => Extend (CofreeT f w)
(Functor f, Comonad w) => Comonad (CofreeT f w)
ComonadTrans (CofreeT f)
(Functor f, Comonad w) => ComonadCofree f (CofreeT f w)
(Functor f) => HoistT (CofreeT f)
InterpretT (CofreeT)
```

#### `coiterT`

``` purescript
coiterT :: forall f w a. Functor f => Comonad w => (w a -> f (w a)) -> w a -> CofreeT f w a
```

Unfolds a `CofreeT` Comonad Transformer from a Coalgebra and an initial
Comonad.

#### `runCofreeT`

``` purescript
runCofreeT :: forall f w a. CofreeT f w a -> w (CofreeF f a (CofreeT f w a))
```

Deconstructs a `CofreeT` Comonad Transformer.


### Re-exported from Proact.Comonad.Class.ComonadCofree:

#### `ComonadCofree`

``` purescript
class (Functor f, Comonad w) <= ComonadCofree f w | w -> f
```

Represents all Comonads that do no work during the extension step beyond
simply extracting two comonadic values together.

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

