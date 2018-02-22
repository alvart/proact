## Module Proact.Trans.Class.Hoist

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


