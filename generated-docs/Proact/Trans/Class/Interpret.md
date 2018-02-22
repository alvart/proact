## Module Proact.Trans.Class.Interpret

#### `InterpretT`

``` purescript
class InterpretT t  where
  interpret :: forall f g. Functor f => Functor g => (f ~> g) -> (forall m. Functor m => (t f m) ~> (t g m))
```

Represents all transformers' transformers that support a natural
transformation.


