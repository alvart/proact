## Module Proact.Monad.Class.MonadFree

#### `MonadFree`

``` purescript
class (Functor f, Monad m) <= MonadFree f m | m -> f where
  layer :: forall a. f (m a) -> m a
  liftFree :: forall a. f a -> m a
```

Represents all Monads that do no work during the normalization step beyond
simply grafting two monadic values together.


