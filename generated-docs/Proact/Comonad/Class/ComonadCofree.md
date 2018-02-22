## Module Proact.Comonad.Class.ComonadCofree

#### `ComonadCofree`

``` purescript
class (Functor f, Comonad w) <= ComonadCofree f w | w -> f where
  lowerCofree :: forall a. w a -> f a
  peel :: forall a. w a -> f (w a)
```

Represents all Comonads that do no work during the extension step beyond
simply extracting two comonadic values together.


