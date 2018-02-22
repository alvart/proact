## Module Proact.Functor.Pairing

#### `Pairing`

``` purescript
class Pairing f g | f -> g, g -> f where
  pair :: forall a b c. (a -> b -> c) -> f a -> g b -> c
```

Represents all pairings between functors f and g.
This asserts that any sums in f can annihilate any products in g, and vice
versa.

##### Instances
``` purescript
Pairing Identity Identity
(Pairing f g) => Pairing (EnvT e f) (ReaderT e g)
(Pairing f g) => Pairing (TracedT t f) (WriterT t g)
(Pairing f g) => Pairing (StoreT s f) (StateT s g)
(Pairing f g, Pairing w m) => Pairing (CofreeT f w) (FreeT g m)
```

#### `PairingM`

``` purescript
class PairingM f g m | f -> g, g -> f where
  pairM :: forall a b c. (a -> b -> m c) -> f a -> g b -> m c
```

Represents a pairing with side effects.

#### `sym`

``` purescript
sym :: forall f g a b c. Pairing f g => (a -> b -> c) -> g a -> f b -> c
```

All pairings are symmetric.

#### `zap`

``` purescript
zap :: forall f g a b. Pairing f g => f (a -> b) -> g a -> b
```

Applies the pair of a function and an argument.


