## Module Proact

Proact is a web framework inspired by Thermite that provides a
model-view-dispatcher abstraction on top of `purescript-react`. It exposes
a clean monadic API to construct and compose components and to define event
handlers with minimal code overhead. Unlike other dispatch architectures
that use messages to communicate components, Proact dispatches event
actions themselves which means there is less boilerplate code and more
rapid development.

#### `Proactive`

``` purescript
class (Monad m) <= Proactive fx state m | m -> fx, m -> state where
  readState :: m state
  dispatcher :: m (Dispatcher fx state)
```

The `Proactive` type class represents any Proact component that provides
access to its underlying state via the `readState` function and to a
dispatcher able to execute the actions of an event handler via the
`dispatcher` function.

An implementation is given for `Component` and for `IndexedComponent`.

Laws:

- readState *> readState = readState
- dispatcher *> dispatcher = dispatcher

##### Instances
``` purescript
Proactive fx state (Component fx state)
Proactive fx state (IndexedComponent fx index state)
```

#### `Component`

``` purescript
newtype Component fx state a
```

A monadic representation of a React component's GUI providing access to the
underlying state through the `MonadAsk` interface.

##### Instances
``` purescript
Functor (Component fx state)
Apply (Component fx state)
Applicative (Component fx state)
Bind (Component fx state)
Monad (Component fx state)
MonadAsk state (Component fx state)
MonadEff fx (Component fx state)
Proactive fx state (Component fx state)
```

#### `EventHandler`

``` purescript
newtype EventHandler fx state a
```

A monadic representation of an event handler that manipulates the
component's state through the `MonadState` interface.

##### Instances
``` purescript
Functor (EventHandler fx state)
Apply (EventHandler fx state)
Applicative (EventHandler fx state)
Bind (EventHandler fx state)
Monad (EventHandler fx state)
MonadAff fx (EventHandler fx state)
MonadEff fx (EventHandler fx state)
MonadState state (EventHandler fx state)
```

#### `IndexedComponent`

``` purescript
newtype IndexedComponent fx index state a
```

A monadic representation of a React component's GUI that is an element of
an indexed data structure.
It provides access to the component's underlying state and its index within
the collection through the `MonadAsk` interface.

##### Instances
``` purescript
Functor (IndexedComponent fx index state)
Apply (IndexedComponent fx index state)
Applicative (IndexedComponent fx index state)
Bind (IndexedComponent fx index state)
Monad (IndexedComponent fx index state)
MonadAsk (Tuple index state) (IndexedComponent fx index state)
MonadEff fx (IndexedComponent fx index state)
Proactive fx state (IndexedComponent fx index state)
```

#### `This`

``` purescript
data This fx props state
  = ReactThis (ReactThis props state)
```

A composable representation of the underlying React `this` object.

#### `Dispatcher`

``` purescript
type Dispatcher fx state = EventHandler fx state Unit -> Eff (EventFx fx) Unit
```

A type synonym for a dispatcher that is accessible from a Component and
that executes the actions of an event handler provided to it.

#### `EventFx`

``` purescript
type EventFx fx = (props :: ReactProps, refs :: ReactRefs ReadOnly, state :: ReactState ReadWrite | fx)
```

A type synonym for the effects associated to events of React elements.

#### `dispatcher'`

``` purescript
dispatcher' :: forall fx props state. This fx props state -> Dispatcher fx state
```

Retrieves a dispatcher from the context of any React component. Once the
dispatcher receives an event handler it will execute its asynchronous code.

#### `focus`

``` purescript
focus :: forall fx state1 state2 render. Monoid render => Traversal' state1 state2 -> Component fx state2 render -> Component fx state1 render
```

Changes a component's state type through the lens of a traversal.
For a less restrictive albeit less general version, consider `focus'`.

#### `focus'`

``` purescript
focus' :: forall fx state1 state2 render. Lens' state1 state2 -> Component fx state2 render -> Component fx state1 render
```

Changes a component's state type through a lens.
For a more general albeit more restrictive version, consider `focus`.

#### `focusThis`

``` purescript
focusThis :: forall fx props state1 state2. Lens' state1 state2 -> This fx props state1 -> This fx props state2
```

Changes the state type of the underlying React `this` object through a
lens.

#### `iFocus`

``` purescript
iFocus :: forall fx index state1 state2 render. Monoid render => Index state1 index state2 => IndexedTraversal' index state1 state2 -> IndexedComponent fx index state2 render -> Component fx state1 render
```

Changes a component's state type through the lens of an indexed traversal.

#### `spec`

``` purescript
spec :: forall fx state. Component fx state ReactElement -> state -> ReactSpec {  } state fx
```

Creates a `ReactSpec` from a Proact Component.


