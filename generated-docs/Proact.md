## Module Proact

Proact is a web framework inspired by Thermite that provides a
model-view-dispatcher abstraction on top of `purescript-react`. It exposes
a clean monadic API to construct and compose components and to define event
handlers with minimal code overhead. Unlike other dispatch architectures
that use messages to communicate components, Proact dispatches event
actions themselves which means there is less boilerplate code and more
rapid development.

#### `Component`

``` purescript
newtype Component fx state a
```

A monadic representation of a component's GUI that provides access to its
state through the `MonadAsk` interface.

##### Instances
``` purescript
Newtype (Component fx state a) _
Functor (Component fx state)
Apply (Component fx state)
Applicative (Component fx state)
Bind (Component fx state)
Monad (Component fx state)
MonadAsk state (Component fx state)
MonadEff fx (Component fx state)
```

#### `EventHandler`

``` purescript
newtype EventHandler fx state a
```

A monadic representation of an event handler that manipulates the
component's state through the `MonadState` interface.

##### Instances
``` purescript
Newtype (EventHandler fx state a) _
Functor (EventHandler fx state)
Apply (EventHandler fx state)
Applicative (EventHandler fx state)
Bind (EventHandler fx state)
Monad (EventHandler fx state)
MonadAff fx (EventHandler fx state)
MonadEff fx (EventHandler fx state)
MonadState state (EventHandler fx state)
```

#### `This`

``` purescript
data This fx props state
  = ReactThis (ReactThis props state)
```

A composable representation of the underlying React `this` object.

#### `EventDispatcher`

``` purescript
type EventDispatcher fx state = forall event. (event -> EventHandler fx state Unit) -> (event -> Eff (ReactContext fx) Unit)
```

A type synonym for an event handler that is accessible from a Component.
An event object may be later provided to the handler to trigger an event
action.

#### `ReactContext`

``` purescript
type ReactContext fx = (props :: ReactProps, refs :: ReactRefs ReadOnly, state :: ReactState ReadWrite | fx)
```

A type synonym for effects associated to React components.

#### `eventDispatcher`

``` purescript
eventDispatcher :: forall fx state. Component fx state (EventDispatcher fx state)
```

Retrieves an event dispatcher from the current Component's context. Once
the dispatcher receives an event handler and an event it will execute the
asynchronous actions of the handler.

#### `eventDispatcher'`

``` purescript
eventDispatcher' :: forall fx props state. This fx props state -> EventDispatcher fx state
```

Retrieves an event dispatcher from the context of any React component. Once
the dispatcher receives an event handler and an event it will execute the
asynchronous actions of the handler.

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

#### `spec`

``` purescript
spec :: forall fx state. Component fx state ReactElement -> state -> ReactSpec {  } state fx
```

Creates a `ReactSpec` from a Proact Component.


