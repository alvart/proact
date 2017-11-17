## purescript-thermite

Proact is a model-view-dispatcher web framework on top of `purescript-react`.

It exposes a clean monadic API to construct and compose components and to define event handlers with minimal code overhead. Unlike other dispatch architectures that use messages to communicate components, Proact dispatches event actions themselves which means there is less boilerplate code and more rapid development.

The components work on an underlying asynchronous layer which allows for cleaner asynchronous code but has the side effect that the rendering of the component will be delayed. Because of this, the user is encouraged to provide a "splash screen" that will show while the component loads the actual GUI.

## Installation
```sh
git clone https://github.com/alvart/proact.git proact
cd proact
npm install
bower install
```

## Building

```sh
npm run build
```

## Testing

```sh
npm run browserify
```

Once the Proact library and the example code is compiled, an index.js file will be generated in the html directory of this project, open the web page in a browser to see it running.

## Overview

Creating components in Proact is meant to be quick and simple:

1. Define the state type of your component.
2. Fetch the state of the component.
3. Fetch an event handler that the HTML controls will pass messages to.
4. Define the event actions to be triggered once an event is fired.
5. Create a GUI based on the state and link its HTML controls to their event logic.

Once you're app components are defined, integrate them simply and elegantly by `focus`ing on them from their parent component. Focusing only requires a lens function which is usually very easy to define for most state types.

## Examples

More examples supporting more complex scenarios coming soon!

```purescript
-- Counter.purs

import Prelude
import Proact as P
import React.DOM as R

type State = Int

clickCounter :: forall fx . P.UIComponent fx State R.ReactElement
clickCounter =
  do
  counter <- P.get
  handler <- P.eventHandler
  pure $ display counter handler
  where
  onClick = P.modify (_ + 1)

  display counter handler =
    R.div'
      [ R.div' [R.text ("# clicks: " <> show counter)]
      , R.button [R.onClick (handler onClick)] [R.text "Increment"]
      ]
```
