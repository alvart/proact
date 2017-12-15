## Proact

Proact is an event driven web framework on top of `purescript-react`.

It allows the user to define and encapsulate components in Monads and later compose them using profunctor lenses, prisms and traversals. Actions performing on the components' states are handled by event listeners which in turn are attached to native HTML controls.

The components work on the Eff monad context when rendering their layout and have read-only access to the state at this point. When they're handling events from the HTML controls, however, they have read & write access to the state and can work on top of the Aff monad context which means asynchronous logic.

The components work on the Aff monad context which allows for cleaner asynchronous code. This, however, has the side effect that the rendering of the component will be asynchronous itself, therefore, the user is encouraged to provide a "splash screen" to be shown while the component loads the actual GUI.

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
npm run demo:todo
```

Once the Proact library and the example code is compiled, an index.js file will be generated in the html directory of this project, open the web page in a browser to see it running.

## Overview

Creating components in Proact is meant to be quick and simple:

1. Define the state of your component.
2. Define a GUI that renders according to the state.
3. Fetch the state and pass it on to the GUI.
4. Define event handlers that will change the state of the component as a response to HTML control events.
5. Fetch an event dispatcher and use it to link handlers and HTML controls together.

Once you're app components are defined, integrate them simply and elegantly by `focus`ing on them from their parent component. Focusing only requires a lens function which is usually very easy to define for most state types.

## Examples

- [To-do Application](https://github.com/alvart/proact/tree/master/examples/todo). The "Hello World!" of web applications.
