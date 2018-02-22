## Proact

Proact is a core library for a family of web frameworks that allow users to define and encapsulate components in Monads to later be composed with profunctor lenses, prisms or traversals. Actions performing on the states of the components are listed in event handlers and attached to native HTML controls.
These actions are expressed in Free commands so that they're separated from their side-effects. The user is then responsible for implementing the function that will trigger such side-effects in response to the commands.

A web framework using `purescipt-react` is provided in this package.

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

## Overview

Creating components in Proact is meant to be quick and simple:

1. Define the state of the component.
2. Define a GUI that renders according to the state.
3. Fetch the state and pass it on to the GUI.
4. Define event handlers that will change the state of the component as a response to HTML control events.
5. Fetch an event dispatcher and use it to link handlers and HTML controls together.

Optionally, if while rendering a component or triggering an event action a side-effect is required, the component will need a Free definition for this action which can be provided specifically in a type or generically by a type class.

Once you're app components are defined, integrate them simply and elegantly by `focus`ing on them from their parent component. Focusing requires lens functions which are usually very easy to implement for most state types.

## Examples

- [To-do Application](https://github.com/alvart/proact-todo). The "Hello World!" of web applications.
- [Chat Application](https://github.com/alvart/proact-chat). A more complex application showcasing side-effects in its components' actions.
