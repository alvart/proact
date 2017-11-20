## Proact

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

### Single component

```purescript
-- Counter.purs

import Control.Comonad (extract)
import Prelude
import Proact as P
import React.DOM as R

type State = Additive Int

clickCounter :: forall fx . P.UIComponent fx State R.ReactElement
clickCounter =
  do
  counter <- extract <$> P.get
  handler <- P.eventHandler
  pure $ display counter handler
  where
  onClick = P.modify $ map (_ + 1)

  display counter handler =
    R.div'
      [ R.div' [R.text ("# clicks: " <> show counter)]
      , R.button [R.onClick (handler onClick)] [R.text "Increment"]
      ]
```

### Tab component

```purescript
-- Tab.purs

import Control.Alt((<|>))
import Data.Lens ((.~), (^.), (^?), Lens', Prism', lens, prism)
import Data.Maybe (maybe)
import Prelude
import Proact as P
import React.DOM as R

import Counter as Counter

infixr 9 o as ..

o :: forall a b c d. Semigroupoid a => a c d -> a b c -> a b d
o = (<<<)

data Display
  = Page1 Counter.State
  | Page2 Counter.State

_Page1 :: Prism' Display Counter.State
_Page1 =
  prism Page1
    \page ->
      case page
        of
        Page1 page1 -> Right page1
        _ -> Left page

_Page2 :: Prism' Display Counter.State
_Page2 =
  prism Page2
    \page ->
      case page
        of
        Page2 page2 -> Right page2
        _ -> Left page

type State =
  { display :: Display
  , page1 :: Counter.State
  , page2 :: Counter.State
  }

_display :: Lens' State Display
_display = lens _.display (_ { display = _ })

_page1 :: Lens' State Counter.State
_page1 = lens _.page1 (_ { page1 = _ })

_page2 :: Lens' State Counter.State
_page2 = lens _.page2 (_ { page2 = _ })

tab :: forall fx . P.UIComponent fx State R.ReactElement
tab =
  do
  handler <- P.eventHandler
  page1 <- P.focus _display $ P.choose _Page1 counter
  page2 <- P.focus _display $ P.choose _Page2 counter
  pure $ display handler (page1 <|> page2)
  where
  display handler page =
    R.div'
    [
      R.div'
        [ R.a [R.onClick $ handler goPage1] [R.text "Page 1"]
        , R.text " "
        , R.a [R.onClick $ handler goPage2] [R.text "Page 2"]
        ]
    , R.div' page
    ]

  goPage1 =
    do
    state <- P.get

    -- Backup the page that will be displaced from the view.
    let page2 = state ^? _display .. _Page2
    P.modify $ maybe id (_page2 .~ _) page2

    -- Switch the view to the new page.
    let page1 = state ^. _page1
    P.modify $ _display .~ Page1 page1

  goPage2 =
    do
    state <- P.get

    -- Backup the page that will be displaced from the view.
    let page1 = state ^? _display .. _Page1
    P.modify $ maybe id (_page1 .~ _) page1

    -- Switch the view to the new page.
    let page2 = state ^. _page2
    P.modify $ _display .~ Page2 page2
```
