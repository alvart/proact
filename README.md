## Proact

Proact is an event driven web framework on top of `purescript-react`.

It allows the user to define and encapsulate components in Monads and later compose them using profunctor lenses, prisms and traversals. Actions performing on the components' states are handled by event listeners which in turn are attached to native HTML controls.

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

### Utility functions

```purescript
infixr 9 o as ..

o :: forall a b c d . Semigroupoid a => a c d -> a b c -> a b d
o = (<<<)

_additive :: forall a . Iso' (Additive a) a
_additive = iso unwrap Additive

_this :: forall a . Iso' a a
_this = id

unwrap2 :: forall a b c d . Newtype c d => (a -> b -> c) -> (a -> b -> d)
unwrap2 fabc a b = unwrap $ fabc a b
```

### Single component

```purescript
-- Counter.purs: Isomorphisms

-- When designing this component we're mindful that its state might be condensed
-- by its container app.
type State = Additive Int

counter :: forall fx . P.UIComponent fx State R.ReactElement
counter =
  -- We can work in our component assuming its state is an integer and wrap it
  -- at the end in the Additive Monoid, doing so makes the code cleaner and more
  -- elegant.
  (P.focus' _additive)
    do
    -- We get the state as an integer.
    clicks <- P.get

    -- Event handlers are action Monoids that can be composed, for now we're not
    -- interested in that behavior so we unwrap the Monoid.
    handler <- unwrap2 <$> P.eventHandler

    -- It's cleaner to define the GUI in a pure separate function.
    pure $ display clicks handler
  where
  display clicks handler =
    R.div'
      [ R.div' [R.text $ "# clicks: " <> show clicks]
      , R.button [R.onClick $ handler onClick] [R.text "Increment"]
      ]

  onClick = _this %= (_ + 1)
```

It's important to note that there are two different monadic contexts in this example: UIComponent and EventComponent. In general, it's recommended to only read the state when on the UIComponent context as this logic will be executed every time the component has its state updated (writing the state on this context won't trigger an infinite loop, though). The EventComponent context is designed to work as a reaction to control events, this is the best place to include logic that alters the state.

### Tab component

```purescript
-- Tab.purs: Lenses & Prisms

import Counter as Counter

-- When designing this component we're mindful that its state might be condensed
-- by its container app.
data DisplayState
  = Page1 Counter.State
  | Page2 Counter.State

instance semigroupDisplayState :: Semigroup (DisplayState)
  where
  append (Page1 state1) (Page1 state2) = Page1 $ state1 <> state2
  append (Page2 state1) (Page2 state2) = Page2 $ state1 <> state2
  append page _ = page

type State' =
  { display :: DisplayState
  , page1 :: Counter.State
  , page2 :: Counter.State
  }

newtype State = State State'

derive instance newtypeState :: Newtype (State) _

instance semigroupState :: Semigroup (State)
  where
  append (State state1) (State state2) =
    State
      { display : state1.display <> state2.display
      , page1 : state1.page1 <> state2.page1
      , page2 : state1.page2 <> state2.page2
      }

-- We define Prisms to query whether a specific page is selected.
_Page1 :: Prism' DisplayState CounterState
_Page1 = prism Page1 _page1
  where
  _page1 (Page1 page1) = Right page1
  _page1 page = Left page

_Page2 :: Prism' DisplayState CounterState
_Page2 = prism Page2 _page2
  where
  _page2 (Page2 page2) = Right page2
  _page2 page = Left page

-- We define three lenses: One to access the page currently selected and the
-- other two to store the state of the pages whenever they're unselected and
-- their states are removed from the entire page altogether.
_display :: Lens' State' DisplayState
_display = lens _.display (_ { display = _ })

_page1 :: Lens' State' Counter.State
_page1 = lens _.page1 (_ { page1 = _ })

_page2 :: Lens' State' Counter.State
_page2 = lens _.page2 (_ { page2 = _ })

tab :: forall fx . P.UIComponent fx State R.ReactElement
tab =
  -- Working with Records it's cleaner than working with Algebraic Data Types.
  (P.focus' _new)
    do
    handler <- unwrap2 <$> P.eventHandler

    -- Prisms only work on Monoidal states and Monoidal monad results. This is
    -- to address the scenario where the prism may fail to select a state and
    -- we're forced to return a `mempty` result.
    -- The counter state is already a Monoid (Additive) but the GUI element is
    -- not (it's a ReactElement) so we lift it to the free Monoid: an Array.
    let _counter = map singleton counter
    page1 <- P.focus (_display .. _Page1) _counter
    page2 <- P.focus (_display .. _Page2) _counter

    pure $ display handler (page1 <|> page2)
  where
  display handler page =
    R.div'
      [
        R.div'
          [ R.a [R.onClick $ handler goPage1] [R.text "Page 1"]
          , R.text " | "
          , R.a [R.onClick $ handler goPage2] [R.text "Page 2"]
          ]
      , R.div' page
      ]

  goPage1 =
    unsafePartial
      do
      state <- get

      -- Backup the page that will be displaced from the view.
      let Just page2 = state ^? _display .. _Page2
      _page2 .= page2

      -- Switch the view to the new page.
      let page1 = state ^. _page1
      _display .= Page1 page1

  goPage2 =
    unsafePartial
      do
      state <- get

      -- Backup the page that will be displaced from the view.
      let Just page1 = state ^? _display .. _Page1
      _page1 .= page1

      -- Switch the view to the new page.
      let page2 = state ^. _page2
      _display .= Page2 page2
```

### App component

Time to put together the app, at last!! ... or almost, right now this framework only supports isomorphisms, lenses, prisms and traversals, which is quite a lot but still not enough to support some scenarios where we have collection of components (lists, tables, etc.), to do so we'll need indexed traversals that are to be added soon. Stay tuned!! :)

In the meantime, check this incomplete and somewhat weird app:

```purescript
-- App.purs: Traversals

import Tab as Tab

-- We don't expect to make this component a part of a bigger one but it's still
-- nice to have an "initial state" function in the form of `mempty`.
newtype State = State State'

derive instance newtypeAppState :: Newtype (State) _

instance semigroupAppState :: Semigroup (State)
  where
  append _ _ = unsafeThrow "`append` is not supported for the global App"

instance monoidAppState :: Monoid (State)
  where
  mempty =
    State
      -- For now let's initialize the state with five tab components.
      { tabs : [mempty, mempty, mempty, mempty, mempty]
      }

_tabs :: Lens' State' (Array Tab.State)
_tabs = lens _.tabs (_ { tabs = _ })

-- One of the coolest thing about traversals is that not only do we get a lens
-- to a list of components but also that we can use it to generalize all the
-- other types of lenses (iso, lens and prism).
-- We can now navigate through the state  of our application as if we were doing
-- OO Programming: We can read the same state that our child components are
-- reading, modify it and all that with very little code, pretty cool!
_tabTotal :: Traversal' State' Tab.State'
_tabTotal = _tabs .. traversed .. _new

-- Notice the difference in type between this traversal and the previous one.
-- The one above has access to any of the states from the hidden (backup) pages
-- of the tab components in our app, the one below has access to the selected
-- page of every tab component.
_tabDisplayTotal :: Traversal' AppState' DisplayState
_tabDisplayTotal = _tabs .. traversed .. _new .. _display

app :: forall fx . P.UIComponent fx State R.ReactElement
app =
  (P.focus' _new)
    do
    -- Another nice feature of traversals is that they're first-class citizens.
    -- We can compose them and store them as you would with functions!
    total1 <- P.focus (_tabDisplayTotal .. _Page1) P.get
    total2 <- P.focus (_tabDisplayTotal .. _Page2) P.get

    handler <- unwrap2 <$> P.eventHandler
    let total = unwrap $ total1 <> total2

    pure $ display handler total
  where
  display handler total =
    R.div'
      [ R.div' [R.text $ "Total # clicks: " <> show total]
      , R.button [R.onClick $ handler onClick] [R.text "Increment Total"]
      ]

  onClick =
    do
    _tabDisplayTotal .. _Page1 .. _additive %= (_ + 1)
    _tabDisplayTotal .. _Page2 .. _additive %= (_ + 1)

    _tabTotal .. _page1 .. _additive %= (_ + 1)
    _tabTotal .. _page2 .. _additive %= (_ + 1)
```

Right now, the app you'll see by running this code is a button that increases a number by five every time it's clicked... quite the app, I know, but there's more to it than meets the eye: We're using the power of traversals to expressively count the total number of clicks of the visible page from each of the five tab components and we're also increasing by one the click count of the hidden pages (that will eventually be swapped after we select them) every time we click one global button. Right now we don't have the ability to show the tabs in our app but once we have indexed traversals it'll be super easy to do. I wonder how many lines of functional & boilerplate code and how many physical files you would need to make this same app in other frameworks/architectures...
