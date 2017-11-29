{-
  @license MIT
  Main.purs
-}

module Main
where

import Control.Alt((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Array (singleton)
import Data.Lens
  ( (%~)
  , (.~)
  , (^.)
  , (^?)
  , Iso'
  , Lens'
  , Prism'
  , Traversal'
  , iso
  , lens
  , prism
  , traversed
  )
import Data.Either (Either(..))
import Data.Maybe (fromJust, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import DOM (DOM)
import DOM.HTML (window) as D
import DOM.HTML.Window (document) as D
import DOM.HTML.Types (htmlDocumentToParentNode) as D
import DOM.Node.ParentNode (QuerySelector(..), querySelector) as D
import Partial.Unsafe (unsafePartial)
import Prelude
import Proact as P
import React
  ( ReactElement
  , ReactProps
  , ReactRefs
  , ReactState
  , ReadOnly
  , ReadWrite
  , createClass
  , createFactory
  )
  as R
import ReactDOM (render) as R
import React.DOM (a, button, div', text) as R
import React.DOM.Props (onClick) as R

infixr 9 o as ..

o :: forall a b c d . Semigroupoid a => a c d -> a b c -> a b d
o = (<<<)

_additive :: forall a . Iso' (Additive a) a
_additive = iso unwrap Additive

_new :: forall s a . Newtype s a => Iso' s a
_new = iso unwrap wrap

unwrap2 :: forall a b c d . Newtype c d => (a -> b -> c) -> (a -> b -> d)
unwrap2 fabc a b = unwrap $ fabc a b

type CounterState = Additive Int

counter :: forall fx . P.UIComponent fx CounterState R.ReactElement
counter =
  (P.swap _additive)
    do
    clicks <- P.get
    handler <- unwrap2 <$> P.eventHandler
    pure $ display clicks handler
  where
  display clicks handler =
    R.div'
      [ R.div' [R.text $ "# clicks: " <> show clicks]
      , R.button [R.onClick $ handler onClick] [R.text "Increment"]
      ]

  onClick = P.modify (_ + 1)

data DisplayState
  = Page1 CounterState
  | Page2 CounterState

instance semigroupDisplayState :: Semigroup (DisplayState)
  where
  append (Page1 state1) (Page1 state2) = Page1 $ state1 <> state2
  append (Page2 state1) (Page2 state2) = Page2 $ state1 <> state2
  append page _ = page

type TabState' =
  { display :: DisplayState
  , page1 :: CounterState
  , page2 :: CounterState
  }

newtype TabState = TabState TabState'

derive instance newtypeTabState :: Newtype (TabState) _

instance semigroupTabState :: Semigroup (TabState)
  where
  append (TabState state1) (TabState state2) =
    TabState
      { display : state1.display <> state2.display
      , page1 : state1.page1 <> state2.page1
      , page2 : state1.page2 <> state2.page2
      }

instance monoidTabState :: Monoid (TabState)
  where
  mempty =
    TabState
      { display : Page1 mempty
      , page1 : mempty
      , page2 : mempty
      }

_Page1 :: Prism' DisplayState CounterState
_Page1 =
  prism Page1
    \page ->
      case page
        of
        Page1 page1 -> Right page1
        _ -> Left page

_Page2 :: Prism' DisplayState CounterState
_Page2 =
  prism Page2
    \page ->
      case page
        of
        Page2 page2 -> Right page2
        _ -> Left page

_display :: Lens' TabState' DisplayState
_display = lens _.display (_ { display = _ })

_page1 :: Lens' TabState' CounterState
_page1 = lens _.page1 (_ { page1 = _ })

_page2 :: Lens' TabState' CounterState
_page2 = lens _.page2 (_ { page2 = _ })

tab :: forall fx . P.UIComponent fx TabState R.ReactElement
tab =
  (P.swap _new)
    do
    handler <- unwrap2 <$> P.eventHandler

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

type AppState' =
  { tabs :: Array TabState
  }

_tabs :: Lens' AppState' (Array TabState)
_tabs = lens _.tabs (_ { tabs = _ })

_tabDisplayTotal :: Traversal' AppState' DisplayState
_tabDisplayTotal = _tabs .. traversed .. _new .. _display

_tabTotal :: Traversal' AppState' TabState'
_tabTotal = _tabs .. traversed .. _new

newtype AppState = AppState AppState'

derive instance newtypeAppState :: Newtype (AppState) _

instance semigroupAppState :: Semigroup (AppState)
  where
  append _ _ = unsafeThrow "`append` is not supported for the global App"

instance monoidAppState :: Monoid (AppState)
  where
  mempty =
    AppState
      { tabs : [mempty, mempty, mempty, mempty, mempty]
      }

app :: forall fx . P.UIComponent fx AppState R.ReactElement
app =
  (P.swap _new)
    do
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
    P.modify $ _tabDisplayTotal .. _Page1 .. _additive %~ (_ + 1)
    P.modify $ _tabDisplayTotal .. _Page2 .. _additive %~ (_ + 1)

    P.modify $ _tabTotal .. _page1 .. _additive %~ (_ + 1)
    P.modify $ _tabTotal .. _page2 .. _additive %~ (_ + 1)

type ReactFx =
  ( dom :: DOM
  , props :: R.ReactProps
  , refs :: R.ReactRefs R.ReadOnly
  , state :: R.ReactState R.ReadWrite
  )

main :: Eff ReactFx Unit
main =
  unsafePartial
    do
    let spec = P.spec app splashScreen mempty
    let element = flip R.createFactory {} $ R.createClass spec
    refDocument <- map D.htmlDocumentToParentNode $ D.window >>= D.document
    refApp <- fromJust <$> D.querySelector (D.QuerySelector "#app") refDocument
    void $ R.render element refApp
  where
  splashScreen = R.text ""
