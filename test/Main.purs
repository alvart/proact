{-
  @license MIT
  Main.purs
-}

module Main
where

import Control.Alt((<|>))
import Control.Monad.Eff (Eff)
import Data.Array (singleton)
import Data.Lens ((.~), (^.), (^?), Iso', Lens', Prism', iso, lens, prism)
import Data.Either (Either(..))
import Data.Maybe (fromJust, maybe)
import Data.Monoid (class Monoid, mempty)
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

o :: forall a b c d. Semigroupoid a => a c d -> a b c -> a b d
o = (<<<)

newIso :: forall s a . Newtype s a => Iso' s a
newIso = iso unwrap wrap

type CounterState' = Int

newtype CounterState = CounterState CounterState'

derive instance newtypeComponentT :: Newtype (CounterState) _

instance semigroupCounterState :: Semigroup (CounterState)
  where
  append _ _ = mempty

instance monoidCounterState :: Monoid (CounterState)
  where
  mempty = CounterState 0

counter :: forall fx . P.UIComponent fx CounterState R.ReactElement
counter =
  (P.mirror newIso)
    do
    clicks <- P.get
    handler <- P.eventHandler
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

type GlobalState =
  { display :: DisplayState
  , page1 :: CounterState
  , page2 :: CounterState
  }

_display :: Lens' GlobalState DisplayState
_display = lens _.display (_ { display = _ })

_page1 :: Lens' GlobalState CounterState
_page1 = lens _.page1 (_ { page1 = _ })

_page2 :: Lens' GlobalState CounterState
_page2 = lens _.page2 (_ { page2 = _ })

global :: forall fx . P.UIComponent fx GlobalState R.ReactElement
global =
  do
  handler <- P.eventHandler
  let counter_ = map singleton counter
  page1 <- P.focus _display $ P.choose _Page1 counter_
  page2 <- P.focus _display $ P.choose _Page2 counter_
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

main
  ::
    Eff
      ( dom :: DOM
      , props :: R.ReactProps
      , refs :: R.ReactRefs R.ReadOnly
      , state :: R.ReactState R.ReadWrite
      )
      Unit
main =
  unsafePartial
    do
    let spec = P.spec global splashScreen iState
    let element = flip R.createFactory {} $ R.createClass spec
    refDocument <- map D.htmlDocumentToParentNode $ D.window >>= D.document
    refApp <- fromJust <$> D.querySelector (D.QuerySelector "#app") refDocument
    void $ R.render element refApp
  where
  iState =
    { display : Page1 mempty
    , page1 : mempty
    , page2 : mempty
    }

  splashScreen = R.text ""
