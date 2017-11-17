{-
  @license MIT
  Main.purs
-}

module Main
where

import Control.Monad.Eff (Eff)
import Data.Lens (Lens', lens)
import Data.Maybe (fromJust)
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
import React.DOM (button, div', text) as R
import React.DOM.Props (onClick) as R

type GlobalState =
  { counter :: Int
  }

_counter :: Lens' GlobalState Int
_counter = lens _.counter (_ { counter = _ })

global :: forall fx . P.UIComponent fx GlobalState R.ReactElement
global =
  do
  clickDisplay <- P.focus _counter clickCounter
  pure $ display clickDisplay
  where
  display clickDisplay = R.div' clickDisplay

type CounterState = Int

clickCounter :: forall fx . P.UIComponent fx CounterState (Array R.ReactElement)
clickCounter =
  do
  counter <- P.get
  handler <- P.eventHandler
  pure $ display counter handler
  where
  onClick = P.modify (_ + 1)

  display counter handler =
    [ R.div' [R.text $ "# clicks: " <> show counter]
    , R.button [R.onClick $ handler onClick] [R.text "Increment"]
    ]

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
    let spec = P.spec global splashScreen { counter : 0 }
    let element = flip R.createFactory {} $ R.createClass spec
    refDocument <- map D.htmlDocumentToParentNode $ D.window >>= D.document
    refApp <- fromJust <$> D.querySelector (D.QuerySelector "#app") refDocument
    void $ R.render element refApp
  where
  splashScreen = R.text ""
