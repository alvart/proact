{-
  @license MIT
  Main.purs
-}

module Main
where

import Control.Monad.Eff (Eff)
import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import DOM (DOM)
import DOM.HTML (window) as D
import DOM.HTML.Window (document) as D
import DOM.HTML.Types (htmlDocumentToParentNode) as D
import DOM.Node.ParentNode (QuerySelector(..), querySelector) as D
import Partial.Unsafe (unsafePartial)
import Prelude
import Proact as P
import React
  ( ReactProps
  , ReactRefs
  , ReactState
  , ReadOnly
  , ReadWrite
  , createClass
  , createFactory
  )
  as R
import ReactDOM (render) as R
import Todo (todo)

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
    let spec = P.spec todo mempty
    let element = flip R.createFactory {} $ R.createClass spec
    rDocument <- map D.htmlDocumentToParentNode $ D.window >>= D.document
    rApp <- fromJust <$> D.querySelector (D.QuerySelector "#app") rDocument
    void $ R.render element rApp
