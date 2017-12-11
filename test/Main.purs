{-
  @license MIT
  Main.purs
-}

module Main
where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Reader (ask, asks)
import Data.Array (singleton, snoc)
import Data.Foldable (intercalate)
import Data.Lens ((%=), (+=), (^.), Iso', Lens', iso, lens, traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (fromJust)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, unwrap)
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
import React.DOM (br', button, div', text) as R
import React.DOM.Props (onClick) as R

infixr 9 o as ..

o :: forall a b c d . Semigroupoid a => a c d -> a b c -> a b d
o = (<<<)

_additive :: forall a . Iso' (Additive a) a
_additive = iso unwrap Additive

_this :: forall a . Iso' a a
_this = id

unwrap2 :: forall a b c d . Newtype c d => (a -> b -> c) -> (a -> b -> d)
unwrap2 fabc a b = unwrap $ fabc a b

type CounterState = Additive Int

counter :: forall fx . P.Component fx CounterState R.ReactElement
counter =
  (P.focus' _additive)
    do
    clicks <- ask
    dispatcher <- unwrap2 <$> P.eventDispatcher
    pure $ display clicks dispatcher
  where
  display clicks dispatcher =
    R.div'
      [ R.div' [R.text $ "#clicks: " <> show clicks]
      , R.button [R.onClick $ dispatcher onClick] [R.text "Increment"]
      ]

  onClick = _this %= (_ + 1)

type AppState' =
  { counters :: Array CounterState
  }

_counters :: Lens' AppState' (Array CounterState)
_counters = lens _.counters (_ { counters = _ })

newtype AppState = AppState AppState'

derive instance newtypeAppState :: Newtype (AppState) _

instance semigroupAppState :: Semigroup (AppState)
  where
  append _ _ = unsafeThrow "`append` is not supported for the global App"

instance monoidAppState :: Monoid (AppState)
  where
  mempty =
    AppState
      { counters : [mempty, mempty, mempty]
      }

app :: forall fx . P.Component fx AppState R.ReactElement
app =
  (P.focus' _Newtype)
    do
    dispatcher <- unwrap2 <$> P.eventDispatcher

    let counter' = map singleton counter
    counterDisplays <- P.focus (_counters .. traversed) counter'

    total <- asks $ unwrap <<< (_ ^. _counters .. traversed)

    pure $ display dispatcher counterDisplays total
  where
  display dispatcher counterDisplays total =
    let counterDisplays' = map singleton counterDisplays
    in
    R.div' $ intercalate [newLine] counterDisplays' <> footer dispatcher total

  footer dispatcher total =
    [ newLine
    , R.div' [R.text $ "Total # clicks: " <> show total]
    , R.button [R.onClick $ dispatcher increment] [R.text "Increment Total"]
    , newLine
    , newLine
    , R.button [R.onClick $ dispatcher addCounter] [R.text "Add Counter"]
    ]

  newLine = R.br' []

  addCounter = _counters %= flip snoc mempty

  increment = _counters .. traversed .. _Newtype += 1

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
    let spec = P.spec app mempty
    let element = flip R.createFactory {} $ R.createClass spec
    rDocument <- map D.htmlDocumentToParentNode $ D.window >>= D.document
    rApp <- fromJust <$> D.querySelector (D.QuerySelector "#app") rDocument
    void $ R.render element rApp
  where
  splashScreen = R.text ""
