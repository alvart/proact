{-
  @license MIT
  FilterMenu.purs
-}

module FilterMenu
( Filter(..)
, filterMenu
)
where

import Control.Monad.Reader (ask)
import Data.Lens ((.=))
import Prelude
import Proact as P
import ProactPlus (_this)
import React (ReactElement) as R
import React.DOM (button, div, text) as R
import React.DOM.Props (className, onClick) as R

-- | The three filters which can be applied to the list of tasks.
data Filter =
  Active
  | All
  | Completed

-- A type synonymous for a filter which is the state of this component.
type State = Filter

-- Filter :: Eq, Show
derive instance eqFilter :: Eq (Filter)

instance showFilter :: Show (Filter)
  where
  show All = "All"
  show Active = "Active"
  show Completed = "Completed"

-- | The top-bar to select filtering options for the tasks.
filterMenu :: forall fx . P.Component fx State R.ReactElement
filterMenu =
  do
  state <- ask
  dispatcher <- P.eventDispatcher

  pure $ view dispatcher state
  where
  view dispatcher state =
    R.div [ R.className "btn-group" ]
      $ map filterButton [ All, Active, Completed ]
    where
    filterButton filter =
      R.button
        [ R.className className, R.onClick $ dispatcher onFilterChanged ]
        [ R.text $ show filter ]
      where
      className =
        if filter == state
        then "btn toolbar active"
        else "btn toolbar"

      onFilterChanged _ = _this .= filter
