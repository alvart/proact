{-
  @license MIT
  Todo.purs
-}

module Todo
( State(..)
, _filter
, _taskDescription
, _tasks
, mempty'
, todo
)
where

import Control.Monad.Reader (ask)
import Data.Array ((:), deleteAt, filter, length, singleton, snoc)
import Data.Lens (Lens', (%=), (.=), (.~), (^.), filtered, lens)
import Data.Lens.Indexed (itraversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype)
import Data.Profunctor (lmap)
import FilterMenu (Filter(..), filterMenu) as Filter
import Partial.Unsafe (unsafePartial)
import Prelude
import Proact as P
import ProactPlus ((..), use', withEvent)
import React (ReactElement) as R
import React.DOM
  ( br'
  , div
  , h1'
  , input
  , p'
  , table
  , tbody'
  , td'
  , text
  , th
  , thead'
  , tr'
  ) as R
import React.DOM.Props (className, onChange, onKeyUp, placeholder, value) as R
import Task as Task
import Unsafe.Coerce (unsafeCoerce)

-- | The state of the to-do application.
newtype State =
  State
    { filter :: Filter.Filter
    , taskDescription :: String
    , tasks :: Array Task.State
    }

-- State :: Newtype
derive instance newtypeState :: Newtype (State) _

-- | Gets or sets the task filter.
_filter :: Lens' State Filter.Filter
_filter = _Newtype .. lens _.filter (_ { filter = _ })

-- | Gets or sets the description of the new task to be added.
_taskDescription :: Lens' State String
_taskDescription =
  _Newtype .. lens _.taskDescription (_ { taskDescription = _ })

-- | Gets or sets the list of tasks.
_tasks :: Lens' State (Array Task.State)
_tasks = _Newtype .. lens _.tasks (_ { tasks = _ })

-- | The initial state of the component.
mempty' :: State
mempty' =
  State
    { filter : Filter.All
    , taskDescription : ""
    , tasks : [ ]
    }

-- A task to which a filter has been applied.
taskBox :: forall fx . P.Component fx State R.ReactElement
taskBox =
  do
  state <- ask
  dispatcher <- withEvent <$> P.dispatcher

  pure $ view dispatcher state
  where
  view dispatcher state =
    R.input
      [ R.className "form-control"
      , R.placeholder "Create a new task"
      , R.value $ state ^. _taskDescription
      ,
        R.onKeyUp
          $ unsafeCoerce
          $ lmap fromInputEvent
          $ dispatcher onNewTaskEnter
      ,
        R.onChange
          $ unsafeCoerce
          $ lmap fromInputEvent
          $ dispatcher onTextChanged
      ]
      []
    where
    fromInputEvent event =
      { keyCode : (unsafeCoerce event).keyCode
      , text : (unsafeCoerce event).target.value
      }

    newTask text = Task._description .~ text

    onNewTaskEnter event =
      if event.keyCode == 13 && event.text /= ""
      then _tasks %= flip snoc (newTask event.text Task.mempty')
      else if event.keyCode == 27
      then _taskDescription .= ""
      else pure unit

    onTextChanged event = _taskDescription .= event.text

-- The table showing the filtered list of tasks.
taskTable :: forall fx . P.Component fx State R.ReactElement
taskTable =
  do
  filter' <- use' _filter
  dispatcher <- withEvent <$> P.dispatcher

  taskBoxView <- taskBox
  tasksView <-
    P.focus' _tasks
      $ P.iFocus (itraversed .. filtered (taskFilter filter'))
      $ map singleton
      $ Task.task
      $ dispatcher onDelete

  pure $ view taskBoxView tasksView
  where
  view taskBoxView tasksView =
    R.table
      [R.className "table table-striped"]
      [
        R.thead'
          [
            R.tr'
              [ R.th [R.className "col-md-1"] []
              , R.th [R.className "col-md-10"] [R.text "Description"]
              , R.th [R.className "col-md-1"] []
              ]
          ]
      , R.tbody' $ R.tr' [R.td' [], R.td' [taskBoxView], R.td' []] : tasksView
      ]

  taskFilter Filter.All _ = true
  taskFilter Filter.Completed task = task ^. Task._completed
  taskFilter Filter.Active task = not $ task ^. Task._completed

  onDelete index = unsafePartial $ _tasks %= fromJust .. deleteAt index

-- | The to-do application.
todo :: forall fx . P.Component fx State R.ReactElement
todo =
  do
  state <- ask
  filterMenuView <- P.focus' _filter Filter.filterMenu
  taskTableView <- taskTable

  pure $ view state filterMenuView taskTableView
  where
  view state filterMenuView taskTableView =
    R.div
      [ R.className "container" ]
      [ R.h1' [ R.text "To-do App" ]
      , filterMenuView
      , R.br' [ ]
      , R.br' [ ]
      , taskTableView
      , R.p' [ R.text $ totalCompleted <> "/" <> total <> " tasks completed." ]
      ]
    where
    tasks = state ^. _tasks

    total = show $ length tasks

    totalCompleted = show $ length $ filter (_ ^. Task._completed) tasks
