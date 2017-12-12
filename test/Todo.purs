{-
  @license MIT
  Todo.purs
-}

module Todo
( Filter
, State
, _filter
, _taskDescription
, _tasks
, todo
)
where

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Reader (ask)
import Data.Array (deleteAt, filter, length, singleton, snoc)
import Data.Identity (Identity)
import Data.Lens (Lens', (%=), (.=), (.~), (^.), lens, traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (lmap)
import Data.TraversableWithIndex (traverseWithIndex)
import Prelude
import Proact as P
import React (ReactElement) as R
import React.DOM
  ( br'
  , button
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
import React.DOM.Props
  (className, onChange, onClick, onKeyUp, placeholder, value) as R
import Task as Task
import Unsafe.Coerce (unsafeCoerce)
import Utility ((..), unwrap3, use')

-- | The three filters which can be applied to the list of tasks.
data Filter =
  Active
  | All
  | Completed

-- | The state of the to-do application.
newtype State =
  State
    { filter :: Filter
    , taskDescription :: String
    , tasks :: Array Task.State
    }

-- State :: NewType, SemiGroup, Monoid
derive instance newtypeState :: Newtype (State) _

instance semigroupState :: Semigroup (State)
  where
  append _ _ = unsafeThrow "To-do app doesn't support `append`"

instance monoidState :: Monoid (State)
  where
  mempty =
    State
      { filter : All
      , taskDescription : ""
      , tasks : []
      }

-- Filter :: Eq, Show
derive instance eqFilter :: Eq (Filter)

instance showFilter :: Show (Filter)
  where
  show All = "All"
  show Active = "Active"
  show Completed = "Completed"

-- | Gets or sets the task filter.
_filter :: Lens' State Filter
_filter = _Newtype .. lens _.filter (_ { filter = _ })

-- | Gets or sets the description of the new task to be added.
_taskDescription :: Lens' State String
_taskDescription =
  _Newtype .. lens _.taskDescription (_ { taskDescription = _ })

-- | Gets or sets the list of tasks.
_tasks :: Lens' State (Array Task.State)
_tasks = _Newtype .. lens _.tasks (_ { tasks = _ })

-- The top-bar to select filtering options for the tasks.
filterBar :: forall fx . P.Component fx State R.ReactElement
filterBar =
  do
  state <- ask
  clickDispatcher <- unwrap3 <$> P.eventDispatcher

  pure $ view clickDispatcher state
  where
  view clickDispatcher state =
    R.div [R.className "btn-group"] $ map filterButton [All, Active, Completed]
    where
    filterButton filter' =
      R.button
        [ R.className className
        , R.onClick $ clickDispatcher onFilterChanged
        ]
        [R.text $ show filter']
      where
      className =
        if filter' == state ^. _filter
        then "btn toolbar active"
        else "btn toolbar"

      onFilterChanged =
        do
        _filter .= filter'

-- A task to which a filter has been applied.
filteredTask
  :: forall fx
   . Filter
  -> P.EventHandle fx Int
  -> P.Component fx Task.State (Array R.ReactElement)
filteredTask All onDelete = singleton <$> Task.task onDelete
filteredTask Completed onDelete =
  do
  completed <- use' Task._completed
  if completed
    then singleton <$> Task.task onDelete
    else pure []
filteredTask Active onDelete =
  do
  completed <- use' Task._completed
  if completed
    then pure []
    else singleton <$> Task.task onDelete

-- A task to which a filter has been applied.
taskBox :: forall fx . P.Component fx State R.ReactElement
taskBox =
  do
  state <- ask
  inputDispatcher <- unwrap3 <$> P.eventDispatcher

  pure $ view inputDispatcher state
  where
  view inputDispatcher state =
    R.input
      [ R.className "form-control"
      , R.placeholder "Create a new task"
      , R.value $ state ^. _taskDescription
      ,
        R.onKeyUp
          $ unsafeCoerce
          $ lmap fromInputEvent
          $ inputDispatcher onNewTaskEnter
      ,
        R.onChange
          $ unsafeCoerce
          $ lmap fromInputEvent
          $ inputDispatcher onTextChanged
      ]
      []
    where
    fromInputEvent event =
      { keyCode : (unsafeCoerce event).keyCode
      , text : (unsafeCoerce event).target.value
      }

    newTask index text = (Task._index .~ index) .. (Task._description .~ text)

    onNewTaskEnter =
      do
      event <- ask
      if event.keyCode == 13 && event.text /= ""
        then
          do
          let index = length $ state ^. _tasks
          _tasks %= flip snoc (newTask index event.text mempty)
        else if event.keyCode == 27
        then _taskDescription .= ""
        else pure unit

    onTextChanged =
      do
      event <- ask
      _taskDescription .= event.text

-- The table showing the filtered list of tasks.
taskTable :: forall fx . P.Component fx State R.ReactElement
taskTable =
  do
  filter' <- use' _filter
  deleteDispatcher <- P.eventDispatcher

  taskBoxView <- taskBox
  tasksView <-
    P.focus (_tasks .. traversed)
      $ filteredTask filter'
      $ deleteDispatcher onDelete

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
      ,
        R.tbody'
          $ [R.tr' [R.td' [], R.td' [taskBoxView], R.td' []]] <> tasksView
      ]

  onDelete =
    do
    index <- ask
    _tasks %= deleteTaskAt index

  deleteTaskAt index array =
    case deleteAt index array
      of
      Just array' -> unwrap $ traverseWithIndex resetIndex array'
      Nothing -> array

  resetIndex :: Int -> Task.State -> Identity Task.State
  resetIndex index task = pure $ task # Task._index .~ index

-- | The to-do application.
todo :: forall fx . P.Component fx State R.ReactElement
todo =
  do
  state <- ask
  filterBarView <- filterBar
  taskTableView <- taskTable

  pure $ view state filterBarView taskTableView
  where
  view state filterBarView taskTableView =
    R.div
      [R.className "container"]
      [ R.h1' [R.text "To-do App"]
      , filterBarView
      , R.br' []
      , R.br' []
      , taskTableView
      , R.p' [R.text $ totalCompleted <> "/" <> total <> " tasks completed."]
      ]
    where
    tasks = state ^. _tasks

    total = show $ length tasks

    totalCompleted = show $ length $ filter (_ ^. Task._completed) tasks
