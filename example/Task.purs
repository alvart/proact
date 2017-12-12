{-
  @license MIT
  Task.purs
-}

module Task
( State
, _completed
, _description
, _index
, task
)
where

import Control.Monad.Reader (ask)
import Data.Array (singleton)
import Data.Lens (Lens', (.=), (^.), lens)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Data.Profunctor (lmap)
import React (ReactElement) as R
import React.DOM (a, input, td', text, tr') as R
import React.DOM.Props
  (_type, checked, className, onChange, onClick, title) as R
import Prelude
import Proact as P
import Unsafe.Coerce (unsafeCoerce)
import Utility (ReactHandler, (..), unwrap2, unwrap3)

-- | The state of a task component.
newtype State =
  State
    { completed :: Boolean
    , description :: String
    , index :: Int
    }

-- State :: NewType, SemiGroup, Monoid
derive instance newtypeState :: Newtype (State) _

instance semigroupState :: Semigroup (State)
  where
  append s1 s2 =
    if s1 ^. _index < 0
    then s2
    else s1

instance monoidState :: Monoid (State)
  where
  mempty =
    State
      { completed : false
      , description : ""
      , index : -1
      }

-- | Gets or sets whether the task completed or not.
_completed :: Lens' State Boolean
_completed = _Newtype .. lens _.completed (_ { completed = _ })

-- | Gets or sets the task description.
_description :: Lens' State String
_description = _Newtype .. lens _.description (_ { description = _ })

-- | Gets or sets the index within the task list.
_index :: Lens' State Int
_index = _Newtype .. lens _.index (_ { index = _ })

-- | The task component.
task
  :: forall fx . ReactHandler fx Int -> P.Component fx State R.ReactElement
task onDelete' =
  do
  state <- ask
  inputDispatcher <- unwrap3 <$> P.eventDispatcher
  pure $ view inputDispatcher state
  where
  view inputDispatcher state =
    (R.tr' .. map (R.td' .. singleton))
      [
        R.input
          [ R._type "checkbox"
          , R.className "checkbox"
          , R.checked $ state ^. _completed
          , R.title "Mark as completed"
          , R.onChange $ lmap fromInputEvent $ inputDispatcher onCompleted
          ]
          []
      , R.text $ state ^. _description
      ,
        R.a
          [ R.className "btn btn-danger pull-right"
          , R.title "Remove item"
          , R.onClick \_ -> onDelete $ state ^. _index
          ]
          [R.text "✖"]
      ]

  fromInputEvent event = { checked : (unsafeCoerce event).target.checked }

  onCompleted event = _completed .= event.checked

  onDelete = unwrap2 onDelete'
