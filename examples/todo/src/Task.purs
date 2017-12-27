{-
  @license MIT
  Task.purs
-}

module Task
( State(..)
, _completed
, _description
, mempty'
, task
)
where

import Control.Monad.Reader (ask)
import Data.Array (singleton)
import Data.Lens (Lens', (.=), (^.), lens)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)
import Data.Profunctor (lmap)
import Data.Tuple (Tuple(..))
import React (ReactElement) as R
import React.DOM (a, input, td', text, tr') as R
import React.DOM.Props
  (_type, checked, className, onChange, onClick, title) as R
import Prelude
import Proact as P
import ProactPlus (ReactHandler, (..), withEvent)
import Unsafe.Coerce (unsafeCoerce)

-- | The state of a task component.
newtype State =
  State
    { completed :: Boolean
    , description :: String
    }

-- State :: Newtype
derive instance newtypeState :: Newtype (State) _

-- | Gets or sets whether the task completed or not.
_completed :: Lens' State Boolean
_completed = _Newtype .. lens _.completed (_ { completed = _ })

-- | Gets or sets the task description.
_description :: Lens' State String
_description = _Newtype .. lens _.description (_ { description = _ })

-- | The initial state of the component.
mempty' :: State
mempty' =
  State
    { completed : false
    , description : ""
    }

-- | The task component.
task
  :: forall fx index
   . ReactHandler fx index -> P.IndexedComponent fx index State R.ReactElement
task onDelete =
  do
  Tuple index state <- ask
  dispatcher <- withEvent <$> P.dispatcher

  pure $ view dispatcher index state
  where
  view dispatcher index state =
    (R.tr' .. map (R.td' .. singleton))
      [
        R.input
          [ R._type "checkbox"
          , R.className "checkbox"
          , R.checked $ state ^. _completed
          , R.title "Mark as completed"
          , R.onChange $ lmap fromInputEvent $ dispatcher onCompleted
          ]
          []
      , R.text $ state ^. _description
      ,
        R.a
          [ R.className "btn btn-danger pull-right"
          , R.title "Remove item"
          , R.onClick \_ -> onDelete index
          ]
          [ R.text "✖" ]
      ]

  fromInputEvent event = { checked : (unsafeCoerce event).target.checked }

  onCompleted event = _completed .= event.checked
