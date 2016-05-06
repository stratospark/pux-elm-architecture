module App.Layout where

import App.SpinSquare as SpinSquare
import App.Routes (Route(Home, NotFound))
import Control.Monad.Aff.AVar (AVAR)
import Control.Timer (TIMER)
import Data.Int (toNumber)
import Math (max)
import Prelude ((#), (+), ($), map, (++), show)
import Pux (mapEffects, mapState, noEffects, EffModel)
import Pux.Html (img, Html, div, h1, p, text)
import Pux.Html.Attributes (width, src)
import Signal.Time (Time)

data Action
  = Child (SpinSquare.Action)
  | Child2 (SpinSquare.Action)
  | PageView Route

type State =
  { route :: Route
  , square :: SpinSquare.State
  , square2 :: SpinSquare.State
}

init :: State
init =
  { route: NotFound
  , square: SpinSquare.init
  , square2: SpinSquare.init
}

update :: forall e. Action -> State -> EffModel State Action (timer :: TIMER, avar :: AVAR | e)
update (PageView route) state = noEffects $ state { route = route }
update (Child action) state =
  SpinSquare.update action state.square
  # mapState (state { square = _})
  # mapEffects Child
update (Child2 action) state =
  SpinSquare.update action state.square2
  # mapState (state { square2 = _})
  # mapEffects Child2

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Spinning Squares" ]
    , map Child $ SpinSquare.view state.square
    , map Child2 $ SpinSquare.view state.square2
    ]
