module App.Layout where

import App.Counter as Counter
import App.Routes (Route(Home, NotFound))
import Prelude (const, ($), map)
import Pux.Html (button, Html, div, h1, p, text)
import Pux.Html.Events (onClick)

data Action
  = Top (Counter.Action)
  | Bottom (Counter.Action)
  | Reset
  | PageView Route

type State =
  { route :: Route
  , topCount :: Counter.State
  , bottomCount :: Counter.State }

init :: State
init =
  { route: NotFound
  , topCount: Counter.init
  , bottomCount: Counter.init }

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (Top action) state = state { topCount = Counter.update action state.topCount }
update (Bottom action) state = state { bottomCount = Counter.update action state.bottomCount }
update Reset state = state { bottomCount = 0, topCount = 0 }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Ex2: Pair of Counters" ]
    , case state.route of
        Home -> div
                  []
                  [ map Top $ Counter.view state.topCount
                  , map Bottom $ Counter.view state.bottomCount
                  , button [ onClick $ const Reset ] [ text "RESET" ] ]
        NotFound -> App.NotFound.view state
    ]
