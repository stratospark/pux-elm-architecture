module App.Layout where

import App.Counter as Counter
import App.Routes (Route(Home, NotFound))
import Data.Array (modifyAt, drop)
import Data.Maybe (fromMaybe)
import Prelude (const, ($), map, (++), (+), (==))
import Pux.Html (button, Html, div, h1, p, text)
import Pux.Html.Events (onClick)

data Action
  = Insert
  | Remove
  | Modify ID Counter.Action
  | PageView Route

type ID = Int

type LocalCounter = { id :: ID, state :: Counter.State }

type State =
  { route :: Route
  , counters :: Array LocalCounter
  , nextID :: ID }

init :: State
init =
  { route: NotFound
  , counters: []
  , nextID: 0 }

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update Insert state =
  let newCounter = { id: state.nextID, state: Counter.init }
      newCounters = state.counters ++ [newCounter]
  in
    state { counters = newCounters, nextID = state.nextID + 1 }
update Remove state =
  state { counters = drop 1 state.counters }
update (Modify id action) state =
  let
    updateCounter c = if c.id == id then c { state = Counter.update action c.state } else c
  in
    state { counters = map updateCounter state.counters }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Pux Starter App" ]
    , p [] [ text "Change src/Layout.purs and watch me hot-reload." ]
    , case state.route of
        Home -> div
                  []
                  ([ button [ onClick $ const Remove ] [ text "Remove" ]
                  , button [ onClick $ const Insert ] [ text "Add" ]
                  ] ++ (map viewCounter state.counters))
        NotFound -> App.NotFound.view state
    ]

viewCounter :: LocalCounter -> Html Action
viewCounter c = map (Modify c.id) $ Counter.view c.state
