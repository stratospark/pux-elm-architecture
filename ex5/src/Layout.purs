module App.Layout where

import App.RandomGif as RandomGif
import App.Routes (Route(Home, NotFound))
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude (map, ($), (#))
import Pux (mapEffects, mapState, noEffects, EffModel)
import Pux.Html (Html, div, text, h1)

data Action
  = Child (RandomGif.Action)
  | PageView Route

type ID = Int

type State =
  { route :: Route
  , randomGif :: RandomGif.State }

init :: State
init =
  { route: NotFound
  , randomGif: RandomGif.init }

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, dom :: DOM | e)
update (PageView route) state = noEffects $ state { route = route }
update (Child action) state = RandomGif.update action state.randomGif
  # mapState (state { randomGif = _ })
  # mapEffects Child

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Ex5: Random Gif Viewer" ]
    , case state.route of
        Home -> div
                  []
                  [ map Child $ RandomGif.view state.randomGif ]
        NotFound -> App.NotFound.view state
    ]
