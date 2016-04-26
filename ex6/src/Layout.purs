module App.Layout where

import App.RandomGif as RandomGif
import App.Routes (Route(Home, NotFound))
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude (map, ($), (#))
import Pux (mapEffects, mapState, noEffects, EffModel)
import Pux.CSS (flex, display, style)
import Pux.Html (Html, div, text, h1)

data Action
  = LeftGif (RandomGif.Action)
  | RightGif (RandomGif.Action)
  | PageView Route

type ID = Int

type State =
  { route :: Route
  , left :: RandomGif.State
  , right :: RandomGif.State }

init :: State
init =
  { route: NotFound
  , left: RandomGif.init "dogs"
  , right: RandomGif.init "cats" }

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, dom :: DOM | e)
update (PageView route) state = noEffects $ state { route = route }
update (LeftGif action) state = RandomGif.update action state.left
  # mapState (state { left = _ })
  # mapEffects LeftGif
update (RightGif action) state = RandomGif.update action state.right
  # mapState (state { right = _ })
  # mapEffects RightGif

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Ex6: Pair of Random GIF Viewers" ]
    , case state.route of
        Home -> div
                    [ style $ do
                        display flex ]
                  [ map LeftGif $ RandomGif.view state.left
                  , map RightGif $ RandomGif.view state.right ]
        NotFound -> App.NotFound.view state
    ]
