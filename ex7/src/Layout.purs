module App.Layout where

import App.RandomGif as RandomGif
import App.Routes (Route(Home, NotFound))
import DOM (DOM)
import Data.Array (concat, snoc)
import Data.Tuple (Tuple(Tuple))
import Network.HTTP.Affjax (AJAX)
import Prelude (map, ($), const, (#), (==), (+))
import Pux (mapState, mapEffects, EffModel, noEffects)
import Pux.Html (br, input, form, text, h1, div, Html)
import Pux.Html.Attributes (type_, value, placeholder, name, style)
import Pux.Html.Events (FormEvent, onChange, onSubmit)

data Action
  = Topic String
  | TopicChange FormEvent
  | Create
  | Modify ID RandomGif.Action
  | PageView Route

type ID = Int

type Gif = { id :: ID, state :: RandomGif.State }

type State =
  { route :: Route
  , topic :: String
  , gifList :: Array Gif
  , nextID :: ID }

init :: State
init =
  { route: NotFound
  , topic: ""
  , gifList: []
  , nextID: 0 }

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, dom :: DOM | e)
update (PageView route) state = noEffects $ state { route = route }
update (Topic topic) state = noEffects $ state { topic = topic }
update (TopicChange ev) state = noEffects $ state { topic = ev.target.value }
update Create state = noEffects $
  state { gifList = snoc state.gifList { id: state.nextID, state: RandomGif.init state.topic }
        , nextID = state.nextID + 1 }
update (Modify id action) state =
  let
    subUpdate entry =
      (if entry.id == id
        then RandomGif.update action entry.state
        else (noEffects entry.state))
        # mapState (entry { state = _ })
        # mapEffects (Modify entry.id)

    updated = map subUpdate state.gifList
  in { state: state { gifList = map _.state updated }
     , effects: concat $ map _.effects updated }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Ex7: List of Random GIF Viewers" ]
    , case state.route of
        Home -> div
                  []
                  [
                    viewForm state
                  , br [] []
                  , div
                    [ style
                      [
                        Tuple "display" "flex"
                      , Tuple "flexDirection" "row"
                      , Tuple "flexWrap" "wrap"
                      , Tuple "justifyContent" "center"
                      , Tuple "alignItems" "center"
                      ]
                    ]
                    (map viewGif state.gifList)
                  ]
        NotFound -> App.NotFound.view state
    ]

viewForm :: State -> Html Action
viewForm state = form
  [ name "topic"
  , onSubmit $ const Create
  ]
  [ input
    [ type_ "text"
    , placeholder "What kind of gifs do you want?"
    , value state.topic
    , onChange TopicChange
    ]
    []
  ]

viewGif :: Gif -> Html Action
viewGif g = map (Modify g.id) $ RandomGif.view g.state
