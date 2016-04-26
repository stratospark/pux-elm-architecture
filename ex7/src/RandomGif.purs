module App.RandomGif where

import Data.Maybe
import Control.Monad.Aff (Aff, attempt)
import Data.Argonaut (decodeJson, class DecodeJson)
import Data.Argonaut.Combinators ((.?))
import Data.Either (Either(Right, Left), either)
import Network.HTTP.Affjax (get, AJAX)
import Prelude (const, ($), (++), return, show, (<<<), bind, pure, (#))
import Pux (noEffects, EffModel)
import Pux.CSS (em, padding, style)
import Pux.Html (br, Html, text, button, img, p, div)
import Pux.Html.Attributes (src)
import Pux.Html.Events (onClick)

data Action = RequestMore | NewGif (Either String Image)

type State =
  { topic :: String
  , gifUrl :: String
  , status :: String }

init :: String -> State
init topic =
  { topic: topic
  , gifUrl: "/waiting.gif"
  , status: "Please request an image." }

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX | e)

update RequestMore state =
  { state: state { status = "Requesting image..." }
  , effects: [getRandomGif state.topic]
  }

update (NewGif (Left err)) state =
  noEffects $ state { status = "Error fetching: " ++ err }

update (NewGif (Right (Image img))) state =
  noEffects $ state { status = "Image loaded.", gifUrl = img.url }

newtype Image = Image { url :: String }

instance decodeJsonImage :: DecodeJson Image where
  decodeJson json = do
    obj <- decodeJson json
    dat <- obj .? "data"
    url <- dat .? "image_url"
    pure $ Image { url: url }

getRandomGif :: forall e. String -> Aff (ajax :: AJAX | e) Action
getRandomGif topic = do
  res <- attempt $ get $ randomUrl topic
  let decode r = decodeJson r.response :: Either String Image
  let image = either (Left <<< show) decode res
  return $ NewGif image

randomUrl :: String -> String
randomUrl topic = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic

view :: State -> Html Action
view state =
  div
    [ style $ do
        padding (1.0 # em) (1.0 # em) (1.0 # em) (1.0 # em) ]
    [ p [] [ text $ "Topic: " ++ state.topic ]
    , p [] [ text $ "Status: " ++ state.status ]
    , img [ src state.gifUrl ] []
    , br [] []
    , button [ onClick $ const RequestMore ] [ text "More Please!" ] ]
