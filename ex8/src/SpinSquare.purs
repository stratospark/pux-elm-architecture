module App.SpinSquare where

import Prelude
import Control.Monad.Aff (Aff, makeAff)
import Control.Timer (TIMER)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple))
import Debug.Trace (spy)
import DOM (DOM)
import DOM.RequestAnimationFrame (requestAnimationFrame)
import Pux (noEffects, EffModel)
import Pux.Html (rect, g, svg, Html)
import Pux.Html.Attributes (transform, style, ry, rx, y, x, viewBox, height, width)
import Pux.Html.Events (onClick)
import Signal.Time (now, Time, second)

type AnimationState = Maybe { prevClockTime :: Time, elapsedTime :: Time}

tick :: forall eff. Aff (timer :: TIMER, dom :: DOM | eff) Time
tick = makeAff \reject resolve -> do
  time <- now
  requestAnimationFrame $ (resolve time)

type State =
  { angle :: Number
  , displayAngle :: Number
  , animationState :: AnimationState
}

init :: State
init =
  { angle: 0.0
  , displayAngle: 0.0
  , animationState: Nothing
  }

rotateStep :: Number
rotateStep = 45.0

data Action = Spin | Tick Time

update :: forall e. Action -> State -> EffModel State Action (dom :: DOM, timer :: TIMER | e)
update Spin state =
  case state.animationState of
    Nothing -> { state: state
               , effects: [ Tick <$> tick ]
               }
    Just {prevClockTime, elapsedTime} -> noEffects state

update (Tick clockTime) state =
  let
    newElapsedTime =
      case state.animationState of
        Nothing -> 0.0
        Just x -> x.elapsedTime + (clockTime - x.prevClockTime)
  in
    if newElapsedTime > second then
      noEffects $ state { angle = state.angle + rotateStep
                        , displayAngle = state.angle + rotateStep
                        , animationState = Nothing }
    else
      { state: state { angle = state.angle
                      , displayAngle = toOffset state.animationState state
                      , animationState = Just { elapsedTime: newElapsedTime
                                              , prevClockTime: clockTime }
                      }
      , effects: [ Tick <$> tick ]
      }

toOffset :: AnimationState -> State -> Number
toOffset animationState state =
  state.angle + case animationState of
    Nothing -> 0.0
    Just {elapsedTime} -> spy $ rotateStep * (elapsedTime / second)

view :: State -> Html Action
view state =
  svg
    [ width "200", height "200", viewBox "0 0 200 200" ]
    [ g
      [ transform $ "translate(100, 100) rotate(" ++ show state.displayAngle ++ ")"
      , onClick $ const Spin ]
      [ rect
        [ x "-50"
        , y "-50"
        , width "100"
        , height "100"
        , rx "15"
        , ry "15"
        , style [ Tuple "fill" "#60B5CC" ]
        ]
        []
      ]
    ]
