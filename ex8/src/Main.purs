module Main where

import App.Layout (Action(PageView), State, view, update)
import App.Routes (match)
import Control.Monad.Eff (Eff)
import Control.Timer (TIMER)
import DOM (DOM)
import Prelude (return, bind)
import Pux (App, CoreEffects, renderToDOM)
import Pux.Router (sampleUrl)
import Signal ((~>))

type AppEffects = (dom :: DOM, timer :: TIMER)

-- | App configuration
-- config :: forall e.
--           State ->
--           Eff (CoreEffects AppEffects)
--               (Config State Action (ajax :: AJAX | e))
config state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleUrl

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)

  return
    { initialState: state
    , update: update
    , view: view
    , inputs: [routeSignal] }

-- | Entry point for the browser.
main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  c <- config state
  app <- Pux.start c

  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  return app

-- | Entry point for the browser with pux-devtool injected.
debug :: State -> Eff (CoreEffects AppEffects) (App State (Pux.Devtool.Action Action))
debug state = do
  c <- config state
  app <- Pux.Devtool.start c

  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  return app
