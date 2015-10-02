-- | The application entry point
module Main where

import Prelude
import Jundo.Rendering
import Jundo.Simulation
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.ST
import Data.Date (Now(), nowEpochMilliseconds)
import Data.Time (Milliseconds(..))
import Data.Maybe
import Data.Nullable
import qualified DOM as D
import qualified DOM.Event.EventTarget as D
import qualified DOM.Event.Experimental as D
import qualified DOM.Event.MouseEvent as D
import qualified DOM.Event.Types (MouseEvent()) as D
import qualified DOM.HTML as D
import qualified DOM.HTML.Types as D
import qualified DOM.HTML.Window as D
import qualified DOM.Node.Document.Experimental as D
import qualified DOM.Node.Element as D
import qualified DOM.Node.Element.Experimental as D
import qualified DOM.Node.Types as D
import qualified DOM.RequestAnimationFrame as D
import Graphics.Canvas (Canvas(), getCanvasElementById)
import Graphics.Canvas.Element

-- | Handle canvas clicks. If we are not in full screen, open the canvas in full screen.
-- | Otherwise, toggle the direction of the cube's rotation.
canvasClick :: forall eff h. STRef h SimulationState -> D.Element -> D.MouseEvent -> Eff (dom :: D.DOM, st :: ST h | eff) Unit
canvasClick stateRef el _ = do
  maybeFullscreenEl <- D.window >>= D.document >>= pure <<< D.htmlDocumentToDocument >>= D.fullscreenElement >>= pure <<< toMaybe
  case maybeFullscreenEl of
    Nothing -> do  
      D.requestFullscreen el
      D.requestPointerLock el
    -- TODO: Check that the fullscreen element is our canvas
    Just fullscreenEl -> do
      modifySTRef stateRef toggleDirection
      return unit

-- | Main loop. Steps the simulation state forwards in time then renders the new state.
tick :: forall eff h. RenderingContext -> STRef h SimulationState -> Milliseconds -> Eff (canvas :: Canvas, dom :: D.DOM, now :: Now, st :: ST h | eff) Unit
tick ctx stateRef time = do
  newTime <- nowEpochMilliseconds
  state <- modifySTRef stateRef $ timestep (newTime - time)
  render ctx state
  D.requestAnimationFrame $ tick ctx stateRef newTime

-- | Start the application: set up the canvas and start the render loop
main :: Eff (canvas :: Canvas, dom :: D.DOM, err :: EXCEPTION, now :: Now) Unit
main = do
  Just canvas <- getCanvasElementById "easel"
  el <- pure $ toElement canvas
  renderingContext <- initialiseWebGL el canvas
  time <- nowEpochMilliseconds
  elEventTarget <- pure $ D.elementToEventTarget $ el

  -- Aaa mutable state!
  -- The event listener needs to update the state read by the render loop somehow, so let's use a mutable reference cell
  runST do
    stateRef <- newSTRef initialSimulationState
    D.addMouseEventListener D.click (D.mouseEventListener $ canvasClick stateRef el) false elEventTarget
    tick renderingContext stateRef time
