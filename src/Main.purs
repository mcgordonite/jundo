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
import qualified DOM.Event.KeyboardEvent as D
import qualified DOM.Event.MouseEvent as D
import qualified DOM.Event.Types (KeyboardEvent(), MouseEvent()) as D
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

-- | Type for tracking which keys are currently depressed
type KeyboardState = {w :: Boolean, a :: Boolean, s :: Boolean, d :: Boolean}

-- Type for passing application state between event listeners and the render loop
type AppState = {keyboard :: KeyboardState, simulation :: SimulationState}

modifyKeyboardState :: (KeyboardState -> KeyboardState) -> AppState -> AppState
modifyKeyboardState f state = {keyboard: f state.keyboard, simulation: state.simulation}

modifySimulationState :: (SimulationState -> SimulationState) -> AppState -> AppState
modifySimulationState f state = {keyboard: state.keyboard, simulation: f state.simulation}

-- | Handle canvas mousemove events by updating the simulation state.
canvasMousemove :: forall eff h. STRef h AppState -> D.MouseEvent -> Eff (dom :: D.DOM, st :: ST h | eff) Unit
canvasMousemove stateRef e = do
  return unit

-- | Monitor canvas keydown events so we can track which keys are depressed
canvasKeyDown :: forall eff h. STRef h AppState -> D.KeyboardEvent -> Eff (dom :: D.DOM, st :: ST h | eff) Unit
canvasKeyDown stateRef e | D.keyCode e == D.wKeyCode = do
  modifySTRef stateRef $ modifyKeyboardState \ks -> {w: true, a : ks.a, d: ks.d, s: ks.s}
  return unit
canvasKeyDown stateRef e | D.keyCode e == D.aKeyCode = do
  modifySTRef stateRef $ modifyKeyboardState \ks -> {w: ks.w, a : true, d: ks.d, s: ks.s}
  return unit
canvasKeyDown stateRef e | D.keyCode e == D.dKeyCode = do
  modifySTRef stateRef $ modifyKeyboardState \ks -> {w: ks.w, a : ks.a, d: true, s: ks.s}
  return unit
canvasKeyDown stateRef e | D.keyCode e == D.sKeyCode = do
  modifySTRef stateRef $ modifyKeyboardState \ks -> {w: ks.w, a : ks.a, d: ks.d, s: true}
  return unit
canvasKeyDown stateRef _ = return unit

-- | Monitor canvas keyup events so we can track which keys are depressed
canvasKeyUp :: forall eff h. STRef h AppState -> D.KeyboardEvent -> Eff (dom :: D.DOM, st :: ST h | eff) Unit
canvasKeyUp stateRef e | D.keyCode e == D.wKeyCode = do
  modifySTRef stateRef $ modifyKeyboardState \ks -> {w: false, a : ks.a, d: ks.d, s: ks.s}
  return unit
canvasKeyUp stateRef e | D.keyCode e == D.aKeyCode = do
  modifySTRef stateRef $ modifyKeyboardState \ks -> {w: ks.w, a : false, d: ks.d, s: ks.s}
  return unit
canvasKeyUp stateRef e | D.keyCode e == D.dKeyCode = do
  modifySTRef stateRef $ modifyKeyboardState \ks -> {w: ks.w, a : ks.a, d: false, s: ks.s}
  return unit
canvasKeyUp stateRef e | D.keyCode e == D.sKeyCode = do
  modifySTRef stateRef $ modifyKeyboardState \ks -> {w: ks.w, a : ks.a, d: ks.d, s: false}
  return unit
canvasKeyUp stateRef _ = return unit

-- | Handle canvas clicks. If we are not in full screen, open the canvas in full screen.
-- | Otherwise, toggle the direction of the cube's rotation.
canvasClick :: forall eff h. STRef h AppState -> D.Element -> D.MouseEvent -> Eff (dom :: D.DOM, st :: ST h | eff) Unit
canvasClick stateRef el _ = do
  maybeFullscreenEl <- D.window >>= D.document >>= pure <<< D.htmlDocumentToDocument >>= D.fullscreenElement >>= pure <<< toMaybe
  case maybeFullscreenEl of
    Nothing -> do  
      D.requestFullscreen el
      D.requestPointerLock el
    -- TODO: Check that the fullscreen element is our canvas
    Just fullscreenEl -> do
      modifySTRef stateRef $ modifySimulationState toggleDirection
      return unit

-- | Main loop. Steps the simulation state forwards in time then renders the new state.
tick :: forall eff h. RenderingContext -> STRef h AppState -> Milliseconds -> Eff (canvas :: Canvas, dom :: D.DOM, now :: Now, st :: ST h | eff) Unit
tick ctx stateRef time = do
  newTime <- nowEpochMilliseconds
  state <- modifySTRef stateRef $ modifySimulationState (timestep (newTime - time))
  render ctx state.simulation
  D.requestAnimationFrame $ tick ctx stateRef newTime

-- | Start the application: set up the canvas and start the render loop
main :: Eff (canvas :: Canvas, dom :: D.DOM, err :: EXCEPTION, now :: Now) Unit
main = do
  Just canvas <- getCanvasElementById "easel"
  el <- pure $ toElement canvas
  renderingContext <- initialiseWebGL el canvas
  time <- nowEpochMilliseconds
  elEventTarget <- pure $ D.elementToEventTarget el

  -- Aaa mutable state!
  -- The event listeners need to update the state read by the render loop somehow, so let's use a mutable reference cell
  runST do
    stateRef <- newSTRef {simulation: initialSimulationState, keyboard: {w: false, a: false, s: false, d: false}}
    mousemoveEventListener <- pure $ D.mouseEventListener (canvasMousemove stateRef)
    clickEventListener <- pure $ D.mouseEventListener (canvasClick stateRef el)
    D.addMouseEventListener D.click clickEventListener false elEventTarget
    tick renderingContext stateRef time
