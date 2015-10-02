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
import Data.Tuple
import qualified DOM as D
import qualified DOM.Event.EventTarget as D
import qualified DOM.Event.Experimental as D
import qualified DOM.Event.KeyboardEvent as D
import qualified DOM.Event.MouseEvent as D
import qualified DOM.Event.Types (Event(), EventTarget(), KeyboardEvent(), MouseEvent()) as D
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

-- | Get the global document as a Document type
globalDocument :: forall eff. Eff (dom :: D.DOM | eff) D.Document
globalDocument = D.window >>= D.document >>= pure <<< D.htmlDocumentToDocument

-- | Returns true if we are in full screen and have pointer lock
inFullscreen :: forall eff. Eff (dom :: D.DOM | eff) Boolean
inFullscreen = do
  document <- globalDocument
  maybeFullscreenEl <- D.fullscreenElement document >>= pure <<< toMaybe
  maybePointerLockEl <- D.pointerLockElement document >>= pure <<< toMaybe
  -- TODO: Check that the full screen and pointer lock elements are our canvas
  case Tuple maybeFullscreenEl maybePointerLockEl of
    Tuple (Just _) (Just _) -> return true
    Tuple _ _ -> return false

-- | Type for tracking which keys are currently depressed
type KeyboardState = {w :: Boolean, a :: Boolean, s :: Boolean, d :: Boolean}

-- | Update the state of the key with the given key code if it is one we are tracking 
updateKey :: Int -> Boolean -> KeyboardState -> KeyboardState
updateKey code isDown ks | code == D.wKeyCode = {w: isDown, a: ks.a, d: ks.d, s: ks.s}
updateKey code isDown ks | code == D.aKeyCode = {w: ks.w, a: isDown, d: ks.d, s: ks.s}
updateKey code isDown ks | code == D.dKeyCode = {w: ks.w, a: ks.a, d: isDown, s: ks.s}
updateKey code isDown ks | code == D.sKeyCode = {w: ks.w, a: ks.a, d: ks.d, s: isDown}
updateKey _ _ ks = ks

-- Type for passing application state between event listeners and the render loop
type AppState = {keyboard :: KeyboardState, simulation :: SimulationState}

modifyKeyboardState :: (KeyboardState -> KeyboardState) -> AppState -> AppState
modifyKeyboardState f state = {keyboard: f state.keyboard, simulation: state.simulation}

modifySimulationState :: (SimulationState -> SimulationState) -> AppState -> AppState
modifySimulationState f state = {keyboard: state.keyboard, simulation: f state.simulation}

-- | Handle canvas mousemove events by updating the simulation state.
canvasMousemove :: forall eff h. STRef h AppState -> D.MouseEvent -> Eff (dom :: D.DOM, st :: ST h | eff) Unit
canvasMousemove stateRef e = do
  movement <- pure $ Tuple (D.movementX e) (D.movementY e)
  return unit

-- | Monitor canvas keydown events so we can track which keys are depressed
canvasKeydown :: forall eff h. STRef h AppState -> D.KeyboardEvent -> Eff (dom :: D.DOM, st :: ST h | eff) Unit
canvasKeydown stateRef e = do
  modifySTRef stateRef $ modifyKeyboardState $ updateKey (D.keyCode e) true
  return unit

-- | Monitor canvas keyup events so we can track which keys are depressed
canvasKeyup :: forall eff h. STRef h AppState -> D.KeyboardEvent -> Eff (dom :: D.DOM, st :: ST h | eff) Unit
canvasKeyup stateRef e = do
  modifySTRef stateRef $ modifyKeyboardState $ updateKey (D.keyCode e) false
  return unit

-- | Handle canvas clicks. If we are not in full screen, open the canvas in full screen.
-- | Otherwise, toggle the direction of the cube's rotation.
canvasClick :: forall eff h. STRef h AppState -> D.Element -> D.MouseEvent -> Eff (dom :: D.DOM, st :: ST h | eff) Unit
canvasClick stateRef el _ = do
  fullScreen <- inFullscreen
  if fullScreen
    then do
      modifySTRef stateRef $ modifySimulationState toggleDirection
      return unit
    else do
      D.requestFullscreen el
      D.requestPointerLock el

-- | Add keyboard and mouse move event listeners when we enter full screen, and remove them when we leave it
documentFullscreenChange :: forall eff. D.KeyboardEventListener (dom :: D.DOM | eff) -> D.KeyboardEventListener (dom :: D.DOM | eff)
  -> D.MouseEventListener (dom :: D.DOM | eff) -> D.EventTarget -> D.Event -> Eff (dom :: D.DOM | eff) Unit
documentFullscreenChange keyupListener keydownListener moveListener targetCanvas _ = do
  fullScreen <- inFullscreen
  if fullScreen
    then do
      D.addKeyboardEventListener D.keyup keyupListener false targetCanvas
      D.addKeyboardEventListener D.keydown keydownListener false targetCanvas
      D.addMouseEventListener D.mousemove moveListener false targetCanvas
    else do
      D.removeKeyboardEventListener D.keyup keyupListener false targetCanvas
      D.removeKeyboardEventListener D.keydown keydownListener false targetCanvas
      D.removeMouseEventListener D.mousemove moveListener false targetCanvas

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

    -- Event listeners added and removed when we enter and leave full screen
    keyupListener <- pure $ D.keyboardEventListener (canvasKeyup stateRef)
    keydownListener <- pure $ D.keyboardEventListener (canvasKeydown stateRef)
    moveListener <- pure $ D.mouseEventListener (canvasMousemove stateRef)

    -- Listener for pointer lock and full screen change events
    fullscreenListener <- pure $ D.eventListener (documentFullscreenChange keyupListener keydownListener moveListener elEventTarget)
    documentEventTarget <- globalDocument >>= pure <<< D.documentToEventTarget
    D.addEventListener D.fullscreenChange fullscreenListener false documentEventTarget
    D.addEventListener D.pointerLockChange fullscreenListener false documentEventTarget

    D.addMouseEventListener D.click (D.mouseEventListener (canvasClick stateRef el)) false elEventTarget
    tick renderingContext stateRef time
