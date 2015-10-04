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

-- | Update the state of the key with the given key code if it is one we are tracking
updateKey :: Int -> Boolean -> KeyboardState -> KeyboardState
updateKey code isDown (KeyboardState ks) | code == D.wKeyCode = KeyboardState {w: isDown, a: ks.a, d: ks.d, s: ks.s}
updateKey code isDown (KeyboardState ks) | code == D.aKeyCode = KeyboardState {w: ks.w, a: isDown, d: ks.d, s: ks.s}
updateKey code isDown (KeyboardState ks) | code == D.dKeyCode = KeyboardState {w: ks.w, a: ks.a, d: isDown, s: ks.s}
updateKey code isDown (KeyboardState ks) | code == D.sKeyCode = KeyboardState {w: ks.w, a: ks.a, d: ks.d, s: isDown}
updateKey _ _ ks = ks

-- Type for passing application state between event listeners and the render loop
newtype AppState = AppState {keyboard :: KeyboardState, simulation :: SimulationState}

instance showAppState :: Show AppState where
  show (AppState s) = "AppState {keyboard" ++ show s.keyboard ++ ", simulation: " ++ show s.simulation ++ "}"

mapKeyboardState :: (KeyboardState -> KeyboardState) -> AppState -> AppState
mapKeyboardState f (AppState s) = AppState {keyboard: f s.keyboard, simulation: s.simulation}

mapSimulationState :: (SimulationState -> SimulationState) -> AppState -> AppState
mapSimulationState f (AppState s) = AppState {keyboard: s.keyboard, simulation: f s.simulation}

-- | Handle canvas mousemove events by updating the simulation state.
canvasMousemove :: forall eff h. STRef h AppState -> D.MouseEvent -> Eff (dom :: D.DOM, st :: ST h | eff) Unit
canvasMousemove stateRef e = do
  modifySTRef stateRef $ mapSimulationState $ applyMouseMove $ MouseMove (D.movementX e) (D.movementY e)
  return unit

-- | Handle canvas clicks. If we are not in full screen, open the canvas in full screen.
-- | Otherwise, toggle the direction of the cube's rotation.
canvasClick :: forall eff h. STRef h AppState -> D.Element -> D.MouseEvent -> Eff (dom :: D.DOM, st :: ST h | eff) Unit
canvasClick stateRef el _ = do
  fullScreen <- inFullscreen
  if fullScreen
    then do
      modifySTRef stateRef $ mapSimulationState toggleDirection
      return unit
    else do
      D.requestFullscreen el
      D.requestPointerLock el

-- | Monitor document keydown events so we can track which keys are depressed
documentKeydown :: forall eff h. STRef h AppState -> D.KeyboardEvent -> Eff (dom :: D.DOM, st :: ST h | eff) Unit
documentKeydown stateRef e = do
  modifySTRef stateRef $ mapKeyboardState $ updateKey (D.keyCode e) true
  return unit

-- | Monitor document keyup events so we can track which keys are depressed
documentKeyup :: forall eff h. STRef h AppState -> D.KeyboardEvent -> Eff (dom :: D.DOM, st :: ST h | eff) Unit
documentKeyup stateRef e = do
  modifySTRef stateRef $ mapKeyboardState $ updateKey (D.keyCode e) false
  return unit

-- | Add keyboard and mouse move event listeners when we enter full screen, and remove them when we leave it
documentFullscreenChange :: forall eff. D.KeyboardEventListener (dom :: D.DOM | eff) -> D.KeyboardEventListener (dom :: D.DOM | eff)
  -> D.MouseEventListener (dom :: D.DOM | eff) -> D.EventTarget -> D.EventTarget -> D.Event -> Eff (dom :: D.DOM | eff) Unit
documentFullscreenChange keyupListener keydownListener moveListener targetCanvas targetDocument _ = do
  fullScreen <- inFullscreen
  if fullScreen
    then do
      D.addKeyboardEventListener D.keyup keyupListener false targetDocument
      D.addKeyboardEventListener D.keydown keydownListener false targetDocument
      D.addMouseEventListener D.mousemove moveListener false targetCanvas
    else do
      D.removeKeyboardEventListener D.keyup keyupListener false targetDocument
      D.removeKeyboardEventListener D.keydown keydownListener false targetDocument
      D.removeMouseEventListener D.mousemove moveListener false targetCanvas

-- | Main loop. Steps the simulation state forwards in time then renders the new state.
tick :: forall eff h. RenderingContext -> STRef h AppState -> Milliseconds -> Eff (canvas :: Canvas, dom :: D.DOM, now :: Now, st :: ST h | eff) Unit
tick ctx stateRef time = do
  newTime <- nowEpochMilliseconds
  (AppState s) <- modifySTRef stateRef (\(AppState s) -> AppState {keyboard: s.keyboard, simulation: timestep (newTime - time) s.keyboard s.simulation})
  render ctx s.simulation
  D.requestAnimationFrame $ tick ctx stateRef newTime

-- | Start the application: set up the canvas and start the render loop
main :: Eff (canvas :: Canvas, dom :: D.DOM, err :: EXCEPTION, now :: Now) Unit
main = do
  Just canvas <- getCanvasElementById "easel"
  el <- pure $ toElement canvas
  renderingContext <- initialiseWebGL el canvas
  time <- nowEpochMilliseconds
  elEventTarget <- pure $ D.elementToEventTarget el
  documentEventTarget <- globalDocument >>= pure <<< D.documentToEventTarget

  -- Aaa mutable state!
  -- The event listeners need to update the state read by the render loop somehow, so let's use a mutable reference cell
  runST do
    stateRef <- newSTRef $ AppState {simulation: initialSimulationState, keyboard: KeyboardState {w: false, a: false, s: false, d: false}}

    -- Event listeners added and removed when we enter and leave full screen
    keyupListener <- pure $ D.keyboardEventListener (documentKeyup stateRef)
    keydownListener <- pure $ D.keyboardEventListener (documentKeydown stateRef)
    moveListener <- pure $ D.mouseEventListener (canvasMousemove stateRef)

    -- Listener for pointer lock and full screen change events
    fullscreenListener <- pure $ D.eventListener (documentFullscreenChange keyupListener keydownListener moveListener elEventTarget documentEventTarget)
    D.addEventListener D.fullscreenChange fullscreenListener false documentEventTarget
    D.addEventListener D.pointerLockChange fullscreenListener false documentEventTarget

    D.addMouseEventListener D.click (D.mouseEventListener (canvasClick stateRef el)) false elEventTarget
    tick renderingContext stateRef time
