module Main where

import Prelude
import Cube
import Shaders
import Simulation
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.ST
import Data.ArrayBuffer.Types (Float32Array())
import Data.Date (Now(), nowEpochMilliseconds)
import Data.Time (Milliseconds(..))
import Data.Either
import Data.Int (toNumber)
import Data.Int.Bits
import Data.Matrix
import Data.Matrix4
import Data.Vector3 (vec3, j3)
import Data.Maybe
import Data.Nullable
import Data.Tuple
import Data.TypedArray (asFloat32Array)
import qualified DOM as D
import qualified DOM.Event.EventTarget as D
import qualified DOM.Event.Experimental as D
import qualified DOM.Event.MouseEvent as D
import qualified DOM.Event.Types (MouseEvent()) as D
import qualified DOM.HTML as D
import qualified DOM.HTML.Types as D
import qualified DOM.HTML.Window as D
import qualified DOM.Node.Document.Experimental as D
import qualified DOM.Node.Element.Experimental as D
import qualified DOM.Node.Types as D
import qualified DOM.RequestAnimationFrame as D
import Graphics.WebGL.Context
import Graphics.WebGL.Free
import qualified Graphics.WebGL.Raw.Enums as GL
import Graphics.WebGL.Raw.Types
import Graphics.Canvas (Canvas(), CanvasElement(), getCanvasElementById, setCanvasDimensions)
import Graphics.Canvas.Element
import Math.Radians

matrixToFloat32Array :: Mat4 -> Float32Array
matrixToFloat32Array = asFloat32Array <<< toArray

mvMatrix :: Radians -> Float32Array
mvMatrix (Radians angle) = matrixToFloat32Array $ rotate angle j3 $ translate (vec3 0.0 0.0 (-6.0)) identity

perspectiveMatrix :: Int -> Int -> Float32Array
perspectiveMatrix bufferWidth bufferHeight = matrixToFloat32Array $
	makePerspective 45.0 (toNumber bufferWidth / toNumber bufferHeight) 0.1 100.0

type CanvasContext = {
	el :: CanvasElement,
	gl :: WebGLContext,
	programLocations :: ProgramLocations,
	cubeBuffers :: CubeBuffers
	}

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

tick :: forall eff h. CanvasContext -> STRef h SimulationState -> Milliseconds -> Eff (canvas :: Canvas, dom :: D.DOM, now :: Now, st :: ST h | eff) Unit
tick c stateRef time = do
	h <- D.clientHeight $ toElement c.el
	w <- D.clientWidth $ toElement c.el
	setCanvasDimensions {height: toNumber h, width: toNumber w} c.el
	newTime <- nowEpochMilliseconds
	newSimulationState <- modifySTRef stateRef $ timestep (newTime - time)
	runWebGL c.gl do
		bufferHeight <- getDrawingBufferHeight
		bufferWidth <- getDrawingBufferWidth
		viewport 0 0 bufferWidth bufferHeight

		clear $ GL.colorBufferBit .|. GL.depthBufferBit
		uniformMatrix4fv c.programLocations.pMatrix false $ perspectiveMatrix bufferWidth bufferHeight
		uniformMatrix4fv c.programLocations.mvMatrix false $ mvMatrix newSimulationState.angle

		bindBuffer GL.arrayBuffer c.cubeBuffers.vertex
		vertexAttribPointer c.programLocations.aVertex 3 GL.float false 0 0
		bindBuffer GL.elementArrayBuffer c.cubeBuffers.index
		drawElements GL.triangles 36 GL.unsignedShort 0
	D.requestAnimationFrame $ tick c stateRef newTime

main :: Eff (canvas :: Canvas, dom :: D.DOM, err :: EXCEPTION, now :: Now) Unit
main = do
	Just el <- getCanvasElementById "easel"
	gl <- getWebGLContext el
	Tuple program locations <- initialiseShaderProgram gl
	cubeBuffers <- runWebGL gl do
		clearColor 0.0 0.0 0.0 1.0
		enable GL.depthTest
		depthFunc GL.lequal
		useProgram program
		initialiseBuffers
	time <- nowEpochMilliseconds
	elEventTarget <- pure $ D.elementToEventTarget $ toElement el
	runST do
		stateRef <- newSTRef initialSimulationState
		D.addMouseEventListener D.click (canvasClick stateRef $ toElement el) false elEventTarget
		tick {el: el, gl: gl, programLocations: locations, cubeBuffers: cubeBuffers} stateRef time

