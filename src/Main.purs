module Main where

import Prelude
import Cube
import Shaders
import Control.Monad.Eff
import Control.Monad.Eff.Exception
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
import Data.Tuple
import Data.TypedArray (asFloat32Array)
import qualified DOM as D
import qualified DOM.Event.EventTarget as D
import qualified DOM.Event.EventTypes (click) as D
import qualified DOM.Event.Experimental as D
import qualified DOM.Event.Types as D
import qualified DOM.Node.Element.Experimental as D
import qualified DOM.Node.Types as D
import qualified DOM.RequestAnimationFrame as D
import Graphics.WebGL.Context
import Graphics.WebGL.Free
import qualified Graphics.WebGL.Raw.Enums as GL
import Graphics.WebGL.Raw.Types
import Graphics.Canvas (Canvas(), CanvasElement(), getCanvasElementById, setCanvasDimensions)
import Graphics.Canvas.Element

matrixToFloat32Array :: Mat4 -> Float32Array
matrixToFloat32Array = asFloat32Array <<< toArray

mvMatrix :: Number -> Float32Array
mvMatrix angle = matrixToFloat32Array $ rotate angle j3 $ translate (vec3 0.0 0.0 (-6.0)) identity

perspectiveMatrix :: Int -> Int -> Float32Array
perspectiveMatrix bufferWidth bufferHeight = matrixToFloat32Array $
	makePerspective 45.0 (toNumber bufferWidth / toNumber bufferHeight) 0.1 100.0

canvasClick :: forall eff. CanvasElement -> D.Event -> Eff (dom :: D.DOM | eff) Unit
canvasClick canvas event = do
	el <- pure $ toElement canvas
	D.requestFullscreen el
	D.requestPointerLock el

tick :: forall eff. CanvasElement -> WebGLContext -> (Tuple WebGLBuffer WebGLBuffer) -> ProgramLocations -> Number -> Number -> Eff (canvas :: Canvas, dom :: D.DOM, now :: Now | eff) Unit
tick el gl (Tuple cubeVertexBuffer cubeIndexBuffer) (ProgramLocations locs) previousTime previousAngle = do
	h <- D.clientHeight $ toElement el
	w <- D.clientWidth $ toElement el
	setCanvasDimensions {height: toNumber h, width: toNumber w} el
	Milliseconds currentTime <- nowEpochMilliseconds
	currentAngle <- pure $ previousAngle + 0.001 * (currentTime - previousTime)
	runWebGL gl do
		bufferHeight <- getDrawingBufferHeight
		bufferWidth <- getDrawingBufferWidth
		viewport 0 0 bufferWidth bufferHeight

		clear $ GL.colorBufferBit .|. GL.depthBufferBit
		uniformMatrix4fv locs.pMatrix false $ perspectiveMatrix bufferWidth bufferHeight
		uniformMatrix4fv locs.mvMatrix false $ mvMatrix currentAngle

		bindBuffer GL.arrayBuffer cubeVertexBuffer
		vertexAttribPointer locs.aVertex 3 GL.float false 0 0
		bindBuffer GL.elementArrayBuffer cubeIndexBuffer
		drawElements GL.triangles 36 GL.unsignedShort 0
	D.requestAnimationFrame $ tick el gl (Tuple cubeVertexBuffer cubeIndexBuffer) (ProgramLocations locs) currentTime currentAngle

main :: Eff (canvas :: Canvas, dom :: D.DOM, err :: EXCEPTION, now :: Now) Unit
main = do
	Just el <- getCanvasElementById "easel"
	D.addEventListener D.click (D.eventListener $ canvasClick el) false (D.elementToEventTarget $ toElement el)
	gl <- getWebGLContext el
	Tuple program locations <- initialiseShaderProgram gl
	buffers <- runWebGL gl do
		clearColor 0.0 0.0 0.0 1.0
		enable GL.depthTest
		depthFunc GL.lequal
		useProgram program
		initialiseBuffers
	Milliseconds time <- nowEpochMilliseconds
	tick el gl buffers locations time 0.0
