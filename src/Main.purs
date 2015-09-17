module Main where

import Prelude
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
import qualified DOM.Node.Element.Extra as D
import qualified DOM.RequestAnimationFrame as D
import Graphics.WebGL.Context
import Graphics.WebGL.Free
import qualified Graphics.WebGL.Raw.Enums as GL
import Graphics.WebGL.Raw.Types
import Graphics.Canvas (Canvas(), CanvasElement(), getCanvasElementById, setCanvasDimensions)
import Graphics.Canvas.Extra

matrixToFloat32Array :: Mat4 -> Float32Array
matrixToFloat32Array = asFloat32Array <<< toArray

mvMatrix :: Number -> Float32Array
mvMatrix angle = matrixToFloat32Array $ rotate angle j3 $ translate (vec3 0.0 0.0 (-6.0)) identity

perspectiveMatrix :: Int -> Int -> Float32Array
perspectiveMatrix bufferWidth bufferHeight = matrixToFloat32Array $
	makePerspective 45.0 (toNumber bufferWidth / toNumber bufferHeight) 0.1 100.0

squareVertices :: Float32Array
squareVertices = asFloat32Array [1.0, 1.0, 0.0, -1.0, 1.0, 0.0, 1.0, -1.0, 0.0, -1.0, -1.0, 0.0]

tick :: forall eff. CanvasElement -> WebGLContext -> WebGLBuffer -> ProgramLocations -> Number -> Number -> Eff (canvas :: Canvas, dom :: D.DOM, now :: Now | eff) Unit
tick el gl buffer (ProgramLocations locs) previousTime previousAngle = do
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
		bindBuffer GL.arrayBuffer buffer
		vertexAttribPointer locs.aVertex 3 GL.float false 0 0
		drawArrays GL.triangleStrip 0 4
	D.requestAnimationFrame $ tick el gl buffer (ProgramLocations locs) currentTime currentAngle

main :: Eff (canvas :: Canvas, dom :: D.DOM, err :: EXCEPTION, now :: Now) Unit
main = do
	Just el <- getCanvasElementById "easel"
	gl <- getWebGLContext el
	Tuple program locations <- initialiseShaderProgram gl
	buffer <- runWebGL gl do
		useProgram program

		-- TODO: This will return Nothing if the context is lost
		Just buffer <- createBuffer
		bindBuffer GL.arrayBuffer buffer
		bufferData GL.arrayBuffer squareVertices GL.staticDraw

		clearColor 0.0 0.0 0.0 1.0
		enable GL.depthTest
		depthFunc GL.lequal
		return buffer
	Milliseconds time <- nowEpochMilliseconds
	tick el gl buffer locations time 0.0
