-- | Functions for converting the simulation state into pixels on the screen
module Jundo.Rendering (
  RenderingContext(),
  initialiseWebGL,
  render
  ) where

import Prelude
import Jundo.Cube
import Jundo.Shaders
import Jundo.Simulation
import Jundo.Vectors
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Data.ArrayBuffer.Types (Float32Array())
import Data.Int (toNumber)
import Data.Int.Bits ((.|.))
import Data.Matrix
import Data.Matrix4
import Data.Tuple
import Data.TypedArray (asFloat32Array)
import Data.Vector3 (vec3, Vec3(), i3, j3)
import Data.Vector4
import qualified DOM as D
import qualified DOM.Node.Element.Experimental as D
import qualified DOM.Node.Types as D
import Graphics.WebGL.Context
import Graphics.WebGL.Free
import Graphics.WebGL.Raw.Types
import Graphics.Canvas (Canvas(), CanvasElement(), setCanvasDimensions)
import Math

-- | The matrix library is based on plain JavaScript arrays. Extract the backing array from the matrix and convert
-- | it to a typed array so we can use it with WebGL.
matrixToFloat32Array :: Mat4 -> Float32Array
matrixToFloat32Array = asFloat32Array <<< toArray

cubeModelMatrix :: CubeState -> Mat4
cubeModelMatrix cs = mulM (makeTranslate cs.position) (makeRotate cs.angle j3)

viewMatrix :: Radians -> Radians -> Mat4
viewMatrix pitch yaw = mulM (makeRotate pitch (rotateVec3 j3 yaw i3)) (makeRotate yaw j3)

-- | Get the cube's model view matrix from the camera and cube angles
cubeMVMatrix :: CubeState -> Radians -> Radians -> Float32Array
cubeMVMatrix cs pitch yaw = matrixToFloat32Array $ mulM (viewMatrix pitch yaw) (cubeModelMatrix cs)

-- | Get a perspective matrix as a typed array for the given buffer dimensions
perspectiveMatrix :: Int -> Int -> Float32Array
perspectiveMatrix width height = matrixToFloat32Array $ makePerspective 45.0 (toNumber width / toNumber height) 0.1 100.0

-- | Type to hold canvas properties such as the WebGL context and references to shader program variables
newtype RenderingContext = RenderingContext {
  canvas :: CanvasElement,
  el :: D.Element,
  gl :: WebGLContext,
  program :: WebGLProgram,
  shaderVariables :: ShaderVariables,
  cubeBuffers :: CubeBuffers
  }

-- | Perform initial set up of the canvas, returning a context containing variables required by the render function
initialiseWebGL :: forall eff. D.Element -> CanvasElement -> Eff (canvas :: Canvas, dom :: D.DOM, err :: EXCEPTION | eff) RenderingContext
initialiseWebGL el canvas = do
  gl <- getWebGLContext canvas
  Tuple program shaderVariables <- initialiseShaderProgram gl
  cubeBuffers <- runWebGL gl do
    clearColor 0.0 0.0 0.0 1.0
    enable depthTest
    depthFunc lequal
    initialiseBuffers program
  return $ RenderingContext {gl: gl, canvas: canvas, el: el, shaderVariables: shaderVariables, cubeBuffers: cubeBuffers, program: program}

-- | Render the simulation state to the canvas
render :: forall eff. RenderingContext -> SimulationState -> Eff (canvas :: Canvas, dom :: D.DOM | eff) Unit
render (RenderingContext ctx) {cube: cubeState, camera: cameraState} = do
  -- Update the canvas dimensions in case the element dimensions have changed
  height <- D.clientHeight $ ctx.el
  width <- D.clientWidth $ ctx.el
  setCanvasDimensions {height: toNumber height, width: toNumber width} ctx.canvas

  runWebGL ctx.gl do
    -- Update the WebGL viewport dimensions to match the available drawing buffer
    bufferHeight <- getDrawingBufferHeight
    bufferWidth <- getDrawingBufferWidth
    viewport 0 0 bufferWidth bufferHeight

    -- Clear the canvas
    clear $ colorBufferBit .|. depthBufferBit

    -- Draw the cube!
    programOperation ctx.program do
      uniformMatrix4fv ctx.shaderVariables.pMatrix false $ perspectiveMatrix bufferWidth bufferHeight
      uniformMatrix4fv ctx.shaderVariables.mvMatrix false $ cubeMVMatrix cubeState cameraState.pitch cameraState.yaw
      arrayBufferOperation ctx.cubeBuffers.vertex $ vertexAttribPointer ctx.shaderVariables.aVertex 3 false 0 0
      elementArrayBufferOperation ctx.cubeBuffers.index $ drawElements triangles 36 0
  return unit
