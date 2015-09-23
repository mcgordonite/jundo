-- | Free monad for WebGL computations
module Graphics.WebGL.Free (
  AttributeLocation(..),
  WebGL(),
  WebGLF(),
  debugWebGL,
  runWebGL,
  attachShader,
  bindBuffer,
  bufferInt8Data,
  bufferInt16Data,
  bufferInt32Data,
  bufferUint8Data,
  bufferUint16Data,
  bufferUint32Data,
  bufferUint8ClampedData,
  bufferFloat32Data,
  bufferFloat64Data,
  clear,
  createBuffer,
  clearColor,
  createProgram,
  compileShader,
  createShader,
  depthFunc,
  drawArrays,
  drawElements,
  enable,
  enableVertexAttribArray,
  getAttribLocation,
  getCanvas,
  getDrawingBufferHeight,
  getDrawingBufferWidth,
  getProgramInfoLog,
  getProgramLinkStatus,
  getShaderCompileStatus,
  getShaderInfoLog,
  getUniformLocation,
  linkProgram,
  shaderSource,
  uniformMatrix4fv,
  useProgram,
  vertexAttribPointer,
  viewport
  ) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Free
import qualified Data.ArrayBuffer.Types as A
import Data.Maybe
import Graphics.Canvas (Canvas(), CanvasElement())
import qualified Graphics.WebGL.Raw as R
import qualified Graphics.WebGL.Raw.Extra as R
import qualified Graphics.WebGL.Raw.Enums as GL
import Graphics.WebGL.Raw.Types

-- | The location of an attribute variable in a WebGL shader program
newtype AttributeLocation = AttributeLocation GLuint

data WebGLF a
  = AttachShader WebGLProgram WebGLShader a
  | BindBuffer GLenum WebGLBuffer a
  | BufferInt8Data GLenum A.Int8Array GLenum a
  | BufferInt16Data GLenum A.Int16Array GLenum a
  | BufferInt32Data GLenum A.Int32Array GLenum a
  | BufferUint8Data GLenum A.Uint8Array GLenum a
  | BufferUint16Data GLenum A.Uint16Array GLenum a
  | BufferUint32Data GLenum A.Uint32Array GLenum a
  | BufferUint8ClampedData GLenum A.Uint8ClampedArray GLenum a
  | BufferFloat32Data GLenum A.Float32Array GLenum a
  | BufferFloat64Data GLenum A.Float64Array GLenum a
  | Clear GLbitfield a
  | ClearColor GLclampf GLclampf GLclampf GLclampf a
  | CompileShader WebGLShader a
  | CreateBuffer (Maybe WebGLBuffer -> a)
  | CreateProgram (Maybe WebGLProgram -> a)
  | CreateShader GLenum (Maybe WebGLShader -> a)
  | DepthFunc GLenum a
  | DrawArrays GLenum GLint GLsizei a
  | DrawElements GLenum GLsizei GLenum GLintptr a
  | Enable GLenum a
  | EnableVertexAttribArray AttributeLocation a
  | GetAttribLocation WebGLProgram DOMString (Maybe AttributeLocation -> a)
  | GetCanvas (CanvasElement -> a)
  | GetDrawingBufferHeight (Int -> a)
  | GetDrawingBufferWidth (Int -> a)
  | GetProgramInfoLog WebGLProgram (Maybe DOMString -> a)
  | GetProgramLinkStatus WebGLProgram (Boolean -> a)
  | GetShaderCompileStatus WebGLShader (Boolean -> a)
  | GetShaderInfoLog WebGLShader (Maybe DOMString -> a)
  | GetUniformLocation WebGLProgram DOMString (Maybe WebGLUniformLocation -> a)
  | LinkProgram WebGLProgram a
  | ShaderSource WebGLShader DOMString a
  | UniformMatrix4fv WebGLUniformLocation GLboolean A.Float32Array a
  | UseProgram WebGLProgram a
  | VertexAttribPointer AttributeLocation GLint GLenum GLboolean GLsizei GLintptr a
  | Viewport GLint GLint GLsizei GLsizei a

instance functorWebGLF :: Functor WebGLF where
  map f (AttachShader p s x) = AttachShader p s (f x)
  map f (BindBuffer t b x) = BindBuffer t b (f x)
  map f (BufferInt8Data t s u x) = BufferInt8Data t s u (f x)
  map f (BufferInt16Data t s u x) = BufferInt16Data t s u (f x)
  map f (BufferInt32Data t s u x) = BufferInt32Data t s u (f x)
  map f (BufferUint8Data t s u x) = BufferUint8Data t s u (f x)
  map f (BufferUint16Data t s u x) = BufferUint16Data t s u (f x)
  map f (BufferUint32Data t s u x) = BufferUint32Data t s u (f x)
  map f (BufferUint8ClampedData t s u x) = BufferUint8ClampedData t s u (f x)
  map f (BufferFloat32Data t s u x) = BufferFloat32Data t s u (f x)
  map f (BufferFloat64Data t s u x) = BufferFloat64Data t s u (f x)
  map f (Clear mask x) = Clear mask (f x)
  map f (ClearColor r g b a x) = ClearColor r g b a (f x)
  map f (CompileShader s x) = CompileShader s (f x)
  map f (CreateBuffer k) = CreateBuffer (f <<< k)
  map f (CreateProgram k) = CreateProgram (f <<< k)
  map f (CreateShader t k) = CreateShader t (f <<< k)
  map f (DepthFunc func x) = DepthFunc func (f x)
  map f (DrawArrays mode first count x) = DrawArrays mode first count (f x)
  map f (DrawElements mode count t offset x) = DrawElements mode count t offset (f x)
  map f (Enable cap x) = Enable cap (f x)
  map f (EnableVertexAttribArray i x) = EnableVertexAttribArray i (f x)
  map f (GetAttribLocation p s k) = GetAttribLocation p s (f <<< k)
  map f (GetCanvas k) = GetCanvas (f <<< k)
  map f (GetDrawingBufferHeight k) = GetDrawingBufferHeight (f <<< k)
  map f (GetDrawingBufferWidth k) = GetDrawingBufferWidth (f <<< k)
  map f (GetProgramInfoLog s k) = GetProgramInfoLog s (f <<< k)
  map f (GetProgramLinkStatus p k) = GetProgramLinkStatus p (f <<< k)
  map f (GetShaderCompileStatus s k) = GetShaderCompileStatus s (f <<< k)
  map f (GetShaderInfoLog s k) = GetShaderInfoLog s (f <<< k)
  map f (GetUniformLocation p n k) = GetUniformLocation p n (f <<< k)
  map f (LinkProgram p x) = LinkProgram p (f x)
  map f (ShaderSource sh src x) = ShaderSource sh src (f x)
  map f (UniformMatrix4fv l t v x) = UniformMatrix4fv l t v (f x)
  map f (UseProgram p x) = UseProgram p (f x)
  map f (VertexAttribPointer l s t n str o x) = VertexAttribPointer l s t n str o (f x)
  map f (Viewport x y w h a) = Viewport x y w h (f a)

-- | Free monad for WebGL computations
type WebGL = Free WebGLF

attachShader :: WebGLProgram -> WebGLShader -> WebGL Unit
attachShader p s = liftF $ AttachShader p s unit

bindBuffer :: GLenum -> WebGLBuffer -> WebGL Unit
bindBuffer t b = liftF $ BindBuffer t b unit

bufferInt8Data :: GLenum -> A.Int8Array -> GLenum -> WebGL Unit
bufferInt8Data t s u = liftF $ BufferInt8Data t s u unit

bufferInt16Data :: GLenum -> A.Int16Array -> GLenum -> WebGL Unit
bufferInt16Data t s u = liftF $ BufferInt16Data t s u unit

bufferInt32Data :: GLenum -> A.Int32Array -> GLenum -> WebGL Unit
bufferInt32Data t s u = liftF $ BufferInt32Data t s u unit

bufferUint8Data :: GLenum -> A.Uint8Array -> GLenum -> WebGL Unit
bufferUint8Data t s u = liftF $ BufferUint8Data t s u unit

bufferUint16Data :: GLenum -> A.Uint16Array -> GLenum -> WebGL Unit
bufferUint16Data t s u = liftF $ BufferUint16Data t s u unit

bufferUint32Data :: GLenum -> A.Uint32Array -> GLenum -> WebGL Unit
bufferUint32Data t s u = liftF $ BufferUint32Data t s u unit

bufferUint8ClampedData :: GLenum -> A.Uint8ClampedArray -> GLenum -> WebGL Unit
bufferUint8ClampedData t s u = liftF $ BufferUint8ClampedData t s u unit

bufferFloat32Data :: GLenum -> A.Float32Array -> GLenum -> WebGL Unit
bufferFloat32Data t s u = liftF $ BufferFloat32Data t s u unit

bufferFloat64Data :: GLenum -> A.Float64Array -> GLenum -> WebGL Unit
bufferFloat64Data t s u = liftF $ BufferFloat64Data t s u unit

clear :: GLbitfield -> WebGL Unit
clear mask = liftF $ Clear mask unit

clearColor :: GLclampf -> GLclampf -> GLclampf -> GLclampf -> WebGL Unit
clearColor r g b a = liftF $ ClearColor r g b a unit

compileShader :: WebGLShader -> WebGL Unit
compileShader s = liftF $ CompileShader s unit

createBuffer :: WebGL (Maybe WebGLBuffer)
createBuffer = liftF $ CreateBuffer id

createProgram :: WebGL (Maybe WebGLProgram)
createProgram = liftF $ CreateProgram id

createShader :: GLenum -> WebGL (Maybe WebGLShader)
createShader t = liftF $ CreateShader t id

depthFunc :: GLenum -> WebGL Unit
depthFunc func = liftF $ DepthFunc func unit

drawArrays :: GLenum -> GLint -> GLsizei -> WebGL Unit
drawArrays mode first count = liftF $ DrawArrays mode first count unit

drawElements :: GLenum -> GLsizei -> GLenum -> GLintptr -> WebGL Unit
drawElements mode count t offset = liftF $ DrawElements mode count t offset unit

enable :: GLenum -> WebGL Unit
enable cap = liftF $ Enable cap unit

enableVertexAttribArray :: AttributeLocation -> WebGL Unit
enableVertexAttribArray location = liftF $ EnableVertexAttribArray location unit

getAttribLocation :: WebGLProgram -> DOMString -> WebGL (Maybe AttributeLocation)
getAttribLocation program name = liftF $ GetAttribLocation program name id

getCanvas :: WebGL CanvasElement
getCanvas = liftF $ GetCanvas id

getDrawingBufferHeight :: WebGL Int
getDrawingBufferHeight = liftF $ GetDrawingBufferHeight id

getDrawingBufferWidth :: WebGL Int
getDrawingBufferWidth = liftF $ GetDrawingBufferWidth id

getProgramInfoLog :: WebGLProgram -> WebGL (Maybe DOMString)
getProgramInfoLog p = liftF $ GetProgramInfoLog p id

getProgramLinkStatus :: WebGLProgram -> WebGL Boolean
getProgramLinkStatus p = liftF $ GetProgramLinkStatus p id

getShaderCompileStatus :: WebGLShader -> WebGL Boolean
getShaderCompileStatus s = liftF $ GetShaderCompileStatus s id

getShaderInfoLog :: WebGLShader -> WebGL (Maybe DOMString)
getShaderInfoLog s = liftF $ GetShaderInfoLog s id

getUniformLocation :: WebGLProgram -> DOMString -> WebGL (Maybe WebGLUniformLocation)
getUniformLocation program name = liftF $ GetUniformLocation program name id

linkProgram :: WebGLProgram -> WebGL Unit
linkProgram p = liftF $ LinkProgram p unit

shaderSource :: WebGLShader -> DOMString -> WebGL Unit
shaderSource sh src = liftF $ ShaderSource sh src unit

uniformMatrix4fv :: WebGLUniformLocation -> GLboolean -> A.Float32Array -> WebGL Unit
uniformMatrix4fv location transpose value = liftF $ UniformMatrix4fv location transpose value unit

useProgram :: WebGLProgram -> WebGL Unit
useProgram p = liftF $ UseProgram p unit

vertexAttribPointer :: AttributeLocation -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> WebGL Unit
vertexAttribPointer l s t n str o = liftF $ VertexAttribPointer l s t n str o unit

viewport :: GLint -> GLint -> GLsizei -> GLsizei -> WebGL Unit
viewport x y w h = liftF $ Viewport x y w h unit

-- Interpret the WebGL functor as actions from the WebGL raw module
interpretWebGL :: forall a eff. WebGLContext -> WebGLF (WebGL a) -> Eff (canvas :: Canvas | eff) (WebGL a)
interpretWebGL gl (AttachShader p s rest) = do
  R.attachShader gl p s
  return rest
interpretWebGL gl (BindBuffer t buffer rest) = do
  R.bindBuffer gl t buffer
  return rest
interpretWebGL gl (BufferInt8Data t source usage rest) = do
  R.bufferInt8Data gl t source usage
  return rest
interpretWebGL gl (BufferInt16Data t source usage rest) = do
  R.bufferInt16Data gl t source usage
  return rest
interpretWebGL gl (BufferInt32Data t source usage rest) = do
  R.bufferInt32Data gl t source usage
  return rest
interpretWebGL gl (BufferUint8Data t source usage rest) = do
  R.bufferUint8Data gl t source usage
  return rest
interpretWebGL gl (BufferUint16Data t source usage rest) = do
  R.bufferUint16Data gl t source usage
  return rest
interpretWebGL gl (BufferUint32Data t source usage rest) = do
  R.bufferUint32Data gl t source usage
  return rest
interpretWebGL gl (BufferUint8ClampedData t source usage rest) = do
  R.bufferUint8ClampedData gl t source usage
  return rest
interpretWebGL gl (BufferFloat32Data t source usage rest) = do
  R.bufferFloat32Data gl t source usage
  return rest
interpretWebGL gl (BufferFloat64Data t source usage rest) = do
  R.bufferFloat64Data gl t source usage
  return rest
interpretWebGL gl (Clear mask rest) = do
  R.clear gl mask
  return rest
interpretWebGL gl (ClearColor r g b a rest) = do
  R.clearColor gl r g b a
  return rest
interpretWebGL gl (CompileShader sh rest) = do
  R.compileShader gl sh
  return rest
interpretWebGL gl (CreateBuffer k) = do
  buffer <- R.createBuffer gl
  return $ k buffer
interpretWebGL gl (CreateProgram k) = do
  program <- R.createProgram gl
  return $ k program
interpretWebGL gl (CreateShader t k) = do
  shader <- R.createShader gl t
  return $ k shader
interpretWebGL gl (DepthFunc func rest) = do
  R.depthFunc gl func
  return rest
interpretWebGL gl (DrawArrays mode first count rest) = do
  R.drawArrays gl mode first count
  return rest
interpretWebGL gl (DrawElements mode count t offset rest) = do
  R.drawElements gl mode count t offset
  return rest
interpretWebGL gl (Enable cap rest) = do
  R.enable gl cap
  return rest
interpretWebGL gl (EnableVertexAttribArray (AttributeLocation location) rest) = do
  R.enableVertexAttribArray gl location
  return rest
interpretWebGL gl (GetAttribLocation program name k) = do
  location <- R.getAttribLocation gl program name
  if location == -1
    then return $ k Nothing
    else return $ k $ Just (AttributeLocation location)
interpretWebGL gl (GetCanvas k) = do
  el <- R.getCanvas gl
  return $ k el
interpretWebGL gl (GetDrawingBufferHeight k) = do
  h <- R.getDrawingBufferHeight gl
  return $ k h
interpretWebGL gl (GetDrawingBufferWidth k) = do
  w <- R.getDrawingBufferWidth gl
  return $ k w
interpretWebGL gl (GetProgramInfoLog p k) = do
  maybeLog <- R.getProgramInfoLog gl p
  return $ k maybeLog
interpretWebGL gl (GetProgramLinkStatus p k) = do
  Just b <- R.getProgramParameter gl p GL.linkStatus
  return $ k b
interpretWebGL gl (GetShaderCompileStatus s k) = do
  Just b <- R.getShaderParameter gl s GL.compileStatus
  return $ k b
interpretWebGL gl (GetShaderInfoLog s k) = do
  maybeLog <- R.getShaderInfoLog gl s
  return $ k maybeLog
interpretWebGL gl (GetUniformLocation program name k) = do
  location <- R.getUniformLocation gl program name
  return $ k location
interpretWebGL gl (LinkProgram p rest) = do
  R.linkProgram gl p
  return rest
interpretWebGL gl (ShaderSource sh src rest) = do
  R.shaderSource gl sh src
  return rest
interpretWebGL gl (UniformMatrix4fv location transpose value rest) = do
  R.uniformMatrix4fv_ gl location transpose value
  return rest
interpretWebGL gl (UseProgram p rest) = do
  R.useProgram gl p
  return rest
interpretWebGL gl (VertexAttribPointer (AttributeLocation l) s t n str o rest) = do
  R.vertexAttribPointer gl l s t n o str
  return rest
interpretWebGL gl (Viewport x y w h rest) = do
  R.viewport gl x y w h
  return rest

-- | Interpret the WebGL monad as actions from the raw WebGL module
runWebGL :: forall a eff. WebGLContext -> WebGL a -> Eff (canvas :: Canvas | eff) a
runWebGL = runFreeM <<< interpretWebGL

-- | Interpret the WebGL monad as actions from the raw WebGL module, checking for an error after every action
debugWebGL :: forall a eff. WebGLContext -> WebGL a -> Eff (canvas :: Canvas, console :: CONSOLE | eff) a
debugWebGL gl = runFreeM interpretDebug
  where
  interpretDebug :: forall a. WebGLF (WebGL a) -> Eff (canvas :: Canvas, console :: CONSOLE | eff) (WebGL a)
  interpretDebug comp = do
    rest <- interpretWebGL gl comp
    err <- R.getError gl
    if err == GL.noError
      then return rest
      else do
        error $ "WebGL Error: " ++ show err
        return rest
