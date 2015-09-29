-- TODO: Enum type safety - methods accepting GL enums only accept a subset of GL enums
-- TODO: Vector/Matrix type safety - methods accepting eg a 4D matrix should only accept arrays of the correct length
-- TODO: Program type safety - methods referring to variable locations should only accept locations from the active program
-- TODO: It should be possible to bufferData before useProgram - type classes to the rescue?

-- | Free monad for WebGL computations.
-- |
-- | A lot of the WebGL API requires you to specify the object that you are going to act on separately from the action.
-- | For example a bindBuffer call is required before bufferData can be called. This makes it easy to shoot yourself in
-- | the foot. This module makes the bind calls so code using it doesn't have to via free monads for operations involving
-- | shader programs and buffers.
module Graphics.WebGL.Free (
  AttributeLocation(..),
  WebGL(),
  WebGLF(),
  WebGLArrayBufferOperation(),
  WebGLArrayBufferOperationF(),
  WebGLElementArrayBufferOperation(),
  WebGLElementArrayBufferOperationF(),
  WebGLProgramOperation(),
  WebGLProgramOperationF(),
  BufferData,
  bufferInt8Data,
  bufferInt16Data,
  bufferInt32Data,
  bufferUint8Data,
  bufferUint16Data,
  bufferUint32Data,
  bufferUint8ClampedData,
  bufferFloat32Data,
  bufferFloat64Data,
  runWebGL,
  arrayBufferOperation,
  attachShader,
  clear,
  createBuffer,
  clearColor,
  createProgram,
  compileShader,
  createShader,
  depthFunc,
  drawArrays,
  drawElements,
  elementArrayBufferOperation,
  enable,
  enableVertexAttribArray,
  getAttribLocation,
  getDrawingBufferHeight,
  getDrawingBufferWidth,
  getProgramInfoLog,
  getProgramLinkStatus,
  getShaderCompileStatus,
  getShaderInfoLog,
  getUniformLocation,
  linkProgram,
  programOperation,
  shaderSource,
  uniformMatrix4fv,
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
  | Clear GLbitfield a
  | ClearColor GLclampf GLclampf GLclampf GLclampf a
  | CompileShader WebGLShader a
  | CreateBuffer (Maybe WebGLBuffer -> a)
  | CreateProgram (Maybe WebGLProgram -> a)
  | CreateShader GLenum (Maybe WebGLShader -> a)
  | DepthFunc GLenum a
  | Enable GLenum a
  | GetAttribLocation WebGLProgram DOMString (Maybe AttributeLocation -> a)
  | GetDrawingBufferHeight (Int -> a)
  | GetDrawingBufferWidth (Int -> a)
  | GetProgramInfoLog WebGLProgram (Maybe DOMString -> a)
  | GetProgramLinkStatus WebGLProgram (Boolean -> a)
  | GetShaderCompileStatus WebGLShader (Boolean -> a)
  | GetShaderInfoLog WebGLShader (Maybe DOMString -> a)
  | GetUniformLocation WebGLProgram DOMString (Maybe WebGLUniformLocation -> a)
  | LinkProgram WebGLProgram a
  | ProgramOperation WebGLProgram (WebGLProgramOperation Unit) a
  | ShaderSource WebGLShader DOMString a
  | Viewport GLint GLint GLsizei GLsizei a

instance functorWebGLF :: Functor WebGLF where
  map f (AttachShader p s x) = AttachShader p s (f x)
  map f (Clear mask x) = Clear mask (f x)
  map f (ClearColor r g b a x) = ClearColor r g b a (f x)
  map f (CompileShader s x) = CompileShader s (f x)
  map f (CreateBuffer k) = CreateBuffer (f <<< k)
  map f (CreateProgram k) = CreateProgram (f <<< k)
  map f (CreateShader t k) = CreateShader t (f <<< k)
  map f (DepthFunc func x) = DepthFunc func (f x)
  map f (Enable cap x) = Enable cap (f x)
  map f (GetAttribLocation p s k) = GetAttribLocation p s (f <<< k)
  map f (GetDrawingBufferHeight k) = GetDrawingBufferHeight (f <<< k)
  map f (GetDrawingBufferWidth k) = GetDrawingBufferWidth (f <<< k)
  map f (GetProgramInfoLog s k) = GetProgramInfoLog s (f <<< k)
  map f (GetProgramLinkStatus p k) = GetProgramLinkStatus p (f <<< k)
  map f (GetShaderCompileStatus s k) = GetShaderCompileStatus s (f <<< k)
  map f (GetShaderInfoLog s k) = GetShaderInfoLog s (f <<< k)
  map f (GetUniformLocation p n k) = GetUniformLocation p n (f <<< k)
  map f (LinkProgram p x) = LinkProgram p (f x)
  map f (ProgramOperation p operation x) = ProgramOperation p operation (f x)
  map f (ShaderSource sh src x) = ShaderSource sh src (f x)
  map f (Viewport x y w h a) = Viewport x y w h (f a)

-- | Free monad for WebGL computations
type WebGL = Free WebGLF

data WebGLProgramOperationF a
  = ArrayBufferOperation WebGLBuffer (WebGLArrayBufferOperation Unit) a
  | ElementArrayBufferOperation WebGLBuffer (WebGLElementArrayBufferOperation Unit) a
  | EnableVertexAttribArray AttributeLocation a
  | UniformMatrix4fv WebGLUniformLocation GLboolean A.Float32Array a

instance functorWebGLProgramOperationF :: Functor WebGLProgramOperationF where
  map f (ArrayBufferOperation buffer operation x) = ArrayBufferOperation buffer operation (f x)
  map f (ElementArrayBufferOperation buffer operation x) = ElementArrayBufferOperation buffer operation (f x)
  map f (EnableVertexAttribArray i x) = EnableVertexAttribArray i (f x)
  map f (UniformMatrix4fv l t v x) = UniformMatrix4fv l t v (f x)

-- | Free monad for WebGL computations using a shader program
type WebGLProgramOperation = Free WebGLProgramOperationF

data WebGLArrayBufferOperationF a
  = BufferInt8ArrayData GLenum A.Int8Array GLenum a
  | BufferInt16ArrayData GLenum A.Int16Array GLenum a
  | BufferInt32ArrayData GLenum A.Int32Array GLenum a
  | BufferUint8ArrayData GLenum A.Uint8Array GLenum a
  | BufferUint16ArrayData GLenum A.Uint16Array GLenum a
  | BufferUint32ArrayData GLenum A.Uint32Array GLenum a
  | BufferUint8ClampedArrayData GLenum A.Uint8ClampedArray GLenum a
  | BufferFloat32ArrayData GLenum A.Float32Array GLenum a
  | BufferFloat64ArrayData GLenum A.Float64Array GLenum a
  | DrawArrays GLenum GLint GLsizei a
  | VertexAttribPointer AttributeLocation GLint GLenum GLboolean GLsizei GLintptr a

instance functorWebGLArrayBufferOperationF :: Functor WebGLArrayBufferOperationF where
  map f (BufferInt8ArrayData t s u x) = BufferInt8ArrayData t s u (f x)
  map f (BufferInt16ArrayData t s u x) = BufferInt16ArrayData t s u (f x)
  map f (BufferInt32ArrayData t s u x) = BufferInt32ArrayData t s u (f x)
  map f (BufferUint8ArrayData t s u x) = BufferUint8ArrayData t s u (f x)
  map f (BufferUint16ArrayData t s u x) = BufferUint16ArrayData t s u (f x)
  map f (BufferUint32ArrayData t s u x) = BufferUint32ArrayData t s u (f x)
  map f (BufferUint8ClampedArrayData t s u x) = BufferUint8ClampedArrayData t s u (f x)
  map f (BufferFloat32ArrayData t s u x) = BufferFloat32ArrayData t s u (f x)
  map f (BufferFloat64ArrayData t s u x) = BufferFloat64ArrayData t s u (f x)
  map f (DrawArrays mode first count x) = DrawArrays mode first count (f x)
  map f (VertexAttribPointer l s t n str o x) = VertexAttribPointer l s t n str o (f x)

-- | Free monad for WebGL computations using an array buffer
type WebGLArrayBufferOperation = Free WebGLArrayBufferOperationF

data WebGLElementArrayBufferOperationF a
  = BufferInt8ElementData GLenum A.Int8Array GLenum a
  | BufferInt16ElementData GLenum A.Int16Array GLenum a
  | BufferInt32ElementData GLenum A.Int32Array GLenum a
  | BufferUint8ElementData GLenum A.Uint8Array GLenum a
  | BufferUint16ElementData GLenum A.Uint16Array GLenum a
  | BufferUint32ElementData GLenum A.Uint32Array GLenum a
  | BufferUint8ClampedElementData GLenum A.Uint8ClampedArray GLenum a
  | BufferFloat32ElementData GLenum A.Float32Array GLenum a
  | BufferFloat64ElementData GLenum A.Float64Array GLenum a
  | DrawElements GLenum GLsizei GLenum GLintptr a

instance functorWebGLElementArrayBufferOperationF :: Functor WebGLElementArrayBufferOperationF where
  map f (BufferInt8ElementData t s u x) = BufferInt8ElementData t s u (f x)
  map f (BufferInt16ElementData t s u x) = BufferInt16ElementData t s u (f x)
  map f (BufferInt32ElementData t s u x) = BufferInt32ElementData t s u (f x)
  map f (BufferUint8ElementData t s u x) = BufferUint8ElementData t s u (f x)
  map f (BufferUint16ElementData t s u x) = BufferUint16ElementData t s u (f x)
  map f (BufferUint32ElementData t s u x) = BufferUint32ElementData t s u (f x)
  map f (BufferUint8ClampedElementData t s u x) = BufferUint8ClampedElementData t s u (f x)
  map f (BufferFloat32ElementData t s u x) = BufferFloat32ElementData t s u (f x)
  map f (BufferFloat64ElementData t s u x) = BufferFloat64ElementData t s u (f x)
  map f (DrawElements mode count t offset x) = DrawElements mode count t offset (f x)

-- | Free monad for WebGL computations using an element array buffer
type WebGLElementArrayBufferOperation = Free WebGLElementArrayBufferOperationF


-- WebGL functions

attachShader :: WebGLProgram -> WebGLShader -> WebGL Unit
attachShader p s = liftF $ AttachShader p s unit

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

enable :: GLenum -> WebGL Unit
enable cap = liftF $ Enable cap unit

getAttribLocation :: WebGLProgram -> DOMString -> WebGL (Maybe AttributeLocation)
getAttribLocation program name = liftF $ GetAttribLocation program name id

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

programOperation :: WebGLProgram -> WebGLProgramOperation Unit -> WebGL Unit
programOperation program operation = liftF $ ProgramOperation program operation unit

shaderSource :: WebGLShader -> DOMString -> WebGL Unit
shaderSource sh src = liftF $ ShaderSource sh src unit

viewport :: GLint -> GLint -> GLsizei -> GLsizei -> WebGL Unit
viewport x y w h = liftF $ Viewport x y w h unit


-- WebGL functions using a shader program

arrayBufferOperation :: WebGLBuffer -> WebGLArrayBufferOperation Unit -> WebGLProgramOperation Unit
arrayBufferOperation buffer operation = liftF $ ArrayBufferOperation buffer operation unit

elementArrayBufferOperation :: WebGLBuffer -> WebGLElementArrayBufferOperation Unit -> WebGLProgramOperation Unit
elementArrayBufferOperation buffer operation = liftF $ ElementArrayBufferOperation buffer operation unit

enableVertexAttribArray :: AttributeLocation -> WebGLProgramOperation Unit
enableVertexAttribArray location = liftF $ EnableVertexAttribArray location unit

uniformMatrix4fv :: WebGLUniformLocation -> GLboolean -> A.Float32Array -> WebGLProgramOperation Unit
uniformMatrix4fv location transpose value = liftF $ UniformMatrix4fv location transpose value unit


-- | This type class allows both array buffers and element array buffers to support the buffer data actions
class BufferData f where
  bufferInt8Data :: GLenum -> A.Int8Array -> GLenum -> f Unit
  bufferInt16Data :: GLenum -> A.Int16Array -> GLenum -> f Unit
  bufferInt32Data :: GLenum -> A.Int32Array -> GLenum -> f Unit
  bufferUint8Data :: GLenum -> A.Uint8Array -> GLenum -> f Unit
  bufferUint16Data :: GLenum -> A.Uint16Array -> GLenum -> f Unit
  bufferUint32Data :: GLenum -> A.Uint32Array -> GLenum -> f Unit
  bufferUint8ClampedData :: GLenum -> A.Uint8ClampedArray -> GLenum -> f Unit
  bufferFloat32Data :: GLenum -> A.Float32Array -> GLenum -> f Unit
  bufferFloat64Data :: GLenum -> A.Float64Array -> GLenum -> f Unit


-- WebGL functions using a shader program and an array buffer

drawArrays :: GLenum -> GLint -> GLsizei -> WebGLArrayBufferOperation Unit
drawArrays mode first count = liftF $ DrawArrays mode first count unit

vertexAttribPointer :: AttributeLocation -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> WebGLArrayBufferOperation Unit
vertexAttribPointer l s t n str o = liftF $ VertexAttribPointer l s t n str o unit

instance bufferDataArray :: BufferData (Free WebGLArrayBufferOperationF) where
  bufferInt8Data t s u = liftF $ BufferInt8ArrayData t s u unit
  bufferInt16Data t s u = liftF $ BufferInt16ArrayData t s u unit
  bufferInt32Data t s u = liftF $ BufferInt32ArrayData t s u unit
  bufferUint8Data t s u = liftF $ BufferUint8ArrayData t s u unit
  bufferUint16Data t s u = liftF $ BufferUint16ArrayData t s u unit
  bufferUint32Data t s u = liftF $ BufferUint32ArrayData t s u unit
  bufferUint8ClampedData t s u = liftF $ BufferUint8ClampedArrayData t s u unit
  bufferFloat32Data t s u = liftF $ BufferFloat32ArrayData t s u unit
  bufferFloat64Data t s u = liftF $ BufferFloat64ArrayData t s u unit


-- WebGL functions using a shader program and an element array buffer

drawElements :: GLenum -> GLsizei -> GLenum -> GLintptr -> WebGLElementArrayBufferOperation Unit
drawElements mode count t offset = liftF $ DrawElements mode count t offset unit

instance bufferDataElementArray :: BufferData (Free WebGLElementArrayBufferOperationF) where
  bufferInt8Data t s u = liftF $ BufferInt8ElementData t s u unit
  bufferInt16Data t s u = liftF $ BufferInt16ElementData t s u unit
  bufferInt32Data t s u = liftF $ BufferInt32ElementData t s u unit
  bufferUint8Data t s u = liftF $ BufferUint8ElementData t s u unit
  bufferUint16Data t s u = liftF $ BufferUint16ElementData t s u unit
  bufferUint32Data t s u = liftF $ BufferUint32ElementData t s u unit
  bufferUint8ClampedData t s u = liftF $ BufferUint8ClampedElementData t s u unit
  bufferFloat32Data t s u = liftF $ BufferFloat32ElementData t s u unit
  bufferFloat64Data t s u = liftF $ BufferFloat64ElementData t s u unit


-- Interpret the WebGL functor as actions from the WebGL raw module
interpretWebGL :: forall a eff. WebGLContext -> WebGLF (WebGL a) -> Eff (canvas :: Canvas | eff) (WebGL a)
interpretWebGL gl (AttachShader p s rest) = do
  R.attachShader gl p s
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
interpretWebGL gl (Enable cap rest) = do
  R.enable gl cap
  return rest
interpretWebGL gl (GetAttribLocation program name k) = do
  location <- R.getAttribLocation gl program name
  if location == -1
    then return $ k Nothing
    else return $ k $ Just (AttributeLocation location)
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
interpretWebGL gl (ProgramOperation program operation rest) = do
  R.useProgram gl program
  runFreeM (interpretWebGLProgramOperation gl) operation
  return rest 
interpretWebGL gl (ShaderSource sh src rest) = do
  R.shaderSource gl sh src
  return rest
interpretWebGL gl (Viewport x y w h rest) = do
  R.viewport gl x y w h
  return rest

-- Interpret shader program operations
interpretWebGLProgramOperation :: forall eff a. WebGLContext -> WebGLProgramOperationF (WebGLProgramOperation a) -> Eff (canvas :: Canvas | eff) (WebGLProgramOperation a)
interpretWebGLProgramOperation gl (EnableVertexAttribArray (AttributeLocation location) rest) = do
  R.enableVertexAttribArray gl location
  return rest
interpretWebGLProgramOperation gl (ArrayBufferOperation buffer operation rest) = do
  R.bindBuffer gl GL.arrayBuffer buffer
  runFreeM (interpretWebGLArrayBufferOperation gl) operation
  return rest
interpretWebGLProgramOperation gl (ElementArrayBufferOperation buffer operation rest) = do
  R.bindBuffer gl GL.elementArrayBuffer buffer
  runFreeM (interpretWebGLElementArrayBufferOperation gl) operation
  return rest
interpretWebGLProgramOperation gl (UniformMatrix4fv location transpose value rest) = do
  R.uniformMatrix4fv_ gl location transpose value
  return rest

-- Interpret array buffer operations
interpretWebGLArrayBufferOperation :: forall eff a. WebGLContext -> WebGLArrayBufferOperationF (WebGLArrayBufferOperation a) -> Eff (canvas :: Canvas | eff) (WebGLArrayBufferOperation a)
interpretWebGLArrayBufferOperation gl (BufferInt8ArrayData t source usage rest) = do
  R.bufferInt8Data gl t source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferInt16ArrayData t source usage rest) = do
  R.bufferInt16Data gl t source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferInt32ArrayData t source usage rest) = do
  R.bufferInt32Data gl t source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferUint8ArrayData t source usage rest) = do
  R.bufferUint8Data gl t source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferUint16ArrayData t source usage rest) = do
  R.bufferUint16Data gl t source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferUint32ArrayData t source usage rest) = do
  R.bufferUint32Data gl t source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferUint8ClampedArrayData t source usage rest) = do
  R.bufferUint8ClampedData gl t source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferFloat32ArrayData t source usage rest) = do
  R.bufferFloat32Data gl t source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferFloat64ArrayData t source usage rest) = do
  R.bufferFloat64Data gl t source usage
  return rest
interpretWebGLArrayBufferOperation gl (DrawArrays mode first count rest) = do
  R.drawArrays gl mode first count
  return rest
interpretWebGLArrayBufferOperation gl (VertexAttribPointer (AttributeLocation l) s t n str o rest) = do
  R.vertexAttribPointer gl l s t n o str
  return rest

-- Interpret element array buffer operations
interpretWebGLElementArrayBufferOperation :: forall eff a. WebGLContext -> WebGLElementArrayBufferOperationF (WebGLElementArrayBufferOperation a) -> Eff (canvas :: Canvas | eff) (WebGLElementArrayBufferOperation a)
interpretWebGLElementArrayBufferOperation gl (BufferInt8ElementData t source usage rest) = do
  R.bufferInt8Data gl t source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferInt16ElementData t source usage rest) = do
  R.bufferInt16Data gl t source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferInt32ElementData t source usage rest) = do
  R.bufferInt32Data gl t source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferUint8ElementData t source usage rest) = do
  R.bufferUint8Data gl t source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferUint16ElementData t source usage rest) = do
  R.bufferUint16Data gl t source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferUint32ElementData t source usage rest) = do
  R.bufferUint32Data gl t source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferUint8ClampedElementData t source usage rest) = do
  R.bufferUint8ClampedData gl t source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferFloat32ElementData t source usage rest) = do
  R.bufferFloat32Data gl t source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferFloat64ElementData t source usage rest) = do
  R.bufferFloat64Data gl t source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (DrawElements mode count t offset rest) = do
  R.drawElements gl mode count t offset
  return rest

-- | Interpret the WebGL monad as actions from the raw WebGL module
runWebGL :: forall a eff. WebGLContext -> WebGL a -> Eff (canvas :: Canvas | eff) a
runWebGL = runFreeM <<< interpretWebGL
