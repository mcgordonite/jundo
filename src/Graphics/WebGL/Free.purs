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
  AttributeLocation(),
  FragmentShader(),
  VertexShader(),
  WebGL(),
  WebGLF(..),
  WebGLArrayBufferOperation(),
  WebGLArrayBufferOperationF(..),
  WebGLElementArrayBufferOperation(),
  WebGLElementArrayBufferOperationF(..),
  WebGLProgramOperation(),
  WebGLProgramOperationF(..),
  WebGLCapability(),
  blend,
  cullFace,
  depthTest,
  polygonOffsetFill,
  scissorTest,
  WebGLDepthComparator(),
  never,
  less,
  equal,
  lequal,
  greater,
  notequal,
  gequal,
  always,
  WebGLDrawMode(),
  points,
  lines,
  lineStrip,
  lineLoop,
  triangles,
  triangleStrip,
  triangleFan,
  WebGLBufferUsage(),
  staticDraw,
  dynamicDraw,
  streamDraw,
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
  WebGLTypedShader,
  attachShader,
  compileShader,
  createShader,
  getShaderCompileStatus,
  getShaderInfoLog,
  setShaderSource,
  colorBufferBit,
  depthBufferBit,
  stencilBufferBit,
  runWebGL,
  arrayBufferOperation,
  clear,
  createBuffer,
  clearColor,
  createProgram,
  depthFunc,
  disable,
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
  getUniformLocation,
  linkProgram,
  programOperation,
  uniformMatrix4fv,
  vertexAttribPointer,
  viewport,
  module Graphics.WebGL.Raw.Types
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

-- | A type representing a WebGL fragment shader, since they are different to vertex shaders
newtype FragmentShader = FragmentShader WebGLShader

-- | A type representing a WebGL vertex shader, since they are different to fragment shaders
newtype VertexShader = VertexShader WebGLShader

-- | A type representing capability enums that can be passed to enable and disable
newtype WebGLCapability = WebGLCapability GLenum
blend = WebGLCapability GL.blend
cullFace = WebGLCapability GL.cullFace
depthTest = WebGLCapability GL.depthTest
polygonOffsetFill = WebGLCapability GL.polygonOffsetFill
scissorTest = WebGLCapability GL.scissorTest

-- | Comparator enums that can be passed to depthFunc
newtype WebGLDepthComparator = WebGLDepthComparator GLenum
never = WebGLDepthComparator GL.never
less = WebGLDepthComparator GL.less
equal = WebGLDepthComparator GL.equal
lequal = WebGLDepthComparator GL.lequal
greater = WebGLDepthComparator GL.greater
notequal = WebGLDepthComparator GL.notequal
gequal = WebGLDepthComparator GL.gequal
always = WebGLDepthComparator GL.always

-- | Draw mode enums that can be passed to drawArrays and drawElements
newtype WebGLDrawMode = WebGLDrawMode GLenum
points = WebGLDrawMode GL.points
lines = WebGLDrawMode GL.lines
lineStrip = WebGLDrawMode GL.lineStrip
lineLoop = WebGLDrawMode GL.lineLoop
triangles = WebGLDrawMode GL.triangles
triangleStrip = WebGLDrawMode GL.triangleStrip
triangleFan = WebGLDrawMode GL.triangleFan

newtype WebGLBufferUsage = WebGLBufferUsage GLenum
staticDraw = WebGLBufferUsage GL.staticDraw
dynamicDraw = WebGLBufferUsage GL.dynamicDraw
streamDraw = WebGLBufferUsage GL.streamDraw


-- Re-export clear function bit fields

colorBufferBit :: GLbitfield
colorBufferBit = GL.colorBufferBit

depthBufferBit :: GLbitfield
depthBufferBit = GL.depthBufferBit

stencilBufferBit :: GLbitfield
stencilBufferBit = GL.stencilBufferBit


data WebGLF a
  = AttachFragmentShader WebGLProgram FragmentShader a
  | AttachVertexShader WebGLProgram VertexShader a
  | Clear GLbitfield a
  | ClearColor GLclampf GLclampf GLclampf GLclampf a
  | CompileFragmentShader FragmentShader a
  | CompileVertexShader VertexShader a
  | CreateBuffer (Maybe WebGLBuffer -> a)
  | CreateProgram (Maybe WebGLProgram -> a)
  | CreateFragmentShader (Maybe FragmentShader -> a)
  | CreateVertexShader (Maybe VertexShader -> a)
  | DepthFunc WebGLDepthComparator a
  | Disable WebGLCapability a
  | Enable WebGLCapability a
  | GetAttribLocation WebGLProgram DOMString (Maybe AttributeLocation -> a)
  | GetDrawingBufferHeight (Int -> a)
  | GetDrawingBufferWidth (Int -> a)
  | GetProgramInfoLog WebGLProgram (Maybe DOMString -> a)
  | GetProgramLinkStatus WebGLProgram (Boolean -> a)
  | GetFragmentShaderCompileStatus FragmentShader (Boolean -> a)
  | GetVertexShaderCompileStatus VertexShader (Boolean -> a)
  | GetFragmentShaderInfoLog FragmentShader (Maybe DOMString -> a)
  | GetVertexShaderInfoLog VertexShader (Maybe DOMString -> a)
  | GetUniformLocation WebGLProgram DOMString (Maybe WebGLUniformLocation -> a)
  | LinkProgram WebGLProgram a
  | ProgramOperation WebGLProgram (WebGLProgramOperation Unit) a
  | SetFragmentShaderSource FragmentShader DOMString a
  | SetVertexShaderSource VertexShader DOMString a
  | Viewport GLint GLint GLsizei GLsizei a

instance functorWebGLF :: Functor WebGLF where
  map f (AttachFragmentShader p s x) = AttachFragmentShader p s (f x)
  map f (AttachVertexShader p s x) = AttachVertexShader p s (f x)
  map f (Clear mask x) = Clear mask (f x)
  map f (ClearColor r g b a x) = ClearColor r g b a (f x)
  map f (CompileFragmentShader s x) = CompileFragmentShader s (f x)
  map f (CompileVertexShader s x) = CompileVertexShader s (f x)
  map f (CreateBuffer k) = CreateBuffer (f <<< k)
  map f (CreateProgram k) = CreateProgram (f <<< k)
  map f (CreateFragmentShader k) = CreateFragmentShader (f <<< k)
  map f (CreateVertexShader k) = CreateVertexShader (f <<< k)
  map f (DepthFunc comparator x) = DepthFunc comparator (f x)
  map f (Disable capability x) = Enable capability (f x)
  map f (Enable capability x) = Enable capability (f x)
  map f (GetAttribLocation p s k) = GetAttribLocation p s (f <<< k)
  map f (GetDrawingBufferHeight k) = GetDrawingBufferHeight (f <<< k)
  map f (GetDrawingBufferWidth k) = GetDrawingBufferWidth (f <<< k)
  map f (GetProgramInfoLog s k) = GetProgramInfoLog s (f <<< k)
  map f (GetProgramLinkStatus p k) = GetProgramLinkStatus p (f <<< k)
  map f (GetFragmentShaderCompileStatus s k) = GetFragmentShaderCompileStatus s (f <<< k)
  map f (GetVertexShaderCompileStatus s k) = GetVertexShaderCompileStatus s (f <<< k)
  map f (GetFragmentShaderInfoLog s k) = GetFragmentShaderInfoLog s (f <<< k)
  map f (GetVertexShaderInfoLog s k) = GetVertexShaderInfoLog s (f <<< k)
  map f (GetUniformLocation p n k) = GetUniformLocation p n (f <<< k)
  map f (LinkProgram p x) = LinkProgram p (f x)
  map f (ProgramOperation p operation x) = ProgramOperation p operation (f x)
  map f (SetFragmentShaderSource sh src x) = SetFragmentShaderSource sh src (f x)
  map f (SetVertexShaderSource sh src x) = SetVertexShaderSource sh src (f x)
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
  = BufferInt8ArrayData A.Int8Array WebGLBufferUsage a
  | BufferInt16ArrayData A.Int16Array WebGLBufferUsage a
  | BufferInt32ArrayData A.Int32Array WebGLBufferUsage a
  | BufferUint8ArrayData A.Uint8Array WebGLBufferUsage a
  | BufferUint16ArrayData A.Uint16Array WebGLBufferUsage a
  | BufferUint32ArrayData A.Uint32Array WebGLBufferUsage a
  | BufferUint8ClampedArrayData A.Uint8ClampedArray WebGLBufferUsage a
  | BufferFloat32ArrayData A.Float32Array WebGLBufferUsage a
  | BufferFloat64ArrayData A.Float64Array WebGLBufferUsage a
  | DrawArrays WebGLDrawMode GLint GLsizei a
  | VertexAttribPointer AttributeLocation GLint GLboolean GLsizei GLintptr a

instance functorWebGLArrayBufferOperationF :: Functor WebGLArrayBufferOperationF where
  map f (BufferInt8ArrayData size usage x) = BufferInt8ArrayData size usage (f x)
  map f (BufferInt16ArrayData size usage x) = BufferInt16ArrayData size usage (f x)
  map f (BufferInt32ArrayData size usage x) = BufferInt32ArrayData size usage (f x)
  map f (BufferUint8ArrayData size usage x) = BufferUint8ArrayData size usage (f x)
  map f (BufferUint16ArrayData size usage x) = BufferUint16ArrayData size usage (f x)
  map f (BufferUint32ArrayData size usage x) = BufferUint32ArrayData size usage (f x)
  map f (BufferUint8ClampedArrayData size usage x) = BufferUint8ClampedArrayData size usage (f x)
  map f (BufferFloat32ArrayData size usage x) = BufferFloat32ArrayData size usage (f x)
  map f (BufferFloat64ArrayData size usage x) = BufferFloat64ArrayData size usage (f x)
  map f (DrawArrays mode first count x) = DrawArrays mode first count (f x)
  map f (VertexAttribPointer location size normalized stride offset x) = VertexAttribPointer location size normalized stride offset (f x)

-- | Free monad for WebGL computations using an array buffer
type WebGLArrayBufferOperation = Free WebGLArrayBufferOperationF

data WebGLElementArrayBufferOperationF a
  = BufferInt8ElementData A.Int8Array WebGLBufferUsage a
  | BufferInt16ElementData A.Int16Array WebGLBufferUsage a
  | BufferInt32ElementData A.Int32Array WebGLBufferUsage a
  | BufferUint8ElementData A.Uint8Array WebGLBufferUsage a
  | BufferUint16ElementData A.Uint16Array WebGLBufferUsage a
  | BufferUint32ElementData A.Uint32Array WebGLBufferUsage a
  | BufferUint8ClampedElementData A.Uint8ClampedArray WebGLBufferUsage a
  | BufferFloat32ElementData A.Float32Array WebGLBufferUsage a
  | BufferFloat64ElementData A.Float64Array WebGLBufferUsage a
  | DrawElements WebGLDrawMode GLsizei GLintptr a

instance functorWebGLElementArrayBufferOperationF :: Functor WebGLElementArrayBufferOperationF where
  map f (BufferInt8ElementData size usage x) = BufferInt8ElementData size usage (f x)
  map f (BufferInt16ElementData size usage x) = BufferInt16ElementData size usage (f x)
  map f (BufferInt32ElementData size usage x) = BufferInt32ElementData size usage (f x)
  map f (BufferUint8ElementData size usage x) = BufferUint8ElementData size usage (f x)
  map f (BufferUint16ElementData size usage x) = BufferUint16ElementData size usage (f x)
  map f (BufferUint32ElementData size usage x) = BufferUint32ElementData size usage (f x)
  map f (BufferUint8ClampedElementData size usage x) = BufferUint8ClampedElementData size usage (f x)
  map f (BufferFloat32ElementData size usage x) = BufferFloat32ElementData size usage (f x)
  map f (BufferFloat64ElementData size usage x) = BufferFloat64ElementData size usage (f x)
  map f (DrawElements mode count offset x) = DrawElements mode count offset (f x)

-- | Free monad for WebGL computations using an element array buffer
type WebGLElementArrayBufferOperation = Free WebGLElementArrayBufferOperationF


-- WebGL functions

clear :: GLbitfield -> WebGL Unit
clear mask = liftF $ Clear mask unit

clearColor :: GLclampf -> GLclampf -> GLclampf -> GLclampf -> WebGL Unit
clearColor r g b a = liftF $ ClearColor r g b a unit

createBuffer :: WebGL (Maybe WebGLBuffer)
createBuffer = liftF $ CreateBuffer id

createProgram :: WebGL (Maybe WebGLProgram)
createProgram = liftF $ CreateProgram id

depthFunc :: WebGLDepthComparator -> WebGL Unit
depthFunc comparator = liftF $ DepthFunc comparator unit

disable :: WebGLCapability -> WebGL Unit
disable capability = liftF $ Enable capability unit

enable :: WebGLCapability -> WebGL Unit
enable capability = liftF $ Enable capability unit

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

getUniformLocation :: WebGLProgram -> DOMString -> WebGL (Maybe WebGLUniformLocation)
getUniformLocation program name = liftF $ GetUniformLocation program name id

linkProgram :: WebGLProgram -> WebGL Unit
linkProgram p = liftF $ LinkProgram p unit

programOperation :: WebGLProgram -> WebGLProgramOperation Unit -> WebGL Unit
programOperation program operation = liftF $ ProgramOperation program operation unit

viewport :: GLint -> GLint -> GLsizei -> GLsizei -> WebGL Unit
viewport x y w h = liftF $ Viewport x y w h unit

-- | This type class allows fragment and vertex shader polymorphism
class WebGLTypedShader s where
  attachShader :: WebGLProgram -> s -> WebGL Unit
  compileShader :: s -> WebGL Unit
  createShader :: WebGL (Maybe s)
  getShaderCompileStatus :: s -> WebGL Boolean
  getShaderInfoLog :: s -> WebGL (Maybe DOMString)
  setShaderSource :: s -> DOMString -> WebGL Unit

instance fragmentTypedShader :: WebGLTypedShader FragmentShader where
  attachShader program shader = liftF $ AttachFragmentShader program shader unit
  compileShader shader = liftF $ CompileFragmentShader shader unit
  createShader = liftF $ CreateFragmentShader id
  getShaderCompileStatus shader = liftF $ GetFragmentShaderCompileStatus shader id
  getShaderInfoLog shader = liftF $ GetFragmentShaderInfoLog shader id
  setShaderSource shader source = liftF $ SetFragmentShaderSource shader source unit

instance vertexTypedShader :: WebGLTypedShader VertexShader where
  attachShader program shader = liftF $ AttachVertexShader program shader unit
  compileShader shader = liftF $ CompileVertexShader shader unit
  createShader = liftF $ CreateVertexShader id
  getShaderCompileStatus shader = liftF $ GetVertexShaderCompileStatus shader id
  getShaderInfoLog shader = liftF $ GetVertexShaderInfoLog shader id
  setShaderSource shader source = liftF $ SetVertexShaderSource shader source unit


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
  bufferInt8Data :: A.Int8Array -> WebGLBufferUsage -> f Unit
  bufferInt16Data :: A.Int16Array -> WebGLBufferUsage -> f Unit
  bufferInt32Data :: A.Int32Array -> WebGLBufferUsage -> f Unit
  bufferUint8Data :: A.Uint8Array -> WebGLBufferUsage -> f Unit
  bufferUint16Data :: A.Uint16Array -> WebGLBufferUsage -> f Unit
  bufferUint32Data :: A.Uint32Array -> WebGLBufferUsage -> f Unit
  bufferUint8ClampedData :: A.Uint8ClampedArray -> WebGLBufferUsage -> f Unit
  bufferFloat32Data :: A.Float32Array -> WebGLBufferUsage -> f Unit
  bufferFloat64Data :: A.Float64Array -> WebGLBufferUsage -> f Unit


-- WebGL functions using a shader program and an array buffer

drawArrays :: WebGLDrawMode -> GLint -> GLsizei -> WebGLArrayBufferOperation Unit
drawArrays mode first count = liftF $ DrawArrays mode first count unit

vertexAttribPointer :: AttributeLocation -> GLint -> GLboolean -> GLsizei -> GLintptr -> WebGLArrayBufferOperation Unit
vertexAttribPointer location size normalized stride offset = liftF $ VertexAttribPointer location size normalized stride offset unit

instance bufferDataArray :: BufferData (Free WebGLArrayBufferOperationF) where
  bufferInt8Data size usage = liftF $ BufferInt8ArrayData size usage unit
  bufferInt16Data size usage = liftF $ BufferInt16ArrayData size usage unit
  bufferInt32Data size usage = liftF $ BufferInt32ArrayData size usage unit
  bufferUint8Data size usage = liftF $ BufferUint8ArrayData size usage unit
  bufferUint16Data size usage = liftF $ BufferUint16ArrayData size usage unit
  bufferUint32Data size usage = liftF $ BufferUint32ArrayData size usage unit
  bufferUint8ClampedData size usage = liftF $ BufferUint8ClampedArrayData size usage unit
  bufferFloat32Data size usage = liftF $ BufferFloat32ArrayData size usage unit
  bufferFloat64Data size usage = liftF $ BufferFloat64ArrayData size usage unit


-- WebGL functions using a shader program and an element array buffer

drawElements :: WebGLDrawMode -> GLsizei -> GLintptr -> WebGLElementArrayBufferOperation Unit
drawElements mode count offset = liftF $ DrawElements mode count offset unit

instance bufferDataElementArray :: BufferData (Free WebGLElementArrayBufferOperationF) where
  bufferInt8Data size usage = liftF $ BufferInt8ElementData size usage unit
  bufferInt16Data size usage = liftF $ BufferInt16ElementData size usage unit
  bufferInt32Data size usage = liftF $ BufferInt32ElementData size usage unit
  bufferUint8Data size usage = liftF $ BufferUint8ElementData size usage unit
  bufferUint16Data size usage = liftF $ BufferUint16ElementData size usage unit
  bufferUint32Data size usage = liftF $ BufferUint32ElementData size usage unit
  bufferUint8ClampedData size usage = liftF $ BufferUint8ClampedElementData size usage unit
  bufferFloat32Data size usage = liftF $ BufferFloat32ElementData size usage unit
  bufferFloat64Data size usage = liftF $ BufferFloat64ElementData size usage unit


-- Interpret the WebGL functor as actions from the WebGL raw module
interpretWebGL :: forall a eff. WebGLContext -> WebGLF (WebGL a) -> Eff (canvas :: Canvas | eff) (WebGL a)
interpretWebGL gl (AttachFragmentShader program (FragmentShader shader) rest) = do
  R.attachShader gl program shader
  return rest
interpretWebGL gl (AttachVertexShader program (VertexShader shader) rest) = do
  R.attachShader gl program shader
  return rest
interpretWebGL gl (Clear mask rest) = do
  R.clear gl mask
  return rest
interpretWebGL gl (ClearColor r g b a rest) = do
  R.clearColor gl r g b a
  return rest
interpretWebGL gl (CompileFragmentShader (FragmentShader shader) rest) = do
  R.compileShader gl shader
  return rest
interpretWebGL gl (CompileVertexShader (VertexShader shader) rest) = do
  R.compileShader gl shader
  return rest
interpretWebGL gl (CreateBuffer k) = do
  buffer <- R.createBuffer gl
  return $ k buffer
interpretWebGL gl (CreateProgram k) = do
  program <- R.createProgram gl
  return $ k program
interpretWebGL gl (CreateFragmentShader k) = do
  maybeShader <- R.createShader gl GL.fragmentShader
  return $ k (FragmentShader <$> maybeShader)
interpretWebGL gl (CreateVertexShader k) = do
  shader <- R.createShader gl GL.vertexShader
  return $ k (VertexShader <$> shader)
interpretWebGL gl (DepthFunc (WebGLDepthComparator comparator) rest) = do
  R.depthFunc gl comparator
  return rest
interpretWebGL gl (Disable (WebGLCapability capability) rest) = do
  R.enable gl capability
  return rest
interpretWebGL gl (Enable (WebGLCapability capability) rest) = do
  R.enable gl capability
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
interpretWebGL gl (GetFragmentShaderCompileStatus (FragmentShader shader) k) = do
  Just b <- R.getShaderParameter gl shader GL.compileStatus
  return $ k b
interpretWebGL gl (GetVertexShaderCompileStatus (VertexShader shader) k) = do
  Just b <- R.getShaderParameter gl shader GL.compileStatus
  return $ k b
interpretWebGL gl (GetFragmentShaderInfoLog (FragmentShader shader) k) = do
  maybeLog <- R.getShaderInfoLog gl shader
  return $ k maybeLog
interpretWebGL gl (GetVertexShaderInfoLog (VertexShader shader) k) = do
  maybeLog <- R.getShaderInfoLog gl shader
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
interpretWebGL gl (SetFragmentShaderSource (FragmentShader shader) source rest) = do
  R.shaderSource gl shader source
  return rest
interpretWebGL gl (SetVertexShaderSource (VertexShader shader) source rest) = do
  R.shaderSource gl shader source
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
interpretWebGLArrayBufferOperation gl (BufferInt8ArrayData source (WebGLBufferUsage usage) rest) = do
  R.bufferInt8Data gl GL.arrayBuffer source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferInt16ArrayData source (WebGLBufferUsage usage) rest) = do
  R.bufferInt16Data gl GL.arrayBuffer source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferInt32ArrayData source (WebGLBufferUsage usage) rest) = do
  R.bufferInt32Data gl GL.arrayBuffer source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferUint8ArrayData source (WebGLBufferUsage usage) rest) = do
  R.bufferUint8Data gl GL.arrayBuffer source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferUint16ArrayData source (WebGLBufferUsage usage) rest) = do
  R.bufferUint16Data gl GL.arrayBuffer source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferUint32ArrayData source (WebGLBufferUsage usage) rest) = do
  R.bufferUint32Data gl GL.arrayBuffer source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferUint8ClampedArrayData source (WebGLBufferUsage usage) rest) = do
  R.bufferUint8ClampedData gl GL.arrayBuffer source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferFloat32ArrayData source (WebGLBufferUsage usage) rest) = do
  R.bufferFloat32Data gl GL.arrayBuffer source usage
  return rest
interpretWebGLArrayBufferOperation gl (BufferFloat64ArrayData source (WebGLBufferUsage usage) rest) = do
  R.bufferFloat64Data gl GL.arrayBuffer source usage
  return rest
interpretWebGLArrayBufferOperation gl (DrawArrays (WebGLDrawMode mode) first count rest) = do
  R.drawArrays gl mode first count
  return rest
interpretWebGLArrayBufferOperation gl (VertexAttribPointer (AttributeLocation index) size normalized stride offset rest) = do
  R.vertexAttribPointer gl index size GL.float normalized stride offset
  return rest

-- Interpret element array buffer operations
interpretWebGLElementArrayBufferOperation :: forall eff a. WebGLContext -> WebGLElementArrayBufferOperationF (WebGLElementArrayBufferOperation a) -> Eff (canvas :: Canvas | eff) (WebGLElementArrayBufferOperation a)
interpretWebGLElementArrayBufferOperation gl (BufferInt8ElementData source (WebGLBufferUsage usage) rest) = do
  R.bufferInt8Data gl GL.elementArrayBuffer source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferInt16ElementData source (WebGLBufferUsage usage) rest) = do
  R.bufferInt16Data gl GL.elementArrayBuffer source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferInt32ElementData source (WebGLBufferUsage usage) rest) = do
  R.bufferInt32Data gl GL.elementArrayBuffer source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferUint8ElementData source (WebGLBufferUsage usage) rest) = do
  R.bufferUint8Data gl GL.elementArrayBuffer source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferUint16ElementData source (WebGLBufferUsage usage) rest) = do
  R.bufferUint16Data gl GL.elementArrayBuffer source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferUint32ElementData source (WebGLBufferUsage usage) rest) = do
  R.bufferUint32Data gl GL.elementArrayBuffer source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferUint8ClampedElementData source (WebGLBufferUsage usage) rest) = do
  R.bufferUint8ClampedData gl GL.elementArrayBuffer source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferFloat32ElementData source (WebGLBufferUsage usage) rest) = do
  R.bufferFloat32Data gl GL.elementArrayBuffer source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (BufferFloat64ElementData source (WebGLBufferUsage usage) rest) = do
  R.bufferFloat64Data gl GL.elementArrayBuffer source usage
  return rest
interpretWebGLElementArrayBufferOperation gl (DrawElements (WebGLDrawMode mode) count offset rest) = do
  R.drawElements gl mode count GL.unsignedShort offset
  return rest

-- | Interpret the WebGL monad as actions from the raw WebGL module
runWebGL :: forall a eff. WebGLContext -> WebGL a -> Eff (canvas :: Canvas | eff) a
runWebGL = runFreeM <<< interpretWebGL
