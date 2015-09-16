-- Free Monad for WebGL
module Graphics.WebGL.Free (
	AttributeLocation(..),
	WebGL(),
	WebGLF(),
	runWebGL,
	attachShader,
	bindBuffer,
	bufferData,
	clear,
	createBuffer,
	clearColor,
	createProgram,
	compileShader,
	createShader,
	depthFunc,
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
	useProgram,
	viewport
	) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Free
import Data.Maybe
import Graphics.Canvas (Canvas(), CanvasElement())
import qualified Graphics.WebGL.Raw as R
import qualified Graphics.WebGL.Raw.Extra as R
import qualified Graphics.WebGL.Raw.Enums as GL
import Graphics.WebGL.Raw.Types

newtype AttributeLocation = AttributeLocation GLuint

data WebGLF a
	= AttachShader WebGLProgram WebGLShader a
	| BindBuffer GLenum WebGLBuffer a
	| BufferData GLenum BufferDataSource GLenum a
	| Clear GLbitfield a
	| ClearColor GLclampf GLclampf GLclampf GLclampf a
	| CompileShader WebGLShader a
	| CreateBuffer (Maybe WebGLBuffer -> a)
	| CreateProgram (Maybe WebGLProgram -> a)
	| CreateShader GLenum (Maybe WebGLShader -> a)
	| DepthFunc GLenum a
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
	| UseProgram WebGLProgram a
	| Viewport GLint GLint GLsizei GLsizei a

instance functorWebGLF :: Functor WebGLF where
	map f (AttachShader p s x) = AttachShader p s (f x)
	map f (BindBuffer t b x) = BindBuffer t b (f x)
	map f (BufferData t s u x) = BufferData t s u (f x)
	map f (Clear mask x) = Clear mask (f x)
	map f (ClearColor r g b a x) = ClearColor r g b a (f x)
	map f (CompileShader s x) = CompileShader s (f x)
	map f (CreateBuffer k) = CreateBuffer (f <<< k)
	map f (CreateProgram k) = CreateProgram (f <<< k)
	map f (CreateShader t k) = CreateShader t (f <<< k)
	map f (DepthFunc func x) = DepthFunc func (f x)
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
	map f (UseProgram p x) = UseProgram p (f x)
	map f (Viewport x y w h a) = Viewport x y w h (f a)

type WebGL = Free WebGLF

attachShader :: WebGLProgram -> WebGLShader -> WebGL Unit
attachShader p s = liftF $ AttachShader p s unit

bindBuffer :: GLenum -> WebGLBuffer -> WebGL Unit
bindBuffer t b = liftF $ BindBuffer t b unit

bufferData :: GLenum -> BufferDataSource -> GLenum -> WebGL Unit
bufferData t s u = liftF $ BufferData t s u unit

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

useProgram :: WebGLProgram -> WebGL Unit
useProgram p = liftF $ UseProgram p unit

viewport :: GLint -> GLint -> GLsizei -> GLsizei -> WebGL Unit
viewport x y w h = liftF $ Viewport x y w h unit

runWebGL :: forall a eff. WebGLContext -> WebGL a -> Eff (canvas :: Canvas | eff) a
runWebGL gl = runFreeM interpret
	where
	interpret :: forall a. WebGLF (WebGL a) -> Eff (canvas :: Canvas | eff) (WebGL a)
	interpret (AttachShader p s rest) = do
		R.attachShader gl p s
		return rest
	interpret (BindBuffer t buffer rest) = do
		R.bindBuffer gl t buffer
		return rest
	interpret (BufferData t source usage rest) = do
		R.bufferData gl t source usage
		return rest
	interpret (Clear mask rest) = do
		R.clear gl mask
		return rest
	interpret (ClearColor r g b a rest) = do
		R.clearColor gl r g b a
		return rest
	interpret (CompileShader sh rest) = do
		R.compileShader gl sh
		return rest
	interpret (CreateBuffer k) = do
		buffer <- R.createBuffer gl
		return $ k buffer
	interpret (CreateProgram k) = do
		program <- R.createProgram gl
		return $ k program
	interpret (CreateShader t k) = do
		shader <- R.createShader gl t
		return $ k shader
	interpret (DepthFunc func rest) = do
		R.depthFunc gl func
		return rest
	interpret (Enable cap rest) = do
		R.enable gl cap
		return rest
	interpret (EnableVertexAttribArray (AttributeLocation location) rest) = do
		R.enableVertexAttribArray gl location
		return rest
	interpret (GetAttribLocation program name k) = do
		location <- R.getAttribLocation gl program name
		if location == -1
			then return $ k Nothing
			else return $ k $ Just (AttributeLocation location)
	interpret (GetCanvas k) = do
		el <- R.getCanvas gl
		return $ k el
	interpret (GetDrawingBufferHeight k) = do
		h <- R.getDrawingBufferHeight gl
		return $ k h
	interpret (GetDrawingBufferWidth k) = do
		w <- R.getDrawingBufferWidth gl
		return $ k w
	interpret (GetProgramInfoLog p k) = do
		maybeLog <- R.getProgramInfoLog gl p
		return $ k maybeLog
	interpret (GetProgramLinkStatus p k) = do
		Just b <- R.getProgramParameter gl p GL.linkStatus
		return $ k b
	interpret (GetShaderCompileStatus s k) = do
		Just b <- R.getShaderParameter gl s GL.compileStatus
		return $ k b
	interpret (GetShaderInfoLog s k) = do
		maybeLog <- R.getShaderInfoLog gl s
		return $ k maybeLog
	interpret (GetUniformLocation program name k) = do
		location <- R.getUniformLocation gl program name
		return $ k location
	interpret (LinkProgram p rest) = do
		R.linkProgram gl p
		return rest
	interpret (ShaderSource sh src rest) = do
		R.shaderSource gl sh src
		return rest
	interpret (UseProgram p rest) = do
		R.useProgram gl p
		return rest
	interpret (Viewport x y w h rest) = do
		R.viewport gl x y w h
		return rest
