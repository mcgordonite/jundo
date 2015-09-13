-- Free Monad for WebGL
module Graphics.WebGL.Free (
	WebGL(),
	WebGLF(),
	runWebGL,
	attachShader,
	clear,
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
import Graphics.WebGL.Raw.Types

data WebGLF a
	= AttachShader WebGLProgram WebGLShader a
	| Clear GLbitfield a
	| ClearColor GLclampf GLclampf GLclampf GLclampf a
	| CompileShader WebGLShader a
	| CreateProgram (Maybe WebGLProgram -> a)
	| CreateShader GLenum (Maybe WebGLShader -> a)
	| DepthFunc GLenum a
	| Enable GLenum a
	| EnableVertexAttribArray GLuint a
	| GetAttribLocation WebGLProgram String (GLuint -> a)
	| GetCanvas (CanvasElement -> a)
	| GetDrawingBufferHeight (Int -> a)
	| GetDrawingBufferWidth (Int -> a)
	| LinkProgram WebGLProgram a
	| ShaderSource WebGLShader String a
	| UseProgram WebGLProgram a
	| Viewport GLint GLint GLsizei GLsizei a

instance functorWebGLF :: Functor WebGLF where
	map f (AttachShader p s x) = AttachShader p s (f x)
	map f (Clear mask x) = Clear mask (f x)
	map f (ClearColor r g b a x) = ClearColor r g b a (f x)
	map f (CompileShader s x) = CompileShader s (f x)
	map f (CreateProgram k) = CreateProgram (f <<< k)
	map f (CreateShader t k) = CreateShader t (f <<< k)
	map f (DepthFunc func x) = DepthFunc func (f x)
	map f (Enable cap x) = Enable cap (f x)
	map f (EnableVertexAttribArray i x) = EnableVertexAttribArray i (f x)
	map f (GetAttribLocation p s k) = GetAttribLocation p s (f <<< k)
	map f (GetCanvas k) = GetCanvas (f <<< k)
	map f (GetDrawingBufferHeight k) = GetDrawingBufferHeight (f <<< k)
	map f (GetDrawingBufferWidth k) = GetDrawingBufferWidth (f <<< k)
	map f (LinkProgram p x) = LinkProgram p (f x)
	map f (ShaderSource sh src x) = ShaderSource sh src (f x)
	map f (UseProgram p x) = UseProgram p (f x)
	map f (Viewport x y w h a) = Viewport x y w h (f a)

type WebGL = Free WebGLF

attachShader :: WebGLProgram -> WebGLShader -> WebGL Unit
attachShader p s = liftF $ AttachShader p s unit

clear :: GLbitfield -> WebGL Unit
clear mask = liftF $ Clear mask unit

clearColor :: GLclampf -> GLclampf -> GLclampf -> GLclampf -> WebGL Unit
clearColor r g b a = liftF $ ClearColor r g b a unit

compileShader :: WebGLShader -> WebGL Unit
compileShader s = liftF $ CompileShader s unit

createProgram :: WebGL (Maybe WebGLProgram)
createProgram = liftF $ CreateProgram id

createShader :: GLenum -> WebGL (Maybe WebGLShader)
createShader t = liftF $ CreateShader t id

depthFunc :: GLenum -> WebGL Unit
depthFunc func = liftF $ DepthFunc func unit

enable :: GLenum -> WebGL Unit
enable cap = liftF $ Enable cap unit

enableVertexAttribArray :: GLuint -> WebGL Unit
enableVertexAttribArray index = liftF $ EnableVertexAttribArray index unit

getAttribLocation :: WebGLProgram -> String -> WebGL GLuint
getAttribLocation program name = liftF $ GetAttribLocation program name id

getCanvas :: WebGL CanvasElement
getCanvas = liftF $ GetCanvas id

getDrawingBufferHeight :: WebGL Int
getDrawingBufferHeight = liftF $ GetDrawingBufferHeight id

getDrawingBufferWidth :: WebGL Int
getDrawingBufferWidth = liftF $ GetDrawingBufferWidth id

linkProgram :: WebGLProgram -> WebGL Unit
linkProgram p = liftF $ LinkProgram p unit

shaderSource :: WebGLShader -> String -> WebGL Unit
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
	interpret (Clear mask rest) = do
		R.clear gl mask
		return rest
	interpret (ClearColor r g b a rest) = do
		R.clearColor gl r g b a
		return rest
	interpret (CompileShader sh rest) = do
		R.compileShader gl sh
		return rest
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
	interpret (EnableVertexAttribArray index rest) = do
		R.enableVertexAttribArray gl index
		return rest
	interpret (GetAttribLocation program name k) = do
		index <- R.getAttribLocation gl program name
		return $ k index
	interpret (GetCanvas k) = do
		el <- R.getCanvas gl
		return $ k el
	interpret (GetDrawingBufferHeight k) = do
		h <- R.getDrawingBufferHeight gl
		return $ k h
	interpret (GetDrawingBufferWidth k) = do
		w <- R.getDrawingBufferWidth gl
		return $ k w
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
