-- Free Monad for WebGL
module Graphics.WebGL.Free (
	WebGL(),
	WebGLF(),
	runWebGL,
	clear,
	clearColor,
	compileShader,
	createShader,
	depthFunc,
	getCanvas,
	getDrawingBufferHeight,
	getDrawingBufferWidth,
	enable,
	shaderSource,
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
	= Clear GLbitfield a
	| ClearColor GLclampf GLclampf GLclampf GLclampf a
	| CompileShader WebGLShader a
	| CreateShader GLenum (Maybe WebGLShader -> a)
	| DepthFunc GLenum a
	| Enable GLenum a
	| GetCanvas (CanvasElement -> a)
	| GetDrawingBufferHeight (Int -> a)
	| GetDrawingBufferWidth (Int -> a)
	| ShaderSource WebGLShader String a
	| Viewport GLint GLint GLsizei GLsizei a

instance functorWebGLF :: Functor WebGLF where
	map f (Clear mask x) = Clear mask (f x)
	map f (ClearColor r g b a x) = ClearColor r g b a (f x)
	map f (CompileShader s x) = CompileShader s (f x)
	map f (CreateShader t k) = CreateShader t (f <<< k)
	map f (DepthFunc func x) = DepthFunc func (f x)
	map f (Enable cap x) = Enable cap (f x)
	map f (GetCanvas k) = GetCanvas (f <<< k)
	map f (GetDrawingBufferHeight k) = GetDrawingBufferHeight (f <<< k)
	map f (GetDrawingBufferWidth k) = GetDrawingBufferWidth (f <<< k)
	map f (ShaderSource sh src x) = ShaderSource sh src (f x)
	map f (Viewport x y w h a) = Viewport x y w h (f a)

type WebGL = Free WebGLF

clear :: GLbitfield -> WebGL Unit
clear mask = liftF $ Clear mask unit

clearColor :: GLclampf -> GLclampf -> GLclampf -> GLclampf -> WebGL Unit
clearColor r g b a = liftF $ ClearColor r g b a unit

compileShader :: WebGLShader -> WebGL Unit
compileShader s = liftF $ CompileShader s unit

createShader :: GLenum -> WebGL (Maybe WebGLShader)
createShader t = liftF $ CreateShader t id

depthFunc :: GLenum -> WebGL Unit
depthFunc func = liftF $ DepthFunc func unit

enable :: GLenum -> WebGL Unit
enable cap = liftF $ Enable cap unit

getCanvas :: WebGL CanvasElement
getCanvas = liftF $ GetCanvas id

getDrawingBufferHeight :: WebGL Int
getDrawingBufferHeight = liftF $ GetDrawingBufferHeight id

getDrawingBufferWidth :: WebGL Int
getDrawingBufferWidth = liftF $ GetDrawingBufferWidth id

shaderSource :: WebGLShader -> String -> WebGL Unit
shaderSource sh src = liftF $ ShaderSource sh src unit

viewport :: GLint -> GLint -> GLsizei -> GLsizei -> WebGL Unit
viewport x y w h = liftF $ Viewport x y w h unit

runWebGL :: forall a eff. WebGLContext -> WebGL a -> Eff (canvas :: Canvas | eff) a
runWebGL gl = runFreeM interpret
	where
	interpret :: forall a. WebGLF (WebGL a) -> Eff (canvas :: Canvas | eff) (WebGL a)
	interpret (Clear mask rest) = do
		R.clear gl mask
		return rest
	interpret (ClearColor r g b a rest) = do
		R.clearColor gl r g b a
		return rest
	interpret (CompileShader sh rest) = do
		R.compileShader gl sh
		return rest
	interpret (CreateShader t k) = do
		shader <- R.createShader gl t
		return $ k shader
	interpret (DepthFunc func rest) = do
		R.depthFunc gl func
		return rest
	interpret (Enable cap rest) = do
		R.enable gl cap
		return rest
	interpret (GetCanvas k) = do
		el <- R.getCanvas gl
		return $ k el
	interpret (GetDrawingBufferHeight k) = do
		h <- R.getDrawingBufferHeight gl
		return $ k h
	interpret (GetDrawingBufferWidth k) = do
		w <- R.getDrawingBufferWidth gl
		return $ k w
	interpret (ShaderSource sh src rest) = do
		R.shaderSource gl sh src
		return rest
	interpret (Viewport x y w h rest) = do
		R.viewport gl x y w h
		return rest
