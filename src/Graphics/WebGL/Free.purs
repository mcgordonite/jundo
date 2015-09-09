module Graphics.WebGL.Free (
	WebGL(),
	WebGLF()
	) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Free
import Graphics.Canvas (Canvas())
import qualified Graphics.WebGL.Raw as R
import Graphics.WebGL.Raw.Types

data WebGLF a
	= Clear GLbitfield a
	| ClearColor GLclampf GLclampf GLclampf GLclampf a
	| DepthFunc GLenum a
	| Enable GLenum a
	| Viewport GLint GLint GLsizei GLsizei a

instance functorWebGLF :: Functor WebGLF where
	map f (Clear mask x) = Clear mask (f x)
	map f (ClearColor r g b a x) = ClearColor r g b a (f x)
	map f (DepthFunc func x) = DepthFunc func (f x)
	map f (Enable cap x) = Enable cap (f x)
	map f (Viewport x y w h a) = Viewport x y w h (f a)

type WebGL = Free WebGLF

clear :: GLbitfield -> WebGL Unit
clear mask = liftF $ Clear mask unit

clearColor :: GLclampf -> GLclampf -> GLclampf -> GLclampf -> WebGL Unit
clearColor r g b a = liftF $ ClearColor r g b a unit

depthFunc :: GLenum -> WebGL Unit
depthFunc func = liftF $ DepthFunc func unit

enable :: GLenum -> WebGL Unit
enable cap = liftF $ Enable cap unit

viewport :: GLint -> GLint -> GLsizei -> GLsizei -> WebGL Unit
viewport x y w h = liftF $ Viewport x y w h unit

runWebGL :: forall a eff. R.WebGLContext -> WebGL a -> Eff (canvas :: Canvas | eff) a
runWebGL gl = runFreeM interpret
	where
	interpret :: WebGLF (WebGL a) -> Eff (canvas :: Canvas | eff) a
	interpret (Clear mask rest) = do
		R.clear gl mask
		return rest
	interpret (ClearColor r g b a rest) = do
		R.clearColor gl r g b a
		return rest
	interpret (DepthFunc func rest) = do
		R.depthFunc gl func
		return rest
	interpret (Enable cap) = do
		R.enable gl cap
		return rest
	interpret (Viewport x y w h rest) = do
		R.viewport gl x y w h
		return rest
	