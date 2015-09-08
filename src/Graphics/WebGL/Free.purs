module Graphics.WebGL.Free (
	WebGL(),
	WebGLF()
	) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Free
import qualified Graphics.WebGL.Raw as GL
import Graphics.WebGL.Raw.Types

enable :: forall eff. WebGLContext -> GLenum -> Eff (canvas :: Canvas | eff) Unit
enable webgl cap = runFn2 enableImpl webgl cap

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

--viewport :: forall eff. WebGLContext -> GLint -> GLint -> GLsizei -> GLsizei- -> Eff (canvas :: Canvas | eff) Unit 
--viewport :: GLint -> GLint -> GLsizei -> GLsizei -> WebGL Unit
--viewport = liftF $ Viewport 
