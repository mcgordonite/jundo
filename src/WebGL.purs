module WebGL (
	WebGL(),
	createBuffer,
	WebGLBuffer(),
	logBuffer,
	WebGLRenderingContext(),
	unsafeGetContext,
	runWebGL
	) where

import Prelude
import Control.Monad.Reader

foreign import data WebGLRenderingContext :: *
foreign import data WebGLBuffer :: *
foreign import createBufferImpl :: WebGLRenderingContext -> WebGLBuffer

type WebGL a = Reader WebGLRenderingContext a

runWebGL :: forall a. WebGLRenderingContext -> WebGL a -> a
runWebGL ctx x = runReader x ctx

createBuffer :: WebGL WebGLBuffer
createBuffer = do
	gl <- ask
	return $ createBufferImpl gl


-- WARNING: This module becomes uncool from this point on

import Control.Monad.Eff
import Control.Monad.Eff.Console
foreign import unsafeGetContext :: String -> WebGLRenderingContext
foreign import logBuffer :: forall eff. WebGLBuffer -> Eff (console :: CONSOLE | eff) Unit
