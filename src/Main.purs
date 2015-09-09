module Main where

import Prelude
import Control.Monad.Eff
import Data.Int.Bits
import Graphics.WebGL.Context
import Graphics.WebGL.Free
import qualified Graphics.WebGL.Raw.Enums as GL
import Graphics.Canvas (Canvas(), getCanvasElementById)

main :: Eff (canvas :: Canvas) Unit
main = do
	Just el <- getCanvasElementById "easel"
	gl <- getWebGLContext el
	runWebGL gl do
		viewport 0 0 500 500
		clearColor 0.0 0.0 0.0 1.0
		enable GL.depthTest
		depthFunc GL.lequal
		clear $ GL.colorBufferBit .|. GL.depthBufferBit
		