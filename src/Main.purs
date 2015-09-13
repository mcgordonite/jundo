module Main where

import Prelude
import Control.Monad.Eff
import Data.Int.Bits
import Data.Maybe
import Graphics.WebGL.Context
import Graphics.WebGL.Free
import qualified Graphics.WebGL.Raw.Enums as GL
import Graphics.WebGL.Raw.Types
import Graphics.Canvas (Canvas(), CanvasElement(), getCanvasElementById, setCanvasDimensions)
import Graphics.Canvas.Extra

resize :: forall eff. CanvasElement -> WebGLContext -> Eff (canvas :: Canvas | eff) Unit
resize el gl = do
	h <- clientHeight el
	w <- clientWidth el
	setCanvasDimensions {height: h, width: w} el
	runWebGL gl do
		bufferHeight <- getDrawingBufferHeight
		bufferWidth <- getDrawingBufferWidth
		viewport 0 0 bufferWidth bufferHeight

main :: Eff (canvas :: Canvas) Unit
main = do
	Just el <- getCanvasElementById "easel"
	gl <- getWebGLContext el
	resize el gl
	runWebGL gl do
		clearColor 0.0 0.0 0.0 1.0
		enable GL.depthTest
		depthFunc GL.lequal
		clear $ GL.colorBufferBit .|. GL.depthBufferBit
