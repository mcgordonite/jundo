module Main where

import Prelude
import Control.Monad.Eff
import Data.Int.Bits
import Data.Maybe
import Data.Nullable
import qualified DOM as D
import qualified DOM.RequestAnimationFrame as D
import qualified DOM.Node.Node as D
import qualified DOM.Node.NonElementParentNode as D
import qualified DOM.Node.Types as D
import qualified DOM.HTML as D
import qualified DOM.HTML.Types as D
import qualified DOM.HTML.Window as D
import Graphics.WebGL.Context
import Graphics.WebGL.Free
import Graphics.WebGL.Free.Shaders
import qualified Graphics.WebGL.Raw.Enums as GL
import Graphics.WebGL.Raw.Types
import Graphics.Canvas (Canvas(), CanvasElement(), getCanvasElementById, setCanvasDimensions)
import Graphics.Canvas.Extra

fragmentShaderId :: D.ElementId
fragmentShaderId = D.ElementId "fragment-shader"

vertexShaderId :: D.ElementId
vertexShaderId = D.ElementId "vertex-shader"

loadShaderSourceFromElement :: forall eff. D.ElementId -> Eff (dom :: D.DOM | eff) String
loadShaderSourceFromElement elementId = do
	document <- D.window >>= D.document >>= pure <<< D.htmlDocumentToNonElementParentNode
	Just el <- D.getElementById elementId document >>= pure <<< toMaybe
	D.textContent $ D.elementToNode el

resize :: forall eff. CanvasElement -> WebGLContext -> Eff (canvas :: Canvas | eff) Unit
resize el gl = do
	h <- clientHeight el
	w <- clientWidth el
	setCanvasDimensions {height: h, width: w} el
	runWebGL gl do
		bufferHeight <- getDrawingBufferHeight
		bufferWidth <- getDrawingBufferWidth
		viewport 0 0 bufferWidth bufferHeight

tick :: forall eff. CanvasElement -> WebGLContext -> Eff (canvas :: Canvas, dom :: D.DOM | eff) Unit
tick el gl = do
	resize el gl
	runWebGL gl do
		clear $ GL.colorBufferBit .|. GL.depthBufferBit
	D.requestAnimationFrame $ tick el gl

main :: Eff (canvas :: Canvas, dom :: D.DOM) Unit
main = do
	fragmentSource <- loadShaderSourceFromElement fragmentShaderId
	vertexSource <- loadShaderSourceFromElement vertexShaderId
	Just el <- getCanvasElementById "easel"
	gl <- getWebGLContext el
	runWebGL gl do
		fragmentShader <- buildShader fragmentSource GL.fragmentShader
		vertexShader <- buildShader vertexSource GL.vertexShader
		program <- buildProgram vertexShader fragmentShader
		useProgram program
		vertexPositionAttribute <- getAttribLocation program "aVertexPosition"
		enableVertexAttribArray vertexPositionAttribute
		clearColor 0.0 0.0 0.0 1.0
		enable GL.depthTest
		depthFunc GL.lequal
	tick el gl
