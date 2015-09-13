module Shaders (
	getFragmentShader,
	getVertexShader
	) where

import Prelude
import Control.Monad.Eff
import Data.Maybe
import Data.Nullable
import Graphics.Canvas (Canvas())
import Graphics.WebGL.Free
import Graphics.WebGL.Raw.Types
import qualified Graphics.WebGL.Raw.Enums as GL
import qualified DOM as D
import qualified DOM.Node.Node as D
import qualified DOM.Node.NonElementParentNode as D
import qualified DOM.Node.Types as D
import qualified DOM.HTML as D
import qualified DOM.HTML.Types as D
import qualified DOM.HTML.Window as D

loadSourceFromElement :: forall eff. D.ElementId -> Eff (dom :: D.DOM | eff) String
loadSourceFromElement elementId = do
	document <- D.window >>= D.document >>= pure <<< D.htmlDocumentToNonElementParentNode
	Just el <- D.getElementById elementId document >>= pure <<< toMaybe
	D.textContent $ D.elementToNode el

getShader :: forall eff. D.ElementId -> GLenum -> WebGLContext -> Eff (canvas :: Canvas, dom :: D.DOM | eff) WebGLShader
getShader elementId shaderType gl = do
	source <- loadSourceFromElement elementId
	runWebGL gl do
		Just shader <- createShader shaderType
		shaderSource shader source
		compileShader shader
		-- TODO: Check shader compile status (getShaderParameter)
		return shader

getFragmentShader :: forall eff. WebGLContext -> Eff (canvas :: Canvas, dom :: D.DOM | eff) WebGLShader
getFragmentShader = getShader (D.ElementId "fragment-shader") GL.fragmentShader

getVertexShader :: forall eff. WebGLContext -> Eff (canvas :: Canvas, dom :: D.DOM | eff) WebGLShader
getVertexShader = getShader (D.ElementId "vertex-shader") GL.vertexShader
