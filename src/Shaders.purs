module Shaders (
	initialiseShaders
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

fragmentShaderId :: D.ElementId
fragmentShaderId = D.ElementId "fragment-shader"

vertexShaderId :: D.ElementId
vertexShaderId = D.ElementId "vertex-shader"

loadSourceFromElement :: forall eff. D.ElementId -> Eff (dom :: D.DOM | eff) String
loadSourceFromElement elementId = do
	document <- D.window >>= D.document >>= pure <<< D.htmlDocumentToNonElementParentNode
	Just el <- D.getElementById elementId document >>= pure <<< toMaybe
	D.textContent $ D.elementToNode el

buildShader :: forall eff. String -> GLenum-> WebGL WebGLShader
buildShader source shaderType = do
	Just shader <- createShader shaderType
	shaderSource shader source
	compileShader shader
	-- TODO: Check shader compile status (getShaderParameter)
	return shader

buildProgram :: forall eff. WebGLShader -> WebGLShader -> WebGL WebGLProgram
buildProgram vs fs = do
	Just program <- createProgram
	attachShader program vs
	attachShader program fs
	linkProgram program
	-- TODO: Check shader program link status (getProgramParameter)
	return program

initialiseShaders :: forall eff. WebGLContext -> Eff (canvas :: Canvas, dom :: D.DOM | eff) Unit
initialiseShaders gl = do
	fragmentSource <- loadSourceFromElement fragmentShaderId
	vertexSource <- loadSourceFromElement vertexShaderId
	runWebGL gl do
		fragmentShader <- buildShader fragmentSource GL.fragmentShader
		vertexShader <- buildShader vertexSource GL.vertexShader
		program <- buildProgram vertexShader fragmentShader
		useProgram program
