module Graphics.WebGL.Free.Shaders (
	buildProgram,
	buildShader
	) where

import Prelude
import Data.Maybe
import Graphics.WebGL.Free
import Graphics.WebGL.Raw.Types

buildShader :: forall eff. DOMString -> GLenum-> WebGL WebGLShader
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
