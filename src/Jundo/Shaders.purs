-- | Functions for loading shader source code from script tags and compiling them into a shader program
module Jundo.Shaders (
  ShaderVariables(..),
  initialiseShaderProgram
  ) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Data.Either
import Data.Maybe
import Data.Nullable
import Data.Tuple
import qualified DOM as D
import qualified DOM.Node.Node as D
import qualified DOM.Node.NonElementParentNode as D
import qualified DOM.Node.Types as D
import qualified DOM.HTML as D
import qualified DOM.HTML.Types as D
import qualified DOM.HTML.Window as D
import Graphics.Canvas (Canvas())
import Graphics.WebGL.Free
import Graphics.WebGL.Free.Shaders (compileAndLinkProgram)

-- | Where we keep the fragment shader code
fragmentShaderId :: D.ElementId
fragmentShaderId = D.ElementId "fragment-shader"

-- | Where we keep the vertex shader code
vertexShaderId :: D.ElementId
vertexShaderId = D.ElementId "vertex-shader"

-- | Type to contain the locations of variables in the compiled shader program
type ShaderVariables = {
  ambientColour :: WebGLUniformLocation,
  directionalColour :: WebGLUniformLocation,
  lightingDirection :: WebGLUniformLocation,
  mvMatrix :: WebGLUniformLocation,
  pMatrix :: WebGLUniformLocation,
  nMatrix :: WebGLUniformLocation,
  position :: AttributeLocation,
  colour :: AttributeLocation,
  normal :: AttributeLocation
  }

shaderVariables :: WebGLUniformLocation -> WebGLUniformLocation -> WebGLUniformLocation -> WebGLUniformLocation -> WebGLUniformLocation
  -> WebGLUniformLocation -> AttributeLocation -> AttributeLocation -> AttributeLocation -> ShaderVariables
shaderVariables mvMatrix pMatrix nMatrix ambientColour directionalColour lightingDirection position colour normal = {
  ambientColour: ambientColour,
  directionalColour: directionalColour,
  lightingDirection: lightingDirection,
  mvMatrix: mvMatrix,
  pMatrix: pMatrix,
  nMatrix: nMatrix,
  position: position,
  colour: colour,
  normal: normal
  }

loadShaderSourceFromElement :: forall eff. D.ElementId -> Eff (dom :: D.DOM | eff) String
loadShaderSourceFromElement elementId = do
  document <- D.window >>= D.document >>= pure <<< D.htmlDocumentToNonElementParentNode
  Just el <- D.getElementById elementId document >>= pure <<< toMaybe
  D.textContent $ D.elementToNode el

-- | Load and compile the fragment and vertex shader code from script tags, throwing a JavaScript exception if this fails
initialiseShaderProgram :: forall eff. WebGLContext -> Eff (canvas :: Canvas, dom :: D.DOM, err :: EXCEPTION | eff) (Tuple WebGLProgram ShaderVariables)
initialiseShaderProgram gl = do
  fragmentSource <- loadShaderSourceFromElement fragmentShaderId
  vertexSource <- loadShaderSourceFromElement vertexShaderId
  eitherProgram <- runWebGL gl $ compileAndLinkProgram vertexSource fragmentSource
  case eitherProgram of
    Left err -> throwException $ error err
    Right program -> do
      maybeVariables <- runWebGL gl do
        maybeMVMatrixLocation <- getUniformLocation program "mvMatrix"
        maybePMatrixLocation <- getUniformLocation program "pMatrix"
        maybeNMatrixLocation <- getUniformLocation program "nMatrix"
        maybeAmbientColourLocation <- getUniformLocation program "ambientColour"
        maybeDirectionalColourLocation <- getUniformLocation program "directionalColour"
        maybeLightingDirectionLocation <- getUniformLocation program "lightingDirection"
        maybePositionLocation <- getAttribLocation program "vertexPosition"
        maybeColourLocation <- getAttribLocation program "vertexColour"
        maybeNormalLocation <- getAttribLocation program "vertexNormal"
        return $ shaderVariables
          <$> maybeMVMatrixLocation
          <*> maybePMatrixLocation
          <*> maybeNMatrixLocation
          <*> maybeAmbientColourLocation
          <*> maybeDirectionalColourLocation
          <*> maybeLightingDirectionLocation
          <*> maybePositionLocation
          <*> maybeColourLocation
          <*> maybeNormalLocation
      case maybeVariables of
        Nothing -> throwException $ error "Missing shader program location"
        Just variables -> do
          runWebGL gl $ programOperation program do
            enableVertexAttribArray variables.position
            enableVertexAttribArray variables.colour
            enableVertexAttribArray variables.normal
          return $ Tuple program variables
