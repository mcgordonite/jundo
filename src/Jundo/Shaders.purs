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
  materialColour :: WebGLUniformLocation,
  mvMatrix :: WebGLUniformLocation,
  pMatrix :: WebGLUniformLocation,
  nMatrix :: WebGLUniformLocation,
  position :: AttributeLocation,
  normal :: AttributeLocation
  }

shaderVariables :: WebGLUniformLocation -> WebGLUniformLocation -> WebGLUniformLocation -> WebGLUniformLocation -> WebGLUniformLocation
  -> WebGLUniformLocation -> WebGLUniformLocation -> AttributeLocation -> AttributeLocation -> ShaderVariables
shaderVariables mvMatrix pMatrix nMatrix materialColour ambientColour directionalColour lightingDirection position normal = {
  ambientColour: ambientColour,
  directionalColour: directionalColour,
  lightingDirection: lightingDirection,
  materialColour: materialColour,
  mvMatrix: mvMatrix,
  pMatrix: pMatrix,
  nMatrix: nMatrix,
  position: position,
  normal: normal
  }

loadShaderSourceFromElement :: forall eff. D.ElementId -> Eff (dom :: D.DOM | eff) String
loadShaderSourceFromElement elementId = do
  document <- D.window >>= D.document >>= pure <<< D.htmlDocumentToNonElementParentNode
  Just el <- D.getElementById elementId document >>= pure <<< toMaybe
  D.textContent $ D.elementToNode el

-- | Attempt to get the named uniform location, returning either the location or an error containing the name
uniformByName :: WebGLProgram -> String -> WebGL (Either String WebGLUniformLocation)
uniformByName program name = do
  maybeLocation <- getUniformLocation program name
  case maybeLocation of
    Nothing -> return $ Left ("Missing uniform location " ++ name)
    Just location -> return $ Right location

-- | Attempt to get the named attribute location, returning either the location or an error containing the name
attributeByName :: WebGLProgram -> String -> WebGL (Either String AttributeLocation)
attributeByName program name = do
  maybeLocation <- getAttribLocation program name
  case maybeLocation of
    Nothing -> return $ Left ("Missing attribute location " ++ name)
    Just location -> return $ Right location

-- | Load and compile the fragment and vertex shader code from script tags, throwing a JavaScript exception if this fails
initialiseShaderProgram :: forall eff. WebGLContext -> Eff (canvas :: Canvas, dom :: D.DOM, err :: EXCEPTION | eff) (Tuple WebGLProgram ShaderVariables)
initialiseShaderProgram gl = do
  fragmentSource <- loadShaderSourceFromElement fragmentShaderId
  vertexSource <- loadShaderSourceFromElement vertexShaderId
  eitherProgram <- runWebGL gl $ compileAndLinkProgram vertexSource fragmentSource
  case eitherProgram of
    Left err -> throwException $ error err
    Right program -> do
      eitherVariables <- runWebGL gl do
        eitherMVMatrixLocation <- uniformByName program "mvMatrix"
        eitherPMatrixLocation <- uniformByName program "pMatrix"
        eitherNMatrixLocation <- uniformByName program "nMatrix"
        eitherMaterialColourLocation <- uniformByName program "materialColour"
        eitherAmbientColourLocation <- uniformByName program "ambientColour"
        eitherDirectionalColourLocation <- uniformByName program "directionalColour"
        eitherLightingDirectionLocation <- uniformByName program "lightingDirection"
        eitherPositionLocation <- attributeByName program "vertexPosition"
        eitherNormalLocation <- attributeByName program "vertexNormal"
        return $ shaderVariables
          <$> eitherMVMatrixLocation
          <*> eitherPMatrixLocation
          <*> eitherNMatrixLocation
          <*> eitherMaterialColourLocation
          <*> eitherAmbientColourLocation
          <*> eitherDirectionalColourLocation
          <*> eitherLightingDirectionLocation
          <*> eitherPositionLocation
          <*> eitherNormalLocation
      case eitherVariables of
        Left err -> throwException $ error err
        Right variables -> do
          runWebGL gl $ programOperation program do
            enableVertexAttribArray variables.position
            enableVertexAttribArray variables.normal
          return $ Tuple program variables
