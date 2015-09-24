-- | Functions for loading shader source code from script tags and compiling them into a shader program
module Jundo.Shaders (
  ProgramLocations(..),
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
import Graphics.WebGL.Raw.Types

-- | Where we keep the fragment shader code
fragmentShaderId :: D.ElementId
fragmentShaderId = D.ElementId "fragment-shader"

-- | Where we keep the vertex shader code
vertexShaderId :: D.ElementId
vertexShaderId = D.ElementId "vertex-shader"

-- | Type to contain the locations of variables in the compiled shader program
type ProgramLocations = {mvMatrix :: WebGLUniformLocation, pMatrix :: WebGLUniformLocation, aVertex :: AttributeLocation}

programLocations :: WebGLUniformLocation -> WebGLUniformLocation -> AttributeLocation -> ProgramLocations
programLocations mvMatrix pMatrix aVertex = {mvMatrix: mvMatrix, pMatrix: pMatrix, aVertex: aVertex}

loadShaderSourceFromElement :: forall eff. D.ElementId -> Eff (dom :: D.DOM | eff) String
loadShaderSourceFromElement elementId = do
  document <- D.window >>= D.document >>= pure <<< D.htmlDocumentToNonElementParentNode
  Just el <- D.getElementById elementId document >>= pure <<< toMaybe
  D.textContent $ D.elementToNode el

-- | Load and compile the fragment and vertex shader code from script tags, throwing a JavaScript exception if this fails
initialiseShaderProgram :: forall eff. WebGLContext -> Eff (canvas :: Canvas, dom :: D.DOM, err :: EXCEPTION | eff) (Tuple WebGLProgram ProgramLocations)
initialiseShaderProgram gl = do
  fragmentSource <- loadShaderSourceFromElement fragmentShaderId
  vertexSource <- loadShaderSourceFromElement vertexShaderId
  eitherProgram <- runWebGL gl $ compileAndLinkProgram vertexSource fragmentSource
  case eitherProgram of
    Left err -> throwException $ error err
    Right program -> do
      maybeLocations <- runWebGL gl do
        maybeMVMatrixLocation <- getUniformLocation program "uMVMatrix"
        maybePMatrixLocation <- getUniformLocation program "uPMatrix"
        maybeAVertexLocation <- getAttribLocation program "aVertexPosition"
        return $ programLocations <$> maybeMVMatrixLocation <*> maybePMatrixLocation <*> maybeAVertexLocation
      case maybeLocations of
        Nothing -> throwException $ error "Missing shader program location"
        Just locations -> do
          runWebGL gl $ enableVertexAttribArray locations.aVertex
          return $ Tuple program locations
